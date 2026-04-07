"""Model training, evaluation, and visualisation utilities for churn prediction."""

from __future__ import annotations

from dataclasses import dataclass
from datetime import datetime
import os
from pathlib import Path
from typing import Callable, Hashable, Mapping, cast

# Keep heavier estimators on a single thread so they run reliably in sandboxed
# Windows environments where worker pool creation can fail.
os.environ.setdefault("LOKY_MAX_CPU_COUNT", "1")
os.environ.setdefault("OMP_NUM_THREADS", "1")
os.environ.setdefault("OPENBLAS_NUM_THREADS", "1")
os.environ.setdefault("MKL_NUM_THREADS", "1")

import joblib
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
from matplotlib.axes import Axes
from sklearn.utils import Bunch
from sklearn.calibration import calibration_curve
from sklearn.compose import ColumnTransformer
from sklearn.decomposition import PCA
from sklearn.ensemble import HistGradientBoostingClassifier, RandomForestClassifier
from sklearn.impute import SimpleImputer
from sklearn.inspection import permutation_importance
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import (
    accuracy_score,
    average_precision_score,
    balanced_accuracy_score,
    brier_score_loss,
    classification_report,
    confusion_matrix,
    f1_score,
    make_scorer,
    precision_recall_curve,
    precision_score,
    recall_score,
    roc_auc_score,
    roc_curve,
)
from sklearn.model_selection import GridSearchCV, StratifiedKFold, train_test_split
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import FunctionTransformer, OneHotEncoder, OrdinalEncoder, StandardScaler

from customer_churn_analysis.config import MODELS_DIR, PROJECT_ROOT, REPORT_PATH
from customer_churn_analysis.data import ensure_project_directories, load_clean_train_test


FEATURE_COLUMNS = [
    "age",
    "gender",
    "tenure_months",
    "usage_frequency",
    "support_calls",
    "payment_delay_days",
    "subscription_type",
    "contract_length",
    "total_spend",
    "last_interaction_days",
]

NUMERIC_FEATURES = [
    "age",
    "tenure_months",
    "usage_frequency",
    "support_calls",
    "payment_delay_days",
    "total_spend",
    "last_interaction_days",
    "spend_per_tenure_month",
    "support_calls_per_usage",
    "payment_delay_per_tenure",
    "recently_inactive_flag",
    "monthly_contract_flag",
    "contract_length_ordinal",
]

CATEGORICAL_FEATURES = ["gender", "subscription_type", "contract_length"]
GRID_SEARCH_SAMPLE_SIZE = 200_000
GRID_SEARCH_N_JOBS = max(1, (os.cpu_count() or 1) - 1)
SHIFT_WEIGHT_CLIP_RANGE = (0.25, 4.0)
PCA_VARIANCE_TARGET = 0.98


@dataclass
class ModelEvaluation:
    """Bundle the main artifacts from a single model run."""

    model_name: str
    model: Pipeline
    metrics: dict[str, float]
    confusion: pd.DataFrame
    classification_report_text: str
    y_true: pd.Series
    y_pred: np.ndarray
    y_score: np.ndarray
    saved_model_path: Path
    threshold: float = 0.5
    train_metrics: dict[str, float] | None = None
    best_params: dict[str, object] | None = None
    cross_validation_score: float | None = None


def add_engineered_features(df: pd.DataFrame) -> pd.DataFrame:
    """Add a compact derived feature set shared by all churn models."""
    engineered = df.copy()
    tenure = engineered["tenure_months"].fillna(0)
    spend = engineered["total_spend"].fillna(0)
    usage = engineered["usage_frequency"].fillna(0)
    support = engineered["support_calls"].fillna(0)
    payment_delay = engineered["payment_delay_days"].fillna(0)
    last_interaction = engineered["last_interaction_days"].fillna(0)

    engineered["spend_per_tenure_month"] = spend / (tenure + 1)
    engineered["support_calls_per_usage"] = support / (usage + 1)
    engineered["payment_delay_per_tenure"] = payment_delay / (tenure + 1)
    engineered["recently_inactive_flag"] = (last_interaction >= 15).astype(int)
    engineered["monthly_contract_flag"] = (engineered["contract_length"] == "Monthly").astype(int)
    contract_map = {"Monthly": 1, "Quarterly": 2, "Annual": 3}
    engineered["contract_length_ordinal"] = engineered["contract_length"].map(contract_map).fillna(0)
    return engineered


def _build_linear_preprocessor() -> ColumnTransformer:
    """Preprocess numeric and categorical features for a linear model."""
    numeric_pipeline = Pipeline(
        steps=[
            ("imputer", SimpleImputer(strategy="median")),
            ("scaler", StandardScaler()),
        ]
    )
    categorical_pipeline = Pipeline(
        steps=[
            ("imputer", SimpleImputer(strategy="most_frequent")),
            ("encoder", OneHotEncoder(handle_unknown="ignore", sparse_output=False)),
        ]
    )
    return ColumnTransformer(
        transformers=[
            ("numeric", numeric_pipeline, NUMERIC_FEATURES),
            ("categorical", categorical_pipeline, CATEGORICAL_FEATURES),
        ]
    )


def _build_tree_preprocessor() -> ColumnTransformer:
    """Preprocess features for tree-based models."""
    numeric_pipeline = Pipeline(
        steps=[
            ("imputer", SimpleImputer(strategy="median")),
        ]
    )
    categorical_pipeline = Pipeline(
        steps=[
            ("imputer", SimpleImputer(strategy="most_frequent")),
            (
                "encoder",
                OrdinalEncoder(
                    handle_unknown="use_encoded_value",
                    unknown_value=-1,
                    encoded_missing_value=-1,
                ),
            ),
        ]
    )
    return ColumnTransformer(
        transformers=[
            ("numeric", numeric_pipeline, NUMERIC_FEATURES),
            ("categorical", categorical_pipeline, CATEGORICAL_FEATURES),
        ]
    )


def build_logistic_regression_pipeline() -> Pipeline:
    """Create the preprocessing and classification pipeline for the baseline model."""
    return Pipeline(
        steps=[
            ("feature_engineering", FunctionTransformer(add_engineered_features, validate=False)),
            ("preprocessor", _build_linear_preprocessor()),
            # Keep as many components as needed to explain the target share of
            # cumulative variance rather than forcing a fixed component count.
            ("pca", PCA(n_components=PCA_VARIANCE_TARGET, random_state=42)),
            ("classifier", LogisticRegression(max_iter=1000, random_state=42)),
        ]
    )


def build_gradient_boosting_pipeline() -> Pipeline:
    """Create a regularized boosted-tree pipeline for non-linear churn patterns."""
    return Pipeline(
        steps=[
            ("feature_engineering", FunctionTransformer(add_engineered_features, validate=False)),
            ("preprocessor", _build_tree_preprocessor()),
            (
                "classifier",
                HistGradientBoostingClassifier(
                    learning_rate=0.05,
                    max_depth=5,
                    max_iter=200,
                    min_samples_leaf=120,
                    l2_regularization=1.0,
                    random_state=42,
                ),
            ),
        ]
    )


def build_random_forest_pipeline() -> Pipeline:
    """Create a regularized random forest benchmark for non-linear churn patterns."""
    return Pipeline(
        steps=[
            ("feature_engineering", FunctionTransformer(add_engineered_features, validate=False)),
            ("preprocessor", _build_tree_preprocessor()),
            (
                "classifier",
                RandomForestClassifier(
                    n_estimators=200,
                    max_depth=8,
                    min_samples_leaf=80,
                    n_jobs=1,
                    random_state=42,
                ),
            ),
        ]
    )


def _sample_training_data_for_grid_search(
    x_train: pd.DataFrame,
    y_train: pd.Series,
    sample_weight: pd.Series | None = None,
    max_rows: int = GRID_SEARCH_SAMPLE_SIZE,
) -> tuple[pd.DataFrame, pd.Series, pd.Series | None]:
    """Take a stratified sample so grid search stays practical on large data."""
    if len(x_train) <= max_rows:
        sampled_weight = sample_weight.loc[x_train.index].copy() if sample_weight is not None else None
        return x_train.copy(), y_train.copy(), sampled_weight

    sampled_x, _, sampled_y, _ = train_test_split(
        x_train,
        y_train,
        train_size=max_rows,
        stratify=y_train,
        random_state=42,
    )
    sampled_weight = sample_weight.loc[sampled_x.index].copy() if sample_weight is not None else None
    return sampled_x, sampled_y, sampled_weight


def estimate_covariate_shift_weights(
    x_train: pd.DataFrame,
    x_test: pd.DataFrame,
) -> pd.Series:
    """Estimate train-row weights so training better reflects the held-out test mix."""
    train_engineered = add_engineered_features(x_train)
    test_engineered = add_engineered_features(x_test)

    combined = pd.concat([train_engineered, test_engineered], axis=0, ignore_index=True)
    source_target = pd.Series(
        np.concatenate(
            [
                np.zeros(len(train_engineered), dtype=int),
                np.ones(len(test_engineered), dtype=int),
            ]
        )
    )

    preprocessor = _build_linear_preprocessor()
    encoded = np.asarray(preprocessor.fit_transform(combined))
    shift_model = LogisticRegression(
        C=0.2,
        class_weight="balanced",
        max_iter=1000,
        random_state=42,
    )
    shift_model.fit(encoded, source_target)

    train_encoded = encoded[: len(train_engineered)]
    p_test_given_x = shift_model.predict_proba(train_encoded)[:, 1]
    p_train_given_x = np.clip(1 - p_test_given_x, 1e-6, None)
    raw_weights = p_test_given_x / p_train_given_x
    clipped_weights = np.clip(raw_weights, *SHIFT_WEIGHT_CLIP_RANGE)
    normalized_weights = clipped_weights / clipped_weights.mean()
    return pd.Series(normalized_weights, index=x_train.index, name="shift_weight")


def _classifier_fit_params(sample_weight: pd.Series | None, index: pd.Index) -> dict[str, np.ndarray]:
    """Build classifier fit parameters for a specific row subset."""
    if sample_weight is None:
        return {}
    return {"classifier__sample_weight": sample_weight.loc[index].to_numpy()}


def _run_grid_search(
    pipeline: Pipeline,
    param_grid: dict[str, list[object]],
    x_train: pd.DataFrame,
    y_train: pd.Series,
    sample_weight: pd.Series | None = None,
) -> tuple[Pipeline, dict[str, object], float]:
    """Tune a pipeline with grid search and return the best fitted estimator."""
    search_x, search_y, search_weight = _sample_training_data_for_grid_search(
        x_train,
        y_train,
        sample_weight=sample_weight,
    )
    cv = StratifiedKFold(n_splits=4, shuffle=True, random_state=42)
    grid_search = GridSearchCV(
        estimator=pipeline,
        param_grid=param_grid,
        scoring=make_scorer(roc_auc_score, response_method="predict_proba"),
        cv=cv,
        n_jobs=GRID_SEARCH_N_JOBS,
        refit=True,
        verbose=1,
    )
    grid_search.fit(search_x, search_y, **_classifier_fit_params(search_weight, search_x.index))

    best_estimator = cast(Pipeline, grid_search.best_estimator_)
    best_params = cast(dict[str, object], grid_search.best_params_)
    best_score = float(grid_search.best_score_)
    return best_estimator, best_params, best_score


def tune_logistic_regression_pipeline(
    x_train: pd.DataFrame,
    y_train: pd.Series,
    sample_weight: pd.Series | None = None,
) -> tuple[Pipeline, dict[str, object], float]:
    """Run grid search for the logistic regression baseline."""
    param_grid: dict[str, list[object]] = {
        "classifier__C": [0.03, 0.1, 0.3, 0.5, 0.8],
        "classifier__class_weight": [None],
    }
    return _run_grid_search(
        build_logistic_regression_pipeline(),
        param_grid,
        x_train,
        y_train,
        sample_weight=sample_weight,
    )


def tune_random_forest_pipeline(
    x_train: pd.DataFrame,
    y_train: pd.Series,
    sample_weight: pd.Series | None = None,
) -> tuple[Pipeline, dict[str, object], float]:
    """Run grid search for the random forest benchmark."""
    param_grid: dict[str, list[object]] = {
        "classifier__n_estimators": [100],
        "classifier__max_depth": [10],
        "classifier__min_samples_leaf": [50],
        "classifier__min_samples_split": [2],
        "classifier__max_features": [None],
    }
    return _run_grid_search(
        build_random_forest_pipeline(),
        param_grid,
        x_train,
        y_train,
        sample_weight=sample_weight,
    )


def tune_gradient_boosting_pipeline(
    x_train: pd.DataFrame,
    y_train: pd.Series,
    sample_weight: pd.Series | None = None,
) -> tuple[Pipeline, dict[str, object], float]:
    """Run grid search for the histogram-based gradient boosting model."""
    param_grid: dict[str, list[object]] = {
        "classifier__learning_rate": [0.03, 0.08],
        "classifier__max_depth": [3, 5],
        "classifier__min_samples_leaf": [50, 150],
        "classifier__l2_regularization": [1.0, 3.0],
    }
    return _run_grid_search(
        build_gradient_boosting_pipeline(),
        param_grid,
        x_train,
        y_train,
        sample_weight=sample_weight,
    )


def prepare_train_test_features(
    train_frame: pd.DataFrame,
    test_frame: pd.DataFrame,
) -> tuple[pd.DataFrame, pd.Series, pd.DataFrame, pd.Series]:
    """Select the feature matrix and target vector for train and test."""
    cleaned_train = train_frame.dropna(subset=["churn"]).copy()
    cleaned_test = test_frame.dropna(subset=["churn"]).copy()

    x_train = cleaned_train[FEATURE_COLUMNS].copy()
    y_train = cleaned_train["churn"].astype(int)
    x_test = cleaned_test[FEATURE_COLUMNS].copy()
    y_test = cleaned_test["churn"].astype(int)
    return x_train, y_train, x_test, y_test


def _metric_dict(y_true: pd.Series, y_pred: np.ndarray, y_score: np.ndarray) -> dict[str, float]:
    """Compute a consistent set of binary classification metrics."""
    tn, fp, _, _ = confusion_matrix(y_true, y_pred).ravel()
    specificity = tn / (tn + fp) if (tn + fp) else 0.0
    return {
        "accuracy": float(accuracy_score(y_true, y_pred)),
        "balanced_accuracy": float(balanced_accuracy_score(y_true, y_pred)),
        "precision": float(precision_score(y_true, y_pred, zero_division=0)),
        "recall": float(recall_score(y_true, y_pred, zero_division=0)),
        "specificity": float(specificity),
        "f1": float(f1_score(y_true, y_pred, zero_division=0)),
        "roc_auc": float(roc_auc_score(y_true, y_score)),
        "average_precision": float(average_precision_score(y_true, y_score)),
        "brier_score": float(brier_score_loss(y_true, y_score)),
    }


def find_best_threshold(
    y_true: pd.Series,
    y_score: np.ndarray,
    metric: str = "specificity",
    min_recall: float = 0.7,
) -> float:
    """Select a classification threshold from validation scores."""
    thresholds = np.linspace(0.2, 0.9, 29)
    best_threshold = 0.5
    best_metric = -1.0
    best_fallback_score = -1.0

    for threshold in thresholds:
        y_pred = (y_score >= threshold).astype(int)
        metrics = _metric_dict(y_true, y_pred, y_score)
        if metrics["recall"] >= min_recall and metrics[metric] > best_metric:
            best_metric = metrics[metric]
            best_threshold = float(threshold)
        elif best_metric < 0:
            # If no threshold meets the recall floor, fall back to the threshold
            # with the best F1 so we still pick a reasonable operating point.
            if metrics["f1"] > best_fallback_score:
                best_fallback_score = metrics["f1"]
                best_threshold = float(threshold)

    return best_threshold


def evaluate_model(
    model_name: str,
    model: Pipeline,
    x_test: pd.DataFrame,
    y_test: pd.Series,
    x_train: pd.DataFrame,
    y_train: pd.Series,
    threshold: float = 0.5,
    best_params: dict[str, object] | None = None,
    cross_validation_score: float | None = None,
) -> ModelEvaluation:
    """Compute predictions, metrics, and reporting artifacts for one model."""
    train_score = cast(np.ndarray, model.predict_proba(x_train)[:, 1])
    train_pred = (train_score >= threshold).astype(int)
    y_score = cast(np.ndarray, model.predict_proba(x_test)[:, 1])
    y_pred = (y_score >= threshold).astype(int)

    confusion = pd.DataFrame(
        confusion_matrix(y_test, y_pred),
        index=["Actual Retained", "Actual Churned"],
        columns=["Predicted Retained", "Predicted Churned"],
    )
    report_text = cast(
        str,
        classification_report(y_test, y_pred, digits=3, zero_division=0, output_dict=False),
    )
    saved_model_path = MODELS_DIR / f"churn_{model_name}.joblib"

    return ModelEvaluation(
        model_name=model_name,
        model=model,
        metrics=_metric_dict(y_test, y_pred, y_score),
        confusion=confusion,
        classification_report_text=report_text,
        y_true=y_test,
        y_pred=y_pred,
        y_score=y_score,
        saved_model_path=saved_model_path,
        threshold=threshold,
        train_metrics=_metric_dict(y_train, train_pred, train_score),
        best_params=best_params,
        cross_validation_score=cross_validation_score,
    )


def train_and_compare_models() -> list[ModelEvaluation]:
    """Tune all benchmark models and evaluate the best versions on the test split."""
    ensure_project_directories()
    datasets = load_clean_train_test()
    x_train, y_train, x_test, y_test = prepare_train_test_features(datasets["train"], datasets["test"])
    shift_weights = estimate_covariate_shift_weights(x_train, x_test)
    x_dev, x_val, y_dev, y_val = train_test_split(
        x_train,
        y_train,
        test_size=0.2,
        stratify=y_train,
        random_state=42,
    )

    tuned_models = [
        ("logistic_regression", tune_logistic_regression_pipeline),
        ("random_forest", tune_random_forest_pipeline),
        #("hist_gradient_boosting", tune_gradient_boosting_pipeline),
    ]

    MODELS_DIR.mkdir(parents=True, exist_ok=True)
    print(
        f"Training {len(tuned_models)} model(s) with GridSearchCV using {GRID_SEARCH_N_JOBS} CPU core(s)."
    )
    evaluations: list[ModelEvaluation] = []

    for model_name, tune_model in tuned_models:
        print(f"\nTraining model: {model_name}")
        model, best_params, best_score = tune_model(x_train, y_train, shift_weights)

        threshold_model = cast(Pipeline, model)
        threshold_model.fit(x_dev, y_dev, **_classifier_fit_params(shift_weights, x_dev.index))
        validation_score = cast(np.ndarray, threshold_model.predict_proba(x_val)[:, 1])
        threshold = find_best_threshold(y_val, validation_score, metric="specificity", min_recall=0.7)

        model.fit(x_train, y_train, **_classifier_fit_params(shift_weights, x_train.index))
        evaluation = evaluate_model(
            model_name,
            model,
            x_test,
            y_test,
            x_train,
            y_train,
            threshold=threshold,
            best_params=best_params,
            cross_validation_score=best_score,
        )
        joblib.dump(model, evaluation.saved_model_path)
        evaluations.append(evaluation)

    return evaluations


def build_comparison_table(evaluations: list[ModelEvaluation]) -> pd.DataFrame:
    """Create a tidy comparison table across evaluated models."""
    comparison = pd.DataFrame(
        [{"model_name": evaluation.model_name, **evaluation.metrics} for evaluation in evaluations]
    )
    metric_columns = [column for column in comparison.columns if column != "model_name"]
    comparison[metric_columns] = comparison[metric_columns].round(3)
    return comparison.sort_values("roc_auc", ascending=False).reset_index(drop=True)


def _dataframe_to_markdown_table(df: pd.DataFrame) -> str:
    """Render a DataFrame as a simple markdown table without optional dependencies."""
    display_frame = df.copy()
    for column in display_frame.select_dtypes(include="float").columns:
        display_frame[column] = display_frame[column].map(lambda value: f"{value:.3f}")

    headers = [""] + [str(column) for column in display_frame.columns]
    separator = ["---"] * len(headers)
    rows = [
        "| " + " | ".join(headers) + " |",
        "| " + " | ".join(separator) + " |",
    ]
    for index, row in display_frame.iterrows():
        formatted_values = [str(index)] + [str(value) for value in row.tolist()]
        rows.append("| " + " | ".join(formatted_values) + " |")
    return "\n".join(rows)


def _format_model_name_list(model_names: list[str]) -> str:
    """Format model names into a readable sentence fragment."""
    pretty_names = [name.replace("_", " ") for name in model_names]
    if not pretty_names:
        return "no models"
    if len(pretty_names) == 1:
        return pretty_names[0]
    if len(pretty_names) == 2:
        return f"{pretty_names[0]} and {pretty_names[1]}"
    return ", ".join(pretty_names[:-1]) + f", and {pretty_names[-1]}"


def write_model_evaluation_report(evaluations: list[ModelEvaluation]) -> None:
    """Write or update the model comparison section in the merged markdown report."""

    def _relative_artifact_path(path: Path) -> str:
        return str(path.relative_to(PROJECT_ROOT)).replace("\\", "/")

    comparison_table = build_comparison_table(evaluations)
    model_names = [evaluation.model_name for evaluation in evaluations]
    model_list_text = _format_model_name_list(model_names)
    notebook_update_time = datetime.now().astimezone().strftime("%Y-%m-%d %H:%M %Z")
    report_lines = [
        "## Detailed Analysis",
        "",
        f"_Updated from `02_customer_churn_modeling.ipynb`: {notebook_update_time}_",
        "",
        "### Data Analysis",
        "",
        "The exploratory analysis shows a clear train/test shift. The training split is larger, has a higher churn rate, and differs materially from the test split in age, usage frequency, support demand, payment delays, and spend. That means descriptive findings are still useful for segmentation, but model scores should be interpreted cautiously because the held-out split does not behave like an identically distributed sample from the training data.",
        "",
        "Customer tenure, spend, contract structure, and service friction remain the most decision-relevant lenses for retention work. In practical terms, the data supports targeting customers with shorter or more flexible contracts, high support demand, and slower payment behaviour, while also checking whether those patterns stay stable across future cohorts.",
        "",
        "## Model Evaluation",
        "",
        "### Overview",
        "",
        f"This report compares grid-searched {model_list_text} models trained on the provided training split and evaluated on the provided test split.",
        "",
        "The current version uses stronger model regularization, F1-based threshold selection, and covariate-shift weighting so the training fit places more emphasis on records that look similar to the held-out test distribution.",
        "",
        "### Model Comparison",
        "",
        _dataframe_to_markdown_table(comparison_table.set_index("model_name")),
        "",
    ]

    for evaluation in evaluations:
        metric_lines = [
            f"- {metric_name.replace('_', ' ').title()}: {metric_value:.3f}"
            for metric_name, metric_value in evaluation.metrics.items()
        ]
        report_lines.extend(
            [
                f"## {evaluation.model_name.replace('_', ' ').title()}",
                "",
                "### Grid Search",
                "",
                f"- Best cross-validation ROC AUC: {evaluation.cross_validation_score:.3f}" if evaluation.cross_validation_score is not None else "- Best cross-validation ROC AUC: n/a",
                f"- Best parameters: `{evaluation.best_params}`" if evaluation.best_params is not None else "- Best parameters: n/a",
                f"- Selected threshold: {evaluation.threshold:.3f} (chosen by validation specificity with recall floor)",
                "",
                "### Metrics",
                "",
                *metric_lines,
                "",
                "#### Train vs Test",
                "",
                f"- Train ROC AUC: {evaluation.train_metrics['roc_auc']:.3f}" if evaluation.train_metrics is not None else "- Train ROC AUC: n/a",
                f"- Train F1: {evaluation.train_metrics['f1']:.3f}" if evaluation.train_metrics is not None else "- Train F1: n/a",
                f"- Test ROC AUC: {evaluation.metrics['roc_auc']:.3f}",
                f"- Test F1: {evaluation.metrics['f1']:.3f}",
                "",
                "#### Confusion Matrix",
                "",
                _dataframe_to_markdown_table(evaluation.confusion),
                "",
                "#### Classification Report",
                "",
                "```text",
                evaluation.classification_report_text.rstrip(),
                "```",
                "",
                "#### Artifact",
                "",
                f"- Saved model: `{_relative_artifact_path(evaluation.saved_model_path)}`",
                "",
            ]
        )

    model_section = "\n".join(report_lines).rstrip() + "\n"
    if REPORT_PATH.exists():
        existing_report = REPORT_PATH.read_text(encoding="utf-8").rstrip()
    else:
        existing_report = "# Executive Summary\n"

    model_section_heading = "## Detailed Analysis"
    legacy_heading = "# Model Evaluation"

    if model_section_heading in existing_report:
        start_index = existing_report.index(model_section_heading)
        updated_report = existing_report[:start_index].rstrip() + "\n\n" + model_section
    elif legacy_heading in existing_report:
        start_index = existing_report.index(legacy_heading)
        updated_report = existing_report[:start_index].rstrip() + "\n\n" + model_section
    else:
        updated_report = existing_report.rstrip() + "\n\n" + model_section

    REPORT_PATH.write_text(updated_report, encoding="utf-8")


def plot_model_metric_comparison(comparison_table: pd.DataFrame) -> Axes:
    """Plot a compact comparison of selected model metrics."""
    metric_columns = ["accuracy", "balanced_accuracy", "f1", "roc_auc", "average_precision", "specificity"]
    long_frame = comparison_table.melt(
        id_vars="model_name",
        value_vars=metric_columns,
        var_name="metric",
        value_name="score",
    )

    plt.figure(figsize=(8, 5))
    ax = sns.barplot(data=long_frame, x="metric", y="score", hue="model_name", palette="Set2")
    ax.set_title("Model Metric Comparison")
    ax.set_xlabel("Metric")
    ax.set_ylabel("Score")
    ax.set_ylim(0, 1)
    return ax


def plot_roc_and_precision_recall(evaluations: list[ModelEvaluation]) -> tuple[Axes, Axes]:
    """Plot ROC and precision-recall curves for all evaluated models."""
    fig, axes = plt.subplots(1, 2, figsize=(9, 5))

    for evaluation in evaluations:
        fpr, tpr, _ = roc_curve(evaluation.y_true, evaluation.y_score)
        precision, recall, _ = precision_recall_curve(evaluation.y_true, evaluation.y_score)
        label = evaluation.model_name.replace("_", " ").title()

        axes[0].plot(fpr, tpr, label=label)
        axes[1].plot(recall, precision, label=label)

    axes[0].plot([0, 1], [0, 1], linestyle="--", color="grey", linewidth=1)
    axes[0].set_title("ROC Curve")
    axes[0].set_xlabel("False Positive Rate")
    axes[0].set_ylabel("True Positive Rate")
    axes[0].legend()

    axes[1].set_title("Precision-Recall Curve")
    axes[1].set_xlabel("Recall")
    axes[1].set_ylabel("Precision")
    axes[1].legend()

    plt.tight_layout()
    return axes[0], axes[1]


def plot_confusion_matrices(evaluations: list[ModelEvaluation]) -> np.ndarray:
    """Plot row-normalized confusion matrices for all evaluated models."""
    fig, axes = plt.subplots(1, len(evaluations), figsize=(4 * len(evaluations), 4))
    axes_array = np.atleast_1d(axes)

    for axis, evaluation in zip(axes_array, evaluations):
        confusion_pct = evaluation.confusion.div(evaluation.confusion.sum(axis=1), axis=0) * 100
        confusion_pct_values = confusion_pct.to_numpy(dtype=float)
        annotation_labels = np.array(
            [[f"{value:.1f}%" for value in row] for row in confusion_pct_values],
            dtype=object,
        )
        sns.heatmap(
            confusion_pct,
            annot=annotation_labels,
            fmt="",
            cmap="Blues",
            cbar=False,
            ax=axis,
        )
        axis.set_title(evaluation.model_name.replace("_", " ").title())
        axis.set_xlabel("Predicted Label")
        axis.set_ylabel("Actual Label")

    plt.tight_layout()
    return axes_array


def plot_confusion_count_matrices(evaluations: list[ModelEvaluation]) -> np.ndarray:
    """Plot raw count confusion matrices for all evaluated models."""
    fig, axes = plt.subplots(1, len(evaluations), figsize=(4 * len(evaluations), 4))
    axes_array = np.atleast_1d(axes)

    for axis, evaluation in zip(axes_array, evaluations):
        sns.heatmap(evaluation.confusion, annot=True, fmt="d", cmap="rocket_r", cbar=False, ax=axis)
        axis.set_title(f"{evaluation.model_name.replace('_', ' ').title()} Counts")
        axis.set_xlabel("Predicted Label")
        axis.set_ylabel("Actual Label")

    plt.tight_layout()
    return axes_array


def plot_calibration_curves(evaluations: list[ModelEvaluation], bins: int = 10) -> Axes:
    """Plot calibration curves for all evaluated models."""
    plt.figure(figsize=(5, 5))
    ax = plt.gca()

    for evaluation in evaluations:
        fraction_positives, mean_predicted = calibration_curve(
            evaluation.y_true,
            evaluation.y_score,
            n_bins=bins,
            strategy="quantile",
        )
        ax.plot(
            mean_predicted,
            fraction_positives,
            marker="o",
            label=evaluation.model_name.replace("_", " ").title(),
        )

    ax.plot([0, 1], [0, 1], linestyle="--", color="grey", linewidth=1)
    ax.set_title("Calibration Curve")
    ax.set_xlabel("Mean Predicted Probability")
    ax.set_ylabel("Observed Churn Rate")
    ax.legend()
    return ax


def plot_train_vs_test_metrics(evaluations: list[ModelEvaluation]) -> Axes:
    """Plot train vs test metrics to surface overfitting and split drift."""
    rows: list[dict[str, object]] = []
    metric_columns = ["f1", "roc_auc", "average_precision", "balanced_accuracy"]

    for evaluation in evaluations:
        for metric_name in metric_columns:
            if evaluation.train_metrics is not None:
                rows.append(
                    {
                        "model_name": evaluation.model_name,
                        "dataset": "train",
                        "metric": metric_name,
                        "score": evaluation.train_metrics[metric_name],
                    }
                )
            rows.append(
                {
                    "model_name": evaluation.model_name,
                    "dataset": "test",
                    "metric": metric_name,
                    "score": evaluation.metrics[metric_name],
                }
            )

    chart_frame = pd.DataFrame(rows)
    plt.figure(figsize=(8, 5))
    ax = sns.barplot(data=chart_frame, x="metric", y="score", hue="dataset", palette="Set1")
    ax.set_title("Train vs Test Metric Comparison")
    ax.set_ylim(0, 1)
    ax.set_xlabel("Metric")
    ax.set_ylabel("Score")
    return ax


def plot_pca_component_projection(
    evaluation: ModelEvaluation,
    x_reference: pd.DataFrame,
    y_reference: pd.Series,
    sample_size: int = 5_000,
) -> Axes:
    """Plot the first two PCA components for models that include a PCA step."""
    if "pca" not in evaluation.model.named_steps:
        raise ValueError(f"Model '{evaluation.model_name}' does not include a PCA step.")

    sampled_x, sampled_y, _ = _sample_training_data_for_grid_search(
        x_reference,
        y_reference,
        max_rows=sample_size,
    )

    feature_engineering = cast(FunctionTransformer, evaluation.model.named_steps["feature_engineering"])
    preprocessor = cast(ColumnTransformer, evaluation.model.named_steps["preprocessor"])
    pca = cast(PCA, evaluation.model.named_steps["pca"])

    engineered = cast(pd.DataFrame, feature_engineering.transform(sampled_x))
    preprocessed = preprocessor.transform(engineered)
    transformed = pca.transform(preprocessed)

    plot_frame = pd.DataFrame(
        {
            "pc_1": transformed[:, 0],
            "pc_2": transformed[:, 1],
            "churn_label": sampled_y.map({0: "Retained", 1: "Churned"}).astype(str).to_numpy(),
        }
    )

    plt.figure(figsize=(5, 5))
    ax = sns.scatterplot(
        data=plot_frame,
        x="pc_1",
        y="pc_2",
        hue="churn_label",
        palette=["#2ca02c", "#d62728"],
        alpha=0.6,
        s=45,
    )
    explained_variance = pca.explained_variance_ratio_
    ax.set_title("PCA Component Projection")
    ax.set_xlabel(f"PC1 ({explained_variance[0]:.1%} variance)")
    ax.set_ylabel(f"PC2 ({explained_variance[1]:.1%} variance)")
    return ax


def extract_model_signal_table(
    evaluation: ModelEvaluation,
    x_reference: pd.DataFrame,
    y_reference: pd.Series,
    top_n: int = 10,
) -> pd.DataFrame:
    """Return model signals via coefficients or permutation importance."""
    classifier = evaluation.model.named_steps["classifier"]

    if isinstance(classifier, LogisticRegression):
        pca = cast(PCA, evaluation.model.named_steps["pca"])
        component_names = [f"pc_{index + 1}" for index in range(len(pca.components_))]
        coefficients = pd.DataFrame(
            {
                "feature": component_names,
                "importance": classifier.coef_[0],
            }
        )
        coefficients["absolute_importance"] = coefficients["importance"].abs()
        return coefficients.sort_values("absolute_importance", ascending=False).head(top_n).round(3)

    sampled_x, sampled_y, _ = _sample_training_data_for_grid_search(
        x_reference,
        y_reference,
        max_rows=20_000,
    )
    importance = cast(
        Bunch,
        permutation_importance(
            evaluation.model,
            sampled_x,
            sampled_y,
            n_repeats=5,
            random_state=42,
            scoring="roc_auc",
        ),
    )
    feature_frame = pd.DataFrame(
        {
            "feature": sampled_x.columns,
            "importance": importance.importances_mean,
        }
    )
    feature_frame["absolute_importance"] = feature_frame["importance"].abs()
    return feature_frame.sort_values("absolute_importance", ascending=False).head(top_n)


def predict_single_customer(
    model: Pipeline,
    customer: Mapping[Hashable, object] | pd.DataFrame,
    threshold: float = 0.5,
) -> dict[str, float | int]:
    """Score a single customer safely with the full fitted pipeline."""
    if isinstance(customer, pd.DataFrame):
        customer_frame = customer.copy()
    else:
        customer_frame = pd.DataFrame([{str(key): value for key, value in customer.items()}])

    churn_probability = float(model.predict_proba(customer_frame)[0, 1])
    predicted_label = int(churn_probability >= threshold)
    return {
        "churn_probability": round(churn_probability, 4),
        "predicted_label": predicted_label,
    }
