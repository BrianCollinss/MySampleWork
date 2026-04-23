"""Modelling utilities for opportunity scoring and monthly revenue forecasting."""

from __future__ import annotations

import pandas as pd
from sklearn.compose import ColumnTransformer
from sklearn.impute import SimpleImputer
from sklearn.linear_model import LinearRegression, LogisticRegression
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score, roc_auc_score
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder, StandardScaler


def train_win_probability_model(opportunities: pd.DataFrame) -> tuple[pd.DataFrame, dict[str, float]]:
    """Train a logistic-regression model that estimates opportunity win probability.

    The model is trained on historically closed opportunities only, using
    opportunities whose `deal_stage` is either `Won` or `Lost`. It learns the
    relationship between a closed opportunity outcome and a compact set of
    commercial features available in this project, including the sales agent,
    product, account, timing fields derived from the opportunity lifecycle, deal
    value, and opportunity age.

    The target being modelled is binary:
    - `1` when the closed opportunity was won
    - `0` when the closed opportunity was lost

    After fitting the model on resolved deals, the function applies the trained
    model across the full opportunity table so each opportunity receives a
    `predicted_win_probability` score. This score is then used downstream in the
    Gold layer for weighted-pipeline analysis and opportunity scoring outputs.

    Returns:
    - A copy of the opportunity DataFrame with `predicted_win_probability` added
    - A small metrics dictionary containing the in-sample ROC AUC used as a
      compact quality indicator for the fitted classifier
    """

    # Restrict the training data to historically resolved opportunities.
    model_df = opportunities.loc[opportunities["deal_stage"].isin(["Won", "Lost"])].copy()
    model_df["target_won"] = model_df["is_won"].astype(int)
    model_df["created_month"] = model_df["created_date"].dt.month
    model_df["close_month"] = model_df["close_date"].dt.month

    # Split the feature set into categorical and numeric groups for preprocessing.
    feature_cols = [
        "sales_agent",
        "product",
        "account",
        "created_month",
        "close_month",
        "close_value",
        "opportunity_age_days",
    ]
    categorical = ["sales_agent", "product", "account"]
    numeric = ["created_month", "close_month", "close_value", "opportunity_age_days"]

    # Build a preprocessing pipeline that imputes and encodes model inputs.
    preprocessor = ColumnTransformer(
        transformers=[
            (
                "cat",
                Pipeline(
                    [("imputer", SimpleImputer(strategy="most_frequent")), ("oh", OneHotEncoder(handle_unknown="ignore"))]
                ),
                categorical,
            ),
            (
                "num",
                Pipeline([("imputer", SimpleImputer(strategy="median")), ("scaler", StandardScaler())]),
                numeric,
            ),
        ]
    )

    # Fit the classification model and score the resolved training sample.
    clf = Pipeline(
        steps=[
            ("prep", preprocessor),
            ("model", LogisticRegression(max_iter=1000)),
        ]
    )
    clf.fit(model_df[feature_cols], model_df["target_won"])
    model_df["predicted_win_probability"] = clf.predict_proba(model_df[feature_cols])[:, 1]
    auc = roc_auc_score(model_df["target_won"], model_df["predicted_win_probability"])

    # Apply the trained model across the full opportunity table for downstream Gold outputs.
    score_df = opportunities.copy()
    score_df["created_month"] = score_df["created_date"].dt.month
    score_df["close_month"] = score_df["close_date"].dt.month.fillna(score_df["created_month"])
    score_df["predicted_win_probability"] = clf.predict_proba(score_df[feature_cols])[:, 1]

    return score_df, {"roc_auc": float(auc)}


def forecast_monthly_revenue(monthly_revenue: pd.DataFrame, horizon: int) -> tuple[pd.DataFrame, dict[str, float]]:
    """Forecast future monthly closed-won revenue from historical monthly totals.

    This function models the `closed_won_revenue` series at monthly grain. It
    expects an input DataFrame containing at least:
    - `date`: the first day of each reporting month
    - `closed_won_revenue`: realised revenue for that month

    The forecasting approach is intentionally lightweight and portfolio-oriented.
    It fits a linear-regression model using:
    - a sequential time index to capture broad trend over time
    - one-hot encoded month-of-year indicators to capture simple seasonality

    The function then projects revenue forward for the requested number of future
    months and returns a combined output containing:
    - historical months labelled as `actual`
    - forecast months labelled as `forecast`

    This combined structure is designed to make Power BI and notebook charting
    straightforward, since actuals and projections can be plotted as one time
    series with a simple filter on `forecast_type`.

    Args:
    - `monthly_revenue`: monthly historical revenue table used to fit the model
    - `horizon`: number of future months to forecast

    Returns:
    - A DataFrame with monthly dates, forecasted revenue values, and a
      `forecast_type` column indicating whether each row is an actual or a
      projected period
    - A metrics dictionary summarising in-sample forecast performance for the
      fitted regression model
    """

    # Prepare a compact feature set capturing time progression and seasonality.
    train = monthly_revenue.copy().sort_values("date")
    train["period_index"] = range(len(train))
    train["month_num"] = train["date"].dt.month

    # Fit a linear model on one-hot encoded calendar month effects.
    X = pd.get_dummies(train[["period_index", "month_num"]].astype(int), columns=["month_num"], drop_first=False)
    y = train["closed_won_revenue"]
    reg = LinearRegression()
    reg.fit(X, y)
    train["fitted_revenue"] = reg.predict(X).clip(min=0)

    # Calculate in-sample evaluation metrics for export to the reporting layer.
    non_zero_actuals = train.loc[train["closed_won_revenue"] != 0].copy()
    metrics = {
        "train_months": int(len(train)),
        "forecast_horizon_months": int(horizon),
        "mae": float(mean_absolute_error(train["closed_won_revenue"], train["fitted_revenue"])),
        "rmse": float(mean_squared_error(train["closed_won_revenue"], train["fitted_revenue"]) ** 0.5),
        "r2": float(r2_score(train["closed_won_revenue"], train["fitted_revenue"])),
    }

    # Create the future forecast horizon and align dummy columns with the training matrix.
    future_dates = pd.date_range(train["date"].max() + pd.offsets.MonthBegin(1), periods=horizon, freq="MS")
    future = pd.DataFrame({"date": future_dates})
    future["period_index"] = range(len(train), len(train) + len(future))
    future["month_num"] = future["date"].dt.month
    future_X = pd.get_dummies(future[["period_index", "month_num"]].astype(int), columns=["month_num"], drop_first=False)
    future_X = future_X.reindex(columns=X.columns, fill_value=0)
    future["forecast_revenue"] = reg.predict(future_X).clip(min=0)
    future["forecast_type"] = "forecast"

    # Append actual history so BI tools can render one continuous actual-versus-forecast series.
    history = train[["date", "closed_won_revenue"]].rename(columns={"closed_won_revenue": "forecast_revenue"})
    history["forecast_type"] = "actual"
    return pd.concat([history, future], ignore_index=True), metrics
