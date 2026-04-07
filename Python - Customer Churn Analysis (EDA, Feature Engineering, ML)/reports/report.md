# Executive Summary

## Overview

This project analyses a customer churn dataset provided as labelled training and testing CSV files. The workflow converts both files into parquet format, standardises the schema, removes the single blank training record, and produces reusable analysis assets suitable for a portfolio or stakeholder demo.

## Initial Findings

- The training split contains 440,832 usable records after removing one blank row.
- The test split contains 64,374 labelled records.
- Churn prevalence differs materially by split, with the training set near 56.7% and the test set near 47.4%.
- The split difference suggests the project should explicitly compare train and test behaviour rather than assuming both partitions come from identical distributions.

## Business Interpretation

The dataset structure supports a retention-focused analysis that links churn to customer tenure, service usage, support demand, payment delay, spend, and contract configuration. In practice, the most useful stakeholder questions are:

- Which customer segments show the highest churn risk?
- Are there signs of train/test distribution shift that could affect model or reporting reliability?
- Which behavioural and commercial features move most clearly with churn outcomes?

## Deliverables

- Conda environment specification in `environment.yml`
- Reusable notebook workflow in `notebooks/`
- Reusable Python helpers in `src/customer_churn_analysis/`
- Jupyter notebooks for both analysis and modelling
- Output-ready figures in `outputs/` and auto-generated summary tables in this report

## Summary Tables

_Updated from `01_customer_churn_analysis.ipynb`: 2026-04-07 21:31 E. Australia Standard Time_

<!-- AUTO-GENERATED TABLES START -->
### Data Quality Summary

| dataset | rows | columns | duplicate_rows | total_missing_values | churn_rate |
| --- | --- | --- | --- | --- | --- |
| combined | 505207 | 13 | 0 | 12 | 0.56 |
| test | 64374 | 13 | 0 | 0 | 0.47 |
| train | 440833 | 13 | 0 | 12 | 0.57 |

### Split Comparison Summary

| source_split | age | tenure_months | usage_frequency | support_calls | payment_delay_days | total_spend | last_interaction_days |
| --- | --- | --- | --- | --- | --- | --- | --- |
| test | 41.97 | 31.99 | 15.08 | 5.40 | 17.13 | 541.02 | 15.50 |
| train | 39.37 | 31.26 | 15.81 | 3.60 | 12.97 | 631.62 | 14.48 |

### Churn Summary By Split

| source_split | customers | churn_rate |
| --- | --- | --- |
| test | 64374 | 0.47 |
| train | 440832 | 0.57 |

### Model Evaluation Summary

- Logistic regression currently provides the strongest thresholded test-set performance, with the best accuracy, F1, and calibration among the compared models.
- Best accuracy: `logistic regression` (0.637)
- Best ROC AUC: `logistic regression` (0.659)
- Best average precision: `logistic regression` (0.555)
- Best Brier score: `logistic regression` (0.386)
- The tree-based models rank customers more effectively overall, but on the current test split they classify almost everyone as churned at their selected thresholds, which hurts specificity and overall accuracy.
- This reinforces the distribution-shift finding from the exploratory analysis: strong cross-validation scores inside the training split do not fully carry over to the held-out test set.
- See the model evaluation section below for the full comparison, thresholds, confusion matrices, and saved model paths.
<!-- AUTO-GENERATED TABLES END -->

## Detailed Analysis

_Updated from `02_customer_churn_modeling.ipynb`: 2026-04-07 23:16 E. Australia Standard Time_

### Data Analysis

The exploratory analysis shows a clear train/test shift. The training split is larger, has a higher churn rate, and differs materially from the test split in age, usage frequency, support demand, payment delays, and spend. That means descriptive findings are still useful for segmentation, but model scores should be interpreted cautiously because the held-out split does not behave like an identically distributed sample from the training data.

Customer tenure, spend, contract structure, and service friction remain the most decision-relevant lenses for retention work. In practical terms, the data supports targeting customers with shorter or more flexible contracts, high support demand, and slower payment behaviour, while also checking whether those patterns stay stable across future cohorts.

## Model Evaluation

### Overview

This report compares grid-searched logistic regression and random forest models trained on the provided training split and evaluated on the provided test split.

The current version uses stronger model regularization, F1-based threshold selection, and covariate-shift weighting so the training fit places more emphasis on records that look similar to the held-out test distribution.

### Model Comparison

|  | accuracy | balanced_accuracy | precision | recall | specificity | f1 | roc_auc | average_precision | brier_score |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| logistic_regression | 0.636 | 0.651 | 0.570 | 0.937 | 0.365 | 0.709 | 0.696 | 0.649 | 0.387 |
| random_forest | 0.506 | 0.531 | 0.490 | 0.998 | 0.063 | 0.657 | 0.532 | 0.490 | 0.494 |

## Logistic Regression

### Grid Search

- Best cross-validation ROC AUC: 0.960
- Best parameters: `{'classifier__C': 0.8, 'classifier__class_weight': None}`
- Selected threshold: 0.900 (chosen by validation specificity with recall floor)

### Metrics

- Accuracy: 0.636
- Balanced Accuracy: 0.651
- Precision: 0.570
- Recall: 0.937
- Specificity: 0.365
- F1: 0.709
- Roc Auc: 0.696
- Average Precision: 0.649
- Brier Score: 0.387

#### Train vs Test

- Train ROC AUC: 0.960
- Train F1: 0.852
- Test ROC AUC: 0.696
- Test F1: 0.709

#### Confusion Matrix

|  | Predicted Retained | Predicted Churned |
| --- | --- | --- |
| Actual Retained | 12352 | 21529 |
| Actual Churned | 1909 | 28584 |

#### Classification Report

```text
              precision    recall  f1-score   support

           0      0.866     0.365     0.513     33881
           1      0.570     0.937     0.709     30493

    accuracy                          0.636     64374
   macro avg      0.718     0.651     0.611     64374
weighted avg      0.726     0.636     0.606     64374
```

#### Artifact

- Saved model: `outputs/models/churn_logistic_regression.joblib`

## Random Forest

### Grid Search

- Best cross-validation ROC AUC: 1.000
- Best parameters: `{'classifier__max_depth': 10, 'classifier__max_features': None, 'classifier__min_samples_leaf': 50, 'classifier__min_samples_split': 2, 'classifier__n_estimators': 100}`
- Selected threshold: 0.650 (chosen by validation specificity with recall floor)

### Metrics

- Accuracy: 0.506
- Balanced Accuracy: 0.531
- Precision: 0.490
- Recall: 0.998
- Specificity: 0.063
- F1: 0.657
- Roc Auc: 0.532
- Average Precision: 0.490
- Brier Score: 0.494

#### Train vs Test

- Train ROC AUC: 0.999
- Train F1: 0.997
- Test ROC AUC: 0.532
- Test F1: 0.657

#### Confusion Matrix

|  | Predicted Retained | Predicted Churned |
| --- | --- | --- |
| Actual Retained | 2148 | 31733 |
| Actual Churned | 51 | 30442 |

#### Classification Report

```text
              precision    recall  f1-score   support

           0      0.977     0.063     0.119     33881
           1      0.490     0.998     0.657     30493

    accuracy                          0.506     64374
   macro avg      0.733     0.531     0.388     64374
weighted avg      0.746     0.506     0.374     64374
```

#### Artifact

- Saved model: `outputs/models/churn_random_forest.joblib`
