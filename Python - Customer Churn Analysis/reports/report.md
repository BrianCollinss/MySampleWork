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

_Updated from `01_customer_churn_analysis.ipynb`: 2026-04-07 18:55 E. Australia Standard Time_

<!-- AUTO-GENERATED TABLES START -->
### Data Quality Summary

| dataset | rows | columns | duplicate_rows | total_missing_values | churn_rate |
| --- | --- | --- | --- | --- | --- |
| combined | 505207 | 14 | 0 | 13 | 0.56 |
| test | 64374 | 14 | 0 | 0 | 0.47 |
| train | 440833 | 14 | 0 | 13 | 0.57 |

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
- Best accuracy: `logistic regression` (0.585)
- Best ROC AUC: `hist gradient boosting` (0.729)
- Best average precision: `hist gradient boosting` (0.625)
- Best Brier score: `logistic regression` (0.384)
- The tree-based models rank customers more effectively overall, but on the current test split they classify almost everyone as churned at their selected thresholds, which hurts specificity and overall accuracy.
- This reinforces the distribution-shift finding from the exploratory analysis: strong cross-validation scores inside the training split do not fully carry over to the held-out test set.
- See the model evaluation section below for the full comparison, thresholds, confusion matrices, and saved model paths.
<!-- AUTO-GENERATED TABLES END -->















## Detailed Analysis

### Data Analysis

The exploratory analysis shows a clear train/test shift. The training split is larger, has a higher churn rate, and differs materially from the test split in age, usage frequency, support demand, payment delays, and spend. That means descriptive findings are still useful for segmentation, but model scores should be interpreted cautiously because the held-out split does not behave like an identically distributed sample from the training data.

Customer tenure, spend, contract structure, and service friction remain the most decision-relevant lenses for retention work. In practical terms, the data supports targeting customers with shorter or more flexible contracts, high support demand, and slower payment behaviour, while also checking whether those patterns stay stable across future cohorts.

## Model Evaluation

### Overview

This report compares grid-searched logistic regression, random forest, and histogram-based gradient boosting models trained on the provided training split and evaluated on the provided test split.

### Model Comparison

|  | accuracy | balanced_accuracy | precision | recall | specificity | f1 | roc_auc | average_precision | brier_score |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| hist_gradient_boosting | 0.5030 | 0.5280 | 0.4880 | 0.9990 | 0.0570 | 0.6560 | 0.7290 | 0.6250 | 0.4960 |
| logistic_regression | 0.5850 | 0.6050 | 0.5340 | 0.9850 | 0.2260 | 0.6920 | 0.6530 | 0.5520 | 0.3840 |
| random_forest | 0.5040 | 0.5280 | 0.4880 | 0.9990 | 0.0580 | 0.6560 | 0.6500 | 0.5670 | 0.4820 |

## Logistic Regression

### Grid Search

- Best cross-validation ROC AUC: 0.9603
- Best parameters: `{'classifier__C': 2.0, 'classifier__class_weight': None}`
- Selected threshold: 0.600

### Metrics

- Accuracy: 0.5854
- Balanced Accuracy: 0.6054
- Precision: 0.5338
- Recall: 0.9849
- Specificity: 0.2259
- F1: 0.6924
- Roc Auc: 0.6531
- Average Precision: 0.5516
- Brier Score: 0.3841

#### Train vs Test

- Train ROC AUC: 0.9600
- Train F1: 0.9044
- Test ROC AUC: 0.6531
- Test F1: 0.6924

#### Confusion Matrix

|  | Predicted Retained | Predicted Churned |
| --- | --- | --- |
| Actual Retained | 7653 | 26228 |
| Actual Churned | 461 | 30032 |

#### Classification Report

```text
              precision    recall  f1-score   support

           0      0.943     0.226     0.364     33881
           1      0.534     0.985     0.692     30493

    accuracy                          0.585     64374
   macro avg      0.738     0.605     0.528     64374
weighted avg      0.749     0.585     0.520     64374
```

#### Artifact

- Saved model: `outputs/models/churn_logistic_regression.joblib`

## Random Forest

### Grid Search

- Best cross-validation ROC AUC: 0.9999
- Best parameters: `{'classifier__max_depth': 14, 'classifier__min_samples_leaf': 10, 'classifier__n_estimators': 300}`
- Selected threshold: 0.300

### Metrics

- Accuracy: 0.5037
- Balanced Accuracy: 0.5284
- Precision: 0.4883
- Recall: 0.9986
- Specificity: 0.0582
- F1: 0.6559
- Roc Auc: 0.6503
- Average Precision: 0.5667
- Brier Score: 0.4822

#### Train vs Test

- Train ROC AUC: 1.0000
- Train F1: 0.9996
- Test ROC AUC: 0.6503
- Test F1: 0.6559

#### Confusion Matrix

|  | Predicted Retained | Predicted Churned |
| --- | --- | --- |
| Actual Retained | 1972 | 31909 |
| Actual Churned | 43 | 30450 |

#### Classification Report

```text
              precision    recall  f1-score   support

           0      0.979     0.058     0.110     33881
           1      0.488     0.999     0.656     30493

    accuracy                          0.504     64374
   macro avg      0.733     0.528     0.383     64374
weighted avg      0.746     0.504     0.369     64374
```

#### Artifact

- Saved model: `outputs/models/churn_random_forest.joblib`

## Hist Gradient Boosting

### Grid Search

- Best cross-validation ROC AUC: 1.0000
- Best parameters: `{'classifier__learning_rate': 0.05, 'classifier__max_depth': 8, 'classifier__min_samples_leaf': 60}`
- Selected threshold: 0.550

### Metrics

- Accuracy: 0.5032
- Balanced Accuracy: 0.5280
- Precision: 0.4881
- Recall: 0.9987
- Specificity: 0.0573
- F1: 0.6557
- Roc Auc: 0.7290
- Average Precision: 0.6247
- Brier Score: 0.4964

#### Train vs Test

- Train ROC AUC: 1.0000
- Train F1: 1.0000
- Test ROC AUC: 0.7290
- Test F1: 0.6557

#### Confusion Matrix

|  | Predicted Retained | Predicted Churned |
| --- | --- | --- |
| Actual Retained | 1943 | 31938 |
| Actual Churned | 40 | 30453 |

#### Classification Report

```text
              precision    recall  f1-score   support

           0      0.980     0.057     0.108     33881
           1      0.488     0.999     0.656     30493

    accuracy                          0.503     64374
   macro avg      0.734     0.528     0.382     64374
weighted avg      0.747     0.503     0.368     64374
```

#### Artifact

- Saved model: `outputs/models/churn_hist_gradient_boosting.joblib`
