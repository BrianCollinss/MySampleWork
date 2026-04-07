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
- Data pipeline in `scripts/run_data_pipeline.py`
- Reusable Python helpers in `src/customer_churn_analysis/`
- Jupyter notebook for analysis in `notebooks/01_customer_churn_analysis.ipynb`
- Output-ready figures in `outputs/` and auto-generated summary tables in this report

## Summary Tables

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
<!-- AUTO-GENERATED TABLES END -->
