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
- Output-ready figures and summary tables in `outputs/`

