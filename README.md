# Sample Work Portfolio

This repository is a curated portfolio of demo projects that I use to illustrate my work across data engineering, analytics, applied machine learning, MLOps, decision-support systems, and research computing.

Many of my production projects and consulting engagements cannot be shared publicly because the code, data, architecture, and delivery materials are proprietary. For that reason, the projects collected here rely on publicly shareable assets, synthetic data, dummy data, trimmed research artefacts, or representative implementation patterns. They are intended to demonstrate technical range and problem-solving approach rather than serve as full replicas of the real-world systems I have delivered.

## Portfolio Scope

```mermaid
flowchart LR
    A[Sample Work Portfolio] --> B[Data Engineering]
    A --> C[Analytics and BI]
    A --> D[Applied AI and ML]
    A --> E[MLOps]
    A --> F[Research and Decision Support]
```

## Projects

| Project | Focus Area | Key Tools | Summary |
| --- | --- | --- | --- |
| [Databricks - Lakehouse Data Engineering Pipeline (e-Commerce)](./Databricks%20-%20Lakehouse%20Data%20Engineering%20Pipeline%20(e-Commerce)) | Databricks, Delta Lake, Medallion Architecture | Databricks, PySpark, Spark SQL, Delta Lake, Unity Catalog | End-to-end e-commerce lakehouse pipeline that ingests CSV data, curates Bronze, Silver, and Gold tables, and prepares analytics-ready outputs. |
| [Databricks - Lakehouse Data Engineering Pipeline (Retail, Two Companies)](./Databricks%20-%20Lakehouse%20Data%20Engineering%20Pipeline%20(Retail,%20Two%20Companies)) | Databricks, Integration, Incremental Fact Processing | Databricks, PySpark, Spark SQL, Delta Lake, Databricks SQL | Multi-entity retail integration pipeline that conforms child-company data to a parent reporting model and produces a dashboard-ready serving layer. |
| [GenAI - Research Assistant (OpenAI, Langchain, Streamlit, FAISS)](./GenAI%20-%20Research%20Assistant%20(OpenAI,%20Langchain,%20Streamlit,%20FAISS)) | GenAI, RAG, Streamlit | Python, OpenAI, LangChain, Streamlit, FAISS | Streamlit research assistant that ingests article URLs, builds a FAISS vector store, and answers questions over extracted content. |
| [MLOps - Azure MLOps Pipeline (with CI-CD and Model Registry)](./MLOps%20-%20Azure%20MLOps%20Pipeline%20(with%20CI-CD%20and%20Model%20Registry)) | Azure ML, CI/CD, Model Governance | Azure ML, MLflow, GitHub Actions, Python, Spark | Parameterised Azure ML workflow for model training, evaluation, registration, deployment, and promotion through CI/CD. |
| [Research Publication Visualisations](./Research%20Publication%20Visualisations) | Research Communication | Markdown, publication graphics, scientific visualisation | Gallery of selected figures from peer-reviewed publications and posters. |
| [PowerBI - E-Commerce Sales Dashboard](./PowerBI%20-%20E-Commerce%20Sales%20Dashboard) | Power BI, Data Modelling | Power BI, R, data.table, star schema modelling | Power BI dashboard built on a synthetic e-commerce star schema with dimension and fact tables generated in R. |
| [Python - Remote Sensing and Image Processing (Google Earth Engine, Landsat, Sentinel-2)](./Python%20-%20Remote%20Sensing%20and%20Image%20Processing%20(Google%20Earth%20Engine,%20Landsat,%20Sentinel-2)) | Remote Sensing, Image Processing | Python, Google Earth Engine, Landsat, Sentinel-2, vegetation indices | Crop-monitoring extraction scripts used to derive vegetation indices from satellite imagery for multiple crops in Vietnam. |
| [R - Irrigation Forecast Modelling (APSIM, Weather Forecasting, ggplot2)](./R%20-%20Irrigation%20Forecast%20Modelling%20(APSIM,%20Weather%20Forecasting,%20ggplot2)) | Crop Modelling, Forecast Evaluation | R, APSIM, SILO data workflows, ggplot2 | Research workflow for evaluating how weather forecast reliability affects irrigation decisions and production outcomes at scale. |
| [R - Shiny Dashboard for Irrigation Management (Shiny, CSS, JavaScript)](./R%20-%20Shiny%20Dashboard%20for%20Irrigation%20Management%20(Shiny,%20CSS,%20JavaScript)) | Shiny, Decision Support | R, Shiny, CSS, JavaScript, modular dashboard design | R Shiny application for interactive decision support built on large simulation datasets. |
| [R - Time Series Analysis and Visualisation (Climate Extremes, ggplot2, SILO)](./R%20-%20Time%20Series%20Analysis%20and%20Visualisation%20(Climate%20Extremes,%20ggplot2,%20SILO)) | Climate Analytics, Time Series | R, time-series analysis, ggplot2, SILO, spatial trend analysis | Research analysis workflow studying long-term compound hot-dry weather extremes across Australia. |

## Repository Structure

Each project folder is self-contained and includes:

- `README.md` describing the project, structure, workflow, and available data or artefacts
- `LICENSE` containing the repository's proprietary usage notice
- `.gitignore` tuned to the tooling used in that project

## Data and Reuse Notes

- Some projects include dummy or synthetic data so the workflow can be inspected end to end.
- Some research projects include analysis scripts and output figures, but not the full original raw data pipeline.
- Where a repository snapshot only contains part of a larger real-world project, the corresponding project README states exactly what is present in this copy.
- Unless stated otherwise, the materials in this repository are provided as portfolio samples and not as reusable open-source project templates.
