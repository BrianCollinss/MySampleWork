# Load the shared setup and helper functions used by this example analysis.
source(here::here("setup", "R", "setup_reticulate.R"))

# Point reticulate at the project conda environment before importing Python.
setup_reticulate()

if (!requireNamespace("reticulate", quietly=TRUE)) {
  stop("Package 'reticulate' is required.")
}

# Create output folders on demand so the script can run from a clean checkout.
processed_data_dir <- here::here("data", "processed")
tables_dir <- here::here("output", "tables")
fs::dir_create(processed_data_dir)
fs::dir_create(tables_dir)

# Import the Python helper module into the current R session.
reticulate::source_python(here::here("python", "helpers.py"))

values <- c(4, 8, 15, 16, 23, 42)
summary_result <- summarize_values(values)

# Convert the Python-style summary into a flat data frame for export.
summary_df <- data.frame(metric=names(summary_result), value=unlist(summary_result), row.names=NULL)

# Write the mixed-language result to a CSV artifact.
write.csv(summary_df, fs::path(tables_dir, "python_summary.csv"), row.names=FALSE)

print(summary_df)
