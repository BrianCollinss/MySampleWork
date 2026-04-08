# Install the small set of R packages used by this project in RStudio.
required_packages <- c(
  "usethis", "here", "fs", "reticulate", "knitr", "rmarkdown", "jsonlite", "ggplot2"
)

# Use a fixed CRAN mirror so the script works non-interactively.
options(repos=c(CRAN="https://cloud.r-project.org"))

# Prefer prebuilt Windows binaries so setup works without requiring Rtools.
if (.Platform$OS.type == "windows") {
  options(pkgType="binary", install.packages.compile.from.source="never")
}

installed <- rownames(installed.packages())
missing <- setdiff(required_packages, installed)

if (length(missing) > 0) {
  # Install missing packages into the active R library.
  install.packages(missing)
}
