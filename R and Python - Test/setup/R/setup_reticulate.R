# Configure reticulate to use the project's conda environment when available.
setup_reticulate <- function(condaenv="r-reticulate", conda="auto") {
  if (!requireNamespace("reticulate", quietly=TRUE)) {
    stop("Package 'reticulate' is required. Install it with install.packages('reticulate').")
  }

  # Query conda defensively so notebooks can still start even if conda
  # is unavailable on PATH for the current shell.
  conda_envs <- tryCatch(reticulate::conda_list(conda=conda), error=function(e) NULL)

  if (!is.null(conda_envs) && condaenv %in% conda_envs$name) {
    # Lock reticulate to the expected environment for reproducible imports.
    reticulate::use_condaenv(condaenv, conda=conda, required=TRUE)
  } else {
    # Fall back to reticulate's normal discovery so users still get a usable session.
    message("Conda environment '", condaenv, "' was not found. Falling back to reticulate default discovery.")
  }

  invisible(TRUE)
}
