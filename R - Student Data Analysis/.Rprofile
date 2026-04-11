# Load the project-local renv library when renv is available.
if (requireNamespace("renv", quietly = TRUE)) {
  renv::load(project = getwd(), quiet = TRUE)
}

# Keep package installs on Windows simple and avoid source compilation.
if (.Platform$OS.type == "windows") {
  options(pkgType = "binary", install.packages.compile.from.source = "never")
}

# Give interactive users a quick hint about the intended renv workflow.
if (interactive()) {
  package_startup_message <- paste(
    "renv setup:",
    "- first open on a machine: source('setup/R/setup_r_environment.R') to restore packages from the root renv.lock",
    "- after intentional package changes: run renv::snapshot() to update that root renv.lock",
    sep = "\n"
  )
  packageStartupMessage(package_startup_message)
}
