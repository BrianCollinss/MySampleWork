# Restore the project library recorded in the root renv.lock.
# Run this once after opening the project on a new machine or any time the
# local library needs to be rebuilt from the lockfile.

# If you intentionally add, remove, or upgrade packages for this project,
# run renv::snapshot() afterwards to save those changes back to the root
# renv.lock file.

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org")
}

# Prefer prebuilt Windows binaries so restore works without requiring Rtools.
if (.Platform$OS.type == "windows") {
  options(pkgType = "binary", install.packages.compile.from.source = "never")
}

renv::restore(prompt = FALSE)
