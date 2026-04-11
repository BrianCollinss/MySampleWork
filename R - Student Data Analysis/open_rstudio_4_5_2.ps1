$ErrorActionPreference = "Stop"

# Open this project in RStudio while preferring the project's R 4.5.2 installation.
$projectFile = Join-Path $PSScriptRoot "Student Data Analysis.Rproj"
$rExecutable = "C:\Program Files\R\R-4.5.2\bin\x64\R.exe"
$rstudioCandidates = @(
  "C:\Program Files\RStudio\rstudio.exe",
  "C:\Program Files\Posit\rstudio\rstudio.exe"
)

if (-not (Test-Path $projectFile)) {
  throw "The RStudio project file was not found at '$projectFile'."
}

if (-not (Test-Path $rExecutable)) {
  throw "R 4.5.2 was not found at '$rExecutable'."
}

$rstudio = $rstudioCandidates | Where-Object { Test-Path $_ } | Select-Object -First 1

if ($null -eq $rstudio) {
  throw "RStudio was not found in the standard install locations."
}

# Posit documents RSTUDIO_WHICH_R for macOS and Linux; on Windows this is a
# best-effort launcher to keep the override local to this project session.
$env:RSTUDIO_WHICH_R = $rExecutable

Write-Host "Launching RStudio with:"
Write-Host "  Project: $projectFile"
Write-Host "  R:       $rExecutable"
Write-Host "  IDE:     $rstudio"

Start-Process -FilePath $rstudio -ArgumentList "`"$projectFile`""
