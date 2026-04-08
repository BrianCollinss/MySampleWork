@echo off
setlocal EnableDelayedExpansion

REM One-time helper for creating or updating the AWS CLI profile used by this project.
REM Use SSO mode if your organization uses AWS IAM Identity Center.
REM Use key mode if you authenticate with access keys instead.

set "PROFILE_NAME=bc-snowflake-training-0001"

where aws >nul 2>nul
if errorlevel 1 (
  echo AWS CLI was not found on PATH.
  echo.
  set /p INSTALL_AWS_CLI="Install AWS CLI v2 now using the official MSI? [Y/N]: "
  if /I "!INSTALL_AWS_CLI!"=="Y" (
    echo Installing AWS CLI v2...
    msiexec.exe /i https://awscli.amazonaws.com/AWSCLIV2.msi
    echo.
    echo After the installer finishes, close and reopen this terminal, then rerun:
    echo   %~nx0 %~1
    exit /b 0
  )
  echo.
  echo Manual install command:
  echo   msiexec.exe /i https://awscli.amazonaws.com/AWSCLIV2.msi
  echo After installation, reopen the terminal and confirm:
  echo   aws --version
  exit /b 1
)

if /I "%~1"=="sso" (
  echo Configuring AWS SSO profile "%PROFILE_NAME%"...
  aws configure sso --profile %PROFILE_NAME%
  if errorlevel 1 exit /b 1
  echo.
  echo Signing in with AWS SSO profile "%PROFILE_NAME%"...
  aws sso login --profile %PROFILE_NAME%
  exit /b %errorlevel%
)

if /I "%~1"=="keys" (
  echo Configuring AWS access-key profile "%PROFILE_NAME%"...
  aws configure --profile %PROFILE_NAME%
  exit /b %errorlevel%
)

echo Usage:
echo   %~nx0 sso   ^(recommended for AWS SSO / IAM Identity Center^)
echo   %~nx0 keys  ^(for AWS access key authentication^)
exit /b 1
