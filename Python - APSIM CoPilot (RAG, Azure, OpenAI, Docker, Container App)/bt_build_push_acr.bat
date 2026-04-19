@echo off
setlocal
set "EXIT_CODE=0"
set "RESOLVED_IMAGE_TAG="
set "LAST_IMAGE_TAG_FILE=.last_image_tag"

REM APSIM Copilot - build and push Docker image to Azure Container Registry
REM
REM Usage:
REM   1. Set deployment values in .env
REM   2. Run this file from the project root
REM
REM Prerequisites:
REM   - Docker Desktop installed and running
REM   - Azure CLI installed
REM   - You have access to the Azure subscription and ACR

REM -----------------------------------------------------------------
REM Optional fallback values.
REM The script first loads .env from the project root. If any of these
REM are still empty after that, the fallback values below are used.
REM -----------------------------------------------------------------
set "AZ_SUBSCRIPTION="
set "ACR_NAME="
set "IMAGE_NAME=apsim-copilot"
set "IMAGE_TAG=latest"
set "BUILD_PLATFORM=linux/amd64"

REM Optional: set to a specific Dockerfile path if you move it later.
set "DOCKERFILE_PATH=Dockerfile"

if exist ".env" (
    echo Loading deployment values from .env...
    for /f "usebackq tokens=1* delims==" %%A in (".env") do (
        if /i "%%~A"=="AZ_SUBSCRIPTION" set "AZ_SUBSCRIPTION=%%~B"
        if /i "%%~A"=="ACR_NAME" set "ACR_NAME=%%~B"
        if /i "%%~A"=="IMAGE_NAME" set "IMAGE_NAME=%%~B"
        if /i "%%~A"=="IMAGE_TAG" set "IMAGE_TAG=%%~B"
        if /i "%%~A"=="BUILD_PLATFORM" set "BUILD_PLATFORM=%%~B"
        if /i "%%~A"=="DOCKERFILE_PATH" set "DOCKERFILE_PATH=%%~B"
    )
)

set "IMAGE_URI=%ACR_NAME%.azurecr.io/%IMAGE_NAME%:%IMAGE_TAG%"

if /i "%IMAGE_TAG%"=="USEDATE" (
    for /f %%I in ('powershell -NoProfile -Command "Get-Date -Format yyyyMMdd-HHmmss"') do set "RESOLVED_IMAGE_TAG=%%I"
) else (
    set "RESOLVED_IMAGE_TAG=%IMAGE_TAG%"
)

set "IMAGE_URI=%ACR_NAME%.azurecr.io/%IMAGE_NAME%:%RESOLVED_IMAGE_TAG%"

echo.
echo ============================================================
echo APSIM Copilot Docker build + push
echo ============================================================
echo Subscription: %AZ_SUBSCRIPTION%
echo Registry:     %ACR_NAME%
echo Image:        %IMAGE_URI%
echo Platform:     %BUILD_PLATFORM%
echo Dockerfile:   %DOCKERFILE_PATH%
echo ============================================================
echo.

if "%AZ_SUBSCRIPTION%"=="" (
    echo ERROR: AZ_SUBSCRIPTION is missing. Set it in .env or this script.
    set "EXIT_CODE=1"
    goto :finish
)

if "%ACR_NAME%"=="" (
    echo ERROR: ACR_NAME is missing. Set it in .env or this script.
    set "EXIT_CODE=1"
    goto :finish
)

if "%IMAGE_NAME%"=="" (
    echo ERROR: Please set IMAGE_NAME in this script.
    set "EXIT_CODE=1"
    goto :finish
)

if "%IMAGE_TAG%"=="" (
    echo ERROR: Please set IMAGE_TAG in this script.
    set "EXIT_CODE=1"
    goto :finish
)

if "%BUILD_PLATFORM%"=="" (
    echo ERROR: Please set BUILD_PLATFORM in this script.
    set "EXIT_CODE=1"
    goto :finish
)

if not exist "%DOCKERFILE_PATH%" (
    echo ERROR: Dockerfile not found at %DOCKERFILE_PATH%.
    set "EXIT_CODE=1"
    goto :finish
)

echo [0/5] Checking Docker...
docker version >nul 2>&1
if errorlevel 1 (
    echo ERROR: Docker CLI is not available.
    echo Make sure Docker Desktop is installed and that the docker command is on PATH.
    set "EXIT_CODE=1"
    goto :finish
)

docker info >nul 2>&1
if errorlevel 1 (
    echo ERROR: Docker Desktop is not running or the Docker daemon is unavailable.
    echo Start Docker Desktop, wait until it shows as running, and then rerun this script.
    set "EXIT_CODE=1"
    goto :finish
)

echo [1/5] Logging into Azure...
call az login
if errorlevel 1 (
    echo ERROR: Azure login failed.
    set "EXIT_CODE=1"
    goto :finish
)

echo [2/5] Setting Azure subscription...
call az account set --subscription "%AZ_SUBSCRIPTION%"
if errorlevel 1 (
    echo ERROR: Could not set Azure subscription.
    set "EXIT_CODE=1"
    goto :finish
)

echo [3/5] Logging into Azure Container Registry...
call az acr login --name "%ACR_NAME%"
if errorlevel 1 (
    echo ERROR: Azure Container Registry login failed.
    set "EXIT_CODE=1"
    goto :finish
)

echo [4/5] Building Docker image...
docker buildx version >nul 2>&1
if errorlevel 1 (
    echo ERROR: Docker Buildx is not available.
    echo Update Docker Desktop and ensure Buildx support is enabled.
    set "EXIT_CODE=1"
    goto :finish
)

echo [4/5] Building and pushing Docker image...
docker buildx build --platform "%BUILD_PLATFORM%" --provenance=false -f "%DOCKERFILE_PATH%" -t "%IMAGE_URI%" --push .
if errorlevel 1 (
    echo ERROR: Docker build/push failed.
    set "EXIT_CODE=1"
    goto :finish
)

> "%LAST_IMAGE_TAG_FILE%" echo %RESOLVED_IMAGE_TAG%

echo [5/5] Image published to registry.

echo.
echo SUCCESS: Image pushed to %IMAGE_URI%
echo Resolved image tag: %RESOLVED_IMAGE_TAG%
echo You can now use this image in Azure Container Apps.
echo.

:finish
echo Press any key to close this window...
pause >nul

endlocal
exit /b %EXIT_CODE%
