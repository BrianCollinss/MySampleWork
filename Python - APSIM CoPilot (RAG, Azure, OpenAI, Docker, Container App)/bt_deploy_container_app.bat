@echo off
setlocal EnableDelayedExpansion
set "EXIT_CODE=0"
set "RESOLVED_IMAGE_TAG="
set "LAST_IMAGE_TAG_FILE=.last_image_tag"

REM APSIM Copilot - deploy/update Azure Container App and configure runtime settings
REM
REM Usage:
REM   1. Set deployment values in .env
REM   2. Run this file from the project root
REM
REM Modes:
REM   - Preferred: Key Vault mode, when AZURE_KEY_VAULT_URL is set
REM   - Fallback: direct Container Apps secrets, when direct secret values are set

set "AZ_SUBSCRIPTION="
set "AZURE_RESOURCE_GROUP="
set "AZURE_LOCATION=australiaeast"
set "ACR_NAME="
set "IMAGE_NAME=apsim-copilot"
set "IMAGE_TAG=latest"
set "ACR_USERNAME="
set "ACR_PASSWORD="
set "ACR_PULL_IDENTITY_RESOURCE_ID="
set "ACR_PULL_IDENTITY_NAME="
set "ACR_PULL_IDENTITY_RESOURCE_GROUP="

set "CONTAINER_APP_NAME="
set "CONTAINER_APP_RESOURCE_GROUP="
set "CONTAINER_APP_ENVIRONMENT="
set "LOG_ANALYTICS_WORKSPACE_NAME=log-apsimcopilot-dev"
set "CONTAINER_APP_INGRESS=external"
set "CONTAINER_APP_TARGET_PORT=8501"
set "CONTAINER_APP_CPU=0.5"
set "CONTAINER_APP_MEMORY=1.0Gi"

set "APP_NAME=APSIM Copilot"
set "LOG_LEVEL=WARNING"
set "STREAMLIT_SERVER_PORT=8501"

set "AZURE_KEY_VAULT_URL="
set "OPENAI_API_KEY_SECRET_NAME="
set "AZURE_SEARCH_API_KEY_SECRET_NAME="
set "AZURE_STORAGE_CONNECTION_STRING_SECRET_NAME="
set "APPLICATIONINSIGHTS_CONNECTION_STRING_SECRET_NAME="

set "OPENAI_API_KEY="
set "OPENAI_CHAT_MODEL=gpt-5.4-mini"
set "OPENAI_EMBEDDING_MODEL=text-embedding-3-small"

set "AZURE_SEARCH_ENDPOINT="
set "AZURE_SEARCH_API_KEY="
set "AZURE_SEARCH_INDEX_NAME=apsim-docs"
set "AZURE_SEARCH_VECTOR_DIMENSIONS=1536"
set "AZURE_SEARCH_SEMANTIC_CONFIG=default"

set "AZURE_STORAGE_CONNECTION_STRING="
set "AZURE_STORAGE_CONTAINER_NAME=apsim-copilot"
set "SAVE_UPLOADS_TO_BLOB=false"

set "APPLICATIONINSIGHTS_CONNECTION_STRING="

set "MAX_SEARCH_RESULTS=5"
set "MAX_CONTEXT_CHARACTERS=12000"
set "CSV_PREVIEW_ROWS=25"
set "LOG_ANALYTICS_WORKSPACE_ID="
set "LOG_ANALYTICS_WORKSPACE_KEY="

if exist ".env" (
    echo Loading deployment values from .env...
    for /f "usebackq tokens=1* delims==" %%A in (".env") do (
        if /i "%%~A"=="AZ_SUBSCRIPTION" set "AZ_SUBSCRIPTION=%%~B"
        if /i "%%~A"=="AZURE_RESOURCE_GROUP" set "AZURE_RESOURCE_GROUP=%%~B"
        if /i "%%~A"=="AZURE_LOCATION" set "AZURE_LOCATION=%%~B"
        if /i "%%~A"=="ACR_NAME" set "ACR_NAME=%%~B"
        if /i "%%~A"=="IMAGE_NAME" set "IMAGE_NAME=%%~B"
        if /i "%%~A"=="IMAGE_TAG" set "IMAGE_TAG=%%~B"
        if /i "%%~A"=="ACR_USERNAME" set "ACR_USERNAME=%%~B"
        if /i "%%~A"=="ACR_PASSWORD" set "ACR_PASSWORD=%%~B"
        if /i "%%~A"=="ACR_PULL_IDENTITY_RESOURCE_ID" set "ACR_PULL_IDENTITY_RESOURCE_ID=%%~B"
        if /i "%%~A"=="ACR_PULL_IDENTITY_NAME" set "ACR_PULL_IDENTITY_NAME=%%~B"
        if /i "%%~A"=="ACR_PULL_IDENTITY_RESOURCE_GROUP" set "ACR_PULL_IDENTITY_RESOURCE_GROUP=%%~B"
        if /i "%%~A"=="CONTAINER_APP_NAME" set "CONTAINER_APP_NAME=%%~B"
        if /i "%%~A"=="CONTAINER_APP_RESOURCE_GROUP" set "CONTAINER_APP_RESOURCE_GROUP=%%~B"
        if /i "%%~A"=="CONTAINER_APP_ENVIRONMENT" set "CONTAINER_APP_ENVIRONMENT=%%~B"
        if /i "%%~A"=="LOG_ANALYTICS_WORKSPACE_NAME" set "LOG_ANALYTICS_WORKSPACE_NAME=%%~B"
        if /i "%%~A"=="CONTAINER_APP_INGRESS" set "CONTAINER_APP_INGRESS=%%~B"
        if /i "%%~A"=="CONTAINER_APP_TARGET_PORT" set "CONTAINER_APP_TARGET_PORT=%%~B"
        if /i "%%~A"=="CONTAINER_APP_CPU" set "CONTAINER_APP_CPU=%%~B"
        if /i "%%~A"=="CONTAINER_APP_MEMORY" set "CONTAINER_APP_MEMORY=%%~B"
        if /i "%%~A"=="APP_NAME" set "APP_NAME=%%~B"
        if /i "%%~A"=="LOG_LEVEL" set "LOG_LEVEL=%%~B"
        if /i "%%~A"=="STREAMLIT_SERVER_PORT" set "STREAMLIT_SERVER_PORT=%%~B"
        if /i "%%~A"=="AZURE_KEY_VAULT_URL" set "AZURE_KEY_VAULT_URL=%%~B"
        if /i "%%~A"=="OPENAI_API_KEY_SECRET_NAME" set "OPENAI_API_KEY_SECRET_NAME=%%~B"
        if /i "%%~A"=="AZURE_SEARCH_API_KEY_SECRET_NAME" set "AZURE_SEARCH_API_KEY_SECRET_NAME=%%~B"
        if /i "%%~A"=="AZURE_STORAGE_CONNECTION_STRING_SECRET_NAME" set "AZURE_STORAGE_CONNECTION_STRING_SECRET_NAME=%%~B"
        if /i "%%~A"=="APPLICATIONINSIGHTS_CONNECTION_STRING_SECRET_NAME" set "APPLICATIONINSIGHTS_CONNECTION_STRING_SECRET_NAME=%%~B"
        if /i "%%~A"=="OPENAI_API_KEY" set "OPENAI_API_KEY=%%~B"
        if /i "%%~A"=="OPENAI_CHAT_MODEL" set "OPENAI_CHAT_MODEL=%%~B"
        if /i "%%~A"=="OPENAI_EMBEDDING_MODEL" set "OPENAI_EMBEDDING_MODEL=%%~B"
        if /i "%%~A"=="AZURE_SEARCH_ENDPOINT" set "AZURE_SEARCH_ENDPOINT=%%~B"
        if /i "%%~A"=="AZURE_SEARCH_API_KEY" set "AZURE_SEARCH_API_KEY=%%~B"
        if /i "%%~A"=="AZURE_SEARCH_INDEX_NAME" set "AZURE_SEARCH_INDEX_NAME=%%~B"
        if /i "%%~A"=="AZURE_SEARCH_VECTOR_DIMENSIONS" set "AZURE_SEARCH_VECTOR_DIMENSIONS=%%~B"
        if /i "%%~A"=="AZURE_SEARCH_SEMANTIC_CONFIG" set "AZURE_SEARCH_SEMANTIC_CONFIG=%%~B"
        if /i "%%~A"=="AZURE_STORAGE_CONNECTION_STRING" set "AZURE_STORAGE_CONNECTION_STRING=%%~B"
        if /i "%%~A"=="AZURE_STORAGE_CONTAINER_NAME" set "AZURE_STORAGE_CONTAINER_NAME=%%~B"
        if /i "%%~A"=="SAVE_UPLOADS_TO_BLOB" set "SAVE_UPLOADS_TO_BLOB=%%~B"
        if /i "%%~A"=="APPLICATIONINSIGHTS_CONNECTION_STRING" set "APPLICATIONINSIGHTS_CONNECTION_STRING=%%~B"
        if /i "%%~A"=="MAX_SEARCH_RESULTS" set "MAX_SEARCH_RESULTS=%%~B"
        if /i "%%~A"=="MAX_CONTEXT_CHARACTERS" set "MAX_CONTEXT_CHARACTERS=%%~B"
        if /i "%%~A"=="CSV_PREVIEW_ROWS" set "CSV_PREVIEW_ROWS=%%~B"
    )
)

if "%CONTAINER_APP_RESOURCE_GROUP%"=="" if not "%AZURE_RESOURCE_GROUP%"=="" set "CONTAINER_APP_RESOURCE_GROUP=%AZURE_RESOURCE_GROUP%"
if "%ACR_PULL_IDENTITY_RESOURCE_GROUP%"=="" if not "%AZURE_RESOURCE_GROUP%"=="" set "ACR_PULL_IDENTITY_RESOURCE_GROUP=%AZURE_RESOURCE_GROUP%"

if /i "%IMAGE_TAG%"=="USEDATE" (
    if exist "%LAST_IMAGE_TAG_FILE%" (
        set /p RESOLVED_IMAGE_TAG=<"%LAST_IMAGE_TAG_FILE%"
    ) else (
        for /f %%I in ('powershell -NoProfile -Command "Get-Date -Format yyyyMMdd-HHmmss"') do set "RESOLVED_IMAGE_TAG=%%I"
        echo WARNING: %LAST_IMAGE_TAG_FILE% was not found, so a new timestamp tag was generated.
    )
) else (
    set "RESOLVED_IMAGE_TAG=%IMAGE_TAG%"
)

set "IMAGE_URI=%ACR_NAME%.azurecr.io/%IMAGE_NAME%:%RESOLVED_IMAGE_TAG%"

echo.
echo ============================================================
echo APSIM Copilot Container App deploy/update
echo ============================================================
echo Subscription:   %AZ_SUBSCRIPTION%
if not "%AZURE_RESOURCE_GROUP%"=="" echo Shared RG:      %AZURE_RESOURCE_GROUP%
echo Location:       %AZURE_LOCATION%
echo Resource group: %CONTAINER_APP_RESOURCE_GROUP%
echo Environment:    %CONTAINER_APP_ENVIRONMENT%
echo Logs workspace: %LOG_ANALYTICS_WORKSPACE_NAME%
echo App name:       %CONTAINER_APP_NAME%
echo Image:          %IMAGE_URI%
echo Ingress:        %CONTAINER_APP_INGRESS%
echo Target port:    %CONTAINER_APP_TARGET_PORT%
echo CPU / Memory:   %CONTAINER_APP_CPU% / %CONTAINER_APP_MEMORY%
if not "%ACR_PULL_IDENTITY_RESOURCE_ID%"=="" echo Pull identity:   %ACR_PULL_IDENTITY_RESOURCE_ID%
if "%ACR_PULL_IDENTITY_RESOURCE_ID%"=="" if not "%ACR_PULL_IDENTITY_NAME%"=="" echo Pull identity:   %ACR_PULL_IDENTITY_NAME%
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

if "%AZURE_LOCATION%"=="" (
    echo ERROR: AZURE_LOCATION is missing. Set it in .env or this script.
    set "EXIT_CODE=1"
    goto :finish
)

if "%CONTAINER_APP_NAME%"=="" (
    echo ERROR: CONTAINER_APP_NAME is missing. Set it in .env or this script.
    set "EXIT_CODE=1"
    goto :finish
)

if "%CONTAINER_APP_RESOURCE_GROUP%"=="" (
    echo ERROR: CONTAINER_APP_RESOURCE_GROUP is missing. Set it in .env or this script.
    set "EXIT_CODE=1"
    goto :finish
)

if "%CONTAINER_APP_ENVIRONMENT%"=="" (
    echo ERROR: CONTAINER_APP_ENVIRONMENT is missing. Set it in .env or this script.
    set "EXIT_CODE=1"
    goto :finish
)

if "%LOG_ANALYTICS_WORKSPACE_NAME%"=="" (
    echo ERROR: LOG_ANALYTICS_WORKSPACE_NAME is missing. Set it in .env or this script.
    set "EXIT_CODE=1"
    goto :finish
)

if "%AZURE_SEARCH_ENDPOINT%"=="" (
    echo ERROR: AZURE_SEARCH_ENDPOINT is missing. Set it in .env or this script.
    set "EXIT_CODE=1"
    goto :finish
)

echo [1/8] Logging into Azure...
call az login
if errorlevel 1 (
    echo ERROR: Azure login failed.
    set "EXIT_CODE=1"
    goto :finish
)

echo [2/8] Selecting Azure subscription...
call az account set --subscription "%AZ_SUBSCRIPTION%"
if errorlevel 1 (
    echo ERROR: Could not set Azure subscription.
    set "EXIT_CODE=1"
    goto :finish
)

echo [3/8] Ensuring Azure Container Apps CLI support is available...
call az containerapp --help >nul 2>&1
if errorlevel 1 (
    echo Azure Container Apps commands are not available yet. Attempting to install the CLI extension...
    call az extension add --name containerapp --only-show-errors
    if errorlevel 1 (
        echo ERROR: Could not install the Azure Container Apps CLI extension.
        echo Try running this manually:
        echo   az extension add --name containerapp
        echo If it is already installed, update Azure CLI and rerun this script.
        set "EXIT_CODE=1"
        goto :finish
    )
)

if "%ACR_PULL_IDENTITY_RESOURCE_ID%"=="" if not "%ACR_PULL_IDENTITY_NAME%"=="" (
    if "%ACR_PULL_IDENTITY_RESOURCE_GROUP%"=="" set "ACR_PULL_IDENTITY_RESOURCE_GROUP=%CONTAINER_APP_RESOURCE_GROUP%"
    echo Resolving pull identity resource ID from Azure...
    for /f "usebackq delims=" %%I in (`az identity show --name "%ACR_PULL_IDENTITY_NAME%" --resource-group "%ACR_PULL_IDENTITY_RESOURCE_GROUP%" --query id --output tsv`) do (
        set "ACR_PULL_IDENTITY_RESOURCE_ID=%%I"
    )
    if "%ACR_PULL_IDENTITY_RESOURCE_ID%"=="" (
        echo ERROR: Could not resolve ACR pull identity resource ID.
        echo Check ACR_PULL_IDENTITY_NAME and ACR_PULL_IDENTITY_RESOURCE_GROUP in .env.
        set "EXIT_CODE=1"
        goto :finish
    )
)

echo [4/8] Ensuring Microsoft.OperationalInsights is registered...
call az provider register -n Microsoft.OperationalInsights --wait
if errorlevel 1 (
    echo ERROR: Failed to register Microsoft.OperationalInsights.
    set "EXIT_CODE=1"
    goto :finish
)

for /f "usebackq delims=" %%I in (`az provider show --namespace Microsoft.OperationalInsights --query registrationState --output tsv`) do (
    set "OPS_REGISTRATION_STATE=%%I"
)
if /i not "%OPS_REGISTRATION_STATE%"=="Registered" (
    echo ERROR: Microsoft.OperationalInsights is not fully registered yet. Current state: %OPS_REGISTRATION_STATE%
    set "EXIT_CODE=1"
    goto :finish
)

echo [5/8] Ensuring Log Analytics workspace exists...
call az monitor log-analytics workspace show --resource-group "%CONTAINER_APP_RESOURCE_GROUP%" --workspace-name "%LOG_ANALYTICS_WORKSPACE_NAME%" --only-show-errors 1>nul 2>nul
if errorlevel 1 (
    call az monitor log-analytics workspace create ^
      --resource-group "%CONTAINER_APP_RESOURCE_GROUP%" ^
      --workspace-name "%LOG_ANALYTICS_WORKSPACE_NAME%" ^
      --location "%AZURE_LOCATION%"
    if errorlevel 1 (
        echo ERROR: Failed to create Log Analytics workspace.
        set "EXIT_CODE=1"
        goto :finish
    )
)

for /f "usebackq delims=" %%I in (`az monitor log-analytics workspace show --resource-group "%CONTAINER_APP_RESOURCE_GROUP%" --workspace-name "%LOG_ANALYTICS_WORKSPACE_NAME%" --query customerId --output tsv`) do (
    set "LOG_ANALYTICS_WORKSPACE_ID=%%I"
)
for /f "usebackq delims=" %%I in (`az monitor log-analytics workspace get-shared-keys --resource-group "%CONTAINER_APP_RESOURCE_GROUP%" --workspace-name "%LOG_ANALYTICS_WORKSPACE_NAME%" --query primarySharedKey --output tsv`) do (
    set "LOG_ANALYTICS_WORKSPACE_KEY=%%I"
)
if "%LOG_ANALYTICS_WORKSPACE_ID%"=="" (
    echo ERROR: Could not resolve the Log Analytics workspace ID.
    set "EXIT_CODE=1"
    goto :finish
)
if "%LOG_ANALYTICS_WORKSPACE_KEY%"=="" (
    echo ERROR: Could not resolve the Log Analytics workspace key.
    set "EXIT_CODE=1"
    goto :finish
)

echo [6/8] Ensuring the Container Apps environment exists...
call az containerapp env show --name "%CONTAINER_APP_ENVIRONMENT%" --resource-group "%CONTAINER_APP_RESOURCE_GROUP%" --only-show-errors 1>nul 2>nul
if errorlevel 1 (
    call az containerapp env create ^
      --name "%CONTAINER_APP_ENVIRONMENT%" ^
      --resource-group "%CONTAINER_APP_RESOURCE_GROUP%" ^
      --location "%AZURE_LOCATION%" ^
      --logs-workspace-id "%LOG_ANALYTICS_WORKSPACE_ID%" ^
      --logs-workspace-key "%LOG_ANALYTICS_WORKSPACE_KEY%"
    if errorlevel 1 (
        echo ERROR: Failed to create the Container Apps environment.
        echo If this is a Free Trial subscription, the usual cause is still provider registration, region availability, or quota timing.
        set "EXIT_CODE=1"
        goto :finish
    )
)

echo [7/8] Creating or updating the Container App...
call az containerapp show --name "%CONTAINER_APP_NAME%" --resource-group "%CONTAINER_APP_RESOURCE_GROUP%" --only-show-errors 1>nul 2>nul
if errorlevel 1 (
    if not "%ACR_PULL_IDENTITY_RESOURCE_ID%"=="" (
        call az containerapp create ^
          --name "%CONTAINER_APP_NAME%" ^
          --resource-group "%CONTAINER_APP_RESOURCE_GROUP%" ^
          --environment "%CONTAINER_APP_ENVIRONMENT%" ^
          --image "%IMAGE_URI%" ^
          --target-port %CONTAINER_APP_TARGET_PORT% ^
          --ingress %CONTAINER_APP_INGRESS% ^
          --registry-server "%ACR_NAME%.azurecr.io" ^
          --user-assigned "%ACR_PULL_IDENTITY_RESOURCE_ID%" ^
          --registry-identity "%ACR_PULL_IDENTITY_RESOURCE_ID%" ^
          --cpu %CONTAINER_APP_CPU% ^
          --memory %CONTAINER_APP_MEMORY%
    ) else if not "%ACR_USERNAME%"=="" if not "%ACR_PASSWORD%"=="" (
        call az containerapp create ^
          --name "%CONTAINER_APP_NAME%" ^
          --resource-group "%CONTAINER_APP_RESOURCE_GROUP%" ^
          --environment "%CONTAINER_APP_ENVIRONMENT%" ^
          --image "%IMAGE_URI%" ^
          --target-port %CONTAINER_APP_TARGET_PORT% ^
          --ingress %CONTAINER_APP_INGRESS% ^
          --registry-server "%ACR_NAME%.azurecr.io" ^
          --registry-username "%ACR_USERNAME%" ^
          --registry-password "%ACR_PASSWORD%" ^
          --cpu %CONTAINER_APP_CPU% ^
          --memory %CONTAINER_APP_MEMORY%
    ) else (
        call az containerapp create ^
          --name "%CONTAINER_APP_NAME%" ^
          --resource-group "%CONTAINER_APP_RESOURCE_GROUP%" ^
          --environment "%CONTAINER_APP_ENVIRONMENT%" ^
          --image "%IMAGE_URI%" ^
          --target-port %CONTAINER_APP_TARGET_PORT% ^
          --ingress %CONTAINER_APP_INGRESS% ^
          --registry-server "%ACR_NAME%.azurecr.io" ^
          --cpu %CONTAINER_APP_CPU% ^
          --memory %CONTAINER_APP_MEMORY%
    )
    if errorlevel 1 (
        echo ERROR: Container App creation failed.
        echo Azure Container Apps still needs a way to pull from a private ACR.
        echo Use one of these options:
        echo   1. Set ACR_PULL_IDENTITY_RESOURCE_ID or ACR_PULL_IDENTITY_NAME in .env and ensure it has AcrPull on the registry
        echo   2. Set ACR_USERNAME and ACR_PASSWORD in .env
        echo   3. Enable the ACR admin user and use those credentials
        set "EXIT_CODE=1"
        goto :finish
    )
) else (
    if not "%ACR_PULL_IDENTITY_RESOURCE_ID%"=="" (
        call az containerapp identity assign ^
          --name "%CONTAINER_APP_NAME%" ^
          --resource-group "%CONTAINER_APP_RESOURCE_GROUP%" ^
          --user-assigned "%ACR_PULL_IDENTITY_RESOURCE_ID%"
        if errorlevel 1 (
            echo ERROR: Failed to assign the user-assigned identity to the existing Container App.
            set "EXIT_CODE=1"
            goto :finish
        )

        call az containerapp registry set ^
          --name "%CONTAINER_APP_NAME%" ^
          --resource-group "%CONTAINER_APP_RESOURCE_GROUP%" ^
          --server "%ACR_NAME%.azurecr.io" ^
          --identity "%ACR_PULL_IDENTITY_RESOURCE_ID%"
        if errorlevel 1 (
            echo ERROR: Failed to configure the Container App registry to use the managed identity.
            set "EXIT_CODE=1"
            goto :finish
        )
    )

    call az containerapp update ^
      --name "%CONTAINER_APP_NAME%" ^
      --resource-group "%CONTAINER_APP_RESOURCE_GROUP%" ^
      --image "%IMAGE_URI%" ^
      --cpu %CONTAINER_APP_CPU% ^
      --memory %CONTAINER_APP_MEMORY%
    if errorlevel 1 (
        echo ERROR: Container App update failed.
        set "EXIT_CODE=1"
        goto :finish
    )
)

set "USE_DIRECT_SECRETS=0"
if not "%OPENAI_API_KEY%"=="" set "USE_DIRECT_SECRETS=1"
if not "%AZURE_SEARCH_API_KEY%"=="" set "USE_DIRECT_SECRETS=1"
if not "%AZURE_STORAGE_CONNECTION_STRING%"=="" set "USE_DIRECT_SECRETS=1"
if not "%APPLICATIONINSIGHTS_CONNECTION_STRING%"=="" set "USE_DIRECT_SECRETS=1"

if "%USE_DIRECT_SECRETS%"=="1" (
    echo Updating Container App secrets...
    set "SECRET_ARGS="
    if not "%OPENAI_API_KEY%"=="" set SECRET_ARGS=!SECRET_ARGS! "openai-api-key=%OPENAI_API_KEY%"
    if not "%AZURE_SEARCH_API_KEY%"=="" set SECRET_ARGS=!SECRET_ARGS! "azure-search-api-key=%AZURE_SEARCH_API_KEY%"
    if not "%AZURE_STORAGE_CONNECTION_STRING%"=="" set SECRET_ARGS=!SECRET_ARGS! "azure-storage-connection-string=%AZURE_STORAGE_CONNECTION_STRING%"
    if not "%APPLICATIONINSIGHTS_CONNECTION_STRING%"=="" set SECRET_ARGS=!SECRET_ARGS! "applicationinsights-connection-string=%APPLICATIONINSIGHTS_CONNECTION_STRING%"
    call az containerapp secret set --name "%CONTAINER_APP_NAME%" --resource-group "%CONTAINER_APP_RESOURCE_GROUP%" --secrets !SECRET_ARGS!
    if errorlevel 1 (
        echo ERROR: Failed to update Container App secrets.
        set "EXIT_CODE=1"
        goto :finish
    )
)

echo [8/8] Updating Container App environment variables...
if "%USE_DIRECT_SECRETS%"=="1" (
    call az containerapp update ^
      --name "%CONTAINER_APP_NAME%" ^
      --resource-group "%CONTAINER_APP_RESOURCE_GROUP%" ^
      --set-env-vars ^
      "APP_NAME=%APP_NAME%" ^
      "LOG_LEVEL=%LOG_LEVEL%" ^
      "STREAMLIT_SERVER_PORT=%STREAMLIT_SERVER_PORT%" ^
      "OPENAI_CHAT_MODEL=%OPENAI_CHAT_MODEL%" ^
      "OPENAI_EMBEDDING_MODEL=%OPENAI_EMBEDDING_MODEL%" ^
      "AZURE_SEARCH_ENDPOINT=%AZURE_SEARCH_ENDPOINT%" ^
      "AZURE_SEARCH_INDEX_NAME=%AZURE_SEARCH_INDEX_NAME%" ^
      "AZURE_SEARCH_VECTOR_DIMENSIONS=%AZURE_SEARCH_VECTOR_DIMENSIONS%" ^
      "AZURE_SEARCH_SEMANTIC_CONFIG=%AZURE_SEARCH_SEMANTIC_CONFIG%" ^
      "AZURE_STORAGE_CONTAINER_NAME=%AZURE_STORAGE_CONTAINER_NAME%" ^
      "SAVE_UPLOADS_TO_BLOB=%SAVE_UPLOADS_TO_BLOB%" ^
      "MAX_SEARCH_RESULTS=%MAX_SEARCH_RESULTS%" ^
      "MAX_CONTEXT_CHARACTERS=%MAX_CONTEXT_CHARACTERS%" ^
      "CSV_PREVIEW_ROWS=%CSV_PREVIEW_ROWS%" ^
      "OPENAI_API_KEY=secretref:openai-api-key" ^
      "AZURE_SEARCH_API_KEY=secretref:azure-search-api-key" ^
      "AZURE_STORAGE_CONNECTION_STRING=secretref:azure-storage-connection-string" ^
      "APPLICATIONINSIGHTS_CONNECTION_STRING=secretref:applicationinsights-connection-string"
) else (
    call az containerapp update ^
      --name "%CONTAINER_APP_NAME%" ^
      --resource-group "%CONTAINER_APP_RESOURCE_GROUP%" ^
      --set-env-vars ^
      "APP_NAME=%APP_NAME%" ^
      "LOG_LEVEL=%LOG_LEVEL%" ^
      "STREAMLIT_SERVER_PORT=%STREAMLIT_SERVER_PORT%" ^
      "AZURE_KEY_VAULT_URL=%AZURE_KEY_VAULT_URL%" ^
      "OPENAI_CHAT_MODEL=%OPENAI_CHAT_MODEL%" ^
      "OPENAI_EMBEDDING_MODEL=%OPENAI_EMBEDDING_MODEL%" ^
      "AZURE_SEARCH_ENDPOINT=%AZURE_SEARCH_ENDPOINT%" ^
      "AZURE_SEARCH_INDEX_NAME=%AZURE_SEARCH_INDEX_NAME%" ^
      "AZURE_SEARCH_VECTOR_DIMENSIONS=%AZURE_SEARCH_VECTOR_DIMENSIONS%" ^
      "AZURE_SEARCH_SEMANTIC_CONFIG=%AZURE_SEARCH_SEMANTIC_CONFIG%" ^
      "AZURE_STORAGE_CONTAINER_NAME=%AZURE_STORAGE_CONTAINER_NAME%" ^
      "SAVE_UPLOADS_TO_BLOB=%SAVE_UPLOADS_TO_BLOB%" ^
      "OPENAI_API_KEY_SECRET_NAME=%OPENAI_API_KEY_SECRET_NAME%" ^
      "AZURE_SEARCH_API_KEY_SECRET_NAME=%AZURE_SEARCH_API_KEY_SECRET_NAME%" ^
      "AZURE_STORAGE_CONNECTION_STRING_SECRET_NAME=%AZURE_STORAGE_CONNECTION_STRING_SECRET_NAME%" ^
      "APPLICATIONINSIGHTS_CONNECTION_STRING_SECRET_NAME=%APPLICATIONINSIGHTS_CONNECTION_STRING_SECRET_NAME%" ^
      "MAX_SEARCH_RESULTS=%MAX_SEARCH_RESULTS%" ^
      "MAX_CONTEXT_CHARACTERS=%MAX_CONTEXT_CHARACTERS%" ^
      "CSV_PREVIEW_ROWS=%CSV_PREVIEW_ROWS%"
)
if errorlevel 1 (
    echo ERROR: Failed to update Container App environment variables.
    set "EXIT_CODE=1"
    goto :finish
)

echo.
echo SUCCESS: Container App deployment settings were applied.
echo.
call az containerapp show ^
  --name "%CONTAINER_APP_NAME%" ^
  --resource-group "%CONTAINER_APP_RESOURCE_GROUP%" ^
  --query properties.configuration.ingress.fqdn ^
  --output tsv

:finish
echo.
echo Press any key to close this window...
pause >nul

endlocal
exit /b %EXIT_CODE%
