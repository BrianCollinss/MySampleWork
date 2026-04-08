#!/usr/bin/env bash
set -euo pipefail

# One-time helper for creating or updating the AWS CLI profile used by this project.
# Use "sso" if your organization uses AWS IAM Identity Center.
# Use "keys" if you authenticate with access keys instead.

PROFILE_NAME="bc-snowflake-training-0001"

if ! command -v aws >/dev/null 2>&1; then
  cat <<'EOF'
AWS CLI was not found on PATH.

Install AWS CLI v2 first, then rerun this helper.
Windows MSI:
  https://awscli.amazonaws.com/AWSCLIV2.msi

After installation, confirm with:
  aws --version
EOF
  exit 1
fi

case "${1:-}" in
  sso)
    echo "Configuring AWS SSO profile \"${PROFILE_NAME}\"..."
    aws configure sso --profile "${PROFILE_NAME}"
    echo
    echo "Signing in with AWS SSO profile \"${PROFILE_NAME}\"..."
    aws sso login --profile "${PROFILE_NAME}"
    ;;
  keys)
    echo "Configuring AWS access-key profile \"${PROFILE_NAME}\"..."
    aws configure --profile "${PROFILE_NAME}"
    ;;
  *)
    echo "Usage:"
    echo "  $(basename "$0") sso   (recommended for AWS SSO / IAM Identity Center)"
    echo "  $(basename "$0") keys  (for AWS access key authentication)"
    exit 1
    ;;
esac
