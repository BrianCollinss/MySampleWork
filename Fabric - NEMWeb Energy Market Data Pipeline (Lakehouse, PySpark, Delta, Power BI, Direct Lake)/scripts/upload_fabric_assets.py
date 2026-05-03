"""Upload notebook support files to Fabric.

The notebooks import `nem_fabric` from `Files/libs` and the package resolves
checked-in YAML files from the Lakehouse `Files/config` folder. This script
uploads those Lakehouse assets through the OneLake DFS endpoint and publishes
local notebooks as Fabric workspace Notebook items.
"""

from __future__ import annotations

import argparse
import base64
import json
import mimetypes
import os
import subprocess
import sys
from pathlib import Path, PurePosixPath
from typing import Iterable
from urllib.error import HTTPError
from urllib.parse import quote
from urllib.request import Request, urlopen

from dotenv import load_dotenv

PROJECT_ROOT = Path(__file__).resolve().parents[1]
ONELAKE_HOST = "onelake.dfs.fabric.microsoft.com"
FABRIC_API_HOST = "https://api.fabric.microsoft.com/v1"
STORAGE_RESOURCE = "https://storage.azure.com/"
FABRIC_RESOURCE = "https://api.fabric.microsoft.com/"
DEVICE_CODE_CREDENTIAL = None


def parse_args() -> argparse.Namespace:
    """Parse command line arguments."""

    parser = argparse.ArgumentParser(
        description="Upload Fabric notebook support files and workspace notebooks."
    )
    parser.add_argument(
        "--workspace",
        help="Fabric workspace name. Defaults to FABRIC_WORKSPACE_NAME from .env.",
    )
    parser.add_argument(
        "--lakehouse",
        help="Fabric Lakehouse name. Defaults to FABRIC_LAKEHOUSE_NAME from .env.",
    )
    parser.add_argument(
        "--skip-notebooks",
        action="store_true",
        help="Do not publish local .ipynb files as workspace Notebook items.",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Print planned uploads without calling OneLake.",
    )
    parser.add_argument(
        "--access-token",
        help=(
            "Bearer token for OneLake. Defaults to FABRIC_ONELAKE_ACCESS_TOKEN "
            "from the environment, then Azure CLI."
        ),
    )
    parser.add_argument(
        "--fabric-access-token",
        help=(
            "Bearer token for Fabric REST API. Defaults to "
            "FABRIC_API_ACCESS_TOKEN from the environment, then Azure CLI."
        ),
    )
    parser.add_argument(
        "--device-code",
        action="store_true",
        help=(
            "Use browser/device-code sign-in through azure-identity when Azure "
            "CLI is unavailable."
        ),
    )
    return parser.parse_args()


def azure_cli_token(resource: str) -> str | None:
    """Return an Azure access token for a resource if Azure CLI is available."""

    command = [
        "az",
        "account",
        "get-access-token",
        "--resource",
        resource,
        "--output",
        "json",
    ]
    try:
        result = subprocess.run(
            command,
            check=True,
            capture_output=True,
            text=True,
        )
    except FileNotFoundError as exc:
        return None
    except subprocess.CalledProcessError as exc:
        raise RuntimeError(
            "Azure CLI could not return a token. Sign in with "
            "`az login --use-device-code` for MFA-protected accounts, or use an "
            "authorised service principal for automation.\n"
            f"{exc.stderr.strip()}"
        ) from exc

    payload = json.loads(result.stdout)
    return payload["accessToken"]


def device_code_token(resource: str) -> str:
    """Return an access token using an interactive device-code sign-in."""

    try:
        from azure.identity import DeviceCodeCredential
    except ImportError as exc:
        python_executable = sys.executable or "python"
        raise RuntimeError(
            "Device-code sign-in requires azure-identity. Install it in the "
            "same Python environment that runs this script with "
            f"`{python_executable} -m pip install azure-identity`, or use "
            "Azure CLI or a supplied bearer token."
        ) from exc

    global DEVICE_CODE_CREDENTIAL
    if DEVICE_CODE_CREDENTIAL is None:
        DEVICE_CODE_CREDENTIAL = DeviceCodeCredential()
    return DEVICE_CODE_CREDENTIAL.get_token(f"{resource}/.default").token


def resolve_access_token(
    args: argparse.Namespace,
    *,
    argument_token: str | None,
    environment_name: str,
    resource: str,
    label: str,
) -> str:
    """Resolve a bearer token from arguments, environment, device code, or CLI."""

    token = argument_token or os.getenv(environment_name, "")
    if token:
        return token

    if args.device_code:
        return device_code_token(resource)

    token = azure_cli_token(resource)
    if token:
        return token

    raise RuntimeError(
        f"Azure CLI is not installed or not on PATH, and no {label} bearer token "
        "was supplied. Install Azure CLI and run "
        "`az login --use-device-code --allow-no-subscriptions`, run this script "
        f"with `--device-code`, or set {environment_name} in your local "
        "shell/.env."
    )


def onelake_url(workspace: str, lakehouse: str, remote_path: PurePosixPath) -> str:
    """Build a OneLake DFS URL for a Lakehouse Files path."""

    filesystem = quote(workspace, safe="")
    lakehouse_path = quote(f"{lakehouse}.Lakehouse/{remote_path.as_posix()}", safe="/")
    return f"https://{ONELAKE_HOST}/{filesystem}/{lakehouse_path}"


def request_onelake(
    method: str,
    url: str,
    token: str,
    *,
    data: bytes | None = None,
    content_type: str | None = None,
    ignore_statuses: set[int] | None = None,
) -> None:
    """Send a OneLake request and raise a readable error on failure."""

    headers = {
        "Authorization": f"Bearer {token}",
        "x-ms-version": "2023-11-03",
    }
    if content_type:
        headers["Content-Type"] = content_type

    request = Request(url, data=data, headers=headers, method=method)
    try:
        with urlopen(request) as response:
            response.read()
    except HTTPError as exc:
        if ignore_statuses and exc.code in ignore_statuses:
            return
        detail = exc.read().decode("utf-8", errors="replace")
        raise RuntimeError(
            f"OneLake request failed: {exc.code} {exc.reason}\n{detail}"
        ) from exc


def request_fabric(
    method: str,
    path: str,
    token: str,
    *,
    payload: dict | None = None,
) -> dict:
    """Send a Fabric REST API request and return the JSON response when present."""

    data = None
    headers = {"Authorization": f"Bearer {token}"}
    if payload is not None:
        data = json.dumps(payload).encode("utf-8")
        headers["Content-Type"] = "application/json"

    request = Request(f"{FABRIC_API_HOST}{path}", data=data, headers=headers, method=method)
    try:
        with urlopen(request) as response:
            body = response.read()
    except HTTPError as exc:
        detail = exc.read().decode("utf-8", errors="replace")
        raise RuntimeError(
            f"Fabric API request failed: {exc.code} {exc.reason}\n{detail}"
        ) from exc

    return json.loads(body) if body else {}


def resolve_workspace_id(workspace: str, token: str) -> str:
    """Resolve a Fabric workspace display name or ID to a workspace ID."""

    if len(workspace) == 36 and workspace.count("-") == 4:
        return workspace

    matches = []
    continuation = ""
    while True:
        path = "/workspaces"
        if continuation:
            path += f"?continuationToken={quote(continuation, safe='')}"
        payload = request_fabric("GET", path, token)
        matches.extend(
            item for item in payload.get("value", []) if item.get("displayName") == workspace
        )
        continuation = payload.get("continuationToken", "")
        if not continuation:
            break

    if not matches:
        raise RuntimeError(f"Fabric workspace not found: {workspace}")
    if len(matches) > 1:
        raise RuntimeError(
            f"Multiple Fabric workspaces are named {workspace!r}. Pass the workspace ID."
        )
    return matches[0]["id"]


def notebook_definition(local_path: Path) -> dict:
    """Build a Fabric Notebook definition from a local ipynb file."""

    payload = base64.b64encode(local_path.read_bytes()).decode("ascii")
    return {
        "format": "ipynb",
        "parts": [
            {
                "path": "notebook-content.ipynb",
                "payload": payload,
                "payloadType": "InlineBase64",
            }
        ],
    }


def list_workspace_notebooks(workspace_id: str, token: str) -> dict[str, str]:
    """Return workspace notebook IDs keyed by display name."""

    notebooks: dict[str, str] = {}
    continuation = ""
    while True:
        path = f"/workspaces/{workspace_id}/notebooks"
        if continuation:
            path += f"?continuationToken={quote(continuation, safe='')}"
        payload = request_fabric("GET", path, token)
        notebooks.update(
            {
                item["displayName"]: item["id"]
                for item in payload.get("value", [])
                if item.get("displayName") and item.get("id")
            }
        )
        continuation = payload.get("continuationToken", "")
        if not continuation:
            break
    return notebooks


def publish_notebooks(
    workspace_id: str,
    source_root: Path,
    token: str,
    dry_run: bool,
) -> int:
    """Create or update Fabric workspace Notebook items from local ipynb files."""

    notebook_paths = sorted(source_root.glob("*.ipynb"))
    existing = {} if dry_run else list_workspace_notebooks(workspace_id, token)

    for local_path in notebook_paths:
        display_name = local_path.stem
        print(f"{local_path.relative_to(PROJECT_ROOT)} -> workspace notebook {display_name}")
        if dry_run:
            continue

        definition = notebook_definition(local_path)
        notebook_id = existing.get(display_name)
        if notebook_id:
            request_fabric(
                "POST",
                f"/workspaces/{workspace_id}/notebooks/{notebook_id}/updateDefinition",
                token,
                payload={"definition": definition},
            )
        else:
            request_fabric(
                "POST",
                f"/workspaces/{workspace_id}/notebooks",
                token,
                payload={
                    "displayName": display_name,
                    "description": "Published from local project notebooks.",
                    "definition": definition,
                },
            )

    return len(notebook_paths)


def ensure_directory(
    workspace: str,
    lakehouse: str,
    directory: PurePosixPath,
    token: str,
    created: set[PurePosixPath],
) -> None:
    """Create a OneLake directory and its parents if they do not already exist."""

    if directory == PurePosixPath("."):
        return

    parents = list(reversed(directory.parents[:-1])) + [directory]
    for parent in parents:
        if parent in created:
            continue
        url = f"{onelake_url(workspace, lakehouse, parent)}?resource=directory"
        request_onelake("PUT", url, token, ignore_statuses={409})
        created.add(parent)


def upload_file(
    workspace: str,
    lakehouse: str,
    local_path: Path,
    remote_path: PurePosixPath,
    token: str,
) -> None:
    """Upload one local file to a Lakehouse Files path, replacing any existing file."""

    content = local_path.read_bytes()
    content_type = (
        mimetypes.guess_type(local_path.name)[0] or "application/octet-stream"
    )
    file_url = onelake_url(workspace, lakehouse, remote_path)

    # OneLake file creation fails if the path already exists, so delete first to
    # make package and config uploads deterministic across reruns.
    request_onelake("DELETE", file_url, token, ignore_statuses={404})
    request_onelake("PUT", f"{file_url}?resource=file", token)
    if content:
        append_url = f"{file_url}?action=append&position=0"
        request_onelake(
            "PATCH",
            append_url,
            token,
            data=content,
            content_type=content_type,
        )
    flush_url = f"{file_url}?action=flush&position={len(content)}"
    request_onelake("PATCH", flush_url, token)


def remove_directory(
    workspace: str,
    lakehouse: str,
    directory: PurePosixPath,
    token: str,
) -> None:
    """Remove a managed OneLake directory tree before re-uploading it."""

    url = f"{onelake_url(workspace, lakehouse, directory)}?recursive=true"
    request_onelake("DELETE", url, token, ignore_statuses={404})


def iter_files(
    source_root: Path,
    target_root: PurePosixPath,
) -> Iterable[tuple[Path, PurePosixPath]]:
    """Yield local files and their matching Lakehouse Files paths."""

    files = sorted(path for path in source_root.rglob("*") if path.is_file())
    for local_path in files:
        if "__pycache__" in local_path.parts or local_path.suffix in {".pyc", ".pyo"}:
            continue
        relative = local_path.relative_to(source_root).as_posix()
        yield local_path, target_root / relative


def upload_tree(
    workspace: str,
    lakehouse: str,
    source_root: Path,
    target_root: PurePosixPath,
    token: str,
    dry_run: bool,
    created: set[PurePosixPath],
) -> int:
    """Upload every file below a local folder to a Lakehouse Files folder."""

    count = 0
    for local_path, remote_path in iter_files(source_root, target_root):
        print(f"{local_path.relative_to(PROJECT_ROOT)} -> {remote_path}")
        count += 1
        if dry_run:
            continue
        ensure_directory(workspace, lakehouse, remote_path.parent, token, created)
        upload_file(workspace, lakehouse, local_path, remote_path, token)
    return count


def main() -> int:
    """Upload required support assets for Fabric notebooks."""

    args = parse_args()
    load_dotenv(PROJECT_ROOT / ".env")

    workspace = args.workspace or os.getenv("FABRIC_WORKSPACE_NAME", "")
    lakehouse = args.lakehouse or os.getenv("FABRIC_LAKEHOUSE_NAME", "")
    if not workspace or not lakehouse:
        print(
            "Set FABRIC_WORKSPACE_NAME and FABRIC_LAKEHOUSE_NAME in .env, "
            "or pass --workspace and --lakehouse.",
            file=sys.stderr,
        )
        return 2

    uploads = [
        (PROJECT_ROOT / "src" / "nem_fabric", PurePosixPath("Files/libs/nem_fabric")),
        (PROJECT_ROOT / "config", PurePosixPath("Files/config")),
    ]

    print(f"Target: {workspace}/{lakehouse}.Lakehouse")
    onelake_token = (
        ""
        if args.dry_run
        else resolve_access_token(
            args,
            argument_token=args.access_token,
            environment_name="FABRIC_ONELAKE_ACCESS_TOKEN",
            resource=STORAGE_RESOURCE,
            label="OneLake",
        )
    )
    fabric_token = (
        ""
        if args.dry_run or args.skip_notebooks
        else resolve_access_token(
            args,
            argument_token=args.fabric_access_token,
            environment_name="FABRIC_API_ACCESS_TOKEN",
            resource=FABRIC_RESOURCE,
            label="Fabric API",
        )
    )
    created: set[PurePosixPath] = set()

    total = 0
    for source_root, target_root in uploads:
        if not source_root.exists():
            print(f"Missing source folder: {source_root}", file=sys.stderr)
            return 1
        if not args.dry_run:
            print(f"Clearing {target_root}")
            remove_directory(workspace, lakehouse, target_root, onelake_token)
        total += upload_tree(
            workspace,
            lakehouse,
            source_root,
            target_root,
            onelake_token,
            args.dry_run,
            created,
        )

    if not args.skip_notebooks:
        notebooks_root = PROJECT_ROOT / "notebooks"
        if not notebooks_root.exists():
            print(f"Missing source folder: {notebooks_root}", file=sys.stderr)
            return 1
        workspace_id = workspace if args.dry_run else resolve_workspace_id(workspace, fabric_token)
        total += publish_notebooks(workspace_id, notebooks_root, fabric_token, args.dry_run)

    action = "Planned" if args.dry_run else "Uploaded"
    print(f"{action} {total} assets.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
