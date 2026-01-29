#!/usr/bin/env python
"""Post figures to GitHub PRs/Issues via the Contents API.

Commits images to an orphan `gh-artifacts` branch and posts (or updates)
a sticky comment with embedded image links.

Requires: gh CLI (authenticated).

Usage:
    python scripts/post_figures.py --target PR:1135 --images fig1.png fig2.png
    python scripts/post_figures.py --target ISSUE:42 --images output/*.png --title "Albedo"
    python scripts/post_figures.py --target PR:1135 --cleanup
"""

from __future__ import annotations

import argparse
import base64
import glob
import json
import subprocess
import sys
from datetime import datetime, timezone
from pathlib import Path

ARTIFACTS_BRANCH = "gh-artifacts"
COMMENT_MARKER = "<!-- suews-figures -->"


# ---------------------------------------------------------------------------
# gh CLI helpers
# ---------------------------------------------------------------------------


def _gh(*args: str, input_data: str | None = None) -> str:
    """Run a gh CLI command and return stdout."""
    cmd = ["gh", *args]
    result = subprocess.run(
        cmd,
        capture_output=True,
        text=True,
        input=input_data,
    )
    if result.returncode != 0:
        raise RuntimeError(
            f"gh command failed: {' '.join(cmd)}\n"
            f"stderr: {result.stderr.strip()}"
        )
    return result.stdout.strip()


def _gh_api(
    endpoint: str,
    method: str = "GET",
    fields: dict | None = None,
    raw_fields: dict | None = None,
    input_data: str | None = None,
) -> dict | list | str:
    """Call the GitHub API via gh api."""
    cmd_args = ["api", endpoint, "--method", method]
    if fields:
        for k, v in fields.items():
            cmd_args.extend(["-f", f"{k}={v}"])
    if raw_fields:
        for k, v in raw_fields.items():
            cmd_args.extend(["-F", f"{k}={v}"])
    if input_data is not None:
        cmd_args.extend(["--input", "-"])
    out = _gh(*cmd_args, input_data=input_data)
    if not out:
        return {}
    try:
        return json.loads(out)
    except json.JSONDecodeError:
        return out


def get_repo_nwo() -> str:
    """Return owner/repo for the current repository."""
    out = _gh("repo", "view", "--json", "nameWithOwner", "-q", ".nameWithOwner")
    if not out:
        raise RuntimeError("Could not determine repository. Are you in a git repo?")
    return out


# ---------------------------------------------------------------------------
# Branch management
# ---------------------------------------------------------------------------


def ensure_artifacts_branch(nwo: str) -> None:
    """Create the orphan gh-artifacts branch if it does not exist."""
    try:
        _gh_api(f"repos/{nwo}/git/ref/heads/{ARTIFACTS_BRANCH}")
        return  # branch exists
    except RuntimeError as exc:
        if "Not Found" not in str(exc) and "404" not in str(exc):
            raise  # re-raise auth errors, network failures, rate limits, etc.
        # branch does not exist, create it below

    print(f"Creating orphan branch '{ARTIFACTS_BRANCH}'...")

    # We need a tree and a commit that have no parent.
    # 1. Create a blob for a README
    readme_content = base64.b64encode(
        b"# gh-artifacts\n\n"
        b"This branch stores images uploaded by `scripts/post_figures.py`.\n"
        b"Do not merge this branch into main.\n"
    ).decode()

    blob = _gh_api(
        f"repos/{nwo}/git/blobs",
        method="POST",
        fields={"content": readme_content, "encoding": "base64"},
    )

    # 2. Create a tree with the README
    tree_payload = json.dumps(
        {"tree": [{"path": "README.md", "mode": "100644", "type": "blob", "sha": blob["sha"]}]}
    )
    tree = _gh_api(
        f"repos/{nwo}/git/trees",
        method="POST",
        input_data=tree_payload,
    )

    # 3. Create an orphan commit (no parents)
    commit_payload = json.dumps(
        {
            "message": "init: create gh-artifacts branch",
            "tree": tree["sha"],
            "parents": [],
        }
    )
    commit = _gh_api(
        f"repos/{nwo}/git/commits",
        method="POST",
        input_data=commit_payload,
    )

    # 4. Create the branch ref
    ref_payload = json.dumps(
        {"ref": f"refs/heads/{ARTIFACTS_BRANCH}", "sha": commit["sha"]}
    )
    _gh_api(
        f"repos/{nwo}/git/refs",
        method="POST",
        input_data=ref_payload,
    )
    print(f"Branch '{ARTIFACTS_BRANCH}' created.")


# ---------------------------------------------------------------------------
# Image upload
# ---------------------------------------------------------------------------


def upload_image(nwo: str, image_path: Path, target_type: str, target_number: str) -> str:
    """Upload a single image to the gh-artifacts branch.

    Returns the raw URL for embedding in Markdown.
    """
    timestamp = datetime.now(timezone.utc).strftime("%Y%m%d_%H%M%S_%f")
    remote_path = f"{target_type}/{target_number}/{timestamp}_{image_path.name}"

    content_b64 = base64.b64encode(image_path.read_bytes()).decode()

    # Check if file already exists (to get its SHA for update)
    sha = None
    try:
        existing = _gh_api(
            f"repos/{nwo}/contents/{remote_path}",
            method="GET",
            fields={"ref": ARTIFACTS_BRANCH},
        )
        sha = existing.get("sha")
    except RuntimeError:
        pass  # file doesn't exist yet

    payload = {
        "message": f"upload: {remote_path}",
        "content": content_b64,
        "branch": ARTIFACTS_BRANCH,
    }
    if sha:
        payload["sha"] = sha

    _gh_api(
        f"repos/{nwo}/contents/{remote_path}",
        method="PUT",
        input_data=json.dumps(payload),
    )

    raw_url = f"https://raw.githubusercontent.com/{nwo}/{ARTIFACTS_BRANCH}/{remote_path}"
    print(f"  Uploaded: {image_path.name} -> {raw_url}")
    return raw_url


# ---------------------------------------------------------------------------
# Comment management
# ---------------------------------------------------------------------------


def _build_comment_body(
    image_urls: list[tuple[str, str]],
    title: str | None,
    existing_body: str | None,
) -> str:
    """Build the Markdown body for the sticky comment.

    Args:
        image_urls: list of (filename, url) pairs
        title: optional section title
        existing_body: existing comment body to preserve in <details>
    """
    now = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M UTC")
    section_title = title or "Figures"

    lines = [COMMENT_MARKER, "", f"## {section_title}", "", f"*Posted {now}*", ""]

    # Display images -- one per line for clarity
    for filename, url in image_urls:
        lines.append(f"**{filename}**")
        lines.append(f"![{filename}]({url})")
        lines.append("")

    # Collapse previous content if any
    if existing_body:
        # Strip the marker from old body
        old_content = existing_body.replace(COMMENT_MARKER, "").strip()
        if old_content:
            lines.append("<details>")
            lines.append("<summary>Previous uploads</summary>")
            lines.append("")
            lines.append(old_content)
            lines.append("")
            lines.append("</details>")
            lines.append("")

    return "\n".join(lines)


def find_sticky_comment(nwo: str, issue_number: str) -> dict | None:
    """Find an existing sticky comment on the issue/PR."""
    page = 1
    while True:
        comments = _gh_api(
            f"repos/{nwo}/issues/{issue_number}/comments",
            fields={"per_page": "100", "page": str(page)},
        )
        if not comments or not isinstance(comments, list):
            break
        for comment in comments:
            if COMMENT_MARKER in comment.get("body", ""):
                return comment
        if len(comments) < 100:
            break
        page += 1
    return None


def post_or_update_comment(
    nwo: str,
    issue_number: str,
    image_urls: list[tuple[str, str]],
    title: str | None,
) -> str:
    """Post a new or update existing sticky comment. Returns the comment URL."""
    existing = find_sticky_comment(nwo, issue_number)

    body = _build_comment_body(
        image_urls,
        title,
        existing["body"] if existing else None,
    )

    if existing:
        result = _gh_api(
            f"repos/{nwo}/issues/comments/{existing['id']}",
            method="PATCH",
            input_data=json.dumps({"body": body}),
        )
        print(f"Updated existing comment: {result.get('html_url', '')}")
        return result.get("html_url", "")
    else:
        result = _gh_api(
            f"repos/{nwo}/issues/{issue_number}/comments",
            method="POST",
            input_data=json.dumps({"body": body}),
        )
        print(f"Created new comment: {result.get('html_url', '')}")
        return result.get("html_url", "")


# ---------------------------------------------------------------------------
# Cleanup
# ---------------------------------------------------------------------------


def cleanup_artifacts(nwo: str, target_type: str, target_number: str) -> None:
    """Delete all artifacts for a given target from the gh-artifacts branch."""
    prefix = f"{target_type}/{target_number}"
    print(f"Cleaning up artifacts under {prefix}/...")

    try:
        contents = _gh_api(
            f"repos/{nwo}/contents/{prefix}",
            fields={"ref": ARTIFACTS_BRANCH},
        )
    except RuntimeError:
        print("No artifacts found.")
        return

    if not isinstance(contents, list):
        contents = [contents]

    for item in contents:
        _gh_api(
            f"repos/{nwo}/contents/{item['path']}",
            method="DELETE",
            input_data=json.dumps(
                {
                    "message": f"cleanup: {item['path']}",
                    "sha": item["sha"],
                    "branch": ARTIFACTS_BRANCH,
                }
            ),
        )
        print(f"  Deleted: {item['path']}")

    print(f"Cleaned up {len(contents)} file(s).")


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------


def parse_target(target_str: str) -> tuple[str, str]:
    """Parse 'PR:1135' or 'ISSUE:42' into (type, number)."""
    target_str = target_str.strip()
    for prefix in ("PR:", "ISSUE:"):
        if target_str.upper().startswith(prefix):
            number = target_str[len(prefix):]
            target_type = "pr" if prefix == "PR:" else "issue"
            if not number.isdigit():
                raise ValueError(f"Invalid target number: {number}")
            return target_type, number
    raise ValueError(
        f"Invalid target format: '{target_str}'. Use PR:<number> or ISSUE:<number>"
    )


def resolve_images(patterns: list[str]) -> list[Path]:
    """Expand glob patterns and return list of image paths."""
    paths = []
    for pattern in patterns:
        expanded = glob.glob(pattern)
        if not expanded:
            print(f"Warning: no files match '{pattern}'", file=sys.stderr)
            continue
        for p in expanded:
            path = Path(p)
            if path.is_file():
                paths.append(path)
    return paths


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Post figures to GitHub PRs/Issues",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    parser.add_argument(
        "--target",
        required=True,
        help="Target PR or Issue, e.g. PR:1135 or ISSUE:42",
    )
    parser.add_argument(
        "--images",
        nargs="*",
        default=[],
        help="Image file paths or glob patterns",
    )
    parser.add_argument(
        "--title",
        help="Section title for the figures (default: 'Figures')",
    )
    parser.add_argument(
        "--cleanup",
        action="store_true",
        help="Remove all artifacts for the target instead of uploading",
    )
    parser.add_argument(
        "--upload-only",
        action="store_true",
        help="Upload images and print Markdown links to stdout, without posting a comment",
    )
    args = parser.parse_args()

    try:
        target_type, target_number = parse_target(args.target)
    except ValueError as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1

    nwo = get_repo_nwo()
    print(f"Repository: {nwo}")
    print(f"Target: {target_type.upper()} #{target_number}")

    if args.cleanup:
        cleanup_artifacts(nwo, target_type, target_number)
        return 0

    # Upload mode
    images = resolve_images(args.images)
    if not images:
        print("Error: no image files found.", file=sys.stderr)
        return 1

    print(f"Uploading {len(images)} image(s)...")
    ensure_artifacts_branch(nwo)

    image_urls = []
    for img in images:
        url = upload_image(nwo, img, target_type, target_number)
        image_urls.append((img.name, url))

    if args.upload_only:
        # Print Markdown-ready image links for embedding in other drafts
        print("\n--- Markdown image links ---")
        for filename, url in image_urls:
            print(f"![{filename}]({url})")
        print("---")
        return 0

    print(f"\nPosting comment to {target_type.upper()} #{target_number}...")
    comment_url = post_or_update_comment(nwo, target_number, image_urls, args.title)
    print(f"\nDone. Comment: {comment_url}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
