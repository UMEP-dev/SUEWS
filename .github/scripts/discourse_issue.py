"""Workflow-owned retry guard; callers must hold the workflow concurrency lock.

REST listings avoid search-index lag, but are not an exactly-once transaction.
Never retry a create within a run: a lost response may already have committed.
"""

import json
import os
import re
import subprocess
import sys
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import urlencode

HOST = "https://community.suews.io"
REPO = re.compile(r"[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+")


def topic_id(value):
    if not re.fullmatch(r"[0-9]{1,20}", value) or not 0 < int(value) < 2**63:
        raise ValueError("Invalid topic ID")
    return str(int(value))


def gh(endpoint, payload=None, paginate=False):
    command = ["gh", "api", endpoint, "-H", "Accept: application/vnd.github+json"]
    if paginate:
        command += ["--paginate", "--slurp"]
    if payload is not None:
        command += ["--method", "POST", "--input", "-"]
    result = subprocess.run(
        command, input=json.dumps(payload) if payload else None, text=True, capture_output=True, check=True, timeout=180
    )
    return json.loads(result.stdout)


def marker(topic):
    return f"<!-- suews-discourse-topic: {HOST}/t/{topic} -->"


def matches(body, topic):
    # Only a terminal footer counts. Copied forum content is followed by the
    # workflow's own footer, so embedded forged markers cannot select an issue.
    if body is not None and not isinstance(body, str):
        raise ValueError("Invalid issue body")
    body = (body or "").rstrip()
    if body.endswith("\n" + marker(topic)):
        return True
    legacy = re.search(
        r"(?:^|\n)\*Created from \[Discourse topic #([0-9]+)\]"
        r"\(https://community\.suews\.io/t/(?:[^/\s()]+/)?([0-9]+)\)\*\Z",
        body,
    )
    return bool(legacy and int(legacy[1]) == int(legacy[2]) == int(topic))


def issue_url(issue, repo):
    number = issue["number"]
    if type(number) is not int or number <= 0 or "pull_request" in issue:
        raise ValueError("Invalid issue response")
    expected = f"https://github.com/{repo}/issues/{number}"
    if not isinstance(issue["html_url"], str) or issue["html_url"].lower() != expected.lower():
        raise ValueError("Unexpected issue URL")
    return expected


def ensure_issue(topic):
    requested = os.environ["TARGET_REPO"]
    if not REPO.fullmatch(requested):
        raise ValueError("Invalid repository")
    # Resolve redirects/renames and casing before either listing or writing.
    repo = gh(f"repos/{requested}")["full_name"]
    if not isinstance(repo, str) or not REPO.fullmatch(repo):
        raise ValueError("Invalid canonical repository")
    pages = gh(f"repos/{repo}/issues?state=all&sort=created&direction=asc&per_page=100", paginate=True)
    if not isinstance(pages, list) or not pages or any(not isinstance(p, list) for p in pages):
        raise ValueError("Incomplete issue listing")
    found = []
    for page in pages:
        for issue in page:
            if not isinstance(issue, dict) or "body" not in issue:
                raise ValueError("Malformed issue listing")
            if "pull_request" not in issue and matches(issue["body"], topic):
                found.append(issue)
    if found:
        # Existing duplicates are left untouched; consistently reuse the oldest.
        return issue_url(min(found, key=lambda issue: issue["number"]), repo)
    body = Path(os.environ["RUNNER_TEMP"], "issue-body.md").read_text(encoding="utf-8")
    body += "\n" + marker(topic) + "\n"
    issue = gh(
        f"repos/{repo}/issues",
        {
            "title": os.environ["ISSUE_TITLE"],
            "body": body,
            "labels": ["0-source:discourse", "1-question"],
        },
    )
    return issue_url(issue, repo)


def discourse(path, payload=None):
    command = [
        "curl",
        "--silent",
        "--show-error",
        "--fail",
        "--max-time",
        "60",
        "-H",
        f"Api-Key: {os.environ['DISCOURSE_API_KEY']}",
        "-H",
        f"Api-Username: {os.environ['DISCOURSE_USERNAME']}",
        HOST + path,
    ]
    if payload is not None:
        command += ["-X", "POST", "-H", "Content-Type: application/json", "--data-binary", "@-"]
    result = subprocess.run(
        command, input=json.dumps(payload) if payload else None, text=True, capture_output=True, check=True, timeout=70
    )
    return json.loads(result.stdout)


class Links(HTMLParser):
    def __init__(self, cooked):
        super().__init__()
        self.urls = []
        self.feed(cooked)

    def handle_starttag(self, tag, attrs):
        if tag == "a":
            self.urls.extend(value for key, value in attrs if key == "href")


def ensure_reply(topic):
    url = os.environ["ISSUE_URL"]
    if not re.fullmatch(r"https://github\.com/[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+/issues/[1-9][0-9]*", url):
        raise ValueError("Invalid issue URL")
    stream = discourse(f"/t/{topic}.json")["post_stream"]["stream"]
    if not isinstance(stream, list) or any(type(i) is not int or i <= 0 for i in stream):
        raise ValueError("Invalid post stream")
    # The topic response embeds only the first page. Inspect every post ID.
    for offset in range(0, len(stream), 20):
        ids = stream[offset : offset + 20]
        query = urlencode({"post_ids[]": ids}, doseq=True)
        posts = discourse(f"/t/{topic}/posts.json?{query}")["post_stream"]["posts"]
        if {p["id"] for p in posts} != set(ids):
            raise ValueError("Incomplete post listing")
        for post in posts:
            if post["username"] == os.environ["DISCOURSE_USERNAME"] and url.lower() in [
                link.lower() for link in Links(post["cooked"]).urls if link
            ]:
                return
    discourse(
        "/posts.json",
        {
            "topic_id": int(topic),
            "raw": (
                "This topic has been tracked as a GitHub issue for the development team.\n\n"
                f"**GitHub issue**: {url}\n\n"
                "The team will follow up there. Updates will be posted in the GitHub issue."
            ),
        },
    )


if __name__ == "__main__":
    try:
        topic = topic_id(os.environ["TOPIC_ID"])
        if sys.argv[1] == "issue":
            url = ensure_issue(topic)
            with open(os.environ["GITHUB_OUTPUT"], "a", encoding="utf-8") as output:
                output.write(f"issue_url={url}\n")
        elif sys.argv[1] == "reply":
            ensure_reply(topic)
        else:
            raise ValueError("Unknown operation")
    except (ValueError, KeyError, TypeError, subprocess.SubprocessError) as error:
        # Do not print API bodies, command arguments/credentials or forum text.
        sys.exit(f"Discourse sync stopped ({type(error).__name__}); inspect before retrying.")
