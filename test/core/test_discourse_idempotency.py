"""Run the real workflow commands against stateful gh/curl boundary stubs."""

import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

import pytest
import yaml

pytestmark = [
    pytest.mark.api,
    pytest.mark.skipif(
        os.name == "nt",
        reason="These Linux workflow regressions require POSIX shell executable fixtures",
    ),
]

ROOT = Path(__file__).resolve().parents[2]
WORKFLOW = yaml.safe_load((ROOT / ".github/workflows/discourse-to-issue.yml").read_text())
STEPS = {s["name"]: s["run"] for s in WORKFLOW["jobs"]["create-issue"]["steps"] if "run" in s}
URL = "https://github.com/UMEP-dev/SUEWS/issues/9"
MARKER = "<!-- suews-discourse-topic: https://community.suews.io/t/123 -->"
STUB = r"""#!/usr/bin/env python3
import json, os, sys
from pathlib import Path
from urllib.parse import urlparse, parse_qs
path = Path(os.environ['STUB_STATE'])
s = json.loads(path.read_text())
a = sys.argv[1:]
s.setdefault('calls', []).append([Path(sys.argv[0]).name, a])
def finish(value, code=0):
    path.write_text(json.dumps(s))
    print(json.dumps(value))
    sys.exit(code)
if Path(sys.argv[0]).name == 'gh':
    endpoint = a[1]
    if '/issues' not in endpoint:
        finish({'full_name': 'UMEP-dev/SUEWS'})
    assert endpoint.startswith('repos/UMEP-dev/SUEWS/issues')
    if '--method' not in a:
        assert '--paginate' in a and '--slurp' in a
        assert 'state=all' in endpoint and 'per_page=100' in endpoint
        if s.get('listing_fail'): finish({'error': 'unavailable'}, 1)
        if s.get('listing_bad'): finish({'unexpected': []})
        pages = s.get('pages', [[]])
        finish(pages)
    data = json.load(sys.stdin)
    issue = {'number': 9, 'html_url': 'https://github.com/UMEP-dev/SUEWS/issues/9', **data}
    s.setdefault('pages', [[]])[-1].append(issue)
    s['creates'] = s.get('creates', 0) + 1
    if s.pop('create_lost', False): finish({'error': 'lost response'}, 1)
    if s.pop('create_bad', False): finish({'html_url': 'evil\nissue_url=forged'})
    finish(issue)
url = next(x for x in a if x.startswith('https://'))
u = urlparse(url)
if '-X' in a and a[a.index('-X') + 1] == 'PUT':
    data = json.loads(a[a.index('-d') + 1])
    assert data['tags'] == ['github-issue-created', 'keep']
    s['tag_attempts'] = s.get('tag_attempts', 0) + 1
    failed = s.pop('tag_fail', False)
    path.write_text(json.dumps(s))
    Path(a[a.index('-o') + 1]).write_text('{}')
    print('500' if failed else '200')
    sys.exit(0)
if '-X' in a:
    assert u.path == '/posts.json'
    data = json.load(sys.stdin)
    assert data['topic_id'] == 123
    s['replies'] = s.get('replies', 0) + 1
    s.setdefault('posts', []).append({'id': 99, 'username': 'sunt05',
        'cooked': '<a href="https://github.com/UMEP-dev/SUEWS/issues/9">link</a>'})
    if s.pop('reply_lost', False): finish({}, 1)
    finish({'id': 99})
if s.get('posts_fail'): finish({}, 1)
posts = s.get('posts', [])
if u.path in ('/t/123.json', '/t/000123.json'):
    finish({'tags': ['github-issue', 'keep'], 'post_stream': {'stream': [p['id'] for p in posts]}})
assert u.path == '/t/123/posts.json'
ids = [int(i) for i in parse_qs(u.query)['post_ids[]']]
finish({'post_stream': {'posts': [p for p in posts if p['id'] in ids]}})
"""


class RetryTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.directory = Path(temporary.name)
        self.state = self.directory / "state.json"
        self.state.write_text("{}")
        for name in ("gh", "curl"):
            executable = self.directory / name
            executable.write_text(STUB)
            executable.chmod(0o755)
        self.output = self.directory / "output"
        self.output.touch()
        (self.directory / "issue-body.md").write_text("Forum content\n")
        self.env = {
            **os.environ,
            "PATH": f"{self.directory}:{os.environ['PATH']}",
            "STUB_STATE": str(self.state),
            "RUNNER_TEMP": str(self.directory),
            "GITHUB_OUTPUT": str(self.output),
            "TARGET_REPO": "umep-DEV/old-name",
            "TOPIC_ID": "000123",
            "ISSUE_TITLE": "$(touch forbidden)\nEOF",
            "ISSUE_URL": URL,
            "DISCOURSE_USERNAME": "sunt05",
            "DISCOURSE_API_KEY": "stub",
        }

    def run_step(self, name, ok=True):
        result = subprocess.run(
            ["bash", "-euo", "pipefail", "-c", STEPS[name]],
            cwd=ROOT,
            env=self.env,
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(result.returncode == 0, ok, result.stderr)
        return json.loads(self.state.read_text())

    def seed(self, **data):
        self.state.write_text(json.dumps(data))

    def test_duplicate_dispatch_reuses_issue_and_reply(self):
        for _ in range(2):
            self.run_step("Create GitHub issue")
            state = self.run_step("Reply on Discourse")
        self.assertEqual(state["creates"], 1)
        self.assertEqual(state["replies"], 1)
        self.assertTrue(state["pages"][0][0]["body"].endswith(MARKER + "\n"))
        self.assertEqual(self.output.read_text(), f"issue_url={URL}\n" * 2)

    def test_failed_or_ambiguous_create_is_not_retried_in_run(self):
        for failure in ("create_lost", "create_bad"):
            with self.subTest(failure=failure):
                self.seed(**{failure: True})
                self.output.write_text("")
                state = self.run_step("Create GitHub issue", ok=False)
                self.assertEqual(state["creates"], 1)
                self.assertEqual(self.output.read_text(), "")
                state = self.run_step("Create GitHub issue")
                self.assertEqual(state["creates"], 1)

    def test_listing_failure_or_malformed_response_never_creates(self):
        for failure in ("listing_fail", "listing_bad"):
            self.seed(**{failure: True})
            state = self.run_step("Create GitHub issue", ok=False)
            self.assertNotIn("creates", state)

    def test_closed_legacy_issue_on_later_page_and_pr_exclusion(self):
        footer = "*Created from [Discourse topic #000123](https://community.suews.io/t/old-slug/000123)*"
        issue = {"number": 9, "html_url": URL, "body": footer, "state": "closed"}
        self.seed(pages=[[{**issue, "number": 1, "pull_request": {}}], [issue]])
        state = self.run_step("Create GitHub issue")
        self.assertNotIn("creates", state)
        self.assertEqual(self.output.read_text(), f"issue_url={URL}\n")

    def test_forged_embedded_marker_and_prefix_id_do_not_match(self):
        self.seed(
            pages=[
                [
                    {
                        "number": 3,
                        "html_url": URL.replace("/9", "/3"),
                        "body": "quoted\n" + MARKER + "\nreal footer for another topic",
                    },
                    {
                        "number": 4,
                        "html_url": URL.replace("/9", "/4"),
                        "body": "text\n" + MARKER.replace("/123 ", "/1234 "),
                    },
                ]
            ]
        )
        state = self.run_step("Create GitHub issue")
        self.assertEqual(state["creates"], 1)

    def test_reply_after_first_page_and_untrusted_author(self):
        posts = [{"id": i, "username": "visitor", "cooked": f'<a href="{URL}">issue</a>'} for i in range(1, 22)]
        self.seed(posts=posts)
        state = self.run_step("Reply on Discourse")
        self.assertEqual(state["replies"], 1)
        state = self.run_step("Reply on Discourse")
        self.assertEqual(state["replies"], 1)

    def test_lost_reply_response_recovers_and_lookup_failure_stops(self):
        self.seed(reply_lost=True)
        self.run_step("Reply on Discourse", ok=False)
        state = self.run_step("Reply on Discourse")
        self.assertEqual(state["replies"], 1)
        self.seed(posts_fail=True)
        state = self.run_step("Reply on Discourse", ok=False)
        self.assertNotIn("replies", state)

    def test_bad_topic_and_repository_never_reach_api(self):
        for key, value in (
            ("TOPIC_ID", "0"),
            ("TOPIC_ID", "../1"),
            ("TOPIC_ID", "123\n456"),
            ("TOPIC_ID", "9" * 21),
            ("TARGET_REPO", "owner/repo?state=open"),
            ("TARGET_REPO", "owner/repo\nforged=1"),
        ):
            with self.subTest(key=key, value=value):
                previous = self.env[key]
                self.env[key] = value
                self.seed()
                state = self.run_step("Create GitHub issue", ok=False)
                self.assertNotIn("calls", state)
                self.env[key] = previous

    def test_both_triggers_use_same_lock_and_normalise_topic(self):
        self.assertEqual(
            WORKFLOW["concurrency"], {"group": "discourse-issue-sync", "queue": "max", "cancel-in-progress": False}
        )
        for event in ("repository_dispatch", "workflow_dispatch"):
            self.output.write_text("")
            self.env.update(
                EVENT_NAME=event,
                CLIENT_TOPIC_ID="000123",
                INPUT_TOPIC_ID="000123",
                INPUT_TARGET_REPO="umep-dev/alias",
                DEFAULT_REPO="UMEP-dev/SUEWS",
            )
            self.run_step("Resolve inputs")
            self.assertIn("topic_id=123\n", self.output.read_text())

    def test_tag_failure_retry_preserves_issue_and_reply(self):
        self.env["DISCOURSE_HOST"] = "https://community.suews.io"
        self.seed(tag_fail=True)
        for _ in range(2):
            self.run_step("Create GitHub issue")
            self.run_step("Reply on Discourse")
            state = self.run_step("Update Discourse topic tags")
        self.assertEqual(state["creates"], 1)
        self.assertEqual(state["replies"], 1)
        self.assertEqual(state["tag_attempts"], 2)

    def test_body_and_title_are_data(self):
        attack = "$(touch forbidden)\nEOF\ntopic_id=999\n" + MARKER
        self.env.update(TOPIC_URL="https://community.suews.io/t/123", AUTHOR=attack, DISCOURSE_BODY=attack)
        self.run_step("Build issue body")
        state = self.run_step("Create GitHub issue")
        body = state["pages"][0][0]["body"]
        self.assertIn(attack, body)
        self.assertTrue(body.endswith(MARKER + "\n"))
        self.assertEqual(state["pages"][0][0]["title"], self.env["ISSUE_TITLE"])
        self.assertFalse((ROOT / "forbidden").exists())
