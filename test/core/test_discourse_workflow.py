"""Execute the workflow's actual input/output shell blocks with mocked curl."""
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

WORKFLOW = Path(__file__).resolve().parents[2] / ".github/workflows/discourse-to-issue.yml"
STEPS = yaml.safe_load(WORKFLOW.read_text(encoding="utf-8"))["jobs"]["create-issue"]["steps"]


def outputs(text):
    result = {}
    lines = iter(text.splitlines())
    for line in lines:
        if "<<" in line:
            name, delimiter = line.split("<<", 1)
            value = []
            for value_line in lines:
                if value_line == delimiter:
                    break
                value.append(value_line)
            else:
                raise AssertionError("Unterminated output")
            result[name] = "\n".join(value)
        else:
            name, value = line.split("=", 1)
            result[name] = value
    return result


class DiscourseTests(unittest.TestCase):
    @staticmethod
    def run_step(name, env, prefix=""):
        script = next(step["run"] for step in STEPS if step["name"] == name)
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory) / "output"
            output.touch()
            result = subprocess.run(["bash", "-euo", "pipefail", "-c", prefix + script],
                                    env={**os.environ, **env, "GITHUB_OUTPUT": str(output)},
                                    capture_output=True, text=True, check=False)
            return result, output.read_text(encoding="utf-8")

    def test_topic_id_guard_for_both_triggers(self):
        for event in ("repository_dispatch", "workflow_dispatch"):
            for topic in ("123", "", "../123", "123?x=y", "1\ntopic_id=2", "1.5", "-1"):
                with self.subTest(event=event, topic=topic):
                    result, output = self.run_step("Resolve inputs", {
                        "EVENT_NAME": event, "CLIENT_TOPIC_ID": topic,
                        "INPUT_TOPIC_ID": topic, "INPUT_TARGET_REPO": "",
                        "DEFAULT_REPO": "UMEP-dev/SUEWS"})
                    self.assertEqual(result.returncode, 0 if topic == "123" else 1)
                    if topic != "123":
                        self.assertEqual(output, "")
                    else:
                        self.assertEqual(outputs(output)["topic_id"], "123")

    def test_forum_text_cannot_forge_outputs(self):
        attack = "hello\nDISCOURSE_EOF\ntopic_id=999\nauthor=forged"
        payload = json.dumps({"title": attack, "slug": "topic", "post_stream": {
            "posts": [{"cooked": attack, "username": attack}]}})
        results = []
        for _ in range(2):
            result, output = self.run_step("Fetch Discourse topic", {
                "TOPIC_ID": "123", "DISCOURSE_API_KEY": "test-only",
                "DISCOURSE_USERNAME": "test", "DISCOURSE_HOST": "https://example.invalid",
                "MOCK_RESPONSE": payload}, 'curl() { printf "%s" "$MOCK_RESPONSE"; }\n')
            self.assertEqual(result.returncode, 0, result.stderr)
            parsed = outputs(output)
            self.assertEqual(parsed["topic_id"], "123")
            for key in ("title", "author", "body"):
                self.assertEqual(parsed[key], attack)
            self.assertEqual(set(parsed), {"topic_id", "title", "author", "body", "topic_url"})
            results.append(output)
        self.assertNotEqual(results[0], results[1])
