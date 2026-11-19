import assert from "node:assert/strict";
import { createHmac, webcrypto } from "node:crypto";
import { readFile } from "node:fs/promises";
import { test } from "node:test";

// Load the deployment module without changing its module/package configuration.
const source = await readFile(new URL("../../scripts/cloudflare-worker/worker.js", import.meta.url), "utf8");
const { default: worker } = await import(`data:text/javascript;base64,${Buffer.from(source).toString("base64")}`);
Object.defineProperty(globalThis, "crypto", { value: webcrypto, configurable: true });
const env = { DISCOURSE_WEBHOOK_SECRET: "test-only", GITHUB_PAT: "test-only" };
const topic = { id: 123, slug: "topic", tags: ["github-issue"] };

async function request(payload = { topic }, options = {}) {
  const body = typeof payload === "string" ? payload : JSON.stringify(payload);
  const signature = "sha256=" + createHmac("sha256", env.DISCOURSE_WEBHOOK_SECRET).update(body).digest("hex");
  const headers = { "X-Discourse-Event": "topic_edited", "X-Discourse-Event-Signature": signature, ...options.headers };
  if (options.missingSignature) delete headers["X-Discourse-Event-Signature"];
  return worker.fetch(new Request("https://example.invalid", { method: "POST", body, headers }), options.env ?? env);
}

test("webhook boundary and dispatch behaviour", async () => {
  let calls = [];
  globalThis.fetch = async (...args) => { calls.push(args); return new Response(null, { status: 204 }); };
  assert.equal((await worker.fetch(new Request("https://example.invalid"), env)).status, 405);
  assert.equal((await request(undefined, { missingSignature: true })).status, 401);
  for (const signature of ["sha256=00", "sha256=" + "gg".repeat(32), "sha256=" + "00".repeat(32), "bad"])
    assert.equal((await request(undefined, { headers: { "X-Discourse-Event-Signature": signature } })).status, 403);
  assert.equal((await request("{")).status, 400);
  for (const payload of [null, {}, { topic: { ...topic, id: "../123" } }, { topic: { ...topic, tags: [null] } }, { topic: { ...topic, tags: "github-issue" } }])
    assert.equal((await request(payload)).status, 400);
  for (const event of ["topic_destroyed", "post_created", "", "topic_recovered"])
    assert.equal((await request(undefined, { headers: { "X-Discourse-Event": event } })).status, 200);
  for (const tags of [[], ["github-issue", "github-issue-created"]])
    assert.equal((await request({ topic: { ...topic, tags } })).status, 200);
  assert.equal((await request(undefined, { env: {} })).status, 503);
  assert.equal(calls.length, 0);
  for (const event of ["topic_created", "topic_edited", "topic_revised"]) {
    assert.equal((await request({ topic: { ...topic, tags: [{ name: "github-issue" }] } }, { headers: { "X-Discourse-Event": event } })).status, 200);
  }
  assert.equal(calls.length, 3);
  assert.deepEqual(JSON.parse(calls[0][1].body), { event_type: "discourse-topic", client_payload: { topic_id: 123, topic_url: "https://community.suews.io/t/topic/123" } });
  globalThis.fetch = async () => { throw new Error("private transport details"); };
  let response = await request();
  assert.equal(response.status, 502);
  assert.equal(await response.text(), "GitHub API unavailable");
  globalThis.fetch = async () => new Response("private upstream details", { status: 503 });
  response = await request();
  assert.equal(response.status, 502);
  assert.equal(await response.text(), "GitHub API error: 503");
  Object.defineProperty(globalThis, "crypto", { value: { subtle: {
    importKey: async () => { throw new Error("crypto failure"); }
  } }, configurable: true });
  assert.equal((await request()).status, 503);
  Object.defineProperty(globalThis, "crypto", { value: webcrypto, configurable: true });
  const brokenBody = new Request("https://example.invalid", { method: "POST", body: "test" });
  await brokenBody.text();
  assert.equal((await worker.fetch(brokenBody, env)).status, 400);
});
