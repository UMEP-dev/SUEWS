/**
 * Discourse → GitHub Dispatch Worker
 *
 * Bridges Discourse webhook events to GitHub repository_dispatch.
 * Discourse can't customise webhook payload format or add Authorization headers,
 * so this worker translates the payload and authenticates with GitHub.
 *
 * Secrets (set via `wrangler secret put`):
 *   DISCOURSE_WEBHOOK_SECRET - shared secret for HMAC-SHA256 verification
 *   GITHUB_PAT              - GitHub PAT with repo scope
 */

export default {
  async fetch(request, env) {
    if (request.method !== "POST") {
      return new Response("Method not allowed", { status: 405 });
    }

    const body = await request.text();

    // 1. Verify Discourse webhook signature
    const signature = request.headers.get("X-Discourse-Event-Signature");
    if (!signature) {
      return new Response("Missing signature", { status: 401 });
    }

    const key = await crypto.subtle.importKey(
      "raw",
      new TextEncoder().encode(env.DISCOURSE_WEBHOOK_SECRET),
      { name: "HMAC", hash: "SHA-256" },
      false,
      ["sign"]
    );
    const expected = await crypto.subtle.sign("HMAC", key, new TextEncoder().encode(body));
    const expectedHex = "sha256=" + [...new Uint8Array(expected)]
      .map((b) => b.toString(16).padStart(2, "0"))
      .join("");

    if (signature !== expectedHex) {
      return new Response("Invalid signature", { status: 403 });
    }

    // 2. Parse payload and extract topic info
    const payload = JSON.parse(body);
    const topic = payload.topic;
    if (!topic) {
      return new Response("No topic in payload", { status: 200 });
    }

    // 3. Deduplication: only forward if github-issue tag present and not already processed
    const tags = (topic.tags || []).map((t) => (typeof t === "string" ? t : t.name || ""));
    if (!tags.includes("github-issue")) {
      return new Response("Tag github-issue not present, skipping", { status: 200 });
    }
    if (tags.includes("github-issue-created")) {
      return new Response("Already processed (github-issue-created present)", { status: 200 });
    }

    const topicId = topic.id;
    const topicUrl = `https://community.suews.io/t/${topic.slug}/${topicId}`;

    // 4. POST to GitHub repository_dispatch
    const ghResponse = await fetch(
      "https://api.github.com/repos/UMEP-dev/SUEWS/dispatches",
      {
        method: "POST",
        headers: {
          Authorization: `token ${env.GITHUB_PAT}`,
          Accept: "application/vnd.github.v3+json",
          "User-Agent": "discourse-to-github-worker",
          "Content-Type": "application/json",
        },
        body: JSON.stringify({
          event_type: "discourse-topic",
          client_payload: {
            topic_id: topicId,
            topic_url: topicUrl,
          },
        }),
      }
    );

    if (!ghResponse.ok) {
      const errText = await ghResponse.text();
      return new Response(`GitHub API error: ${ghResponse.status} ${errText}`, {
        status: 502,
      });
    }

    return new Response("Dispatched", { status: 200 });
  },
};
