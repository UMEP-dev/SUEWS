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

    if (!env.DISCOURSE_WEBHOOK_SECRET || !env.GITHUB_PAT) {
      return new Response("Webhook configuration unavailable", { status: 503 });
    }

    let body;
    try {
      body = await request.text();
    } catch {
      return new Response("Unreadable request body", { status: 400 });
    }

    // 1. Verify Discourse webhook signature
    const signature = request.headers.get("X-Discourse-Event-Signature");
    if (!signature) {
      return new Response("Missing signature", { status: 401 });
    }

    if (!/^sha256=[0-9a-fA-F]{64}$/.test(signature)) {
      return new Response("Invalid signature", { status: 403 });
    }
    const signatureBytes = Uint8Array.from(
      signature.slice(7).match(/../g), (byte) => parseInt(byte, 16)
    );
    let verified;
    try {
      const key = await crypto.subtle.importKey(
        "raw",
        new TextEncoder().encode(env.DISCOURSE_WEBHOOK_SECRET),
        { name: "HMAC", hash: "SHA-256" },
        false,
        ["verify"]
      );
      verified = await crypto.subtle.verify(
        "HMAC", key, signatureBytes, new TextEncoder().encode(body)
      );
    } catch {
      return new Response("Signature verification unavailable", { status: 503 });
    }
    if (!verified) {
      return new Response("Invalid signature", { status: 403 });
    }

    // Only topic creation and edits can request an issue. In particular,
    // deletion/recovery and post events must not create issues.
    const event = request.headers.get("X-Discourse-Event");
    if (!["topic_created", "topic_edited", "topic_revised"].includes(event)) {
      return new Response("Unsupported event, skipping", { status: 200 });
    }

    // 2. Parse payload and validate the fields used below.
    let payload;
    try {
      payload = JSON.parse(body);
    } catch {
      return new Response("Invalid JSON", { status: 400 });
    }
    const topic = payload?.topic;
    if (!topic || !Number.isSafeInteger(topic.id) || topic.id <= 0 ||
        typeof topic.slug !== "string" || !Array.isArray(topic.tags) ||
        !topic.tags.every((tag) => typeof tag === "string" ||
          (tag && typeof tag.name === "string"))) {
      return new Response("Invalid topic payload", { status: 400 });
    }

    // 3. Best-effort tag guard only: concurrent deliveries can still dispatch
    // twice before the workflow adds github-issue-created.
    const tags = topic.tags.map((tag) => typeof tag === "string" ? tag : tag.name);
    if (!tags.includes("github-issue")) {
      return new Response("Tag github-issue not present, skipping", { status: 200 });
    }
    if (tags.includes("github-issue-created")) {
      return new Response("Already processed (github-issue-created present)", { status: 200 });
    }

    const topicId = topic.id;
    const topicUrl = `https://community.suews.io/t/${encodeURIComponent(topic.slug)}/${topicId}`;

    // 4. POST to GitHub repository_dispatch
    let ghResponse;
    try {
      ghResponse = await fetch(
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
    } catch {
      return new Response("GitHub API unavailable", { status: 502 });
    }

    if (!ghResponse.ok) {
      return new Response(`GitHub API error: ${ghResponse.status}`, {
        status: 502,
      });
    }

    return new Response("Dispatched", { status: 200 });
  },
};
