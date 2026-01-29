---
name: gh-post
description: Post figures to GitHub PRs/Issues. Upload images and create/update sticky comments with embedded visuals.
---

# GH Post

Draft and post GitHub comments with embedded figures on PRs or Issues.

## Workflow

1. **Identify target** from argument or auto-detect from current branch
   - Argument: `PR:1135`, `ISSUE:42`
   - Auto-detect: `gh pr view --json number -q .number`
2. **Collect images** from explicit paths or glob patterns
3. **Upload images** to `gh-artifacts` branch via `scripts/post_figures.py --upload-only`
4. **Draft comment** combining prose/analysis with embedded image Markdown
5. **Present draft** for user review (fenced with `---`)
6. **STOP and ask for approval** -- do NOT proceed until the user explicitly says "yes", "post it", or similar. This is mandatory.
7. **Post** via `gh` CLI only after approval received

## Two Posting Modes

**Figures-only** (quick): Upload images and post a sticky comment with just the figures. Still requires approval -- show the user the image list and target before running.

**Drafted comment with figures** (full workflow): Compose a comment that weaves analysis, explanation, and figures together. Follow the drafting steps below.

## Drafting a Comment with Figures

### Step 1: Upload images

```bash
python scripts/post_figures.py --target PR:1135 --images fig1.png fig2.png --upload-only
```

Capture the Markdown image links from the output.

### Step 2: Draft the comment

Compose a Markdown comment body that includes:
- Context and analysis (what changed, why it matters)
- The uploaded image links inline at appropriate points
- Any supporting data or code references

### Step 3: Present for review

Show the full draft fenced with `---` lines:

**Type**: Comment
**Target**: UMEP-dev/SUEWS#1135
**Figures**: 2 images uploaded

---

[Full draft with embedded `![caption](url)` image links]

---

### Step 4: Ask for approval

**MANDATORY**: After presenting the draft, STOP and explicitly ask:

> "Shall I post this to [target]?"

Do NOT proceed until the user responds with "yes", "post it", "go ahead", or similar affirmative. Any other response means revise or cancel.

### Step 5: Post after approval

```bash
# Comment on issue
gh issue comment OWNER/REPO#NUM --body "BODY"

# Comment on PR
gh pr comment OWNER/REPO#NUM --body "BODY"
```

## Style Guide

Choose style based on context. Default to **hybrid** (conversational framing + structured content) for figure-heavy posts.

- **Conversational**: Prose paragraphs for discussion, community responses
- **Structured**: Headers, bullets, code blocks for technical analysis
- **Hybrid**: Conversational framing + structured figures/data. Best for "here's what changed and why"

Example hybrid comment with figures:

```
This PR fixes the grass albedo calculation (see #1134). The model was incorrectly increasing albedo with LAI for grass surfaces.

**Impact**

The fix reverses the albedo-LAI relationship for grass, bringing it in line with the scientific literature. Key changes in the sample output:

![Albedo comparison](https://raw.githubusercontent.com/.../albedo_comparison.png)

Albedo now decreases as LAI increases for grass, while deciduous and evergreen behaviour remains unchanged.

![Energy balance difference](https://raw.githubusercontent.com/.../energy_balance_diff.png)

The net effect on energy balance is modest (< 5 W/m2) but physically correct.
```

## Script Reference

```bash
# Post figures as sticky comment
python scripts/post_figures.py --target PR:1135 --images fig1.png fig2.png

# Upload only (get Markdown links for drafted comment)
python scripts/post_figures.py --target PR:1135 --images fig1.png --upload-only

# With title
python scripts/post_figures.py --target ISSUE:42 --images *.png --title "Energy balance"

# Clean up uploaded artifacts
python scripts/post_figures.py --target PR:1135 --cleanup
```

## Safety Rules

**ALWAYS**:
- Show the full draft (with figure links) before posting
- Wait for explicit "yes" / "post it" before executing
- Report the comment URL after posting

**NEVER**:
- Post without user confirmation
- Upload without telling the user which files

## After Review

- "yes" / "post it" -> Post with `gh` CLI
- "change X" -> Revise and show again
- "copy" -> Use `/copy-rich` for clipboard
- "save" -> Write to file

## GH Post Conventions

- No sign-off (GH shows author automatically)
- Use @handles not names
- Keep it concise and direct
- No "happy to discuss" / "feel free to" phrases

## References

- `references/usage-examples.md` - Detailed examples and patterns
