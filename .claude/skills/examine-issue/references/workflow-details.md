# Workflow Details

## Step 1: Parse Issue Number

| Pattern | Action |
|---------|--------|
| `123` or `#123` | Use `gh issue view 123` |

## Step 2: Gather Issue Context

```bash
# Full issue details
gh issue view {number} --json title,body,labels,assignees,state,milestone,comments,createdAt,updatedAt

# Related PRs (if any)
gh pr list --search "fixes #{number}" --json number,title,state

# Issue timeline
gh api repos/{owner}/{repo}/issues/{number}/timeline --jq '.[] | {event, created_at, actor: .actor.login}'
```

### Summarise Discussion

When comments exist, provide:

```
=== DISCUSSION SUMMARY ===

**Timeline**: [date range]
**Participants**: @user1, @user2

**Key Points Raised**:
1. @user1: [observation]
2. @user2: [context]

**Emerging Consensus** (if any):
- [agreed approach]
- [outstanding questions]

**Open Questions**:
- [unresolved 1]
- [unresolved 2]
```

Focus on: technical insights, proposed solutions, consensus vs disagreement, blockers.

Skip if: no comments, or purely administrative.

---

## Step 3: Assess Complexity

Present:
```
=== ISSUE ASSESSMENT ===

Issue: [identifier] - [title]
Type: [Simple/Complex]
Reason: [1-2 sentence justification]

Key Points:
- [point 1]
- [point 2]
```

---

## Step 4: Interactive Co-working (Complex)

### 4.1 Clarifying Questions

```
=== CLARIFYING QUESTIONS ===

Before we proceed:

1. [Scope question]
2. [Constraints question]
3. [Dependencies question]

Take your time - answers shape the approach.
```

### 4.2 Exploration Phase

- Find related code and patterns
- Identify affected files
- Understand existing implementations
- Note risks or conflicts

### 4.3 Approach Options

```
=== APPROACH OPTIONS ===

**Option A: [Name]**
- Description: [1-2 sentences]
- Pros: [advantages]
- Cons: [disadvantages]
- Effort: [relative]

**Option B: [Name]**
- Description: [1-2 sentences]
- Pros: [advantages]
- Cons: [disadvantages]
- Effort: [relative]

**Recommendation**: Option [X] because [reason]
```

### 4.4 Iterative Refinement

Continue until:
- Approach agreed
- Implementation steps clear
- Issues identified
- User ready

### 4.5 Breaking Down into Sub-Issues

See `breakdown-strategies.md` for when and how.

```
=== PROPOSED BREAKDOWN ===

Parent: #[number] - [title]

Sub-issues:
1. **[Title]** - Scope: [x], Dependencies: [y], Complexity: [z]
2. **[Title]** - Scope: [x], Dependencies: [y], Complexity: [z]

=== APPROVAL REQUIRED ===

Reply: `create all` | `create N` | `modify` | `skip`
```

```bash
# Create sub-issues
gh sub-issue create <parent-number>
gh sub-issue add <parent-number> <child-number>
gh sub-issue list <parent-number>
```

---

## Step 5: Direct Suggestions (Simple)

```
=== ANALYSIS ===

Issue: [identifier] - [title]

**Problem Statement**
[Description]

**Root Cause** (if applicable)
[Cause]

**Affected Areas**
- [file/module 1]: [effect]
- [file/module 2]: [effect]

---

=== SUGGESTED APPROACH ===

**Implementation Steps**
1. [Step with file:line]
2. [Step with file:line]

**Testing Strategy**
- [Verification method]
- [Edge cases]

**Potential Risks**
- [Risk 1 and mitigation]

---

=== NEXT STEPS ===

Options: `proceed` | `modify` | `questions` | `defer`
```

---

## Step 6: Draft Actions (Approval Required)

### Comments

```
=== DRAFT COMMENT ===

Target: GitHub issue #X
Type: [Comment / Status Update / Label Change]

Content:
---
[Exact text]
---

=== APPROVAL REQUIRED ===

Reply: `post` | `edit` | `cancel`
```

### Code Changes

```
=== DRAFT CHANGES ===

Files to modify:
1. [file1.py]: [summary]
2. [file2.py]: [summary]

=== APPROVAL REQUIRED ===

Reply: `implement` | `review` | `modify` | `cancel`
```
