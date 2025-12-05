---
name: examine-issue
description: Examine GitHub issues carefully and provide actionable suggestions. Use when asked to analyse, investigate, or help with GitHub issues. Supports both interactive co-working mode for complex issues and direct suggestions for simpler ones. CRITICAL - Never posts comments, updates, or changes without explicit human approval.
---

# Examine Issue

Thorough issue analysis with interactive suggestions. All actions require explicit approval.

## Quick Reference

```bash
# GitHub issue
gh issue view <number> --json title,body,labels,assignees,state,comments
```

---

## Step 1: Parse Issue Number

Extract the issue number from the identifier:

| Pattern | Action |
|---------|--------|
| `123` or `#123` | Use `gh issue view 123` |

---

## Step 2: Gather Issue Context

```bash
# Full issue details
gh issue view {number} --json title,body,labels,assignees,state,milestone,comments,createdAt,updatedAt

# Related PRs (if any)
gh pr list --search "fixes #{number}" --json number,title,state

# Issue timeline
gh api repos/{owner}/{repo}/issues/{number}/timeline --jq '.[] | {event, created_at, actor: .actor.login}'
```

### Summarise Discussion (if comments exist)

When an issue has comments, especially development-related discussion, provide a structured summary:

```
=== DISCUSSION SUMMARY ===

**Timeline**: [date range of discussion]
**Participants**: @user1, @user2, @user3

**Key Points Raised**:
1. @user1: [main observation or concern]
2. @user2: [additional context or question]
3. @user3: [proposed solution or direction]

**Emerging Consensus** (if any):
- [agreed approach or solution]
- [outstanding questions]

**Proposed Actions** (from discussion):
- [action 1 suggested by participants]
- [action 2 suggested by participants]

**Open Questions**:
- [unresolved question 1]
- [unresolved question 2]
```

Focus on:
- **Technical insights**: Code references, root cause analysis shared by participants
- **Proposed solutions**: Concrete suggestions from domain experts
- **Consensus vs disagreement**: Note where participants agree/diverge
- **Blockers**: Dependencies or questions blocking progress

Skip this section if:
- No comments exist
- Comments are purely administrative (assignments, labels)

---

## Step 3: Assess Complexity

Classify the issue using these criteria:

### Simple Issue (Direct Suggestions)

Characteristics:
- Clear, single problem statement
- Obvious solution path
- Limited scope (1-3 files)
- No architectural decisions needed
- Well-defined acceptance criteria

Action: Proceed to **Step 5: Direct Suggestions**

### Complex Issue (Interactive Co-working)

Characteristics:
- Ambiguous requirements
- Multiple valid approaches
- Cross-cutting concerns
- Architectural implications
- Missing context or specifications
- Dependencies on other work

Action: Proceed to **Step 4: Interactive Co-working**

### Assessment Output

Present complexity assessment:

```
=== ISSUE ASSESSMENT ===

Issue: [identifier] - [title]
Type: [Simple/Complex]

Reason: [1-2 sentence justification]

Key Points:
- [point 1]
- [point 2]
- [point 3]
```

---

## Step 4: Interactive Co-working (Complex Issues)

For complex issues, engage in structured dialogue:

### 4.1 Clarifying Questions

Ask focused questions to understand:
- **Scope**: What is in/out of scope?
- **Constraints**: Technical, timeline, or resource constraints?
- **Dependencies**: What must be done first or in parallel?
- **Acceptance**: How will we know it's complete?

Format:
```
=== CLARIFYING QUESTIONS ===

Before we proceed, I need to understand:

1. [Question about scope or requirements]
2. [Question about constraints or priorities]
3. [Question about dependencies or context]

Take your time - answers to these will shape the approach.
```

### 4.2 Exploration Phase

After gathering answers, explore the codebase:
- Find related code and patterns
- Identify affected files and modules
- Understand existing implementations
- Note potential risks or conflicts

### 4.3 Approach Options

Present 2-3 approaches with trade-offs:

```
=== APPROACH OPTIONS ===

Based on our discussion, here are the viable approaches:

**Option A: [Name]**
- Description: [1-2 sentences]
- Pros: [key advantages]
- Cons: [key disadvantages]
- Effort: [relative effort]

**Option B: [Name]**
- Description: [1-2 sentences]
- Pros: [key advantages]
- Cons: [key disadvantages]
- Effort: [relative effort]

**Recommendation**: Option [X] because [reason]

Which approach would you like to explore further?
```

### 4.4 Iterative Refinement

Continue dialogue until:
- Approach is agreed upon
- Implementation steps are clear
- Potential issues are identified
- User is ready for suggestions

### 4.5 Breaking Down into Sub-Issues

For issues that are too large for single implementation, suggest decomposition.

**When to suggest breakdown:**
- Issue spans multiple components or modules
- Estimated effort exceeds 1-2 days
- Multiple independent workstreams identified
- Different expertise required for different parts
- Risk of scope creep without clear boundaries

**Breakdown Strategies:**

| Strategy | When to Use | Example |
|----------|-------------|---------|
| By Component | Issue touches multiple modules | Physics, Data Model, Docs as separate sub-issues |
| By Phase | Sequential dependencies | Design → Implement → Test → Document |
| By Risk | Mix of safe and risky changes | Low-risk refactoring first, then risky physics changes |
| By Expertise | Different skills needed | Fortran core vs Python wrapper vs User docs |
| By Priority | Some parts more urgent | Critical bug fix now, enhancements later |

**Draft Sub-Issue Breakdown:**

```
=== PROPOSED BREAKDOWN ===

Parent: #[number] - [title]

Sub-issues to create:

1. **[Sub-issue title 1]**
   - Scope: [what's included]
   - Dependencies: [what must be done first]
   - Effort: [relative size]

2. **[Sub-issue title 2]**
   - Scope: [what's included]
   - Dependencies: [depends on #1 or independent]
   - Effort: [relative size]

3. **[Sub-issue title 3]**
   - Scope: [what's included]
   - Dependencies: [depends on #1, #2, or independent]
   - Effort: [relative size]

Suggested order: [1] → [2] → [3] (or parallel if independent)

=== APPROVAL REQUIRED ===

Reply with:
- `create all` - Create all sub-issues and link to parent
- `create N` - Create only sub-issue N
- `modify` - Adjust the breakdown
- `skip` - Don't create sub-issues, work on parent directly
```

**Creating Sub-Issues (after approval):**

```bash
# Create new sub-issue under parent
gh sub-issue create <parent-number>

# Or link existing issue as sub-issue
gh sub-issue add <parent-number> <child-number>

# List sub-issues for a parent
gh sub-issue list <parent-number>
```

**Post-Creation Update:**

After creating sub-issues, update the parent issue body with a tracking checklist:

```markdown
## Sub-Issues

- [ ] #[child1] - [title]
- [ ] #[child2] - [title]
- [ ] #[child3] - [title]
```

---

## Step 5: Direct Suggestions (Simple Issues)

For simple issues, provide structured suggestions:

```
=== ANALYSIS ===

Issue: [identifier] - [title]

**Problem Statement**
[Clear description of what needs to be done]

**Root Cause** (if applicable)
[What's causing the issue]

**Affected Areas**
- [file/module 1]: [how it's affected]
- [file/module 2]: [how it's affected]

---

=== SUGGESTED APPROACH ===

**Implementation Steps**
1. [Step with specific file:line references]
2. [Step with specific file:line references]
3. [Step with specific file:line references]

**Testing Strategy**
- [How to verify the fix]
- [Edge cases to consider]

**Potential Risks**
- [Risk 1 and mitigation]
- [Risk 2 and mitigation]

---

=== NEXT STEPS ===

Ready to proceed? Options:
- `proceed` - Start implementation
- `modify` - Adjust the approach
- `questions` - Ask clarifying questions
- `defer` - Save for later
```

---

## Step 6: Draft Actions (Approval Required)

When ready to take action, ALWAYS draft first:

### Drafting Comments

```
=== DRAFT COMMENT ===

Target: GitHub issue #X
Type: [Comment / Status Update / Label Change]

Content:
---
[Exact text of proposed comment]
---

=== APPROVAL REQUIRED ===

Reply with:
- `post` - Post as drafted
- `edit` - Provide edited version
- `cancel` - Don't post
```

### Drafting Code Changes

```
=== DRAFT CHANGES ===

Files to modify:
1. [file1.py]: [summary of changes]
2. [file2.py]: [summary of changes]

=== APPROVAL REQUIRED ===

Reply with:
- `implement` - Proceed with changes
- `review` - Show detailed diff first
- `modify` - Adjust the approach
- `cancel` - Don't make changes
```

---

## Safety Rules

**NEVER** do any of the following without explicit approval:
- Post comments to issues
- Update issue status or labels
- Create or modify code
- Close or resolve issues
- Assign or reassign issues
- Link issues or PRs
- Create sub-issues or link parent-child relationships

**ALWAYS**:
- Draft actions for review
- Wait for explicit approval
- Confirm understanding before proceeding
- Offer `cancel` option at every decision point

---

## Workflow Summary

```
Parse Issue
    |
    v
Gather Context
    |
    v
Summarise Discussion (if comments exist)
    |
    v
Assess Complexity
    |
    +--[Simple]---> Direct Suggestions
    |                    |
    +--[Complex]--> Interactive Co-working
                         |
                         v
                    Clarify & Explore
                         |
                         v
                    Present Options
                         |
                         v
                  [Too Large?]
                    /      \
                 Yes        No
                  |          |
                  v          v
           Propose      Refine Approach
           Breakdown         |
              |              |
              v              v
       Draft Sub-Issues      |
              |              |
              +------+-------+
                     |
                     v
              Draft Actions (NEVER AUTO-POST)
                     |
                     v
              Wait for Approval
                     |
                     v
              Execute (only if approved)
```

---

## References

- Complexity assessment: `references/complexity-criteria.md`
- Question templates: `references/clarifying-questions.md`
- Breakdown strategies: `references/breakdown-strategies.md`
