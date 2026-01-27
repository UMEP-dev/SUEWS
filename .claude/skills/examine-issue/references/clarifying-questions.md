# Clarifying Question Templates

Question templates for different issue types.

## Bug Reports

### Missing Reproduction Info
- "Can you share the exact steps to reproduce this?"
- "What version/commit are you running?"
- "What's in your configuration file?"

### Environment Questions
- "What OS and Python version are you using?"
- "Are you running in a virtual environment?"
- "Any relevant environment variables set?"

### Expected vs Actual
- "What output did you expect?"
- "What output did you actually get?"
- "Is this a regression or has it always been this way?"

## Feature Requests

### Scope Questions
- "What problem are you trying to solve?"
- "Who will use this feature?"
- "What's the minimum viable version?"

### Constraints
- "Any performance requirements?"
- "Does this need backward compatibility?"
- "Any security considerations?"

### Alternatives
- "Have you tried any workarounds?"
- "Are there similar features elsewhere we should match?"
- "What would you do if we couldn't implement this?"

## Refactoring/Technical Debt

### Motivation
- "What's driving this refactoring now?"
- "What problems does the current code cause?"
- "Is this blocking other work?"

### Scope Control
- "Can this be done incrementally?"
- "What's the minimum change to address the issue?"
- "Are there related areas we should also address?"

### Risk Assessment
- "What could go wrong?"
- "How will we verify nothing breaks?"
- "Any performance-sensitive code involved?"

## Design/Architecture

### Requirements Gathering
- "What are the must-haves vs nice-to-haves?"
- "What are the scaling requirements?"
- "What's the expected usage pattern?"

### Constraints Discovery
- "What can't we change?"
- "Any existing patterns we must follow?"
- "What's the timeline pressure?"

### Trade-off Exploration
- "If we can only have two of [X, Y, Z], which two?"
- "What's more important: speed or flexibility?"
- "Are we optimising for maintainability or performance?"

## Question Selection Guide

Ask at most 3 questions at a time. Prioritise:

1. Questions that block understanding
2. Questions that reveal hidden complexity
3. Questions that surface constraints

Avoid:
- Leading questions
- Yes/no questions (prefer open-ended)
- Multiple questions in one
