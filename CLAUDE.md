# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Git Worktrees for Claude Code

This repository uses nested git worktrees to enable parallel development with Claude Code. All worktrees are located under `worktrees/` directory for Claude Code accessibility.

### Worktree Structure
```
SUEWS/
├── worktrees/              # All worktrees nested here (in .gitignore)
│   ├── core-bugs/         # feature/core-runtime-fixes
│   ├── enhancements/      # feature/infrastructure-enhancements
│   ├── fast-dev-build/    # feature/fast-dev-build
│   └── ...
└── .claude/              # Claude Code workspace
    └── plans/            # Feature-specific development plans (in master)
        ├── todo/         # Features planned but not started
        ├── doing/        # Features actively being worked on
        ├── done/         # Completed features (to be cleaned up)
        └── README.md
```

### Installation and Setup

- `npm install -g @anthropic-ai/claude-code` -- This is the correct command to install Claude Code

(Rest of the existing content remains unchanged)