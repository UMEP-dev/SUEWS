# Claude Code Workspace Directory

This directory contains all Claude Code-specific documentation, plans, and configuration for SUEWS development.

## Directory Structure

```
.claude/
├── howto/               # Step-by-step guides
│   ├── setup-worktree.md
│   ├── setup-environment.md
│   ├── parallel-development.md
│   └── README.md
├── reference/           # Technical documentation
│   ├── build-isolation.md
│   ├── environment-types.md
│   ├── uv-adoption.md
│   └── README.md
├── templates/           # Reusable templates
│   ├── feature-plan.md
│   ├── commit-message.md
│   └── README.md
├── commands/            # Custom slash commands
│   └── log-changes.md
├── settings.json        # Claude Code settings (committed)
└── settings.local.json  # Local settings (ignored)
```

## Directory Purposes

### howto/
**Purpose**: Step-by-step guides for common tasks
- Setting up worktrees with different tools
- Managing Python environments (uv, venv, mamba)
- Running parallel Claude Code agents
- **Quick start with uv**: `make setup && source .venv/bin/activate && make dev`
- **Quick start with mamba**: `mamba activate suews-dev && make dev`

### reference/
**Purpose**: Technical documentation and analysis
- Build system architecture and isolation
- Environment management comparison
- Tool adoption strategies

### templates/
**Purpose**: Reusable templates for consistency
- Feature plan template
- Commit message format
- Other common documents

### commands/
**Purpose**: Custom slash commands for automation
- `/log-changes` - Analyse recent changes and update docs/CHANGELOG
- Add new commands as .md files in this directory

## Quick Navigation

**"How do I...?"** → Check `howto/`
**"Why does X work this way?"** → Check `reference/`
**"What's the status of feature Y?"** → Check GitHub issues and PRs
**"I need to create a new Z"** → Check `templates/`

## For Claude Code Sessions

1. Check current branch: `git branch --show-current`
2. Check related GitHub issue or PR for context
3. Read setup guide: `cat .claude/howto/setup-worktree.md`


## Slash Commands

Custom commands for streamlined workflows:

### /worktree
Comprehensive worktree management with four simple subcommands:

- **`new`** - Start a new feature worktree
  - Interactive setup with feature name, issue, and lead developer
  - Creates worktree, plan, and environment automatically

- **`sync`** - Synchronize with master
  - Pull latest changes and update dependencies
  - Show conflicts if any

- **`pr`** - Create pull request
  - Push changes and create PR via GitHub CLI
  - Link to issue and show PR URL

- **`finish`** - Complete or abandon worktree
  - Option to finish via PR or abandon with reason
  - Clean up and archive plan

**Usage**: `/worktree [subcommand]`

**Examples**:
```bash
/worktree new        # Start new feature
/worktree sync       # Update from master
/worktree pr         # Create pull request
/worktree finish     # Complete feature
```

See `.claude/howto/worktree-workflow.md` for detailed workflow guide.

### /log-changes
Analyses recent code changes and updates documentation:

- Checks commits since last CHANGELOG.md update
- Categorises changes by type ([feature], [bugfix], etc.)
- Updates CHANGELOG.md with new entries
- Identifies and updates affected documentation
- Runs documentation generation scripts as needed

**Usage**: `/log-changes`

This command helps maintain up-to-date documentation by automatically detecting what has changed and where updates are needed.

## Git Policy
- ✅ Commit: All directories and files (except settings.local.json)
- ❌ Ignore: settings.local.json, any temp-* files