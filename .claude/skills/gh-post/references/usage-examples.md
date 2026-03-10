# Post Figures - Usage Examples

## Common Scenarios

### After a bug fix with before/after comparison

```bash
# 1. Generate comparison plots (user or agent does this)
python -c "
import matplotlib.pyplot as plt
import numpy as np
# ... generate before/after plots ...
plt.savefig('albedo_before_after.png', dpi=150, bbox_inches='tight')
"

# 2. Upload to the PR
python .claude/skills/gh-post/scripts/post_figures.py \
  --target PR:1135 \
  --images albedo_before_after.png \
  --title "Albedo fix: before vs after"
```

### Multiple figures from a test run

```bash
python .claude/skills/gh-post/scripts/post_figures.py \
  --target PR:1135 \
  --images test_output/*.png \
  --title "Validation results"
```

### Posting to an issue for discussion

```bash
python .claude/skills/gh-post/scripts/post_figures.py \
  --target ISSUE:42 \
  --images investigation/fig*.png \
  --title "Investigation findings"
```

### Auto-detecting PR from current branch

```bash
PR_NUM=$(gh pr view --json number -q .number 2>/dev/null)
if [ -n "$PR_NUM" ]; then
  python .claude/skills/gh-post/scripts/post_figures.py --target "PR:${PR_NUM}" --images fig.png
else
  echo "No open PR for current branch"
fi
```

## How It Works Internally

1. Images are base64-encoded and committed to the `gh-artifacts` orphan branch
2. Path convention: `{pr|issue}/{number}/{timestamp}_{filename}`
3. Raw URLs: `https://raw.githubusercontent.com/{owner}/{repo}/gh-artifacts/{path}`
4. A sticky comment (marked with `<!-- suews-figures -->`) is created or updated
5. Previous uploads are collapsed into `<details>` sections

## Cleanup

Remove all artifacts for a target:

```bash
python .claude/skills/gh-post/scripts/post_figures.py --target PR:1135 --cleanup
```

This deletes files from the `gh-artifacts` branch but does NOT remove the comment.

## Troubleshooting

- **"gh command failed"**: Check `gh auth status` -- you need to be authenticated
- **"Could not determine repository"**: Run from within the git repository
- **Images not rendering**: Raw URLs may take a few minutes to become available via GitHub's CDN. Refresh the page.
- **Comment not updating**: The script looks for `<!-- suews-figures -->` marker. If someone manually edited the comment and removed the marker, a new comment will be created instead.
