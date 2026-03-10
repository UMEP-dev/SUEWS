---
name: doc-styler
description: Check and fix non-ASCII characters in documentation source files. Replaces Unicode with proper RST/LaTeX/ASCII markup per context. Use when editing tutorials, docstrings, or any sphinx-gallery .py files.
---

# Doc Styler

Enforce ASCII-only output in documentation source files per `00-project-essentials.md`.
Sphinx-gallery `.py` tutorials need special handling because comments become RST prose,
print statements become terminal output, and matplotlib strings render via LaTeX.

## Triggers

- Editing `docs/source/tutorials/tutorial_*.py`
- "Check docs for Unicode", "fix non-ASCII in tutorials"
- After creating or modifying sphinx-gallery tutorial files
- `/doc-styler` or `/doc-styler path/to/file.py`

## Workflow

1. **Scan** target files for non-ASCII characters
   ```bash
   grep -rPn '[^\x00-\x7F]' docs/source/tutorials/tutorial_*.py
   ```
   If a specific file is given as argument, scan only that file.

2. **Classify** each occurrence by context (see `references/replacement-rules.md`)

3. **Replace** using context-appropriate markup

4. **Verify** no non-ASCII remains:
   ```bash
   grep -rPn '[^\x00-\x7F]' <scanned files>
   ```

5. **Report** changes made

## Output Format

```
[doc-styler] Analysis

=== docs/source/tutorials/tutorial_03_initial_conditions.py ===
  L35: [RST comment] m^2/m^2 -> :math:`m^2/m^2`  (superscript 2)
  L36: [RST comment] degC.d -> :math:`^{\circ}C \cdot d`  (degree + middle dot)
  L193: [matplotlib] m^2/m^2 -> m$^2$/m$^2$  (superscript 2)

Summary: 1 file, 3 replacements, 0 non-ASCII remaining
```

## References

- `references/replacement-rules.md` - Full replacement table by context
- `docs/source/conf.py` lines 414-459 - Available `rst_prolog` substitutions
- `.claude/rules/00-project-essentials.md` - ASCII-only rule
