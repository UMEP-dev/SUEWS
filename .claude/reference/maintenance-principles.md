# Documentation & Code Maintenance Principles

## Documentation Updates for Code Changes

When making code changes to SUEWS/SuPy:
- **Always update relevant documentation** in `docs/` directory when functionality changes
- Update Sphinx documentation for user-facing changes
- **Documentation generation scripts** (run ONLY when specific changes occur):
  - Run `python docs/generate_datamodel_rst.py` - ONLY when Pydantic data model structure changes (adding/removing fields, changing types)
  - This script is NOT run for routine CHANGELOG updates or validator migrations
- Ensure examples and tutorials reflect current API
- Update parameter tables and input file documentation as needed
- **Update CHANGELOG.md promptly** for remarkable changes using categories:
  - [feature]: New features
  - [bugfix]: Bug fixes (also create GitHub issue)
  - [change]: User-facing changes
  - [maintenance]: Codebase maintenance (including Claude Code development aspects AND updates to CLAUDE.md)
  - [doc]: Documentation updates (user-facing documentation in docs/, NOT CLAUDE.md)
- **IMPORTANT**: Updates to CLAUDE.md should be categorised as [maintenance], not [doc]
- **CRITICAL CHANGELOG.md RULES**:
  - **DO NOT** modify or regenerate the Annual Statistics table (if present) - it causes merge conflicts
  - **DO NOT** run `.claude/scripts/changelog_helper.py` unless explicitly requested via `/log-changes` slash command
  - Only add new entries under the appropriate date heading
  - Keep existing entries and structure intact
  - Simply append new entries without restructuring

## RST (reStructuredText) Writing Rules

**CRITICAL: RST markup cannot be nested or overlayed!**

- **WRONG**: `**:doc:`link text <target>`**` - Cannot combine bold with doc reference
- **WRONG**: `*:option:`parameter`*` - Cannot combine italic with option reference
- **CORRECT**: Use markup separately: `:doc:`link text <target>` or make text bold separately from the link

Common RST pitfalls to avoid:
1. No nested inline markup (no bold inside links, no links inside emphasis, etc.)
2. Inline markup must be separated by whitespace or punctuation from surrounding text
3. Use backslashes to escape special characters when needed
4. Remember that `**text**` is bold, `*text*` is italic, and they cannot contain other markup

When generating RST programmatically:
- Keep `:doc:`, `:ref:`, `:option:` and other role references standalone
- Apply text formatting (bold, italic) to separate text elements only
- For emphasis, structure the document layout rather than relying on nested formatting

## Documentation Principles

1. **Single Source of Truth (DRY)**
   - Every piece of information should exist in exactly ONE place
   - Example: Package lists in one file, referenced everywhere else

2. **Reference Over Duplication**
   - Use `See: path/to/doc.md` instead of copying content
   - Example: `For complete setup, see .claude/howto/setup-worktree.md`

3. **Clear Documentation Hierarchy**
   ```
   .claude/
   ├── howto/          # Step-by-step guides (practical)
   ├── reference/      # Technical details & specifications
   ├── templates/      # Reusable templates
   ```

4. **Focused Documents**
   - Each file should have ONE clear purpose
   - Example: `testing-guide.md` focuses solely on testing requirements

5. **Brief Overview Pattern**
   - Main files (like CLAUDE.md) should be concise overviews
   - Details go in sub-documents with clear references

6. **Centralize Common Lists**
   - Package lists, commands, requirements → single file
   - Example: `core-requirements.txt` instead of inline lists everywhere

## Code Principles

7. **Single Responsibility**
   - Each function/class does ONE thing well
   - Example: `save_supy()` only saves, doesn't validate or transform

8. **Explicit Over Implicit**
   ```python
   # Good: Clear what parameters are needed
   save_supy(df_output, df_state, freq_s=3600, site="London")

   # Bad: Hidden configuration dependencies
   save_supy(df_output, df_state, config_obj)
   ```

9. **Extract Common Patterns**
   - Common operations in utility functions
   - Example: Validation logic in `validation_utils.py`

10. **Configuration Over Code Duplication**
    ```python
    # Define once, use everywhere
    PACKAGE_MAPPING = {
        'matplotlib-base': 'matplotlib',  # mamba → pip name
        'pytables': 'tables'
    }
    ```

11. **Composition Over Complex Inheritance**
    ```python
    # Flexible and testable
    class Model:
        def __init__(self, validator, processor, saver):
            self.validator = validator
            self.processor = processor
            self.saver = saver
    ```

12. **Version/Platform Isolation**
    ```python
    # compat.py - isolate compatibility code
    if sys.version_info >= (3, 13):
        from new_module import feature
    else:
        from old_module import feature
    ```

## Maintenance Best Practices

- **Important information first**: Style guidelines, critical warnings at the top
- **Progressive disclosure**: Quick start → Details → Troubleshooting
- **Cross-reference related content**: "See also:" sections for navigation
- **Use templates for repetitive patterns**: Avoid explaining the same structure multiple times
- **Document package name differences ONCE**: Create mappings, reference everywhere
