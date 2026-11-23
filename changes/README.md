# Changelog fragments

Do not edit `CHANGELOG.md` in a pull request.
Add one file here per PR (or issue) and category instead, so that PRs touching different files can share a merge-queue batch.
`CHANGELOG.md` stays the published record; the fragments are folded into it before each release.

## Writing a fragment

Name the file `<number>.<category>.md`:

- `<number>` is the PR number (or the issue number when there is no PR yet).
- `<category>` is one of `feature`, `bugfix`, `change`, `maintenance`, `doc`.

The body is the exact bullet text that would otherwise go into `CHANGELOG.md`, following `.claude/rules/changelog/format.md`.
Write no date heading: the fold takes the date from the commit that added the file.

```markdown
- [feature][experimental] Added a new radiation scheme (#2171)
  - Optional sub-bullet, indented two spaces (abc1234)
```

Each top-level bullet must carry the file's category tag.
A PR with entries in two categories adds two files, for example `2171.feature.md` and `2171.doc.md`.

Check fragments locally with:

```bash
python scripts/suews/fold_changelog.py --check
```

## Folding fragments into CHANGELOG.md

A maintainer runs this on an up-to-date `master` checkout, normally as part of release preparation, and lands the result through a PR:

```bash
python scripts/suews/fold_changelog.py --dry-run   # review the diff
python scripts/suews/fold_changelog.py             # write CHANGELOG.md, delete the fragments
```

Each entry goes under the `### D Mon YYYY` heading for the day its fragment was committed to `master`.
Pass `--date YYYY-MM-DD` to use one date for every fragment.
The fold only inserts lines: existing entries and the Annual Statistics table are never touched.
