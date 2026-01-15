# Release Assessment Criteria

## RELEASE NOW (any one triggers)

- Critical bugfix merged (user-impacting bug)
- Security vulnerability patched
- External deadline imminent (conference, teaching, collaborator request)

## RELEASE RECOMMENDED (score ≥5)

| Factor | Points |
|--------|--------|
| ≥1 feature commit | +3 |
| ≥2 bugfix commits | +2 |
| ≥1 user-facing change | +2 |
| >30 days since last release | +2 |
| >60 days since last release | +3 |
| User/collaborator request | +2 |

## WAIT (any one triggers)

- <7 days since last release (unless critical)
- Only maintenance/docs commits
- Major feature branch not yet merged
- Tests failing on master
