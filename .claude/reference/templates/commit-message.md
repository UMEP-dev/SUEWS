# Commit Message Template

```
type: brief description (max 50 chars)

- Detailed change 1
- Detailed change 2
- Detailed change 3

Addresses #issue-number
```

## Types
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation only changes
- `style`: Changes that don't affect code meaning (formatting)
- `refactor`: Code change that neither fixes a bug nor adds a feature
- `test`: Adding missing tests or correcting existing tests
- `chore`: Changes to build process or auxiliary tools

## Examples

```
feat: add YAML configuration support

- Add YAML parser to configuration module
- Update config loader to handle .yml files
- Add tests for YAML parsing edge cases
- Update documentation with YAML examples

Addresses #245
```

```
fix: correct energy balance calculation for night-time

- Fix sign error in longwave radiation term
- Add bounds checking for negative values
- Update unit tests with night-time scenarios

Addresses #391
```

```
docs: update installation guide for Windows users

- Add PowerShell commands for Windows
- Include troubleshooting section for common errors
- Update screenshots to latest version

Addresses #420
```