# Schema Versioning - Next Steps

## Immediate Actions (Before Merge)

1. **Test GitHub Pages deployment**
   - Push to master to trigger schema-management workflow
   - Verify https://umep-dev.github.io/SUEWS/ shows landing page
   - Confirm schema registry at /schema/suews-config/

2. **Documentation updates**
   - Update main README.md with schema versioning info
   - Add schema versioning guide to ReadTheDocs
   - Update CHANGELOG.md

## Short-term Enhancements (Post-merge)

1. **CLI validation command**
   ```bash
   supy validate config.yml
   # Validates against schema version in file
   ```

2. **VS Code integration guide**
   - Document how to use $schema for IntelliSense
   - Create .vscode/settings.json template

3. **Migration tooling**
   ```python
   from supy.data_model.schema import migrate
   migrate("config.yml", from_version="0.1", to_version="1.0")
   ```

## Medium-term Goals

1. **Schema documentation generation**
   - Auto-generate parameter docs from schema
   - Include in ReadTheDocs build

2. **Compatibility matrix**
   - Document which schema versions work with which SUEWS releases
   - Add to schema registry page

3. **Schema evolution process**
   - Document when to bump major vs minor version
   - Create PR template for schema changes

## Long-term Vision

1. **Schema-driven features**
   - Auto-generate UI forms from schema
   - Config file generators/wizards
   - Smart defaults based on schema

2. **Cross-tool compatibility**
   - Share schema with UMEP-for-Processing
   - Standardise across urban climate tools

3. **Schema extensions**
   - Allow custom fields for research
   - Plugin system for model extensions

## Testing Checklist

- [ ] Schema generation works without SUEWS build
- [ ] All tests pass with new schema version
- [ ] GitHub Pages deploys correctly
- [ ] CODEOWNERS protection works
- [ ] PR validation prevents manual edits
- [ ] VS Code recognises schema URL
- [ ] Registry updates automatically

## Success Metrics

- Zero build dependency for schema updates
- All historical schemas accessible
- IDE validation working for users
- Clear migration path between versions
- Protected from accidental corruption