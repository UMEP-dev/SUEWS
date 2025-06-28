# Reference Documentation

This directory contains technical reference documentation for SUEWS development infrastructure.

## Available References

### build-isolation.md
Technical analysis of the build system and isolation strategies:
- Build system architecture
- Common build conflicts
- Isolation solutions
- Performance considerations

### environment-types.md
Detailed comparison of different environment management approaches:
- Visual overview of environment structure
- mamba/conda setup details
- Environment isolation patterns
- Legacy reference for complex setups

### uv-adoption.md
Comprehensive guide for adopting uv throughout the project:
- Benefits and performance comparisons
- Migration strategies
- Full project adoption path
- CI/CD integration

## When to Use These References

**Build failures in worktrees?**
→ See `build-isolation.md`

**Need to understand environment options?**
→ See `environment-types.md`

**Considering full uv adoption?**
→ See `uv-adoption.md`

## Note

These are technical references, not how-to guides. For step-by-step instructions, see the `howto/` directory.