# Plan: Documentation Overhaul

**Status**: TODO  
**Scope**: Comprehensive documentation restructuring  
**Languages**: reStructuredText, Markdown, Python (Sphinx)  
**Duration**: 3-4 weeks  
**Priority**: High  

## Background
Current SUEWS documentation mixes API reference, user guides, and theory. Users struggle to find relevant information for their use case. We need a complete restructure following modern documentation patterns (Diátaxis framework).

## Objectives
- Create user-journey based documentation structure
- Separate concerns: tutorials, how-to guides, reference, explanation
- Add practical examples and real-world use cases
- Improve searchability and navigation
- Enable offline access and versioning

## Key Deliverables

### 1. Documentation Architecture

```
docs/
├── quickstart/              # Get running in 5 minutes
│   ├── installation.rst
│   ├── first-simulation.rst
│   └── next-steps.rst
│
├── tutorials/               # Learning-oriented
│   ├── basic-concepts.rst
│   ├── urban-simulation.rst
│   ├── data-preparation.rst
│   └── output-analysis.rst
│
├── how-to/                  # Task-oriented
│   ├── configure-surface-types.rst
│   ├── prepare-forcing-data.rst
│   ├── run-multiple-sites.rst
│   ├── optimize-performance.rst
│   └── couple-with-wrf.rst
│
├── reference/               # Information-oriented
│   ├── api/
│   ├── configuration/
│   ├── parameters/
│   ├── outputs/
│   └── file-formats/
│
├── explanation/             # Understanding-oriented
│   ├── model-physics.rst
│   ├── energy-balance.rst
│   ├── water-balance.rst
│   ├── anthropogenic-heat.rst
│   └── validation-studies.rst
│
└── community/               # Contribution-oriented
    ├── contributing.rst
    ├── governance.rst
    ├── development.rst
    └── support.rst
```

### 2. Content Creation

#### Quickstart Guide
- 5-minute installation
- Running sample simulation
- Viewing basic outputs
- Clear next steps

#### Tutorial Series
- **Basic Concepts**: What SUEWS does and why
- **First Urban Simulation**: Step-by-step neighborhood modeling
- **Data Preparation**: From raw data to SUEWS inputs
- **Output Analysis**: Understanding and visualizing results

#### How-To Guides
- Common configuration tasks
- Performance optimization
- Troubleshooting guide
- Integration scenarios
- Migration from legacy API

#### Reference Documentation
- Complete API reference (auto-generated)
- Parameter descriptions with ranges
- Output variable definitions
- Configuration schema
- File format specifications

#### Explanation Section
- Model physics deep-dives
- Theoretical background
- Validation against observations
- Limitations and assumptions
- Comparison with other models

### 3. Documentation Infrastructure

#### Technical Improvements
- **Version Switcher**: Access docs for any release
- **Search Enhancement**: Full-text search with filters
- **Auto-generation**: API docs from docstrings
- **Cross-references**: Smart linking between sections
- **Code Examples**: Tested, downloadable examples

#### Output Formats
- HTML (primary, with responsive design)
- PDF (for offline reading)
- ePub (for e-readers)
- Man pages (for CLI reference)

#### Quality Assurance
- Link checking
- Code example testing
- Spell checking
- Style guide enforcement
- Accessibility compliance

## Implementation Steps

### Week 1: Architecture and Migration
- [ ] Set up new documentation structure
- [ ] Migrate existing content to appropriate sections
- [ ] Configure Sphinx extensions
- [ ] Set up versioning system

### Week 2: Content Creation (Core)
- [ ] Write quickstart guide
- [ ] Create first tutorial
- [ ] Develop 5 how-to guides
- [ ] Review API reference generation

### Week 3: Content Creation (Advanced)
- [ ] Complete tutorial series
- [ ] Add explanation articles
- [ ] Create troubleshooting guide
- [ ] Add interactive examples

### Week 4: Polish and Deploy
- [ ] Technical review
- [ ] User testing
- [ ] Fix issues
- [ ] Deploy new documentation

## Success Metrics
- Time to first successful simulation (<10 minutes)
- Documentation satisfaction survey
- Search success rate
- Page views and dwell time
- Support ticket reduction
- Community contributions

## Dependencies
- API modernization plan (for examples)
- Current documentation audit
- User feedback collection
- Technical writing resources

## Risks and Mitigation
- **Risk**: Scope creep
  - **Mitigation**: Strict prioritization, phased approach
- **Risk**: Maintenance burden
  - **Mitigation**: Automation, community contributions
- **Risk**: Outdated examples
  - **Mitigation**: Automated testing, version pinning

## Technical Considerations
- Sphinx vs. other generators
- Hosting (GitHub Pages, ReadTheDocs)
- Translation infrastructure
- Mobile responsiveness
- SEO optimization

## Content Guidelines
- Clear, concise writing
- Consistent terminology
- Visual aids where helpful
- Real-world examples
- Progressive disclosure

## Open Questions
- [ ] Translation strategy?
- [ ] Video content integration?
- [ ] Interactive notebooks?
- [ ] API playground?
- [ ] Documentation feedback system?

## Notes for Review
<!-- Please add your comments below -->