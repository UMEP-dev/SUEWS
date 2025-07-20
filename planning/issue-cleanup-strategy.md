# SUEWS Issue Cleanup Strategy

## Executive Summary

The SUEWS repository (UMEP-dev/SUEWS) currently has **59 open issues** dating from December 2020 to July 2025. This document outlines a comprehensive strategy to clean up, organize, and manage these issues effectively.

## Current State Analysis

### Issue Distribution by Category

- **Enhancement requests**: 11 issues
- **STEBBS (new building model)**: 9 issues  
- **Documentation**: 7 issues
- **Bugs**: 6 issues
- **RSL (Roughness Sublayer)**: 4 issues
- **SPARTACUS integration**: 4 issues
- **SuPy (Python wrapper)**: 4 issues
- **CO2 modelling**: 4 issues

### Priority Distribution

- **P0 (Critical)**: 1 issue
- **P1 (High)**: 5 issues
- **P2 (Medium)**: 3 issues
- **P3 (Low)**: 4 issues
- **P4 (Very Low)**: 3 issues
- **Unprioritised**: 43 issues

### Staleness Analysis

- **13 issues** have not been updated since 2023 or earlier
- **2 issues** are marked with "no-issue-activity"
- **46 issues** have been updated in 2024-2025

## Cleanup Strategy

### Phase 1: Immediate Actions (Week 1)

#### 1.1 Critical Bug Triage
- Review P0/P1 bugs for immediate action:
  - #473: Numerical divergence in OHM calculations (P0)
  - #240: nan QF due to zero population density (P1)
  - #161: Problem with forcing interpolation (P1)
  - #419: Issue using MOST in RSL scheme (P1)

#### 1.2 Close Stale Issues
Close issues with no activity for >1 year after verification:
- #15: Water use "overuse" determination
- #50: Infiltration settings issue
- #136: Initial conditions problem (marked no-issue-activity)
- #138: Documentation issue (marked no-issue-activity)
- #140: Documentation page issue
- #145: LAI method clarification
- #146: EmissionsMethod separation

#### 1.3 Label Cleanup
- Add priority labels to all 43 unprioritised issues
- Standardise label naming (e.g., "Anthropogenic Emissions" → "anthropogenic-emissions")
- Remove redundant labels (e.g., "💬 comment")

### Phase 2: Organisation (Week 2)

#### 2.1 Create Issue Templates
Implement GitHub issue templates for:
- Bug reports (with reproduction steps, environment info)
- Feature requests (with use case, expected behaviour)
- Documentation improvements
- Questions/Support

#### 2.2 Milestone Creation
Create milestones for:
- v2025.1 Release (Q1 2025)
- v2025.2 Release (Q2 2025)
- STEBBS Integration (ongoing)
- SPARTACUS Enhancement (ongoing)
- Documentation Overhaul

#### 2.3 Project Board Setup
Create GitHub Projects for:
- Active Development (In Progress, Review, Done)
- Bug Tracking (Reported, Confirmed, In Progress, Fixed)
- Feature Roadmap (Planned, In Development, Testing, Released)

### Phase 3: Issue Resolution (Weeks 3-4)

#### 3.1 Documentation Sprint
Address all 7 documentation issues:
- #154-157: Tutorial series on SUEWS usage
- #80: References page update
- #212: Runoff explanation
- Convert to modern documentation format

#### 3.2 Quick Wins
Resolve low-hanging fruit issues:
- #243: Split WUDay_id variables
- #245: FcRespi output investigation
- #528: Warning message cleanup
- #527: load_sample_data() error

#### 3.3 Feature Consolidation
Group related enhancement requests:
- **STEBBS Integration** (9 issues): Create meta-issue for tracking
- **RSL Improvements** (4 issues): Consolidate into single epic
- **CO2 Modelling** (4 issues): Create unified approach

### Phase 4: Long-term Management (Ongoing)

#### 4.1 Regular Triage Process
- Weekly triage meetings for new issues
- Monthly review of stale issues
- Quarterly milestone planning

#### 4.2 Automation Setup
- GitHub Actions for:
  - Auto-labelling based on file changes
  - Stale issue warnings (60 days)
  - Auto-close after 90 days of inactivity
  - Welcome bot for first-time contributors

#### 4.3 Communication Improvements
- Issue discussion guidelines
- Response time SLAs (24h for P0, 72h for P1)
- Regular project updates in issues

## Implementation Checklist

### Immediate (This Week)
- [ ] Review and action P0/P1 bugs
- [ ] Close verified stale issues with explanatory comments
- [ ] Create issue templates
- [ ] Standardise existing labels

### Short-term (Next 2 Weeks)
- [ ] Set up GitHub Projects
- [ ] Create milestones for 2025 releases
- [ ] Assign priorities to all issues
- [ ] Group STEBBS/RSL/CO2 issues

### Medium-term (Next Month)
- [ ] Complete documentation sprint
- [ ] Implement GitHub Actions automation
- [ ] Establish regular triage schedule
- [ ] Create contributor guidelines

### Long-term (Ongoing)
- [ ] Monthly issue health reviews
- [ ] Quarterly roadmap updates
- [ ] Annual issue cleanup sprints
- [ ] Continuous process improvement

## Success Metrics

- Reduce open issues to <40 within 1 month
- All issues prioritised within 2 weeks
- Average issue age <6 months
- Response time to new issues <72 hours
- 90% of P0/P1 issues addressed within sprint

## Recommendations

1. **Immediate Focus**: Address critical bugs affecting users
2. **Communication**: Update all issue reporters on status
3. **Documentation**: Prioritise user-facing documentation
4. **Automation**: Reduce manual triage burden
5. **Community**: Encourage contributions through clear guidelines

## Conclusion

This strategy provides a structured approach to managing the SUEWS issue backlog. By combining immediate cleanup with long-term process improvements, we can maintain a healthy, responsive issue tracking system that serves both users and developers effectively.