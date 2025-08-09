# SUEWS Release Versioning & Cycle Discussion

## 1. Version Schema Options

Looking at the current situation:
- **Current approach**: Daily dev tags like `2025.8.8.dev`
- **Issue**: No clear distinction between stable/experimental releases

### Option A: Year-Based Semantic Versioning
```
YYYY.MINOR.PATCH
2025.1.0 → 2025.2.0 → 2025.2.1
```
**Pros:**
- Clear year visibility for citations
- Academic papers can reference specific year
- Aligns with annual funding/project cycles

**Cons:**
- Major breaking changes harder to signal
- Year rollover might seem like major version bump

### Option B: Traditional Semantic Versioning
```
MAJOR.MINOR.PATCH
1.0.0 → 1.1.0 → 2.0.0
```
**Pros:**
- Industry standard
- Clear breaking change signals
- Flexible release timing

**Cons:**
- Less clear for academic citations
- No inherent time reference

### Option C: Hybrid Academic-Semantic
```
YYYY.TERM.PATCH
2025.SP.0 → 2025.SU.0 → 2025.AU.0
(Spring/Summer/Autumn terms)
```
**Pros:**
- Academic calendar alignment
- Clear seasonal reference for papers
- Natural research cycle fit

**Cons:**
- Non-standard format
- International confusion (Northern/Southern hemisphere)


TS: go with year.month.day[dev] - without dev then formal - otherwise dev version
## 2. Release Cycle for Academic Pattern

### Current Academic Constraints:
- **September-December**: New academic year, teaching heavy, new PhD students
- **January-March**: Active research period, paper deadlines
- **April-June**: Conference season, end of academic year
- **July-August**: Summer break (but often fieldwork/writing time)

### Proposed Academic-Aligned Cycles:

#### Option 1: Termly Releases (3 per year)
- **January Release** (2025.1.0): Start of spring term
  - Major features from autumn development
  - Ready for spring semester projects
- **May Release** (2025.2.0): End of spring term
  - Updates from winter development
  - Available for summer research
- **September Release** (2025.3.0): Start of academic year
  - Summer developments
  - Stable for teaching/new students

#### Option 2: Semester Releases (2 per year)
- **February Release**: Spring semester
  - After winter break development
  - Before major spring conferences
- **September Release**: Autumn semester
  - After summer development
  - Ready for new academic year
TS:go with this opt 2

#### Option 3: Research-Focused Quarterly
- **March** (Q1): Post-winter development
- **June** (Q2): Pre-summer version
- **September** (Q3): Academic year start
- **December** (Q4): Minor updates only (holiday period)

### Key Considerations:

**For Academic Users:**
- Need stable versions for multi-year PhD projects
- Require citable versions for publications
- Want predictable updates that don't disrupt teaching

**For Research Groups:**
- Need cutting-edge features for papers
- Want rapid bug fixes
- Require reproducibility guarantees

## My Recommendation:

Based on SUEWS' academic nature, I suggest:

### Version Schema: Modified Year-Based
```
YY.RELEASE.PATCH[-TAG]
25.1.0 → 25.2.0 → 26.1.0
```
- Shorter year format (25 vs 2025)
- 2-3 releases per year
- Clear patch versions for fixes
- Optional tags for pre-releases (25.1.0-beta.1)

### Release Cycle: Academic Tri-Annual
1. **February Release** (YY.1.0)
   - "Spring Release"
   - Major features from autumn/winter
   - 1-month beta (January)
   
2. **June Release** (YY.2.0)
   - "Summer Release"
   - Features from spring development
   - 2-week beta (mid-May)
   
3. **October Release** (YY.3.0)
   - "Autumn Release"
   - Summer work + stability fixes
   - 2-week beta (mid-September)
   - Focus on stability for teaching

**December & August**: No releases (holidays/conferences)

## Questions for Discussion:

1. **Version Schema**: 
   - Should we prioritise academic citation needs or industry standards?
   - How important is the year visibility in version numbers?

2. **Release Frequency**:
   - Is 3 releases/year too many or too few?
   - Should we have a "teaching stable" version each September?

3. **Beta Testing**:
   - Can we realistically get academic users to test betas during term time?
   - Should we have longer beta periods during summer?

4. **LTS for PhD Students**:
   - Should September releases be LTS (3-year support) for PhD cohorts?
   - How do we handle reproducibility for published papers?

## Additional Considerations:

### Daily Dev Builds
- **Current**: Daily `.dev` tags (e.g., `2025.8.8.dev`)
- **Question**: Continue daily or move to weekly/on-demand?
- **Distribution**: TestPyPI only or also nightly Docker images?
TS:pypi only

### Breaking Changes
- **Academic need**: Stability during academic year
- **Research need**: New features for cutting-edge papers
- **Proposal**: Major breaking changes only in February release?
TS:probably the sep one

### Documentation Versioning
- **Challenge**: Multiple versions in use simultaneously
- **Solution**: Maintain docs for each YY.RELEASE version?
- **Resources**: Can we sustain multiple documentation versions?
TS:docs need to go with code 


### Citation Format
How should users cite SUEWS in papers?
- Option 1: "SUEWS version 25.1.0 (Sun et al., 2025)"
- Option 2: "SUEWS 2025.1 (Sun et al., 2025)"
- Option 3: DOI for each release via Zenodo?
TS:opt3

### Transition Plan
If we adopt a new versioning scheme:
- When do we start? (Next release? January 2025?)
- How do we map current versions to new scheme?
- Migration guide for existing users?

TS:coming sept
---

*Please add your comments and thoughts throughout this document. Use `[TS: comment]` format for inline comments.*
