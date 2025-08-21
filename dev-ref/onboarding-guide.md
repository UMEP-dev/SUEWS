# SUEWS Developer Onboarding Guide

## Overview

This guide provides a structured onboarding process for new developers joining the SUEWS development team. It combines technical setup instructions with practical workflow guidance to ensure smooth integration into the development process.

## Prerequisites

Before starting development, ensure you have:

1. **Essential Tools**:
   - Git installed and configured
   - GitHub account with repository access
   - `gfortran` compiler installed (platform-specific installation)
   - Python 3.9+ installed
   - Text editor or IDE of choice

2. **Recommended Tools**:
   - AI coding assistants (for understanding complex issues)
   - Multiple communication platforms access (Teams, Slack, etc.)

## Part 1: User Perspective - Understanding SUEWS

### 1.1 Familiarise as a New User

Before diving into development, understand SUEWS from a user's perspective:

1. **Use the SUEWSSimulation Object**:
   - This is the primary interface for running simulations
   - Replaces the older table-based input system
   - Review user documentation to understand typical workflows

2. **Explore Documentation Structure**:
   - **User Guide**: Basic SUEWS usage and tutorials
   - **Development Guide**: Technical documentation for contributors
   - **Scientific References**: Papers and background material
   - **Community Resources**: GitHub discussions and issues

3. **Key Areas to Review**:
   - Getting Started tutorial
   - Input file formats (YAML-based configuration)
   - Running sample simulations
   - Understanding model outputs

### 1.2 Transition from Legacy Systems

If familiar with older SUEWS versions:
- Table-based input format is being deprecated
- YAML configuration is the new standard
- Conversion tools available (`sys convert`) for migrating existing data
- Documentation includes migration guides for legacy users

## Part 2: Development Workflow

### 2.1 Issue-Driven Development

All development follows an issue-first approach:

1. **Create or Claim an Issue**:
   - Browse existing issues for suitable tasks
   - Create new issues for features/bugs/questions
   - Use proper labelling (refer to existing labels for guidance)
   - Define clear, specific targets (avoid "moving targets")
   - Break complex tasks into smaller, manageable modules

2. **Issue Best Practices**:
   - Start simple - choose easier implementations first
   - Be specific about expected outcomes
   - Link related issues and pull requests
   - Update issue status regularly
   - Use AI tools to help draft clear issue descriptions

### 2.2 Pull Request Workflow

1. **Create a Draft PR**:
   - Link to the corresponding issue
   - Mark as "Draft" to indicate work in progress
   - This prevents premature reviews while allowing progress tracking

2. **Development Environment Setup**:
   ```bash
   # Clone repository and checkout new branch
   git clone https://github.com/UMEP-dev/SUEWS.git
   cd SUEWS
   git checkout -b feature/your-feature-name
   
   # Setup development environment with uv (ultrafast Python tool)
   make setup  # Creates virtual environment with uv
   
   # Build local development version
   make dev    # Builds SUEWS with development settings
   
   # Run test suite to verify setup
   make test   # Runs all tests - should see hundreds passing
   ```

3. **Core Development Areas**:

   #### Source Code (`src/`)
   - **`suews/`**: Core Fortran physics engine
     - Modify only if changing fundamental model physics
     - Most Python-level changes won't require Fortran modifications
   
   - **`supy/`**: Python interface layer
     - Data model definitions
     - Input/output handling
     - API for users
   
   - **`supy_driver/`**: Python-Fortran bridge
     - Rarely needs modification
     - Consult core team if structural changes needed

   #### Testing (`test/`)
   - Add comprehensive tests for new features
   - Use existing test patterns as templates
   - Test edge cases and error conditions
   - Ensure tests are independent and reproducible
   
   #### Documentation (`docs/`)
   - Update relevant documentation sections
   - YAML reference pages are auto-generated - don't edit directly
   - Modify source parsing scripts for structural changes
   - Include examples for new features

4. **Local Testing Protocol**:
   ```bash
   # Run full test suite
   make test
   
   # Run specific test file
   pytest test/test_your_feature.py
   
   # Run with verbose output
   pytest -v test/
   ```

5. **Prepare for Review**:
   - Ensure all tests pass
   - Update documentation
   - Clean up debug code and comments
   - Change PR status from "Draft" to "Ready for review"
   - Request reviews from appropriate team members

### 2.3 Code Review and Merge Process

1. **Review Process**:
   - Core team members (Ting, Matthew, Sue, others) review code
   - Address feedback constructively
   - Update PR based on review comments
   - Re-request review after changes

2. **Merge Criteria**:
   - All tests passing
   - Documentation updated
   - At least one approval from core team
   - No unresolved conversations

3. **Post-Merge**:
   - Developers may merge approved PRs themselves
   - Delete feature branch after merge
   - Close related issues
   - Update project boards if applicable

## Part 3: Technical Deep Dive

### 3.1 Build System (Meson)

The project uses Meson build system for robust compilation:

- **Configuration**: `meson.build` files define build structure
- **Adding Files**: New source files must be declared in `meson.build`
- **Recursive Structure**: Subdirectories have their own `meson.build`
- **Error Detection**: Build system catches many errors at compile time

### 3.2 Data Model System

Understanding the YAML-based data model:

1. **Structure**:
   - Hierarchical configuration
   - Type-safe parameter definitions
   - Validation at load time
   - Clear separation of user vs. developer parameters

2. **Modifying the Data Model**:
   - Edit source definitions, not generated documentation
   - Run parsing scripts to update documentation
   - Validate changes with sample configurations
   - Test backward compatibility

### 3.3 Common Development Scenarios

#### Scenario 1: Fixing a Bug
1. Reproduce the issue locally
2. Write a failing test
3. Implement the fix
4. Verify test passes
5. Check for regressions

#### Scenario 2: Adding a Feature
1. Design the interface first
2. Implement with minimal changes
3. Add comprehensive tests
4. Document usage and parameters
5. Include example configurations

#### Scenario 3: Performance Optimization
1. Profile existing code
2. Identify bottlenecks
3. Implement improvements
4. Benchmark changes
5. Ensure no accuracy loss

## Part 4: Common Development Areas

### 4.1 Model Physics

When working on model physics components:
- Review relevant scientific literature first
- Understand existing implementation before modifying
- Coordinate with team members who have domain expertise
- Validate changes against published results
- Ensure physical consistency across components

### 4.2 Data Model and Input Systems

For work on configuration and input handling:
- Understand YAML structure and validation
- Maintain backward compatibility where possible
- Document new parameters thoroughly
- Provide migration tools for breaking changes
- Test with various configuration scenarios

### 4.3 Performance and Optimization

When optimising code:
- Profile before optimising
- Maintain scientific accuracy
- Document performance improvements
- Consider memory usage alongside speed
- Benchmark against previous versions

## Part 5: Best Practices

### 5.1 Scientific Integrity

- Always validate against published results
- Document assumptions and limitations
- Preserve reproducibility
- Maintain physical consistency

### 5.2 Code Quality

- Follow existing code style
- Write self-documenting code
- Add meaningful test coverage
- Keep changes focused and minimal

### 5.3 Communication

- Regular updates on progress
- Ask questions early
- Share knowledge with team
- Document decisions and rationale

### 5.4 Time Management

- Start with simple tasks
- Build complexity gradually
- Set realistic timelines
- Communicate availability constraints

## Part 6: Resources and Support

### 6.1 Documentation

- **User Documentation**: https://suews.readthedocs.io
- **GitHub Repository**: https://github.com/UMEP-dev/SUEWS
- **Scientific Papers**: Available in references section
- **Meeting Notes**: Stored in team repository

### 6.2 Communication Channels

- **GitHub Issues**: Primary for technical discussions
- **Team Meetings**: Regular synchronisation (mid-month preferred)
- **Chat Platforms**: Quick questions and informal discussion
- **Email**: Formal communications and scheduling

### 6.3 Getting Help

- Review existing issues and discussions
- Check documentation and examples
- Ask team members directly
- Use AI tools for code understanding
- Schedule pair programming sessions if needed

## Part 7: Onboarding Checklist

### Week 1: Environment Setup
- [ ] Install all prerequisites
- [ ] Clone repository
- [ ] Set up development environment
- [ ] Run test suite successfully
- [ ] Review user documentation

### Week 2: Familiarisation
- [ ] Explore code structure
- [ ] Understand data model
- [ ] Review existing tests
- [ ] Read relevant scientific papers
- [ ] Identify first issue to work on

### Week 3-4: First Contribution
- [ ] Create/claim an issue
- [ ] Set up feature branch
- [ ] Implement solution
- [ ] Add tests
- [ ] Submit draft PR

### Month 2: Integration
- [ ] Complete first PR review cycle
- [ ] Merge first contribution
- [ ] Take on more complex issues
- [ ] Participate in team discussions
- [ ] Contribute to documentation

## Conclusion

Successful onboarding requires both technical proficiency and understanding of team workflows. Start simple, ask questions, and gradually build expertise. The team is supportive and values scientific rigour alongside code quality.

Remember: The goal is to contribute to a scientifically robust urban climate model while maintaining code quality and team collaboration. Your contributions help advance urban climate science and support researchers worldwide.

---

*Last Updated: [Date]*  
*Version: 1.0*  
*Maintainer: SUEWS Development Team*