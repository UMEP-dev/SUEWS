# YAML Config Builder Requirements

## Feature Overview
**Feature Name**: yaml-config-builder
**Brief Description**: A CLI wizard tool to help users set up valid and physically sensible input YAML files for SUEWS simulations, leveraging the Pydantic-based data model.

**Related GitHub Issues**:
- #400: Pydantic Hierarchy - Explores conditional validation based on model switches
- #392: Pydantic-related cleanup - Maps existing parameter validation rules



## Context
**User Impact**:

greatly improve user experience for setting up SUEWS input YAML files.

**Current Limitations**:

- the traditional way of setting up SUEWS input YAML files is to manually edit the YAML file, which is error-prone and time-consuming.
- runctrl based table format is not user-friendly and will be deprecated in the future.

**Success Criteria**:
user can follow the wizard to set up a valid and physically sensible input YAML file for SUEWS simulations.

## Scope
**What's Included**:
- all the parameters in the SUEWS input YAML file

**What's Excluded**:
- the parameters that are not relevant to the SUEWS input YAML file

**Dependencies**:
- the wizard should be able to handle the parameters that are not relevant to the SUEWS input YAML file

## Requirements (EARS Notation)

### Core Functionality

#### R1: CLI Wizard Interface
WHEN a user runs the yaml-config-builder command THE SYSTEM SHALL launch an interactive CLI wizard that guides them through the configuration process step by step.

#### R2: Data Model Integration
WHEN the wizard collects user input THE SYSTEM SHALL validate each field against the Pydantic data model constraints defined in the SUEWS codebase.

#### R3: Physical Sensibility Checks
WHEN a user enters parameter values THE SYSTEM SHALL validate that the values are physically sensible according to the rules defined in the data model (ref: #392).

#### R4: Conditional Validation
WHEN certain model options are enabled or disabled THE SYSTEM SHALL apply conditional validation rules as appropriate (ref: #400).

### User Experience

#### R5: Progressive Disclosure
WHEN a user is configuring the system THE SYSTEM SHALL only show relevant options based on previous selections to avoid overwhelming the user.

#### R6: Help and Documentation
WHEN a user requests help for any parameter THE SYSTEM SHALL display contextual help including parameter description, valid ranges, and physical meaning.

#### R7: Default Values
WHEN a parameter has a sensible default value THE SYSTEM SHALL pre-populate it and allow the user to accept or modify it.

### Validation and Error Handling

#### R8: Real-time Validation
WHEN a user enters a value THE SYSTEM SHALL immediately validate it and provide feedback if the value is invalid.

#### R9: Cross-field Validation
WHEN multiple related fields are entered THE SYSTEM SHALL validate their relationships and ensure consistency (e.g., time step vs forcing resolution as per #161).

#### R10: Error Recovery
WHEN a user enters an invalid value THE SYSTEM SHALL explain why it's invalid and suggest valid alternatives.

### Output Generation

#### R11: YAML Generation
WHEN the user completes the wizard THE SYSTEM SHALL generate a valid YAML configuration file that conforms to the SUEWS input schema.

#### R12: Configuration Preview
WHEN generating the final configuration THE SYSTEM SHALL allow the user to preview the complete YAML before saving.

#### R13: Save Options
WHEN saving the configuration THE SYSTEM SHALL allow the user to specify the output filename and location.

### Advanced Features

#### R14: Configuration Templates
WHEN starting the wizard THE SYSTEM SHALL offer pre-defined templates for common use cases (e.g., urban, suburban, rural).

#### R15: Import Existing Config
WHEN a user has an existing configuration file THE SYSTEM SHALL allow importing it as a starting point for modification.

#### R16: Validation-Only Mode
WHEN a user has an existing YAML file THE SYSTEM SHALL provide a validation-only mode to check its validity without modification.

## Non-Functional Requirements

### Performance
#### NFR1: Response Time
WHEN validating user input THE SYSTEM SHALL provide feedback within 100ms to maintain a responsive user experience.

### Usability
#### NFR2: Keyboard Navigation
WHEN using the CLI wizard THE SYSTEM SHALL support full keyboard navigation including arrow keys, tab completion, and shortcuts.

#### NFR3: Undo/Redo Support
WHEN entering configuration values THE SYSTEM SHALL allow users to undo and redo changes at each step of the wizard.

#### NFR4: Progress Indicator
WHEN progressing through the wizard THE SYSTEM SHALL display a clear indicator showing the current step and overall progress towards completion.

#### NFR5: Colour Coding
WHEN displaying validation results THE SYSTEM SHALL use colour coding to distinguish between valid (green), warning (yellow), and error (red) states.

### Compatibility
#### NFR6: Platform Support
WHEN running on different platforms THE SYSTEM SHALL work consistently on Windows, macOS, and Linux.

#### NFR7: Python Version
WHEN checking Python compatibility THE SYSTEM SHALL support Python 3.9+ as per SUEWS requirements.

---
*Last Updated*: 2025-07-22
*Status*: ACTIVE