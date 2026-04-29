# SUEWS Naming Convention

Rules for naming user-facing identifiers in SUEWS — YAML parameters, YAML
enum values, forcing column names, error-message field references, and
the Python data-model field names that mirror them.

> **Status (2026-04-29)**: This document is under review. It is the
> canonical reference for any rename PR while the team signs off. Once
> signed off, the "draft" framing here is removed. Track sign-off in
> the originating PR.

> **Scope**: User-facing identifiers and the Python data-model fields
> that mirror them. Fortran TYPE members and Rust struct field names
> follow the same ordering rules where practical, but their migrations
> are tracked separately in gh#1324 / gh#1325 / gh#1326.

> **Relationship to other rules**: Supersedes the *Identifier Naming
> (Python)* and *Variant-Neutral Naming* sections of
> `.claude/rules/00-project-essentials.md` for naming questions; that
> file remains the source of truth for British-English style, ASCII
> output, and the rest of the project essentials. Schema-version bumps
> that ship a rename obey `.claude/rules/python/schema-versioning.md`.

---

## Why this exists

Before this convention, SUEWS field names accreted along three different
paths:

- Legacy Fortran identifiers, fused with no separators
  (`netradiationmethod`, `soildepth`).
- Snake-cased renames where each PR picked its own word order
  (`wall_external_thickness` vs. `external_wall_thickness`).
- STEBBS additions following a building-energy idiom that disagreed
  with the rest of the model.

The result: users cannot grep for "all heights" or "all temperatures"
because the relevant tokens float to arbitrary positions in the name.
Translation tables papered over each individual rename but never
established what *correct* looks like.

This rule fixes one shape. Future renames either follow it or
explicitly opt out with an issue.

---

## The three core principles

1. **snake_case with one underscore between every meaningful word.**
2. **Physical quantity first, then component/location, then sub-class.**
3. **Methods are nested families, not flat tokens with a `_method` suffix.**

Everything below is a consequence of these three.

---

## Rule 1 — snake_case, every word separated

Every meaningful word boundary gets exactly one underscore. Domain
abbreviations (OHM, RSL, FAI, LAI, SMD, GDD, SDD) count as one
"word" for separator purposes but stay as a single token.

- `same_albedo_wall`, not `samealbedo_wall`.
- `thermal_conductivity`, not `thermalconductivity`.
- `snow_density_max`, not `snowdensmax`.
- `ohm_inc_qf`, not `ohmincqf`.

There is no `kebab-case`, `camelCase`, or `PascalCase` in user-facing
identifiers. The Python class names that hold these fields are
`PascalCase`, but their *fields* are snake_case.

### Allowed characters

`a-z`, `0-9`, `_`. Numbers may appear inside an identifier but a name
must not start with a digit. No accented characters, no Unicode
superscripts, no spaces.

---

## Rule 2 — ordering: physical quantity → component/location → sub-class

When a field has a physical quantity (a thing you can measure or assign
units to), the **physical quantity comes first**, then the part of the
system it applies to, then any sub-classification.

This is the rule that lets users grep for "all temperatures" or "all
thicknesses" by prefix.

- `temperature_wall_internal` ✓ — physical quantity (`temperature`),
  component (`wall`), sub-class (`internal`).
- `thickness_wall_outer` ✓ — quantity, component, layer.
- `density_roof_outer` ✓.
- `lai_evergreen_tree` ✓ — quantity (`lai`), land cover.
- `temperature_paved` ✓.

Wrong shapes (and what they should become):

- `wall_external_thickness` → `thickness_wall_outer`
  (also `external` → `outer`, see *Specific tokens* below).
- `internal_wall_temperature` → `temperature_wall_internal`.
- `building_height` → `height_building` (when not in archetype scope;
  see archetype exception below).

### Non-physical-quantity fields

Some fields have no physical quantity to lead with: metadata, profiles,
control flags, ratios, fractions, thresholds. For these, **the
category noun leads**, drawn from the small controlled list below:

- `name_*` — for human-readable names
- `count_*` — for integer counts
- `type_*` — for enum-typed classifiers
- `profile_*` — for time-series of values (24-hour, weekly, monthly)
- `control_*` — for boolean / mode switches
- `flag_*` — for true/false markers
- `ratio_*`, `fraction_*` — for dimensionless ratios
- `threshold_*` — for trigger values

Examples:

- `profile_appliance` (was `appliance_profile`).
- `profile_metabolism` (was `metabolism_profile`).
- `control_daylight` (was `daylight_control`).
- `ratio_internal_volume` (was `internal_volume_ratio`).
- `threshold_lighting_illuminance` (was `lighting_illuminance_threshold`).

Adding a new category prefix requires a convention amendment. The list
above is closed; do not invent new prefixes inline.

### Exception — namespace-bound fields keep the namespace prefix

When a field is permanently scoped to a single namespace and the
namespace name carries meaning the rest of the system needs to see, the
namespace prefix may lead even if there is a physical quantity in the
name. The current list of allowed namespace prefixes:

- `archetype_*` — fields that describe a STEBBS building archetype as
  a whole and only have meaning inside an archetype. Examples:
  - `archetype_name`
  - `archetype_count`
  - `archetype_height`
  - `archetype_building_count`

This exception exists because `name_archetype` reads as *the name of
the namespace itself*, not *a name field within an archetype*. New
namespace prefixes require a convention amendment — they are not
added casually.

---

## Rule 3 — methods are nested families, not flat tokens

Method-style fields previously followed the pattern
`<topic>method` (e.g. `netradiationmethod`, `storageheatmethod`). This
implies all methods are siblings of each other, which is not true:
methods cluster in *families* (the choice of net-radiation scheme is
independent of the choice of stability scheme).

The new shape:

- The **family name** lives at the top of its section, with no
  `_method` suffix and no fused legacy token. Use the natural
  English name: `net_radiation`, `storage_heat`, `stability`,
  `roughness_sublayer`, `frontal_area_index`, `surface_conductance`.
- The **family value** is a string-valued enum naming the scheme:
  `narp`, `spartacus`, `narp_plus_spartacus` for `net_radiation`;
  `ohm`, `anohm` for `storage_heat`; etc.
- Combination schemes use `<a>_plus_<b>` (e.g. `narp_plus_spartacus`),
  not `<a>+<b>` or `<a>and<b>`.

The mapping from legacy integer codes to the new enum strings is
maintained in `src/supy/util/converter/yaml_upgrade.py` and is part of
the schema-version bump that ships each rename.

### Numeric option codes

Numeric option codes (`1`, `2`, `3`) are not accepted at the YAML
surface — they were always a leaky abstraction over Fortran integer
flags. Migration converts the integer to the corresponding enum
string.

---

## Rule 4 — logical name vs YAML leaf name

The convention has two related but distinct concepts:

- **Logical name** — the canonical, fully-qualified identifier for a
  field. Used in error messages, CLI lookups, the validator's
  reports, and developer docs. Always written as a dotted path:
  `stebbs.archetype.wall.outer.thickness`.
- **YAML leaf name** — the short identifier that appears at the leaf
  of a nested YAML structure. Disambiguated by the parent path:

  ```yaml
  stebbs:
    archetypes:
      - wall:
          outer:
            thickness: 0.20
            density: 1800
  ```

The Python data model registers the **logical name** as canonical. The
YAML loader's job is to map a nested YAML position to its logical name
(and back, on dump). The mapping is unambiguous because the YAML schema
is fixed.

### Why both exist

- The grep test ("look down a column and find all heights") needs the
  flat, fully-qualified logical name to exist as a single string —
  `stebbs.archetype.wall.outer.thickness` lets you grep for
  `\.thickness$` across the whole codebase.
- Users writing YAML do not want to type `wall_outer_thickness` once
  per archetype-wall-outer field; nested YAML reads better.

### Practical rule for authors

- When *writing YAML*, use the nested form. Field names at the leaf
  follow Rule 2 (physical quantity first, etc.).
- When *citing a field in docs, error messages, or PR text*, use the
  logical name in dotted form.
- When *grepping*, search either form — the data model exposes a
  `field_to_logical_name()` lookup.

---

## Rule 5 — forcing column names are aliases of long names

The forcing file uses short, urban-meteorology-community-conventional
column names (`Tair`, `RH`, `kdown`, `rain`, `wuh`). These are not
re-named to the long form: the community has used them for decades and
external forcing files would break.

Instead, each canonical YAML parameter has an **alias** — a short
forcing-column name registered in the schema:

- `tair` ↔ `temperature_air`
- `rh` ↔ `relative_humidity`
- `u` ↔ `wind_speed`
- `kdown` ↔ `shortwave_radiation_downward`
- `rain` ↔ `precipitation`
- `lai` ↔ `lai_bulk`
- (full list in the schema)

Forcing-file aliases are case-insensitive on read (so existing files
with `Tair` still match), but the canonical form in docs and schemas
is lowercase to keep it consistent with snake_case rules.

Per-land-cover forcing variables (introduced in gh#1372) use the long
form (`lai_evergreen_tree`, `water_use_paved`, etc.) — there is no
established short alias for these, and inventing one now would create
a new namespace to maintain.

---

## Rule 6 — small items

### Enum / option values

Use snake_case for all string-valued enum values:

- `forcing_disaggregation: distribution_based` ✓
- `forcing_disaggregation: DistributionBased` ✗
- `net_radiation: narp_plus_spartacus` ✓
- `net_radiation: NARP+SPARTACUS` ✗

### Units do not appear in field names

Units are documented in the schema metadata and the field's docstring —
not in the identifier.

- `temperature_air` ✓ (unit is `°C`, in metadata)
- `temperature_air_celsius` ✗
- `precipitation` ✓ (unit is `mm h⁻¹`, in metadata)
- `precipitation_mm_per_hour` ✗

### Plurals and singulars

- A list/sequence field uses the **plural** name: `archetypes:`,
  `forcing_columns:`, `land_covers:`.
- The fields *inside* a list element use the **singular** form:
  `archetypes[i].name`, `archetypes[i].height`.

### Domain abbreviations

Domain abbreviations stay lowercase inside identifiers but may be
capitalised in prose:

- Identifier: `ohm_inc_qf`, `lai_max`, `rsl_level`.
- Prose: "the OHM scheme", "LAI maximum", "RSL level".

In particular, do **not** write `OHM_inc_qf` or `LAI_max` in
identifiers — capitalisation triggers parser confusion and does not
add information.

---

## Specific tokens that are now standard

A few words have multiple plausible spellings; these are the
canonical choices.

- **Layer vs. surface for walls / roofs / windows** — these are two
  distinct physical concepts, and the convention preserves both:
  - **`outer` / `inner`** for layer-resolved *bulk* properties of the
    multilayer construction (thickness, density, conductivity,
    specific heat capacity, heat capacity fraction). Examples:
    `thickness_wall_outer`, `density_wall_outer`,
    `conductivity_roof_inner`.
  - **`external` / `internal`** for the *surface itself* — the
    radiative or boundary-layer face of the wall, roof, or window
    (emissivity, transmissivity, absorptivity, reflectivity).
    Examples: `emissivity_wall_external`,
    `transmissivity_window_internal`.
  - The two coexist by design: a wall has both an *outer layer* (a
    slab of material with a thickness) and an *external surface* (the
    radiative face of that slab). They are different things and the
    field names must reflect that.
  - `external` also covers fields genuinely *outside the building
    footprint* (e.g. `external_ground_conductivity` for the ground
    beyond the building). The wall / roof / window context
    disambiguates.
- **Building floor at ground level** — `ground_floor` (two words),
  not `groundfloor`. Snake_case applies.
- **Heating / cooling distinguishers** — when a STEBBS field could
  apply to either air or water systems, the qualifier must be
  explicit. Combined with Rule 2 (physical quantity first), the
  resulting forms are:
  - Setpoint temperatures: `temperature_air_heating_setpoint`,
    `temperature_air_cooling_setpoint`,
    `temperature_water_heating_setpoint`. The physical quantity is
    `temperature`; `setpoint` is a sub-class qualifier (a target
    temperature, as opposed to an observed or controlled one), so it
    falls at the end. Setpoint *profiles* extend this:
    `temperature_air_heating_setpoint_profile`.
  - Maximum powers: `power_air_heating_max`, `power_air_cooling_max`,
    `power_water_heating_max`. The physical quantity is `power`;
    `max` is the sub-class qualifier. Same shape as `lai_max`,
    `snow_density_max`, etc.
  - Do not leave the `air_` / `water_` qualifier out — bare
    `temperature_heating_setpoint` is ambiguous between air-system
    and DHW-system control.
- **`effective_` qualifier** — drop it unless the partner parameters
  for the same component (density, specific heat capacity) also use
  it. The current STEBBS use of `effective_conductivity` next to
  plain `density` is inconsistent and resolves to plain
  `conductivity`.

---

## Migration path

This convention does not try to rename everything in one go. The
practical approach:

1. **One PR per logical group** (STEBBS archetype, OHM family,
   forcing aliases, …). Each PR cites this convention and updates the
   `_HANDLERS` registry in
   `src/supy/util/converter/yaml_upgrade.py` for the schema bump.
2. The migration handler must record renames as
   `legacy_name → new_name` so that an older YAML still loads.
3. **Drops** require an explicit `reason` in the handler — Pydantic's
   default `extra="ignore"` will otherwise swallow user values
   silently.
4. The CHANGELOG and the relevant
   `docs/source/version-history/v<release>.rst` page must list the
   user-visible renames in the release that ships them.
5. The vendored release fixtures under
   `test/fixtures/release_configs/` get the updated shape so
   round-trip tests catch regressions.

The schema-versioning rule
(`.claude/rules/python/schema-versioning.md`) governs the
`CURRENT_SCHEMA_VERSION` bump that accompanies each rename PR.

---

## Open question — pending decisions

- **Soil moisture units** — mass-based vs volume-based. The
  convention reserves `soil_moisture` as the canonical name; the unit
  decision affects the validator's accepted range and the
  bulk-density input requirement, not the field name itself. Pending
  decision; will be appended here once locked.

If you spot another convention question this doc does not answer,
open an issue with the `2-doc:naming` label and reference this page.

---

## Appendix — worked example

How a STEBBS archetype block transforms under this convention.
Left = current; right = under this convention.

```yaml
# Current (mixed legacy + first-pass rename)
stebbs:
  archetypes:
    - building_name: residential_terrace
      building_count: 120
      stebbs_height: 7.5
      wall_external_thickness: 0.30
      wall_external_effective_conductivity: 0.6
      wall_external_density: 1800
      wall_external_specific_heat_capacity: 920
      wall_internal_emissivity: 0.93
      heating_setpoint_temperature: 19.0
      cooling_setpoint_temperature: 24.0
      max_heating_power: 8000.0
      max_cooling_power: 6000.0
      hot_water_heating_setpoint_temperature: 55.0
      maximum_hot_water_heating_power: 4000.0
      appliance_profile: [...]
      daylight_control: true
      internal_volume_ratio: 0.3
```

```yaml
# Under this convention
stebbs:
  archetypes:
    - archetype_name: residential_terrace
      archetype_count: 120
      archetype_height: 7.5
      thickness_wall_outer: 0.30
      conductivity_wall_outer: 0.6      # 'effective_' dropped
      density_wall_outer: 1800
      specific_heat_capacity_wall_outer: 920
      emissivity_wall_external: 0.92             # surface property
      emissivity_wall_internal: 0.93             # surface property
      temperature_air_heating_setpoint: 19.0     # Rule 2: temperature leads
      temperature_air_cooling_setpoint: 24.0
      temperature_water_heating_setpoint: 55.0
      power_air_heating_max: 8000.0              # Rule 2: power leads
      power_air_cooling_max: 6000.0
      power_water_heating_max: 4000.0
      profile_appliance: [...]
      control_daylight: true
      ratio_internal_volume: 0.3
```

Notes on the diff:

- `building_name` / `building_count` / `stebbs_height` use the
  `archetype_*` namespace exception (Rule 2).
- `wall_external_*` (bulk-material rows) → `*_wall_outer`
  (Rule 2 + Specific tokens — `outer` for layer, distinct from
  `external` for surface).
- `wall_external_emissivity` → `emissivity_wall_external` (Rule 2
  alone — surface property keeps `external`).
- `effective_conductivity` → `conductivity` (Specific tokens).
- `heating_setpoint_temperature` →
  `temperature_air_heating_setpoint`. Three changes at once:
  Rule 2 puts `temperature` first; the `air_` qualifier
  disambiguates from DHW (`temperature_water_heating_setpoint`);
  `setpoint` becomes the trailing sub-class qualifier.
- `max_heating_power` → `power_air_heating_max`. Rule 2 puts
  `power` first; `max` is the sub-class qualifier.
- `appliance_profile` → `profile_appliance` (Rule 2 —
  non-physical-quantity, category leads).
- `daylight_control` → `control_daylight` (same reason).
- `internal_volume_ratio` → `ratio_internal_volume` (same reason).

The forcing file under this convention (using aliases per Rule 5):

```
# header (case-insensitive, canonical lowercase shown)
year doy hour minute  tair  rh  u  pres  kdown  rain  wuh  lai
2026  120  10  0      18.5  72  3.1 1013  640    0.0  0.0  3.2
```

— with `tair ↔ temperature_air`, `rh ↔ relative_humidity`, etc.,
registered in the forcing-schema alias table.
