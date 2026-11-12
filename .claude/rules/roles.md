# Roles and Roster

Rules name roles, not people.
This file defines each role and holds the roster that maps it to the person who holds it.
A decision record names the role and the date ("Decided by the maintainer on 12 Nov 2026"); the roster's dates resolve that to one person.

---

## Roles

- **Maintainer.**
  Answers for the articles in `AGENTS.md`, for this roster, and for every decision that no rule gives to another role.
  Holds the domain-owner role for any subsystem without its own owner.
- **Domain owner** (one per owned subsystem).
  Signs off changes to that subsystem's own physics or behaviour (`physics-change-evidence.md`) and approves `[stable]` changelog entries for it (`changelog/format.md`).
  The scope of each owned subsystem is given in its roster entry.
- **Advisory scientific reviewer.**
  Reviews on request and carries no gate; an approval from this role is not a sign-off.
  Reviewer routing is in `dev-ref/SCIENTIFIC_REVIEWERS.md`.

---

## Roster

- **Maintainer:** `@sunt05` (on the roster from 12 Nov 2026).
- **Domain owner, STEBBS** (`suews_phys_stebbs.f95`, the STEBBS data model, the STEBBS fixtures): `@yiqing1021` (on the roster from 12 Nov 2026).

---

## Changing the roster

- Adding, removing or reassigning a role is a decision by the maintainer, made in its own pull request.
- When a holder changes, keep the previous holder in the entry with "until <date>", so that an earlier record naming the role and a date still resolves to one person.
- Other rules and skills cite a role and this file; they do not repeat a handle.
