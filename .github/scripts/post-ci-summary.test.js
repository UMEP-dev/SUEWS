// Unit tests for post-ci-summary.js (no GitHub API calls).
//
// Run locally:  node --test .github/scripts/post-ci-summary.test.js
// Also run by workflow-security.yml when the script or this file changes.

const test = require('node:test');
const assert = require('node:assert/strict');

const { buildBody, describeTier, describeMatrix, TIER_DESC } = require('./post-ci-summary.js');

const MINIMAL = '[["ubuntu-latest", "manylinux", "x86_64"]]';
const PR = '[["ubuntu-latest", "manylinux", "x86_64"], ["macos-15", "macosx", "arm64"], ["windows-2025", "win", "AMD64"]]';
const FULL = '[["ubuntu-latest", "manylinux", "x86_64"], ["macos-15-intel", "macosx", "x86_64"], ["macos-15", "macosx", "arm64"], ["windows-2025", "win", "AMD64"]]';

function envFor(overrides) {
  return {
    FORTRAN_CHANGED: 'true',
    FORTRAN_FILES: '["src/suews/src/suews_phys_dailystate.f95"]',
    NEEDS_BUILD: 'true',
    TEST_TIER: 'standard',
    BUILDPLAT_JSON: PR,
    PYTHON_JSON: '["cp312"]',
    COMMIT_SHA: 'deadbeef',
    ...overrides,
  };
}

const ctx = { isDraft: false, owner: 'UMEP-dev', repo: 'SUEWS' };

test('every tier emitted by determine-matrix.sh has a description', () => {
  for (const tier of ['smoke', 'cfg', 'core', 'standard', 'physics-full', 'all']) {
    assert.ok(TIER_DESC[tier], `missing tier description for ${tier}`);
    assert.notEqual(describeTier(tier), tier, `tier ${tier} renders as the bare token`);
  }
});

test('physics-full renders with its description, not the bare token', () => {
  const body = buildBody(envFor({ TEST_TIER: 'physics-full' }), ctx);
  assert.match(body, /\| \*\*Test tier\*\* \| physics-full \(full physics tier incl\. slow; 0-physics:change label\) \|/);
  assert.match(body, /0-physics:change label -> full physics tier/);
});

test('matrix descriptor follows buildplat, not the draft flag', () => {
  assert.equal(describeMatrix(JSON.parse(MINIMAL)), 'minimal (1 platform)');
  assert.equal(describeMatrix(JSON.parse(PR)), 'reduced (3 platforms)');
  assert.equal(describeMatrix(JSON.parse(FULL)), 'full (4 platforms)');
  assert.equal(describeMatrix([]), 'none (0 platforms)');

  // A ready python-only PR runs the minimal preset: the comment must say so.
  const ready = buildBody(envFor({ BUILDPLAT_JSON: MINIMAL, FORTRAN_CHANGED: 'false', PYTHON_CHANGED: 'true' }), ctx);
  assert.match(ready, /\| \*\*Matrix\*\* \| minimal \(1 platform\) \|/);
  assert.match(ready, /\| \*\*PR status\*\* \| Ready for review \|/);

  // A draft fortran PR runs the same three platforms as a ready one.
  const draft = buildBody(envFor({ TEST_TIER: 'core' }), { ...ctx, isDraft: true });
  assert.match(draft, /\| \*\*Matrix\*\* \| reduced \(3 platforms\) \|/);
  assert.match(draft, /\| \*\*PR status\*\* \| Draft \|/);
  assert.doesNotMatch(draft, /reduced matrix\)|standard matrix\)/);
});

test('ready physics-change fortran PR (the gh#1759 shape) renders its real plan', () => {
  const body = buildBody(envFor({ TEST_TIER: 'physics-full', TESTS_CHANGED: 'true' }), ctx);
  assert.match(body, /\| \*\*Matrix\*\* \| reduced \(3 platforms\) \|/);
  assert.match(body, /\| \*\*Platforms\*\* \| Linux x86_64, macOS ARM64, Windows x64 \|/);
  assert.match(body, /\| \*\*Python\*\* \| 3\.12 \|/);
  assert.match(body, /\| \*\*Test tier\*\* \| physics-full /);
  assert.match(body, /\| \*\*PR status\*\* \| Ready for review \|/);
  assert.match(body, /blob\/deadbeef\/\.github\/path-filters\.yml/);
});

test('no-build PRs skip the configuration table', () => {
  const body = buildBody(envFor({ NEEDS_BUILD: 'false', FORTRAN_CHANGED: 'false', FORTRAN_FILES: '[]', DOCS_CHANGED: 'true' }), ctx);
  assert.match(body, /No code build required/);
  assert.doesNotMatch(body, /\*\*Matrix\*\*/);
});
