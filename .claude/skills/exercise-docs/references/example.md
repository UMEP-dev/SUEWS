# Illustrative compiled step and assertion

This is an illustration, not run output, a cached compile or evidence of a
successful installation. A real compile covers the whole selected journey and
its branches. Re-read the requested revision rather than copying this example
as a test input. [procedure: Principle 1]

The source is `docs/source/workflow.rst` at
`dc9575f62593e9639bd97751a99ea47b5e089e6b`, lines 30-35. Inspect it with:
[procedure: Principle 1]

```sh
git show dc9575f62593e9639bd97751a99ea47b5e089e6b:docs/source/workflow.rst | nl -ba | sed -n '30,35p'
```

One compiled step and assertion could be represented as follows. `work` is the
fresh record's reader directory, not a repository path. [procedure: Principle 1]

```yaml
step:
  id: install
  kind: verbatim
  language: shell
  command: pip install supy
  cwd: work
  session: shell
  sources:
    - path: docs/source/workflow.rst
      revision: dc9575f62593e9639bd97751a99ea47b5e089e6b
      lines: [35, 35]
      quote: '   pip install supy'
assertion:
  id: supy-installed
  after_step: install
  expectation: explicit
  sources:
    - path: docs/source/workflow.rst
      revision: dc9575f62593e9639bd97751a99ea47b5e089e6b
      lines: [30, 30]
      quote: '**Install SuPy:**'
  predicate: importlib.metadata.version('supy') returns a non-empty string
  observation_method: Query distribution metadata with the fresh environment's Python.
  status: executable
```

The script executes the literal install command in the fresh virtual environment,
records its exit code and output, then checks distribution metadata. The compiler
generates that observation code separately from the page command. This assertion
checks installation only; it does not claim that importing or simulating works.
No observed value or pass result is supplied here. [procedure: Principle 1]
