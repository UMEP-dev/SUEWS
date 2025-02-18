# SUEWS

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5721639.svg)](https://doi.org/10.5281/zenodo.5721639)

This is a public repo for SUEWS source code and documentation.

---



## Documentation

* Documentation site: <https://suews.readthedocs.io/>

* Documentation source: `docs` folder in this repo


## Compilation

*Note: the following steps have been tested on macOS 14.1 and above, and WSL on Windows 10.*

### Prerequisites

#### Essential
* [gfortran](https://gcc.gnu.org/wiki/GFortran) (>= 9.3.0)
* [git](https://git-scm.com/) - for version control
* [mamba](https://mamba.readthedocs.io/en/latest/) - avoid using `conda` as it is too slow

#### Recommended
* [VS Code](https://code.visualstudio.com/) - for code editing
* [VS Code Co-pilot](https://marketplace.visualstudio.com/items?itemName=GitHub.copilot) - for AI-assisted code writing (free for academic use)
* [WSL](https://docs.microsoft.com/en-us/windows/wsl/install-win10) - for Linux-like environment on Windows (Windows users only)

### Compilation Steps

1. Since SUEWS includes a dependency package [SPARTACUS](https://github.com/Urban-Meteorology-Reading/spartacus-surface), initialise this submodule by:
```shell
git submodule init
git submodule update
```

Then source code of SPARTACUS will be loaded into `ext_lib/spartacus-surface`

*Note: if a `permission denied` error occurs, one usually needs to fix the SSH connection to GitHub by following the [official guide here](https://docs.github.com/en/github/authenticating-to-github/connecting-to-github-with-ssh).*

2. Configure mamba environment:
```shell
mamba env create -f env.yml
```
This will create a new environment named `suews-dev` with all required packages installed.

3. Activate the environment:
```shell
mamba activate suews-dev
```

4. Compile SUEWS:

4.1. For development with all tests:
```shell
make
```

4.2. For development without tests:
```shell
make dev
```

This will install Python packages `supy-driver` and `supy` into the current environment (`suews-dev`).
Several command-line tools will be installed under `bin` folder for SUEWS simulations:
- `suews-run`: the main SUEWS binary
- `suews-convert`: a tool to convert SUEWS input files between formats

The usage of both tools can be checked with the `--help` option (e.g., `suews-run --help`).

5. Verify installation:
```shell
pip show supy
```



## Developer Note

> [!NOTE]
> **the following is deprecated and will be updated**

<!-- When doing `pip install -e supy-driver` using WSL in VS Code on Windows 10 I got the error "[Errno 13] Permission denied: 'build/bdist.linux-x86_64/wheel/supy_driver-2021a2.dist-info'". The solution was in the Windows file explorer to right-click the project directory (SUEWS) -> properties -> security -> edit -> everyone -> tick allow -> apply.

### Branch

#### `master` branch

`master` is the main branch that keeps milestone and stable features.
  * `push` is restricted to admin members.

If one needs to fix a bug or implement a new feature, please open an issue with details and then submit a pull request with respect to that issue.


### Documentation

* Please keep the development changes in [CHANGELOG.md](CHANGELOG.md).

### Test

Whenever changes are made, please run `make test` in the repo root to check if your changes are working or not.
If any error, please resolve it or justify that the test is giving false alarm.

#### Tests and purposes
`make test` will perform the following checks:

- if single-grid-multi-year run could be successful: to check if multi-year run is functional;
- if multi-grid-multi-year run could be successful: to check if multi-grid run is functional;
- if test run results could match those from the standard run (runs under `BaseRun`): to check if any non-functional changes would break the current code;
- if all physics schemes are working: to check possible invalid physics schemes.

<!-- #### Workflow
The test workflow is as follows (details refer to the Makefile `test` recipe and related python code):

1. clean existing build and rebuild the code;
2. create a temporary directory as working directory to perform checks;
3. copy the rebuilt `SUEWS_{version}` binary to the temporary folder;
4. copy the version specific input files under `Release/InputTables/{version}` to the temporary folder (see below for its preparation);
5. run python code to perform the above checks and write out test results to the console:
   1. if all tests are successful, the code is generally good to go;
   2. if any test failed, we NEED to look into the reasons for the failure and resolve them before any further feature-added operations. -->

<!-- #### Preparation of tests

1. Prepare a base run:
   - under `Test/BaseRun`, create a folder named with version/feature info (e.g., `2019a`);
   - perform a simulation to produce example output files, which will later be used as standard run to verify the correct code functionalities.

   *Note: all the above input files will be automatically copied under `Release/InputTables` with explicit version/feature (e.g., `Release/InputTables/2019a`) and later archived in public releases for users; so carefully construct test data to include in the input files.*
2. Configure test namelist file `Test/code/BTS_config.nml`:

   - `name_exe`: the SUEWS binary name that will be used for testing.
   - `dir_exe`: the directory to copy `name_exe`.
   - `dir_input`: the directory to copy input files; suggested to be `Release/InputTables/{version}`.
   - `dir_baserun`: the base run against which to test identity in results. -->

### Debugging with GDB

GDB is a generic debugging tool used along with gfortran.
Here are some tips to debug SUEWS code:

#### GDB on macOS

Recent macOS (since High Sierra) introduces extra security procedures for system level operations that makes installation GDB more tedious than before.
The best practice, in TS's opinion, to avoid hacking your macOS, is to use Linux docker images with gfortran & gdb installations: e.g., [alpine-gfortran](https://github.com/cmplopes/alpine-gfortran)
(otherwise, [this guide](https://dev.to/jasonelwood/setup-gdb-on-macos-in-2020-489k#generate-cert) might be useful for installation of GDB on macOS; also run `set startup-with-shell off` *inside GDB* before `run` the debuggng process)

Once the docker image is installed, simply run this from the SUEWS root folder for debugging:

```bash
 docker run --rm -it -v $(pwd):/source sunt05/alpine-gfortran /bin/bash

```
 which will mount the current `SUEWS` directory to docker's path `/source` and enter the interactive mode for debugging.


#### debugging with GDB

1. enable the debugging related flags in `Makefile` under `SUEWS-SourceCode` by removing the `#` after the equal sign `=`:

```makefile
FCNOOPT = -O0
FFLAGS = -O3 $(STATIC) $(FCDEBUG) -Wall -Wtabs -fbounds-check -cpp \
					-Wno-unused-dummy-argument -Wno-unused-variable
```

2. fully clean and recompile `SUEWS`:
```
make clean; make
```

3. copy the recompiled `SUEWS` binary into your SUEWS testing folder (e.g., `Test/BaseRun/2019a`) and load it into GDB:

```
gdb SUEWS

run

```
then you should have stack info printed out by GDB if any runtime error occurs.

More detailed GDB tutorial can be found [here](https://github.com/jackrosenthal/gdb-tutorial/blob/master/notes.pdf).



### Questions

* Please [raise issues](https://github.com/UMEP-dev/SUEWS/issues/new) for questions in the development so our progress can be well managed.
