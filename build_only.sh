#!/bin/bash

# Build script for SUEWS without clean step

echo "Building SUEWS without clean step..."

# Activate venv
source .venv/bin/activate

# Unset all problematic conda/mamba environment variables
unset CMAKE_ARGS
unset CONDA_PYTHON_EXE
unset _CONDA_PYTHON_SYSCONFIGDATA_NAME
unset CONDA_TOOLCHAIN_HOST
unset CONDA_PROMPT_MODIFIER
unset CONDA_TOOLCHAIN_BUILD
unset CMAKE_PREFIX_PATH
unset CONDA_EXE
unset CONDA_PREFIX
unset CONDA_ROOT
unset _CE_CONDA
unset CONDA_PREFIX_1
unset CONDA_SHLVL
unset CONDA_DEFAULT_ENV
unset MESON_ARGS
unset AR
unset CONDA_PREFIX_3
unset CONDA_PREFIX_2

# Set clean compiler environment
export CC=clang
export CXX=clang++
export FC=gfortran
export F77=gfortran
export F90=gfortran
export FORTRAN=gfortran
export AR=ar
export RANLIB=ranlib

echo "Starting build with Python $(python --version)..."
make dev