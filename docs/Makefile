# Minimal makefile for Sphinx documentation
#

# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
SPHINXPROJ    = SUEWS
SOURCEDIR     = source
BUILDDIR      = build
SUPYDIR      = ./source/related-softwares/supy

# Put it first so that "make" without argument is like "make help".
help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

.PHONY: help Makefile pip

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
%: Makefile
	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

livehtml:
	sphinx-autobuild --re-ignore ["supy.util.*"] -b html -a  "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS)

pip:
	pip install -e ../src/supy

proc-csv:
	python $(SUPYDIR)/proc_var_info/gen_rst.py