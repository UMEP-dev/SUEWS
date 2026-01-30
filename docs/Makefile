# Simplified Sphinx documentation Makefile
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
SOURCEDIR     = source
BUILDDIR      = build

# RST generation outputs and deps - input config
RST_OUTDIR    = $(SOURCEDIR)/inputs/yaml/config-reference
RST_STAMP     = $(RST_OUTDIR)/.generated.stamp

# RST generation outputs and deps - output variables
OUTPUT_RST_OUTDIR = $(SOURCEDIR)/outputs/variables
OUTPUT_RST_STAMP  = $(OUTPUT_RST_OUTDIR)/.generated.stamp

# Data model sources that affect generated RST
DM_SRC        = $(shell find ../src/supy/data_model -type f -name '*.py' 2>/dev/null)

# Tutorial notebooks generated from percent-format .py sources
TUTORIAL_DIR  = $(SOURCEDIR)/tutorials/python
TUTORIAL_PY   = $(wildcard $(TUTORIAL_DIR)/*_tutorial.py)
TUTORIAL_IPYNB = $(TUTORIAL_PY:_tutorial.py=-tutorial.ipynb)

.PHONY: help html clean livehtml

# Default target
help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS)

# Build HTML documentation (most common use case)
html: $(RST_STAMP) $(OUTPUT_RST_STAMP) $(TUTORIAL_IPYNB)
	@$(SPHINXBUILD) -M html "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS)

# Clean build artifacts
clean:
	@$(SPHINXBUILD) -M clean "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS)

# Live-reloading server with incremental builds
livehtml: $(RST_STAMP) $(OUTPUT_RST_STAMP) $(TUTORIAL_IPYNB)
	@echo "Starting live-reload server with incremental builds..."
	@echo "Tip: Edit .rst files and see changes instantly (2-5s rebuilds)"
	@echo "     Data model RST auto-regenerates when Python sources change"
	sphinx-autobuild -j auto \
		--re-ignore 'supy\.util\..*' \
		--ignore "source/SuPy.log" \
		--ignore "source/auto_examples/*" \
		--watch ../src/supy/data_model \
		--open-browser \
		"$(SOURCEDIR)" "$(BUILDDIR)/html" \
		$(SPHINXOPTS)

# Generate RST from input data models only when sources change
$(RST_STAMP): generate_datamodel_rst.py $(DM_SRC)
	@echo "Generating RST files for input data models..."
	@python3 generate_datamodel_rst.py --output-dir $(RST_OUTDIR)
	@touch $(RST_STAMP)

# Generate RST from output variable registry only when sources change
$(OUTPUT_RST_STAMP): generate_output_variable_rst.py $(DM_SRC)
	@echo "Generating RST files for output variables..."
	@python3 generate_output_variable_rst.py --output-dir $(OUTPUT_RST_OUTDIR)
	@touch $(OUTPUT_RST_STAMP)

# Convenience target: preserve existing name, now non-phony
# Provide a no-op recipe so it won't be picked up by the generic
# "%:" rule (which would try to call sphinx with a non-existent builder).
generate-rst: $(RST_STAMP) $(OUTPUT_RST_STAMP)
	@:

# Generate tutorial notebooks from percent-format .py sources
# Pattern: foo_tutorial.py -> foo-tutorial.ipynb (underscore to hyphen)
$(TUTORIAL_DIR)/%-tutorial.ipynb: $(TUTORIAL_DIR)/%_tutorial.py
	@echo "Generating $@ from $<..."
	@jupytext --to ipynb "$<" -o "$@"

# Catch remaining sphinx targets without auto-generation
%:
	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS)
