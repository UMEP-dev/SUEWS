# Simplified Sphinx documentation Makefile
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
SOURCEDIR     = source
BUILDDIR      = build

# RST generation outputs and deps
RST_OUTDIR    = $(SOURCEDIR)/inputs/yaml/config-reference
RST_STAMP     = $(RST_OUTDIR)/.generated.stamp
# Data model sources that affect generated RST
DM_SRC        = $(shell find ../src/supy/data_model -type f -name '*.py' 2>/dev/null)

.PHONY: help html clean livehtml

# Default target
help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS)

# Build HTML documentation (most common use case)
html: $(RST_STAMP)
	@$(SPHINXBUILD) -M html "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS)

# Clean build artifacts
clean:
	@$(SPHINXBUILD) -M clean "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS)

# Live-reloading server with incremental builds
livehtml: $(RST_STAMP)
	@echo "Starting live-reload server with incremental builds..."
	@echo "Tip: Edit .rst files and see changes instantly (2-5s rebuilds)"
	@echo "     Data model RST auto-regenerates when Python sources change"
	sphinx-autobuild -j auto \
		--re-ignore 'supy\.util\..*' \
		--ignore "source/SuPy.log" \
		--watch ../src/supy/data_model \
		--open-browser \
		"$(SOURCEDIR)" "$(BUILDDIR)/html" \
		$(SPHINXOPTS)

# Generate RST from data models only when sources change
$(RST_STAMP): generate_datamodel_rst.py $(DM_SRC)
	@echo "Generating RST files for data models..."
	@python3 generate_datamodel_rst.py --output-dir $(RST_OUTDIR)
	@touch $(RST_STAMP)

# Convenience target: preserve existing name, now non-phony
# Provide a no-op recipe so it won't be picked up by the generic
# "%:" rule (which would try to call sphinx with a non-existent builder).
generate-rst: $(RST_STAMP)
	@:

# Catch remaining sphinx targets without auto-generation
%:
	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS)
