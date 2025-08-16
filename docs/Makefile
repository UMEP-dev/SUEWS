# Simplified Sphinx documentation Makefile
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
SOURCEDIR     = source
BUILDDIR      = build

.PHONY: help html clean livehtml generate-rst

# Default target
help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS)

# Build HTML documentation (most common use case)
html: generate-rst
	@$(SPHINXBUILD) -M html "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS)

# Clean build artifacts
clean:
	@$(SPHINXBUILD) -M clean "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS)

# Live-reloading server (no auto-generation for speed)
livehtml:
	sphinx-autobuild -j auto \
		--re-ignore 'supy\.util\..*' \
		--ignore "source/SuPy.log" \
		-b html -a "$(SOURCEDIR)" "$(BUILDDIR)" \
		$(SPHINXOPTS)

# Generate RST from data models (run manually when needed)
generate-rst:
	@echo "Generating RST files for data models..."
	@python3 generate_datamodel_rst.py

# Catch remaining sphinx targets without auto-generation
%:
	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS)