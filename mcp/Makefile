.PHONY: build-source-index bundle-source dev

build-source-index:
	python3 scripts/build_source_index.py

bundle-source:
	rsync -a --include='*.f95' --exclude='*' ../src/suews/src/ src/suews_mcp/source/

dev: bundle-source build-source-index
	python3 -m pip install -e ".[dev]"
