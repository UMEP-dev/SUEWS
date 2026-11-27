#!/usr/bin/env python3
"""Build and validate the static SUEWS data-contracts site tree.

Always loads ``src/supy/data_model/contracts.py`` directly from this checkout
so local and CI publication cannot silently pick up an older installed ``supy``
package.
"""

from __future__ import annotations

import argparse
import importlib
import json
from pathlib import Path
import sys
from types import ModuleType

PROJECT_ROOT = Path(__file__).resolve().parents[2]
_DATA_MODEL_ROOT = PROJECT_ROOT / "src/supy/data_model"
_CHECKOUT_PACKAGE = "_suews_contracts_checkout"


def _register_namespace_package(name: str, directory: Path) -> ModuleType:
    existing = sys.modules.get(name)
    if existing is not None:
        return existing
    module = ModuleType(name)
    module.__package__ = name
    module.__path__ = [str(directory)]
    sys.modules[name] = module
    return module


def _load_contracts_module() -> ModuleType:
    """Load the checkout-local ``contracts`` module deterministically."""
    _register_namespace_package(_CHECKOUT_PACKAGE, _DATA_MODEL_ROOT)
    _register_namespace_package(
        f"{_CHECKOUT_PACKAGE}.forcing", _DATA_MODEL_ROOT / "forcing"
    )
    _register_namespace_package(
        f"{_CHECKOUT_PACKAGE}.output", _DATA_MODEL_ROOT / "output"
    )
    return importlib.import_module(f"{_CHECKOUT_PACKAGE}.contracts")


def main(argv: list[str] | None = None) -> int:
    """Publish and/or validate the static forcing and output contracts tree."""
    contracts = _load_contracts_module()

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--output-dir",
        "-o",
        type=Path,
        default=None,
        help="Destination contracts directory to materialise (e.g. _site/contracts).",
    )
    parser.add_argument(
        "--validate-only",
        type=Path,
        default=None,
        metavar="CONTRACTS_DIR",
        help="Validate an already-built contracts directory without writing files.",
    )
    parser.add_argument(
        "--write-forcing-snapshot",
        metavar="VERSION",
        default=None,
        help=(
            "Write overwrite-protected schema and manifest snapshots for forcing "
            "VERSION under src/supy/data_model/contract_artefacts/forcing/<VERSION>/ "
            "and print the pinned manifest digest."
        ),
    )
    parser.add_argument(
        "--base-url",
        default=contracts.DEFAULT_BASE_URL,
        help=f"Base URL recorded in index.json (default: {contracts.DEFAULT_BASE_URL}).",
    )
    parser.add_argument(
        "--preserve-from",
        type=Path,
        default=None,
        help="Existing contracts directory whose historical releases are preserved.",
    )
    parser.add_argument(
        "--no-validate",
        action="store_true",
        help="Skip post-build validation when publishing.",
    )
    parser.add_argument(
        "--json",
        dest="as_json",
        action="store_true",
        help="Emit the publication or validation report as JSON on stdout.",
    )
    args = parser.parse_args(argv)

    selected_modes = sum(
        opt is not None
        for opt in (args.output_dir, args.validate_only, args.write_forcing_snapshot)
    )
    if selected_modes != 1:
        print(
            "[contracts] FAILED: specify exactly one of --output-dir, "
            "--validate-only, or --write-forcing-snapshot",
            file=sys.stderr,
        )
        return 2

    try:
        if args.write_forcing_snapshot is not None:
            digest = contracts.write_forcing_published_snapshot(
                args.write_forcing_snapshot,
                PROJECT_ROOT,
            )
            if args.as_json:
                print(
                    json.dumps(
                        {
                            "interface": "forcing",
                            "version": args.write_forcing_snapshot,
                            "manifest_digest": digest,
                        },
                        indent=2,
                        sort_keys=True,
                    )
                )
            else:
                print(
                    f"[OK] Wrote forcing snapshot {args.write_forcing_snapshot}: {digest}"
                )
            return 0
        if args.validate_only is not None:
            report = contracts.validate_published_contracts(
                args.validate_only,
                base_url=args.base_url,
            )
            if args.as_json:
                print(json.dumps(report, indent=2, sort_keys=True))
            else:
                print(f"[OK] Validated contracts directory: {report['contracts_dir']}")
                for iface, versions in report["interfaces"].items():
                    print(f"  - {iface}: {', '.join(versions)}")
                print(
                    f"  - HTML pages: {report['html_files_checked']}, "
                    f"relative links: {report['relative_links_checked']}"
                )
            return 0

        report = contracts.publish_contracts_site(
            args.output_dir,
            base_url=args.base_url,
            preserve_from=args.preserve_from,
            validate=not args.no_validate,
        )
        if args.as_json:
            print(json.dumps(report, indent=2, sort_keys=True))
        else:
            print(f"[OK] Published contracts to {report['output_dir']}")
            for iface, details in report["interfaces"].items():
                print(
                    f"  - {iface}: current={details['current_version']}, "
                    f"versions={len(details['versions'])}, "
                    f"preserved={len(details['preserved_versions'])}"
                )
            if report["validated"] and report["validation"] is not None:
                val = report["validation"]
                print(
                    f"[OK] Validated {val['html_files_checked']} HTML page(s) and "
                    f"{val['relative_links_checked']} relative link(s)"
                )
        return 0
    except (
        OSError,
        contracts.ContractError,
        contracts.ContractPublicationError,
    ) as exc:
        print(f"[contracts] FAILED: {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
