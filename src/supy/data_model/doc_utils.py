"""
Documentation extraction utilities for SUEWS Pydantic data models.

This module provides model introspection and documentation extraction
capabilities that generate a JSON structure consumed by RST generators.
"""

import inspect
import json
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any, Dict, List, Optional, Set, Type, Union, get_args, get_origin

from pydantic import BaseModel
from pydantic.fields import FieldInfo


class ModelDocExtractor:
    """Extract documentation from Pydantic models into structured JSON format."""

    def __init__(self):
        self.visited_models: Set[str] = set()
        self.all_models: Dict[str, Type[BaseModel]] = {}
        self.internal_classes = {
            "HDD_ID",
            "WaterUse",
            # Add other internal classes to skip
        }

    def extract_all_models(self, include_internal: bool = False) -> Dict[str, Any]:
        """
        Extract documentation from all discovered models.

        Args:
            include_internal: Whether to include internal/developer options

        Returns:
            Dictionary containing complete documentation structure
        """
        # Discover all models
        self._discover_models()

        # Extract documentation for each model
        models_doc = {}
        hierarchy = {"root": [], "tree": {}}

        for model_name, model_class in self.all_models.items():
            if model_name in self.internal_classes and not include_internal:
                continue

            model_doc = self._extract_model(model_class, include_internal)
            models_doc[model_name] = model_doc

            # Identify root models (not referenced by others)
            if self._is_root_model(model_name):
                hierarchy["root"].append(model_name)

        # Build hierarchy tree
        hierarchy["tree"] = self._build_hierarchy_tree(models_doc)

        return {
            "models": models_doc,
            "hierarchy": hierarchy,
            "metadata": {
                "generated_at": datetime.now().isoformat(),
                "total_models": len(models_doc),
                "total_fields": sum(len(m["fields"]) for m in models_doc.values()),
                "include_internal": include_internal,
            },
        }

    def _discover_models(self) -> None:
        """Discover all Pydantic models in the data_model package."""
        import importlib

        # Import through the proper supy package hierarchy
        modules_to_import = [
            "supy.data_model.core.type",
            "supy.data_model.core.profile",
            "supy.data_model.core.config",
            "supy.data_model.core.model",
            "supy.data_model.core.ohm",
            "supy.data_model.core.hydro",
            "supy.data_model.core.human_activity",
            "supy.data_model.core.state",
            "supy.data_model.core.surface",
            "supy.data_model.core.site",
        ]

        for module_name in modules_to_import:
            try:
                module = importlib.import_module(module_name)
                for name, obj in inspect.getmembers(module):
                    if (
                        inspect.isclass(obj)
                        and issubclass(obj, BaseModel)
                        and obj.__module__ == module.__name__
                    ):
                        self.all_models[name] = obj
            except ImportError as e:
                print(f"Warning: Could not import {module_name}: {e}")
                continue

    def _extract_model(
        self, model_class: Type[BaseModel], include_internal: bool
    ) -> Dict[str, Any]:
        """Extract documentation from a single model."""
        model_name = model_class.__name__

        # Prevent circular references
        if model_name in self.visited_models:
            return {"name": model_name, "circular_ref": True}

        self.visited_models.add(model_name)

        # Extract basic info
        model_doc = {
            "name": model_name,
            "title": self._get_model_title(model_class),
            "description": inspect.cleandoc(model_class.__doc__ or ""),
            "fields": [],
            "nested_models": [],
            "parent_models": [],
        }

        # Extract fields
        for field_name, field_info in model_class.model_fields.items():
            if self._should_skip_field(field_info, include_internal):
                continue

            field_doc = self._extract_field(
                field_name, field_info, model_name, include_internal
            )
            model_doc["fields"].append(field_doc)

            # Track nested models
            nested_model = field_doc.get("nested_model")
            if nested_model and nested_model not in model_doc["nested_models"]:
                model_doc["nested_models"].append(nested_model)

        # Clear visited for next model (but keep all_models)
        self.visited_models.discard(model_name)

        return model_doc

    def _extract_field(
        self,
        field_name: str,
        field_info: FieldInfo,
        model_name: str,
        include_internal: bool,
    ) -> Dict[str, Any]:
        """Extract documentation from a single field."""
        field_type = field_info.annotation

        field_doc = {
            "name": field_name,
            "type": self._get_type_string(field_type),
            "type_info": self._extract_type_info(field_type),
            "description": getattr(field_info, "description", ""),
            "is_required": self._is_required_field(field_info),
            "is_site_specific": self._is_site_specific(field_name, model_name),
        }

        # Extract default value
        default_info = self._extract_default(field_info)
        if default_info:
            field_doc.update(default_info)

        # Extract constraints
        constraints = self._extract_constraints(field_info, field_type)
        if constraints:
            field_doc["constraints"] = constraints

        # Extract unit
        if isinstance(field_info.json_schema_extra, dict):
            unit = field_info.json_schema_extra.get("unit")
            if unit:
                field_doc["unit"] = unit

        # Extract enum options
        enum_class = self._get_enum_class(field_info, field_type)
        if enum_class:
            options = self._extract_enum_options(
                field_info.description or "", enum_class, include_internal
            )
            if options:
                field_doc["options"] = options

        # Find nested model
        nested_model = self._find_nested_model(field_type)
        if nested_model:
            field_doc["nested_model"] = nested_model

        return field_doc

    def _extract_type_info(self, type_hint: Any) -> Dict[str, Any]:
        """Extract detailed type information."""
        origin = get_origin(type_hint)
        args = get_args(type_hint)

        type_info = {
            "origin": origin.__name__
            if origin and hasattr(origin, "__name__")
            else None,
            "args": [self._get_type_string(arg) for arg in args] if args else [],
            "is_optional": self._is_optional(type_hint),
            "is_list": origin in {list, List},
            "is_dict": origin in {dict, Dict},
            "is_union": origin is Union,
        }

        # Check for RefValue/FlexibleRefValue
        if origin and hasattr(origin, "__name__"):
            if origin.__name__ in {"RefValue", "FlexibleRefValue"}:
                type_info["is_ref_value"] = True
                type_info["ref_type"] = origin.__name__

        return type_info

    def _extract_default(self, field_info: FieldInfo) -> Optional[Dict[str, Any]]:
        """Extract default value information."""
        if (
            field_info.default is not None
            and field_info.default != inspect.Parameter.empty
        ):
            return self._serialize_value(field_info.default, "default")
        elif field_info.default_factory is not None:
            try:
                value = field_info.default_factory()
                return self._serialize_value(value, "default_factory")
            except:
                return {
                    "default_type": "default_factory",
                    "default": "Dynamically generated",
                }
        return None

    def _serialize_value(self, value: Any, default_type: str) -> Dict[str, Any]:
        """Serialize a value for JSON, handling complex types."""
        result = {"default_type": default_type}

        if isinstance(value, BaseModel):
            result["default"] = f"{value.__class__.__name__} object"
            result["is_complex"] = True
        elif isinstance(value, Enum):
            result["default"] = {"value": value.value, "name": value.name}
            result["is_enum"] = True
        elif isinstance(value, list) and value and isinstance(value[0], BaseModel):
            result["default"] = (
                f"List of {len(value)} {value[0].__class__.__name__} objects"
            )
            result["is_complex"] = True
        elif isinstance(value, (dict, list)) and len(str(value)) > 200:
            result["default"] = f"{type(value).__name__} with {len(value)} items"
            result["is_complex"] = True
        else:
            # Simple types
            result["default"] = value
            result["is_complex"] = False

        return result

    def _extract_constraints(
        self, field_info: FieldInfo, field_type: Any
    ) -> Optional[Dict[str, Any]]:
        """Extract field constraints."""
        constraints = {}

        # Numeric constraints
        for attr in ["gt", "ge", "lt", "le", "multiple_of"]:
            if hasattr(field_info, attr):
                val = getattr(field_info, attr)
                if val is not None:
                    constraints[attr] = val

        # String constraints
        for attr in ["min_length", "max_length", "pattern"]:
            if hasattr(field_info, attr):
                val = getattr(field_info, attr)
                if val is not None:
                    constraints[attr] = val

        # Literal constraints
        origin = get_origin(field_type)
        args = get_args(field_type)

        from typing import Literal

        if origin is Literal:
            constraints["allowed_values"] = list(args)
        elif origin is Union:
            literal_values = []
            for arg in args:
                if get_origin(arg) is Literal:
                    literal_values.extend(get_args(arg))
            if literal_values:
                constraints["allowed_values"] = literal_values

        return constraints if constraints else None

    def _extract_enum_options(
        self, description: str, enum_class: Type[Enum], include_internal: bool
    ) -> List[Dict[str, Any]]:
        """Extract options from enum class and description."""
        if "Options:" not in description:
            return []

        import re

        options = []
        options_text = description.split("Options:", 1)[1].strip()

        for opt_text in options_text.split(";"):
            opt_text = opt_text.strip()
            if not opt_text:
                continue

            match = re.match(r"^(\d+(?:-\d+)?)\s*\(([^)]+)\)\s*=\s*(.+)$", opt_text)
            if match:
                num, name, desc = match.groups()

                # Check if internal
                if not include_internal and self._is_internal_option(num, enum_class):
                    continue

                options.append({
                    "value": num,
                    "name": name,
                    "description": desc.strip(),
                })

        return options

    def _is_internal_option(self, num: str, enum_class: Optional[Type[Enum]]) -> bool:
        """Check if an option is marked as internal."""
        if not enum_class:
            return False

        try:
            if "-" in num:
                start, end = map(int, num.split("-"))
                for v in range(start, end + 1):
                    for member in enum_class:
                        if member.value == v and getattr(member, "_internal", False):
                            return True
            else:
                for member in enum_class:
                    if member.value == int(num) and getattr(member, "_internal", False):
                        return True
        except:
            pass

        return False

    def _get_enum_class(
        self, field_info: FieldInfo, field_type: Any
    ) -> Optional[Type[Enum]]:
        """Extract enum class from field type."""
        origin = get_origin(field_type)
        args = get_args(field_type)

        # Check Union types
        if origin is Union:
            for arg in args:
                enum_class = self._get_enum_class(field_info, arg)
                if enum_class:
                    return enum_class

        # Check RefValue[EnumClass]
        if (
            origin
            and hasattr(origin, "__name__")
            and origin.__name__ in {"RefValue", "FlexibleRefValue"}
        ):
            if args and hasattr(args[0], "__bases__") and Enum in args[0].__bases__:
                return args[0]

        # Direct enum
        if hasattr(field_type, "__bases__") and Enum in field_type.__bases__:
            return field_type

        return None

    def _find_nested_model(self, field_type: Any) -> Optional[str]:
        """Find nested model name in field type."""
        origin = get_origin(field_type)
        args = get_args(field_type)

        # Check all possible types
        possible_types = [field_type]
        if args:
            possible_types.extend(args)

        # Check inside List/Dict
        for arg in args:
            if get_origin(arg) in {list, dict}:
                possible_types.extend(get_args(arg))

        for pt in possible_types:
            origin_pt = get_origin(pt) or pt
            if (
                hasattr(origin_pt, "__name__")
                and origin_pt.__name__ in self.all_models
                and issubclass(origin_pt, BaseModel)
            ):
                return origin_pt.__name__

        return None

    def _get_type_string(self, type_hint: Any) -> str:
        """Get string representation of type."""
        origin = get_origin(type_hint)
        args = get_args(type_hint)

        if origin is Union:
            if len(args) == 2 and type(None) in args:
                non_none = next(arg for arg in args if arg is not type(None))
                return f"Optional[{self._get_type_string(non_none)}]"
            return f"Union[{', '.join(self._get_type_string(arg) for arg in args)}]"
        elif origin is list:
            return f"List[{self._get_type_string(args[0])}]" if args else "List"
        elif origin is dict:
            if args and len(args) == 2:
                return f"Dict[{self._get_type_string(args[0])}, {self._get_type_string(args[1])}]"
            return "Dict"
        elif hasattr(type_hint, "__name__"):
            return type_hint.__name__
        else:
            return str(type_hint)

    def _get_model_title(self, model_class: Type[BaseModel]) -> str:
        """Get display title for model."""
        if hasattr(model_class, "model_config"):
            config = model_class.model_config
            if isinstance(config, dict) and "title" in config:
                return config["title"]

        # Default: convert name to title case
        name = model_class.__name__

        # Special case for SUEWS (keep as acronym)
        if name == "SUEWSConfig":
            return "SUEWS Config"

        # Insert spaces before capitals (but not for consecutive capitals)
        import re

        # This regex inserts space before capital letters that follow lowercase letters
        spaced = re.sub(r"([a-z])([A-Z])", r"\1 \2", name)
        # Also handle cases like "LAIParams" -> "LAI Params"
        spaced = re.sub(r"([A-Z]+)([A-Z][a-z])", r"\1 \2", spaced)
        return spaced.replace("_", " ").strip()

    def _should_skip_field(self, field_info: FieldInfo, include_internal: bool) -> bool:
        """Check if field should be skipped."""
        return (
            not include_internal
            and isinstance(field_info.json_schema_extra, dict)
            and field_info.json_schema_extra.get("internal_only", False)
        )

    def _is_required_field(self, field_info: FieldInfo) -> bool:
        """Check if field is required."""
        if (
            field_info.default is not None
            and field_info.default != inspect.Parameter.empty
        ):
            # Check for PydanticUndefined
            default_str = str(field_info.default)
            return (
                "PydanticUndefined" in default_str or "undefined" in default_str.lower()
            )
        return field_info.default is None and field_info.default_factory is None

    def _is_optional(self, type_hint: Any) -> bool:
        """Check if type is Optional."""
        origin = get_origin(type_hint)
        if origin is Union:
            args = get_args(type_hint)
            return type(None) in args
        return False

    def _is_site_specific(self, field_name: str, model_name: str) -> bool:
        """Check if field is site-specific."""
        site_patterns = [
            "lat",
            "lng",
            "longitude",
            "latitude",
            "alt",
            "altitude",
            "timezone",
            "area",
            "height",
            "width",
            "depth",
            "z",
            "z_meas",
            "tstep",
            "forcing_file",
            "output_file",
            "start_time",
            "end_time",
            "population",
            "traffic",
            "popdens",
            "albedo",
            "emissivity",
            "z0m_in",
            "zdm_in",
            "temp_c",
            "temp_s",
            "tsurf",
            "tair",
            "state_",
            "initial",
            "soilstore",
            "soil_moisture",
            "fraction",
            "frac",
            "lai",
            "veg_frac",
            "bldg_frac",
            "conductance",
            "resistance",
            "g_max",
            "g_min",
            "ohm",
            "qf",
            "qh",
            "qe",
            "qs",
            "qn",
            "bldg_height",
            "wall_area",
            "roof_area",
        ]

        model_contexts = [
            "initialstate",
            "properties",
            "landcover",
            "modelcontrol",
            "site",
            "spartacus",
        ]

        field_lower = field_name.lower()
        model_lower = model_name.lower()

        return any(p in field_lower for p in site_patterns) or any(
            c in model_lower for c in model_contexts
        )

    def _is_root_model(self, model_name: str) -> bool:
        """Check if model is a root model."""
        # Simple heuristic: root models are typically named
        # Model, Site, or similar top-level names
        root_names = {"Model", "Site"}
        return model_name in root_names

    def _build_hierarchy_tree(self, models_doc: Dict[str, Dict]) -> Dict[str, Any]:
        """Build model hierarchy tree."""
        tree = {}

        for model_name, model_doc in models_doc.items():
            children = model_doc.get("nested_models", [])
            if children:
                tree[model_name] = children

        return tree

    def save_json(self, data: Dict[str, Any], filepath: Path) -> None:
        """Save extracted documentation to JSON file."""
        with open(filepath, "w", encoding="utf-8") as f:
            json.dump(data, f, indent=2, default=str)

    def load_json(self, filepath: Path) -> Dict[str, Any]:
        """Load documentation from JSON file."""
        with open(filepath, "r", encoding="utf-8") as f:
            return json.load(f)
