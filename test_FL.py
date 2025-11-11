from supy.data_model.schema.publisher import generate_json_schema


def get_limits_from_json_schema(variable_name: str, schema: dict = None) -> dict:
    """
    Retrieve limits for a variable from the JSON schema.

    Args:
        variable_name: Name of the variable (e.g., 'pormin_dec')
        schema: Optional pre-generated schema dict. If None, generates
            dynamically from data models.

    Returns:
        Dictionary with 'min', 'max', 'default', 'description', 'unit' keys.
        Returns None if variable not found.
    """
    if schema is None:
        schema = generate_json_schema()

    # Search through all definitions to find the variable
    result = None

    # Check in $defs (where all model classes are defined)
    for def_name, def_content in schema.get("$defs", {}).items():
        if "properties" in def_content:
            if variable_name in def_content["properties"]:
                prop = def_content["properties"][variable_name]

                # Check if key exists to handle cases where limit is 0
                min_val = prop.get("ge") if "ge" in prop else prop.get("gt")
                max_val = prop.get("le") if "le" in prop else prop.get("lt")

                result = {
                    "variable": variable_name,
                    "model_class": def_name,
                    "min": min_val,  # ge = >=, gt = >
                    "max": max_val,  # le = <=, lt = <
                    "default": prop.get("default"),
                    "description": prop.get("description", ""),
                    "unit": prop.get("unit", ""),
                    "display_name": prop.get("display_name", "")
                }

                # Print message if limits are missing
                if min_val is None and max_val is None:
                    print(f"NOTE: Variable '{variable_name}' in {def_name} has no min/max limits defined.")
                elif min_val is None:
                    print(f"NOTE: Variable '{variable_name}' in {def_name} has no minimum limit (only max={max_val}).")
                elif max_val is None:
                    print(f"NOTE: Variable '{variable_name}' in {def_name} has no maximum limit (only min={min_val}).")

                break

    return result


laimax_name="laimax"
laimax_value = get_limits_from_json_schema(laimax_name, None)
print(laimax_value)
