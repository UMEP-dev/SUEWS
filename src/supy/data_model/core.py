from typing import Dict, List, Optional, Union, Literal, Tuple, Type, Generic, TypeVar
from pydantic import (
    BaseModel,
    Field,
    model_validator,
    field_validator,
    PrivateAttr,
    conlist,
)
import pandas as pd
import yaml
import datetime


from .model import Model
from .site import Site, SiteProperties, InitialStates
from .. import load_sample_data


class SUEWSConfig(BaseModel):
    name: str = Field(
        default="sample config", description="Name of the SUEWS configuration"
    )
    description: str = Field(
        default="this is a sample config for testing purposes ONLY - values are not realistic",
        description="Description of this SUEWS configuration",
    )
    model: Model = Field(
        default_factory=Model,
        description="Model control and physics parameters",
    )
    site: List[Site] = Field(
        default=[Site()],
        description="List of sites to simulate",
        min_items=1,
    )

    class Config:
        extra = "allow"

    @classmethod
    def export_schema_json(
        cls,
        path: str = "./suews-config-schema.json",
        *,
        indent: int = 2,
        by_alias: bool = True,
        ref_template: str = "#/$defs/{model}",
        schema_version: str = "http://json-schema.org/draft/2020-12/schema",
        include_enum_descriptions: bool = True,
        include_title: bool = True,
        include_field_examples: bool = True,
        include_refs: bool = True,
    ) -> None:
        """Export the SUEWSConfig schema to a JSON file.

        This function exports the complete Pydantic model schema of SUEWSConfig,
        including all nested models and their validation rules, to a JSON Schema file.

        Args:
            path (str, optional): Path where to save the schema file.
                Defaults to "./suews-config-schema.json".
            indent (int, optional): Number of spaces for JSON indentation.
                Defaults to 2.
            by_alias (bool, optional): Whether to use field aliases in the schema.
                Defaults to True.
            ref_template (str, optional): Template for JSON Schema references.
                Defaults to "#/$defs/{model}".
            schema_version (str, optional): JSON Schema version to use.
                Defaults to "http://json-schema.org/draft/2020-12/schema".
            include_enum_descriptions (bool, optional): Whether to include enum value descriptions.
                Defaults to True.
            include_title (bool, optional): Whether to include model titles.
                Defaults to True.
            include_field_examples (bool, optional): Whether to include field examples.
                Defaults to True.
            include_refs (bool, optional): Whether to include schema references.
                Defaults to True.
        """
        import json
        from pathlib import Path

        # Get the JSON schema with custom options
        schema = cls.model_json_schema(
            by_alias=by_alias,
            ref_template=ref_template,
            mode="validation",  # This ensures validation rules are included
        )

        # Add schema version
        schema["$schema"] = schema_version

        # Process schema based on options
        if not include_title and "title" in schema:
            del schema["title"]

        def process_schema_node(node):
            if isinstance(node, dict):
                # Handle enum descriptions
                if not include_enum_descriptions and "enum" in node:
                    if "description" in node:
                        del node["description"]

                # Handle examples
                if not include_field_examples and "examples" in node:
                    del node["examples"]

                # Handle references
                if not include_refs and "$ref" in node:
                    pass

                # Ensure validation rules are preserved
                if "$defs" in node:
                    for def_name, def_schema in node["$defs"].items():
                        if def_name == "SiteProperties":
                            def_schema["additionalProperties"] = False
                            def_schema.setdefault("required", []).extend([
                                "lat", "lng", "alt", "timezone", "surfacearea",
                                "z", "z0m_in", "zdm_in", "pipecapacity",
                                "runofftowater", "narp_trans_site"
                            ])

                # Recursively process all dictionary values
                for key, value in node.items():
                    node[key] = process_schema_node(value)

            elif isinstance(node, list):
                return [process_schema_node(item) for item in node]

            return node

        # Process the entire schema
        schema = process_schema_node(schema)

        # Add metadata
        schema["metadata"] = {
            "generator": "supy.data_model.SUEWSConfig",
            "generated_at": datetime.datetime.now(datetime.UTC).isoformat(),
            "version": "1.0.0",
            "format_options": {
                "by_alias": by_alias,
                "include_enum_descriptions": include_enum_descriptions,
                "include_title": include_title,
                "include_field_examples": include_field_examples,
                "include_refs": include_refs,
            }
        }

        # Ensure parent directory exists
        Path(path).parent.mkdir(parents=True, exist_ok=True)

        # Write schema to file with custom formatting
        with open(path, "w", encoding="utf-8") as f:
            json.dump(
                schema,
                f,
                indent=indent,
                ensure_ascii=False,
                sort_keys=True,  # Consistent ordering
            )

    @classmethod
    def from_yaml(cls, path: str) -> "SUEWSConfig":
        """Initialize SUEWSConfig from YAML file.

        Args:
            path (str): Path to YAML configuration file

        Returns:
            SUEWSConfig: Instance of SUEWSConfig initialized from YAML
        """
        with open(path, "r") as file:
            config = yaml.load(file, Loader=yaml.FullLoader)
        return cls(**config)

    def create_multi_index_columns(self, columns_file: str) -> pd.MultiIndex:
        """Create MultiIndex from df_state_columns.txt"""
        with open(columns_file, "r") as f:
            lines = f.readlines()

        tuples = []
        for line in lines:
            col_name, indices = line.strip().split(",", 1)
            str_indices = f"{indices}" if indices != "0" else "0"
            tuples.append((col_name, str_indices))

        return pd.MultiIndex.from_tuples(tuples)

    def to_df_state(self) -> pd.DataFrame:
        """Convert config to DataFrame state format"""
        list_df_site = []
        for i in range(len(self.site)):
            grid_id = self.site[i].gridiv
            df_site = self.site[i].to_df_state(grid_id)
            df_model = self.model.to_df_state(grid_id)
            df_site = pd.concat([df_site, df_model], axis=1)
            list_df_site.append(df_site)

        df = pd.concat(list_df_site, axis=0)
        # remove duplicate columns
        df = df.loc[:, ~df.columns.duplicated()]

        # set index name
        df.index.set_names("grid", inplace=True)
        # set column names
        df.columns.set_names(["var", "ind_dim"], inplace=True)

        # Reindex columns to match the sample data columns order
        sample_columns = load_sample_data()[0].columns

        # Reindex to match sample columns and then append non-matching columns
        df = df.reindex(columns=sample_columns)
        return df

    @classmethod
    def from_df_state(cls, df: pd.DataFrame) -> "SUEWSConfig":
        """Create config from DataFrame state format.

        Args:
            df (pd.DataFrame): DataFrame containing SUEWS configuration state.

        Returns:
            SUEWSConfig: Instance of SUEWSConfig reconstructed from DataFrame.
        """
        # Initialize with default values
        config = cls()

        # Get grid IDs from DataFrame index
        grid_ids = df.index.tolist()

        # Create list of sites
        sites = []
        for grid_id in grid_ids:
            # Create site instance
            site = Site(gridiv=grid_id)

            # Set site properties
            site_properties = SiteProperties.from_df_state(df, grid_id)
            site.properties = site_properties

            # Set initial states
            initial_states = InitialStates.from_df_state(df, grid_id)
            site.initial_states = initial_states

            sites.append(site)

        # Update config with reconstructed data
        config.site = sites

        # Reconstruct model
        config.model = Model.from_df_state(df, grid_ids[0])


        return config

    def to_yaml(self, path: str = "./config-suews.yml"):
        """Convert config to YAML format"""
        with open(path, "w") as file:
            yaml.dump(
                self.model_dump(exclude_none=True),
                file,
                sort_keys=False,
                allow_unicode=True,
            )


def init_config_from_yaml(path: str = "./config-suews.yml") -> SUEWSConfig:
    """Initialize SUEWSConfig from YAML file"""
    with open(path, "r") as file:
        config = yaml.load(file, Loader=yaml.FullLoader)
    return SUEWSConfig(**config)
