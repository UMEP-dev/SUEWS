from pydantic import BaseModel, Field, ValidationError
import yaml
from typing import List


class SnowAlb(BaseModel):
    snowalb: float = Field(ge=0, le=1, description="Snow albedo")


class WaterUse(BaseModel):
    wu_total: float = Field(ge=0, description="Total water use")
    wu_auto: float = Field(ge=0, description="Automatic water use")
    wu_manual: float = Field(ge=0, description="Manual water use")


class SUEWSConfig(BaseModel):
    snowalb: SnowAlb
    wateruse: WaterUse


if __name__ == "__main__":
    # Create list for collecting all exceptions
    exceptions: List[ValidationError] = []

    # Load YAML config
    try:
        with open("./config_test.yml", "r") as file:
            yaml_config = yaml.safe_load(file)

        # Attempt to create SUEWSConfig object
        suews_config = SUEWSConfig(**yaml_config[0])

    except ValidationError as e:
        # Collect the exception
        exceptions.append(e)

    # Print all collected exceptions
    if exceptions:
        print("Validation Errors Detected:\n")
        for exc in exceptions:
            for error in exc.errors():
                loc = " -> ".join(map(str, error["loc"]))  # Location of the error
                print(f"Field: {loc}")
                print(f"  Error: {error['msg']}")
                print(f"  Type: {error['type']}\n")
