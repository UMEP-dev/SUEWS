from pydantic import BaseModel, Field, ValidationError, model_validator
from typing import Optional, List


class Class1(BaseModel):
    rule1: Optional[float] = Field(default=None, gt=0, description="Must be > 0")
    rule2: Optional[float] = Field(default=None, gt=10, description="Must be > 10")

    @model_validator(mode="after")
    def rule3(self) -> "Class1":
        if self.rule1 is not None and self.rule2 is not None:
            if self.rule2 < 2 * self.rule1:
                raise ValueError("rule3 failed: rule2 must be at least twice rule1")
        return self


class Class2(BaseModel):
    rule1: Optional[float] = Field(default=None, gt=5, description="Must be > 5")
    rule2: Optional[float] = Field(default=None, ge=0, description="Must be >= 0")
    rule3: Optional[float] = Field(default=None, lt=100, description="Must be < 100")

    @model_validator(mode="after")
    def check_combination(self) -> "Class2":
        if self.rule1 is not None and self.rule2 is not None:
            if (self.rule1 + self.rule2) >= 100:
                raise ValueError("rule3 failed: sum of rule1 and rule2 must be < 100")
        return self


class Controller(BaseModel):
    model1_on: bool = False
    model2_on: bool = False

    class1: Class1
    class2: Class2

    def enforce(self):
        print("=== Running Controller ===")
        errors: List[str] = []

        def validate_rule(model_cls, field_name, value, source_model):
            try:
                model_cls.construct(**{field_name: value})
                model_cls.parse_obj({field_name: value})
            except ValidationError as e:
                for err in e.errors():
                    msg = err['msg']
                    errors.append(f"[{source_model}] {model_cls.__name__}: {field_name} failed - {msg}")

        if self.model1_on:
            print("Model1 is ON: checking Class1.rule2 and Class2.rule2")
            if self.class1.rule2 is not None:
                validate_rule(Class1, 'rule2', self.class1.rule2, "Model1")
            if self.class2.rule2 is not None:
                validate_rule(Class2, 'rule2', self.class2.rule2, "Model1")

        if self.model2_on:
            print("Model2 is ON: checking Class1.rule1 and Class2.rule1 + rule3")
            if self.class1.rule1 is not None:
                validate_rule(Class1, 'rule1', self.class1.rule1, "Model2")
            if self.class2.rule1 is not None:
                validate_rule(Class2, 'rule1', self.class2.rule1, "Model2")
            if self.class2.rule3 is not None:
                validate_rule(Class2, 'rule3', self.class2.rule3, "Model2")

        if not self.model1_on and not self.model2_on:
            print("No models are ON. Nothing to check.")
            return

        if errors:
            print("\nValidation Errors:")
            for err in errors:
                print("  -", err)
        else:
            print("\nAll active rules passed.")



