from pydantic import BaseModel, ConfigDict


class TestModel1(BaseModel):
    model_config = ConfigDict(title="Test Title")


class TestModel2(BaseModel):
    model_config = ConfigDict(title="Test Title 2")


print("TestModel1 config:", TestModel1.model_config)
print("TestModel2 config:", TestModel2.model_config)
print("Title in TestModel1:", "title" in TestModel1.model_config)
print("Title in TestModel2:", "title" in TestModel2.model_config)
