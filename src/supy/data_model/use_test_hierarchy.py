from test_hierarchy import Controller, Class1, Class2
import json

if __name__ == "__main__":
    #c1 = Class1.model_construct(rule1=0, rule2=15)
    #c2 = Class2.model_construct(rule1=4, rule2=97, rule3=105)

    c1 = Class1.model_construct(rule1=0, rule2=5)
    c2 = Class2.model_construct(rule1=3, rule2=-1, rule3=150)

    controller = Controller.model_construct(
        model1_on=True,
        model2_on=True,
        class1=c1,
        class2=c2
    )

    controller.enforce()

    schema = Controller.model_json_schema()
    print(json.dumps(schema, indent=2))
