# TEST FILE: Python formatting issues for workflow testing
# This file intentionally has formatting issues

def poorly_formatted_function(  x,y,   z ):
    """Bad spacing and indentation"""
    result=x+y*z
    if result>100:
        print("Result is large")
    else:
            print("Result is small")
    return    result


class BadlyFormattedClass:
    def __init__(self,name,value):
        self.name=name
        self.value=value
    
    def calculate(self):
        return self.value*2


# Long line that needs wrapping
very_long_variable_name_that_exceeds_the_line_limit = "This is a very long string that probably exceeds the recommended line length and should be wrapped"

# Inconsistent quotes
mixed_quotes = "This uses double quotes"
another_string = 'This uses single quotes'

# Missing blank lines between functions
def function_one():
    pass
def function_two():
    pass

# Trailing whitespace and blank lines below    

