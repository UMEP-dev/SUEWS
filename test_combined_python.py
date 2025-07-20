# TEST FILE: Combined test - Python part
# This file has formatting issues

def unformatted_function(a,b,c):
    """Test function"""
    result=a+b*c
    if    result>0:
        return True
    else:
            return False


class TestClass:
    def __init__(self,value):
        self.value=value
    
    def process(self):
        return self.value**2