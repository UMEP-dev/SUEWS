#!/usr/bin/env python3
"""Validate test file syntax without importing pytest."""

import ast
import sys
from pathlib import Path

def validate_python_syntax(file_path):
    """Validate Python syntax of a file."""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        # Parse the AST to check syntax
        ast.parse(content)
        return True, "Valid Python syntax"
    except SyntaxError as e:
        return False, f"Syntax error: {e}"
    except Exception as e:
        return False, f"Error: {e}"

def analyze_test_structure(file_path):
    """Analyze test file structure."""
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        # Count test classes and functions
        tree = ast.parse(content)
        
        test_classes = []
        test_functions = []
        imports = []
        
        for node in ast.walk(tree):
            if isinstance(node, ast.ClassDef) and node.name.startswith('Test'):
                test_classes.append(node.name)
            elif isinstance(node, ast.FunctionDef) and node.name.startswith('test_'):
                test_functions.append(node.name)
            elif isinstance(node, ast.Import):
                for alias in node.names:
                    imports.append(alias.name)
            elif isinstance(node, ast.ImportFrom):
                module = node.module or ""
                for alias in node.names:
                    imports.append(f"{module}.{alias.name}")
        
        return {
            'classes': test_classes,
            'functions': test_functions,
            'imports': imports
        }
    
    except Exception as e:
        return {'error': str(e)}

def main():
    """Main validation function."""
    print("SUEWS MCP Server - Test Syntax Validation")
    print("=" * 50)
    
    base_dir = Path(__file__).parent
    test_files = [
        base_dir / "tests" / "conftest.py",
        base_dir / "tests" / "test_server.py"
    ]
    
    all_good = True
    
    for test_file in test_files:
        print(f"\nValidating {test_file.name}:")
        
        if not test_file.exists():
            print(f"❌ File not found: {test_file}")
            all_good = False
            continue
        
        # Check syntax
        is_valid, message = validate_python_syntax(test_file)
        if is_valid:
            print(f"✓ Syntax is valid")
        else:
            print(f"❌ {message}")
            all_good = False
            continue
        
        # Analyze structure
        analysis = analyze_test_structure(test_file)
        
        if 'error' in analysis:
            print(f"❌ Analysis error: {analysis['error']}")
            all_good = False
            continue
        
        if test_file.name == "conftest.py":
            print(f"✓ Found {len(analysis['imports'])} imports")
            print(f"✓ Found {len(analysis['functions'])} fixture functions")
        else:
            print(f"✓ Found {len(analysis['classes'])} test classes")
            print(f"✓ Found {len(analysis['functions'])} test functions") 
            print(f"✓ Found {len(analysis['imports'])} imports")
    
    print("\n" + "=" * 50)
    
    if all_good:
        print("🎉 ALL SYNTAX VALIDATIONS PASSED! 🎉")
        print("\nTest files are syntactically correct and ready to run!")
        print("\nTest Structure Summary:")
        
        # Analyze test_server.py specifically
        test_server_path = base_dir / "tests" / "test_server.py"
        analysis = analyze_test_structure(test_server_path)
        
        print(f"Test Classes: {len(analysis['classes'])}")
        for cls in analysis['classes']:
            print(f"  - {cls}")
        
        print(f"\nTotal Test Functions: {len(analysis['functions'])}")
        print("\nReady for pytest execution (when pytest is available)!")
    else:
        print("❌ SYNTAX VALIDATION FAILED!")
        print("Please fix the syntax issues above.")
    
    return all_good

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)