"""
This module tests the defined names parser.
"""
from pyqode.cobol.api.parsers import names as parser


def test_parse_names():
    """
    Parses the hello world example
    """
    with open('test/testfiles/HelloWorld.cbl') as f:
        content = f.read()
    ast, vars, procs = parser.defined_names(content)
    # 4 divs
    assert len(ast.children) == 4
    # 2 sections in env div
    assert len(ast.children[1].children) == 2
    # 2 sections in data div
    assert len(ast.children[2].children) == 2
    assert len(vars) == 0
    assert len(procs) == 1


def test_free_parser():
    """
    HelloWorld.cbl and HelloWorldFree.cbl must have the same ast.
    """
    with open('test/testfiles/HelloWorld.cbl') as f:
        content = f.read()
    non_free_ast, non_free_vars, non_free_procs = parser.defined_names(content)
    with open('test/testfiles/HelloWorldFree.cbl') as f:
        content = f.read()
    free_ast, free_vars, free_procs = parser.defined_names(
        content, free_format=True)
    result = parser.cmp_name(non_free_ast, free_ast)
    assert result


def test_variables():
    """
    Virtual printer must have 8 vars
    """
    with open('test/testfiles/VIRTUAL-PRINTER.cbl') as f:
        content = f.read()
    ast, vars, procs = parser.defined_names(content)
    # 8 variables
    assert len(vars) == 8


def test_paragraphes():
    """
    Test printer must have 2 procedures
    """
    with open('test/testfiles/TEST-PRINTER.cbl') as f:
        content = f.read()
    ast, vars, procs = parser.defined_names(content)
    # 1 procedure
    assert len(procs) == 2


def test_malformed():
    """
    Parses the hello world example
    """
    with open('test/testfiles/MALFORMED.cbl') as f:
        content = f.read()
    ast, vars, procs = parser.defined_names(content)
    # 4 divs
    assert len(ast.children) == 4
    # 2 sections in env div
    assert len(ast.children[0].children) == 2
    # 2 sections in data div
    assert len(ast.children[1].children) == 2
    assert len(vars) == 0
    assert len(procs) == 1


def test_parse_pco():
    """
    Parses a pco file, which contains characters in column 1-6 (see bug #23)
    """
    with open('test/testfiles/HelloWorld.pco') as f:
        content = f.read()
    ast, vars, procs = parser.defined_names(content)
    # 4 divs
    assert len(ast.children) == 4
    # 2 sections in env div
    assert len(ast.children[1].children) == 2
    # 2 sections in data div
    assert len(ast.children[2].children) == 2
    assert len(vars) == 0
    assert len(procs) == 1
