"""
Tests the pic fields parser
"""
from pyqode.cobol.api import get_field_infos


source = """       01 INPUT-DATA.
           05 U1-DINRAR PIC X(06)        .
           05 FILLER PIC X(01)  VALUE ';'.
           05 U1-DINR PIC X(07).
           05 FILLER PIC X(01).
           05 U1-ORGNR PIC X(10).
           05 FILLER PIC X(01).
           05 U1-INKDAT PIC X(08).
           05 FILLER PIC X(01).
           05 U1-TOMPER PIC X(08).
           05 FILLER PIC X(01).
           05 U1-RUBR1 PIC X(06).
           05 FILLER PIC X(01).
           05 U1-RUBR2 PIC X(06).
           05 FILLER PIC X(01).
           05 U1-RUBR3 PIC X(06).
           05 FILLER PIC X(01).
           05 U1-HDL1 PIC X(04).
           05 FILLER PIC X(01).
           05 U1-HDL2 PIC X(04).
           05 FILLER PIC X(01).
"""


def test_offset_calculator():
    """
    Test the pic parser with the example supplied in issue #14
    ( https://github.com/OpenCobolIDE/OpenCobolIDE/issues/14) and ensure the
    offset of the last element is 75.
    """
    fi = get_field_infos(source)
    assert fi[-1].offset == 75


advanced_sample = '''       78 someconst VALUE 'test'.
       01 test01.
          03 test03 PIC X(03).
             88 test-empty VALUE SPACES.
             88 test-a     VALUE "a".
             88 test-b     VALUE "b".
             88 test-c     VALUE "c".
          03 test03-2 PIC X(03).
       78 someconst2 VALUE 'test2'.
       01 test2.
          03 test2-01 PIC X.
          03 test2-03 PIC X(03).
             88 test-empty VALUE SPACES.
             88 test-a     VALUE "a".
             88 test-b     VALUE "b".
             88 test-c     VALUE "c".
          03 test2-03-red redefines test2-03.
            05 test2-03-02 PIC X(02).
               88 test2-03-02-set VALUE 'XX'.
            05 test2-03-03 PIC X(01).
               88 test2-03-03-empty VALUE SPACE.
'''


def test_advanced():
    """
    A more advanced test (cobol code given by Simon Sobisch)
    """
    results = [0, 1, 1, 1, 1, 1, 1, 4, 0, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 4, 4]
    infos = get_field_infos(advanced_sample)
    assert len(infos) == len(results)
    for info, result in zip(infos, results):
        assert info.offset == result


name_sample = """
       01  PIC-X10.
           05 FILLER                   PIC X(2).
           05 PIC-X8                   PIC X(8).
"""


def test_name():
    """
    Test if the part "PIC-" from PIC-X8 is not omitted.
    """
    infos = get_field_infos(name_sample)
    assert len(infos) == 3
    assert infos[-1].name == 'PIC-X8'
