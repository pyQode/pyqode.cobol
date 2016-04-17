import functools
import os
import pytest
from pyqode.qt import QtTest
from pyqode.core.api import folding, TextHelper, TextBlockHelper, FoldScope
from pyqode.cobol.widgets import CobolCodeEdit


def delete_file_on_return(path):
    """
    Decorator to run function at `path`.

    :type path: str
    :arg path: relative path from repository root (e.g., 'pyqode' or 'test').
    """
    def decorator(func):
        @functools.wraps(func)
        def wrapper(*args, **kwds):
            try:
                return func(*args, **kwds)
            finally:
                try:
                    os.remove(path)
                except (IOError, OSError):
                    pass
        return wrapper
    return decorator


class FoldDetectorTestCase:
    """
    Checks that the fold detector detects the corret layout when loading
    files (or setting text). Dynamic checks (i.e. when the user edit the text
    are performed by DynamicFoldDetectorTestCase).
    """
    def __init__(self, test_file, results_file):
        with open(test_file, 'r') as f:
            self.test_file_content = f.read()
        with open(results_file, 'r') as f:
            self.expected_results_content = f.read()

    @delete_file_on_return('file_structure')
    def execute(self, free_format=False):
        editor = CobolCodeEdit(free_format=free_format)
        editor.setPlainText(self.test_file_content, '', '')
        with open('file_structure', 'w') as f:
            folding.print_tree(editor, file=f, print_blocks=True)
        with open('file_structure', 'r') as f:
            results_content = f.read()
        assert results_content == self.expected_results_content
        editor.clear()


@pytest.mark.parametrize('case, free_format', [
    (FoldDetectorTestCase('test/test_api/folding_cases/cobjapi.cob',
                          'test/test_api/folding_cases/cobjapi.static_results'), True),
    (FoldDetectorTestCase('test/test_api/folding_cases/cobolmac.cob',
                          'test/test_api/folding_cases/cobolmac.static_results'), True),
    (FoldDetectorTestCase('test/test_api/folding_cases/foo.cbl',
                          'test/test_api/folding_cases/foo.static_results'), False),

])
def test_fold_detection_static(case, free_format):
    case.execute(free_format=free_format)


section_code = """       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-PROGRAM.
       DATA DIVISION.
       PROCEDURE DIVISION.
       END PROGRAM TEST-PROGRAM.
"""


def test_dynamic_folding_insert_section():
    # test insert section under empty data division
    # data division (which is not a fold trigger initially) block will become a fold trigger
    editor = CobolCodeEdit()
    editor.setPlainText(section_code, '', '')
    th = TextHelper(editor)
    block = editor.document().findBlockByNumber(2)
    assert TextBlockHelper.is_fold_trigger(block) is False
    cursor = th.goto_line(2, column=len('       DATA DIVISION.'))
    cursor.insertText('\n       WORKING-STORAGE SECTION.')
    assert TextBlockHelper.is_fold_trigger(block) is True


end_if_code = """       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-PROGRAM.
       DATA DIVISION.
       PROCEDURE DIVISION.
       IF FOO
          DISPLAY "FOO"

       DISPLAY "FOO"
       END PROGRAM TEST-PROGRAM.
"""


def test_dynamic_folding_insert_end_if():
    # test insert section under empty data division
    # data division (which is not a fold trigger initially) block will become a fold trigger
    editor = CobolCodeEdit()
    editor.setPlainText(end_if_code, '', '')
    editor.show()
    th = TextHelper(editor)
    block = editor.document().findBlockByNumber(4)
    first, last = FoldScope(block).get_range()
    assert first == 4
    assert last == 8
    cursor = th.goto_line(5, column=len('          DISPLAY "FOO"'))
    cursor.insertText('\n       END-IF')
    first, last = FoldScope(block).get_range()
    assert first == 4
    assert last == 6


end_perform_code = """       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-PROGRAM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 A PIC 9(2) VALUE '0'.
       PROCEDURE DIVISION.
       PERFORM A TIMES
          DISPLAY "FOO"

       DISPLAY "FOO"
       END PROGRAM TEST-PROGRAM.
"""


def test_dynamic_folding_insert_end_perform():
    # test insert section under empty data division
    # data division (which is not a fold trigger initially) block will become a fold trigger
    editor = CobolCodeEdit()
    editor.setPlainText(end_perform_code, '', '')
    editor.show()
    th = TextHelper(editor)
    block = editor.document().findBlockByNumber(6)
    first, last = FoldScope(block).get_range()
    assert first == 6
    assert last == 10
    cursor = th.goto_line(7, column=len('          DISPLAY "FOO"'))
    cursor.insertText('\n       END-PERFORM')
    first, last = FoldScope(block).get_range()
    assert first == 6
    assert last == 8
