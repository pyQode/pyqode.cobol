import functools
import os
import pytest
from pyqode.core.api import folding
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
