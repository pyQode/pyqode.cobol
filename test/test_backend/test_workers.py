from pyqode.cobol.backend.workers import CobolCodeCompletionProvider
from pyqode.cobol.api.keywords import INTRINSICS, RESERVED

def test_code_completion():
    with open('test/testfiles/VIRTUAL-PRINTER.cbl', 'r') as f:
        code = f.read()
    provider = CobolCodeCompletionProvider()
    completions = provider.complete(code, 0, 0, '', '', '')
    assert len(completions) >= len(INTRINSICS + RESERVED)
