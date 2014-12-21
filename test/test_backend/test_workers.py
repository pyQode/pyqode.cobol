from pyqode.cobol.backend.workers import CobolCodeCompletionProvider


def test_code_completion():
    with open('test/testfiles/VIRTUAL-PRINTER.cbl', 'r') as f:
        code = f.read()
    provider = CobolCodeCompletionProvider()
    completions = provider.complete(code, 0, 0, '', '', '')
    assert len(completions) == 491
