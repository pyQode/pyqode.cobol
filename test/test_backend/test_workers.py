from pyqode.cobol.backend.workers import CobolAnalyserProvider


def test_code_completion():
    with open('test/testfiles/VIRTUAL-PRINTER.cbl', 'r') as f:
        code = f.read()
    provider = CobolAnalyserProvider()
    completions = provider.complete(code, 0, 0, '', '', '')
    assert len(completions) == 491
