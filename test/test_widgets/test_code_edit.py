from pyqode.qt.QtTest import QTest


def test_free_format(editor):
    editor.file.open('test/testfiles/TEST-PRINTER.cbl')
    assert editor.free_format is False
    editor.free_format = True
    QTest.qWait(500)
    assert editor.free_format is True
    assert editor.margins.enabled is False
    editor.free_format = False
    QTest.qWait(500)
    assert editor.free_format is False
    assert editor.margins.enabled is True


def test_comment_indicator(editor):
    editor.file.open('test/testfiles/TEST-PRINTER.cbl')
    assert editor.comment_indicator == '*> '
    editor.comment_indicator = '* '
    assert editor.comment_indicator == '* '
