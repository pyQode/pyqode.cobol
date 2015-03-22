"""
To test comment/uncomment one of more line. This test is very similar
to the auto indent test except that we use the | to mark the line tha must be
selected before calling comment/uncomment.

E.g.::

    blabla
    |blabla
    |blabla
    |blabla
    blabla
    ->
    blabla
    *> blabla
    *> blabla
    *> blabla
    blabla
"""
import glob
import sys
from pyqode.core.api import TextHelper
from pyqode.qt import QtCore, QtWidgets
from pyqode.qt.QtTest import QTest
import pytest
from test.helpers import cwd_at


class Context:
    """
    A context is loaded from a .in or .out file. It basically consist of a
    code fragment and the text cursor position.

    """
    def __init__(self, file_path):
        self.input_code = ''
        self.expected_code = ''
        self.sel_start = 0
        self.sel_end = 0
        self._load(file_path)

    def _get_line_range(self, code):
        start = sys.maxsize
        end = -1
        for i, line in enumerate(code.splitlines()):
            if line.startswith('|'):
                if i < start:
                    start = i
                end = i
        if start == sys.maxsize and end == -1:
            start = 0
            end = len(code.splitlines()) - 1
        return start, end

    def _load(self, file_path):
        with open(file_path) as file:
            code = file.readlines()
        self.symbol = code[0].replace('symbol: ', '').replace('\n', '')
        code = ''.join(code[1:])
        input_context, output_context = code.split('\n->\n')
        self.sel_start, self.sel_end = self._get_line_range(input_context)
        self.input_code = input_context.replace('|', '')
        self.expected_code = output_context.replace('|', '')


class Case:
    """
    Auto indent test case.

    The test consists of 3 steps:
        - setup input context (.in file)
        - execute a key pressed event with Key_Return
        - compare the results with the output context (.out file)

    """

    def __init__(self, file_path):
        self.name = file_path
        self.context = Context(file_path)

    def run(self, editor):
        editor.comment_indicator = self.context.symbol
        editor.setPlainText(self.context.input_code, '', '')
        TextHelper(editor).select_lines(self.context.sel_start,
                                        self.context.sel_end)
        mode = editor.modes.get('CommentsMode')
        mode.comment()
        assert editor.toPlainText() == self.context.expected_code


@cwd_at('test/test_modes')
def collect_cases():
    cases = []
    for file_path in sorted(glob.glob('comment_cases/*.ctx')):
        # if '44' in file_path:
        cases.append(Case(file_path))
    return cases


@pytest.mark.parametrize('test_case', collect_cases())
def test_comment_selection(editor, test_case):
    QtWidgets.QApplication.setActiveWindow(editor)
    editor.setFocus(True)
    test_case.run(editor)


def test_key_pressed(editor):
    editor.free_format = False
    editor.comment_indicator = '*> '
    editor.setPlainText('123456DISPLAY "Hello".', '', '')
    QTest.keyPress(editor, QtCore.Qt.Key_Slash, QtCore.Qt.ControlModifier)
    assert editor.toPlainText() == '123456*> DISPLAY "Hello".'
    QTest.keyPress(editor, QtCore.Qt.Key_Slash, QtCore.Qt.ControlModifier)
    assert editor.toPlainText() == '123456DISPLAY "Hello".'


def test_disabled(editor):
    editor.setPlainText('  DISPLAY "Hello".', '', '')
    mode = editor.modes.get('CommentsMode')
    mode.enabled = False
    QTest.keyPress(editor, QtCore.Qt.Key_Slash, QtCore.Qt.ControlModifier)
    assert '  DISPLAY "Hello".' in editor.toPlainText()
