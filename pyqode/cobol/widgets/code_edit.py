"""
This module contains the cobol code edit widget.
"""
import os
import sys
from pyqode.cobol.backend import server
from pyqode.core import api, panels, modes
from pyqode.cobol import modes as cobmodes
from pyqode.cobol.api import CobolFoldDetector
from pyqode.cobol._forms import resources_rc
from pyqode.core.backend import NotConnected
from pyqode.core.qt import QtCore


class CobolCodeEdit(api.CodeEdit):
    """
    CodeEdit specialized for cobol source code editing.
    """
    @property
    def free_format(self):
        return self._free_format

    def _update_backend_format(self):
        from pyqode.cobol.backend.workers import set_free_format
        try:
            self.backend.send_request(set_free_format, self.free_format)
        except NotConnected:
            QtCore.QTimer.singleShot(100, self._update_backend_format)

    @free_format.setter
    def free_format(self, free_fmt):
        if free_fmt != self._free_format:
            self._free_format = free_fmt
            self.min_indent_column = 7 if not free_fmt else 0
            self.left_margin.enabled = not free_fmt
            self.right_margin.enabled = not free_fmt
            self.syntax_highlighter.rehighlight()
            self._update_backend_format()

    @property
    def comment_indicator(self):
        return self._comment_indicator

    @comment_indicator.setter
    def comment_indicator(self, value):
        self._comment_indicator = value

    def __init__(self, parent=None):
        super().__init__(parent)
        self._free_format = False
        self.min_indent_column = 7
        self._comment_indicator = '*> '
        self.word_separators.remove('-')
        self._start_server()
        self._setup_panels()
        self._setup_modes()

    def _start_server(self):
        if hasattr(sys, "frozen"):
            cwd = os.path.dirname(sys.executable)
            base = 'cobol-backend'
            srv = base + '.exe' if sys.platform == 'win32' else base
            srv = os.path.join(cwd, srv)
            self.backend.start(srv)
        else:
            self.backend.start(server.__file__)

    def _setup_modes(self):
        self.outline_mode = self.modes.append(
            cobmodes.DocumentOutlineMode()
        )
        self.goto_def_mode = self.modes.append(
            cobmodes.GoToDefinitionMode()
        )
        self.code_completion_mode = self.modes.append(
            modes.CodeCompletionMode()
        )
        self.code_completion_mode.trigger_symbols[:] = []
        self.file_watcher = self.modes.append(
            modes.FileWatcherMode()
        )
        self.auto_indent_mode = self.modes.append(
            cobmodes.CobolAutoIndentMode()
        )
        self.caret_line_mode = self.modes.append(
            modes.CaretLineHighlighterMode()
        )
        self.zoom_mode = self.modes.append(
            modes.ZoomMode()
        )
        self.indenter_mode = self.modes.append(
            modes.IndenterMode()
        )
        self.case_converter = self.modes.append(
            modes.CaseConverterMode()
        )
        self.auto_indent_mode = self.modes.append(
            modes.AutoIndentMode()
        )
        self.modes.append(cobmodes.CobolSyntaxHighlighter(self.document()))
        self.syntax_highlighter.fold_detector = CobolFoldDetector()
        self.left_margin = self.modes.append(cobmodes.LeftMarginMode())
        self.right_margin = self.modes.append(modes.RightMarginMode())
        self.right_margin.position = 72
        self.comments_mode = self.modes.append(cobmodes.CommentsMode())
        self.add_separator()
        self.offset_calculator = self.modes.append(
            cobmodes.OffsetCalculatorMode())

    def _setup_panels(self):
        self.folding_panel = self.panels.append(
            panels.FoldingPanel(), api.Panel.Position.LEFT
        )
        self.line_nbr_panel = self.panels.append(
            panels.LineNumberPanel(), api.Panel.Position.LEFT
        )
        self.checker_panel = self.panels.append(
            panels.CheckerPanel(), api.Panel.Position.LEFT
        )
        self.encoding_panel = self.panels.append(
            panels.EncodingPanel(), api.Panel.Position.TOP
        )
        self.search_panel = self.panels.append(
            panels.SearchAndReplacePanel(), api.Panel.Position.BOTTOM
        )
