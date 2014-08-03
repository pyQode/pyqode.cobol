"""
This module contains the cobol code edit widget.
"""
from pyqode.core import api, panels, modes
from pyqode.cobol import modes as cobmodes
from pyqode.cobol.api import CobolFoldDetector


class CobolCodeEdit(api.CodeEdit):
    """
    CodeEdit specialized for cobol source code editing.
    """
    @property
    def free_format(self):
        return self._free_format

    @free_format.setter
    def free_format(self, val):
        self._free_format = val

    @property
    def comment_indicator(self):
        return self._comment_indicator

    @comment_indicator.setter
    def comment_indicator(self, value):
        self._comment_indicator = value

    def __init__(self, parent=None):
        super().__init__(parent)
        self._free_format = False
        self._comment_indicator = '*> '
        #
        # setup panels
        #
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
        #
        # setup modes
        #
        self.caret_line_mode = self.modes.append(
            modes.CaretLineHighlighterMode()
        )
        self.zoom_mode = self.modes.append(
            modes.ZoomMode()
        )
        self.indenter_mode = self.modes.append(
            modes.IndenterMode()
        )
        self.indenter_mode.min_indent = 7
        self.case_converter = self.modes.append(
            modes.CaseConverterMode()
        )
        self.code_completion_mode = self.modes.append(
            modes.CodeCompletionMode())
        self.code_completion_mode.trigger_symbols[:] = []
        self.auto_indent_mode = self.modes.append(
            modes.AutoIndentMode()
        )
        self.auto_indent_mode.min_indent = 7 * ' '
        self.word_click_mode = self.modes.append(
            modes.WordClickMode()
        )
        self.modes.append(cobmodes.CobolSyntaxHighlighter(self.document()))
        self.syntax_highlighter.fold_detector = CobolFoldDetector()

        self.left_margin = self.modes.append(cobmodes.LeftMarginMode())
        self.right_margin = self.modes.append(modes.RightMarginMode())
        self.right_margin.position = 72
        self.comments_mode = self.modes.append(cobmodes.CommentsMode())
