"""
This module contains the cobol code edit widget.
"""
from pyqode.core import api, panels, modes
from pyqode.cobol import modes as cobmodes


class CobolCodeEdit(api.CodeEdit):
    """
    CodeEdit specialized for cobol source code editing.
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        #
        # setup panels
        #
        # self.folding_panel = self.panels.append(
        #     panels.FoldingPanel(), api.Panel.Position.LEFT
        # )
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
            modes.CodeCompletionMode()
        )
        # no triggers in cobol
        self.code_completion_mode.trigger_symbols[:] = []
        self.auto_indent_mode = self.modes.append(
            modes.AutoIndentMode()
        )
        self.auto_indent_mode.min_indent = 7 * ' '
        self.word_click_mode = self.modes.append(
            modes.WordClickMode()
        )
        self.modes.append(cobmodes.CobolSyntaxHighlighter(self.document()))
        self.syntax_highlighter.color_scheme = api.ColorScheme('default')

        # self.show_whitespaces = True
