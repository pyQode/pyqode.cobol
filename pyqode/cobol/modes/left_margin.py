"""
This module contains the left margin used to highlight the 7th column, for
fixed format cobol editors.

"""
from pyqode.core.modes import RightMarginMode


class LeftMarginMode(RightMarginMode):
    """
    Show a left margin at column 7
    """
    IDENTIFIER = "leftMarginMode"

    def __init__(self):
        super().__init__()

    def on_install(self, editor):
        super().on_install(editor)
        self.position = 7
