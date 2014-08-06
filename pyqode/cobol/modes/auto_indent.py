"""
This module contains a cobol specific auto indenter mode.
"""
from pyqode.core.modes import AutoIndentMode
from pyqode.cobol.api import regex, names


class CobolAutoIndentMode(AutoIndentMode):
    def _get_indent(self, cursor):
        pre_indent, post_indent = super()._get_indent(cursor)
        text = cursor.block().text()
        if regex.PAR_OR_STRUCT_PATTERN.indexIn(text) != -1:
            post_indent += self.editor.tab_length * ' '
        print(text)
        return pre_indent, post_indent
