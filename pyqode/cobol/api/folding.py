import re
from pyqode.core.api import FoldDetector, TextBlockHelper
from pyqode.core.qt import QtCore
import sys


class CobolFoldDetector(FoldDetector):
    # paragraph or struct pattern
    PROG = QtCore.QRegExp(
        r'((^|^\s{7})[\w\-]+\.\s*$)')

    def __init__(self):
        super().__init__()
        self.proc_division = None
        self._proc_div_txt = ""
        self.data_division = None
        self._data_div_txt = ""

    def stripped_texts(self, block, prev_block):
        ctext = block.text().rstrip().upper()
        if ctext.find(' USING ') != -1:
            ctext = ctext[:ctext.find(' USING ')] + '.'
        ptext = prev_block.text().rstrip().upper()
        if ptext.find(' USING ') != -1:
            ptext = ptext[:ptext.find(' USING ')] + '.'
        return ctext, ptext

    def detect_fold_level(self, prev_block, block):
        if not prev_block:
            return 0
        ctext, ptext = self.stripped_texts(block, prev_block)
        if ctext.endswith('DIVISION.'):
            if 'DATA' in ctext:
                self.data_division = block
                self._data_div_txt = block.text()
            if 'PROCEDURE' in ctext:
                self.proc_division = block
                self._proc_div_txt = block.text()
            # print(1, ctext)
            return 0
        elif ctext.endswith('SECTION.'):
            # print(1, ctext)
            return 1
        elif ptext.endswith('DIVISION.'):
            # print(1, ctext)
            return 1
        elif ptext.endswith('SECTION.'):
            # print(2, ctext)
            return 2
        # data division
        # procedure division
        if self.proc_division:
            print(self.proc_division.text())
        if (self.proc_division and
                self.proc_division.text() != self._proc_div_txt):
            self.proc_division = None
        if (self.data_division and
                self.data_division.text() != self._data_div_txt):
            self.data_division = None
        if (self.proc_division and self.proc_division.isValid() and
                block.blockNumber() > self.proc_division.blockNumber()):
            if self.PROG.indexIn(block.text()) != -1:
                # paragraph
                return 1
            else:
                # content of a paragraph
                return 2
        elif (self.data_division and self.data_division.isValid() and
                block.blockNumber() > self.data_division.blockNumber() + 1):
            # here folding is based on the indentation level
            offset = 6
            indent = ((len(ctext) - len(ctext.lstrip()) - offset) //
                      self.editor.tab_length)
            return 2 + indent
        plvl = TextBlockHelper.get_fold_lvl(prev_block)
        return plvl
