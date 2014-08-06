"""
This module contains the various regular expressions used through the whole
project.

"""
from pyqode.core.qt import QtCore


#: This pattern identifies a structure or paragraph definition
PAR_OR_STRUCT_PATTERN = QtCore.QRegExp(
        r'((^|^\s{7})[\w\-]+\.\s*$)')
#: This pattern identifies a loop pattern
LOOP_PATTERN = QtCore.QRegExp(
        r'PERFORM.+(UNTIL|TIMES){1}'
)
