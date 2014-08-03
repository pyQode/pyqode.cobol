"""
This module contains a native python syntax highlighter.
"""
import logging
import re
from pyqode.core.qt import QtGui
from pyqode.core.api import SyntaxHighlighter as BaseSH, TextHelper
from pyqode.core.api import TextBlockHelper


def any(name, alternates):
    """Return a named group pattern matching list of alternates."""
    return "(?P<%s>" % name + "|".join(alternates) + ")"


RESERVED_KEYWORDS = [
    'ACCEPT', 'ADD', 'ALLOCATE', 'CALL', 'CANCEL', 'CLOSE', 'COMPUTE',
    'CONFIGURATION', 'CONTINUE', 'DATA', 'DELETE', 'DISPLAY', 'DIVIDE',
    'DIVISION', 'ELSE', 'END', 'END-ACCEPT', 'END-ADD', 'END-CALL',
    'END-COMPUTE', 'END-DELETE', 'END-DISPLAY', 'END-DIVIDE', 'END-EVALUATE',
    'END-IF', 'END-MULTIPLY', 'END-OF-PAGE', 'END-PERFORM', 'END-READ',
    'END-RETURN', 'END-REWRITE', 'END-SEARCH', 'END-START', 'END-STRING',
    'END-SUBTRACT', 'END-UNSTRING', 'END-WRITE', 'ENVIRONMENT', 'EVALUATE',
    'EXIT', 'FD', 'FILE', 'FILE-CONTROL', 'FOREVER', 'FREE', 'GENERATE', 'GO',
    'GOBACK', 'IDENTIFICATION', 'IF', 'INITIALIZE', 'INITIATE',
    'INPUT-OUTPUT', 'INSPECT', 'INVOKE', 'I-O-CONTROL', 'LINKAGE',
    'LOCAL-STORAGE', 'MERGE', 'MOVE', 'MULTIPLY', 'OPEN', 'PERFORM',
    'PROCEDURE', 'PROGRAM-ID', 'RAISE', 'READ', 'RELEASE', 'RESUME', 'RETURN',
    'REWRITE', 'SCREEN', 'SD', 'SEARCH', 'SECTION', 'SET', 'SORT', 'START',
    'STOP', 'STRING', 'SUBTRACT', 'SUPPRESS', 'TERMINATE', 'THEN', 'UNLOCK',
    'UNSTRING', 'USE', 'VALIDATE', 'WORKING-STORAGE', 'WRITE'
]

PSEUDO_KEYWORDS = [
    'ACCESS', 'ADDRESS', 'ADVANCING', 'AFTER', 'ALL', 'ALPHABET', 'ALPHABETIC',
    'ALPHABETIC-LOWER', 'ALPHABETIC-UPPER', 'ALPHANUMERIC',
    'ALPHANUMERIC-EDITED', 'ALSO', 'ALTER', 'ALTERNATE', 'ANY', 'ARE', 'AREA',
    'AREAS', 'ARGUMENT-NUMBER', 'ARGUMENT-VALUE', 'AS', 'ASCENDING', 'ASSIGN',
    'AT', 'AUTO', 'AUTO-SKIP', 'AUTOMATIC', 'AUTOTERMINATE',
    'BACKGROUND-COLOR', 'BASED', 'BEEP', 'BEFORE', 'BELL', 'BLANK', 'BLINK',
    'BLOCK', 'BOTTOM', 'BY', 'BYTE-LENGTH', 'CHAINING', 'CHARACTER',
    'CHARACTERS', 'CLASS', 'CODE', 'CODE-SET', 'COL', 'COLLATING', 'COLS',
    'COLUMN', 'COLUMNS', 'COMMA', 'COMMAND-LINE', 'COMMIT', 'COMMON',
    'CONSTANT', 'CONTAINS', 'CONTENT', 'CONTROL', 'CONTROLS', 'CONVERTING',
    'COPY', 'CORR', 'CORRESPONDING', 'COUNT', 'CRT', 'CURRENCY', 'CURSOR',
    'CYCLE', 'DATE', 'DAY', 'DAY-OF-WEEK', 'DE', 'DEBUGGING', 'DECIMAL-POINT',
    'DECLARATIVES', 'DEFAULT', 'DELIMITED', 'DELIMITER', 'DEPENDING',
    'DESCENDING', 'DETAIL', 'DISK', 'DOWN', 'DUPLICATES', 'DYNAMIC',
    'EBCDIC', 'ENTRY', 'ENVIRONMENT-NAME', 'ENVIRONMENT-VALUE', 'EOL', 'EOP',
    'EOS', 'ERASE', 'ERROR', 'ESCAPE', 'EXCEPTION', 'EXCLUSIVE', 'EXTEND',
    'EXTERNAL', 'FILE-ID', 'FILLER', 'FINAL', 'FIRST', 'FIXED', 'FLOAT-LONG',
    'FLOAT-SHORT', 'FOOTING', 'FOR', 'FOREGROUND-COLOR', 'FORMAT', 'FROM',
    'FULL', 'FUNCTION', 'FUNCTION-ID', 'GIVING', 'GLOBAL', 'GROUP', 'HEADING',
    'HIGHLIGHT', 'I-O', 'ID', 'IGNORE', 'IGNORING', 'IN', 'INDEX', 'INDEXED',
    'INDICATE', 'INITIAL', 'INITIALIZED', 'INPUT', 'INTO', 'INTRINSIC',
    'INVALID', 'IS', 'JUST', 'JUSTIFIED', 'KEY', 'LABEL', 'LAST', 'LEADING',
    'LEFT', 'LENGTH', 'LIMIT', 'LIMITS', 'LINAGE', 'LINAGE-COUNTER', 'LINE',
    'LINES', 'LOCALE', 'LOCK', 'LOWLIGHT', 'MANUAL', 'MEMORY', 'MINUS',
    'MODE', 'MULTIPLE', 'NATIONAL', 'NATIONAL-EDITED', 'NATIVE', 'NEGATIVE',
    'NEXT', 'NO', 'NULL', 'NULLS', 'NUMBER', 'NUMBERS', 'NUMERIC',
    'NUMERIC-EDITED', 'OBJECT-COMPUTER', 'OCCURS', 'OF', 'OFF', 'OMITTED',
    'ON', 'ONLY', 'OPTIONAL', 'ORDER', 'ORGANIZATION', 'OTHER', 'OUTPUT',
    'OVERFLOW', 'OVERLINE', 'PACKED-DECIMAL', 'PADDING', 'PAGE', 'PARAGRAPH',
    'PLUS', 'POINTER', 'POSITION', 'POSITIVE', 'PRESENT', 'PREVIOUS', 
    'PRINTER', 'PRINTING', 'PROCEDURE-POINTER', 'PROCEDURES', 'PROCEED',
    'PROGRAM', 'PROGRAM-POINTER', 'PROMPT', 'QUOTE', 'QUOTES', 'RANDOM', 'RD',
    'RECORD', 'RECORDING', 'RECORDS', 'RECURSIVE', 'REDEFINES', 'REEL',
    'REFERENCE', 'RELATIVE', 'REMAINDER', 'REMOVAL', 'RENAMES', 'REPLACING',
    'REPORT', 'REPORTING', 'REPORTS', 'REPOSITORY', 'REQUIRED', 'RESERVE',
    'RETURNING', 'REVERSE-VIDEO', 'REWIND', 'RIGHT', 'ROLLBACK', 'ROUNDED',
    'RUN', 'SAME', 'SCROLL', 'SECURE', 'SEGMENT-LIMIT', 'SELECT', 'SENTENCE',
    'SEPARATE', 'SEQUENCE', 'SEQUENTIAL', 'SHARING', 'SIGN', 'SIGNED',
    'SIGNED-INT', 'SIGNED-LONG', 'SIGNED-SHORT', 'SIZE', 'SORT-MERGE',
    'SOURCE', 'SOURCE-COMPUTER', 'SPECIAL-NAMES', 'STANDARD', 'STANDARD-1',
    'STANDARD-2', 'STATUS', 'SUM', 'SYMBOLIC', 'SYNC', 'SYNCHRONIZED',
    'TALLYING', 'TAPE', 'TEST', 'THROUGH', 'THRU', 'TIME', 'TIMES', 'TO',
    'TOP', 'TRAILING', 'TRANSFORM', 'TYPE', 'UNDERLINE', 'UNIT', 'UNSIGNED',
    'UNSIGNED-INT', 'UNSIGNED-LONG', 'UNSIGNED-SHORT', 'UNTIL', 'UP', 'UPDATE',
    'UPON', 'USAGE', 'USING', 'VALUE', 'VALUES', 'VARYING', 'WAIT', 'WHEN',
    'WITH', 'WORDS', 'YYYYDDD', 'YYYYMMDD'
]

NAME_CONSTANTS = [
    'ZEROES',
    'HIGH-VALUE', 'LOW-VALUE', 'QUOTE', 'SPACE', 'ZERO',
    'HIGH-VALUES', 'LOW-VALUES', 'QUOTES', 'SPACES', 'ZEROS'
]

FUNCTIONS = [
    'ABS', 'ACOS', 'ANNUITY', 'ASIN', 'ATAN', 'BYTE-LENGTH', 'CHAR', 
    'COMBINED-DATETIME', 'CONCATENATE', 'COS', 'CURRENT-DATE',
    'DATE-OF-INTEGER', 'DATE-TO-YYYYMMDD', 'DAY-OF-INTEGER', 'DAY-TO-YYYYDDD',
    'EXCEPTION-(?:FILE', 'LOCATION', 'STATEMENT', 'STATUS)', 'EXP10', 'EXP',
    'E', 'FACTORIAL', 'FRACTION-PART', 'INTEGER-OF-(?:DATE', 'DAY', 'PART)',
    'INTEGER', 'LENGTH', 'LOCALE-(?:DATE', 'TIME(?:-FROM-SECONDS)?)', 'LOG10',
    'LOG', 'LOWER-CASE', 'MAX', 'MEAN', 'MEDIAN', 'MIDRANGE', 'MIN', 'MOD',
    'NUMVAL(?:-C)?', 'ORD(?:-MAX', '-MIN)?', 'PI', 'PRESENT-VALUE', 'RANDOM',
    'RANGE', 'REM', 'REVERSE', 'SECONDS-FROM-FORMATTED-TIME',
    'SECONDS-PAST-MIDNIGHT', 'SIGN', 'SIN', 'SQRT', 'STANDARD-DEVIATION',
    'STORED-CHAR-LENGTH', 'SUBSTITUTE(?:-CASE)?', 'SUM', 'TAN',
    'TEST-DATE-YYYYMMDD', 'TEST-DAY-YYYYDDD', 'TRIM', 'UPPER-CASE', 'VARIANCE',
    'WHEN-COMPILED', 'YEAR-TO-YYYY'
]


def make_cobol_patterns(fixed_format=True):
    if fixed_format:
        comment = any('comment', [r"(^.{6})\*>[^\n]*|(^.{6})\*[^\n]*"])
    else:
        comment = any('comment', [r"^[ \t]*\*>[^\n]*|\s*\*[^\n]*"])
    keywords_reserved = any(
        'keyword_reserved',
        ['(^|(?<=[^0-9a-zA-Z_\-]))(%s)\s*($|(?=[^0-9a-zA-Z_\-]))' %
         '|'.join(RESERVED_KEYWORDS)])
    keywords = any(
        'keyword',
        [r'(^|(?<=[^0-9a-zA-Z_\-]))(%s)\s*($|(?=[^0-9a-zA-Z_\-]))' %
        '|'.join(PSEUDO_KEYWORDS)])
    constants = any(
        'constant',
        [r'(^|(?<=[^0-9a-zA-Z_\-]))((%s)|(%s))\s*($|(?=[^0-9a-zA-Z_\-]))' %
         (NAME_CONSTANTS[0], '|'.join(NAME_CONSTANTS[1:])),
         '(^|(?<=[^0-9a-zA-Z_\-]))'
         '(PIC\s+.+?(?=(\s|\.\s?))|PICTURE\s+.+?(?=(\s|\.\s?))|'
         '(COMPUTATIONAL)(-[1-5X])?|(COMP)(-[1-5X])?|'
         'BINARY-C-LONG|'
         'BINARY-CHAR|BINARY-DOUBLE|BINARY-LONG|BINARY-SHORT|'
         'BINARY)\s*($|(?=[^0-9a-zA-Z_\-]))',
         '(^|(?<=[^0-9a-zA-Z_\-]))(equal|equals|ne|lt|le|gt|ge|'
         'greater|less|than|not|and|or)\s*($|(?=[^0-9a-zA-Z_\-]))'])
    # operator = any('operator', ['(\*\*|\*|\+|-|/|<=|>=|<|>|==|/=|=)'])
    punctuation = any('punctuation', ['([(),;:&%.])'])
    name_builtin = any(
        'builtin', [
            '(^|(?<=[^0-9a-zA-Z_\-]))(true|false)\s*($|(?=[^0-9a-zA-Z_\-]))'])
    string = any('string', ['"[^"\n]*("|\n)', r"'[^'\n]*('|\n)"])
    numbers = any('number', [
        '\d+(\s*|\.$|$)',
        '[+-]?\d*\.\d+([eE][-+]?\d+)?',
        '[+-]?\d+\.\d*([eE][-+]?\d+)?'
    ])
    variable = any(
        'instance',
        [r'[a-zA-Z0-9]([_a-zA-Z0-9\-]*[a-zA-Z0-9]+)?'])

    return "|".join(
        [
            keywords_reserved,
            keywords,
            constants,
            # definitions,
            comment,
            punctuation,
            name_builtin,
            string,
            numbers,
            variable,
            any("SYNC", [r"\n"])
        ]
    )


def _logger():
    return logging.getLogger(__name__)


class CobolSyntaxHighlighter(BaseSH):
    """
    Native cobol highlighter (fixed format).
    """
    PROG = re.compile(make_cobol_patterns(), re.S)

    def highlight_cobol(self, text):
        text = text.upper()
        self.setFormat(0, len(text), self.formats["normal"])
        match = self.PROG.search(text)
        while match:
            for key, value in list(match.groupdict().items()):
                if value:
                    start, end = match.span(key)
                    try:
                        fmt = self.formats[key]
                    except KeyError:
                        _logger().debug('unsupported format: %s' % key)
                    else:
                        self.setFormat(start, end - start, fmt)
            match = self.PROG.search(text, match.end())

    def highlight_disabled_columns(self, text):
        fmt = QtGui.QTextCharFormat()
        fmt.setForeground(QtGui.QBrush(self.editor.whitespaces_foreground))
        try:
            self.setFormat(0, 6 if text[6] in ['*', '-'] else 7, fmt)
        except IndexError:
            self.setFormat(0, len(text), fmt)

    def highlight_block(self, text, block):
        self.highlight_cobol(text)
        self.highlight_disabled_columns(text)


class FreeCobolSyntaxHighlighter(CobolSyntaxHighlighter):
    """
    Native cobol highlighter (fixed format).
    """
    PROG = re.compile(make_cobol_patterns(fixed_format=False), re.S)

    def highlight_block(self, text, block):
        self.highlight_cobol(text)
