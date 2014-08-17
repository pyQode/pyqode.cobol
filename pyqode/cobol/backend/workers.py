from pyqode.cobol.api.keywords import PSEUDO_KEYWORDS
from pyqode.cobol.api.keywords import RESERVED_KEYWORDS
from pyqode.cobol.api.keywords import NAME_CONSTANTS
from pyqode.cobol.api.keywords import FUNCTIONS
from pyqode.cobol.api.parsers.names import defined_names


ICON_FUNC = ":/icons/share/icons/pyqode.cobol/func.png"
ICON_VAR = ":/icons/share/icons/pyqode.cobol/var.png"
ICON_KEYWORD = ":/icons/share/icons/pyqode.cobol/keyword.png"


class CobolAnalyserProvider:
    def __init__(self):
        self.__keywordsCompletions = []
        for keyword in PSEUDO_KEYWORDS + RESERVED_KEYWORDS:
            self.__keywordsCompletions.append(
                {'name': keyword, 'icon': ICON_KEYWORD})
        for keyword in NAME_CONSTANTS + FUNCTIONS:
            self.__keywordsCompletions.append(
                {'name': keyword, 'icon': ICON_FUNC})

    def complete(self, code, line, column, completionPrefix,
                 file_path, file_encoding):
        completions = []
        try:
            root, vars, functions = defined_names(code)
        except AttributeError:
            vars = []
            functions = []
        for var in vars:
            completions.append({
                'name': var.name,
                'icon': ICON_VAR,
                'tooltip': var.description
            })
        for func in functions:
            completions.append({
                'name': func.name,
                'icon': ICON_FUNC
            })
        completions += self.__keywordsCompletions
        return completions
