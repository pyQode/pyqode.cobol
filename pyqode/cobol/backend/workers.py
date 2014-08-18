from pyqode.cobol.api import icons
from pyqode.cobol.api import keywords
from pyqode.cobol.api.parsers.names import defined_names

free_format = False


def set_free_format(value):
    global free_format
    free_format = value


class CobolAnalyserProvider:
    def __init__(self):
        self.__keywordsCompletions = []
        for keyword in (keywords.PSEUDO + keywords.RESERVED +
                keywords.CONSTANTS):
            self.__keywordsCompletions.append(
                {'name': keyword, 'icon': icons.ICON_KEYWORD})
        for keyword in keywords.NAME_CONSTANTS + keywords.FUNCTIONS:
            self.__keywordsCompletions.append(
                {'name': keyword, 'icon': icons.ICON_FUNC})

    def complete(self, code, line, column, completionPrefix,
                 file_path, file_encoding):
        global free_format
        completions = []
        try:
            root, vars, functions = defined_names(
                code, free_format=free_format)
        except AttributeError:
            vars = []
            functions = []
        for var in vars:
            completions.append({
                'name': var.name,
                'icon': icons.ICON_VAR,
                'tooltip': var.description
            })
        for func in functions:
            completions.append({
                'name': func.name,
                'icon': icons.ICON_FUNC
            })
        completions += self.__keywordsCompletions
        return completions
