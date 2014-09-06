from .parsers.pic import process_cobol


class PicFieldInfo(object):
    """
    This structure holds the information about a PIC field.
    """
    offset = 0
    name = ""
    level = 0
    pic = ""
    occurs = None
    redefines = None
    indexed_by = None


def _clean_code(code):
    """
    Cleans the received code (the parser does not like extra spaces not a VALUE
    statement). Returns the cleaned code as a list of lines.

    :param code: The COBOL code to clean

    :return The list of code lines (cleaned)
    """
    lines = []
    # cleanup lines, the parser is very sensitive to extra spaces,...
    for l in code.splitlines():
        # remove last .
        l = l[:-1]
        # the parser doe not like VALUE xxx.
        if "VALUE" in l:
            l = l[:l.find("VALUE")]
        # the parser does not like extra spaces between "PIC X(xxx)" and "."
        indent = len(l) - len(l.lstrip())
        tokens = l.split(" ")
        while "" in tokens:
            tokens.remove("")
        if not tokens[-1].endswith("."):
            tokens[-1] += "."
        lines.append(" " * indent + " ".join(tokens))

    return lines


def get_field_infos(code):
    """
    Gets the list of pic fields information from line |start| to line |end|.

    :param code: code to parse

    :returns: the list of pic fields info found in the specified text.
    """
    offset = 0
    field_infos = []
    lines = _clean_code(code)

    for row in process_cobol(lines):
        fi = PicFieldInfo()
        fi.offset = offset
        fi.name = row["name"]
        fi.level = row["level"]
        fi.pic = row["pic"]
        fi.occurs = row["occurs"]
        fi.redefines = row["redefines"]
        fi.indexed_by = row["indexed_by"]
        field_infos.append(fi)

        # compute offset of next PIC field.
        if row['pic']:
            offset += row['pic_info']['length']
        else:
            offset += 1

    return field_infos
