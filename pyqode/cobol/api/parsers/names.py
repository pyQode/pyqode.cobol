"""
This parser parses the defined names in a cobol program and store them
under the appropriate division/section.

The code comes from OpenCobolIDE and has been left mostly intact.

"""
import logging
from pyqode.cobol.api.keywords import ALL_KEYWORDS


def _logger():
    return logging.getLogger(__name__)


class Name(object):
    """
    A Name is a node in the simplified abstract syntax tree.
    """

    class Type:
        """
        Enumerates the possible name types (div, section, paragraph,...)
        """
        Root = -1
        Division = 0
        Section = 1
        Variable = 2
        Paragraph = 3

    def __init__(self, node_type, line, column, name, description=None):
        self.node_type = node_type
        self.line = line
        self.column = column
        self.end_line = -1
        self.name = name
        if description is None:
            description = name
        self.description = description.replace(".", "")
        self.children = []

    def add_child(self, child):
        """
        Add a child to the node

        :param child: The child node to add
        """
        self.children.append(child)

    def __repr__(self):
        type_names = {
            self.Type.Root: "Root",
            self.Type.Division: "Division",
            self.Type.Paragraph: "Paragraph",
            self.Type.Section: "Section",
            self.Type.Variable: "Variable"
        }
        return "%s(name=%s, line=%s, end_line=%s)" % (
            type_names[self.node_type], self.name, self.line, self.end_line)


def cmp_name(first_node, second_node):
    """
    Compare two name recursively.

    :param first_node: First node

    :param second_node: Second state

    :return: 0 if same name, 1 if names are differents.
    """
    if len(first_node.children) == len(second_node.children):
        for first_child, second_child in zip(first_node.children,
                                             second_node.children):
            for key in first_child.__dict__.keys():
                if key.startswith('_'):
                    continue
                if first_child.__dict__[key] != second_child.__dict__[key]:
                    return 1
            ret_val = cmp_name(first_child, second_child)
            if ret_val != 0:
                return 1
    else:
        return 1
    return 0


def parse_division(l, c, line, root_node, last_section_node):
    """
    Extracts a division node from a line

    :param l: The line number (starting from 0)

    :param c: The column number

    :param line: The line string (without indentation)

    :param root_node: The document root node.

    :return: tuple(last_div_node, last_section_node)
    """
    name = line
    name = name.replace(".", "")
    node = Name(Name.Type.Division, l + 1, c, name)
    root_node.add_child(node)
    last_div_node = node
    # do not take previous sections into account
    if last_section_node:
        last_section_node.end_line = l
    last_section_node = None
    return last_div_node, last_section_node


def parse_section(l, c, last_div_node, last_vars, line):
    """
    Extracts a section node from a line.

    :param l: The line number (starting from 0)

    :param last_div_node: The last div node found

    :param last_vars: The last vars dict

    :param line: The line string (without indentation)

    :return: last_section_node
    """
    name = line
    name = name.replace(".", "")
    node = Name(Name.Type.Section, l + 1, c, name)
    last_div_node.add_child(node)
    last_section_node = node
    # do not take previous var into account
    last_vars.clear()
    return last_section_node


def parse_pic_field(l, c, last_section_node, last_vars, line):
    """
    Parse a pic field line. Return A VariableNode or None in case of malformed code.

    :param l: The line number (starting from 0)
    :param c: The column number (starting from 0)
    :param last_section_node: The last section node found
    :param last_vars: The last vars dict
    :param line: The line string (without indentation)
    :return: The extracted variable node
    """
    parent_node = None
    raw_tokens = line.split(" ")
    tokens = []
    for t in raw_tokens:
        if not t.isspace() and t != "":
            tokens.append(t)
    try:
        if tokens[0] == "FD":
            lvl = 1
        else:
            lvl = int(tokens[0], 16)
        name = tokens[1]
    except ValueError:
        lvl = 1
        name = tokens[0]
    except IndexError:
        # line not complete
        return None
    name = name.replace(".", "")
    description = line
    if lvl == 1:
        parent_node = last_section_node
        last_vars.clear()
    else:
        # find parent level
        levels = sorted(last_vars.keys(), reverse=True)
        for lv in levels:
            if lv < lvl:
                parent_node = last_vars[lv]
                break
    if not parent_node:
        # malformed code
        return None
    node = Name(Name.Type.Variable, l + 1, c, name, description)
    parent_node.add_child(node)
    last_vars[lvl] = node
    return node


def parse_paragraph(l, c, last_div_node, last_section_node, line):
    """
    Extracts a paragraph node

    :param l: The line number (starting from 0)
    :param last_div_node: The last div node found
    :param last_section_node: The last section node found
    :param line: The line string (without indentation)
    :return: The extracted paragraph node
    """
    if not line.endswith('.'):
        return None
    name = line.replace(".", "")
    if name.strip() == '':
        return None
    parent_node = last_div_node
    if last_section_node is not None:
        parent_node = last_section_node
    node = Name(Name.Type.Paragraph, l + 1, c, name)
    parent_node.add_child(node)
    return node


def defined_names(code, free_format=False):
    """
    Parses a cobol document and build a name tree.

    For convenience, it also returns the list of variables (PIC) and
    procedures (paragraphs).

    :param code: cobol code to parse. Default is None.
    :param free_format: True if the source code must be considered as coded
        in free format.

    :return: A tuple made up of the name tree root node, the list of variables
        and  the list of paragraphs.
    :rtype: Name, list of Name, list of Name
    """
    root_node = Name(Name.Type.Root, 0, 0, 'root')
    variables = []
    paragraphs = []
    lines = code.splitlines()
    last_div_node = None
    last_section_node = None
    last_vars = {}
    last_par = None

    for i, line in enumerate(lines):
        if not free_format:
            if len(line) >= 6:
                line = 6 * " " + line[6:]
        column = len(line) - len(line.lstrip())
        if not line.isspace() and not line.strip().startswith("*"):
            line = line.strip()
            # DIVISIONS
            if "DIVISION" in line.upper():
                # remember
                if last_div_node is not None:
                    last_div_node.end_line = i
                last_div_node, last_section_node = parse_division(
                    i, column, line, root_node, last_section_node)
            # SECTIONS
            elif "SECTION" in line:
                if last_section_node:
                    last_section_node.end_line = i
                last_section_node = parse_section(
                    i, column, last_div_node, last_vars, line)
            # VARIABLES
            elif (last_div_node is not None and
                    "DATA DIVISION" in last_div_node.name):
                v = parse_pic_field(
                    i, column, last_section_node, last_vars, line)
                if v:
                    variables.append(v)
            # PARAGRAPHS
            elif (last_div_node is not None and
                          "PROCEDURE DIVISION" in last_div_node.name):
                tokens = line.split(" ")
                if len(tokens) == 1 and not tokens[0] in ALL_KEYWORDS:
                    p = parse_paragraph(
                        i, column, last_div_node, last_section_node, line)
                    if p:
                        paragraphs.append(p)
                        if last_par:
                            last_par.end_line = i
                        last_par = p

    # close last div
    if last_par:
        last_par.end_line = len(lines) - 1
    if last_div_node:
        last_div_node.end_line = len(lines)
    root_node.end_line = last_div_node.end_line
    return root_node, variables, paragraphs
