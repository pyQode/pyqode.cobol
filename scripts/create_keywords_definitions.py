import re

INTRINSICS_FILE = "intrinsics.lst.txt"
INTRINSICS = "INTRINSICS"
RESERVED_FILE = "reserved.lst.txt"
RESERVED = "RESERVED"
MNEMONICS_FILE = "mnemonic.lst.txt"
MNEMONICS = "MNEMONICS"
KEYWORDS_PY_FILE = "../pyqode/cobol/api/keywords.py"
KEYWORDS_PY_TEMPLATE = '''"""
This module contains cobol keywords, functions and builtins names for use in
auto completion and syntax highlighting
"""

INTRINSICS = %r

RESERVED = %r

MNEMONICS = %r

SQL_COBOL_KEYWORDS = ['EXEC', 'SQL', 'END-EXEC', 'INSERT', 'INCLUDE']

ALL_KEYWORDS = INTRINSICS + RESERVED + MNEMONICS
'''


def read_keywords(path):
    keywords = []
    with open(path) as f:
        contents = f.read()
    for line in contents.splitlines():
        if line.startswith('#') or not line.strip():
            continue
        keywords.append(re.split(r"\s\s+", line)[0])
    return keywords


intrinsics_list = read_keywords(INTRINSICS_FILE)
for kw in intrinsics_list:
    print(kw)
print("--------------------------------------------------------")
reserved_list = read_keywords(RESERVED_FILE)
for kw in reserved_list:
    print(kw)
print("--------------------------------------------------------")
mnemonics_list = read_keywords(MNEMONICS_FILE)
for kw in mnemonics_list:
    print(kw)
print("--------------------------------------------------------")


keywords_py_contents = KEYWORDS_PY_TEMPLATE % (intrinsics_list, reserved_list, mnemonics_list)
with open(KEYWORDS_PY_FILE, 'w') as f:
    f.write(keywords_py_contents)
