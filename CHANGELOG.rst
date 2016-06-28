Change log
==========

2.10.0
-------

Fixed bugs:

- correct recognition of SECTION/DIVISION in oder to prevent EXIT SECTION to be shown in the document outline

2.9.0
-----

New features:

- add a public method to refresh pic field info
- improve name parser: assume missing headers
- many improvements to the cobol code folding (tested on cobolmac and cobjapi).

Fixed bugs:

- wait for fixed/free format to be set on the backend before performing outline analysis
- fix comments mode in fixed format not inserting the comment symbol at column 7 but two columns before the first
  non whitepsace character.


2.8.0
-----

New features:

- add a multi-margins mode: user can configure up to 4 margins

Fixed bugs:

- fix wrong offset for level 77 items

2.7.0
-----

New features:
    - add the new cursor history mode to the cobol editor
    - add the new read only panel to the cobol editor
    - add color_scheme argument to code editor constructor

Fixed bugs:
    - fix many potential memory leaks by breaking the circular dependencies
      correctly before removing a mode/panel

2.6.4
-----

Improvements:
    - OffsetCalculator: allow user to resize columns
    - OffsetCalculator: handle REDEFINES and COMP fields

Fixed bugs:
    - OffsetCalculator: fix wrong offset calculation for lvl 78/88
    - OffsetCalculator: fix some gui bugs

2.6.3
-----

Fixed bugs:
    - Fix pic field regex missing ``OCCURS`` and ``INDEXED BY``

2.6.2
-----

Fixed bugs:
    - Fix conflicting goto shortuct: use F7 instead of F3 which is already used by the search & replace panel.

2.6.1
-----

Improvements:
    - Show picture information in outline widget
    - Improve linux icon theme integration (icon like code-variable,
      code-function will be fetched from icon theme if the theme has theme,
      e.g. on plasma 5 with breeze icon theme)

Fixed bugs:
    - Fix division/section not correctly recognized in case where the user
      added some spaces before the keyword and the period
    - Fix auto-indent of file where column 1-6 are not empty

2.6.0
------

New features:
    - Add support for stdeb (ppa packages will be available soon)
    - Add support for bdist_wheel
    - Rework editor context menu


2.5.0
-----

New features:
    - Unified API for document outline (see pyQode/pyQode#24)

Fixed bugs:
    - fix comment/uncomment in fixed mode when the column 1-6 are not empty (see OpenCobolIDE/OpenCobolIDE#108)

2.4.1
-----

New features:

- allow user to choose keyword completion convention: lower case or UPPER CASE
- add support for editing .scb files (needed by the new minor version of OpenCobolIDE)

Bug fixes:

- fix linter confusion when switching from one tab to another very quicly (OpenCobolIDE/OpenCobolIDE#92)
- fix indentation bug: keep formatting for character beyond col 7 in non-free COBOL
- fixed a couple of bugs with the name parser: don't show END-EXEC, EXEC SQL, LVL 88 variables,...

2.4.0
-----

*starting from version 2.4, all pyqode packages share the same master version (== 2 first numbers)*

New features:

- new indenter made specifically for COBOL
- improve syntax highlighter: the lexer is now able to make the distinctions
  between regular keywords and types (PIC clauses will have a different color
  when using some specific pygments styles, not all styles define different
  colors for the different types of keywords).

Fixed bugs:

- fix OpenCobolIDE/OpenCobolIDE#76: end-if/end-perform appear in the outline
  view


1.1.0
-----

New features:
    - Add smart backspace mode
    - Add global checker panel

Fixed bugs:
    - Fix shortcut conflict between goto and extended selection


1.0.0
-----

Initial release. Most of the code come from OpenCobolIDE v3.0. The creation
of this package is an effort to share the widget and to make the IDE code
focused ont he application, not the editor widget.
