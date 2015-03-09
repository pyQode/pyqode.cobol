Change log
==========

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
