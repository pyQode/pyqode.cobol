Change log
==========

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
