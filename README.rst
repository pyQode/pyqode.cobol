.. image:: https://raw.githubusercontent.com/pyQode/pyQode/master/media/pyqode-banner.png

|

.. image:: https://img.shields.io/pypi/v/pyqode.cobol.svg
   :target: https://pypi.python.org/pypi/pyqode.cobol/
   :alt: Latest PyPI version

.. image:: https://img.shields.io/pypi/dm/pyqode.cobol.svg
   :target: https://pypi.python.org/pypi/pyqode.cobol/
   :alt: Number of PyPI downloads

.. image:: https://img.shields.io/pypi/l/pyqode.cobol.svg

.. image:: https://travis-ci.org/pyQode/pyqode.cobol.svg?branch=master
   :target: https://travis-ci.org/pyQode/pyqode.cobol
   :alt: Travis-CI build status


.. image:: https://coveralls.io/repos/pyQode/pyqode.cobol/badge.svg?branch=master
   :target: https://coveralls.io/r/pyQode/pyqode.cobol?branch=master
   :alt: Coverage Status


About
-----
*pyqode.cobol* adds **COBOL** support to `pyQode`_ (code completion,
code folding,...).


- `Issue tracker`_
- `Wiki`_
- `API reference`_
- `Contributing`_
- `Changelog`_
- `Screenshots`_


Features:
---------

* cobol code completion provider based on the cobol keywords and the defined
  names of the current document (pic fields, paragraphes)
* code folding mode
* auto indent mode (after struct, paragraph or inside procedures)
* native (fast) syntax highlighter with pygments themes support.
* document outline widget

License
-------

pyqode.cobol is licensed under the **GPL v3**.


Requirements
------------

pyqode.python depends on the following libraries:

- python 3 (>= 3.2)
- pyqode.core


Installation
------------

::

    $ pip3 install pyqode.cobol --upgrade

Testing
-------

To test pyqode.cobol, just run the `runtests.py` script with a python 3
interpreter.


.. _pyQode: https://github.com/pyQode/pyQode
.. _Screenshots: https://github.com/pyQode/pyQode/wiki/Screenshots-and-videos#opencobolide-screenshots
.. _Issue tracker: https://github.com/pyQode/pyQode/issues
.. _Wiki: https://github.com/pyQode/pyQode/wiki
.. _API reference: http://pyqodecobol.readthedocs.org/en/latest/
.. _Changelog: https://github.com/pyQode/pyqode.cobol/blob/master/CHANGELOG.rst
.. _Contributing: https://github.com/pyQode/pyqode.cobol/blob/master/CONTRIBUTING.rst
