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

pyqode.core has a test suite and measure its coverage.

To run the tests, just run ``python setup.py test``

To measure coverage, run::

    python3 setup.py test -a "--cov pyqode"

To check for PEP8 warnings, install pytest-pep8 and run::

    python3 setup.py test -a "--pep8 -m pep8"


To run a single test, use ``-a "-- test_file_path.py::test_function"``, e.g.::

    python3 setup.py test -a "-- test/test_api/test_code_edit.py::test_set_plain_text"


Testing Matrix
++++++++++++++

We test the following combinations on Travis-CI:

+--------------------------+---------+---------+
|                          | PyQt4   | PyQt5   |
+==========================+=========+=========+
| GNU/Linux - Python 3.4   | yes     | yes     |
+--------------------------+---------+---------+


.. _pyQode: https://github.com/pyQode/pyQode
.. _Screenshots: https://github.com/pyQode/pyQode/wiki/Screenshots-and-videos#opencobolide-screenshots
.. _Issue tracker: https://github.com/pyQode/pyQode/issues
.. _Wiki: https://github.com/pyQode/pyQode/wiki
.. _API reference: http://pyqodecobol.readthedocs.org/en/latest/
.. _Changelog: https://github.com/pyQode/pyqode.cobol/blob/master/CHANGELOG.rst
.. _Contributing: https://github.com/pyQode/pyqode.cobol/blob/master/CONTRIBUTING.rst
