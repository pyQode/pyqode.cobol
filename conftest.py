# -*- coding: utf-8 -*-
"""
This scripts configures the test suite. We do two things:

    - setup the logging module
    - create ONE SINGLE INSTANCE of QApplication:
      this implies that you must use **QApplication.instance** in your
      test scripts.
"""
import logging
import os
import sys
import pytest
from pyqode.qt.QtWidgets import QApplication


try:
    import faulthandler
    faulthandler.enable()
except ImportError:
    pass


# -------------------
# Setup runtest
# -------------------
def pytest_runtest_setup(item):
    """
    Logs test name into pytest.log
    """
    if isinstance(item, item.Function):
        logging.info("------------------- %s -------------------",
                     item.name)


# -------------------
# Setup logging
# -------------------
logging.basicConfig(level=logging.INFO,
                    filename='pytest.log',
                    filemode='w')

# -------------------
# Setup QApplication
# -------------------
# 2. create qt application
print('isinstanciating QApplication')
_app = QApplication(sys.argv)
_widget = None


# -------------------
# Session fixtures
# -------------------
@pytest.fixture(scope="session")
def app(request):
    global _app
    return app


@pytest.fixture(scope="session")
def editor(request):
    import gettext
    gettext.NullTranslations().install()
    global _app, _widget
    from pyqode.core import modes
    from pyqode.cobol.widgets import CobolCodeEdit
    from pyqode.qt.QtTest import QTest

    logging.info('################ setup session editor ################')

    _widget = CobolCodeEdit()
    _widget.resize(800, 600)
    _widget.show()
    _app.setActiveWindow(_widget)
    while not _widget.backend.connected:
        QTest.qWait(100)

    _widget.modes.get(modes.FileWatcherMode).file_watcher_auto_reload = True
    _widget.save_on_focus_out = False

    def fin():
        global _widget
        logging.info('################ teardown session editor ###############'
                     '#')
        _widget.backend.stop()
        while _widget.backend.connected:
            QTest.qWait(100)
        del _widget

    request.addfinalizer(fin)

    return _widget
