#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
This setup script packages pyqode.cobol
"""
import sys
from setuptools import setup, find_packages
from pyqode.cobol import __version__
from setuptools.command.test import test as TestCommand

#
# add ``build_ui command`` (optional, for development only)
# this command requires the following packages:
#   - pyqt_distutils
#   - pyqode-uic
#
try:
    from pyqt_distutils.build_ui import build_ui
    cmdclass = {'build_ui': build_ui}
except ImportError:
    cmdclass = {}


class PyTest(TestCommand):
    user_options = [('pytest-args=', 'a', "Arguments to pass to py.test")]

    def initialize_options(self):
        TestCommand.initialize_options(self)
        self.pytest_args = []

    def run_tests(self):
        # import here, cause outside the eggs aren't loaded
        import pytest
        if self.pytest_args:
            self.pytest_args = self.pytest_args.replace('"', '').split(' ')
        else:
            self.pytest_args = []
        print('running test command: py.test "%s"' % ' '.join(
            self.pytest_args))
        errno = pytest.main(self.pytest_args)
        sys.exit(errno)

cmdclass['test'] = PyTest


DESCRIPTION = 'Adds COBOL support to pyqode.core'


def readme():
    if 'bdist_deb' in sys.argv or 'sdist_dsc' in sys.argv:
        return DESCRIPTION
    return str(open('README.rst').read())


# get requirements
requirements = ['pyqode.core']


setup(
    name='pyqode.cobol',
    namespace_packages=['pyqode'],
    version=__version__,
    packages=[p for p in find_packages() if 'test' not in p],
    keywords=["CodeEdit PySide PyQt code editor widget cobol"],
    package_dir={'pyqode': 'pyqode'},
    url='https://github.com/pyQode/pyqode.cobol',
    license='MIT',
    author='Colin Duquesnoy',
    author_email='colin.duquesnoy@gmail.com',
    description=DESCRIPTION,
    long_description=readme(),
    install_requires=requirements,
    tests_require=['pytest-cov', 'pytest-pep8', 'pytest'],
    zip_safe=False,
    cmdclass=cmdclass,
    classifiers=[
        'Development Status :: 5 - Production/Stable',
        'Environment :: X11 Applications :: Qt',
        'Environment :: Win32 (MS Windows)',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: MIT License',
        'Operating System :: Microsoft :: Windows',
        'Operating System :: POSIX :: Linux',
        'Programming Language :: Python :: 3.2',
        'Programming Language :: Python :: 3.3',
        'Programming Language :: Python :: 3.4',
        'Topic :: Software Development :: Libraries :: Application Frameworks',
        'Topic :: Software Development :: Widget Sets',
        'Topic :: Text Editors :: Integrated Development Environments (IDE)'
    ]
)
