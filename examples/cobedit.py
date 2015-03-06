"""
A very simple cobol editor
"""
import logging
logging.basicConfig(level=logging.INFO)
import sys
from pyqode.qt import QtCore, QtWidgets
from pyqode.cobol.widgets import CobolCodeEdit
from pyqode.core.widgets import OutlineTreeWidget
from pyqode.cobol.widgets import PicOffsetsTable


class MainWindow(QtWidgets.QMainWindow):
    def __init__(self):
        super().__init__()
        self.editor = CobolCodeEdit()
        self.editor.file.open('hello.cbl')
        self.setCentralWidget(self.editor)
        self.setMinimumSize(950, 600)

        self.outline_tree = OutlineTreeWidget()
        dock_widget = QtWidgets.QDockWidget(self)
        dock_widget.setWidget(self.outline_tree)
        dock_widget.setWindowTitle('Document outline')
        self.addDockWidget(QtCore.Qt.RightDockWidgetArea, dock_widget)
        self.outline_tree.set_editor(self.editor)

        self.pic_offsets_table = PicOffsetsTable(self)
        dock_widget = QtWidgets.QDockWidget(self)
        dock_widget.setWidget(self.pic_offsets_table)
        dock_widget.setWindowTitle('PIC offsets table')
        self.addDockWidget(QtCore.Qt.RightDockWidgetArea, dock_widget)
        self.pic_offsets_table.set_editor(self.editor)


if __name__ == '__main__':
    app = QtWidgets.QApplication(sys.argv)
    window = MainWindow()
    window.show()
    app.exec_()
    del window
    del app
