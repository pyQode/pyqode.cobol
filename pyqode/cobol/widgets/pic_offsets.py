from pyqode.qt import QtCore, QtWidgets


class PicOffsetsTable(QtWidgets.QTableWidget):
    show_requested = QtCore.Signal()

    def __init__(self, parent=None):
        super().__init__(parent)
        self._update([])
        self._editor = None
        self.verticalHeader().setVisible(False)
        self.setColumnCount(4)
        self.setHorizontalHeaderLabels(
            ['Level', 'Name', 'Offset', 'PIC'])

    def set_editor(self, editor):
        if self._editor is not None:
            try:
                self._editor.offset_calculator.pic_infos_available.disconnect(
                    self._update)
            except (AttributeError, RuntimeError):
                # see https://github.com/OpenCobolIDE/OpenCobolIDE/issues/89
                pass
        self._editor = editor
        try:
            self._editor.offset_calculator.pic_infos_available.connect(
                self._update)
        except AttributeError:
            pass

    def _update(self, infos):
        self.clearContents()
        self.setRowCount(len(infos))

        # process each info in a separate row
        for i, info in enumerate(infos):
            self.setItem(
                i, 0, QtWidgets.QTableWidgetItem("%s" % info.level))
            self.setItem(
                i, 1, QtWidgets.QTableWidgetItem(info.name))
            self.setItem(
                i, 2, QtWidgets.QTableWidgetItem("%s" % info.offset))
            self.setItem(
                i, 3, QtWidgets.QTableWidgetItem(info.pic))
        self.setSortingEnabled(False)

        self.show_requested.emit()
