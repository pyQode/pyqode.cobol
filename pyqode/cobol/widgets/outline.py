"""
This document contains the tree widget used to display the editor document
outline.

"""
from pyqode.qt import QtCore, QtGui, QtWidgets
from pyqode.cobol.api import icons, Name
from pyqode.core.api import TextBlockHelper, TextHelper, TextBlockUserData

ICONS = {
    Name.Type.Root: icons.ICON_MIMETYPE,
    Name.Type.Division: icons.ICON_DIVISION,
    Name.Type.Section: icons.ICON_SECTION,
    Name.Type.Variable: icons.ICON_VAR,
    Name.Type.Paragraph: icons.ICON_FUNC
}


class OutlineTreeWidget(QtWidgets.QTreeWidget):
    """
    Displays the outline of a CobolCodeEdit.

    To use the widget, you just have to set the active editor using
    :func:`OutlineTreeWidget.set_editor`.

    """
    def __init__(self, parent=None):
        super().__init__(parent)
        self._editor = None
        self.setHeaderHidden(True)
        self.currentItemChanged.connect(self._on_click)
        self.itemCollapsed.connect(self._on_item_state_changed)
        self.itemExpanded.connect(self._on_item_state_changed)
        self._updating = True

    def _on_click(self, item):
        if item:
            name = item.data(0, QtCore.Qt.UserRole)
            if name:
                TextHelper(self._editor).goto_line(name.block.blockNumber(),
                                                   column=name.column)

    def _on_item_state_changed(self, item):
        if self._updating:
            return
        if item == self._root_item:
            item_state = not item.isExpanded()
            if item_state:
                QtCore.QTimer.singleShot(
                    1, self._editor.folding_panel.collapse_all)
            else:
                QtCore.QTimer.singleShot(
                    1, self._editor.folding_panel.expand_all)
        else:
            block = item.data(0, QtCore.Qt.UserRole).block
            assert isinstance(item, QtWidgets.QTreeWidgetItem)
            item_state = not item.isExpanded()
            block_state = TextBlockHelper.get_fold_trigger_state(block)
            if item_state != block_state:
                self._editor.folding_panel.toggle_fold_trigger(block)

    def _on_block_state_changed(self, block, state):
        data = block.userData()
        if data is not None:
            item_state = not data.tree_item.isExpanded()
            if item_state != state:
                if state:
                    self.collapseItem(data.tree_item)
                else:
                    self.expandItem(data.tree_item)

    def _update(self, root, *args):
        self._updating = True
        self.clear()
        self._to_collapse = []
        if root:
            self._root_item = self._name_to_items(root, self._editor)
            self._root_item.setText(0, self._editor.file.name)
            self.addTopLevelItem(self._root_item)
            self.expandAll()
            for item_to_collapse in reversed(self._to_collapse):
                self.collapseItem(item_to_collapse)
        else:
            root = QtWidgets.QTreeWidgetItem()
            root.setText(0, 'No data')
            root.setIcon(0, QtGui.QIcon.fromTheme(
                'emblem-important',
                QtGui.QIcon(':/ide-icons/rc/emblem-important.png')))
            self.addTopLevelItem(root)
        self._updating = False

    def set_editor(self, editor):
        """
        :type editor: pyqode.cobol.widgets.CobolCodeEdit
        :return:
        """
        if self._editor and self._editor != editor:
            try:
                self._editor.outline_mode.changed.disconnect(self._update)
                self._editor.folding_panel.trigger_state_changed.disconnect(
                    self._on_block_state_changed)
                self._editor.folding_panel.collapse_all_triggered.disconnect(
                    self.collapseAll)
                self._editor.folding_panel.expand_all_triggered.disconnect(
                    self.expandAll)
            except (AttributeError, RuntimeError):
                pass
        self._editor = editor
        try:
            self._editor.outline_mode.changed.connect(self._update)
            self._editor.folding_panel.trigger_state_changed.connect(
                self._on_block_state_changed)
            self._editor.folding_panel.collapse_all_triggered.connect(
                self.collapseAll)
            self._editor.folding_panel.expand_all_triggered.connect(
                self.expandAll)
        except AttributeError:
            self._update(None)
        else:
            self._update(editor.outline_mode.root_node)

    def _convert_name(self, name, editor):
        ti = QtWidgets.QTreeWidgetItem()
        ti.setText(0, name.name)
        ti.setIcon(0, QtGui.QIcon(ICONS[name.node_type]))
        ti.setToolTip(0, name.description)
        name.block = editor.document().findBlockByNumber(name.line)
        ti.setData(0, QtCore.Qt.UserRole, name)

        block_data = name.block.userData()
        if block_data is None:
            block_data = TextBlockUserData()
            name.block.setUserData(block_data)
        block_data.tree_item = ti

        if TextBlockHelper.get_fold_trigger_state(name.block):
            self._to_collapse.append(ti)

        for child in name.children:
            ti_ch = self._convert_name(child, editor)
            ti.addChild(ti_ch)

        return ti

    def _name_to_items(self, root, editor):
        """
        Converts each node in an AST (returned by the parser module) to a
        QTreeWidgetItem for display.
        """
        return self._convert_name(root, editor)
