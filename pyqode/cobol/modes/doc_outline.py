import logging
from pyqode.qt.QtCore import QObject, Signal
from pyqode.cobol.api import defined_names, cmp_name, Name
from pyqode.core.api import Mode, DelayJobRunner


def _logger():
    return logging.getLogger(__name__)


class DocumentOutlineMode(QObject, Mode):
    """
    Parses the current cobol document when the text changed and emit the
    changed event if any properties of any document node has changed.

    This mode can be used to implement a document outline widget.

    """
    #: Signal emitted when the document layout changed
    changed = Signal(Name, list, list)

    @property
    def root_node(self):
        """
        Returns the document root node.
        """
        return self._root_node

    @property
    def variables(self):
        """
        Returns the list of variable document nodes
        """
        return self._vars

    @property
    def paragraphs(self):
        """
        Returns the list of paragraphs document nodes
        """
        return self._paragraphs

    def __init__(self):
        QObject.__init__(self)
        Mode.__init__(self)
        self._root_node = None
        self._vars = []
        self._paragraphs = []
        self._runner = DelayJobRunner()

    def on_state_changed(self, state):
        """
        Called when the mode is activated/deactivated
        """
        if state:
            self.editor.new_text_set.connect(self.parse)
            self.editor.textChanged.connect(self._parse)
        else:
            self.editor.new_text_set.disconnect(self.parse)
            self.editor.textChanged.disconnect(self._parse)
            self._runner.cancel_requests()

    def _parse(self):
        self._runner.request_job(self.parse)

    def parse(self):
        """ Parse the document layout.

        To get the results, use the following properties:
            - root_node
            - variables
            - paragraphs
        """
        # preview in preferences dialog have no file path
        if not self.editor.file.path:
            return
        txt = self.editor.toPlainText()
        fmt = self.editor.free_format
        try:
            root_node, variables, paragraphs = defined_names(txt, fmt)
        except AttributeError:
            # this should never happen but we must exit gracefully
            _logger().exception("Failed to parse document, probably due to "
                                "a malformed syntax.")
        else:
            changed = False
            if self._root_node is None or cmp_name(root_node, self._root_node):
                changed = True
            self._root_node = root_node
            self._vars = variables
            self._paragraphs = paragraphs
            if changed:
                _logger().debug('changed')
                self.changed.emit(
                    self.root_node, self.variables, self.paragraphs)
