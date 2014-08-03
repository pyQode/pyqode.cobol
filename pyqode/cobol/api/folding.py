from pyqode.core.api import FoldDetector, TextBlockHelper


class CobolFoldDetector(FoldDetector):
    def detect_fold_level(self, prev_block, block):
        if not prev_block:
            return 0
        ctext = block.text().rstrip().upper()
        if ctext.find(' USING ') != -1:
            ctext = ctext[:ctext.find(' USING ')] + '.'
        ptext = prev_block.text().rstrip().upper()
        if ptext.find(' USING ') != -1:
            ptext = ptext[:ptext.find(' USING ')] + '.'
        print(ctext, ptext)
        if ctext.endswith('DIVISION.'):
            return 0
        elif ctext.endswith('SECTION.'):
            return 1
        elif ptext.endswith('DIVISION.'):
            return 1
        elif ptext.endswith('SECTION.'):
            return 2
        return TextBlockHelper.get_fold_lvl(prev_block)
