"""Check the two trim outputs produced by footnote_fixture.rb."""
from pathlib import Path
import pymupdf

for trim in ('paperback', 'hardcover'):
    document = pymupdf.open(Path('output/bb166') / f'footnotes-{trim}.pdf')
    texts = [page.get_text() for page in document]
    raw = ''.join(texts)
    for note in range(1, 51):
        for word in range(1, 17):
            assert raw.count(f'NOTE{note:02}_WORD{word:02}') == 1, (trim, note, word)
        counts = [sum(f'NOTE{note:02}_' in line for line in text.splitlines())
                  for text in texts]
        fragments = [count for count in counts if count]
        assert len(fragments) == 1 or min(fragments) >= 2, (trim, note, fragments)
    print(f'{trim}: 50 notes, 800 words each present once, no single-line note splits')
