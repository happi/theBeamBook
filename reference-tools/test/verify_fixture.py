"""Assert repeated citations, chapter-local labels, print text and deep links."""
from pathlib import Path
from zipfile import ZipFile
from xml.etree import ElementTree as ET
import pymupdf
root = Path('output/reference-tests')
expected = {'source-one': '[S1]', 'source-github': '[S2]', 'source-two': '[S1]'}
pdf = pymupdf.open(root / 'references.pdf')
names = pdf.resolve_names()
assert all(n in names for n in expected)
text = '\n'.join(p.get_text() for p in pdf)
assert 'https://doi.org/10.1234/test' in text
assert 'https://github.com/example/project' in text
assert 'src/file.c' in text and 'v1' in text
assert 'source in One' in text
links = [l for p in pdf for l in p.get_links()]
assert sum(l.get('nameddest') == 'source-one' for l in links) == 3
assert any(l.get('uri') == 'https://github.com/example/project/blob/v1/src/file.c#L12' for l in links)
assert any(pdf.xref_get_key(l['xref'], 'A/S')[1] == '/GoToR'
           and pdf.xref_get_key(l['xref'], 'A/F')[1] == 'references.pdf'
           and pdf.xref_get_key(l['xref'], 'A/D')[1] == 'chapter-two' for l in links)
with ZipFile(root / 'references.epub') as archive:
    docs = [ET.fromstring(archive.read(n)) for n in archive.namelist() if n.endswith('.xhtml')]
    entries = {e.get('id'): e for doc in docs for e in doc.iter() if e.get('class') == 'source-note'}
    assert set(entries) == set(expected)
    for name, label in expected.items():
        assert ''.join(entries[name].itertext()).startswith(label + ' ')
    # Native source definitions must not survive as duplicate note entries.
    assert not any(e.get('id') in ('note-1', 'note-3', 'note-4') for doc in docs for e in doc.iter())
print('PASS: chapter counters, repeated references, print metadata, deep URLs and remote PDF destinations')
