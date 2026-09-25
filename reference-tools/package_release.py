#!/usr/bin/env python3
"""Package linked reading files together, refusing missing PDF companions."""
import argparse
from pathlib import Path
from urllib.parse import unquote, urlsplit
from zipfile import ZipFile, ZIP_DEFLATED
import pymupdf

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--output', required=True, type=Path)
parser.add_argument('files', nargs='+', type=Path)
args = parser.parse_args()
names = {p.name for p in args.files}
assert len(names) == len(args.files), 'Duplicate release filenames'
for path in args.files:
    assert path.is_file(), f'Missing release file: {path}'
    if path.suffix != '.pdf':
        continue
    with pymupdf.open(path) as doc:
        for page in doc:
            for link in page.get_links():
                target = doc.xref_get_key(link['xref'], 'A/F')[1] if link.get('file') else None
                if not target and 'uri' in link:
                    url = urlsplit(link['uri'])
                    target = url.path if not url.scheme else None
                if target:
                    assert unquote(target) in names, f'{path.name} links to unpackaged file: {target}'
args.output.parent.mkdir(parents=True, exist_ok=True)
with ZipFile(args.output, 'w', ZIP_DEFLATED) as archive:
    for path in args.files:
        archive.write(path, path.name)
    archive.writestr('READ-ME.txt', 'The BEAM Book PDF release\n\nExtract all files into one directory before opening the PDFs. Keep their\nfilenames unchanged so links between the book and its companion work.\n\nImplementation Deep Dives is optional: it contains specialised runtime\nimplementation notes and source walkthroughs. Each book link names the\nrelevant section. The EPUB includes these notes in its own navigation.\n')
print(args.output)
