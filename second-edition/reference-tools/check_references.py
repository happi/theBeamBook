#!/usr/bin/env python3
"""Check PDF destinations and EPUB navigation without accessing the network.

Requires PyMuPDF for PDF input. Run from the directory containing the PDFs
and their companion, or pass absolute paths. Exits nonzero on broken links.
"""
import argparse
import json
from pathlib import Path
import posixpath
import re
from urllib.parse import unquote, urlsplit
from xml.etree import ElementTree as ET
from zipfile import ZipFile


def epub(path):
    with ZipFile(path) as archive:
        files = set(archive.namelist())
        docs = {n: ET.fromstring(archive.read(n)) for n in files if n.endswith('.xhtml')}
    ids = {}
    for name, doc in docs.items():
        elements = [e for e in doc.iter() if e.get('id')]
        ids[name] = {e.get('id'): e for e in elements}
        assert len(ids[name]) == len(elements), f'{name}: duplicate IDs'
    count = external = internal = 0
    sources = {}
    for name, doc in docs.items():
        for e in doc.iter():
            if e.get('class') == 'source-note':
                text = ' '.join(''.join(e.itertext()).split())
                urls = [x.get('href') for x in e.iter() if urlsplit(x.get('href', '')).scheme in ('http', 'https')]
                assert urls and re.match(r'\[S\d+\]', text), (name, text)
                assert 'http' in text, f'{name}: source URL is not printed: {text}'
                sources[e.get('id')] = {'text': text, 'urls': urls}
            for attr in ('href', 'src'):
                href = e.get(attr)
                if not href:
                    continue
                target = urlsplit(href)
                if target.scheme or target.netloc:
                    external += target.scheme in ('http', 'https')
                    continue
                destination = posixpath.normpath(posixpath.join(posixpath.dirname(name), unquote(target.path))) if target.path else name
                assert destination in files, f'{name}: missing file {href}'
                if target.fragment:
                    fragment = unquote(target.fragment)
                    assert fragment in ids.get(destination, {}), f'{name}: missing destination {href}'
                    label = ''.join(e.itertext()).strip()
                    if re.fullmatch(r'\[S\d+\]', label) and fragment.startswith('source-'):
                        definition = ' '.join(''.join(ids[destination][fragment].itertext()).split())
                        assert definition.startswith(label + ' '), (name, label, href, definition)
                        count += 1
                internal += 1
    assert count, f'{path}: no source citations checked'
    return {'path': str(path), 'citations': count, 'sources': len(sources), 'internal_links': internal, 'external_links': external}


def pdf(path):
    import pymupdf
    doc = pymupdf.open(path)
    names = doc.resolve_names()
    count = external = internal = remote = 0
    sources = {n: d for n, d in names.items() if n.startswith('source-') and not n.endswith('-ref')}
    for name, dest in sources.items():
        page = doc[dest['page']]
        point = pymupdf.Point(dest['to']) * page.transformation_matrix
        assert page.search_for('[S'), f'{path}: source page has no source labels: {name}'
        assert 0 <= point.y <= page.rect.height, (name, point)
    for page in doc:
        for link in page.get_links():
            if name := link.get('nameddest'):
                assert name in names, f'{path}: missing destination {name}'
                assert 0 <= names[name]['page'] < len(doc), (name, names[name])
                internal += 1
                if name in sources:
                    label = page.get_textbox(link['from']).strip()
                    marker = re.search(r'\[S\d+\]', label)
                    assert marker, (path, name, label)
                    label = marker.group()
                    target = names[name]
                    target_page = doc[target['page']]
                    point = pymupdf.Point(target['to']) * target_page.transformation_matrix
                    matches = target_page.search_for(label)
                    assert any(abs(rect.y0 - point.y) < 2 for rect in matches), (path, name, label, point)
                    count += 1
            elif link['kind'] == pymupdf.LINK_GOTO:
                assert 0 <= link['page'] < len(doc), (path, link)
                internal += 1
            elif link.get('file'):
                assert doc.xref_get_key(link['xref'], 'A/S')[1] == '/GoToR', (path, link)
                filename = doc.xref_get_key(link['xref'], 'A/F')[1]
                destination = doc.xref_get_key(link['xref'], 'A/D')[1]
                target = path.parent / unquote(filename)
                assert target.is_file(), f'{path}: missing file {target}'
                with pymupdf.open(target) as other:
                    assert destination in other.resolve_names(), (path, filename, destination)
                remote += 1
            elif uri := link.get('uri'):
                url = urlsplit(uri)
                if url.scheme in ('http', 'https', 'mailto'):
                    external += 1
                elif not url.scheme:
                    target = path.parent / unquote(url.path)
                    assert target.is_file(), f'{path}: missing file {uri}'
                    if url.fragment and target.suffix == '.pdf':
                        with pymupdf.open(target) as other:
                            assert unquote(url.fragment) in other.resolve_names(), (path, uri)
                    remote += 1
                else:
                    raise AssertionError(f'{path}: unsupported URI {uri}')
            else:
                raise AssertionError(f'{path}: unresolved link {link}')
    assert count, f'{path}: no source citations checked'
    assert doc.get_toc(), f'{path}: missing PDF outline'
    return {'path': str(path), 'pages': len(doc), 'citations': count, 'sources': len(sources), 'internal_links': internal, 'external_links': external, 'companion_links': remote}


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('files', type=Path, nargs='+')
    args = parser.parse_args()
    for path in args.files:
        print(json.dumps(epub(path) if path.suffix == '.epub' else pdf(path)))
