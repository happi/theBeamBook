"""Check before/after list_items_fixture.rb PDFs for both print trims."""
import collections
import json
import re
from pathlib import Path

import pymupdf
from PIL import Image, ImageDraw

ROOT = Path('output/bb166-lists')
results = {}
for trim in ('paperback', 'hardcover'):
    records = {}
    for state in ('before', 'after'):
        path = ROOT / f'{state}-{trim}.pdf'
        doc = pymupdf.open(path)
        pages = [p.get_text() for p in doc]
        # Remove only folios, retaining numbering and note markers for comparison.
        text = '\n'.join(re.sub(r'(?m)^\d+$', '', p) for p in pages)
        # Source-note labels intentionally gained an S prefix in BB-168.
        normalized = re.sub(r'\[S(\d+)\]', r'[\1]', text).replace('Sources and links', '')
        tokens = collections.Counter(re.findall(r'\w+|[•◦☑☐]', normalized))
        metadata = json.loads(path.with_suffix('.pdf.json').read_text())
        assert metadata['entry_options_before'] == metadata['entry_options_after'], (trim, state, 'entry options changed')
        assert len(metadata['footnotes']) == 3, metadata
        for note in ('LISTNOTEONCE', 'TERMNOTEONCE', 'BODYNOTEONCE'):
            assert text.count(note) == 1, (state, trim, note)

        def locations(token):
            pattern = rf'\b{re.escape(token)}\b'
            found = [i + 1 for i, page in enumerate(pages) if re.search(pattern, page)]
            assert sum(len(re.findall(pattern, page)) for page in pages) == 1, (token, found)
            return found[0]

        split = []
        groups = []
        for kind in ('ULIST', 'OLIST', 'DLIST'):
            for remaining in (28, 50, 80):
                for item in range(1, 4):
                    prefix = f'{kind}R{remaining}I{item}'
                    group = [prefix + f'L{n}' for n in range(1, 4)]
                    if kind == 'DLIST':
                        group.append(prefix + 'TERM')
                    groups.append(group)
        groups += [
            ['COMPOUNDHEAD', 'COMPOUNDCODE1', 'COMPOUNDCODE2'],
            ['NESTEDHEAD', 'NESTEDCHILD1', 'NESTEDCHILD2'],
            ['ALIASTERM1', 'ALIASTERM2', 'ALIASBODY', 'ALIASCLOSE'],
            ['NOTEITEM1', 'NOTEITEM1END'], ['NOTETERM', 'NOTEBODY', 'NOTEBODYEND'],
            ['REVERSE1', 'REVERSE1END'], ['ROMAN1', 'ROMAN1END'],
            ['CHECKEDITEM', 'CHECKEDEND'], ['QUESTION1', 'ANSWER1', 'ANSWER1END'],
            ['ORDERTERM1', 'ORDERBODY1', 'ORDERBODY1END'],
        ]
        for group in groups:
            found = sorted(set(locations(token) for token in group))
            if len(found) > 1:
                split.append([group[0], found])
        if state == 'after':
            assert not split, (trim, split)
        else:
            assert split, 'Fixture must expose baseline list splits'
        for token in ('EMPTYTERM', 'NEXTTERM', 'NEXTBODY', 'COMPOUNDNEXT', 'NESTEDNEXT',
                      'HORIZONTALTERM', 'HORIZONTALBODY', 'UNCHECKEDITEM'):
            locations(token)
        long_pages = {}
        for kind in ('ULIST', 'OLIST', 'DLIST'):
            found = [locations(f'LONG{kind}L{n}') for n in range(1, 111)]
            assert found == sorted(found), (trim, kind, 'reordered content')
            assert len(set(found)) >= 3, (trim, kind, 'oversized entry did not flow')
            assert locations(f'LONG{kind}NEXT') >= found[-1]
            long_pages[kind] = sorted(set(found))
        breakable_pages = sorted(set(locations(f'BREAKABLE{n}') for n in range(1, 7)))
        assert len(breakable_pages) > 1, (trim, state, 'breakable option ignored')

        # Confirm visible marker values and their baseline against each entry's
        # first line, catching numbers left on the preceding page or trial increments.
        markers = {'COMPOUNDHEAD': '12.', 'COMPOUNDNEXT': '13.',
                   'NESTEDHEAD': '4.', 'NESTEDCHILD1': 'a.', 'NESTEDCHILD2': 'b.',
                   'NESTEDNEXT': '5.', 'NOTEITEM1': '17.', 'NOTEITEM2': '18.',
                   'REVERSE1': '5.', 'REVERSE2': '4.', 'ROMAN1': 'iii.', 'ROMAN2': 'iv.',
                   'LONGOLISTL1': '1.', 'LONGOLISTNEXT': '2.'}
        for remaining in (28, 50, 80):
            for item in range(1, 4):
                markers[f'OLISTR{remaining}I{item}L1'] = f'{item + 6}.'
                markers[f'ULISTR{remaining}I{item}L1'] = '•'
        for token, marker in markers.items():
            page = doc[locations(token) - 1]
            word = next(w for w in page.get_text('words') if w[4] == token)
            assert any(w[4] == marker and w[2] <= word[0] and abs(w[1] - word[1]) < 2
                       for w in page.get_text('words')), (state, trim, token, marker)

        # Superscript labels must retain their numbers, including repeated refs.
        for token, label in [('NOTEITEM1', '[1]'), ('NOTEITEM2', '[1]'),
                             ('NOTETERM', '[2]'), ('NOTEBODY', '[3]'), ('NOTEBODYEND', '[1]')]:
            if state == 'after':
                label = label.replace('[', '[S')
            words = doc[locations(token) - 1].get_text('words')
            word = next(w for w in words if w[4] == token)
            assert any(w[4].rstrip('.') == label and w[0] > word[0] and abs(w[1] - word[1]) < 2
                       for w in words), (state, trim, token, label)
        for token, target in [('COMPOUNDLINK', 'COMPOUNDHEAD'), ('ALIASLINK', 'ALIASTERM1')]:
            page = doc[locations(token) - 1]
            rect = page.search_for(token)[0]
            matching = [link for link in page.get_links() if link['from'].intersects(rect)]
            assert len(matching) == 1 and matching[0].get('page') == locations(target) - 1, (state, trim, token, matching)
        assert any(link.get('uri') == 'https://example.org'
                   for page in doc for link in page.get_links()), (state, trim, 'external link lost')
        records[state] = {'pages': len(doc), 'split_small_entries': split,
                          'long_entry_pages': long_pages, 'breakable_pages': breakable_pages,
                          'tokens': tokens, 'footnotes': metadata['footnotes']}

        # Full 200 dpi pages for visual checks of representative moved entries,
        # compound content, terms, notes, and oversized fallback boundaries.
        selected = sorted(set(locations(token) for token in (
            'ULISTR28I1L1', 'COMPOUNDHEAD', 'NESTEDHEAD', 'ALIASBODY',
            'NOTEBODY', 'LONGDLISTL1', 'LONGDLISTL110')))
        thumbnails = []
        for number in selected:
            pix = doc[number - 1].get_pixmap(dpi=200)
            im = Image.frombytes('RGB', [pix.width, pix.height], pix.samples)
            im.save(ROOT / f'{state}-{trim}-p{number:03}.png')
            im.thumbnail((630, 850))
            thumbnails.append((number, im))
        canvas = Image.new('RGB', (630 * 4, 880 * 2), 'white')
        draw = ImageDraw.Draw(canvas)
        for index, (number, im) in enumerate(thumbnails):
            x, y = (index % 4) * 630, (index // 4) * 880
            draw.text((x + 5, y + 5), f'{state} {trim} physical {number}', fill='black')
            canvas.paste(im, (x, y + 25))
        canvas.save(ROOT / f'{state}-{trim}-visual.jpg')
    assert records['before'].pop('tokens') == records['after'].pop('tokens'), (trim, 'content changed')
    assert records['before']['footnotes'] == [
        [index, note_id.removeprefix('source-'), text]
        for index, note_id, text in records['after']['footnotes']
    ]
    results[trim] = records
(ROOT / 'list-item-results.json').write_text(json.dumps(results, indent=2) + '\n')
print(json.dumps(results, indent=2))
