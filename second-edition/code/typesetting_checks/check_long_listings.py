"""Compare long-listing fixtures and check native frame geometry."""
import collections
import json
from pathlib import Path
import re
import sys
import pymupdf

root = Path(sys.argv[1] if len(sys.argv) > 1 else 'output/bb166-hardcover-final')
marker_re = re.compile(r'(L\d+R\d+(?:plain|numbered|literal))N(\d+)')
results = {}
for trim in ['paperback', 'hardcover']:
    reports = {}
    sequences = {}
    contents = {}
    for mode in ['baseline', 'fixed']:
        path = root / f'long-{mode}-{trim}.pdf'
        document = pymupdf.open(path)
        expected = dict(json.loads(Path(str(path) + '.json').read_text()))
        pages = collections.defaultdict(collections.Counter)
        sequence = []
        geometry_errors = []
        code_tokens = []
        for number, page in enumerate(document, 1):
            frames = [drawing['rect'] for drawing in page.get_drawings()
                      if drawing['rect'].width > 200 and drawing['rect'].height > 5
                      and drawing['color'] and abs(drawing['color'][0] - .8) < .01]
            hits = collections.Counter()
            for block in page.get_text('dict')['blocks']:
                for line in block.get('lines', []):
                    for span in line['spans']:
                        text = span['text']
                        for case, index in marker_re.findall(text):
                            pages[case][number] += 1
                            sequence.append((case, int(index)))
                        # Body listings use the theme's monospaced font. This
                        # includes continuation fragments with no marker.
                        if not span['font'].startswith('Hack-') or not text.strip():
                            continue
                        code_tokens.extend(re.findall(r'\S+', text))
                        box = pymupdf.Rect(span['bbox'])
                        matched = False
                        for index, frame in enumerate(frames):
                            if frame.y0 - 1 <= box.y0 and box.y1 <= frame.y1 + 1:
                                hits[index] += 1
                                matched = True
                        if not matched:
                            geometry_errors.append([number, text, list(box)])
            if mode == 'fixed':
                assert len(hits) == len(frames), ('empty frame', trim, number)
        assert set(pages) == set(expected)
        for case, count in expected.items():
            indexes = [index for key, index in sequence if key == case]
            assert indexes == list(range(1, count + 1)), (trim, mode, case, indexes)
        singletons = {case: dict(counts) for case, counts in pages.items()
                      if len(counts) > 1 and min(counts.values()) == 1}
        if mode == 'fixed':
            assert not singletons, (trim, singletons)
            assert not geometry_errors, (trim, geometry_errors[:3])
        reports[mode] = {'pages': len(document), 'cases': len(expected),
                         'single_line_cases': len(singletons),
                         'geometry_errors': len(geometry_errors)}
        sequences[mode] = sequence
        contents[mode] = re.sub(r"\s+", "", "".join(code_tokens))
    assert sequences['baseline'] == sequences['fixed']
    assert contents['baseline'] == contents['fixed'], ('code content changed', trim)
    results[trim] = reports
print(json.dumps(results, indent=2))
(root / 'long-listing-results.json').write_text(json.dumps(results, indent=2) + '\n')
