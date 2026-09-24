"""Check both trim fixtures produced by short_blocks_fixture.rb."""
import json
from pathlib import Path

import pymupdf

ROOT = Path('output/bb166-pagination')
results = {}
for trim in ('paperback', 'hardcover'):
    path = ROOT / f'blocks-{trim}.pdf'
    document = pymupdf.open(path)
    calls = json.loads(path.with_suffix('.pdf.json').read_text())
    text = [page.get_text() for page in document]
    blocks = {}
    for kind in ('listing', 'literal', 'admonition'):
        for policy in ('auto', 'breakable', 'unbreakable', 'oversize'):
            marker = f'{kind}_{policy}_'
            pages = [i + 1 for i, content in enumerate(text) if marker in content]
            if policy in ('auto', 'unbreakable'):
                assert len(pages) == 1, (trim, marker, pages)
            else:
                assert len(pages) > 1, (trim, marker, pages)
            for number in range(1, 4 if policy == 'oversize' else 9):
                assert sum(content.count(f'{marker}{number}') for content in text) == 1
            blocks[marker.rstrip('_')] = pages
    assert len(calls['checked']) == 12
    for block_id, kind, breakable, unbreakable in calls['checked']:
        assert breakable == block_id.endswith('_breakable'), (block_id, breakable)
        assert unbreakable == (not breakable), (block_id, unbreakable)
    for block_id, breakable, unbreakable in calls['after']:
        assert breakable == block_id.endswith('_breakable')
        assert unbreakable == block_id.endswith('_unbreakable'), (block_id, unbreakable)
    assert sum(content.count('wrapped') for content in text) == 3150
    results[trim] = {'pages': len(document), 'block_marker_pages': blocks,
                     'wrapped_tokens': 3150, 'option_restoration': 'pass'}
(ROOT / 'short-block-results.json').write_text(json.dumps(results, indent=2) + '\n')
print(json.dumps(results, indent=2))
