#!/usr/bin/env python3
"""Compare the pilot's module and instruction blocks with captured erlc -S output."""
from pathlib import Path
import re
import sys

assembly = Path(sys.argv[1]).read_text()
chapter = Path(sys.argv[2] if len(sys.argv) > 2 else
               'chapters/07_beam_instructions.asciidoc').read_text()
pilot = chapter.split('[[instruction-voice-pilot]]', 1)[1].split(
    '=== Record Instructions', 1)[0]
blocks = re.findall(r'\[source,erlang\]\n----\n(.*?)\n----', pilot, re.S)
normalize = lambda text: re.sub(r'\s+', '', text)
source = Path(__file__).with_name('packet_demo.erl').read_text()
assert normalize(blocks[0]) == normalize(source), 'Published module differs'
assembly = re.sub(r'^\s*\{line,.*\}\.$', '', assembly, flags=re.M)
excerpts = [block for block in blocks if block.startswith('{')]
assert excerpts, 'No instruction excerpts found'
for index, excerpt in enumerate(excerpts, 1):
    assert normalize(excerpt) in normalize(assembly), ('Unmatched excerpt', index)
print(f'PASS exact module and {len(excerpts)} assembly excerpts '
      '(ignoring whitespace and line annotations)')
