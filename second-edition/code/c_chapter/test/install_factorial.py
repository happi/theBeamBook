#!/usr/bin/env python3
"""Install the book example into a disposable OTP 29 tree, never an installation."""
from pathlib import Path
import sys
root = Path(sys.argv[1]).resolve()
example = Path(__file__).resolve().parents[1] / 'erl_math.c'
assert (root / 'OTP_VERSION').read_text().strip() == '29.0'
beam = root / 'erts/emulator/beam'
math_c = beam / 'erl_math.c'
source = math_c.read_text()
if '#include "book_factorial.c"' not in source:
    math_c.write_text(source + '\n#include "book_factorial.c"\n')
(beam / 'book_factorial.c').write_text(example.read_text())
for name, line in [('bif.tab', 'bif math:factorial/1'),
                   ('atom.names', 'atom book_factorial_continue')]:
    p = beam / name
    if line not in p.read_text().splitlines():
        p.write_text(p.read_text() + '\n' + line + '\n')
p = beam / 'erl_init.c'
s = p.read_text()
if 'erts_init_book_factorial' not in s:
    s = s.replace('    erts_init_bif();',
                  '    erts_init_bif();\n    erts_init_book_factorial();')
    s = s.replace('static void erl_init(',
                  'extern void erts_init_book_factorial(void);\n\nstatic void erl_init(', 1)
    p.write_text(s)
p = root / 'lib/stdlib/src/math.erl'
s = p.read_text()
if '-export([factorial/1]).' not in s:
    s = s.replace('-export([pi/0,tau/0]).',
                  '-export([pi/0,tau/0]).\n-export([factorial/1]).')
    s += '\n-spec factorial(0..10000) -> pos_integer().\nfactorial(_) -> erlang:nif_error(undef).\n'
    p.write_text(s)
print('Installed factorial sources into', root)
