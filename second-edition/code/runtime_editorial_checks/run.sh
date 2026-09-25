#!/usr/bin/env bash
set -euo pipefail
runtime_check_root=$(cd "$(dirname "$0")/../.." && pwd)
runtime_check_tmp=$(mktemp -d)
trap 'rm -rf "$runtime_check_tmp"' EXIT
cd "$runtime_check_root"
escript code/runtime_editorial_checks/check.escript
erlc -o "$runtime_check_tmp" code/runtime_editorial_checks/listlen.erl code/runtime_editorial_checks/arith_typed.erl code/runtime_editorial_checks/dispatch.erl
(
    cd "$runtime_check_tmp"
    erl +S 2 +JDdump true -noshell -eval '3 = listlen:len([a,b,c]), 1999998 = arith_typed:sum_pair({999999,999999}), {1,3,1,42} = dispatch:run(1,2), halt().' >stdout.txt 2>stderr.txt
)
# The interpreter listing in the JIT chapter needs an emu-flavor build,
# which installed releases rarely carry; check it when it is there.
if erl -emu_flavor emu -noshell -eval 'halt().' >/dev/null 2>&1; then
    (
        cd "$runtime_check_tmp"
        erl -emu_flavor emu -noshell -eval '{1,3,1,42} = dispatch:run(1,2), erts_debug:df(dispatch), halt().' >>stdout.txt 2>>stderr.txt
    )
    for line in 'i_plus_xxjd x(0) x(1) j(0) x(1)' 'move_cx `42` x(3)' 'move_rx r(0) x(2)' 'i_call_only_f loc(`dispatch`:`collect`/4)'; do
        grep -qF "$line" "$runtime_check_tmp/dispatch.dis" || { echo "missing in emu disassembly: $line"; exit 1; }
    done
    echo 'PASS emu flavor loaded the dispatch sequence shown in the JIT chapter'
else
    echo 'SKIP emu flavor check (no -emu_flavor emu in this OTP)'
fi
python3 - "$runtime_check_tmp" "$runtime_check_root" <<'PY'
from pathlib import Path
import re
import sys
out, root = map(Path, sys.argv[1:])
assert (out/'stderr.txt').stat().st_size == 0
actual = (out/'listlen.asm').read_text()
fixture = (root/'code/runtime_editorial_checks/listlen-excerpts.asm').read_text()
for part in fixture.split('; ... omitted emitted instructions ...'):
    assert part.strip() in actual, part
assert 'mov qword ptr [rbx], 15' in actual
bounded = (out/'arith_typed.asm').read_text()
assert 'add/2:' in bounded
helper = bounded.split('add/2:',1)[1].split('# i_func_label_L',1)[0]
assert 'i_plus' in helper
assert 'jno ' not in helper and 'jo ' not in helper, helper
# Local label numbers (L13, L14, ...) depend on the build, so compare with them masked.
mask = lambda t: re.sub(r'\bL\d+\b', 'L', t)
dispatch = mask((out/'dispatch.asm').read_text())
excerpt = mask((root/'code/runtime_editorial_checks/dispatch-excerpts.asm').read_text())
assert excerpt.strip() in dispatch, excerpt
print('PASS captured JIT excerpts, .asm output path, bounded no-overflow helper, and dispatch sequence')
PY
