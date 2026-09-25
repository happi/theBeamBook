#!/usr/bin/env bash
# Run from any directory with OTP 29's erl and erlc on PATH.
set -euo pipefail
cd "$(dirname "$0")/../.."
export ERL_FLAGS="${ERL_FLAGS:-+S 2}"
check_dir=.proof/compiler-voice-check
mkdir -p "$check_dir/on" "$check_dir/off"
for stage in to_core dssa to_asm; do
    erlc -o "$check_dir/on" -DUSE_MAYBE \
        +'{feature,maybe_expr,enable}' +"$stage" code/compiler_voice/feature_demo.erl
    erlc -o "$check_dir/off" +'{feature,maybe_expr,disable}' \
        +"$stage" code/compiler_voice/feature_demo.erl
done
if erlc -o "$check_dir/off" -DUSE_MAYBE \
    +'{feature,maybe_expr,disable}' code/compiler_voice/feature_demo.erl \
    > "$check_dir/disabled-error.log" 2>&1; then
    echo 'Unexpected success: maybe syntax compiled with feature disabled' >&2
    exit 1
fi
erlc -o "$check_dir/on" -DUSE_MAYBE \
    +'{feature,maybe_expr,enable}' code/compiler_voice/feature_demo.erl
erlc -o "$check_dir/off" +'{feature,maybe_expr,disable}' \
    code/compiler_voice/feature_demo.erl
for mode in on off; do
    erl -pa "$check_dir/$mode" -noshell -eval '
        "29" = erlang:system_info(otp_release),
        5 = feature_demo:value({ok,4}),
        error = feature_demo:value(error),
        {error,missing} = feature_demo:value({error,missing}),
        try feature_demo:value({ok,wrong}) of
            _ -> error(accepted_non_number)
        catch error:badarith -> ok end,
        halt().'
done
python3 - <<'PY'
from pathlib import Path
import re
root = Path('.proof/compiler-voice-check')
normalized = lambda s: re.sub(r'^.*\{line,.*\n', '', s, flags=re.M)
a = (root / 'on/feature_demo.S').read_text()
b = (root / 'off/feature_demo.S').read_text()
assert normalized(a) == normalized(b), 'Final instructions differ'
assert 'maybe_else_fail' in (root / 'on/feature_demo.core').read_text()
assert 'maybe_else_fail' not in (root / 'off/feature_demo.core').read_text()
assert 'phi' in (root / 'on/feature_demo.ssa').read_text()
assert 'phi' not in (root / 'off/feature_demo.ssa').read_text()
print('Feature-on/off behavior, failure, Core/SSA distinction and BEAM convergence passed')
PY
erl -noshell -eval '
    [5,7,9] = [A+B || A <- [1,2,3] && B <- [4,5,6]],
    [5,6,7,6,7,8,7,8,9] = [A+B || A <- [1,2,3], B <- [4,5,6]],
    [1,2] = [N || {ok,N} <- [{ok,1},error,{ok,2}]],
    [11,12,21,22] = [X+1,X+2 || X <- [10,20]],
    try [N || {ok,N} <:- [{ok,1},error,{ok,2}]] of
        _ -> error(skipped_invalid_element)
    catch error:{badmatch,error} -> ok end,
    io:format("Comprehension observations passed~n"),halt().'
