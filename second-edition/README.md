# The BEAM Book, second edition

## Examples

Use Erlang/OTP 29 and follow each chapter's setup instructions. When a
chapter says "from the repository root", start in this directory:

```sh
cd second-edition
erl
```

```erlang
1> c("code/beam_chapter/src/stack_machine").
{ok,stack_machine}
```

Native examples need the C toolchain and architecture specified in the
chapter. The WebSocket example needs Cowboy.

## Companion

[Implementation Deep Dives](implementation-deep-dives.pdf) contains the
optional runtime implementation notes, including the loader source walkthrough
and transformation rules in Chapter 9. Keep it beside your purchased
`beam-book-2nded.pdf` to use links between the two PDFs.

## Licence

Code examples use the repository's [CC BY 4.0 licence](../LICENSE).
The companion PDF carries its own copyright and licence notices, including
those for reused first-edition material and Erlang/OTP excerpts.
