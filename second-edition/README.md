# Code examples for The BEAM Book, second edition

This directory contains the code examples for the second edition of
The BEAM Book (2026), revised against Erlang/OTP 29.0.

When a chapter says "from the repository root", it means this directory.
Clone the repository, enter `second-edition`, then start the Erlang shell:

```sh
git clone https://github.com/happi/theBeamBook.git
cd theBeamBook/second-edition
erl
```

You can then compile an example:

```erlang
1> c("code/beam_chapter/src/stack_machine").
{ok,stack_machine}
```

The examples are organised under `code/`, with sources in `src/` where a
chapter has several files. Use Erlang/OTP 29 and follow the setup in the
chapter. Native examples also need the stated C toolchain and architecture;
the WebSocket example needs Cowboy.

Verification scripts accompany the examples. They check selected runtime
behaviors and compiler output. Some require an OTP source tree, an
interpreter build, or the second-edition manuscript. The scripts under
`code/typesetting_checks/` check manuscript layout and require the private
book sources and build tools. These checks do not reproduce every
transcript in the book.

The examples use the same licence as the first-edition examples in this
repository; see [LICENSE](../LICENSE). Existing third-party notices remain
in their source files. The second-edition manuscript is a separate,
commercial work.
