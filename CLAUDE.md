# CLAUDE.md — The BEAM Book

## Project

**The BEAM Book** — A deep dive into the Erlang runtime system internals.
by Erik Stenman (Happi)

Status: Published (1st edition). Uses AsciiDoc, not Markdown.

## Build

This project uses AsciiDoc (not the standard Markdown pipeline). See the repository's build instructions.

## Backlog

Task tracking uses **Backlog.md** with prefix `bb`. The backlog lives in `backlog/` with config in `backlog/config.yml`.

- **ID server**: `http://backlog.lan.stenmans.org` — generates unique, monotonic task IDs
- **CLI**: `backlog task create "Title" --priority medium` (note: `-p` is `--parent`, use `--priority` for priority)
- **MCP**: The backlog MCP server exposes task tools automatically via `.mcp.json`
