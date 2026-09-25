# BB-166 list pagination proof

Small entries previously split because the native list renderer inks their
text directly, bypassing paragraph widow protection. Markers are already drawn
by that point. The converter now arranges each complete entry before drawing
its marker, or its description-list terms. Entries that exceed a page use the
native breakable fallback. The default description-list layout is retained from
asciidoctor-pdf 2.3.24; special styles continue through the native converter.

The fixture was rendered with the paperback `pdf-2nded` and hardcover
`pdf-2nded-hc` themes, before and after the change. Both trims exposed 13 small
entry splits before the change and none afterward. Page counts remained 51 for
paperback and 49 for hardcover. Each of the three 110-line entries still spans
four or five pages, and the explicit breakable list remains split.

The checker verifies unchanged text tokens, decimal/reversed/Roman numbering,
nested numbering, numbering after oversized entries, markers on the same line
as their item, all three footnotes appearing once, repeated inline note labels,
internal cross-reference destinations, an external link, restored item options,
multiple description terms, a description without a body, and an item containing
a short source listing. It checks every numbered long-entry line exactly once.

Seven selected final pages per trim were rendered at 200 dpi and visually
inspected in contact sheets: paperback 2, 20, 22, 24, 27, 47, 51; hardcover
2, 20, 22, 24, 27, 45, 48. Paperback 20 and 27 were also inspected as enlarged
single pages. Compound code, aliases, nested markers, footnote references and
oversized continuation were readable and intact. These are fixture checks;
the final full book still needs review after integration.

Generated evidence lives under `output/bb166-lists/` in the isolated
`beambook-BB-166-lists` worktree. `list-item-results.json` records the assertions.

To reproduce, save the parent converter as
`output/bb166-lists/before-converter.rb`, then run the fixture for each converter
and theme with the repository's Bundler environment:

```sh
bundle3.2 exec ruby code/typesetting_checks/list_items_fixture.rb \
  style/custom-pdf-converter.rb pdf-2nded \
  output/bb166-lists/after-paperback.pdf
```

Use `pdf-2nded-hc` and `after-hardcover.pdf` for hardcover. Repeat with the saved
converter and `before-paperback.pdf` / `before-hardcover.pdf`, then run:

```sh
# needs a python with pymupdf and Pillow on its path
python3 code/typesetting_checks/check_list_items.py
```

The parent agent's separate endnote widow wrapper was not included in this
isolated fixture; the combined integration should run these same checks.
