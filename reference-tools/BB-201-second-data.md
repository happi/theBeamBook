# BB-201: second-edition reference metadata

The public reference-data file contains 156 external source records. Each record supplies a source title, project or author, section or file information, and a printable URL. It contains bibliographic facts about public sources and no manuscript passages.

The JSON key is the complete source URL, including its version, query and fragment. Sentence punctuation outside a parsed URL is excluded from the key. No source target was replaced. The build converter must match the parsed, HTML-unescaped target against that key.

The 48 GitHub records print the repository URL. Their section field retains the release tag or commit, repository-relative file and referenced line numbers when present. The full deep link remains the JSON key and the digital hyperlink target. The 13 DOI records print `https://doi.org/...`. The 49 versioned Erlang documentation records retain their original major-version qualification; AtomVM documentation retains its release or prerelease version.

Titles were checked against the primary pages, repository file names and release references. Existing bibliographic facts were retained for papers and books. The source inventory covers named source footnotes in every chapter, optional deep dive, bibliography and root book entry point. The metadata deliberately makes no changes to citation order or numbering.

## Verification and limits

On 2026-09-25, direct requests completed for 143 of the 156 source URLs. None returned HTTP 404. Ten ACM DOI landing pages, GNU Emacs and Erlang Solutions rejected automated requests with HTTP 403. The Dragon Book site failed certificate verification. Two institutional resolver pages returned bot challenges, and several successful responses were PDFs or plain text without HTML titles. These responses do not establish that the original targets are missing.

A second check against Crossref confirmed seven DOI titles; six queries encountered rate limiting. The two Springer DOI titles were also available from their publisher pages. No blocked response was used to invent replacement bibliographic information. Existing paper and book titles, author lists, publication venues, dates and report identifiers remain the fallback for those entries.

A data check confirms that all 156 inventoried URL keys are present exactly once, all entries have nonempty title, project and display URL fields, and all display URLs are external. PDF destination correctness and printable layout are verified by the integration build, rather than inferred from this metadata inventory.
