#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
bundle_cmd="${BUNDLE:-bundle}"
mkdir -p output/reference-tests
fixture=reference-tools/test/references.adoc
"$bundle_cmd" exec asciidoctor-pdf -r ./style/custom-pdf-converter.rb "$fixture" -o output/reference-tests/references.pdf
"$bundle_cmd" exec asciidoctor -r ./reference-tools/digital_sources.rb "$fixture" -o output/reference-tests/references.html
"$bundle_cmd" exec asciidoctor-epub3 -r ./reference-tools/digital_sources.rb "$fixture" -o output/reference-tests/references.epub
python3 reference-tools/check_references.py output/reference-tests/references.pdf output/reference-tests/references.epub
python3 reference-tools/test/verify_fixture.py
