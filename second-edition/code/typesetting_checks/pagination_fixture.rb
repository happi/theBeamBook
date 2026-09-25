require 'asciidoctor-pdf'
load ARGV.fetch(0)
module PositionFixture
  def convert_open node
    if node.has_role? 'fixture-columns'
      column_box([bounds.left, cursor], columns: 2, width: bounds.width, height: 120) { super }
    else
      super
    end
  end
  def convert_floating_title node
    move_cursor_to(node.attr('remaining').to_f) if node.attr? 'remaining'
    theme.heading_min_height_after = 16.875 if ARGV[0].include? 'baseline'
    super
  end
  def convert_paragraph node
    move_cursor_to(node.attr('remaining').to_f) if node.attr? 'remaining'
    super
  end
end
CustomPDFConverter.prepend PositionFixture
text = "= Pagination fixture\n:doctype: article\n:!sectnums:\n:!toc:\n:hyphens: en_us\n\n"
[8, 18, 27, 40, 53, 66].each do |remaining|
  [false, true].each do |indented|
    [2, 3, 5].each do |count|
      id = "R#{remaining}I#{indented ? 1 : 0}N#{count}"
      text += "<<<\n\n[.#{indented ? 'indent' : 'fixture'},remaining=#{remaining}]\n"
      text += (1..count).map { |n| "#{id}L#{n} *bold* `code` https://example.org[x]." }.join(" +\n") + "\n\n"
    end
  end
end
text += "<<<\n\n[remaining=40,%hardbreaks]\n" + (1..110).map { |n| "LONG#{n} Text in a paragraph spanning several pages." }.join(" +\n") + "\n\n"
text += "<<<\n\n[remaining=20]\nFOOT1 A paragraph with footnote:source-fixture-alpha[FOOTNOTE_ALPHA unique note content.] enough words to take several lines when the page is nearly full. " + "We inspect the output and ensure the note appears once.\n\n"
text += "== Heading before block\n\n[NOTE]\n====\nThis is an unbreakable note followed by normal prose.\n====\n\nA normal paragraph.\n\n* A list item.\n+\nA paragraph inside a list, with `code` and a https://example.org[link].\n\n.A table caption\n[cols=\"a,a\",options=header]\n|===\n|First |Second\n|A cell paragraph.\n\nA second cell paragraph.\n|Another cell paragraph.\n|===\n"
text += <<~ADOC

<<<

[discrete,remaining=60]
== HEADINGMARK

[NOTE]
====
NOTEAFTERHEADING starts here. +
Second note line. +
Third note line. +
Fourth note line.
====

[%unbreakable]
--
BLOCKMARK A paragraph in an unbreakable block with a footnote:source-fixture-beta[FOOTNOTE_BETA unique nested note.]

Another paragraph in the block.
--

* LISTMARK A list item.
+
[remaining=27]
LISTPARA A paragraph within a list. +
Second line. +
Third line. +
Fourth line.

<<<

[remaining=60]
TABLELEAD A paragraph above a captioned table.

.TABLECAPTION
[cols="a,a"]
|===
|TABLECELL1 A paragraph. +
Second line. +
Third line.

Another cell paragraph.
|TABLECELL2 A paragraph with a footnote:source-fixture-gamma[FOOTNOTE_GAMMA table note.]
|===

<<<

[.fixture-columns]
--
[remaining=22]
COLUMNMARK1 First column line. +
COLUMNMARK2 Second column line. +
COLUMNMARK3 Third column line. +
COLUMNMARK4 Fourth column line.
--
ADOC
Asciidoctor.convert text, backend: 'pdf', safe: :unsafe, to_file: ARGV.fetch(2), attributes: {'pdf-theme' => ARGV.fetch(1), 'pdf-themesdir' => 'style', 'pdf-fontsdir' => 'style/fonts', 'media' => 'prepress'}
