require 'asciidoctor-pdf'
require 'json'
load ARGV.fetch(0)

module ListPositionFixture
  [:convert_ulist, :convert_olist, :convert_dlist].each do |method|
    define_method method do |node|
      move_cursor_to node.attr('remaining').to_f if node.attr? 'remaining'
      super node
    end
  end
end
CustomPDFConverter.prepend ListPositionFixture

text = "= List pagination fixture\n:doctype: article\n:!sectnums:\n:!toc:\n\n"
%w[ulist olist dlist].each do |kind|
  [28, 50, 80].each do |remaining|
    id = "#{kind.upcase}R#{remaining}"
    text += "<<<\n\nLead before #{id}.\n\n[remaining=#{remaining}#{kind == 'olist' ? ',start=7' : ''}]\n"
    3.times do |index|
      marker = "#{id}I#{index + 1}"
      prefix = {'ulist' => '* ', 'olist' => '. ', 'dlist' => "#{marker}TERM:: "}[kind]
      text += prefix + (1..3).map { |line| "#{marker}L#{line} bold *words* and `code`." }.join(" +\n") + "\n"
    end
    text += "\n"
  end
end
text += <<~ADOC
<<<

Lead before compound entries.

[remaining=48,start=12]
. [[compound-entry]]COMPOUNDHEAD Write the script.
+
[source,erlang]
----
COMPOUNDCODE1 = first.
COMPOUNDCODE2 = second.
----
. COMPOUNDNEXT Run the script.

<<<

Lead before nested entries.

[remaining=60,start=4]
. NESTEDHEAD Outer entry.
.. NESTEDCHILD1 Child entry one.
.. NESTEDCHILD2 Child entry two.
. NESTEDNEXT Outer entry two.

<<<

Lead before description aliases and continuation.

[remaining=48]
[[alias-entry]]ALIASTERM1::
ALIASTERM2:: ALIASBODY The body belongs to both terms.
+
ALIASCLOSE Continued paragraph in the same entry.
EMPTYTERM::
NEXTTERM:: NEXTBODY Next entry after a term without a body.

<<<

Lead before repeated notes.

[remaining=30,start=17]
. NOTEITEM1 An entry with footnote:source-listnote[LISTNOTEONCE unique shared list note.] a note. +
NOTEITEM1END Its complete ending.
. NOTEITEM2 References the same footnote:source-listnote[] again.

[remaining=30]
NOTETERM footnote:source-termnote[TERMNOTEONCE unique term note.]:: NOTEBODY with footnote:source-bodynote[BODYNOTEONCE unique body note.]. +
NOTEBODYEND Its complete ending, citing footnote:source-listnote[] again.

<<<

Lead before reversed numbering.

[remaining=30,start=5,options=reversed]
. REVERSE1 First entry. +
REVERSE1END Final line.
. REVERSE2 Next entry.

[lowerroman,remaining=30,start=3]
. ROMAN1 First entry. +
ROMAN1END Final line.
. ROMAN2 Next entry.

[remaining=30]
* [x] CHECKEDITEM First entry. +
CHECKEDEND Final line.
* [ ] UNCHECKEDITEM Next entry.

<<<

Lead before special description styles.

[qanda,remaining=30]
QUESTION1:: ANSWER1 First answer. +
ANSWER1END Final line.
QUESTION2:: ANSWER2 Next answer.

[ordered,remaining=30]
ORDERTERM1:: ORDERBODY1 First answer. +
ORDERBODY1END Final line.
ORDERTERM2:: ORDERBODY2 Next answer.

[horizontal]
HORIZONTALTERM:: HORIZONTALBODY Existing horizontal layout retained.

Check <<compound-entry,COMPOUNDLINK>> and <<alias-entry,ALIASLINK>>, plus https://example.org[EXTERNALLINK].

<<<

Lead before explicit breakable list.

[remaining=45,options=breakable]
* BREAKABLE1 First line. +
BREAKABLE2 Second line. +
BREAKABLE3 Third line. +
BREAKABLE4 Fourth line. +
BREAKABLE5 Fifth line. +
BREAKABLE6 Sixth line.

ADOC
%w[ulist olist dlist].each do |kind|
  id = "LONG#{kind.upcase}"
  prefix = {'ulist' => '* ', 'olist' => '. ', 'dlist' => "#{id}TERM:: "}[kind]
  text += "<<<\n\nLead before #{id}.\n\n[remaining=55]\n" + prefix
  text += (1..110).map { |line| "#{id}L#{line} A long entry continues naturally." }.join(" +\n")
  text += "\n" + prefix + "#{id}NEXT The following entry remains present.\n\n"
end
doc = Asciidoctor.load text, backend: 'pdf', safe: :unsafe, attributes: {
  'pdf-theme' => ARGV.fetch(1), 'pdf-themesdir' => 'style',
  'pdf-fontsdir' => 'style/fonts', 'media' => 'prepress'
}
entries = doc.find_by.select { |node| [:ulist, :olist, :dlist].include? node.context }
  .flat_map { |list| list.items.flatten.compact }
# Preserve a pre-existing entry option as well as removing temporary options.
entries.find { |entry| entry.text.start_with? 'ULISTR50I2L1' }.set_attr 'unbreakable-option', ''
before_options = entries.map { |entry| entry.option? 'unbreakable' }
doc.convert
doc.converter.write doc.converter, ARGV.fetch(2)
File.write ARGV.fetch(2) + '.json', JSON.pretty_generate(
  footnotes: doc.catalog[:footnotes].map { |note| [note.index, note.id, note.text] },
  entry_options_before: before_options,
  entry_options_after: entries.map { |entry| entry.option? 'unbreakable' }
)
