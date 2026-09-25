require 'asciidoctor-pdf'
require 'json'
load ARGV.fetch(0)
module ListingPositionFixture
  def convert_listing node
    move_cursor_to node.attr('remaining').to_f if node.attr? 'remaining'
    super
  end
  def convert_literal node
    move_cursor_to node.attr('remaining').to_f if node.attr? 'remaining'
    super
  end
end
CustomPDFConverter.prepend ListingPositionFixture
text = "= Long listing fixture\n:doctype: article\n:!toc:\n:!sectnums:\n:source-highlighter: rouge\n\n"
cases = []
[31, 50, 110].each do |count|
  [27, 40, 80, 200, 400, 530].each do |remaining|
    %w[plain numbered literal].each do |kind|
      id = "L#{count}R#{remaining}#{kind}"
      lines = (1..count).map { |n| "    /* #{id}N#{n} body */" }
      lines[-1] = "#endif /* #{id}N#{count} */"
      attrs = kind == 'literal' ? 'literal' : 'source,c'
      attrs += ',linenums,highlight=2;3;30' if kind == 'numbered'
      text += "<<<\n\nLead #{id}.\n\n[#{attrs},remaining=#{remaining},id=#{id}]\n----\n#{lines.join("\n")}\n----\n\nAfter #{id}.\n\n"
      cases << [id, count]
    end
  end
end
# Actual sources implicated by the hardcover sweep, with cursor positions chosen
# to exercise both the first-line and last-line boundary of their long blocks.
[['parser', 'double_parse.h', 400], ['factorial', 'erl_math.c', 27]].each do |id, source, remaining|
  text += "<<<\n\nLead #{id}.\n\n[source,c,remaining=#{remaining},id=#{id}]\n----\n#{File.read("code/c_chapter/#{source}")}\n----\n\nAfter #{id}.\n\n"
end
# Oversized native unbreakable fallback and wrapped source comments.
text += "<<<\n\nLead wrapped.\n\n[source,c,%unbreakable,remaining=80,id=wrapped]\n----\n" + (1..50).map{|n| "/* WRAPPED#{n} " + ('comment ' * 18) + '*/'}.join("\n") + "\n----\n\nAfter wrapped.\n"
doc = Asciidoctor.load text, backend: 'pdf', safe: :unsafe, attributes: {'pdf-theme'=>ARGV.fetch(1), 'pdf-themesdir'=>'style', 'pdf-fontsdir'=>'style/fonts', 'media'=>'prepress'}
before = doc.find_by(context: :listing).to_h{|n| [n.id, n.lines.dup]}
doc.convert
after = doc.find_by(context: :listing).to_h{|n| [n.id, n.lines.dup]}
raise 'source lines mutated' unless before == after
doc.converter.write doc.converter, ARGV.fetch(2)
File.write(ARGV.fetch(2)+'.json', JSON.pretty_generate(cases))
