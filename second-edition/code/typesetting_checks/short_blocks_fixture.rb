require 'asciidoctor-pdf'
require 'json'
load ARGV.fetch(0)
$block_checks=[]
module BlockFixture
  def fixture_position node
    move_cursor_to node.attr('remaining').to_f if node.attr? 'remaining'
  end
  def convert_listing node
    fixture_position node
    super
  end
  def convert_literal node
    fixture_position node
    super
  end
  def convert_admonition node
    fixture_position node
    super
  end
  def arrange_block node, &block
    $block_checks << [node.id, node.context, node.option?('breakable'), node.option?('unbreakable')] unless scratch?
    super
  end
end
CustomPDFConverter.prepend BlockFixture
text="= Short block fixture\n:doctype: article\n:!sectnums:\n:!toc:\n\n"
%w[listing literal admonition].each do |kind|
 %w[auto breakable unbreakable oversize].each do |policy|
  id="#{kind}_#{policy}"; opts= %w[breakable unbreakable].include?(policy) ? ",options=#{policy}" : ''
  content=(1..8).map{|n| "#{id}_#{n} is visible here."}
  content=(1..3).map{|n| "#{id}_#{n} " + ('wrapped ' * 350)} if policy=='oversize'
  text+="<<<\n\n[#lead_#{id}]\nLead before #{id}.\n\n"
  case kind
  when 'listing'
   text+="[source,erlang,id=block_#{id}#{opts},remaining=80]\n----\n#{content.join("\n")}\n----\n\n"
  when 'literal'
   text+="[literal,id=block_#{id}#{opts},remaining=80]\n....\n#{content.join("\n")}\n....\n\n"
  when 'admonition'
   text+="[NOTE,id=block_#{id}#{opts},remaining=80]\n#{content.join(" +\n")}\n\n"
  end
 end
end
doc=Asciidoctor.load(text, backend:'pdf',safe: :unsafe,attributes:{'pdf-theme'=>ARGV.fetch(1),'pdf-themesdir'=>'style','pdf-fontsdir'=>'style/fonts','media'=>'prepress'})
doc.convert
File.write(ARGV.fetch(2)+'.json',JSON.pretty_generate({checked:$block_checks,after:doc.find_by.select{|n|n.id.to_s.start_with? 'block_'}.map{|n|[n.id,n.option?('breakable'),n.option?('unbreakable')]}}))
doc.converter.write doc.converter, ARGV.fetch(2)
