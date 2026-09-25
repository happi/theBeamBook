# frozen_string_literal: true
require 'asciidoctor/converter/html5'
require 'asciidoctor-epub3'
require 'pathname'
require_relative 'source_catalog'

# Relocate only source notes. Native explanatory notes, bibliography entries,
# heading IDs and the EPUB spine stay with their original converter.
module BeamDigitalSources
  def convert_inline_footnote(node)
    index = node.attr 'index'
    fn = node.document.footnotes.find { |item| item.index == index }
    return super unless fn && BeamSourceCatalog.source?(fn)
    info = BeamSourceCatalog.info(node, fn)
    @source_entries ||= {}
    entry = @source_entries[index] ||= { id: BeamSourceCatalog.note_id(fn), label: info[:label], text: node.text, count: 0 }
    occurrence = entry[:count] += 1
    %(<span class="source-ref"><a id="#{entry[:id]}-ref-#{occurrence}" data-source-index="#{index}" href="##{entry[:id]}">[#{entry[:label]}]</a>#{BeamSourceCatalog.qualifier(node, info)}</span>)
  end

  def convert_document(node)
    result = super
    return result unless @source_entries && !@source_entries.empty?
    epub = node.backend == 'epub3'
    if epub
      items = result.items.values.select { |item| item.media_type == 'application/xhtml+xml' }
      documents = items.to_h { |item| [item.href, Nokogiri::XML(item.content)] }
    else
      documents = { '' => Nokogiri::HTML(result) }
    end
    lists = {}
    @source_entries.each do |index, entry|
      refs = documents.flat_map do |path, doc|
        doc.css("[data-source-index='#{index}']").map { |ref| [path, ref] }
      end
      raise "Source #{entry[:id]} has no rendered marker" if refs.empty?
      path, first = refs.first
      doc = documents.fetch(path)
      owner = first.ancestors.find { |element| (element['class'] || '').split.include?(epub ? 'chapter' : 'sect1') } || doc.at_css(epub ? 'body' : '#content')
      list = lists[[path, owner.object_id]] ||= begin
        section = Nokogiri::XML::Node.new('section', doc)
        section['class'] = 'source-notes'
        section['aria-label'] = 'Sources and links'
        heading = Nokogiri::XML::Node.new(epub ? 'h2' : 'h3', doc)
        heading.content = 'Sources and links'
        section.add_child(heading)
        owner.add_child(section)
        section
      end
      paragraph = Nokogiri::XML::Node.new('p', doc)
      paragraph['id'] = entry[:id]
      paragraph['class'] = 'source-note'
      paragraph.add_child(Nokogiri::XML::Text.new("[#{entry[:label]}] ", doc))
      paragraph.add_child(paragraph.fragment(BeamSourceCatalog.describe(node, entry[:text])))
      refs.each do |ref_path, ref|
        ref.remove_attribute('data-source-index')
        relative = ref_path == path ? '' : Pathname.new(path).relative_path_from(Pathname.new(ref_path).dirname).to_s
        ref['href'] = "#{relative}##{entry[:id]}"
        ref['role'] = 'doc-noteref'
        back = Nokogiri::XML::Node.new('a', doc)
        back_path = ref_path == path ? '' : Pathname.new(ref_path).relative_path_from(Pathname.new(path).dirname).to_s
        back['href'] = "#{back_path}##{ref['id']}"
        back['aria-label'] = "Return to source #{entry[:label]} citation"
        back.content = ' ↩'
        paragraph.add_child(back)
      end
      list.add_child(paragraph)
      documents.each_value do |document|
        document.css("#_footnotedef_#{index}, aside#note-#{index}").each(&:remove)
      end
    end
    documents.each_value do |document|
      if epub
        document.css('img[width]').each do |img|
          if img['width'].end_with?('%')
            img['style'] = [img['style'], "width: #{img['width']};"].compact.join(' ')
            img.remove_attribute('width')
          end
        end
      end
      document.css('#footnotes').each { |notes| notes.remove unless notes.at_css('.footnote') }
    end
    if epub
      items.each { |item| item.add_raw_content(documents.fetch(item.href).to_xml) }
      result
    else
      documents.fetch('').to_html
    end
  end
end
Asciidoctor::Converter::Html5Converter.prepend(BeamDigitalSources)
Asciidoctor::Epub3::Converter.prepend(BeamDigitalSources)
