# frozen_string_literal: true
require_relative 'source_catalog'

# Source notes use semantic destinations; explanatory notes retain the host
# converter's placement and numbering policy.
module BeamPDFSources
  # A remote PDF destination is a GoToR action, not a web URI containing a
  # filename. The release packager ensures this file accompanies the book.
  def link_annotation(rect, options = {})
    action = options[:A]
    if action && action[:S] == :URI && (match = /\A([^:#]+\.pdf)#(.+)\z/.match(action[:URI].to_s))
      options = options.merge(A: { Type: :Action, S: :GoToR,
        F: PDF::Core::LiteralString.new(match[1]),
        D: PDF::Core::LiteralString.new(match[2]), NewWindow: true })
    end
    super(rect, options)
  end

  def convert_inline_footnote(node)
    index = node.attr 'index'
    fn = node.document.footnotes.find { |item| item.index == index }
    return super unless fn && BeamSourceCatalog.source?(fn)
    info = BeamSourceCatalog.info(node, fn)
    target = BeamSourceCatalog.note_id(fn)
    anchor = node.type == :xref ? '' : %(<a id="#{target}-ref">#{Asciidoctor::PDF::Converter::DummyText}</a>)
    %(<span class="wj">#{anchor}<a anchor="#{target}">[#{info[:label]}]</a>#{BeamSourceCatalog.qualifier(node, info)}</span>)
  end

  def ink_footnotes(node)
    sources = node.document.footnotes.select do |fn|
      BeamSourceCatalog.source?(fn) && !@rendered_footnotes.include?(fn)
    end
    unless sources.empty?
      theme_margin :block, :bottom
      theme_font :footnotes do
        advance_page if !at_page_top? && cursor < font_size * 6
        ink_caption 'Sources and links', category: :footnotes
        sources.each do |fn|
          info = fn.instance_variable_get(:@book_note_info)
          raise "Source #{fn.id} has no citation label" unless info
          target = BeamSourceCatalog.note_id(fn)
          text = BeamSourceCatalog.describe(node.document, fn.text)
          row = %(<a id="#{target}">#{Asciidoctor::PDF::Converter::DummyText}</a><a anchor="#{target}-ref">[#{info[:label]}]</a> #{text})
          height = height_of_typeset_text row, inline_format: true
          advance_page if !at_page_top? && cursor < height
          ink_prose row,
            margin_bottom: @theme.footnotes_item_spacing, hyphenate: false, align: :left
        end
      end
      @rendered_footnotes += sources
    end
    super
  end
end

# The first edition's anonymous explanations remain separate from the new
# source counter. This adapter is not needed by hosts with page-footnotes.
module BeamChapterExplanations
  def convert_inline_footnote(node)
    index = node.attr 'index'
    fn = node.document.footnotes.find { |item| item.index == index }
    return super unless fn && !BeamSourceCatalog.source?(fn)
    info = BeamSourceCatalog.info(node, fn)
    target = BeamSourceCatalog.note_id(fn)
    anchor = node.type == :xref ? '' : %(<a id="#{target}-ref">#{Asciidoctor::PDF::Converter::DummyText}</a>)
    %(<sup class="wj">#{anchor}<a anchor="#{target}">#{info[:label]}</a></sup>)
  end

  def ink_footnotes(node)
    notes = node.document.footnotes.reject { |fn| @rendered_footnotes.include?(fn) }
    return if notes.empty?
    theme_margin :block, :bottom
    theme_font :footnotes do
      ink_caption 'Notes', category: :footnotes
      notes.each do |fn|
        info = fn.instance_variable_get(:@book_note_info)
        target = BeamSourceCatalog.note_id(fn)
        ink_prose %(<a id="#{target}">#{Asciidoctor::PDF::Converter::DummyText}</a><a anchor="#{target}-ref">#{info[:label]}.</a> #{fn.text}),
          margin_bottom: @theme.footnotes_item_spacing
      end
    end
    @rendered_footnotes += notes
  end
end
