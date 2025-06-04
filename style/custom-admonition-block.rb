require 'asciidoctor/extensions'

class VersionBlock < Asciidoctor::Extensions::BlockProcessor
  use_dsl
  named :VERSION
  on_context :example

  def process parent, reader, attrs
    # For EPUB3, use NOTE admonition type with custom styling
    if parent.document.backend == 'epub3'
      attrs['name'] = 'note'
      attrs['caption'] = 'Version Info'
      # Add a custom class for styling
      attrs['role'] = 'version'
    else
      attrs['name'] = 'version'
      attrs['caption'] = 'version'
    end
    
    admon = create_block parent, :admonition, nil, attrs, content_model: :compound
    parse_content admon, reader
    admon
  end
end

class VersionBlockCss < Asciidoctor::Extensions::DocinfoProcessor
  use_dsl

  def process doc
    if doc.backend == 'html5'
      '<style>
.admonitionblock td.icon .icon-version:before{content:"\f02c";color:#900C3F}
</style>'
    elsif doc.backend == 'epub3'
      '<style>
.admonitionblock.version {
  border-left: 5px solid #900C3F;
  background-color: #f9f2f4;
}
.admonitionblock.version .title {
  color: #900C3F;
  font-weight: bold;
}
</style>'
    else
      ''
    end
  end
end

Asciidoctor::Extensions.register do
  block VersionBlock
  docinfo_processor VersionBlockCss
end
