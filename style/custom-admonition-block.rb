require 'asciidoctor/extensions'

class VersionBlock < Asciidoctor::Extensions::BlockProcessor
  use_dsl
  named :VERSION
  on_context :example

  def process parent, reader, attrs
    attrs['name'] = 'version'
    attrs['caption'] = 'version'
    admon = create_block parent, :admonition, nil, attrs, content_model: :compound
    parse_content admon, reader
    admon
  end
end

class VersionBlockCss < Asciidoctor::Extensions::DocinfoProcessor
  use_dsl

  def process doc
    '<style>
.admonitionblock td.icon .icon-version:before{content:"\f02c";color:#900C3F}
</style>'
  end
end

Asciidoctor::Extensions.register do
  block VersionBlock
  docinfo_processor VersionBlockCss
end
