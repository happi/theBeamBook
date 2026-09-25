# frozen_string_literal: true
# Bibliographic facts live beside this formatter; builds never fetch metadata.
require 'json'
require 'cgi'
require 'nokogiri'

module BeamSourceCatalog
  def self.catalog(document)
    name = document.attr 'source-catalog'
    return {} unless name
    @catalogs ||= {}
    @catalogs[name] ||= JSON.parse(File.read(File.expand_path(name, File.join(__dir__, 'data'))))
  end

  def self.entry(document, url)
    catalog(document)[CGI.unescapeHTML(url)]
  end

  def self.describe(document, text)
    return text unless document.attr? 'source-catalog'
    fragment = Nokogiri::HTML.fragment(text)
    fragment.css('a[href]').each do |link|
      url = link['href']
      next unless url.start_with?('http://', 'https://')
      record = entry(document, url)
      raise ArgumentError, "Missing source metadata: #{url}" unless record
      display = record.fetch('display_url')
      link.content = display
      words = [record.fetch('project'), record.fetch('title'), record['section']].compact.reject(&:empty?)
      link.add_previous_sibling(Nokogiri::XML::Text.new(words.join('. ') + '. ', fragment.document))
    end
    fragment.to_html
  end

  def self.note_id(fn)
    # Semantic footnote IDs do not change when an earlier citation is inserted.
    fn.id || "note-#{fn.index}"
  end

  def self.qualifier(node, info)
    owner = node.parent
    owner = owner.parent while owner.parent && !(owner.context == :section && owner.level == 1)
    owner.id == info[:owner] ? '' : " (source in #{info[:owner_title]})"
  end

  def self.source?(fn)
    fn.id.to_s.start_with?('source-')
  end

  def self.info(node, fn)
    info = fn.instance_variable_get(:@book_note_info)
    return info if info
    owner = node.parent
    owner = owner.parent while owner.parent && !(owner.context == :section && owner.level == 1)
    kind = source?(fn) ? :source : :explanation
    count = node.document.footnotes.count do |other|
      prior = other.instance_variable_get(:@book_note_info)
      prior && prior[:owner] == owner.id && prior[:kind] == kind
    end
    info = { owner: owner.id, owner_title: owner.title, kind: kind, label: "#{kind == :source ? 'S' : ''}#{count + 1}" }
    fn.instance_variable_set(:@book_note_info, info)
    info
  end
end
