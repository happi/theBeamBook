# Usage: bundle exec ruby reference-tools/check_html.rb site/index.html
require 'nokogiri'
require 'uri'
require 'pathname'
files = ARGV.map { |name| File.expand_path(name) }
docs = {}
load_doc = lambda { |path| docs[path] ||= Nokogiri::HTML(File.read(path)) }
files.each do |path|
  doc = load_doc.call(path)
  ids = doc.css('[id]').map { |e| e['id'] }
  raise "Duplicate IDs in #{path}" unless ids.uniq.size == ids.size
  count = 0
  doc.css('a[href], img[src]').each do |element|
    href = element['href'] || element['src']
    next if href.match?(%r{\A(?:[a-z]+:|//)}i)
    name, fragment = href.split('#', 2)
    target = name.empty? ? path : File.expand_path(URI::DEFAULT_PARSER.unescape(name), File.dirname(path))
    raise "Missing file #{href} in #{path}" unless File.file?(target)
    if fragment
      id = URI::DEFAULT_PARSER.unescape(fragment)
      destination = load_doc.call(target).at_xpath('//*[@id=$id]', nil, id: id)
      raise "Missing target #{href} in #{path}" unless destination
      if element.text.match?(/\A\[S\d+\]\z/)
        raise "Wrong source label #{href}" unless destination.text.start_with?(element.text + ' ')
        count += 1
      end
    end
  end
  puts "PASS: #{path}: #{count} source citations; all local links resolve"
end
