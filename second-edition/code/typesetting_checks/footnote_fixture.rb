require 'asciidoctor-pdf'
load ARGV.fetch(0)
text = "= URL note pagination fixture\n:doctype: book\n:!toc:\n:!sectnums:\n\n== References\n\n"
(1..50).each do |n|
  words = (1..16).map {|i| "NOTE#{n.to_s.rjust(2, '0')}_WORD#{i.to_s.rjust(2, '0')}" }.join(' ')
  text << "Reference #{n}.footnote:source-fixture-#{n}[#{words}]\n\n"
end
Asciidoctor.convert text, backend: 'pdf', safe: :unsafe, to_file: ARGV.fetch(2), attributes: {
  'pdf-themesdir' => 'style', 'pdf-theme' => ARGV.fetch(1), 'pdf-fontsdir' => 'style/fonts'
}
