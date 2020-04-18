class CustomPDFConverter < (Asciidoctor::Converter.for 'pdf')
  register_for 'pdf'

  # Customize the layout of part titles
  def layout_part_title node, title, opts = {}
    puts 'Processing node.id:' + node.id
    puts 'Processing title:' + title
    num_part, title = title.split ':', 2
    move_down 150
    typeset_text num_part + "\n" + title, (calc_line_metrics 1.5), inline_format: true, align: :center
  end

  # Customize the layout of:
  #  - Preface
  #  - Appendix
  #  - Chapter
  def layout_chapter_title node, title, opts = {}
    puts 'Processing node.id:' + node.id
    puts 'Processing title:' + title
    if title == "Preface" or title.include? "Appendix"
      move_down 1
      typeset_text title, (calc_line_metrics 1.5), inline_format: true, align: :right, size: 21
      stroke_horizontal_rule '000000', line_width: 0.5, line_style: :solid
    else
      num_chapter, title = title.split '.', 2
      typeset_text num_chapter, (calc_line_metrics 1.5), inline_format: true, align: :right, size: 15
      move_down 1
      stroke_horizontal_rule '000000', line_width: 0.5, line_style: :solid
      typeset_text title, (calc_line_metrics 1.5), inline_format: true, align: :right, size: 21
    end
    move_down 50
  end
end

