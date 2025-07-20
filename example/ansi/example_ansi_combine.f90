program example_ansi_combine
  use stdlib_ansi, only : fg_color_red, style_bold, ansi_code, operator(+), to_string
  implicit none
  type(ansi_code) :: bold_red

  bold_red = fg_color_red + style_bold
  print '(a)', to_string(bold_red)
  
end program example_ansi_combine