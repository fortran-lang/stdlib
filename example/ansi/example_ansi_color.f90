program example_ansi_color
  use stdlib_ansi, only : fg_color_blue, style_bold, style_reset, ansi_code, &
    & operator(//), operator(+)
  implicit none
  type(ansi_code) :: highlight, reset

  print '(a)', highlight // "Dull text message" // reset

  highlight = fg_color_blue + style_bold
  reset = style_reset

  print '(a)', highlight // "Colorful text message" // reset
end program example_ansi_color