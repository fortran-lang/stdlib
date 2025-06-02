program example_ansi_concat
  use stdlib_ansi, only : fg_color_red, style_reset, operator(//)
  implicit none

  print '(a)', fg_color_red // "Colorized text message" // style_reset
end program example_ansi_concat