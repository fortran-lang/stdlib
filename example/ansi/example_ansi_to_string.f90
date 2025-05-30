program example_ansi_to_string
  use stdlib_ansi, only : fg_color_green, style_reset, to_string
  implicit none

  print '(a)', to_string(fg_color_green) // "Colorized text message" // to_string(style_reset)
end program example_ansi_to_string