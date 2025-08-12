! Demonstrate usage of `is_symlink`
program example_is_symlink
  use stdlib_system, only: is_symlink, is_directory
  implicit none

  character(*), parameter :: path = "path/to/check"

  ! Test if path is a symbolic link
  if (is_symlink(path)) then
    print *, "The specified path is a symlink."
    ! Further check if it is linked to a file or a directory
    if (is_directory(path)) then
        print *, "Further, it is a link to a directory."
    else
        print *, "Further, it is a link to a file."
    end if
  else
    print *, "The specified path is not a symlink."
  end if
end program example_is_symlink
