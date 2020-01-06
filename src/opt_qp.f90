submodule (stdlib_experimental_optval) opt_qp

implicit none

contains

module procedure optval_qp
  if (present(x)) then
      y = x
  else
      y = default
  end if
end procedure optval_qp

end submodule opt_qp