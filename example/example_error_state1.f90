program example_error_state1
  use stdlib_error, only: state_type, STDLIB_VALUE_ERROR, STDLIB_SUCCESS, operator(/=) 
  implicit none
  type(state_type) :: err

  ! To create a state variable, we enter its integer state flag, followed by a list of variables 
  ! that will be automatically assembled into a formatted error message. No need to provide string formats
  err = state_type(STDLIB_VALUE_ERROR,'just an example with scalar ',&
                   'integer=',1,'real=',2.0,'complex=',(3.0,1.0),'and array ',[1,2,3],'inputs')

  ! Print flag
  print *, err%print()

  ! Check success
  print *, 'Check error: ',err%error()
  print *, 'Check flag : ',err /= STDLIB_SUCCESS

end program example_error_state1
