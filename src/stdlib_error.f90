module stdlib_error
    !! Provides support for catching and handling errors
    !! ([Specification](../page/specs/stdlib_error.html))
    use, intrinsic :: iso_fortran_env, only: stderr => error_unit, ilp => int32
    use stdlib_optval, only: optval
    use stdlib_kinds, only: int8, int16, int32, int64, sp, dp, xdp, qp, lk
    implicit none
    private

    interface ! f{08,18}estop.f90
        module subroutine error_stop(msg, code)
            !! version: experimental
            !!
            !! Provides a call to `error stop` and allows the user to specify a code and message
            !! ([Specification](..//page/specs/stdlib_error.html#description_1))
            character(*), intent(in) :: msg
            integer, intent(in), optional :: code
        end subroutine error_stop
    end interface

    public :: check, error_stop

    !> Version: experimental
    !>
    !> A fixed-storage state variable for error handling of linear algebra routines
    public :: state_type

    !> Version: experimental
    !>
    !> Interfaces for comparison operators of error states with integer flags
    public :: operator(==),operator(/=)
    public :: operator(<),operator(<=)
    public :: operator(>),operator(>=)

    !> Base state return types for 
    integer(ilp),parameter,public :: STDLIB_SUCCESS        =  0_ilp
    integer(ilp),parameter,public :: STDLIB_VALUE_ERROR    = -1_ilp
    integer(ilp),parameter,public :: STDLIB_LINALG_ERROR   = -2_ilp
    integer(ilp),parameter,public :: STDLIB_INTERNAL_ERROR = -3_ilp
    integer(ilp),parameter,public :: STDLIB_IO_ERROR       = -4_ilp
    integer(ilp),parameter,public :: STDLIB_FS_ERROR       = -5_ilp

    !> Use fixed-size character storage for performance
    integer(ilp),parameter :: MSG_LENGTH = 512_ilp
    integer(ilp),parameter :: NAME_LENGTH = 32_ilp

    !> `state_type` defines a general state return type for a
    !> stdlib routine. State contains a status flag, a comment, and a
    !> procedure specifier that can be used to mark where the error happened
    type :: state_type

        !> The current exit state
        integer(ilp) :: state = STDLIB_SUCCESS

        !> Message associated to the current state
        character(len=MSG_LENGTH) :: message = repeat(' ',MSG_LENGTH)

        !> Location of the state change
        character(len=NAME_LENGTH) :: where_at = repeat(' ',NAME_LENGTH)

        contains

           !> Cleanup
           procedure :: destroy   => state_destroy
           
           !> Parse error constructor
           procedure, private :: state_parse_at_location
           procedure, private :: state_parse_arguments
           generic   :: parse     => state_parse_at_location, &
                                     state_parse_arguments

           !> Print error message
           procedure :: print     => state_print
           procedure :: print_msg => state_message

           !> State properties
           procedure :: ok        => state_is_ok
           procedure :: error     => state_is_error
           
           !> Handle optional error message 
           procedure :: handle    => error_handling

    end type state_type

    !> Comparison operators
    interface operator(==)
        module procedure state_eq_flag
        module procedure flag_eq_state
    end interface
    interface operator(/=)
        module procedure state_neq_flag
        module procedure flag_neq_state
    end interface
    interface operator(<)
        module procedure state_lt_flag
        module procedure flag_lt_state
    end interface
    interface operator(<=)
        module procedure state_le_flag
        module procedure flag_le_state
    end interface
    interface operator(>)
        module procedure state_gt_flag
        module procedure flag_gt_state
    end interface
    interface operator(>=)
        module procedure state_ge_flag
        module procedure flag_ge_state
    end interface
    
    !> Assignment operator
    interface assignment(=)
        module procedure state_assign_state
    end interface assignment(=)

    interface state_type
        module procedure new_state
        module procedure new_state_nowhere
    end interface state_type

    !> Format strings with edit descriptors for each type and kind
    !> cannot be retrieved from stdlib_io due to circular dependencies
    character(*), parameter :: &
      FMT_INT         = '(i0)', &
      FMT_REAL_SP     = '(es15.8e2)', &
      FMT_REAL_DP     = '(es24.16e3)', &
      FMT_REAL_XDP    = '(es26.18e3)', &
      FMT_REAL_QP     = '(es44.35e4)', &
      FMT_COMPLEX_SP  = '(es15.8e2,1x,es15.8e2)', &
      FMT_COMPLEX_DP  = '(es24.16e3,1x,es24.16e3)', &
      FMT_COMPLEX_XDP = '(es26.18e3,1x,es26.18e3)', &
      FMT_COMPLEX_QP  = '(es44.35e4,1x,es44.35e4)'

contains

    subroutine check(condition, msg, code, warn)
        !! version: experimental
        !!
        !! Checks the value of a logical condition
        !! ([Specification](../page/specs/stdlib_error.html#description))
        !!
        !!##### Behavior
        !!
        !! If `condition == .false.` and:
        !!
        !!   * No other arguments are provided, it stops the program with the default
        !!     message and exit code `1`;
        !!   * `msg` is provided, it prints the value of `msg`;
        !!   * `code` is provided, it stops the program with the given exit code;
        !!   * `warn` is provided and `.true.`, it doesn't stop the program and prints
        !!     the message.
        !!
        !!##### Examples
        !!
        !!* If `a /= 5`, stops the program with exit code `1`
        !!  and prints `Check failed.`
        !!``` fortran
        !!  call check(a == 5)
        !!```
        !!
        !!* As above, but prints `a == 5 failed`.
        !!``` fortran
        !!  call check(a == 5, msg='a == 5 failed.')
        !!```
        !!
        !!* As above, but doesn't stop the program.
        !!``` fortran
        !!  call check(a == 5, msg='a == 5 failed.', warn=.true.)
        !!```
        !!
        !!* As example #2, but stops the program with exit code `77`
        !!``` fortran
        !!  call check(a == 5, msg='a == 5 failed.', code=77)
        !!```

        !
        ! Arguments
        ! ---------

        logical, intent(in) :: condition
        character(*), intent(in), optional :: msg
        integer, intent(in), optional :: code
        logical, intent(in), optional :: warn
        character(*), parameter :: msg_default = 'Check failed.'

        if (.not. condition) then
            if (optval(warn, .false.)) then
                write(stderr,*) optval(msg, msg_default)
            else
                call error_stop(optval(msg, msg_default), optval(code, 1))
            end if
        end if

    end subroutine check

    !> Cleanup the object
    elemental subroutine state_destroy(this)
        class(state_type),intent(inout) :: this

        this%state = STDLIB_SUCCESS
        this%message = repeat(' ',len(this%message))
        this%where_at = repeat(' ',len(this%where_at))

    end subroutine state_destroy

    !> Interface to print stdlib error messages
    pure function state_flag_message(flag) result(msg)
        integer(ilp),intent(in) :: flag
        character(len=:),allocatable :: msg

        select case (flag)
           case (STDLIB_SUCCESS);        msg = 'Success!'
           case (STDLIB_VALUE_ERROR);    msg = 'Value Error'
           case (STDLIB_LINALG_ERROR);   msg = 'Linear Algebra Error'
           case (STDLIB_IO_ERROR);       msg = 'I/O Error'
           case (STDLIB_FS_ERROR);       msg = 'Filesystem Error'
           case (STDLIB_INTERNAL_ERROR); msg = 'Internal Error'
           case default;                 msg = 'INVALID/UNKNOWN STATE FLAG'
        end select

    end function state_flag_message

    !> Return a formatted message
    pure function state_message(this) result(msg)
        class(state_type),intent(in) :: this
        character(len=:),allocatable :: msg

        if (this%state == STDLIB_SUCCESS) then
           msg = 'Success!'
        else
           msg = state_flag_message(this%state)//': '//trim(this%message)
        end if

    end function state_message

    !> Flow control: on output flag present, return it; otherwise, halt on error
    pure subroutine error_handling(ierr,ierr_out)
         class(state_type), intent(in) :: ierr
         class(state_type), optional, intent(inout) :: ierr_out

         character(len=:),allocatable :: err_msg

         if (present(ierr_out)) then
             ! Return error flag
             ierr_out = ierr
         elseif (ierr%error()) then
             err_msg = ierr%print()
             error stop err_msg
         end if

    end subroutine error_handling

    !> Produce a nice error string
    pure function state_print(this) result(msg)
         class(state_type),intent(in) :: this
         character(len=:),allocatable :: msg

         if (len_trim(this%where_at) > 0) then
            msg = '['//trim(this%where_at)//'] returned '//this%print_msg()
         elseif (this%error()) then
            msg = 'Error encountered: '//this%print_msg()
         else
            msg = this%print_msg()
         end if

    end function state_print

    !> Check if the current state is successful
    elemental logical(lk) function state_is_ok(this)
        class(state_type),intent(in) :: this
        state_is_ok = this%state == STDLIB_SUCCESS
    end function state_is_ok

    !> Check if the current state is an error state
    elemental logical(lk) function state_is_error(this)
        class(state_type),intent(in) :: this
        state_is_error = this%state /= STDLIB_SUCCESS
    end function state_is_error

    !> Compare an error state with an integer flag
    elemental logical(lk) function state_eq_flag(err,flag)
        class(state_type),intent(in) :: err
        integer,intent(in) :: flag
        state_eq_flag = err%state == flag
    end function state_eq_flag

    !> Compare an integer flag with the error state
    elemental logical(lk) function flag_eq_state(flag,err)
        integer,intent(in) :: flag
        class(state_type),intent(in) :: err
        flag_eq_state = err%state == flag
    end function flag_eq_state

    !> Compare the error state with an integer flag
    elemental logical(lk) function state_neq_flag(err,flag)
        class(state_type),intent(in) :: err
        integer,intent(in) :: flag
        state_neq_flag = .not. state_eq_flag(err,flag)
    end function state_neq_flag

    !> Compare an integer flag with the error state
    elemental logical(lk) function flag_neq_state(flag,err)
        integer,intent(in) :: flag
        class(state_type),intent(in) :: err
        flag_neq_state = .not. state_eq_flag(err,flag)
    end function flag_neq_state

    !> Compare the error state with an integer flag
    elemental logical(lk) function state_lt_flag(err,flag)
        class(state_type),intent(in) :: err
        integer,intent(in) :: flag
        state_lt_flag = err%state < flag
    end function state_lt_flag

    !> Compare the error state with an integer flag
    elemental logical(lk) function state_le_flag(err,flag)
        class(state_type),intent(in) :: err
        integer,intent(in) :: flag
        state_le_flag = err%state <= flag
    end function state_le_flag

    !> Compare an integer flag with the error state
    elemental logical(lk) function flag_lt_state(flag,err)
        integer,intent(in) :: flag
        class(state_type),intent(in) :: err
        flag_lt_state = err%state < flag
    end function flag_lt_state

    !> Compare an integer flag with the error state
    elemental logical(lk) function flag_le_state(flag,err)
        integer,intent(in) :: flag
        class(state_type),intent(in) :: err
        flag_le_state = err%state <= flag
    end function flag_le_state

    !> Compare the error state with an integer flag
    elemental logical(lk) function state_gt_flag(err,flag)
        class(state_type),intent(in) :: err
        integer,intent(in) :: flag
        state_gt_flag = err%state > flag
    end function state_gt_flag

    !> Compare the error state with an integer flag
    elemental logical(lk) function state_ge_flag(err,flag)
        class(state_type),intent(in) :: err
        integer,intent(in) :: flag
        state_ge_flag = err%state >= flag
    end function state_ge_flag

    !> Compare an integer flag with the error state
    elemental logical(lk) function flag_gt_state(flag,err)
        integer,intent(in) :: flag
        class(state_type),intent(in) :: err
        flag_gt_state = err%state > flag
    end function flag_gt_state

    !> Compare an integer flag with the error state
    elemental logical(lk) function flag_ge_state(flag,err)
        integer,intent(in) :: flag
        class(state_type),intent(in) :: err
        flag_ge_state = err%state >= flag
    end function flag_ge_state

    !> Assign a state type to another
    elemental subroutine state_assign_state(to, from)
        class(state_type), intent(inout) :: to
        class(state_type), intent(in)    :: from
        
        to%state = from%state
        to%message = from%message
        to%where_at = from%where_at
        
    end subroutine state_assign_state

    !> Append a generic value to the error flag (rank-agnostic)
    pure subroutine appendr(msg,a,prefix)
       class(*),optional,intent(in) :: a(..)
       character(len=*),intent(inout) :: msg
       character,optional,intent(in) :: prefix
       
       if (present(a)) then 
          select rank (v=>a)
             rank (0)
                call append (msg,v,prefix)
             rank (1)
                call appendv(msg,v)
             rank default
                msg = trim(msg)//' <ERROR: INVALID RANK>'
          
          end select   
       endif
       
    end subroutine appendr

    ! Append a generic value to the error flag
    pure subroutine append(msg,a,prefix)
       class(*),intent(in) :: a
       character(len=*),intent(inout) :: msg
       character,optional,intent(in) :: prefix

       character(len=MSG_LENGTH) :: buffer,buffer2
       character(len=2) :: sep
       integer :: ls

       ! Do not add separator if this is the first instance
       sep = '  '
       ls = merge(1,0,len_trim(msg) > 0)

       if (present(prefix)) then
           ls = ls + 1
           sep(ls:ls) = prefix
       end if

       select type (aa => a)

        !> String type
        type is (character(len=*))
            msg = trim(msg)//sep(:ls)//aa

        !> Numeric types
        type is (real(sp))
            write (buffer,FMT_REAL_sp) aa
            msg = trim(msg)//sep(:ls)//trim(adjustl(buffer))

        type is (real(dp))
            write (buffer,FMT_REAL_dp) aa
            msg = trim(msg)//sep(:ls)//trim(adjustl(buffer))

        type is (integer(int8))
            write (buffer,FMT_INT) aa
            msg = trim(msg)//sep(:ls)//trim(adjustl(buffer))

        type is (integer(int16))
            write (buffer,FMT_INT) aa
            msg = trim(msg)//sep(:ls)//trim(adjustl(buffer))

        type is (integer(int32))
            write (buffer,FMT_INT) aa
            msg = trim(msg)//sep(:ls)//trim(adjustl(buffer))

        type is (integer(int64))
            write (buffer,FMT_INT) aa
            msg = trim(msg)//sep(:ls)//trim(adjustl(buffer))

        type is (complex(sp))
            write (buffer, FMT_REAL_sp) aa%re
            write (buffer2,FMT_REAL_sp) aa%im
            msg = trim(msg)//sep(:ls)//'('//trim(adjustl(buffer))//','//trim(adjustl(buffer2))//')'

        type is (complex(dp))
            write (buffer, FMT_REAL_dp) aa%re
            write (buffer2,FMT_REAL_dp) aa%im
            msg = trim(msg)//sep(:ls)//'('//trim(adjustl(buffer))//','//trim(adjustl(buffer2))//')'

        class default
            msg = trim(msg)//' <ERROR: INVALID TYPE>'

       end select

    end subroutine append

    !> Append a generic vector to the error flag
    pure subroutine appendv(msg,a)
       class(*),intent(in) :: a(:)
       character(len=*),intent(inout) :: msg

       integer :: j,ls
       character(len=MSG_LENGTH) :: buffer,buffer2,buffer_format
       character(len=2) :: sep

       if (size(a) <= 0) return

       ! Default: separate elements with one space
       sep = '  '
       ls = 1

       ! Open bracket
       msg = trim(msg)//' ['

       ! Do not call append(msg(aa(j))), it will crash gfortran
       select type (aa => a)

        !> Strings (cannot use string_type due to `sequence`)
        type is (character(len=*))
            msg = trim(msg)//adjustl(aa(1))
            do j = 2,size(a)
               msg = trim(msg)//sep(:ls)//adjustl(aa(j))
            end do

        !> Numeric types
        type is (real(sp))
            buffer_format = FMT_REAL_sp

            write (buffer,buffer_format) aa(1)
            msg = trim(msg)//adjustl(buffer)
            do j = 2,size(a)
               write (buffer,buffer_format) aa(j)
               msg = trim(msg)//sep(:ls)//adjustl(buffer)
            end do
            msg = trim(msg)//sep(:ls)//trim(adjustl(buffer))
        type is (real(dp))
            buffer_format = FMT_REAL_dp

            write (buffer,buffer_format) aa(1)
            msg = trim(msg)//adjustl(buffer)
            do j = 2,size(a)
               write (buffer,buffer_format) aa(j)
               msg = trim(msg)//sep(:ls)//adjustl(buffer)
            end do
            msg = trim(msg)//sep(:ls)//trim(adjustl(buffer))
        type is (integer(int8))
            buffer_format = FMT_INT

            write (buffer,buffer_format) aa(1)
            msg = trim(msg)//adjustl(buffer)
            do j = 2,size(a)
               write (buffer,buffer_format) aa(j)
               msg = trim(msg)//sep(:ls)//adjustl(buffer)
            end do
            msg = trim(msg)//sep(:ls)//trim(adjustl(buffer))
        type is (integer(int16))
            buffer_format = FMT_INT

            write (buffer,buffer_format) aa(1)
            msg = trim(msg)//adjustl(buffer)
            do j = 2,size(a)
               write (buffer,buffer_format) aa(j)
               msg = trim(msg)//sep(:ls)//adjustl(buffer)
            end do
            msg = trim(msg)//sep(:ls)//trim(adjustl(buffer))
        type is (integer(int32))
            buffer_format = FMT_INT

            write (buffer,buffer_format) aa(1)
            msg = trim(msg)//adjustl(buffer)
            do j = 2,size(a)
               write (buffer,buffer_format) aa(j)
               msg = trim(msg)//sep(:ls)//adjustl(buffer)
            end do
            msg = trim(msg)//sep(:ls)//trim(adjustl(buffer))
        type is (integer(int64))
            buffer_format = FMT_INT

            write (buffer,buffer_format) aa(1)
            msg = trim(msg)//adjustl(buffer)
            do j = 2,size(a)
               write (buffer,buffer_format) aa(j)
               msg = trim(msg)//sep(:ls)//adjustl(buffer)
            end do
            msg = trim(msg)//sep(:ls)//trim(adjustl(buffer))
        type is (complex(sp))
            write (buffer,FMT_REAL_sp)  aa(1)%re
            write (buffer2,FMT_REAL_sp) aa(1)%im
            msg = trim(msg)//'('//trim(adjustl(buffer))//','//trim(adjustl(buffer2))//')'
            do j = 2,size(a)
                write (buffer,FMT_REAL_sp)  aa(j)%re
                write (buffer2,FMT_REAL_sp) aa(j)%im
                msg = trim(msg)//sep(:ls)//'('//trim(adjustl(buffer))//','//trim(adjustl(buffer2))//')'
            end do
        type is (complex(dp))
            write (buffer,FMT_REAL_dp)  aa(1)%re
            write (buffer2,FMT_REAL_dp) aa(1)%im
            msg = trim(msg)//'('//trim(adjustl(buffer))//','//trim(adjustl(buffer2))//')'
            do j = 2,size(a)
                write (buffer,FMT_REAL_dp)  aa(j)%re
                write (buffer2,FMT_REAL_dp) aa(j)%im
                msg = trim(msg)//sep(:ls)//'('//trim(adjustl(buffer))//','//trim(adjustl(buffer2))//')'
            end do
        class default
            msg = trim(msg)//' <ERROR: INVALID TYPE>'

       end select

       ! Close bracket
       msg = trim(msg)//']'

    end subroutine appendv

    !> Error creation message, with location location
    pure type(state_type) function new_state(where_at,flag,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10, &
                                             a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) 

       !> Location
       character(len=*),intent(in) :: where_at

       !> Input error flag
       integer,intent(in) :: flag

       !> Optional rank-agnostic arguments
       class(*),optional,intent(in),dimension(..) :: a1,a2,a3,a4,a5,a6,a7,a8,a9,a10, &
                                                     a11,a12,a13,a14,a15,a16,a17,a18,a19,a20  

       ! Init object
       call new_state%parse(where_at,flag,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10, &
                            a11,a12,a13,a14,a15,a16,a17,a18,a19,a20)

    end function new_state

    !> Error creation message, from N input variables (numeric or strings)
    pure type(state_type) function new_state_nowhere(flag,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10, &
                                                     a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) &
                                                     result(new_state)

       !> Input error flag
       integer,intent(in) :: flag

       !> Optional rank-agnostic arguments
       class(*),optional,intent(in),dimension(..) :: a1,a2,a3,a4,a5,a6,a7,a8,a9,a10, &
                                                     a11,a12,a13,a14,a15,a16,a17,a18,a19,a20  

       ! Init object
       call new_state%parse(flag,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10, &
                                 a11,a12,a13,a14,a15,a16,a17,a18,a19,a20)

    end function new_state_nowhere
    
    !> Parse a generic list of arguments provided to the error constructor
    pure subroutine state_parse_at_location(new_state,where_at,flag,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10, &
                                                      a11,a12,a13,a14,a15,a16,a17,a18,a19,a20)
                                                    
       !> The current state variable 
       class(state_type), intent(inout) :: new_state 

       !> Error Location
       character(len=*),intent(in) :: where_at

       !> Input error flag
       integer,intent(in) :: flag

       !> Optional rank-agnostic arguments
       class(*),optional,intent(in),dimension(..) :: a1,a2,a3,a4,a5,a6,a7,a8,a9,a10, &
                                                     a11,a12,a13,a14,a15,a16,a17,a18,a19,a20  
                                                     
       ! Init object
       call new_state%destroy()

       !> Set error flag
       new_state%state = flag

       !> Set chain
       new_state%message = ""
       call appendr(new_state%message,a1)
       call appendr(new_state%message,a2)
       call appendr(new_state%message,a3)
       call appendr(new_state%message,a4)
       call appendr(new_state%message,a5)
       call appendr(new_state%message,a6)
       call appendr(new_state%message,a7)
       call appendr(new_state%message,a8)
       call appendr(new_state%message,a9)
       call appendr(new_state%message,a10)
       call appendr(new_state%message,a11)
       call appendr(new_state%message,a12)
       call appendr(new_state%message,a13)
       call appendr(new_state%message,a14)
       call appendr(new_state%message,a15)
       call appendr(new_state%message,a16)
       call appendr(new_state%message,a17)
       call appendr(new_state%message,a18)
       call appendr(new_state%message,a19)
       call appendr(new_state%message,a20)                
                                      
       !> Add location
       if (len_trim(where_at) > 0) new_state%where_at = adjustl(where_at)                                      
                                        
    end subroutine state_parse_at_location
    
    !> Parse a generic list of arguments provided to the error constructor
    pure subroutine state_parse_arguments(new_state,flag,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10, &
                                                    a11,a12,a13,a14,a15,a16,a17,a18,a19,a20)
                                                    
       !> The current state variable 
       class(state_type), intent(inout) :: new_state 

       !> Input error flag
       integer,intent(in) :: flag

       !> Optional rank-agnostic arguments
       class(*),optional,intent(in),dimension(..) :: a1,a2,a3,a4,a5,a6,a7,a8,a9,a10, &
                                                     a11,a12,a13,a14,a15,a16,a17,a18,a19,a20  
                                                     
       ! Init object
       call new_state%destroy()

       !> Set error flag
       new_state%state = flag

       !> Set chain
       new_state%message = ""
       call appendr(new_state%message,a1)
       call appendr(new_state%message,a2)
       call appendr(new_state%message,a3)
       call appendr(new_state%message,a4)
       call appendr(new_state%message,a5)
       call appendr(new_state%message,a6)
       call appendr(new_state%message,a7)
       call appendr(new_state%message,a8)
       call appendr(new_state%message,a9)
       call appendr(new_state%message,a10)
       call appendr(new_state%message,a11)
       call appendr(new_state%message,a12)
       call appendr(new_state%message,a13)
       call appendr(new_state%message,a14)
       call appendr(new_state%message,a15)
       call appendr(new_state%message,a16)
       call appendr(new_state%message,a17)
       call appendr(new_state%message,a18)
       call appendr(new_state%message,a19)
       call appendr(new_state%message,a20)                
                                        
    end subroutine state_parse_arguments

end module stdlib_error
