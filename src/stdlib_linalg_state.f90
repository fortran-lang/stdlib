module stdlib_linalg_state
     !! Version: experimental
     !!
     !! Provides a state/error handling derived type for advanced error handling of
     !! BLAS/LAPACK based linear algebra procedures. All procedures are pure.
     !! ([Specification](../page/specs/stdlib_linalg.html))
     use stdlib_linalg_constants,only: ilp
     use stdlib_kinds, only: int8, int16, int32, int64, sp, dp, xdp, qp, lk
     use stdlib_error, only: state_type, operator(==), operator(/=), operator(<), operator(>), &
         operator(<=), operator(>=), STDLIB_SUCCESS, STDLIB_VALUE_ERROR, STDLIB_LINALG_ERROR, STDLIB_INTERNAL_ERROR
     use stdlib_io, only: FMT_REAL_SP, FMT_REAL_DP, FMT_REAL_QP, FMT_COMPLEX_SP, FMT_COMPLEX_DP, &
         FMT_COMPLEX_QP, FMT_REAL_XDP, FMT_COMPLEX_XDP
     implicit none
     private

     !> Version: experimental
     !>
     !> A fixed-storage state variable for error handling of linear algebra routines
     public :: linalg_state_type

     !> Version: experimental
     !>
     !> Error state handling: if the user requested the error state variable on
     !> output, just return it to the user. Otherwise, halt the program on error.
     public :: linalg_error_handling

     !> Version: experimental
     !>
     !> Interfaces for comparison operators of error states with integer flags
     public :: operator(==),operator(/=)
     public :: operator(<),operator(<=)
     public :: operator(>),operator(>=)

     !> State return types for linear algebra
     integer(ilp),parameter,public :: LINALG_SUCCESS        = STDLIB_SUCCESS
     integer(ilp),parameter,public :: LINALG_VALUE_ERROR    = STDLIB_VALUE_ERROR
     integer(ilp),parameter,public :: LINALG_ERROR          = STDLIB_LINALG_ERROR
     integer(ilp),parameter,public :: LINALG_INTERNAL_ERROR = STDLIB_INTERNAL_ERROR

     !> `linalg_state_type` defines a state return type for a
     !> linear algebra routine. State contains a status flag, a comment, and a
     !> procedure specifier that can be used to mark where the error happened
     type, extends(state_type) :: linalg_state_type
         contains
         
         !> Print error message
         procedure :: print_msg => state_message
         
     end type linalg_state_type

    interface linalg_state_type
        module procedure new_state
        module procedure new_state_nowhere
    end interface linalg_state_type

     contains

     !> Interface to print linalg state flags
     pure function linalg_message(flag) result(msg)
        integer(ilp),intent(in) :: flag
        character(len=:),allocatable :: msg

        select case (flag)
           case (LINALG_SUCCESS);        msg = 'Success!'
           case (LINALG_VALUE_ERROR);    msg = 'Value Error'
           case (LINALG_ERROR);          msg = 'Algebra Error'
           case (LINALG_INTERNAL_ERROR); msg = 'Internal Error'
           case default;                 msg = 'ERROR/INVALID FLAG'
        end select

     end function linalg_message

     !> Flow control: on output flag present, return it; otherwise, halt on error
     pure subroutine linalg_error_handling(ierr,ierr_out)
         type(linalg_state_type),intent(in) :: ierr
         type(linalg_state_type),optional,intent(out) :: ierr_out

         character(len=:),allocatable :: err_msg

         if (present(ierr_out)) then
             ! Return error flag
             ierr_out = ierr
         elseif (ierr%error()) then
             err_msg = ierr%print()
             error stop err_msg
         end if

     end subroutine linalg_error_handling

     !> Formatted message
     pure function state_message(this) result(msg)
         class(linalg_state_type),intent(in) :: this
         character(len=:),allocatable :: msg

         if (this%state == LINALG_SUCCESS) then
            msg = 'Success!'
         else
            msg = linalg_message(this%state)//': '//trim(this%message)
         end if

     end function state_message

    !> Error creation message, with location location
    pure type(linalg_state_type) function new_state(where_at,flag,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10, &
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
    pure type(linalg_state_type) function new_state_nowhere(flag,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10, &
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


end module stdlib_linalg_state
