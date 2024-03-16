module stdlib_linalg_state
     use stdlib_linalg_constants,only:ilp,lk
     use iso_fortran_env,only:real32,real64,real128,int8,int16,int32,int64,stderr => error_unit
     implicit none(type,external)
     private

     !> Public interfaces
     public :: linalg_state
     public :: linalg_error_handling
     public :: operator(==),operator(/=)
     public :: operator(<),operator(<=)
     public :: operator(>),operator(>=)

     !> State return types
     integer(ilp),parameter,public :: LINALG_SUCCESS = 0_ilp
     integer(ilp),parameter,public :: LINALG_VALUE_ERROR = -1_ilp
     integer(ilp),parameter,public :: LINALG_ERROR = -2_ilp
     integer(ilp),parameter,public :: LINALG_INTERNAL_ERROR = -3_ilp

     !> Use fixed-size character storage for performance
     integer(ilp),parameter :: MSG_LENGTH = 512_ilp
     integer(ilp),parameter :: NAME_LENGTH = 32_ilp

     !> `linalg_state` defines a state return type for a
     !> linear algebra routine. State contains a status flag, a comment, and a
     !> procedure specifier that can be used to mark where the error happened
     type,public :: linalg_state

         !> The current exit state
         integer(ilp) :: state = LINALG_SUCCESS

         !> Message associated to the current state
         character(len=MSG_LENGTH) :: message = repeat(' ',MSG_LENGTH)

         !> Location of the state change
         character(len=NAME_LENGTH) :: where_at = repeat(' ',NAME_LENGTH)

         contains

            !> Cleanup
            procedure :: destroy => state_destroy

            !> Print error message
            procedure :: print => state_print
            procedure :: print_msg => state_message

            !> State properties
            procedure :: ok => state_is_ok
            procedure :: error => state_is_error

     end type linalg_state

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

     interface linalg_state
         module procedure new_state
         module procedure new_state_nowhere
     end interface linalg_state

     contains

     !> Interface to print linalg state flags
     pure function LINALG_MESSAGE(flag) result(msg)
        integer(ilp),intent(in) :: flag
        character(len=:),allocatable :: msg

        select case (flag)
           case (LINALG_SUCCESS);        msg = 'Success!'
           case (LINALG_VALUE_ERROR);    msg = 'Value Error'
           case (LINALG_ERROR);          msg = 'Algebra Error'
           case (LINALG_INTERNAL_ERROR); msg = 'Internal Error'
           case default;                 msg = 'ERROR/INVALID FLAG'
        end select

     end function LINALG_MESSAGE

     !> Flow control: on output flag present, return it; otherwise, halt on error
     pure subroutine linalg_error_handling(ierr,ierr_out)
         type(linalg_state),intent(in) :: ierr
         type(linalg_state),optional,intent(out) :: ierr_out

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
         class(linalg_state),intent(in) :: this
         character(len=:),allocatable :: msg

         if (this%state == LINALG_SUCCESS) then
            msg = 'Success!'
         else
            msg = LINALG_MESSAGE(this%state)//': '//trim(this%message)
         end if

     end function state_message

     !> Produce a nice error string
     pure function state_print(this) result(msg)
         class(linalg_state),intent(in) :: this
         character(len=:),allocatable :: msg

         if (len_trim(this%where_at) > 0) then
            msg = '['//trim(this%where_at)//'] returned '//state_message(this)
         elseif (this%error()) then
            msg = 'Error encountered: '//state_message(this)
         else
            msg = state_message(this)
         end if

     end function state_print

     !> Cleanup object
     elemental subroutine state_destroy(this)
        class(linalg_state),intent(inout) :: this

        this%state = LINALG_SUCCESS
        this%message = repeat(' ',len(this%message))
        this%where_at = repeat(' ',len(this%where_at))

     end subroutine state_destroy

     !> Check if the current state is successful
     elemental logical(lk) function state_is_ok(this)
        class(linalg_state),intent(in) :: this
        state_is_ok = this%state == LINALG_SUCCESS
     end function state_is_ok

     !> Check if the current state is an error state
     elemental logical(lk) function state_is_error(this)
        class(linalg_state),intent(in) :: this
        state_is_error = this%state /= LINALG_SUCCESS
     end function state_is_error

     !> Compare an error flag with an integer
     elemental logical(lk) function state_eq_flag(err,flag)
        type(linalg_state),intent(in) :: err
        integer,intent(in) :: flag
        state_eq_flag = err%state == flag
     end function state_eq_flag
     elemental logical(lk) function flag_eq_state(flag,err)
        integer,intent(in) :: flag
        type(linalg_state),intent(in) :: err
        flag_eq_state = err%state == flag
     end function flag_eq_state
     elemental logical(lk) function state_neq_flag(err,flag)
        type(linalg_state),intent(in) :: err
        integer,intent(in) :: flag
        state_neq_flag = .not. state_eq_flag(err,flag)
     end function state_neq_flag
     elemental logical(lk) function flag_neq_state(flag,err)
        integer,intent(in) :: flag
        type(linalg_state),intent(in) :: err
        flag_neq_state = .not. state_eq_flag(err,flag)
     end function flag_neq_state
     elemental logical(lk) function state_lt_flag(err,flag)
        type(linalg_state),intent(in) :: err
        integer,intent(in) :: flag
        state_lt_flag = err%state < flag
     end function state_lt_flag
     elemental logical(lk) function state_le_flag(err,flag)
        type(linalg_state),intent(in) :: err
        integer,intent(in) :: flag
        state_le_flag = err%state <= flag
     end function state_le_flag
     elemental logical(lk) function flag_lt_state(flag,err)
        integer,intent(in) :: flag
        type(linalg_state),intent(in) :: err
        flag_lt_state = err%state < flag
     end function flag_lt_state
     elemental logical(lk) function flag_le_state(flag,err)
        integer,intent(in) :: flag
        type(linalg_state),intent(in) :: err
        flag_le_state = err%state <= flag
     end function flag_le_state
     elemental logical(lk) function state_gt_flag(err,flag)
        type(linalg_state),intent(in) :: err
        integer,intent(in) :: flag
        state_gt_flag = err%state > flag
     end function state_gt_flag
     elemental logical(lk) function state_ge_flag(err,flag)
        type(linalg_state),intent(in) :: err
        integer,intent(in) :: flag
        state_ge_flag = err%state >= flag
     end function state_ge_flag
     elemental logical(lk) function flag_gt_state(flag,err)
        integer,intent(in) :: flag
        type(linalg_state),intent(in) :: err
        flag_gt_state = err%state > flag
     end function flag_gt_state
     elemental logical(lk) function flag_ge_state(flag,err)
        integer,intent(in) :: flag
        type(linalg_state),intent(in) :: err
        flag_ge_state = err%state >= flag
     end function flag_ge_state

    !> Error creation message, with location location
    pure type(linalg_state) function new_state(where_at,flag,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10, &
                                               v1,v2,v3,v4,v5)

       !> Location
       character(len=*),intent(in) :: where_at

       !> Input error flag
       integer,intent(in) :: flag

       !> Optional scalar arguments
       class(*),optional,intent(in) :: a1,a2,a3,a4,a5,a6,a7,a8,a9,a10

       !> Optional vector arguments
       class(*),optional,intent(in),dimension(:) :: v1,v2,v3,v4,v5

       !> Create state with no message
       new_state = new_state_nowhere(flag,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,v1,v2,v3,v4,v5)

       !> Add location
       if (len_trim(where_at) > 0) new_state%where_at = adjustl(where_at)

    end function new_state

    !> Error creation message, from N input variables (numeric or strings)
    pure type(linalg_state) function new_state_nowhere(flag,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10, &
                                               v1,v2,v3,v4,v5) result(new_state)

       !> Input error flag
       integer,intent(in) :: flag

       !> Optional scalar arguments
       class(*),optional,intent(in) :: a1,a2,a3,a4,a5,a6,a7,a8,a9,a10

       !> Optional vector arguments
       class(*),optional,intent(in),dimension(:) :: v1,v2,v3,v4,v5

       ! Init object
       call new_state%destroy()

       !> Set error flag
       new_state%state = flag

       !> Set chain
       new_state%message = ""
       call append(new_state%message,a1)
       call append(new_state%message,a2)
       call append(new_state%message,a3)
       call append(new_state%message,a4)
       call append(new_state%message,a5)
       call append(new_state%message,a6)
       call append(new_state%message,a7)
       call append(new_state%message,a8)
       call append(new_state%message,a9)
       call append(new_state%message,a10)
       call appendv(new_state%message,v1)
       call appendv(new_state%message,v2)
       call appendv(new_state%message,v3)
       call appendv(new_state%message,v4)
       call appendv(new_state%message,v5)

    end function new_state_nowhere

    ! Append a generic value to the error flag
    pure subroutine append(msg,a,prefix)
       class(*),optional,intent(in) :: a
       character(len=*),intent(inout) :: msg
       character,optional,intent(in) :: prefix

       character(len=MSG_LENGTH) :: buffer,buffer2
       character(len=2) :: sep
       integer :: ls

       if (.not. present(a)) return

       ! Do not add separator if this is the first instance
       sep = '  '
       ls = merge(1,0,len_trim(msg) > 0)

       if (present(prefix)) then
           ls = ls + 1
           sep(ls:ls) = prefix
       end if

       select type (aa => a)

        type is (character(len=*))

            msg = trim(msg)//sep(:ls)//aa

        type is (integer(int8))

            write (buffer,'(i0)') aa
            msg = trim(msg)//sep(:ls)//trim(adjustl(buffer))

        type is (integer(int16))

            write (buffer,'(i0)') aa
            msg = trim(msg)//sep(:ls)//trim(adjustl(buffer))

        type is (integer(int32))

            write (buffer,'(i0)') aa
            msg = trim(msg)//sep(:ls)//trim(adjustl(buffer))

        type is (integer(int64))

            write (buffer,'(i0)') aa
            msg = trim(msg)//sep(:ls)//trim(adjustl(buffer))

        type is (real(real32))

            write (buffer,'(es15.8e2)') aa
            msg = trim(msg)//sep(:ls)//trim(adjustl(buffer))

        type is (real(real64))

            write (buffer,'(es24.16e3)') aa
            msg = trim(msg)//sep(:ls)//trim(adjustl(buffer))

        type is (real(real128))

            write (buffer,'(es44.35e4)') aa
            msg = trim(msg)//sep(:ls)//trim(adjustl(buffer))

        type is (complex(real32))

            write (buffer,'(es15.8e2)') aa%re
            write (buffer2,'(es15.8e2)') aa%im
            msg = trim(msg)//sep(:ls)//'('//trim(adjustl(buffer))//','//trim(adjustl(buffer2))//')'

        type is (complex(real64))

            write (buffer,'(es24.16e3)') aa%re
            write (buffer2,'(es24.16e3)') aa%im
            msg = trim(msg)//sep(:ls)//'('//trim(adjustl(buffer))//','//trim(adjustl(buffer2))//')'

        type is (complex(real128))

            write (buffer,'(es44.35e4)') aa%re
            write (buffer2,'(es44.35e4)') aa%im
            msg = trim(msg)//sep(:ls)//'('//trim(adjustl(buffer))//','//trim(adjustl(buffer2))//')'

        class default

            msg = trim(msg)//' <ERROR: INVALID TYPE>'

       end select

    end subroutine append

    ! Append a generic vector to the error flag
    pure subroutine appendv(msg,a)
       class(*),optional,intent(in) :: a(:)
       character(len=*),intent(inout) :: msg

       integer :: j,ls
       character(len=MSG_LENGTH) :: buffer,buffer2
       character(len=2) :: sep

       if (.not. present(a)) return
       if (size(a) <= 0) return

       ! Default: separate elements with one space
       sep = '  '
       ls = 1

       ! Open bracket
       msg = trim(msg)//' ['

       ! Do not call append(msg(aa(j))), it will crash gfortran
       select type (aa => a)

        type is (character(len=*))

            msg = trim(msg)//adjustl(aa(1))
            do j = 2,size(a)
               msg = trim(msg)//sep(:ls)//adjustl(aa(j))
            end do

        type is (integer(int8))

            write (buffer,'(i0)') aa(1)
            msg = trim(msg)//adjustl(buffer)
            do j = 2,size(a)
               write (buffer,'(i0)') aa(j)
               msg = trim(msg)//sep(:ls)//adjustl(buffer)
            end do

        type is (integer(int16))

            write (buffer,'(i0)') aa(1)
            msg = trim(msg)//adjustl(buffer)
            do j = 2,size(a)
               write (buffer,'(i0)') aa(j)
               msg = trim(msg)//sep(:ls)//adjustl(buffer)
            end do

        type is (integer(int32))

            write (buffer,'(i0)') aa(1)
            msg = trim(msg)//adjustl(buffer)
            do j = 2,size(a)
               write (buffer,'(i0)') aa(j)
               msg = trim(msg)//sep(:ls)//adjustl(buffer)
            end do

        type is (integer(int64))

            write (buffer,'(i0)') aa(1)
            msg = trim(msg)//adjustl(buffer)
            do j = 2,size(a)
               write (buffer,'(i0)') aa(j)
               msg = trim(msg)//sep(:ls)//adjustl(buffer)
            end do

        type is (real(real32))

            write (buffer,'(es15.8e2)') aa(1)
            msg = trim(msg)//adjustl(buffer)
            do j = 2,size(a)
               write (buffer,'(es15.8e2)') aa(j)
               msg = trim(msg)//sep(:ls)//adjustl(buffer)
            end do

        type is (real(real64))

            write (buffer,'(es24.16e3)') aa(1)
            msg = trim(msg)//adjustl(buffer)
            do j = 2,size(a)
               write (buffer,'(es24.16e3)') aa(j)
               msg = trim(msg)//sep(:ls)//adjustl(buffer)
            end do

        type is (real(real128))

            write (buffer,'(es44.35e4)') aa(1)
            msg = trim(msg)//adjustl(buffer)
            do j = 2,size(a)
               write (buffer,'(es44.35e4)') aa(j)
               msg = trim(msg)//sep(:ls)//adjustl(buffer)
            end do

        type is (complex(real32))

            write (buffer,'(es15.8e2)') aa(1)%re
            write (buffer2,'(es15.8e2)') aa(1)%im
            msg = trim(msg)//'('//trim(adjustl(buffer))//','//trim(adjustl(buffer2))//')'
            do j = 2,size(a)
                write (buffer,'(es15.8e2)') aa(j)%re
                write (buffer2,'(es15.8e2)') aa(j)%im
                msg = trim(msg)//sep(:ls)//'('//trim(adjustl(buffer))//','//trim(adjustl(buffer2))//')'
            end do

        type is (complex(real64))

            write (buffer,'(es24.16e3)') aa(1)%re
            write (buffer2,'(es24.16e3)') aa(1)%im
            msg = trim(msg)//'('//trim(adjustl(buffer))//','//trim(adjustl(buffer2))//')'
            do j = 2,size(a)
                write (buffer,'(es24.16e3)') aa(j)%re
                write (buffer2,'(es24.16e3)') aa(j)%im
                msg = trim(msg)//sep(:ls)//'('//trim(adjustl(buffer))//','//trim(adjustl(buffer2))//')'
            end do

        type is (complex(real128))

            write (buffer,'(es44.35e4)') aa(1)%re
            write (buffer2,'(es44.35e4)') aa(1)%im
            msg = trim(msg)//'('//trim(adjustl(buffer))//','//trim(adjustl(buffer2))//')'
            do j = 2,size(a)
                write (buffer,'(es44.35e4)') aa(j)%re
                write (buffer2,'(es44.35e4)') aa(j)%im
                msg = trim(msg)//sep(:ls)//'('//trim(adjustl(buffer))//','//trim(adjustl(buffer2))//')'
            end do

        class default

            msg = trim(msg)//' <ERROR: INVALID TYPE>'

       end select

       ! Close bracket
       msg = trim(msg)//']'

    end subroutine appendv

end module stdlib_linalg_state
