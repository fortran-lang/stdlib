! SPDX-Identifier: MIT


!> Implementation for saving multidimensional arrays to Matrix Market files
submodule (stdlib_io_mm) stdlib_io_mm_save
    use stdlib_error, only : error_stop
    use stdlib_strings, only : to_string
    use stdlib_io, only : open
    use stdlib_ascii, only : to_lower
    implicit none

    ! Matrix Market format constants
    character(len=*), parameter :: MM_BANNER = "%%MatrixMarket"
    character(len=*), parameter :: MM_COMMENT_CHAR = "%"

    ! Matrix Market object types
    character(len=*), parameter :: &
        MM_MATRIX = "matrix", &
        MM_VECTOR = "vector"

    ! Matrix Market format types
    character(len=*), parameter :: &
        MM_COORDINATE = "coordinate", &
        MM_ARRAY = "array"

    ! Matrix Market data types
    character(len=*), parameter :: &
        MM_REAL = "real", &
        MM_COMPLEX = "complex", &
        MM_INTEGER = "integer", &
        MM_PATTERN = "pattern"

    ! Matrix Market storage schemes
    character(len=*), parameter :: &
        MM_GENERAL = "general", &
        MM_SYMMETRIC = "symmetric", &
        MM_SKEW_SYMMETRIC = "skew-symmetric", &
        MM_HERMITIAN = "hermitian", &
        MM_AUTO = "auto"

contains

    module subroutine save_mm_dense_sp(filename, matrix, comment, format, symmetry, iostat, iomsg)
        !> Name of the Matrix Market file to save to
        character(len=*), intent(in) :: filename
        !> Matrix to be saved to the Matrix Market file
        real(sp), intent(in) :: matrix(:,:)
        !> Optional comment information
        character(len=*), intent(in), optional :: comment
        !> Format in which matrix data needs to be stored
        character(len=*), intent(in), optional :: format
        !> Symmetry type of the matrix (general, symmetric, skew-symmetric, hermitian)
        character(len=*), intent(in), optional :: symmetry
        !> Error status of saving, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        integer :: io, stat, i, j, nnz, nrows, ncols
        character(len=:), allocatable :: msg
        character(len=:), allocatable :: field_type
        character(len=:), allocatable :: fmt_
        character(len=:), allocatable :: symmetry_
        if(present(iostat)) iostat = 0
        if(present(iomsg)) iomsg = ''
        stat = 0

            fmt_ = "ES24.16E3"
        if(present(format)) fmt_ = format

        fmt_ = '(' // fmt_ //')'

        io = open(filename, "w", iostat=stat)
        if (stat /= 0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Could not create file: " // filename)
            return
        end if

        nrows = size(matrix, 1)
        ncols = size(matrix, 2)
        ! Determine symmetry type
        symmetry_ = MM_GENERAL
        if (present(symmetry)) then
            symmetry_ = to_lower(trim(symmetry))
            if (symmetry_ == MM_AUTO) then

                symmetry_block: block
                    ! Non-square matrices cannot be symmetric/skew/hermitian
                    if (nrows /= ncols) then
                        symmetry_ = MM_GENERAL
                        exit symmetry_block
                    end if

                    ! Try symmetric
                    symmetry_ = MM_SYMMETRIC
                    do j = 1, ncols
                        do i = j+1, nrows
                            if (matrix(i,j) /= matrix(j,i)) then
                                symmetry_ = MM_GENERAL
                                exit
                            end if
                        end do
                        if (symmetry_ == MM_GENERAL) exit
                    end do
                    if (symmetry_ == MM_SYMMETRIC) exit symmetry_block

                    ! Try skew-symmetric
                    symmetry_ = MM_SKEW_SYMMETRIC
                    do j = 1, ncols
                        do i = j, nrows
                            if (matrix(i,j) /= -matrix(j,i)) then
                                symmetry_ = MM_GENERAL
                                exit
                            end if
                        end do
                        if (symmetry_ == MM_GENERAL) exit
                    end do
                    if (symmetry_ == MM_SKEW_SYMMETRIC) exit symmetry_block


                    symmetry_ = MM_GENERAL
                end block symmetry_block
            end if
        end if

        ! Determine field type based on matrix type
        field_type = MM_REAL

        catch: block
            ! Write header
            call write_mm_header(io, MM_ARRAY, field_type, symmetry_, &
                                nrows, ncols, comment=comment, iostat=stat, iomsg=msg)
            if (stat /= 0) exit catch

            ! Write array format (column-major order)
            if(symmetry_ == MM_GENERAL) then
                do j = 1, ncols
                    do i = 1, nrows
                        write(io, fmt=fmt_, iostat=stat) matrix(i, j)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // &
                                to_string(i) // "," // to_string(j) // ")"
                            exit catch
                        end if
                    end do
                end do
            else
                ! For symmetric and hermitian matrices, only the lower triangle
                ! (including the diagonal) is written.
                ! For skew-symmetric matrices, only the strictly lower triangle is written
                ! (the diagonal is omitted and assumed zero).
                do j = 1, ncols
                    do i = j, nrows
                        if(symmetry_ == MM_SKEW_SYMMETRIC .and. i == j) cycle
                        write(io, fmt=fmt_, iostat=stat) matrix(i, j)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // &
                                to_string(i) // "," // to_string(j) // ")"
                            exit catch
                        end if
                    end do
                end do
            end if
        end block catch

        close(io)
        if(stat/=0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Failed to save Matrix Market file '" // filename // "': " // msg)
            return
        end if

        if (present(iomsg) .and. allocated(msg)) call move_alloc(msg, iomsg)
    end subroutine
    module subroutine save_mm_dense_dp(filename, matrix, comment, format, symmetry, iostat, iomsg)
        !> Name of the Matrix Market file to save to
        character(len=*), intent(in) :: filename
        !> Matrix to be saved to the Matrix Market file
        real(dp), intent(in) :: matrix(:,:)
        !> Optional comment information
        character(len=*), intent(in), optional :: comment
        !> Format in which matrix data needs to be stored
        character(len=*), intent(in), optional :: format
        !> Symmetry type of the matrix (general, symmetric, skew-symmetric, hermitian)
        character(len=*), intent(in), optional :: symmetry
        !> Error status of saving, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        integer :: io, stat, i, j, nnz, nrows, ncols
        character(len=:), allocatable :: msg
        character(len=:), allocatable :: field_type
        character(len=:), allocatable :: fmt_
        character(len=:), allocatable :: symmetry_
        if(present(iostat)) iostat = 0
        if(present(iomsg)) iomsg = ''
        stat = 0

            fmt_ = "ES24.16E3"
        if(present(format)) fmt_ = format

        fmt_ = '(' // fmt_ //')'

        io = open(filename, "w", iostat=stat)
        if (stat /= 0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Could not create file: " // filename)
            return
        end if

        nrows = size(matrix, 1)
        ncols = size(matrix, 2)
        ! Determine symmetry type
        symmetry_ = MM_GENERAL
        if (present(symmetry)) then
            symmetry_ = to_lower(trim(symmetry))
            if (symmetry_ == MM_AUTO) then

                symmetry_block: block
                    ! Non-square matrices cannot be symmetric/skew/hermitian
                    if (nrows /= ncols) then
                        symmetry_ = MM_GENERAL
                        exit symmetry_block
                    end if

                    ! Try symmetric
                    symmetry_ = MM_SYMMETRIC
                    do j = 1, ncols
                        do i = j+1, nrows
                            if (matrix(i,j) /= matrix(j,i)) then
                                symmetry_ = MM_GENERAL
                                exit
                            end if
                        end do
                        if (symmetry_ == MM_GENERAL) exit
                    end do
                    if (symmetry_ == MM_SYMMETRIC) exit symmetry_block

                    ! Try skew-symmetric
                    symmetry_ = MM_SKEW_SYMMETRIC
                    do j = 1, ncols
                        do i = j, nrows
                            if (matrix(i,j) /= -matrix(j,i)) then
                                symmetry_ = MM_GENERAL
                                exit
                            end if
                        end do
                        if (symmetry_ == MM_GENERAL) exit
                    end do
                    if (symmetry_ == MM_SKEW_SYMMETRIC) exit symmetry_block


                    symmetry_ = MM_GENERAL
                end block symmetry_block
            end if
        end if

        ! Determine field type based on matrix type
        field_type = MM_REAL

        catch: block
            ! Write header
            call write_mm_header(io, MM_ARRAY, field_type, symmetry_, &
                                nrows, ncols, comment=comment, iostat=stat, iomsg=msg)
            if (stat /= 0) exit catch

            ! Write array format (column-major order)
            if(symmetry_ == MM_GENERAL) then
                do j = 1, ncols
                    do i = 1, nrows
                        write(io, fmt=fmt_, iostat=stat) matrix(i, j)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // &
                                to_string(i) // "," // to_string(j) // ")"
                            exit catch
                        end if
                    end do
                end do
            else
                ! For symmetric and hermitian matrices, only the lower triangle
                ! (including the diagonal) is written.
                ! For skew-symmetric matrices, only the strictly lower triangle is written
                ! (the diagonal is omitted and assumed zero).
                do j = 1, ncols
                    do i = j, nrows
                        if(symmetry_ == MM_SKEW_SYMMETRIC .and. i == j) cycle
                        write(io, fmt=fmt_, iostat=stat) matrix(i, j)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // &
                                to_string(i) // "," // to_string(j) // ")"
                            exit catch
                        end if
                    end do
                end do
            end if
        end block catch

        close(io)
        if(stat/=0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Failed to save Matrix Market file '" // filename // "': " // msg)
            return
        end if

        if (present(iomsg) .and. allocated(msg)) call move_alloc(msg, iomsg)
    end subroutine
    module subroutine save_mm_dense_csp(filename, matrix, comment, format, symmetry, iostat, iomsg)
        !> Name of the Matrix Market file to save to
        character(len=*), intent(in) :: filename
        !> Matrix to be saved to the Matrix Market file
        complex(sp), intent(in) :: matrix(:,:)
        !> Optional comment information
        character(len=*), intent(in), optional :: comment
        !> Format in which matrix data needs to be stored
        character(len=*), intent(in), optional :: format
        !> Symmetry type of the matrix (general, symmetric, skew-symmetric, hermitian)
        character(len=*), intent(in), optional :: symmetry
        !> Error status of saving, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        integer :: io, stat, i, j, nnz, nrows, ncols
        character(len=:), allocatable :: msg
        character(len=:), allocatable :: field_type
        character(len=:), allocatable :: fmt_
        character(len=:), allocatable :: symmetry_
        real(sp) :: real_part, imag_part
        if(present(iostat)) iostat = 0
        if(present(iomsg)) iomsg = ''
        stat = 0

            fmt_ = "ES24.16E3"
        if(present(format)) fmt_ = format

        fmt_ = '(' // fmt_ //',1X,'// fmt_ //')'

        io = open(filename, "w", iostat=stat)
        if (stat /= 0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Could not create file: " // filename)
            return
        end if

        nrows = size(matrix, 1)
        ncols = size(matrix, 2)
        ! Determine symmetry type
        symmetry_ = MM_GENERAL
        if (present(symmetry)) then
            symmetry_ = to_lower(trim(symmetry))
            if (symmetry_ == MM_AUTO) then

                symmetry_block: block
                    ! Non-square matrices cannot be symmetric/skew/hermitian
                    if (nrows /= ncols) then
                        symmetry_ = MM_GENERAL
                        exit symmetry_block
                    end if

                    ! Try symmetric
                    symmetry_ = MM_SYMMETRIC
                    do j = 1, ncols
                        do i = j+1, nrows
                            if (matrix(i,j) /= matrix(j,i)) then
                                symmetry_ = MM_GENERAL
                                exit
                            end if
                        end do
                        if (symmetry_ == MM_GENERAL) exit
                    end do
                    if (symmetry_ == MM_SYMMETRIC) exit symmetry_block

                    ! Try skew-symmetric
                    symmetry_ = MM_SKEW_SYMMETRIC
                    do j = 1, ncols
                        do i = j, nrows
                            if (matrix(i,j) /= -matrix(j,i)) then
                                symmetry_ = MM_GENERAL
                                exit
                            end if
                        end do
                        if (symmetry_ == MM_GENERAL) exit
                    end do
                    if (symmetry_ == MM_SKEW_SYMMETRIC) exit symmetry_block

                    ! Try hermitian
                    symmetry_ = MM_HERMITIAN
                    do j = 1, ncols
                        do i = j, nrows
                            if (matrix(i,j) /= conjg(matrix(j,i))) then
                                symmetry_ = MM_GENERAL
                                exit
                            end if
                        end do
                        if (symmetry_ == MM_GENERAL) exit
                    end do
                    if (symmetry_ == MM_HERMITIAN) exit symmetry_block

                    symmetry_ = MM_GENERAL
                end block symmetry_block
            end if
        end if

        ! Determine field type based on matrix type
        field_type = MM_COMPLEX

        catch: block
            ! Write header
            call write_mm_header(io, MM_ARRAY, field_type, symmetry_, &
                                nrows, ncols, comment=comment, iostat=stat, iomsg=msg)
            if (stat /= 0) exit catch

            ! Write array format (column-major order)
            if(symmetry_ == MM_GENERAL) then
                do j = 1, ncols
                    do i = 1, nrows
                        real_part = real(matrix(i, j), kind=sp)
                        imag_part = aimag(matrix(i, j))
                        write(io, fmt=fmt_, iostat=stat) real_part, imag_part
                        if (stat /= 0) then
                            msg = "Error writing array element (" // &
                                to_string(i) // "," // to_string(j) // ")"
                            exit catch
                        end if
                    end do
                end do
            else
                ! For symmetric and hermitian matrices, only the lower triangle
                ! (including the diagonal) is written.
                ! For skew-symmetric matrices, only the strictly lower triangle is written
                ! (the diagonal is omitted and assumed zero).
                do j = 1, ncols
                    do i = j, nrows
                        if(symmetry_ == MM_SKEW_SYMMETRIC .and. i == j) cycle
                        real_part = real(matrix(i, j), kind=sp)
                        imag_part = aimag(matrix(i, j))
                        if(i==j .and. symmetry_ == MM_HERMITIAN) imag_part = 0
                        write(io, fmt=fmt_, iostat=stat) real_part, imag_part
                        if (stat /= 0) then
                            msg = "Error writing array element (" // &
                                to_string(i) // "," // to_string(j) // ")"
                            exit catch
                        end if
                    end do
                end do
            end if
        end block catch

        close(io)
        if(stat/=0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Failed to save Matrix Market file '" // filename // "': " // msg)
            return
        end if

        if (present(iomsg) .and. allocated(msg)) call move_alloc(msg, iomsg)
    end subroutine
    module subroutine save_mm_dense_cdp(filename, matrix, comment, format, symmetry, iostat, iomsg)
        !> Name of the Matrix Market file to save to
        character(len=*), intent(in) :: filename
        !> Matrix to be saved to the Matrix Market file
        complex(dp), intent(in) :: matrix(:,:)
        !> Optional comment information
        character(len=*), intent(in), optional :: comment
        !> Format in which matrix data needs to be stored
        character(len=*), intent(in), optional :: format
        !> Symmetry type of the matrix (general, symmetric, skew-symmetric, hermitian)
        character(len=*), intent(in), optional :: symmetry
        !> Error status of saving, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        integer :: io, stat, i, j, nnz, nrows, ncols
        character(len=:), allocatable :: msg
        character(len=:), allocatable :: field_type
        character(len=:), allocatable :: fmt_
        character(len=:), allocatable :: symmetry_
        real(dp) :: real_part, imag_part
        if(present(iostat)) iostat = 0
        if(present(iomsg)) iomsg = ''
        stat = 0

            fmt_ = "ES24.16E3"
        if(present(format)) fmt_ = format

        fmt_ = '(' // fmt_ //',1X,'// fmt_ //')'

        io = open(filename, "w", iostat=stat)
        if (stat /= 0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Could not create file: " // filename)
            return
        end if

        nrows = size(matrix, 1)
        ncols = size(matrix, 2)
        ! Determine symmetry type
        symmetry_ = MM_GENERAL
        if (present(symmetry)) then
            symmetry_ = to_lower(trim(symmetry))
            if (symmetry_ == MM_AUTO) then

                symmetry_block: block
                    ! Non-square matrices cannot be symmetric/skew/hermitian
                    if (nrows /= ncols) then
                        symmetry_ = MM_GENERAL
                        exit symmetry_block
                    end if

                    ! Try symmetric
                    symmetry_ = MM_SYMMETRIC
                    do j = 1, ncols
                        do i = j+1, nrows
                            if (matrix(i,j) /= matrix(j,i)) then
                                symmetry_ = MM_GENERAL
                                exit
                            end if
                        end do
                        if (symmetry_ == MM_GENERAL) exit
                    end do
                    if (symmetry_ == MM_SYMMETRIC) exit symmetry_block

                    ! Try skew-symmetric
                    symmetry_ = MM_SKEW_SYMMETRIC
                    do j = 1, ncols
                        do i = j, nrows
                            if (matrix(i,j) /= -matrix(j,i)) then
                                symmetry_ = MM_GENERAL
                                exit
                            end if
                        end do
                        if (symmetry_ == MM_GENERAL) exit
                    end do
                    if (symmetry_ == MM_SKEW_SYMMETRIC) exit symmetry_block

                    ! Try hermitian
                    symmetry_ = MM_HERMITIAN
                    do j = 1, ncols
                        do i = j, nrows
                            if (matrix(i,j) /= conjg(matrix(j,i))) then
                                symmetry_ = MM_GENERAL
                                exit
                            end if
                        end do
                        if (symmetry_ == MM_GENERAL) exit
                    end do
                    if (symmetry_ == MM_HERMITIAN) exit symmetry_block

                    symmetry_ = MM_GENERAL
                end block symmetry_block
            end if
        end if

        ! Determine field type based on matrix type
        field_type = MM_COMPLEX

        catch: block
            ! Write header
            call write_mm_header(io, MM_ARRAY, field_type, symmetry_, &
                                nrows, ncols, comment=comment, iostat=stat, iomsg=msg)
            if (stat /= 0) exit catch

            ! Write array format (column-major order)
            if(symmetry_ == MM_GENERAL) then
                do j = 1, ncols
                    do i = 1, nrows
                        real_part = real(matrix(i, j), kind=dp)
                        imag_part = aimag(matrix(i, j))
                        write(io, fmt=fmt_, iostat=stat) real_part, imag_part
                        if (stat /= 0) then
                            msg = "Error writing array element (" // &
                                to_string(i) // "," // to_string(j) // ")"
                            exit catch
                        end if
                    end do
                end do
            else
                ! For symmetric and hermitian matrices, only the lower triangle
                ! (including the diagonal) is written.
                ! For skew-symmetric matrices, only the strictly lower triangle is written
                ! (the diagonal is omitted and assumed zero).
                do j = 1, ncols
                    do i = j, nrows
                        if(symmetry_ == MM_SKEW_SYMMETRIC .and. i == j) cycle
                        real_part = real(matrix(i, j), kind=dp)
                        imag_part = aimag(matrix(i, j))
                        if(i==j .and. symmetry_ == MM_HERMITIAN) imag_part = 0
                        write(io, fmt=fmt_, iostat=stat) real_part, imag_part
                        if (stat /= 0) then
                            msg = "Error writing array element (" // &
                                to_string(i) // "," // to_string(j) // ")"
                            exit catch
                        end if
                    end do
                end do
            end if
        end block catch

        close(io)
        if(stat/=0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Failed to save Matrix Market file '" // filename // "': " // msg)
            return
        end if

        if (present(iomsg) .and. allocated(msg)) call move_alloc(msg, iomsg)
    end subroutine
    module subroutine save_mm_dense_int8(filename, matrix, comment, format, symmetry, iostat, iomsg)
        !> Name of the Matrix Market file to save to
        character(len=*), intent(in) :: filename
        !> Matrix to be saved to the Matrix Market file
        integer(int8), intent(in) :: matrix(:,:)
        !> Optional comment information
        character(len=*), intent(in), optional :: comment
        !> Format in which matrix data needs to be stored
        character(len=*), intent(in), optional :: format
        !> Symmetry type of the matrix (general, symmetric, skew-symmetric, hermitian)
        character(len=*), intent(in), optional :: symmetry
        !> Error status of saving, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        integer :: io, stat, i, j, nnz, nrows, ncols
        character(len=:), allocatable :: msg
        character(len=:), allocatable :: field_type
        character(len=:), allocatable :: fmt_
        character(len=:), allocatable :: symmetry_
        if(present(iostat)) iostat = 0
        if(present(iomsg)) iomsg = ''
        stat = 0

            fmt_ = "I0"
        if(present(format)) fmt_ = format

        fmt_ = '('// fmt_ //')'

        io = open(filename, "w", iostat=stat)
        if (stat /= 0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Could not create file: " // filename)
            return
        end if

        nrows = size(matrix, 1)
        ncols = size(matrix, 2)
        ! Determine symmetry type
        symmetry_ = MM_GENERAL
        if (present(symmetry)) then
            symmetry_ = to_lower(trim(symmetry))
            if (symmetry_ == MM_AUTO) then

                symmetry_block: block
                    ! Non-square matrices cannot be symmetric/skew/hermitian
                    if (nrows /= ncols) then
                        symmetry_ = MM_GENERAL
                        exit symmetry_block
                    end if

                    ! Try symmetric
                    symmetry_ = MM_SYMMETRIC
                    do j = 1, ncols
                        do i = j+1, nrows
                            if (matrix(i,j) /= matrix(j,i)) then
                                symmetry_ = MM_GENERAL
                                exit
                            end if
                        end do
                        if (symmetry_ == MM_GENERAL) exit
                    end do
                    if (symmetry_ == MM_SYMMETRIC) exit symmetry_block

                    ! Try skew-symmetric
                    symmetry_ = MM_SKEW_SYMMETRIC
                    do j = 1, ncols
                        do i = j, nrows
                            if (matrix(i,j) /= -matrix(j,i)) then
                                symmetry_ = MM_GENERAL
                                exit
                            end if
                        end do
                        if (symmetry_ == MM_GENERAL) exit
                    end do
                    if (symmetry_ == MM_SKEW_SYMMETRIC) exit symmetry_block


                    symmetry_ = MM_GENERAL
                end block symmetry_block
            end if
        end if

        ! Determine field type based on matrix type
        field_type = MM_INTEGER

        catch: block
            ! Write header
            call write_mm_header(io, MM_ARRAY, field_type, symmetry_, &
                                nrows, ncols, comment=comment, iostat=stat, iomsg=msg)
            if (stat /= 0) exit catch

            ! Write array format (column-major order)
            if(symmetry_ == MM_GENERAL) then
                do j = 1, ncols
                    do i = 1, nrows
                        write(io, fmt=fmt_, iostat=stat) matrix(i, j)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // &
                                to_string(i) // "," // to_string(j) // ")"
                            exit catch
                        end if
                    end do
                end do
            else
                ! For symmetric and hermitian matrices, only the lower triangle
                ! (including the diagonal) is written.
                ! For skew-symmetric matrices, only the strictly lower triangle is written
                ! (the diagonal is omitted and assumed zero).
                do j = 1, ncols
                    do i = j, nrows
                        if(symmetry_ == MM_SKEW_SYMMETRIC .and. i == j) cycle
                        write(io, fmt=fmt_, iostat=stat) matrix(i, j)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // &
                                to_string(i) // "," // to_string(j) // ")"
                            exit catch
                        end if
                    end do
                end do
            end if
        end block catch

        close(io)
        if(stat/=0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Failed to save Matrix Market file '" // filename // "': " // msg)
            return
        end if

        if (present(iomsg) .and. allocated(msg)) call move_alloc(msg, iomsg)
    end subroutine
    module subroutine save_mm_dense_int16(filename, matrix, comment, format, symmetry, iostat, iomsg)
        !> Name of the Matrix Market file to save to
        character(len=*), intent(in) :: filename
        !> Matrix to be saved to the Matrix Market file
        integer(int16), intent(in) :: matrix(:,:)
        !> Optional comment information
        character(len=*), intent(in), optional :: comment
        !> Format in which matrix data needs to be stored
        character(len=*), intent(in), optional :: format
        !> Symmetry type of the matrix (general, symmetric, skew-symmetric, hermitian)
        character(len=*), intent(in), optional :: symmetry
        !> Error status of saving, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        integer :: io, stat, i, j, nnz, nrows, ncols
        character(len=:), allocatable :: msg
        character(len=:), allocatable :: field_type
        character(len=:), allocatable :: fmt_
        character(len=:), allocatable :: symmetry_
        if(present(iostat)) iostat = 0
        if(present(iomsg)) iomsg = ''
        stat = 0

            fmt_ = "I0"
        if(present(format)) fmt_ = format

        fmt_ = '('// fmt_ //')'

        io = open(filename, "w", iostat=stat)
        if (stat /= 0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Could not create file: " // filename)
            return
        end if

        nrows = size(matrix, 1)
        ncols = size(matrix, 2)
        ! Determine symmetry type
        symmetry_ = MM_GENERAL
        if (present(symmetry)) then
            symmetry_ = to_lower(trim(symmetry))
            if (symmetry_ == MM_AUTO) then

                symmetry_block: block
                    ! Non-square matrices cannot be symmetric/skew/hermitian
                    if (nrows /= ncols) then
                        symmetry_ = MM_GENERAL
                        exit symmetry_block
                    end if

                    ! Try symmetric
                    symmetry_ = MM_SYMMETRIC
                    do j = 1, ncols
                        do i = j+1, nrows
                            if (matrix(i,j) /= matrix(j,i)) then
                                symmetry_ = MM_GENERAL
                                exit
                            end if
                        end do
                        if (symmetry_ == MM_GENERAL) exit
                    end do
                    if (symmetry_ == MM_SYMMETRIC) exit symmetry_block

                    ! Try skew-symmetric
                    symmetry_ = MM_SKEW_SYMMETRIC
                    do j = 1, ncols
                        do i = j, nrows
                            if (matrix(i,j) /= -matrix(j,i)) then
                                symmetry_ = MM_GENERAL
                                exit
                            end if
                        end do
                        if (symmetry_ == MM_GENERAL) exit
                    end do
                    if (symmetry_ == MM_SKEW_SYMMETRIC) exit symmetry_block


                    symmetry_ = MM_GENERAL
                end block symmetry_block
            end if
        end if

        ! Determine field type based on matrix type
        field_type = MM_INTEGER

        catch: block
            ! Write header
            call write_mm_header(io, MM_ARRAY, field_type, symmetry_, &
                                nrows, ncols, comment=comment, iostat=stat, iomsg=msg)
            if (stat /= 0) exit catch

            ! Write array format (column-major order)
            if(symmetry_ == MM_GENERAL) then
                do j = 1, ncols
                    do i = 1, nrows
                        write(io, fmt=fmt_, iostat=stat) matrix(i, j)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // &
                                to_string(i) // "," // to_string(j) // ")"
                            exit catch
                        end if
                    end do
                end do
            else
                ! For symmetric and hermitian matrices, only the lower triangle
                ! (including the diagonal) is written.
                ! For skew-symmetric matrices, only the strictly lower triangle is written
                ! (the diagonal is omitted and assumed zero).
                do j = 1, ncols
                    do i = j, nrows
                        if(symmetry_ == MM_SKEW_SYMMETRIC .and. i == j) cycle
                        write(io, fmt=fmt_, iostat=stat) matrix(i, j)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // &
                                to_string(i) // "," // to_string(j) // ")"
                            exit catch
                        end if
                    end do
                end do
            end if
        end block catch

        close(io)
        if(stat/=0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Failed to save Matrix Market file '" // filename // "': " // msg)
            return
        end if

        if (present(iomsg) .and. allocated(msg)) call move_alloc(msg, iomsg)
    end subroutine
    module subroutine save_mm_dense_int32(filename, matrix, comment, format, symmetry, iostat, iomsg)
        !> Name of the Matrix Market file to save to
        character(len=*), intent(in) :: filename
        !> Matrix to be saved to the Matrix Market file
        integer(int32), intent(in) :: matrix(:,:)
        !> Optional comment information
        character(len=*), intent(in), optional :: comment
        !> Format in which matrix data needs to be stored
        character(len=*), intent(in), optional :: format
        !> Symmetry type of the matrix (general, symmetric, skew-symmetric, hermitian)
        character(len=*), intent(in), optional :: symmetry
        !> Error status of saving, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        integer :: io, stat, i, j, nnz, nrows, ncols
        character(len=:), allocatable :: msg
        character(len=:), allocatable :: field_type
        character(len=:), allocatable :: fmt_
        character(len=:), allocatable :: symmetry_
        if(present(iostat)) iostat = 0
        if(present(iomsg)) iomsg = ''
        stat = 0

            fmt_ = "I0"
        if(present(format)) fmt_ = format

        fmt_ = '('// fmt_ //')'

        io = open(filename, "w", iostat=stat)
        if (stat /= 0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Could not create file: " // filename)
            return
        end if

        nrows = size(matrix, 1)
        ncols = size(matrix, 2)
        ! Determine symmetry type
        symmetry_ = MM_GENERAL
        if (present(symmetry)) then
            symmetry_ = to_lower(trim(symmetry))
            if (symmetry_ == MM_AUTO) then

                symmetry_block: block
                    ! Non-square matrices cannot be symmetric/skew/hermitian
                    if (nrows /= ncols) then
                        symmetry_ = MM_GENERAL
                        exit symmetry_block
                    end if

                    ! Try symmetric
                    symmetry_ = MM_SYMMETRIC
                    do j = 1, ncols
                        do i = j+1, nrows
                            if (matrix(i,j) /= matrix(j,i)) then
                                symmetry_ = MM_GENERAL
                                exit
                            end if
                        end do
                        if (symmetry_ == MM_GENERAL) exit
                    end do
                    if (symmetry_ == MM_SYMMETRIC) exit symmetry_block

                    ! Try skew-symmetric
                    symmetry_ = MM_SKEW_SYMMETRIC
                    do j = 1, ncols
                        do i = j, nrows
                            if (matrix(i,j) /= -matrix(j,i)) then
                                symmetry_ = MM_GENERAL
                                exit
                            end if
                        end do
                        if (symmetry_ == MM_GENERAL) exit
                    end do
                    if (symmetry_ == MM_SKEW_SYMMETRIC) exit symmetry_block


                    symmetry_ = MM_GENERAL
                end block symmetry_block
            end if
        end if

        ! Determine field type based on matrix type
        field_type = MM_INTEGER

        catch: block
            ! Write header
            call write_mm_header(io, MM_ARRAY, field_type, symmetry_, &
                                nrows, ncols, comment=comment, iostat=stat, iomsg=msg)
            if (stat /= 0) exit catch

            ! Write array format (column-major order)
            if(symmetry_ == MM_GENERAL) then
                do j = 1, ncols
                    do i = 1, nrows
                        write(io, fmt=fmt_, iostat=stat) matrix(i, j)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // &
                                to_string(i) // "," // to_string(j) // ")"
                            exit catch
                        end if
                    end do
                end do
            else
                ! For symmetric and hermitian matrices, only the lower triangle
                ! (including the diagonal) is written.
                ! For skew-symmetric matrices, only the strictly lower triangle is written
                ! (the diagonal is omitted and assumed zero).
                do j = 1, ncols
                    do i = j, nrows
                        if(symmetry_ == MM_SKEW_SYMMETRIC .and. i == j) cycle
                        write(io, fmt=fmt_, iostat=stat) matrix(i, j)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // &
                                to_string(i) // "," // to_string(j) // ")"
                            exit catch
                        end if
                    end do
                end do
            end if
        end block catch

        close(io)
        if(stat/=0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Failed to save Matrix Market file '" // filename // "': " // msg)
            return
        end if

        if (present(iomsg) .and. allocated(msg)) call move_alloc(msg, iomsg)
    end subroutine
    module subroutine save_mm_dense_int64(filename, matrix, comment, format, symmetry, iostat, iomsg)
        !> Name of the Matrix Market file to save to
        character(len=*), intent(in) :: filename
        !> Matrix to be saved to the Matrix Market file
        integer(int64), intent(in) :: matrix(:,:)
        !> Optional comment information
        character(len=*), intent(in), optional :: comment
        !> Format in which matrix data needs to be stored
        character(len=*), intent(in), optional :: format
        !> Symmetry type of the matrix (general, symmetric, skew-symmetric, hermitian)
        character(len=*), intent(in), optional :: symmetry
        !> Error status of saving, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        integer :: io, stat, i, j, nnz, nrows, ncols
        character(len=:), allocatable :: msg
        character(len=:), allocatable :: field_type
        character(len=:), allocatable :: fmt_
        character(len=:), allocatable :: symmetry_
        if(present(iostat)) iostat = 0
        if(present(iomsg)) iomsg = ''
        stat = 0

            fmt_ = "I0"
        if(present(format)) fmt_ = format

        fmt_ = '('// fmt_ //')'

        io = open(filename, "w", iostat=stat)
        if (stat /= 0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Could not create file: " // filename)
            return
        end if

        nrows = size(matrix, 1)
        ncols = size(matrix, 2)
        ! Determine symmetry type
        symmetry_ = MM_GENERAL
        if (present(symmetry)) then
            symmetry_ = to_lower(trim(symmetry))
            if (symmetry_ == MM_AUTO) then

                symmetry_block: block
                    ! Non-square matrices cannot be symmetric/skew/hermitian
                    if (nrows /= ncols) then
                        symmetry_ = MM_GENERAL
                        exit symmetry_block
                    end if

                    ! Try symmetric
                    symmetry_ = MM_SYMMETRIC
                    do j = 1, ncols
                        do i = j+1, nrows
                            if (matrix(i,j) /= matrix(j,i)) then
                                symmetry_ = MM_GENERAL
                                exit
                            end if
                        end do
                        if (symmetry_ == MM_GENERAL) exit
                    end do
                    if (symmetry_ == MM_SYMMETRIC) exit symmetry_block

                    ! Try skew-symmetric
                    symmetry_ = MM_SKEW_SYMMETRIC
                    do j = 1, ncols
                        do i = j, nrows
                            if (matrix(i,j) /= -matrix(j,i)) then
                                symmetry_ = MM_GENERAL
                                exit
                            end if
                        end do
                        if (symmetry_ == MM_GENERAL) exit
                    end do
                    if (symmetry_ == MM_SKEW_SYMMETRIC) exit symmetry_block


                    symmetry_ = MM_GENERAL
                end block symmetry_block
            end if
        end if

        ! Determine field type based on matrix type
        field_type = MM_INTEGER

        catch: block
            ! Write header
            call write_mm_header(io, MM_ARRAY, field_type, symmetry_, &
                                nrows, ncols, comment=comment, iostat=stat, iomsg=msg)
            if (stat /= 0) exit catch

            ! Write array format (column-major order)
            if(symmetry_ == MM_GENERAL) then
                do j = 1, ncols
                    do i = 1, nrows
                        write(io, fmt=fmt_, iostat=stat) matrix(i, j)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // &
                                to_string(i) // "," // to_string(j) // ")"
                            exit catch
                        end if
                    end do
                end do
            else
                ! For symmetric and hermitian matrices, only the lower triangle
                ! (including the diagonal) is written.
                ! For skew-symmetric matrices, only the strictly lower triangle is written
                ! (the diagonal is omitted and assumed zero).
                do j = 1, ncols
                    do i = j, nrows
                        if(symmetry_ == MM_SKEW_SYMMETRIC .and. i == j) cycle
                        write(io, fmt=fmt_, iostat=stat) matrix(i, j)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // &
                                to_string(i) // "," // to_string(j) // ")"
                            exit catch
                        end if
                    end do
                end do
            end if
        end block catch

        close(io)
        if(stat/=0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Failed to save Matrix Market file '" // filename // "': " // msg)
            return
        end if

        if (present(iomsg) .and. allocated(msg)) call move_alloc(msg, iomsg)
    end subroutine

    module subroutine save_mm_coo_sp(filename, index, data, comment, format, symmetry, iostat, iomsg)
        !> Name of the Matrix Market file to save to
        character(len=*), intent(in) :: filename
        !> Matrix indices in COO format to be written to the file:
        !> index(1, :) = row indices, index(2, :) = column indices
        integer, intent(in) :: index(:, :)
        !> Nonzero matrix values corresponding to each (row, column) index pair
        real(sp), intent(in) :: data(:)
        !> Optional comment information
        character(len=*), intent(in), optional :: comment
        !> Format in which matrix data needs to be stored
        character(len=*), intent(in), optional :: format
        !> Symmetry type of the matrix (general, symmetric, skew-symmetric, hermitian)
        character(len=*), intent(in), optional :: symmetry
        !> Error status of saving, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        integer :: io, stat, i, nnz_to_write
        character(len=:), allocatable :: msg
        character(len=:), allocatable :: field_type
        character(len=:), allocatable :: fmt_
        character(len=:), allocatable :: symmetry_
        logical :: is_pattern
        if(present(iostat)) iostat = 0
        if(present(iomsg)) iomsg = ''
        stat = 0

        if(size(data)==1 .and. size(data)/=size(index,dim=2)) then
            is_pattern = .true.
        else
            is_pattern = .false.
        end if

        if(is_pattern) then
            fmt_ = '(I0,1X,I0)'
        else
                fmt_ = "ES24.16E3"
            if(present(format)) fmt_ = format
            fmt_ = '(I0,1X,I0,1X,' // fmt_ //')'
        end if

        io = open(filename, "w", iostat=stat)
        if (stat /= 0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Could not create file: " // filename)
            return
        end if

        if (.not. is_pattern .and. size(data) /= size(index,dim=2)) then
            call mm_fail_process(iostat=iostat, iomsg=iomsg, code=1, &
                message="Invalid COO data size")
            return
        end if

        if(size(index, dim=1)/=2) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Invalid index dimensions: first dimension must be 2")
            return
        end if

        ! Determine symmetry type
        symmetry_ = MM_GENERAL
        if (present(symmetry)) then
            symmetry_ = to_lower(trim(symmetry))
        end if

        ! Determine field type based on matrix type
        if(is_pattern) then
            field_type = MM_PATTERN
        else
            field_type = MM_REAL
        end if

        catch: block
            ! Calculate the nnz to write inside mtx file
            if (symmetry_ == MM_GENERAL) then
                nnz_to_write = size(index, dim=2)
            else if(symmetry_ == MM_SKEW_SYMMETRIC) then
                nnz_to_write = count(index(1,:) > index(2,:))
            else
                nnz_to_write = count(index(1,:) >= index(2,:))
            end if
            ! Write header
            if(nnz_to_write == 0) then
                call write_mm_header(io, MM_COORDINATE, field_type, symmetry_, &
                                    0, 0,&
                                    nnz=nnz_to_write, comment=comment, iostat=stat, iomsg=msg)
                exit catch
            end if
            call write_mm_header(io, MM_COORDINATE, field_type, symmetry_, &
                                maxval(index(1,:)), maxval(index(2,:)),&
                                nnz=nnz_to_write, comment=comment, iostat=stat, iomsg=msg)
            if (stat /= 0) exit catch

            ! Write coordinate format (row, column, value)
            if(symmetry_ == MM_GENERAL) then
                if(is_pattern) then
                    do i = 1, nnz_to_write
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                else
                    do i = 1, nnz_to_write
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i), data(i)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                end if
            else
                ! For symmetric and hermitian matrices, only the lower triangle
                ! (including the diagonal) is written.
                ! For skew-symmetric matrices, only the strictly lower triangle is written
                ! (the diagonal is omitted and assumed zero).
                if(is_pattern) then
                    do i = 1, size(index, dim=2)
                        if(index(1,i) < index(2,i)) cycle
                        if(symmetry_ == MM_SKEW_SYMMETRIC .and. index(1,i) == index(2,i)) cycle
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                else
                    do i = 1, size(index, dim=2)
                        if(index(1,i) < index(2,i)) cycle
                        if(symmetry_ == MM_SKEW_SYMMETRIC .and. index(1,i) == index(2,i)) cycle
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i), data(i)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                end if
            end if
        end block catch

        close(io)

        if(stat/=0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Failed to save Matrix Market file '" // filename // "': " // msg)
                return
        end if

        if (present(iomsg) .and. allocated(msg)) call move_alloc(msg, iomsg)
    end subroutine
    module subroutine save_mm_coo_dp(filename, index, data, comment, format, symmetry, iostat, iomsg)
        !> Name of the Matrix Market file to save to
        character(len=*), intent(in) :: filename
        !> Matrix indices in COO format to be written to the file:
        !> index(1, :) = row indices, index(2, :) = column indices
        integer, intent(in) :: index(:, :)
        !> Nonzero matrix values corresponding to each (row, column) index pair
        real(dp), intent(in) :: data(:)
        !> Optional comment information
        character(len=*), intent(in), optional :: comment
        !> Format in which matrix data needs to be stored
        character(len=*), intent(in), optional :: format
        !> Symmetry type of the matrix (general, symmetric, skew-symmetric, hermitian)
        character(len=*), intent(in), optional :: symmetry
        !> Error status of saving, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        integer :: io, stat, i, nnz_to_write
        character(len=:), allocatable :: msg
        character(len=:), allocatable :: field_type
        character(len=:), allocatable :: fmt_
        character(len=:), allocatable :: symmetry_
        logical :: is_pattern
        if(present(iostat)) iostat = 0
        if(present(iomsg)) iomsg = ''
        stat = 0

        if(size(data)==1 .and. size(data)/=size(index,dim=2)) then
            is_pattern = .true.
        else
            is_pattern = .false.
        end if

        if(is_pattern) then
            fmt_ = '(I0,1X,I0)'
        else
                fmt_ = "ES24.16E3"
            if(present(format)) fmt_ = format
            fmt_ = '(I0,1X,I0,1X,' // fmt_ //')'
        end if

        io = open(filename, "w", iostat=stat)
        if (stat /= 0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Could not create file: " // filename)
            return
        end if

        if (.not. is_pattern .and. size(data) /= size(index,dim=2)) then
            call mm_fail_process(iostat=iostat, iomsg=iomsg, code=1, &
                message="Invalid COO data size")
            return
        end if

        if(size(index, dim=1)/=2) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Invalid index dimensions: first dimension must be 2")
            return
        end if

        ! Determine symmetry type
        symmetry_ = MM_GENERAL
        if (present(symmetry)) then
            symmetry_ = to_lower(trim(symmetry))
        end if

        ! Determine field type based on matrix type
        if(is_pattern) then
            field_type = MM_PATTERN
        else
            field_type = MM_REAL
        end if

        catch: block
            ! Calculate the nnz to write inside mtx file
            if (symmetry_ == MM_GENERAL) then
                nnz_to_write = size(index, dim=2)
            else if(symmetry_ == MM_SKEW_SYMMETRIC) then
                nnz_to_write = count(index(1,:) > index(2,:))
            else
                nnz_to_write = count(index(1,:) >= index(2,:))
            end if
            ! Write header
            if(nnz_to_write == 0) then
                call write_mm_header(io, MM_COORDINATE, field_type, symmetry_, &
                                    0, 0,&
                                    nnz=nnz_to_write, comment=comment, iostat=stat, iomsg=msg)
                exit catch
            end if
            call write_mm_header(io, MM_COORDINATE, field_type, symmetry_, &
                                maxval(index(1,:)), maxval(index(2,:)),&
                                nnz=nnz_to_write, comment=comment, iostat=stat, iomsg=msg)
            if (stat /= 0) exit catch

            ! Write coordinate format (row, column, value)
            if(symmetry_ == MM_GENERAL) then
                if(is_pattern) then
                    do i = 1, nnz_to_write
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                else
                    do i = 1, nnz_to_write
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i), data(i)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                end if
            else
                ! For symmetric and hermitian matrices, only the lower triangle
                ! (including the diagonal) is written.
                ! For skew-symmetric matrices, only the strictly lower triangle is written
                ! (the diagonal is omitted and assumed zero).
                if(is_pattern) then
                    do i = 1, size(index, dim=2)
                        if(index(1,i) < index(2,i)) cycle
                        if(symmetry_ == MM_SKEW_SYMMETRIC .and. index(1,i) == index(2,i)) cycle
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                else
                    do i = 1, size(index, dim=2)
                        if(index(1,i) < index(2,i)) cycle
                        if(symmetry_ == MM_SKEW_SYMMETRIC .and. index(1,i) == index(2,i)) cycle
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i), data(i)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                end if
            end if
        end block catch

        close(io)

        if(stat/=0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Failed to save Matrix Market file '" // filename // "': " // msg)
                return
        end if

        if (present(iomsg) .and. allocated(msg)) call move_alloc(msg, iomsg)
    end subroutine
    module subroutine save_mm_coo_csp(filename, index, data, comment, format, symmetry, iostat, iomsg)
        !> Name of the Matrix Market file to save to
        character(len=*), intent(in) :: filename
        !> Matrix indices in COO format to be written to the file:
        !> index(1, :) = row indices, index(2, :) = column indices
        integer, intent(in) :: index(:, :)
        !> Nonzero matrix values corresponding to each (row, column) index pair
        complex(sp), intent(in) :: data(:)
        !> Optional comment information
        character(len=*), intent(in), optional :: comment
        !> Format in which matrix data needs to be stored
        character(len=*), intent(in), optional :: format
        !> Symmetry type of the matrix (general, symmetric, skew-symmetric, hermitian)
        character(len=*), intent(in), optional :: symmetry
        !> Error status of saving, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        integer :: io, stat, i, nnz_to_write
        character(len=:), allocatable :: msg
        character(len=:), allocatable :: field_type
        character(len=:), allocatable :: fmt_
        character(len=:), allocatable :: symmetry_
        logical :: is_pattern
        real(sp) :: real_part, imag_part
        if(present(iostat)) iostat = 0
        if(present(iomsg)) iomsg = ''
        stat = 0

        if(size(data)==1 .and. size(data)/=size(index,dim=2)) then
            is_pattern = .true.
        else
            is_pattern = .false.
        end if

        if(is_pattern) then
            fmt_ = '(I0,1X,I0)'
        else
                fmt_ = "ES24.16E3"
            if(present(format)) fmt_ = format
            fmt_ = '(I0,1X,I0,1X,' // fmt_//',1X,'//fmt_//')'
        end if

        io = open(filename, "w", iostat=stat)
        if (stat /= 0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Could not create file: " // filename)
            return
        end if

        if (.not. is_pattern .and. size(data) /= size(index,dim=2)) then
            call mm_fail_process(iostat=iostat, iomsg=iomsg, code=1, &
                message="Invalid COO data size")
            return
        end if

        if(size(index, dim=1)/=2) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Invalid index dimensions: first dimension must be 2")
            return
        end if

        ! Determine symmetry type
        symmetry_ = MM_GENERAL
        if (present(symmetry)) then
            symmetry_ = to_lower(trim(symmetry))
        end if

        ! Determine field type based on matrix type
        if(is_pattern) then
            field_type = MM_PATTERN
        else
            field_type = MM_COMPLEX
        end if

        catch: block
            ! Calculate the nnz to write inside mtx file
            if (symmetry_ == MM_GENERAL) then
                nnz_to_write = size(index, dim=2)
            else if(symmetry_ == MM_SKEW_SYMMETRIC) then
                nnz_to_write = count(index(1,:) > index(2,:))
            else
                nnz_to_write = count(index(1,:) >= index(2,:))
            end if
            ! Write header
            if(nnz_to_write == 0) then
                call write_mm_header(io, MM_COORDINATE, field_type, symmetry_, &
                                    0, 0,&
                                    nnz=nnz_to_write, comment=comment, iostat=stat, iomsg=msg)
                exit catch
            end if
            call write_mm_header(io, MM_COORDINATE, field_type, symmetry_, &
                                maxval(index(1,:)), maxval(index(2,:)),&
                                nnz=nnz_to_write, comment=comment, iostat=stat, iomsg=msg)
            if (stat /= 0) exit catch

            ! Write coordinate format (row, column, value)
            if(symmetry_ == MM_GENERAL) then
                if(is_pattern) then
                    do i = 1, nnz_to_write
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                else
                    do i = 1, nnz_to_write
                        real_part = real(data(i), kind=sp)
                        imag_part = aimag(data(i))
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i), real_part, imag_part
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                end if
            else
                ! For symmetric and hermitian matrices, only the lower triangle
                ! (including the diagonal) is written.
                ! For skew-symmetric matrices, only the strictly lower triangle is written
                ! (the diagonal is omitted and assumed zero).
                if(is_pattern) then
                    do i = 1, size(index, dim=2)
                        if(index(1,i) < index(2,i)) cycle
                        if(symmetry_ == MM_SKEW_SYMMETRIC .and. index(1,i) == index(2,i)) cycle
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                else
                    do i = 1, size(index, dim=2)
                        if(index(1,i) < index(2,i)) cycle
                        if(symmetry_ == MM_SKEW_SYMMETRIC .and. index(1,i) == index(2,i)) cycle
                        real_part = real(data(i), kind=sp)
                        imag_part = aimag(data(i))
                        if(index(1,i)==index(2,i) .and. symmetry_ == MM_HERMITIAN) imag_part = 0
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i), real_part, imag_part
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                end if
            end if
        end block catch

        close(io)

        if(stat/=0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Failed to save Matrix Market file '" // filename // "': " // msg)
                return
        end if

        if (present(iomsg) .and. allocated(msg)) call move_alloc(msg, iomsg)
    end subroutine
    module subroutine save_mm_coo_cdp(filename, index, data, comment, format, symmetry, iostat, iomsg)
        !> Name of the Matrix Market file to save to
        character(len=*), intent(in) :: filename
        !> Matrix indices in COO format to be written to the file:
        !> index(1, :) = row indices, index(2, :) = column indices
        integer, intent(in) :: index(:, :)
        !> Nonzero matrix values corresponding to each (row, column) index pair
        complex(dp), intent(in) :: data(:)
        !> Optional comment information
        character(len=*), intent(in), optional :: comment
        !> Format in which matrix data needs to be stored
        character(len=*), intent(in), optional :: format
        !> Symmetry type of the matrix (general, symmetric, skew-symmetric, hermitian)
        character(len=*), intent(in), optional :: symmetry
        !> Error status of saving, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        integer :: io, stat, i, nnz_to_write
        character(len=:), allocatable :: msg
        character(len=:), allocatable :: field_type
        character(len=:), allocatable :: fmt_
        character(len=:), allocatable :: symmetry_
        logical :: is_pattern
        real(dp) :: real_part, imag_part
        if(present(iostat)) iostat = 0
        if(present(iomsg)) iomsg = ''
        stat = 0

        if(size(data)==1 .and. size(data)/=size(index,dim=2)) then
            is_pattern = .true.
        else
            is_pattern = .false.
        end if

        if(is_pattern) then
            fmt_ = '(I0,1X,I0)'
        else
                fmt_ = "ES24.16E3"
            if(present(format)) fmt_ = format
            fmt_ = '(I0,1X,I0,1X,' // fmt_//',1X,'//fmt_//')'
        end if

        io = open(filename, "w", iostat=stat)
        if (stat /= 0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Could not create file: " // filename)
            return
        end if

        if (.not. is_pattern .and. size(data) /= size(index,dim=2)) then
            call mm_fail_process(iostat=iostat, iomsg=iomsg, code=1, &
                message="Invalid COO data size")
            return
        end if

        if(size(index, dim=1)/=2) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Invalid index dimensions: first dimension must be 2")
            return
        end if

        ! Determine symmetry type
        symmetry_ = MM_GENERAL
        if (present(symmetry)) then
            symmetry_ = to_lower(trim(symmetry))
        end if

        ! Determine field type based on matrix type
        if(is_pattern) then
            field_type = MM_PATTERN
        else
            field_type = MM_COMPLEX
        end if

        catch: block
            ! Calculate the nnz to write inside mtx file
            if (symmetry_ == MM_GENERAL) then
                nnz_to_write = size(index, dim=2)
            else if(symmetry_ == MM_SKEW_SYMMETRIC) then
                nnz_to_write = count(index(1,:) > index(2,:))
            else
                nnz_to_write = count(index(1,:) >= index(2,:))
            end if
            ! Write header
            if(nnz_to_write == 0) then
                call write_mm_header(io, MM_COORDINATE, field_type, symmetry_, &
                                    0, 0,&
                                    nnz=nnz_to_write, comment=comment, iostat=stat, iomsg=msg)
                exit catch
            end if
            call write_mm_header(io, MM_COORDINATE, field_type, symmetry_, &
                                maxval(index(1,:)), maxval(index(2,:)),&
                                nnz=nnz_to_write, comment=comment, iostat=stat, iomsg=msg)
            if (stat /= 0) exit catch

            ! Write coordinate format (row, column, value)
            if(symmetry_ == MM_GENERAL) then
                if(is_pattern) then
                    do i = 1, nnz_to_write
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                else
                    do i = 1, nnz_to_write
                        real_part = real(data(i), kind=dp)
                        imag_part = aimag(data(i))
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i), real_part, imag_part
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                end if
            else
                ! For symmetric and hermitian matrices, only the lower triangle
                ! (including the diagonal) is written.
                ! For skew-symmetric matrices, only the strictly lower triangle is written
                ! (the diagonal is omitted and assumed zero).
                if(is_pattern) then
                    do i = 1, size(index, dim=2)
                        if(index(1,i) < index(2,i)) cycle
                        if(symmetry_ == MM_SKEW_SYMMETRIC .and. index(1,i) == index(2,i)) cycle
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                else
                    do i = 1, size(index, dim=2)
                        if(index(1,i) < index(2,i)) cycle
                        if(symmetry_ == MM_SKEW_SYMMETRIC .and. index(1,i) == index(2,i)) cycle
                        real_part = real(data(i), kind=dp)
                        imag_part = aimag(data(i))
                        if(index(1,i)==index(2,i) .and. symmetry_ == MM_HERMITIAN) imag_part = 0
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i), real_part, imag_part
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                end if
            end if
        end block catch

        close(io)

        if(stat/=0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Failed to save Matrix Market file '" // filename // "': " // msg)
                return
        end if

        if (present(iomsg) .and. allocated(msg)) call move_alloc(msg, iomsg)
    end subroutine
    module subroutine save_mm_coo_int8(filename, index, data, comment, format, symmetry, iostat, iomsg)
        !> Name of the Matrix Market file to save to
        character(len=*), intent(in) :: filename
        !> Matrix indices in COO format to be written to the file:
        !> index(1, :) = row indices, index(2, :) = column indices
        integer, intent(in) :: index(:, :)
        !> Nonzero matrix values corresponding to each (row, column) index pair
        integer(int8), intent(in) :: data(:)
        !> Optional comment information
        character(len=*), intent(in), optional :: comment
        !> Format in which matrix data needs to be stored
        character(len=*), intent(in), optional :: format
        !> Symmetry type of the matrix (general, symmetric, skew-symmetric, hermitian)
        character(len=*), intent(in), optional :: symmetry
        !> Error status of saving, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        integer :: io, stat, i, nnz_to_write
        character(len=:), allocatable :: msg
        character(len=:), allocatable :: field_type
        character(len=:), allocatable :: fmt_
        character(len=:), allocatable :: symmetry_
        logical :: is_pattern
        if(present(iostat)) iostat = 0
        if(present(iomsg)) iomsg = ''
        stat = 0

        if(size(data)==1 .and. size(data)/=size(index,dim=2)) then
            is_pattern = .true.
        else
            is_pattern = .false.
        end if

        if(is_pattern) then
            fmt_ = '(I0,1X,I0)'
        else
                fmt_ = "I0"
            if(present(format)) fmt_ = format
            fmt_ = '(I0,1X,I0,1X,'// fmt_ //')'
        end if

        io = open(filename, "w", iostat=stat)
        if (stat /= 0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Could not create file: " // filename)
            return
        end if

        if (.not. is_pattern .and. size(data) /= size(index,dim=2)) then
            call mm_fail_process(iostat=iostat, iomsg=iomsg, code=1, &
                message="Invalid COO data size")
            return
        end if

        if(size(index, dim=1)/=2) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Invalid index dimensions: first dimension must be 2")
            return
        end if

        ! Determine symmetry type
        symmetry_ = MM_GENERAL
        if (present(symmetry)) then
            symmetry_ = to_lower(trim(symmetry))
        end if

        ! Determine field type based on matrix type
        if(is_pattern) then
            field_type = MM_PATTERN
        else
            field_type = MM_INTEGER
        end if

        catch: block
            ! Calculate the nnz to write inside mtx file
            if (symmetry_ == MM_GENERAL) then
                nnz_to_write = size(index, dim=2)
            else if(symmetry_ == MM_SKEW_SYMMETRIC) then
                nnz_to_write = count(index(1,:) > index(2,:))
            else
                nnz_to_write = count(index(1,:) >= index(2,:))
            end if
            ! Write header
            if(nnz_to_write == 0) then
                call write_mm_header(io, MM_COORDINATE, field_type, symmetry_, &
                                    0, 0,&
                                    nnz=nnz_to_write, comment=comment, iostat=stat, iomsg=msg)
                exit catch
            end if
            call write_mm_header(io, MM_COORDINATE, field_type, symmetry_, &
                                maxval(index(1,:)), maxval(index(2,:)),&
                                nnz=nnz_to_write, comment=comment, iostat=stat, iomsg=msg)
            if (stat /= 0) exit catch

            ! Write coordinate format (row, column, value)
            if(symmetry_ == MM_GENERAL) then
                if(is_pattern) then
                    do i = 1, nnz_to_write
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                else
                    do i = 1, nnz_to_write
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i), data(i)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                end if
            else
                ! For symmetric and hermitian matrices, only the lower triangle
                ! (including the diagonal) is written.
                ! For skew-symmetric matrices, only the strictly lower triangle is written
                ! (the diagonal is omitted and assumed zero).
                if(is_pattern) then
                    do i = 1, size(index, dim=2)
                        if(index(1,i) < index(2,i)) cycle
                        if(symmetry_ == MM_SKEW_SYMMETRIC .and. index(1,i) == index(2,i)) cycle
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                else
                    do i = 1, size(index, dim=2)
                        if(index(1,i) < index(2,i)) cycle
                        if(symmetry_ == MM_SKEW_SYMMETRIC .and. index(1,i) == index(2,i)) cycle
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i), data(i)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                end if
            end if
        end block catch

        close(io)

        if(stat/=0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Failed to save Matrix Market file '" // filename // "': " // msg)
                return
        end if

        if (present(iomsg) .and. allocated(msg)) call move_alloc(msg, iomsg)
    end subroutine
    module subroutine save_mm_coo_int16(filename, index, data, comment, format, symmetry, iostat, iomsg)
        !> Name of the Matrix Market file to save to
        character(len=*), intent(in) :: filename
        !> Matrix indices in COO format to be written to the file:
        !> index(1, :) = row indices, index(2, :) = column indices
        integer, intent(in) :: index(:, :)
        !> Nonzero matrix values corresponding to each (row, column) index pair
        integer(int16), intent(in) :: data(:)
        !> Optional comment information
        character(len=*), intent(in), optional :: comment
        !> Format in which matrix data needs to be stored
        character(len=*), intent(in), optional :: format
        !> Symmetry type of the matrix (general, symmetric, skew-symmetric, hermitian)
        character(len=*), intent(in), optional :: symmetry
        !> Error status of saving, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        integer :: io, stat, i, nnz_to_write
        character(len=:), allocatable :: msg
        character(len=:), allocatable :: field_type
        character(len=:), allocatable :: fmt_
        character(len=:), allocatable :: symmetry_
        logical :: is_pattern
        if(present(iostat)) iostat = 0
        if(present(iomsg)) iomsg = ''
        stat = 0

        if(size(data)==1 .and. size(data)/=size(index,dim=2)) then
            is_pattern = .true.
        else
            is_pattern = .false.
        end if

        if(is_pattern) then
            fmt_ = '(I0,1X,I0)'
        else
                fmt_ = "I0"
            if(present(format)) fmt_ = format
            fmt_ = '(I0,1X,I0,1X,'// fmt_ //')'
        end if

        io = open(filename, "w", iostat=stat)
        if (stat /= 0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Could not create file: " // filename)
            return
        end if

        if (.not. is_pattern .and. size(data) /= size(index,dim=2)) then
            call mm_fail_process(iostat=iostat, iomsg=iomsg, code=1, &
                message="Invalid COO data size")
            return
        end if

        if(size(index, dim=1)/=2) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Invalid index dimensions: first dimension must be 2")
            return
        end if

        ! Determine symmetry type
        symmetry_ = MM_GENERAL
        if (present(symmetry)) then
            symmetry_ = to_lower(trim(symmetry))
        end if

        ! Determine field type based on matrix type
        if(is_pattern) then
            field_type = MM_PATTERN
        else
            field_type = MM_INTEGER
        end if

        catch: block
            ! Calculate the nnz to write inside mtx file
            if (symmetry_ == MM_GENERAL) then
                nnz_to_write = size(index, dim=2)
            else if(symmetry_ == MM_SKEW_SYMMETRIC) then
                nnz_to_write = count(index(1,:) > index(2,:))
            else
                nnz_to_write = count(index(1,:) >= index(2,:))
            end if
            ! Write header
            if(nnz_to_write == 0) then
                call write_mm_header(io, MM_COORDINATE, field_type, symmetry_, &
                                    0, 0,&
                                    nnz=nnz_to_write, comment=comment, iostat=stat, iomsg=msg)
                exit catch
            end if
            call write_mm_header(io, MM_COORDINATE, field_type, symmetry_, &
                                maxval(index(1,:)), maxval(index(2,:)),&
                                nnz=nnz_to_write, comment=comment, iostat=stat, iomsg=msg)
            if (stat /= 0) exit catch

            ! Write coordinate format (row, column, value)
            if(symmetry_ == MM_GENERAL) then
                if(is_pattern) then
                    do i = 1, nnz_to_write
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                else
                    do i = 1, nnz_to_write
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i), data(i)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                end if
            else
                ! For symmetric and hermitian matrices, only the lower triangle
                ! (including the diagonal) is written.
                ! For skew-symmetric matrices, only the strictly lower triangle is written
                ! (the diagonal is omitted and assumed zero).
                if(is_pattern) then
                    do i = 1, size(index, dim=2)
                        if(index(1,i) < index(2,i)) cycle
                        if(symmetry_ == MM_SKEW_SYMMETRIC .and. index(1,i) == index(2,i)) cycle
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                else
                    do i = 1, size(index, dim=2)
                        if(index(1,i) < index(2,i)) cycle
                        if(symmetry_ == MM_SKEW_SYMMETRIC .and. index(1,i) == index(2,i)) cycle
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i), data(i)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                end if
            end if
        end block catch

        close(io)

        if(stat/=0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Failed to save Matrix Market file '" // filename // "': " // msg)
                return
        end if

        if (present(iomsg) .and. allocated(msg)) call move_alloc(msg, iomsg)
    end subroutine
    module subroutine save_mm_coo_int32(filename, index, data, comment, format, symmetry, iostat, iomsg)
        !> Name of the Matrix Market file to save to
        character(len=*), intent(in) :: filename
        !> Matrix indices in COO format to be written to the file:
        !> index(1, :) = row indices, index(2, :) = column indices
        integer, intent(in) :: index(:, :)
        !> Nonzero matrix values corresponding to each (row, column) index pair
        integer(int32), intent(in) :: data(:)
        !> Optional comment information
        character(len=*), intent(in), optional :: comment
        !> Format in which matrix data needs to be stored
        character(len=*), intent(in), optional :: format
        !> Symmetry type of the matrix (general, symmetric, skew-symmetric, hermitian)
        character(len=*), intent(in), optional :: symmetry
        !> Error status of saving, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        integer :: io, stat, i, nnz_to_write
        character(len=:), allocatable :: msg
        character(len=:), allocatable :: field_type
        character(len=:), allocatable :: fmt_
        character(len=:), allocatable :: symmetry_
        logical :: is_pattern
        if(present(iostat)) iostat = 0
        if(present(iomsg)) iomsg = ''
        stat = 0

        if(size(data)==1 .and. size(data)/=size(index,dim=2)) then
            is_pattern = .true.
        else
            is_pattern = .false.
        end if

        if(is_pattern) then
            fmt_ = '(I0,1X,I0)'
        else
                fmt_ = "I0"
            if(present(format)) fmt_ = format
            fmt_ = '(I0,1X,I0,1X,'// fmt_ //')'
        end if

        io = open(filename, "w", iostat=stat)
        if (stat /= 0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Could not create file: " // filename)
            return
        end if

        if (.not. is_pattern .and. size(data) /= size(index,dim=2)) then
            call mm_fail_process(iostat=iostat, iomsg=iomsg, code=1, &
                message="Invalid COO data size")
            return
        end if

        if(size(index, dim=1)/=2) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Invalid index dimensions: first dimension must be 2")
            return
        end if

        ! Determine symmetry type
        symmetry_ = MM_GENERAL
        if (present(symmetry)) then
            symmetry_ = to_lower(trim(symmetry))
        end if

        ! Determine field type based on matrix type
        if(is_pattern) then
            field_type = MM_PATTERN
        else
            field_type = MM_INTEGER
        end if

        catch: block
            ! Calculate the nnz to write inside mtx file
            if (symmetry_ == MM_GENERAL) then
                nnz_to_write = size(index, dim=2)
            else if(symmetry_ == MM_SKEW_SYMMETRIC) then
                nnz_to_write = count(index(1,:) > index(2,:))
            else
                nnz_to_write = count(index(1,:) >= index(2,:))
            end if
            ! Write header
            if(nnz_to_write == 0) then
                call write_mm_header(io, MM_COORDINATE, field_type, symmetry_, &
                                    0, 0,&
                                    nnz=nnz_to_write, comment=comment, iostat=stat, iomsg=msg)
                exit catch
            end if
            call write_mm_header(io, MM_COORDINATE, field_type, symmetry_, &
                                maxval(index(1,:)), maxval(index(2,:)),&
                                nnz=nnz_to_write, comment=comment, iostat=stat, iomsg=msg)
            if (stat /= 0) exit catch

            ! Write coordinate format (row, column, value)
            if(symmetry_ == MM_GENERAL) then
                if(is_pattern) then
                    do i = 1, nnz_to_write
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                else
                    do i = 1, nnz_to_write
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i), data(i)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                end if
            else
                ! For symmetric and hermitian matrices, only the lower triangle
                ! (including the diagonal) is written.
                ! For skew-symmetric matrices, only the strictly lower triangle is written
                ! (the diagonal is omitted and assumed zero).
                if(is_pattern) then
                    do i = 1, size(index, dim=2)
                        if(index(1,i) < index(2,i)) cycle
                        if(symmetry_ == MM_SKEW_SYMMETRIC .and. index(1,i) == index(2,i)) cycle
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                else
                    do i = 1, size(index, dim=2)
                        if(index(1,i) < index(2,i)) cycle
                        if(symmetry_ == MM_SKEW_SYMMETRIC .and. index(1,i) == index(2,i)) cycle
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i), data(i)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                end if
            end if
        end block catch

        close(io)

        if(stat/=0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Failed to save Matrix Market file '" // filename // "': " // msg)
                return
        end if

        if (present(iomsg) .and. allocated(msg)) call move_alloc(msg, iomsg)
    end subroutine
    module subroutine save_mm_coo_int64(filename, index, data, comment, format, symmetry, iostat, iomsg)
        !> Name of the Matrix Market file to save to
        character(len=*), intent(in) :: filename
        !> Matrix indices in COO format to be written to the file:
        !> index(1, :) = row indices, index(2, :) = column indices
        integer, intent(in) :: index(:, :)
        !> Nonzero matrix values corresponding to each (row, column) index pair
        integer(int64), intent(in) :: data(:)
        !> Optional comment information
        character(len=*), intent(in), optional :: comment
        !> Format in which matrix data needs to be stored
        character(len=*), intent(in), optional :: format
        !> Symmetry type of the matrix (general, symmetric, skew-symmetric, hermitian)
        character(len=*), intent(in), optional :: symmetry
        !> Error status of saving, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        integer :: io, stat, i, nnz_to_write
        character(len=:), allocatable :: msg
        character(len=:), allocatable :: field_type
        character(len=:), allocatable :: fmt_
        character(len=:), allocatable :: symmetry_
        logical :: is_pattern
        if(present(iostat)) iostat = 0
        if(present(iomsg)) iomsg = ''
        stat = 0

        if(size(data)==1 .and. size(data)/=size(index,dim=2)) then
            is_pattern = .true.
        else
            is_pattern = .false.
        end if

        if(is_pattern) then
            fmt_ = '(I0,1X,I0)'
        else
                fmt_ = "I0"
            if(present(format)) fmt_ = format
            fmt_ = '(I0,1X,I0,1X,'// fmt_ //')'
        end if

        io = open(filename, "w", iostat=stat)
        if (stat /= 0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Could not create file: " // filename)
            return
        end if

        if (.not. is_pattern .and. size(data) /= size(index,dim=2)) then
            call mm_fail_process(iostat=iostat, iomsg=iomsg, code=1, &
                message="Invalid COO data size")
            return
        end if

        if(size(index, dim=1)/=2) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Invalid index dimensions: first dimension must be 2")
            return
        end if

        ! Determine symmetry type
        symmetry_ = MM_GENERAL
        if (present(symmetry)) then
            symmetry_ = to_lower(trim(symmetry))
        end if

        ! Determine field type based on matrix type
        if(is_pattern) then
            field_type = MM_PATTERN
        else
            field_type = MM_INTEGER
        end if

        catch: block
            ! Calculate the nnz to write inside mtx file
            if (symmetry_ == MM_GENERAL) then
                nnz_to_write = size(index, dim=2)
            else if(symmetry_ == MM_SKEW_SYMMETRIC) then
                nnz_to_write = count(index(1,:) > index(2,:))
            else
                nnz_to_write = count(index(1,:) >= index(2,:))
            end if
            ! Write header
            if(nnz_to_write == 0) then
                call write_mm_header(io, MM_COORDINATE, field_type, symmetry_, &
                                    0, 0,&
                                    nnz=nnz_to_write, comment=comment, iostat=stat, iomsg=msg)
                exit catch
            end if
            call write_mm_header(io, MM_COORDINATE, field_type, symmetry_, &
                                maxval(index(1,:)), maxval(index(2,:)),&
                                nnz=nnz_to_write, comment=comment, iostat=stat, iomsg=msg)
            if (stat /= 0) exit catch

            ! Write coordinate format (row, column, value)
            if(symmetry_ == MM_GENERAL) then
                if(is_pattern) then
                    do i = 1, nnz_to_write
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                else
                    do i = 1, nnz_to_write
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i), data(i)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                end if
            else
                ! For symmetric and hermitian matrices, only the lower triangle
                ! (including the diagonal) is written.
                ! For skew-symmetric matrices, only the strictly lower triangle is written
                ! (the diagonal is omitted and assumed zero).
                if(is_pattern) then
                    do i = 1, size(index, dim=2)
                        if(index(1,i) < index(2,i)) cycle
                        if(symmetry_ == MM_SKEW_SYMMETRIC .and. index(1,i) == index(2,i)) cycle
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                else
                    do i = 1, size(index, dim=2)
                        if(index(1,i) < index(2,i)) cycle
                        if(symmetry_ == MM_SKEW_SYMMETRIC .and. index(1,i) == index(2,i)) cycle
                        write(io, fmt=fmt_, iostat=stat) &
                            index(1,i), index(2,i), data(i)
                        if (stat /= 0) then
                            msg = "Error writing array element (" // to_string(i) // ")"
                            exit catch
                        end if
                    end do
                end if
            end if
        end block catch

        close(io)

        if(stat/=0) then
            call mm_fail_process(iostat = iostat, iomsg = iomsg, code = stat,&
                message = "Failed to save Matrix Market file '" // filename // "': " // msg)
                return
        end if

        if (present(iomsg) .and. allocated(msg)) call move_alloc(msg, iomsg)
    end subroutine

    !> Write Matrix Market header
    subroutine write_mm_header(io, format, field, symmetry, nrows, ncols, nnz, &
                              comment, iostat, iomsg)
        integer, intent(in) :: io
        character(len=*), intent(in) :: format, field, symmetry
        integer, intent(in) :: nrows, ncols
        integer, intent(in), optional :: nnz
        character(len=*), intent(in), optional :: comment
        integer, intent(out) :: iostat
        character(len=:), allocatable, intent(out) :: iomsg

        integer :: stat
        character(len=*), parameter :: iso_date_fmt = '(I4.4,"-",I2.2,"-",I2.2)'
        integer :: date_values(8)

        iostat = 0

        ! Write banner line
        write(io, '(A)', iostat=stat) MM_BANNER // " " // MM_MATRIX // " " // &
              format // " " // field // " " // symmetry
        if (stat /= 0) then
            iostat = stat
            iomsg = "Error writing Matrix Market banner"
            return
        end if

        ! Write comments (including optional header_info and generation info)
        call date_and_time(values=date_values)
        write(io, '(A)', iostat=stat) "% Generated by Fortran stdlib on " // &
              to_string(date_values(1)) // "-" // &
              to_string(date_values(2)) // "-" // &
              to_string(date_values(3))
        if (stat /= 0) then
            iostat = stat
            iomsg = "Error writing comment line"
            return
        end if

        if (present(comment)) then
            if(len_trim(comment) > 0) then
                write(io, '(A)', iostat=stat) "% " // trim(comment)
                if (stat /= 0) then
                    iostat = stat
                    iomsg = "Error writing header info"
                    return
                end if
            end if
        end if

        ! Write size line
        if (format == MM_COORDINATE) then
            write(io, '(I0,1X,I0,1X,I0)', iostat=stat) nrows, ncols, nnz
        else
            write(io, '(I0,1X,I0)', iostat=stat) nrows, ncols
        end if

        if (stat /= 0) then
            iostat = stat
            iomsg = "Error writing matrix dimensions"
            return
        end if
    end subroutine write_mm_header

    subroutine mm_fail_process(code, message, iostat, iomsg)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg
        integer, intent(in) :: code
        character(*), intent(in) :: message

        if (present(iostat)) iostat = code
        if (present(iomsg)) then
            iomsg = message
        else
            call error_stop(message)
        end if
    end subroutine

end submodule stdlib_io_mm_save