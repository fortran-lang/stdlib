module stdlib_experimental_sparse
use stdlib_experimental_kinds, only: dp
implicit none
private
public coo2dense, dense2coo, getnnz, coo2csr, coo2csc, &
    csr_has_canonical_format, csr_sum_duplicates, csr_sort_indices, &
    coo2csr_canonical, csr_matvec, csr_getvalue

contains

! Dense

subroutine dense2coo(B, Ai, Aj, Ax)
real(dp), intent(in) :: B(:, :)
integer, intent(out) :: Ai(:), Aj(:)
real(dp), intent(out) :: Ax(:)
integer :: i, j, idx
idx = 1
do j = 1, size(B, 2)
    do i = 1, size(B, 1)
        if (abs(B(i, j)) < tiny(1._dp)) cycle
        Ai(idx) = i
        Aj(idx) = j
        Ax(idx) = B(i, j)
        idx = idx + 1
    end do
end do
end subroutine

integer function getnnz(B) result(nnz)
real(dp), intent(in) :: B(:, :)
integer :: i, j
nnz = 0
do j = 1, size(B, 2)
    do i = 1, size(B, 1)
        if (abs(B(i, j)) < tiny(1._dp)) cycle
        nnz = nnz + 1
    end do
end do
end function

! COO

subroutine coo2dense(Ai, Aj, Ax, B)
integer, intent(in) :: Ai(:), Aj(:)
real(dp), intent(in) :: Ax(:)
real(dp), intent(out) :: B(:, :)
integer :: n
B = 0
do n = 1, size(Ai)
    B(Ai(n), Aj(n)) = B(Ai(n), Aj(n)) + Ax(n)
end do
end subroutine

subroutine coo2csr(Ai, Aj, Ax, Bp, Bj, Bx)
! Converts from COO (Ai, Aj, Ax) into CSR (Bp, Bj, Bx)
! Row and column indices are *not* assumed to be ordered.
! Duplicate entries are carried over to the CSR representation.
integer, intent(in) :: Ai(:), Aj(:)
real(dp), intent(in) :: Ax(:)
integer, intent(out) :: Bp(:), Bj(:)
real(dp), intent(out) :: Bx(:)
integer :: n, i, n_row, nnz, cumsum, temp, row, dest
n_row = size(Bp)-1
nnz = size(Ai)
Bp = 0
forall(n = 1:nnz) Bp(Ai(n)) = Bp(Ai(n)) + 1
cumsum = 1
do i = 1, n_row
    temp = Bp(i)
    Bp(i) = cumsum
    cumsum = cumsum + temp
end do
do n = 1, nnz
    row = Ai(n)
    dest = Bp(row)
    Bj(dest) = Aj(n)
    Bx(dest) = Ax(n)
    Bp(row) = Bp(row) + 1
end do
Bp(2:) = Bp(:n_row)
Bp(1) = 1
end subroutine

subroutine coo2csc(Ai, Aj, Ax, Bp, Bi, Bx)
! Converts from COO (Ai, Aj, Ax) into CSC (Bp, Bi, Bx)
! Row and column indices are *not* assumed to be ordered.
! Duplicate entries are carried over to the CSC representation.
integer, intent(in) :: Ai(:), Aj(:)
real(dp), intent(in) :: Ax(:)
integer, intent(out) :: Bp(:), Bi(:)
real(dp), intent(out) :: Bx(:)
! Calculate CSR of the transposed matrix:
call coo2csr(Aj, Ai, Ax, Bp, Bi, Bx)
end subroutine

subroutine coo2csr_canonical(Ai, Aj, Ax, Bp, Bj, Bx, verbose)
! Converts from COO (Ai, Aj, Ax) into CSR (Bp, Bj, Bx)
! Row and column indices are *not* assumed to be ordered.
! Duplicate entries are summed up and the indices are ordered.
integer, intent(in) :: Ai(:), Aj(:)
real(dp), intent(in) :: Ax(:)
integer, allocatable, intent(out) :: Bp(:), Bj(:)
real(dp), allocatable, intent(out) :: Bx(:)
logical, optional, intent(in) :: verbose
integer :: Bj_(size(Ai))
real(dp) :: Bx_(size(Ai))
integer :: nnz
logical :: verbose_
verbose_ = .false.
if (present(verbose)) verbose_ = verbose
allocate(Bp(maxval(Ai)+1))
if (verbose_) print *, "coo2csr"
call coo2csr(Ai, Aj, Ax, Bp, Bj_, Bx_)
if (verbose_) print *, "csr_sort_indices"
call csr_sort_indices(Bp, Bj_, Bx_)
if (verbose_) print *, "csr_sum_duplicates"
call csr_sum_duplicates(Bp, Bj_, Bx_)
if (verbose_) print *, "done"
nnz = Bp(size(Bp))-1
allocate(Bj(nnz), Bx(nnz))
Bj = Bj_(:nnz)
Bx = Bx_(:nnz)
end subroutine

! CSR

logical function csr_has_canonical_format(Ap, Aj) result(r)
! Determine whether the matrix structure is canonical CSR.
! Canonical CSR implies that column indices within each row
! are (1) sorted and (2) unique.  Matrices that meet these
! conditions facilitate faster matrix computations.
integer, intent(in) :: Ap(:), Aj(:)
integer :: i, j
r = .false.
do i = 1, size(Ap)-1
    if (Ap(i) > Ap(i+1)) return
    do j = Ap(i)+1, Ap(i+1)-1
        if (Aj(j-1) >= Aj(j)) return
    end do
end do
r = .true.
end function

subroutine csr_sum_duplicates(Ap, Aj, Ax)
! Sum together duplicate column entries in each row of CSR matrix A
! The column indicies within each row must be in sorted order.
! Explicit zeros are retained.
! Ap, Aj, and Ax will be modified *inplace*
integer, intent(inout) :: Ap(:), Aj(:)
real(dp), intent(inout) :: Ax(:)
integer :: nnz, r1, r2, i, j, jj
real(dp) :: x
nnz = 1
r2 = 1
do i = 1, size(Ap) - 1
    r1 = r2
    r2 = Ap(i+1)
    jj = r1
    do while (jj < r2)
        j = Aj(jj)
        x = Ax(jj)
        jj = jj + 1
        do while (jj < r2)
            if (Aj(jj) == j) then
                x = x + Ax(jj)
                jj = jj + 1
            else
                exit
            end if
        end do
        Aj(nnz) = j
        Ax(nnz) = x
        nnz = nnz + 1
    end do
    Ap(i+1) = nnz
end do
end subroutine

subroutine csr_sort_indices(Ap, Aj, Ax)
! Sort CSR column indices inplace
integer, intent(inout) :: Ap(:), Aj(:)
real(dp), intent(inout) :: Ax(:)
integer :: i, r1, r2, l, idx(size(Aj))
do i = 1, size(Ap)-1
    r1 = Ap(i)
    r2 = Ap(i+1)-1
    l = r2-r1+1
    idx(:l) = iargsort_quicksort(Aj(r1:r2))
    Aj(r1:r2) = Aj(r1+idx(:l)-1)
    Ax(r1:r2) = Ax(r1+idx(:l)-1)
end do
end subroutine

function csr_matvec(Ap, Aj, Ax, x) result(y)
! Compute y = A*x for CSR matrix A and dense vectors x, y
integer, intent(in) :: Ap(:), Aj(:)
real(dp), intent(in) :: Ax(:), x(:)
real(dp) :: y(size(Ap)-1)
integer :: i
!$omp parallel default(none) shared(Ap, Aj, Ax, x, y) private(i)
!$omp do
do i = 1, size(Ap)-1
    y(i) = dot_product(Ax(Ap(i):Ap(i+1)-1), x(Aj(Ap(i):Ap(i+1)-1)))
end do
!$omp end do
!$omp end parallel
end function

integer function lower_bound(A, val) result(i)
! Returns the lowest index "i" into the sorted array A so that A(i) >= val
! It uses bisection.
integer, intent(in) :: A(:), val
integer :: l, idx
if (A(1) >= val) then
    i = 1
    return
end if
if (A(size(A)) < val) then
    i = size(A)+1
    return
end if
l = 1
i = size(A)
! Now we always have A(l) < val; A(i) >= val and we must make sure that "i" is
! the lowest possible such index.
do while (l + 1 < i)
    idx = (l+i) / 2
    if (A(idx) < val) then
        l = idx
    else
        i = idx
    end if
end do
end function


real(dp) function csr_getvalue(Ap, Aj, Ax, i, j) result(r)
! Returns A(i, j) where the matrix A is given in the CSR format using
! (Ap, Aj, Ax) triple. Assumes A to be in canonical CSR format.
integer, intent(in) :: Ap(:), Aj(:)
real(dp), intent(in) :: Ax(:)
integer, intent(in) :: i, j
integer :: row_start, row_end, offset
row_start = Ap(i)
row_end = Ap(i+1)-1
offset = lower_bound(Aj(row_start:row_end), j) + row_start - 1
if (offset <= row_end) then
    if (Aj(offset) == j) then
        r = Ax(offset)
        return
    end if
end if
r = 0
end function

pure elemental subroutine swap_int(x,y)
  integer, intent(in out) :: x,y
  integer :: z
  z = x
  x = y
  y = z
end subroutine

pure subroutine interchange_sort_map_int(vec,map)
  integer, intent(in out) :: vec(:)
  integer, intent(in out) :: map(:)
  integer :: i,j
  do i = 1,size(vec) - 1
     j = minloc(vec(i:),1)
     if (j > 1) then
        call swap_int(vec(i),vec(i + j - 1))
        call swap_int(map(i),map(i + j - 1))
     end if
  end do
end subroutine

pure function iargsort_quicksort(vec_) result(map)
  integer, intent(in) :: vec_(:)
  integer :: map(size(vec_))
  integer, parameter :: levels = 300
  integer, parameter :: max_interchange_sort_size = 20
  integer :: i,left,right,l_bound(levels),u_bound(levels)
  integer :: pivot
  integer :: vec(size(vec_))

  vec = vec_

  forall(i=1:size(vec)) map(i) = i

  l_bound(1) = 1
  u_bound(1) = size(vec)
  i = 1
  do while(i >= 1)
     left = l_bound(i)
     right = u_bound(i)
     if (right - left < max_interchange_sort_size) then
        if (left < right) call interchange_sort_map_int(vec(left:right),map(left:right))
        i = i - 1
     else
        pivot = (vec(left) + vec(right)) / 2
        left = left - 1
        right = right + 1
        do
           do
              left = left + 1
              if (vec(left) >= pivot) exit
           end do
           do
              right = right - 1
              if (vec(right) <= pivot) exit
           end do
           if (left < right) then
              call swap_int(vec(left),vec(right))
              call swap_int(map(left),map(right))
           elseif(left == right) then
              if (left == l_bound(i)) then
                 left = left + 1
              else
                 right = right - 1
              end if
              exit
           else
              exit
           end if
           end do
           u_bound(i + 1) = u_bound(i)
           l_bound(i + 1) = left
           u_bound(i) = right
           i = i + 1
     end if
  end do
end function

end module
