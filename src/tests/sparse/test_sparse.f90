program test_sparse
use stdlib_experimental_kinds, only: dp
use stdlib_experimental_sparse, only: coo2dense, dense2coo, getnnz, coo2csr, &
    csr_has_canonical_format, csr_sum_duplicates, csr_sort_indices, &
    coo2csr_canonical, csr_matvec, csr_getvalue
use stdlib_experimental_error, only: check
implicit none

real(dp), allocatable :: A(:, :), B(:, :), Ax(:), Bx(:), x(:), y(:)
integer, allocatable :: Ai(:), Aj(:), Bp(:), Bj(:)
integer :: nnz

! Tests on square matrix for:
! * dense <-> COO
! * COO -> CSR
allocate(A(4, 4), B(4, 4))
A = reshape([1, 7, 0, 0, &
             0, 2, 8, 0, &
             5, 0, 3, 9, &
             0, 6, 0, 4], [4, 4], order=[2, 1])
nnz = getnnz(A)
allocate(Ai(nnz), Aj(nnz), Ax(nnz))
call dense2coo(A, Ai, Aj, Ax)
call check(all(Ai == [1, 3, 1, 2, 4, 2, 3, 3, 4]))
call check(all(Aj == [1, 1, 2, 2, 2, 3, 3, 4, 4]))
call check(all(abs(Ax - [1, 5, 7, 2, 6, 8, 3, 9, 4]) < 1e-12_dp))
call coo2dense(Ai, Aj, Ax, B)
call check(all(abs(A-B) < 1e-12_dp))
allocate(Bp(maxval(Ai)+1), Bj(size(Ai)), Bx(size(Ai)))
call coo2csr(Ai, Aj, Ax, Bp, Bj, Bx)
call check(all(Bp-1 == [0, 2, 4, 7, 9]))
call check(all(Bj-1 == [0, 1, 1, 2, 0, 2, 3, 1, 3]))
call check(all(abs(Bx - [1, 7, 2, 8, 5, 3, 9, 6, 4]) < 1e-12_dp))
call check(csr_has_canonical_format(Bp, Bj))
deallocate(A, B, Ai, Aj, Ax, Bp, Bj, Bx)


! Tests on rectangular matrix for:
! * dense <-> COO
! * COO -> CSR
allocate(A(4, 5), B(4, 5))
A = reshape([1, 7, 0, 0, 1, &
             0, 2, 8, 0, 2, &
             5, 0, 3, 9, 3, &
             0, 6, 0, 4, 4], [4, 5], order=[2, 1])
nnz = getnnz(A)
allocate(Ai(nnz), Aj(nnz), Ax(nnz))
call dense2coo(A, Ai, Aj, Ax)
call check(all(Ai == [1, 3, 1, 2, 4, 2, 3, 3, 4, 1, 2, 3, 4]))
call check(all(Aj == [1, 1, 2, 2, 2, 3, 3, 4, 4, 5, 5, 5, 5]))
call check(all(abs(Ax - [1, 5, 7, 2, 6, 8, 3, 9, 4, 1, 2, 3, 4]) < 1e-12_dp))
call coo2dense(Ai, Aj, Ax, B)
call check(all(abs(A-B) < 1e-12_dp))
allocate(Bp(maxval(Ai)+1), Bj(size(Ai)), Bx(size(Ai)))
call coo2csr(Ai, Aj, Ax, Bp, Bj, Bx)
call check(all(Bp == [1, 4, 7, 11, 14]))
call check(all(Bj == [1, 2, 5, 2, 3, 5, 1, 3, 4, 5, 2, 4, 5]))
call check(all(abs(Bx - [1, 7, 1, 2, 8, 2, 5, 3, 9, 3, 6, 4, 4]) < 1e-12_dp))
call check(csr_has_canonical_format(Bp, Bj))
allocate(x(5), y(4))
x = [1, 2, 3, 4, 5]
y = csr_matvec(Bp, Bj, Bx, x)
call check(all(abs(y-matmul(A, x)) < 1e-12_dp))
deallocate(A, B, Ai, Aj, Ax, Bp, Bj, Bx, x, y)


! Tests on rectangular matrix for:
! * COO -> dense
! * COO -> CSR
allocate(Ai(6), Aj(6), Ax(6), A(3, 4), B(3, 4))
Ai = [1, 2, 3, 1, 2, 2]
Aj = [1, 3, 2, 1, 4, 3]
Ax = [1, 2, 3, 4, 1, 5]
call coo2dense(Ai, Aj, Ax, B)
A = reshape([5, 0, 0, 0, &
             0, 0, 7, 1, &
             0, 3, 0, 0], [3, 4], order=[2, 1])
call check(all(abs(A-B) < 1e-12_dp))
allocate(Bp(maxval(Ai)+1), Bj(size(Ai)), Bx(size(Ai)))
call coo2csr(Ai, Aj, Ax, Bp, Bj, Bx)
call check(all(Bp == [1, 3, 6, 7]))
call check(all(Bj == [1, 1, 3, 4, 3, 2]))
call check(all(abs(Bx - [1, 4, 2, 1, 5, 3]) < 1e-12_dp))
call check(.not. csr_has_canonical_format(Bp, Bj))
call csr_sort_indices(Bp, Bj, Bx)
call check(all(Bp == [1, 3, 6, 7]))
call check(all(Bj == [1, 1, 3, 3, 4, 2]))
call check(all(abs(Bx - [1, 4, 2, 5, 1, 3]) < 1e-12_dp))
call csr_sum_duplicates(Bp, Bj, Bx)
nnz = Bp(size(Bp))-1
call check(all(Bp == [1, 2, 4, 5]))
call check(all(Bj(:nnz) == [1, 3, 4, 2]))
call check(all(abs(Bx(:nnz) - [5, 7, 1, 3]) < 1e-12_dp))
call check(csr_has_canonical_format(Bp, Bj(:nnz)))

call coo2csr_canonical(Ai, Aj, Ax, Bp, Bj, Bx)
call check(all(Bp == [1, 2, 4, 5]))
call check(all(Bj == [1, 3, 4, 2]))
call check(all(abs(Bx - [5, 7, 1, 3]) < 1e-12_dp))
call check(csr_has_canonical_format(Bp, Bj))

call check(abs(csr_getvalue(Bp, Bj, Bx, 1, 1) - 5) < tiny(1._dp))
call check(abs(csr_getvalue(Bp, Bj, Bx, 1, 2) - 0) < tiny(1._dp))
call check(abs(csr_getvalue(Bp, Bj, Bx, 1, 3) - 0) < tiny(1._dp))
call check(abs(csr_getvalue(Bp, Bj, Bx, 1, 4) - 0) < tiny(1._dp))
call check(abs(csr_getvalue(Bp, Bj, Bx, 2, 1) - 0) < tiny(1._dp))
call check(abs(csr_getvalue(Bp, Bj, Bx, 2, 2) - 0) < tiny(1._dp))
call check(abs(csr_getvalue(Bp, Bj, Bx, 2, 3) - 7) < tiny(1._dp))
call check(abs(csr_getvalue(Bp, Bj, Bx, 2, 4) - 1) < tiny(1._dp))
call check(abs(csr_getvalue(Bp, Bj, Bx, 3, 1) - 0) < tiny(1._dp))
call check(abs(csr_getvalue(Bp, Bj, Bx, 3, 2) - 3) < tiny(1._dp))
call check(abs(csr_getvalue(Bp, Bj, Bx, 3, 3) - 0) < tiny(1._dp))
call check(abs(csr_getvalue(Bp, Bj, Bx, 3, 4) - 0) < tiny(1._dp))

deallocate(A, B, Ai, Aj, Ax, Bp, Bj, Bx)

end
