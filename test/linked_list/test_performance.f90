program test_link
  use stdlib_linked_list
  implicit none

  type struct
      integer:: a=1,b=2,c=3
      double precision::d=5
  end type struct
  type(struct):: Vel2

  type vector
      double precision, dimension(3):: vec
  end type vector
  type(vector)::Vel

  type(linked_list):: L
  integer :: i,j,length
  real    :: T1,T2,F, r
  integer :: cnt1, cnt2, count_rate

  class(*), pointer :: data

  do i=1,size(Vel%vec)
      Vel%vec(i) = i
  end do
  ! !-------------
  ! !Append items
  ! !-------------
  print*, "Length Of Required List"
  read(*,*) length

  call system_clock( cnt1, count_rate = count_rate )
  call cpu_time(T1)
  do i=1,length
    call L%append(i)
  end do
  call cpu_time(T2)
  call system_clock( cnt2, count_rate = count_rate )
  i = 1

  write(*,*) T2-T1, (cnt2 - cnt1)/real(count_rate)

  call system_clock( cnt1, count_rate = count_rate )
  call cpu_time(T1)
  do while (i<=100)
    call random_number( r )
    j = r*length
    data => L%get(j)
    select type (data)
    type is (integer)
    end select
    i = i+1
  end do
  call cpu_time(T2)
  call system_clock( cnt2, count_rate = count_rate )

  write(*,*) (T2-T1), (cnt2 - cnt1)/real(count_rate)
  write(*,*)'Done'

  !-------------
  !Destroy the list and frees the memmory
  !-------------
  call system_clock( cnt1, count_rate = count_rate )
  call cpu_time(T1)
  call L%destroy()
  call cpu_time(T2)
  call system_clock( cnt2, count_rate = count_rate )

  write(*,*) T2-T1, (cnt2 - cnt1)/real(count_rate)

end program test_link
