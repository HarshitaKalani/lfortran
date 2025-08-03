! Fortran code after applying the pass: implied_do_loops
program test_trim_write
implicit none
integer(4) :: ind
character(len=:, kind=1), dimension(:), allocatable :: arr
integer(4) :: iii
character(len=20, kind=1) :: tmp_line
tmp_line = "Hello"
! deallocate(arr)
allocate(arr(3), source = arr)
ind = lbound(arr, 1)
do iii = 1, 3
    arr(ind) = trim(tmp_line)
    ind = ind + 1
end do
print *, arr
end program test_trim_write
