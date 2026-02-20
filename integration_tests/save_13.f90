program save_13
  ! Test that save array variables with constant initializers inside
  ! subroutines are correctly initialized (not zeroed out)
  implicit none
  integer, parameter :: k(5) = [-1, 2, 3, 3, 3], ksize = size(k)
  integer :: n, diffs1(ksize-1) = [(abs(k(n)-k(n-1)), n=2,ksize)]
  if (diffs1(1) /= 3) error stop
  if (diffs1(2) /= 1) error stop
  if (diffs1(3) /= 0) error stop
  if (diffs1(4) /= 0) error stop
  call subprogram
contains
  subroutine subprogram
    integer, save :: n, diffs2(ksize-1) = [(abs(k(n)-k(n-1)), n=2,ksize)]
    if (diffs2(1) /= 3) error stop
    if (diffs2(2) /= 1) error stop
    if (diffs2(3) /= 0) error stop
    if (diffs2(4) /= 0) error stop
  end subroutine subprogram
end program save_13
