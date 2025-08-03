program test_trim_write
  implicit none
  character(len=20) :: tmp_line
  integer :: iii
  tmp_line = "Hello"
  print *,  (trim(tmp_line), iii=1, 3)
end program test_trim_write