module lfortran_intrinsic_string
    use, intrinsic :: iso_fortran_env, only: i64 => int64
implicit none

contains

integer elemental function len_trim(string) result(r)
    character(len=*), intent(in) :: string
    r = len(string)
    if (r == 0) return
    do while(string(r:r) == " ")
        r = r - 1
        if (r == 0) exit
    end do
end function

function trim(x) result(r)
    character(len=*),intent(in) :: x
    character(len=len_trim(x)) :: r
    ! This does not work yet in LFortran:
    !r = x(1:len(r))
    ! So we do this workaroud that works:
    integer :: i
    do i = 1, len(r)
        r(i:i) = x(i:i)
    end do
end function

function new_line(c) result(r)
    character(len=1), intent(in) :: c
    character(len=1) :: r
    r = '\n'
end function

subroutine date_and_time(date, time, zone, values)
    character(len=*), intent(out), optional :: date, time, zone
    integer, intent(out), optional :: values(8)
end subroutine

function scan_util(string, set, back) result(r)
    character(len=*) :: string
    character(len=*) :: set
    logical, optional :: back
    integer :: r, i, j, start, end, inc
    r = 0
    start = 1
    end = len(string)
    inc = 1
    if (present(back)) then
        if (back .eqv. .true.) then
            start = end
            end = 1
            inc = -1
        end if
    end if
    do i = start, end, inc
        do j = 1, len(set)
            if (string(i:i) == set(j:j)) then
                r = i
                exit
            end if
        end do
        if (r /= 0) exit
    end do
end function

function scan_kind4(string, set, back) result(r)
    character(len=*) :: string
    character(len=*) :: set
    logical, optional :: back
    integer :: r
    r = scan_util(string, set, back)
end function

function scan_kind8(string, set, back) result(r)
    character(len=*) :: string
    character(len=*) :: set
    logical, optional :: back
    integer(8) :: r
    r = scan_util(string, set, back)
end function

end module
