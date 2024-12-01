program main
    real:: zmat(5) 
    zmat = 12
    print *, zmat([2,4])
end program

! program main
!     integer, parameter :: RP = kind(0.0D0)
!     integer, parameter :: IK = kind(0)
!     real(RP) :: G(2, 2)
!     integer(IK) :: knew
!     real(RP):: zmat(1:5) 
!     G = planerot(zmat([1, 2]))
!     contains
    
!     function planerot(x) result(G)
!         real(RP), intent(in) :: x(:)
!         real(RP) :: c, s, r
!         real(RP) :: G(2, 2)
!         c = x(1) / r
!         G = reshape([c, -s, s, c], [2, 2])
!     end function
! end program