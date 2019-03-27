!*******************************************************************************!
!   set the fc function for calculation
!*******************************************************************************!
real*8 function volap( lambda1,vib1, lambda2,vib2)
    implicit none

    integer, intent (in) :: vib1, vib2
    real*8, intent(in) :: lambda1, lambda2
    integer:: k
    real*8, external :: factorial
    real*8:: lambda
    
    !get the shift between potential wells
    lambda = lambda2 - lambda1
!    
!    print*,  lambda1,vib1,lambda2,vib2
    volap = 0.d0
    do k = 0, min( vib1, vib2 )
        volap = volap+(-1.d0)**(vib2-k)/        &
                (factorial(vib1-k)*factorial(k)*&
                 factorial(vib2-k))*            &
                 lambda**(vib1+vib2-2*k)
    end do
    volap = volap*dsqrt(1.d0*factorial(vib1)*   &
                             factorial(vib2))*  &
                             dexp(-1.d0*        &
                             lambda**2/2.d0)
    if ( volap /= volap ) then
        print*, 'Volap Error:: volap: ', volap
        print*, 'Aborting'
        stop
    end if
    
!    print*, volap,lambda1,vib1, lambda2,vib2
end function

!it is technically an integer, but real is more convienient
real*8 function factorial( n )
    implicit none
    integer, intent(in):: n
    integer i

    if ( n<0 ) then
            print*, 'Factorial not calculatable for: ', n
            stop
    else
            factorial = 1.0d0
        if( n .ge. 0 ) then
            do i = 2, n
                factorial = factorial * i
            end do
        end if
        end if
end function