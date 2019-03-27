!***************************************************************************************!
!    Get the current state
!***************************************************************************************!
subroutine get_current_state()
    use common_variables
    implicit none

    integer:: j    
    complex*16:: modulate
    
    if ( .not. allocated( current_state ) ) allocate( current_state(sys_kount) )
    current_state = complex_zero
    
    do j = 1, sys_kount
        modulate = cdexp((-1.d0*img/hbar)*sys_eval(j)*current_t*time_step)
        current_state = current_state + sys_h(:,j)*alpha_coeff(j)*modulate
    end do

end subroutine