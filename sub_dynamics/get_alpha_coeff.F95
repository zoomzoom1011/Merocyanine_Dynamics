!***************************************************************************************!
!    Get the initial state
!***************************************************************************************!
subroutine get_alpha_coeff()
    use common_variables
    implicit none
    
    integer:: j,i, state_a, n
    real*8:: coef, rho
    
    if ( .not. allocated( alpha_coeff ) ) allocate( alpha_coeff(sys_kount) )
    alpha_coeff = 0.d0
    rho = (1.d0-nz/dsqrt(nz**2+4*tz**2))/2.d0
    print*, 'rho is: ', rho
    ! print*, rho
    do j = 1, sys_kount
    do i = 1, sys_kount
        coef = 1.d0
        do n = 1, nmax
            state_a = sys_state(i,n)%es_state
            !set the initial state as sqrt(rho)N+sqrt(1-rho)Z on molecule 1, and all others in the 
            !MOLECULAR ground state -sqrt(1-rho)N+sqrt(rho)Z
            !find the coefficient for each state

            !put the first molecule in sqrt(rho)N+sqrt(1-rho)Z
            ! print*, state_a
            if ( n == 1 ) then
                if ( state_a == es_n ) then
                    coef = coef*dsqrt(rho)
                else if ( state_a == es_z ) then
                    coef = coef*dsqrt(1.d0-rho)
                end if
                ! print*, coef
            !put every other molecule in the ground state -sqrt(1-rho)N+sqrt(rho)Z
            else
                
                if ( state_a == es_n ) then
                    coef = -coef*dsqrt(1.d0-rho)
                else if ( state_a == es_z ) then
                    coef = coef*dsqrt(rho)
                end if
                ! print*, coef
            end if
            
            
        end do
        ! print*, coef
        alpha_coeff(j) = alpha_coeff(j) + coef*sys_h(i, j)
    end do
    end do
    
    ! print*,alpha_coeff
    ! if ( sum(alpha_coeff**2) - 1.d0 > 1.d-5 ) then        
        ! print*, 'Alpha_coeff sum: ', sum(alpha_coeff**2)
        ! print*, 'Something is wrong...aborting'
    ! end if
            
end subroutine