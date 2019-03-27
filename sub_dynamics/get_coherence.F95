!***************************************************************************************!
!    Get the coherence number in molecule
!***************************************************************************************!
subroutine get_coherence()
    use common_variables
    implicit none

    integer:: state1, state2
    integer:: state_a, vib_a
    integer:: state_b, vib_b
    integer:: diff, diffn
    real*8:: prob(nmax)
    character*200:: fname
    real*8:: volap, rho
    integer:: fno, i, n1, n2, n3, r
    real*8, allocatable:: temp_vector1(:)
    real*8, allocatable:: temp_vector2(:)
    real*8:: coef
    real*8:: coherence_number
    real*8:: coherence(0:nmax-1)
    

    allocate(temp_vector1(sys_kount))
    allocate(temp_vector2(sys_kount))

    !calculate the probability of being in sqrt(rho)N+sqrt(1-rho)Z
    ! the operator is (C^2|N><N|+a^2|Z><Z|+ac|N><Z|+ac|Z><N|)
    prob = 0.d0
    rho = (1.d0-nz/dsqrt(nz**2+4*tz**2))/2.d0
    
    do r = 0, nmax-1
    do n1 = 1, nmax !ket
        n2 = n1 + r !bra
        if(n2>nmax .or. n2<1) call bring_inside_aggregate_range(n2)
        
        !go over all basis states to get ket
        do i = 1, sys_kount
            coef = 1.d0
        do n3 = 1, nmax
            state_a = sys_state(i,n3)%es_state
            if(n1 == n3) then !in excited state
                if ( state_a == es_n ) then
                    coef = coef*dsqrt(rho)
                else if ( state_a == es_z ) then
                    coef = coef*dsqrt(1.d0-rho)
                end if
            else
                if ( state_a == es_n ) then
                    coef = -coef*dsqrt(1.d0-rho)
                else if ( state_a == es_z ) then
                    coef = coef*dsqrt(rho)
                end if
            end if
        end do    
            temp_vector1(i) = coef
        end do
    
        !go over all basis states to get bra
        do i = 1, sys_kount
            coef = 1.d0
        do n3 = 1, nmax
            state_a = sys_state(i,n3)%es_state
            if(n2 == n3) then !in excited state
                if ( state_a == es_n ) then
                    coef = coef*dsqrt(rho)
                else if ( state_a == es_z ) then
                    coef = coef*dsqrt(1.d0-rho)
                end if
            else !in ground state
                if ( state_a == es_n ) then
                    coef = -coef*dsqrt(1.d0-rho)
                else if ( state_a == es_z ) then
                    coef = coef*dsqrt(rho)
                end if
            end if
        end do    
            temp_vector2(i) = coef
        end do
    
        !go over all states
        do state1 = 1, sys_kount
        do state2 = 1, sys_kount
            coherence(r) = coherence(r) + dconjg(current_state(state1))* &
                temp_vector1(state1)*temp_vector2(state2)*current_state(state2)
        end do
        end do
    end do
    end do
    ! if(current_t==0) print*, coherence(0)
    coherence_number = sum(coherence)/coherence(0)
    cohere(current_t) = coherence_number
    
    fno = 29
    fname = trim(task_title)//'_coherence.dat'
    if ( current_t == 0 ) then 
        open( unit = fno, file = trim(fname) )
        write( fno, * ) 'time(s) ', 'coherence_number'
        close( fno )
    end if
    open( unit = fno, file = trim(fname), position = 'append' )
        write( fno, * ) current_t*time_step, coherence_number
    close( fno )

end subroutine