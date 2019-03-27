!***************************************************************************************!
!    Get the population of excitation in each mode
!***************************************************************************************!
subroutine get_population()
    use common_variables
    implicit none

    integer:: state1, state2
    integer:: state_a, vib_a
    integer:: state_b, vib_b
    integer:: diff, diffn
    real*8:: prob(nmax)
    character*200:: fname
    real*8:: volap, rho
    integer:: fno, i, n1, n2
    real*8, allocatable:: temp_vector(:)
    real*8:: coef
    
    allocate(temp_vector(sys_kount))

    !calculate the probability of being in sqrt(rho)N+sqrt(1-rho)Z
    ! the operator is (C^2|N><N|+a^2|Z><Z|+ac|N><Z|+ac|Z><N|)
    prob = 0.d0
    rho = (1.d0-nz/dsqrt(nz**2+4*tz**2))/2.d0
    !go over all basis states
    !get the working matrix for non-vibration 
    
    do n1 = 1, nmax !excited molecule measurement
    do i = 1, sys_kount
        coef = 1.d0
    do n2 = 1, nmax
        state_a = sys_state(i,n2)%es_state
        ! if(current_t ==0 .and. n1==1) print*, state_a
        if(n1 == n2) then !in excited state
            if ( state_a == es_n ) then
                coef = coef*dsqrt(rho)
            else if ( state_a == es_z ) then
                coef = coef*dsqrt(1.d0-rho)
            end if
            ! if(current_t ==0) print*, coef
        else
            if ( state_a == es_n ) then
                coef = -coef*dsqrt(1.d0-rho)
            else if ( state_a == es_z ) then
                coef = coef*dsqrt(rho)
            end if
            ! if(current_t ==0) print*, coef
        end if
    end do    
        temp_vector(i) = coef
    end do
        ! if(current_t ==0 .and. n1==1) print*, temp_vector
        !operator
        ! temp_matrix = matmul( RESHAPE(temp_vector,(/sys_kount,1/)), RESHAPE(temp_vector,(/1,sys_kount/)) )
        ! if(current_t==0) print*, temp_matrix
        !go over all states
        do state1 = 1, sys_kount
        do state2 = 1, sys_kount
            prob(n1) = prob(n1) + dconjg(current_state(state1))* &
            temp_vector(state1)*temp_vector(state2)*current_state(state2)
        end do
        end do
        
!         if(prob(n1)<0.01d0) prob(n1)=0.d0
    end do

    
    fno = 29
    fname = trim(task_title)//'_expop.dat'
    if ( current_t == 0 ) then 
        open( unit = fno, file = trim(fname) )
        write( fno, * ) 'num_nmax'
        write( fno, * ) nmax
        write( fno, * ) 'num_steps'
        write( fno, * ) t_max
        write( fno, * ) 'trajectories'
        write( fno, * ) 'time(s) ', (i, i = 1, nmax)
        close( fno )
    end if
    open( unit = fno, file = trim(fname), position = 'append' )
    write( fno, * ) current_t*time_step, (prob(i), i=1, nmax)
    close( fno )
    
    probability(current_t) = prob(nmax)

    ! fno = 29
    ! fname = trim(task_title)//'_time.dat'
    ! do while(.not. first_value)
        ! if(prob(nmax)>0.1) then
            ! open( unit = fno, file = trim(fname) )
                ! write( fno, * ) 'excitation transfer rate'
                ! write( fno, * ) nmax*z_offset/(current_t*time_step)
            ! close( fno )
            ! first_value = .True.
        ! end if
    ! end do

end subroutine