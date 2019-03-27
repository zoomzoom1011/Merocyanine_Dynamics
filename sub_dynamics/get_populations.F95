!***************************************************************************************!
!    Get the current state
!***************************************************************************************!
subroutine get_populations()
    use common_variables
    implicit none

    integer:: state1, state2
    integer:: state_a, vib_a
    integer:: state_b, vib_b
    integer:: diff, diffn
    real*8:: prob(nmax)
    character*200:: fname
    real*8:: volap, rho
    integer:: fno, i, n1

    !calculate the probability of being in sqrt(rho)N+sqrt(1-rho)Z
    ! the operator is (C^2|N><N|+a^2|Z><Z|+ac|N><Z|+ac|Z><N|)
    prob = 0.d0
    rho = (1.d0-nz/dsqrt(nz**2+4*tz**2))/2.d0
    !go over all basis states
    do state1 = 1, sys_kount
    do n1 = 1, nmax
        state_a = sys_state(state1,n1)%es_state
        !calculate the probability
        if (state_a == es_n) then
            prob(n1) = prob(n1)+ &
               (dconjg(current_state(state1))*current_state(state1))*rho
        else if (state_a == es_z) then
            prob(n1) = prob(n1)+ &
               (dconjg(current_state(state1))*current_state(state1))*(1-rho)
        end if
    end do
    end do
    
    do state1 = 1, sys_kount
    do state2 = 1, sys_kount
        if(state1 == state2) cycle
        diff = 0
        do n1 = 1, nmax
            state_a = sys_state(state1,n1)%es_state
            vib_a = sys_state(state1,n1)%vib
            state_b = sys_state(state2,n1)%es_state
            vib_b = sys_state(state2,n1)%vib
                    
            if ( vib_a .ne. vib_b .or. & 
                 state_a .ne. state_b ) then
            
                diffn = n1
                diff = diff + 1
            end if
            if ( diff > 1 ) cycle
        end do
        
        !at max, only one molecule can have a different configuration
        if ( diff .ne. 1 ) cycle
        state_a = sys_state(state1,diffn)%es_state
        vib_a = sys_state(state1,diffn)%vib
        state_b = sys_state(state2,diffn)%es_state
        vib_b = sys_state(state2,diffn)%vib
            prob(diffn) = prob(diffn) + & 
            (dconjg(current_state(state1))*current_state(state2))*dsqrt((1-rho)*rho)* &
            volap( es_lambda_z( state_a ),vib_a,es_lambda_z( state_b ), vib_b)
    end do
    end do
        

    
    do n1 = 1, nmax
        if ( prob( n1 ) < 0.d0 ) prob(n1) = 0.d0
    end do    
    
    if ( .not. allocated(probsum) ) then
        allocate (probsum(4,0:t_max))
        probsum = 0.d0
        do i = 0, t_max
            probsum(1,i) = i
        end do
    end if
    probsum(2,current_t) = prob(1)!sum(prob)
    probsum(3,current_t) = prob(nmax)!sum(prob)
    probsum(4,current_t) = sum(prob)!sum(prob)
    
    !now display the evolution on a graph
! !    call slumber( 100 )
    ! call erase()
! !    call center()
    ! call axslen( 2200, 700 )
    ! call axspos( 400, 1850 )
    ! call name('Probability', 'y')
    ! call name('Site', 'x')
    ! call graf(0.d0, nmax+1.d0 , 0.d0, 1.d0, &
              ! 0.d0, 1.05d0, 0.d0, 0.2d0)
    ! call titlin('Exciton Probability', 4)
    ! call title()
    ! call thkcrv( 4 )
    ! call polcrv( 'FBARS' )
    ! call color('BLUE')
    ! call curve(cpos, prob, nmax)
    ! call color('Red')
    ! call endgrf()

    ! call axslen(2200,700)
    ! call axspos(400 ,850)
    ! call name('Probability Sum', 'y')
    ! call name('Timestep', 'x')
    ! call graf(0.0d0, t_max*1.d0 , 0.d0, t_max/5.d0, &
              ! -0.05d0, 1.05d0, 0.0d0, 0.25d0)
    ! call titlin('Probability', 4)
    ! call title()
    ! call thkcrv( 4 )
    ! call polcrv( 'LINEAR' )
    ! call color('RED')
    ! call curve(probsum(1,:), probsum(2,:), current_t+1)
    ! call color('ORANGE')
    ! call curve(probsum(1,:), probsum(3,:), current_t+1)
    ! call color('Green')
    ! call curve(probsum(1,:), probsum(4,:), current_t+1)
    ! call color('WHITE')
    ! call endgrf()
    ! call sendbf
    
    fno = 29
    fname = trim(task_title)//'_expop.dat'
    if ( current_t == 0 ) then 
        open( unit = fno, file = trim(fname) )
        write( fno, * ) 'time(s) ', (i, i = 1, nmax)
        close( fno )
    end if
    open( unit = fno, file = trim(fname), position = 'append' )
    write( fno, * ) current_t*time_step, (prob(i), i=1, nmax)
    
    close( fno )
end subroutine