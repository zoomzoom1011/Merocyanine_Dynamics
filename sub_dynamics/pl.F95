!*********************************************************************!
!        try to find the lowest symmetric and as state         !
!*********************************************************************!
subroutine pl()
    use common_variables
    implicit none
        
    integer:: fno
    character*256:: fname
    integer:: state, hx
    real*8:: coeff
    real*8,allocatable :: pl_shift(:)
    integer:: state_a, n
    
    allocate(pl_shift(sys_kount))
    
    fno = 101
    fname = trim(task_title)//'_shift.dat'  
    open( unit = fno, file = fname )
    write(fno, *) 'state energy shift'
    pl_shift = 0.d0
    ! do state = 1, sys_kount
        ! coeff = 0.d0
        ! do hx = 1, sys_kount
            ! coeff = coeff + sys_h(hx,1) * sys_h(hx,state)
            ! ! print*, coeff, sys_h(hx,1), sys_h(hx,state)
        ! end do

        ! do hx = 1, sys_kount
            ! pl_shift(state) = pl_shift(state) + sys_h(hx,state)**2
        ! end do
        ! pl_shift(state) = coeff**2 * pl_shift(state)
        
        ! write(fno, '(i4, 2f14.5)') state, (sys_eval(state)-sys_eval(1))*hw, pl_shift(state)
    ! end do
        do state = 1, sys_kount
        do hx = 1, sys_kount
            coeff = 0.d0
        do n = 1, nmax
            state_a = sys_state(hx,n)%es_state
            ! the electronic energy
            coeff = coeff + es_lambda_z(state_a)
        end do
            pl_shift(state) = pl_shift(state) + sys_h(hx,state)**2* coeff
        end do
            write(fno, '(i4, 2f14.5)') state, (sys_eval(state)-sys_eval(1))*hw, pl_shift(state)
        end do
    close(fno)
    
    do state = 2, sys_kount
        if(pl_shift(state)>2*pl_shift(state-1)) then
            pl_start = state
            exit
        end if
    end do
    ! print*, pl_start
end subroutine