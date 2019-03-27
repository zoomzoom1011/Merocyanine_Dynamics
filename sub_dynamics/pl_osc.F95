!*********************************************************************!
!                pl spectrum to each state         !
!*********************************************************************!
subroutine pl_osc()
    use common_variables
    implicit none
    
    integer:: statelo, stateup, hx
    character*256 f_name
    integer f_no
    
!    if(.not. allocated(sys_pl_osc)) allocate(sys_pl_osc(sys_kount, sys_kount))
!    sys_pl_osc = 0.d0
    
!    do statelo = 1, sys_kount-1
!    do stateup = statelo+1, sys_kount
!        do state = 1, sys_kount
!            sys_pl_osc(statelo,stateup) = sys_pl_osc(statelo,stateup)+ sys_h(state, statelo)*sys_h(state, stateup)*sys_mu(state,state)
!        end do
!            sys_pl_osc(statelo,stateup) = sys_pl_osc(statelo,stateup) * sys_pl_osc(statelo,stateup) 
!    end do
!    end do
    
    if(.not. allocated(pl_osc_x)) allocate (pl_osc_x(pl_start))
    if(.not. allocated(pl_osc_y)) allocate (pl_osc_y(pl_start))
    if(.not. allocated(pl_osc_z)) allocate (pl_osc_z(pl_start))
    pl_osc_x = 0.d0
    pl_osc_y = 0.d0
    pl_osc_z = 0.d0

    ! do statelo = 1, 8
    ! do stateup = 9, 10
    do statelo = 1, pl_start-1
        do hx = 1, sys_kount
            pl_osc_x(statelo) = pl_osc_x(statelo) + ux(hx) * sys_h(hx,statelo) * sys_h(hx,pl_start)
            pl_osc_y(statelo) = pl_osc_y(statelo) + uy(hx) * sys_h(hx,statelo) * sys_h(hx,pl_start)
            pl_osc_z(statelo) = pl_osc_z(statelo) + uz(hx) * sys_h(hx,statelo) * sys_h(hx,pl_start)
        end do
        ! print*,statelo,stateup, pl_osc_x(statelo,stateup)
    end do
    
    pl_osc_x = pl_osc_x ** 2
    pl_osc_y = pl_osc_y ** 2
    pl_osc_z = pl_osc_z ** 2
    
    f_no = 100
    f_name = trim(task_title)//'_plosc.dat'
    
    open(unit=f_no, file=f_name)
        do statelo = 1, pl_start-1
            write(f_no,'(i8,2f14.5)') statelo,(sys_eval(pl_start)-sys_eval(statelo))*hw, pl_osc_x(statelo)        
        end do
    close(f_no)
    
    ! print*, (sys_eval(pl_start)-sys_eval(1))*hw
end subroutine