!*********************************************************************!
!        calculate the absorption spectrum oscillator strength       !
!*********************************************************************!
subroutine abs_osc()
    use common_variables
    implicit none

    integer state, hx
!    real*8 valmax1, valmax2
!    integer state1, state2
!    real*8 val1, val2
    character*256 f_name
    integer f_no
    
    if(.not. allocated(ab_osc_x)) allocate (ab_osc_x(sys_kount))
    if(.not. allocated(ab_osc_y)) allocate (ab_osc_y(sys_kount))
    if(.not. allocated(ab_osc_z)) allocate (ab_osc_z(sys_kount))
    ab_osc_x = 0.d0
    ab_osc_y = 0.d0
    ab_osc_z = 0.d0
    
    do state = 2, sys_kount
        do hx = 1, sys_kount
            ab_osc_x(state) = ab_osc_x(state) + ux(hx) * sys_h(hx,state) * sys_h(hx,1)
            ab_osc_y(state) = ab_osc_y(state) + uy(hx) * sys_h(hx,state) * sys_h(hx,1)
            ab_osc_z(state) = ab_osc_z(state) + uz(hx) * sys_h(hx,state) * sys_h(hx,1)
        end do
    end do
    
    ! print*,ux
    ! print*,uy
    ! print*,uz
!    valmax1 = 0.d0
!    valmax2 = 0.d0
!    
!    do state = 2, kount
!            val1 = abs(h(2, state))
!            if(val1> valmax1) then
!                    valmax1 = val1
!                    state1 = state
!            end if
!            
!            val1 = abs(h(vibmax +2, state))
!            if(val1> valmax1) then
!                    valmax1 = val1
!                    state1 = state
!            end if
!            
!                val2 = abs(h(vibmax +3, state))
!            if(val2> valmax2) then
!                    valmax2 = val2
!                    state2 = state
!            end if
!    end do
!        
!        print*,osc_x(state1)**2
!    print*,osc_x(state2)**2
!    print'(16(f4.2," "))',ab_osc_x
!       print*,ab_osc_x
    ab_osc_x = ab_osc_x ** 2
    ab_osc_y = ab_osc_y ** 2
    ab_osc_z = ab_osc_z ** 2
    
    f_no = 100
    f_name = trim(task_title)//'_osc.dat'
    
    open(unit=f_no, file=f_name)
        
        do state = 2, sys_kount
            write(f_no,'(i8,2f14.5)') state,(sys_eval(state)-sys_eval(1))*hw, ab_osc_x(state)        
        end do
    
    close(f_no)

end subroutine