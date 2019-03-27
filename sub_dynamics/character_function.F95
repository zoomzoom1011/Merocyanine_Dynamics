!*********************************************************************!
!       get the character of each state       !
!*********************************************************************!
subroutine charafxn()
    use common_variables
    implicit none
    
    integer:: fno
    character*256:: fname
    real*8:: fe, ct
    real*8:: ab_osc(sys_kount)
    integer:: state, hx, n1, state_a
    
    fno = 101
    fname = trim(task_title)//'_chara.dat'  
    open( unit = fno, file = fname )
    
    if(ct_state) then
    
    ab_osc = ab_osc_x + ab_osc_y + ab_osc_z
    write(fno, *) 'energy frenkel ct'
    
    do state = 1, sys_kount
        fe = 0.d0
        ct = 0.d0
        do hx = 1, sys_kount
        do n1 = 1, nmax
            state_a = sys_state(hx,n1)%es_state
            if (state_a == es_z) then
                fe = fe + sys_h(hx,state)**2
            else if(state_a == es_a .or. state_a == es_c) then
                ct = ct + sys_h(hx,state)**2
            end if
        end do
        end do
        
        fe = ab_osc(state) * fe
        ct = ab_osc(state) * ct
        
        write(fno, '(f14.5, 2f14.5)') (sys_eval(state)-sys_eval(1))*hw, fe, ct
    end do     
        
    close(fno)
    
    end if
end subroutine
