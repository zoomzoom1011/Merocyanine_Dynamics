!*********************************************************************!
!        index the basis state      !
!*********************************************************************!
subroutine  index_mon_state()
    use common_variables
    implicit none
    
    integer:: es_state
    integer:: vib
    integer:: run
    
    do run = 1, 2
        mon_kount = 0
        do es_state = 1, es_zmax
        do vib = 0, vibmax
            mon_kount = mon_kount + 1
            if(run == 2) then
                mon_state(mon_kount)%es_state = es_state
                mon_state(mon_kount)%vib = vib
!                    print*, es_state, vib
            end if
        end do 
        end do
        
        if (run == 1) then
            if (.not. allocated (mon_state)) allocate(mon_state(mon_kount)) 
        end if
    end do
end subroutine