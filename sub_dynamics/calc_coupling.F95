!*********************************************************************!
!        calculate coulomb coupling and tdm for n >2 state
!*********************************************************************!
subroutine  calc_coupling()
    use common_variables
    implicit none

    real*8:: distance
    integer:: n1,n2,n3,dn,da1,da2,sysnx
    integer:: state1, state2
!    integer:: num
    real*8:: coeff
!    integer:: delta_ES
!    real*8:: v
    integer:: bring_inside_aggregate_range
    real*8:: temp
    
    !physical constants
    !constants for au to cm-1 conversion
    real*8, parameter :: eo = 8.854187817d-12 !(f/m)
    real*8, parameter :: plancks  = 6.62606957d-34  !kg m**2 s**-2
    real*8, parameter :: csol  = 2.99792458d8    !m s**-1
    real*8, parameter :: aucharge_c  = 1.602176565d-19 !c per au
    
    if (.not. allocated(coulomb_coupling)) allocate(coulomb_coupling(sys_kount))
    coulomb_coupling = 0.d0
    
!    if (nmax > 1) then
!        do da1 = 1, 2
!        do da2 = 1, 2
!            distance = (molecule_pos(1, da1, 1)- molecule_pos(2, da2, 1))**2 + &
!                        (molecule_pos(1, da1, 2)- molecule_pos(2, da2, 2))**2 + &
!                        (molecule_pos(1, da1, 3)- molecule_pos(2, da2, 3))**2
!            distance = dsqrt(distance)
!            coupling = coupling + tq(es_z, da1)* tq(es_z, da2)/distance
!                
!        end do
!        end do
!    
!        do sysnx = 1, sys_kount
!            
!            do n1 = 1, nmax
!                state1 = sys_state(sysnx,n1)%es_state
!                !periodic and nearest neighbor 
!                n2 = mod( n1 , nmax ) + 1 
!                state2 = sys_state(sysnx,n2)%es_state
!                
!                coulomb_coupling(sysnx) =coulomb_coupling(sysnx)+ coupling*&
!                        delta_ES(state1, ES_Z)*delta_ES(state2,ES_Z)  
!             end do
!!            print*, coulomb_coupling(sysnx)
!        end do
!    end if
    
!    v =-10.d0
    if(nmax > 1) then
        do sysnx = 1, sys_kount
            if (periodic .and. nearest_neighbor) then 
                do n1 = 1, nmax
                    state1 = sys_state(sysnx,n1)%es_state
                    !periodic and nearest neighbor
                    do dn = -1, 1
                        if (dn == 0) cycle 
                        n2 = n1 + dn
                        n3 = bring_inside_aggregate_range(n2)
                        ! if(n3 < n1) cycle
                        state2 = sys_state(sysnx,n3)%es_state
                        
                        do da1 = 1, 2
                        do da2 = 1, 2
                            distance = (molecule_pos(n1, da1, 1)- molecule_pos(n2, da2, 1))**2 + &
                                        (molecule_pos(n1, da1, 2)- molecule_pos(n2, da2, 2))**2 + &
                                        (molecule_pos(n1, da1, 3)- molecule_pos(n2, da2, 3))**2
                            distance = dsqrt(distance)
                            coulomb_coupling(sysnx) =coulomb_coupling(sysnx)+ &
                                    (tq(state1, da1)* tq(state2, da2))*1.d0/distance
                        end do
                        end do
                    end do
!                    if (state1 == es_z .and. state2 == es_z ) then
!                            coulomb_coupling(sysnx) =coulomb_coupling(sysnx)+ v 
!                    end if 
                end do
                coulomb_coupling(sysnx) = coulomb_coupling(sysnx)/2.d0
                If ( nmax == 2 ) then
                    coulomb_coupling(sysnx) = coulomb_coupling(sysnx)/2.D0        !There should only be one coupling between 1 and 2, above i overcount by 1
                End If
            else if ((.not. periodic) .and. nearest_neighbor) then
                do n1 = 1, nmax-1
                    state1 = sys_state(sysnx,n1)%es_state
                    !open boundary and nearest neighbor
                    n2 = n1 + 1
                    state2 = sys_state(sysnx,n2)%es_state
                    
!                    if (state1 == es_z .and. state2 == es_z ) then
!                            coulomb_coupling(sysnx) =coulomb_coupling(sysnx)+ v 
!                    end if 
                    do da1 = 1, 2
                    do da2 = 1, 2
                        distance = (molecule_pos(n1, da1, 1)- molecule_pos(n2, da2, 1))**2 + &
                                    (molecule_pos(n1, da1, 2)- molecule_pos(n2, da2, 2))**2 + &
                                    (molecule_pos(n1, da1, 3)- molecule_pos(n2, da2, 3))**2
                        distance = dsqrt(distance)
                        coulomb_coupling(sysnx) =coulomb_coupling(sysnx)+ &
                                (tq(state1, da1)* tq(state2, da2))*1.d0/distance

                    end do
                    end do
                end do
            else if ((.not. periodic) .and. (.not. nearest_neighbor)) then 
                do n1 = 1, nmax-1
                    state1 = sys_state(sysnx,n1)%es_state
                    !open boundary and not nearest neighbor
                do n2 = n1+1, nmax
                    state2 = sys_state(sysnx,n2)%es_state
                    do da1 = 1, 2
                    do da2 = 1, 2
                        distance = (molecule_pos(n1, da1, 1)- molecule_pos(n2, da2, 1))**2 + &
                                    (molecule_pos(n1, da1, 2)- molecule_pos(n2, da2, 2))**2 + &
                                    (molecule_pos(n1, da1, 3)- molecule_pos(n2, da2, 3))**2
                        distance = dsqrt(distance)
                        coulomb_coupling(sysnx) =coulomb_coupling(sysnx)+ &
                                (tq(state1, da1)* tq(state2, da2))*1.d0/distance

                    end do
                    end do
                    end do
                    end do
            else if(periodic .and. (.not. nearest_neighbor)) then
                
                do n1 = 1, nmax
                    state1 = sys_state(sysnx,n1)%es_state
                    !periodic and not nearest neighbor
                    ! print*, sysnx, n1, state1
                    do dn = -nmax/2 + 1, nmax/2
                        
                        if (dn == 0) cycle 
                        n2 = n1 + dn
                        ! print*, n1, n2
                        n3 = bring_inside_aggregate_range(n2)
                        ! print*, n1, n2
                        ! print*,'goood'
                        state2 = sys_state(sysnx,n3)%es_state
                        ! print*, sysnx, n3, state2
                        do da1 = 1, 2
                        do da2 = 1, 2
                            distance = (molecule_pos(n1, da1, 1)- molecule_pos(n2, da2, 1))**2 + &
                                        (molecule_pos(n1, da1, 2)- molecule_pos(n2, da2, 2))**2 + &
                                        (molecule_pos(n1, da1, 3)- molecule_pos(n2, da2, 3))**2
                            distance = dsqrt(distance)
                            coulomb_coupling(sysnx) =coulomb_coupling(sysnx)+ &
                                    (tq(state1, da1)* tq(state2, da2))*1.d0/distance
                            ! print*, da1, da2
                            ! print*, molecule_pos(n1, da1, 1), molecule_pos(n2, da2, 1)
                            ! print*, distance
                            ! print*, tq(state1, da1), tq(state2, da2)
                            ! print*, (tq(state1, da1)* tq(state2, da2))/distance
                        end do
                        end do
                    end do
                end do
                coulomb_coupling(sysnx) = coulomb_coupling(sysnx)/2.d0
                ! If ( nmax == 2 ) then
                    ! coulomb_coupling(sysnx) = coulomb_coupling(sysnx)/2.D0        !There should only be one coupling between 1 and 2, above i overcount by 1
                ! End If
            end if
        end do
    end if

    coeff = (aucharge_c**2)*(1.d10)/(4.d0*pi*100.d0*Eo*plancks*csol)/hw/dielectric
!    coeff = 1.d0
    coulomb_coupling = coeff * coulomb_coupling
    ! print*, coulomb_coupling(5) * hw
    ! print*, coulomb_coupling(6) * hw
    if (no_frenkel) coulomb_coupling = 0.d0
    ! coulomb_coupling(sys_kount) = -30000.d0/hw
    
    temp = 0.d0
    do da1 = 1, 2
    do da2 = 1, 2
        distance = (molecule_pos(1, da1, 1)- molecule_pos(2, da2, 1))**2 + &
                    (molecule_pos(1, da1, 2)- molecule_pos(2, da2, 2))**2 + &
                    (molecule_pos(1, da1, 3)- molecule_pos(2, da2, 3))**2
        distance = dsqrt(distance)
        temp = temp + &
                (tq(es_z, da1)* tq(es_z, da2))*1.d0/distance
    end do
    end do
    print*,"coulombic coupling:",temp*hw*coeff
!    print*, coulomb_coupling*hw
    
    if (.not. allocated(ux) ) allocate(ux(sys_kount))
    if (.not. allocated(uy) ) allocate(uy(sys_kount))
    if (.not. allocated(uz) ) allocate(uz(sys_kount))
    ux = 0.d0
    uy = 0.d0
    uz = 0.d0
    
    do sysnx = 1, sys_kount
        do n1 = 1, nmax
            state1 = sys_state(sysnx,n1)%es_state
            do da1 = 1, 2
                ux(sysnx) = ux(sysnx) + tq(state1, da1)* molecule_pos(n1, da1, 1)
                uy(sysnx) = uy(sysnx) + tq(state1, da1)* molecule_pos(n1, da1, 2)
                uz(sysnx) = uz(sysnx) + tq(state1, da1)* molecule_pos(n1, da1, 3)
            
            end do
        end do
    end do
    
!    print*, ux
end subroutine

!*********************************************************************!
!        bring the aggregate range inside the boundary        !
!*********************************************************************!

integer function bring_inside_aggregate_range(n1)
    use common_variables
    implicit none
        
    integer, intent(in):: n1
    integer:: n2
    
    n2 = n1
    if (n1<1) n2 = n1 + nmax
    if (n1>nmax) n2 = n1 - nmax
        
    bring_inside_aggregate_range = n2

    return
end function