!*********************************************************************!
!         build hamiltonian                    !
!*********************************************************************!
subroutine hamiltonian()
    use common_variables
    implicit none
    
    integer:: state1,state2
    integer:: n1, n2, delta
    integer:: state_a, vib_a
    integer:: state_b, vib_b
    integer:: diff, diffn
    integer:: diffterm(3)
    real*8:: volap
    integer:: state_1a, state_2a, state_1b, state_2b
    integer:: vib_1a, vib_2a, vib_1b, vib_2b
    real*8:: get_tct
    integer:: delta_NZ, delta_ES
!    integer:: sign_fxn
    
    do state1 = 1, sys_kount
        !on diagonal
        do n1 = 1, nmax
            state_a = sys_state(state1,n1)%es_state
            vib_a = sys_state(state1,n1)%vib
            !the electronic energy
            if (state_a == es_z) then
                sys_h(state1,state1)=sys_h(state1,state1) + nz
            else if (state_a == es_a) then
                sys_h(state1,state1)=sys_h(state1,state1) + nct/2.d0
            else if (state_a == es_c) then
                sys_h(state1,state1)=sys_h(state1,state1) + nct/2.d0
            end if

            !the vibration energy
            sys_h(state1, state1) = sys_h(state1, state1) + (vib_a)*1.d0
            
            !Add a defect state
            if ( defect .eqv. .true. ) then
                if ( n1 == defect_site ) then 
                    sys_h( state1, state1 ) = sys_h( state1, state1 ) + defect_energy
                end if
            end if
        end do
        !coupling for the dimer
        if(nmax > 1) then
            sys_h(state1, state1) =  sys_h(state1, state1) + coulomb_coupling(state1)
            ! print*, state1, coulomb_coupling(state1) *hw
        end if
    end do
    
    !off diagonal
    do state1 = 1 ,sys_kount
    do state2 = 1 ,sys_kount
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
            if ( diff > 1 ) exit
        end do
        
        !at max, only one molecule can have a different configuration
        if ( diff .ne. 1 ) cycle
            
        state_a = sys_state(state1,diffn)%es_state
        vib_a = sys_state(state1,diffn)%vib
        state_b = sys_state(state2,diffn)%es_state
        vib_b = sys_state(state2,diffn)%vib
            
        !intramolecule coupling
        
!        if ( state_a == es_n .and. state_b == es_z) then
!            sys_h(state1, state2) = sys_h(state1, state2)  + tz* volap( es_lambda_z( state_a ), vib_a,&
!                                   es_lambda_z( state_b ), vib_b)*sign_fxn(diffn)
!        else if ( state_a == es_z .and. state_b == es_n ) then
!            sys_h(state1, state2) = sys_h(state1, state2) + tz* volap( es_lambda_z( state_a ), vib_a,&
!                                   es_lambda_z( state_b ), vib_b)*sign_fxn(diffn)
!        end if
            sys_h(state1, state2) = sys_h(state1, state2) + tz* volap( es_lambda_z( state_a ), vib_a,&
                                   es_lambda_z( state_b ), vib_b)
                                   
!        print*, sign_fxn(diffn)
!            sys_h(state1, state2) =  tz* volap( es_lambda_z( state_a ), vib_a,&
!                                   es_lambda_z( state_b ), vib_b)*sign_fxn(diffn)

    end do
    end do

    !intermolecule coupling
    !exchange part
    if(nmax > 1 .and. ct_state) then
    
    do state1 = 1 ,sys_kount
    do state2 = 1 ,sys_kount
        if(state1 == state2) cycle
        
        diff = 0
        do n1 = 1, nmax
            state_a = sys_state(state1,n1)%es_state
            vib_a = sys_state(state1,n1)%vib
            state_b = sys_state(state2,n1)%es_state
            vib_b = sys_state(state2,n1)%vib
                    
            if ( vib_a .ne. vib_b .or. & 
                 state_a .ne. state_b ) then
                
                diff = diff + 1
                diffterm(diff) = n1
            end if
            if ( diff > 2 ) exit
        end do
        
        !at max, only two molecules can have different configurations
        if ( diff .ne. 2 ) cycle
        !make sure it is charge transfer state
        
        
        state_1a = sys_state(state1,diffterm(1))%es_state
        vib_1a = sys_state(state1,diffterm(1))%vib
        state_1b = sys_state(state1,diffterm(2))%es_state
        vib_1b = sys_state(state1,diffterm(2))%vib
        state_2a = sys_state(state2,diffterm(1))%es_state
        vib_2a = sys_state(state2,diffterm(1))%vib
        state_2b = sys_state(state2,diffterm(2))%es_state
        vib_2b = sys_state(state2,diffterm(2))%vib
        
        delta = delta_NZ(state_1a)*delta_NZ(state_1b)* &
            delta_ES(es_c,state_2a)*delta_ES(es_a,state_2b) +&
            delta_NZ(state_1a)*delta_NZ(state_1b)* &
            delta_ES(es_c,state_2b)*delta_ES(es_a,state_2a) +&
            delta_NZ(state_2a)*delta_NZ(state_2b)* &
            delta_ES(es_c,state_1a)*delta_ES(es_a,state_1b) +&
            delta_NZ(state_2a)*delta_NZ(state_2b)* &
            delta_ES(es_c,state_1b)*delta_ES(es_a,state_1a)
        ! print*,state_1a,state_1b,state_2a,state_2b,delta
        !vibrational coupling
        sys_h(state1, state2) = sys_h(state1, state2) + get_tct(diffterm(1),diffterm(2)) * &
                                    volap( es_lambda_z( state_1a ), vib_1a,&
                                    es_lambda_z( state_2a ), vib_2a) * &
                                    volap( es_lambda_z( state_1b ), vib_1b,&
                                    es_lambda_z( state_2b ), vib_2b) * delta
                                
!        sys_h(state1, state2) = sys_h(state1, state2) + tct* &
!                                    volap( es_lambda_z( state_1a ), vib_1a,&
!                                    es_lambda_z( state_2a ), vib_2a) * &
!                                    volap( es_lambda_z( state_1b ), vib_1b,&
!                                    es_lambda_z( state_2b ), vib_2b)
    end do
    end do

    end if
    
    ! if(nmax > 2 .and. ct_state) then
    
    ! do state1 = 1 ,sys_kount
    ! do state2 = 1 ,sys_kount
        ! if(state1 == state2) cycle
        ! ! find the same anion in the same molecule
        ! do n1 = 1, nmax
            ! state_a = sys_state(state1,n1)%es_state
            ! vib_a = sys_state(state1,n1)%vib
            ! state_b = sys_state(state2,n1)%es_state
            ! vib_b = sys_state(state2,n1)%vib
            ! ! anion linker part 
            ! if ( vib_a == vib_b .and. state_a == es_a &
                ! .and. state_b == es_a ) then
                ! ! print*,state_a,state_b
                ! diff = 0
                ! do n2 = 1, nmax
                    ! if(n2 == n1) cycle
                    ! state_a = sys_state(state1,n2)%es_state
                    ! vib_a = sys_state(state1,n2)%vib
                    ! state_b = sys_state(state2,n2)%es_state
                    ! vib_b = sys_state(state2,n2)%vib
                            
                    ! if ( vib_a .ne. vib_b .or. & 
                         ! state_a .ne. state_b ) then
                        
                        ! diff = diff + 1
                        ! diffterm(diff) = n2
                    ! end if
                    ! if ( diff > 2 ) cycle
                ! end do
                ! state_1a = sys_state(state1,diffterm(1))%es_state
                ! vib_1a = sys_state(state1,diffterm(1))%vib
                ! state_1b = sys_state(state1,diffterm(2))%es_state
                ! vib_1b = sys_state(state1,diffterm(2))%vib
                ! state_2a = sys_state(state2,diffterm(1))%es_state
                ! vib_2a = sys_state(state2,diffterm(1))%vib
                ! state_2b = sys_state(state2,diffterm(2))%es_state
                ! vib_2b = sys_state(state2,diffterm(2))%vib
                
                ! ! at max, only two molecules can have different configurations
                ! if ( diff .ne. 2 ) cycle

                ! delta = delta_NZ(state_1a)*delta_NZ(state_2b)* &
                        ! delta_ES(es_c,state_1b)*delta_ES(es_c,state_2a) + &
                        ! delta_NZ(state_1b)*delta_NZ(state_2a)* &
                        ! delta_ES(es_c,state_1a)*delta_ES(es_c,state_2b)
                ! ! print*,state_1a,state_1b,state_2a,state_2b,delta
                ! sys_h(state1, state2) = sys_h(state1, state2) + get_tct(diffterm(1),diffterm(2)) * &
                    ! volap( es_lambda_z( state_1a ), vib_1a,&
                    ! es_lambda_z( state_2a ), vib_2a) * &
                    ! volap( es_lambda_z( state_1b ), vib_1b,&
                    ! es_lambda_z( state_2b ), vib_2b) * delta

            ! end if
            
            ! ! cation linker part 
            ! if ( vib_a == vib_b .and. state_a == es_c &
                ! .and. state_b == es_c ) then
                ! diff = 0
                ! do n2 = 1, nmax
                    ! if(n2 == n1) cycle
                    ! state_a = sys_state(state1,n2)%es_state
                    ! vib_a = sys_state(state1,n2)%vib
                    ! state_b = sys_state(state2,n2)%es_state
                    ! vib_b = sys_state(state2,n2)%vib
                            
                    ! if ( vib_a .ne. vib_b .or. & 
                         ! state_a .ne. state_b ) then
                        
                        ! diff = diff + 1
                        ! diffterm(diff) = n2
                    ! end if
                    ! if ( diff > 2 ) cycle
                ! end do
                ! state_1a = sys_state(state1,diffterm(1))%es_state
                ! vib_1a = sys_state(state1,diffterm(1))%vib
                ! state_1b = sys_state(state1,diffterm(2))%es_state
                ! vib_1b = sys_state(state1,diffterm(2))%vib
                ! state_2a = sys_state(state2,diffterm(1))%es_state
                ! vib_2a = sys_state(state2,diffterm(1))%vib
                ! state_2b = sys_state(state2,diffterm(2))%es_state
                ! vib_2b = sys_state(state2,diffterm(2))%vib
                
                ! ! at max, only two molecules can have different configurations
                ! if ( diff .ne. 2 ) cycle
                    ! delta = delta_NZ(state_1a)*delta_NZ(state_2b)* &
                            ! delta_ES(es_a,state_1b)*delta_ES(es_a,state_2a) + &
                            ! delta_NZ(state_1b)*delta_NZ(state_2a)* &
                            ! delta_ES(es_a,state_1a)*delta_ES(es_a,state_2b)
                    ! sys_h(state1, state2) = sys_h(state1, state2) + get_tct(diffterm(1),diffterm(2)) * &
                        ! volap( es_lambda_z( state_1a ), vib_1a,&
                        ! es_lambda_z( state_2a ), vib_2a) * &
                        ! volap( es_lambda_z( state_1b ), vib_1b,&
                        ! es_lambda_z( state_2b ), vib_2b) * delta

            ! end if
        ! end do
    ! end do
    ! end do  
    ! end if
    ! check if the hamiltonian is symmetric
    
    ! do state1 = 1 ,sys_kount
    ! do state2 = 1 ,sys_kount
        ! if(state1 == state2) cycle
        ! if( sys_h(state1, state2) .ne. sys_h(state2, state1)) then
            ! print*, 'not symmetric hamiltonian, abort'
            ! stop
        ! end if
        
    ! end do
    ! end do
    
end subroutine