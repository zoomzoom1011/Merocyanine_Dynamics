subroutine mon_osc()
    use common_variables
    implicit none

    real*8  tdm, dm

    integer:: state
        
   !monomer transition dipole moment 
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
    
    
    allocate( monomer_osc( 2, mon_kount ) )
    monomer_osc = 0.d0

    !loop over excited states
    do state = 2, mon_kount
        do hx = 1, mon_kount
                !calculate the dipole moment of each basis state
                dm = mu(monomer_basis_set(stateB)%state, &
                        monomer_basis_set(stateB)%state)
                tdm = tdm + esHmon(stateB,stateG)*esHmon(stateB,stateE)*dm
        end do

        monomer_osc(1,stateE) = tdm*tdm
        monomer_osc(2,stateE) = esHmonEval( stateE ) - esHmonEval( stateG )
    end do
    

end subroutine