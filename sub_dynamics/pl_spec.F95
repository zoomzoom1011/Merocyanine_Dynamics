!*********************************************************************!
!        calculate the pl spectrum         !
!*********************************************************************!
subroutine pl_spec()
    use common_variables
    implicit none
        
    real*8 trans_e, energy, lineshape 
    integer stateup, statelo
    integer spec_point
    
    if(.not. allocated(pl_x)) allocate(pl_x(spec_step))
    if(.not. allocated(pl_y)) allocate(pl_y(spec_step))
    if(.not. allocated(pl_z)) allocate(pl_z(spec_step))
    pl_x = 0.d0
    pl_y = 0.d0
    pl_z = 0.d0
    
    ! do statelo = 1, 8
    ! do stateup = 9, 10
    do statelo = 1, pl_start-1
        trans_e = sys_eval(pl_start) - sys_eval(statelo)
    do spec_point = 1, spec_step
        energy = spec_start_pl + spec_point/(spec_step*1.d0)*(spec_end_pl - spec_start_pl)
        
        if (lorentzian) then
            lineshape = abs_lw/((energy-trans_e)**2+abs_lw**2)/pi
!            lineshape = lineshape/(lattice_x*1.d0)
        else
            lineshape = dexp(-((energy - trans_e)/abs_lw)**2)
!            lineshape = lineshape / ( dsqrt(pi) * abs_lw * lattice_x)
        end if    

        pl_x(spec_point) = pl_x(spec_point) + lineshape * pl_osc_x(statelo)
        pl_y(spec_point) = pl_y(spec_point) + lineshape * pl_osc_y(statelo)
        pl_z(spec_point) = pl_z(spec_point) + lineshape * pl_osc_z(statelo)
    end do
    end do
end subroutine