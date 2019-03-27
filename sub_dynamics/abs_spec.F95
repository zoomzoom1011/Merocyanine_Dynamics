!*********************************************************************!
!        calculate the absorption spectrum        !
!*********************************************************************!
subroutine abs_spec()
    use common_variables
    implicit none
    
    real*8 tran_e,energy, lineshape
    integer state, spec_point
    
    if(.not. allocated(ab_x)) allocate (ab_x(spec_step))
    if(.not. allocated(ab_y)) allocate (ab_y(spec_step))
    if(.not. allocated(ab_z)) allocate (ab_z(spec_step))
    ab_x = 0.d0
    ab_y = 0.d0
    ab_z = 0.d0
    
    
    do spec_point = 1, spec_step
        energy = spec_start_ab + (spec_end_ab - spec_start_ab)/(spec_step*1.d0)*spec_point
    do state = 2, sys_kount
        tran_e = sys_eval(state) - minval(sys_eval)
    
    if (lorentzian) then
        lineshape = abs_lw/((energy-tran_e)**2+abs_lw**2)/pi
!        lineshape = lineshape/(lattice_x*1.d0)
    else
        lineshape = dexp(-((energy - tran_e)/abs_lw)**2)
!        lineshape = lineshape/(lattice_x*1.d0)
    end if
    
        ab_x(spec_point) = ab_x(spec_point) + lineshape * ab_osc_x(state)
        ab_y(spec_point) = ab_y(spec_point) + lineshape * ab_osc_y(state)
        ab_z(spec_point) = ab_z(spec_point) + lineshape * ab_osc_z(state)
    
    end do
    end do
    
end subroutine