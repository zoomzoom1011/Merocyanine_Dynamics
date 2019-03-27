!*********************************************************************!
!    Normalization for the abs spectrum
!*********************************************************************!
subroutine normalization_ab()
        use common_variables
        implicit none
        
        real*8 maxvalue
        character*256 f_name
    real*8 energy
    integer spec_point, f_no
        real*8, allocatable:: ab(:)
       
       allocate(ab(spec_step))
       
       ab = 0.d0
       
        maxvalue = 0
        do spec_point=1, spec_step
                ab(spec_point) = ab_x(spec_point) + ab_y(spec_point) + ab_z(spec_point)
                if (ab(spec_point) > maxvalue) maxvalue=ab(spec_point)
        end do
        
        ab = ab / maxvalue
    
    f_no = 205
    f_name = trim(task_title)//'_ab_normal.dat'
    
    open(unit=f_no, file=f_name)
    
        write( f_no, * ) 'Energy A(\g(w))'
                write( f_no, * ) 'Energy System'
                write( f_no, * ) 'cm\+(-1) a.u.'
                
        do spec_point = 1, spec_step
            energy = spec_start_ab + (spec_end_ab - spec_start_ab)/(spec_step*1.d0)*spec_point
            write(f_no,'(14f14.5)') energy*hw, ab(spec_point)
!                       write(f_no,'(14f14.5)') energy, ab(spec_point)
        end do
        
    close(f_no)
                          
end subroutine