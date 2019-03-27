!*********************************************************************!
!    Normalization for the pl spectrum
!*********************************************************************!
subroutine normalization_pl()
        use common_variables
        implicit none
        
        real*8 maxvalue
        character*256 f_name
    real*8 energy
    integer spec_point, f_no
                
        maxvalue = 0
        do spec_point=1, spec_step
                if (pl_x(spec_point) > maxvalue) maxvalue=pl_x(spec_point)
        end do
        
        pl_x = pl_x / maxvalue
    
    f_no = 205
    f_name = trim(task_title)//'_pl_normal.dat'
    
    open(unit=f_no, file=f_name)
                write( f_no, * ) 'Energy pl(\g(w))'
                write( f_no, * ) 'Energy System'
                write( f_no, * ) 'cm\+(-1) a.u.'
        do spec_point = 1, spec_step
            energy = spec_start_pl + (spec_end_pl - spec_start_pl)/(spec_step*1.d0)*spec_point
            write(f_no,'(14f14.5)') energy*hw, pl_x(spec_point)
        end do
    
    close(f_no)
        
end subroutine