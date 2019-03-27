!*********************************************************************!
!        write the pl spectrum into the file        !
!*********************************************************************!
subroutine pl_out()
    use common_variables
    implicit none
    
    character*256 f_name
    real*8 energy
    integer spec_point, f_no
    
    f_no = 205
    f_name = trim(task_title)//'_pl.dat'
    
    open(unit=f_no, file=f_name)
    
    write( f_no, * ) 'Energy F(\g(w))'
        write( f_no, * ) 'Energy System'
        write( f_no, * ) 'cm\+(-1) a.u.'
    
        do spec_point = 1, spec_step
            energy = spec_start_pl + (spec_end_pl - spec_start_pl)/(spec_step*1.d0)*spec_point
            write(f_no,'(2f14.5)') energy*hw, pl_x(spec_point)+pl_y(spec_point)+pl_z(spec_point)
!            pl_x(spec_point), pl_y(spec_point), pl_z(spec_point)
        end do
    
    close(f_no)
                
end subroutine