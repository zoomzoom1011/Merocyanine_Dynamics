!*********************************************************************!
!        write the absorption spectrum into file        !
!*********************************************************************!
subroutine abs_out()
    use common_variables
    implicit none
    
    character*256 f_name
    real*8 energy
    integer spec_point, f_no
    
    f_no = 204
    f_name = trim(task_title)//'_ab.dat'
    
    open(unit=f_no, file=f_name)
    
        write( f_no, * ) 'Energy A(\g(w))'
        write( f_no, * ) 'Energy System'
        write( f_no, * ) 'cm\+(-1) a.u.'
        
        do spec_point = 1, spec_step
            energy = spec_start_ab + (spec_end_ab - spec_start_ab)/(spec_step*1.d0)*spec_point
            write(f_no,'(5f14.5)') energy*hw, ab_x(spec_point)+ab_y(spec_point)+ab_z(spec_point), &
            ab_x(spec_point), ab_y(spec_point), ab_z(spec_point)
!            write(f_no,'(2f14.5)') energy, ab_x(spec_point)+ab_y(spec_point)+ab_z(spec_point)
        end do
    
    close(f_no)
    
    f_name = trim(task_title)//'_ab_nm.dat'
    
    open(unit=f_no, file=f_name)
    
        write( f_no, * ) 'Energy A(\g(w))'
        write( f_no, * ) 'Energy System'
        write( f_no, * ) 'nm\+(-1) a.u.'
        
        do spec_point = 1, spec_step
            energy = spec_start_ab + (spec_end_ab - spec_start_ab)/(spec_step*1.d0)*spec_point
            write(f_no,'(5f14.5)') 10**7/energy/hw, ab_x(spec_point)+ab_y(spec_point)+ab_z(spec_point), &
            ab_x(spec_point), ab_y(spec_point), ab_z(spec_point)
!            write(f_no,'(2f14.5)') energy, ab_x(spec_point)+ab_y(spec_point)+ab_z(spec_point)
        end do
    
    close(f_no)
    
end subroutine