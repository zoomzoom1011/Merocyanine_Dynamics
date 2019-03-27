!*********************************************************************!
!        write parameter into file        !
!*********************************************************************!
subroutine para_out()
        use common_variables
        implicit none
        
        integer f_no
        character*256 f_name
        
        f_name = trim(task_title)//'_para.csv'
        f_no = 203
        
        open( unit = f_no, file = f_name) 
        
    write( f_no, * ) 'parameter, value'
    write( f_no, * ) '@@@@@@@@,@@@@@@@@'
    write( f_no, * ) 'task title, ', trim(task_title)
    write( f_no, * ) 'es_zmax, ', es_zmax
    write( f_no, * ) 'nmax, ', nmax
    write( f_no, *) 'intermolecule charge transfer states,', ct_state
    write( f_no, * ) 'vibrational energy (cm-1),', hw
    write( f_no, * ) 'Ion pair energy (cm-1),', nz
    write( f_no, * ) 'Intramolecule charge transfer (cm-1),', tz
    write( f_no, * ) 'Charge transfer energy (cm-1),', nct
    write( f_no, * ) 'Intermolecule charge transfer (cm-1),', tct
    write( f_no, * ) 'vibmax, ', vibmax
    write( f_no, * ) 'sys_vibmax, ', sys_vibmax
    write( f_no, * ) 'lambda_n,', lambda_n
    write( f_no, * ) 'lambda_z,', lambda_z
    write( f_no, * ) 'lambda_a,', lambda_a
    write( f_no, * ) 'lambda_c,', lambda_c
    write( f_no, * )  'monomer kount, ', mon_kount
    write( f_no, * )  'system kount, ', sys_kount
    write( f_no, * ) '@@@@@@@@,@@@@@@@@'
    write( f_no, *) 'abs linewidth (cm-1),', abs_lw
    write( f_no, *) 'lineshape is lorentzian,', lorentzian
    write( f_no, * ) 'x_offset,', x_offset
    write( f_no, * ) 'y_offset,', y_offset
    write( f_no, * ) 'z_offset,', z_offset
    write( f_no, * ) 'armlength,', armlength
!    write( f_no, * ) 'agg_angle,', agg_angle/pi*180.d0
    write( f_no, * ) 'dielectric,', dielectric
    write( f_no, * ) 'no_frenkel,', no_frenkel
    write( f_no, * ) 'periodic,', periodic
    write( f_no, * ) 'geometry,', geometry
    write( f_no, * ) 'max_coupling,', coulomb_coupling(sys_kount)
!    write( f_no, *) 'abs frequency depencence,', abs_freq_dep
       
        close(f_no)

end subroutine

subroutine para_out_append( strings )
    use common_variables
    implicit none

    character*128 f_name, strings
    integer f_number

    !name of file
    f_name = trim(task_title)//'_para.csv'
    f_number   = 25

    !open file
    open (unit = f_number, file = f_name, position='append')

    !append
    write (f_number, *) trim(strings)

    !close file
    close (f_number)

end subroutine