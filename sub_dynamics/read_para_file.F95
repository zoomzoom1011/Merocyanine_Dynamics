!*********************************************************************!
!                       read_para_file        !
!*********************************************************************!
subroutine read_para_file()
    use common_variables
    implicit none
    
    logical exists
    integer fno, ios, line, pos
    character*100 fname, buff, label
    
    fno = 201

    call getarg(1,fname)
    if (fname =="") then 
            print*, 'No control file given. Using default parameters'
            goto 1010
    end if 
    
!        make sure the control file exists
    inquire (file = trim (fname), exist = exists)       
    if (.not. exists) then 
            print*, 'Input file not found...aborting'
            stop
    end if
           
!        open the file to read in
    open (unit = fno, file= fname, status = 'old', action = 'read')
   
!--------------------------------------------------------------------------------------------------------! 
!        This process reads the control file
!        The ios changes if end of record or end of file is reached
!--------------------------------------------------------------------------------------------------------!

    ios = 0
    line = 0
    print*, 'Reading the input file...'
    
    do while(ios == 0)
!    print*, ios
        read(fno, '(a)', iostat = ios) buff
        if (ios == 0 ) then 
        line = line + 1 
        
        pos = scan(buff, ' ')
        label = buff(1:pos)
        buff = buff(pos + 1: )
        if (label (1:1)=='#' ) cycle
        select case (label)
            
        case('task_title')
            read(buff, *, iostat = ios) task_title
            print*, 'setting task_title to:'//trim(task_title)
        case('es_zmax')
            read( buff, *, iostat=ios) es_zmax
            print'(a,i4)', 'Setting the number of ESM mode: ', es_zmax
        case( 'nmax' )
            read( buff, *, iostat=ios ) nmax
            print'(a,i4)', 'Setting nmax to: ', nmax
        case('vibmax')
            read( buff, *, iostat=ios ) vibmax
            print'(a,i4)', 'Setting vibmax to: ', vibmax
        case('sys_vibmax')
            read( buff, *, iostat=ios ) sys_vibmax
            print'(a,i4)', 'Setting sys_vibmax to: ', sys_vibmax
        case('hw')
            read( buff, *, iostat=ios ) hw
            print'(a,f10.2)', 'Setting vibration energy to: ', hw 
!        case('s_z')
!            read( buff, *, iostat=ios ) s_z
!            print'(a,f8.2)', 'Setting s_z to : ', s_z
!        case('s_ani')
!            read( buff, *, iostat=ios ) s_ani
!            print'(a,f8.2)', 'Setting s_ani to : ', s_ani
!        case('s_cat')
!            read( buff, *, iostat=ios ) s_cat
!            print'(a,f8.2)', 'Setting s_cat to : ', s_cat
        case('periodic')
            read(buff, *, iostat=ios) periodic
            if (periodic) print*, '   periodic condition is on.'
            if (.not. periodic) print*, '   periodic condition is off.'
        case( 'lambda_n' )
            read( buff, *, iostat=ios ) lambda_n
            print'(a,f10.2)', 'Setting lambda_n to: ', lambda_n
        case( 'lambda_z' )
            read( buff, *, iostat=ios ) lambda_z
            print'(a,f10.2)', 'Setting lambda_z to: ', lambda_z
        case( 'lambda_a' )
            read( buff, *, iostat=ios ) lambda_a
            print'(a,f10.2)', 'Setting lambda_a to: ', lambda_a
        case( 'lambda_c' )
            read( buff, *, iostat=ios ) lambda_c
            print'(a,f10.2)', 'Setting lambda_c to: ', lambda_c
        case('abs_lw')
            read( buff, *, iostat=ios) abs_lw
            print'(a,f8.2)', 'Setting the abs linewidth to (cm-1): ', abs_lw
        case('nz')
            read( buff, *, iostat=ios) nz
            print'(a,f8.2)', 'Setting zwitter energy to (cm-1): ', nz
        case('tz')
            read( buff, *, iostat=ios) tz
            print'(a,f8.2)', 'Setting intra charge transfer to (cm-1): ', tz
        case('nct')
            read( buff, *, iostat=ios) nct
            print'(a,f8.2)', 'Setting ion pair energy to (cm-1): ', nct
        case('tct')
            read( buff, *, iostat=ios) tct
            print'(a,f8.2)', 'Setting inter charge transfer to (cm-1): ', tct
        case('spec_step')
            read( buff, *, iostat=ios) spec_step
            print'(a,i8)', 'Setting spec_step to (cm-1): ', spec_step
        case('spec_start_ab')
            read( buff, *, iostat=ios) spec_start_ab
            print'(a,f8.2)', 'Setting spec_start_ab to (cm-1): ', spec_start_ab
        case('spec_end_ab')
            read( buff, *, iostat=ios) spec_end_ab
            print'(a,f8.2)', 'Setting spec_end_ab to (cm-1): ', spec_end_ab
        case('spec_start_pl')
            read( buff, *, iostat=ios) spec_start_pl
            print'(a,f8.2)', 'Setting spec_start_pl to (cm-1): ', spec_start_pl
        case('spec_end_pl')
            read( buff, *, iostat=ios) spec_end_pl
            print'(a,f8.2)', 'Setting spec_end_pl to (cm-1): ', spec_end_pl
        case('lorentzian')
            read(buff, *, iostat=ios) lorentzian
            if (lorentzian) print*, 'Lineshape set to Lorentzian.'
            if (.not. lorentzian) print*, 'Lineshape set to Gaussian.'
        case('calc_pl')
            read( buff, *, iostat=ios ) calc_pl
            if ( calc_pl ) print'(a)', 'Will calculate all eigenstates.'
            if ( .not. calc_pl ) print'(a)', 'Will calculate only absorption.'
        case('ct_state')
            read( buff, *, iostat=ios) ct_state
            if ( ct_state ) print*, 'ct_states are turned on.'
            if ( .not.ct_state ) print*, 'ct_states are turned off.'
        case('ct_truncate')
            read( buff, *, iostat=ios) ct_truncate
            if ( ct_truncate ) print*, 'ct truncate to one one ac pair.'
        case('x_offset')
            read( buff, *, iostat=ios) x_offset
            print'(a,f8.2)', 'Setting x_offset to (A): ', x_offset
        case('y_offset')
            read( buff, *, iostat=ios) y_offset
            print'(a,f8.2)', 'Setting y_offset to (A): ', y_offset
        case('z_offset')
            read( buff, *, iostat=ios) z_offset
            print'(a,f8.2)', 'Setting z_offset to (A): ', z_offset
        case('armlength')
            read( buff, *, iostat=ios) armlength
            print'(a,f8.2)', 'Setting armlength to (A): ', armlength
        case('geometry')
            read( buff, *, iostat=ios) geometry
            if (geometry == 1) then
                print'(a)','Calcualte parallel geometry '
            else if (geometry == 2) then 
                print'(a)','Calcualte antiparallel geometry '
            else
                print'(a)','geometry is wrong'
                exit
            end if
!        case('agg_angle')
!            read( buff, *, iostat=ios) agg_angle
!            print'(a,f8.2)', 'Setting agg_angle to (degree): ', agg_angle
        case('dielectric')
            read( buff, *, iostat=ios) dielectric
            print'(a,f8.2)', 'Setting dielectric to (A): ', dielectric
        case('no_frenkel')
            read( buff, *, iostat=ios) no_frenkel
            if ( no_frenkel ) print*, 'no frenkel coupling will account.'
            if ( .not. no_frenkel ) print*, 'frenkel coupling will account.'
        case('nearest_neighbor')
            read( buff, *, iostat=ios) nearest_neighbor
            if ( nearest_neighbor ) print*, 'calc coupling from nearest neighbor.'
            if ( .not. nearest_neighbor ) print*, 'calc coupling from long range.' 
        case('t_max')
            read( buff, *, iostat=ios) t_max
            print'(a,i8)', 'Setting the maximum time: ', t_max
        case default
            print*, 'invalid label at line, ', line
            print*, buff
            read*
        end select
        
        end if
        
    end do
    
!    print*, x_offset
!    print*, z_offset
    close(fno)

1010 continue
     print*, 'Calculating parameters in units of hw'

    !normalize parameters to hamiltonian units of vib quanta
    tz = tz/hw
    nz = nz/hw
    tct = tct/hw
    nct = nct/hw
    abs_lw = abs_lw/hw
    spec_start_ab = spec_start_ab/hw
    spec_end_ab = spec_end_ab/hw
    spec_start_pl = spec_start_pl/hw
    spec_end_pl = spec_end_pl/hw
    time_step = 1.D-14               !Seconds D-15 is fs
    ! t_max = 10            !number of timesteps
    
    if(ct_state) es_zmax = es_zmax + 2
    
!    agg_angle = agg_angle/180.d0*pi
    
    !set lambda variables
    es_lambda_z( es_n ) = lambda_n
    es_lambda_z( es_z ) = lambda_z
    es_lambda_z( es_a ) = lambda_a
    es_lambda_z( es_c ) = lambda_c
    
    if ( vibmax == 0 ) then
        es_lambda_z = 0.d0
    end if
    

    call system('if [ ! -d '//trim(task_title)//' ]; then '//&
            'mkdir '//trim(task_title)//'; fi' )
    print*, 'Entering ./'//trim(adjustl(task_title))
    call chdir( task_title )
    
end subroutine