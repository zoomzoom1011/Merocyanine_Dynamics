!*********************************************************************!
!    Make a directory for the output files and enter the directory
!*********************************************************************!
subroutine mkdir( dir )
    implicit none
        
    character(*), intent(in) :: dir
    integer fno
    character(100) fname
    parameter ( fno = 15 )
    fname=trim(dir)//'mkdir.bat'

    open( unit = fno, file = fname )
    write( fno, * ) '@echo off'
    write( fno, * ) 'if not exist '//dir//' mkdir '//dir
!    call get_command_argument( 1, inp )
!    write( fno, * ) 'move /Y '//trim(inp)//' '//trim(dir)//' 1>NUL 2>&1'
    close (fno)
    
    call system( fname )
    call sleep(1)
    open( unit = fno, file = fname )
    close( fno, status = 'delete' )

    print*,
    print*, 
    print*, '*********************************************************************'
    print*, 'entering \' //trim(dir)
    call chdir( dir )
end subroutine