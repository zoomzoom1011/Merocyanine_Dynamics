!*********************************************************************!
!        print out the kounter to check results        !
!*********************************************************************!
subroutine kount_out()
    use common_variables
    implicit none
    
    print*, '*******************************************************'
    print*, 'monomer states are:' , mon_kount
    print*, 'total states are:' , sys_kount
    print*, '*******************************************************'
                
end subroutine