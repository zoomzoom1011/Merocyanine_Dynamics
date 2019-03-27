!*********************************************************************!
!            get the intermolecule charge function                    !
!*********************************************************************!
real*8 function get_tct( n1, n2 )
    use common_variables
    implicit none

    integer, intent(in) :: n1, n2
    integer dn
    
    get_tct = 0.d0
    dn = abs( n1 - n2 )

    if (periodic) then
        dn = min( dn, nmax - dn )
    end if

    if (dn == 1) get_tct = tct
    
    return
    
end function