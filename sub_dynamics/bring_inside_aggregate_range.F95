!*********************************************************************!
!        bring the aggregate range inside the boundary        !
!*********************************************************************!

integer function bring_inside_aggregate_range(n1)
    use common_variables
    implicit none
        
    integer, intent(in):: n1
    integer:: n2
    
    n2 = n1
    if (n1<1) n2 = n1 + nmax
    if (n1>nmax) n2 = n1 - nmax
        
    bring_inside_aggregate_range = n2

    return
end function