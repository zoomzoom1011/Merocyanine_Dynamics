!***************************************************************************!
!       Dirac Delta Function
!***************************************************************************!
Integer Function delta_ES( state1, state2 )
    Implicit None

    Integer, INTENT(IN) :: state1, state2
    
    !Initialize
    delta_ES = 0
    
    If (state1 == state2 ) delta_ES = 1
    
    return
End Function