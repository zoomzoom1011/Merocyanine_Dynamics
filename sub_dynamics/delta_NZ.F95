!***************************************************************************!
!       get NZ state
!***************************************************************************!
Integer Function delta_NZ( state1 )
    use common_variables
    Implicit None

    Integer, INTENT(IN) :: state1
    
    !Initialize
    delta_NZ = 0
    
    If (state1 == es_N ) delta_NZ = 1
    If (state1 == es_Z ) delta_NZ = 1
    
    return
End Function