!*********************************************************************!
!        allocate hamiltonian, eval, osc        !
!*********************************************************************!
subroutine allocate_hev()
    use common_variables
    implicit none

    if(.not. allocated(sys_h)) allocate (sys_h(sys_kount,sys_kount))
    if(.not. allocated(sys_eval)) allocate (sys_eval(sys_kount))

    sys_h = 0.d0
    sys_eval = 0.d0
                
end subroutine