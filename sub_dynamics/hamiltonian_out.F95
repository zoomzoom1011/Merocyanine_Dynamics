!*********************************************************************!
!        print the hamiltonian to check result        !
!*********************************************************************!
subroutine hamiltonian_out()
        use common_variables
        implicit none
        
        integer::fno = 202
        integer i,j
        character*256::fname
        
        fname = trim(task_title)//'_h.csv'
        open(unit = fno, file = fname)
        
        do i = 1, sys_kount
                write(fno, '(1000f14.5,", ")')  (sys_h(i,j)*hw,j = 1, sys_kount)
        end do
        
        write(fno, *)
        
        close(fno)
                
end subroutine