!*********************************************************************!
!        write the eigenvector into the file        !
!*********************************************************************!
subroutine evec_out()
    use common_variables
    implicit none
    
    integer::fno = 202
    integer i,j
    character*256::fname
    
    fname = trim(task_title)//'_evec.csv'
    open(unit = fno, file = fname)

    write(fno, *) 'Eigenvalues'
    write(fno, '(1000f14.5,", ")')  (sys_eval(j)*hw,j = 1, sys_kount)
    write(fno, * )
    
    write(fno, *) 'Eigenvectors'
    do i = 1, sys_kount
            write(fno, '(1000f14.5,", ")')  (sys_h(i,j),j = 1, sys_kount)
!        print*,  (h(i,j),j = 1, kount)
    end do
    write(fno, *)
    
    close(fno)
                
end subroutine