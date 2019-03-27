!*********************************************************************!
!        calculate energy dispersion according to 
!*********************************************************************!
subroutine dispersion_out()
    use common_variables
    implicit none
    
    complex*16, parameter:: complex_zero = ( 0.d0, 0.d0 )
    complex*16, parameter:: img = ( 0.d0, 1.d0 )
    integer:: n, i
    integer:: fno = 51
    
    !let the k index be in reference to sublattice 1
    do n1 = 1, nmax
        k(n1) = 
    do n2 = 1, nmax
        coeff = complex_zero
        coeff = coeff + exp(img*k(n1)*n2)
    do state = 1, effectivees
        kounthb_all = kounthb_all + 1
        allhb_nx( ka, kb, kc, state ) = kounthb_all
    end do
    end do
    end do
    end do  
    
    
    do 
    
    open( unit = fno, file = trim(task_title)//'_disp.dat' )
    write( fno, * ) 'ka kb kc', (i, i=1, statemax)

    do n = 1, nmax
        write( fno, * ) k(n),(sys_eval( ( n, i ) ), i = 1, statemax )
    end do
    
    close( fno )
    
end subroutine
