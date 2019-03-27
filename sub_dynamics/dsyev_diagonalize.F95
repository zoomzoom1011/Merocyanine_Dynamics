!*********************************************************************!
!        Real Diagonalization        !
!*********************************************************************!
subroutine dev_diagonalize( h, kount, eval ) 
    implicit none
    
    integer, intent(in) :: kount
    real*8, intent(inout) :: h( kount, kount ), eval(kount)
    character*4 :: jobz = 'v'
    character*4 :: uplo = 'u'
    real*8, allocatable :: work(:)
    integer lwork
    integer nb, info, ilaenv
    real*8 time_start, time_finish

    nb = ilaenv( 1, 'dsytrd', 'u', kount, -1,-1,-1)
    lwork = (nb+2) * kount
    allocate(work(lwork))
    print*, 'the size is :: ', kount
    print*, '//                call to dsyev                \\'
    call cpu_time( time_start )
    call dsyev( jobz, uplo, kount, h, kount, eval, work, lwork, info )
    call cpu_time( time_finish )
    print*, '\\                     done                    //'
    print'(a, f14.4, a)', 'diagonalization time is :: ', time_finish - time_start, ' seconds'
    
    !eigenvalues are in acending order.!!!
end subroutine