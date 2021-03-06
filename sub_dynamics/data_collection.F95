!***************************************************************************************!
!    get biggest coherence and transport time
!***************************************************************************************!
subroutine data_collection()
    use common_variables
    implicit none
    
    integer:: get_value1
    character*200:: fname
    integer:: fno, t

    do t = 0, t_max
        if(probability(t)>0.1) then
            get_value1 = t
            ! print*, t
            exit
        end if
    end do
    
    fno = 29
    fname = 'collection_data.dat'
    
    !output the maximum coherence number and excitation transfer speed 
    open( unit = fno, file = trim(fname) )
        write( fno, * ) 'maximum_coherence_number'
        write( fno, * ) maxval(cohere)
        write( fno, * ) 'transport_speed'
        write( fno, * ) dsqrt(z_offset**2+x_offset**2)*1.d-10*(nmax-1)/get_value1/time_step
    close( fno )

end subroutine 