program merocyanine
    use common_variables
    implicit none
    
    real*8 time_start, time_finish
    character*128:: cwd
    Integer:: istat, getcwd
    
    call cpu_time( time_start )
    
    !Get the current working directory
    istat = getcwd( cwd )
    if ( istat .ne. 0 ) stop 'getcwd: error'

    !read para file
    call read_para_file()
!    call mkdir(trim(task_title))
    
    !index the basis states
    call build_molecule_geometry()
    call index_mon_state()
    call index_sys_state()

    !allocate the hamiltonian eval
    call allocate_hev()
    
    !output the kounter
    call kount_out()
    
    !set the coupling and transition dipole moment
    call calc_coupling()
    
    !building the hamiltonian
    call hamiltonian()

    !output the hamiltonian 
    ! call hamiltonian_out()
 
    !diagonalize
    call dev_diagonalize(sys_h, sys_kount, sys_eval)
   
    !output eigenvalues and eigenvectors
    ! call evec_out()
        
    !calculate the absorption spectrum
    call abs_osc()        
    call abs_spec()
    call abs_out()
    call normalization_ab()
        
    !now do the dynamics
    call get_alpha_coeff()    !first get the initial state
    !setup dislin for visualization
    ! call setup_dislin()
    !now evolve the state
    ! first_value = .False.
    
    if(.not. allocated(cohere)) allocate(cohere(0:t_max))
    if(.not. allocated(probability)) allocate(probability(0:t_max)) 
    
    do current_t = 0, t_max
        call get_current_state()
        ! if(current_t==0) print*,current_state
        ! print*,current_state
        call get_population()
        call get_coherence()
        ! if(mod(current_t, t_max/10) == 0) print'(f4.1,a)',current_t*1.d0/t_max*100,"% are done"
    end do
    ! call finish_dislin()
    !get biggest value in coherence
    call data_collection(cwd)

    !write the para to files
    call para_out()

    call cpu_time( time_finish )
    print'(a, f14.4, a)', 'total time is :: ', time_finish - time_start, ' seconds'
    
    
    !////////////////////////////////////////////////////////////////////////////!

    !Display in output
!     if ( display_evolution ) then
!         call setup_dislin()
!     end if
    
    !Evolve the wavefunction
    !\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\!

    ! call get_alpha_coeff() !first get the initial state

!     call get_t0_state()

!     call open_files()
!     Print*, '>Calculating Temporal Evolution::'
        !Initialize the x-axis
!     Do n = 1, nmax
!         pos( n ) = n*1.D0
!     End Do

    ! Do current_t = 0, t_max
        ! call get_current_state()
        ! call get_populations()
!                call position_expectation()
!                call velocity_expectation()
    ! End Do
!     Print*, '>Done'
!     call close_files()
    !\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\!
    
!     If ( display_evolution ) call finish_dislin()
    
    !\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\!

end program