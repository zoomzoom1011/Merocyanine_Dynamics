!*********************************************************************!
!    Module with all variables
!*********************************************************************!
module common_variables
    implicit none
    
    !system parameters
    character*256:: task_title = 'default'
    integer:: nmax = 2
    integer:: es_n = 1, es_z = 2
    
    !geometry
    real*8:: x_offset
    real*8:: y_offset
    real*8:: z_offset
    real*8:: armlength
    real*8:: dielectric
    real*8, allocatable:: molecule_pos(:, :, :)
    real*8, allocatable:: coulomb_coupling(:)
    integer, allocatable:: tq(:, :)
    integer:: geometry
    
    !vibration
    integer:: vibmax = 1
    integer:: sys_vibmax = 2
    real*8:: hw = 1400.d0
    real*8:: s_z = 1.00d0
    real*8:: s_cat = 0.50d0
    real*8:: s_ani = 0.50d0
    
    !CT coupling
    real*8:: tz
    real*8:: nz
    real*8:: nct
    real*8:: tct
    
    !transition dipole moment
    real*8,allocatable:: ux(:)
    real*8,allocatable:: uy(:)
    real*8,allocatable:: uz(:)
    
    !coupling
!    real*8:: jo_x = 350
!    real*8:: dielectric = 1.d0
!    logical :: nofrenkel = .false.
!    real*8 :: tdm_scaling_factor = 1.d0
    
    !monomer states
    integer:: mon_kount
!    real*8, allocatable:: mon_h(:, :)
!    real*8, allocatable:: mon_eval(:)
    
    !system states
    integer:: sys_kount
    real*8, allocatable:: sys_h(:, :)
    real*8, allocatable:: sys_eval(:)
    
    !intermolecule charge transfer
    logical:: ct_state = .false.
    logical:: ct_truncate = .false.
    integer:: es_a = 3, es_c = 4
    integer:: leftdonor = 1, acceptor = 2
    real*8:: es_lambda_z(4)=(/0.d0, 1.d0, 0.5d0, 0.5d0/)
    
    !harmonic potential well shift
    real*8:: lambda_n
    real*8:: lambda_z
    real*8:: lambda_a
    real*8:: lambda_c
    
    !absorption & pl spectra
    real*8, allocatable:: ab_osc_x(:)
    real*8, allocatable:: ab_osc_y(:)
    real*8, allocatable:: ab_osc_z(:)
    real*8, allocatable:: ab_x(:)
    real*8, allocatable:: ab_y(:)
    real*8, allocatable:: ab_z(:)
    real*8, allocatable:: pl_osc_x(:)
    real*8, allocatable:: pl_osc_y(:)
    real*8, allocatable:: pl_osc_z(:)
    real*8, allocatable:: pl_x(:)
    real*8, allocatable:: pl_y(:)
    real*8, allocatable:: pl_z(:)
    logical:: calc_pl = .false.
    integer:: spec_step = 5000
    real*8:: spec_start_ab,spec_end_ab
    real*8:: spec_start_pl,spec_end_pl
    logical:: lorentzian = .false.
    real*8:: abs_lw = 200.d0
    logical:: no_frenkel = .false.
    logical:: nearest_neighbor = .true.
    
    integer:: pl_start
    
    !temperature
!    integer:: tnum=13
!    real*8, allocatable :: em_temp(:)
    
    !normalization for spectra
    logical:: normalization = .true.

    !periodic 
    logical:: periodic = .false. 
    
    !state index
    integer::es_zmax = 2
    
    type basis
        integer::es_state
        integer::vib
    end type basis
    
!    type mon
!        integer:: es_state
!        integer:: vib
!    end type mon
!    
!    type sys
!        type(mon), allocatable:: molecule_state(:)
!    end type sys
    
!    type(mon), allocatable:: mon_state(:)
!    type(sys), allocatable:: sys_state(:)
    type(basis), allocatable:: mon_state(:)
    type(basis), allocatable:: sys_state(:,:)
    
    !physical constants
    !constants for au to cm-1 conversion
    real*8, parameter:: pi = datan(1.d0) * 4

    !dynamics
    complex*16, allocatable :: current_state(:)
    complex*16, parameter :: complex_zero = (0.d0, 0.d0), img = (0.d0, 1.d0)
    real*8, allocatable :: alpha_coeff(:)
    real*8, parameter:: hbar = 5.308837D-12        !cm-1 * s
    real*8:: time_step
    integer:: current_t, t_max
    real*8,allocatable:: cohere(:)
    real*8,allocatable:: probability(:)
    
    ! real*8, allocatable:: cpos(:), probsum(:,:)
    ! logical:: first_value
    
    !defect
    logical:: defect = .false.                !Is there a defect in the chain?
    integer:: defect_site = 2                !Defect site
    real*8:: defect_energy = 0.d0
    
end module
