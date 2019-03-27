!*******************************************************************************!
!   build the geometry of molecule
!*******************************************************************************!
subroutine build_molecule_geometry()
    use common_variables
    implicit none
    
    integer n, da
    ! integer n1, n2
    !x: long axis 
    !y: short axis
    !z: pi stack

    !dipolar with three dimensions
    ! if (.not. allocated(molecule_pos)) allocate(molecule_pos(nmax,es_zmax,3))
    ! molecule_pos = 0.d0
    
!    do n = 1, nmax
!    do da = 1, 2 !dipolar
!        molecule_pos(n,da,1) = (n-1)*x_offset + (da -1.5d0) * armlength
!        molecule_pos(n,da,2) = (n-1)*y_offset
!        molecule_pos(n,da,3) = (n-1)*z_offset
!    end do
!    end do
    
!    print*, dcos(pi/2.d0)
!    do n = 1, nmax
!    do da = 1, 2 !dipolar
!            print*, dcos((n-1)*agg_angle) 
!        molecule_pos(n,da,1) = (-1)**(da-1)*armlength/2.d0*dcos((n-1)*agg_angle) 
!        molecule_pos(n,da,2) = (-1)**(da-1)*armlength/2.d0*dsin((n-1)*agg_angle) 
!        molecule_pos(n,da,3) = (n-1)*z_offset
!    end do
!    end do

    ! ref_pos()

    if (.not. periodic) then
        !dipolar with three dimensions
        if (.not. allocated(molecule_pos)) allocate(molecule_pos(nmax,es_zmax,3))
        molecule_pos = 0.d0

        do n = 1, nmax
        do da = 1, 2 !dipolar
            if (geometry == 1) then
                molecule_pos(n,da,1) = (n-1)*x_offset + (da -1.5d0) * armlength
            else if(geometry == 2) then 
                molecule_pos(n,da,1) = (n-1)*x_offset + (da -1.5d0) * armlength * (-1)**(n-1)
            else
                print'(a)',"geometry is wrong"
                exit
            end if
            molecule_pos(n,da,2) = (n-1)*y_offset
            molecule_pos(n,da,3) = (n-1)*z_offset
        end do
        end do
    else if (periodic .and. nearest_neighbor) then
        if (.not. allocated(molecule_pos)) allocate(molecule_pos(0:nmax+1,es_zmax,3))
        molecule_pos = 0.d0

        do n = 0, nmax + 1
        do da = 1, 2 !dipolar
            if (geometry == 1) then
                molecule_pos(n,da,1) = (n-1)*x_offset + (da -1.5d0) * armlength
            else if(geometry == 2) then 
                molecule_pos(n,da,1) = (n-1)*x_offset + (da -1.5d0) * armlength * (-1)**(n-1)
            else
                print'(a)',"geometry is wrong"
                exit
            end if
            molecule_pos(n,da,2) = (n-1)*y_offset
            molecule_pos(n,da,3) = (n-1)*z_offset
        end do
        end do
    else if (periodic .and. (.not. nearest_neighbor)) then
        if (.not. allocated(molecule_pos)) allocate(molecule_pos(2-nmax/2:nmax*3/2,es_zmax,3))
        molecule_pos = 0.d0

        do n = 2 - nmax/2,nmax*3/2
        do da = 1, 2 !dipolar
            if (geometry == 1) then
                molecule_pos(n,da,1) = (n-1)*x_offset + (da -1.5d0) * armlength
            else if(geometry == 2) then 
                molecule_pos(n,da,1) = (n-1)*x_offset + (da -1.5d0) * armlength * (-1)**(n-1)
            else
                print'(a)',"geometry is wrong"
                exit
            end if
            molecule_pos(n,da,2) = (n-1)*y_offset
            molecule_pos(n,da,3) = (n-1)*z_offset
        end do
        end do
    end if

    !allocate the charge
    if(.not. allocated(tq)) allocate(tq(es_zmax, 2))
    
    tq = 0
    tq(es_z,leftdonor) = 1
    tq(es_z,acceptor) = -1
    if(es_zmax > 2) then
        tq(es_c,leftdonor) = 1
        tq(es_a,acceptor) = -1
    end if

end subroutine