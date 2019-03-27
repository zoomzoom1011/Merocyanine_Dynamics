integer function sign_fxn(n)
    use common_variables
    implicit none
    
    real*8:: ux1(2),uy1(2),uz1(2)
    integer,intent(in):: n
    integer:: n1, da1
    
    ux1 = 0.d0
    uy1 = 0.d0
    uz1 = 0.d0
    
    sign_fxn = 0
    
    do n1 = 1, 2
        do da1 = 1, 2
            ux1(n1) = ux1(n1) + tq(es_z, da1)* molecule_pos(n1, da1, 1)
            uy1(n1) = uy1(n1) + tq(es_z, da1)* molecule_pos(n1, da1, 2)
            uz1(n1) = uz1(n1) + tq(es_z, da1)* molecule_pos(n1, da1, 3)
        end do
    end do
        
    if ((ux1(1)*ux1(2) + uy1(1)*uy1(2) + uz1(1)*uz1(2)) > 0) then
        sign_fxn = 1
    else if((ux1(1)*ux1(2) + uy1(1)*uy1(2) + uz1(1)*uz1(2)) < 0) then
        if(mod(n,2) == 1) then
            sign_fxn = 1
        else if(mod(n,2) == 0) then
            sign_fxn = -1
        end if
    end if

    return
end function
