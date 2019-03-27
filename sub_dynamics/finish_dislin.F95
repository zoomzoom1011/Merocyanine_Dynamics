!***************************************************************************************!
!    Slow down the visualization a little
!***************************************************************************************!
subroutine finish_dislin()
    implicit none

    call winopt( 1, 'delay' )            !set dealy time to 1 second
    call winmod( 'delay')                !automatically close the window after some delay
    call errmod('protocol', 'off')      !turn off the exit message.
    call disfin()                                !wrap up dislin
end subroutine