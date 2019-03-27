!***************************************************************************************!
!    Slow down the visualization a little
!***************************************************************************************!
subroutine setup_dislin()
    use common_variables
    use dislin
    implicit none

    Integer:: frame_rate = 15                 !ms pause between frame.
    
        !REAL TIME
        !Initialize PLplot
        call WINDOW( 200, 50, 1000, 800)                              !Set size and position of graph window
!!!        call METAFL('xwin')                                                        !Declare the graphics driver
!!!        call DISINI()                                                                  !Initialize DISLIN
!!!        call PAGERA()                                                                !Create a border around the page
!!!        call NAME( 'Probability', 'Y')                                           !Label the Y axis
!!!!        call TITLIN( 'Time Dependence of Wavefunction', 1 )  !Title the graph
!!!        call THKCRV( 4 )                                                            !Set line thickness
!!!        call POLCRV( 'FBARS' )                                                    !Set interpolation type
!!!        call TICKS( 1, 'XY' )                                                        !Set to one tick.
!!!        call BUFMOD( 'OFF', 'SENDFB' )                                        !So ENDGRF doesn't call SENDBF
!!!        call TICLEN( 1, 1 )                                                         !Set tic length to very small
!!!        call VKYTIT( -50 )                                                           !Set graph titles to just above the axis
!!!        call LABDIG( -1, 'X' )                                                                    !Set the labels to integers

        !Initialize the x-axis
        Do n = 1, Lattice_X
                pos( n ) = n*1.D0
        End Do

        !File To plot prob as fxn of t on molecule 10
        f_no = 25
        f_name = trim(task_title)//'_prob.dat'
        Open( unit = f_no, file=f_name )
        Write(f_no, *) 'Time, Site'
        Write(f_no, * ) 'fs ', ( n, n = 1, Lattice_X )
        Write(f_no, * )
!        Write( f_no, * ) ' ', (n, n, n = 1, kount ) 
        
        Do time = 0, sim_time

            !Write the data to a file when does the frenkel get to the end?
            Write( f_no, * ) time, ( frenkel_pos_expect(n),  n=1, Lattice_X )

            !Give some time between frames
            call slumber( frame_rate )                                                       !Sleep for 15 ms to give allusion of a video  
                
!                write( time_out, '(a7, i5)' ) 'fs/10: ', time                              !Display the time on the graph
            write( time_out, '(a4, i5, a4)' ) 't=  ', time, '  fs'                              !Display the time on the graph
            time_out = trim(time_out)
            
            !To output for movie
            call METAFL('PNG')
            Write( tmp, * ) time
            call setfil( trim(task_title)//'_'//trim(adjustl(tmp))//'.png' )
            call SCRMOD('REVERSE')        !Make BG White
            call DISINI()
            call PAGERA()
            call NAME( 'Probability', 'Y')                                           !Label the Y axis
            call NAME( 'Site', 'X')                                                   !Label the X axis
            call THKCRV( 4 )                                                            !Set line thickness
            call POLCRV( 'FBARS' )                                                    !Set interpolation type
            call LABDIG( -1, 'X' )                                                                    !Set the labels to integers
            call GRAF( min_x, max_x, min_x, Lattice_X/10.D0, &
                    min_y, max_y, 0.D0, 0.5D0)
!                call TITLIN( 'Time Evolution of an Exciton', 1 )
            call TITLE()
            call COLOR('BLUE')
            call CURVE( pos, frenkel_pos_expect, Lattice_X )
            call COLOR('White')
            call MESSAG( time_out, 1200, 250 )
            call ERRMOD('protocol', 'off')      !Turn off the exit message.
            call ENDGRF()
            call DISFIN()
                        
        End Do

End Subroutine