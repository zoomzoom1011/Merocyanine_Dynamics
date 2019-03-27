subroutine mkdir(task_title)
    implicit none
    character*100 task_title
    
    call system('if [ ! -d '//trim(task_title)//' ]; then '//&
                'mkdir '//trim(task_title)//'; fi' )
    print*, 'Entering ./'//trim(adjustl(task_title))
    call chdir( task_title )
    
end subroutine