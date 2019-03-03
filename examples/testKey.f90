program test
use fcurses
implicit none
integer :: i
character, dimension(5) :: c

! puts terminal into raw mode
call init_screen( "/tmp" )

do i =1, 20 
   c=" "
   call getkey(c)
   !call fget(c(1))
write(6,'(a4,5(i3.3,x),x,5(Z2.2,x),a)') "key:",iachar(c),c,achar(13)

enddo

! return terminal to before: cooked
call kill_screen( "/tmp" )

end program test
