program key
use iso_c_binding !, only : usleep
use fcurses
implicit none
interface
 subroutine usleep(useconds) bind(C)
  use iso_c_binding !, only : usleep
  implicit none
  integer(c_int32_t), value :: useconds
 end subroutine
end interface
integer :: i, cps, t
integer, dimension(8) :: date_time
character(len=10), dimension(3) :: b
character, dimension(5) :: c
real :: ltime, ctime

cps = 4 ! keyboard checks per second

! puts terminal into raw mode
call init_screen("/tmp")
call cls !clear screen
call tput("Press 'q' to quit", 1, 3)
call tput("TIME: ", 1, 4)
call tput("     timestep  quartersec    currenttime      lasttime   time since last key" , 1, 9)

c=" "
i=0; t=0; ctime=0.0
do
   i=i+1; t=t+1
   call usleep(1000000/cps)
   ! check whether the keyboard was used
   if (kbhit().eq.1) then
      ltime = ctime
      call date_and_time(b(1), b(2)) !, b(3), date_time)
      read(b(2),*) ctime
      call getkey(c) 
      if (c(1).eq."q") exit
      call tput(" " , 1, 10) ! trick to position cursor
      write(6,*) t, i, ctime, ltime, ctime-ltime 
   endif 
   ! Update the displayed time every second, no sooner
   if (i.eq.cps) then
      i=0
      call date_and_time(b(1), b(2)) !, b(3), date_time)
      call tput(b(2), 7, 2)  ! post the time at the top
   endif
enddo

! return terminal to before: cooked! return terminal to before: cooked
call kill_screen("/tmp")

end program key
