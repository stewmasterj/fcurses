program test
use fcurses
implicit none
integer :: i, j
character, dimension(5) :: c

! puts terminal into raw mode
call init_screen( "/tmp" )

c=" "
j=0
do !infinite loop
   j=j+1
   write(6,*) "Press a key!!! ",j,achar(13)  !implicit LF after writing
   ! call C function kbhit() to determine whether STDIN has buffer contents
   if (kbhit().eq.1) then
      call getkey(c)
      write(6,'(a4,5i3.3,x,5Z2.2,x,6a)') "key:",iachar(c),c,c,achar(13)
      do i=1,10
         write(6,*) "I could have kept going, you know. I paused for YOU!"//achar(13)
      enddo
      exit
   endif
enddo

! return terminal to before: cooked
call kill_screen( "/tmp" )

end program test
