! compile: gfortran -c grabkeys.f90
! compile: gfortran -lc -o grabkeys fcurses.o kbhit.o grabkeys.o
program grabkeys
use fcurses
integer :: j
character, dimension(7) :: c, f
 
call init_screen( "/tmp" ) ! puts terminal into raw mode
do
   call getfullkey(c)
   if (c(1).eq."q") exit
   do j = 1, 7
     if (iachar(c(j)).ge.32) then; f(j) = c(j)
     else;                         f(j) = "_"; endif
   enddo
   write(6,'(a4,7(i3.3,x),x,7(Z2.2,x),x,7(A),a)') "key:",iachar(c),c,f,achar(13)
enddo
call kill_screen( "/tmp" ) ! return terminal to before: cooked
 
!contains
!subroutine getfullkey(c)
!integer :: i, err
!character, dimension(7), intent(out) :: c
! 
!c=" "
!do i=1,7
!   call fgetc(5,c(i),err)
!   if (kbhit().eq.0) return !check if the buffer is empty yet
!enddo
! 
!end subroutine getfullkey
end program grabkeys
