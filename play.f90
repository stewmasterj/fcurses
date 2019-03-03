program play
use iso_c_binding
use fcurses
implicit none
interface
 subroutine usleep(useconds) bind(C)
  use iso_c_binding !, only : usleep
  implicit none
  integer(c_int32_t), value :: useconds
 end subroutine
end interface
integer(kind=4) :: t,i,ln,cl
character, dimension(5) :: ch
character(60) :: line
character(3) :: Fc
real, dimension(2) :: bull
real :: dt, bs
logical :: NoBull

dt = 0.01  ! seconds
! set blocks per second
bs = 100.0 ! blocks per second, 1 per dt
NoBull = .true.

call init_screen( "/tmp" )
call cls
write(6,*) "press F10 to quit, ESC+char=char+128"
write(6,'(a7,i4.3,a10,i4.3,a6)',advance='no') "lines: ",lines, &
        " columns: ",cols,char(27)//'[0;0H'
ln=0;cl=0;t=0

do      !main do loop for capturing keys
 call usleep(nint(dt*1e6)) !save CPU cycle time by polling keyboard 100 times per second
 t=t+1
 if (kbhit().eq.1) then
  call getkey(ch)
  !!! Interpretation of ctrl chars and such
  if (ch(1).eq.char(27)) then
   if (ch(2).eq.'[') then
     Fc=ch(3)//ch(4)//ch(5)    !curser keys work
     if (Fc.eq.'D  '.and.cl.ne.1) then
      call tput(" ", cl, ln);  cl=cl-1
     elseif (Fc.eq.'C  '.and.cl.ne.cols) then
      call tput(" ", cl, ln);  cl=cl+1
     elseif (Fc.eq.'A  '.and.ln.ne.1) then
      call tput(" ", cl, ln);  ln=ln-1
     elseif (Fc.eq.'B  '.and.ln.ne.lines) then
      call tput(" ", cl, ln);  ln=ln+1
     elseif (Fc.eq.'21~') then   !F10 Quit Exit
      write(6,*)
      exit
     endif
   endif
  elseif (ch(1).eq." ".and.NoBull) then
   ! create a bullet form the cursor position
   bull(:) = (/real(cl), real(ln)/) !current location of cursor
   NoBull = .false.
  elseif (ch(1).eq.'q') then   !Quit Exit
   write(6,*)
   exit
  else
   write(6,'(a1)',advance='no') ch(1); cl=cl+1
  endif
 endif ! end keystroke

 if (.not.NoBull) then
  ! Draw bullet and you
  call tput(" ", nint(bull(1)), nint(bull(2)))
  bull(2)=bull(2)-bs*dt ! moving up so negative
  if (nint(bull(2)).le.1) then
   NoBull = .true.
  else ! it is within bounds
   call tput("|", nint(bull(1)), nint(bull(2)))
  endif
 endif

 ! draw yourself
 call tput("A", cl, ln)

!write(6,'(5Z2.2,7a)') ch," "," "," ",ch(2:5)
enddo

call kill_screen( "/tmp" )
end program play


