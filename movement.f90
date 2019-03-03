!compile this program with: gfortran -lc -o play play.f90 readkey.o


program play
use fcurses
implicit none

! interface for sub inkey(int) a C function
 interface
! I named this c function INKEY after the old BASIC function
!  character(C_SIGNED_CHAR) function inkey() bind(C)
   subroutine inkey(a,b,c,d,e) bind(c)
!(1),chr(2),chr(3),chr(4),chr(5)) bind(c)
   use iso_c_binding
    implicit none
    character(C_CHAR) :: a,b,c,d,e
   end subroutine
!  end function
 end interface

integer(kind=4) :: i,ln,cl
character, dimension(5) :: ch
character(60) :: line
character(3) :: Fc

!call system("stty -a > /tmp/screen")
!open(10,file='/tmp/screen')
! read(10,'(a60)') line
!close(10,status='delete')
!call system("rm -f /tmp/screen")
!do i=1,60
! if (line(i:i+2) .eq. '; c') then
!  read (line(i-3:i-1),'(I3)') lines
! elseif (line(i:i+2) .eq. '; l') then
!  read (line(i-3:i-1),'(I3)') cols
!  exit
! endif
!enddo
call init_screen( "/tmp" )
call cls
write(6,*) "press F10 to quit, ESC+char=char+128"
write(6,'(a7,i4.3,a10,i4.3,a6)',advance='no') "lines: ",lines, &
        " columns: ",cols,char(27)//'[0;0H'
ln=0;cl=0

do      !main do loop for capturing keys
 !call inkey(ch(1),ch(2),ch(3),ch(4),ch(5))
 call getkey(ch)
 !!! Interpretation of ctrl chars and such
 if (ch(1).eq.char(27)) then
  if (ch(2).eq.'[') then
    Fc=ch(3)//ch(4)//ch(5)    !curser keys work
    if (Fc.eq.'D  ') then
    write(6,'(a3)',advance='no') char(27)//'[D';  cl=cl-1
    elseif (Fc.eq.'C  ') then
    write(6,'(a3)',advance='no') char(27)//'[C';  cl=cl+1
    elseif (Fc.eq.'A  ') then
    write(6,'(a3)',advance='no') char(27)//'[A';  ln=ln-1
    elseif (Fc.eq.'B  ') then
    write(6,'(a3)',advance='no') char(27)//'[B';  ln=ln+1
    elseif (Fc.eq.'21~') then   !F10 Quit Exit
     write(6,*)
     exit
    else
    write(6,'(a3)',advance='no') char(27)//'['//trim(Fc)
    endif
  elseif (ch(2).eq.char(27)) then
   if (ch(3).eq.char(27)) then
   write(6,'(a1)',advance='no') char(155); cl=cl+1 !extended vhar
   else
   write(6,'(a1)',advance='no') ch(2); cl=cl+1
   endif
  else
  write(6,'(a1)',advance='no') char(iachar(ch(2))+128) !ESC+KEY=KEY+128 or extended char
  cl=cl+1
  endif
 elseif (ch(1).eq.char(13)) then
 write(6,'(a1)',advance='no') char(10);  ln=ln+1; cl=0 !remap Enter to be LF ^J not CR ^m
 elseif (ch(1).eq.char(127)) then
 write(6,'(a6)',advance='no') char(27)//'[D'//char(27)//'[P'; cl=cl-1  !backspace 
 elseif (ch(1).eq.char(11)) then
 write(6,'(a4)',advance='no') char(27)//'[4B'; ln=ln+4  ! vertical tab down 4 lines
 else
 write(6,'(a1)',advance='no') ch(1); cl=cl+1
 endif
!write(6,'(5Z2.2,7a)') ch," "," "," ",ch(2:5)
enddo

call kill_screen( "/tmp" )
end program play
