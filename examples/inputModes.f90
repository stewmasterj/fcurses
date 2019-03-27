! alternate between raw and cooked keyboard modes, like vim's input vs command modes
! compile: gfortran -c ../kbhit.c ../fcurses.f90 ../../stringParseMods/lineParse.f90 inputModes.f90
! compile: gfortran -lc -o inputModes fcurses.o kbhit.o lineParse.o inputModes.o
program inputmodes
use fcurses
integer :: j
character, dimension(7) :: c, f
character(120) :: line
 
write(6,*) "returns decimal and hex values of keyboard scan codes"
write(6,*) "   q   quits"
write(6,*) "   :   enters command mode. records keys into string. terminated with Enter"
write(6,*)
write(6,*) "commands:"
write(6,*) " q     exit command mode"
write(6,*) " ^[    exit command mode (ESC)"
write(6,*) " test  test string"
write(6,*) " wc    returns number of words in Command"

call init_screen( "/tmp" ) ! puts terminal into raw mode
do
   call getfullkey(c)
   if (c(1).eq."q") exit
   if (c(1).eq.":") then
     write(6,'(A)',advance="no") ":"
     call getrawline( line, achar(13), .true. ) !echo the keys
     write(6,'(A)') achar(13)
     call interpretLine( line )
     cycle
   endif
   do j = 1, 7
     if (iachar(c(j)).ge.32) then; f(j) = c(j)
     else;                         f(j) = "_"; endif
   enddo
   write(6,'(a4,7(i3.3,x),x,7(Z2.2,x),x,7(A),a)') "key:",iachar(c),c,f,achar(13)
enddo
call kill_screen( "/tmp" ) ! return terminal to before: cooked
 
end program inputmodes
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine interpretLine( s )
use lineParse
implicit none
integer :: wc
character(*) :: s
character(80) :: word 

! this routine could be used to set variables or perform complex things that a
! single input character can't

wc = s_word_count( s )
word = s_get_word( 1, s )
select case(trim(word))
  case("q") ;; return
  case(char(27)) ;; return
  case("test") ;; write(6,*) "test string found"//achar(13)
  case("wc") ;; write(6,*) "command word count: ",wc,achar(13)
  case default;; write(6,'(A)') "don't have interpretation for: '"//trim(word)//"'"//achar(13)
end select

end subroutine interpretLine
