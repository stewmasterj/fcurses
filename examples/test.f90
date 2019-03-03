program test
use fcurses
implicit none
character(:), allocatable :: scrn1
character(len=30) :: sgot

! identify the properties of the terminal screen
! and set a null screen called: clear_screen
call init_screen( "/tmp" )
write(0,*) "lines: ", lines, " columns: ", cols

! Allocate a screen buffer string to write to
allocate(character(len=lines*cols) :: scrn1 )

! initialize the screen to a blank one
scrn1 = clear_screen
call draw_screen( clear_screen )
! write a string to a column and line location for a specific screen
call sput( "hello", cols/4, lines/3, scrn1 )
call sput( "@", 1, 2, scrn1 )
call sput( "@", cols, lines, scrn1 )

call sget( sgot, cols/4, lines/3, 30, scrn1 )
call draw_screen( scrn1 )

call tput( "override text", 40, lines/2 )
call tput( char(27)//'[31m'//"color text"//char(27)//'[0m', 40, lines/2+1 )
write(0,*) "I got: ", sgot
call kill_screen( "/tmp" )

end program test
