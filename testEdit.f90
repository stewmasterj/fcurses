program testedit
use fcurses
implicit none
character(len=18) :: str, key
character, dimension(5) :: c

str = "default text"

call init_screen('/tmp')
call cls !clear screen
!make a box
call tbox8( 10, 10, 30, 12 )
! get the entry the user edits
call getEntry( str, 11, 11, key )

call kill_screen('/tmp')

write(6,*) "you entered: "//str
c(1) = key(1:1)
c(2) = key(2:2)
c(3) = key(3:3)
c(4) = key(4:4)
c(5) = key(5:5)
write(6,'(a,5i3.3,x,5Z2.2)') "last key: ", iachar(c), c

end program testedit
