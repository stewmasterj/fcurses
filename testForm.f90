! Example of an input form
!  special guest:  call getEntry( str, xl, yl, key)
! Ross J. Stewart, February 12, 2016
program testform
use fcurses
implicit none
character(len=5) :: key
character(len=80) :: tmp
character(len=40),dimension(5) :: str
character, dimension(5) :: c
integer :: i, yy

do i = 1, 5
   str(i) = ""
enddo

call init_screen('/tmp')
call cls !clear screen
!make a box
call tbox8( 10, 10, 44, 24 )
! set the format and text fields
call tput( "First Name     [               ]", 11, 12 )
call tput( "Middle Initial [ ]", 11, 14 )
call tput( "Last Name      [               ]", 11, 16 )
call tput( "D.O.B.         [          ]", 11, 18 )
call tput( "Phone Number   [            ]", 11, 20 )
call tput( "Enter", 20, 22 )
call tput( "Cancel", 30, 22 )

yy = 12; i = 1 ! we need to count where our focus is
do  ! this loop could probably have a Select Case statement for each field
   ! get the entry the user edits
   call getEntry( str(i), 27, yy, key )
   if (key(1:1).eq.char(13).or.key(1:1).eq.char(09)) then !ENTER or TAB
      yy = yy + 2; i = i + 1
      ! special condition for the Enter and Cancel buttons
      if (yy.eq.22) then
         tmp = 'Enter'
         call sattr( tmp, '7' )
         call tput( trim(tmp), 20, 22 )
         call getkey( c )
         if (c(1).eq.char(13)) exit !exit loop
         if (c(1).eq.char(09)) then
            tmp = 'Enter' !erase the highlight
            call tput( trim(tmp), 20, 22 )
            tmp = 'Cancel'
            call sattr( tmp, '7' )
            call tput( trim(tmp), 30, 22 )
            call getkey( c )
            if (c(1).eq.char(13)) then
               call kill_screen('/tmp'); STOP
            endif
            yy = 12; i = 1
            tmp = 'Cancel'  !erase the highlight
            call tput( trim(tmp), 30, 22 )
         endif
      endif
   endif ! end if the key
enddo

call kill_screen('/tmp')

do i = 1, 5  ! write out what the user inputted
   write(6,*) "you entered: "//str(i)
enddo

end program testform
