program test
use fcurses
implicit none
integer :: i
real(4), dimension(2,400) :: XY
real(4), dimension(2,2) :: pr
integer, dimension(2,2) :: sr
character, dimension(5) :: key

! make data to plot
do i=1, 400
   XY(1,i) = 3.1415926535*float(i)/200.0
   XY(2,i) = sin(XY(1,i)) + sin(5.0*XY(1,i)) + 0.2*sin(130.0*XY(1,i)) + &
           &  0.2*sin(1300.0*XY(1,i))
enddo

! X and Y data ranges
pr(1,:) = (/ 0.0, 3.14159*2.0 /)
pr(2,:) = (/ -2.0, 2.0 /)

call init_screen( "/tmp" )
 call cls ! clear screen

 ! X and Y plot location, screen range [sr] as fractions of terminal size
 sr(1,:) = (/ 0.1*cols, 0.9*cols /)
 sr(2,:) = (/ 0.1*lines, 0.9*lines /)
 call tplot( XY, pr, sr )
 write(6,*) "press any key to make small plot."
 key=" "
 call getkey( key ) !effectively pauses execution untill keypress
! call cls

 ! new X and Y plot location
 sr(1,:) = (/ 0.1*cols, 0.5*cols /)
 sr(2,:) = (/ int(0.4*lines), lines-1 /)
 call tbox8( sr(1,1), sr(2,1), sr(1,2), sr(2,2) )
 call tplot( XY, pr, sr )

! write(6,*) "press any key to enter interactive plot. type 'q' to quit"
! key=" "
! call getkey( key )
! if (key(1).eq.'q') then
!    write(6,*) "pressed q"
! endif

 !do

 !enddo

call kill_screen( "/tmp" )

end program test
