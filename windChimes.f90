program windchimes
use fcurses
implicit none
integer :: fs(5), i, dmin, dmax, cnt
integer, dimension(20) :: f, d
real :: rnd

! Giant, Ominous Wind Chimes
!  https://milwaukeemakerspace.org/2011/09/giant-ominous-wind-chimes/
fs(1:5) = (/294, 415, 440, 554, 587/)
dmin = 200 !half second
dmax = 1000 !1 second max??

write(0,*) "random sequence and duration of 20 notes"
write(0,*) " duration range (msec): ",dmin,dmax
cnt = 0
do
   cnt = cnt + 1
   write(0,*) "seq:", cnt
   do i = 1, 20
      call random_number(rnd)
      f(i) = fs(int(rnd*5.0)+1)
      call random_number(rnd)
      d(i) = int(rnd*(dmax-dmin))+dmin
   enddo
   write(0,*) f
   write(0,*) d

   call music(f, d, 20)

enddo

end program windchimes
