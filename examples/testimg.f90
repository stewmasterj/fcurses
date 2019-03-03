program testimg
use fcurses
implicit none
type(tpx), dimension(8) :: timg

! initialize pixel array
timg(:) = NULL_tpx

call cls ! clear screen

timg(1)%ch = "+"; timg(1)%lx = 1; timg(1)%ly = 1
timg(2)%ch = "-"; timg(2)%lx = 2; timg(2)%ly = 1
timg(3)%ch = "+"; timg(3)%lx = 3; timg(3)%ly = 1
timg(4)%ch = "|"; timg(4)%lx = 1; timg(4)%ly = 2
timg(5)%ch = "|"; timg(5)%lx = 3; timg(5)%ly = 2
timg(6)%ch = "+"; timg(6)%lx = 1; timg(6)%ly = 3
timg(7)%ch = "-"; timg(7)%lx = 2; timg(7)%ly = 3
timg(8)%ch = "+"; timg(8)%lx = 3; timg(8)%ly = 3

call drawtimg(timg, 8, 10, 10)

timg(:)%a = 7
call drawtimg(timg, 8, 14, 10)

timg(:)%a = 1
timg(:)%bg = 4
call drawtimg(timg, 8, 18, 10)

timg(:)%a = 0
timg(:)%fg = 3
timg(:)%bg = 9 !default
call drawtimg(timg, 8, 22, 10)

end program testimg
