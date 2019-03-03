program viewtimg
use fcurses
implicit none
integer :: bufferSize, i, n, err
integer, dimension(2) :: off
type(tpx), dimension(:), allocatable :: timg
character(len=80) :: fil, line
logical :: FE
character, dimension(5) :: ch

!typical terminal screen size
bufferSize = 80*24

allocate( timg(bufferSize) )

if (iargc().eq.0) then
  write(0,*) "View terminal images"
  write(0,*) "Usage: viewtimg  FILE_LIST"
  STOP
endif

call init_screen("/tmp")

do i = 1, iargc()
 call getarg(i,fil)
 inquire(file=trim(fil), exist=FE)
 if (FE) then
  open(10,file=trim(fil))
  read(10,*,iostat=err) line, off(1:2)
  if (adjustl(trim(line)).ne."offset".or.err.ne.0) then
    write(0,*) "File: "//trim(fil)//" not a terminal image format"
    cycle
  endif

  timg(:) = NULL_tpx
  call cls
  n = 1
  do !loop to count lines
     read(10,*,iostat=err) timg(n)
     if (err.ne.0) exit
     n = n + 1
  enddo
  close(10)
  call drawtimg(timg, n, off(1), off(2))

  call tput("File:  "//trim(fil), 1, lines)

  call getkey(ch)
 endif
enddo

call kill_screen("/tmp")
end program viewtimg
