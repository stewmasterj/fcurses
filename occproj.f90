! Free source code
!compile: gfortran -c -fbounds-check occproj.f90
!compile: gfortran -lc -o occproj kbhit.o fcurses.o occproj.o
! vim:fdm=marker
! Ross J. Stewart
! Thursday, May 12, 2016
module shar
real(4), dimension(:,:,:), allocatable :: cell
real(4), dimension(:,:), allocatable :: pts
character(8), dimension(:), allocatable :: ele
integer, dimension(:,:), allocatable :: ptc
integer :: Ntot, axis
character(80) :: infile, tmpfile
character(:), allocatable :: scrn1
end module shar
!!!!!!!!!!!!!!!!!!!!!!
program occproj
use fcurses
use shar
implicit none
integer(kind=4) :: i,ln,cl
character, dimension(5) :: ch
character(60) :: line
character(3) :: Fc

tmpfile = "." !current directory

call init_screen( trim(tmpfile) )
if (cols/2.gt.lines) then
 write(0,*) "ERROR: terminal line number must be greater than half the column count", &
  &  " lines=",lines," columns=",cols
 call kill_screen (trim(tmpfile) )
 STOP
endif
call cls
axis = 3
allocate( cell(cols,cols,cols) )
allocate(character(len=lines*cols) :: scrn1 )
cell = 0.0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Read input file coordinates !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
call getarg(1, infile)
call readFile
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

call mkscreen

!write(6,*) "press F10 to quit, ESC+char=char+128"
!write(6,'(a7,i4.3,a10,i4.3,a6)',advance='no') "lines: ",lines, &
!        " columns: ",cols,char(27)//'[0;0H'
ln=0;cl=0

do      !main do loop for capturing keys
 call getkey(ch)
 !!! Interpretation of ctrl chars and such !{{{
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
 elseif (ch(1).eq.'x') then;   axis = 1; call mkscreen
 elseif (ch(1).eq.'y') then;   axis = 2; call mkscreen
 elseif (ch(1).eq.'z') then;   axis = 3; call mkscreen
 elseif (ch(1).eq.'q') then;   write(6,*); exit
 !elseif (ch(1).eq.char(13)) then
 !write(6,'(a1)',advance='no') char(10);  ln=ln+1; cl=0 !remap Enter to be LF ^J not CR ^m
 !elseif (ch(1).eq.char(127)) then
 !write(6,'(a6)',advance='no') char(27)//'[D'//char(27)//'[P'; cl=cl-1  !backspace 
 !elseif (ch(1).eq.char(11)) then
 !write(6,'(a4)',advance='no') char(27)//'[4B'; ln=ln+4  ! vertical tab down 4 lines
 !else
 !write(6,'(a1)',advance='no') ch(1); cl=cl+1
 endif !}}}
!write(6,'(5Z2.2,7a)') ch," "," "," ",ch(2:5)
enddo

call kill_screen( trim(tmpfile) )
end program occproj
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine readFile !{{{
use shar
use fcurses, only : cols, kill_screen
implicit none
integer :: i, maxcell, err
integer, dimension(3) :: c
real(4), dimension(3) :: mins, maxs, boxsize, frac

open(10,file=trim(infile),iostat=err)

if (err.ne.0) then
   write(0,*) "Cannot open file: "//trim(infile)
   call kill_screen( trim(tmpfile) )
endif

read(10,*) Ntot
read(10,*)

allocate( pts(Ntot,3), ele(Ntot), ptc(Ntot,3) )

do i = 1, Ntot
   read(10,*) ele(i), pts(i,:)
enddo

close(10)

!!!!!!!!!!!!!!!!!!!!!

do i = 1, 3
   mins(i) = minval(pts(:,i))
   maxs(i) = maxval(pts(:,i))
enddo
boxsize = maxs-mins

cell = 0.0
maxcell = 0
do i = 1, Ntot
   frac = (pts(i,:)-mins)/boxsize
   c    = int( frac*real(cols-1) ) + 1
   ptc(i,:) = c ! save the cell coordinate for this particle
   cell(c(1),c(2),c(3)) = cell(c(1),c(2),c(3)) + 1.0
   if (cell(c(1),c(2),c(3)) > real(maxcell)) then
      maxcell = int(cell(c(1),c(2),c(3)))
   endif
enddo

!divide by max value to get normalized values to display
if (maxcell > 0 ) cell = cell/real(maxcell)

end subroutine readFile !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine mkscreen !{{{
use shar
use fcurses
implicit none
integer :: i, j, loc
character(69) :: greyscl
character(1) :: pt
greyscl  = '$@B%8&WM#*oahkbdpqwmZO0QLCJUYXzcvunxrjft/\|()1{}[]?-_+~<>i!lI;:,"^`. '

scrn1 = clear_screen !will this clear the string?
call draw_screen( clear_screen )

do i = 1, cols
   do j = 1, cols/2  !vertical direction block is two
      if (axis == 1) then
          loc = int( (1.0 - ((sum(cell(:,i,j*2))+sum(cell(:,i,j*2-1))) &
                 &  /real(2*cols)))*68.0 ) + 1
          pt = greyscl(loc:loc)
          call sput( pt, i, j+2, scrn1 )
      elseif (axis == 2) then
          loc = int( (1.0 - ((sum(cell(i,:,j*2))+sum(cell(i,:,j*2-1))) &
                 & /real(2*cols)))*68.0 ) + 1
          pt = greyscl(loc:loc)
          call sput( pt, i, j+2, scrn1 )
      elseif (axis == 3) then
          loc = int( (1.0 - ((sum(cell(i,j*2,:))+sum(cell(i,j*2-1,:))) &
                 & /real(2*cols)))*68.0 ) + 1
          pt = greyscl(loc:loc)
          call sput( pt, i, j+2, scrn1 )
      endif
   enddo
enddo

call draw_screen( scrn1 )

end subroutine mkscreen !}}}


