program timg
use fcurses
implicit none
integer(kind=4) :: i,j,ln,cl, err, n, cnt
character, dimension(5) :: ch
character(60) :: line, fil
character(3) :: Fc, cln, ccl
integer, dimension(3) :: off, cattr
type(tpx), dimension(:), allocatable :: sctmp
type(tpx), dimension(:,:), allocatable :: sc
logical :: FE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   Preamble
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
write(6,'(A)') "press F10 to quit, Enter to save"
write(6,'(A)') "Use ALT+[0..9]       for primary attribute:    " &
   &  //char(27)//'[0m0'//char(27)//'[1m1'//char(27)//'[0m'//char(27)//'[2m2' &
   &  //char(27)//'[0m'//char(27)//'[3m3'//char(27)//'[0m'//char(27)//'[4m4'  &
   &  //char(27)//'[0m'//char(27)//'[5m5'//char(27)//'[0m'//char(27)//'[6m6'  &
   &  //char(27)//'[0m'//char(27)//'[7m7'//char(27)//'[0m'//char(27)//'[8m8'  &
   &  //char(27)//'[0m'//char(27)//'[9m9'//char(27)//'[0m'
write(6,'(A)') "Use ALT+[q,w..p]     for foreground colour:    " &
   & //char(27)//'[30m0'//char(27)//'[31m1'//char(27)//'[32m2' &
   & //char(27)//'[33m3'//char(27)//'[34m4'//char(27)//'[35m5' &
   & //char(27)//'[36m6'//char(27)//'[37m7'//char(27)//'[0m'   &
   & //char(27)//'[38m8'//char(27)//'[39m9'//char(27)//'[0m'
write(6,'(A)') "Use ALT+SHIFT+[q..p] for background colour:    " &
   & //char(27)//'[40m0'//char(27)//'[41m1'//char(27)//'[42m2' &
   & //char(27)//'[43m3'//char(27)//'[44m4'//char(27)//'[45m5' &
   & //char(27)//'[46m6'//char(27)//'[47m7'//char(27)//'[0m'   &
   & //char(27)//'[48m8'//char(27)//'[49m9'//char(27)//'[0m'
write(6,'(A)') "Use ALT+c  to set the local image origin"


write(6,*) "Press any key to continue... or 'q' to quit"
call init_screen( "/tmp" )
call getkey(ch) ! just wait for a key stroke
if (ch(1).eq.'q') then
  call kill_screen( "/tmp" )
  STOP
endif
allocate( sc(cols,lines) ) !setup terminal-pixel
sc(:,:) = NULL_tpx
off=(/1,1,0/)
call cls

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   Read terminal image file if provided one
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
if (iargc().gt.0) then
 call getarg(1,fil)
 inquire(file=trim(fil), exist=FE)
 if (FE) then
  open(10,file=trim(fil))
  read(10,*)  
  n = 0
    do !loop to count lines
      read(10,*,iostat=err)
      if (err.ne.0) exit
      n = n + 1
    enddo
  allocate( sctmp(n) )
  rewind(10)
  read(10,*) line, off(1:2) 
  cnt = n
    do n = 1, cnt
      read(10,*,iostat=err) sctmp(n)
      i = sctmp(n)%lx+off(1)
      j = sctmp(n)%ly+off(2)
      sc(i,j) = sctmp(n)
      ! don't let 'sc' hold any positions until it's written
      sc(i,j)%lx = 0
      sc(i,j)%ly = 0
      !call drawtpx(sc(i,j), i, j)
    enddo
  close(10)
  call drawtimg(sctmp, cnt, off(1), off(2))
  deallocate( sctmp )
 else
  if (trim(fil).eq."-h".or.trim(fil).eq."--help") then
   write(0,*) "Terminal image editor"
   write(0,*) "Usage: editimg  FILE.tmg"
   call kill_screen( "/tmp" )
   STOP
  else
   write(0,*) "WARNING: No such file: "//trim(fil)
   call kill_screen( "/tmp" )
   STOP
  endif
 endif
endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   Begin Program
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ln=1;cl=1
cattr = (/0,10,10/)
call tput("", cl, ln)

do      !main do loop for capturing keys
 call getkey(ch)
 !!! Interpretation of ctrl chars and such
 if (ch(1).eq.char(27)) then
  select case (ch(2))
  case ('[') 
    Fc=ch(3)//ch(4)//ch(5)    !curser keys work
    if (Fc.eq.'D  '.and.cl.ne.1) then
    write(6,'(a3)',advance='no') char(27)//'[D';  cl=cl-1
    elseif (Fc.eq.'C  '.and.cl.ne.cols) then
    write(6,'(a3)',advance='no') char(27)//'[C';  cl=cl+1
    elseif (Fc.eq.'A  '.and.ln.ne.1) then
    write(6,'(a3)',advance='no') char(27)//'[A';  ln=ln-1
    elseif (Fc.eq.'B  '.and.ln.ne.lines) then
    write(6,'(a3)',advance='no') char(27)//'[B';  ln=ln+1
    elseif (Fc.eq.'21~') then   !F10 Quit Exit
     write(6,*); exit
    else
    write(6,'(a3)',advance='no') char(27)//'['//trim(Fc)
    endif
  case ('1'); sc(cl,ln)%a = 1 !ALT+# or ESC+# 
  case ('2'); sc(cl,ln)%a = 2 
  case ('3'); sc(cl,ln)%a = 3
  case ('4'); sc(cl,ln)%a = 4
  case ('5'); sc(cl,ln)%a = 5
  case ('6'); sc(cl,ln)%a = 6
  case ('7'); sc(cl,ln)%a = 7
  case ('8'); sc(cl,ln)%a = 8
  case ('9'); sc(cl,ln)%a = 9
  case ('0'); sc(cl,ln)%a = 0

  case ('q'); sc(cl,ln)%fg = 0
  case ('w'); sc(cl,ln)%fg = 1
  case ('e'); sc(cl,ln)%fg = 2
  case ('r'); sc(cl,ln)%fg = 3
  case ('t'); sc(cl,ln)%fg = 4
  case ('y'); sc(cl,ln)%fg = 5
  case ('u'); sc(cl,ln)%fg = 6
  case ('i'); sc(cl,ln)%fg = 7
  case ('o'); sc(cl,ln)%fg = 8
  case ('p'); sc(cl,ln)%fg = 9

  case ('Q'); sc(cl,ln)%bg = 0
  case ('W'); sc(cl,ln)%bg = 1
  case ('E'); sc(cl,ln)%bg = 2
  case ('R'); sc(cl,ln)%bg = 3
  case ('T'); sc(cl,ln)%bg = 4
  case ('Y'); sc(cl,ln)%bg = 5
  case ('U'); sc(cl,ln)%bg = 6
  case ('I'); sc(cl,ln)%bg = 7
  case ('O'); sc(cl,ln)%bg = 8
  case ('P'); sc(cl,ln)%bg = 9

  case ('c'); off = (/cl,ln,0/) ! set coordinate origin for data
  end select
 elseif (ch(1).eq.char(13)) then
!  write(6,'(a1)',advance='no') char(10);  ln=ln+1; cl=0 !remap Enter to be LF ^J not CR ^m
  call kill_screen( "/tmp" )
  call tput("Input a file name for the image data: ", 1, lines-1)
  read(5,*) fil
  open(10,file=trim(fil))
  write(10,*) "offset ", off(1), off(2) ! write offset
  do i=1,cols
     do j=1, lines
        if (sc(i,j)%ch.ne.char(0)) then
           write(10,'(a4,5i4)') sc(i,j)%ch, sc(i,j)%a, sc(i,j)%fg, sc(i,j)%bg, &
                  &   i-off(1), j-off(2)
        endif
     enddo
  enddo
  close(10)
  STOP
 elseif (ch(1).eq.char(127)) then
  sc(cl,ln) = NULL_tpx !backspace will clear the terminal-pixel
 else
  sc(cl,ln)%ch = ch(1)
  ! if No attributes use the last ones used
  if (sc(cl,ln)%a.eq.0 .and. sc(cl,ln)%fg.eq.10 .and. sc(cl,ln)%bg.eq.10) then
    sc(cl,ln)%a  = cattr(1)
    sc(cl,ln)%fg = cattr(2)
    sc(cl,ln)%bg = cattr(3)
  endif
 endif

 if (sc(cl,ln)%ch.ne.char(0)) then
  !plot the pixel but keep cursor on top
  call drawtpx(sc(cl,ln), cl, ln)
!  call tput("", cl, ln)
  ! save current attributes to use by default next time
  cattr(1) = sc(cl,ln)%a
  cattr(2) = sc(cl,ln)%fg
  cattr(3) = sc(cl,ln)%bg
 endif

 write(ccl,'(i3)') cl
 write(cln,'(i3)') ln
 call tput(ccl//'x'//cln, cols-8, lines)
 call tput("", cl, ln)

enddo

call kill_screen( "/tmp" )
end program timg
