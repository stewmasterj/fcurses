! vim:fdm=marker
!         type 'za' to refold the folds
module fcurses

!=============================================================================80
!
! fcurses module: Fortran curses
!
! init_screen( DIR )
!      gets the properties of the terminal screen
!      sets variables: cols, lines, and a clear_screen
!
! kill_screen( DIR )
!      returns terminal to cooked mode
!
! cls
!      clear the terminal
!
! draw_screen( scrn )
!      draws a given screen to the terminal on standard output FD=6
!
! sput( str, col, line, scrn )
!      write a string to position (col, line) of screen
!      Screen is just a long string buffer so escape or control characters
!      will mess it up.
!
! sget( str, col, line, leng, scrn )
!      get a string from position (col, line) of screen for a length of leng
!
! tput( str, col, line )
!      write a string to position (col, line) of the terminal
!      This allows for escape sequences in the string for colour etc.
!
! getkey( key )
! OLD     returns the value of a key press in a 5 character array: key
!
! getfullkey( key, n )
!      returns the value of a key press
!      optional, n, provides byte length of scan code
!
! getrawline( line, term )
!	captures a string from keyboard when in raw mode, terminated by byte 'term'
!
! sattr( str, val )
!      set the attributes of the string to val= font, colour etc.
!
! scset( str, val )
!      set the character set for the string, val: USA="B" UK="A" DEC="0"
!
! tplot( XY, pr, sr )
!      plot data XY within the plot range pr into a terminal domain sr
!
! tbox( x1, y1, x2, y2 )
!      Draw a rectangular box to the terminal directly
!
! tbox8( x1, y1, x2, y2 )
!      Draw a rectangular box to the terminal directly using UTF-8
!
! tline( x1, y1, x2, y2 )
!      Draw a line from point to point to the terminal directly
!
! tlinepolar( x, y, r, theta )
!      Draw a line from point with radius and angle to the terminal directly
!
! beep( freq, dur )
!      Beeps at the Frequency and Duration specified by writing to /dev/tty1
!
! music( freq, dur, N )
!      Beeps at the Frequencies and Durations in array of length N by writing 
!      to /dev/tty1
!
! drawtimg( atpx, N, x, y )
!       draws a terminal-pixel array to the screen at position x y.
!
! drawtpx( str, x, y )
!       draws a terminal-pixel to the screen at position x y.
!
! getEntry( str, x, y, retkey )
!       Edit a character string at location x y and return it with the last key
!
! Author: Ross J. Stewart
! Date: Wednesday, November 4, 2015
! Date: Saturday,  January 30, 2016
!============================================================================80
use iso_c_binding, only : C_INT
implicit none
interface
   function kbhit () bind (C, name="kbhit")
      use iso_c_binding, only : C_INT
      integer(kind=C_INT) :: kbhit
   end function
end interface
integer(4) :: lines, cols
character(len=:), allocatable :: clear_screen
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
type tpx !terminal pixel, array this for an image
   character(len=3) :: ch ! allow 3 bytes for UTF-8
   integer :: a, fg, bg, lx, ly
end type
type(tpx) :: NULL_tpx=tpx(char(0),0,10,10,0,0)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! init_screen( tmpDir )
!      gets the properties of the terminal screen and saves to tmpDir
!      sets variables: cols, lines, and a clear_screen
subroutine init_screen( tmp )  !{{{
implicit none
integer(4) :: i
character(60) :: line
character(*) :: tmp

! must make a system call to save current terminal settings
call system("stty -g > "//trim(tmp)//"/screen")
call system("stty raw -echo")
call system("stty -a > "//trim(tmp)//"/screen0")
open(10,file=adjustl(trim(tmp))//'/screen0')
   read(10,'(a60)') line
close(10,status='delete')
call unlink(adjustl(trim(tmp))//"/screen0")
do i=1,60
 if (line(i:i+2) .eq. '; c') then
  read (line(i-3:i-1),'(I3)') lines
 elseif (line(i:i+2) .eq. '; l') then
  read (line(i-3:i-1),'(I3)') cols
  exit
 endif
enddo

allocate(character(len=lines*cols) :: clear_screen )

do i=1, lines*cols
   clear_screen(i:i)=" "
enddo

end subroutine init_screen  !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! kill_screen( tmpDir )
!      returns terminal to cooked mode, use same tempDir
subroutine kill_screen( tmp ) !{{{
character(*) :: tmp
integer :: e
! set cursor to bottom left and put terminal into cooked mode
write(6,'(a2,i3.3,a3)') char(27)//'[',lines,';0H'
!call system("stty `cat "//trim(tmp)//"/screen`")
call execute_command_line("stty `cat "//trim(tmp)//"/screen`", wait=.true., exitstat=e)
if (e.eq.0) return
end subroutine  !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! cls
!      clear the terminal
subroutine cls !{{{
write(6,*) char(27)//'[2J'//char(27)//'[0;0H'
end subroutine cls !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! draw_screen( scrn )
!      draws a given screen to the terminal on standard output FD=6
subroutine draw_screen( scrn ) !{{{
implicit none
character(*), intent(in) :: scrn

write(6,'(A)') char(27)//'[0;0H'//scrn

end subroutine draw_screen !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! sput( str, col, line, scrn )
!      write a string to position (col, line) of screen
!      Screen is just a long string buffer so escape or control characters
!      will mess it up.
subroutine sput( str, col, line, scrn ) !{{{
implicit none
character(*), intent(in) :: str
character(*), intent(inout) :: scrn
integer, intent(in) :: col, line
integer :: pos, leng

! length of the string to write
leng = len( str )
pos = cols*(line-1) + col

scrn(pos:pos+leng-1) = str
end subroutine sput  !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! sget( str, col, line, leng, scrn )
!      get a string from position (col, line) of screen for a length of leng
subroutine sget( str, col, line, leng, scrn ) !{{{
implicit none
character(*), intent(inout) :: str
character(*), intent(in) :: scrn
integer, intent(in) :: col, line
integer :: pos, leng

pos = cols*(line-1) + col

str = scrn(pos:pos+leng-1)

end subroutine sget  !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! tput( str, col, line )
!      write a string to position (col, line) of the terminal
!      This allows for escape sequences in the string for colour etc.
subroutine tput( str, col, line ) !{{{
implicit none
character(*), intent(in) :: str
integer, intent(in) :: col, line

! set cursor position
write(6,'(a2,i3.3,a1,i3.3,a)',advance='no') char(27)//'[',line,';',col,'H'//str

end subroutine tput !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! getkey( key )
! OLD     returns the value of a key press
subroutine getkey(c)  !{{{
implicit none
integer :: i
character, dimension(5), intent(out) :: c

c=" "
i=1
do while(i.lt.6)
   !call fget(c(i))   !F77 format
   call fgetc(5,c(i)) !new format
   if (c(1).ne.char(27)) then
      return            ! just return if it's not an escape sequence
   elseif (i.gt.1.and.c(2).ne.char(27).and.c(2).ne.'['.and.c(2).ne.'O') then
      return ! ^[A
   elseif (iachar(c(3)).gt.63) then
      return            ! ^[[D
   elseif (c(4).eq.'~') then
      return                    ! ^[[1~
!   elseif (i.eq.5) then
!       i=5
   endif
   i=i+1
enddo

end subroutine getkey !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! getfullkey( key, n )
!      returns the value of a key press
!      optional, n, provides byte length of scan code
subroutine getfullkey(c,n) !{{{
integer :: i, err
integer, optional, intent(out) :: n
character, dimension(7), intent(out) :: c

c=char(0)
do i=1,7
   call fgetc(5,c(i),err)
   if (kbhit().eq.0) then
     if (present(n)) n = i
     return !check if the buffer is empty yet
   endif
enddo

end subroutine getfullkey !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! getrawline( line, term, echo )
!	captures a string from keyboard when in raw mode, terminated by byte 'term'
subroutine getrawline( line, term, echo ) !{{{
implicit none
character(*) :: line
character :: term
logical :: echo
character, dimension(7) :: c
integer :: n, i, l, j

l=len(line) !how big is this string?
line = ""
i = 0
do 
  call getfullkey(c,n)
  if (c(1).eq.term) return
  if (echo) write(6,'(A)',advance="no") c(1:n)
  if (i.gt.l) then
    write(0,*) "getrawline: buffer overflow"
    return
  endif
  do j = 1, n
    line(i+j:i+j) = c(j)
  enddo
  i = i + n
enddo

end subroutine getrawline !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! fancygetrawline( line, term, echo )
!	captures a string from keyboard when in raw mode, terminated by byte 'term'
!       can use backspace key. Arrows and tab completion in future?
subroutine fancygetrawline( line, term, echo ) !{{{
implicit none
character(*) :: line
character :: term, BS
logical :: echo
character, dimension(7) :: c
character(len=7) :: DEL, LEFT, RIGHT
integer :: n, i, l, j, p

DEL=char(0); LEFT=char(0); RIGHT=char(0)
BS        = achar(127) !normal delete code is usually mapped to backspace per VT220
DEL(1:4)  = achar(27)//"[3~"    !delete key is weird per VT220
LEFT(1:3) = achar(27)//"[D"
RIGHT(1:3)= achar(27)//"[C"
l=len(line) !how big is this string?
line = ""
i = 0; p = 1
do 
  call getfullkey(c,n)
  if (c(1).eq.term) return
  if (i.gt.l) then
    write(0,*) "getrawline: buffer overflow"; return
  endif
  !forall (i=1:n) cs(i:i)=c(i)
  if (c(1).eq.BS) then
  !if (c(1).eq.BS(1:1)) then
    line(i:i) = ""
    i = max(0,i - 1); p = max(1,p - 1)
    write(6,'(A)',advance="no") LEFT(1:3)//" "//LEFT(1:3)
    cycle
  !elseif (cs.eq.DEL) then
  !  line(p:l-1) = line(p+1:l)
  !  i = i - 1; cycle
  !elseif (cs.eq.LEFT) then
  !  p = p - 1; cycle
  !elseif (cs.eq.RIGHT) then
  !  p = p + 1; cycle
  endif
  if (echo) write(6,'(A)',advance="no") c(1:n)
  do j = 1, n
    line(i+j:i+j) = c(j)
  enddo
  i = i + n;  p = p + n
enddo

end subroutine fancygetrawline !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! sattr( str, val )
!      set the attributes of the string to val= font, colour etc.
subroutine sattr( str, val ) !{{{
implicit none
character(*), intent(inout) :: str
character(*), intent(in) :: val
! 1  bright
! 2  faint
! 4  underline
! 7  negative
! 8  conceal
! 9  strikethrough
!foreground colour background !!!
!30  dark grey 40
!31  red       41
!32  green     42
!33  yellow    43
!34  blue      44
!35  purple    45
!36  cyan      46
!37  white     47
!39  default   49

! val='0'        reset to default
! val='39;49'    default
! val='1;31;40'  bright red on grey
str = char(27)//'['//trim(adjustl(val))//'m'//trim(str)//char(27)//'[0m'
end subroutine sattr  !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! scset( str, val )
!      set the character set for the string, val: USA="B" UK="A" DEC="0"
subroutine scset( str, val ) !{{{
implicit none
character(*), intent(inout) :: str
character(*), intent(in) :: val
character(len=2) :: G0
!G0 Sets Seq   G1 Sets Seq   Meaning
!  ESC ( A       ESC ) A       United Kingdom Set
!  ESC ( B       ESC ) B       ASCII Set
!  ESC ( 0       ESC ) 0       DEC Special Graphics
!  ESC ( 1       ESC ) 1       Alternate Character ROM Standard Character Set
!  ESC ( 2       ESC ) 2       Alternate Character ROM Special Graphic   

G0 = '('//trim(adjustl(val))
str = char(27)//G0//trim(str)//char(27)//'(B'
end subroutine scset !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! tplot( XY, pr, sr )
!      plot data XY within the plot range pr into a terminal domain sr
subroutine tplot( XY, pr, sr ) !{{{
implicit none
real(4), dimension(:,:), intent(in) :: XY, pr
integer, dimension(2,2), intent(in) :: sr
integer :: i, x, y
real(4) :: dx, dy

! if there's too much data to fit it must be binned
dx = (pr(1,2)-pr(1,1))/float(sr(1,2)-sr(1,1)-1)
dy = (pr(2,2)-pr(2,1))/float(sr(2,2)-sr(2,1)-1)

do i=1, size(XY,2) 
   x = int(XY(1,i)/dx+pr(1,1)) + sr(1,1) + 1
   y = -int((XY(2,i)+pr(2,2))/dy) + sr(2,2) - 1
   ! check if out or on terminal domain bounds
   if ( (x.ge.sr(1,2).or.x.le.sr(1,1)).and. &
      & (y.ge.sr(2,2).or.y.le.sr(2,1)) ) cycle
   call tput( "@", x, y )
enddo

end subroutine tplot !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! tbox( x1, y1, x2, y2 )
!      Draw a rectangular box to the terminal directly
subroutine tbox( x1, y1, x2, y2, op) !{{{
implicit none
integer, optional, intent(in) :: op
integer(kind=4) :: x1,x2,y1,y2,w,i
character(len=x2-x1+1) :: top, mid, bar, bot
w = x2-x1+1
do i =2, w-1
 top(i:i)=char(113) !horizontal line
 mid(i:i)=" " !fill in the middle with spaces
enddo
bar=top
! draw corner symbols
top(1:1)=char(108)
top(w:w)=char(107)
bot     = top !bottom same as top except for corners
bot(1:1)=char(109)
bot(w:w)=char(106)
! draw edges
mid(1:1)=char(120)
mid(w:w)=char(120)
bar(1:1)=char(116)
bar(w:w)=char(117)

write(6,'(a)') char(27)//'(0'  ! to DEC line drawing mode
write(6,'(a2,i3.3,a1,i3.3,a)') char(27)//'[',y1,';',x1,'H'//top
do i=(y1+1),(y2-1) ! loop through height
   write(6,'(a2,i3.3,a1,i3.3,a)') char(27)//'[',i,';',x1,'H'//mid
enddo
if (present(op)) then
   write(6,'(a2,i3.3,a1,i3.3,a)') char(27)//'[',y1+2,';',x1,'H'//bar
endif
write(6,'(a2,i3.3,a1,i3.3,a)') char(27)//'[',y2,';',x1,'H'//bot
write(6,'(a)') char(27)//'(B' ! back to USA ASCII mode
end subroutine tbox !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! tbox8( x1, y1, x2, y2 )
!      Draw a rectangular box to the terminal directly (in UTF-8)
subroutine tbox8( x1, y1, x2, y2, op) !{{{
implicit none
integer, optional, intent(in) :: op
integer(kind=4) :: x1,x2,y1,y2,w,i
character(len=3*(x2-x1+1)) :: top, mid, bar, bot
w = (x2-x1+1)*3
do i =4, w-3, 3
 top(i:i+3)=char(226)//char(148)//char(129) !horizontal line
 mid(i:i+3)=" " !fill in the middle with spaces
enddo
bar=top
! draw corner symbols
top(1:3)=char(226)//char(148)//char(143)
top(w-2:w)=char(226)//char(148)//char(147)
bot     = top !bottom same as top except for corners
bot(1:3)=char(226)//char(148)//char(151)
bot(w-2:w)=char(226)//char(148)//char(155)
! draw edges
mid(1:3)=char(226)//char(148)//char(131)
mid(w/3+2:w/3+5)=char(226)//char(148)//char(131)
bar(1:3)=char(226)//char(148)//char(163)
bar(w-2:w)=char(226)//char(148)//char(171)

write(6,'(a2,i3.3,a1,i3.3,a)') char(27)//'[',y1,';',x1,'H'//top
do i=(y1+1),(y2-1) ! loop through height
   write(6,'(a2,i3.3,a1,i3.3,a)') char(27)//'[',i,';',x1,'H'//trim(mid)
enddo
if (present(op)) then
   write(6,'(a2,i3.3,a1,i3.3,a)') char(27)//'[',y1+2,';',x1,'H'//bar
endif
write(6,'(a2,i3.3,a1,i3.3,a)') char(27)//'[',y2,';',x1,'H'//bot
end subroutine tbox8 !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! tline( x1, y1, x2, y2 )
!      Draw a line from point to point to the terminal directly
subroutine tline(x1,y1,x2,y2) !{{{
implicit none
integer(kind=4) :: i,x1,x2,y1,y2
 if ((x2-x1).ne.0 .and. abs((y2-y1)/(x2-x1)).lt.1) then
  do i=min(x1,x2),max(x1,x2)
    write(6,'(a2,i3.3,a1,i3.3,2a1)') char(27)//'[',PS(i,x1,y1,x2,y2),';',i,'H', "*"
  end do
 else
  do i=min(y1,y2),max(y1,y2)
    write(6,'(a2,i3.3,a1,i3.3,2a1)') char(27)//'[',i,';',PS(i,y1,x1,y2,x2),'H', "*"
  end do
 endif
Contains
 integer function PS(i,x1,y1,x2,y2)
  implicit none
  integer(kind=4) :: i,x1,x2,y1,y2
  !I forgot point slope form
    PS=y2-((x2-i)*(y2-y1))/(x2-x1)
 end function PS
end subroutine tline !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! tlinepolar( x, y, r, theta )
!      Draw a line from point with radius and angle to the terminal directly
subroutine tlinepolar(x,y,r,theta) !{{{
implicit none
integer(kind=4) :: x,y,theta,r
real(kind=4) :: deg
 deg=theta/57.2957795130823
 !write(6,*) nint(r*cos(deg)), x,y, r, deg
 !    write(6,*) nint(r*sin(deg))
  call tline(x,y,x+nint(r*cos(deg)),y-nint(r*sin(deg)))
end subroutine tlinepolar !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! beep( freq, dur )
!      Beeps at the Frequency and Duration specified by writing to /dev/tty1
subroutine beep(freq,dur) !{{{
use iso_c_binding
implicit none
interface
 subroutine usleep(useconds) bind(C)
  use iso_c_binding !, only : usleep
  implicit none
  integer(c_int32_t), value :: useconds
 end subroutine
end interface
integer :: freq, dur

open(1,file="/dev/tty1") !tty1 has user write privaliges
write(1,'(A,i5.5,A,i4.4,A)') char(27)//'[10;',freq,']'//char(27)//'[11;',dur,']'//char(7)
call usleep(dur*1000) !wait the duration it will play
close(1)
end subroutine beep !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! music( freq, dur, N )
!      Beeps at the Frequencies and Durations in array of length N by writing 
!      to /dev/tty1
subroutine music(freq,dur, N) !{{{
use iso_c_binding
implicit none
interface
 subroutine usleep(useconds) bind(C)
  use iso_c_binding !, only : usleep
  implicit none
  integer(c_int32_t), value :: useconds
 end subroutine
end interface
integer, intent(in) :: N
integer, dimension(:), intent(in) :: freq, dur
integer :: i
character(len=5) :: pre
character(len=6) :: mid
character(len=2) :: post

!a single note takes 22 bytes

pre=char(27)//'[10;'
mid=']'//char(27)//'[11;'
post=']'//char(7)

! try to open the /dev/tty1 device
open(1,file="/dev/tty1") !tty1 has user write privaliges
do i = 1, N
   write(1,'(A,i5.5,A,i4.4,A)') pre,freq(i),mid,dur(i),post
   call usleep(dur(i)*1000) !wait the duration it will play
enddo
close(1)
end subroutine music !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! drawtimg( atpx, N, x, y )
!       draws a terminal-pixel array to the screen at position x y.
subroutine drawtimg(atpx, N, x, y) !{{{
type(tpx), dimension(:), intent(in) :: atpx !terminal pixel array
!   character(len=1) :: ch
!   integer :: a, fg, bg, lx, ly
integer, intent(in) :: N, x, y
integer :: i
character(len=2) :: tmp
character(len=20) :: val
character(len=80) :: str

val = ""

do i = 1, N
   ! set attribute, write integer to string
   write(tmp,'(i1)') atpx(i)%a;  val = trim(tmp)
   if (atpx(i)%fg.lt.10) then
      ! set foreground colour
      write(tmp,'(i2)') atpx(i)%fg+30;  val = trim(val)//';'//trim(tmp)
   endif
   if (atpx(i)%bg.lt.10) then
      ! set background colour
      write(tmp,'(i2)') atpx(i)%bg+40;  val = trim(val)//';'//trim(tmp)
   endif
   str = trim(atpx(i)%ch)
   call sattr( str, val) ! returns 'str' with attribute codes
   ! plot the terminal-pixel
   call tput(trim(str), atpx(i)%lx + x, atpx(i)%ly + y)
enddo
end subroutine drawtimg !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! drawtpx( str, x, y )
!       draws a terminal-pixel to the screen at position x y.
subroutine drawtpx(atpx, x, y) !{{{
type(tpx), intent(in) :: atpx !terminal pixel array
integer, intent(in) :: x, y
character(len=2) :: tmp
character(len=20) :: val
character(len=80) :: str

val = ""

! set attribute, write integer to string
write(tmp,'(i1)') atpx%a;  val = trim(tmp)
if (atpx%fg.lt.10) then
   ! set foreground colour
   write(tmp,'(i2)') atpx%fg+30;  val = trim(val)//';'//trim(tmp)
endif
if (atpx%bg.lt.10) then
   ! set background colour
   write(tmp,'(i2)') atpx%bg+40;  val = trim(val)//';'//trim(tmp)
endif
str = trim(atpx%ch)
call sattr( str, val) ! returns 'str' with attribute codes
! plot the terminal-pixel
call tput(trim(str), atpx%lx + x, atpx%ly + y)
end subroutine drawtpx !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! getEntry( str, x, y, retkey )
!       Edit a character string at location x y and return it with the last key
subroutine getEntry(str, xl, yl, retkey) !{{{
character(*), intent(inout) :: str, retkey
integer, intent(in) :: xl, yl
integer :: pos
character, dimension(5) :: ch

! place the default text
call tput( trim(str), xl, yl)
pos = len_trim(str) + 1 !position of cursor
retkey = " "
do
   call getkey(ch)
   retkey = ch(1)//ch(2)//ch(3)//ch(4)//ch(5)
   if (ch(1).eq.char(27)) then
      if (retkey(2:4).eq.char(91)//char(51)//char(126)) then !DELETE
         str(pos:pos) = " " !cursor should stay put
      else 
         Return
      endif
   elseif (ch(1).eq.char(09)) then !TAB
      Return
   elseif (ch(1).eq.char(13)) then !ENTER
      Return
   elseif (ch(1).eq.char(127)) then !BACKSPACE
      str(pos-1:pos-1) = " "; pos = pos - 1
      !call tput( "", xl+pos-1, yl )
      write(6,'(a7)',advance='no') char(27)//"[D "//char(27)//'[D'
   else
      ! set typed character into string at position
      str(pos:pos) = ch(1)
      pos = pos + 1
      call tput( str(pos-1:pos-1), xl+pos-2, yl )
   endif
enddo


end subroutine getEntry !}}}

end module fcurses
