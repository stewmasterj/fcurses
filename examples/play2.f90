program play
use iso_c_binding
use fcurses
implicit none
interface
 subroutine usleep(useconds) bind(C)
  use iso_c_binding !, only : usleep
  implicit none
  integer(c_int32_t), value :: useconds
 end subroutine
end interface
integer(kind=4) :: t,i,ln,cl,ne, kc, seed
character, dimension(5) :: ch
character(60) :: line, tmp
character(3) :: Fc
real, dimension(2) :: bull
real :: dt, bs, rn
logical :: NoBull
type evil
 logical :: killed, NB
 real :: bs
 real, dimension(2) :: pos, bull, vel
end type evil
type (evil), dimension(:), allocatable :: ev

dt = 0.01  ! seconds
! set bullet speed in blocks per second
bs = 100.0 ! blocks per second, 1 per dt
NoBull = .true.
ne = 10    ! number of enemies
seed = 12345
call random_seed(seed) ! random seed for enemey fire

call init_screen( "/tmp" )
call cls
write(6,*) "press F10 or q to quit. Space to shoot."

! initial character location on screen
ln=lines-2; cl=cols/2
call tput(" ", ln, cl ) 

! initialize the enemies
allocate( ev(ne) )
ev(:)%killed = .false.  !not killed
ev(:)%NB     = .true.   !no bullet
ev(:)%vel(2) = 1.0
ev(:)%bs     = 10.0  !enemy bullet velocity
do i=1,ne
 ev(i)%bull(:) = 0.0
 ! set equally spaced columns
 ev(i)%pos(1) = cols*i/(ne+1)
 ev(i)%pos(2) = -6.283    ! 2*pi before 0.0
enddo

t=0; kc=0
do      !main do loop for capturing keys
 call usleep(nint(dt*1e6)) !save CPU cycle time by polling keyboard 100 times per second
 t=t+1
 if (kbhit().eq.1) then
  call getkey(ch)
  !!! Interpretation of ctrl chars and such
  if (ch(1).eq.char(27)) then
   if (ch(2).eq.'[') then
     Fc=ch(3)//ch(4)//ch(5)    !curser keys work
     if (Fc.eq.'D  '.and.cl.ne.1) then
      call tput(" ", cl, ln);  cl=cl-1
     elseif (Fc.eq.'C  '.and.cl.ne.cols) then
      call tput(" ", cl, ln);  cl=cl+1
     elseif (Fc.eq.'A  '.and.ln.ne.1) then
      call tput(" ", cl, ln);  ln=ln-1
     elseif (Fc.eq.'B  '.and.ln.ne.lines) then
      call tput(" ", cl, ln);  ln=ln+1
     elseif (Fc.eq.'21~') then   !F10 Quit Exit
      call EndGame
     endif
   endif
  elseif (ch(1).eq." ".and.NoBull) then
   ! create a bullet form the cursor position
   bull(:) = (/real(cl), real(ln)/) !current location of cursor
   NoBull = .false.
   ! bullet sound
   call music( (/1450,1250/), (/10,10/), 2)
  elseif (ch(1).eq.'q') then   !Quit Exit
   call EndGame
  else
   write(6,'(a1)',advance='no') ch(1); cl=cl+1
  endif
 endif ! end keystroke

 if (.not.NoBull) then
  ! Draw bullet and you
  call tput(" ", nint(bull(1)), nint(bull(2)))
  bull(2)=bull(2)-bs*dt ! moving up so negative
  if (nint(bull(2)).le.1) then
   NoBull = .true.
  else ! it is within bounds
   call tput("|", nint(bull(1)), nint(bull(2)))
  endif
 endif

 ! draw yourself
 call tput("A", cl, ln)

 ! draw the enemies
 do i=1,ne
  if (.not.ev(i)%killed) then
   if (ev(i)%pos(2).ge.1.0) then
    call tput(" ", nint(ev(i)%pos(1)), nint(ev(i)%pos(2)) )
    ! also randomize the possibility of enemey fire
    call random_number(rn)
    if (rn.lt.dt.and.ev(i)%NB) then ! about once per second
     ev(i)%NB = .false.
     ev(i)%bull = ev(i)%pos  !set bullet position to the current position
    endif
   endif
   ! single drop speed
   ev(i)%pos(2) = ev(i)%pos(2) + ev(i)%vel(2)*dt
   ! sinosoidal horizontal oscilation based on height
   ev(i)%pos(1) = ev(i)%pos(1) + sin(ev(i)%pos(2))*dt*real(cols/(2*(ne+1))-1)
   ! Enemey reached base
   if (nint(ev(i)%pos(2)).ge.lines) then
    call tbox8( 10, lines/2-1, cols-10, lines/2+1)
    call tput("The Cylons infiltrated your base", cols/4+2, lines/2)
    call music( (/150,100/), (/500,700/), 2)
    call EndGame
   endif
   ! hero's bullet hit enemey
   if (.not.NoBull.and.nint(bull(1)).eq.nint(ev(i)%pos(1)) .and. &
                    &  nint(bull(2)).eq.nint(ev(i)%pos(2))) then
    ev(i)%killed = .true.;   tmp = "H"; NoBull = .true.; kc=kc+1
    call sattr(tmp,"1;30;41") !dark grey on bright red
    call tput(tmp, nint(bull(1)), nint(bull(2)) )
    ! death music
    call music( (/750,150,650,250,550/), (/10,10,10,10,10/), 5)
   endif
   ! draw the enemey if not killed
   if (ev(i)%pos(2).ge.1.0.and..not.ev(i)%killed) then
    if (ev(i)%pos(1).ge.cols) ev(i)%pos(1) = real(cols)
    if (ev(i)%pos(1).le.1)    ev(i)%pos(1) = 1.0
    call tput("H", nint(ev(i)%pos(1)), nint(ev(i)%pos(2)) )
   endif
  endif
  ! enemey bullets
  if (.not.ev(i)%NB) then
   ! Draw bullet
   call tput(" ", nint(ev(i)%bull(1)), nint(ev(i)%bull(2)))
   ev(i)%bull(2)=ev(i)%bull(2)+ev(i)%bs*dt
   if (nint(ev(i)%bull(2)).ge.lines) then
    ev(i)%NB = .true.
   else ! it is within bounds
    call tput("*", nint(ev(i)%bull(1)), nint(ev(i)%bull(2)))
   endif
   ! their bullet hit YOU!!
   if (nint(ev(i)%bull(1)).eq.cl.and.nint(ev(i)%bull(2)).eq.ln) then
    tmp = "A"; ev(i)%NB = .true.
    call sattr(tmp,"1;30;41") !dark grey on bright red
    call tput(tmp, nint(ev(i)%bull(1)), nint(ev(i)%bull(2)) )
    call music( (/150,100/), (/500,700/), 2)
    call EndGame
   endif
  endif
 enddo

 If (kc.eq.ne) then
    call tbox8( 10, lines/2-1, cols-10, lines/2+1)
    call tput("Congratulations you've Defeated the Cylons!!!", cols/4+2, lines/2)
    call music( (/784,0,784,0,784,0,784,622,698,784,0,698,784/), &
              & (/100,40,100,40,100,70,500,500,400,200,150,200,800/), 13)
    call EndGame
 endif

enddo

call kill_screen( "/tmp" )
end program play
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine Endgame
use fcurses
implicit none
write(6,*)  !for somereason I need this to exit
call kill_screen( "/tmp" )
STOP
end subroutine EndGame
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
