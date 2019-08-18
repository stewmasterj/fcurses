program test
use fcurses
implicit none
integer :: i, j, k
character :: cs
character(len=80) :: str
character(len=38) :: chl
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

!do i=1, 55
!   write(str,'(i2)') i
!   call sattr( str, str )
!   write(6,'(i3.3,x,11Z3,2x,a)') i, (str(j:j),j=1,11), trim(str)
!enddo
!
!str = 'faint'
!call sattr( str, '31;40')
!write(6,'(a)')  str
!str = 'bright'
!call sattr( str, '1;31;40')
!write(6,'(a)')  str
!
!G0 Sets Seq   G1 Sets Seq   Meaning
!  ESC ( A       ESC ) A       United Kingdom Set
!  ESC ( B       ESC ) B       ASCII Set
!  ESC ( 0       ESC ) 0       DEC Special Graphics
!  ESC ( 1       ESC ) 1       Alternate Character ROM Standard Character Set
!  ESC ( 2       ESC ) 2       Alternate Character ROM Special Graphic  
do k=1,3
  if (k.eq.1) then
     cs='B'
     write(6,*) "United States (USASCII) character set"
  elseif (k.eq.2) then
     cs='A'
     write(6,*) char(10),"United Kingdom (UK) character set " !,char(27)//'('//cs
  elseif (k.eq.3) then
     cs='0'
     write(6,*) char(10),"DEC Special Character and Line Drawing set" !,char(27)//'('//cs
  endif
  write(6,'(a6)') "00 000"
 do i=16,112,16
  chl="                                                  "
  do j=0, 15
     chl(2*(j+1):2*(j+1)) = char(i+j)
  enddo
  call scset( chl, cs )
  write(6,'(Z2.2,I4.3,1x,a38)') i,i,chl
 enddo
  write(6,*)
 do i=128,240,16 !write extended set
  chl="                                                  "
  do j=0, 15
     chl(2*(j+1):2*(j+1)) = char(i+j)
  enddo
  call scset( chl, cs )
  write(6,'(Z2.2,I4.3,1x,a38)') i,i,chl
 enddo
enddo

write(6,'(a)') char(27)//'(B'
end program test
