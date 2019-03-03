program testmusic
use fcurses
implicit none
integer, dimension(23) :: f, d

write(6,*) "testing a single beep at 130 Hz and 100 milliseconds"
call beep(130,100)
write(6,*) "wait a sec... for Axel Foley's theme song"
call sleep(1)
! Axel Foley’s theme
!  http://blog.dhampir.no/content/fun-with-beep
f( 1:13) = (/659, 784, 659, 659, 880, 659, 587, 659, 988, 659, 659, 1047, 988/)
f(14:23) = (/784, 659, 988, 1318, 659, 587, 587, 494, 740, 659/)
d( 1:13) = (/460, 340, 230, 110, 230, 230, 230, 460, 340, 230, 110,  230, 230/)
d(14:23) = (/230, 230, 230,  230, 110, 230, 110, 230, 230, 460/)

call music(f, d, 23)
write(6,*) "You're welcome :)"
call sleep(1)

call beep(784,100)
call beep(0,40)
call beep(784,100)
call beep(0,40)
call beep(784,100)
call beep(0,70)
call beep(784,500)
call beep(622,500)
call beep(698,400)
call beep(784,200)
call beep(0,150)
call beep(698,200)
call beep(784,800)
write(6,*) "Victory!!!"

! try on commandline this:
!beep -f 659 -l 460 -n -f 784 -l 340 -n -f 659 -l 230 -n -f 659 -l 110 -n -f 880 -l 230 -n -f 659 -l 230 -n -f 587 -l 230 -n -f 659 -l 460 -n -f 988 -l 340 -n -f 659 -l 230 -n -f 659 -l 110 -n -f 1047-l 230 -n -f 988 -l 230 -n -f 784 -l 230 -n -f 659 -l 230 -n -f 988 -l 230 -n -f 1318 -l 230 -n -f 659 -l 110 -n -f 587 -l 230 -n -f 587 -l 110 -n -f 494 -l 230 -n -f 740 -l 230 -n -f 659 -l 460
end program testmusic
