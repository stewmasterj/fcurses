program ANSIseq
implicit none
character :: key
write(6,*) char(27)//'[2J'	!erase entire screen
write(6,*) "1:cursor up:", char(27)//'[A', "1 2:cursor down:", char(27)//'[B', "2"
write(6,*) "3:cursor forward 4:cursor backward   ", char(27)//'[2C', "3", char(27)//'[2D', "4"
write(6,*)
write(6,*)
write(6,*) "5:cursor on previous line", char(27)//'[F', "5"
write(6,*)
write(6,*) "6:cursor on next line",  char(27)//'[E', "6"
write(6,*)
write(6,*) "7:cursor to col 30", char(27)//'[30G', "7"
!write(6,*) "saving cursor position", char(27)//'[s'
write(6,*) "8:red cursor to row 20 col 32 H", char(27)//'[20;32H', char(27)//'[31m', "8", char(27)//'[0m'
!write(6,*) char(27)//'[u'
write(6,*)
!write(6,*) "restore cursor position, then print this a line below"
write(6,*) "will now erase to the begining of the screen, press enter"
read(5,'(1a)') key
write(6,*) "9:moving cursor to row 13 col 40 f", char(27)//'[13;40f', "9"
write(6,*) "device status: ", char(27)//'[6n'
write(6,*) "Code	Effect"
write(6,*) char(27)//'[0m', "0   reset"
write(6,*) char(27)//'[1m', "1   Bright"
write(6,*) char(27)//'[2m', "2   Faint"
write(6,*) char(27)//'[3m', "3   Italic"
write(6,*) char(27)//'[4m', "4   Underline"
write(6,*) char(27)//'[5m', "5   Blink slow"
write(6,*) char(27)//'[6m', "6   Blink rapid"
write(6,*) char(27)//'[7m', "7   Negative"
write(6,*) char(27)//'[8m', "8   Conceal"
write(6,*) char(27)//'[9m', "9   Crossed out"
write(6,*) char(27)//'[10m', "10   Primary font"
write(6,*) char(27)//'[11m', "11   Alt font1"
write(6,*) char(27)//'[12m', "12   Alt font2"
write(6,*) char(27)//'[13m', "13   Alt font3"
write(6,*) char(27)//'[14m', "14   Alt font4"
write(6,*) char(27)//'[15m', "15   Alt font5"
write(6,*) char(27)//'[16m', "16   Alt font6"
write(6,*) char(27)//'[17m', "17   Alt font7"
write(6,*) char(27)//'[18m', "18   Alt font8"
write(6,*) char(27)//'[19m', "19   Alt font9"
write(6,*) char(27)//'[20m', "20   Fraktur"
write(6,*) char(27)//'[21m', "21   Double underline"
write(6,*) char(27)//'[22m', "22   Normal color or intensity"
write(6,*) char(27)//'[23m', "23   Not italic not fraktur"
write(6,*) char(27)//'[24m', "24   Underline none"
write(6,*) char(27)//'[25m', "25   Blink off"
write(6,*) char(27)//'[26m', "26   Reserved"
write(6,*) char(27)//'[27m', "27   image Positive"
write(6,*) char(27)//'[28m', "28   reveal"
write(6,*) char(27)//'[29m', "29   not crossed out"
write(6,*) char(27)//'[30m', "30   Set text colour", char(27)//'[31m', "31", &
	char(27)//'[32m',"32",char(27)//'[33m',"33",char(27)//'[34m',"34", &
	char(27)//'[35m',"35",char(27)//'[36m',"36",char(27)//'[37m',"37"
write(6,*) char(27)//'[38m', "38   reserved"
write(6,*) char(27)//'[39m', "39   default text colour"
write(6,*) char(27)//'[40m', "40   Set background colour", char(27)//'[41m', "41", &
	char(27)//'[42m',"42",char(27)//'[43m',"43",char(27)//'[44m',"44", &
	char(27)//'[45m',"45",char(27)//'[46m',"46",char(27)//'[47m',"47"
write(6,*) char(27)//'[48m', "48   reserved"
write(6,*) char(27)//'[49m', "49   Default background colour"
write(6,*) char(27)//'[50m', "50   reserved"
write(6,*) char(27)//'[51m', "51   Framed"
write(6,*) char(27)//'[52m', "52   enCircled"
write(6,*) char(27)//'[53m', "53   Overlined"
write(6,*) char(27)//'[54m', "54   not framed or encircled"
write(6,*) char(27)//'[55m', "55   not overlined"
write(6,*) char(27)//'[56m', "56-59   reserved"
write(6,*) char(27)//'[60m', "60   ideogram underline or right side line"
write(6,*) char(27)//'[61m', "61   ideogram double underline or double line on the right side"
write(6,*) char(27)//'[62m', "62   ideogram overline or left side line"
write(6,*) char(27)//'[63m', "63   ideogram double overline or double line on the left side"
write(6,*) char(27)//'[64m', "64   ideogram stress marking"
write(6,*)

end program ANSIseq
