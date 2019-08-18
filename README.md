
## fcurses is the FORTRAN Curses Library

These routines were tested in GNU Linux with xterm family of terminals

### Author: Ross J. Stewart
### Date: Saturday, November 7, 2015


## Possibilities:  
With these routines it would be possible to write a program that could interact
 with the plot routine for user interactive plotting.
Forms could be designed for data entry.

## Warnings
Music and sound require write access to /dev/tty1, or you can change what terminal it writes beeps to.  
If the keyboard doesn't work on your system when running the examples, it may use different scan codes for the keys, try using the grabkeys.f90 program to see.

## Requirements
This does make some initial external system calls using 'stty'.
Tested with gfortran a part of gcc version 6.3.0

## Example Files:  
-ANSIseq    A program that exemplifies ANSI escape sequences for terminal control  
-test       Shows the use of character and string placement to screens  
-testkey    The function getkey can be used to read raw keyboard input  
-testANSI   shows ways to apply attributes to text such as colour  
-testPlot   can plot data points into a subdomain of the terminal screen  
-movement   move the curser around interactively with arrow keys  
-testMusic  tests playing music in terminal by changing the frequency and duration of Beeps  
-windChimes plays "music" of random tones, like windchimes  
-testimg    displays a test pattern on the terminal  
-editimg    move cursor to position and set character and attributes and save  
-viewtimg   displays a previously saved terminal image  
-testEdit   displays a text entry box for editing  
-testForm   example of a form to fill out  
-occproj    projects a density map from an XYZ file, use keys x,y,z to change view axis (cyl.xyz is an example)  
-testkbhit  runs program while waiting for a keyboard key press. No native fortran can do this.  
-keytiming  counts the time since the last keypress.  
-play       first step, a character that moves and can fire a blaster with "Space"  
-play1      passive space invaders that you can use as target practice, plus sound!  
-play2      active space invaders that you must shoot down!  
-grabkeys   displays the Decimal and Hex values of the 7-Bytes in a keyboard scancode.  
-inputModes same as 'grabkeys' but includes a "command mode" when ":" is pressed.  

## Routines

-`init_screen( tmpDir )`  
gets the properties of the terminal screen and saves to tmpDir sets variables: cols, lines, and a clear\_screen.

-`kill_screen( tmpDir )`  
returns terminal to cooked mode, use same tempDir

-`cls`  
clear the terminal

-`draw_screen( scrn )`  
draws a given screen to the terminal on standard output FD=6

-`sput( str, col, line, scrn )`  
write a string to position (col, line) of screen. Screen is just a long string buffer so escape or control characters will mess it up.

-`sget( str, col, line, leng, scrn )`  
get a string from position (col, line) of screen for a length of leng

-`tput( str, col, line )`  
write a string to position (col, line) of the terminal. This allows for escape sequences in the string for colour etc.

-`getkey( key )`  
*OLD*  returns the value of a key press (only uses 5-Bytes for keybpoard scan codes, and tries to estimate the end of them.)

-`getfullkey( key )`  
returns the value of a key press

-`getrawline( line, term, echo )`  
captures a string from keyboard when in raw mode, terminated by byte 'term'

-`sattr( str, val )`  
set the attributes of the string to val= font, colour etc.

-`scset( str, val )`  
set the character set for the string, val: USA="B" UK="A" DEC="0"

-`tplot( XY, pr, sr )`  
plot data XY within the plot range pr into a terminal domain sr

-`tbox( x1, y1, x2, y2 )`  
Draw a rectangular box to the terminal directly

-`tbox8( x1, y1, x2, y2 )`  
Draw a rectangular box to the terminal directly (in UTF-8)

-`tline( x1, y1, x2, y2 )`  
Draw a line from point to point to the terminal directly

-`tlinepolar( x, y, r, theta )`  
Draw a line from point with radius and angle to the terminal directly

-`beep( freq, dur )`  
Beeps at the Frequency and Duration specified by writing to /dev/tty1

-`music( freq, dur, N )`  
Beeps at the Frequencies and Durations in array of length N by writing to /dev/tty1

-`drawtimg( atpx, N, x, y )`  
draws a terminal-pixel array to the screen at position x y.

-`drawtpx( str, x, y )`  
draws a terminal-pixel to the screen at position x y.

-`getEntry( str, x, y, retkey )`  
Edit a character string at location x y and return it with the last key





