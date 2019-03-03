#!/bin/bash

CO="kbhit.o fcurses.o"
FC=gfortran

if [ -z $1 ]; then

rm *.o *.mod 
gcc -c kbhit.c
$FC -c fcurses.f90 
$FC -c test.f90
$FC -c testKey.f90
$FC -c testANSI.f90
$FC -c testPlot.f90
$FC -c movement.f90
$FC -c testkbhit.f90
$FC -c keytiming.f90
$FC -c testMusic.f90
$FC -c windChimes.f90
$FC -c play.f90
$FC -c play1.f90
$FC -c play2.f90
$FC -c testimg.f90
$FC -c editimg.f90
$FC -c viewtimg.f90
$FC -c testEdit.f90
$FC -c testForm.f90
$FC -c occproj.f90
$FC -c grabkeys.f90
 

$FC -lc -o test       $CO test.o
$FC -lc -o testkey    $CO testKey.o
$FC -lc -o testANSI   $CO testANSI.o
$FC -lc -o testPlot   $CO testPlot.o
$FC -lc -o movement   $CO movement.o
$FC -lc -o testMusic  $CO testMusic.o
$FC -lc -o windChimes $CO windChimes.o
$FC -lc -o testimg    $CO testimg.o
$FC -lc -o editimg    $CO editimg.o
$FC -lc -o viewtimg   $CO viewtimg.o
$FC -lc -o testEdit   $CO testEdit.o
$FC -lc -o testForm   $CO testForm.o
$FC -lc -o occproj    $CO occproj.o
$FC -lc -o testkbhit  $CO testkbhit.o
$FC -lc -o keytiming  $CO keytiming.o
$FC -lc -o play       $CO play.o
$FC -lc -o play1      $CO play1.o
$FC -lc -o play2      $CO play2.o
$FC -lc -o grabkeys   $CO grabkeys.o

else
 if [ $1 = "clean" ]; then
  rm *.o *.mod
 fi
fi
