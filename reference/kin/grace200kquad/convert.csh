#!/bin/tcsh
sed s/"implicit double precision"/"implicit real(16)"/I $1 | sed s/"(a-h,o-z)"/"(a-h,o-z)\n      implicit integer (i-n)"/I | sed s/"d0"/"q0"/Ig | sed s/"double complex"/"complex(16)"/I | sed s/"double precision"/"real(16)"/I > new/$1
