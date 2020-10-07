#!/bin/sh
#
#  call the new stagein or if it fails call the old one
#
#  Joel Closier   27 April 1994
#
para=""
while [ $# -gt 0 ]
do
 para="$para $1"
 shift
done

/aleph/script/alstagein5.1 $para
err=$?
if [ $err -ne 0 ]
  then
    /aleph/script/alstagein4.1 $para
fi
exit 0
