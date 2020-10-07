#!/bin/csh
# ===============================================
#      readevi.com
#                    FLR 
# 970826
# run $ALEPH/bin/readevi.exe
# ==================================================
# P1 = file name to be read
# ====================================================
#set echo on

if (-e fort.1) rm fort.1
ln -fs $1 fort.1
readevi.exe

exit
