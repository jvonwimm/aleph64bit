#! /bin/csh
#============================================================================
#                             f.to.F
# 29/04/97                                                      F.Blin
#============================================================================
# procedure to shift file(s).f to file(s).F into current directory
#============================================================================
#set echo on
   set files = `ls *.f`
   foreach file ( $files[*] )
     set tmp = `echo ${file} | awk '{print substr($0,1,length($0)-1)}'`
#     echo " ${file} -> ${tmp}F"
     mv -f ${file} ${tmp}F
   end
exit
