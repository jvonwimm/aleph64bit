#
#  This is a simple script called by cvs commit which runs the perl script
#  given below. It is required because cvs commit for some reason doesn't run
#  perl.  RD Schaffer 21.2.95
#
perl $CVSROOT/CVSROOT/cvs_acls.pl $*
