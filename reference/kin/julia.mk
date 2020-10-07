# makefile to build a new julia version
# julia module is built in 'pwd'
# SYS is given as SYS=new when pwd=$(ALEPH)/jul (official directory)
# SYS is used to distinguished OSF1, IRIX5, HPUX9 user area
# LDFLAFS=-Wl,-m to get a map. Don't use it on csf: julia cannot link
# FCOPT is an environment variable setting fortran options
# UNDEF is an environment variable which makes undefined aboldr, dafrds, babend

include $(ALEPH)/jul/julia.version
include $(ALEPH)/gen/alephlib.version
 
SYS     = _$(OS)
VERSION = $(julver)
ALEVER  = $(alephver)
NAME    = myjul
CFILES  =
FFILES  = rloopr.F alevnum.F adbchk.F rinjob.F
FFLAGS  = $(FCOPT) -I/afs/cern.ch/aleph/reference/cvs/jul313/inc
LDFLAGS = 
MAIN    = $(ALEPH)/jul/jmain$(VERSION).o
CERNLIB = `cernlib mathlib/2001 packlib/2001`

LIBS    = \
		$(ALEPH)/jul/libjul$(VERSION).a \
		$(ALEPH)/gen/libalephio$(ALEVER).a \
		$(ALEPH)/gen/libbos77.a \
                $(CERNLIB)


# Rules...

SRCFILES = $(CFILES) $(FFILES)
OBJFILES = $(CFILES:.c=.o) $(FFILES:.f=.o) 

$(NAME) : $(MAIN)
	 $(FC) -o $@ $(MAIN) $(OBJFILES) $(UNDEF) $(LIBS) $(LDFLAGS) > $(NAME).map



