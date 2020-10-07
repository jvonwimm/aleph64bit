# makefile to build a new galeph version
# galeph module is built in 'pwd'
# GALVER is the current galeph version number
# VERSION is the galeph version to be linked with
# SYS is given as SYS=new when pwd=$(ALEPH)/gal (official directory)
# SYS is used to distinguished OSF1, IRIX5, HPUX9 user area
# LDFLAFS=-Wl,-m to get a map. Don't use it on csf: galeph cannot link
# FC=fort77 on HPUX9
# FCOPT is an environment variable setting fortran options
# GUSER is an environment variable which makes undefined all GU.. subroutines
# UNDEF is an environment variable which makes undefined aboldr, dafrds, babend

include $(ALEPH)/gal/galeph.version
include $(ALEPH)/gal/tpcsim.version
include $(ALEPH)/gen/alephlib.version
include $(ALEPH)/gen/bos77.version 
VERSION = $(galver)
SYS     = _$(OS)
DBX     =
GEANT   = 321
#NAME    = gal$(VERSION)$(SYS)
NAME    = mygal
CFILES  =
FFILES  = asieve.F alevnum.F
ifeq ($(OS),Linux)
  GOBJ =  $(ALEPH)/gal/gmults.o
else
  GOBJ =
endif   
FFLAGS  = $(FCOPT) 
LDFLAGS = 
MAIN    = $(ALEPH)/gal/gmain$(VERSION).o
CERNLIB = `cernlib geant$(GEANT)/new graflib/X11 packlib mathlib kernlib shift`

LIBS    = \
		$(ALEPH)/gal/libgal$(VERSION).a \
		$(ALEPH)/gal/libtpc$(tpcver).a \
		$(ALEPH)/gen/libalephio$(alephver).a \
		$(ALEPH)/gen/libbos$(bosver).a \
                $(CERNLIB)


# Rules...

SRCFILES = $(CFILES) $(FFILES)
OBJFILES = $(CFILES:.c=.o) $(FFILES:.f=.o) 

$(NAME) : $(OBJFILES)
	 $(FC) -o $@ $(MAIN) $(OBJFILES) $(GOBJ) $(GUSER) $(UNDEF) $(LIBS) $(LDFLAGS)







