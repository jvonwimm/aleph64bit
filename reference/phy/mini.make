# makefile to build a new mini version
# galeph module is built in 'pwd'
# miniver is the current mini version number
# VERSION is the mini version to be linked with
# SYS is given as SYS=new when pwd=$(ALEPH)/phy (official directory)
# SYS is used to distinguished OSF1, HPUX10 user area
# FCOPT is an environment variable setting fortran options
# UNDEF is an environment variable which makes undefined aboldr, dafrds, babend

include $(ALEPH)/phy/alpha.version
include $(ALEPH)/phy/mini.version
include $(ALEPH)/jul/julia.version
include $(ALEPH)/gen/alephlib.version
include $(ALEPH)/gen/bos77.version
 
VERSION = $(miniver)
SYS     = _$(OS)
DBX     =
NAME    = mini$(VERSION)$(SYS)

CFILES  =
FFILES  =
FFLAGS  = $(FCOPT) 
LDFLAGS = 

MAIN    = $(ALEPH)/phy/qmain.o $(ALEPH)/phy/qusig.o
MQALPHA = mqalpha.o$(SYS)

CERNLIB = `cernlib packlib mathlib kernlib shift`

LIBS    = \
		$(ALEPH)/phy/libalpha$(alphaver).a \
		$(ALEPH)/phy/libmini$(VERSION).a \
		$(ALEPH)/jul/libjul$(julver).a \
		$(ALEPH)/gen/libalephio$(alephver).a \
		$(ALEPH)/gen/libbos$(bosver).a \
                $(CERNLIB)


# Rules...

SRCFILES = $(CFILES) $(FFILES)
OBJFILES = $(CFILES:.c=.o) $(FFILES:.f=.o) 

$(NAME) : $(OBJFILES) $(MQALPHA)
	 $(FC) -o $@ $(MAIN) $(MQALPHA) $(OBJFILES) $(JMUID) $(UNDEF) $(LIBS)

$(MQALPHA) :
	ar -x $(ALEPH)/phy/libmini$(VERSION).a mqalpha.o;
	mv mqalpha.o mqalpha.o$(SYS);

