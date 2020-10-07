NAME    = kinagain204
CFILES  =  
CFLAGS  = 
FFILES  = kinagain204.F
FFLAGS  = -O -w -c -static -DUNIX -DALEPH_DEC -I/afs/cern.ch/aleph/reference/cvs/inc 
#    $(FCOPT)
LDFLAGS = 
CERNLIB = `cernlib packlib mathlib kernlib`

LIBS    = \
		/afs/cern.ch/aleph/aleph/kin/kinmar.o \
		/afs/cern.ch/aleph/aleph/kin/ludata74.o \
                /afs/cern.ch/aleph/aleph/kin/ardata04.o \
		/afs/cern.ch/aleph/aleph/kin/hwudat12.o \
                /afs/cern.ch/aleph/aleph/kin/pydata61.o \
                /afs/cern.ch/aleph/aleph/kin/libhrwg12.a \
                /afs/cern.ch/aleph/aleph/kin/libdymu02.a \
                /afs/cern.ch/aleph/aleph/kin/libaria04.a \
                /afs/cern.ch/aleph/aleph/kin/libjetset74.a \
                /afs/cern.ch/aleph/aleph/kin/libkinextra.a \
		$(ALEPH)/gen/libalephlib.a \
                /afs/cern.ch/aleph/aleph/kin/libpythia61.a \
		$(ALEPH)/gen/libbos77.a \
		$(CERNLIB)
# Rules...

SRCFILES = $(CFILES) $(FFILES)
OBJFILES = $(CFILES:.c=.o) $(FFILES:.F=.o)

$(NAME): $(OBJFILES)
	$(FC) -o $@  $(OBJFILES) $(LIBS) $(LDFLAGS)


