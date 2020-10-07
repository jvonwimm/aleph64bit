NAME    = kinagain
CFILES  =  
CFLAGS  = 
FFILES  = kinagain103.F
FFLAGS  = -O -w -c -static -DUNIX -DALEPH_DEC -I/afs/cern.ch/aleph/reference/cvs/inc 
#    $(FCOPT)
LDFLAGS = 
CERNLIB = `cernlib packlib mathlib kernlib`

LIBS    = \
		/afs/cern.ch/aleph/aleph/kin/kinmar.o \
		/afs/cern.ch/aleph/aleph/kin/ludata74.o \
                /afs/cern.ch/aleph/aleph/kin/ardata04.o \
		/afs/cern.ch/aleph/aleph/kin/hwudat10.o \
                /afs/cern.ch/aleph/aleph/kin/libhrwg10.a \
                /afs/cern.ch/aleph/aleph/kin/libdymu02.a \
                /afs/cern.ch/aleph/aleph/kin/libaria04.a \
                /afs/cern.ch/aleph/aleph/kin/libjetset74.a \
                /afs/cern.ch/aleph/aleph/kin/libkinextra.a \
		$(ALEPH)/gen/libalephlib.a \
		$(ALEPH)/gen/libbos77.a \
		$(CERNLIB)
# Rules...

SRCFILES = $(CFILES) $(FFILES)
OBJFILES = $(CFILES:.c=.o) $(FFILES:.F=.o)

$(NAME): $(OBJFILES)
	$(FC) -o $@  $(OBJFILES) $(LIBS) $(LDFLAGS)



