NAME    = $(ALEPH)/kin/kinagain204x
CFILES  =  
CFLAGS  = 
FFILES  = $(ALEPH_ROOT)/reference/kin/kinagain204.F
FFLAGS  = $(FCOPT)
LDFLAGS = 
CERNLIB = `cernlib packlib mathlib kernlib`

LIBS    = \
		$(ALEPH)/kin/kinmar.o \
		$(ALEPH)/kin/ludata74.o \
                $(ALEPH)/kin/ardata64.o \
		$(ALEPH)/kin/hwudat12.o \
                $(ALEPH)/kin/pydata61.o \
                $(ALEPH)/kin/libhrwg12.a \
                $(ALEPH)/kin/libdymu02.a \
                $(ALEPH)/kin/libaria64.a \
                $(ALEPH)/kin/libjetset74.a \
                $(ALEPH)/kin/libkinextra.a \
		$(ALEPH)/gen/libalephlib.a \
                $(ALEPH)/kin/libpythia61.a \
		$(ALEPH)/gen/libbos77.a \
		$(CERNLIB)
# Rules...

SRCFILES = $(CFILES) $(FFILES)
OBJFILES = $(CFILES:.c=.o) $(FFILES:.F=.o)

$(NAME): $(OBJFILES)
	$(FC) -o $@  $(OBJFILES) $(LIBS) $(LDFLAGS)



