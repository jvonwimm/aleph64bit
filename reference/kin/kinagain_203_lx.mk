NAME    = $(ALEPH)/kin/kinagain203x
CFILES  =  
CFLAGS  = 
FFILES  = $(ALEPH_ROOT)/reference/kin/kinagain203.F $(ALEPH_ROOT)/reference/kin/arpyww.F
FFLAGS  = $(FCOPT)
LDFLAGS = 
CERNLIB = `cernlib packlib mathlib kernlib`

LIBS    = \
		$(ALEPH)/kin/kinmar.o \
		$(ALEPH)/kin/ludata74.o \
                $(ALEPH)/kin/ardata04.o \
		$(ALEPH)/kin/hwudat12.o \
                $(ALEPH)/kin/libhrwg12.a \
                $(ALEPH)/kin/libdymu02.a \
                $(ALEPH)/kin/libaria04.a \
                $(ALEPH)/kin/libjetset74.a \
                $(ALEPH)/kin/libkinextra.a \
		$(ALEPH)/gen/libalephlib.a \
		$(ALEPH)/gen/libbos77.a \
		$(CERNLIB)
# Rules...

SRCFILES = $(CFILES) $(FFILES)
OBJFILES = $(CFILES:.c=.o) $(FFILES:.F=.o)

$(NAME): $(OBJFILES)
	$(FC) -o $@  $(OBJFILES) $(LIBS) $(LDFLAGS)



