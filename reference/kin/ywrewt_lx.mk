NAME    = $(ALEPH)/kin/ywrewt.exe 
CFILES  =  
CFLAGS  = 
FFILES  = $(ALEPH_ROOT)/reference/kin/ywrewt.f $(ALEPH_ROOT)/reference/kin/ywrewt_origine.f
FFLAGS = $(FCOPT) -I$(ALEPH_ROOT)/reference/kin/ywrewt_inc
LDFLAGS = 
CERNLIB = `cernlib mathlib packlib`

LIBS    = \
	$(CERNLIB)


# Rules...

SRCFILES = $(CFILES) $(FFILES)
OBJFILES = $(CFILES:.c=.o) $(FFILES:.f=.o)

$(NAME): $(OBJFILES) 
	$(FC) -o $@  $(OBJFILES) $(LIBS) $(LDFLAGS)
