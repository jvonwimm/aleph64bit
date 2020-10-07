NAME    = /afs/cern.ch/aleph/aleph/kin/ywrewt.exe 
CFILES  =  
CFLAGS  = 
FFILES  = /afs/cern.ch/aleph/reference/kin/ywrewt.f 

FFLAGS =  -O -w -c -static  -I/afs/cern.ch/aleph/reference/kin/ywrewt_inc
LDFLAGS = 
CERNLIB = `cernlib mathlib packlib shift`

LIBS    = \
                 /afs/cern.ch/aleph/aleph/kin/libywrewt.a \
    		$(CERNLIB)


# Rules...

SRCFILES = $(CFILES) $(FFILES)
OBJFILES = $(CFILES:.c=.o) $(FFILES:.f=.o)

$(NAME): $(OBJFILES) 
	f77 -o $@  $(OBJFILES) $(LIBS) $(LDFLAGS)
