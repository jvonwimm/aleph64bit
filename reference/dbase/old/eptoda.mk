vpath   = /al/reference/dbase
NAME    = eptoda
# get source from cernvm::"[pubxu.210]eptoda.vdsfor"
#
CFILES  = 
CFLAGS  =
FFILES  = eptoda.f
FFLAGS  = -c -g -static -w1
LDFLAGS =
CERNLIBS = `cernlib mathlib packlib shift`

LIBS    = \
		$(ALEPH)/lib/libalephlib.a \
		$(ALEPH)/lib/libbos77.a \
		$(CERNLIBS)
# Rules...

SRCFILES = $(CFILES) $(FFILES)
OBJFILES = $(CFILES:.c=.o) $(FFILES:.f=.o) 

$(NAME): $(OBJFILES) 
	f77 -o $@  $(OBJFILES) $(LIBS) $(LDFLAGS)

