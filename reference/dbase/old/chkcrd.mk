NAME    = ${HOME}/${OS}/chkcrd
# get source from dbslib
#
CFILES  = 
CFLAGS  =
FFILES  = 
FFLAGS  = -c -g -static -w1
LDFLAGS =
CERNLIBS = `cernlib mathlib packlib shift`

LIBS    = \
		${HOME}/dbase/dbslib/chkcrd.o \
		${CERNLIBS}
# Rules...

SRCFILES = $(CFILES) $(FFILES)
OBJFILES = $(CFILES:.c=.o) $(FFILES:.f=.o) 

$(NAME): $(OBJFILES) 
	f77 -o $@  $(OBJFILES) $(LIBS) $(LDFLAGS)

