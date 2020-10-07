#--- Program name
NAME    = $(HOME)/$(OS)/cardaf
#--- Library versions (Default)
VALE    = lib
VBOS    = 77
#--- Libraries
LIBGEN  = /aleph/gen/libaleph$(VALE).a /aleph/gen/libbos$(VBOS).a
#
#--- C, Fortran Files, Options
CFILES   =
CFLAGS   = -g
FC       = f77
FFILES   = 
FFLAGS   = -static -w -O
LDFLAGS  = 
CERNLIBS = `cernlib mathlib packlib shift`
#
#--- set up libraries
#
LIBS    = \
                $(HOME)/$(OS)/cardaf.o \
                $(LIBGEN) \
		$(CERNLIBS)
#
#--- Rules...
#
SRCFILES = $(CFILES) $(FFILES)
OBJFILES = $(CFILES:.c=.o) $(FFILES:.f=.o) 

$(NAME): $(OBJFILES) 
	$(FC) -o $@  $(OBJFILES) $(LIBS) $(LDFLAGS)
