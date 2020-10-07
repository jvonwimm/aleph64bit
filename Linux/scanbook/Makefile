NAME    = scanbookdev 
FFILES  = srcbook/FORTRAN/scanbookdev.f
FFLAGS = $(FCOPT)
LDFLAGS =
CERNLIB = `cernlib mathlib packlib`

LIBS    = \
		$(ALEPH)/jul/libjulia.a \
		$(ALEPH)/gen/libalephlib.a \
		$(ALEPH)/gen/libbos77.a \
		$(CERNLIB)

OBJFILES = $(FFILES:.f=.o)

$(NAME): $(OBJFILES) 
	$(FC) -o $@  $(OBJFILES) -Wl,-u,aboldr_,-u,babend_,-u,dafrds_ $(LIBS) $(LDFLAGS)
