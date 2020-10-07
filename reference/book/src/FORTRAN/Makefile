include ../UTILITIES/version.h

NAME    = scanbookdev 
FFILES  = jscanbook/fortran/scanbookdev.f
FFLAGS = -c -O -fno-automatic -fdollar-ok -fno-backslash -DUNIX -DALEPH_LINUX
LDFLAGS =
CERNLIB = `cernlib mathlib packlib shift`

LIBS    = \
		/afs/cern.ch/aleph/Linux/lib/libjulia.a \
		/afs/cern.ch/aleph/Linux/lib/libalephlib.a \
		/afs/cern.ch/aleph/Linux/lib/libbos77.a \
		$(CERNLIB)

OBJFILES = $(FFILES:.f=.o)

$(NAME): $(OBJFILES) 
	g77 -w -o  $@  $(OBJFILES) -Wl,-u,aboldr_,-u,babend_,-u,dafrds_ $(LIBS) $(LDFLAGS)


$(FFILES):
# get the Fortran file :
	cvs checkout $(VERSION) jscanbook/fortran

install: $(NAME)
	cp $(NAME)  /afs/cern.ch/user/a/alsoft/prod/prog/updabook
	cp $(FFILES) .

clean:
	rm -rf jscanbook
	rm -f $(NAME)


 
