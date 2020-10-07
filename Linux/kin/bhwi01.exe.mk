NAME    = bhwi01.exe 
CFILES  =  
CFLAGS  = 
FFILES  = ../../reference/kin/bhwi01.f
FFLAGS = -c -O -std=legacy -fno-automatic -fdollar-ok -fno-backslash -DUNIX -DALEPH_LINUX
LDFLAGS = 
CERNLIB = `cernlib mathlib packlib`

LIBS    = \
     		/home/wimmer/aleph64/Linux/kin/kingal.o /home/wimmer/aleph64/Linux/kin/kinmar.o  \
		/home/wimmer/aleph64/Linux/kin/libbhwi01.a \
     		 \
		/home/wimmer/aleph64/Linux/kin/libkinextra.a \
		/home/wimmer/aleph64/Linux/lib/libalephlib.a \
		/home/wimmer/aleph64/Linux/lib/libbos77.a \
		$(CERNLIB)


# Rules...

SRCFILES = $(CFILES) $(FFILES)
OBJFILES = $(CFILES:.c=.o) $(FFILES:.f=.o)

$(NAME): $(OBJFILES) 
	gfortran -no-pie -o $@  $(OBJFILES) $(LIBS) $(LDFLAGS)
