NAME    = sbank_$(OS)
CFILES  = sbankc.c
CFLAGS  = -DC
FFILES  = sbankf.F
FFLAGS  = $(FCOPT)
# on OSF1 add the following LDFLAGS
// LDFLAGS = -nofor_main
LIBS    = /cern/pro/lib/libkernlib.a -ltermcap


# Rules...

SRCFILES = $(CFILES) $(FFILES)
OBJFILES = $(CFILES:.c=.o) $(FFILES:.F=.o)

$(NAME): $(OBJFILES)
	f77 -o $@   $(OBJFILES) $(LIBS) $(LDFLAGS)
