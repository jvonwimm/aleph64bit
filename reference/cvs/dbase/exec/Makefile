# make a module of NAME which is given on the command line

# gmake NAME=eptoda
# gmake NAME=chkcrd
# gmake NAME=cardaf
# gmake NAME=epiofmt
# gmake NAME=sbank
# gmake NAME=hacpar
# gmake NAME=creind

##########################################################

NAME    = 
DBX     =

####

SRCDIR  = $(ALROOT)
OBJDIR  = $(HOME)/$(OS)

CFLAGS   = -c 
FFLAGS   = $(FCOPT) -I$(ALROOT)/dbase/inc

ifeq ($(NAME),sbank)
   CFILES  = sbankc.c
   FFILES  =
else
   FFILES  = $(NAME).F
   CFILES  =
endif 

ifeq ($(DBX),_dbx)
   CFLAGS += -g
   FFLAGS += -g
endif 

####

OFILES   := $(FFILES:.F=.o) $(CFILES:.c=.o)
OBJFILES := $(addprefix $(OBJDIR)/,$(notdir $(OFILES)))

#####

LDFLAGS  = -Wl,-u,dafrds_

CERNLIBS = `cernlib mathlib packlib shift`

LIBS    = \
		$(ALEPH)/dbase/libdbase$(DBX).a \
		$(ALEPH)/gen/libalephlib$(DBX).a \
		$(ALEPH)/gen/libbos77$(DBX).a \
		$(CERNLIBS)
####

ifeq ($(NAME),sbank)
  LIBS += -ltermcap
  ifeq ($(OS),OSF1)
     LDFLAGS += -nofor_main
  endif
endif

####

vpath %.F $(SRCDIR)/dbase/prog
vpath %.c $(SRCDIR)/dbase/c
vpath %.o $(OBJDIR)

$(OBJDIR)/%.o : %.c
	cc    $(CFLAGS)  $< -o $@
$(OBJDIR)/%.o : %.F
	$(FC) $(FFLAGS)  $< -o $@

$(OBJDIR)/$(NAME): $(OBJFILES) 
	$(FC) -o $@  $(OBJFILES) $(LIBS) $(LDFLAGS)




