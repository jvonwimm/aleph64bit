include $(ALEPH)/gen/alephlib.version
include $(ALEPH)/gen/bos77.version
include $(ALEPH)/gen/look.version

VER     = $(lookver)
DBX     =
NAME    = look$(VER)$(DBX)

AVER    = $(alephver)
BVER    = $(bosver)

FFILES   =
FPPFILES = lkmain.F 
FFLAGS   = $(FCOPT) -I$(ALROOT)/alook/inc

#MAIN     = $(ALEPH)/gen/lmain$(VER).o
ALIB     = $(ALEPH)/gen/liblook$(VER)$(DBX).a

ifeq ($(DBX),_dbx)
  FFLAGS  += -g
endif

LDFLAGS = $(UNDEF)

OBJDIR  = $(HOME)/$(OS)
LIBDIR  = $(ALEPH)/gen
 
vpath %.F $(ALROOT)/alook/look:
vpath %.o $(OBJDIR)

ALIB   += $(ALEPH)/gen/libalephio$(AVER)$(DBX).a $(ALEPH)/gen/libbos$(BVER)$(DBX).a
CERNLIB = `cernlib mathlib packlib`

# Rules...

F_FILES := $(FFILES) $(FPPFILES)
O_FILES := $(FFILES:%.f=%.o) $(FPPFILES:%.F=%.o) 
OBJFILES := $(addprefix $(OBJDIR)/,$(notdir $(O_FILES)))

$(OBJDIR)/%.o : %.f
	$(FC) $(FFLAGS) $< -o $@

$(OBJDIR)/%.o : %.F
	$(FC) $(FFLAGS)  $< -o $@

$(LIBDIR)/$(NAME): $(OBJFILES) 
	$(FC) -o $@   $(OBJFILES) $(ALIB) $(CERNLIB) $(LDFLAGS)
