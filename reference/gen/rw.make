include $(ALEPH)/gen/alephlib.version
include $(ALEPH)/gen/bos77.version

VER     = 
FILE    =
DBX     =
NAME    = $(FILE)$(VER)$(DBX)

AVER    = $(alephver)
BVER    = $(bosver)

FFLAGS   = $(FCOPT)
ifeq ($(FILE),look)
  FPPFILES = lklook.F
  ALIB     = $(ALEPH)/gen/liblook$(VER)$(DBX).a
  FFLAGS  += -I$(ALROOT)/alook/inc
else
  FPPFILES = $(FILE).F
  ALIB     =
  FFLAGS  += -I$(ALROOT)/alephio/inc -I$(ALROOT)/aleph$(AVER)/inc
endif

ifeq ($(DBX),_dbx)
  FFLAGS  += -g -DARDEB

endif

LDFLAGS = $(UNDEF)

OBJDIR  = $(HOME)/$(OS)
LIBDIR  = $(HOME)/$(OS)
 
#$(ALEPH)/gen

vpath %.F $(ALROOT)/alephio/ard: $(ALROOT)/alook/look: 
vpath %.o $(OBJDIR)

ALIB   += $(ALEPH)/gen/libalephio$(AVER)$(DBX).a $(ALEPH)/gen/libbos$(BVER)$(DBX).a
CERNLIB = `cernlib mathlib packlib shift`

# Rules...

F_FILES := $(FFILES) $(FPPFILES)
O_FILES := $(FFILES:%.f=%.o) $(FPPFILES:%.F=%.o) 
OBJFILES := $(addprefix $(OBJDIR)/,$(notdir $(O_FILES)))

$(OBJDIR)/%.o : %.f
	$(FC) $(FFLAGS) $< -o $@

$(OBJDIR)/%.o : %.F
	$(FC) $(FFLAGS)  $< -o $@

$(LIBDIR)/$(NAME): $(OBJFILES) 
	$(FC) -o $@  $(OBJFILES) $(ALIB) $(CERNLIB) $(LDFLAGS)





