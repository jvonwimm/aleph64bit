include galeph.version
include tpcsim.version

VER     = $(galver)
DIR    := $(ALROOT)/gal$(VER)
DBX     =
NAME   := gal$(VER)$(DBX)
CFILES  =
CFLAGS  = -g
FFILES  = 
FPPFILES =
# galeph.F asrust.F asinit.F aswsum.F asktrk.F

ifeq ($(DBX),_dbx)
 FFLAGS = $(FCOPT) -g -I$(DIR)/inc
else 
 FFLAGS = $(FCOPT) -I$(DIR)/inc
endif

LDFLAGS = $(UNDEF)

OBJDIR  = .
#$(HOME)/$(OS)
vpath %.f /u3/xu/flr/galeph
vpath %.F $(DIR)/gal: $(DIR)/skel:
vpath %.o $(OBJDIR)

CERNLIB = `cernlib  geant321/new graflib/X11 mathlib packlib shift`
LIBS    = \
		/aleph/gal/lib$(NAME).a \
		/aleph/gal/libtpcsim$(DBX).a \
		/aleph/gen/libalephlib$(DBX).a \
		/aleph/lib/libbos77$(DBX).a \
		$(CERNLIB)
# Rules...
F_FILES := $(FFILES) $(FPPFILES)
O_FILES := $(FFILES:%.f=%.o) $(FPPFILES:%.F=%.o) 
OBJFILES := $(addprefix $(OBJDIR)/,$(notdir $(O_FILES)))

$(OBJDIR)/%.o : %.f
	$(FC) $(FFLAGS) $< -o $@

$(OBJDIR)/%.o : %.F
	$(FC) $(FFLAGS)  $< -o $@

/aleph/gal/lib$(NAME).a : $(OBJFILES)
		ar rls $@ $(OBJFILES)        

$(OBJDIR)/$(NAME): $(OBJFILES) 
	$(FC) -o $@  $(OBJFILES) $(GUSER) $(LIBS) $(LDFLAGS) > $(NAME).map


