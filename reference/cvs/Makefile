#######  specific to one program

NAME     =
include $(ALROOT)/gmake.common

##### job variables
DBX   =

ifeq ($(DBX),-g)
  fdbx = _dbx
else
  fdbx = 
endif

###########################################################

#### common to all programs ########

MAKE = gmake
SHELL =/bin/csh

##### program directories
pobj    = $(PSRC)/$(OS)
pmod    = $(PLIB)

##### FC flags
FFLAGS  = $(FCOPT) $(DBX) -I$(PSRC)/inc

##### AR flags
ARFLAGS  = ruz
ifndef RANLIB
   RANLIB = $(shell which ranlib)
   ifneq ($(findstring ' ',$(RANLIB)),)
      RANLIB = true
   endif
endif

##### file names
name     := $(DIR)$(VERSION)$(fdbx)
prog     := $(pmod)/$(name)
libfile  := $(PLIB)/lib$(name).a

#### source files to be compiled
nodir    := $(addprefix $(PSRC)/,OSF1 IRIX5 HPUX9 CVS Makefile news inc spy)

srcdir   := $(filter-out $(nodir),$(wildcard $(PSRC)/*)) 
srcfiles := $(foreach src,$(srcdir),$(wildcard $(src)/*.F))

#### object files
objfiles := $(filter-out $(v_depend), $(notdir $(srcfiles:.F=.o)))
objv_dep := $(v_depend:.F=.o)

#### path
vpath %.F $(srcdir)
vpath %.h $(PSRC)/inc
vpath %.o $(pobj)

#### libraries
CERNLIB = `cernlib $(clib) mathlib packlib shift`
LIBS    = \
                $(libfile) \
                /aleph/lib/libalephlib$(DBX).a \
                /aleph/lib/libbos77$(DBX).a \
                $(CERNLIB)


##############################################################

.PHONY: lib
lib: $(libfile)

$(libfile)  : $(libfile)($(objfiles)) $(libfile)($(objv_dep))
	ar $(ARFLAGS) $@ $^
	$(RANLIB) $@

$(objv_dep) :  $(v_depend) ${version_h}
	$(FC) $(FFLAGS) $< -o $@

%.o : %.F
	$(FC) $(FFLAGS) $^ -o $@

##########

.PHONY: clean
clean:
	rm -f $(pobj)/*.o

##########

.PHONY: module
module: $(pmod)/$(name)

$(pmod)/$(name) :
	$(FC) -o $@  $(main) $(UNDEF) $(LIBS) 



##########

.PHONY: print
print:
	@echo "PSRC" $(PSRC)
	@echo "pobj" $(pobj)
	@echo "nodir" $(nodir)
	@echo "srcdir" $(srcdir)
	@echo "srcfiles" $(srcfiles)
	@echo "v-depend" $(v_depend)
	@echo "objfiles" $(objfiles)
	@echo "ARFLAGS" $(ARFLAGS)
	@echo "RANLIB" $(RANLIB)  
	@echo "PLIB" $(PLIB)    
	@echo "libfile" $(libfile)	
#########











