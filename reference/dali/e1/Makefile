# Makefile for Dali version E1, production or debug version.
# Bjorn S. Nilsson, 13-May-1997.
# This Makefile only works with gmake
# The ALEPH environmental variable OS must be defined. If it is not,
# sometimes "gmake OS=`uname` other-parameters" may work.
#

VER		= e1
SHELL		= /bin/sh
NAME            = dali_$(VER).$(OS)
CC		= cc
###FC		= f77
LD		= $(FC)
IC              = ./ic.$(OS)


#
# Platform specific options. These depend on the ALEPH OS variable.
#

# Digital UNIX, still aka OSF1
ifeq ($(OS),OSF1)
###CCOPTIONS	= -float -c -DCtrl_Signals_Only
  CCOPTIONS	= -float -c -DNo_Signals -std1 -migrate
  FOPTIONS	= -fpe2 -warn nouncalled -warn uninitialized -c -static -assume backslash
  XLIBS		= -lXt -lX11
  ICFLAGS       = -O
endif

# SGI, known as IRIX5
ifeq ($(OS),IRIX5)
###CCOPTIONS	= -float -c -DCtrl_Signals_Only
  CCOPTIONS	= -float -common -c -DNo_Signals
  FOPTIONS	= -c -static -backslash -Olimit 1500
  XLIBS		= -lXt -lX11
  ICFLAGS       = -O
endif

# HP, known as HPUX9
ifeq ($(OS),HPUX9)
###CCOPTIONS	= -float -c -DCtrl_Signals_Only
  CCOPTIONS	= -Aa -c -DNo_Signals -I/usr/include/ -I/usr/include/X11R5/
  FOPTIONS	= +e -c -K +U77 +ppu -v
  XLIBS		= -L/usr/lib/X11R5 -lXt -lX11 -L/lib -lm -L/usr/lib -lU77
  ICFLAGS	= +e -K +U77 +ppu -v
endif

# HP 10.20 and up, known as HPUX10
ifeq ($(OS),HPUX10)
###CCOPTIONS    = -float -c -DCtrl_Signals_Only
  CCOPTIONS     = -Aa -c -DNo_Signals
  FOPTIONS      = +e -c -K +U77 +ppu -v
  XLIBS         = -lXt -lX11
  ICFLAGS       = +e -K +U77 +ppu -v
endif

# Linux
ifeq ($(OS),Linux)
###CCOPTIONS    = -float -c -DCtrl_Signals_Only
  CCOPTIONS       = -c -DNo_Signals
###  FOPTIONS      = -fdollar-ok -c -fno-backslash -fno-automatic -fno-second-underscore
  FOPTIONS      = -fdollar-ok -c -fno-backslash -fno-automatic -fno-second-underscore -fno-globals -Wno-globals
  XLIBS         = -L/usr/X11/lib -lXt -lX11
  ICFLAGS	= -fdollar-ok -fbackslash -O
endif

###CDEBUGFLAGS	= -g
CDEBUGFLAGS	=
###FDEBUGFLAGS	= -g3 -O
###FDEBUGFLAGS	= -g
FDEBUGFLAGS	=
CFLAGS		= $(CDEBUGFLAGS) $(CCOPTIONS)
FFLAGS		= $(FDEBUGFLAGS) $(FOPTIONS)
LDUNDEF		= -Wl,-u,babend_,-u,dafrds_,-u,ftrack_,-u,hmfind_,-u,mpredg_,-u,mpredm_,-u,muass_
LDFLAGS		= $(LDUNDEF)
###LDFLAGS	= -g $(LDUNDEF)


ENFLWL	= ${ALEPH}/phy/alenflw.o
MINIL	= ${ALEPH}/phy/libmini.a
JUL	= ${ALEPH}/jul/libjulia.a
PHYL	= ${ALEPH}/phy/libalpha.a
LOOKL	= ${ALEPH}/gen/liblook.a
ALEPHL	= ${ALEPH}/gen/libalephlib.a
BOSL	= ${ALEPH}/gen/libbos77.a

LIBS = $(ENFLWL) $(MINIL) $(JUL) $(PHYL) $(LOOKL) $(ALEPHL) $(BOSL)
CERNLIBS = `cernlib mathlib packlib`

################################################################################
# Files
################################################################################

#
# Fortran routines.
#
FSRC1 = \
 c_dalb9.f c_dalb_aleph.f n_dalba.f n_dalbb.f c_dalbc.f c_dalbd.f\
 c_dalbe.f n_dalbf.f c_dalbg.f c_dalbh.f c_dalbi.f c_dalbj.f c_dalbl.f\
 c_dalbm.f c_dalbo.f c_dalbp.f c_dalbq.f c_dalbr.f n_dalbs.f c_dalbt.f\
 c_dalbv.f c_dalbw1.f c_dalbw2.f c_dalbx.f c_dalby.f\
 c_dali1.f c_dali_vdet.f       alpha_c.f alpha_v.f alpha_p.f\
 c_dalia.f c_dalib.f c_dalic.f c_dalid.f c_dalie.f c_dalif.f c_dalig.f\
 c_dalih.f c_dalii.f c_dalij.f c_dalik.f c_dalil.f c_dalim.f c_dalin.f\
 n_dalio.f c_dalip.f n_daliq.f c_dalir.f c_dalis.f c_dalit.f c_daliu.f\
 c_daliv.f c_daliw.f c_dalix.f c_daliy.f c_daliz.f\
 algtdb_tficor.f dafilm.f draw_state.f partid.f

FSRC2 = \
 dali_$(OS)_x.f

FSRC3 =

FSRC = $(FSRC1) $(FSRC2) $(FSRC3)

#
# C sources.
#
CSRC = dalb_x.c dali_x.c dali_meta.c

SRC  = $(FSRC) $(CSRC)

FOBJ1  = $(FSRC1:.f=.o)
FOBJ2  = $(FSRC2:.f=.o)
FOBJ3  = $(FSRC3:.f=.o)
FOBJ   = $(FOBJ1) $(FOBJ2) $(FOBJ3)
COBJ   = $(CSRC:.c=.o)
OBJ    = $(FOBJ) $(COBJ)

COMMONS = DALI_CF.INC DALI_MU.INC DALI_EX.INC DALI_KI.INC DALI_UIS.INC

H_FILES =  commons.h ps_abbrevs.h dali_defs.h unix_common_defs.h\
 unix_externals_defs.h

################################################################################
# Rules
################################################################################

all:: sayit $(NAME)

dali: sayit $(NAME)

sayit:
	@echo " "
	@echo "   Making dali_$(VER).$(OS) i.e., DALI version $(VER) for $(OS)"
	@echo " "

$(NAME): $(IC) $(COBJ) $(FOBJ1) $(FOBJ2) $(FOBJ3)
	@echo " "
	@echo "   Linking dali_$(VER).$(OS)"
	@echo " "
	$(LD) $(LDFLAGS) -o $(NAME) $(OBJ) $(LIBS) $(CERNLIBS) $(XLIBS)

dalitest: $(COBJ) $(FOBJ1) $(FOBJ2) $(FOBJ3)
	$(LD) -m -v $(LDFLAGS) -o $(NAME) $(OBJ) $(LIBS) $(CERNLIBS) $(XLIBS)

clean:
	rm -f *~ *.o *_ic.f core $(NAME) $(IC)

ic.$(OS): ic.$(OS).f
	$(FC) -o $(IC) $(ICFLAGS) ic.$(OS).f

################################################################################
# Dependencies and object file generation.
################################################################################
$(FOBJ2): $(FSRC2)
	$(FC) $(FFLAGS) -o $*.o $*.f

dalb_x.o: dalb_x.c x_resources.h $(H_FILES)
	$(CC) $(CFLAGS) -o $*.o $*.c

dali_x.o: dali_x.c symbol_font.h $(H_FILES)
	$(CC) $(CFLAGS) -o $*.o $*.c

dali_meta.o: dali_meta.c $(H_FILES)
	$(CC) $(CFLAGS) -o $*.o $*.c

# Everything else must be FOBJ1
%.o: $(COMMONS) %.f
	$(IC) DALI_CF.INC $*.f $*_ic.f
	$(FC) $(FFLAGS) -o $*.o $*_ic.f

