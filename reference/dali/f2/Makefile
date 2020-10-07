# Makefile for Dali version F2, production or debug version.
# Bjorn S. Nilsson, 16-Jan-1998.
# Version F2 15-Jan-2002.
# This Makefile only works with gmake
# The ALEPH environmental variable OS must be defined. If it is not,
# sometimes "gmake OS=`uname` other-parameters" may work.
#

VER		= f2
SHELL		= /bin/sh
NAME            = dali_$(VER).$(OS)
CC		= cc
###FC		= f77
LD		= $(FC)
IC              = ./ic.$(OS)


#
# Platform specific options. These depend on the ALEPH OS variable.
#

# Digital UNIX, still aka OSF1 and now Tru64 UNIX
ifeq ($(OS),OSF1)
###CCOPTIONS	= -float -c -DCtrl_Signals_Only
  CCOPTIONS	= -float -c -DNo_Signals -std1
  FOPTIONS	= -fpe2 -warn nouncalled -warn uninitialized -c -static -assume backslash
  XLIBS		= -lXt -lX11
  ICFLAGS       = -O
endif

# Linux
ifeq ($(OS),Linux)
###CCOPTIONS    = -float -c -DCtrl_Signals_Only
  CCOPTIONS       = -c -DNo_Signals
#  FC		= gfortran -no-pie
###FOPTIONS     = -warn uninitialized -warn nouncalled -c -static -assume backslash
###FOPTIONS     = -fdollar-ok -c -fbackslash
  FOPTIONS      = -std=legacy -fdollar-ok -c -fno-backslash -fno-automatic -fno-second-underscore
  XLIBS         = -lXt -lX11
  ICFLAGS	= -fdollar-ok -fbackslash -O
endif

###CDEBUGFLAGS	= -g
CDEBUGFLAGS	=
###FDEBUGFLAGS	= -g3 -O
###FDEBUGFLAGS	= -g
FDEBUGFLAGS	=
CFLAGS		= $(CDEBUGFLAGS) $(CCOPTIONS)
FFLAGS		= $(FDEBUGFLAGS) $(FOPTIONS)
LDUNDEF		= $(UNDEF) $(JMUID)
LDFLAGS		= $(LDUNDEF)
###LDFLAGS	= -g $(LDUNDEF)


ENFLWL	= ${ALEPH}/phy/alenflw.o
MINIL	= ${ALEPH}/phy/libmini.a
JUL	= ${ALEPH}/jul/libjulia.a
PHYL	= ${ALEPH}/phy/libalpha.a
LOOKL	= ${ALEPH}/gen/liblook.a
ALEPHL	= ${ALEPH}/gen/libalephlib.a
BOSL	= ${ALEPH}/gen/libbos77.a

LIBS = $(ENFLWL) $(PHYL) $(MINIL) ${JUL} $(LOOKL) $(ALEPHL) $(BOSL)
CERNLIBS = `cernlib mathlib packlib`

################################################################################
# Files
################################################################################

#
# Fortran routines.
#
FSRC1 = \
 algtdb_tficor.f\
 dafilm.f\
 dal_1.f\
 dal_9.f\
 dal_a.f\
 dal_aleph.f\
 dal_alpha_c.f\
 dal_alpha_p.f\
 dal_alpha_v.f\
 dal_bcal.f\
 dal_br.f\
 dal_bt.f\
 dal_c.f\
 dal_d.f\
 dal_e.f\
 dal_f.f\
 dal_geom.f\
 dal_gr.f\
 dal_h.f\
 dal_i.f\
 dal_j.f\
 dal_k.f\
 dal_l.f\
 dal_m.f\
 dal_n.f\
 dal_o.f\
 dal_p.f\
 dal_qg.f\
 dal_qz.f\
 dal_r.f\
 dal_s.f\
 dal_t.f\
 dal_u.f\
 dal_var.f\
 dal_vdet.f\
 dal_vx.f\
 dal_w.f\
 dal_write.f\
 dal_x.f\
 dal_y.f\
 dal_z.f\
 dala_g.f\
 dala_op.f\
 dala_p.f\
 dala_prnt_hlp.f\
 dala_qg.f\
 dala_qh.f\
 dala_qz.f\
 dala_t.f\
 dala_w.f\
 dala_y.f\
 draw_state.f\
 partid.f\
 alstgqry.f

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

