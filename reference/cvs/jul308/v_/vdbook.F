      SUBROUTINE  VDBOOK
C----------------------------------------------------------------------
C!  - book VDET histograms
C!
C!
C!    by Hans-Guenther Moser
C!
C!    created:           10/04/88
C!
C!    last modification: 15-4-91
C!    Rewrite for 1991 VDET, Dave Brown
C!
C!  Modified March 1995 A. Bonissent, M. Thulasidas
C!                 reorganise and debug
C!======================================================================
#ifndef DOC
C
C  Here, I have to declare all the variables in RPARAC if I want to use
C  implicit none.  Also a few in RFLAGS.  What a bunch of turkeys.
C
      INTEGER JULVD,JULIT,JULTP,JULEC,JULLC
      INTEGER JULSA,JULHC,JULMU,JULCA,JULSK
      INTEGER JULYR,JULYT,JULEF,JULBC,JULFA,JULBO
      INTEGER MODEND,MODREA,MODPRV,MODPRI,MODPRT
      INTEGER MODPRE,MODPRL,MODPRS,MODPRH,MODPRM
      INTEGER MODTPT,MODTPW,MODDEX,MODITC,MODVDE
      INTEGER MODFIT,MODFTR,MODECL,MODMIP,MODEID
      INTEGER MODFEO,MODFEP,MODHCL,MODHDP,MODEHG
      INTEGER MODMUF,MODMUA,MODLCA,MODSAT,MODLTR
      INTEGER MODPID,MODYMA,MODYTO,MODYV0,MODHIS
      INTEGER MODPOT,MODMIN,MODWRI
      INTEGER MODCOM,MODBCA,MODMUO,MODMON,MODENF
      INTEGER MODGAM,MODEDI
      INTEGER LASTMO
      INTEGER JCMORF,JEBIRF,JFBIRF
      INTEGER NWAF
C
      INTEGER VNTWAF
C
#include "rparac.h"
#include "rflags.h"
#include "vprtnc.h"
C
C  Local variables
C
      INTEGER ILAY,IWAF,IPHI,IVIEW,IBNUM
      REAL MINPH,MAXPH,MINSM,MAXSM,MAXCM
      CHARACTER*60 TITLE
C -----------------------------------------------------------
C  Setup the wafer index common
C
C  Get the histogram level and offset from JULIA.  Also set the
C  print level here.
C
      HISTLV = JHISRF(JULVD)
      PRNTLV = JPRSRF(JULVD)
      HISOFF  = JULVD * 1000
C
C As a temporary fix, we put the maximum  possible number of
C wafers, since the geometry package is not yet initialised
C A better solution might be found
C A. Bonissent January 1995
C
C      NWAF = VNTWAF()
C (9 faces first layer, 15 second layer)
C  *2 modules/face
C  *3 r/o modules/module
       NWAF = (9+15)*2*3
C
C
C  Interpret the histogram level as a bit flag
C
      IF(IAND(HISTLV,1).EQ.1)THEN
C
C  generic hit-reconstruction histograms
C
        MINPH = -500.
        MAXPH = 1000.
        MINSM = -200.
        MAXSM = 2000.
        MAXCM = 500.
C
        CALL HBOOK1(HISOFF+10,'Raw strip PH, no flag',
     &  150,MINPH,MAXPH,0.)
        CALL HBOOK1(HISOFF+11,'Raw strip PH, noisy flag',
     &  150,MINPH,MAXPH,0.)
        CALL HBOOK1(HISOFF+12,'Raw strip PH, overflow flag',
     &  150,MINPH,MAXPH,0.)
        CALL HBOOK1(HISOFF+15,'# of strips in raw cluster',
     &  50,-.5,49.5,0.)
        CALL HBOOK1(HISOFF+20,'Final sum PH',100,MINSM,MAXSM,0.)
        CALL HBOOK1(HISOFF+21,'Sum PH after separation',
     &  100,MINSM,MAXSM,0.)
        CALL HBOOK1(HISOFF+31,'# of strips in position, Phi',
     &  10,-.5,9.5,0.)
        CALL HBOOK1(HISOFF+32,'# of strips in position, Z',
     &  10,-.5,9.5,0.)
        CALL HBOOK1(HISOFF+40,'CM value',100,-MAXCM,MAXCM,0.0)
        CALL HBOOK1(HISOFF+41,'CM RMS',100,0.,MAXCM,0.0)
        CALL HBOOK1(HISOFF+42,'# of CM strips',50,-.5,49.5,0.0)
        CALL HBOOK1(HISOFF+43,'# of CM strips after cuts',
     &  50,-.5,49.5,0.0)
        CALL HBOOK2(HISOFF+51,'Online flags by ROM Z',
     &  NWAF,.5,FLOAT(NWAF)+.5,3,-.5,2.5,0.0)
        CALL HBOOK2(HISOFF+52,'Online flags by ROM PHI',
     &  NWAF,.5,FLOAT(NWAF)+.5,3,-.5,2.5,0.0)
      END IF
      IF(IAND(HISTLV,2).EQ.2)THEN
C
C  Hit-track matching histograms
C
      END IF
      IF(IAND(HISTLV,4).EQ.4)THEN
C
C  Some special histograms for the hot channel matching
C
        CALL HBOOK1(HISOFF+211,' Z-Strips Occupancy',100,0.,0.1,0.)
        CALL HBOOK1(HISOFF+212,' Z-Strips Occupancy',100,0.,1.0,0.)
        CALL HBOOK1(HISOFF+213,' Phi-Strips Occupancy',100,0.,0.1,0.)
        CALL HBOOK1(HISOFF+214,' Phi-Strips Occupancy',100,0.,1.0,0.)
        CALL HBPROF(HISOFF+215,'# Z hot channels/readout module',
     &       NWAF,0.5,FLOAT(NWAF)+.5,0.0,1024.,' ')
        CALL HBPROF(HISOFF+216,'# PHI hot channels/readout module',
     &       NWAF,0.5,FLOAT(NWAF)+.5,0.0,1024.,' ')
      END IF
      IF(IAND(HISTLV,8).EQ.8)THEN
C
C  Check charge division linearity
C
        CALL HBOOK1(HISOFF+111,'Eta r-phi LD1 no corr',
     &       100,0.0,1.0,0.0)
        CALL HBOOK1(HISOFF+112,'Eta r-phi LD1 corr',
     &       100,0.0,1.0,0.0)
        CALL HBOOK1(HISOFF+121,'Eta r-phi LD2 no corr',
     &       100,0.0,1.0,0.0)
        CALL HBOOK1(HISOFF+122,'Eta r-phi LD2 corr',
     &       100,0.0,1.0,0.0)
        CALL HBOOK1(HISOFF+130,'Eta Z',
     &       100,0.0,1.0,0.0)
      END IF

      RETURN
      END
#endif
