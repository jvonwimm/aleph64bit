      SUBROUTINE EDEFMD
C.----------------------------------------------------------------------
CKEY ECALDES MODULE INITIALISATION / INTERNAL
C     H.Videau      Creation 15/12/85   Modification 26/01/88
C! Initialises module system
C  In this routine the modules are defined as angular sectors of pi/6
C  in their theoretical position. We consider that an actual module can
C  never gets out of its sector by an alignment mistake. The 12 sector
C  make a partition of the complete space.
C  The fine definition is given by the column, row, plane, stack
C  description or by the sensitive zone of the module: ETZSMD
C   Calls: EROGEN,EROTPL.
C   Called by EXPNEC
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
#include "alcons.h"
C
C    Points de definition des plans de separation des modules
      REAL PTPL(ECALLS)
C
      REAL VDOT
      EXTERNAL VDOT
C Variables locales
      INTEGER I,PN,BPMD(ECALLP)
      REAL PHI,CS,SN
C Plans temporaires. Variables locales.
      REAL PTEC(ECALLS)
C
C
C Generation des rotations theoriques des modules
      CALL EROGEN(EMODDM)
C
C     Mise en place des plans de reference
      BPMD(1)=ECALM1
      BPMD(2)=ECALM2
      DO 2017 PN=1,ECALLP
      CALL EROTPL(PTEC,EBPLLF(1,BPMD(PN)),ECALDM)
      DO 2007 I=1,ECALSC
            CALL EROTPL(ESCOMD(1,PN,I),PTEC,ETSCDM(1,1,ESCOET(I)))
 2007 CONTINUE
 2017 CONTINUE
C   Definition des coordonnees locales . pencil coordinates
      DO 2008 I=1,ECALMD
            EMODID(I)=I
C    Generation du point definissant le plan
C    Ne devrait on le faire en appliquant les rotations de modules?
            PHI=FLOAT(I-1)*ECALAP
            CS=COS(PHI)
            SN=SIN(PHI)
            PTPL(1)=CS
            PTPL(2)=SN
            PTPL(3)=0.
            PTPL(4)=1.
            DO 2018 PN=1,ECALLP
            EMODPC(PN,I)=VDOT(PTPL(1),EBPLLF(1,BPMD(PN)),ECALLS)
 2018       CONTINUE
 2008 CONTINUE
      END
#endif
