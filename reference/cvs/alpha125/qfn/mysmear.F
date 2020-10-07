      SUBROUTINE MYSMEAR(QSMPAR,PROB,ISBIN,IVIEW,DMIN,S_DMIN)
CKEY QIPBTAG / INTERNAL
C---------------------------------------------------------------------
C Auxiliary to QSMEAR (QIPBTAG package)
C  Generate a smearing value for the impact parameter significance,
C  based on a delta function plus exponential fit comparing the
C  negative impact parameter spectrum between data and MC.
C  Uses random number generator.
C  Author:   Dave Brown, 24-3-92
C  Modified: Ian Tomalin 27-4-95
C  Modified: P-F Giraud 17-8-99
C---------------------------------------------------------------------
#ifndef DOC
      INTEGER KSMIR,TTYPE,IVIEW
      REAL DMIN,S_DMIN
      DIMENSION PROB(3), QSMPAR(4, *)
#include "qcde.h"
#include "qmacro.h"
C---------------------------------------------------------------------
C Unpack appropriate row of SMIR/SMI2 bank.
      SIG1  = QSMPAR(1, ISBIN)
      AMP1  = QSMPAR(2, ISBIN)
      SIG2  = QSMPAR(3, ISBIN)
      AMP2  = QSMPAR(4, ISBIN)
      IF (SIG1.LT.SIG2) THEN
        SIGT = SIG1
        AMPT = AMP1
        SIG1 = SIG2
        AMP1 = AMP2
        SIG2 = SIGT
        AMP2 = AMPT
      END IF
C Find smearing parallel to impact parameter vector.
      DELX = 0.0
      RANX = 2.0*PROB(2) - 1.0
      IF (PROB(1).LT.AMP1) THEN
        DELX = SIG1*SIGN(LOG(MAX(ABS(RANX),1.0E-30)),RANX)
      ELSE IF (PROB(1).LT.AMP1 + AMP2) THEN
        DELX = SIG2*SIGN(LOG(MAX(ABS(RANX),1.0E-30)),RANX)
      END IF
C For 3D tracks, also find smearing perpendicular to impact parameter.
      DELY = 0.0
      RANY = 2.0*PROB(3) - 1.0
      IF (IVIEW.EQ.3) THEN
        IF (PROB(1).LT.AMP1) THEN
          DELY = SIG1*SIGN(LOG(MAX(ABS(RANY),1.0E-30)),RANY)
        ELSE IF (PROB(1).LT.AMP1 + AMP2) THEN
          DELY = SIG2*SIGN(LOG(MAX(ABS(RANY),1.0E-30)),RANY)
        END IF
      END IF
C Smear impact parameter.
      XNEW = DMIN + S_DMIN*DELX
      YNEW =        S_DMIN*DELY
      DMIN = SIGN(SQRT(XNEW**2 + YNEW**2),XNEW)
  999 CONTINUE
      END
#endif
