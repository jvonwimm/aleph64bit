      SUBROUTINE LBHCOR(FACT)
C-----------------------------------------------------------------
C!     Interpolates electroweak correction to Bhabha cross section and
C     performs energy scaling according to formula from J.D.Hansen
C     This expression is used by line shape fitters at present.
C     Modified : J.Wear   900315
C     Modified : P.Hansen 951102
C     Modified : P.Hansen 960501 New MC reference energies
C     Modified : P.Hansen 970616 Use BHZ0 bank for Z0 runs from 1996
C     Modified : D.Smith  991027 Double precision fixes for Linux
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rcurnt.h"
#include "rconds.h"
#include "rparac.h"
#include "rflags.h"
#include "rstatr.h"
#include "lcnamc.h"
#include "bhabjj.h"
      REAL*8 ECMS,ZMASS,GZ,MREF,GREF,
     &  EREF, FTERM, STERM
      REAL*8 B,C
      PARAMETER(B=-0.018D0,C=0.0028D0)
      PARAMETER( ELEP1 = 91.2,MREF = 91.2D0, GREF=2.4D0, ELEP2 = 160.)
      PARAMETER( ZMASS = 91.188D0, GZ= 2.496D0)
#include "bmacro.h"
C
C----------------------------------------------------------------------
C
      FACT = 1.
C Find beam energy
      ECMS = ENERRC
C Find reference energy at which the MC was produced
C
      KBHAB=IW(NABHAB)
C For LEPII calibration runs use BHZ0:
      IF( IRUNRC.GE.40000 .AND. ENERRC.LT.96. ) THEN
        KBHAB = IW(NABHZ0)
      ENDIF
C
      IF(KBHAB.GT.0.AND.LCOLS(KBHAB).GE.25) THEN
        IF(LROWS(KBHAB).GE.9) THEN
          EREF = RTABL(KBHAB,9,JBHAEG)
        ELSE
          EREF = RTABL(KBHAB,6,JBHAEG)
        ENDIF
      ELSE
        IF(ECMS.LT.96.D0) THEN
          EREF = DBLE(ELEP1)
        ELSE
          EREF = DBLE(ELEP2)
        ENDIF
      ENDIF
C
      FTERM = 1+ (B*(ECMS-ZMASS)+C)/
     &  (  (ECMS-ZMASS)**2 + GZ*GZ/4D0 )
C
      STERM  = 1+ (B*(EREF-MREF)+C)/
     &  (  (EREF-MREF)**2 + GREF*GREF/4D0 )
C
      FACT = (EREF/ECMS)**2 *FTERM/STERM
C
      IF(JPRERF(5).GT.1) THEN
        WRITE(IW(6),'(/,1X,A,F8.2,A,F8.2,A,F8.4)')
     &   'Lep energy:',ECMS,'  Ref energy:',EREF,
     &   '  Corr factor:',FACT
      ENDIF
C
      RETURN
      END
#endif
