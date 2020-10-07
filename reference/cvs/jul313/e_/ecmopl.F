      SUBROUTINE ECMOPL(IT,CX,CY,CZ)
C---------------------------------------------------------------------
C!  CORRECT THE ECAL THICKNESS BY INCOMING ANGLE(BARREL OR ENDCAP
C!  BUT NOT OVERLAP)
C!
C!  AUTHOR : A. FALVARD
C!  MODIFIED:
C?
C!======================================================================
#ifndef DOC
#include "ecmode.h"
#include "e4com1.h"
#include "etopos.h"
#include "etpinf.h"
      CX1=DROITE(IT,1)
      CY1=DROITE(IT,2)
      CZ1=DROITE(IT,3)
      ASA=CX*CX1+CY*CY1+CZ*CZ1
      DO 1 I=1,45
      IF(BARREL)ECPLAN(I)=ECBAPL(I)/ASA
      IF(ENDCAP)ECPLAN(I)=ECENPL(I)/ASA
 1    CONTINUE
      RETURN
      END
#endif
