      SUBROUTINE ECLOPE(I,CX,CY,CZ)
C----------------------------------------------------------------------
C!  - COMPUTES THE THE ANGLE BETWEEN CHARGED TRACK AND INNER FACE
C!  OF THE ECAL MODULE
C!
C!  AUTHOR : A. FALVARD
C!  MODIFIED:
C?
C!======================================================================
#ifndef DOC
#include "ezluge.h"
#include "etopos.h"
#include "e4com1.h"
#include "esubco.h"
      IMOD=IMODUL(1)
      ISUB=ISUBCO(1)
      CX=0.
      CY=0.
      CZ=1.
      IF(ISUB.EQ.3)CZ=-1.
          IF(BARREL)THEN
          CX=UVWMEZ(1,IMOD)
          CY=UVWMEZ(2,IMOD)
          CZ=UVWMEZ(3,IMOD)
          ENDIF
      RETURN
      END
#endif
