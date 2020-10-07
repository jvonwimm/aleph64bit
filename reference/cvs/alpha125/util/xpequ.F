      FUNCTION XPEQU(ITK,CPAR)
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C! Test if particle ITK is of type CPAR
C     J. Boucrot  10-Nov-1993           adapted from I.Tomalin   1/2/93
C----------------------------------------------------------------------
#ifndef DOC
#include "parna.h"
      LOGICAL XPEQU
      CHARACTER*(*) CPAR
#include "qcdesh.h"
      KTPCOD(KI)=IW(KOQVEC+KI*KCQVEC+JQVEPA)
C----------------------------------------------------------------------
      ICODE = KTPCOD(ITK)
      XPEQU = FLISPA(ICODE).EQ.CPAR
      END
#endif
