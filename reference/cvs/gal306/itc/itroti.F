*DK itroti
      SUBROUTINE ITROTI
C.
C...ITROTI  1.10  870922  14:47                     R.Beuselinck
C.
C!  Initialise ITC alignment constants.
C.
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "itrotc.h"
#include "bcs.h"
#include "ijjpar.h"
C
      EXTERNAL NAMIND
C
C--  Fill the alignment data.
C--
      IND = IW(NAMIND('IALI')) + LMHLEN
      DO 20 I=1,3
        DXYZIT(I) = RW(IND+JIALDX+I-1)
        EULRIT(I) = RW(IND+JIALDR+I-1)
        ITSHFT = ITSHFT.OR.(DXYZIT(I).NE.0.).OR.(EULRIT(I).NE.0.)
   20 CONTINUE
C
      C1 = COS(EULRIT(1))
      C2 = COS(EULRIT(2))
      C3 = COS(EULRIT(3))
      S1 = SIN(EULRIT(1))
      S2 = SIN(EULRIT(2))
      S3 = SIN(EULRIT(3))
C
      ROTITC(1,1) =  C3*C2*C1 - S3*S1
      ROTITC(2,1) =  C3*C2*S1 + S3*C1
      ROTITC(3,1) = -C3*S2
      ROTITC(1,2) = -S3*C2*C1 - C3*S1
      ROTITC(2,2) = -S3*C2*S1 + C3*C1
      ROTITC(3,2) =  S3*S2
      ROTITC(1,3) =  S2*C1
      ROTITC(2,3) =  S2*S1
      ROTITC(3,3) =  C2
      END
#endif
