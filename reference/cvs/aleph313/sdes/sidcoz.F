      SUBROUTINE SIDCOZ(IBD,IMD,IST,IPH,IRD)
C.---------------------------------------------------------------------
CKEY SCALDES DECODE ADDRESS / USER
C     B.BLOCH       October 91
C! Address Decoding routine  SICAL
C   Input :
C          IBD   ADDRESS encoded (14 bits)
C   Output:
C          IMD    Module number1-2
C          IST    Stack  number 1-12
C          IPH    PHibin number 1-32
C          IRD    Radialbin number 1-16
C      any Ixx = -1 means error
C   Called by USER program
C.---------------------------------------------------------------------
#ifndef DOC
C  Maximum address from JULIA has 14 bits
      PARAMETER ( IADMX = 16383)
#include "simask.h"
#include "simacr.h"
C
      IAD = IBD+4095
      IF ( IAD.LT.0 .OR. IAD.GT.IADMX) GO TO 998
      IMD = SIMD(IAD)
      JOR = SIOR(IAD)
      IST = SIST(IAD,JOR)
      IPH = SIPH(IAD)
      IRD = SIRD(IAD)
      RETURN
 998  CONTINUE
      IMD = -1
      IST = -1
      IPH = -1
      IRD = -1
      RETURN
      END
#endif
