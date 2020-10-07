      SUBROUTINE SIDCOD(IAD,IOR,IMD,IST,IPH,IRD)
C.---------------------------------------------------------------------
CKEY SCALDES DECODE ADDRESS / USER
C     B.BLOCH       October 91
C! Address Decoding routine  SICAL
C   Input :
C          IAD   ADDRESS encoded (16 bits)
C          IOR   Bin number in triplet (0-2)
C   Output:
C          IMD    Module number1-2
C          IST    Stack  number 1-12
C          IPH    PHibin number 1-32
C          IRD    Radialbin number 1-16
C      any Ixx = -1 means error
C   Called by USER program
C.---------------------------------------------------------------------
#ifndef DOC
C  Maximum address from data has 12 bits and possible 3 overflow bits = 15bits
      PARAMETER ( IADMX = 32767)
#include "simask.h"
#include "simacr.h"
      IF ( IAD.LT.0 .OR. IAD.GT.IADMX) GO TO 998
      IMD = SIMD(IAD)
      IST = SIST(IAD,IOR)
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
