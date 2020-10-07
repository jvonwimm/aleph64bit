      SUBROUTINE SIENCD(IAD,IOR,IMD,IST,IPH,IRD)
C.---------------------------------------------------------------------
CKEY SCALDES ENCODE ADDRESS / USER
C     B.BLOCH       October 91
C! Address encoding routine  SICAL
C   Input :
C          IMD    Module number1-2
C          IST    Stack  number 1-12
C          IPH    PHibin number 1-32
C          IRD    Radialbin number 1-16
C   Output:
C          IAD   ADDRESS encoded (16 bits)
C          IOR   Bin number in triplet (0-2)
C          IAD = -1 means error
C   Called by USER program
C.---------------------------------------------------------------------
#ifndef DOC
#include "simask.h"
C
      IAD = (IMD-1)*SIBIMD
      IAD = IAD + (IPH-1)*SIBIPH
      IAD = IAD + (IRD-1)*SIBIRD
      ITP = (IST-1)/3
      IOR = MOD(IST-1,3)
      IAD = IAD + ITP*SIBITP
      RETURN
      END
#endif
