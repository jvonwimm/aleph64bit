      INTEGER FUNCTION SIPTOJ(PHI,IMD,IST)
C.---------------------------------------------------------------------
CKEY SCALDES ENCODE ADDRESS / USER
C     B.BLOCH       February 92
C! Find PHI bin number frm PHI position in module
C   Input :
C          PHI  PHI (radian)
C          IMD  Module number ( 1 or 2)
C          IST  Stack number  ( 1 to 12)
C   Output:
C          SIPTOJ integer Phi bin ( 1 to 32)
C                 0 means outside detector or error
C   Called by USER program
C.---------------------------------------------------------------------
#ifndef DOC
#include "alcons.h"
#include "sigeco.h"
      SIPTOJ = 0
C GET PHI bin
      ITP = MOD(IST-1,3)+1
      PHIT= PHI-PHSHFT(ITP,IMD)
      PHIT = MOD(PHIT,TWOPI)
      IF (PHIT.LT.0.) PHIT= PHIT+TWOPI
      IF (PHIT.GT.TWOPI) PHIT= PHIT-TWOPI
      IPH = INT(PHIT/PHISTP)+1
      IF ( IPH.LE.0 .OR. IPH.GT.NPBNSI) GO TO 999
      SIPTOJ = IPH
 999  RETURN
      END
#endif
