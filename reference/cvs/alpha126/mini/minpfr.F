      SUBROUTINE MINPFR
CKEY TERM/INTERNAL
C ----------------------------------------------------------------------
C Recreate the PRFR bank for GAMPEX
C J. Boucrot 9-Jan-2001
C Called from QFCHGD
C Calls the JULIA routine FITWRI
C ----------------------------------------------------------------------
#ifndef DOC
      PARAMETER (MINNV = 205 )
#include "qcde.h"
C ----------------------------------------------------------------------
      IF (.NOT.XMINI) GO TO 999
      IMIV=MINGTV(DUMM)
      IF (IMIV.LT.MINNV) GO TO 999
C Build PFRF from FRFT and add it to the S list:
      CALL FITWRI
      CALL BLIST(IW,'S+','PFRF')
C
 999  RETURN
      END
#endif
