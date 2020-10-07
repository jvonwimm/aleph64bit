      SUBROUTINE ABWEND
C ----------------------------------------------------------------
C - F.Ranjard - 900927          from H.Albrecht
C! close output file
CKEY ALPHARD CLOSE OUTPUT FILE
C ----------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "abrcom.h"
C ---------------------------------------------------------------
#ifndef DOC
      IF (WLIST .NE. ' ')  CALL ABWEVE
      IF (CLOSE1)  CALL BCLAST (IUTSEL)
      CLOSE1 = .FALSE.
      IF (CLOSE2)  CALL BWRITE (IW, IUTDAT(1), '0')
      CLOSE2 = .FALSE.
      INIT = .TRUE.
C
      END
#endif
#endif
