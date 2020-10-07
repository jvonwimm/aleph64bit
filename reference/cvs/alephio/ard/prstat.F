      SUBROUTINE PRSTAT  (SUBR,FNAME,ISTATUS)
C --------------------------------------------------
CKEY ALREAD PRINT OPEN STATUS
C - M.N.Minard - 920327
C! print status message in case of error in OPEN
C - Input     : ISTATUS / I = status word
C               SUBR    / A = subroutine name
C               FNAME   / A = file name
C --------------------------------------------------
#ifndef DOC
      CHARACTER*(*) SUBR, FNAME
#include "bcs.h"
C ----------------------------------------------------------------
      LE = LNBLNK(FNAME)
      LS = LNBLNK(SUBR)
      IF (LE.NE.0) THEN
         WRITE (IW(6),*) ' ***PRSTAT*** ',SUBR(1:LS),' cannot open ',
     &                    FNAME(1:LE),' IST= ',ISTATUS
      ENDIF
#if defined(APOLLO)
      CALL ERROR_$PRINT(ISTATUS)
#endif
      RETURN
      END
#endif
