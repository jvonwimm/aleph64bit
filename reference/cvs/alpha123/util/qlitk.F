      SUBROUTINE QLITK (ITK)
CKEY LOCK /USER
C----------------------------------------------------------------------
C! set lock flag for track ITK.
C
C called from user routines
C                                                   H.Albrecht 27.11.88
C----------------------------------------------------------------------
#ifndef DOC
      SAVE IERR
#include "qcdesh.h"
C----------------------------------------------------------------------
C
      DATA IERR / 0 /
      IF(IERR.LE.4)THEN
        CALL QWMESS
     &  ('_QLITK_ WARNING! QLITK is obsolete. See ALPHA109 NEWS')
        IERR=IERR+1
      ENDIF
      IW(KOQVEC+ITK*KCQVEC+JQVELK) = 1
      END
#endif
