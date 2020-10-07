      SUBROUTINE QVDROP (CNAM,ICLASS)
CKEY CLASS TRACKS /USER
C----------------------------------------------------------------------
C! Drop tracks with name CNAM in class ICLASS. CNAM = ' ' : all tracks.
C                                                    H.Albrecht  2.1.89
C----------------------------------------------------------------------
#ifndef DOC
      SAVE IERR
#include "qcdesh.h"
      CHARACTER *(*) CNAM
      DATA IERR /0/
C----------------------------------------------------------------------
C
      ICL = KSCOLD (ICLASS)
      IF (ICL .EQ. 0)  THEN
        IERR = IERR + 1
        IF (IERR .LE. 10)  CALL QWMESE
     &    ('_QVDROP_ Class does not exist')
        GO TO 90
      ENDIF
C
      CALL QVDROA (CNAM,ICL)
C
   90 END
#endif
