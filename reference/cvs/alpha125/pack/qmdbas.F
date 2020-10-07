      SUBROUTINE QMDBAS
CKEY PART /INTERNAL
C----------------------------------------------------------------------
C! read in short-term database
C! called from QMINIT
C!                                                  E.Blucher  03.04.90
C! 1991/93 old beam positions now read by getbp     J. Boucrot 24.09.98
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
C
      CHARACTER CDBASE*80
C----------------------------------------------------------------------
C
      IF (XCOPYJ)  GO TO 900
C
C       read temporary database.
C
      CALL BKFMT('EGAC','2I,(2I,36F)')
        CDBASE = 'DBASBANK'
        CALL AOPEN (KUCAR2, CDBASE, 'CARDS', 'DISK', IER)
        IDUM = MRESET (KUCAR2)
C---Turn printing off in BREADC
       IF(IER.EQ.0)THEN
         IW(4)=0
         CALL BREADC
         IW(4)=1
         CALL QWMESS ('_QMDBAS_ Read temporary database: ' // CDBASE)
       ELSE
         CALL QWMESS ('_QMDBAS_ Temporary database does not exist')
       ENDIF
C
  900 CONTINUE
      END
#endif
