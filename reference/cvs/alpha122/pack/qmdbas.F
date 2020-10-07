      SUBROUTINE QMDBAS
CKEY PART /INTERNAL
C----------------------------------------------------------------------
C! read in short-term database and 1991/1992 beam positions
C! called from QMINIT
C!                                                  E.Blucher  03.04.90
C!                                                  J. Boucrot 14.06.93
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
C       read database for beam positions .
C
      CALL BDROP(IW,'BPBB')
      CDBASE = 'BEAMPOSITION'
      CALL AOPEN (KUCAR2, CDBASE, 'CARDS', 'DISK', IER)
      IDUM = MRESET (KUCAR2)
C---Turn printing off in BREADC
      IF (IER.EQ.0) THEN
         IW(4)=0
         CALL BREADC
         IW(4)=1
         CALL QWMESS ('_QMDBAS_ Read beam position database: '//CDBASE)
      ELSE
         CALL QWMESS ('_QMDBAS_ Beam position database does not exist')
      ENDIF
C
C
  900 CONTINUE
      END
#endif
