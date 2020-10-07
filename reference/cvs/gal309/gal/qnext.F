*DK qnext
      SUBROUTINE QNEXT
C -----------------------------------------------------------------
C - F.Ranjard - 870105
C! calls next event or end of job
C - Called from QNEXTE                                from cernlib
C - Calls       ACRUN, ASCEVE, ASPRUN                 from this .HLB
C ------------------------------------------------------------------
#ifndef DOC
#include "jobcom.h"
C ------------------------------------------------------------------
C     normal entry
      IF (TACTJO .EQ. 'RETURN') THEN
C     process loop over event
         CALL ASPRUN
      ELSEIF (TACTJO .EQ. 'NEXT') THEN
         CALL RDMOUT(NRNDJO)
         CALL ASPRUN
      ELSEIF (TACTJO .EQ. 'END') THEN
C     close job
         CALL ASCRUN
         CALL ASCJOB
         STOP
      ENDIF
C
      END
#endif
