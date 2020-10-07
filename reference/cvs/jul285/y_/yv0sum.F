      SUBROUTINE YV0SUM
C----------------------------------------------------------------------
C!Print run statistics for V0
C!   AUTHOR: M.A.Ciocci,L.Rolandi 13/4/88
C!   MODIFIED : M.Bosman 12/04/89
C?
C!======================================================================
#ifndef DOC
#include "rlunit.h"
#include "rstatr.h"
#include "y0josu.h"
C
C+     TEST IF THER'ARE V0s
C
      IF(NEVV0F.EQ.0.AND.NV0FOU.NE.0)GO TO 9999
      IF(NRUNRS(4).NE.0) THEN
       YV0ME=(FLOAT(NV0FOU)/FLOAT(NRUNRS(4)))
      ELSE
       YV0ME=0.0
      ENDIF
      WRITE(LOUTRL,777) NEVV0F,YV0ME
 777  FORMAT(' V0 Job Summary :',/,
     +' Number of events with V0.......................',I5,/,
     +' Average number of V0s per event................',F5.2,/)
      RETURN
 9999 CALL REPORT('Y0JOSU','ERROR NOT FOUND V0',0)
      RETURN
      END
#endif
