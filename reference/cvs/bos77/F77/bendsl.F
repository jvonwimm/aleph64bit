      SUBROUTINE BENDSL(LUN,NRCPTR,JWREC)
#include "boscom.h"
      INTEGER JWREC(*)
*     internal routine to handle select output
*     to be called after new input
      IF(LUN.NE.LUNDAT) GOTO 100
      NRUN=0
      NEVT=0
      NPTR=NRCPTR
      IF(NPTR.EQ.0) GOTO 100
      DO 20 ITRY=1,2
         DO 10 I=1,NRE
            IF(JWREC(1).NE.NAMERE(I)) GOTO 10
            IF(ITRY.EQ.1) THEN
              IF(JWREC(2).NE.NUMMRE(I)) GOTO 10
            ENDIF
            NRUN=JWREC(4+IRUNRE(I))
            NEVT=JWREC(4+IEVTRE(I))
            GOTO 100
   10    CONTINUE
   20 CONTINUE
  100 RETURN
      END
