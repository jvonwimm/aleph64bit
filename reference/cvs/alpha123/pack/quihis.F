      SUBROUTINE QUIHIS
CKEY HIST INIT /USER
C----------------------------------------------------------------------
C! initialize histogram package
C called from QMINIT
C                                                   H.Albrecht 20.09.88
C                                       modified    J.Boucrot  03.05.93
C----------------------------------------------------------------------
#ifndef DOC
C
#include "qcde.h"
      PARAMETER (LQPAW = 100000)
      COMMON /PAWC/ PAWC(LQPAW)
C
C       HBOOK initialisation
C
      IF (KDEBUG.NE.0)  THEN
         WRITE ( KUPRNT , 1001 ) LQPAW
         IF (KUPTER.NE.0) WRITE ( KUPTER , 1001 ) LQPAW
      ENDIF
      CALL HLIMIT (LQPAW)
C
C       read HISTOGRAM AND HTITLE card and open histogram file
C
      CALL QMIHIS
C
 1001 FORMAT ('0_QUIHIS_ Init HBOOK4 with ',I8,' words working space')
      END
#endif
