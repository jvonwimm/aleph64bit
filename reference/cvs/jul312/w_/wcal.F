      SUBROUTINE WCAL( ITYP, X , WEIG , IFIT)
C========================================================================
C!   To create weighted P-vectors for Energy-flow calculation
C-
C-   Authors : M.N. Minard , M. Pepe       10/05/89
C
C-   Input : ITYP    element type as defined in subroutine FLOWFI
C-           X       element P-vector
C            IFIT    fit type (1=Calorimeters 2=Masks)
C-   Output: WEIG    weighting coefficient for each element
C
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "alcons.h"
      DIMENSION X(3)
      DIMENSION THZONE(6)
      EXTERNAL NAMIND
      DATA IFIRST / 0/
#include "bmacro.h"
C
      IF(IFIRST.EQ.0) THEN
C
C- Read definition of angular regions
C
       KEFCZ = IW(NAMIND('EFZC'))
       NZONE = 6
       IF (KEFCZ.NE.0) THEN
        DO 10 I=1,NZONE
         THZONE(I) = RW(KEFCZ+LMHLEN+I)
 10     CONTINUE
       IFIRST= 1
       ENDIF
      ENDIF

      KEFCA = IW(NAMIND('EFCA'))

C
C-   According to fit type search for appropriate weights
C
      RTR = SQRT ( X(1)**2+X(2)**2)
      IF (RTR . LT . 0.01) THEN
         WEIG = 0.
         RETURN
      ENDIF
      TET = ATAN2 ( RTR , X(3))
      IF (TET.GT.PIBY2) TET = PI - TET
      IZONE = 0
      TET = TET*RADEG
      DO 100 I=1,5
         IF(TET.GT.THZONE(I).AND.TET.LE.THZONE(I+1)) THEN
           IZONE = I
          ENDIF
 100  CONTINUE
      IZONE = 6 -IZONE
      IF ( IZONE.EQ.6 ) IZONE = 5
C
C
      IF ( ITYP.GT.20 ) THEN
         WEIG = 1
      ELSE
         WEIG = RTABL(KEFCA,IFIT,(ITYP-1)*(NZONE-1)+IZONE)
      ENDIF
      RETURN
      END

#endif
