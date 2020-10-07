      SUBROUTINE EHREG(  X , IREG)
C=======================================================================
C!   To create weighted P-vectors for Energy-flow calculation
C-
C-   Authors : M.N. Minard , M. Pepe       10/01/90
C
C-   Input : X       element P-vector
C    Output: Ireg    E-Flow region number for hcal
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


C
C-   Determine region number
C
      IREG = 5
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
      IREG = IZONE
      IF (IZONE .GT.2 ) THEN
         IF ( X(3).GT.0) THEN
            IREG = 3
         ELSE
            IREG = 4
         ENDIF
      ENDIF
      RETURN
      END
#endif
