      SUBROUTINE QJOPTM (COPT,CADD)
CKEY SHAPE JETS MC /USER
C----------------------------------------------------------------------
C! options for QJ routines (MC particles)
C                                                    H.Albrecht 22.2.89
C----------------------------------------------------------------------
#ifndef DOC
      CHARACTER *(*) COPT,CADD
      CHARACTER * 2 C2
#include "qcdesh.h"
C-----------------------------------------------------------------------
      C2 = COPT
      IF (C2 .EQ. 'EP')  THEN
        KJOPTN(2,1) = 1
      ELSE IF (C2 .EQ. 'VI')  THEN
        KJOPTN(2,1) = 2
      ELSE IF (C2 .EQ. 'AL')  THEN
        KJOPTN(2,1) = 3
      ELSE IF (C2 .EQ. 'NO')  THEN
        KJOPTN(2,1) = 4
      ELSE
        CALL QMTERM ('_QJOPTM_ Unknown option ' // C2)
      ENDIF
C
      KJOPTN(2,2) = 0
      IF (CADD .NE. ' ')  KJOPTN(2,2) = KPART (CADD)
C
      END
#endif
