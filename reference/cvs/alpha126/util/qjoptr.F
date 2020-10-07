      SUBROUTINE QJOPTR (COPT,CADD)
CKEY SHAPE JETS /USER
C----------------------------------------------------------------------
C! options for QJ routines (reconstructed particles)
C                                                    H.Albrecht 22.2.89
C                                    Modified:       E.Blucher  30.1.92
C----------------------------------------------------------------------
#ifndef DOC
      CHARACTER *(*) COPT,CADD
      CHARACTER * 2 C2
#include "qcdesh.h"
C-----------------------------------------------------------------------
      C2 = COPT
      IF (C2 .EQ. 'CH')  THEN
        KJOPTN(1,1) = 1
      ELSE IF (C2 .EQ. 'CO')  THEN
        KJOPTN(1,1) = 2
      ELSE IF (C2 .EQ. 'RE')  THEN
        KJOPTN(1,1) = 3
      ELSE IF (C2 .EQ. 'NO')  THEN
        KJOPTN(1,1) = 4
      ELSE IF (C2 .EQ. 'AL')  THEN
        KJOPTN(1,1) = 5
      ELSE IF (C2 .EQ. 'EC')  THEN
           CALL QMTERM
     *    ('_QJOPTR_ Options EC and EM are obsolete; use option EF')
      ELSE IF (C2 .EQ. 'EM')  THEN
           CALL QMTERM
     *    ('_QJOPTR_ Options EC and EM are obsolete; use option EF')
      ELSE IF (C2 .EQ. 'EF')  THEN
        KJOPTN(1,1) = 6
      ELSE IF (C2 .EQ. 'PC')  THEN
        KJOPTN(1,1) = 7
      ELSE IF (C2 .EQ. 'EJ')  THEN
        KJOPTN(1,1) = 8
      ELSE
        CALL QMTERM ('_QJOPTR_ Unknown option ' // C2)
      ENDIF
C
      KJOPTN(1,2) = 0
      IF (CADD .NE. ' ')THEN
        IF(KJOPTN(1,1).EQ.7)CALL QMTERM
     & ('_QJOPTR_ Additional particles cannot be given with PC option')
        KJOPTN(1,2) = KPART (CADD)
      ENDIF
      IF(KJOPTN(1,1).EQ.7) KJOPTN(1,2) = KPART('NEOB')
C
      END
#endif
