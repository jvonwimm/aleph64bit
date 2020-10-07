      SUBROUTINE QJTHRU (THRU,CNAM,ICLASS)
CKEY SHAPE /USER
C----------------------------------------------------------------------
C! Thrust
C                                                    H.Albrecht 22.2.89
C  Modified for QMTHRU                               J.Boucrot 18.10.94
C----------------------------------------------------------------------
#ifndef DOC
      CHARACTER *(*) CNAM
#include "qcde.h"
#include "qctbuf.h"
C-----------------------------------------------------------------------
C
C Unless XJTHRU is set to .TRUE. before the CALL QJTHRU,
C read only the pre-calculated thrust stored in bank PTHR, if any.
C
      IF ( XJTHRU ) GOTO 50
      CALL QMTHRU
      IF (KTBO.LE.0) GO TO 50
      GO TO 100
 50   CALL QJSETU(ICLASS)
C
      CALL QGTHRU
C
100   THRU = QTBOR(1)
      IF (CNAM .NE. ' ')  CALL QJSAVE (CNAM,1,ICLASS,0,0,0)
C
C Restore the default value of XJTHRU to avoid ambiguities in
C later CALLs of QJTHRU
C
      XJTHRU = .FALSE.
C
      END
#endif
