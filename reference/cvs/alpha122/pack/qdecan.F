      FUNCTION QDECAN (I,J)
CKEY KINEMATICS /USER
C----------------------------------------------------------------------
C   Author   : E. Blucher      27-JAN-1989
C   Purpose:
C   ===========
C!  For the decay M(I) --> m(J)+...+m(n), this function calculates the
C  angle between the M(I) momentum vector and the m(J) momentum
C  vector in the M(I) rest frame.
C
C======================================================================
#ifndef DOC
      SAVE NERR
#include "qcde.h"
      DATA NERR /0/
#include "qmacro.h"
C
      PP = QP(I)
      BETA = PP / QE(I)
      PPAR = (QX(I) * QX(J) + QY(I) * QY(J) + QZ(I) * QZ(J)) / PP
      PP = PPAR - BETA * QE(J)
      QDECAN = PP**2 + (QP(J) - PPAR) * (QP(J) + PPAR) *
     1        (1. - BETA) * (1. + BETA)
      IF (QDECAN .GT. 0.)  THEN
        QDECAN = PP / SQRT (QDECAN)
      ELSE
        NERR = NERR + 1
        IF (NERR .LE. 10)  CALL QWMESE
     +    ('_QDECAN_ 2nd particle is not a decay product of 1st')
        QDECAN = 1.
      ENDIF
C======================================================================
      RETURN
      END
#endif
