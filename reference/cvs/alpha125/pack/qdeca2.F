      FUNCTION QDECA2 (I,J)
CKEY KINEMATICS /USER
C----------------------------------------------------------------------
C   Author   : E. Blucher      7-OCT-1988
C   Modification:  L. Bellantoni 8-Jan-1994
C   Purpose:
C   ===========
C!  For the 2-body decay M --> m(I) + m(J), this function calculates the
C  angle between the M momentum vector and the m(I) momentum
C  vector in the M rest frame.
C======================================================================
#ifndef DOC
#include "qcde.h"
      REAL*8 PP, BETA, PPAR, P_1, P_2, PMAG
      DIMENSION P_1(4), P_2(4)
#include "qmacro.h"
C
      P_1(1) = DBLE(QX(I))
      P_1(2) = DBLE(QY(I))
      P_1(3) = DBLE(QZ(I))
      P_1(4) = DBLE(QE(I))
      PMAG = DSQRT( P_1(1)**2 +P_1(2)**2 +P_1(3)**2 )
C
      P_2(1) = DBLE(QX(J))
      P_2(2) = DBLE(QY(J))
      P_2(3) = DBLE(QZ(J))
      P_2(4) = DBLE(QE(J))
C
      PP = DSQRT( (P_1(1)+P_2(1))**2 +(P_1(2)+P_2(2))**2
     +                               +(P_1(3)+P_2(3))**2)
      BETA = PP / ( P_1(4)+P_2(4) )
      PPAR = ( P_1(1) *(P_1(1)+P_2(1)) + P_1(2) *(P_1(2)+P_2(2))
     +                                 + P_1(3) *(P_1(3)+P_2(3)) )/PP
      PP = PPAR - BETA * P_1(4)
      QDECA2 = PP /
     +        DSQRT(PP**2 + (PMAG-PPAR)*(PMAG+PPAR)*(1.-BETA)*(1.+BETA))
C======================================================================
      RETURN
      END
C
#endif
