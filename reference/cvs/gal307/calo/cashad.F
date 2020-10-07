*DK cashad
      FUNCTION CASHAD( S , INIT )
C --------------------------------------------------------------
CKEY CALO GEANTINO HADRONIC
C - M.N.Minard
C!
C - Input   : INIT   / I = initialisation flag
C           : S      / R = track length
C - Output  : CASHAD / R =
C ------------------------------------------------------------
#ifndef DOC
      SAVE
#include "capano.h"
C -----------------------------------------------------------
      IF( INIT .EQ.  0) HLONEN = HABETA ** HALPHA / GAMMA( HALPHA ) *
     +HADNRJ
C ----- Parametrisation longitudinale de galeph normalisee.
      IF( S .LE. . 0001) GO TO 10
      X = HABETA * S
      A = -X + ( HALPHA - 1. ) * ALOG(S)
      IF(ABS(A) .GT. 100.) GO TO 10
      CASHAD = EXP(A) * HLONEN
      GO TO 20
   10 CASHAD = 0.
   20 IF (CASHAD .LT. 0.00001) CASHAD = 0.00001
      END
#endif
