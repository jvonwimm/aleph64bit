      SUBROUTINE SISOL2(COE,X1,X2,IOK)
C-------------------------------------------------------------
C!  - Solve equation A x**2 + B x + C = 0
C!
C!    Author B.Bloch-Devaux JUNE 92
C!    Input:  COE(3) coefficients of equation
C!    Output: X1,X2 solutions
C!            IOK Status flag      0 = ok 2 solutions
C!                                 1 = single solution
C!                                 2 = no solution
C!
C?
C!======================================================================
#ifndef DOC
      DIMENSION COE(3)
      IF (COE(1).EQ.0.) THEN
        IF ( COE(2).NE.0.) THEN
          X1 = -COE(3)/COE(2)
        ELSE
          X1 = 0.
        ENDIF
        IOK = 1
        GO TO 999
      ENDIF
      DELTA = COE(2)**2-4.*COE(1)*COE(3)
      IF (DELTA.LT.0.) THEN
        IOK = 2
        GO TO 999
      ELSE
        X1 = 0.5*( -COE(2)+SQRT(DELTA) )/COE(1)
        X2 = 0.5*( -COE(2)-SQRT(DELTA) )/COE(1)
        IOK  = 0
      ENDIF
  999 RETURN
      END
#endif
