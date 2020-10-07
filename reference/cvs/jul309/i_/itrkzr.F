      SUBROUTINE ITRKZR (Z,RI,TL,P0,D0,Z0,R)
C
C------------------------------------------------------------------------------
C!    Calculate r as a function of z for a helix.
C!
C!    Author               M J Phillips with J Sedgbeer      01-Jan-1991
C!
C!    Input                Z          z-coordinate
C!                      (  RI         inverse radius of curvature
C!         usual        (  TL         tan (lambda) = tan (dip angle)
C!          track       (  P0         phi-zero
C!           parameters (  D0         d-zero
C!                      (  Z0         z-zero
C!    Output               R          r-coordinate of helix
C!
C!    Description
C!    ===========
C!    Subroutine calculates r as a function of z for a helix
C!    parametrised by (RI,TL,P0,D0,Z0). If subroutine fails for any
C!    reason, a negative value of r (-1.0) is returned to indicate this.
C!
C?    Equation used is
C?
C?              (    2                 4        2 (  RI           ) )
C?     R = SQRT (  D0  + (1-D0*RI) *  ---  * SIN  ( ---- * (Z-Z0) ) )
C?              (                    RI*RI        ( 2*TL          ) )
C?
C!======================================================================
#ifndef DOC
C
C  Track must have some "z-component"
C
      IF (ABS(TL).LT.0.0000001) GOTO 210
      A = (Z - Z0) * RI / (2. * TL)
C
C  Use small angle approximation if necessary
C
      IF (ABS(A).LT.0.0000001) THEN
            C = (Z - Z0) * (Z - Z0) / (TL * TL)
            GOTO 100
            ENDIF
C
C  Check potential overflow
C
      IF (ABS(A).GT.800000.) GOTO 210
      B = SIN (A)
C
C  Check potential overflow
C
      IF (RI.LT.0.0001) GOTO 210
      C = B * B * 4. / (RI * RI)
 100  D = 1. - D0 * RI
      E = C * D
      F = E + (D0 * D0)
      IF (F.LT.0.) GOTO 210
      R = SQRT (F)
      GOTO 220
 210  R = -1.0
 220  RETURN
      END
#endif
