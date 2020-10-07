      SUBROUTINE ITRKEQ (R,RI,TL,P0,D0,Z0,Z,PH,IFLAG)
C
C------------------------------------------------------------------------------
C!    Calculate z and phi coords of a helical track at radius r.
C!
C!    Author               M J Phillips with J Sedgbeer       01-Jan-1991
C!
C!    Input                R          r-coordinate of track
C!                      (  RI         inverse radius of curvature
C!         usual        (  TL         tan (lambda) = tan (dip angle)
C!          track       (  P0         phi-zero
C!           parameters (  D0         d-zero
C!                      (  Z0         z-zero
C!
C!    Output               Z          z-coordinate of track
C!                         PH         phi-coordinate of track
C!                                    in range 0 to 2*PI
C!                         IFLAG = 0  Successful
C!                                 1  Unsuccessful
C!
C!    Description
C!    ===========
C!    Returns z and phi coords of a helical track at radius r.
C!    Only first half of first loop of helix is considered.
C!    Track is parametrised by the usual (RI,TL,P0,D0,Z0).
C!
C?
C!======================================================================
#ifndef DOC
#include "alcons.h"
      IFLAG = 0
      A = R*R - D0*D0
      B = 1. - D0*RI
      C = A / B
      IF (RI.EQ.0.0) THEN
            S = 1.0
      ELSE
            S = ABS(RI)/RI
      ENDIF
      D = S * (RI * (-C) / (2. * R) + D0 / R)
      IF (ABS(D).GT.1.0) GOTO 100
      PH = P0 - S * ASIN (D)
      IF (PH.LT.0.0) PH=PH+TWOPI
      IF (C.LT.0.0) GOTO 100
      E = -RI * SQRT (C) / 2.
      IF (ABS(E).GT.1.0) GOTO 100
      Z = Z0 - 2. * TL * ASIN (E) / RI
      GOTO 200
 100  IFLAG = 1
 200  RETURN
      END
#endif
