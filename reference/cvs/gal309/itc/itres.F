*DK itres
      SUBROUTINE ITRES(LAY, DIST, ANGLE)
C.
C...ITRES  3.00  930902  11:42                          R.Beuselinck
C.
C!  Smear ITC drift distance by parameterised resolution from IRRF.
C.
C.  LAY    [S,I,INTE] : layer number for current hit.
C.  DIST   [S,M,REAL] : distance of hit from sense wire.
C.  ANGLE  [S,I,REAL] : phi angle of line from sense wire to track
C.                      wrt cell.
C.
C.  Subroutines called:
C.    RANNOR                                      - from CERN library.
C.
C-----------------------------------------------------------------------
#ifndef DOC
#include "itresc.h"
#include "itwirc.h"
C
C--  Choose whether to use the positive-phi or negative-phi coefficients
C--  for the resolution on the basis of whether the drift path is in
C--  the positive or negative part of the cell, (+/- is defined wrt.
C--  the increase of decrease of the global phi angle as the cell is
C--  traversed azimuthally.
C--
      IF (COS(ANGLE).GT.0.0) THEN
        C1 = RESITN(1,LAY)
        C2 = RESITN(2,LAY)
        C3 = RESITN(3,LAY)
        C4 = RESITN(4,LAY)
        C5 = RESITN(5,LAY)
      ELSE
        C1 = RESITP(1,LAY)
        C2 = RESITP(2,LAY)
        C3 = RESITP(3,LAY)
        C4 = RESITP(4,LAY)
        C5 = RESITP(5,LAY)
      ENDIF
C
C--  Compute the resolution at the current drift distance and smear it
C--  by a unit Gaussian.
C--
      F = DIST/(0.5*CELWIT(LAY))
      DRES = (((C5*F + C4)*F + C3)*F + C2)*F + C1
      CALL RANNOR(ERR,WASTE)
      DIST = DIST + DRES*ERR
      END
#endif
