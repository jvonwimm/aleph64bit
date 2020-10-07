      SUBROUTINE IRUCHK(RWANT, U, DRDU)
C.
C...IRUCHK  1.00  900509  17:07                         R.Beuselinck
C.
C!  Check interpolation along ITC track element.
C.
C.  Arguments:
C.  RWANT [S,I ,REAL] : Required radius of interpolated point.
C.  U     [S,IO,REAL] : Fraction of distance along track element.
C.  DRDU  [S,I ,REAL] : Gradient of R wrt. U using quadratic approx.
C.
C.  The purpose of this routine is to correct for any significant
C.  errors in the predictions of the intersections of track elements
C.  with the inner and outer radial boundaries of the ITC sense wire
C.  layers.  These predictions are made by solving the quadratic
C.  approximation for R(u). In a small number of cases there may be
C.  significant discrepancy from the value of R calculated using the
C.  cubic spline formula.
C.
C.  RWANT is the radius required in (x,y) projection for intersection
C.  with an ITC wire cylinder.
C.  U is the fraction along the current track element at which the
C.  crossing is predicted.  DRDU is the local gradient derived from the
C.  quadratic approximation formula.
C.
C?  Calculate the actual value of R using the cubic spline formula at U.
C?  If the difference from RWANT is greater than tolerance
C?  calculate the predicted shift in U needed using the local gradient.
C?  Calculate RNEW using the updated U value according to the cubic
C?  spline formula.  Keep the new U value if RNEW is closer to RWANT.
C.
C-----------------------------------------------------------------------
      SAVE
      REAL RWANT, U, DRDU, V(3)
      PARAMETER (TOL=0.05, TINY=1.E-6)
C
C--  Calculate the value of R using the full cubic spline and check if
C--  it is within tolerance.
C--
      CALL ISPLIO(U, V)
      RCUB = SQRT(V(1)**2 + V(2)**2)
      DELR = RWANT-RCUB
      IF (ABS(DELR).LT.TOL) GO TO 999
C
C--  Use the gradient predicted from the quadratic approximation to
C--  try to find a better value for U.
C--
      IF (ABS(DRDU).LT.TINY) GO TO 999
      DELU = DELR/DRDU
      UNEW = U + DELU
      CALL ISPLIO(UNEW, V)
      RNEW = SQRT(V(1)**2 + V(2)**2)
      DNEW = RWANT-RNEW
      IF (ABS(DNEW).LT.ABS(DELR)) U = UNEW
  999 CONTINUE
      END
