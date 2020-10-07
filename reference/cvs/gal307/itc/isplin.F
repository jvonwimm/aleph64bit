*DK isplin
      SUBROUTINE ISPLIN(U, V1, V2, SLENG, VP)
C.
C...ISPLIN  1.00  900424  13:04                        R.Beuselinck.
C.
C!  Compute point on cubic spline curve, 1st call.
C.
C.  Arguments:
C.  U     [S,I,REAL] : Fraction of distance along total spline length.
C.  V1    [V,I,REAL] : (x,y,z,dx,dy,dz) of first point on spline.
C.  V2    [V,I,REAL] : (x,y,z,dx,dy,dz) of last point on spline.
C.  SLENG [S,I,REAL] : Total path length from V1 to V2.
C.  VP    [V,O,REAL] : (x,y,z) of requested point on spline.
C.
C?  Use a standard cubic spline formula to compute the coordinates of
C?  the point at a distance U along a track element, where U ranges
C?  from 0.0 to 1.  The coordinate origin is shifted to the first point
C?  on the track element to remove one term from the spline formula.
C.
C-----------------------------------------------------------------------
#ifndef DOC
      REAL U, V1(*), V2(*), SLENG, VP(*)
      REAL X1, Y1, Z1, XN, YN, ZN, DX1, DY1, DZ1, DX2, DY2, DZ2, SL
      REAL U2, U3, C1, C2, C3
      SAVE X1, Y1, Z1, XN, YN, ZN, DX1, DY1, DZ1, DX2, DY2, DZ2, SL
C
C--  Setup constants required for all calls.
C--
      SL = SLENG
      X1 = V1(1)
      Y1 = V1(2)
      Z1 = V1(3)
      XN = (V2(1)-V1(1))/SL
      YN = (V2(2)-V1(2))/SL
      ZN = (V2(3)-V1(3))/SL
      DX1 = V1(4)
      DY1 = V1(5)
      DZ1 = V1(6)
      DX2 = V2(4)
      DY2 = V2(5)
      DZ2 = V2(6)
      GO TO 10
#endif
      ENTRY ISPLIO(U, VP)
C.
C...ISPLIO  1.00  900424  13:04                        R.Beuselinck.
C.
C!  Compute point on cubic spline curve, later call.
C.
C.  Arguments:
C.  U  [S,I,REAL] : Fraction of distance along total spline length.
C.  VP [V,O,REAL] : (x,y,z) of requested point on spline.
C.
C.  This is an entry point of ISPLIN to reduce the number of calling
C.  arguments required for subsequent calls for the same spline.
C.
C-----------------------------------------------------------------------
#ifndef DOC
C
C--  Compute spline coefficients as a function of U.
C--
   10 U2 = U*U
      U3 = U2*U
      C1 = 3*U2 - 2*U3
      C2 = U - 2*U2 + U3
      C3 = -U2 + U3
      VP(1) = (C1*XN + C2*DX1 + C3*DX2)*SL + X1
      VP(2) = (C1*YN + C2*DY1 + C3*DY2)*SL + Y1
      VP(3) = (C1*ZN + C2*DZ1 + C3*DZ2)*SL + Z1
      END
#endif
