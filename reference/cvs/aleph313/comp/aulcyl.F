      SUBROUTINE AULCYL( RAD, ZMAX, PAR, PAREX, ICODE)
C  M. MERMIKIDES   171186
C  Modified by B. Bloch-Devaux march 1988 to make it work also if
C  initial start point is outside the cylinder
C  Modified by B.Bloch-Devaux June 1989 : fix bug when track entering
C  from outside by endplate.
C
C! Computes intersection of a line with a cylinder with axis along the
C  z-axis.
CKEY COMPUTE TRACK CYLINDER   /USER
C
C  Input:
C        RAD   Radius of cylinder       (cm)
C        ZMAX  Half-length of cylinder. (cm)
C        PAR(1) = x
C           (2) = y      Coordinates of start point on the line (cm)
C           (3) = z
C           (4) = dx/ds
C           (5) = dy/ds  Direction cosines at start point
C           (6) = dz/ds
C
C  Output:
C        PAREX(1) = x
C             (2) = y      Coordinates at point of intersection
C             (3) = z      with cylinder.
C             (4) = dx/ds
C             (5) = dy/ds  Direction cosines at end point
C             (6) = dz/ds
C
C        ICODE  = Return code (1 = intersection in barrel part
C                              2     "          "   end cap
C                              0     "          not possible
C --------------------------------------------------------------
#ifndef DOC
      SAVE
      DIMENSION PAR(6),PAREX(6)
C
      ICODE = 0
C See if we intersect cylinder
      B = PAR(5)/PAR(4)
      A = PAR(2) - B*PAR(1)
      TERM = RAD*RAD*(1.0 + B*B) - A*A
      IF (TERM.LT.0.) GO TO 999
      TERM = SQRT(TERM)
      X1 = (-A*B + TERM)/(1.0 + B*B)
      X2 = (-A*B - TERM)/(1.0 + B*B)
C Pick solution consistent with line direction
      IF (PAR(4)*(X1-PAR(1))*(X2-PAR(1)).GT.0.) THEN
         X = X2
         IF( PAR(1).GT.X1) GO TO 999
      ELSE
         X = X1
         IF (PAR(1).LT.X2 ) GO TO 999
      ENDIF
      Y = A + B*X
C Check if we cross the end plane
      IF (ABS(PAR(5)).GT.0.001) THEN
         Z = PAR(3) + (Y - PAR(2))*PAR(6)/PAR(5)
      ELSE
         Z = PAR(3) + (X - PAR(1))*PAR(6)/PAR(4)
      ENDIF
      IF (ABS(Z).GT.ZMAX) THEN
C      Check if inside or outside
         IF (ABS(PAR(3)).LT.ZMAX) THEN
           Z = SIGN(ZMAX,PAR(6))
         ELSE
           Z=SIGN(ZMAX,PAR(3))
           IF (Z*PAR(6).GT.0.) GO TO 999
         ENDIF
         S = (Z - PAR(3))/PAR(6)
         X = PAR(1) + S*PAR(4)
         Y = PAR(2) + S*PAR(5)
         IF (X*X+Y*Y.GT.RAD*RAD) GO TO 999
         ICODE = 2
      ELSE
         S = (X - PAR(1))/PAR(4)
         ICODE = 1
      ENDIF
      PAREX(1) = X
      PAREX(2) = Y
      PAREX(3) = Z
      CALL UCOPY(PAR(4),PAREX(4),3)
  999 RETURN
      END
#endif
