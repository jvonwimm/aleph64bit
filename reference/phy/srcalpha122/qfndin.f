      SUBROUTINE QFNDIN(EVECE,PLAN,IERR)
CKEY  FILL / INTERNAL
C ----------------------------------------------------------------------
C! Find intersection between a line and a plane
C  Called from QEXTRA
C      Authors : M.N. Minard                 11/11/93
C
C Input : EVECE(6)       = x,y,z,u,v,w (point coordinates and
C                          direction cosines)
C         PLAN (4)       = coefficients of plane equation
C Output: EVECE(3)       = x,y,z of intersection point
C         IERR           = 1 no solution
C ----------------------------------------------------------------------
      DIMENSION EVECE(7),PLAN(4)
      IERR = 0
      IF (PLAN(4).NE.0.) THEN
        DO 10 I=1,4
          PLAN(I)= PLAN(I)/PLAN(4)
  10    CONTINUE
      ENDIF
      A11 = PLAN(1)
      A12 = PLAN(2)
      A13 = PLAN(3)
      A21 = EVECE(5)
      A22 = - EVECE(4)
      A23 = 0.
      A31 = EVECE(6)
      A32 = 0.
      A33 = -EVECE(4)
      B1  = -1.
      B2  = EVECE(5)*EVECE(1)-EVECE(4)*EVECE(2)
      B3  = EVECE(6)*EVECE(1)-EVECE(4)*EVECE(3)
      DET = A11*(A22*A33-A23*A32)-A12*(A21*A33-A23*A31)+
     &      A13*(A21*A32-A22*A31)
      IF(ABS(DET).LT.1.E-6) THEN
       IERR = 1
       RETURN
      ENDIF
C
C- Solution of linear system
C
      EVECE(1) = (B1*(A22*A33-A23*A32)-A12*(B2*A33-B3*A23)+
     &            A13*(B2*A32-A22*B3))/DET
      EVECE(2) = (A11*(B2*A33-B3*A23)-B1*(A21*A33-A31*A23)+
     &            A13*(A21*B3-A31*B2))/DET
      EVECE(3) = (A11*(A22*B3-A32*B2)-A12*(A21*B3-A31*B2)+
     &            B1*(A21*A32-A31*A22))/DET
      RETURN
      END
