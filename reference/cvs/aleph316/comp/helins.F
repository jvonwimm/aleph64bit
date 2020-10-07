      SUBROUTINE HELINS(NR1,NR2,R,V,PHI,Z)
C -----------------------------------------------------------------
C! Calculates intersection of a helix with a set of radii
C    AUTHOR: MARTIN POPPE
C!   CHANGE: MARTIN POPPE   22.JULY 1987
C!   INPUT: NR1, NR2    = RANGE OF INDICES IN R,V,PHI,Z
C!          R(NR1-NR2)  = RADII
C!          V(1-5)      = HELIX PARAMETERS (AS IN TGFT)
C!  OUTPUT: PHI(NR1-NR2)= PHI VALUES PREDICTED
C!          Z(NR1-NR2)  = Z   VALUES PREDICTED
C -------------------------------------------------------------
#ifndef DOC
      SAVE
C
      DIMENSION R(*),V(*),PHI(*),Z(*)
                           A =  1.0
      IF(V(1).LT.0.0)      A = -1.0
      IF(ABS(V(1)).GT.0.0000001) THEN
        DO 10 I=NR1,NR2
        IF(R(I).LT.ABS(V(4)))    GO TO 10
        X =
     1    ( V(1)*(R(I)*R(I)-V(4)*V(4))/( 2.0*R(I)*(V(1)*V(4)-A) )
     2     +V(4)/R(I) )
        Y =   0.5*V(1)* SQRT(    ( R(I)*R(I)-V(4)*V(4) )
     1                         /( 1.0-A*V(1)*V(4) )        )
        IF(ABS(X).LT.1.0.AND.ABS(Y).LT.1.0) THEN
          PHI(I) = V(3) + A*ASIN(X)
          S = (2.0/V(1))*ASIN(Y)
          Z(I) = V(5) + V(2)*S
        ELSE
          PHI(I) = 10000.0
          Z(I)   = 10000.0
        END IF
   10   CONTINUE
      ELSE
        DO 20 I=NR1,NR2
C       PRINT *,' USE TAYLOR ET AL'
        PHI(I) = V(3) + A*
     1    ( V(1)*(R(I)*R(I)-V(4)*V(4))/( 2.0*R(I)*(V(1)*V(4)-A) )
     2     +V(4)/R(I) )
        S =  SQRT( R(I)*R(I)+V(4)*V(4)
     1                   +2.0*A*V(4)*R(I)*SIN (V(3)-PHI(I)) )
        Z (I) = V(5) + V(2)*S
   20   CONTINUE
      END IF
      RETURN
      END
#endif
