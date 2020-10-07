      SUBROUTINE YZPHA1(PR,RL1,Z0,PSIN)
C-----------------------------------------------------
CKEY YV0 ANGLE DISTANCE /INTERNAL
C! Find psi angle respect to the point pr,minimizing the z distance
C
C AUTHOR : M.A.CIOCCI 20/1/90
C MODIFIED:
C    INPUT
C          PR(3)/R  Generally the beam-beam crossing point coordinates
C          RL1/R    (Radius of curvature)*(Tangent of dip)
C          Z0/R     Z0 (see frft bank)
C          PSIN/R   psi angle mod twopi
C
C    OUTPUT
C          PSIN/R   psi angle
C
C     CALLED:
C             YPSIVE
C
C     CALLS:
C             NONE
C
C                      DESCRIPTION
C                      ============
C     Select the correct phase for the psi angle minimizing also
C     the z distance and recalculates psi
C
C
C------------------------------------------------------------------
      SAVE
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
       REAL PR(3)
C
      DISM=9999.
      I1=0
      DO 2 I=-1,1
      DIST=ABS(PR(3)-
     $        (Z0+RL1*(PSIN+FLOAT(I)*TWOPI)))
      IF(DIST.LT.DISM)THEN
      DISM=DIST
      I1=I
      ENDIF
   2  CONTINUE
      PSIN=PSIN+FLOAT(I1)*TWOPI
      RETURN
      END
