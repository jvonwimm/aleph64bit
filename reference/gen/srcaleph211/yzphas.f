      SUBROUTINE YZPHAS(Z1,Z2,PSI1,PSI2,RL1,RL2,DISM)
C-----------------------------------------------------
C! Select the correct phase minimizing the z distance
CKEY YV0 PHASE DISTANCE /INTERNAL
C
C AUTHOR : M.A.CIOCCI,L.ROLANDI   7-4-88
C MODIFIED:
C
C    INPUT Z1/R  Z COORDINATE ALONG FIRST HELIX
C                DEFINED MOD TWOPI
C          Z2/R  Z COORDINATE ALONG SECOND HELIX
C                DEFINED MOD TWOPI
C        PSI1/R  PSI ANGLE ALONG FIRST HELIX
C                DEFINED MOD TWOPI
C        PSI2/R  PSI ANGLE ALONG SECOND HELIX
C                DEFINED MOD TWOPI
C         RL1/R  (RADIUS OF CURVATURE)*(TANGENT OF DIP)
C                 FOR FIRST HELIX
C         RL2/R  (RADIUS OF CURVATURE)*(TANGENT OF DIP)
C                FOR SECOND HELIX
C    OUTPUT PSI1/R PSI ANGLE ALONG FIRST HELIX
C           PSI2/R PSI ANGLE ALONG SECOND HELIX
C           DISM/R MINIMUM Z DISTANCE WHEN DISTANCE ON X-Y IS MINIMIZED
C
C            DESCRIPTION
C           ============
C
C Select the correct phase minimizing the Z distance
C see aleph note about V0 reconstruction
C
C     CALLED BY yfpsin
C
C--------------------------------------------------
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
C
      DISM=9999.
      I1=0
      J1=0
      DO 2 I=-1,1
      DO 1 J=-1,1
      DIST=ABS(Z1+RL1*(PSI1+FLOAT(I)*TWOPI)-
     $        (Z2+RL2*(PSI2+FLOAT(J)*TWOPI)))
      IF(DIST.LT.DISM)THEN
      DISM=DIST
      I1=I
      J1=J
      ENDIF
   1  CONTINUE
   2  CONTINUE
      PSI1=PSI1+FLOAT(I1)*TWOPI
      PSI2=PSI2+FLOAT(J1)*TWOPI
      RETURN
      END
