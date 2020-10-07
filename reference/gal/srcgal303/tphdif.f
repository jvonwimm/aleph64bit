      FUNCTION TPHDIF(PHI1,PHI2)
C
C  Returns difference between PHI1 and PHI2 in range -pi to + pi
C  (PHI1, PHI2 are in range 0 - 2pi)
C
C ---------------------------------------------------------
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      INTEGER NBITW, NBYTW, LCHAR
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)
C
C
      DPHI = PHI1 - PHI2
      IF(DPHI.GT. PI) DPHI =-TWOPI + DPHI
      IF(DPHI.LT.-PI) DPHI = TWOPI + DPHI
      TPHDIF = DPHI
      RETURN
      END
