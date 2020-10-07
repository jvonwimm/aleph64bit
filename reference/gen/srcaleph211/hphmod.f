      REAL FUNCTION HPHMOD(PHITRK)
C.*******************************************************
CKEY HCALDES HCAL BARREL COORDINATES PHI ANGLE / USER
C.*                                   011186 G.Catanesi *
C! To evaluate the phi value in the barrel module coordinates
C.*
C.*******************************************************
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
C.
      IF(MOD(PHITRK,PIBY6) .LE. PIBY12)THEN
         HPHMOD = MOD(PHITRK,PIBY6)
      ELSE
         HPHMOD = PIBY6 - MOD(PHITRK,PIBY6)
      ENDIF
      IF(HPHMOD.GT.PIBY12)HPHMOD = HPHMOD - PIBY12
      RETURN
      END
