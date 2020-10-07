      SUBROUTINE AUHDIS(FIELD,SLONG,VI,VO)
C--------------------------------------------------------------------
C J. Hilgart 10/12/86
C This routine gives the distance along a singly charged
C particle's helical trajectory between
C two specified points.
C
C      Input:  FIELD   in kG
C              VI(7)    xi, yi, zi, dc1, dc2, dc3,ptot
C              VO(3)    x0, y0
C      Output: SLONG, in cm
C ----------------------------------------------------------------
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
      DIMENSION VI(7),VO(2)
C
      XYLEN = VDIST(VI,VO,2)
            SZ = MAX(SQRT(1. - VI(6)**2),1.E-5)
      PT = VI(7)*SZ
      RHO = PT/(ABS(FIELD)*CLGHT*1.E-5)
      SLONG = 2.*RHO*ASIN(MIN(XYLEN/2./RHO,1.))/SZ
C
      RETURN
      END
