      SUBROUTINE EBYPHI( YCO , PHI )
C ----------------------------------------------------
C   AUTHOR   : R.Clifft 08/06/88
C               J.Badier   29/11/89
C! Convert local coordinate y ,orthogonal to a crack, to ALEPH
C! coordinate phi.
CKEY PHOTONS CRACK COORDINATE / INTERNAL
C
C   Input  :    YCO  Local coordinate.
C
C   Output    :    PHI  ALEPH coordinate phi.
C
C     called by      EBRANC
C     calls          NONE
C
C     banks          NONE
C
C ----------------------------------------------------
      SAVE
C            DISB = ECAL barrel radius parameter
C            PHIH = ECAL module half phi angle
C            PHIO = ECAL barrel - end cap phi offset.
      PARAMETER( PHIO = .0327 , PHIH = .2618 )
      PARAMETER( DISB = 186.8 , DISF = .26795 , YTMP = 2.5 )
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
      COMMON/EBENEC/ENCRAT,ENECRA(2),ENECA1(2),EESTYA(3),EESTYB(3),
     1        RATIO1,RATIO2,R11STY,R12STY,
     2        ITRWEB,JFCLEB, KODEEB(4),NREGEB(3),SINCEB,
     3        ENETOT,ENEERR,YCOFIN,YCOERR,PHICOR,
     4        YLIMIT(3)
C
      PARAMETER ( ETHRL = 0.03 , CECT1 = 0.0121 , DISFE = 255. )
      PARAMETER ( CECT2 = 0.1904 , PETIT = .0001 )
      PARAMETER ( YLIM1 = 1.8 , YLIM2 = 3.2 , YLIM3 = 1.3 )
      IF( NREGEB(1) .NE. 2 ) THEN
C
C *** PHI from Y endcap
C
        JR = 2
        IF( KODEEB(3) .EQ. NREGEB(1) .OR.
     +      KODEEB(3) .EQ. NREGEB(1) + 1 )   JR = 1
        YTEMP = 3. - YLIMIT(JR)
        PHICR = FLOAT( NREGEB(2) - 1 ) * 2. * PHIH -
     +          SIGN( PHIH , YTEMP ) - PHIO
        YDIF = SIGN( YCO , YTEMP )
        SIHARG = ( FLOAT(ITRWEB) - .5 ) * CECT1 + CECT2
        HDIST = DISFE * ( EXP(SIHARG) - EXP(-SIHARG) ) / 2.
        PHI = PHICR + ATAN2( YDIF , HDIST )
        IF(PHI .LT. 0.) PHI = PHI + TWOPI
C
      ELSE
C
C *** PHI from Y barrel
C
        PHIMD = ( FLOAT(NREGEB(2)) - .5 ) * 2. * PHIH - PHIO
        YDIF = DISB * DISF - YCO
        YTEMP = YTMP - FLOAT(KODEEB(3))
        YDIF = SIGN( YDIF , YTEMP )
        PHI = PHIMD - ATAN2( YDIF , DISB )
C
      ENDIF
      RETURN
      END
