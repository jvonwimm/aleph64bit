      SUBROUTINE EBECMD
C ----------------------------------------------------
C   AUTHOR   : R.Clifft 08/06/88
C              J.Badier 29/11/89
C! Analysis for a photon hitting an end cap module near a crack.
C! Calculate a corrected energy and a position.
CKEY PHOTONS CRACK ENERGY POSITION / INTERNAL
C  Use data from crack clusters within functions derived empirically
C  from Monte Carlo studies which relate the energy ratios RATIO1
C  and RATIO2 to missing energy and impact coordinate
C
C
C     called by      EBCRAD
C     calls          NONE
C
C     banks          NONE
C
C ----------------------------------------------------
#ifndef DOC
      SAVE
C            ECCS = end cap energy correction extrapolation factor
      PARAMETER( ECCS = -.0014 )
#include "ebenec.h"
      REAL ECFAC(4,3,2) , PHIRG(4) , ECFAJ(4)
C
      DATA PHIRG / 0.0655,0.0327,0.0218,0.0164 /
      DATA ECFAJ / 5.,16.,33.,43. /
      DATA ECFAC / 16.,17.,18.,18.,9.5,10.5,11.5,12.5,3.5,5.,6.5,8.,
     &             10.,10.,11.,12.,4.5,5.,5.5,7.,19.5,3*30. /
C
C *** Interogate cluster position and extent with respect to crack
C *** to determine region parameters for correction algorithms
C
      KNEXT = 3
      KR2 = 0
      IF(RATIO2 .LT. PETIT) KR2 = 1
      IF(KODEEB(3) .LT. 3)  THEN
        KREGI = 1
        IF(KODEEB(3) .EQ. 1) KNEXT = 1 + KR2
      ELSE
        KREGI = 2
        IF(KODEEB(3) .EQ. 4) KNEXT = 1 + KR2
      ENDIF
      IF( NREGEB(1) .NE. 1 ) KREGI = 3 - KREGI
C
C *** Derive a corrected energy
C
      FREFC = ECFAC(NREGEB(3) , KNEXT , KREGI) / 100.
      FENEC = FREFC + ECCS * ( FLOAT( ITRWEB ) - ECFAJ( NREGEB(3) ) )
      IF( FENEC .GT. .9 ) FENEC = .9
      ENETOT = ENCRAT / (1. - FENEC)
C
C *** Derive a photon impact coordinate orthogonal to the crack
C
      SIHARG = (FLOAT(ITRWEB) - .5) * CECT1 + CECT2
      HTHET = DISFE / 2. * ( EXP(SIHARG) - EXP(-SIHARG) )
      YST1 = HTHET * PHIRG( NREGEB(3) )
      IF(KODEEB(3) .EQ. 1 .OR. KODEEB(3) .EQ. 4) THEN
        YCOFIN = ( YST1 - YLIMIT(KREGI) ) / 2.
        IF(YCOFIN .LT. 0.) YCOFIN = 0.
        YCOFIN = YCOFIN + YLIMIT(KREGI)
      ELSE
        YCOFIN = 1.5 * YST1
        IF( YST1 .LT. YLIMIT(KREGI) ) YCOFIN =
     +      YLIMIT(KREGI) + ( 2. * YST1 - YLIMIT(KREGI) ) / 2.
      ENDIF
      RETURN
      END
#endif
