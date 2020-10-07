      SUBROUTINE EBCRAD
C ----------------------------------------------------
C   AUTHOR   :  J.Badier   29/11/89  ( from R. Clifft )
C! Analysis for a photon hitting a barrel module near a crack.
CKEY PHOTONS CRACK BARREL / INTERNAL
C  Calculate a corrected energy and a position.
C  Use data from crack clusters within functions derived empirically
C  from Monte Carlo studies which relate the energy ratios RATIO1
C  and RATIO2 to missing energy and impact coordinate
C
C
C     called by      EBRANC
C     calls          NONE
C
C     banks          NONE
C
C ----------------------------------------------------
#ifndef DOC
      SAVE
      PARAMETER ( CRAD1 = 1.144 , CRAD2 =.016 , CRAD3 = 2.5558 )
      PARAMETER ( CRAD4 = .1775 , CRAD5 = 1.4025 )
      PARAMETER ( CRAS1 = .005 , CRAS2 = .12 , CRAS3 = .75 )
      PARAMETER ( CRAS4 = .5 , CRAS5 = .05 )
      PARAMETER ( CRAB1=.16 , CRAB2=.04 , CRAB3 =.15 , CRAB4=.43 )
#include "ebenec.h"
C
C *** Derive a corrected energy using RATIO1
C
      IF(RATIO1 .LE. PETIT) THEN
        ENER1 = 0.
      ELSE
        ENER1 = ENECRA(1) * ( CRAD1 + CRAD2 * RATIO1 ) + ENECRA(2)
      ENDIF
C
C *** Derive a corrected energy using RATIO2
C
      IF(RATIO2 .LE. CRAS1) THEN
        ENER2 = ENCRAT
      ELSE
        IF( RATIO2 .LE. CRAS2 ) THEN
          PD = SQRT( ( RATIO2 - CRAS1 ) / CRAD3 )
        ELSE
          PD = ( RATIO2 + CRAD4 ) / CRAD5
        ENDIF
        ENER2 = ENCRAT * ( 1. + PD )
      ENDIF
C
C *** Calculate errors on the determinations of energy.
C
      ERD1 = CRAB1 + CRAB2 * RATIO1
      ERD2 = CRAB3 + CRAB4 * RATIO2
C   Weighted mean.
      IF(RATIO1 .LE. PETIT)   THEN
        ENETOT = ENER2
        ENEERR = ERD2
      ELSE
        IF( RATIO2 .LE. PETIT .OR. RATIO1 .GT. CRAS3 .OR.
     +  ( RATIO1 .GT. CRAS4 .AND. RATIO2 .LT. CRAS5 ) ) THEN
          ENETOT = ENER1
          ENEERR = ERD1
        ELSE
          RED1 = 1. / ERD1 ** 2
          RED2 = 1. / ERD2 ** 2
          ENETOT = (ENER1*RED1 + ENER2*RED2) / (RED1+RED2)
          ENEERR = SQRT( 1. / (RED1+RED2) )
        ENDIF
      ENDIF
C
C *** Set to 0.the impact coordinate.
C
      YCOFIN = 0.
      RETURN
      END
#endif
