      SUBROUTINE EBARYC( NUST,INDX,ESTO,IKOD,BARY )
C ----------------------------------------------------
C   AUTHOR   : J. Badier     05/10/89
C! Barycenter calculation.
CKEY PHOTONS BARYCENTER / INTERNAL
C
C   The storeys are weighted with the raw signal.
C
C   Input     : NUST    Number of storeys of the cluster ICLN.
C               INDX(1,IST) Theta index of the storey IST.
C               INDX(2,IST) Phi index of the storey IST.
C               INDX(3,IST) Stack number of the storey IST.
C               ESTO(IST)   Raw content of the storey IST.
C                           IST = 1 , NUST
C
C   Output    : IKOD(9)
C               1   Theta index of the barycenter tower.
C               2   Phi index of the barycenter tower.
C               3   Subcomponent number( 1 to 3 ).
C               4   Module number( 1 to 12 ).
C               5   Region number( 1 to 4 ).
C               6   1 or 2 if limit of region.
C               7   1 if endcap crack in the overlap.
C               8   1 , 2 , 3 or 4 if near a crack.
C               9   1 to 6:barrel/endcap overlap, 7:endcap/luminometer.
C
C               BARY(1) Theta of the barycenter in radians.
C               BARY(2) Phi of the barycenter in radians.
C
C
C   BANKS :
C     INPUT   : NONE
C     OUTPUT  : NONE
C
C ----------------------------------------------------
#ifndef DOC
      SAVE
      DIMENSION INDX(3,*) , ESTO(*) , BARY(*) , IKOD(*)
      DIMENSION CENT(3) , EBAR(3)
#include "alcons.h"
C   Initialisation
      EBAR(1) = 0.
      EBAR(2) = 0.
      EBAR(3) = 0.
C   Loop over storeys.
      DO 1 IST = 1 , NUST
C   Storey barycenter.
        CALL ESRBC('ALEPH',INDX(1,IST),INDX(2,IST),INDX(3,IST),CENT)
        DO 2 J = 1 , 3
C   Weighted sum.
          EBAR(J) = EBAR(J) + ESTO(IST) * CENT(J)
    2   CONTINUE
    1 CONTINUE
C   Cluster barycenter direction.
      RD = SQRT( EBAR(1)**2 + EBAR(2)**2 )
C   Polar angle.
      BARY(1) = ATAN2( RD , EBAR(3) )
C   Azimuth.
      BARY(2) = ATAN2( EBAR(2) , EBAR(1) )
      IF( BARY(2) .LT. 0. ) BARY(2) = BARY(2) + TWOPI
C   Central tower region code.
      CALL EBCDRG(BARY(1),BARY(2),IKOD(1),IKOD(2),IKOD(6),IKOD(3),IER)
      IF( IER .NE. 0 ) IKOD(1) = 0
      RETURN
      END
#endif
