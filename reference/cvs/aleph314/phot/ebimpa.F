      SUBROUTINE EBIMPA( ITET , JPHI , BARI , BARJ , GERB )
C ----------------------------------------------------
C   AUTHOR   : J.Badier   31/10/88
C!  Corrected impact estimation for a gamma.
CKEY PHOTONS IMPACT / INTERNAL
C
C   Input   : ITET    Tetha index of a tower.
C             JPHI    Phi index of a tower.
C             BARI    { Barycenter coordinates by respect to the
C             BARJ    { tower center expressed in pads units.
C
C   Output  : GERB    Calculated impact on the first EMCAL plane.
C
C   BANKS :
C     INPUT   : NONE
C     OUTPUT  : NONE
C     CREATED : NONE
C
C ----------------------------------------------------
#ifndef DOC
      SAVE
      EXTERNAL ESRPT
      DIMENSION GERB(*)
      DIMENSION U(3) , V(3)
C   A0 and A1 were obtain from test runs data , the barycenter being
C   calculated over a 3 by 3 towers system.
      PARAMETER ( A0 = .75 , A1 = .09 , PADI = 3.03 )
C   Theta pad size.
C   Phi pad size
      TET1 = ITET
      TET0 = TET1 + .5
      PHI1 = JPHI
      PHI2 = PHI1 + 1.
      PHI0 = PHI1 + .5
      CALL ESRPT( 'ALEPH' , TET0 , PHI1 , 1. , U )
      CALL ESRPT( 'ALEPH' , TET0 , PHI2 , 1. , V )
      PADJ = SQRT( ( V(1) - U(1) ) ** 2
     +           + ( V(2) - U(2) ) ** 2
     +           + ( V(3) - U(3) ) ** 2 )
C   Barycenter correction.
C   Corrected Itet.
      ATET = ( A0 + A1 * PADI ) ** 2
      ABRI = 2. * ABS( BARI )
      CORI = ( 1. + ATET - ABRI ) / ( 1. + ABRI * ( ATET - 1. ) )
      DI = TET0 + BARI * CORI
C   Corrected Jphi.
      APHI = ( A0 + A1 * PADJ ) ** 2
      ABRJ = 2. * ABS( BARJ )
      CORJ = ( 1. + APHI - ABRJ ) / ( 1. + ABRJ * ( APHI - 1. ) )
      DJ = PHI0 + BARJ * CORJ
C   Estimated impact.
      CALL ESRPT( 'ALEPH' , DI , DJ , 1. , GERB )
      RETURN
      END
#endif
