      SUBROUTINE GBIMPA( ITET , JPHI , BARI , BARJ , GERB )
C ----------------------------------------------------
CKEY PHOTONS
C   AUTHOR   : M.N. Minard 17/03/03
C   Extracted from EBIMPA : Author J.Badier 31/10/88
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
      INTEGER ROW,COL,ST,LEPLAN,NCOINS,I
      REAL POINT(4),PLAN(4),PLANES(4,6),COINS(3,4),MILIEU(3,4)
      REAL LO,LA
      DIMENSION U(3) , V(3)
C   A0 and A1 were obtain from test runs data , the barycenter being
C   calculated over a 3 by 3 towers system.
      PARAMETER ( A0 = .75 , A1 = .088)
C   Theta pad size.
C   Phi pad size
       CALL ESRBC('ALEPH',ITET,JPHI,1,POINT)
       PLAN(1)=POINT(1)
       PLAN(2)=POINT(2)
       PLAN(3)=POINT(3)
       PLAN(4)=-(POINT(1)**2+POINT(2)**2+POINT(3)**2)
       CALL ESRPL('ALEPH',ITET,JPHI,1,LEPLAN,PLANES)
       CALL ACOUPE(LEPLAN,PLANES,PLAN,NCOINS,COINS)
       LO=0.
       LA=0.
       DO 2 I=1,3
          MILIEU(I,1)=(COINS(I,1)+COINS(I,2))/2.
          MILIEU(I,2)=(COINS(I,2)+COINS(I,3))/2.
          MILIEU(I,3)=(COINS(I,3)+COINS(I,4))/2.
          MILIEU(I,4)=(COINS(I,4)+COINS(I,1))/2.
          LO=LO+(MILIEU(I,3)-MILIEU(I,1))**2
          LA=LA+(MILIEU(I,4)-MILIEU(I,2))**2
    2  CONTINUE
       PADI=SQRT(LO)
       PADJ=SQRT(LA)
C   Barycenter correction.
C   Corrected Itet.
      ATET = ( A0 + A1 * PADI ) ** 2
      ABRI = 2. * ABS( BARI )
      CORI = ( 1. + ATET - ABRI ) / ( 1. + ABRI * ( ATET - 1. ) )
      TET0 = FLOAT(ITET) +0.5
      DI = TET0 + BARI * CORI
C   Corrected Jphi.
      APHI = ( A0 + A1 * PADJ ) ** 2
      ABRJ = 2. * ABS( BARJ )
      CORJ = ( 1. + APHI - ABRJ ) / ( 1. + ABRJ * ( APHI - 1. ) )
      PHI0 = FLOAT(JPHI) +0.5
      DJ = PHI0 + BARJ * CORJ
C   Estimated impact.
      CALL ESRPT( 'ALEPH' , DI , DJ , 1. , GERB )
      RETURN
      END
#endif
