      SUBROUTINE GXIMPA( ITTT , JPHI , BARI , BARJ , GERB , CTHCOR )
C ----------------------------------------------------
CKEY PHOTONS IMPACT / INTERNAL
C!  Corrected impact estimation for a gamma.
C   AUTHOR   : J.Badier   31/10/88
C
C   MODIF    : A. Rouge  10/12/92 PADI and PADJ computed
C              using H. Videau's ERWIDT  code
C              A. Rouge  24/2/93   Compute the expected F4
C
C   Input   : ITET    Tetha index of a tower.
C             JPHI    Phi index of a tower.
C             BARI    Barycenter coordinates by respect to the
C             BARJ       tower center expressed in pads units.
C
C   Output  : GERB    Calculated impact on the first EMCAL plane.
C             CTHCOR  Expected   F4
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
C   A0 and A1 were obtain from test runs data , the barycenter being
C   calculated over a 3 by 3 towers system.
      PARAMETER ( A0 = .75 , A1 = .088  )
C     A1 modified for pad size at mid height of stack 1
C
       FRAC(X) = X - INT(X)
       GJBD(X) = 1. - 1. / (2.+2.4*X+1.35*X**1.9)
C
C   Theta pad size.
C   Phi pad size
C
       ITET = ITTT
       IF(ITET.GE.46.AND.ITET.LE.50)ITET=ITET+5
       IF(ITET.GE.179.AND.ITET.LE.183)ITET=ITET-5
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
C
C     F4 first approximation   (ex GATRAL)
C
       XPAD = 0.5*(PADI+PADJ)
       CTHCOR = 3. * GJBD(XPAD) - 2.
C
C   Barycenter correction.
C   Corrected Itet.
C
      ATET = ( A0 + A1 * PADI ) ** 2
      ABRI = 2. * ABS( BARI )
      CORI = ( 1. + ATET - ABRI ) / ( 1. + ABRI * ( ATET - 1. ) )
      TET0 = FLOAT(ITET) + .5
      DI = TET0 + BARI * CORI
C   Corrected Jphi.
      APHI = ( A0 + A1 * PADJ ) ** 2
      ABRJ = 2. * ABS( BARJ )
      CORJ = ( 1. + APHI - ABRJ ) / ( 1. + ABRJ * ( APHI - 1. ) )
      PHI0 = FLOAT(JPHI) + .5
      DJ = PHI0 + BARJ * CORJ
C   Estimated impact.
      CALL ESRPT( 'ALEPH' , DI , DJ , 1. , GERB )
C
C     F4 correction ( barycenter offset )
C
      DI = FRAC(ABS(DI))
      DJ = FRAC(ABS(DJ))
      DI = AMIN1(DI,1.-DI)
      DJ = AMIN1(DJ,1.-DJ)
      CTHCOR = GFRAC(3.*DI,3.*DJ) * CTHCOR / .851
C
      RETURN
      END
#endif
