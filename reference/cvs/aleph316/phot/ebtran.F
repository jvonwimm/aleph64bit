      SUBROUTINE EBTRAN( NUST,INDX,ESTO,BARY,NATU,GAMM )
C -----------------------------------------------------------------
C   AUTHOR   : J.Badier   09/10/89
C!  Particle identification using transverse distribution.
CKEY PHOTONS TRANSVERSE PARTICLES / INTERNAL
C
C   The better inpact estimation is calculated from the barycenter
C   of 3 by 3 towers around a central tower. The 2 by 2 towers around
C   the barycenter allow to perform an identification.
C
C   Input  :    NUST    Number of storeys of the cluster ICLN.
C               INDX(1,IST) Theta index of the storey IST.
C               INDX(2,IST) Phi index of the storey IST.
C               INDX(3,IST) Stack number of the storey IST.
C               ESTO(IST)   Raw content of the storey IST.
C                           IST = 1 , NUST
C
C   Output :    NATU(1)  Transverse identification.
C               NATU(2)  Nature of the first identified particle.
C               NATU(3)  Nature of the second identified particle.
C               GAMM(1)  First transverse estimator : proportion of
C                        energy in the four central towers.
C               GAMM(2)  Second transverse estimator : deviation from th
C                        mean shape of a single photon. The distribution
C                        is a normal law for an isolated gamma.
C               GAMM(3)  Best estimate of the first particle energy (Gev
C               GAMM(4)  Teta line of flight of the first particle.
C               GAMM(5)  Phi line of flight of the first particle.
C               GAMM(6)  Best estimate of the 2nd particle Energy (Gev)
C               GAMM(7)  Teta line of flight of the second particle.
C               GAMM(8)  Phi line of flight of the second particle.
C
C   BANKS :
C     INPUT   : NONE
C     OUTPUT  : NONE
C     CREATED : NONE
C
C ----------------------------------------------------
#ifndef DOC
      SAVE
      DIMENSION INDX(3,*) , ESTO(*) , BARY(*) , NATU(*) , GAMM(*)
      DIMENSION  GRID(3,3,3) , GERB(3)
      PARAMETER ( ACOF = 1.25 , PREC = 5. , PTIT = .0001 )
      PARAMETER ( FLCT = .4 )
      PARAMETER ( COMP = .8 , ENEST = .85 , GAGA = 1.5 , ZECP = 260. )
#include "alcons.h"
      CHARACTER*16 MAL
      KPAS = 0
C   Storey max. index and raw energy ( ERAW ).
      ERAW = 0.
      EMX = 0.
      IMX = 1
      DO 1 IST = 1 , NUST
        ENER = ESTO(IST)
        ERAW = ERAW + ENER
        IF( ENER .GT. EMX ) THEN
          EMX = ENER
          IMX = IST
        ENDIF
    1 CONTINUE
C   Tower containing the storey max.
      ITCR = INDX(1,IMX)
      JFCR = INDX(2,IMX)
   11 CONTINUE
C   At the second pass ( KPAS = 1 ) it is the central tower of
C   the second gamma.
C   3 X 3 storeys around central tower.
      CALL EBGRID( NUST,INDX,ESTO,ITCR,JFCR,GRID)
C   Barycenter over 3 towers in pads units.
      EIP = 0.
      EIC = 0.
      EIM = 0.
      EJP = 0.
      EJM = 0.
      DO 2 L = 1 , 3
      DO 2 K = 1 , 3
        EIP = EIP + GRID( 3 , L , K )
        EIC = EIC + GRID( 2 , L , K )
        EIM = EIM + GRID( 1 , L , K )
        EJP = EJP + GRID( L , 3 , K )
        EJM = EJM + GRID( L , 1 , K )
    2 CONTINUE
      ETT = EIP + EIC + EIM
      IF( ETT .LT. PTIT ) ETT = PTIT
      BARJ = ( EJP - EJM) / ETT
      BARI = ( EIP - EIM) / ETT
C   Analysis of 2 X 2 towers around barycenter.
      IV = 1
      JV = 1
      IF( BARI .GT. 0. ) IV = 3
      IF( BARJ .GT. 0. ) JV = 3
C   Initialisation of transverse identificators.
      X = 0.
      Y = 0.
      Z = 0.
      T = 0.
C   Calculation of transverse identificators.
      DO 3 K = 1,3
C   2 X 2 central towers content.
        T = T + GRID(  2 , 2 , K )
        X = X + GRID( IV , 2 , K )
        Y = Y + GRID(  2 ,JV , K )
        Z = Z + GRID( IV ,JV , K )
    3 CONTINUE
        IF( T .LE. 0. ) T = .01
C   Summation of 2 X 2 towers.
      SOM4 = T + X + Y + Z
      GAM1 = SOM4 / ERAW
C   Transverse deviation from single gamma expectation.
      X = X / T
      Y = Y / T
C   ZTH is the expected value of Z if it is a single gamma.
      ZTH =  X * Y * ( 1. + ACOF * ( 1. - X ) * ( 1. - Y ) )
      IF (ZTH.LE.0) ZTH = 0.01
      ZTH = ZTH * T
      GAM2 = PREC * ( Z - ZTH ) / SQRT( T + ZTH )
      IF( KPAS .EQ. 1 ) GO TO 12
C   First gamma treatment only.
      GAMM(1) = GAM1
      GAMM(2) = GAM2
C   Compactness test.
      IF( GAMM(1) .LT. COMP ) GO TO 4
C   Only one identified particle.
      GAMM(3) = ERAW
      NATU(3) = 0
      GAMM(6) = 0.
      GAMM(7) = 0.
      GAMM(8) = 0.
      IF( GAMM(2) .LT. GAGA ) THEN
C   Isolated gamma.
        NATU(1) = 0
        NATU(2) = 1
C   Impact estimation.
        CALL EBIMPA( ITCR , JFCR , BARI , BARJ , GERB )
C   Direction
        RGER = SQRT( GERB(1) ** 2 + GERB(2) ** 2 )
        GAMM(4) = ATAN2( RGER , GERB(3) )
        GAMM(5) = ATAN2( GERB(2) , GERB(1) )
        IF( GAMM(5) .LT. 0. ) GAMM(5) = GAMM(5) + TWOPI
      ELSE
C   Unresolved gamma-gamma.
        NATU(1) = 1
        NATU(2) = 10
        GAMM(4) = BARY(1)
        GAMM(5) = BARY(2)
      ENDIF
      GO TO 98
C   Resolved multi gammas.
    4 CONTINUE
C   Estimation using the first gamma : #A
      EN1A = SOM4 / ENEST
      EN2A = ERAW - EN1A
C   Impact estimation #A of the first gamma.
      CALL EBIMPA( ITCR , JFCR , BARI , BARJ , GERB )
C   Direction #A of the first gamma.
      RGER = SQRT( GERB(1) ** 2 + GERB(2) ** 2 )
      TH1A = ATAN2( RGER , GERB(3) )
      PH1A = ATAN2( GERB(2) , GERB(1) )
      IF( PH1A .LT. 0. ) PH1A = PH1A + TWOPI
C   Direction #A of the second gamma.
      FI1A = PH1A
      IF( PH1A - BARY(2) .GT. PI ) FI1A = PH1A - TWOPI
      IF( PH1A - BARY(2) .LT. - PI ) FI1A = PH1A + TWOPI
      PH2A = ( ERAW * BARY(2) - EN1A * FI1A ) / EN2A
      TH2A = ( ERAW * BARY(1) - EN1A * TH1A ) / EN2A
      GERB(1) = COS( PH2A )
      GERB(2) = SIN( PH2A )
      GERB(3) = 1. / TAN( TH2A )
C   Row and column of the second gamma.
      CALL EFNCRS( GERB , CL , RW , ST , MAL )
      IF( MAL .EQ. 'ROW   ' ) THEN
        GRB3 = ABS( GERB(3) )
        GERB(1) = ZECP * GERB(1) / GRB3
        GERB(2) = ZECP * GERB(2) / GRB3
        GERB(3) = SIGN( ZECP , GERB(3) )
        CALL EFNCRS( GERB , CL , RW , ST , MAL )
        IF( MAL .EQ. 'REGION' ) GO TO 101
      ENDIF
      ITCR = RW
      JFCR = CL
      IF( ITCR .LE. 0 .OR. ITCR .GT. 228 ) GO TO 13
      KPAS = 1
      GO TO 11
C   Symetrical treatment centred on the second candidate gamma.
   12     CONTINUE
      CALL EBGAGA( 0 , GAM1 , GAM2 )
      ESTI = EN2A * COMP
      IF( SOM4 .LT. ESTI - FLCT * SQRT(ESTI) ) GO TO 13
C   The separation into two gammas is coherent.
      NATU(1) = 3
      NATU(2) = 2
      NATU(3) = 2
      EN2B = SOM4 / ENEST
      IF( ERAW .LE. EN2B ) EN2B = ERAW - PTIT
      EN1B = ERAW - EN2B
C   Impact estimation #B of the second gamma.
      CALL EBIMPA( ITCR , JFCR , BARI , BARJ , GERB )
C   Direction #B of the second gamma.
      RGER = SQRT( GERB(1) ** 2 + GERB(2) ** 2 )
      TH2B = ATAN2( RGER , GERB(3) )
      PH2B = ATAN2( GERB(2) , GERB(1) )
      IF( PH2B .LT. 0. ) PH2B = PH2B + TWOPI
C   Direction #B of the first gamma.
      TH1B = ( ERAW * BARY(1) - EN2B * TH2B ) / EN1B
      FI2B = PH2B
      IF( PH2B - BARY(2) .GT. PI ) FI2B = PH2B - TWOPI
      IF( PH2B - BARY(2) .LT. - PI ) FI2B = PH2B + TWOPI
      PH1B = ( ERAW * BARY(2) - EN2B * FI2B ) / EN1B
C   Weights : smaller is EN2, smaller is W2.
      W1 = ( EN1A + EN1B ) * .5 /  ERAW
      W2 = ( EN2A + EN2B ) * .5 /  ERAW
      GAMM(3) = W1 * EN1A + W2 * EN1B
      GAMM(4) = W1 * TH1A + W2 * TH1B
      FI1B = PH1B
      IF( PH1B - FI1A .GT. PI ) FI1B = PH1B - TWOPI
      IF( PH1B - FI1A .LT. - PI ) FI1B = PH1B + TWOPI
      GAMM(5) = W1 * FI1A + W2 * FI1B
      IF( GAMM(5) .LT. 0. ) GAMM(5) = GAMM(5) + TWOPI
      GAMM(6) = W1 * EN2A + W2 * EN2B
      GAMM(7) = W1 * TH2A + W2 * TH2B
      FI2A = PH2A
      IF( PH2A - PH2B .GT. PI ) FI2A = PH2A - TWOPI
      IF( PH2A - PH2B .LT. - PI ) FI2A = PH2A + TWOPI
      GAMM(8) = W1 * FI2A + W2 * PH2B
      IF( GAMM(8) .LT. 0. ) GAMM(8) = GAMM(8) + TWOPI
      GO TO 98
C   The second gamma is not well localised.
   13 CONTINUE
      NATU(1) = 2
      NATU(2) = 2
      NATU(3) = 12
      GAMM(3) = EN1A
      GAMM(4) = TH1A
      GAMM(5) = PH1A
      GAMM(6) = EN2A
      GAMM(7) = TH2A
      GAMM(8) = PH2A
   98 CONTINUE
      RETURN
C
C   Error.
  101 CONTINUE
      NATU(1) = 0
      NATU(2) = 0
      NATU(3) = 0
      DO 111 I = 3 , 8
        GAMM(I) = 0.
  111 CONTINUE
      GO TO 98
      END
#endif
