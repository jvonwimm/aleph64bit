      SUBROUTINE GBTRAN( NUST,INDX,ESTO,BARY,NATU,GAMM )
C -----------------------------------------------------------------
C   AUTHOR   : M.N. Minard 17/03/93
C   Extracted from EBTRAN : Author J.Badier 09/10/89
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
      PARAMETER ( COMP = .1 , ENEST = .85 , GAGA = 1.5 , ZECP = 260. )
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
      IF( KPAS .EQ. 1 ) GO TO 98
C   First gamma treatment only.
      GAMM(1) = GAM1
      GAMM(2) = GAM2
C   Compactness test.
      IF( GAMM(1) .LT. COMP ) GO TO 98
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
        CALL GBIMPA( ITCR , JFCR , BARI , BARJ , GERB )
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
   98 CONTINUE
      RETURN
      END
#endif
