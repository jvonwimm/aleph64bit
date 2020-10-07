*DK cahhrb
      SUBROUTINE CAHHRB(ENER,IFIN,XGERB,NRDJ)
C     Deposition of hadronic energy in HCAL
C     M.N Minard  10/05/88
C     ENER  : hadronic energy to be deposited
C     IFIN  : 0  no more points to be generated
C     IFIN  : 1  points have to be generated to deposit
C                energy
C     XGERB : coordinate of generated points
C     NRDJ  : 0 generated point does not deposit energy
C           : #0 it does
C     calls to   CASHAD ,POISSN , CAHTRV
C     called from HCSHOW
C======================================================
#ifndef DOC
      SAVE
      DIMENSION XGERB(*)
#include "alcons.h"
#include "iocom.h"
#include "ehpash.h"
#include "gcmate.h"
#include "capano.h"
#include "trkcom.h"
#include "ehcond.h"
#include "cahtra.h"
#include "hcloc.h"
      DATA RHMX / .4 / , TANG / .21132 / ,CHNRJ/ 4.27 /
      IF( IFIN .EQ. 0 ) THEN
        IF( ENER .LT. .001 ) GO TO 2
        IFIN = 1
C ----- Initialisation
C ----- Angle avec le module.
        CSTE = ABS( HCTRKM(6,1))
        CSTE = MAX ( .01 , CSTE )
        CSTE = MIN ( .99995,CSTE)
        TGTE = SQRT( 1. / CSTE ** 2 - 1. )
        IF( ABS( HCTRKM(3,1) ) .LT. 250. ) TGTE = 1. / TGTE
C ----- Limits for generation.
        DELT = RHMX * TGTE / HSHGAB
        SMIN = HTIN01 - DELT * HCTRKM(9,1)
        SMAX = TINOL0 + DELT * HCTRKM(9,2)
C
C
C ----- Integration step.
        DS = .2
        DDS = .5 * DS
        TDS = TANG * DS
C ----- Number of steps.
        ISTP = ( SMAX - SMIN ) / DS
        ISTP = ISTP + 1
C ----- Number of points per abs. len.
        CNRJ = CHNRJ * DDS
C ----- First step initialisation.
        NRDJ = 1
        S1 = SMIN
        S2 = SMIN + DS
        NPT = 0
        INIT = 0
      ELSE
C ----- Points generation.
        NRDJ = 0
        IF( NPT .EQ. 0 ) THEN
    1     CONTINUE
          IF( ISTP .EQ. 0 ) GO TO 2
          ISTP = ISTP - 1
          S1 = S2
          S2 = S1 + DS
C ----- Number of points for this step.
          SP1 = S1 + TDS
          F1 = CASHAD( SP1 , INIT )
          INIT = 1
          SP2 = S2 - TDS
          F2 = CASHAD( SP2 , INIT )
          EF = CNRJ * ( F1 + F2 )
          CALL POISSN( EF , NPT , IER )
          IF( NPT .EQ. 0 ) GO TO 1
          RHO = F1 / ( F1 + F2 )
          IF (RHO.LT.0.0001)RHO=0.0001
          RH1 = DDS / RHO
          IF (RHO.GT.0.999) RHO=0.999
          RH2 = DDS / ( 1. - RHO )
        ENDIF
        ALEA = RNDM(ALEA)
        IF( ALEA .LT. RHO ) THEN
          ST = S1 + RH1 * ALEA
        ELSE
          ST = S2 - RH2 * ( 1. - ALEA )
        ENDIF
        NPT = NPT - 1
        XGERB(1) = ( ST - HTIN01 ) * HSHGAB
        XGERB1 = HCPAX0+XGERB(1)*HCPACX
        XGERB2 = HCPAY0+XGERB(1)*HCPACY
        XGERB3 = HCPAZ0+XGERB(1)*HCPACZ
        DGRB = XGERB(1) + HCTRKM(9,1)
C ----- Radial.
        CALL CAHTRV( DGRB , RAD )
        IF ( RAD .GT. 0. ) THEN
          PHI = TWOPI * RNDM(RAD)
          XGERB(2) = RAD * COS(PHI)
          XGERB(3) = RAD * SIN(PHI)
C
C-      ROTATE SYS
C
        ELSE
          XGERB(2)=0.
          XGERB(3)=0.
        ENDIF
        XGERB(1) = XGERB1+ COS(HCPPHI)*SIN(HCPDIP)*XGERB(2)
     +           - SIN(HCPPHI)*XGERB(3)
        XREL = XGERB(2)
        XGERB(2) = XGERB2 + SIN(HCPPHI)*SIN(HCPDIP)*XGERB(2)
     +             +COS(HCPPHI)*XGERB(3)
        XGERB(3) = XGERB3 - COS(HCPDIP)*XREL
C ----- Energie deposee.
C
      NRDJ = 1
      ENDIF
C
   98 CONTINUE
      RETURN
C ----- Plus de points!
    2 CONTINUE
      IFIN = 0
      GO TO 98
      END
#endif
