*DK cahgrb
      SUBROUTINE CAHGRB(ENER,IFIN,XGERB,NRJDP)
C
C     Deposition of hadronic energy in ECAL
C     J.Badier  10/03/88
C     ENER  : hadronic energy to be deposited
C     IFIN  : 0  no more points to be generated
C     IFIN  : 1  points have to be generated to deposit
C                energy
C     XGERB : coordinate of generated points
C     NRDJ  : energy deposited by generated point
C     calls to   CASHAD ,POISSN , CAHTRV  ,EHDEPH
C     called from EHSHOW
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
      NRJDP = 0
      IF( IFIN .EQ. 0 ) THEN
         IF( ENER .LT. . 001) GO TO 30
         IFIN = 1
C ----- Initialisation
C ----- Angle avec le module.
         CSTE = ABS( TRKELE(6) )
         CSTE = MAX ( . 01, CSTE )
         TGTE = SQRT( 1. / CSTE **  2- 1. )
         IF ( ABS(TRKELE(3)).GT.EZMAX) TGTE =1./TGTE
C ----- Limits for generation.
         DELT = ERHMX*TGTE/GABSL
         SMIN = TINOL0 - DELT * TRKELE(9) - TRKELE(11) / GABSL
         SMAX = TINOL0 + DELT * TRKNXT(9)
C ----- Test medium type
         IF ( EMNAME.EQ.TRKVOL) THEN
           IF ( SMIN.LT.SMAXLA ) SMIN = SMAXLA
           IF ( SMAX.LT.SMAXLA ) SMAX = SMAXLA
         ENDIF
         SMAXLA = SMAX
         EMNAME = TRKVOL
C ----- Integration step.
         DDS = .5 * EDSHAD
         TDS = ETANG * EDSHAD
         ISTP = ( SMAX - SMIN ) / EDSHAD
         ISTP = ISTP + 1
C ----- Number of points per abs. len.
         CNRJ = PARGVH * DDS
C ----- First step initialisation.
         S1 = SMIN - EDSHAD
         S2 = SMIN
         NPT = 0
         INIT = 0
      ELSE
C ----- Points generation.
         IF( NPT .EQ.  0) THEN
   10       CONTINUE
            IF( ISTP .EQ.  0) GO TO 30
            ISTP = ISTP - 1
            S1 = S2
         S2 = S1 + EDSHAD
C ----- Number of points for this step.
            SP1 = S1 + TDS
            F1 = CASHAD( SP1 , INIT )
            INIT = 1
            SP2 = S2 - TDS
            F2 = CASHAD( SP2 , INIT )
            EF = CNRJ * ( F1 + F2 )
            CALL POISSN( EF , NPT , IER )
            IF( NPT .EQ.  0) GO TO 10
            RHO = F1 / ( F1 + F2 )
            RH1 = DDS / RHO
            IF (RHO .GT. 0.999) RHO = 0.999
            RH2 = DDS / ( 1. - RHO )
         ENDIF
         ALEA = RNDM(ALEA)
         IF( ALEA .LT. RHO ) THEN
            ST = S1 + RH1 * ALEA
         ELSE
            ST = S2 - RH2 * ( 1. - ALEA )
         ENDIF
         NPT = NPT - 1
         XGERB(1) = ( ST - TINOL0 ) * GABSL
         DGRB = XGERB(1) + TRKNXT(9)
C ----- Radial.
         CALL CAHTRV( DGRB , RAD )
         IF ( RAD .GT. 0. ) THEN
            PHI = TWOPI * RNDM(RAD)
            XGERB(2) = RAD * COS(PHI)
            XGERB(3) = RAD * SIN(PHI)
         ELSE
            XGERB(2)=0.
            XGERB(3)=0.
         ENDIF
C ----- Energie deposee.
      CALL EHDEPH(NRJDP)
C
      ENDIF
C
   20 CONTINUE
      RETURN
C ----- Plus de points!
   30 CONTINUE
      IFIN = 0
      GO TO 20
      END
#endif
