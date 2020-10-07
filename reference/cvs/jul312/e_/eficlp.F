      SUBROUTINE EFICLP
C***********************************************************************
C!- TO COMPUTE CLUSTER PARAMETERS AS DEFINED IN 'ECLU'
C!         INPUT:'ESDA','ECLU'
C!         OUPUT:'ECLU'(MODIFIED)
C!           COMPUTE E,R,THETA,FI
C!           FOR CLUSTER AND PER LEVEL
C!
C***********************************************************************
#ifndef DOC
#include "bcs.h"
#include "ecnamc.h"
#include "esdajj.h"
#include "eclujj.h"
#include "alcons.h"
         DIMENSION NP(3),XX(4),X1B(3),X2B(3),X3B(3)
C
      KESDA  = IW(NAESDA)
      KECLU  = IW(NAECLU)
      NECLU  = IW(KECLU + LMHROW)
      DO 20 ICL=1,NECLU
         ICUCL = KECLU + LMHLEN + LECLUA*(ICL-1)
         IMT   = IW(ICUCL  + JECLES)
C SKIP CLUSTERS NO MORE CONNECTED TO ANY STOREY
         IF(IMT.EQ.0) GO TO 20
C
         CALL VZERO(NP,3)
         CALL VZERO(X1B,3)
         CALL VZERO(X2B,3)
         CALL VZERO(X3B,3)
         X1BA = 0.
         X2BA = 0.
         X3BA = 0.
   10    CONTINUE
         ICUST = KESDA + LMHLEN + LESDAA*(IMT-1)
         ENER  = RW(ICUST + JESDME)
         RW(ICUCL+JECLE4) = RW(ICUCL+JECLE4) + ENER
         K = IW(ICUST+JESDDK)
         RW(ICUCL+JECLE4+K) = RW(ICUCL+JECLE4+K) + ENER
C
         J = IW(ICUST+JESDTJ)
         I = IW(ICUST+JESDFI)
C? ROUTINE VIDEAU TO COMPUTE CENTER COORDINATES OF STOREY
            CALL ESRBC( 'ALEPH',J,I,K,XX)
         X1BA = X1BA + XX(1)*ENER
         X2BA = X2BA + XX(2)*ENER
         X3BA = X3BA + XX(3)*ENER
         NP(K) = NP(K) + 1
         X1B(K) = X1B(K) + XX(1)*ENER
         X2B(K) = X2B(K) + XX(2)*ENER
         X3B(K) = X3B(K) + XX(3)*ENER
C
         IMT = IW(ICUST + JESDES)
         IF (IMT .NE. 0)                           GOTO 10
         R   = SQRT(X1BA**2+X2BA**2+X3BA**2)/RW(ICUCL+JECLE4)
         FI  = ATAN2(X2BA,X1BA)
          IF (FI .LT. 0.) FI = FI +2.*PI
         TH  = ATAN2(SQRT(X1BA**2+X2BA**2),X3BA)
          IF (TH .LT. 0.) TH = TH +2.*PI
         RW (ICUCL + JECLR4) = R
         RW (ICUCL + JECLT4) = TH
         RW (ICUCL + JECLF4) = FI
         DO 30 K=1,3
         IF (NP(K) .EQ. 0) GOTO 30
         R   = SQRT(X1B(K)**2+X2B(K)**2+X3B(K)**2)/RW(ICUCL+JECLE4+K)
         FI  = ATAN2(X2B(K),X1B(K))
          IF (FI .LT. 0.) FI = FI +2.*PI
         TH  = ATAN2(SQRT(X1B(K)**2+X2B(K)**2),X3B(K))
          IF (TH .LT. 0.) TH = TH +2.*PI
         RW (ICUCL + JECLR4 + K) = R
         RW (ICUCL + JECLT4 + K) = TH
         RW (ICUCL + JECLF4 + K) = FI
   30    CONTINUE
   20  CONTINUE
C
      RETURN
      END
#endif
