      SUBROUTINE LESTIM(IADDR,ECLUS,ENE4,ENE9,R)
C-----------------------------------------------------------------------
C! Find shower shape estimators
C!
C!   Author   : P. H. Hansen   890101
C!   Input    : ECLUS          Cluster energy
C!              ENE4           Energy of 4 towers closest to barycenter
C!              ENE9           Energy of tower nonet
C!              IADDR          Address of central tower
C!   Output   : R(1)           (a-a(expect))/da       (storey centers)
C!              R(2)           (b-b(expect))/db       (storey centers)
C!              R(3)           (a-a(expect))/da       (wire planes)
C!              R(4)           (b-b(expect))/db       (wire planes)
C!                             where a = (<s**2>-<s>**2)/<s>**2
C!                             and   b = 1./<s>
C!              R(5)           (E4/E-E4/E(expect))/d(E4/E)
C!              R(6)           (E9/E-E9/E(expect))/d(E9/E)
C!
C!   Modified : P.H. Hansen, P. Comas (29-SEP-1994)
C!             Protect SAV, SAV2 to avoid division by zero in rare
C!             coincidences for low energy clusters
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "lcnamc.h"
#include "lpdajj.h"
#include "lcrejj.h"
      DIMENSION ENE4(*),ENE9(*),R(*)
#include "bmacro.h"
#include "lcmacr.h"
C----------------------------------------------------------------------
C
      CALL VZERO(R,6)
      KLCRE = IW(NALCRE)
      MODU = LCMOD(IADDR)
      IF(MODU.LE.0.OR.MODU.GE.5)        GOTO 999
      KLPDA = IW(NALPDA)
      EWIR = RTABL(KLPDA,MODU,JLPDES)
      IF(EWIR.LT.0.5)                   GOTO 999
C
C ** Transverse shape **
C
      E4 = ENE4(1)+ENE4(2)+ENE4(3)
      IF(E4.LT.0.5)                     GOTO 999
      E9 = ENE9(1)+ENE9(2)+ENE9(3)
      R(5) = (E4/ECLUS-RTABL(KLCRE,1,JLCRE4))/RTABL(KLCRE,1,JLCRD4)
      R(6) = (E9/ECLUS-RTABL(KLCRE,1,JLCRE9))/RTABL(KLCRE,1,JLCRD9)
C
C ** Longitudinal shape **
C
C Expected values (see J.Badier and M Bardadin  ALEPH 87-9)
      ABYB = 5.10 + 1.02*ALOG(E4)
      BINV = 1.86 + 0.02*ALOG(E4)
      EB = 1./ABYB
      EA = BINV/ABYB
C
C Average fluctuations
      DB = SQRT(0.010+0.025/E4)*EB
      DA = SQRT(0.025+0.085/E4)*EA
C
C Measured values using storeys
      SAV = (ENE4(1)*RTABL(KLCRE,1,JLCRS1)+
     &       ENE4(2)*RTABL(KLCRE,1,JLCRS2)+
     &       ENE4(3)*RTABL(KLCRE,1,JLCRS3))/E4
      SAV2= (ENE4(1)*RTABL(KLCRE,1,JLCRS1)**2+
     &       ENE4(2)*RTABL(KLCRE,1,JLCRS2)**2+
     &       ENE4(3)*RTABL(KLCRE,1,JLCRS3)**2)/E4
      SAV = AMAX1(SAV,0.5)
      SAV2 = AMAX1(SAV2,0.25)
      A = (SAV2-SAV**2)/SAV**2
      B = 1./SAV
      R(1) = (A-EA)/DA
      R(2) = (B-EB)/DB
C
C Measured values using wires
      SAV  = 0.
      SAV2 = 0.
      RL = 1.
      ETOT  = 0.
      IF(KLPDA.GT.0)  THEN
         DO 11 IPL=1,38
            IF(IPL.LE.29) RL = RL+0.5
            IF(IPL.GE.30) RL = RL+1.
            E = RTABL(KLPDA,MODU,JLPDEN+IPL-1)
            SAV  = SAV  + E*RL
            SAV2 = SAV2 + E*RL*RL
            ETOT = ETOT + E
  11     CONTINUE
         SAV = SAV/AMAX1(ETOT,0.1)
         SAV = AMAX1(SAV,0.5)
         SAV2 = SAV2/AMAX1(ETOT,0.1)
         SAV2 = AMAX1(SAV2,0.5)
         A = (SAV2-SAV**2)/SAV**2
         B = 1./SAV
         R(3) = (A-EA)/DA
         R(4) = (B-EB)/DB
      ENDIF
C
  999 CONTINUE
      END
#endif
