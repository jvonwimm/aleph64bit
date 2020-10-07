      SUBROUTINE HCLUFI(NHCLU)
C------------------------------------------------------------------
C
C! fill HCLU bank
C
C!     G.Capon               date : 861001
C!     input : NHCLU = number of Hcal clusters
C!     called by HCFCLU
C
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "hcnamc.h"
#include "hclujj.h"
#include "hsdajj.h"
#include "hstojj.h"
#include "hcjosu.h"
#include "alcons.h"
#include "rlunit.h"
#include "rcurnt.h"
#include "rflags.h"
#include "hdebug.h"
#include "hwbnkw.h"
      REAL SUMT(2),SUMT1(2),SUMT2(2),SUMP(2),SUMP1(2),SUMP2(2)
      REAL SUMR(2),SUMR1(2)
C
C?              create HCLU bank
C
      LNCLU=2+NHCLU*LHCLUA
      CALL AUBOS('HCLU',0,LNCLU,JHCLU,IGARB)
      IF (IGARB.EQ.2) THEN
         CALL RERROR('HCLUFI',1,'no space for HCLU bank')
         RETURN
      ENDIF
      KHCLU=JHCLU+2
C
C?              loop on clusters and fill bank
C
      JHSDA=IW(NAHSDA)
      JHSTO=IW(NAHSTO)
      JHCSR=IW(NAHCSR)
      KHCSR=JHCSR
      IW(JHCLU+1)=LHCLUA
      IW(JHCLU+2)=NHCLU
      FIRED=IW(JHSDA+2)
      IF (JHISRF(JULHC).GE.1) CALL HFILL(7001,FIRED,0.,1.)
      IF (JHISRF(JULHC).GE.1) CALL HFILL(7004,FLOAT(NHCLU),0.,1.)
      ETOHC=0.
      DO 10 NC=1,NHCLU
         NSTOC=IW(KHCSR+1)
         FNSTO=NSTOC
         IF (JHISRF(JULHC).GE.1) CALL HFILL(7005,FNSTO,0.,1.)
         NS1=IW(KHCSR+2)
         IW(KHCLU+JHCLFS)=NS1
         IOVRL=0
         DO 9 KS=1,2
         SUMT(KS) = 0.
         SUMP(KS) = 0.
         SUMR(KS) = 0.
         SUMT1(KS) = 0.
         SUMP1(KS) = 0.
         SUMR1(KS) = 0.
         SUMT2(KS) = 0.
         SUMP2(KS) = 0.
 9       CONTINUE
C
C?              loop on storeys of current cluster
C
         DO 11 IS=1,NSTOC
            NS=IW(KHCSR+1+IS)
            KHSDA=JHSDA+2+LHSDAA*(NS-1)
            KHSTO=JHSTO+2+LHSTOA*(NS-1)
            EGEV=RW(KHSDA+JHSDDE)
            IF (JHISRF(JULHC).GE.1) CALL HFILL(7002,EGEV,0.,1.)
            ETOHC=ETOHC+EGEV
            KS=IW(KHSDA+JHSDSN)
            RW(KHCLU+JHCLEC)=RW(KHCLU+JHCLEC)+EGEV
            RW(KHCLU+JHCLEC+KS)=RW(KHCLU+JHCLEC+KS)+EGEV
            CALCN=1.
            IF(ICORHW.GT.0) CALCN=RW(ICORHW+NS)
            ERAW=EGEV
            IF(CALCN.GT.0.) ERAW=EGEV/CALCN
            RW(KHCLU+JHCLER)=RW(KHCLU+JHCLER)+ERAW
C             !!  Jhclxx+ks ; do not change jhclx1,jhclx2 parameters
            IT=IW(KHSDA+JHSDTI)
            IF (IT.GE.14 .AND. IT.LE.16) IOVRL=1
            IF (IT.GE.47. AND. IT.LE.49) IOVRL=1
            JF=IW(KHSDA+JHSDPI)
            NR=IW(KHSDA+JHSDRN)
            RHO=RW(KHSTO+JHSTSX)*RW(KHSTO+JHSTSX)+RW(KHSTO+JHSTSY)*
     +      RW(KHSTO+JHSTSY)+RW(KHSTO+JHSTSZ)*RW(KHSTO+JHSTSZ)
            RHO=SQRT(RHO)
            SUMR(KS) = SUMR(KS) + EGEV
            SUMR1(KS) = SUMR1(KS) +EGEV*RHO
            CALL HUSRAN(IT,JF,KS,TETA,PHI,DTETA,DPHI)
            SUMT(KS) = SUMT(KS) + EGEV*DTETA
            SUMT1(KS) = SUMT1(KS) + EGEV*TETA*DTETA
            TT1=TETA-0.5*DTETA
            TT2=TETA+0.5*DTETA
C              Dteta is a projected value !!
            SUMT2(KS) = SUMT2(KS) + EGEV*(TT2*TT2*TT2-TT1*TT1*TT1)/3.
            IF(IS.EQ.1) THEN
               PHI1=PHI
            ELSE
               PHI=PROXIM(PHI,PHI1)
            ENDIF
            SUMP(KS) = SUMP(KS) + EGEV*DPHI
            SUMP1(KS) = SUMP1(KS) + EGEV*PHI*DPHI
            PP1=PHI-0.5*DPHI
            PP2=PHI+0.5*DPHI
            SUMP2(KS) = SUMP2(KS) + EGEV*(PP2*PP2*PP2-PP1*PP1*PP1)/3.
   11    CONTINUE
C
C               compute weighted theta,phi,rho for cluster
C
      IW(KHCLU+JHCLKD) = IOVRL
      TETAV = (SUMT1(1) + SUMT1(2))/(SUMT(1)+SUMT(2))
      PHIAV = (SUMP1(1) + SUMP1(2))/(SUMP(1)+SUMP(2))
      RW(KHCLU+JHCLRC) = (SUMR1(1)+SUMR1(2))/(SUMR(1)+SUMR(2))
      RW(KHCLU+JHCLTC) = TETAV
      RW(KHCLU+JHCLPC) = PROXIM(PHIAV,0.)
      TETWD=(SUMT2(1)+SUMT2(2))/(SUMT(1)+SUMT(2))-TETAV*TETAV
      PHIWD=(SUMP2(1)+SUMP2(2))/(SUMP(1)+SUMP(2))-PHIAV*PHIAV
      RW(KHCLU+JHCLSC) = SQRT(TETWD + PHIWD)
C
      DO 12 KS=1,2
      IF(SUMR(KS).GT.0.) THEN
         RW(KHCLU+JHCLRC+KS) = SUMR1(KS)/SUMR(KS)
         TETAV=SUMT1(KS)/SUMT(KS)
         RW(KHCLU+JHCLTC+KS)=TETAV
         PHIAV=SUMP1(KS)/SUMP(KS)
         RW(KHCLU+JHCLPC+KS)=PROXIM(PHIAV,0.)
         TETWD=SUMT2(KS)/SUMT(KS)-TETAV*TETAV
         PHIWD=SUMP2(KS)/SUMP(KS)-PHIAV*PHIAV
         RW(KHCLU+JHCLSC+KS)=SQRT(TETWD+PHIWD)
      ENDIF
 12   CONTINUE
         IF (JHISRF(JULHC).GE.1) CALL HFILL(7006,RW(KHCLU+JHCLEC),0.,1.)
         KHCLU=KHCLU+LHCLUA
         KHCSR=KHCSR+1+NSTOC
   10 CONTINUE
C               drop work bank and HCSR bank
      NCLUHC=NCLUHC+NHCLU
      IND = NDROP('HCSR',0)
      CALL WDROP(IW,ICORHW)
      IF (JHISRF(JULHC).GE.1) CALL HFILL(7003,ETOHC,0.,1.)
      IF(IHPRIN.GE.1)WRITE(LDEBRL,77)ETOHC
 77   FORMAT(' TOTAL ENERGY IN HCAL = ',F5.1,' GEV')
      RETURN
      END
#endif
