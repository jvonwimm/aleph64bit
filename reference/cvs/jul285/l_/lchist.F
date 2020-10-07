      SUBROUTINE LCHIST
C-----------------------------------------------------------------------
C! Fill LCAL histograms
C!
C!   Author   : P. H. Hansen  880307
C!   Modified : P. H. Hansen  890102
C!   Modified : H. Meinhard   05-Mar-1990
C!   Modified : P. H. Hansen  960501
C!
C-----------------------------------------------------------------------
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "rflags.h"
#include "rcurnt.h"
#include "rconds.h"
#include "lcnamc.h"
#include "lobjjj.h"
#include "lsdajj.h"
#include "lpdajj.h"
#include "bhabjj.h"
#include "sanamc.h"
#include "sftrjj.h"
#include "srecpc.h"
#include "lifljj.h"
#include "lclujj.h"
#include "lcrejj.h"
#include "alcons.h"
C
C Parameters for selection of calibration events
      PARAMETER( CTMIN=0.060,CTMAX=0.125,CCMIN=.10,
     &           CEMIN=.3,CDMAX=0.15 )
C
C CTMIN : min theta
C CTMAX : max theta
C CCMIN : min cos(phi)
C CEMIN : min energy
C CDMAX : max acollinearity
C
      DIMENSION EPAD(4),P1(3),P2(3)
      EXTERNAL VDOT
      LOGICAL BTEST
#include "bmacro.h"
#include "lcmacr.h"
C---------------------------------------------------------------------
      LHID = JULLC*1000
      KLCLU = IW(NALCLU)
      IF(KLCLU.LE.0)                            GOTO 999
      NCLU = LROWS(KLCLU)
      KLCRE = IW(NALCRE)
C
C? Histogram events with Lcal clusters for general monitoring
C
C Cathode data
      CALL VZERO(EPAD,4)
      EST1 = 0.
      EST2 = 0.
      EST3 = 0.
      KLSDA = IW(NALSDA)
      IF(KLSDA.GT.0) THEN
         DO 10 I=1,LROWS(KLSDA)
            MODU = LCMOD(ITABL(KLSDA,I,JLSDAD))
            E1 = RTABL(KLSDA,I,JLSDEN)
            E2 = RTABL(KLSDA,I,JLSDEN+1)
            E3 = RTABL(KLSDA,I,JLSDEN+2)
            EPAD(MODU) = EPAD(MODU) + E1+E2+E3
            EST1 = EST1 + E1
            EST2 = EST2 + E2
            EST3 = EST3 + E3
   10    CONTINUE
      ENDIF
      ETOT = EPAD(1)+EPAD(2)+EPAD(3)+EPAD(4)
      CALL HF1(LHID+1,ETOT,1.)
      CALL HF1(LHID+4,EST1/AMAX1(ETOT,.1),1.)
      CALL HF1(LHID+5,EST3/AMAX1(ETOT,.1),1.)
C
C Cluster data
      DO 11 I=1,NCLU
         ECLUS = RTABL(KLCLU,I,JLCLE4)
         CALL HF1(LHID+2,ECLUS,1.)
   11 CONTINUE
C
C Wire data
      EWIR = 0.
      KLPDA = IW(NALPDA)
      IF(KLPDA.GT.0) THEN
         DO 21 I=1,LROWS(KLPDA)
            EWIR = EWIR + RTABL(KLPDA,I,JLPDES)
   21    CONTINUE
         CALL HF1(LHID+3,EWIR,1.)
      ENDIF
C
C Find Bhabha event candidates
C
      KLOBJ = IW(NALOBJ)
      IF(KLOBJ.LE.0)                                 GOTO 999
      NOBJ = LROWS(KLOBJ)
      IF(NOBJ.LE.0)                                  GOTO 999
      IA = 0
      IB = 0
      EA = 0.
      EB = 0.
C
C Find two largest clusters on A and B side
      DO 31 I=1,NOBJ
        EC = RTABL(KLOBJ,I,JLOBEC)
        IF(RTABL(KLOBJ,I,JLOBTC).LT.PIBY2) THEN
           IF(EC.GT.EA) THEN
              EA = EC
              IA = I
           ENDIF
        ELSE
           IF(EC.GT.EB) THEN
              EB = EC
              IB = I
           ENDIF
        ENDIF
   31 CONTINUE
C
C Monitor single cluster triggers
      ELOW=CEMIN*ENERRC/2.
      IF((EA.GT.ELOW.AND.EB.LT.1.).OR.
     &   (EB.GT.ELOW.AND.EA.LT.1.)) THEN
        DO 32 I=1,NOBJ
          EC = RTABL(KLOBJ,I,JLOBEC)
          IF(EC.LT.ELOW) GOTO 32
          THS = MIN(RTABL(KLOBJ,I,JLOBTC),
     &            PI-RTABL(KLOBJ,I,JLOBTC))*1000.
          PHS = RTABL(KLOBJ,I,JLOBPC)*RADEG
          IF(EA.GT.ELOW) CALL HF1(LHID+6,PHS,1.)
          IF(EB.GT.ELOW) CALL HF1(LHID+7,PHS,1.)
          CALL HF1(LHID+9,THS,1.)
          CALL HF1(LHID+8,EC,1.)
   32   CONTINUE
        GOTO 999
      ELSE
        IF(IA.EQ.0.AND.IB.EQ.0)                      GOTO 999
      ENDIF
C
      I1 = IA
      I2 = IB
      IF(EB.GT.EA) THEN
        I1 = IB
        I2 = IA
      ENDIF
C
C? Histogram default Bhabha selection for luminosity checks
C
      KBHAB = IW(NABHAB)
      IF(KBHAB.LE.0)                                 GOTO 999
      IF(LROWS(KBHAB).LT.10)                         GOTO 999
      KLIFL = IW(NALIFL)
      IF(KLIFL.LE.0)                                 GOTO 999
      K2 = ITABL(KLIFL,I2,JLIFK2)
      K1 = ITABL(KLIFL,I1,JLIFK1)
      KL = ITABL(KLIFL,I2,JLIFKL)
      KH = ITABL(KLIFL,I1,JLIFKH)
      IF(.NOT.BTEST(KH,9).OR..NOT.BTEST(K1,9).OR.
     &   .NOT.BTEST(KL,9).OR..NOT.BTEST(K2,9))       GOTO 999
C
C Get parameters for event selection
      ZREF = RTABL(KLCRE,1,JLCRZC)
      E1  = RTABL(KLOBJ,I1,JLOBEC)
      E2  = RTABL(KLOBJ,I2,JLOBEC)
      TH1 = RTABL(KLOBJ,I1,JLOBTC)
      PH1 = RTABL(KLOBJ,I1,JLOBPC)
      TH2 = RTABL(KLOBJ,I2,JLOBTC)
      PH2 = RTABL(KLOBJ,I2,JLOBPC)
      P1(1) = E1*COS(PH1)*SIN(TH1)
      P1(2) = E1*SIN(PH1)*SIN(TH1)
      P1(3) = E1*COS(TH1)
      P2(1) = E2*COS(PH2)*SIN(TH2)
      P2(2) = E2*SIN(PH2)*SIN(TH2)
      P2(3) = E2*COS(TH2)
      DPHI = ABS(PH1-PH2)
      DPHI = AMIN1(DPHI,TWOPI-DPHI)
      P12  = VDOT(P1(1),P2(1),3)
      CPSI = AMAX1(P12/E1/E2,-1.)
      DPSI = PI-ACOS(CPSI)
      SIG = 1.
      IF(EB.GT.EA) SIG = -1.
      X1 = SIG*ZREF*COS(PH1)*TAN(TH1)
      X2 = -SIG*ZREF*COS(PH2)*TAN(TH2)
      Y1 = SIG*ZREF*SIN(PH1)*TAN(TH1)
      Y2 = -SIG*ZREF*SIN(PH2)*TAN(TH2)
      IF(TH1.GT.PIBY2) TH1=PI-TH1
      IF(TH2.GT.PIBY2) TH2=PI-TH2
C
C Plot Delta phi
      CALL HF1(LHID+43,DPHI*RADEG,1.)
      IF(DPHI.LT.RTABL(KBHAB,10,JBHADP))            GOTO 999
C
C Plot energy per module
      CALL HF1(LHID+41,E1,1.)
      CALL HF1(LHID+42,E2,1.)
      MOD1 = ITABL(KLCLU,I1,JLCLMO)
      CALL HF1(LHID+10+MOD1,EPAD(MOD1),1.)
      MOD2 = ITABL(KLCLU,I2,JLCLMO)
      CALL HF1(LHID+10+MOD2,EPAD(MOD2),1.)
      DO 40 I=1,LROWS(KLPDA)
         IF(ITABL(KLPDA,I,JLPDMO).EQ.MOD1) IP1=I
         IF(ITABL(KLPDA,I,JLPDMO).EQ.MOD2) IP2=I
   40 CONTINUE
      DO 41 IP=1,29
         RL = 1.5 + FLOAT(IP-1)*0.5
         E = RTABL(KLPDA,IP1,1+IP)
         CALL HF1(LHID+14+MOD1,RL,E)
         E = RTABL(KLPDA,IP2,1+IP)
         CALL HF1(LHID+14+MOD2,RL,E)
   41 CONTINUE
      DO 42 IP=30,38
         RL = 15.5 + FLOAT(IP-30)
         E = RTABL(KLPDA,IP1,1+IP)/2.
         CALL HF1(LHID+14+MOD1,RL+0.5,E)
         CALL HF1(LHID+14+MOD1,RL+1.,E)
         E = RTABL(KLPDA,IP2,1+IP)/2.
         CALL HF1(LHID+14+MOD2,RL+0.5,E)
         CALL HF1(LHID+14+MOD2,RL+1.,E)
   42 CONTINUE
C
C Plot average X and Y of vertex and cluster
      XAV = (X1+X2)*0.5
      YAV = (Y1+Y2)*0.5
      CALL HFILL(LHID+31,XAV,0.,1.)
      CALL HFILL(LHID+32,YAV,0.,1.)
      CALL HF2(LHID+44,ABS(X1),ABS(Y1),1.)
C
  999 CONTINUE
      END
#endif
