      SUBROUTINE LCALIB
C-----------------------------------------------------------------------
C! Get the energy calibrations
C!
C!   Author   : F. Bird     890711
C!   Modified : P.H. Hansen 892111
C!   Modified : P.H. Hansen 960501 Method 10 selection cuts
C!   Input    : LIDT LCLU LSDA LPDA
C!   Output   : LEHI: col 1 2 ....... NBINS NBINS+1 NBINS+2
C!                    row
C!                      1 Estoreys module 1  BINSI   ELOW
C!                      2 Ewires   module 1  BINSI   ELOW
C!                      .
C!                      .
C!                      7 Estoreys module 4  BINSI   ELOW
C!                      8 Ewires   module 4  BINSI   ELOW
C!              LVHI: col 1 2 ........NBINX NBINX+1 NBINX+2
C!                    row
C!                      1 (X1+X2)/2 modu 1+4 BINSX   XLOW
C!                      2 (Y1+Y2)/2 modu 1+4 BINSX   XLOW
C!                      3 (X1+X2)/2 modu 2+3 BINSX   XLOW
C!                      4 (Y1+Y2)/2 modu 2+3 BINSX   XLOW
C!
C!
C!   Description :
C!   ===========
C?   Select fully contained Bhabha events
C?   Histogram the cluster energies in LEHI
C?   Histogram the vertex positions in LVHI
C!
C-----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "lcnamc.h"
#include "lsdajj.h"
#include "lpdajj.h"
#include "lphist.h"
#include "lehijj.h"
#include "lvhijj.h"
#include "lidtjj.h"
#include "lifljj.h"
#include "alcons.h"
#include "rconds.h"
C
C Parameters for selection of calibration events
      PARAMETER( CTMIN=0.060,CTMAX=0.120,CCMIN=2.793,
     &           CEMIN=0.5,CDMAX=0.10 )
C
C CTMIN : min theta
C CTMAX : max theta
C CCMIN : min dphi
C CEMIN : min energy
C CDMAX : max acollinearity
C
      DIMENSION P1(3),P2(3),ICL(2),E(2),TH(2),PH(2),M(2),X(2),Y(2)
      LOGICAL KEEP,BTEST
      EXTERNAL VDOT
#include "bmacro.h"
#include "lcmacr.h"
C---------------------------------------------------------------------
C
C
      KLEHI = IW(NALEHI)
      IF(KLEHI.LE.0)                                 GOTO 999
      KLVHI = IW(NALVHI)
      IF(KLVHI.LE.0)                                 GOTO 999
      KLIDT = IW(NALIDT)
      IF(KLIDT.LE.0)                                 GOTO 999
      NIDT = LROWS(KLIDT)
      IF(NIDT.LE.0)                                  GOTO 999
      KLSDA = IW(NALSDA)
      KLPDA = IW(NALPDA)
      IF(KLSDA.LE.0.OR.KLPDA.LE.0)                   GOTO 999
      EBEAM = ENERRC/2.
      ELO=FLOAT(INT(EBEAM))-10.
C
C Find two largest clusters on A and B side
      IA = 0
      IB = 0
      EA = CEMIN
      EB = CEMIN
      DO 31 I=1,NIDT
        EC = RTABL(KLIDT,I,JLIDEC)
        IF(RTABL(KLIDT,I,JLIDTC).LT.PIBY2) THEN
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
      IF(IA.EQ.0.OR.IB.EQ.0)                          GOTO 999
      ICL(1) = IA
      ICL(2) = IB
C
C Make an event selection (METHOD 10)
      KEEP = .TRUE.
      KLIFL = IW(NALIFL)
      IF(KLIFL.LE.0)                                 GOTO 999
      IF(EA.GT.EB) THEN
        K1 = ITABL(KLIFL,IA,JLIFK1)
        K2 = ITABL(KLIFL,IB,JLIFK2)
        KH = ITABL(KLIFL,IA,JLIFKH)
        KL = ITABL(KLIFL,IB,JLIFKL)
      ELSE
        K1 = ITABL(KLIFL,IB,JLIFK1)
        K2 = ITABL(KLIFL,IA,JLIFK2)
        KH = ITABL(KLIFL,IB,JLIFKH)
        KL = ITABL(KLIFL,IA,JLIFKL)
      ENDIF
      IF(.NOT.BTEST(K1,9).OR..NOT.BTEST(K2,9).OR.
     &   .NOT.BTEST(KH,9).OR..NOT.BTEST(KL,9)) KEEP = .FALSE.
      DO 40 J=1,2
C
C Energy, Theta, Phi and module number
        E(J)  = RTABL(KLIDT,ICL(J),JLIDEC)
        TH(J) = RTABL(KLIDT,ICL(J),JLIDTC)
        PH(J) = RTABL(KLIDT,ICL(J),JLIDPC)
        M(J) = 1
        IF(COS(PH(J)).GT.0.) M(J) = 2
        IF(TH(J).LT.PIBY2) THEN
          M(J) = M(J)+2
          SIG = 1.
        ELSE
          SIG =-1.
        ENDIF
C
C Impact corrected for the magnetic field
        X(J) = SIG*280.*TAN(TH(J))*COS(PH(J)-SIG*0.014)
        Y(J) = SIG*280.*TAN(TH(J))*SIN(PH(J)-SIG*0.014)
   40 CONTINUE
      DPHI = ABS(PH(1)-PH(2))
      DPHI = AMIN1(DPHI,TWOPI-DPHI)
      IF(DPHI.LT.CCMIN) KEEP = .FALSE.
      IF(.NOT.KEEP)                                   GOTO 999
C
C Fill histograms
C
      DO 60 J=1,2
C
C Cluster energy for this module
        KLS = KROW(KLEHI,2*(M(J)-1)+1)
        ESTOR = E(J)
C
C Wire energy for this module
        KLW = KROW(KLEHI,2*(M(J)-1)+2)
        EWIRE=0.
        DO 50 IPL=1,38
          EWIRE = EWIRE + RTABL(KLPDA,M(J),JLPDEN+IPL-1)
   50   CONTINUE
C
C Fill energy histograms
        IBIN = INT((ESTOR-ELO)/BINSI)+1
        IF(IBIN.GE.1.AND.IBIN.LE.NBINS)
     &   RW(KLS+IBIN) = RW(KLS+IBIN) + 1.
        IBIN = INT((EWIRE-ELO)/BINSI)+1
        IF(IBIN.GE.1.AND.IBIN.LE.NBINS)
     &   RW(KLW+IBIN) = RW(KLW+IBIN) + 1.
C
C Average X for this module
        MM = M(J)
        IF(M(J).EQ.4) MM=1
        IF(M(J).EQ.3) MM=2
        KLX = KROW(KLVHI,2*(MM-1)+1)
        XAV = 0.5*(X(1)+X(2))
C
C Average Y for this module
        KLY = KROW(KLVHI,2*(MM-1)+2)
        YAV = 0.5*(Y(1)+Y(2))
C
C Fill vertex histograms
        IBIN = INT((XAV-XLOW)/BINSX)+1
        IF(IBIN.GE.1.AND.IBIN.LE.NBINX)
     &   RW(KLX+IBIN) = RW(KLX+IBIN) + 1.
        IBIN = INT((YAV-XLOW)/BINSX)+1
        IF(IBIN.GE.1.AND.IBIN.LE.NBINX)
     &   RW(KLY+IBIN) = RW(KLY+IBIN) + 1.
   60 CONTINUE
C
  999 CONTINUE
      END
#endif
