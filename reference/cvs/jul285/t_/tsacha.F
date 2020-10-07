      SUBROUTINE TSACHA(IRMIN,IRMAX,NCROW,JCROW)
C!--------------------------------------------------
C!  Find CHAINS from coordinates in the current
C!  cluster.
C?
C!======================================================================
#ifndef DOC
C
#include "bcs.h"
#include "alcons.h"
#include "tpcojj.h"
#include "tbcojj.h"
#include "tchajj.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpccon.h"
#include "trfdct.h"
#include "tsawrk.h"
#include "tchain.h"
#include "tpcirc.h"
#include "tplinc.h"
C
      DIMENSION NCROW(21),JCROW(20,21)
      DIMENSION IREF(20),MREF(20),JREF(20)
C   The following are needed for the Fast Helix Fitting Routine:
C
      PARAMETER (MPT = 40)
      DIMENSION RCIRCL(MPT),PHCIRC(MPT),ERRMAT(15)
      DOUBLE PRECISION XFIT(MPT),YFIT(MPT),WFIT(MPT)
      DIMENSION HELPAR(5)
C
      LOGICAL LFRST
      SAVE NTPCO,NTBCO
      DATA NDCHA/20/,NDTCT/100/
      DATA CHCUT/10./
      DATA DPCUT/0.15/, DZCUT/1.8/
      DATA TD0CT/38./,TZ0CT/60./
      DATA LFRST/.TRUE./
      DATA IOPT /2/
C
#include "bmacro.h"
C
      IF (LFRST) THEN
        LFRST=.FALSE.
        NTPCO=NAMIND('TPCO')
        NTBCO=NAMIND('TBCO')
      ENDIF
C
      KTPCO = IW(NTPCO)
      KTBCO = IW(NTBCO)
C
C Some work to be done to try to resolve ambiguities
C
      DO 5 IR=IRMIN,IRMAX
C
C  If we have only one point on this padrow, we don't have to do anythin
         IF (NCROW(IR).LE.1) GO TO 5
         NREF = 0
         CALL VZERO(JREF,20)
         DO 6 IH = 1,NCROW(IR)
C Find number of TPCO hits refenced on this padrow
            IC = ITABL(JTNWCO,JCROW(IH,IR),JTNWIC)
            IFND = 0
            DO 66 I=1,NREF
               IF(IREF(I).EQ.IC) THEN
                  IFND = I
                  MREF(I) = MREF(I) + 1
               ENDIF
   66       CONTINUE
            IF (IFND.EQ.0) THEN
               NREF = NREF + 1
C TPCO/TBCO ref
               IREF(NREF) = IC
C No of JTNWCO fits refering to this TPCO/TBCO
               MREF(NREF) = 1
C First entry in JTNWCO refering to this TPCO/TBCO
C (Rely on the fact that the fits for a given coordinate are consecutive
               JREF(NREF) = IH
            ENDIF
    6    CONTINUE
C-----------------------------------------------------------------------
C Resolve ambiguities on this row (ie keep only one fit/coord)
C Remember:
C  NREF is the number of distinct TPCO/TBCO coords on current row
C  MREF(nref) = No of wire fits obtained (ie JTNWCO entries)
C               for each TPCO/TBCO entry
C  JREF(nref) = JTNWCO index of first wire fit (NB fits are consecutive)
C  IREF(nref) = TPCO/TBCO pointer
C-----------------------------------------------------------------------
C
         DO 4 JR = 1,NREF
C If there is only one reference to this coordinate, accept fit
            IF (MREF(JR).EQ.1) GO TO 4
C
C Here we have more than one candidate for a TPCO/TBCO entry,
C Delete the one furthest from original z.
C
            IH = JREF(JR)
            JN = JCROW(IH,IR)
            IC = IREF(JR)
            IF (IC.GT.0) THEN
               ZCO = RTABL(KTPCO,IC,JTPCZV)
            ELSE
               ZCO = RTABL(KTBCO,-IC,JTBCZV)
            ENDIF
            ZDF1 = ABS(ZCO-RTABL(JTNWCO,JN  ,JTNWZZ))
            ZDF2 = ABS(ZCO-RTABL(JTNWCO,JN+1,JTNWZZ))
            IF (ZDF1.GT.ZDF2) THEN
               IW(KROW(JTNWCO,JN)+JTNWZO) = -1
            ELSE
               IW(KROW(JTNWCO,JN+1)+JTNWZO) = -1
            ENDIF
    4    CONTINUE
C
    5 CONTINUE
C
C  Now we look for tracks
C
      NHCHAI = 0
      CALL VZERO(IRCHAI,MXHTCH)
      CALL VZERO(MCHAIN,MXHTCH)
C
      IRST = IRMAX + 1
C
C  Start search from new outer padrow   <===================
C  (We need at least 3 padrows)
    1 IRST = IRST - 1
      IF (IRST.LT.IRMIN+2) GO TO 999
C  Select hit from outer row and search inner rows in succession
C  for hits matching z and dz/dr within phi range
      DO 30 IH1 = 1,NCROW(IRST)
C  Skip marked hits
         I1 = JCROW(IH1,IRST)
         IF (ITABL(JTNWCO,I1,JTNWZO).NE.0) GO TO 30
C  We have found a hit to follow. Get its slope and phi value.
         SLOP1 = RTABL(JTNWCO,I1,JTNWTG)
         DSQ1 = RTABL(JTNWCO,I1,JTNWD2)
         DTQ1 = RTABL(JTNWCO,I1,JTNWG2)
         PHI1 = RTABL(JTNWCO,I1,JTNWPH)
         NHCHAI = NHCHAI + 1
         IRCHAI(NHCHAI) = IRST
         IHCHAI(NHCHAI) = I1
         D2CHAI(NHCHAI) = RTABL(JTNWCO,I1,JTNWS2)
         SLSUM = SLOP1
         SLSIG = DSQ1
         IC = ITABL(JTNWCO,I1,JTNWIC)
         IF (IC.GT.0) THEN
            IRS = ITABL(KTPCO,IC,JTPCIN)
         ELSE
            IRS = ITABL(KTBCO,-IC,JTBCIN)
         ENDIF
         ISEC = MOD(IRS/1000,100)
C
C  Look for points on the inner padrows                     <==========
         ISEAR = IRST
    2    ISEAR = ISEAR - 1
         IF(ISEAR.LT.IRMIN) GO TO 35
         CHMIN = 999999.
         IBEST = 0
         DO 32 IH2 = 1,NCROW(ISEAR)
            I2 = JCROW(IH2,ISEAR)
            IF (ITABL(JTNWCO,I2,JTNWZO).NE.0) GO TO 32
C If we have three or more points do a consistency test for z
            IF(NHCHAI.GT.2) THEN
               ZPR = Z3 - (R3 - RTABL(JTNWCO,I2,JTNWRR))*SL32
               DZ = ABS(ZPR-RTABL(JTNWCO,I2,JTNWZZ))
               IF (DZ.GT.DZCUT) GO TO 32
            ELSE
               DZ = 0.
            ENDIF
            SLOP2 = RTABL(JTNWCO,I2,JTNWTG)
            DSQ2 = RTABL(JTNWCO,I2,JTNWD2)
            DTQ2 = RTABL(JTNWCO,I2,JTNWNN)
            PHI2 = RTABL(JTNWCO,I2,JTNWPH)
            DPHI = TRPHDI(PHI2,PHI1)
C  Place a limit on the turning angle
            IF (ABS(DPHI).GT.DPCUT) GO TO 32
C
C  "Chisq" test based on comparison of slopes and deviation from
C   predicted z-value
C
            CHI2 = (SLOP2 - SLOP1)**2/(DSQ1 + DSQ2)
     &         + 0.1* DZ**2/((DTQ1 + DTQ2)*TVDRFT(IENDTP(ISEC))**2)
            IF (CHI2.GT.CHCUT) GO TO 32
C  Create road point when we get the first acceptable hit
            IF (IBEST.EQ.0) NHCHAI = NHCHAI+1
C  Save the hit with the best match
            IF (CHI2.LT.CHMIN) THEN
               IRCHAI(NHCHAI) = ISEAR
               IHCHAI(NHCHAI) = I2
               D2CHAI(NHCHAI) = RTABL(JTNWCO,I2,JTNWS2)
               CHMIN = CHI2
               IBEST = I2
            ENDIF
   32    CONTINUE
C
C  Update slope running average and phi with last value
         IF (IBEST.GT.0) THEN
            SLSUM = SLSUM + RTABL(JTNWCO,IBEST,JTNWTG)
            SLOP1 = SLSUM/FLOAT(NHCHAI)
            SLSIG = SLSIG + RTABL(JTNWCO,IBEST,JTNWD2)
            DSQ1 = SLSIG/FLOAT(NHCHAI)
            PHI1 =  RTABL(JTNWCO,IBEST,JTNWPH)
C  Use last two points to predict next Z
            IF (NHCHAI.GE.2) THEN
               I3 = IHCHAI(NHCHAI-1)
               Z3 = RTABL(JTNWCO,I3,JTNWZZ)
               R3 = RTABL(JTNWCO,I3,JTNWRR)
               Z2 = RTABL(JTNWCO,IBEST,JTNWZZ)
               R2 = RTABL(JTNWCO,IBEST,JTNWRR)
               SL32 = (Z3-Z2)/(R3-R2)
            ENDIF
         ENDIF
C
C  Look at next row in    =============================>
         GO TO 2
C  We have now exhausted all rows for this candidate. See what we have.
C  Delete chain if we don't have at least 3 points
C
   35    IF (NHCHAI.LT.3) GO TO 40
C--------------------------------------------------------------
C  Do chain validation and fit here.
C
C      (1) Compute circle parameters from middle and extreme points
C      (2) Apply cut on rho, d0, z0
C      (3) Do usual helix fit
C--------------------------------------------------------------
         IC1 = IHCHAI(NHCHAI)
         IC2 = IHCHAI((NHCHAI+1)/2)
         IC3 = IHCHAI(1)
         RR = RTABL(JTNWCO,IC1,JTNWRR)
         PHI = RTABL(JTNWCO,IC1,JTNWPH)
         X1 = RR*COS(PHI)
         Y1 = RR*SIN(PHI)
         Z1 = RTABL(JTNWCO,IC1,JTNWZZ)
         RR = RTABL(JTNWCO,IC2,JTNWRR)
         PHI = RTABL(JTNWCO,IC2,JTNWPH)
         X2 = RR*COS(PHI)
         Y2 = RR*SIN(PHI)
         Z2 = RTABL(JTNWCO,IC2,JTNWZZ)
         RR = RTABL(JTNWCO,IC3,JTNWRR)
         PHI = RTABL(JTNWCO,IC3,JTNWPH)
         X3 = RR*COS(PHI)
         Y3 = RR*SIN(PHI)
         Z3 = RTABL(JTNWCO,IC3,JTNWZZ)
         A1 = X2-X1
         B1 = Y2-Y1
         C1 = 0.5*(X1*X1-X2*X2+Y1*Y1-Y2*Y2)
         A2 = X3-X2
         B2 = Y3-Y2
         C2 = 0.5*(X2*X2-X3*X3+Y2*Y2-Y3*Y3)
         DET = A1*B2-A2*B1
C
C Centre of track orbit; protect against 3 perfectly aligned points
C
         IF (DET.EQ.0.) GO TO 40
         XC = (B1*C2-B2*C1)/DET
         YC =-(A1*C2-A2*C1)/DET
         RHOSQ = (X1-XC)**2 + (Y1-YC)**2
C
C  Check on radius of curvature
C  (demand larger radius than for normal tracks)
         IF (RHOSQ.LT.4.*RHOMIN**2) GO TO 40
         RHO = SQRT(RHOSQ)
         RC = SQRT(XC*XC + YC*YC)
         X01 = XC*(1. + RHO/RC)
         X02 = XC*(1. - RHO/RC)
C
C Protect against circle centre on Y-axis
C
         IF (ABS(XC).LT.0.00001) THEN
            Y01 = YC*(1. + RHO/RC)
            Y02 = YC*(1. - RHO/RC)
         ELSE
            Y01 = YC*X01/XC
            Y02 = YC*X02/XC
         ENDIF
C
C   Find closest point to origin (D0)
C   D0 is positive if track encloses origin, otherwise negative
C
         D1SQ = X01*X01 + Y01*Y01
         D2SQ = X02*X02 + Y02*Y02
         IF (D1SQ.LT.D2SQ) THEN
            D0=SQRT(D1SQ)
            X0 = X01
            Y0 = Y01
         ELSE
            D0=SQRT(D2SQ)
            X0 = X02
            Y0 = Y02
         ENDIF
C
C  Cut on D0 and Z0 (tighter than for normat tracks)
C
         IF (D0.GT.TD0CT) GO TO 40
         IF (RC.GT.RHO)   D0 = -D0
         CH10 = SQRT((X1-X0)**2 + (Y1-Y0)**2)
         PSI0 = 2.*ASIN(CH10/(2.*RHO))
         CH13 = SQRT((X1-X3)**2+(Y1-Y3)**2)
         ARG = CH13/(2.*RHO)
         IF (ARG.GE.1.0) GO TO 40
         S13 = 2.*RHO*ASIN(ARG)
         TANL = (Z3-Z1)/S13
         Z0 = Z1 - RHO*PSI0*TANL
         IF (ABS(Z0).GT.TZ0CT) GO TO 40
C
C  Find charge (assuming track direction is away from origin)
C  Get rotation angle to bring centre of track circle on x-axis
C
         PHIC = ATAN2(YC,XC)
         CP = COS(PHIC)
         SP = SIN(PHIC)
C
C  All 3 points must be on the same semicircle
C
         Y1R= Y1*CP - X1*SP
         Y2R= Y2*CP - X2*SP
         Y3R= Y3*CP - X3*SP
         IF (Y2R.GE.0.) THEN
            IF (Y1R.LT.0. .OR. Y3R.LT.0.) GO TO 40
            Q = 1.
         ELSE
            IF (Y1R.GT.0. .OR. Y3R.GT.0.) GO TO 40
            Q = -1.
         ENDIF
         PHI0 = PHIC + Q*PIBY2
         IF (PHI0.LT.0.) PHI0 = PHI0 + TWOPI
C
C  Chain fit
C
         ISTCHA = 0
         NCIRCL = 0
         IR1 = 0
         IR2 = 0
C
C  Transfer hits to fitter array in increasing R order
C
         DO 10 IH=NHCHAI,1,-1
C
            NCIRCL = NCIRCL+1
            RCIRCL(NCIRCL) = RTABL(JTNWCO,IHCHAI(IH),JTNWRR)
            PHCIRC(NCIRCL) = RTABL(JTNWCO,IHCHAI(IH),JTNWPH)
            XCIRCL(NCIRCL) = RCIRCL(NCIRCL)*COS(PHCIRC(NCIRCL))
            YCIRCL(NCIRCL) = RCIRCL(NCIRCL)*SIN(PHCIRC(NCIRCL))
            XFIT(NCIRCL) = XCIRCL(NCIRCL)
            YFIT(NCIRCL) = YCIRCL(NCIRCL)
            ZCIRCL(NCIRCL) = RTABL(JTNWCO,IHCHAI(IH),JTNWZZ)
            IF (NCIRCL.EQ.1) THEN
               IR1 = IRCHAI(IH)
            ENDIF
C
            SIGRP = D2CHAI(IH)
            SIGZ =  RTABL(JTNWCO,IHCHAI(IH),JTNWG2)
            IR2 = IRCHAI(IH)
            WFIT(NCIRCL) = 1./SIGRP
            WLINEA(NCIRCL) = 1./SIGZ
   10    CONTINUE
C
C  least sqrs fit w/error mat calc.
C
         CALL TFITHL(NCIRCL,XFIT,YFIT,RCIRCL,PHCIRC,WFIT,
     1               ZCIRCL,WLINEA,IOPT,
     2               HELPAR,ERRMAT,CHIRP,CHISZ)
C
C  Convert the CHI2 values to the ones this routine expects, calculate
C  CHISQ as a check vs. TFITHL failure
C  In case of 3-point circle fit (ie 0 degrees of freedom) use nominal
C  value for CHIRP.
C
         CHITOT = CHIRP + CHISZ
         IF (NCIRCL.LE.3) THEN
            CHIRP = 1.0
         ELSE
            CHIRP=CHIRP/FLOAT(NCIRCL-3)
         ENDIF
         CHISZ=CHISZ/FLOAT(NCIRCL-2)
C
         IF (CHITOT.GT.5000.0) GO TO 40
C
C Update chain parameters
C
         PCHAIN(1) = 1./HELPAR(1)
         CALL UCOPY (HELPAR(2),PCHAIN(JTCHTL),4)
         PCHAIN(JTCHLC) = S13
         PCHAIN(JTCHC1) = CHIRP
         PCHAIN(JTCHC2) = CHISZ
         ISTCHA = 10000*IR1 + 100*IR2 + NCIRCL
         CALL UCOPY (ERRMAT(1),PCHAIN(JTCHER),15)
C
C  Store fit in JTTCHA workbank.
C
         IF (LFRROW(JTTCHA).LT.1) THEN
            NDAT = IW(JTTCHA) + 10*LCOLS(JTTCHA)
            CALL WBANK(IW,JTTCHA,NDAT,*800)
         ENDIF
C
         RW(KNEXT(JTTCHA) + JTCHIR) = 1./PCHAIN(1)
         CALL UCOPY(PCHAIN(JTCHTL),RW(KNEXT(JTTCHA) + JTCHTL),7)
         CALL UCOPY(PCHAIN(JTCHER),RW(KNEXT(JTTCHA) + JTCHER),15)
         IW(KNEXT(JTTCHA) + JTCHOF) = IW(JTTCTC + LMHROW)
         IW(KNEXT(JTTCHA) + JTCHNC) = NHCHAI
C
C   Define hit row word
         IHPAT = 0
         DO 50 JH = 1,NHCHAI
            IW(KROW(JTNWCO,IHCHAI(JH))+JTNWZO)=IW(JTTCHA+LMHROW)+1
            IHPAT = IBSET(IHPAT,IRCHAI(JH))
   50    CONTINUE
         IW(KNEXT(JTTCHA) + JTCHPI) = IHPAT
C
         IW(JTTCHA + LMHROW) = IW(JTTCHA + LMHROW) + 1
C
         IF (LFRROW(JTTCTC).LT.1) THEN
            NDAT = IW(JTTCTC) + 50*LCOLS(JTTCTC)
            CALL WBANK(IW,JTTCTC,NDAT,*800)
         ENDIF
C
C   Enter pointers to JTNWCO in JTTCTC work bank
         DO 55 JH = 1,NHCHAI
            IW(KNEXT(JTTCTC)+1)=IHCHAI(NHCHAI+1-JH)
            IW(JTTCTC+LMHROW) = IW(JTTCTC+LMHROW) + 1
   55    CONTINUE
C
C Clear chain for next candidate
C
   40    NHCHAI = 0
         CALL VZERO(IRCHAI,MXHTCH)
         CALL VZERO(MCHAIN,MXHTCH)
   30 CONTINUE
C  Next top row
      GO TO 1
C
  800 CALL RERROR('TSACHA',4,'Error extending TCHA or TCTC bank')
  999 RETURN
      END
#endif
