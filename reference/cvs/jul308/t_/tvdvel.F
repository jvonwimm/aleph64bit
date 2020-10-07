      SUBROUTINE TVDVEL
C-------------------------------------------------------------------
C! Calculate TPC drift velocity using VDET.
C!
C!    Author  : I. Tomalin  15-8-95
C!    Modified: I. Tomalin   8-7-96 (Protection for small no. of VDET coords)
C!
C! Input banks: TSVD, JCON
C! Output bank: TVVD
C!
C! Calculate drift velocity using VDET residuals from TSVD bank.
C! First find truncated mean residual in each side.
C! Then find V_d using difference of these two numbers.
C! See ALEPH note 95-108 for details.
C?
C!------------------------------------------------------------------
#ifndef DOC
C If ratio of used/expected number of VDET coords falls below this,
C then the input drift velocity was probably too far out.
      PARAMETER(EXPRAT=0.35)
C Require minimum number of observed VDET coords in each half to trust result.
      PARAMETER(MINOBS=5)
C Fraction of entries to be excluded in truncated mean.
      PARAMETER(FTRUNC=0.10)
C
      DIMENSION NUMEXP(2),NUMOBS(2),ZRES(2),Z2RES(2),E2ZRES(2)
C
C TSVD work bank containing histogram of VDET z residuals.
#include "tsvdwb.h"
C Number of bins in z residual.
      PARAMETER(NZBIN=JTSVNZ-JTSVHZ)
C
C-- General parameters/arrays.
C
#include "tvvdjj.h"
#include "jconjj.h"
#include "alcons.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "rcurnt.h"
#include "bcs.h"
#include "bmacro.h"
C--------------------------------------------------------------------
C Calculate drift velocity from VDET.
      IF (KTSVD.GT.0) THEN
C Find drift velocity used by JULIA.
        KJCON = NLINK('JCON',IRUNRC)
        IF (KJCON.GT.0) THEN
          VELJUL = RW(KJCON + LMHLEN + JJCODZ)
        ELSE
          CALL RERROR ('TVDVEL',2,'Unable to find JCON bank')
          GOTO 999
        END IF
C
        CHI2  = 0.0
        XNDF  = 0.0
        SPRDMX = 0.0
C
C--- Loop over sides A and B of detector.
        DO 350 IEND = 1,2
          ITSVD = KROW(KTSVD,IEND)
C Find window and bin size in z residual.
          ZWIN = RW(ITSVD + JTSVWZ)
          ZBIN = 2.0*ZWIN/FLOAT(NZBIN)
C Note number of VDET coordinates expected and observed.
          NUMEXP(IEND) = IW(ITSVD + JTSVEX)
          NUMOBS(IEND) = IW(ITSVD + JTSVNU)
C
C--- Find truncated mean z residual.
C
          IF (NUMOBS(IEND).GT.NUMEXP(IEND)*EXPRAT.AND.
     +        NUMOBS(IEND).GT.MINOBS) THEN
C
C Find unweighted median.
            IMED = 1
            NUM = 0
            DO 320 IBIN = 1,NZBIN
              NUM   = NUM + IW(ITSVD + (JTSVNZ - 1) + IBIN)
              IF (NUM.LE.NUMOBS(IEND)/2) IMED = IBIN
  320       CONTINUE
C
C Find limits for truncation.
            ILOW   = IMED + 1
            IHIGH  = IMED
            INSIDE = 0
  330       CONTINUE
C
              IF (ILOW.GT.1) THEN
                ILOW  = ILOW - 1
                INSIDE = INSIDE + IW(ITSVD + (JTSVNZ - 1) + ILOW)
              END IF
              IF (IHIGH.LT.NZBIN) THEN
                IHIGH = IHIGH + 1
                INSIDE = INSIDE + IW(ITSVD + (JTSVNZ - 1) + IHIGH)
              END IF
C
            IF (ILOW.GT.1.AND.IHIGH.LT.NZBIN.AND.
     +          FLOAT(INSIDE).LE.NUMOBS(IEND)*(1.0 - FTRUNC)) 
     +      GOTO 330
C
C Find total weight.
            WTOT = 0.0
            DO 310 IBIN = ILOW,IHIGH
              WTOT = WTOT + RW(ITSVD + (JTSVHZ - 1) + IBIN)
  310       CONTINUE
C
C Now get mean.
            SUM    = 0.0
            ZSUM   = 0.0
            Z2SUM  = 0.0
            DO 340 IBIN = ILOW,IHIGH
              ZR     = -ZWIN + ZBIN*(FLOAT(IBIN) - 0.5)
              WW     = RW(ITSVD + (JTSVHZ - 1) + IBIN)
C Don't allow individual track to have more than 1/MINOBS weight.
              IF (MINOBS.GT.1) WW = MIN(WW, 
     +          WTOT*IW(ITSVD + (JTSVNZ - 1) + IBIN)/FLOAT(MINOBS))
              SUM    = SUM    + WW
              ZSUM   = ZSUM   + WW*ZR
              Z2SUM  = Z2SUM  + WW*ZR**2
  340       CONTINUE
            ZRES(IEND)   = ZSUM/SUM
            Z2RES(IEND)  = Z2SUM/SUM
            E2ZRES(IEND) = 1.0/SUM
C Calculate width of z residual distribution.
            SPREAD   = SQRT(MAX(Z2RES(IEND) - ZRES(IEND)**2,0.0))
C Make approximate correction for truncation if at less than 10 sigma.
            ZCUT     = 0.5*ZBIN*FLOAT(IHIGH - ILOW + 1)
            IF (ZCUT.LT.10.0*SPREAD) THEN
              SIGCUT = ZCUT/SPREAD
              ACCEPT = ERF(SIGCUT/SQRT(2.0))
              SCOR2  = 1.0 - SIGCUT/SQRT(PIBY2)*EXP(-0.5*SIGCUT**2)/
     +                              ACCEPT
              SCOR   = SQRT(SCOR2)
              SPREAD = SPREAD/SCOR
            ELSE
              ACCEPT = 1.0
            END IF
C Calculate CHI2 and ndf corrected for truncation.
            CHI2     = CHI2 + SUM/ACCEPT*SPREAD**2
            XNDF     = XNDF + FLOAT(NUMOBS(IEND) - 1)*ACCEPT
C Don't allow spread to be less than that corresponding to CHI2/ndf = 1.
            SPREAD   = MAX(SPREAD,ACCEPT*
     +                 SQRT(FLOAT(NUMOBS(IEND) - 1)/SUM))
C Spread should be identical on each side. Note the worst side.
            SPRDMX   = MAX(SPREAD,SPRDMX)
C
          ELSE
C
C Give up: too few VDET hits observed.
            GOTO 390
          END IF
  350   CONTINUE
C
C Find relative correction needed to drift velocity, using this face.
        CORVEL = -(ZRES(1) - ZRES(2))/(2.0*ZTPCMX)
C and its error**2.
        ERCOR2 =  (E2ZRES(1) + E2ZRES(2))/(2.0*ZTPCMX)**2
C Calculate chi**2/ndf of fit.
        CH2NDF = CHI2/XNDF
C Increase error estimate if CHI**2 is too bad.
C       IF (CH2NDF.GT.2.0) ERCOR2 = ERCOR2*SQRT(CH2NDF)
C Note number of expected and observed entries on worst side.
        IF (FLOAT(NUMOBS(1))/FLOAT(NUMEXP(1)).LT.
     +      FLOAT(NUMOBS(2))/FLOAT(NUMEXP(2))) THEN
          NEXPW = NUMEXP(1)
          NOBSW = NUMOBS(1)
        ELSE
          NEXPW = NUMEXP(2)
          NOBSW = NUMOBS(2)
        END IF
C Note clearance in standard deviations from edge of window on worst side.
C (If less than 2, then output V_d is unreliable).
        CLEAR  = MIN(ZWIN - ABS(ZRES(1)),ZWIN - ABS(ZRES(2)))/SPRDMX
C
C Create TVVD bank.
        CALL AUBOS('TVVD',IRUNRC,LMHLEN+LTVVDA,KTVVD,IGARB)
        IF(IGARB.EQ.2) THEN
          CALL RERROR ('TVDVEL',1,'Unable to create TVVD bank')
          GOTO 999
        ENDIF
        CALL BKFMT('TVVD','2I,(2F,2I,2F)')
C
        IW(KTVVD + LMHCOL) = LTVVDA
        IW(KTVVD + LMHROW) = 1
C Fill TVVD bank.
        RW(KTVVD + LMHLEN + JTVVVZ) = VELJUL*(1.0 + CORVEL)
        RW(KTVVD + LMHLEN + JTVVER) = VELJUL*SQRT(ERCOR2)
        IW(KTVVD + LMHLEN + JTVVEX) = NEXPW
        IW(KTVVD + LMHLEN + JTVVNU) = NOBSW
        RW(KTVVD + LMHLEN + JTVVC2) = CH2NDF
        RW(KTVVD + LMHLEN + JTVVCL) = CLEAR
C
  390   CONTINUE
C
C Drop TSVD work bank.
        CALL WDROP(IW,KTSVD)
      END IF
C
  999 RETURN
      END
#endif
