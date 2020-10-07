      SUBROUTINE TSAPAT
C
C----------------------------------------------------------------
C! Steering for track finding at low polar angles.
C!
C!    Author:    M. Mermikides  890904
C!
C!    Description
C!    ===========
C?      1.  Look for unassociated or bad coordinates in the low
C?          polar angle region (within cone subtented by K sectors)
C?      2.  Use histogramming method to find potential tracks
C?          among the missed and bad coordinates (It is assumed that
C?          we are dealing with fast tracks originating near the IP)
C?      3.  Improve or define z-coordinate using wire information
C?      4.  Find tracks using improved coordinates
C?
C!======================================================================
#ifndef DOC
#include "tflags.h"
#include "bcs.h"
#include "rlunit.h"
#include "tpcojj.h"
#include "tbcojj.h"
#include "alcons.h"
#include "tpgpar.h"
#include "tpccon.h"
#include "tsawrk.h"
C
      LOGICAL LFRST,FLAGS(3)
      SAVE NTPCO,NTBCO,NTCTC
C
      DATA LFRST/.TRUE./
C Work bank parameters
      DATA LTHPH/5/,NTHPH/100/,NEXTP/25/
      DATA NBPHI/24/, NBTHE/80/
C Cut in theta corresponding to last padrow of K sector
C      DATA THCUT/0.39248/
      DATA THCUT/0.40491/
C Cuts on max. number of pads and buckets in bad coordinate clusters
      DATA MXNPD/6/,MXNBK/40/
C
#include "bmacro.h"
C
      IF (LFRST) THEN
         LFRST=.FALSE.
         NTPCO=NAMIND('TPCO')
         NTBCO=NAMIND('TBCO')
         NTCTC=NAMIND('TCTC')
         JTHPHI = 0
         JTPHIS = 0
         JTPHCL = 0
         JTNWCO = 0
         JTTCHA = 0
         JTTCTC = 0
         JTWRK1 = 0
         JTWRK2 = 0
         JTWRK3 = 0
      ENDIF
C
C++   Check that the data have been prepared.
C     At this stage we expect TPCO bank NR=3 and TBCO bank NR = 1
C
      KTPCO = IW(NTPCO)
      IF (KTPCO.NE.0) THEN
         IF (IW(KTPCO-2).LE.2) GO TO 999
      ELSE
         GO TO 999
      ENDIF
      KTBCO = IW(NTBCO)
      IF (KTBCO.NE.0) THEN
         IF (IW(KTBCO-2).LE.0) GO TO 999
      ENDIF
C
C++   Book work banks for binned coordinates and histogram
C
      CALL WBANK(IW,JTHPHI,LMHLEN+NTHPH*LTHPH,*800)
      IW(JTHPHI+LMHCOL) = LTHPH
      IW(JTHPHI+LMHROW) = 0
      IW(JTHPHI-3) = INTCHA('JTP1')
      CALL WBANK(IW,JTPHIS,LMHLEN+NBTHE*NBPHI,*800)
      IW(JTPHIS+LMHCOL) = NBTHE
      IW(JTPHIS+LMHROW) = NBPHI
      IW(JTPHIS-3) = INTCHA('JTP2')
C
C++   Use JTWRK1 work bank to flag the coordinates already associated
C++   with chains
C
      CALL WBANK(IW,JTWRK1,LMHLEN+LROWS(KTPCO),*800)
      IW(JTWRK1+LMHCOL) = 1
      IW(JTWRK1+LMHROW) = LROWS(KTPCO)
      KTCTC = IW(NTCTC)
      IF (KTCTC.GT.0) THEN
         DO 5 I=1,LROWS(KTCTC)
            ICH = IW(KTCTC+LMHLEN+I)
            IW(JTWRK1+LMHLEN+ICH) = 1
    5    CONTINUE
      ENDIF
C
C++   Now look for unassigned hits within polar angle cut and store
C++   theta and phi bins and TPCO bank row pointers
C
      DO 10 I = 1,LROWS(KTPCO)
         IF (IW(JTWRK1+LMHLEN+I).NE.0) GO TO 10
C  Check that we have enough space to store coordinate
         IF (LFRROW(JTHPHI).LT.1) THEN
            NDAT = IW(JTHPHI) + NEXTP*LTHPH
            CALL WBANK(IW,JTHPHI,NDAT,*800)
         ENDIF
         IRS = ITABL(KTPCO,I,JTPCIN)
         ISEC = MOD(IRS/1000,100)
         IROW = IRS/100000
         PHI = RTABL(KTPCO,I,JTPCPH)
         ZV = RTABL(KTPCO,I,JTPCZV)
         IF (ABS(ZV).LT.0.001) GO TO 10
         THE = ATAN(RTABL(KTPCO,I,JTPCRV)/ZV)
C  Theta acceptance test (Forward/backward cones)
         IF (ABS(THE).GT.THCUT) GO TO 10
         IF (THE.LT.0.0) THE = THE + PI
         ITHE = THE*FLOAT(NBTHE)/PI + 1
         IF(ITHE.GT.NBTHE) ITHE = 1
         IPHI = PHI*FLOAT(NBPHI)/TWOPI + 1
         IF (IPHI.GT.NBPHI) IPHI = 1
         IND = KROW(JTPHIS,IPHI)+ITHE
         IW(IND) = IW(IND) + 1
         IW(KNEXT(JTHPHI)+ 1) = ITHE
         IW(KNEXT(JTHPHI)+ 2) = IPHI
         IW(KNEXT(JTHPHI)+ 3) = I
C  Set the cluster number to 0 initially
         IW(KNEXT(JTHPHI)+ 4) = IROW
         IW(KNEXT(JTHPHI)+ 5) = ISEC
         IW(JTHPHI + LMHROW) = IW(JTHPHI + LMHROW) + 1
   10 CONTINUE
C
C  Similarly for bad coordinates. We store in this case the negative
C  value of the TBCO pointer to distiguish from TPCO
C
      IF (KTBCO.EQ.0) GO TO 500
C
      DO 20 I=1,LROWS(KTBCO)
C  Check that we have enough space to store coordinate
         IF (LFRROW(JTHPHI).LT.1) THEN
            NDAT = IW(JTHPHI) + NEXTP*LTHPH
            CALL WBANK(IW,JTHPHI,NDAT,*800)
         ENDIF
C
C  Do not accept clusters of too many pads or of excessive length
C
         IF (ITABL(KTBCO,I,JTBCRW).GT.MXNPD) GO TO 20
         IF (ITABL(KTBCO,I,JTBCZW).GT.MXNBK) GO TO 20
         IRS = ITABL(KTBCO,I,JTBCIN)
         ISEC = MOD(IRS/1000,100)
         IROW = IRS/100000
         PHI = RTABL(KTBCO,I,JTBCPH)
         ZV = RTABL(KTBCO,I,JTBCZV)
         IF (ABS(ZV).LT.0.001) GO TO 20
         THE = ATAN(RTABL(KTBCO,I,JTBCRV)/ZV)
C  Theta acceptance test
         IF (ABS(THE).GT.THCUT) GO TO 20
         IF (THE.LT.0.0) THE = THE + PI
         ITHE = THE*FLOAT(NBTHE)/PI + 1
         IF(ITHE.GT.NBTHE) ITHE = 1
         IPHI = PHI*FLOAT(NBPHI)/TWOPI + 1
         IF (IPHI.GT.NBPHI) IPHI = 1
         IND = KROW(JTPHIS,IPHI)+ITHE
         IW(IND) = IW(IND) + 1
         IW(KNEXT(JTHPHI)+ 1) = ITHE
         IW(KNEXT(JTHPHI)+ 2) = IPHI
         IW(KNEXT(JTHPHI)+ 3) = -I
         IW(KNEXT(JTHPHI)+ 4) = IROW
         IW(KNEXT(JTHPHI)+ 5) = ISEC
         IW(JTHPHI + LMHROW) = IW(JTHPHI + LMHROW) + 1
   20 CONTINUE
C  Drop work bank no longer needed
      CALL WDROP(IW,JTWRK1)
C  Compress JTHPHI work bank
  500 IF (LROWS(JTHPHI).GT.0) THEN
         CALL WBANK(IW,JTHPHI,LMHLEN+LCOLS(JTHPHI)*LROWS(JTHPHI),*800)
      ENDIF
C
C----------------------------------------------------------------
C  Look for theta-phi clusters.
C----------------------------------------------------------------
      CALL TSACLU
C----------------------------------------------------------------
C  For each cluster do wire association and coordinate refinement
C----------------------------------------------------------------
      CALL TSAWIR
C----------------------------------------------------------------
C  Find tracks using new coordinates
C----------------------------------------------------------------
      CALL TSATRK
C----------------------------------------------------------------
C  Append new chains to TCHA.
C  Update and sort coordinate-related banks
C----------------------------------------------------------------
      CALL TSACOR
C
      CALL WDROP(IW,JTTCTC)
      CALL WDROP(IW,JTTCHA)
      CALL WDROP(IW,JTNWCO)
      CALL WDROP(IW,JTPHCL)
      CALL WDROP(IW,JTPHIS)
      CALL WDROP(IW,JTHPHI)
C
      GO TO 999
C
  800 CALL RERROR('TSAPAT',4,'Error booking/extending work bank')
  999 RETURN
      END
#endif
