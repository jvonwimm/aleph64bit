      SUBROUTINE TMPDDX(NAME,ITK,RI,RSIG,XNS,TL,IER)
C
C-------------------------------------------------------------------
C! Return reduced and calibrated dE/dx for a single track.
C!
CKEY DEDX TPC MDST
C!
C!    Author:  R. Johnson    31-05-89
C!    Modified:W. Wiedenmann 31-08-93 use new banks TCGX/TCSX
C!             D. Casper     14-06-95 create TMPDDX from TMDEDX
C!
C! Input:   NAME        /A       'PAD' or 'WIRE'
C!          ITK         /I       Track number in FRFT bank
C! Output:  RI          /R       Measured ionization (1.0=minion, Q=1)
C!          RSIG        /R       Relative error on the dE/dx
C!                               The error to be used in analysis
C!                               should be calculated from:
C!                               SIGMA**2= (RSIG*Iexp)**2 + SIG_P**2
C!                               where Iexp is the expected ionization
C!                               for a given hypothesis, and SIG_P
C!                               is the contribution from momentum
C!                               error.
C!          XNS         /R       Number of useful wire samples on track
C!          TL          /T       Useful length of the track (cm)
C!          IER         /I       Error return= 0 for success
C!                               2= can't find dE/dx bank
C!                               3= track has no dE/dx information
C!                               4= cannot find calibration banks
C!                               6= no valid dE/dx calibration exists
C!                                  for this run
C!
C! Input banks:
C!       WIRE dE/dx - TEXS (data), TC1X (calibration)
C!       PAD  dE/dx - TPXS (data), TP1X (calibration)
C! HAC parameters are the same for TEXS and TPXS
C! HAC parameters are different for TC1X and TP1X, only one is used
C! JTNATP set to JTC1TP or JTP1TP (percentage used for truncated mean)
C----------------------------------------------------------------------
#ifndef DOC
      CHARACTER*(*) NAME
C
#include "bcs.h"
#include "texsjj.h"
#include "tc1xjj.h"
#include "tp1xjj.h"
#include "tpgpar.h"
      DIMENSION SNR(LTSECT)
C
      LOGICAL FOUND
      INTEGER ALGTDB,GTSTUP
      CHARACTER  DET*2, LIST*8
      PARAMETER (DET='TP', LIST='TC1XTP1X')
      DATA IROLD /0/
      DATA NTC1X, NTEXS, NTP1X, NTPXS / 4*0/
C
C++   Distance between TPC sense wires
C
      DATA DWIR/0.400/
C
#include "bmacro.h"
C
      IF (NTC1X.EQ.0) THEN
        NTPXS=NAMIND('TPXS')
        NTP1X=NAMIND('TP1X')
        NTEXS=NAMIND('TEXS')
        NTC1X=NAMIND('TC1X')
      ENDIF
C
#include "gtdbbk.h"
C
C - Pad or Wire
C
      IF (NAME(1:3) .EQ. 'PAD') THEN
         NDNAM = NTPXS
         NTNAM = NTP1X
         JTNATP= JTP1TP
      ELSE
         NDNAM = NTEXS
         NTNAM = NTC1X
         JTNATP= JTC1TP
      ENDIF
C
      KTNAM=IW(NTNAM)
      IF (KTNAM.EQ.0) THEN
         IER=4
         GOTO 999
      ENDIF
      CALL TDEDXV(RNRMA,SNR,IER)
      IF (IER.NE.0) THEN
         IER=4
         GOTO 999
      ENDIF
C
C++   Link to the dE/dx reconstructed information
C
      KDNAM=IW(NDNAM)
      IF (KDNAM.EQ.0) THEN
        IER=2
        GO TO 999
      ENDIF
C
C++   Overall normalization.  If this is zero, then there is no
C++   valid dE/dx calibration for this run.
C
      IF (RNRMA.LE.0.) THEN
        IER=6
        RETURN
      ENDIF
C
C++   Get the particle's measured dE/dx, track length, and # samples
C
      XNS=0.
      TRMN=0.
      TL=0.
      FOUND=.FALSE.
      DO 100 ISG=1,LROWS(KDNAM)
        IPNT=ITABL(KDNAM,ISG,JTEXTN)
        IF (IPNT.NE.ITK) THEN
          IF (FOUND) GO TO 101
          GO TO 100
        ENDIF
        FOUND=.TRUE.
C
        IF (NAME(1:3).EQ.'WIR') THEN
C++     Skip sectors with more than 40% of hits saturated
           IF (ITABL(KDNAM,ISG,JTEXSF).EQ.1) GO TO 100
        ENDIF
C
C++     Skip sectors which could not be calibrated (RNRMS=0)
C
        ISLOT=ITABL(KDNAM,ISG,JTEXSI)
        RNRMS= SNR(ISLOT)
        IF (RNRMS.LE.0.) GO TO 100
C
C++     Measured ionization (truncated mean)
C
        TMS=RTABL(KDNAM,ISG,JTEXTM)
C
C++     Correction for sample length
C
        IF (NAME(1:3).EQ.'WIR') THEN
           RNSMP = REAL(ITABL(KDNAM,ISG,JTEXNS))
        ELSE
           RNSMP = RTABL(KDNAM,ISG,JTEXNS)
        ENDIF
        IF (RNSMP.LE.0.) GO TO 100
C
C++     Correct the sector-to-sector normalization
C
        TMS= TMS*RNRMS
C
C++     Add the contributions from different sectors, weighted by the
C++     number of wire pulses/pad samples in the sector
C
        TRMN=TRMN + TMS * RNSMP
C
C++     Add up the total track length and number of samples
C
        XNS=XNS + RNSMP
        TL=TL+ RTABL(KDNAM,ISG,JTEXTL)
  100 CONTINUE
C
  101 CONTINUE
      IF (XNS.LE.0.) THEN
        IER=3
        GO TO 999
      ENDIF
      RI=TRMN/XNS
C
C++   Correct the truncated mean with overall normalization factor
C
      RI= RI*RNRMA
C
C++   We want XNS to represent the number of samples BEFORE truncation,
C++   so we divide by JTRUNK.  The same applies to TL, the length.
C
      JTRUNK=ITABL(KTNAM,1,JTNATP)
      XNS=(XNS*100)/JTRUNK
      TL=TL*100./FLOAT(JTRUNK)
C
C++   Get the resolution corresponding to this track length and number
C++   of samples.
C
      RSIG=TPXERR(NAME,XNS,TL,IER)
C
      IER=0
  999 CONTINUE
      RETURN
      END
#endif
