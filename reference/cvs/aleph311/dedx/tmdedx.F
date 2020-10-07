      SUBROUTINE TMDEDX(ITK,RI,RSIG,NS,TL,IER)
C
C-------------------------------------------------------------------
C! Return reduced and calibrated dE/dx for a single track.
C!
CKEY DEDX TPC MDST
C!
C!    Author:  R. Johnson    31-05-89
C!    Modified:W. Wiedenmann 31-08-93 use new banks TCGX/TCSX
C!
C! Input:   ITK         /I       Track number in FRFT bank
C! Output:  RI          /R       Measured ionization (1.0=minion, Q=1)
C!          RSIG        /R       Relative error on the dE/dx
C!                               The error to be used in analysis
C!                               should be calculated from:
C!                               SIGMA**2= (RSIG*Iexp)**2 + SIG_P**2
C!                               where Iexp is the expected ionization
C!                               for a given hypothesis, and SIG_P
C!                               is the contribution from momentum
C!                               error.
C!          NS          /R       Number of useful wire samples on track
C!          TL          /T       Useful length of the track (cm)
C!          IER         /I       Error return= 0 for success
C!                               2= can't find dE/dx bank
C!                               3= track has no dE/dx information
C!                               4= cannot find calibration banks
C!                               6= no valid dE/dx calibration exists
C!                                  for this run
C!
C----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "texsjj.h"
#include "tc1xjj.h"
#include "tpgpar.h"
      DIMENSION SNR(LTSECT)
C
      LOGICAL FOUND
      INTEGER ALGTDB,GTSTUP
      CHARACTER  DET*2, LIST*4
      PARAMETER (DET='TP', LIST='TC1X')
      DATA IROLD /0/
      DATA NTC1X, NTEXS / 2*0/
C
C++   Distance between TPC sense wires
C
      DATA DWIR/0.400/
C
#include "bmacro.h"
C
      IF (NTC1X.EQ.0) THEN
        NTEXS=NAMIND('TEXS')
        NTC1X=NAMIND('TC1X')
      ENDIF
C
#include "gtdbbk.h"
      KTC1X=IW(NTC1X)
      IF (KTC1X.EQ.0) THEN
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
      KTEXS=IW(NTEXS)
      IF (KTEXS.EQ.0) THEN
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
      NS=0
      TRMN=0.
      TL=0.
      FOUND=.FALSE.
      DO 100 ISG=1,LROWS(KTEXS)
        IPNT=ITABL(KTEXS,ISG,JTEXTN)
        IF (IPNT.NE.ITK) THEN
          IF (FOUND) GO TO 101
          GO TO 100
        ENDIF
        FOUND=.TRUE.
C
C++     Skip sectors with more than 40% of hits saturated
C
        IF (ITABL(KTEXS,ISG,JTEXSF).EQ.1) GO TO 100
C
C++     Skip sectors which could not be calibrated (RNRMS=0)
C
        ISLOT=ITABL(KTEXS,ISG,JTEXSI)
        RNRMS= SNR(ISLOT)
        IF (RNRMS.LE.0.) GO TO 100
C
C++     Measured ionization (truncated mean)
C
        TMS=RTABL(KTEXS,ISG,JTEXTM)
C
C++     Correction for sample length
C
        NSMP=ITABL(KTEXS,ISG,JTEXNS)
        IF (NSMP.LE.1) GO TO 100
        RNSMP=FLOAT(NSMP)
        ASL=RTABL(KTEXS,ISG,JTEXTL)/RNSMP
        SMPL=ALOG(ASL/DWIR)
C
C++     Correct the sector-to-sector normalization
C
        TMS= TMS*RNRMS
C
C++     Add the contributions from different sectors, weighted by the
C++     number of wire pulses in the sector
C
        TRMN=TRMN + TMS * RNSMP
C
C++     Add up the total track length and number of samples
C
        NS=NS + NSMP
        TL=TL+ RTABL(KTEXS,ISG,JTEXTL)
  100 CONTINUE
  101 CONTINUE
      IF (NS.EQ.0) THEN
        IER=3
        GO TO 999
      ENDIF
      RI=TRMN/FLOAT(NS)
C
C++   Correct the truncated mean with overall normalization factor
C
      RI= RI*RNRMA
C
C++   We want NS to represent the number of samples BEFORE truncation,
C++   so we divide by JTRUNK.  The same applies to TL, the length.
C
      JTRUNK=ITABL(KTC1X,1,JTC1TP)
      NS=(NS*100)/JTRUNK
      TL=TL*100./FLOAT(JTRUNK)
C
C++   Get the resolution corresponding to this track length and number
C++   of samples.
C
      RSIG=TDXERR(NS,TL,IER)
C
      IER=0
  999 CONTINUE
      RETURN
      END
#endif
