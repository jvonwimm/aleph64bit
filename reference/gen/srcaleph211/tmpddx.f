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
      CHARACTER*(*) NAME
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JTEXSI=1,JTEXTM=2,JTEXTL=3,JTEXNS=4,JTEXAD=5,JTEXTN=6,
     +          JTEXSF=7,LTEXSA=7)
      PARAMETER(JTC1ID=1,JTC1VR=2,JTC1NR=4,JTC1SN=5,JTC1TP=77,JTC1SL=78,
     +          JTC1AP=79,LTC1XA=79)
      INTEGER JTP1NR,JTP1SN,JTP1TP,JTP1MS,LTP1XA
      PARAMETER(JTP1NR=1,JTP1SN=2,JTP1TP=38,JTP1MS=39,LTP1XA=39)
      PARAMETER (LTPDRO=21,LTTROW=19,LTSROW=12,LTWIRE=200,LTSTYP=3,
     +           LTSLOT=12,LTCORN=6,LTSECT=LTSLOT*LTSTYP,LTTPAD=4,
     +           LMXPDR=150,LTTSRW=11)
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
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C
      IF (NTC1X.EQ.0) THEN
        NTPXS=NAMIND('TPXS')
        NTP1X=NAMIND('TP1X')
        NTEXS=NAMIND('TEXS')
        NTC1X=NAMIND('TC1X')
      ENDIF
C
C! Get banks from DB depending on run and setup code
C
      CALL ABRUEV (IRUN,IEVT)
      IRET = 0
      IF (IRUN.NE.IROLD) THEN
        IROLD = IRUN
        IF (IRUN.LE.2000) THEN
           ITP = GTSTUP (DET,IRUN)
        ELSE
           ITP = IRUN
        ENDIF
        IRET= ALGTDB(JUNIDB(0),LIST,-ITP)
      ENDIF
C
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
