      SUBROUTINE QDEDXM(ITK,N,RMASS,QH,RI,NS,TL,RIEXP,SIGMA,IER)
CKEY DEDX MONTE /USER
C----------------------------------------------------------------------
C! Analyse dE/dx for Monte Carlo events by faking the ionization
C! with a gaussian random number.  If called for real data, the
C! result will be the same as if QDEDX were called.
C
C  R. Johnson   10-5-90
C  modified D. Casper  05-04-1993 for 1992 MCarlos
C  modified G. Graefe  26-04-1994 for NanoDst input
C
C     Input:   ITK      /I       ALPHA track number
C              N        /I       Number of hypothesis to try
C              RMASS(n) /R       Mass hypotheses
C              QH(n)    /R       Charge hypotheses (sign doesn't matter)
C     Output:  RI       /R       Measured ionization (1.O=minion, Q=1)
C              NS       /I       Number of useful wire samples on track
C              TL       /R       Useful length of the track (cm)
C              RIEXP(n) /R       Expected ionization for the given
C                                mass hypothesis (1.0=minion, Q=1)
C              SIGMA(n) /R       Sigma of dE/dx measurement error,
C                                including the momentum error.
C                                Note that one can calculate a
C                                chi-squared with 1 d.o.f. as:
C                                chi2 = ((RI-RIEXP)/SIGMA)**2
C              IER      /I       Error return= 0 for success
C                                1= can't find track bank or not
C                                   a reconstructed charged track
C                                2= can't find dE/dx bank
C                                3= track has no dE/dx information
C                                4= can't find calibration banks
C                                   TC1X, TC2X, and/or TC3X
C                                5= cannot find RUNH or EVEH bank
C                                   from which to get the run number
C                                6= No MC truth
C                                7= No RHAH bank
C                                8= 'JULIA' not found in RHAH
C   Input to the entry point QMTAIL.  This entry may be called to
C   -----                    ------   set the parameter TAIL, which
C                                     otherwise defaults to 999. It
C                                     only need be called once.
C              TAIL     /R       If the dE/dx from TPCSIM is more than
C                                TAIL sigmas from the "true" mean dE/dx
C                                then the TPCSIM dE/dx will be kept.
C                                This is to allow one to retain some
C                                of the simulation of tails, which
C                                usually arise from track overlap.
C                                If TAIL is set less than 3.0, then
C                                the value 3.0 is used. Set to a large
C                                value if you don't want this feature.
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "rhahjj.h"
#include "qcde.h"
      CHARACTER PNAME*8,CHAINT*4
      LOGICAL FIRST,WIRBUG
C    Parameters for correction of number of samples for 1992 MC data.
      PARAMETER (XLO = 0.)
      PARAMETER (XMID = 260.)
      PARAMETER (XHI = 340.)
      PARAMETER (SCALE_LO = 1.2)
      PARAMETER (XBK = XLO + (XMID-XLO)/SCALE_LO)
      PARAMETER (SCALE_HI = (XHI-XBK)/(XHI-XMID))
C
      DIMENSION RMASS(*),QH(*),RIEXP(*),SIGMA(*)
      DATA TLMN/3./,RPLIM/999./,FIRST/.TRUE./
C
#include "qmacro.h"
C-----------------------------------------------------------------------
      IF (XNANO) THEN
        CALL NADDXM(ITK,N,RMASS,QH,RI,NS,TL,RIEXP,SIGMA,IER)
        GOTO 999
      ENDIF
      IF (KCLASS(ITK).NE.KRECO) THEN
        IER=1
        GOTO 999
      ENDIF
      IF (QCH(ITK).EQ.0.) THEN
        IER=1
        GOTO 999
      ENDIF
      IT1=KCHT(ITK)
      IF (KRUN.GT.2000) THEN
        CALL TIDHYP(KTN(IT1),QMFLD,N,RMASS,QH,RI,NS,TL,RIEXP,SIGMA,IER)
        GOTO 999
      ENDIF
      IF (.NOT.XMCEV) THEN
        IER=6
        GOTO 999
      ENDIF
C
C++   Find the appropriate run number in data to simulate
C
      KRHAH=IW(NARHAH)
      IF (KRHAH.NE.0) THEN
C JC 920506 - must take latest, not first JULIA version number
        IPVER=0
        WIRBUG = .FALSE.
        DO 891 I=1,LROWS(KRHAH)
          PNAME(1:4)=CHAINT(ITABL(KRHAH,I,JRHAPN))
          PNAME(5:8)=CHAINT(ITABL(KRHAH,I,JRHAPN+1))
          IF (PNAME.EQ.'GALEPH')THEN
            IGAVER=ITABL(KRHAH,I,JRHAAV)
          ELSEIF (PNAME.EQ.'JULIA') THEN
            IPVER=ITABL(KRHAH,I,JRHAPV)
            IJCOR=ITABL(KRHAH,I,JRHACV)
C           GO TO 892
          ENDIF
  891   CONTINUE
        IF(IPVER.NE.0) GO TO 892
        IER=8
        RETURN
  892   CONTINUE
      ELSE
        IER=7
        RETURN
      ENDIF
C++   Version 252 of JULIA was used starting with the 1991 runs.
C++   Version 264 of JULIA was used for 1992 data.  This also
C++   requires a different set of constants.
C++   Also, two bugs in the wire reduction code (TWRRED, in ALEPHLIB,
C++   called by GALEPH) and dead pad simulator (TPKILL in JULIA)
C++   resulted in bad wire data for the first 92 production.  The
C++   number of samples must be corrected before before being used
C++   to predict the resolution.  While the corrected distribution
C++   has the right mean and general shape, it is not perfectly
C++   smooth due to binning effects.
C++   The calibration of the earlier runs is very different, so they
C++   need a special simulation, unless they get reprocessed someday.
C
      IF (IPVER.GE.264) THEN
        IRUN=16001
        IF (IGAVER.LT.145 .OR. IJCOR.LT.26408) WIRBUG =.TRUE.
      ELSEIF (IPVER.GE.252) THEN
        IRUN=10001
      ELSE
        IRUN=6001
      ENDIF
      IF (FIRST) THEN
        FIRST=.FALSE.
        WRITE(KUPRNT,8913) IRUN
 8913   FORMAT(/' You are using QDEDXM to analyze dE/dx in Monte ',/
     &  ' Carlo events.  Note that the only information taken from',/
     &  ' the detailed TPC simulation (TPCSIM) is the number of',/
     &  ' isolated wire hits on each track and the total track',/
     &  ' length corresponding to those hits.  The dE/dx itself is',/
     &  ' simulated by finding the true particle type, using the',/
     &  ' routine KBESTM in ALPHA, and using its mass to derive the',/
     &  ' velocity of the particle.  This WILL NOT be correct for',/
     &  ' tracks which decay in flight or overlap with other tracks,',/
     &  ' so beware.  QDEDXK is called to translate the velocity',/
     &  ' and the number of hits and track length into a mean dE/dx',/
     &  ' and a resolution, using database constants for run ',I8,','/
     &  ' which have been fit to data.  A gaussian random number',/
     &  ' then is used to generate an output dE/dx distributed',/
     &  ' around the expected mean with a width equal to the expected',/
     &  ' resolution.  Keep in mind that this simulation is ',/
     &  ' simplistic.  It will be better to use the dE/dx straight',/
     &  ' from TPCSIM once that program is adjusted to give the ',/
     &  ' correct resolution and relativistic rise (volunteers?).')
      ENDIF
C
C++   Get the mass of the true particle.  If there is no MC match,
C++   then call it a pion.
C
      IMAT= KBESTM(ITK)
      IF (IMAT.NE.0) THEN
        JTYP= KTPCOD(IMAT)
        RMCM= QCMASS(JTYP)
        Q=ABS(QCH(IMAT))
      ELSE
        RMCM= QPMASS('pi+')
        Q=0.
      ENDIF
      IF (RMCM.EQ.0.) RMCM= QPMASS('pi+')
      IF (Q.EQ.0.) Q=1.
C
C++   Compare the measured dE/dx with the expected dE/dx
C
      CALL TIDHYP(KTN(IT1),QMFLD,1,RMCM,Q,RI,NS,TL,RIEX1,SIGM1,IER)
      IF (IER.NE.0) GO TO 999
      R5=(RI-RIEX1)/SIGM1
C
C++   In case TPCSIM+JULIA put this particle way out on the
C++   tail of the dE/dx, then keep the TPCSIM dE/dx, since the
C++   fake method here will not simulate the tails.  Otherwise
C++   get the dE/dx by smearing by a gaussian.  Don't simulate
C++   the tails past RPLIM here; that is done above.
C
      IF (ABS(R5).LT.RPLIM) THEN
        DX=TL/FLOAT(NS)
C
C++   Due to the problems described above, the number of samples must be
C++   corrected for some 1992 MC data.
C
        IF(WIRBUG)THEN
            XSAMPL = NS
            IF(XSAMPL.GT.XLO .AND. XSAMPL.LT.XHI)THEN
                IF(XSAMPL .LE. XBK)THEN
                    XSAMPL = SCALE_LO * (XSAMPL - XLO) + XLO
                ELSE
                    XSAMPL = XHI - (XHI - XSAMPL)/SCALE_HI
                ENDIF
            ENDIF
            NS = INT(XSAMPL + 0.5)
        ENDIF
C DWC add to make routine always produce same answer for a given track
      SDEN = FLOAT(KRUN)+FLOAT(KEVT)
      IF (SDEN.EQ.0.)  SDEN=FLOAT(KRUN)
      SEED = FLOAT(KTN(ITK))/SDEN
      CALL RDMIN(SEED)
      CALL RANNOR(XRAN,YRAN)
 8450   CONTINUE
          CALL QDEDXK(IRUN,QP(ITK),Q,RMCM,DX,NS,RI,RITRU,SGTRU)
          R5=(RI-RITRU)/(SGTRU*RITRU)
        IF (ABS(R5).GT.RPLIM) GO TO 8450
        DO 8460 I=1,N
          BG=QP(ITK)/RMASS(I)
          RIEXP(I)=QKBLOK(IRUN,BG,Q)
          SIGMA(I)=SGTRU*RIEXP(I)
 8460   CONTINUE
      ELSE
        CALL TIDHYP(KTN(IT1),QMFLD,N,RMASS,QH,RI,NS,TL,RIEXP,SIGMA,IER)
        IF (IER.NE.0) GO TO 999
      ENDIF
C
  999 CONTINUE
      RETURN
      ENTRY QMTAIL(TAIL)
C
C++   Call this entry to set the parameter which determines how much
C++   of the "tail" produced by TPCSIM will be kept.  This is only
C++   relevant for Monte Carlo.
C
      IF(XNANO)THEN
        CALL NMTAIL(TAIL)
      ELSE
        IF (ABS(TAIL).LT.TLMN) THEN
          RPLIM=TLMN
        ELSE
          RPLIM=ABS(TAIL)
        ENDIF
      ENDIF
      RETURN
      END
#endif
