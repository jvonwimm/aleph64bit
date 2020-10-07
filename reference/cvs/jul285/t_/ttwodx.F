      SUBROUTINE TTWODX(IER)
C----------------------------------------------------------------------
C!  - Calculate dE/dx for overlapping TPC tracks
C!
C!   Author   :- D.Cinabro             30-NOV-1989
C!   Modified :- R.Johnson             12-APR-1991
C!               fixed bug in addition of non-overlapping samples and
C!               changed truncation mean scheme to match that in TRKELS
C!               G.Taylor               9-SEP-1991
C!               fixed many bugs-->1st tested, working version
C!
C!   Inputs: TGFT,TWTB,TWOL
C!
C!   Outputs: T2XS
C!
C!   Libraries required: BOS,JULIA
C!
C!   Description
C!   ===========
C!   Calculate the truncated mean for TPC tracks which overlap with
C!   one other TPC track.  The output is stored in T2XS.
C?
C!======================================================================
#ifndef DOC
C
#include "bcs.h"
#include "tflags.h"
#include "tpgpar.h"
#include "tgftjj.h"
#include "twirct.h"
#include "twircl.h"
#include "tpgeow.h"
#include "twtbjj.h"
#include "twoinc.h"
#include "t2xsjj.h"
#include "twoljj.h"
C
      DIMENSION NCLST(LTSECT),SPLST(LTSECT),SZLST(LTSECT)
      DIMENSION SLLST(LTSECT),NHTOT(LTSECT)
      LOGICAL FIRST,BANK
      DATA FIRST /.TRUE./
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        NTGFT = NAMIND('TGFT')
        NTWTB = NAMIND('TWTB')
        NTWOL = NAMIND('TWOL')
        NT2XS = NAMIND('T2XS')
        ITSORT = 0
      ENDIF
C
      KTGFT = IW(NTGFT)
      KTWTB = IW(NTWTB)
      KTWOL = IW(NTWOL)
      IF (KTGFT.EQ.0.OR.KTWTB.EQ.0.OR.KTWOL.EQ.0) GOTO 999
C
C Get rid of the output bank if it is still  hanging around
C
      KT2XS = IW(NT2XS)
      IF (KT2XS.NE.0) CALL BDROP(IW,'T2XS')
C
C Loop over all track pairs in TWOL
C
      BANK = .FALSE.
      CINT=1.0-SLOPCL*ALOG(TWSTEP(1))
      NENT = 0
      DO 10 IPR = 1,LROWS(KTWOL)
        ITK1 = ITABL(KTWOL,IPR,JTWOT1)
        ITK2 = ITABL(KTWOL,IPR,JTWOT2)
C
C Make a workbank of the overlapping hits
C
        CALL TTWOSH(ITK1,ITK2)
        IF (IRELHT.EQ.0) GOTO 10
C
C These two tracks have enough shared hits to make the calculation
C of double track dE/dx worthwhile.
C
C So I have the list of shared pulses and logical list that tells me
C in which sectors there are shared pulses.
C OK I have all the lists I need now start summing.  The following
C is copied as faithfully as possible from TRKELS.
C
C Create a workbank for sorting the dE/dx samples
C
        NSAMP = LROWS(IRELHT)
        CALL WBANK(IW,ITSORT,2*NSAMP,*998)
        IW(ITSORT-3) = INTCHA('WTST')
C
C The first time through a named bank must be created for the output.
C
        IF (.NOT.BANK) THEN
          BANK = .TRUE.
          LEN = LMHLEN + 7*LROWS(KTGFT)
          CALL AUBOS('T2XS',0,LEN,KT2XS,IGARB)
          IF (IGARB.EQ.2) GOTO 997
          KTGFT = IW(NTGFT)
          KTWTB = IW(NTWTB)
          KTWOL = IW(NTWOL)
          IW(KT2XS+LMHCOL) = 7
          IW(KT2XS+LMHROW) = LROWS(KTGFT)
        ENDIF
C
C Make a list for sorting the dE/dx values.
C Keep track of the number of values in each sector and the total
C track length.  Apply the same corrections for sample length as
C in TRKELS.
C
        SMP0 = RTABL(KTWTB,ITABL(IRELHT,1,1),JTWTSL)
        SMPL = ALOG(SMP0)
        CALL VZERO(NHTOT(1),LTSECT)
        DO 20 IWR = 1,LROWS(IRELHT)
          ITW=ITABL(IRELHT,IWR,1)
          SMP = RTABL(KTWTB,ITW,JTWTSL)
          RNCR = (SMP-SMP0)/SMP0
          IF (ABS(RNCR).LT.RNCRTW) THEN
            SMPL = SMPL + (1. - 0.5*RNCR)*RNCR
          ELSE
            SMPL = ALOG(SMP)
          ENDIF
          DEDX = RTABL(KTWTB,ITABL(IRELHT,IWR,1),JTWTCE)/
     &            SMP/(CINT+SLOPCL*SMPL)
          IDD=ITABL(KTWTB,ITW,JTWTWI)
          ISLOT=IDD/65536
C
C++       Make a non-linear correction for sample length
C
          ZTK=RTABL(KTWTB,ITW,JTWTPN)
          Q=RTABL(KTWTB,ITW,JTWTCE)/(CINT+SLOPCL*SMPL)
          IF (GRNMCL.GT.0.) THEN
            QCNL=GRNMCL*(Q/SMP + (1.-(TWSTEP(1)/SMP))*RNLNCL)
     &                                           / (1.+ADSPCL*ZTK)
          ELSE
            QCNL=Q/SMP
          ENDIF
          RW(ITSORT+IWR)=QCNL
          NHTOT(ISLOT)=NHTOT(ISLOT)+1
          SMP0=SMP
   20   CONTINUE
C
C Sort the dE/dx values
C
        CALL SORTZV(RW(ITSORT+1),IW(ITSORT+NSAMP+1),NSAMP,1,0,0)
C
C Calculate the truncated sum of dE/dx and the average drift
C separately for each sector.
C
        CALL VZERO(NCLST(1),LTSECT)
        CALL VZERO(SZLST(1),LTSECT)
        CALL VZERO(SPLST(1),LTSECT)
        CALL VZERO(SLLST(1),LTSECT)
        NTRNK=NSAMP*JTRNCL/100
        IF (FSRTDX) THEN
          DO 368 ISLOT=1,LTSECT
            NHTOT(ISLOT)=NHTOT(ISLOT)*JTRNCL/100
  368     CONTINUE
          NLOOP=NSAMP
        ELSE
          NLOOP=NSAMP*JTRNCL/100
        ENDIF
        DO 40 K=1,NLOOP
            IPT=IW(ITSORT+NSAMP+K)
            ITW=ITABL(IRELHT,IPT,1)
            IDD=ITABL(KTWTB,ITW,JTWTWI)
            ISLOT=IDD/65536
            IF (FSRTDX) THEN
              IF (NCLST(ISLOT).GE.NHTOT(ISLOT)) GO TO 40
            ENDIF
            ZPUL=RTABL(KTWTB,ITW,JTWTPN)
            NCLST(ISLOT)=NCLST(ISLOT) + 1
            SPLST(ISLOT)=SPLST(ISLOT) + RW(ITSORT+IPT)
            SZLST(ISLOT)=SZLST(ISLOT) + ZPUL
            SLLST(ISLOT)=SLLST(ISLOT) + RTABL(KTWTB,ITW,JTWTSL)
   40   CONTINUE
C
C An entry into PT2X
C
        NSECT = 0
        DO 50 ISLOT=1,LTSECT
          IF (NCLST(ISLOT).EQ.0) GO TO 50
          NENT = NENT + 1
C
C Extend T2XS if it might run out of space
C
          IF (LROWS(KT2XS).EQ.NENT) THEN
            LEN= IW(KT2XS) + 5*7
            CALL AUBOS('T2XS',0,LEN,KTEXS,IGARB)
            IF (IGARB.EQ.2) GO TO 997
            IF (IGARB.NE.0) THEN
              KTGFT = IW(NTGFT)
              KTWTB = IW(NTWTB)
              KTWOL = IW(NTWOL)
            ENDIF
          ENDIF
          NSECT=NSECT+1
C
          RTRNK=FLOAT(NCLST(ISLOT))
          IW(KROW(KT2XS,NENT)+JT2XSI)=ISLOT
          RW(KROW(KT2XS,NENT)+JT2XTM)=SPLST(ISLOT)/RTRNK
          RW(KROW(KT2XS,NENT)+JT2XTL)=SLLST(ISLOT)
          IW(KROW(KT2XS,NENT)+JT2XNS)=NCLST(ISLOT)
          RW(KROW(KT2XS,NENT)+JT2XAD)=SZLST(ISLOT)/RTRNK
          IW(KROW(KT2XS,NENT)+JT2XT1)=ITK1
          IW(KROW(KT2XS,NENT)+JT2XT2)=ITK2
   50   CONTINUE
C
C Drop the workbanks for this track pair
C
        CALL WDROP(IW,IRELHT)
        CALL WDROP(IW,ITSORT)
   10 CONTINUE
C
C Drop TWOL
C
      CALL BDROP(IW,'TWOL')
C
C Set size of T2XS
C
      IF (NENT.NE.0) THEN
        IW(KT2XS+LMHROW) = NENT
        CALL AUBPRS('T2XS')
      ENDIF
      IER=0
      GOTO 999
C
  997 CONTINUE
      CALL RERROR('TTWODX',1,'No room in BOS for named bank')
      IER=1
      GOTO 999
C
  998 CONTINUE
      CALL RERROR('TTWODX',2,'No room in BOS for workbank')
      IER=2
C
  999 RETURN
      END
#endif
