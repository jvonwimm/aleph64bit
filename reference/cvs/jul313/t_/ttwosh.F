      SUBROUTINE TTWOSH(IT1,IT2)
C----------------------------------------------------------------------
C!  - Calculate number of wire hits shared by two tracks
C!
C!   Author   :- D.Cinabro             30-NOV-1989
C!
C!   Inputs: IT1 /I One TGFT track number
C!           IT2 /I A different TGFT track number
C!
C!   Outputs: Via workbank existence or not.
C!
C!   Libraries required: BOS
C!
C!   Description
C!   ===========
C!    Given two TGFT track numbers calculate the number of wire
C!    hits shared by the two.  Store the pointers to these hits
C!    in a workbank if there are more than MNSPTW many otherwise make
C!    no bank.
C?
C!======================================================================
#ifndef DOC
C
#include "bcs.h"
#include "twitjj.h"
#include "twatjj.h"
#include "twirct.h"
#include "twtbjj.h"
#include "twoinc.h"
C
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        NTWIT = NAMIND('TWIT')
        NTWAT = NAMIND('TWAT')
        NTWTB = NAMIND('TWTB')
        IRELHT = 0
      ENDIF
C
C Banks must be here to continue
C
      KTWIT = IW(NTWIT)
      KTWAT = IW(NTWAT)
      KTWTB = IW(NTWTB)
      IF (KTWIT.EQ.0.OR.KTWAT.EQ.0.OR.KTWTB.EQ.0) GOTO 999
C
C Zero out the flag array
C
      DO 1 ISEC = 1,36
    1   TWOSEC(ISEC) = .FALSE.
C
C Bad hits associated to the first track?
C
      NB1 = ITABL(KTWIT,IT1,JTWINB)
C
C If not out, if so bad hits associated to the second track?
C
      IF (NB1.EQ.0) GOTO 999
      NB2 = ITABL(KTWIT,IT2,JTWINB)
C
C If not out, if so begin to loop.
C
      IF (NB2.EQ.0) GOTO 999
      NG1 = ITABL(KTWIT,IT1,JTWING)
      NG2 = ITABL(KTWIT,IT2,JTWING)
      NREL = 0
      DO 10 IH1 = 1,NB1
C
C Loop over the bad hits of the first track.
C
        IHT1 = IW(KTWAT+LMHLEN+ITABL(KTWIT,IT1,JTWIOS)+NG1+IH1)
        DO 9 IH2 = 1,NB2
C
C Loop over the bad hits of the second track.
C
          IHT2 = IW(KTWAT+LMHLEN+ITABL(KTWIT,IT2,JTWIOS)+NG2+IH2)
          IF (IHT1.EQ.IHT2) THEN
            NREL = NREL + 1
C
C The first match book the work bank.
C
            IF (NREL.EQ.1) THEN
              LEN = LMHLEN + NB1 + NB2
              CALL WBANK(IW,IRELHT,LEN,*998)
              IW(IRELHT+LMHROW) = NB1 + NB2
              IW(IRELHT+LMHCOL) = 1
              IW(IRELHT-3) = INTCHA('WTWO')
            ENDIF
C
C If they are the same store them in the work bank.
C
            IW(KROW(IRELHT,NREL)+1) = IHT1
            ISEC = ITABL(KTWTB,IHT1,JTWTWI)/65536
            TWOSEC(ISEC) = .TRUE.
            GOTO 10
          ENDIF
    9   CONTINUE
   10 CONTINUE
C
C Done.  If less than MINSPTW share hits drop the work bank and return.
C
      IF (NREL.LT.MNSPTW) THEN
        CALL WDROP(IW,IRELHT)
        GOTO 999
      ENDIF
C
C Otherwise compress the workbank and return.
C
      IW(IRELHT+LMHROW) = NREL
      LEN = LMHLEN + NREL
      CALL WBANK(IW,IRELHT,LEN,*998)
      GOTO 999
C
C Not enough space for the workbank.  Bug out.
C
  998 CALL RERROR('TTWOSH',1,'No room in BOS for workbank')
      CALL WDROP(IW,IRELHT)
C
  999 RETURN
      END
#endif
