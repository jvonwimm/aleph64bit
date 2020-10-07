      SUBROUTINE TTWOSI(IT1,IT2)
C----------------------------------------------------------------------
C!  - Calculate number of unshared wire hits of two tracks on the same w
C!
C!   Author   :- D.Cinabro             2-DEC-1989
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
C!    hits on the same wire of the two tracks.  Store the pointers to
C!    these hits in a workbank.
C?
C!======================================================================
#ifndef DOC
C
#include "bcs.h"
#include "twitjj.h"
#include "twatjj.h"
#include "twirct.h"
#include "twtbjj.h"
#include "twrrjj.h"
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
        NTWRR = NAMIND('TWRR')
        IRELSI = 0
      ENDIF
C
C Banks must be here to continue
C
      KTWIT = IW(NTWIT)
      KTWAT = IW(NTWAT)
      KTWTB = IW(NTWTB)
      KTWRR = IW(NTWRR)
      IF (KTWIT.EQ.0.OR.KTWAT.EQ.0.OR.KTWTB.EQ.0.OR.
     &    KTWRR.EQ.0) GOTO 999
C
C Good hits associated to the first track?
C
      NG1 = ITABL(KTWIT,IT1,JTWING)
C
C If not out, if so good hits associated to the second track?
C
      IF (NG1.EQ.0) GOTO 999
      NG2 = ITABL(KTWIT,IT2,JTWING)
C
C If not out, if so begin to loop.
C
      IF (NG2.EQ.0) GOTO 999
      NREL = 0
C
C Loop over sectors with shared hits
C
      DO 10 ISEC = 1,36
        IF (.NOT.TWOSEC(ISEC)) GOTO 10
C
C Loop over good hits on track 1 in this sector
C
        DO 9 IH1 = 1,NG1
          IHT1 = IW(KTWAT+LMHLEN+ITABL(KTWIT,IT1,JTWIOS)+IH1)
          ISEC1 = ITABL(KTWTB,IHT1,JTWTWI)/65536
          IF (ISEC1.NE.ISEC) GOTO 9
C
C Get the wire that is hit
C
          IPT = ITABL(KTWTB,IHT1,JTWTWI) - ISEC*65536
          IWIRP = ITABL(KTWRR,IPT,JTWRWH)
          IWIR1 = IBITS(IWIRP,24,8)
C
C Now the same for track 2
C
          DO 8 IH2 = 1,NG2
            IHT2 = IW(KTWAT+LMHLEN+ITABL(KTWIT,IT2,JTWIOS)+IH2)
            ISEC2 = ITABL(KTWTB,IHT2,JTWTWI)/65536
            IF (ISEC2.NE.ISEC) GOTO 8
            IPT = ITABL(KTWTB,IHT2,JTWTWI) - ISEC*65536
            IWIRP = ITABL(KTWRR,IPT,JTWRWH)
            IWIR2 = IBITS(IWIRP,24,8)
C
C See if it is the same wire.
C
            IF (IWIR2.EQ.IWIR1) THEN
              NREL = NREL + 1
C
C The first match book the work bank.

              IF (NREL.EQ.1) THEN
                LEN = LMHLEN + 2*(NG1+NG2)
                CALL WBANK(IW,IRELSI,LEN,*998)
                IW(IRELSI+LMHROW) = NG1 + NG2
                IW(IRELSI+LMHCOL) = 2
                IW(IRELSI-3) = INTCHA('WTSI')
              ENDIF
C
C Store them in the work bank.
C
              IW(KROW(IRELSI,NREL)+1) = IHT1
              IW(KROW(IRELSI,NREL)+2) = IHT2
              GOTO 9
            ENDIF
    8     CONTINUE
    9   CONTINUE
   10 CONTINUE
C
C Done.  If the bank exists commpress and return
C
      IF (IRELSI.NE.0) THEN
        IW(IRELSI+LMHROW) = NREL
        LEN = LMHLEN + 2*NREL
        CALL WBANK(IW,IRELSI,LEN,*998)
        GOTO 999
      ENDIF
      RETURN
C
C Not enough space for the workbank.  Bug out.
C
  998 CALL RERROR('TTWOSI',1,'No room in BOS for workbank')
      CALL WDROP(IW,IRELSI)
C
  999 RETURN
      END
#endif
