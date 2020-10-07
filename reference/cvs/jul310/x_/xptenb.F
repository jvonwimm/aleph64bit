      LOGICAL FUNCTION XPTENB(IRUN)
C----------------------------------------------------------------------
C!  - TRUE if principal physics and bhabha triggers are enabled
C!
C!   Author   :- J.Wear                  22-MAR-1990
C! Modified   : P. Comas      940502
C!           Use of X1RG instead of XTOP dropped in the readout upgrade.
C!
C!   Inputs:  IRUN -- Run number
C!        -
C!   Outputs: XPTENB -- logical variable
C!        -
C!   Libraries required:
C!
C!   Description
C!   ===========  If principal physics triggers and bhabha
C!   triggers are enabled XPTENB returns true.  For use in determining
C!   whether to count runs/events in the "ideal case" luminosity
C!   determination.  Each running period has distinct trigger names for
C!   these required triggers, so each period must have its own list.
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rcurnt.h"
#include "xtbnjj.h"
#include "xtcnjj.h"
#include "xtopjj.h"
#include "x1rgjj.h"
      PARAMETER (MXBIT=100)
      PARAMETER (JSEP89=1,JFAL89=2,JMAR90=3,JMAR92=4,JJUN96=5,NPER=5)
      CHARACTER*4 CHAINT
      CHARACTER*8 MNM,TTREQ(NPER,MXBIT)
      DIMENSION ICH(2)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
#include "bmacro.h"
C
      IF (FMCRUN) THEN
        XPTENB = .TRUE.
        GOTO 999
      ENDIF
C
C Load run-period specific trigger names into storage arrays
C
      NAXTBN=NAMIND('XTBN')
      KXTBN=IW(NAXTBN)
      NAXTCN=NAMIND('XTCN')
      KXTCN=IW(NAXTCN)
      NAXTOP=NAMIND('XTOP')
      KXTOP=IW(NAXTOP)
      NAX1RG=NAMIND('X1RG')
      KX1RG=IW(NAX1RG)
      IF(KXTBN.NE.0)THEN
        NBIT=LROWS(KXTBN)
        IF (FIRST) THEN
          FIRST = .FALSE.
          DO 91 IPER = 1,NPER
            DO 92 ITBIT = 1,NBIT
              TTREQ(IPER,ITBIT) = ' '
 92         CONTINUE
 91       CONTINUE
          DO 10 IPER = 1,NPER
            TTREQ(IPER,1) = 'SNG_MUON'
            TTREQ(IPER,2) = 'SNG_C_EM'
            TTREQ(IPER,3) = 'ETT_EWBA'
            TTREQ(IPER,5) = 'ETT_EWEA'
            TTREQ(IPER,6) = 'ETT_EWEB'
 10       CONTINUE
C          TTREQ(JSEP89,4) = 'ETT_ETE*' ! Screwup in current database.
          TTREQ(JSEP89,4) = 'ETT_EWE*'
          TTREQ(JFAL89,4) = 'ETT_EWE*'
          TTREQ(JMAR90,4) = 'ETT_EWE*'
          TTREQ(JMAR92,4) = 'ETT_EWE*'
          TTREQ(JJUN96,4) = 'ETT_EWE*'
C
C Bhabha coincidence triggers
C
          TTREQ(JSEP89,7) = 'LC_HI_LO'
          TTREQ(JFAL89,7) = 'LC_HI_LO'
          TTREQ(JMAR90,7) = 'LT_HI_LO'
          TTREQ(JSEP89,8) = 'LC_LO_HI'
          TTREQ(JFAL89,8) = 'LC_LO_HI'
          TTREQ(JMAR90,8) = 'LT_LO_HI'
          TTREQ(JMAR92,7) = 'LW_ET_HI'
          TTREQ(JMAR92,8) = 'LW_A+BVH'
          TTREQ(JJUN96,7) = 'LW_LO_HI'
          TTREQ(JJUN96,8) = 'LW_A+BVH'
        ENDIF
C
        IF (IRUN.LE.4535) THEN
          KPER = JSEP89
         ELSEIF ((IRUN.GT.4535).AND.(IRUN.LE.5909)) THEN
          KPER = JFAL89
         ELSEIF (IRUN.LT.14000) THEN
          KPER = JMAR90
         ELSEIF (IRUN.LT.41000) THEN
          KPER = JMAR92
         ELSE
          KPER = JJUN96
        ENDIF
C
C Count number of required physics and luminosity triggers for this run
C
        DO 31 ITBIT = 1,NBIT
 31       IF (TTREQ(KPER,ITBIT).EQ.' ') GOTO 311
 311    NTREQ = ITBIT - 1
C
C Now check to see if all these triggers are enabled.
C Compare bit assignment in XTBN with bits turned on in XTCN, word 8.
C
        DO 100 ITREQ = 1,NTREQ
          DO 110 ITBIT = 1,NBIT
            IDEF=ITABL(KXTBN,ITBIT,JXTBTN)
            INUM=ITABL(KXTBN,ITBIT,JXTBTB)
C
C XTBN is corrupted so we perform some major league hardwiring fixes
            IF(INUM.LT.0.OR.INUM.GT.31) THEN
              KXTBN=0
              IF(IRUN.LE.5909)THEN
                MKREQ=51561216
               ELSE
                MKREQ=2016000
              ENDIF
              GOTO 888
            ENDIF
C
            IF(IDEF.NE.0.AND.INUM.NE.31)THEN
              ICH(1) =ITABL(KXTBN,ITBIT,JXTBBM)
              ICH(2) =ITABL(KXTBN,ITBIT,JXTBBM+1)
              MNM(1:4) = CHAINT(ICH(1))
              MNM(5:8) = CHAINT(ICH(2))
              IF(MNM.EQ.TTREQ(KPER,ITREQ)) MKREQ=IBSET(MKREQ,INUM)
            ENDIF
 110      CONTINUE
 100    CONTINUE
      ENDIF
C
 888  CONTINUE
C
C Do bit-wise AND of Trigger Enable mask and Required Physics mask
C Enable mask moved in March `90 to XTOP bank.
C Check if physics triggers enabled equal the required mask.
C
      MPENB = 0
      IF((IRUN.LE.5909).AND.(KXTCN.NE.0))
     &   MPENB = IAND(MKREQ,ITABL(KXTCN,1,JXTCTR+1))
      IF((IRUN.GT.5909).AND.(KXTOP.NE.0))
     &   MPENB = IAND(MKREQ,ITABL(KXTOP,1,JXTOTR))
      IF((IRUN.GT.25000).AND.(KX1RG.NE.0))
     &   MPENB = IAND(MKREQ,ITABL(KX1RG,1,JX1RCO+2))

C
      XPTENB = (MPENB.EQ.MKREQ)
C
 999  RETURN
      END
#endif
