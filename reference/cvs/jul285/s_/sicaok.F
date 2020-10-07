      LOGICAL FUNCTION SICAOK(KRUN)
C----------------------------------------------------------------------
C!  - Checks HV bits and Trigger enabled for Sical analysis
C!
C!
C!    Author:     B. Bloch-Devaux   11-May-1993
C!          Check X1RG if XTOP missing - April 29 , 1994
C!
C!    Input:      - KRUN : current Run number
C!
C!    Output:     SICAOK is TRUE if all conditions ok
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
      LOGICAL XTGOK,XHVOK,XHV(16),XHVE,XHVTR,XHVH
      INTEGER GTSTUP
      EXTERNAL GTSTUP
      CHARACTER*8 MNM,TTREQ(8)
      CHARACTER*4 CHAINT
      DIMENSION ICH(2)
#include "xtbnjj.h"
#include "xtcnjj.h"
#include "xtopjj.h"
#include "x1rgjj.h"
      PARAMETER (NBIT=32)
      DATA LRUN /-9999/
      DATA IFIR / 0/
#include "bmacro.h"
C
      IF ( IFIR.EQ.0) THEN
        IFIR = 1
        ISITP = GTSTUP('SI',KRUN)
        NAXTCN = NAMIND('XTCN')
        NAXTBN = NAMIND('XTBN')
        NAXTOP = NAMIND('XTOP')
      NAX1RG = NAMIND('X1RG')
C
C Load run-period specific trigger names into storage arrays
C
        TTREQ(1) = 'SNG_MUON'
        TTREQ(2) = 'SNG_C_EM'
        TTREQ(3) = 'ETT_EWBA'
        TTREQ(4) = 'ETT_EWE*'
        TTREQ(5) = 'ETT_EWEA'
        TTREQ(6) = 'ETT_EWEB'
        TTREQ(7) = 'SiCAL_ME'
        TTREQ(8) = 'SICAL_ME'
        NTREQ = 8
      ENDIF
C
      SICAOK = .FALSE.
C   Monte Carlo
      IF (KRUN.LE.2000   ) THEN
         IF ( ISITP.GT.0 )  SICAOK = .TRUE.
         GOTO 999
      ENDIF
C
C Check that physics triggers are all enabled for ideal HV luminosity
C Only check when the run changes.Update also setup code
      IF(LRUN.NE.KRUN)THEN
        LRUN  = KRUN
        ISITP = GTSTUP('SI',KRUN)
        XTGOK  = .FALSE.
        IERR = 0
        KXTBN=IW(NAXTBN)
        KXTOP=IW(NAXTOP)
      KX1RG = IW(NAX1RG)
C
C Now check to see if all these triggers are enabled.
C Build mask of required bits from their name-->bit assignment in XTBN.
C
      MKREQ = 0
      IF (KXTBN.NE.0) THEN
        DO 100 ITREQ = 1,NTREQ
          DO 110 ITBIT = 1,NBIT
            IDEF=ITABL(KXTBN,ITBIT,JXTBTN)
            INUM=ITABL(KXTBN,ITBIT,JXTBTB)
C
C XTBN is corrupted so we perform some major league hardwiring fixes
            IF(INUM.LT.0.OR.INUM.GT.31) THEN
              IERR = 1
              GOTO 888
            ENDIF
C
            IF(IDEF.NE.0.AND.INUM.NE.31)THEN
              ICH(1) =ITABL(KXTBN,ITBIT,JXTBBM)
              ICH(2) =ITABL(KXTBN,ITBIT,JXTBBM+1)
              MNM(1:4) = CHAINT(ICH(1))
              MNM(5:8) = CHAINT(ICH(2))
              IF(MNM.EQ.TTREQ(ITREQ)) MKREQ=IBSET(MKREQ,INUM)
            ENDIF
  110     CONTINUE
  100   CONTINUE
        IF (MKREQ.EQ.0) IERR = 1
       ELSE
C
C No XTBN bank at all -- use hardwire masks
        IERR = 1
      ENDIF
C
  888 CONTINUE
C
C Route all error conditions through hardwiring of required masks
      IF (IERR.NE.0) THEN
          MKREQ=1966864
      ENDIF
C
      MKENB = 0
C
C Use XTOP, word 6 of run header to get the trigger enabled mask.
      IF (KXTOP.NE.0) MKENB = ITABL(KXTOP,1,JXTOTR)
      IF (KX1RG.GT.0) MKENB = ITABL(KX1RG,1,JX1RCO+2)
C
C Do bit-wise AND of Trigger Enable mask and Required Physics mask
C Check if physics triggers enabled equal the required physics mask.
      MPENB = IAND(MKREQ,MKENB)
      XTGOK  = (MPENB.EQ.MKREQ)
      ENDIF
C
C HV bit pattern from XTCN bank
      JHVST = 0
      JXTCN = IW(NAXTCN)
      IF (JXTCN .NE. 0) JHVST = ITABL(JXTCN,1,JXTCTR)
      DO 10 IBIT = 1,16
        XHV(IBIT) = BTEST(JHVST,(IBIT-1))
   10 CONTINUE
      XHVE  = XHV(1).AND.XHV(2).AND.XHV(3).AND.XHV(4).AND.XHV(11)
      XHVTR = XHV(16).AND.XHV(6)
      XHVH  = XHV(8).AND.XHV(9).AND.XHV(10).AND.XHV(14).AND.XHV(15)
      XHVOK = XHVE.AND.XHVTR.AND.XHVH
C
      SICAOK = XTGOK.AND.XHVOK
 999  RETURN
      END
#endif
