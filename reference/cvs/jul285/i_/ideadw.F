      SUBROUTINE IDEADW
C-----------------------------------------------------------------------
C! Simulate dead wires/channels in Monte Carlo data
C!
C!   Author          :-  J. Sedgbeer  89/06/05
C!   Modified        :-  J. Sedgbeer  89/07/28
C!   Modified        :-  J.Sedgbeer 90/05/23 Allow for IHIT M.C. Bank
C!   Modified        :-  J.Sedgbeer 91/01/07 Ensure IWST bank exists.
C!
C!   Input:
C!                INOZ     bank (card)
C!    params.:    IWSTJJ   for IWST bank
C!                IDIGJP   for IDIG bank
C!                ITHTJJ   for ITHT bank
C!                IHITJJ   for IHIT bank
C!                IDHRJJ   for IDHR bank
C!                ALCONS
C!    commons:    /BCS/    for banks  IWST, IDIG, ITHT, IDHR
C!                /RCURNT/ for run number
C!
C!   Output:      Modified IDIG bank
C!
C!   calls     : AGETDB (Alephlib)
C!
C!   libraries:  BOS,CERNLIB
C!
C!   Description:
C! Simulate dead wires/channels in Monte Carlo data.
C! If INOZ card exists then kill all Z digitisings in ITC.
C! Get wire status bank IWST. Use this to adjust/kill some digitisings
C! to simulate real data.
C! If no valid bank then no dead wire map assumed.
C!
C? Get IDIG bank - if missing return
C? Get IHIT (or ITHT) and IDHR banks - if missing return
C?                                        (assures Monte carlo)
C? If INOZ bank (card) exists then
C?   Loop over all digits in IDIG
C?     Kill Z digitising
C?   Endloop
C? endif
C? Get IWST bank.
C? If no IWST bank then return
C? Loop over dead wires in IWST
C?   Loop over digits in IDIG
C?     if wire number matches then adjust contents of IDIG
C?   endloop
C? endloop
C-----------------------------------------------------------------------
#ifndef DOC
C I/O commons etc.
#include "bcs.h"
#include "rcurnt.h"
#include "iwstjj.h"
#include "idigjp.h"
#include "ihitjj.h"
#include "ithtjj.h"
#include "idhrjj.h"
#include "alcons.h"
C-----------------------------------------------------------------------
      EXTERNAL NAMIND,NLINK
      INTEGER  NAMIND,NLINK
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
C Check for non-empty IDIG bank
C
      KIDIG = IW(NAMIND('IDIG'))
      IF(KIDIG.LE.0) GOTO 999
      NDIG = LROWS(KIDIG)
      IF(NDIG.LE.0) GOTO 999
C
C Get MC banks IHIT (or ITHT) and IDHR
C
      KIHIT = IW(NAMIND('IHIT'))
      KITHT = IW(NAMIND('ITHT'))
      KIDHR = IW(NAMIND('IDHR'))
      IF(KIDHR.LE.0) GOTO 999
      IF(KIHIT.LE.0.AND.KITHT.LE.0) GOTO 999
C
C If 'INOZ' then kill all Z digits in IDIG.
C
      IF(NLINK('INOZ',0).GT.0) THEN
        DO 50 I=1,NDIG
          JCODE = IW(KIDIG+LMHLEN+I)
          CALL MVBITS(0,0,NBITZT,JCODE,IBITZT)
          IW(KIDIG+LMHLEN+I) = JCODE
   50   CONTINUE
      ENDIF
C
C Check for non-empty IWST bank
C
      KIWST = IW(NAMIND('IWST'))
      IF(KIWST.LE.0) GOTO 999
      NWST = LROWS(KIWST)
      IF(NWST.LE.0) GOTO 999
C
C Use contents of IWST to set dead wires/channels in IDIG bank.
C
      DO 200 I=1,NWST
        IWIRE = ITABL(KIWST,I,JIWSIW)
        IFLAG = ITABL(KIWST,I,JIWSFL)
C
C Loop over digis. Search for wire number match. If digit wire
C number is > IWIRE then end search (IDIG is ordered by increasing wire)
C
        J = 0
  100   J = J+1
        IF(J.GT.NDIG) GOTO 200
        JCODE = IW(KIDIG+LMHLEN+J)
        JWIRE = IBITS(JCODE,IBITWN,NBITWN)
        IF(JWIRE.GT.IWIRE) GOTO 200
        IF(JWIRE.LT.IWIRE) GOTO 100
C
C Act on wire status flag:
C  flag = 1   wire dead ; r-phi and Z dead
C         2   B-end dead; Z dead
C         3   A-end dead; Z dead, r-phi from B-end
C         4   r-phi TDC dead; r-phi dead
C
C Set Z digitising zero
        IF(IFLAG.GE.1.AND.IFLAG.LE.3)
     +           CALL MVBITS(0,0,NBITZT,JCODE,IBITZT)
C
C Set R-Phi digitising zero
        IF(IFLAG.EQ.1.OR.IFLAG.EQ.4)
     +           CALL MVBITS(0,0,NBITRP,JCODE,IBITRP)
C
C Adjust r-phi digitising for read-out at B-end.
C Get hit(s) which gave this r-phi digit; get Z coord of this
C hit - use this to adjust for signal time down wire.(Approximate -
C i.e. use speed of light, also TDC bin width of 0.5ns)
        IF(IFLAG.EQ.3) THEN
          ITDIG = IBITS(JCODE,IBITRP,NBITRP)
          JHITA = ITABL(KIDHR,J,JIDHHA)
          JHITB = ITABL(KIDHR,J,JIDHHB)
          IF(KIHIT.GT.0) THEN
            ZIA = RTABL(KIHIT,JHITA,JIHIZH)
            ZIB = RTABL(KIHIT,JHITB,JIHIZH)
          ELSE
            ZIA = RTABL(KITHT,JHITA,JITHZI)
            ZIB = RTABL(KITHT,JHITB,JITHZI)
          ENDIF
          CORR = -(ZIA+ZIB)/(CLGHT*0.5)
          ITDIG = ITDIG + IFIX(CORR)
          CALL MVBITS(ITDIG,0,NBITRP,JCODE,IBITRP)
        ENDIF
C
C Put altered digit. back into IDIG
        IW(KIDIG+LMHLEN+J) = JCODE
  200 CONTINUE
C
  999 CONTINUE
      END
#endif
