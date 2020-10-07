      SUBROUTINE IPSTAT
C-----------------------------------------------------------------------
C! ITC Prepare data Statistics and Histograms.
C!
C!    Author       :- J. Sedgbeer   89/02/06
C!    Last Modified:- J. Sedgbeer   89/06/05
C!
C!    Input:
C!      commons:       /BCS/ => banks IDIG and ITCO
C!                     /RCURNT/  run and event number
C!      params.:       IDIGJP
C!
C!    Output:
C!      commons:       /IPDSTC/ Prepare data statistics updated.
C!
C!    calls     : none
C!
C!    Libraries required: BOS,CERNLIB
C!
C! IPSTAT.  ITC prepare statistics - update common /IPDSTC/.
C!    Histograms for the IDIG bank (Done here and not in ITHIST
C!    as the raw data banks are dropped after the prepare data phase.
C-----------------------------------------------------------------------
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
#include "rlunit.h"
C-----------------------------------------------------------------------
C I/O commons and parameters
#include "bcs.h"
#include "ipdstc.h"
#include "idigjp.h"
#include "rcurnt.h"
      INTEGER ILST(8)
      EXTERNAL NAMIND
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
      NDIG = 0
      NCO  = 0
      DO 10 I=1,8
        ILST(I) = 0
   10 CONTINUE
      JOFF = JULIT*1000
      ISTAIP(1) = ISTAIP(1) + 1
C
C Check that IDIG bank exists.
C
      KIDIG = IW(NAMIND('IDIG'))
      IF(KIDIG.EQ.0) GOTO 990
      ISTAIP(2) = ISTAIP(2) + 1
      IF(IW(KIDIG).LE.0) GOTO 990
C
C Find number of useful digits.
C
      NDIG  = LROWS(KIDIG)
      XNDIG = FLOAT(NDIG)
      IF(JHISRF(JULIT).GE.2) CALL HFILL(JOFF+ 1,XNDIG,0.,1.)
      IF(NDIG.LE.0) THEN
        ISTAIP(3) = ISTAIP(3) + 1
        GOTO 990
      ENDIF
      ISTAIP(4) = ISTAIP(4) + 1
      ISTAIP(5) = ISTAIP(5) + NDIG
      IF(NDIG.GT.ISTAIP(6)) ISTAIP(6) = NDIG
      IF(NDIG.LT.ISTAIP(7)) ISTAIP(7) = NDIG
C
C Loop over all digits.
C
      DO 100 JD=1,NDIG
        IWIRE = IBITS(IW(KIDIG+LMHLEN+JD),IBITWN,NBITWN)
        IF(IWIRE.LE.384) THEN
          IL = (IWIRE-1)/96 + 1
        ELSE
          IL = (IWIRE-385)/144 + 5
        ENDIF
        ILST(IL) = ILST(IL) + 1
C
C If histos. wanted ..
C
        IF(JHISRF(JULIT).GE.2) THEN
          WIRE = FLOAT(IWIRE)
          TDC  = FLOAT(IBITS(IW(KIDIG+LMHLEN+JD),IBITRP,NBITRP))
          ZDIG = FLOAT(IBITS(IW(KIDIG+LMHLEN+JD),IBITZT,NBITZT)*2)
            IF(IWIRE.LE.384) THEN
              CALL HFILL(JOFF+ 2,WIRE,0.,1.)
            ELSE
              CALL HFILL(JOFF+ 3,WIRE,0.,1.)
            ENDIF
            CALL HFILL(JOFF+ 4, TDC,0.,1.)
            CALL HFILL(JOFF+20,ZDIG,0.,1.)
            IF(JHISRF(JULIT).GE.3) THEN
              CALL HFILL(JOFF+10+IL, TDC,0.,1.)
              CALL HFILL(JOFF+20+IL,ZDIG,0.,1.)
            ENDIF
        ENDIF
  100 CONTINUE
C
C Loop over layers .. stats. per layer.
C
      DO 200 IL=1,8
        ILSTIP(IL,1) = ILSTIP(IL,1) + ILST(IL)
        IF(ILST(IL).GT.ILSTIP(IL,2)) ILSTIP(IL,2) = ILST(IL)
        IF(ILST(IL).LT.ILSTIP(IL,3)) ILSTIP(IL,3) = ILST(IL)
  200 CONTINUE
C
C Check that ITCO bank exists.
C
      KITCO = IW(NAMIND('ITCO'))
      IF(KITCO.EQ.0) GOTO 990
      NCO = IW(KITCO+LMHROW)
      ISTAIP(8) = ISTAIP(8) + 1
      ISTAIP(9) = ISTAIP(9) + NCO
      IF(NCO.GT.ISTAIP(10)) ISTAIP(10) = NCO
      IF(NCO.LT.ISTAIP(11)) ISTAIP(11) = NCO
C-----------------------------------------------------------------------
  990 CONTINUE
C
      IF(FDEBRF.AND.JDBDRF(JULIT).GE.1) WRITE(LDEBRL,1000) IRUNRC,
     +      IEVTRC,NDIG,NCO
 1000 FORMAT('0IPSTAT: run',I6,' event',I10,'. ITC prepare data:',
     +      I8,' digits.',I9,' coords.')
      END
#endif
