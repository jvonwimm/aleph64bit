      SUBROUTINE ICLIMB(ILNK,MODE)
C-----------------------------------------------------------------------
C! Climb up the tree via interconnecting links.
C!
C!    Author      :- J. Sedgbeer 88/04/15
C!    Modified    :- I. Tomalin  89/06/07
C!
C!   Input:
C!    ILNK    /I  : Link number
C!    MODE    /I  : = 1 if in TPC track extension phase.
C!                  = 2 if in ITC stand alone phase.
C!         commons: /BCS/, /ITRKPA/, /ITGAPC/, /IWBNKT/
C!
C!   Output: None
C!
C!   called by : IFIND1 and IFIND2
C!   calls     : ISAVE
C!
C!   Libraries required: none
C!
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
#include "rlunit.h"
C-----------------------------------------------------------------------
C I/O Commons
#include "bcs.h"
#include "itrkpa.h"
#include "itgapc.h"
#include "iwbnkt.h"
#include "ipathc.h"
C-----------------------------------------------------------------------
      INTEGER NBRN(8),LLINK(8),IMISS(0:8)
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
C Initialise
C
      MNLNK = MNLINK(MODE)
      MXJMP = MXJMPD(MODE)
      ILINK = ILNK
      IF(IW(INNXIW+LMHLEN+ILINK).LE.0) GOTO 999
      IGAP = ITABL(ILNKIW,ILINK,3)
      ILAY = IOGPIT(IGAP)
C Note the number of layers missed by starting with link ILINK.
      IMISS(0) = NOLIIT - ILAY
      IDPTH = 0
C
C Climb up ILINK
C
   10 CONTINUE
C See if link ILINK jumps too many layers.
      IF(NUMISS(IGAP).GT.MXJMP) GOTO 30
C See if using ILINK would make it impossible to form a chain of the
C required length.
      IF(NILIIT-(IMISS(IDPTH)+NUMISS(IGAP)).LT.MNLNK) GOTO 50
C
C This link appears to be O.K., so store it.
C
C Note the length of the chain so far and store its new extremity.
      IDPTH = IDPTH + 1
      LLINK(IDPTH) = ILINK
C Keep a tally of the total number of missed layers on this chain.
      IMISS(IDPTH) = IMISS(IDPTH-1) + NUMISS(IGAP)
C Note the number of branches out of ILINK tested so far.
      NBRN(IDPTH)  = 0
C Stop climbing if the chain is long enough
      IF (IDPTH.EQ.MXLINK(MODE)) GOTO 30
C
C Climb up ILINK's next branch
C
   20 CONTINUE
      NBRN(IDPTH) = NBRN(IDPTH) + 1
C See if we have tried all possible extensions to link ILINK.
      IF(NBRN(IDPTH).GT.IW(INNXIW+LMHLEN+ILINK)) GOTO 30
      IOFF = IW(IOFNIW+LMHLEN+ILINK)
      ILINK = IW(INXLIW+LMHLEN+IOFF+NBRN(IDPTH))
      IGAP  = ITABL(ILNKIW,ILINK,3)
      GOTO 10
C
C Reached an extremity: can climb no further. Save path.
C
   30 CONTINUE
C
      IF(FDEBRF.AND.JDBDRF(JULIT).GE.6) WRITE(LDEBRL,1001) IDPTH,
     +     (LLINK(J),J=1,IDPTH)
 1001 FORMAT(' ICLIMB: call Isave: idpth,llink',9I5)
C
C Save track if it has enough links.
      IF (IDPTH.GE.MNLNK) CALL ISAVE(IDPTH,LLINK,MODE)
C Save CPU time, by not bothering to look for shorter links than the
C longest found so far.
      IF (NPTHIP.GT.0) THEN
        MNLNK = LPTHIP
        MXJMP = MIN(MXJMP,NILIIT-MNLNK)
      END IF
C
C Climb down ILINK
C
   50 CONTINUE
      IDPTH = IDPTH - 1
      IF(IDPTH.LE.0) GOTO 999
      ILINK = LLINK(IDPTH)
      IF(NBRN(IDPTH).LT.IW(INNXIW+LMHLEN+ILINK)) GOTO 20
      GOTO 50
C
  999 CONTINUE
      END
#endif
