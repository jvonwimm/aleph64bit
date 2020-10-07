      SUBROUTINE ILATCH(LIST,IER)
C-----------------------------------------------------------------------
CKEY ITC WIRE
C! Create 'latched' wire bank IWCR (wire-to-coord relation)
C!
C!    Author     :- W. Atwood
C!    Modified   :- J. Sedgbeer  89/12/08
C!
C!    Input:
C!      LIST   /C    : BOS list to which IWCR is to be added e.g. 'T+'
C!      commons:     /BCS/     for bank ITCO
C!      params:      ITCOJJ
C!
C!    Output:
C!      IER    /I    : = 0 successful
C!                     = 1 input bank is empty or does not exist
C!                     = 2 not enough space (garbage coll. may have
C!                                 been done)
C!                     =-1 O.K. but garbage collection
C!      IWCR bank
C!
C! ILATCH:
C! Create bank IWCR of latched ITC wires for this event. IWCR is
C! really a relation bank which gives the coordinate number for
C! a given wire, or zero if no coord. for the wire - see DDL.
C!
C?  If first then Set name indices and bank formats
C?  Check for non-empty ITCO bank.
C?  Create IWCR bank
C?  Loop over coordinates
C?    fill IWCR
C?  End Loop
C?  Add IWCR to BOS list
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C I/O commons and parameters
#include "bcs.h"
#include "itcojj.h"
      CHARACTER*(*) LIST*(*)
      EXTERNAL NAMIND
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA NWMAX/960/
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST = .FALSE.
        NITCO = NAMIND('ITCO')
        NIWCR = NAMIND('IWCR')
        CALL BKFMT('IWCR','I')
      ENDIF
C
C Check for ITCO bank
C
      IER = 1
      JITCO = IW(NITCO)
      IF(JITCO.EQ.0) GOTO 999
      NCO = LROWS(JITCO)
      IF(NCO.EQ.0) GOTO 999
C
C Create IWCR bank
C Use IGARB to note if garbage collection done.
C
      IGARB = 0
      CALL AUBOS('IWCR',0,LMHLEN+NWMAX,JIWCR,IER)
      IF(IER.EQ.2) GOTO 999
      IF(IER.EQ.1) THEN
        IGARB = 1
        JITCO = IW(NITCO)
      ENDIF
      IW(JIWCR+LMHCOL) = 1
      IW(JIWCR+LMHROW) = NWMAX
C
C-----------------------------------------------------------------------
C Loop over the coordinates.
C
      DO 10 I=1,NCO
        KK = KROW(JITCO,I)
        IWIRE = IW(KK+JITCWN)
        IWIRE = MOD(IWIRE,1000)
        IW(JIWCR+LMHLEN+IWIRE) = I
   10 CONTINUE
C-----------------------------------------------------------------------
C
C Add IWCR to list
C
      CALL BLIST (IW,LIST,'IWCR')
C
C If garbage collection then set error flag to -1
C
      IER = 0
      IF(IGARB.EQ.1) IER = -1
C
  999 CONTINUE
      END
#endif
