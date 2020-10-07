      SUBROUTINE INTRAK(IER)
C-----------------------------------------------------------------------
C! ITC tracking prog. initialisation
C!
C!    Author      :- J. Sedgbeer 88/03/29
C!    Modified    :- J. Sedgbeer 88/05/09  Part moved to start of job.
C!    Modified    :- I. Tomalin  88/11/17
C!
C!   Input:
C!      commons:     /BCS/
C!      params.:     ITCOJJ,ITFTJJ,IGTLJJ
C!
C!   Output:
C!      IER   /I  : Error flag: = 0 if O.K.,
C!                              = 1 if no ITCO bank,
C!                              = 2 if insufficient BOS space.
C!      commons:     /IWBNKT/   Set pointers to work banks used
C!
C!   called by : ITCTRK
C!   calls     : none
C!
C!   Libraries required: BOS, Alephlib
C!
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
#include "rlunit.h"
#include "bosext.h"
C-----------------------------------------------------------------------
C I/O Commons etc.
#include "bcs.h"
#include "iwbnkt.h"
C-----------------------------------------------------------------------
C Parameters
#include "itcojj.h"
#include "igtljj.h"
#include "itftjj.h"
C-----------------------------------------------------------------------
      PARAMETER (NTRMX=20)
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
      IER = 0
C
C Find the total number of ITC coordinates.
C
      KITC0 = NLINK('ITCO',0)
      IF (KITC0.EQ.0) THEN
        IER = 1
        GOTO 999
      END IF
      NCO = LROWS(KITC0)
C
C Book banks IGCL#2, IGTL#2, ITFT#2 for ITC tracking.
C
      LIGCL = NCO + LMHLEN
      LIGTL = LIGTLA*NTRMX + LMHLEN
      LITFT = LITFTA*NTRMX + LMHLEN
      CALL AUBOS('IGCL',2,LIGCL,KIGCL,IGARB)
      IF(IGARB.EQ.2) GOTO 910
      CALL AUBOS('IGTL',2,LIGTL,KIGTL,IGARB)
      IF(IGARB.EQ.2) GOTO 910
      CALL AUBOS('ITFT',2,LITFT,KITFT,IGARB)
      IF(IGARB.EQ.2) GOTO 910
C
      KIGCL = NLINK('IGCL',2)
      KIGTL = NLINK('IGTL',2)
      KITFT = NLINK('ITFT',2)
C
C Initialise IGCL, IGTL, ITFT banks
C
      IW(KIGCL+LMHCOL) = 1
      IW(KIGCL+LMHROW) = 0
      IW(KIGTL+LMHCOL) = LIGTLA
      IW(KIGTL+LMHROW) = 0
      IW(KITFT+LMHCOL) = LITFTA
      IW(KITFT+LMHROW) = 0
C
      GOTO 999
C
C-----------------------------------------------------------------------
C Error returns
C
  910 CONTINUE
      CALL RERROR('INTRAK',1,'No room to create BOS banks.')
      IER = 2
C Drop work banks
      IF (IUCOIW.NE.0) CALL WDROP(IW,IUCOIW)
      IF (IWHTIW.NE.0) CALL WDROP(IW,IWHTIW)
C
  999 CONTINUE
      END
#endif
