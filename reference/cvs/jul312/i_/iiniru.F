      SUBROUTINE IINIRU
C-----------------------------------------------------------------------
C! Initialise the ITC run
C!
C!   Author          :-  J. Sedgbeer  87/03/02
C!   Modified        :-  J. Sedgbeer  90/01/04 Mod. RERROR calls.
C!   Modified        :-  J. Sedgbeer  92/01/30 Mod. RERROR calls.
C!
C!   Input:  /RCURNT/ for run number
C!
C!   Output: none
C!
C!   called by : RINRUN
C!
C!   calls : IRDDAF (Alephlib)    Set up ITC constants for 'prepare data
C!           IPRDAF (Alephlib)    Print ITC 'prepare data' consts.
C!           IINTRK               Initialise ITC tracking constants
C!           ITRKCI               Initialise ITC tracking commons
C!
C!  Description
C!  -----------
C! Initialise constants and parameters needed in reconstruction program.
C-----------------------------------------------------------------------
#ifndef DOC
#include "rparac.h"
#include "rlunit.h"
#include "rflags.h"
C-----------------------------------------------------------------------
C I/O commons etc.
#include "rcurnt.h"
C-----------------------------------------------------------------------
      IF(FDEBRF.AND.JDBDRF(JULIT).GE.3) WRITE(LDEBRL,1000) IRUNRC
 1000 FORMAT(' IINIRU: Initialise ITC for run',I10)
C
C Initialise ITC 'prepare data' consts.
C
      CALL IRDDAF(IRUNRC,IRET)
      IF(IRET.EQ.0) THEN
        CALL RERROR('IINIRU',-1,'Missing ITC DB banks. FATAL.')
        GOTO 999
      ENDIF
C
C Print
C
      IF(FDEBRF.AND.JDBDRF(JULIT).GE.3) CALL IPRDAF
C
C-----------------------------------------------------------------------
C Initialise ITC tracking constants
C
      CALL IINTRK
C
C Initialise ITC tracking commons
C
      CALL ITRKCI
C
  999 CONTINUE
      END
#endif
