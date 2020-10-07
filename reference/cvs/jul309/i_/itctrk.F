      SUBROUTINE ITCTRK
C-----------------------------------------------------------------------
C!   ITC tracking - Steering.
C!
C!    Author      :- J. Sedgbeer 88/03/19
C!    Modified    :- I. Tomalin  88/11/17
C!
C!   Input: None
C!
C!   Output: None
C!
C!   called by : ITCREC
C!
C!   calls : INTRAK, ITRAK, IFTRAK
C!
C!   Libraries required: none
C!
C! ITCTRK: ITC stand alone tracking based on a "link and tree" method.
C!         See H.Kowalski and D.G.Cassel  DESY 80/107 (1980)
C!
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
#include "rlunit.h"
C-----------------------------------------------------------------------
C Initialisation
C
      CALL INTRAK(IER)
      IF(IER.NE.0) THEN
        IF(FDEBRF.AND.JDBDRF(JULIT).GE.1) CALL RERROR('ITCTRK',1,
     +  'Subroutine INTRAK gave IER > 0,'//
     +  ' so skipping ITC stand-alone tracking.')
        GOTO 999
      ENDIF
C
      CALL ITRAK
C
C Finalisation
C
      CALL IFTRAK
C
  999 CONTINUE
      END
#endif
