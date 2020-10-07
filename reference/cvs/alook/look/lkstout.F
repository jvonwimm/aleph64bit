      SUBROUTINE LKSTOUT (LNEW)
C -----------------------------------------------------------
C! set LOUT logical unit : terminal output unit
C  the first call defines the default
CKEY LOOK OUTPUT LOUT
C - F.Ranjard - 911114
C - Input  : LNEW / I : terminal output unit
C - ENTRY LKRSOUT
C   reset LOUT to ULDEF
C -----------------------------------------------------------
#ifndef DOC
      INTEGER LNEW
#include "lkfil.h"
      DATA IFI /0/
C -----------------------------------------------------------
C - 1st entry
      IF (IFI.EQ.0) THEN
         IFI = 1
         LDEF = LNEW
      ENDIF
C
C - next entry
      LOUT = LNEW
      RETURN
C -----------------------------------------------------------
C
C - entry
      ENTRY LKRSOUT
      LOUT = LDEF
      RETURN
C -----------------------------------------------------------
      END
#endif
