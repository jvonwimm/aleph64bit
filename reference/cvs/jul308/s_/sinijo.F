      SUBROUTINE SINIJO
C----------------------------------------------------------------------
C! Initialise the SATR job
C!
C!    Author:     H. Meinhard       16-Aug-1987
C!    Modified:   H. Meinhard       27-Jul-1988
C!
C?
C!======================================================================
#ifndef DOC
#include "sstatc.h"
C----------------------------------------------------------------------
C? Set name indices and bank formats for named SATR banks, put banks
C? on lists
      CALL SRINBS
C
C? Zero statistics arrays
      NERDSS = 0
      CALL VZERO(NRAWSS,2)
      CALL VZERO(NCOOSS,2)
      CALL VZERO(NETRSS,6)
      CALL VZERO(NTRKSS,6)
C
      GOTO 999
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
#endif
