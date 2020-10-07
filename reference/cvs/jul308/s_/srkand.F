      SUBROUTINE SRKAND(IER)
C----------------------------------------------------------------------
C! Find track candidates
C!
C!    Author:     H. Meinhard       04-Sep-1987
C!    Modified:   H. Meinhard       03-Mar-1988
C!
C!    Output:     - IER       /I    error flag (0 = no error)
C!
C!    Description
C!    ===========
C!    Look for track candidates performing several steps:
C!    - Perform first track fit and eliminate iteratively wire
C!      paralleles which lead to a bad chisquare probability
C!    - Among the track candidates being derived from one patch,
C!      select the best one
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "rflags.h"
#include "rlunit.h"
#include "sanamc.h"
C----------------------------------------------------------------------
C look whether bank SKAN exists; if yes, return
      IF (IW(NASKAN) .NE. 0)                                GOTO 999
C
C first track fit ...
      CALL SRFITF(IER)
      IF (IER .EQ. 1)                                       GOTO 999
      IF (IER .NE. 0)                                       GOTO 901
      IF (JDBDRF(JULSA) .GE. 4) THEN
        WRITE (LOUTRL,'(/A)') ' ++++    SATR reconstruction: '//
     +    'First track fit done. Track candidates found:'
        CALL SKANPR(LOUTRL)
      ENDIF
C
C select track candidates ...
      CALL SRSELK(IER)
      IF (IER .EQ. 1)                                       GOTO 999
      IF (IER .NE. 0)                                       GOTO 902
      IF (JDBDRF(JULSA) .GE. 4) THEN
        WRITE (LOUTRL,'(/A)') ' ++++    SATR reconstruction: '//
     +    'Track candidates selected. Track candidates found:'
        CALL SKANPR(LOUTRL)
      ENDIF
C
      GOTO 999
C----------------------------------------------------------------------
  901 IER = 2
      GOTO 999
  902 IER = 3
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
