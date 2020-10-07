      SUBROUTINE SRTRCK(IER)
C----------------------------------------------------------------------
C! Find SATR tracks
C!
C!    Author:     H. Meinhard       01-Mar-1988
C!    Modified:   H. Meinhard       03-Mar-1988
C!
C!    Output:     - IER       /I    error flag (0 = no error)
C!
C!    Description
C!    ===========
C!    Look for tracks performing several steps:
C!    - On each side, find track candidate with best chisquare proba-
C!      bility, remove all used hits from other candidates, if neces-
C!      sary, redo fit and repeat procedure with remaining candidates
C!    - On each side, look whether track distance is below a minimum
C!      distance. If yes, tracks are too close together, find mean
C!      value of parameters
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "rflags.h"
#include "rlunit.h"
#include "sanamc.h"
#include "srecpc.h"
C----------------------------------------------------------------------
C look whether bank STRK exists; if yes, return
      IF (IW(NASTRK) .NE. 0)                                GOTO 999
C
C remove double used hits ...
      CALL SRFTRK(IER)
      IF (IER .EQ. 1)                                       GOTO 999
      IF (IER .NE. 0)                                       GOTO 901
      IF (JDBDRF(JULSA) .GE. 4) THEN
        WRITE (LOUTRL,'(/A)') ' ++++    SATR reconstruction: '//
     +    'Double used hits removed. Tracks found:'
        CALL STRKPR(LOUTRL)
      ENDIF
C
C remove tracks being close together ...
      CALL SRSELO('STRK','STCP','SSTP',ACTRSR,IER)
      IF (IER .EQ. 1)                                       GOTO 999
      IF (IER .NE. 0)                                       GOTO 902
      IF (JDBDRF(JULSA) .GE. 4) THEN
        WRITE (LOUTRL,'(/A)') ' ++++    SATR reconstruction: '//
     +    'Tracks selected. Tracks found:'
        CALL STRKPR(LOUTRL)
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
