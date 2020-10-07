      SUBROUTINE SPREDA
C--------------------------------------------------------------------
C! Prepare SATR Data
C!
C!    Author:     H. Meinhard       04-Sep-1987
C!    Modified:   H. Meinhard       06-Dec-1989  (2)
C!
C!    Description
C!    ===========
C!    Steering routine for prepare data of SATR data
C!    Look whether raw data contain TDC time information; if not,
C!    generate time information; calculate SATR coordinates from
C!    raw data
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "rcurnt.h"
#include "rflags.h"
#include "rlunit.h"
#include "sanamc.h"
C--------------------------------------------------------------------
C
C? put banks on lists
C? 'T': temporary list, to be purged after each event
      CALL BLIST(IW,'T+','SCOOSTRKSTCP')
      CALL BLIST(IW,'T+','SRADSADISRTDSCRPSSCPSCLUSCCPSSBPSPATSPCPSSPP')
      CALL BLIST(IW,'T+','SWPASKANSKCPSSKPSSTPSSUPSUPASUCP')
      CALL BLIST(IW,'T+','SACOSFTR')
C
C
C>>>>>>> Error handling is done only preliminarily
C
      IER = 0
C
C apply corrections to real raw data
      IF (.NOT.FMCRUN) THEN
        CALL SREALD(IER)
        IF (IER .NE. 0)                                     GOTO 901
      ENDIF
C
C? look whether bank SRTD exists; if not, look for SRAD and SADI banks
      IF (IW(NASRTD) .EQ. 0) THEN
        IF (IW(NASRAD) .NE. 0 .OR. IW(NASADI) .NE. 0) THEN
          CALL SPREMC(IER)
          IF (IER .NE. 0)                                   GOTO 901
        ELSE
          GOTO 999
        ENDIF
      ENDIF
C
C? calculate coordinates from raw data
      CALL SRCOOR(IER)
      IF (IER .EQ. 1)                                       GOTO 999
      IF (IER .NE. 0)                                       GOTO 901
C
C? print coordinates, if desired
      IF (JDBDRF(JULSA) .GE. 3) THEN
        WRITE (LOUTRL,'(/A)') ' ++++    SATR reconstruction: '//
     +    'Coordinates found:'
        CALL SCOOPR(LOUTRL)
      ENDIF
C
      GOTO 999
C--------------------------------------------------------------------
  901 GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
