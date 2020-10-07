      SUBROUTINE SINIRU
C--------------------------------------------------------------------
C! Initialise the SATR run
C!
C!    Author:     H. Meinhard       19-Aug-1987
C!    Modified:   H. Meinhard       12-Jul-1989  (3)
C!
C?
C!======================================================================
#ifndef DOC
C>>> HM890307 TEMPORARY MOD FOR BACKWARD COMPATIBILITY
#include "bcs.h"
#include "ajobjj.h"
#include "sflagc.h"
#include "bosext.h"
#include "bmacro.h"
C>>> END HM890307
C----------------------------------------------------------------------
C>>> HM890307 TEMPORARY MOD FOR BACKWARD COMPATIBILITY
      LOLDSF = .FALSE.
      LOLGSF = .FALSE.
      KAJOB = IW(NAMIND('AJOB'))
      IF (KAJOB .NE. 0) THEN
        LOLDSF = .TRUE.
        IGALV = ITABL(KAJOB,1,JAJOGV)
        IF (IGALV .LE. 230) LOLGSF = .TRUE.
C use 14 explicitely since AJOBJJ may not contain JAJODV
        IF (LCOLS(KAJOB) .GE. 14) THEN
          IDAFV = ITABL(KAJOB,1,14)
          IF (IDAFV .GE. 113) LOLDSF = .FALSE.
        ENDIF
      ENDIF
C>>> END HM890307
C? fill common block with geometrics constants
      CALL SRGEOM
C
C? fill common block with calibration constants
      CALL SRCONS
C
C? Fill common SRECPC (reconstruction cuts)
      CALL SRCUTS
C
C? Print the constants for this run
      CALL SRPRCN
C
      GOTO 999
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
#endif
