      SUBROUTINE ECDFRD
C.----------------------------------------------------------------------
CKEY ECALDES EXPAND GEOMETRY INITIALISATION / USER
C     H.Videau      Creation 15/12/85      Modification 25/01/88
C! Init. ECAL geometry
C MUST BE called by USER after ERDDAF.
C Expand geometry for ECal .
C It defines redundant relationships by product and inversion
C and expands the content of the structure to make the access faster and
C easier.
C   Called by USER.
C   Calls: EINVRL,EXPNEC
C.----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "erdvrs.h"
#include "bcs.h"
C Locales
      LOGICAL INIT
      DATA INIT/.TRUE./
C
      LOUTIO = IW(6)
      IF(INIT) THEN
        WRITE(LOUTIO,100) ERDVRS
  100   FORMAT (10X,'Ecal Geometry package version ',I6,' running')
        INIT=.FALSE.
      ENDIF
C
C    extends the description by redundant information to ease the
C      access
C     First we invert some relationships.
              CALL EINVRL
C     Expands the description
              CALL EXPNEC
C
      END
#endif
