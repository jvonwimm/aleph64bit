*DK lcdigi
      SUBROUTINE LCDIGI
C--------------------------------------------------------------
C! Controls digitalisation in LCal
C. - J.Dines Hansen & P.Hansen - 860417
C. - Called by  ASDIGI                           from this .HLB
C. - Calls      LCTRHT,LCADC,LCNOIS,LCROC,LCTRDI from this .HLB
C -----------------------------------------------
#ifndef DOC
      DIMENSION XAV(2),YAV(2)
#include "iocom.h"
#include "jobcom.h"
#include "lccomc.h"
C ------------------------------------------------------------
C
C - Form analog trigger sums
      CALL LCTRHT
C
C - Create digitalization banks 'LWDI' and 'LTDI'
      CALL LCADC
C
C - Convert trigger bank 'LTTR' from hits to digitizations
      CALL LCTRDI
C
C - Add noise if the second parameter on RUNC 'LCAL' is = 1
      IF (ICLCJO(2) .EQ. 1) CALL LCNOIS
C
C - Create final digitalization banks in Read Out Controller
      CALL LCROC
C
C - Reduce bank size to real one
      CALL AUBPRS ('LTDILWDILTTRLWTR')
C
C - Print the output banks for events to be debugged
      IF (LCPRNT .GE. 1) THEN
         CALL LCBKPR
      ENDIF
         CALL LCBHAB (LCBHA,EFORW,EBACK)
  999 RETURN
C
 1000 FORMAT(' +++ LCDIGI +++')
      END
#endif
