*DK lcroc
      SUBROUTINE LCROC
C--------------------------------------------------------------
C! Simulate Read Out Controller
C. - J.Dines Hansen & P.Hansen - 860604
C. - Zero suppression and range saturation
C. - Modify banks 'LTDI'
C. - Called by  LCDIGI                           from this .HLB
C -----------------------------------------------
#ifndef DOC
      SAVE
      LOGICAL ACCPT
#include "jqcom.h"
#include "lcnamc.h"
#include "lccomc.h"
#include "bmacro.h"
C -------------------------------------------------------------
C - Process bank LTDI
        KINDX = IW(NALTDI)
        NOLD  = LROWS(KINDX)
        IF (KINDX .LE. 0 .OR. NOLD .LE. 0)             GOTO 999
        NEW = 0
C - Loop over stored digitalizations
        DO 110 IDIG = 1,NOLD
          KADR = KROW(KINDX,IDIG)
          ACCPT = .FALSE.
C         Loop over storeys
          DO 120 ISTOR = 1,3
C
C           Zero suppression in ADC
            IF (IW(KADR+1+ISTOR) .GE. LCSTRH(ISTOR))
     &         ACCPT = .TRUE.
C
C           Saturate signal at 12+3 bits
            MAXCO = NINT(ADCOLC*32767)
            IW(KADR+1+ISTOR) = MIN0(MAXCO,IW(KADR+1+ISTOR))
  120     CONTINUE
          IF (ACCPT) THEN
            NEW = NEW + 1
            KADRA  = KROW(KINDX,NEW)
            DO 130 ICOL = 1,LCOLS(KINDX)
  130       IW(KADRA+ICOL) = IW(KADR+ICOL)
          ENDIF
  110   CONTINUE
        IW(KINDX+2) = NEW
C
  999 RETURN
      END
#endif
