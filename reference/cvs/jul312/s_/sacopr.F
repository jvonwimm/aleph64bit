      SUBROUTINE SACOPR(LUN)
C----------------------------------------------------------------------
C! Print the SACO bank
C!
C!    Author:     H. Meinhard       06-Mar-1990
C!
C!    Input:      - LUN       /I    logical unit for printout
C!                - bank 'SACO'
C----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "sanamc.h"
#include "sacojj.h"
      CHARACTER*1 CAB(2),CLH(2)
      DATA CAB/'A','B'/, CLH/'L','H'/
#include "bmacro.h"
C----------------------------------------------------------------------
C link to bank
      JSACO = IW(NASACO)
      IF (JSACO .EQ. 0)                                     GOTO 901
C if bank exists, but no entries, print corresponding message
      IF (LROWS(JSACO) .EQ. 0) THEN
        WRITE (LUN,500)
      ELSE
        WRITE (LUN,510)
        DO 300 ICO = 1, LROWS(JSACO)
          ZFOOT = RTABL(JSACO,ICO,JSACBF+2)
          IF (ZFOOT .GT. 0.) THEN
            ISIDE = 1
          ELSE
            ISIDE = 2
          ENDIF
          WRITE (LUN,520) (ICO+1)/2,CLH(MOD(ICO+1,2)+1),CAB(ISIDE),
     +      RTABL(JSACO,ICO,JSACRA),(RTABL(JSACO,ICO,JSACBF+I-1),I=1,9)
  300   CONTINUE
      ENDIF
      GOTO 999
C----------------------------------------------------------------------
  901 WRITE (LUN,530)
      GOTO 999
  500 FORMAT (9X,'No coordinates found.')
  510 FORMAT (
     +  '   Nr  Side  Radius      Radius foot point (1:3)       ',
     +  'Radius unit vector (1:3)      Wire parallel direction (1:3)')
  520 FORMAT (I4,A1,4X,A1,F9.3,3X,3F9.3,3X,3F9.3,6X,3F9.3)
  530 FORMAT (' --> SACOPR: Bank SACO not found')
  999 CONTINUE
      END
#endif
