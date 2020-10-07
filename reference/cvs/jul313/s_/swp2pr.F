      SUBROUTINE SWP2PR(LUN)
C----------------------------------------------------------------------
C! Print the SWPA bank after double arm pattern recognition
C!
C!    Author:     H. Meinhard       06-Mar-1990
C!
C!    Input:      - LUN       /I    logical unit for printout
C!                - bank 'SWPA'
C----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "sanamc.h"
#include "swpajj.h"
      CHARACTER*45 TEXT
      CHARACTER*1 CAB(2),CLH(2)
      DATA CAB/'A','B'/, CLH/'L','H'/
#include "bmacro.h"
C----------------------------------------------------------------------
C link to bank
      JSWPA = IW(NASWPA)
      IF (JSWPA .EQ. 0)                                     GOTO 901
C if bank exists, but no entries, print corresponding message
      IF (LROWS(JSWPA) .EQ. 0) THEN
        WRITE (LUN,500)
      ELSE
        WRITE (LUN,510)
        DO 300 IWPA = 1, LROWS(JSWPA), 2
          NPAR1 = ITABL(JSWPA,IWPA,JSWPNP)
          ISID1 = ITABL(JSWPA,IWPA,JSWPSI)
          NPAR2 = ITABL(JSWPA,IWPA+1,JSWPNP)
          ISID2 = ITABL(JSWPA,IWPA+1,JSWPSI)
          TEXT = ' '
          WRITE (TEXT,'(9(1X,I3,A1))') (ITABL(JSWPA,IWPA,JSWPMC+I-1)/2,
     +      CLH(MOD(ITABL(JSWPA,IWPA,JSWPMC+I-1),2)+1),I=1,NPAR1)
          WRITE (LUN,520) IWPA,CAB(ISID1),ITABL(JSWPA,IWPA,JSWPMP),
     +      NPAR1,TEXT,IWPA+1,CAB(ISID2),ITABL(JSWPA,IWPA+1,JSWPMP),
     +      NPAR2,(ITABL(JSWPA,IWPA+1,JSWPMC+I-1)/2,
     +      CLH(MOD(ITABL(JSWPA,IWPA+1,JSWPMC+I-1),2)+1),I=1,NPAR2)
  300   CONTINUE
      ENDIF
      GOTO 999
C----------------------------------------------------------------------
  901 WRITE (LUN,530)
      GOTO 999
  500 FORMAT (9X,'No wire parallels corresponding to patches found.')
  510 FORMAT ('  Nr side patch #co  coordinates used (L=lower, H=',
     +  'higher theta)     Nr side patch #co  coordinates used (L=',
     +  'lower, H=higher theta)')
  520 FORMAT (I4,3X,A1,I5,I5,2X,A45,I4,3X,A1,I5,I5,2X,9(1X,I3,A1))
  530 FORMAT (' --> SWP2PR: Bank SWPA not found')
  999 CONTINUE
      END
#endif
