      SUBROUTINE SWPAPR(LUN)
C----------------------------------------------------------------------
C! Print the SWPA bank
C!
C!    Author:     H. Meinhard       27-Jan-1988
C!
C!    Input:      - LUN       /I    Logical unit for output
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "sanamc.h"
#include "swpajj.h"
      CHARACTER*1 CAB(2), CLH(2)
      DATA CAB/'A','B'/, CLH/'L','H'/
#include "bmacro.h"
C----------------------------------------------------------------------
C link to bank
      KSWPA = IW(NASWPA)
      IF (KSWPA .EQ. 0)                                     GOTO 901
C
C if bank exists, but no entries, print corresponding message
      IF (LROWS(KSWPA) .EQ. 0) THEN
        WRITE (LUN,500)
      ELSE
        WRITE (LUN,510)
        DO 300 IWPA = 1, LROWS(KSWPA)
          NPAR = ITABL(KSWPA,IWPA,JSWPNP)
          WRITE (LUN,520) IWPA,CAB(ITABL(KSWPA,IWPA,JSWPSI)),
     +      ITABL(KSWPA,IWPA,JSWPMP),NPAR,
     +      (ITABL(KSWPA,IWPA,JSWPMC+I-1)/2,
     +      CLH(MOD(ITABL(KSWPA,IWPA,JSWPMC+I-1),2)+1), I=1,NPAR)
  300   CONTINUE
      ENDIF
      GOTO 999
C----------------------------------------------------------------------
  901 WRITE (LUN,530) 'SWPA'
      GOTO 999
  999 CONTINUE
      RETURN
C
C Formats
  500 FORMAT (9X,'No correspondings found.')
  510 FORMAT ('  Nr   side   patch   # coord.   coordinates used (L = ',
     +  'lower, H = higher theta)')
  520 FORMAT (I4,5X,A1,I7,I10,6X,9(1X,I3,A1))
  530 FORMAT (' --> SWPAPR: BANK ',A4,' NOT FOUND')
      END
#endif
