      SUBROUTINE QADATI
CKEY INIT /INTERNAL
C----------------------------------------------------------------------
C! Print ALPHA header
C! called from QMINIT
C!                                                  H.Albrecht 06.11.88
C!                                                  E. Blucher 08.04.89
C!                               Modified:          E. Blucher 02.09.92
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
C----------------------------------------------------------------------
      DIMENSION JDAT(2), JTIM(2)
      CHARACTER * 4 CHAINT
      PARAMETER (NUMAC= 20)
      CHARACTER*8 COMPU
      CHARACTER*8 CALVER
C Get computer type
      CALL ALMACH(COMPU)
C Alephlib number
      IW(6)=0
      CALL ALVERS (ALVER)
      IF (ALVER.LT.314.0) THEN
         WRITE(KUPRNT,*)' _QADATI_ STOP because ALEPHLIB ',
     &   ALVER,' is not Y2K compliant - use ALEPHLIB>=314 '
         CALL EXIT
      ENDIF
C
C           date,time
C
      CALL TIMEST(1000000.)
      CALL TIMEL (QTIMEL)
      CALL TIMEX (QTIMES)
C
      CALL DATIMH (JDAT,JTIM)
      IF(COMPU.EQ.'CRAY')THEN
C       on Cray all 8 character are in one word JDAT or JTIM
        WRITE(CQDATE(1:8),'(A8)') JDAT(1)
        WRITE(CQTIME(1:8),'(A8)') JTIM(1)
      ELSE
        WRITE(CQDATE(1:4),'(A4)') JDAT(1)
        WRITE(CQDATE(5:8),'(A4)') JDAT(2)
        WRITE(CQTIME(1:4),'(A4)') JTIM(1)
        WRITE(CQTIME(5:8),'(A4)') JTIM(2)
      ENDIF
C
C           print header
C
      CQHTIT = 'A L P H A   ' // CQVERS // '   ' // CQTIME // ' ' //
     +         CQDATE
      CALL QWMESS ('--------------------------------------------------')
      CALL QWMESS (CQHTIT(39:55) // CQHTIT(1:38))
      CALL QWMESS (' ')
      CALL QWMESS (CQHTIT(39:64) // ' running on ' // COMPU)
      CALL QWMESS (' ')
      WRITE (KUPRNT, 1000)  ALVER
      IF (KUPTER .NE. 0)
     &  WRITE (KUPTER, 1000) ALVER
 1000 FORMAT(26X,'ALEPHLIB Version ',F7.2)
      CALL QWMESS ('--------------------------------------------------')
      END
#endif
