      SUBROUTINE VDCRUN
C----------------------------------------------------------------------
C!  - VDET End of Run
C!
C!   Author   :- E. Lancon             26-FEB-1993
C!
C!  Modified March 1995 D. Brown, A. Bonissent, M. Thulasidas
C!                 reorganise and debug for VDET 95
C?
C!======================================================================
#ifndef DOC
#include "rlunit.h"
#include "vprtnc.h"
#include "vrecon.h"
C
C  Local variables
C
      REAL RAW(2),FINAL(2),MATCH(2),NOISY(2)
      INTEGER IVIEW
      CHARACTER*1 BAR(80)
      DATA BAR/80*'-'/
C--------------------------------------------------------------------
      WRITE(LOUTRL,'(''0'',80A)') BAR
C
C? print out result of MVD pattern recognition test with MC events
C
      WRITE(LOUTRL,100)
  100 FORMAT( '0VDET Summary : ',/)

C
C  Calculate the averages
C
      IF(NEVNTS .GT. 0)THEN
        WRITE(LOUTRL,'(1X,I6,A)')NEVNTS,
     &  ' Events proccessed for VDET this RUN'
        DO 1001 IVIEW=1,2
          RAW(IVIEW)   = NRAWHT(IVIEW)/FLOAT(NEVNTS)
          FINAL(IVIEW) = NFNLHT(IVIEW)/FLOAT(NEVNTS)
          MATCH(IVIEW) = NMATHT(IVIEW)/FLOAT(NEVNTS)
          NOISY(IVIEW) = NNOSHT(IVIEW)/FLOAT(NEVNTS)
 1001   CONTINUE
        WRITE(LOUTRL,'(5X,F6.2,1X,F6.2,A)')RAW,
     &  ' U, W raw     hits/event'
        WRITE(LOUTRL,'(5X,F6.2,1X,F6.2,A)')FINAL,
     *  ' U, W final   hits/event'
        WRITE(LOUTRL,'(5X,F6.2,1X,F6.2,A)')MATCH,
     *  ' U, W matched hits/event'
        WRITE(LOUTRL,'(5X,F6.2,A)')NOISY(1),
     *  ' killed noisy hits/event'
      ELSE
        WRITE(LOUTRL,*)'No events processed for VDET this RUN'
      END IF
C
C call VCRHOT if it is a special run to produce the hot channel list
C
      IF(MKVHOT)  CALL VCRHOT
C
  999 RETURN
      END
#endif
