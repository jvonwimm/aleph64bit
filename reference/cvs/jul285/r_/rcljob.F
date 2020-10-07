      SUBROUTINE RCLJOB
C----------------------------------------------------------------------
C!  - Close the reconstruction job
C!
C!   Author        J. Bunn
C!
C!   Modified      S. Orteu      9-FEB-1987
C!
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "rflags.h"
#include "rlunit.h"
#include "rtimes.h"
C--------------------------------------------------------------------
C
C Call the user's close job
C
      CALL USCJOB
C
C summarise the runs that have been processed
C
      CALL RSMJOB
C
C Print some BOS statistics
C
      IF (JPRERF (JULSK).GT.1) CALL BOSTA
C
C?   close the files
C
      CALL ABWEND
C
C
C?   Close all opened files
C
      CALL ACLOSE(0,IER)
C
C?   Get End of Job Counter
C
      CALL ALTIME(AJBFRT)
      TIME = AJBFRT-AJBSRT
C
      IF (JPRERF(JULSK).GE.1) WRITE(LDEBRL,500) TIME
  500 FORMAT(//,1X,80('!'),/,1X,30X,'This job took ',F12.2,
     +' CPU seconds',/,1X,80('!'))
C--------------------------------------------------------------------
      END
#endif
