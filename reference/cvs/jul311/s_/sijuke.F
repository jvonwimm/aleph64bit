      SUBROUTINE SIJUKE( NWAG,DELAY,DELTA)
C----------------------------------------------------------------------
C!  - SICAL decode Juke box configuration
C!
C!   Author   :- E. Lancon    October 1995
C!   Input    : SRCO bank in Run header
C!   Output   : NWAG  = number of Hold signals
C!              DELAY = delay ( in ns) for the first Hold
C!              DELTA = time ( in ns ) between 2 successive Hold's
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
      EXTERNAL CHAINT
      CHARACTER*4 CHAINT
#include "bmacro.h"
C Access the Run header bank
      NASRCO = NAMIND('SRCO')
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C   SiCAL configuration
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      KSRCO = IW(NASRCO)
      JUKENB = 4
      JUKEOF = 272
      JUKEDT = 62
      JUKENW = 1
      IF ( KSRCO.GT.0 ) THEN
        DO 10 ISRCO = 1, LROWS(KSRCO)
C  the key word is JUKE for the time step , CHOO for the config
          IF (CHAINT(ITABL(KSRCO,ISRCO,1)).EQ.'JUKE'  ) THEN
            JUKENB = ITABL(KSRCO,ISRCO,2)
          ENDIF
          IF (CHAINT(ITABL(KSRCO,ISRCO,1)).EQ.'CHOO'  ) THEN
            IWORD = ITABL(KSRCO,ISRCO,2)
            JUKENW = IBITS(IWORD,8,2)+1
            JUKEDT = IBITS(IWORD,0,8)
            JUKEOF = IBITS(IWORD,10,6)*16
          ENDIF

 10   CONTINUE
      ENDIF
C
      NWAG = JUKENW
      DELAY = JUKEOF
      DELTA = JUKEDT*JUKENB
      RETURN
      END
#endif
