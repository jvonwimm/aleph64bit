      SUBROUTINE TCDUMP
C
C----------------------------------------------------------------------
C! Print TPC clusters
C!
C!    Author:    D. Schlatter
C!    Modified:  R. Johnson   6-05-87   5-09-93
C!
C!    Called by TPREDA
C!
C---------------------------------------------------------------------
#ifndef DOC
C
#include "tparac.h"
#include "tpgpar.h"
#include "tclujj.h"
#include "tcrljj.h"
#include "tlrljj.h"
#include "tpuljj.h"
#include "rlunit.h"
#include "rcurnt.h"
#include "twirct.h"
#include "bcs.h"
#include "tflags.h"
#include "tpatel.h"
#include "tpgeom.h"
C
C--------------------------- END COMMON -------------------------------
C
#include "bmacro.h"
#include "tsfunc.h"
C
C------------------------------------------------------------------
C
      WRITE(LDEBRL,1005) IRUNRC,IEVTRC
      KTSEC=IW(NAMIND('TSEC'))
C
      IF (.NOT.NOPADS) THEN
C
        MAL=NTSECT
        IF (KTSEC.NE.0) MAL=IW(KTSEC)
C
        DO 100 M=1,MAL
          IF (KTSEC.EQ.0) THEN
            ISECT=M
          ELSE
            ISECT=IW(KTSEC+M)
          ENDIF
C
          KTCLU=NLINK('TCLU',ISECT)
          IF (KTCLU.EQ.0) GO TO 100
C
          WRITE(LDEBRL,1000) ISECT
          IF (IW(KTCLU+LMHROW).LE.0) GO TO 100
C
C++       Loop over and dump all clusters in the sector
C
          DO 90 IC=1,IW(KTCLU+LMHROW)
            CALL TCLDMP(IC,ISECT,LDEBRL)
   90     CONTINUE
  100   CONTINUE
      ENDIF
C
      IF (FNOWIR) RETURN
C
      CALL TWRRDP(ISECT,LDEBRL)
C
      RETURN
C
 1000 FORMAT(/20X,' *** TPC pad dump of sector ',I2,' ***')
 1005 FORMAT(/2X,20(1H=),' Run = ',I6,'  Event = ',I10,1X,20(1H=) )
      END
#endif
