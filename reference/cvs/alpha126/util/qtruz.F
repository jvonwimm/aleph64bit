      SUBROUTINE QTRUZ
C***********************************************************************
C Reset the history of the event wrt MC-EF matching.
C Author: Paul Bright-Thomas
C Mods:     03/03/97 PBT Created.
C***********************************************************************
#ifndef DOC
      INTEGER II,JJ
#include "mcef.h"
C***********************************************************************
C-- Zero the EF-MC matches
      NEFIN = 0
      DO 100 II = 1,MXEF
        NEFMC(II)  = 0
        ITEFIN(II) = 0
        DO 110 JJ = 1,MXMTCH
          ITEFMC(II,JJ) = 0
          SEFMC(II,JJ)  = 0.
 110    END DO
 100  END DO
C-- Zero the MC-EF matches
      NMCIN = 0
      DO 200 II = 1,MXMC
        NMCEF(II)  = 0
        ITMCIN(II) = 0
        DO 210 JJ = 1,MXMTCH
          ITMCEF(II,JJ) = 0
          SMCEF(II,JJ)  = 0.
 210    END DO
 200  END DO
C
 999  RETURN
      END
#endif
