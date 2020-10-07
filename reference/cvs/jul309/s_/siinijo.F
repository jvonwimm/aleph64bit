      SUBROUTINE SIINIJO
C----------------------------------------------------------------------
C!  - SICAL Job Initialisation
C!
C!   Author   : B. Bloch-Devaux 911029
C!              Modified for Bunch train mode October 1994
C!              Book SSTA bank (moved here from SIINIRU) M.Cattaneo Sept. 97
C!
C!   Description :
C!   ===========
C?   Set name indices for Scal banks
C?   Init Lumi counters
C?   Reset reconstruction job statistics
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "sinamc.h"
      PARAMETER ( LSSTA = 9)
C Fix number of wagons to maximum (for SSTA internal bank)
      PARAMETER ( NWAG = 4 )
      COMMON/SILUMC/NSILEV(25,4)
C
C----------------------------------------------------------------------
C
C Name indices
      CALL SINAMX
C Lumi statistics
      CALL VZERO(NSILEV,100)
C
C SSTA bank
      IF ( IW(NASSTA).LE.0) THEN 
        LEN = NWAG*LSSTA+LMHLEN
        CALL AUBOS('SSTA',0,LEN,KSSTA,IGARB)
        IF ( IGARB.EQ.2) THEN
          CALL RERROR ('SIINIJO',-1,'Unable to create SSTA')
        ELSE
          IW(KSSTA+1) = LSSTA
          IW(KSSTA+2) = NWAG
        ENDIF
      ENDIF
C
      RETURN
      END
#endif
