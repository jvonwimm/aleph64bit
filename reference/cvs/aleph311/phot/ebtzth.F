      SUBROUTINE EBTZTH(ESTHR,IRC)
C -----------------------------------------------------------------
CKEY PHOTONS ECAL ROC EZTH
C  Author : R.Edgecock - 900115
C! Unpacks the EZTH bank to find the zero suppression thresholds
C  used the ECAL ROCs.
C
C   Input:    None
C   Output:   ESTHR(STACK,ECAL_SUBCOMPONENT) = the thresholds per stack
C                                               for ECA, barrel and ECB,
C                                               in MeV(!)
C             IRC = 0 - all OK
C             IRC = 1 - EZTH doesn't exist, defaults used
C             IRC = 2 - EZTH exists, but cannot be used, defaults taken
C             IRC = 3 - Bank error, defaults used
C -------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
      REAL ESTHR(3,3)
      SAVE NEZTH
C     for safety SAVE all
      SAVE
      DATA NEZTH /0/
C
      IF (NEZTH.EQ.0) NEZTH = NAMIND ('EZTH')
C
C Fill ESTHR with the default values
C
      ESTHR(1,1) = 28.
      ESTHR(2,1) = 44.
      ESTHR(3,1) = 60.
      ESTHR(1,2) = 20.
      ESTHR(2,2) = 28.
      ESTHR(3,2) = 42.
      ESTHR(1,3) = 28.
      ESTHR(2,3) = 44.
      ESTHR(3,3) = 60.
      IRC = 0
C
C Find EZTH bank
C
      KEZTH = IW(NEZTH)
      IF (KEZTH.EQ.0) THEN
         IRC = 1
         GOTO 999
      ENDIF
      NW    = IW(KEZTH + 1)
      NMODS = IW(KEZTH + 2)
      IF (NMODS.NE.3) THEN
C Situation is too complicated for the existing software...use defaults
C and quit
         IRC = 2
         GOTO 999
      ENDIF
C
      DO 30 IMOD = 1,NMODS
         MODNU = IW(KEZTH + 2 + (IMOD-1)*NW + 1)/65536
         IF (MODNU.LE.0.OR.MODNU.GE.37) THEN
            IRC = 3
            GOTO 999
         ENDIF
         MODR = 2
         IF (MODNU.LE.12) MODR = 1
         IF (MODNU.GT.24) MODR = 3
         ESTHR(1,MODR) = RW(KEZTH + 2 + (IMOD-1)*NW + 2)
         ESTHR(2,MODR) = RW(KEZTH + 2 + (IMOD-1)*NW + 3)
         ESTHR(3,MODR) = RW(KEZTH + 2 + (IMOD-1)*NW + 4)
   30 CONTINUE
C
  999 CONTINUE
      RETURN
      END
#endif
