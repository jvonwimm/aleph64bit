      SUBROUTINE E1BPEI(ENT,ISO)
C----------------------------------------------------------------------
C!  - FILL PEID WORDS IN ONE BYTE
C!
C!  AUTHOR   : D. PALLIN
C!  MODIFIED :
C!
C?
C!======================================================================
#ifndef DOC
      ISO=NINT(ENT*10.)
      IF(IABS(ISO).GT.9999) GOTO 1
      IF(ISO.GT.127)ISO=127
      IF(ISO.LT.-127)ISO=-127
1     IF(IABS(ISO).GT.9999)ISO=128
      RETURN
      END
#endif
