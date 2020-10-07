      SUBROUTINE E2BPEI(ENT,ISO)
C----------------------------------------------------------------------
C!  - FILL PEID WORDS IN TWO BYTES
C!
C!  AUTHOR   : D. PALLIN
C!  MODIFIED :
C!
C?
C!======================================================================
#ifndef DOC
      RAT=65534./100.
      IF(ENT.GT.100.)ISO=65535
      ISO=NINT(RAT*ENT)
      RETURN
      END
#endif
