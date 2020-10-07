      SUBROUTINE E3BPEI(ENT,ISO)
C----------------------------------------------------------------------
C!  - FILL PEID WORDS IN ONE BYTE
C!
C!  AUTHOR   : D. PALLIN
C!  MODIFIED :
C!
C?
C!======================================================================
#ifndef DOC
      ISO=NINT(255.*ENT)
      RETURN
      END
#endif
