      INTEGER FUNCTION LSUMAM(IPAD)
C--------------------------------------------------------------
C! Find summing amplifier
C!   Author :      P.H.Hansen - 891010
C!   Input  : IPAD    = Tower address
C!   Output : LSUMAM  = Amplifier No. [1,48]
C?
C!======================================================================
#ifndef DOC
      DIMENSION LOCAM(15,16)
      DATA LOCAM / 0, 0, 0, 0, 0, 3, 3, 3, 3, 3, 3, 3, 6, 6, 0,
     +             0, 0, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 6, 6, 0,
     +             0, 0, 0, 2, 2, 3, 3, 3, 3, 3, 3, 3, 6, 6, 6,
     +             1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 6, 6, 6, 6,
     +             1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 6, 6, 6, 6,
     +             1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 6, 6, 6, 6,
     +             1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 6, 6, 6, 6, 6,
     +             1, 1, 1, 1, 2, 2, 2, 2, 2, 5, 5, 6, 6, 6, 6,
     +             1, 1, 1, 1, 1, 2, 2, 2, 5, 5, 5, 5, 6, 6, 0,
     +             1, 1, 1, 1, 1, 1, 2, 5, 5, 5, 5, 5, 6, 6, 0,
     +             1, 1, 1, 1, 1, 5, 5, 5, 5, 5, 5, 5, 5, 0, 0,
     +             4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 0, 0, 0,
     +             4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 0, 0, 0, 0,
     +             4, 4, 4, 4, 4, 4, 4, 5, 5, 0, 0, 0, 0, 0, 0,
     +             4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0,
     +             4, 4, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
#include "lcmacr.h"
C -------------------------------------------------------------
      LSUMAM = 0
C
C - Decode pad number
      MODU = LCMOD(IPAD)
      IY = LCROW(IPAD)
      IF (IY .LE. 0 .OR. IY .GT. 30) GOTO 999
      ICOL = LCCOL(IPAD)
      IF (ICOL .LE. 0 .OR. ICOL .GT. 16) GOTO 999
C
C - Reflect everything into module 1, lower half
      IUP = 0
      IF (IY .GT. 15) THEN
        IROW  = IY - 15
        IUP = 1
      ELSE
        IROW  = 16 - IY
      ENDIF
      IF (LOCAM(IROW,ICOL).EQ. 0)                    GOTO 999
C
C - Encode amplifier
      LSUMAM   = 12*(MODU-1) + IUP*6 + LOCAM(IROW,ICOL)
  999 RETURN
      END
#endif
