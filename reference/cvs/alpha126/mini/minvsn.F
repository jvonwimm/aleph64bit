      INTEGER FUNCTION MINVSN(DUMMY)
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Mini-DST version number (of current library).
C
C     This can be overrode by use of MVSN card which allows a new
C     version to be started without changing MINI code.
C     Useful if other code has major modifications.
C-----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "mversn.h"
C
      PARAMETER ( MVEROK = 205 )
      LOGICAL FIRST
      SAVE MVER,FIRST
      DATA FIRST / .TRUE. /
C-----------------------------------------------------------------------
C
C++   Check for card input.
C
      IF (FIRST) THEN
         KMVSN = NLINK('MVSN',0)
         IF (KMVSN.GT.0) THEN
            MVER = IW(KMVSN+1)
      ELSE
         MVER = MINIVER
         ENDIF
         FIRST = .FALSE.
      ENDIF
C Overwrite MVSN card:
      MINVSN = MVEROK
C
      RETURN
      END
#endif
