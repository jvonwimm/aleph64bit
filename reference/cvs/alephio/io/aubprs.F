      SUBROUTINE AUBPRS (LIST)
C -----------------------------------------------------------
C - F.RANJARD - 860307
C!  Compress the banks put on the list 'LIST', following the
C   convention : bank length = 1st word * 2nd word + LMHLEN
C - Calls       NLISTB, NBANK                   from BOS77.hlb
C  -----------------------------------------------
#ifndef DOC
#include "bcs.h"
      EXTERNAL NLISTB, NBANK
      CHARACTER LIST*(*), NAME*4, NLISTB*4
C -------------------------------------------------------------
      I = 0
 1    I = I+1
      NAME = NLISTB (IW,I,LIST)
      IF (NAME.EQ.' ') GOTO 999
      KNAME = NAMIND(NAME)+1
 2    KNAME = IW(KNAME-1)
      IF (KNAME.NE.0) THEN
         LE = IW(KNAME+1)*IW(KNAME+2) + LMHLEN
         IF (LE .EQ. LMHLEN) THEN
            IDROP = NDROP (NAME,IW(KNAME-2))
         ELSE IF (LE .LT. IW(KNAME)) THEN
            KNAME = NBANK (NAME,IW(KNAME-2),LE)
         ENDIF
         GOTO 2
      ENDIF
      GOTO 1
C
 999  CONTINUE
      RETURN
      END
#endif
