      INTEGER FUNCTION ALFIND (LIST,NAME)
C ----------------------------------------------------------
C - F.Ranjard - 880301
C!  Find NAME bank in LIST list
C - Input  : LIST   = list name
C             NAME  = bank name which is searched
C - Output : ALFIND = serial number of the NAME bank in the list
C                     0 means the bank is not in the list
C --------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
      CHARACTER LIST*(*), NAME*4, NLISTB*4, NAML*4
C ----------------------------------------------------------------
      I = 0
 1    I = I+1
      NAML = NLISTB(IW,I,LIST)
      IF (NAML .NE. ' ') THEN
         IF (NAML .NE. NAME) GOTO 1
         ALFIND = I
      ELSE
         ALFIND = 0
      ENDIF
C
      END
#endif
