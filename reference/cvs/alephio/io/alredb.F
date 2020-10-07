      INTEGER FUNCTION ALREDB (LBASE,LIST)
C -------------------------------------------------------------------
C - F.Ranjard - 870505
C! Read the data base
C - input   : LBASE = data base logical unit (must be .ne. 0)
C             LIST  = list of bank names to be retreived
C - output  : ALREDB= # of banks found (0 if error)
C   ---------------------------------------------------
#ifndef DOC
      CHARACTER*(*) LIST,NAME*4,NLIST*4,CHAINT*4,TKARD*4
#include "bcs.h"
#include "bmacro.h"
C ---------------------------------------------------------------
      LCARD = IW(5)
      IW(5) = LBASE
C
      ALREDB = 0
C
 1    CONTINUE
      KARD = MREADC (IW)
      IF (KARD .NE. 0) THEN
         TKARD = CHAINT (IW(KARD-3))
         I = 0
 2       I = I+1
         NAME = NLIST (IW,I,LIST)
         IF (NAME .NE. ' ') THEN
            IF (TKARD .NE. NAME) GOTO 2
            CALL BLIST (IW,'C+',TKARD)
            ALREDB = ALREDB + 1
            GOTO 1
         ELSE
            CALL BDROP (IW,TKARD)
         ENDIF
         GOTO 1
      ENDIF
C
      IW(5) = LCARD
      END
#endif
