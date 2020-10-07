      INTEGER FUNCTION FYNHIS (IOHIS)
C ---------------------------------------------------------
CKEY FYXX  / INTERNAL
C! Return the new history code
C - F.Ranjard - 881125
C - Input     : IOHIS  old history code
C - Output    : FYNHIS new history code
C - Called by : FYFKIN
C               assume that JDKNFO exists, use IHTYPE history type
C               to recalculate the history code properly.
C               returns the old history code in case the history type
C               is unknown, or a new history code cannot be computed.
#ifndef DOC
      SAVE
#include "bcs.h"
#include "fyrela.h"
      DATA ILUD/ 10000/
#include "bmacro.h"
C ---------------------------------------------------------------
C
      FYNHIS = IOHIS
C
      IF (IHTYPE .EQ. 0) THEN
C -    LUND history code or NO history code
         IOMOM = MOD(IOHIS,ILUD)
         IF (IOMOM .NE. 0) THEN
            INEW = ITABL (JDKNFO,IOMOM,1)
            FYNHIS = INEW + ILUD*(IOHIS/ILUD)
         ENDIF
      ENDIF
C
C
C  special for HERWIG history code
C
      IF (IHTYPE .EQ. 1) THEN
         IOMOM = MOD(IOHIS,1000000)
         IOMOM2= IOMOM / 1000
         IOMOM = IOMOM - IOMOM2*1000
         IF (IOMOM .NE. 0) THEN
            INEW = ITABL (JDKNFO,IOMOM,1)
            FYNHIS = INEW + 1000000*(IOHIS/1000000)
            IF (IOMOM2.NE. 0) THEN
               INEW = ITABL (JDKNFO,IOMOM2,1)
               FYNHIS = INEW*1000 + FYNHIS
            ENDIF
         ENDIF
      ENDIF
C
      END
#endif
