      INTEGER FUNCTION ALTRHV (IBITHV)
C --------------------------------------------------------------
CKEY ALEF TRIGGER HV
C - F.Ranjard -940426
C! returns trigger HV word from XTCN or X1RG
C - Output : - IBITHV  / I = word 4
C              ALTRHV  / I = 0 if no bank there
C                            1 if XTCN is there
C                            2 if X1RG is there
C
C ----------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
      SAVE NXTCN, NX1RG
      DATA NXTCN / 0/
#include "bmacro.h"
C ------------------------------------------------------------
C - 1st entry : get name indices
      IF (NXTCN.EQ.0) THEN
        NXTCN = NAMIND ('XTCN')
        NX1RG = NAMIND ('X1RG')
      ENDIF
C
C - next entry
      IF (IW(NX1RG).NE.0) THEN
         JTRIG = IW(NX1RG)
         IROW  = 2
         ICOL  = 4
         ALTRHV = 2
      ELSEIF (IW(NXTCN).NE.0) THEN
         JTRIG = IW(NXTCN)
         IROW  = 1
         ICOL  = 7
         ALTRHV = 1
      ELSE
         ALTRHV = 0
         RETURN
      ENDIF
C
C - get HV trigger word
      IBITHV = ITABL (JTRIG,IROW,ICOL)
C
      END
#endif
