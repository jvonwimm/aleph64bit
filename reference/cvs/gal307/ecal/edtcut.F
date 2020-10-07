*DK edtcut
      INTEGER FUNCTION EDTCUT(IMOD,ISTK,JPHI,KTET)
C-----------------------------------------------------------------------
C     O.CALLOT   3-DEC-85
C!   Modified :- E. Lancon              3-DEC-1990
C.
C.  give the ADC count threshold for mode IMOD ( =1 or 2 )
C   take zero sup. values from EZTH bank
C. - called from ECTDZS                                    this .HLB
C-----------------------------------------------------------------------
#ifndef DOC
#include "edpara.h"
      SAVE NCALL
      EXTERNAL EBTZTH
      DATA NCALL / 0 /
C
C?   Get Zero Sup. Thresholds From EZTH Bank
C
      IF ( NCALL.EQ.0 ) THEN
        CALL EBTZTH (EDZTHR,IRC)
        NCALL = 1
      ENDIF
C
      IREG = 1
      IF (KTET.GE.51.AND.KTET.LE.178) IREG=2
      IF (KTET.GE.179) IREG=3
C
      IF(IMOD.EQ.1) THEN
        SIG = 1.
      ELSE IF(IMOD.EQ.2) THEN
        SIG = EDCUT2 / EDCUT1
      ENDIF
      EDTCUT = INT(EDTGAI(ISTK)*SIG*EDZTHR(ISTK,IREG)*1000.
     &     + 1.)
      RETURN
      END
#endif
