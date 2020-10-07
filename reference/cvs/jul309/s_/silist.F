      INTEGER FUNCTION SILIST(IAD,NROW)
C-------------------------------------------------------------
C!  - Check if address IAD is among first NROW rows of bank SPDA
C!
C!    Author B.Bloch-Devaux JUNE 92
C!    Input:  IAD pad address ( 14 bits)
C!            NROW current number of rows to be searched in SPDA
C!    Return value : SILIST = row number if address exists
C!                            0 if not found
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "sinamc.h"
#include "spdajj.h"
#include "bmacro.h"
      SILIST = 0
        KSPDA = IW(NASPDA)
      IF ( KSPDA.GT.0 ) THEN
        DO 10 I = 1 ,NROW
          IF (IAD.EQ.ITABL(KSPDA,I,JSPDAD))GO TO 11
   10   CONTINUE
        GO TO 99
   11   CONTINUE
        SILIST = I
      ENDIF
   99 RETURN
      END
#endif
