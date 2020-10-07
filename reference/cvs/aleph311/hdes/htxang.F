         REAL FUNCTION HTXANG(IDHEI,ILAY)
C----------------------------------------------------------
C
CKEY HCALDES HCAL EIGHTFOLD TUBE ANGLE /USER
C!  Return the double_eightfold inclination
C!
C!                          Author: G.Catanesi 5/06/89
C!
C!         INPUT:
C!                IDHEI/I = double_eightfold#
C!                ILAY/I  = layer#
C!
C---------------------------------------------------------------
#ifndef DOC
#include "hcgeom.h"
C
        HTXANG = 0.
C
        IF(ILAY.LE.NHCINL)THEN
           HTXANG = HCTAEI(IDHEI)
        ELSE
           HTXANG = HCTAEO(IDHEI)
        ENDIF
C
        END
#endif
