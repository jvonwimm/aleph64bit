          REAL FUNCTION HTXSPL(IDHEI,ILAY)
C-----------------------------------------------------------
CKEY HCALDES HCAL EIGHTFOLD TUBE LENGTH /USER
C
C!  Return the difference in lenght from one wire and the following
C!  in a double_eightfold
C!
C!                          Author: G.Catanesi 5/06/89
C!
C!         INPUT:
C!                IDHEI/I = double eightfold#
C!                ILAY/I  = layer#
C!
C-----------------------------------------------------------
#ifndef DOC
#include "hcgeom.h"
            HTXSPL  = 0.
C
            IF(ILAY.LE.NHCINL)THEN
               HTXSPL = HCTLEI(1,IDHEI) - HCTLEI(4,IDHEI)
            ELSE
               HTXSPL = HCTLEO(1,IDHEI) - HCTLEO(4,IDHEI)
C
C  Special tratement for layer 8
C
               IF(ILAY.EQ.NHDLEC)THEN
                 IF(IDHEI.EQ.3)THEN
                    HTXSPL = HCTLEO(1,14) - HCTLEO(4,14)
                 ENDIF
                 IF(IDHEI.EQ.7)THEN
                    HTXSPL = HCTLEO(1,16) - HCTLEO(4,16)
                 ENDIF
                 IF(IDHEI.EQ.13)THEN
                    HTXSPL = HCTLEI(1,9) - HCTLEI(4,9)
                 ENDIF
                 IF(IDHEI.EQ.18)THEN
                    HTXSPL = 0.
                 ENDIF
               ENDIF
            ENDIF
C
            HTXSPL = ABS(HTXSPL/(2*NHCEIT))
C
            RETURN
            END
#endif
