         REAL FUNCTION HTXDTL(IDHEI,ILAY)
C---------------------------------------------------------
CKEY HCALDES HCAL EIGHTFOLD TUBE LENGTH /USER
C
C!  Return the double_eightfold maximum length
C!
C!                          Author: G.Catanesi 5/06/89
C!
C!         INPUT:
C!                IDHEI/I = double_eightfold#
C!                ILAY/I  = layer#
C!
C----------------------------------------------------------
#ifndef DOC
#include "hcgeom.h"
C
            HTXDTL = 0.
C
            IF(ILAY.LE.NHCINL)THEN
               HTXDTL = HCTLEI(1,IDHEI)
            ELSE
               HTXDTL = HCTLEO(1,IDHEI)
C
C  Special tratement for layer 8
C
               IF(ILAY.EQ.NHDLEC)THEN
                 IF(IDHEI.EQ.3)THEN
                    HTXDTL = HCTLEO(1,14)
                 ENDIF
                 IF(IDHEI.EQ.7)THEN
                    HTXDTL = HCTLEO(1,16)
                 ENDIF
                 IF(IDHEI.EQ.13)THEN
                    HTXDTL = HCTLEI(1,9)
                 ENDIF
                 IF(IDHEI.EQ.18)THEN
                    HTXDTL = 0.
                 ENDIF
               ENDIF
            ENDIF
C
            RETURN
            END
#endif
