            SUBROUTINE HNEIGH(ITUB,ILAY,IPOR,IHEIF,IDHEI)
C-----------------------------------------------------------
CKEY HCALDES HCAL EIGHTFOLD TUBE STATUS /USER
C
C!   Evaluate eightfold and double_eightfold number
C!   (if IHEIF=0 tube is outside the range
C!
C!                             Author:G.Catanesi 6/06/89
C!
C!         INPUT:
C!                 ITUB/I = Tube#
C!                 ILAY/I = Layer#
C!                 IPOR/I = Portion#
C!
C!         OUTPUT:
C!                 IHEIF/I = eightfold#
C!                 IDHEI/I = double_eightfold#
C!
C---------------------------------------------------------------
#ifndef DOC
C
#include "hcgeom.h"
C
            IHEIF = 0
            IDHEI = 0
C
            IHEIF = (ITUB-1)/NHCEIT + 1
            IDHEI = (IHEIF-1)/2 + 1
C
            IF(IPOR.EQ.LPBAR)THEN
               LAST = NEITHC(ILAY)
               IF(IHEIF.LE.LAST)GOTO 99
            ELSE
               IF(ILAY.LE.NHCINL)THEN
                  LAST = NHCIND
               ELSE
                  LAST = NHCOUD
               ENDIF
               IF(IDHEI.LE.LAST)GOTO 99
            ENDIF
C
            IHEIF = 0
            IDHEI = 0
C
C  Good tube
 99         CONTINUE
            END
#endif
