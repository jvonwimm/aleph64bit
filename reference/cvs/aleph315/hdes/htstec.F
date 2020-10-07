              REAL FUNCTION HTSTEC(ITUB,ILAY,IDHEI)
C--------------------------------------------------------------
CKEY HCALDES HCAL ENDCAP POSITION TUBE /USER
C
C!  Return the starting position of the tube ITUB in the EndCap
C!
C!                                  Author:G.Catanesi 14/06/89
C!
C!  INPUT:
C!           ITUB/I = tube#
C!           ILAY/I = layer#
C!           IDHEI/I= double_eightfold#
C!
C--------------------------------------------------------------
#ifndef DOC
#include "alcons.h"
#include "hcgeom.h"
                HTSTEC = 0.
C
C iron wall + dead zone inside tube
                XDEAD = HCDRBM(LPECA)+HTPIEC-HTINEC(2)
C
C Double_eightfold at 0 degrees
                IF(IDHEI.EQ.1)THEN
                   HTSTEC = HCRMIN(2) + XDEAD + HBWCEC(2)
                ELSEIF(IDHEI.EQ.2)THEN
                      XDIFF  = HTXDTL(IDHEI,ILAY) -
     &                            HTXDTL(IDHEI-1,ILAY)
                   IF(MOD((ITUB-1),2*NHCEIT).GE.NHCEIT)THEN
                      XDIFF  = XDIFF + 2*NHCEIT*HTXSPL(IDHEI,ILAY)
                   ENDIF
                   HTSTEC = HCRMIN(2) + XDEAD + HBWCEC(2) - XDIFF
                ELSEIF(IDHEI.EQ.3)THEN
                   YGAP   = HCDRBM(LPECA) + NHDET0*HCDOWI
                   XGAP   = YGAP/TAN(PIBY3)
                   HTSTEC = XGAP + XDEAD +HBWCEC(2)
                ELSE
C  Double_eightfold at 30 degrees
                   YGAP   = HCDRBM(LPECA) +
     &                         INT((IDHEI-1)/NHDEBS)*(HCDSTP+HCAPSL) +
     &                         MOD((IDHEI-1),NHDEBS)*HCDOWI
                   XGAP   = YGAP/TAN(PIBY3)
                   HTSTEC = XDEAD + XGAP + HBWCEC(1)  +
     &                     MOD((ITUB-1),2*NHCEIT) * HTXSPL(IDHEI,ILAY)
                ENDIF
C
                RETURN
                END
#endif
