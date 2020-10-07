      SUBROUTINE QFLEPT
CKEY FILL LEPTONS /INTERNAL
C----------------------------------------------------------------------
C!  - Fill Lepton Tags from PDLT/PMLT banks
C!
C!   Author   :- J. Boucrot     10-Nov-1994
C!
C!  Called from QFILL or from QLEVNT
C!======================================================================
#ifndef DOC
#include "qcde.h"
#include "qmacro.h"
C----------------------------------------------------------------------
C Lepton identification banks PDLT and PDMT :
C
      NAPDLT  = NAMIND('PDLT')
      NAPMLT  = NAMIND('PMLT')
      JPDLT = IW(NAPDLT)
      JPMLT = IW(NAPMLT)
C      IF (JPDLT.EQ.0) GO TO 999
C Loop on all charged tracks , find the JULIA track number :
      JQDET = KOQDET + KCQDET * (KFCHT-KLUST)
      DO 500 NJUL = 1, KNCHT
        IW(JQDET+JQDEPD) = KQZER
        IW(JQDET+JQDEPM) = KQZER
        IF (JPDLT.GT.0) THEN
           DO 100 IPDLT = 1,LROWS(JPDLT)
              KPDLT = KROW(JPDLT,IPDLT)
              IF (IW(KPDLT+JPDLFR).NE.NJUL) GO TO 100
              IW(JQDET+JQDEPD) = KPDLT
              GO TO 101
 100       CONTINUE
        ENDIF
 101    CONTINUE
        IF (JPMLT.GT.0) THEN
           DO 200 IPMLT = 1,LROWS(JPMLT)
              KPMLT = KROW(JPMLT,IPMLT)
              IF (IW(KPMLT+JPMLFR).NE.NJUL) GO TO 200
              IW(JQDET+JQDEPM) = KPMLT
              GO TO 201
 200       CONTINUE
        ENDIF
 201    JQDET = JQDET + KCQDET
 500  CONTINUE
C
C Now build the 'LJET' section of jets from QSELEP ( from bank PLJT )
C
      CALL QFLJET
C
C Update EDIR class word for Lepton tags ( classes 27/28 ) :
C
      CALL QEDIRUPD
C
 999  RETURN
      END
#endif
