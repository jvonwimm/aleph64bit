      SUBROUTINE ECOENT(IER)
C-----------------------------------------------------------------------
C! Compute correction to total EcObject energy
C!     AUTHOR :JP Albanese
C!     MODIFIED : D PALLIN
C?
C!======================================================================
#ifndef DOC
      PARAMETER ( MXST = 100 )
      DIMENSION INDX(3,MXST) , ESTO(MXST) , EVLP(5)
#include "eanclu.h"
#include "ecobjj.h"
#include "ecnamc.h"
#include "bcs.h"
#include "bmacro.h"
       CALL ECLOST(ICLNEA,ECLOSS,IER)
       IF(ICREEA.GE.64)THEN
          CALL EBPREP(MXST,ICLNEA,NUST,INDX,ESTO,IER2)
          IF( IER2 .EQ. 0 ) THEN
             CALL EBCLAP(IOPT,NUST,INDX,ESTO,EVLP,IVLP)
             IF( IVLP .GT. 0 ) THEN
                DO 1 I = 1 , 5
                   ECLOSS = ECLOSS + EVLP(I)
    1           CONTINUE
             ENDIF
          ENDIF
      ENDIF
       KECOB = IW(NAECOB)
C
C? Get total uncorrected energy in JECOEG
       ENER = RTABL(KECOB,ICLNEA,JECOEG)
       RW(KROW(KECOB,ICLNEA)+JECOEG) = ENER + ECLOSS
C
 999  RETURN
      END
#endif
