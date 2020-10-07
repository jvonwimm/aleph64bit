      SUBROUTINE QTRESC
CKEY FILL /INTERNAL
C-----------------------------------------------------------------------
C! Apply TPCORR to all tracks
C! called from QFILL 
C!                                                   F.Ligabue  16.07.98
C-----------------------------------------------------------------------
#ifndef DOC
#include "qdecl.h"
#include "qcde.h"
      LOGICAL ASCALE
#include "qmacro.h"
C-----------------------------------------------------------------------
       ASCALE = .TRUE.
       NVDET = 0
C read user-provided NVDET
C
       IPCOR = NLINK('PCOR',0)
       IF(IPCOR.LE.0) RETURN
       NVD = IW(IPCOR+1)
       IF (NVD.LT.0) NVD = 0 
       NVDET = MIN(NVD,1)
       DO ITK = KFCHT,KLCHT
          CALL QPCORR(ITK,ASCALE,NVDET,ERCORR)
       ENDDO
C rescale DCT section as well
       DO ITK = KFDCT,KLDCT
          CALL QPCORR(ITK,ASCALE,NVDET,ERCORR)
       ENDDO
      END
#endif
