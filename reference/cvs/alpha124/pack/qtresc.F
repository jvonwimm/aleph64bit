      SUBROUTINE QTRESC
CKEY FILL /INTERNAL
C-----------------------------------------------------------------------
C! Apply TPCORR to all charged tracks
C! called from QFILL 
C!                                                   F.Ligabue  16.07.98
C  modified I. Tomalin  19.10.98
C-----------------------------------------------------------------------
#ifndef DOC
#include "qdecl.h"
#include "qcde.h"
      COMMON / QTRESCI / ITRESC
      LOGICAL ASCALE
      PARAMETER (ASCALE=.TRUE.)
      REAL PSCALE
#include "qmacro.h"
C-----------------------------------------------------------------------
      NVDET = 0
C read user-provided NVDET
C
      IPCOR = NLINK('PCOR',0)
      IF(IPCOR.LE.0) RETURN
      NVD = 0
      IF (IW(IPCOR).EQ.1) NVD = IW(IPCOR+1)
      NVDET = MAX(NVD,0)
C
      IF (ITRESC.EQ.1) THEN
C Rescale charged tracks:
        DO ITK = KFCHT,KLCHT
          PSCALE = QPCORR(ITK,ASCALE,NVDET,ERCORR)
        ENDDO
C rescale DCT section as well
        DO ITK = KFDCT,KLDCT
          PSCALE = QPCORR(ITK,ASCALE,NVDET,ERCORR)
        ENDDO

      ELSE IF (ITRESC.EQ.2.AND.XMINI) THEN
C Rescale EFLOW objects which are charged tracks (MINI input only):
        DO ITK = KFEFT,KLEFT
          IF (KEFOTY(ITK).LE.3) PSCALE = QPCORR(ITK,ASCALE,NVDET,ERCORR)
        ENDDO 
      END IF
      END
#endif
