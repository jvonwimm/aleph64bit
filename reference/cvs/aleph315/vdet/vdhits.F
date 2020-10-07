      SUBROUTINE VDHITS(IHITPAT,MAXTRK)
C ---------------------------------------------------------------------
C!  count the number of VDET hits by layer and view.
CKEY VDET HITS
C
C  Input:  MAXTRK       = Dimension of IHITPAT array, should
C                         be large enough to contain the largest event
C                         (IE around 200)
C          FRFT and VDCO must be in the bank list
C
C  Output: IHITPAT, an integer array numbered by FRFT track number,
C          of which only the first 8 bits are used, as described below
C          bits:     meaning
C          0-1       number of inner layer U hits on track (0,1,or2)
C          2-3       number of outer layer U hits on track     "
C          4-5       number of inner layer W hits on track     "
C          6-7       number of outer layer W hits on track     "
C
C  10-3-92 Dave Brown
C ----------------------------------------------------------------------
#ifndef DOC
      INTEGER IHITPAT(*),MAXTRK
      INTEGER NAMIND
      INTEGER NVDCO,IVDCO,NFRFT,IFRFT,NHIT,IHIT,NTRK,ITRK,QF
      INTEGER IMOD,ILAY,IWAF,IPHI,IVIEW
C
#include "bmacrod.h"
#include "bcs.h"
      DATA NVDCO,NFRFT / 0, 0/
#include "bmacro.h"
C
C  Initialize the name indices the first time through
C
      IF(NVDCO.EQ.0) THEN
        NVDCO = NAMIND('VDCO')
        NFRFT = NAMIND('FRFT')
      END IF
C
C  Find the VDCO and FRFT banks
C
      IVDCO = IW(NVDCO)
      IFRFT = IW(NFRFT)
C
C  If no FRFT bank, return
C
      IF(IFRFT .LE. 0)RETURN
C
C  Zero the entries
C
      NTRK = MIN(LROWS(IFRFT),MAXTRK)
      DO 600 ITRK=1,NTRK
        IHITPAT(ITRK) = 0
 600  CONTINUE
C
C  If no VDCO bank, return
C
      IF(IVDCO .LE. 0)RETURN
C
C  Loop over the hits
C
      NHIT = LROWS(IVDCO)
      DO 700 IHIT=1,NHIT
C
C  Get the quality flag, track number, and row number from the hit
C
        ITRK = ITABL(IVDCO,IHIT,8)
        QF = ITABL(IVDCO,IHIT,7)
        IMOD = ITABL(IVDCO,IHIT,1)
        CALL VADEWA(IMOD,ILAY,IWAF,IPHI,IVIEW)
C
C   Separate the U and W hits and layers
C
        IF(ITRK .LE. MAXTRK)THEN
          IF(ILAY.EQ.1)THEN
            IF(IAND(QF,1).EQ.1)IHITPAT(ITRK) = IHITPAT(ITRK) + 1
            IF(IAND(QF,2).EQ.2)IHITPAT(ITRK) = IHITPAT(ITRK) + 16
          ELSE
            IF(IAND(QF,1).EQ.1)IHITPAT(ITRK) = IHITPAT(ITRK) + 4
            IF(IAND(QF,2).EQ.2)IHITPAT(ITRK) = IHITPAT(ITRK) + 64
          END IF
        END IF
 700  CONTINUE
      RETURN
      END
#endif
