      SUBROUTINE QVSGTL0(VTX,MGTK,NGTK,JGTK)
CKEY  QVSRCH / INTERNAL
C ------------------------------------------------------------------------------
C! Returns list of good ALPHA tracks for coarse z vertex
C ------------------------------------------------------------------------------
#ifndef DOC
      DIMENSION VTX(3),JGTK(MGTK)
      LOGICAL QVSGTK0
C
#include "qcde.h"
#include "qmacro.h"
C ------------------------------------------------------------------------------
C
      NGTK=0
C
C LOOP OVER TRACKS
      DO 150 ITK=KFCHT,KLCHT
C APPLY COARSE Z VERTEX TRACK CUTS
        IF (QVSGTK0(ITK,VTX)) THEN
C SAVE TRACK NUMBER
          NGTK=MIN(NGTK+1,MGTK)
          JGTK(NGTK)=ITK
        ENDIF
  150 CONTINUE
      RETURN
      END
#endif
