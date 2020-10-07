      SUBROUTINE QVSTKJ(ITK,DJET,JET)
CKEY  QVSRCH / INTERNAL
C ------------------------------------------------------------------------------
C! Returns which jet a track is closer to
C  Author : T. MATTISON  U.A.BARCELONA/SLAC  1 DECEMBER 1992
C
C  Input Arguments :
C     ITK IS ALPHA TRACK NUMBER
C     DJET(3,2) IS TWO NORMALIZED JET DIRECTION VECTORS
C  Output Arguments :
C     JET=1 OR 2
C ------------------------------------------------------------------------------
#ifndef DOC
      DIMENSION DJET(3,2)
#include "qcde.h"
#include "qmacro.h"
C ------------------------------------------------------------------------------
C
C CHECK TRACK DIRECTION
      DOT1=QX(ITK)*DJET(1,1)+QY(ITK)*DJET(2,1)+QZ(ITK)*DJET(3,1)
      DOT2=QX(ITK)*DJET(1,2)+QY(ITK)*DJET(2,2)+QZ(ITK)*DJET(3,2)
      JET=1
      IF (DOT2 .GT. DOT1) JET=2
      RETURN
      END
#endif
