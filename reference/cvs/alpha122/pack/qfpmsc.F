      SUBROUTINE QFPMSC
CKEY FILL CHARGED / INTERNAL
C----------------------------------------------------------------------
C! Fill word for mult.scatt in FRFT from MINI bank 'PMSC'
C                                                    J.Boucrot 28.10.94
C  Called from QFCHGD
C----------------------------------------------------------------------
#ifndef DOC
      SAVE IERR
#include "qcde.h"
#include "pmscjj.h"
      DATA IERR / 0 /
#include "bmacro.h"
C-----------------------------------------------------------------------
      JPMSC=IW(NAPMSC)
      JFRFT=IW(NAFRFT)
      NPMSC=LROWS(JPMSC)
      NFRFT=LROWS(JFRFT)
      IF (NPMSC.NE.NFRFT) THEN
         IERR=IERR+1
         IF (IERR.LE.10) CALL QWMESE(
     +     '_QFPMSC_ Number of columns in PMSC and FRFT are different!')
         GO TO 999
      ENDIF
      DO 100 IK=1,NFRFT
         KFRFT=KROW(JFRFT,IK)
         KPMSC=KROW(JPMSC,IK)
         RW(KFRFT+JFRFAL)=RW(KPMSC+JPMSSA)
 100  CONTINUE
C
 999  RETURN
      END
#endif
