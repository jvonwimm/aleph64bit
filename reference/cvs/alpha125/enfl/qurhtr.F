      SUBROUTINE QURHTR
C-----------------------------------------------------------------------
C! Decode HCAL wires trigger thresholds
C-----------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
#include "qhac.h"
#include "cuhnto.h"
      CHARACTER*4 CHAINT,ANAM
#include "qmacro.h"
C-----------------------------------------------------------------------
C----- Default values
      NPLTHR(1)= 4
      NPLTHR(2)= 6
      NPLTHR(3)= 8
      NPLTHR(4)= 12
C
      IXTHH= IW(NAMIND('XTHH'))
      IF(IXTHH.NE.0) THEN
        NXTHH= LROWS(IXTHH)
        IF(NXTHH.LE.100) THEN
          ITHR= 0
          DO 5 L1=1,NXTHH
            ANAM= CHAINT(ITABL(IXTHH,L1,4))
            IF(ANAM.NE.'HCW_') GOTO 5
            ITHR= ITHR+1
            IF(ITHR.GT.4) GOTO 5
            IADC= ITABL(IXTHH,L1,8)
            NPLTHR(ITHR)= 2*(INT(IADC/20.)+1)
5         CONTINUE
        ELSE
          WRITE (IW(6),99)
        END IF
      END IF
99    FORMAT(/,1X,'_QURHTR_: problems reading XTHH',/
     &        ,1X,' Default values used',/)
C
      RETURN
      END
#endif
