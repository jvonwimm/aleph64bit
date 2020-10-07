      INTEGER FUNCTION VDSTCH(IROMD,ISTRIP)
C----------------------------------------------------------------------
C! Mapping from physical readout strip to electronic readout channel
C! taking into account the bonding map in the VREG bank
C!                    G. Taylor 20/8/95
C!
C  INPUT  :
C      IROMD   :  Readout module address
C      ISTRIP  :  Physical readout strip
C  OUTPUT :
C      VDSTCH  :  Electronic readout channel
C                 =0 if the is an error
#ifndef DOC
C ---------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "vregjj.h"
      INTEGER VNELRM
#include "bmacro.h"
C----------------------------------------------------------------------
C
C  Link mapping bank
C
      VDSTCH=0
      CALL VADEWA(IROMD,ILAY,IZ,IPH,IVIEW)
      NCHAN = VNELRM(IVIEW)
      KVREG = NLINK('VREG',IROMD)
      IF(KVREG .EQ. 0)THEN
        CALL RERROR('VDSTCH',1,'Cant find mapping information bank')
        RETURN
      END IF
C
C And apply channel to strip mapping
C
      NVREG=LROWS(KVREG)
      DO IVREG=1,NVREG
       IFCRG = ITABL(KVREG,IVREG,JVREFC)
       ILCRG = ITABL(KVREG,IVREG,JVRELC)
       JSTRP = ITABL(KVREG,IVREG,JVRECS)
       JFLAG = ITABL(KVREG,IVREG,JVRERF)
       IF(IFCRG.LT.1.OR.ILCRG.GT.NCHAN)THEN
        CALL RERROR('VDSTCH',1,'Mapping bank confusion')
        RETURN
       END IF
       NSTRP=ILCRG-IFCRG
       IF(ISTRIP.GE.JSTRP.AND.ISTRIP.LE.JSTRP+NSTRP.AND.JSTRP.NE.0)
     &      VDSTCH = IFCRG+ISTRIP-JSTRP
      ENDDO
      RETURN
      END
#endif
#endif
