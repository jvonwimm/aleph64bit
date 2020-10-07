      INTEGER FUNCTION VDCHST(IROMD,ICHAN)
C----------------------------------------------------------------------
C! Mapping from electronic readout channel to physical readout strip
C! taking into account the bonding map in the VREG bank
C!                    G. Taylor 20/8/95
C!
C  INPUT  :
C      IROMD   :  Readout module address
C      ICHAN   :  Electronic readout channel
C  OUTPUT :
C      VDCHST  :  Physical readout strip
C                 =0 if not bonded to a readout strip
C======================================================================
#ifndef DOC
#include "bcs.h"
#include "vregjj.h"
      INTEGER VNELRM
#include "bmacro.h"
C----------------------------------------------------------------------
C
C  Link mapping bank
C
      VDCHST=0
      CALL VADEWA(IROMD,ILAY,IZ,IPH,IVIEW)
      NCHAN = VNELRM(IVIEW)
      KVREG = NLINK('VREG',IROMD)
      IF(KVREG.EQ.0)THEN
       CALL RERROR('VDCHST',1,'Cant find mapping information bank')
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
        CALL RERROR('VDCHST',1,'Mapping bank confusion')
        RETURN
       END IF
       IF(ICHAN.GE.IFCRG.AND.ICHAN.LE.ILCRG.AND.JSTRP.NE.0)
     &    VDCHST = JSTRP+ICHAN-IFCRG
      ENDDO
      RETURN
      END
#endif
