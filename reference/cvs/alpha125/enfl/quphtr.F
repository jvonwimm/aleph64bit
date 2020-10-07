      SUBROUTINE QUPHTR(ICL,NPL)
C----------------------------------------------------------------
C! Look for fired planes in the corresponding trigger segment
C  Input  ICL  PHCO index
C  Output NPL  # of fired planes
C----------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
#include "qhac.h"
#include "cuhnto.h"
      DIMENSION IHW(4),ITRI(4)
      LOGICAL FBIT
#include "qmacro.h"
C
      DATA THE1,THE2,THE3,THE4,THE5 / 6.,26.,39.,60.,90. /
C----------------------------------------------------------------
      NPL= 0
C
      IPHCO= IW(NAPHCO)
      IF(IPHCO.EQ.0) THEN
        NPL= -1
        RETURN
      END IF
      IXTEB= IW(NAMIND('XTEB'))
      IF(IXTEB.EQ.0) RETURN
C
C----- Get theta and phi
      THE= RTABL(IPHCO,ICL,JPHCTH)
      PHI= RTABL(IPHCO,ICL,JPHCPH)
      THE= THE*180./QQPI
      PHI= PHI*180./QQPI
C
      ISEC= 1
      IF(THE.GT.THE2) ISEC= ISEC+1
      IF(THE.GT.THE3) ISEC= ISEC+1
      IF(THE.GT.THE4) ISEC= ISEC+1
      IF(THE.GT.THE5) ISEC= ISEC+1
      IF(THE.GT.(180.-THE4)) ISEC= ISEC+1
      IF(THE.GT.(180.-THE3)) ISEC= ISEC+1
      IF(THE.GT.(180.-THE2)) ISEC= ISEC+1
C
      IF(ISEC.LE.2.OR.ISEC.GE.7) THEN
        IPHI= INT(PHI/60.) + 1
        IF(IPHI.GT.6) IPHI= 6
      ELSE
        IPHI= INT(PHI/30.) + 1
        IF(IPHI.GT.12) IPHI= 12
      END IF
C
      ISEG= 0
      DO 10 L1=1,ISEC-1
        IF(L1.LE.2.OR.L1.GE.7) ISEG= ISEG+6
        IF(L1.GE.3.AND.L1.LE.6) ISEG= ISEG+12
10    CONTINUE
      ISEG= ISEG+IPHI
C
      IWD= 1
      IBIT= ISEG-1
      IF(ISEG.GT.32) THEN
        IWD=2
        IBIT= ISEG-33
      END IF
      IF(ISEG.GT.64) THEN
        IWD=3
        IBIT= ISEG-65
      END IF
C
      K1= 0
      DO 20 L1=IWD,12,3
        K1= K1+1
        IHW(K1)= ITABL(IXTEB,1,L1+15)
20    CONTINUE
C
      DO 30 L0=1,4
        IAD= IHW(L0)
        FBIT= BTEST(IAD,IBIT)
        ITRI(L0)= 0
        IF(FBIT) ITRI(L0)= 1
30    CONTINUE
C
      DO 40 L1=1,4
        IF(ITRI(L1).EQ.1) THEN
          IF(NPLTHR(L1).GT.NPL) NPL= NPLTHR(L1)
        END IF
40    CONTINUE
C
      RETURN
      END
#endif
