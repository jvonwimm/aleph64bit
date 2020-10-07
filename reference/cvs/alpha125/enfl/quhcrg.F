      SUBROUTINE QUHCRG(ICL,IREG,IMOD)
C----------------------------------------------------------------
C!  Get region and module number for a specified PHCO cluster
C
C Gerardo Ganis -- 1989
C----------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
#include "qhac.h"
      DIMENSION IMOD(2)
      PARAMETER( TH1= 36., TH2= 55. )
#include "qmacro.h"
C----------------------------------------------------------------
      IREG= 0
      IMOD(1)= 0
      IMOD(2)= 0
C
      IPHCO= IW(NAPHCO)
      IF(IPHCO.EQ.0) THEN
        IREG= -1
        RETURN
      END IF
C
      IKD= ITABL(IPHCO,ICL,JPHCKD)
      THE= RTABL(IPHCO,ICL,JPHCTH)
      THE= THE * 180. / QQPI
      IREG= 1
      IF(THE.GT.TH1) IREG= 2
      IF(THE.GT.TH2) IREG= 3
      IF(THE.GT.(180.-TH2)) IREG= 4
      IF(THE.GT.(180.-TH1)) IREG= 5
C
      PHI= RTABL(IPHCO,ICL,JPHCPH)
      PHI= PHI * 180. / QQPI
      IMOD(2)= INT(PHI/15.)+1
      IF(IMOD(2).GT.24) IMOD(2)= 24
      IMOD(1)= INT(PHI/60.)+1
      IF(IMOD(1).GT.6) IMOD(1)= 6
C
      RETURN
      END
#endif
