      SUBROUTINE EFPARD
C----------------------------------------------------------------------
C!  READ PARAMETERS FOR T0 CALCULATON FROM BOS CARDS OR DATA BASE
C
C   AUTHOR : D. PALLIN             10/10/89
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "efipjj.h"
#include "efipr2.h"
#include "rcurnt.h"
#include "rlunit.h"
        EXTERNAL NAMIND
        INTEGER ALGTDB
C
C  LOOK FOR PARAMETERS IN  DATA BASE (NO RUN-RANGE DEPENDENCE)
C       OLD DATA BASE
C
        IND = MDARD(IW,LRCONS,'EFIP',0)
        IF(IND.EQ.0)THEN
C
C  READ THE EFIP BANK FROM THE DATA BASE ( RUN DEPENDENT )
C
          IND=ALGTDB(LRCONS,'EFIP',IRUNRC)
          IF ( IND.NE.0) IND = IW(NAMIND('EFIP'))
        ENDIF
        IF(IND.NE.0)THEN
          INEFI=IND+2
          DO 1 I=1,36
          TM(I)=RW(INEFI+JEFITM+I-1)
 1        CONTINUE
          COE1=RW(INEFI+JEFIC1)
          COE2=RW(INEFI+JEFIC2)
        ELSE
           CALL RERROR('EFPARD', 1 ,'ECAL T0 offset not found(EFIP)')
        ENDIF
        RETURN
        END
#endif
