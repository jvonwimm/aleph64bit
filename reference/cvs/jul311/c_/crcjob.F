      SUBROUTINE CRCJOB
C***********************************************************************
C!Close job for calorimeter process                                    *
C!                                                                     *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870615                                    *
C!                                                                     *
C!                                                                     *
C***********************************************************************
#ifndef DOC
#include "rparac.h"
#include "rstatr.h"
#include "crunp.h"
#include "bcs.h"
C
C?   get number of events processed
C
      IF(JCALPR.EQ.3)THEN
         NEVTP=NRUNRS(4)
         IF(NEVTP.GT.0)THEN
            C1=1./FLOAT(NEVTP)
            WRITE(IW(6),*)' NEVTP=',NEVTP
            NHIST=JULCA*1000+103
            CALL HOPERA(NHIST,'+   ',NHIST,NHIST,C1,0.)
         ENDIF
      ENDIF
      RETURN
      END
#endif
