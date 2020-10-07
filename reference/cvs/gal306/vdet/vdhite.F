*DK vdhite
      SUBROUTINE VDHITE
C-----------------------------------------------------------------------
C! updates end segment flag in VDSS when track leaves the active volume
CKEY VDET DIGITIZE
C!
C!
C!  Author         G. Taylor       1/8/95
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "vdssjj.h"
      DATA NAVDSS /0/
#include "bmacro.h"
      IF (NAVDSS.EQ.0) NAVDSS = NAMIND('VDSS')
      KVDSS=IW(NAVDSS)
      IF(KVDSS.GT.0) THEN
       ILAST=LROWS(KVDSS)
       IF(ILAST.GT.0) IW(KROW(KVDSS,ILAST)+JVDSES)=1
      ENDIF
      RETURN
      END
#endif
