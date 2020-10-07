      SUBROUTINE CCUTPE(DIST,PT,EHCLU,IASSO)
C***********************************************************************
C! Make cuts on ec-hc association                                      *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870831                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : NONE                                                   *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!   DIST : Angular distance
C!   PT      : transverse momentum                                     *
C!   EHCLU   : Energy of the hcal cluster                              *
C!   IASSO   : Output, 1 if association is good, 0 else                *
C!                                                                     *
C***********************************************************************
#ifndef DOC
#include "cthres.h"
      IASSO=0
C
C    Perform cut on dist
C
      IF(DIST.LT.DLIMCT)THEN
C
C  Now, cut on P transverse
C
        IF(PT.LE.PTLOCT.OR.(EHCLU.LT.EHLICT.AND.PT.LE.PTHICT))IASSO=1
      ENDIF
      RETURN
      END
#endif
