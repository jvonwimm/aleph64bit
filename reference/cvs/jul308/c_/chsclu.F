      SUBROUTINE CHSCLU(ITWA,ICLUH)
C***********************************************************************
C! Go from HCstorey to Hclu                                            *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870831                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : KINE,VERT                                              *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!   ITWA    : Hcal tower address (encoded)                            *
C!   ICLUH   : Hcal cluster number to which tower belongs              *
C!                                                                     *
C***********************************************************************
#ifndef DOC
#include "hsdajj.h"
#include "hcnamc.h"
#include "bcs.h"
#include "bmacro.h"
      KHSDA=IW(NAHSDA)
C
C?   get number of hc storeys
C
      NHSDA=LROWS(KHSDA)
C
C?   loop on hc storeys
C
      ICLUH=0
      DO 10 ISDA=1,NHSDA
      IPH=ITABL(KHSDA,ISDA,JHSDPI)
      JTH=ITABL(KHSDA,ISDA,JHSDTI)
      K=ITABL(KHSDA,ISDA,JHSDSN)
      ITWL=IPH*10000+JTH*100+K
      IF(ITWL.EQ.ITWA)THEN
         ICLUH=ITABL(KHSDA,ISDA,JHSDCN)
         GO TO 999
      ENDIF
   10 CONTINUE
  999 CONTINUE
      RETURN
      END
#endif
