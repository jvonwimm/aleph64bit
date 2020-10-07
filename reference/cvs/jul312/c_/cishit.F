      SUBROUTINE CISHIT(IFI,JT,KS,IHCLU)
C***********************************************************************
C! See if hstorey is hit                                               *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  871023                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : HSDA                                                   *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!   ifi      = phi     (input)                                        *
C!   jt       = theta   (input)                                        *
C!   kc       = stack   (input)                                        *
C!                                                                     *
C!   ihclu    = hcluster number, 0 if not hit  (output)                *
C!                                                                     *
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "hcnamc.h"
#include "hsdajj.h"
#include "bmacro.h"
C
C?   Loop on all hstoreys and see if one of them corresponds
C
      KHSDA=IW(NAHSDA)
      NHSDA=LROWS(KHSDA)
      IHCLU=0
      DO 10 IHS=1,NHSDA
      JTLOC=ITABL(KHSDA,IHS,JHSDTI)
      IFLOC=ITABL(KHSDA,IHS,JHSDPI)
      KSLOC=ITABL(KHSDA,IHS,JHSDSN)
      IF(IFLOC.EQ.IFI.AND.JTLOC.EQ.JT.AND.KSLOC.EQ.KS)THEN
         IHCLU=ITABL(KHSDA,IHS,JHSDCN)
         GO TO 999
      ENDIF
   10 CONTINUE
  999 CONTINUE
      RETURN
      END
#endif
