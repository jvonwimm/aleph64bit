      SUBROUTINE CTHCLU(ICLUH,ITRHC,IER)
C
C***********************************************************************
C!Find true Hcal Cluster type                                          *
C!                                                                     *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870615                                    *
C!                                                                     *
C!  ICLUH  = Hcal cluster number                                       *
C!  ITRHC = type :                                                     *
C!             1 : neutral                                             *
C!             2 : charged, mip                                        *
C!             3 : charged, nomip                                      *
C!   IER = return code 0 : normal                                      *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : HCLU,HTDT                                              *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!                                                                     *
C***********************************************************************
#ifndef DOC
#include "rlunit.h"
C
#include "bcs.h"
#include "hcnamc.h"
#include "hclujj.h"
#include "hsdajj.h"
#include "bmacro.h"
      KHSDA=IW(NAHSDA)
      KHCLU=IW(NAHCLU)
      KHTD1=NLINK('HTDT',1)
      KHTD2=NLINK('HTDT',2)
      KHTD3=NLINK('HTDT',3)
      KHTHT=NLINK('HTHT',0)
C
C?   say that cluster is neutral
C
      ITRHC=1
C
C?  Loop on storeys in the cluster
C
      IHSTO=ITABL(KHCLU,ICLUH,JHCLFS)
   40 CONTINUE
C
C?  get storey address
C
      I=ITABL(KHSDA,IHSTO,JHSDPI)
      J=ITABL(KHSDA,IHSTO,JHSDTI)
      K=ITABL(KHSDA,IHSTO,JHSDSN)
      IADDR=I*10000+J*100+K
C
C?  get storey number in bank htht
C
      NHTHT=LROWS(KHTHT)
      DO 10 IHTHT=1,NHTHT
      IADL=ITABL(KHTHT,IHTHT,1)
         IF(IADL.EQ.IADDR)GO TO 20
   10 CONTINUE
      WRITE(IW(6),*)' CTHCLU --> ERROR'
      IFLAG=4
   20 CONTINUE
C
C?  loop on tracks which contribute to the storey
C
      NTRA=ITABL(KHTD1,IHTHT,1)
      MSTRT=ITABL(KHTD2,IHTHT,1)+LMHLEN
      DO 30 ITRA=1,NTRA
      ITRK=IW(KHTD3+MSTRT+ITRA)
C
C?  Get index number and charge of the primary
C
      CALL CTKCHG(ITRK,ITRKP,ICHG,IRC)
      IF(IRC.NE.0)GO TO 998
      IF(ICHG.NE.0)THEN
C
C?  see if track is mip
C
         CALL CCTRMI(ITRKP,IFMIP)
         IF(IFMIP.EQ.0)THEN
C
C?   If track is nomip, the cluster is nomip, no need
C?    to look any further
C
            ITRHC=3
            GO TO 999
         ELSE
            ITRHC=2
         ENDIF
      ENDIF
   30 CONTINUE
      IHSTO=ITABL(KHSDA,IHSTO,JHSDNS)
      IF(IHSTO.NE.0)GO TO 40
  999 CONTINUE
      IER=0
      RETURN
  998 CONTINUE
      CALL RERROR(' CTHCLU',1,' ABORT')
      IER=1
      RETURN
      END
#endif
