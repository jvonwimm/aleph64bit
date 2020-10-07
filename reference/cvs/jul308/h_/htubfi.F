      SUBROUTINE HTUBFI(KHTUB,NHTUB)
C-----------------------------------------------------------------
C
C! fill HTUB bank
C
C      G.Capon               date : 861001
C!     output : NHTUB = number of hits in Hcal tubes
C!     called by HPRDIG
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "htubec.h"
#include "htubjj.h"
#include "hclust.h"
#include "rcurnt.h"
#include "rlunit.h"
      REAL XHIT(3),CLUW(3)
      INTEGER MODBA(3)
      DATA NERR/0/
C
      ISUBC=(MODAHT-1)/12+1
C
C?             get hit coordinate
C
      IF(ISUBC.EQ.2)THEN
C?           barrel data
         CALL HFBAHI(ISUBC,MODAHT,ILAYHT,ILWIHT,NWCLHT,MODBA,XHIT, CLUW,
     +   NHIT,NNWIRE,NHITWI)
      ELSE
C?           endcap data
         CALL HFENHI(ISUBC,MODAHT,ILAYHT,ILWIHT,NWCLHT,ISEXT,XHIT, CLUW,
     +   NHIT,NNWIRE,NHITWI)
      ENDIF
C
      IF(NHIT.EQ.0) GO TO 99
      DO 10 IHIT=1,NHIT
         IW(KHTUB+JHTUSN)=ISUBC
         IF(ISUBC.EQ.2) IW(KHTUB+JHTUMN)=MODBA(IHIT)
         IF(ISUBC.EQ.1.OR.ISUBC.EQ.3)IW(KHTUB+JHTUMN)=ISEXT
         IW(KHTUB+JHTULN)=ILAYHT
         RW(KHTUB+JHTULC)=XHIT(IHIT)
         RW(KHTUB+JHTUCW)=CLUW(IHIT)
         IW(KHTUB+JHTUPR)=ICURHW
         KHTUB=KHTUB+LHTUBA
         NHTUB=NHTUB+1
   10 CONTINUE
C
C
      RETURN
C
C?           error condition
 99   NERR=NERR+1
      IF (NERR.LE.10) THEN
      WRITE(LDEBRL,*)' routine HTUBFI:Hcal wire number out of range -',
     + ' RU,EV =',IRUNRC,IEVTRC
      WRITE(LDEBRL,*)' MODAHT,ILAYHT,ILWIHT,NWCLHT',MODAHT,ILAYHT,
     +ILWIHT,NWCLHT
      ENDIF
      CALL RERROR('HTUBFI',1,'hcal wire out of range')
      RETURN
      END
#endif
