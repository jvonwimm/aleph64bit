      SUBROUTINE HPRPOT
C---------------------------------------------------------------
C
C! prepare P.O.T. banks for Hcal
C!
C! G.Capon                         4-jan-1989
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "rcurnt.h"
#include "phhyjj.h"
#include "phobjj.h"
#include "phtojj.h"
#include "phstjj.h"
#include "hsdajj.h"
#include "hclujj.h"
#include "bmacro.h"
C
C             book and fill PHST, PHHY, PHTO, PHOB banks
C
      JHSDA=NLINK('HSDA',0)
      IF(JHSDA.LE.0) GO TO 999
      NROWS=LROWS(JHSDA)
      LPHST=2+NROWS*LPHSTA
      CALL AUBOS('PHST',0,LPHST,JPHST,IGARB)
      IF (IGARB.EQ.1) JHSDA = NLINK('HSDA',0)
      IF(IGARB.EQ.2)THEN
         CALL RERROR('HPRPOT',1,'no space for PHST bank')
         GO TO 999
      ENDIF
      IW(JPHST+1)=LPHSTA
      IW(JPHST+2)=NROWS
      KPHST=JPHST+2
      DO 20 NROW=1,NROWS
      INTET=ITABL(JHSDA,NROW,JHSDTI)
      ISTAK=ITABL(JHSDA,NROW,JHSDSN)
      IF(ISTAK.EQ.2) INTET=INTET+128
      IW(KPHST+JPHSTI)=INTET
      IW(KPHST+JPHSPI)=ITABL(JHSDA,NROW,JHSDPI)
      RW(KPHST+JPHSCE)=RTABL(JHSDA,NROW,JHSDDE)
      IW(KPHST+JPHSPH)=ITABL(JHSDA,NROW,JHSDCN)
      KPHST=KPHST+LPHSTA
 20   CONTINUE
C
      JHCLU=NLINK('HCLU',0)
      IF (JHCLU.LE.0) GO TO 999
      NROWS=LROWS(JHCLU)
C
      LPHOB=2+NROWS*LPHOBA
      CALL AUBOS('PHOB',0,LPHOB,JPHOB,IGARB)
      IF (IGARB.EQ.1) JHCLU = NLINK('HCLU',0)
      IF (IGARB.EQ.2) THEN
         CALL RERROR('HPRPOT',2,'no space for PHOB bank')
         GO TO 999
      ENDIF
      IW(JPHOB+1)=LPHOBA
      IW(JPHOB+2)=NROWS
      KPHOB=JPHOB+2
      DO 21 NROW=1,NROWS
      EHCLU=RTABL(JHCLU,NROW,JHCLEC)
      RW(KPHOB+JPHOER)=RTABL(JHCLU,NROW,JHCLER)
      EHCL1=RTABL(JHCLU,NROW,JHCLE1)
      RW(KPHOB+JPHOE1)=0.
      IF(EHCLU.GT.0.) RW(KPHOB+JPHOE1)=EHCL1/EHCLU
      RW(KPHOB+JPHOEC)=EHCLU
      IW(KPHOB+JPHOKD)=ITABL(JHCLU,NROW,JHCLKD)
C         the estimators R1 and R2 are set to zero for the moment !!
      RW(KPHOB+JPHOR1)=0.
      RW(KPHOB+JPHOR2)=0.
      KPHOB=KPHOB+LPHOBA
 21   CONTINUE
C
      LPHTO=2+NROWS*LPHTOA
      CALL AUBOS('PHTO',0,LPHTO,JPHTO,IGARB)
      IF (IGARB.EQ.1) JHCLU = NLINK('HCLU',0)
      IF (IGARB.EQ.2) THEN
         CALL RERROR('HPRPOT',3,'no space for PHTO bank')
         GO TO 999
      ENDIF
      IW(JPHTO+1)=LPHTOA
      IW(JPHTO+2)=NROWS
      KPHTO=JPHTO+2
      DO 22 NROW=1,NROWS
      RW(KPHTO+JPHTT1) = RTABL(JHCLU,NROW,JHCLT1)
      RW(KPHTO+JPHTP1) = RTABL(JHCLU,NROW,JHCLP1)
      RW(KPHTO+JPHTS1) = RTABL(JHCLU,NROW,JHCLS1)
      RW(KPHTO+JPHTT2) = RTABL(JHCLU,NROW,JHCLT2)
      RW(KPHTO+JPHTP2) = RTABL(JHCLU,NROW,JHCLP2)
      RW(KPHTO+JPHTS2) = RTABL(JHCLU,NROW,JHCLS2)
      IW(KPHTO+JPHTPH) = NROW
      KPHTO=KPHTO+LPHTOA
 22   CONTINUE
C
      LPHHY=2+NROWS*LPHHYA
      CALL AUBOS('PHHY',0,LPHHY,JPHHY,IGARB)
      IF (IGARB.EQ.2) THEN
         CALL RERROR('HPRPOT',4,'no space for PHHY bank')
         GO TO 999
      ENDIF
      IW(JPHHY+1)=LPHHYA
      IW(JPHHY+2)=NROWS
      KPHHY=JPHHY+2
C                best hyp and hyp type yet to be specified !!
      DO 23 NROW=1,NROWS
      IW(KPHHY+JPHHBH) = 0
      IW(KPHHY+JPHHKT) = 0
      IW(KPHHY+JPHHPH) = NROW
      KPHHY = KPHHY + LPHHYA
 23   CONTINUE
 999  RETURN
      END
#endif
