      SUBROUTINE MUDSO(NPLIS,IPLIS,KPPDS,NPPDS,KPPOB,NHPLT)
C***********************************************************
C! Looking for pattern in  barrel-endcap overlap           *
CKEY MUCAL MUON CALOBJ / INTERNAL
C                                                          *
C  Authors: U. Bottigli, A.Messineo  - 890310
C                                                          *
C  Input : NPLIS = Number of element of IPLIS              *
C          IPLIS = Vector of patterns index                *
C          KPPDS = Bank index of PPDS                      *
C          NPPDS = Rows of PPDS                            *
C          KPPOB = Bank index of PPOB                      *
C  Output:                                                 *
C          NHPLT = Vector of hits for each hcal plane      *
C                                                          *
C                                                          *
************************************************************
#ifndef DOC
      SAVE
#include "bcs.h"
#include "ppobjj.h"
#include "ppdsjj.h"
#include "mucapa.h"
      INTEGER IPLIS(*),NHPLT(*)
      INTEGER NHPLS(MUFLAG),NHPL(MUFLAG)
      PARAMETER(ALB=1.025)
#include "bmacro.h"
C
      DO 2000 LL=1,MUFLAG
       NHPLS(LL)=0
       NHPL(LL)=0
 2000 CONTINUE
      DO 10 I=1,NPLIS
       IND=IPLIS(I)
       DO 20 LL=1,NPPDS
        NPDSPT=ITABL(KPPDS,LL,JPPDPP)
        IF(NPDSPT.EQ.IND) THEN
         NLAYE=ITABL(KPPDS,LL,JPPDNL)
         IF(NLAYE.GT.LASPLN) GOTO 997
         FIREL=RTABL(KPPDS,LL,JPPDFL)
         NXXX=IFIX(FIREL/ALB+0.5)
         IZONE=ITABL(KPPOB,IND,JPPODI)
         IF(IZONE.EQ.1) THEN
C
C Here we fill for the barrel's layers
C
          NHPL(NLAYE)=NHPL(NLAYE)+NXXX
          IULT=LASPLN
          IF(NHPL(IULT).GE.1.AND.NHPL(IULT-1).GE.1) NHPL(MUFLAG)=1
         ELSE
C
C Here we fill for the overlap's layers
C
          NHPLS(NLAYE)=NHPLS(NLAYE)+NXXX
          IULT=LASPLN-1
          IF(NHPLS(IULT).GE.1.AND.NHPLS(IULT-1).GE.1) NHPLS(MUFLAG)=1
         ENDIF
        ENDIF
 20    CONTINUE
 10   CONTINUE
C
C Here we correct for the endcap's layers
C
      LAST=-1
      DO 30 I=1,LASPLN
       IF(NHPL(I).NE.0) LAST=I
 30   CONTINUE
      INDPL=0
      DO 40 LL=1,LASPLN-1
       IF(LL.LE.7) THEN
        NHPL(LL)=NHPL(LL)+NHPLS(LL)
       ELSE
        IF(LAST.NE.0) THEN
         INDPL=INDPL+1
        ELSE
         INDPL=LL
        ENDIF
        IPLSV=INDPL+LAST
        IF(IPLSV.GE.LASPLN-1) IPLSV=LASPLN-1
        NHPL(IPLSV)=NHPL(IPLSV)+NHPLS(LL)
       ENDIF
  40  CONTINUE
      IF(NHPL(MUFLAG).EQ.1.OR.NHPLS(MUFLAG).EQ.1)NHPLT(MUFLAG)=1
      DO 50 I=1,LASPLN
       NHPLT(I)=NHPLT(I)+NHPL(I)
 50   CONTINUE
 997  CONTINUE
      RETURN
      END
#endif
