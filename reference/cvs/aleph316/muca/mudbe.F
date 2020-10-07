      SUBROUTINE MUDBE(NPLIS,IPLIS,KPPDS,NPPDS,KPPOB,NHPLT)
C***********************************************************
C! Looking for pattern in barrel or endcap                 *
CKEY MUCAL MUON CALOBJ / INTERNAL
C  Authors: U. Bottigli, A.Messineo  -  890310             *
C                                                          *
C                                                          *
C  Input : NPLIS = Number of element of IPLIS              *
C          IPLIS = Vector of patterns index                *
C          KPPDS = Bank index of PPDS                      *
C          NPPDS = Rows of PPDS                            *
C          KPPOB = Bank index of PPOB                      *
C  Output:                                                 *
C          NHPLT = Vector of hits for each hcal plane      *
C                                                          *
************************************************************
#ifndef DOC
      SAVE
#include "bcs.h"
#include "ppobjj.h"
#include "ppdsjj.h"
#include "mucapa.h"
      INTEGER IPLIS(*),NHPLT(*)
      INTEGER NHPLE(MUFLAG)
      PARAMETER(ALB=1.025)
#include "bmacro.h"
C
      DO 2000 LL=1,MUFLAG
       NHPLE(LL)=0
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
         NHPLE(NLAYE)=NHPLE(NLAYE)+NXXX
        ENDIF
 20    CONTINUE
       IULT=LASPLN-1
       IZONE=ITABL(KPPOB,IND,JPPODI)
       IF(IZONE.EQ.1)IULT=LASPLN
       IF(NHPLE(IULT).GE.1.AND.NHPLE(IULT-1).GE.1) NHPLT(MUFLAG)=1
 10   CONTINUE
      DO 50 I=1,LASPLN
       NHPLT(I)=NHPLT(I)+NHPLE(I)
 50   CONTINUE
 997  CONTINUE
      RETURN
      END
#endif
