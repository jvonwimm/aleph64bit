      SUBROUTINE YPRPOT
C-----------------------------------------------------------------------
C!  creates the vertex banks for the POT
C  input banks: YVXL
C  ouput banks: PYFR  PYER
C             Author : LL. Garrido 30/9/88
C?
C!======================================================================
#ifndef DOC
      DIMENSION PP(6),PDUM(6)
#include "bcs.h"
#include "pneujj.h"
#include "pyerjj.h"
#include "pyfrjj.h"
#include "pynejj.h"
#include "yv0vjj.h"
#include "yvxljj.h"
#include "ytrljj.h"
#include "rlunit.h"
#include "bmacro.h"
C
C     Dropping banks to be created (for modular reasons)
C
      IND = NDROP('PYER',0)
      IND = NDROP('PYFR',0)
C
C     checking if some vertex has been found
C
      KYVXL = NLINK('YVXL',0)
        IF (KYVXL.NE.0) THEN
          NVX = LROWS(KYVXL)
        ELSE
          NVX = 0
        ENDIF
      NVER =  NVX
      IF(NVER.EQ.0) RETURN
        NTRA=0
        DO 1 J1 = 1 , NVX
        NTRA=NTRA+ITABL(KYVXL,J1,JYVXNT)
1       CONTINUE
        CALL AUBOS('PYER',0,LMHLEN+LPYERA*NVER,KPYER,I2)
        CALL AUBOS('PYFR',0,LMHLEN+LPYFRA*NTRA,KPYFR,I1)
        IF (I1.EQ.2.OR.I2.EQ.2) GOTO 999
C
C       POINTERS
C
      KYVXL = NLINK('YVXL',0)
      KYTRL = NLINK('YTRL',0)
      KPYER = NLINK('PYER',0)
      KPYFR = NLINK('PYFR',0)
      IW(KPYER+LMHCOL) = LPYERA
      IW(KPYER+LMHROW) = NVER
      IW(KPYFR+LMHCOL) = LPYFRA
      IW(KPYFR+LMHROW) = NTRA
C
C     FILLING BANKS
C
      NT=0
      DO 2 J1=1,NVX
          IYVXL  = KROW(KYVXL,J1)
          IPYER  = KROW(KPYER,J1)
          IW(IPYER+1)=0
          IF(IW(IYVXL+1).EQ.0) IW(IPYER+1)=1
          CALL UCOPY(RW(IYVXL+2),RW(IPYER+2),3)
          CALL UCOPY(RW(IYVXL+5),PP(1),6)
          CALL SMINV(PP,PDUM,3,0,NRANK)
          CALL UCOPY(PP(1),RW(IPYER+5),6)
          CALL UCOPY(RW(IYVXL+11),RW(IPYER+11),2)
          NOFF=IW(IYVXL+JYVXOF)
          NTJ1=IW(IYVXL+JYVXNT)
            DO 3 J2=1,NTJ1
            NT=NT+1
            IPYFR  = KROW(KPYFR,NT)
            IW(IPYFR+JPYFVN) = J1
            IW(IPYFR+JPYFTN)=IW(KYTRL+2+NOFF+JYTRTN+(J2-1)*LCOLS(KYTRL))
3        CONTINUE
2       CONTINUE
         RETURN
999      WRITE(LOUTRL,*) 'FROM YPRPOT: ERROR CREATING NEW BANKS'
         END
#endif
