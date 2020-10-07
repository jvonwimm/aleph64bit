      SUBROUTINE XTCNCR
C ------------------------------------------------------------
CKEY X_SET XTCN X1RG
C - F.Ranjard - 940427
C! create XTCN from X1RG
C  if X1RG is there and XTCN is not there then
C     create and fill XTCN
C  endif
C ------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
      DATA NXTCN,NX1RG /2*0/
      DATA LXTCNA /16/
#include "bmacro.h"
C ----------------------------------------------------------------
C - 1st entry
      IF (NXTCN.EQ.0) THEN
         NXTCN = NAMIND('XTCN')
         NX1RG = NAMIND('X1RG')
         CALL BKFMT ('XTCN','I')
      ENDIF
C
C - next entry
C
      JXTCN = IW(NXTCN)
      JX1RG = IW(NX1RG)
C
      IF (JX1RG.GT.0 .AND. JXTCN.EQ.0) THEN
C
C ---  Create XTCN with LMHLEN+LXTCNA words
         CALL AUBOS ('XTCN',0,LMHLEN+LXTCNA,JXTCN,IGARB)
         IF (IGARB.EQ.2) GOTO 999
         IF (IGARB.EQ.1) JX1RG = IW(NX1RG)
         IW(JXTCN+LMHCOL) = LXTCNA
         IW(JXTCN+LMHROW) = 1
C
C ---  Fill XTCN with X1RG content
         KXTCN = JXTCN+LMHLEN
C       trigger time
         IW(KXTCN+1) = ITABL(JX1RG,1,2)
         IW(KXTCN+2) = ITABL(JX1RG,1,3)
C       number of GBXs
         IW(KXTCN+3) = ITABL(JX1RG,2,2)
C       level1 yes + bunch number
         IPACK       = ITABL(JX1RG,2,3)
         IW(KXTCN+4) = IBITS (IPACK,0,24)
         IW(KXTCN+5) = IBITS (IPACK,24,8)
C       HV status word
         IW(KXTCN+7) = ITABL(JX1RG,2,4)
C       mask of enabled triggers
         IW(KXTCN+8) = ITABL(JX1RG,1,4)
C
         CALL BLIST (IW,'E+','XTCN')
      ENDIF
C
 999  CONTINUE
      END
#endif
