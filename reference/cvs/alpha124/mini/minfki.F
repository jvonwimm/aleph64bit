      SUBROUTINE MINFKI
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill FKIN from DTMC.
C
C     Author: Stephen Haywood      21-Nov-90
C
C     Input  : DTMC bank
C     Output : FKIN bank
C
C     Called by MINFIL
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "fkinjj.h"
#include "dtmcjj.h"
#include "minfac.h"
#include "bmacro.h"
C-----------------------------------------------------------------------
C
C++   Pick up DTMC bank.
C
      KDTMC = NLINK('DTMC',0)
      IF(KDTMC.LE.0) RETURN
      NDTMC = LROWS(KDTMC)
      IF(NDTMC.LE.0) RETURN
C
C++   Create the FKIN bank.
C
      LEN = LMHLEN + LFKINA * NDTMC
      CALL AUBOS('FKIN',0,LEN, KFKIN,IGARB)
      CALL BLIST(IW,'S+','FKIN')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDTMC = NLINK('DTMC',0)
      ENDIF
      IW(KFKIN+LMHCOL) = LFKINA
      IW(KFKIN+LMHROW) = NDTMC
C
C++   Fill FKIN bank.
C++   Note: there is a check to ensure that the pt is not 0;
C++   if it is, it is modified to protect later calculations.
C++   In reality, such tracks will never be used.
C
      I=0
      DO 10 KI=1,NDTMC
         IPX = ITABL(KDTMC,KI,JDTMPX)
         IPY = ITABL(KDTMC,KI,JDTMPY)
         IF(IPX.EQ.0 .AND. IPY.EQ.0) IPX = 1
         IPA = ITABL(KDTMC,KI,JDTMPA)
         IFV = ITABL(KDTMC,KI,JDTMOV)
         ILV = ITABL(KDTMC,KI,JDTMEV)
         IF (IPA.LE.0.OR.(IFV.EQ.0.AND.ILV.EQ.0)) GO TO 10
         I=I+1
         RW(KROW(KFKIN,I)+JFKIPX) = FLOAT(IPX)/EFACTM
         RW(KROW(KFKIN,I)+JFKIPY) = FLOAT(IPY)/EFACTM
         RW(KROW(KFKIN,I)+JFKIPZ) = FLOAT(ITABL(KDTMC,KI,JDTMPZ))/EFACTM
         RW(KROW(KFKIN,I)+JFKIMA) = FLOAT(ITABL(KDTMC,KI,JDTMMA))/EFACTM
         CALL UCOPY(IW(KROW(KDTMC,KI)+JDTMPA),IW(KROW(KFKIN,I)+
     +              JFKIPA),4)
 10   CONTINUE
      IW(KFKIN+LMHROW) = I
C
      RETURN
      END
#endif
