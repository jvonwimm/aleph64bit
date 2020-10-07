      SUBROUTINE MINPEC
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill PECO bank from DECO.
C
C     Author: Stephen Haywood      03-Apr-90
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "decojj.h"
#include "dcrljj.h"
#include "pecojj.h"
#include "minfac.h"
#include "bmacro.h"
C-----------------------------------------------------------------------
      IF (IW(NAMIND('PECO')).GT.0) RETURN
C
C++   Determine the bank number (not from DECO/100).
C
      KDECO = IW(NAMIND('DECO'))
      NR = IW(KDECO-2)
C
C++   Pick up links.
C
      KDECO = NLINK('DECO',100)
      IF (KDECO.LE.0) THEN
         CALL MINUPD('DECO')
         KDECO = NLINK('DECO',100)
         IF (KDECO.LE.0) RETURN
      ENDIF
C
C++   Create PECO bank.
C
      NPECO = LROWS(KDECO)
      IF(NPECO.LE.0) RETURN
      LEN = LMHLEN + LPECOA * NPECO
      CALL AUBOS('PECO',NR,LEN, KPECO,IGARB)
      CALL BLIST(IW,'S+','PECO')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDECO = NLINK('DECO',100)
      ENDIF
      IW(KPECO+LMHCOL) = LPECOA
      IW(KPECO+LMHROW) = NPECO
C
C++   Fill PECO bank.
C
      DO 100 I=1,NPECO
         RW(KROW(KPECO,I)+JPECEC) = FLOAT(ITABL(KDECO,I,JDECE0))/EFACTM
         RW(KROW(KPECO,I)+JPECTH) = FLOAT(ITABL(KDECO,I,JDECTH))/AFACTM
         RW(KROW(KPECO,I)+JPECPH) = FLOAT(ITABL(KDECO,I,JDECPH))/AFACTM
         RW(KROW(KPECO,I)+JPECE1) = FLOAT(ITABL(KDECO,I,JDECEF+0))/1000.
         RW(KROW(KPECO,I)+JPECE2) = FLOAT(ITABL(KDECO,I,JDECEF+1))/1000.
         IW(KROW(KPECO,I)+JPECCC) = ITABL(KDECO,I,JDECCC)
  100 CONTINUE
C
C++   Now fill relational bits from DCRL.
C
      KDCRL = NLINK('DCRL',0)
      IF(KDCRL.LE.0) RETURN
      NDCRL = LROWS(KDCRL)
      IF(NDCRL.LE.0) RETURN
C
      DO 200 I=1,NDCRL
         IDECO = ITABL(KDCRL,I,JDCRDE)
         IF(IDECO.LE.0) GOTO 200
         IRBIT = ITABL(KPECO,IDECO,JPECRB)
         IF(ITABL(KDCRL,I,JDCRDT).GT.0) CALL SBIT1(IRBIT,1)
         IF(ITABL(KDCRL,I,JDCRDH).GT.0) CALL SBIT1(IRBIT,2)
         IW(KROW(KPECO,IDECO)+JPECRB) = IRBIT
  200 CONTINUE
C
      RETURN
      END
#endif
