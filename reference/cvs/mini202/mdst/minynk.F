      SUBROUTINE MINYNK
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill kink bank YKNK for ALPHA.
C
C     Author: Agnieszka Jacholkowska    30-Apr-97
C
C     Input  : DKNK bank
C     Output : YKNK bank
C
C     Called by MINFIL
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "minfac.h"
#include "yknkjj.h"
#include "dknkjj.h"
#include "bmacro.h"
C
C++   Pick up DKNK bank.
C
      KDKNK = NLINK('DKNK',0)
      IF(KDKNK.EQ.0) KDKNK = NLINK('DKNK',0)
      IF(KDKNK.LE.0) RETURN
      NDKNK = LROWS(KDKNK)
      IF(NDKNK.LE.0) RETURN
C
C++   Create YKNK bank.
C
      NYKNK = NDKNK
      LEN = LMHLEN + LYKNKA * NYKNK
      CALL AUBOS('YKNK',0,LEN, KYKNK,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINYNK: Cannot create YKNK bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDKNK = NLINK('DKNK',0)
      ENDIF
      IW(KYKNK+LMHCOL) = LYKNKA
      IW(KYKNK+LMHROW) = NYKNK
C
C++   Loop over DKNK.
C
      DO 100 I=1,NDKNK
         IW(KROW(KYKNK,I)+JYKNIT) = ITABL(KDKNK,I,JDKNIT)
         IW(KROW(KYKNK,I)+JYKNOT) = ITABL(KDKNK,I,JDKNOT)
         RW(KROW(KYKNK,I)+JYKNVX) = FLOAT(ITABL(KDKNK,I,JDKNVX))/AFACTM
         RW(KROW(KYKNK,I)+JYKNVX+1) = FLOAT(ITABL(KDKNK,I,JDKNVX+1))
     &   /AFACTM
         RW(KROW(KYKNK,I)+JYKNVX+2) = FLOAT(ITABL(KDKNK,I,JDKNVX+2))
     &   /AFACTM
         RW(KROW(KYKNK,I)+JYKNPK) = FLOAT(ITABL(KDKNK,I,JDKNPK))/AFACTM
         RW(KROW(KYKNK,I)+JYKNPT) = FLOAT(ITABL(KDKNK,I,JDKNPT))/AFACTM
         RW(KROW(KYKNK,I)+JYKNPL) = FLOAT(ITABL(KDKNK,I,JDKNPL))/AFACTM
         RW(KROW(KYKNK,I)+JYKNC2) = FLOAT(ITABL(KDKNK,I,JDKNC2))/AFACTM
         IW(KROW(KYKNK,I)+JYKNBH) = ITABL(KDKNK,I,JDKNBH)
  100 CONTINUE
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('YKNK')
C
      RETURN
      END
#endif
