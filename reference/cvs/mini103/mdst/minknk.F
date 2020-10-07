      SUBROUTINE MINKNK
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill kink bank DKNK for Mini-DST.
C
C     Author: Agnieszka Jacholkowska    30-Apr-97
C
C     Input  : YKNK bank
C     Output : DKNK bank
C
C     Called by MINDST
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "minfac.h"
#include "yknkjj.h"
#include "dknkjj.h"
#include "bmacro.h"
C
C++   Pick up YKNK bank.
C
      KYKNK = NLINK('YKNK',0)
      IF(KYKNK.EQ.0) KYKNK = NLINK('YKNK',0)
      IF(KYKNK.LE.0) RETURN
      NYKNK = LROWS(KYKNK)
      IF(NYKNK.LE.0) RETURN
C
C++   Create DKNK bank.
C
      NDKNK = NYKNK
      LEN = LMHLEN + LDKNKA * NDKNK
      CALL AUBOS('DKNK',0,LEN, KDKNK,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINKNK: Cannot create DKNK bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KYKNK = NLINK('YKNK',0)
      ENDIF
      IW(KDKNK+LMHCOL) = LDKNKA
      IW(KDKNK+LMHROW) = NDKNK
C
C++   Loop over YKNK.
C
      DO 100 I=1,NYKNK
         IW(KROW(KDKNK,I)+JDKNIT) = ITABL(KYKNK,I,JYKNIT)
         IW(KROW(KDKNK,I)+JDKNOT) = ITABL(KYKNK,I,JYKNOT)
         IW(KROW(KDKNK,I)+JDKNVX) = NINT(AFACTM*RTABL(KYKNK,I,JYKNVX))
         IW(KROW(KDKNK,I)+JDKNVX+1) = NINT(AFACTM*RTABL(KYKNK,I
     &   ,JYKNVX+1))
         IW(KROW(KDKNK,I)+JDKNVX+2) = NINT(AFACTM*RTABL(KYKNK,I
     &   ,JYKNVX+2))
         IW(KROW(KDKNK,I)+JDKNPK) = NINT(AFACTM*RTABL(KYKNK,I,JYKNPK))
         IW(KROW(KDKNK,I)+JDKNPT) = NINT(AFACTM*RTABL(KYKNK,I,JYKNPT))
         IW(KROW(KDKNK,I)+JDKNPL) = NINT(AFACTM*RTABL(KYKNK,I,JYKNPL))
         IW(KROW(KDKNK,I)+JDKNC2) = NINT(AFACTM*RTABL(KYKNK,I,JYKNC2))
         IW(KROW(KDKNK,I)+JDKNBH) = ITABL(KYKNK,I,JYKNBH)
  100 CONTINUE
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DKNK')
C
      RETURN
      END
#endif
