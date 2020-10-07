      SUBROUTINE MINTMC
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill track MC bank DTMC for Mini-DST.
C
C     Author: Stephen Haywood      21-Nov-90
C
C     Input  : FKIN bank
C     Output : DTMC bank
C
C     Called by MINDST
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
C++   Pick up FKIN bank.
C
      KFKIN = NLINK('FKIN',0)
      IF (KFKIN.LE.0) RETURN
      NFKIN = LROWS(KFKIN)
      IF(NFKIN.LE.0) RETURN
C
C++   Create the DTMC bank.
C
      LEN = LMHLEN + LDTMCA * NFKIN
      CALL AUBOS('DTMC',0,LEN, KDTMC,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINTMC: Cannot create DTMC bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KFKIN = NLINK('FKIN',0)
      ENDIF
      IW(KDTMC+LMHCOL) = LDTMCA
      IW(KDTMC+LMHROW) = NFKIN
C
C++   Copy FKIN to DTMC, integerising momenta and masses.
C
      DO I=1,NFKIN
         IW(KROW(KDTMC,I)+JDTMPX) = NINT(EFACTM * RTABL(KFKIN,I,JFKIPX))
         IW(KROW(KDTMC,I)+JDTMPY) = NINT(EFACTM * RTABL(KFKIN,I,JFKIPY))
         IW(KROW(KDTMC,I)+JDTMPZ) = NINT(EFACTM * RTABL(KFKIN,I,JFKIPZ))
         IW(KROW(KDTMC,I)+JDTMMA) = NINT(EFACTM * RTABL(KFKIN,I,JFKIMA))
         CALL UCOPY(IW(KROW(KFKIN,I)+JFKIPA),IW(KROW(KDTMC,I)+JDTMPA),4)
      ENDDO
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DTMC')
C
      RETURN
      END
#endif
