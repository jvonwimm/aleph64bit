      SUBROUTINE MINPYF
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill PYFR bank from DTRA.
C
C     Author: Stephen Haywood      03-Apr-90
C
C     DVER bank also used.
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "dtrajj.h"
#include "dverjj.h"
#include "pyfrjj.h"
#include "bmacro.h"
C
      KDTRA = NLINK('DTRA',100)
      IF (KDTRA.LE.0) THEN
         CALL MINUPD('DTRA')
         KDTRA = NLINK('DTRA',100)
         IF (KDTRA.LE.0) RETURN
      ENDIF
      NFRFT = LROWS(KDTRA)
      IF (NFRFT.LE.0) RETURN
C
      KDVER = NLINK('DVER',100)
      IF (KDVER.LE.0) THEN
         CALL MINUPD('DVER')
         KDVER = NLINK('DVER',100)
         IF (KDVER.LE.0) RETURN
      ENDIF
      KDVER = NLINK('DVER',0)
      IF (KDVER.LE.0) RETURN
C
C++   Determine how many main vertices will be created.
C
      NPYER = 0
      DO I=1,LROWS(KDVER)
         IF(ITABL(KDVER,I,JDVEMV).GT.0) NPYER = NPYER + 1
      ENDDO
      IF(NPYER.LE.0) RETURN
C
C++   Create PYFR bank.
C++   Must estimate the maximum number of rows.
C
      NPYFR = NFRFT * NPYER
      LEN = LMHLEN + LPYFRA * NPYFR
      CALL AUBOS('PYFR',0,LEN, KPYFR,IGARB)
      CALL BLIST(IW,'S+','PYFR')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDTRA = NLINK('DTRA',100)
      ENDIF
      IW(KPYFR+LMHCOL) = LPYFRA
      IW(KPYFR+LMHROW) = NPYFR
C
C++   Fill PYFR bank.
C
      NPYFR = 0
      DO 100 I=1,NFRFT
         MAPVB = ITABL(KDTRA,I,JDTRVB)
         DO 150 IDVER=1,NPYER
            MATCH = JBIT(MAPVB,IDVER)
            IF(MATCH.NE.1) GOTO 150
            NPYFR = NPYFR + 1
            IW(KROW(KPYFR,NPYFR)+JPYFTN) = I
            IW(KROW(KPYFR,NPYFR)+JPYFVN) = IDVER
  150    CONTINUE
  100 CONTINUE
C
C++   Compress bank to required size.
C
      LEN = LMHLEN + LPYFRA * NPYFR
      CALL AUBOS('PYFR',0,LEN, KPYFR,IGARB)
      IW(KPYFR+LMHROW) = NPYFR
C
      RETURN
      END
#endif
