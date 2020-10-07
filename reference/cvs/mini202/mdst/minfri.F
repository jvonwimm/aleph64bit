      SUBROUTINE MINFRI
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill FRID bank from DTRA.
C
C     Author: Stephen Haywood      14-Jan-91
C
C     Track quality is filled from DTRA if it is contained there.
C     If not, UFITQL is called, and this requires FRFT, FRTL and YV0V.
C     These banks can be obtained by calling MINFRF, MINFRT and MINYV0.
C     These banks will not be present if NOCH or NOV0 are used.
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "dtrajj.h"
#include "fridjj.h"
C
      INTEGER UFITQL
C
#include "bmacro.h"
C
      KDTRA = NLINK('DTRA',100)
      IF (KDTRA.LE.0) THEN
         CALL MINUPD('DTRA')
         KDTRA = NLINK('DTRA',100)
         IF (KDTRA.LE.0) RETURN
      ENDIF
C
C++   Get version number: used to decide whether to call UFITQL.
C
      MVER = MINGTV(DUM)
C
C++   Create FRID bank.
C
      NFRID = LROWS(KDTRA)
      IF(NFRID.LE.0) RETURN
      LEN = LMHLEN + LFRIDA * NFRID
      CALL AUBOS('FRID',0,LEN, KFRID,IGARB)
      CALL BLIST(IW,'S+','FRID')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDTRA = NLINK('DTRA',100)
      ENDIF
      IW(KFRID+LMHCOL) = LFRIDA
      IW(KFRID+LMHROW) = NFRID
C
C++   Fill FRID bank.
C
      DO 100 I=1,NFRID
         IW(KROW(KFRID,I)+JFRIBP) = ITABL(KDTRA,I,JDTRHO)
         IW(KROW(KFRID,I)+JFRIDZ) = ITABL(KDTRA,I,JDTRHM)
         IF (MVER.GE.53) THEN
            IW(KROW(KFRID,I)+JFRIQF) = ITABL(KDTRA,I,JDTRQF)
         ELSE
            IW(KROW(KFRID,I)+JFRIQF) = UFITQL(I)
         ENDIF
  100 CONTINUE
C
      RETURN
      END
#endif
