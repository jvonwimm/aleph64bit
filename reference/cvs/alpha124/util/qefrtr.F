      SUBROUTINE QEFRTR(JEFOL,ITRACK)
CKEY FILL EFLOW /INTERNAL
C----------------------------------------------------------------------
C
C!    Search for track correponding to electron
C
C!   Author   :- M.N. Minard    02-Apr-1992
C!======================================================================
#ifndef DOC
#include "pcrljj.h"
#include "qcde.h"
#include "qmacro.h"
      KEFOL = IW(NAEFOL)
      KEIDT = IW(NAEIDT)
C
C-    Check track number is there
C
      IF(ITABL(KEFOL,JEFOL,JEFOTY).LT.4) THEN
        ITYP = ITABL(KEFOL,JEFOL,JEFOTY)
        ITRACK = ITABL(KEFOL,JEFOL,JEFOLT)
        IF ( ITRACK.EQ.0 ) THEN
          IPECO = ITABL(KEFOL,JEFOL,JEFOLE)
          DO 10 IEIDT=1,LROWS(KEIDT)
          IF(ITABL(KEIDT,IEIDT,JEIDPE).EQ.IPECO) THEN
            ITRACK = ITABL(KEIDT,IEIDT,JEIDFR)
          ENDIF
10        CONTINUE
C
C-   Search in PCRL in case track not found
C
          IF(ITRACK.EQ.0) THEN
            KPCRL = IW(NAPCRL)
            DO 20 IPCRL =1,LROWS(KPCRL)
            IF (ITABL(KPCRL,IPCRL,JPCRPE).EQ.IPECO) THEN
              IF(ITABL(KPCRL,IPCRL,JPCRPF).NE.0) THEN
                ITRACK = ITABL(KPCRL,IPCRL,JPCRPF)
              ENDIF
            ENDIF
20          CONTINUE
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END
#endif
