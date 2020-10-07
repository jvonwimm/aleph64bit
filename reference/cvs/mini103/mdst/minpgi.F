      SUBROUTINE MINPGI
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill PGID bank from DGID.
C
C     Author: Stephen Haywood      02-Mar-93
C-----------------------------------------------------------------------
C
#ifndef DOC
C
#include "bcs.h"
#include "minfac.h"
#include "pgidjj.h"
#include "dgidjj.h"
#include "bmacro.h"
C
C++   Pick up DGID bank.
C
      KDGID = NLINK('DGID',0)
      IF (KDGID.LE.0) RETURN
      NDGID = LROWS(KDGID)
      IF (NDGID.LE.0) RETURN
C
C++   Create PGID bank.
C
      LEN = LMHLEN + LPGIDA * NDGID
      CALL AUBOS('PGID',0,LEN, KPGID,IGARB)
      CALL BLIST(IW,'S+','PGID')
      IF (IGARB.GE.2) THEN
         RETURN
      ELSE IF (IGARB.NE.0) THEN
         KDGID = NLINK('DGID',0)
      ENDIF
      IW(KPGID+LMHCOL) = LPGIDA
      IW(KPGID+LMHROW) = NDGID
C
C++   Fill PGID bank.
C
      DO 100 I=1,NDGID
         IW(KROW(KPGID,I)+JPGIIF) = ITABL(KDGID,I,JDGIIF)
         RW(KROW(KPGID,I)+JPGIDE) = FLOAT(ITABL(KDGID,I,JDGIDE))/100.
         RW(KROW(KPGID,I)+JPGICM) = FLOAT(ITABL(KDGID,I,JDGICM))/100.
         RW(KROW(KPGID,I)+JPGIM1) = FLOAT(ITABL(KDGID,I,JDGIM1))/100.
         RW(KROW(KPGID,I)+JPGIM2) = FLOAT(ITABL(KDGID,I,JDGIM2))/100.
         RW(KROW(KPGID,I)+JPGIM3) = FLOAT(ITABL(KDGID,I,JDGIM3))/EFACTM
         RW(KROW(KPGID,I)+JPGICE) = FLOAT(ITABL(KDGID,I,JDGICE))/EFACTM
         RW(KROW(KPGID,I)+JPGITH) = FLOAT(ITABL(KDGID,I,JDGITH))/AFACTM
         RW(KROW(KPGID,I)+JPGIPH) = FLOAT(ITABL(KDGID,I,JDGIPH))/AFACTM
         IW(KROW(KPGID,I)+JPGIPE) = ITABL(KDGID,I,JDGIPE)
  100 CONTINUE
C
      RETURN
      END
#endif
