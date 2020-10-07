      SUBROUTINE MINEJE
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill EJET bank from DJET.
C
C     Author: Stephen Haywood      05-Feb-91
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "djetjj.h"
#include "ejetjj.h"
#include "minfac.h"
#include "bmacro.h"
C-----------------------------------------------------------------------
C++   Loop over different energy flow algorithms.
C
      DO 1000 IBNK=0,3
C
      KDJET = NLINK('DJET',IBNK)
      IF(KDJET.LE.0) GOTO 1000
C
C++   Create EJET bank.
C
      NEJET = LROWS(KDJET)
      IF(NEJET.LE.0) GOTO 1000
      LEN = LMHLEN + LEJETA * NEJET
      CALL AUBOS('EJET',IBNK,LEN, KEJET,IGARB)
      CALL BLIST(IW,'S+','EJET')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDJET = NLINK('DJET',IBNK)
      ENDIF
      IW(KEJET+LMHCOL) = LEJETA
      IW(KEJET+LMHROW) = NEJET
C
C++   Fill EJET bank.
C
      DO 100 I=1,NEJET
         RW(KROW(KEJET,I)+JEJEPX) = FLOAT(ITABL(KDJET,I,JDJEPX))/EFACTM
         RW(KROW(KEJET,I)+JEJEPY) = FLOAT(ITABL(KDJET,I,JDJEPY))/EFACTM
         RW(KROW(KEJET,I)+JEJEPZ) = FLOAT(ITABL(KDJET,I,JDJEPZ))/EFACTM
         RW(KROW(KEJET,I)+JEJEPE) = FLOAT(ITABL(KDJET,I,JDJEE0))/EFACTM
  100 CONTINUE
C
 1000 CONTINUE
C
      RETURN
      END
#endif
