      SUBROUTINE MINJET
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill jet bank DJET for Mini-DST.
C
C     Author: Stephen Haywood      05-Feb-91
C
C     Input  : EJET banks
C     Output : DJET bank
C
C     Called by MINDST
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "minfac.h"
#include "ejetjj.h"
#include "djetjj.h"
C
      LOGICAL GOTIT
C
#include "bmacro.h"
C-----------------------------------------------------------------------
C
C++   Loop over different energy flow algorithms.
C++   Now just jets created by ENFLW.
C
      GOTIT = .FALSE.
      DO 1000 IBNK=3,3
C
C++   Pick up EJET bank.
C
      KEJET = NLINK('EJET',IBNK)
      IF(KEJET.LE.0) GOTO 1000
      NEJET = LROWS(KEJET)
      IF(NEJET.LE.0) GOTO 1000
C
C++   Create the DJET bank.
C
      NDJET = NEJET
      LEN = LMHLEN + LDJETA * NDJET
      CALL AUBOS('DJET',IBNK,LEN, KDJET,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINJET: Cannot create DJET bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KEJET = NLINK('EJET',IBNK)
      ENDIF
      IW(KDJET+LMHCOL) = LDJETA
      IW(KDJET+LMHROW) = NDJET
C
C++   Loop over EJET storing information in DJET.
C
      DO 100 I=1,NEJET
         IW(KROW(KDJET,I)+JDJEPX) = NINT(EFACTM * RTABL(KEJET,I,JEJEPX))
         IW(KROW(KDJET,I)+JDJEPY) = NINT(EFACTM * RTABL(KEJET,I,JEJEPY))
         IW(KROW(KDJET,I)+JDJEPZ) = NINT(EFACTM * RTABL(KEJET,I,JEJEPZ))
         IW(KROW(KDJET,I)+JDJEE0) = NINT(EFACTM * RTABL(KEJET,I,JEJEPE))
  100 CONTINUE
      GOTIT = .TRUE.
C
 1000 CONTINUE
C
C++   Add the banks to the Mini list.
C
      IF (GOTIT) CALL MINLIS('DJET')
C
      RETURN
      END
#endif
