      SUBROUTINE DROPDK
C-----------------------------------------------------------------------
C! Drop used modes at the end of execution
CKEY ALREAD GIME DROP USED MODES / INTERNAL
C
C - Author : M.Talby               May 90
C
C     Input :    none
C
C     Output:    none
C
C-----------------------------------------------------------------------
#ifndef DOC
#if defined(IBM)
#include "alrcom.h"
      CHARACTER*20 CDROP
      CHARACTER*7 OPT
      DATA OPT/' (QUIET'/
C --
C -- Loop over used modes in read and write access and drop them
C --
      DO 10 I = 1,5
        IF(DKMODW(I).NE.' ') THEN
           CDROP = 'EXEC DROP '//DKMODW(I)//OPT
           CALL VMCMS(CDROP,K)
        ENDIF
        IF(I.GT.4) GOTO 10
        IF(DKMODR(I).NE.' ') THEN
           CDROP = 'EXEC DROP '//DKMODR(I)//OPT
           CALL VMCMS(CDROP,K)
        ENDIF
   10 CONTINUE
C --
      RETURN
#endif
      END
#endif
