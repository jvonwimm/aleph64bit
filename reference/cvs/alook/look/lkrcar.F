      SUBROUTINE LKRCAR(COMAND)
C ------------------------------------------------------------
C ------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "lkfil.h"
      CHARACTER*80 COMAND,FILENM
      LOGICAL LEX
C-------------------------------------------------------------
      CALL LKGTPAR(COMAND,1,'_File: ',FILENM,LF)
      CALL AOPEN (LCARD,FILENM,'CARD','DISK',IER)
      IF (IER.EQ.0) CALL BREADC
      RETURN
      END
#endif
