      SUBROUTINE LKOPDAF(COMAND)
C ---------------------------------------------------------------
C --------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "lkfil.h"
      CHARACTER*80 ULIST
      CHARACTER COMAND*80,FILENM*80
C ---------------------------------------------------------------
      CALL LKGTPAR(COMAND,1,'_File: ',FILENM,LF)
      CALL AOPDBS(FILENM(1:LF),IRC)
      IF (IRC.NE.0) GOTO 999
      WRITE(LOUT,*)'[File ',FILENM(1:LF),' opened read-only]'
      DIRFIL=.TRUE.
      CALL BDROP (IW,'+DIS')
      CALL DAFRST (LDBAS)
      RETURN
 999  WRITE(LOUT,*) '[Error opening direct access file ',FILENM(1:LF),
     +            ' RC= ',IRC,' ]'
      DIRFIL=.FALSE.
      RETURN
      END
#endif
