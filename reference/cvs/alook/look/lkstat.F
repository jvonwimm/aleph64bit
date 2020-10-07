      SUBROUTINE LKSTAT
C --------------------------------------------------------------
C! status of I/O files
C --------------------------------------------------------------
#ifndef DOC
#include "lkfil.h"
C -------------------------------------------------------------
      IF (SEQFIL) THEN
          LF=LNBLNK(SEQNAM)
          WRITE(LOUT,*) '[Sequential file "',SEQNAM(1:LF),'" opened]'
      ELSE
          WRITE(LOUT,*) '[No sequential file opened]'
      ENDIF
      IF (DIRFIL) THEN
          LF=LNBLNK(DIRNAM)
          WRITE(LOUT,*) '[Direct access file "',DIRNAM(1:LF),'" opened]'
      ELSE
          WRITE(LOUT,*) '[No direct access file opened]'
      ENDIF
      DO I=1,3
        IF (FLGFIL(I)) THEN
          LF=LNBLNK(FILNAM(I))
          WRITE(LOUT,*) '[Output file "',FILNAM(I)(1:LF),'" opened]'
        ELSE
          WRITE(LOUT,*) '[No Output file opened]'
        ENDIF
      ENDDO
      RETURN
      END
#endif
