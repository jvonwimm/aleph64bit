      SUBROUTINE LKGTUNP(COMAND)
C -------------------------------------------------------------
C -------------------------------------------------------------
#ifndef DOC
      CHARACTER*80 COMAND,TEMP
#include "lkfil.h"
C -------------------------------------------------------------
      TEMP=' '
      CALL LKCOMPA(COMAND,'ULIST',TEMP)
      IF (TEMP.EQ.' ') THEN
          WRITE(LOUT,*) ' Give unpacking list [ AL ]'
          READ(5,1010) TEMP
          IF (TEMP.EQ.' ') TEMP='AL '
      ENDIF
      CALL LKSTUNP (TEMP)
 1010 FORMAT(A4)
      RETURN
      END
#endif
