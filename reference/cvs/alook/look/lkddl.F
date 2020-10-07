      SUBROUTINE LKDDL(COMAND)
C -------------------------------------------------------------
#ifndef DOC
#include "lkfil.h"
#include "bcs.h"
      CHARACTER COMAND*80,CH,NMBK*4
C ------------------------------------------------------------
      IF (.NOT.DIRFIL) THEN
          WRITE(LOUT,*) '[Direct access file not opened]'
          RETURN
      ENDIF
      CALL LKGTNAM(COMAND,NMBK,*999)
      IF (NMBK.EQ.'*') GOTO 998
      CALL LKGTINT(COMAND,2,'_Bank number: ',NR,CH)
      IF (CH.EQ.'*') GOTO 998
      IF (CH.NE.'I') GOTO 999
      CALL UTDBPR(NMBK,NR,0,LDBAS,IW(6),IFLAG)
      RETURN
 998  WRITE(LOUT,*) '[Wild cards not permitted for this operation]'
      RETURN
 999  WRITE(LOUT,*) '[Four letter bank name and bank number required]'
      RETURN
      END
#endif
