#if defined(IBM) || defined(UNIX)
      SUBROUTINE LKREAD0(PROMPT,INISTR,COMAND)
C ------------------------------------------------------------
C! reads a command
C ------------------------------------------------------------
#ifndef DOC
#include "lkfil.h"
      CHARACTER*(*) PROMPT,INISTR,COMAND
      CHARACTER*80 COM1
C -----------------------------------------------------------
      IF(INISTR.NE.' ') THEN
         COMAND=INISTR
         RETURN
      ENDIF
 200  WRITE(LOUT,'($,A)') PROMPT
      READ(LTERM,'(A80)',END=210,ERR=210) COMAND
      RETURN
 210  COMAND = ' '
      REWIND LTERM
      RETURN
      END
#endif
#endif
