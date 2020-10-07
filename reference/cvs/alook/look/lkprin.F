      SUBROUTINE LKPRIN(COMAND)
C -----------------------------------------------------
C ----------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "lkfil.h"
      CHARACTER COMAND*80,FILENM*80
C -----------------------------------------------------
      IF (.NOT.FLGFIL(2)) CALL LKOPWR(COMAND,'CARD',132)
      IW(6)=LFILE(2)
      CALL DLUNP (IW(6))
      CALL LKBANK(COMAND)
      IW(6)=6
      CALL DLUNP (IW(6))
      RETURN
      END
#endif
