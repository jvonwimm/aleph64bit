      SUBROUTINE LKPRBNK(COMAND)
C ----------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "lkfil.h"
      CHARACTER*80 COMAND,FILENM,BNK*4,C*1
      DATA IFI /0/
C -------------------------------------------------------------
      CALL LKGTNAM (COMAND,BNK,*999)
      IF (BNK.EQ.'*') GOTO 999
      CALL LKGTINT(COMAND,2,'_Bank number: ',NR,C)
      IF (C.NE.'I') GOTO 999
      IF (.NOT.FLGFIL(2)) CALL LKOPWR (COMAND,'CARD',132)
      IW(6) = LFILE(2)
      CALL DLUNP (IW(6))
      CALL LKNICE(COMAND)
      IW(6)=6
      CALL DLUNP (IW(6))
      RETURN
 999  WRITE(LOUT,*) '[Input parameters are BANK_NAME BANK_NUMBER',
     +            ' OUTPUT_FILE . No wild cards permitted]'
      END
#endif
