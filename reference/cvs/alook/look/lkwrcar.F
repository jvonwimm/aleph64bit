      SUBROUTINE LKWRCAR(COMAND)
C ------------------------------------------------------------
C! write single bank in card format (72 col.)
C ------------------------------------------------------------
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
      FILENM = ' '
      IF (.NOT.FLGFIL(1)) THEN
         CALL LKOPWR (COMAND,'CARD',72)
         IF (.NOT.FLGFIL(1)) GOTO 999
      ENDIF
      KN=NLINK(BNK,NR)
      IF (KN.LE.0) THEN
          WRITE(LOUT,*)' [Bank "',BNK,NR,'" does not exist]'
      ELSE
          CALL BWRSB(IW,LFILE(1),BNK,NR)
      ENDIF
      RETURN
 999  WRITE(LOUT,*) '[Input parameters are BANK_NAME BANK_NUMBER',
     +            ' OUTPUT_FILE . No wild cards permitted]'
      RETURN
      END
#endif
