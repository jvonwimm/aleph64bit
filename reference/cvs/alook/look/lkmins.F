      SUBROUTINE LKMINS(COMAND)
C -----------------------------------------------------------
C -----------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "lkfil.h"
#include "lklast.h"
      CHARACTER COMAND*80,C
C ------------------------------------------------------------
      IF (LBNKNM.EQ.' ') THEN
          WRITE(LOUT,*) '[No previous bank recorded]'
          RETURN
      ENDIF
      CALL LKGTINT(COMAND,1,' ',I,C)
      IF (COMAND(1:1).EQ.'*') C='*'
      IF (C.EQ.'A') THEN
          WRITE(LOUT,*) '[Optional step size must be integer or "*"]'
          RETURN
      ENDIF
      IF (C.EQ.'I') THEN
          LPRNUM=I
      ELSEIF (C.EQ.'*') THEN
          LPRNUM=999999
      ENDIF
      ISZ=LPRNUM
      CALL LKPRROW(LBNKNM,LBNKNR,LROWNM-ISZ,LROWNM-1,'I')
      LROWNM=LROWNM-ISZ
      RETURN
      END
#endif
