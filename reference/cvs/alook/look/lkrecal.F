      SUBROUTINE LKRECAL(COMAND,PRECOM,NUM,STR)
C ---------------------------------------------------
C ---------------------------------------------------
#ifndef DOC
      CHARACTER*80 COMAND,PRECOM(20),PAR,C*1,STR,COM
#include "lkfil.h"
C --------------------------------------------------
      STR=' '
      NP=0
      CALL LKGTINT(COMAND,1,' ',N,C)
      IF (C.EQ.'I') THEN
          IF (N.GT.0.AND.N.LE.NUM) NP=N
      ELSE
          IF (C.EQ.' ') THEN
              NP=NUM
          ELSE
              LEN=LNBLNK(COMAND)
              I=NUM+1
 10           IF (I.GT.1.AND.NP.EQ.0) THEN
                  I=I-1
                  COM=PRECOM(I)(1:LEN)
                  CALL CLTOU(COM)
                  IF (COM.EQ.COMAND) NP=I
                  GOTO 10
              ENDIF
          ENDIF
      ENDIF
      IF (NP.EQ.0) THEN
          WRITE(LOUT,*) '[Not found, type "RECALL/ALL" for list]'
      ELSE
          STR=PRECOM(NP)
      ENDIF
      RETURN
      END
#endif
