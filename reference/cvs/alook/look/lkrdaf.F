      SUBROUTINE LKRDAF(COMAND)
C ---------------------------------------------------------------
C ---------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "lkfil.h"
      CHARACTER COMAND*80,BNK*4,C
C --------------------------------------------------------------
      IF (.NOT.DIRFIL) THEN
          WRITE(LOUT,*) '[Direct access file not opened]'
          RETURN
      ENDIF
      CALL LKGTNAM(COMAND,BNK,*999)
      IF (BNK.EQ.'*') THEN
          WRITE(LOUT,*) '[Wild card not permitted for bankname]'
          RETURN
      ENDIF
      LDBAS = JUNIDB(0)
      CALL BDAPR(LDBAS,BNK)
      CALL LKGTINT(COMAND,2,' ',NR,C)
      IF (C.EQ.'A') GOTO 999
      IF (C.EQ.' '.OR.C.EQ.'*') THEN
          NR=NDANR(LDBAS,BNK,'LE',99999999)
          IF (NR.LT.0) GOTO 998
          WRITE(LOUT,*) '[Highest bank number will be used]'
      ENDIF
      IND=MDARD(IW,LDBAS,BNK,NR)
      IF (IND.LE.0) THEN
        WRITE(LOUT,FMT='(2A,I8,A)')' [Bank "',BNK,NR,'" does not exist]'
      ELSE
        CALL BLIST(IW,'T+',BNK)
      ENDIF
      RETURN
 998  WRITE(LOUT,*) '[Bank "',BNK,'" does not exist]'
      RETURN
 999  WRITE(LOUT,*) '[Please enter with four character bank name ',
     +            'and optional bank number]'
      RETURN
      END
#endif
