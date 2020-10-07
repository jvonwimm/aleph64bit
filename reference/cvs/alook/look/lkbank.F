      SUBROUTINE LKBANK(COMAND)
C ---------------------------------------------------------------
C ---------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "lkevt.h"
#include "lkfil.h"
      CHARACTER COMAND*80,CH
      CHARACTER*4 NMBK
      LOGICAL LKOPTI
      INTEGER NPRNT
      EXTERNAL LKOPTI,NPRNT
      INTEGER IRET
C --------------------------------------------------------------
      CALL LKGTNAM(COMAND,NMBK,*999)
      IF (NMBK.EQ.'*') THEN
          CALL BPRNT(IW,'C')
          CALL BPRNT(IW,'E')
          CALL BPRNT(IW,'S')
          CALL BPRNT(IW,'T')
          CALL BPRNT(IW,'0')
          RETURN
      ENDIF
      CALL LKGTINT(COMAND,2,'_Bank number: ',NR,CH)
      INDEX=IW(NAMIND(NMBK))
      IF(INDEX.EQ.0)THEN
         WRITE(LOUT,*)'[Bank "',NMBK,'" does not exist]'
      ELSE
         IF(CH.EQ.'*')THEN
            CALL BLIST(IW,'R=','0')
            CALL BLIST(IW,'R+',NMBK)
            CALL BPRNT(IW,'R')
            CALL BLIST(IW,'R=','0')
         ELSE
            IF (CH.NE.'I') GOTO 999
            INDEX=NLINK(NMBK,NR)
            IF (INDEX.EQ.0) THEN
              WRITE(LOUT,111) '[Bank "',NMBK,' ',NR,'" does not exist]'
            ELSE
              IRET = NPRNT(NMBK,NR)
            ENDIF
         ENDIF
      ENDIF
      RETURN
 999  WRITE(LOUT,*) '[Four character BOS name and bank number',
     +            ' required, wild characters accepted]'
      RETURN
 111  FORMAT (1X,3A,I8,A)
      END
#endif
