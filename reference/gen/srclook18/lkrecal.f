C*EI
C*DK LKRECAL
C*DF UNIX
      SUBROUTINE LKRECAL(COMAND,PRECOM,NUM,STR)
C ---------------------------------------------------
C ---------------------------------------------------
C*IF .NOT.DOC
      CHARACTER*80 COMAND,PRECOM(20),PAR,C*1,STR,COM
C*CA LKFIL
      COMMON/LKFIL/SEQFIL,DIRFIL,FLGFIL(3)
     &            ,LTERM,LCARD,LDBAS,LUFMT,LOUT
     &            ,LFILE(3)
     &            ,LINDAT,LINSEL,LUTDAT,LUTSEL
      LOGICAL SEQFIL,DIRFIL,FLGFIL
      COMMON/FILCAR/SEQNAM,DIRNAM,FILNAM(3)
      CHARACTER*80 SEQNAM,DIRNAM,FILNAM
C*CC LKFIL
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
