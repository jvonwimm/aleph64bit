C*EI
C*DK LKBANK
C*DF UNIX
      SUBROUTINE LKBANK(COMAND)
C ---------------------------------------------------------------
C ---------------------------------------------------------------
C*IF .NOT.DOC
C*CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C*CC BCS
C*CA LKEVT
      COMMON/LKEVT/IRUNTR,IEVTR,IEVE,IVOULU,IRUNV
C*CC LKEVT
C*CA LKFIL
      COMMON/LKFIL/SEQFIL,DIRFIL,FLGFIL(3)
     &            ,LTERM,LCARD,LDBAS,LUFMT,LOUT
     &            ,LFILE(3)
     &            ,LINDAT,LINSEL,LUTDAT,LUTSEL
      LOGICAL SEQFIL,DIRFIL,FLGFIL
      COMMON/FILCAR/SEQNAM,DIRNAM,FILNAM(3)
      CHARACTER*80 SEQNAM,DIRNAM,FILNAM
C*CC LKFIL
      CHARACTER COMAND*80,CH
      CHARACTER*4 NMBK
      LOGICAL LKOPTI
      EXTERNAL LKOPTI
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
              CALL NPRNT(NMBK,NR)
            ENDIF
         ENDIF
      ENDIF
      RETURN
 999  WRITE(LOUT,*) '[Four character BOS name and bank number',
     +            ' required, wild characters accepted]'
      RETURN
 111  FORMAT (1X,3A,I8,A)
      END
