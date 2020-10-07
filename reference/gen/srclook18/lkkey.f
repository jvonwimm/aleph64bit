C*EI
C*DK LKKEY
C*DF UNIX
      SUBROUTINE LKKEY(STR,KW,NUMCOM,KEY,*)
C ----------------------------------------------------
C! get a keyword from a command string
C  get the KEY from a STR string of characters limited
C  or not by a "/".
C - Input : STR      / A = command string
C           KW       / A = list of keys
C           NUMCOM   / I = number of keys
C - Output: KEY      / A = found key
C ----------------------------------------------------
C*IF .NOT.DOC
C*CA LKFIL
      COMMON/LKFIL/SEQFIL,DIRFIL,FLGFIL(3)
     &            ,LTERM,LCARD,LDBAS,LUFMT,LOUT
     &            ,LFILE(3)
     &            ,LINDAT,LINSEL,LUTDAT,LUTSEL
      LOGICAL SEQFIL,DIRFIL,FLGFIL
      COMMON/FILCAR/SEQNAM,DIRNAM,FILNAM(3)
      CHARACTER*80 SEQNAM,DIRNAM,FILNAM
C*CC LKFIL
      CHARACTER STR*80,KEY*10,KW(20)*10
      LOGICAL FOUND,LKEQUAL
      EXTERNAL LKEQUAL
C ----------------------------------------------------
      IP = ICFNBL (STR,1,80)
      IF (IP.EQ.81) GOTO 99
      LEN=LKLWRD(STR(IP:80))-1
      IF (LEN.GT.10.OR.LEN.EQ.-2) THEN
          IF (LEN.EQ.0) LEN=81-IP
          WRITE(LOUT,*) 'Keyword too long, please re-enter ("HELP"',
     +                ' for command summary)'
          WRITE(LOUT,*) '"',STR(IP:IP+LEN),'"'
          GOTO 99
      ELSEIF (LEN.EQ.-1) THEN
          GOTO 99
      ENDIF
      KEY=STR(IP:IP+LEN)
      I=0
      FOUND=.FALSE.
 10   I=I+1
      IF (LKEQUAL(KEY,KW(I))) THEN
          FOUND=.TRUE.
      ELSE IF (I.LT.NUMCOM) THEN
          GOTO 10
      ENDIF
      IF (.NOT.FOUND) THEN
          WRITE(LOUT,*) 'Unknown keyword, please re-enter ("HELP"',
     +                'for command summary)'
          WRITE(LOUT,*) '"',STR(IP:IP+LEN),'"'
          GOTO 99
      ENDIF
      KEY=KW(I)
      STR=STR(IP+LEN+1:80)
      I=1
 20   IF (STR(I:I).EQ.' ') THEN
          I=I+1
          GOTO 20
      ENDIF
      IF (I.GT.1) STR=STR(I:80)
      RETURN
 99   RETURN 1
      END
