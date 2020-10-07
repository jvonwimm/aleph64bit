C*EI
C*DK LKREAD0
C*DF UNIX
C*IF IBM.OR.UNIX
      SUBROUTINE LKREAD0(PROMPT,INISTR,COMAND)
C ------------------------------------------------------------
C! reads a command
C ------------------------------------------------------------
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
      CHARACTER*(*) PROMPT,INISTR,COMAND
      CHARACTER*80 COM1
C -----------------------------------------------------------
      IF(INISTR.NE.' ') THEN
         COMAND=INISTR
         RETURN
      ENDIF
 200  WRITE(LOUT,*) PROMPT
      READ(LTERM,'(A80)',END=210,ERR=210) COMAND
      RETURN
 210  COMAND = ' '
      REWIND LTERM
      RETURN
      END
