C*DK LKGTUNP
C*DF UNIX
      SUBROUTINE LKGTUNP(COMAND)
C -------------------------------------------------------------
C -------------------------------------------------------------
C*IF .NOT.DOC
      CHARACTER*80 COMAND,TEMP
C*CA LKFIL
      COMMON/LKFIL/SEQFIL,DIRFIL,FLGFIL(3)
     &            ,LTERM,LCARD,LDBAS,LUFMT,LOUT
     &            ,LFILE(3)
     &            ,LINDAT,LINSEL,LUTDAT,LUTSEL
      LOGICAL SEQFIL,DIRFIL,FLGFIL
      COMMON/FILCAR/SEQNAM,DIRNAM,FILNAM(3)
      CHARACTER*80 SEQNAM,DIRNAM,FILNAM
C*CC LKFIL
C -------------------------------------------------------------
      TEMP=' '
      CALL LKCOMPA(COMAND,'ULIST',TEMP)
      IF (TEMP.EQ.' ') THEN
          WRITE(LOUT,*) ' Give unpacking list [ AL ]'
          READ(5,1010) TEMP
          IF (TEMP.EQ.' ') TEMP='AL '
      ENDIF
      CALL LKSTUNP (TEMP)
 1010 FORMAT(A4)
      RETURN
      END
