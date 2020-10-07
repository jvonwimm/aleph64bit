C*EI
C*DK LKSTAT
C*DF UNIX
      SUBROUTINE LKSTAT
C --------------------------------------------------------------
C! status of I/O files
C --------------------------------------------------------------
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
C -------------------------------------------------------------
      IF (SEQFIL) THEN
          LF=LNBLNK(SEQNAM)
          WRITE(LOUT,*) '[Sequential file "',SEQNAM(1:LF),'" opened]'
      ELSE
          WRITE(LOUT,*) '[No sequential file opened]'
      ENDIF
      IF (DIRFIL) THEN
          LF=LNBLNK(DIRNAM)
          WRITE(LOUT,*) '[Direct access file "',DIRNAM(1:LF),'" opened]'
      ELSE
          WRITE(LOUT,*) '[No direct access file opened]'
      ENDIF
      DO I=1,3
        IF (FLGFIL(I)) THEN
          LF=LNBLNK(FILNAM(I))
          WRITE(LOUT,*) '[Output file "',FILNAM(I)(1:LF),'" opened]'
        ELSE
          WRITE(LOUT,*) '[No Output file opened]'
        ENDIF
      ENDDO
      RETURN
      END
