C*EI
C*DK LKOPDAF
C*DF UNIX
      SUBROUTINE LKOPDAF(COMAND)
C ---------------------------------------------------------------
C --------------------------------------------------------------
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
C*CA LKFIL
      COMMON/LKFIL/SEQFIL,DIRFIL,FLGFIL(3)
     &            ,LTERM,LCARD,LDBAS,LUFMT,LOUT
     &            ,LFILE(3)
     &            ,LINDAT,LINSEL,LUTDAT,LUTSEL
      LOGICAL SEQFIL,DIRFIL,FLGFIL
      COMMON/FILCAR/SEQNAM,DIRNAM,FILNAM(3)
      CHARACTER*80 SEQNAM,DIRNAM,FILNAM
C*CC LKFIL
      CHARACTER*80 ULIST
      CHARACTER COMAND*80,FILENM*80
C ---------------------------------------------------------------
      CALL LKGTPAR(COMAND,1,'_File: ',FILENM,LF)
      CALL AOPDBS(FILENM(1:LF),IRC)
      IF (IRC.NE.0) GOTO 999
      WRITE(LOUT,*)'[File ',FILENM(1:LF),' opened read-only]'
      DIRFIL=.TRUE.
      CALL BDROP (IW,'+DIS')
      CALL DAFRST (LDBAS)
      RETURN
 999  WRITE(LOUT,*) '[Error opening direct access file ',FILENM(1:LF),
     +            ' RC= ',IRC,' ]'
      DIRFIL=.FALSE.
      RETURN
      END
