C*EI
C*DK LKRCAR
C*DF UNIX
      SUBROUTINE LKRCAR(COMAND)
C ------------------------------------------------------------
C ------------------------------------------------------------
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
      CHARACTER*80 COMAND,FILENM
      LOGICAL LEX
C-------------------------------------------------------------
      CALL LKGTPAR(COMAND,1,'_File: ',FILENM,LF)
      CALL AOPEN (LCARD,FILENM,'CARD','DISK',IER)
      IF (IER.EQ.0) CALL BREADC
      RETURN
      END
