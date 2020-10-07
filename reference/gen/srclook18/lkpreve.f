C*EI
C*DK LKPREVE
C*DF UNIX
      SUBROUTINE LKPREVE
C -----------------------------------------------------------
C! print E-list on unit LFILE(2)
C ----------------------------------------------------------
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
C*CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C*CC BCS
C -----------------------------------------------------------
      IW(6)=LFILE(2)
      CALL DLUNP (IW(6))
      CALL BPRNT(IW,'E')
      IW(6)=6
      CALL DLUNP (IW(6))
      RETURN
      END
