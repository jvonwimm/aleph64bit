C*EI
C*DK LKWRLIS
C*DF UNIX
      SUBROUTINE LKWRLIS(COMAND)
C ----------------------------------------------------------
C! write a list on unit 13
C ----------------------------------------------------------
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
      LOGICAL LKOPTI
      CHARACTER*(*) COMAND
      CHARACTER*200 TEMP,LISTS*80
C ----------------------------------------------------------
      LISTS=' '
      TEMP=' '
      CALL LKGTPAR(COMAND,1,'_List) to be written out: ',LISTS,LL)
      IF (.NOT. FLGFIL(3)) THEN
          CALL LKOPWR(COMAND,'   ',32040)
          IF (.NOT.FLGFIL(3)) RETURN
      ENDIF
      IF (INDEX(LISTS,'C').GT.0) CALL BWRITE(IW,LFILE(3),'C')
      IF (INDEX(LISTS,'E').GT.0) CALL BWRITE(IW,LFILE(3),'E')
      IF (INDEX(LISTS,'S').GT.0) CALL BWRITE(IW,LFILE(3),'S')
      IF (INDEX(LISTS,'T').GT.0) CALL BWRITE(IW,LFILE(3),'T')
      RETURN
      END
