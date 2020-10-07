C*EI
C*DK LKPLUS
C*DF UNIX
      SUBROUTINE LKPLUS(COMAND)
C ----------------------------------------------------------
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
C*CA LKLAST
      COMMON/LKLAST/LBNKNM,LBNKNR,LROWNM,LPRNUM
      CHARACTER*4 LBNKNM
      INTEGER LBNKNR,LROWNM,LPRNUM
C*CC LKLAST
      CHARACTER COMAND*80,C
C ---------------------------------------------------------
      IF (LBNKNM.EQ.' ') THEN
          WRITE(LOUT,*) '[No previous bank recorded]'
          RETURN
      ENDIF
      CALL LKGTINT(COMAND,1,' ',I,C)
      IF (COMAND(1:1).EQ.'*') C='*'
      IF (C.EQ.'A') THEN
          WRITE(LOUT,*) '[Optional step size must be integer or "*"]'
          RETURN
      ENDIF
      IF (C.EQ.'I') THEN
          LPRNUM=I
      ELSEIF (C.EQ.'*') THEN
          LPRNUM=999999
      ENDIF
      ISZ=LPRNUM
      CALL LKPRROW(LBNKNM,LBNKNR,LROWNM+1,LROWNM+ISZ,'I')
      LROWNM=LROWNM+ISZ
      RETURN
      END
