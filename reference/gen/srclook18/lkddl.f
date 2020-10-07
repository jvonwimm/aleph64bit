C*EI
C*DK LKDDL
C*DF UNIX
      SUBROUTINE LKDDL(COMAND)
C -------------------------------------------------------------
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
      CHARACTER COMAND*80,CH,NMBK*4
C ------------------------------------------------------------
      IF (.NOT.DIRFIL) THEN
          WRITE(LOUT,*) '[Direct access file not opened]'
          RETURN
      ENDIF
      CALL LKGTNAM(COMAND,NMBK,*999)
      IF (NMBK.EQ.'*') GOTO 998
      CALL LKGTINT(COMAND,2,'_Bank number: ',NR,CH)
      IF (CH.EQ.'*') GOTO 998
      IF (CH.NE.'I') GOTO 999
      CALL UTDBPR(NMBK,NR,0,LDBAS,IW(6),IFLAG)
      RETURN
 998  WRITE(LOUT,*) '[Wild cards not permitted for this operation]'
      RETURN
 999  WRITE(LOUT,*) '[Four letter bank name and bank number required]'
      RETURN
      END
