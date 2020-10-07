C*EI
C*DK LKRDAF
C*DF UNIX
      SUBROUTINE LKRDAF(COMAND)
C ---------------------------------------------------------------
C ---------------------------------------------------------------
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
      CHARACTER COMAND*80,BNK*4,C
C --------------------------------------------------------------
      IF (.NOT.DIRFIL) THEN
          WRITE(LOUT,*) '[Direct access file not opened]'
          RETURN
      ENDIF
      CALL LKGTNAM(COMAND,BNK,*999)
      IF (BNK.EQ.'*') THEN
          WRITE(LOUT,*) '[Wild card not permitted for bankname]'
          RETURN
      ENDIF
      LDBAS = JUNIDB(0)
      CALL BDAPR(LDBAS,BNK)
      CALL LKGTINT(COMAND,2,' ',NR,C)
      IF (C.EQ.'A') GOTO 999
      IF (C.EQ.' '.OR.C.EQ.'*') THEN
          NR=NDANR(LDBAS,BNK,'LE',99999999)
          IF (NR.LT.0) GOTO 998
          WRITE(LOUT,*) '[Highest bank number will be used]'
      ENDIF
      IND=MDARD(IW,LDBAS,BNK,NR)
      IF (IND.LE.0) THEN
        WRITE(LOUT,FMT='(2A,I8,A)')' [Bank "',BNK,NR,'" does not exist]'
      ELSE
        CALL BLIST(IW,'T+',BNK)
      ENDIF
      RETURN
 998  WRITE(LOUT,*) '[Bank "',BNK,'" does not exist]'
      RETURN
 999  WRITE(LOUT,*) '[Please enter with four character bank name ',
     +            'and optional bank number]'
      RETURN
      END
