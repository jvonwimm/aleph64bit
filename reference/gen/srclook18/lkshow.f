C*EI
C*DK LKSHOW
C*DF UNIX
      SUBROUTINE LKSHOW (JTYP)
C -----------------------------------------------------------
C - Input  : JTYP /I = LKGTEVT return code
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
C*CA LKEVT
      COMMON/LKEVT/IRUNTR,IEVTR,IEVE,IVOULU,IRUNV
C*CC LKEVT
C*CA LKFIL
      COMMON/LKFIL/SEQFIL,DIRFIL,FLGFIL(3)
     &            ,LTERM,LCARD,LDBAS,LUFMT,LOUT
     &            ,LFILE(3)
     &            ,LINDAT,LINSEL,LUTDAT,LUTSEL
      LOGICAL SEQFIL,DIRFIL,FLGFIL
      COMMON/FILCAR/SEQNAM,DIRNAM,FILNAM(3)
      CHARACTER*80 SEQNAM,DIRNAM,FILNAM
C*CC LKFIL
      DATA NEVEH,NRUNR,NRUNH/3*0/
C -----------------------------------------------------------
      IF (NEVEH.EQ.0) THEN
         NEVEH = NAMIND('EVEH')
         NRUNR = NAMIND('RUNR')
         NRUNH = NAMIND('RUNH')
      ENDIF
C
      ITYP = JTYP
      IF (ITYP .EQ. 0) CALL LKGTEVR (ITYP)
C
      IF(ITYP .EQ.1) THEN
        IEVTR = IW(IW(NEVEH)+6)
        IRUNTR = IW(IW(NEVEH)+2)
        WRITE(LOUT,*)' EVENT RECORD,    event:',IEVTR,' , run:',IRUNTR
      ELSEIF(ITYP.EQ.2) THEN
        IRUNTR = IW(IW(NRUNR)+2)
        WRITE(LOUT,*)' RUN RECORD,    run:',IRUNTR
      ELSEIF(ITYP.EQ.3) THEN
        WRITE(LOUT,*)' UNKNOWN RECORD'
      ENDIF
      RETURN
      END
