C*EI
C*DK LKSCAN
C*DF UNIX
      SUBROUTINE LKSCAN
C ----------------------------------------------------
C! scan a file
C ----------------------------------------------------
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
C*CA LKEVT
      COMMON/LKEVT/IRUNTR,IEVTR,IEVE,IVOULU,IRUNV
C*CC LKEVT
C*CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C*CC BCS
C*CA LKUNPK
      COMMON/LKUNPK/ ULIST
      CHARACTER*80 ULIST
C*CC LKUNPK
      SAVE
      DATA NRUNR /0/
C -------------------------------------------------
      IF (NRUNR.EQ.0) NRUNR = NAMIND ('RUNR')
      ICURR=-1
      NEVT = 0
      NUNC = 0
      IER=0
      IF (.NOT.SEQFIL) THEN
        WRITE(LOUT,*) '[SEQUENTIAL FILE NOT OPENED]'
        RETURN
      ENDIF
      CALL ABMODE ('BATC')
C
      IF (LINSEL.EQ.0) THEN
         CALL BRWND(LINDAT)
      ELSE
C       rewind EDIR, drop current $PTS bank, skip FILM card
        REWIND LINSEL
        CALL BDROP (IW,'$PTS')
    1   CALL BTERD (IW,LINSEL,'R',*1,*20)
      ENDIF
   20 CALL BDROP(IW,'C')
      CALL BLIST(IW,'C=','0')
      CALL ABSMAX (999999,999999)
      CALL BDROP (IW,'SEVTSRUNNEVT')
C
   50 CALL BDROP(IW,'E')
      CALL BDROP(IW,'R')
      CALL BLIST(IW,'E=','0')
      CALL BLIST(IW,'R=','0')
      CALL BGARB(IW)
      IER = 0
      IF (LINSEL.EQ.0) THEN
        CALL ABRREC ('E','NODE',IRET)
        IRUNR=IW(NRUNR)
        IF(IRUNR.NE.0) IRUN = IW(IRUNR+2)
      ELSE
         CALL AREDIR (IEVT,IRUN,IPTR,IRET)
      ENDIF
      IF(IRET.EQ.2) THEN
        IF(IRUN.NE.ICURR)  THEN
          IF (ICURR.NE.-1) THEN
             WRITE(LOUT,*) ' End of run ',ICURR,' with ',NEVT,
     +      ' event records and ',NUNC, ' unknown records'
          ENDIF
          WRITE(LOUT,*) ' Begin of run ',IRUN
          ICURR = IRUN
          NEVT = 0
          NUNC = 0
        ENDIF
      ELSEIF(IRET.EQ.1) THEN
        NEVT = NEVT + 1
      ELSE IF (IRET.EQ.3) THEN
        NUNC = NUNC + 1
      ENDIF
      CALL BLIST(IW,'R=','0')
      IF (IRET.LT.4) GOTO 50
C
      IF(NUNC.NE.0.OR.NEVT.NE.0) THEN
         WRITE(LOUT,*) ' End of run ',ICURR,' with ',NEVT,
     +   ' event records and ',NUNC, ' unknown records'
      ENDIF
      IF (LINSEL.EQ.0) THEN
        CALL BRWND(LINDAT)
      ELSE
C       rewind EDIR, drop current $PTS bank, skip FILM card
        REWIND LINSEL
        CALL BDROP (IW,'$PTS')
    2   CALL BTERD (IW,LINSEL,'R',*2,*30)
      ENDIF
   30 CALL BDROP(IW,'E')
      CALL BDROP(IW,'R')
      CALL BDROP(IW,'C')
      CALL BDROP(IW,'T')
      CALL BLIST(IW,'E=','0')
      CALL BLIST(IW,'R=','0')
      CALL BLIST(IW,'C=','0')
      CALL BLIST(IW,'T=','0')
C
      CALL ABMODE ('INTE')
      END
