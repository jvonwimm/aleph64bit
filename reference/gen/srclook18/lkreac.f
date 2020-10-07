C*EI
C*EI
C*DK LKREAC
C*DF UNIX
      SUBROUTINE LKREAC
C -------------------------------------------------------
C! Read C-list (until 1st event found)
C  called at start of file
CKEY LOOK RUNH CLIST
C -------------------------------------------------------
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
C*CA LKUNPK
      COMMON/LKUNPK/ ULIST
      CHARACTER*80 ULIST
C*CC LKUNPK
      DATA NSLOW/ 0/
C ------------------------------------------------------
      IF (NSLOW.EQ.0) NSLOW=NAMIND('SLOW')
C           READ till the begin of run or 1st event
      CALL BDROP(IW,'E')
      CALL BDROP(IW,'C')
      CALL BLIST(IW,'E=','0')
      CALL BLIST(IW,'C=','0')
      IF (LINSEL.GT.0) CALL BDROP (IW,'$PTS')
      CALL ABMODE ('BATC')
C
 10   IRET = 0
      CALL ABRREC ('E',ULIST,IRET)
      IF (IRET.GE.5) THEN
         WRITE (LOUT,*) ' [EOF or data file not opened]'
         goto 999
      ENDIF
C     reset ULIST
      CALL LKRSUNP
      CALL LKSHOW (IRET)
      IF (IRET.EQ.3) THEN
        CALL BLIST (IW,'T+','E')
        CALL BLIST (IW,'E=','0')
      ENDIF
      IF (IW(NSLOW).EQ.0 .AND. IRET.NE.1) GOTO 10
C
 999  CONTINUE
      CALL ABMODE ('INTE')
      END
