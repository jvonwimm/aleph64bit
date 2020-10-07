C*EI
C*DK LKCHKRU
C*DF UNIX
      SUBROUTINE LKCHKRU (NRUN,NOLD,IRET)
C -------------------------------------------------------------
CKEY LOOK CHECK RUN
C - F.Ranjard - 920121
C! check if run # NRUN is on the current file
C - Input    : NRUN   / I  = new run number
C              NOLD   / I  = previous run number
C - Output   : IRET   / I  = return code
C                            = 17  run is present
C                            = 0   run is not present
C -------------------------------------------------------------
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
      CHARACTER TAPOLD*8, CHAINT*4
      LOGICAL FSEQ
      DATA FSEQ /.TRUE./
      DATA NARUNL /0/
C -----------------------------------------------------------
      IF (NARUNL.EQ.0) NARUNL = NAMIND('RUNL')
      IRET = 0
      JRUNL = IW(NARUNL)
      IF (JRUNL.NE.0) THEN
         TAPOLD(1:4) = CHAINT(IW(JRUNL+1))
         TAPOLD(5:8) = CHAINT(IW(JRUNL+2))
         IF (INDEX(SEQNAM,TAPOLD(1:6)).EQ.0) GOTO 3
         DO 1 I=1,IW(JRUNL+3)
            IF (NRUN.EQ.IW(JRUNL+3+I)) GOTO 2
 1       CONTINUE
         GOTO 3
 2       CONTINUE
      ENDIF
      IRET = 17
      IF (NRUN.LT.NOLD) THEN
         KSRUN = NBANK('SRUN',0,1)
         IW(KSRUN+1) = NRUN
         IF (LINSEL.NE.0) THEN
            CALL LKRWND('/EDIR')
         ELSE
            CALL LKRWND('/DATA')
         ENDIF
      ELSEIF(NRUN.GT.NOLD) THEN
         IEVT = 0
         CALL LKGTEVT(FSEQ,'E',ULIST,NRUN,IEVT,IER)
         CALL LKSHOW (IER)
      ENDIF
      KSRUN = NDROP('SRUN',0)
C
 3    CONTINUE
      END
