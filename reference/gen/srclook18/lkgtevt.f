C*EI
C*DK LKGTEVT
C*DF UNIX
      SUBROUTINE LKGTEVT(FSEQ,LIST,ULIST,NRUN,NEVT,IER )
C----------------------------------------------------------------------
C - F.Ranjard - 911015
CKEY LOOK READ EVENT / USER
C
C!  read selected event from sequential or direct access file
C   Only event records are returned, all others  are skipped.
C
C   Inputs    :
C               FSEQ     .TRUE. when runs are in increasing order
C               LIST     list of bank names
C               ULIST    character string to steer unpacking
C                        Definition see in AUNPCK.
C               NRUN     run number   ( =0 means ignore run number   )
C               NEVT     event number ( =0 means ignore event number )
C                        ( NRUN=NEVT=0 means read next event, or read
C                          next slow control record if class 25 is on)
C
C   Outputs   : IER = 1  event record found
C                   = 0  event NOT found
C                   = 5  EOF
C
C   Calls:      ABRREC,BDROP,BGARB
C   Entry points:
C               LKGTEVR (ITYP)
C               returns in ITYP the ABRREC return code
C----------------------------------------------------------------------
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
      SAVE IABRREC
      LOGICAL FSEQ, BTEST
      CHARACTER*(*) LIST,ULIST
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------

      IF(FIRST) THEN
        FIRST=.FALSE.
        NASEVT=NAMIND('SEVT')
        NASRUN=NAMIND('SRUN')
      ENDIF

      IER=0

C                      SELECT EVENT
C                     WITH RUN AND EVENT #

      IF(NEVT.NE.0) THEN
        IF(IW(NASRUN).NE.0) CALL BDROP(IW,'SRUN')
        KSEVT=IW(NASEVT)
        IF(KSEVT.LE.0) KSEVT=NBANK('SEVT',0,2)
        IW(KSEVT+1)=NRUN
        IW(KSEVT+2)=NEVT
        LASRUN = NRUN
        LASEVT = NEVT
      ENDIF
C                SELECT RUN ONLY
      IF(NEVT.EQ.0.AND.NRUN.NE.0) THEN
        IF(IW(NASEVT).NE.0) CALL BDROP(IW,'SEVT')
        KSRUN=IW(NASRUN)
        IF(KSRUN.LE.0) KSRUN=NBANK('SRUN',0,1)
        IW(KSRUN+1)=NRUN
        LASRUN = NRUN
        LASEVT = 999999
      ENDIF
C                NO SELECTION, READ NEXT EVENT
      IF(NEVT.EQ.0.AND.NRUN.EQ.0) THEN
        IF(IW(NASEVT).NE.0) CALL BDROP(IW,'SEVT')
        IF(IW(NASRUN).NE.0) CALL BDROP(IW,'SRUN')
        LASRUN = 999999
        LASEVT = 999999
      ENDIF
      IF (.NOT.FSEQ) LASRUN = 999999
      CALL ABSMAX (LASRUN,LASEVT)
C
  1   CALL ABRREC (LIST,ULIST,IRET)
      IABRREC = IRET
C                accept event records only
      IF(IRET.GE.4) GOTO 999
      IF(IRET.EQ.2) THEN
C      run record : it has been put on C-list by ABRREC
C                   look forward to the event required if any
         GOTO 1
      ELSEIF(IRET.EQ.3) THEN
C      unknown record: skip it if an event is required
        IF (NEVT.GT.0 .OR. NRUN.GT.0) GOTO 1
      ELSEIF(IRET.EQ.1) THEN
C      event record: skip it if slow control is required
C      and NO EDIR.
        IF (LINSEL.EQ.0 .AND. NEVT.EQ.0 .AND. NRUN.EQ.0) THEN
           CALL ABGTRCL (MASKR)
           IF (BTEST(MASKR,24)) GOTO 1
        ENDIF
      ENDIF
C
      CALL BDROP (IW,'SRUNSEVT')
      RETURN
  999 IER= IRET
      CALL BDROP (IW,'SRUNSEVT')
      RETURN
C
C ------------------------------------------------------------
C
      ENTRY LKGTEVR (ITYP)
      ITYP = IABRREC
C
      END
