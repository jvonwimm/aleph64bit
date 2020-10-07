C*EI
C*DK LKINIT
C*DF UNIX
      SUBROUTINE LKINIT
C ----------------------------------------------------------
C! read data cards, format file  and open data base
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
      CHARACTER FNAME*200
      CHARACTER*120 FMT
      INTEGER ALFMT, ALGTENV
      LOGICAL LEX
C ---------------------------------------------------------
C
C     init ULIST
      CALL LKSTUNP('    ')
C     open data card file if any
      IST = ALGTENV ('LOOKCARDS',FNAME)
      LF = LENOCC(FNAME)
      CALL AINQUI(0,FNAME(1:LF),LEX)
C*IF DEBUG
C*EI
      IF(.NOT.LEX) THEN
        WRITE(IW(6),*) ' File ',FNAME(1:LF),' not found'
        IER = -1
      ELSE
        IER = 0
        CALL AOPEN (IW(5),FNAME,'CARD','DISK',IER)
      ENDIF
      IF (IER.EQ.0) CALL BREADC
C
C - get all formats
      IER = ALFMT (LUFMT,'ALL ',FMT)
      CALL BKFMT ('FILI','A')
C
C - default card, print, output files
      DO I=1,3
        FLGFIL(I)=.FALSE.
      ENDDO
      FILNAM(1)='LOOK_OUT.CARDS'
      FILNAM(2)='LOOK_OUT.PRINT'
      FILNAM(3)='LOOK_OUT.EPIO'
C
C - open default DAF on unit 4 by default
C
      CALL AOPDBS('    ',IER)
      IF (IER.NE.0) THEN
          DIRFIL=.FALSE.
      ELSE
          DIRFIL=.TRUE.
          CALL ADBVER(IVERS,IDAT)
      ENDIF
C
C - open input file if any given by data card
C
      FNAME = ' '
      SEQFIL=.FALSE.
      CALL ABMODE ('INTE')
      CALL ABRSEL ('E','     ',JRET)
      IRUNTR = 0
      IVOULU=0
      LINSEL = 0
      LINDAT = 0
      CALL LKOPRD(FNAME,FORMA,IER)
      IF(IER.NE.0) GOTO 100
 100  CONTINUE
C
      RETURN
      END
