C*EI
C*DK LKNWSEQ
C*DF UNIX
      SUBROUTINE LKNWSEQ(COMAND)
C ------------------------------------------------------------
C! open a new sequential file on input
C ------------------------------------------------------------
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
      CHARACTER FILENM*80, FORMA*6, CH, TAPE*9
      CHARACTER*(*) COMAND
      LOGICAL LKOPTI
      INTEGER ALSTGQRY
      PARAMETER (MXTYP=5)
      CHARACTER*4 TYPE,DATYP(MXTYP)
      DATA DATYP /'RAW ','POT ','DST ','MINI','NANO'/

C ----------------------------------------------------------
      TYPE = ' '
      LRUK7 = 9
      KSRUN = NDROP('SRUN',0)
      DO 1 ITYP =1,MXTYP
         IF (LKOPTI(COMAND,DATYP(ITYP))) THEN
            TYPE = DATYP(ITYP)
            CALL LKGTINT (COMAND,1,'  ',NRUN,CH)
            IF (CH.NE.' ' .AND. CH.NE.'I') THEN
               WRITE(LOUT,*) '[Invalid run number]'
               RETURN
            ENDIF
            IRK7 = LKK7RU  (LRUK7,NRUN,TYPE,TAPE,FILENM,FORMA)
            IF (IRK7.EQ.0) THEN
               LF = LNBLNK(FILENM)
               KSRUN = NBANK('SRUN',0,1)
               IW(KSRUN+1) = NRUN
               LASRUN = NRUN
               LASEVT = 999999
               CALL ABSMAX (LASRUN,LASEVT)
               GOTO 100
            ELSE
C*IF IBM
C*EI
               RETURN
            ENDIF
         ENDIF
 1    CONTINUE
C
      IF (LKOPTI(COMAND,'CART')) THEN
         FORMA = 'CART'
      ELSEIF (LKOPTI(COMAND,'MOUNT')) THEN
         FORMA = 'MOUNT'
      ELSEIF (LKOPTI(COMAND,'EPIO')) THEN
         FORMA = 'EPIO'
      ELSEIF (LKOPTI(COMAND,'EDIR')) THEN
         FORMA = 'EDIR'
      ELSEIF (LKOPTI(COMAND,'NATIVE')) THEN
         FORMA = 'NATIVE'
      ENDIF
      CALL LKGTPAR(COMAND,1,'_File: ',FILENM,LF)
C
 100  CONTINUE
C
      IF (LKOPTI(COMAND,'QUERY')) THEN
         IER = ALSTGQRY (FILENM)
         IF (IER.EQ.0) THEN
            WRITE(LOUT,*)'[file ',FILENM(1:LF),' is ',
     &                   'NOT staged]'
         ELSE
            WRITE(LOUT,*)'[file ',FILENM(1:LF),' is ',
     &                   'staged]'
         ENDIF
         RETURN
      ENDIF
C
      SEQNAM=FILENM
C*IF DEBUG
C*EI
C - open new input file and read it till the 1st event
      CALL LKOPRD(FILENM,FORMA,IER)
      KSRUN = NDROP('SRUN',0)
      END
