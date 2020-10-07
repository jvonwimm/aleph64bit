      PROGRAM RWALEPH
C ----------------------------------------------------------
C! read/write aleph data in native or epio format.
C - F.Ranjard - 920414
C - cards file name : alpha.cards on ALWS
C                     $ALPHACARDS on UNIX
C
C - data cards : FILI, FILO, SRUN, SEVT, IRUN, CLAS, UNPK, COMP
C                READ  another data cards file
C                FDBA read a data base file (not ADBSCONS)
C                FFMT read bank format file (not BANKALFMT)
C                DEBUG rec1 rec2 ! debug evt record# rec1 to
C                                        evt record# rec2,
C                                        slow control record# rec1 to
C                                        slow control rcord# rec2,
C                                        all run records.
C                for each of them print the sequential record#,
C                                       the evt# and run# if relevant,
C                                       the number of banks,
C                                       the record length.
C
C - some examples:
C   * example #1
C   * Select class 16 events in the first 1000 event records
C   * write them onto DATA:P41283.EPIO
C   FILI 'AL$DATA:P0041283.EPIO'
C   FILO 'DATA:P41283_16.EPIO'
C   CLASS 16
C   NEVT 1000 
C   ENDQ
C
C   * example #2
C   * concatenate SOR file, EOR file and triggers 9781 and 14196
C   * from run 41352.
C   * write the output file in EPIO format
C   FILI 'DISK$FALCON17:[RAW]R0041352.SOR | NATIVE'
C   FILI 'DISK$FALCON17:[RAW]R0041352.EOR | NATIVE'
C   FILI 'DISK$FALCON17:[RAW]R0041352.DAT | NATIVE'
C   FILO 'R0041352.EPIO'
C   SRUN 41352
C   SEVT 41352 9781 14196
C   ENDQ
C
C --------------------------------------------------------------------
#ifndef(DOC)
C
      PARAMETER (LIW=400000, LNAMES=3000)

      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
      COMMON /BCS/   IW(LIW)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C --
      LOGICAL FWRIT, BTEST, CHKCLAS
      INTEGER ALFMT, STATUS, AGETDB, ALSTGQRY, ALTRIG
      CHARACTER RLIST*1,OLIST*1,ULIST*12,FNAME*80,FMT*75,ELIST*1
      CHARACTER MACHI*8, NAME*4, CHAINT*4, MSG*80, NLISTB*4
      CHARACTER*80 STRING,FN*60,FT*6,FD*40
C --
      DATA NAGSUM, NAPRIN, NAWRIT /3*0/
C --
#include "bmacro.h"
C ---------------------------------------------------------------
      CALL TIMEST (1000000.)
      CALL BNAMES(LNAMES)
      CALL BOS(IW,LIW)
      CALL IOVERS (ALVER,ALIOV)
      WRITE(6,*) ' ALEPHLIB # ',ALVER,' ALEPHIO # ',ALIOV
      CALL ALMACH (MACHI)
      WRITE (6,*) ' *** ALMACH *** version ',MACHI
C --
C   Opens the cards file ALEPH CARDS or whatever cards file name
C   which contain input and output (FILI/FILO) data files.
C --
      IW5 = IW(5)
      LCAR = 7
      LWRI = 80
      CALL GETENVF ('ALPHACARDS',FNAME)
      IF (FNAME .NE. ' ') CALL AOPEN(LCAR,FNAME,'CARD','DISK',IER)
      IDUM = MRESET(LCAR)
      CALL BREADC
C
C - if READ data cards open card files on unit LCAR+1
C
    1 CALL AGTFIL ('READ','READ',LCAR+1,IER)
      IF (IER.NE.-1) THEN
        IF(IER.NE.0) THEN
          WRITE (6,*) ' RWALEPH : cannot open extra data card file'
        ELSE
          IDUM = MRESET(LCAR+1)
          CALL BREADC
        ENDIF
        GOTO 1
      ENDIF

C
C - get formats from BANKAL.FMT
      LUFMT = 11
      IRET = ALFMT(LUFMT,'ALL',FMT)
C
C - DEBUg card
      IDB1 = 0
      IDB2 = 0
      JDEBU = IW(NAMIND('DEBU'))
      IF (JDEBU.GT.0) THEN
        IF (IW(JDEBU).GE.1) IDB1 = IW(JDEBU+1)
        IF (IW(JDEBU).GE.2) IDB2 = IW(JDEBU+2)
        IF (IDB2.EQ.0) THEN
          IDB2 = IDB1
        ENDIF
      ENDIF
C
C - open data base
      CALL AOPDBS ('    ',IER)
      IF (IER .NE. 0) THEN
        CALL AWERRC (IW(6),'ALRWEV','data base file ',IER)
      ENDIF
      CALL ADBVER(IVERS,IDATE)
C --
C   Open input and/or output files and write event directory file.
C   ALRWEV can also be used to write an EPIO file from an EDIR or
C   Native file and vice versa.
C --
      IFILE= 0
      ELIST = 'E'
      ULIST = ' '
C
C ==================== open  input/output  unit ==========
    9 IFILE = IFILE+1
      IREC = 0
      ISLOW = 0
      IRUNR = 0
      IEVR = 0
      NRUN = 0
      LTOT = 0
C
C - read a record ==========================================
C
   10 CALL ABRSEL(ELIST,ULIST,IRET)
      IREC = IREC+1
      IF (IFILE.EQ.1 .AND. IREC.EQ.1) CALL EPIOT (6)
      IF (IREC.GE.IDB1 .AND.IREC.LE.IDB2) THEN
        WRITE(6,*) 'FILE=',IFILE,' IREC= ',IREC,' IRET= ',IRET
      ENDIF
      IF (IRET.GE.17) GOTO 10
C                 error in FILI card , cannot open file
      IF (IRET.GE.12 .AND. IRET.LE.14) GOTO 9
      IF (IRET.GE.6) GOTO 9999
C                 if end of file or end of data open next file
      IF(IRET.GE.4) THEN
        WRITE (6,*) ' FILE ',IFILE,' RUN ',NRUN,
     &    ' 1STEVT= ',NFIRST, ' LASTEVT= ',NLAST,
     &    ' IREC= ',IREC,
     &    ' RUNR= ',IRUNR,' EVTR= ',IEVR ,' SLOW= ',ISLOW
        GOTO 9
      ENDIF
C
C

C - set write flag
      FWRIT = .TRUE.
C - set read list and output list
      RLIST = ELIST
      IF (IRET.EQ.2) RLIST='C' 
      OLIST = RLIST
C
C - get the run and event #s.
      CALL ABRUEV (NRUN,NEVT)
C - get number of banks in the record
      NBK = IGTLEN (IW,RLIST)
C - get logical record length
      LENREC=0
      K=0
 111  K = K+1
      NAME = NLISTB(IW,K,RLIST)
      IF (NAME .EQ. ' ') GOTO 112
      KNAME = IW(NAMIND(NAME))
      IF (KNAME .NE. 0) LENREC = LENREC+IW(KNAME)
      GOTO 111
 112  LTOT = LTOT + LENREC
C
C - event record
C
      IF (IRET.EQ.1) THEN
        KCLASW = 0
        IF (.NOT.CHKCLAS(KCLASW)) GO TO 10
        IEVR = IEVR+1
        IF (IEVR.EQ.1) NFIRST = NEVT 
        NLAST = NEVT
        IF (IEVR.GE.IDB1 .AND. IEVR.LE.IDB2) THEN
          WRITE (6,*) ' IEVTR= ',IEVR,' RUN= ',NRUN,' NEVT= ',NEVT
     &                   ,' NBK= ',NBK,' LENREC= ',LENREC
          CALL AUBLIS (RLIST)
          CALL AUBLIS ('S')
          CALL AUBLIS ('T')
        ENDIF
C
C - run record
C
      ELSEIF (IRET.EQ.2) THEN
        IRUNR = IRUNR+1
        WRITE(6,*)'irunr= ',irunr, ' run= ',nrun,' LENREC= ',LENREC
     &            ,' nbk= ',nbk 
        CALL AUBLIS (RLIST)
C
C - slow control record
C
      ELSEIF (IRET.EQ.3) THEN
       ISLOW = ISLOW+1
       IF (ISLOW.GE.IDB1 .AND. ISLOW.LE.IDB2) THEN
        WRITE(6,*)' islow= ',islow,' run= ',nrun,' LENREC= ',LENREC
     &           ,' nbk= ',nbk 
        CALL AUBLIS (RLIST)
       ENDIF
C
      ENDIF
C
C - write the record
C
        IF (FWRIT) THEN
          CALL ABWSEL (OLIST)
        ENDIF
C
C --
        GOTO 10
C --
 9999   CONTINUE
        WRITE (6,*) ' return code= ',IRET
        WRITE (6,*) ' FILE ',IFILE,' RUN ',NRUN,' IREC= ',IREC
     &    ,' RUNR= ',IRUNR,' EVTR= ',IEVR
     &    ,' SLOW= ',ISLOW,' LTOT= ',LTOT
     &    ,' 1STEVT= ',NFIRST, ' LASTEVT= ',NLAST
        CALL ABWEND
        CALL BOSIO
        CALL ACLOSE(0,IER)
C --
        END
#endif

