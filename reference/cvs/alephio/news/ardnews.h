C! basic routines to open files

 ! ALEPHIO 5.2
   ALSTGQRY - on UNIX look also on ALWS to find whether a file is staged.
              BECAREFUL this FUNCTION works at CERN, outside it should be
              customized to use other stager if necessary.
              For the moment this FUNCTION is called by LOOK on request,
              and by DALI when a FILM card contains a CART parameter
              to request the staging of a file.

 ! ALEPHIO 5.1
   AWRTAP   - change name of script 'alstagewrt' to 'alstageout'
   ALSTGQRY - remove options if any

 ! ALEPHIO 5
   ASTAGE, AWRTAP - to adapt to new stageout script
   ALSTGQRY - integer function to make a stagequery on VAX or UNIX
              IER = ALSTGQRY (fname)   i.e. - fname = ab1234.1.sl
              IER =0 if file is NOT staged, =1 if it is staged

 ! ALEPHIO 2
   AOPTAP - modify the test to distinguish between AB1234.1.SL and AB1234.11.SL
            AB1234.1.SL is kept as ab1234.epio
            AB1234.2.SL is kept as ab1234.2.epio
            AB1234.11.SL is kept as ab1234.11.epio

 ! ALEPHLIB 15.8
   ASTAGE, AWRTAP - introduce stageout on VAX machines (U.Schaefer)

 ! ALEPHLIB 15.7
   AOPERD, AOPTAP - on VAX and AXP declare SMG$blabla, LIB$..., SYS$..
                    INTEGER when they are used as function
                    (mandatory on AXP when running in debug mode)
   AOPATP - on VAX and AXP add an OPEN statment for NATIVE staged files.
            test existence of ALEPH$STAGE instead of ALWS.
   ACLOSE - when using EPIOc (status word 32 = 2) the EPIO file has to
            be closed before another one is opened to reduce the
            number of opened files (RFIO supports a small number<200
            of opened files)
   AOPTAP - on VAX open the staged NATIVE file in READONLY mode.
   AWRTAP - Environmental variable ALSTOUT has to be defined on UNIX
            systems!
            ALSTOUT defines the disk pool for the files to be staged out
            May be different from the disk pool for stagein data.
            sfget creates a symbolic link in $ALSTAGE to the file
            in $ALSTOUT.

 ! ALEPHLIB 15.7
   ADSPOS - extend CHNAM to 80 characters

 ! ALEPHLIB 15.6
   ACLOSE - when using EPIOC (status word 33 = 2) the EPIO file has to
            be rewinded before another one can be opened on the same
            unit.

 ! ALEPHLIB 15.5
   ACARD2 - rewrite the routine to suppress ASSIGN statments which give
            problems on ALPHA/OSF1 machines.

 ! ALEPHLIB 15.4
   REMEMBER for the moment the use of EPIO/CFIO is yanked until the release
            of the cernlib 93d.
   AOPERD - on ALWS as on UNIX when reading an IBM FILM card for a standard
            EPIO file (a GIME is present) transform it for a standard
            cartdridge:
            i.e. - FILM 'AB1234.EPIO | GIME PUBXU 456'
                   becomes
                   FILM 'ALDATA | EPIO | CART AB1234.1.SL'

   AOPTAP  : remove restriction to 'CART' or 'TAPE' on VAX to allow
             stagein of Robot cartridges through SHIFT.

 ! ALEPHLIB 15.3
   ALGTENV : add BEAMPOSITION new environment variable to access
             phy:beam.position file.
   AOPEN,AOPENW,AOPTAP,AWRTAP : set EPIO status word 33 to 2 to use CFIO
                                and RFIO package where it is avalaible,
   AOPTAP  : add access to cartridges on ALWS through SHIFT6
   AOPERD  : on ALWS open staged file on SHIFT6

 ! ALEPHLIB 15.1
   AOPTAP, AWRTAP, AOPERD - let TMS find out where a cartridge resides.
   AOPTAP - On IBM , if SIZE is not given then force SIZE 200
   AWRTAP - On IBM , if no option force CHOPT=' '

 ! ALEPHLIB 14.6
   ADSPOS  - add a SAVE statment
   ALGTENV - on VAX check that EDIR or EPIO files starting with A or I
             have only 6 characters before prefixing them with
             AL$EDIR or AL$DATA
             on UNIX call GETENVF instead of GETENV to please APOLLO
   ABOPEN, ALOPEN, AOPEN, AOPENW, AOPERD, AOPEWR, AOPTAP, AWRTAP
           - reoganize error codes
   --------------------------------------------------------------------

        ***           ALREAD   Package       ****


     Following is a description of a set of subroutines

                to OPEN, READ and CLOSE
                =======================

           all  ALEPH BOS-files on various machines
           =========================================


 In particular event selection and file opening  with internally defined
 names is possible. Unpacking of POT/DST banks is done automatically.

 The programs run on VAX and IBM. NATIVE, EPIO, DAF, CARDS and EDIR file
 can be used by the same subroutines. The programs are stored on the
 ALEPHLIB.

 The data base has to be opened by the user program beforehand.

   AOPDBS opens the database ADBSCONS DAF on unit# 4 .
   The unit is defined internally and it is used by all suboutines or
   functions accessing the database. The data base unit# can be
   accessed by a call to JUNIDB:
             LDBASE = JUNIDB(0)

 the user interface to the ALEPH I/O package is ABRSEL

      CALL ABRSEL (Elist, Ulist,IER)

 for a full description look at *CD ALPHNEWS


 Below are described the routines called by ABRSEL to open, close, read
 files.

   File Opening:      AOPEN,AOPENW
   _____________

   AOPEN,AOPENW are basic opening routines. For event directories
   one has to open the masterfile separately.

   EPIO files are opened with fixed record format and 32040 bytes.
   NATIVE files are opened with fixed record format and 32040 bytes
   (30720 bytes on IBMRS6000).
   BOS_DAF files are opened with fixed record format and 1024 words.
   CARDS files are opened with 72 bytes.
   EDIR files are opened with 80 bytes.

   the record length for data files is set internally and can be
   accessed by a call to JALREC:
        LRECL = JALREC(0)
   the number of bytes per word is set in NBYTW (*CD ALBITW)
   the NATIve data file is declared to BOS:
        CALL BUNIT (LUN,'FORT',LRECL/NBYTW)
   the EPIO data file is declared to BOS:
        CALL BUNIT (LUN,'EPIO',LRECL/2)


   Input of filenames:   AFILIN,AFILOU
   ___________________

   Filenames are made of 'fname ftype fmode' on IBM
                         'fname.ext'         on VAX
   Filetypes are made of 'ftype'             on IBM
                         'ext'               on VAX

   Filenames can be given on runcards. For input use runcard:
                    FILI 'filename'
   and for output:
                    FILO 'filename'
   where filename is the full name including type or version.
   If the filetype is not one of the standard ALEPH filetypes
   the filename has to be extended with |NATIVE , |EPIO ,
   |CARDS , |DAF or |EDIR e.g. FILI 'ABC.DAT | EPIO'
   More than one FILInput or FILOutput card can be used. The files
   are treated in the order of the cards.


   Special IBM:
     To access the proper minidisk one can add the GIME command
     at the end of the filename string.
     E.g.: FILI 'fn ft * | GIME PUBXU 401 ' or
           FILO 'fn ft fm  | NATIVE | GIME PUBXU 403 fm (MR'
           FILO 'fn ft fm  | GIME PUBXU 403 fm (MR | NATIVE'

   Summary of FILI/FILO/READ card format:
      FILI 'fn.ft'
      FILI 'fn.ft | EDIR | GIME PUBXU 209'
      READ 'my.CARD | FETCH FLR 191'
      on UNIX machines:  userid has to be given when it is
      different from UNIX userid
      fetch file alws::sys$login:my.fili for user ranjard
      READ 'my.fili | CARD | FETCH ALWS RANJARD'
      fetch file al1w10::disk$aleph:[schlatter.a]my.fili
      for user ranjard
      READ 'my.fili | FETCH AL1W10::DISK$ALEPH:[SCHLATTER.A] RANJARD'

      dispose output file to the cernvm:reader or to the remoteuser
      main directory
      FILO 'MY.EDIR | DISPOSE'

      data files from cartdriges starting by "A" or "I" can be accessed
      in the same way on all machines:
      FILI 'AB1234 | EDIR'
      FILI ' I12345 | EPIO'

      cartdriges can be stagein :
      FILI 'ALDATA | EPIO | CART AB1234.1.SL SIZE 200'
      FILI 'MYLABEL | EPIO | SMCF I12345.n.SL SIZE 50'

      cartdriges can be stageout:
      FILI 'ALDATA | EPIO | CART AC1234.1.SL'
      FILI 'MYLABEL| EPIO | SMCF I12345.n.SL'

  Summary of Subroutine calls:
  ____________________________

    for general user:
    =================


  LUN=JUNIAL(CIO,I,0)     return logical unit for:
                           CIO='INPUT'  I=1  for DATA
                                        I=2  1. select file
                                        I=3  2. select file
                                        I=4  2. parallel DATA  file

                           CIO='OUTPUT' I=1  for DATA
                                        I=2   select file
                                        I=3  2. parallel DATA  file
                                        I=5  3. parallel DATA  file

  LUN=NUNIAL(CIO,I,NEWVAL) set logical units to NEWVAL

  CALL AFILIN(FNAME, ATYPE, FDEVI, IER)
                          get input  filename, type and device from
                          runcard FILI
  CALL AFILOU(FNAME, ATYPE, FDEVI, IER)
                          get output filename, type and device from
                          runcard FILO
  CALL AOPDBS(FNAME, IER) open data base on unit# 4. Use database
                         as defined by runcard FDBA, or argument
                         or by default.
  CALL AOPEN (LUN, FNAME, ATYPE, FDEVI, IER )
                         open old file  (READONLY)
  CALL AOPENW(LUN, FNAME, ATYPE, FDEVI, IER )
                         open new file  (write)

    for experts:
    ============

  CALL ACDARG(CNAME,DTYPE,DMODE, FNAME,ATYPE,FDEVI,IER)
                         get arguments FNAME/ATYPE/FDEVI
                         of runcard CNAME/DTYPE/DMODE
  KEY = ACARD3(NPRM,CKEY,COUT)
                         look for position of key CKEY
                         in the card just read by ACDARG
                         starting at parameter# NPRM
                         return the field COUT conter in CKEY
  CALL ACLOSE(LUN,IER)   close unit LUN.
  CALL AEVNUM(NRUN,NEVT) return run + event number.
  CALL AFILMI(FNAME, FTYPE, FDEVI, IRET )
                         get master filename for EDIR from bank FILM
  CALL AFTYPE(CAFT,NFT)
                         return strings of alephtypes
  CALL AIOCAR(CARINP, NINP, CAROUT, NOUT )
                         get valid card names for input/output
  CALL ALOPEN( LINDAT, LINSEL, LINSE2, LUTDAT, LUTSEL, IER)
                         open input and output files
  CALL AOPERD( LINDAT, LINSEL, LINSE2, IER )
                         open files ( read only ) including EDIR's
  CALL AOPEWR( LINDAT, LUTSEL, LUTDAT, IER )
                         open files ( write only ) including EDIR's
  CALL AUNPCK(LIST,ULIST,IER)
                         unpack POT/DST banks.

    User Subroutine:

    SUBROUTINE ANEWRN( IRUN )
                         is called at the first event- record
                          with a new run number.
    SUBROUTINE AUNPUS(LIST, ULIST)
                         is called by AUNPCK


------------------------------------------------------------------------

Examples:
=========

  1)  General purpose pgm to read/ write .NATIVE, .EPIO, .EDIR files.

C
      COMMON/BCS/IW(400000)
      CHARACTER*6 TYPDAT
      CHARACTER*80 FDEV,ULIST/'   '/

      CALL BNAMES(400)
      CALL BOS(IW,400000)
      CALL AOPEN(5,'TRW.CARDS',' ',' ',IER)
      CALL BREADC
C
C - initialize new input file
 9    IREC = 0
C
C - open input/output unit(s) and read a record
C
 10   CALL ABRSEL('E',ULIST,IRET)
      IF (IREC.EQ.0) THEN
C      get logical unit #s.
         CALL ABUNIT(LINDAT,LINSEL,LINSE2 , LUTDAT,LUTSEL)
C      IRET>5 means end of job, no more input file
         IF(IRET.GT.5) GOTO 9999
      ENDIF
C
      IREC=IREC+1
C                 if end of file or end of data open next file
      IF(IRET.GE.4) GOTO 9
C
C                get run and event #s.
      CALL ABRUEV (NRUN,NEVT)
C                get number of banks in R-list
      NBK = IGTLEN (IW,'E')
      KREVH = IW (NREVH)
      KCLASW = 0
C
      IF (IRET.EQ.1) THEN
C - event record
         IF(KREVH.GT.0) THEN
C          get write class word from REVH bank
             KCLASW = IW(KREVH+LMHLEN+JREVEC)
C         set write class word
             CALL ABSTCL (KCLASW)
          ELSE
C          build the EDIR  class word
             CALL ALCLASW (KCLASW)
          ENDIF
C
      ELSEIF (IRET.EQ.2) THEN
C - run record
         ICLAS  = -1
C      set all bits in output class word
         CALL ABCLAS (ICLAS )
C
      ELSEIF (IRET.EQ.3) THEN
C - slow control record
         ICLAS = 25
C      set bit#25 in output class word
         CALL ABCLAS (ICLAS)
      ENDIF
C
C            write the record
C
C       set the write flag for this record
          CALL ABWSEL ('R')
          IF (LUTSEL.NE.0) THEN
C           an EDIR is written , get output class word of the record
             CALL ABGTWCL (KCLASW)
          ELSE
C           an EDIR is not written, get input class word of the record
             CALL ABGTCL (KCLASW)
          ENDIF
C       fill class word statistic
          CALL ALSUMCL (KCLASW)
C
C - read next record
      GOTO 10
C --
 9999 CONTINUE
C - write last record
      CALL ABWEND
C - print statistic of class word
      CALL ALSUMCL (-1)
C --
      END

     Use the following runcards from file  BCOPY.CARDS:
     To copy from NATIVE to EPIO:

       FILI  'ALEPHDATA:DST123.NATIVE'
       FILO  'SCWEEK:DST123.EPIO'

     or from EPIO to NATIVE:

       FILO  'DST123.NATIVE'
       FILI  'DST123.EPIO'


     To read data and create an event directory:

       FILI  'ALEPHDATA:DST123.NATIVE'
       FILO  'SCWEEK:DST123.EDIR'

     To read selected events a with an event directory and write them.

       SEVT  1989  5 11 187 689
       FILI  'DST123.EDIR'
       FILO  'HIGGS.NATIVE'

 -----------------------------------------------------------------------

   2)  Open data file and  select events randomly ( very time consuming
       und unpack all banks ( IBM version):

          PARAMETER (LBOS=400000)
          COMMON/BCS/IW(LBOS)
C
          CALL BNAMES(400)
          CALL BOS(IW,LBOS)

          LUN1=1
C            open data-file
          CALL AOPEN(LUN1,'ALEPHDATA:MCLUND.EPIO','  ','DISK',IER)
C              open database for TPC coordinate unpacking
          CALL AOPDBS(' ',IER)


     1    CONTINUE
          NRUN=1
          NEVT=100*RNDM(XX)+1

          CALL ASEVT(LUN1,'E','AL ',NRUN,NEVT,IER)
C                EOF   ?
          IF(IER.NE.0 ) GOTO 999
C            .
C            .
           GOTO 1
999       CONTINUE
          END

    --------------------------------------------------------------------

   3)  Read all events from several files. Filenames  are given
       with   runcards. Unpack TPC and ITC banks only. Initialize
       Run conditions at every new run:

          PARAMETER (LBOS=400000)
          COMMON/BCS/ IW(LBOS)
          CHARACTER*80 FNAME
          CHARACTER*4  ATYPE,FDEVI
          CHARACTER*8 ULIST/'TP IT FI'/

          CALL BNAMES(400)
          CALL BOS(IW,LBOS)
C             open and read runcard
          CALL AOPEN(5,'TEST DAT A','CARDS','DISK',IER)
          CALL BREADC
C             open database  ADBSCONS DAF
          CALL AOPDBS(' ',IER)
C
          LUN1=1
          NFIL=0
1         NFIL=NFIL+1
C                get next filename from runcards
            CALL AFILIN(FNAME,ATYPE,FDEVI,IRET)
            IF(IRET.LT.0) GOTO 999

            CALL AOPEN(LUN1,FNAME,ATYPE,FDEVI,IER)
            IF(IER.NE.0) GOTO 999

  2       CONTINUE
            CALL ASEVT(LUN1,'E',ULIST,0,0,IER)
            IF(IER.LT.0) GOTO 1
C            .
C            .
            GOTO 2
999       CONTINUE
          END
          SUBROUTINE ANEWRN( IRUN )
          INCLUDE 'RCURNT INC *'
          IRUNRC=IRUN
C               get run constants ( e.g. BFIELD )
          CALL RDCONS
          RETURN
          END

       With the following runcards on file TEST DAT,
       3 files ( IBM file names )  are read in sequence:

        FILI  'RUN22 NATIVE W'
        FILI  'RUN11 NATIVE *'
        FILI  'RUN33 DAT V | NATIVE'


 -----------------------------------------------------------------------

