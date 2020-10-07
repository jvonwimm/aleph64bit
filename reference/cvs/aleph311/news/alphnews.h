C! ALPHARD package used by ALEPH programs to read and write data files

  +++  BECAREFUL it makes use of /BOSCOM/ and /BITCONST/ BOS common +++

 ! ALEPHLIB 202
   ABSEVT - correct a bug with SEVT and SRUN cards.

 ! ALEPHLIB 154
    remove ENTRY ABWSEL, introduce SUBROUTINE ABWSEL to please IBM/VM

 ! ALEPHLIB
    Subr. ABRREC : Read next record with or without event directories.
    Entry ABCLAS : Set event class bits for output, 1 bit at a time.
    Entry ABGTCL : Get event class word just read from input.
    Entry ABGTWCL: Get output event class word as it is set so far.
    Entry ABSTCL : Set event class word for output, full word at a time.
    Subr. ABWEND : Write last output buffer.
    Subr. ABWEVE : Write output list.
    Subr. ABWSEL : Write record with or without event directories.
    Entry ABRUEV : Return run / event number.
    Entry ABUNIT : Return logical unit numbers.

    Routines called from ABRREC which can be supplied by the user :
    ABSKIP called for every skipped event
    ABOLDR called for the old run before a new run starts.

    +----------------------------------+
    | CALL ABRREC (ELIST, ULIST, IRET) |
    +----------------------------------+

    ABRREC reads from the "select" input stream which is the only
    one which allows for reading event directories. Several files can
    be read one after the other but only one file at a time.
    To read data from different files in parallel : use the routine
    ABREAD (with the same arguments as the BOS routine BREAD) for the
    2nd, 3rd, ... file. BREAD is not recommended as it might mix up
    files with and without event directories.

    Input file names are expected to be given on FILI data cards.
    The files are opened inside ABRREC.

    For input from event directories, class bits allow for selecting
    events of specific event classes. The data card CLAS denotes the
    classes to be accepted. For more complicated selection algorithms,
    the BOS routine BSELEC can be modified. Unfortunately, this feature
    is not (yet ?) available for input files without event directories.

    NEVT, SEVT, SRUN, IRUN data cards allow for reading N events or for
    selecting events according to their run/event number.

    Input : ELIST   List name (character * 1) for input banks.
                    Recommended : 'E'. Forbidden : ' ', 'C', 'R', 'Z'.
                    N.b.: For run records, the list 'R' (one run record)
                          and 'C' (accumulated banks of all run records)
                          is used.
            ULIST   Character string to steer unpacking. For a complete
                    description, see routine AUNPCK. Special values are
                     'AL '   All banks are unpacked. Coordinates are
                             not sorted.
                     ' '     No unpacking.
                     'NODE'  No decompressing, no unpacking.

   Output: IRET

      1 ... 3 : normal return

          1 Event record. Bank names are stored in ...
              - list ELIST for banks read from input file.
              - list 'S' for banks created during unpacking.
              Both ELIST and 'S' are dropped before a new record
              is read in.
          2 Run record. The bank names are stored in list 'C'.
              Banks on list 'C' are dropped before an new run
              record is read in.
          3 Other record. Bank names are stored in list ELIST.
              The class word of event directories is tested. SEVT etc.
              cards are ignored.

          4  future application.
          5  End-of-file condition. The next input file is now
               opened. Call ABRREC again.

      6 ... 16  : End conditions. User response : Stop the program.

          6 No more input files.
          7 NEVT limit reached.
          8 End of selected data according to SEVT/SRUN cards.
              Precondition : The events on the input file(s) must be
              sorted according to increasing run/event number. If this
              is not fulfilled : ignore IRET = 8 and call ABRREC again.
          9 Time limit reached.
         10 Input file not properly opened.
         11 Syntax error in SEVT, SRUN, IRUN, NEVT, TIME, CLAS cards.
         12 Read error on event directory.
         13 Error in FILI cards.
         14 Cannot open input file.
         15 Error on FILO card (n.b.: the output file, if given, is
         16 Cannot open output file.                      opened here).

     17 ... 21 : Error conditions. Call ABRREC again.

         17 Read error.
         18 Error in decompressing.
         19 Not enough space for unpacking.
         20 Error in TPC unpacking.
         21 Error during data base reading.

    +---------------------+
    | CALL ABWSEL (ELIST) |
    +---------------------+

    ABWSEL sets an output flag. Actual output is done during the next
    call to ABRREC. Thus, there is no danger to call it more than once
    for one event. For the very last event, ABWEND (see below) has
    to be called.

    The "select" output stream used in ABWSEL is again the only one
    which allows for writing event directories. For parallel output
    streams, the standard BOS routine BWRITE has to be used.

    Input file names are expected to be given on FILO data cards.
    The files are opened inside ABWSEL.

    By default, the class bits from an input event directory are
    copied to the output event directory. Note that all class bits are
    0 if there is no input event directory, and that class bits cannot
    be written to files without event directories. ABCLAS (see below)
    allows to clear all bits, to set specific bits, or to set all bits.

    Input : ELIST   List name (character * 1) for output banks.
                    Recommended : 'E' , 'R', 'C'. Forbidden : ' ', 'Z'.
            This parameter is ignored if the output is an event
            directory which refers to an input file.

    +----------------------+
    | CALL ABCLAS (ICLASS) |
    +----------------------+

    Modifies class bits for output event directories; ignored if there
    is none. Note that it does NOT cause any output; an additional CALL
    ABWSEL is necessary. The order of CALL ABCLAS and CALL ABWSEL does
    not matter. Several CALL ABCLAS for the same event are allowed.

    CALL ABCLAS (0)   sets all class bits to 0.
    CALL ABCLAS (N)   with 1 <= N <= 30 : Set class bit N.
    CALL ABCLAS (-1)  Sets all class bits (1 ... 30).
    no CALL ABCLAS at all : output class bits = input class bits.

C   +---------------------+
C   | CALL ABGWTCL (ICLAS) |
C   +---------------------+
C
C   Return write class word MASKW in ICLAS
C
C   +---------------------+
C   | CALL ABSTCL (ICLAS) |
C   +---------------------+
C
C   Set output class word MASKW = ICLAS
C
    +-------------+
    | CALL ABWEND |
    +-------------+

    Writes the last record if there is any. Should be called at the
    end of each job although it is unnecessary in most cases.

    +--------------------------+
    | CALL ABRUEV (IRUN, IEVT) |
    +--------------------------+

    Returns current run and event number.
    For event records : IRUN > 0 ; IEVT > 0.
    For run records :   IRUN > 0 ; IEVT = 0.
    For other records : IRUN = 0 ; IEVT = 0.

    +------------------------------------------------------+
    | CALL ABUNIT (KUNDAT, KUNSEL, KUNSE2, KUTDAT, KUTSEL) |
    +------------------------------------------------------+

    Returns the logical unit numbers used inside the ABRSEL package.
    Must not be called before the first CALL ABRSEL.

    Output : KUNDAT : Input data file.
             KUNSEL : Input event directory.
             KUNSE2 : future application.
             KUTDAT : Output data file.
             KUTSEL : Output event directory.

  example
  =======

      General purpose pgm to read/ write .NATIVE, .EPIO, .EDIR files.

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
 10   CALL ABRSEL('R',ULIST,IRET)
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
      NBK = IGTLEN (IW,'R')
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

