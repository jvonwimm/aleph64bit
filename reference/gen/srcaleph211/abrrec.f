      SUBROUTINE ABRREC (ELIST, ULIST, IRET)
C-----------------------------------------------------------------------
CKEY ALPHARD READ BOS
C  Author :      H. Albrecht            Nov 89
C  Modified:     E. Blucher             Apr 90
C  Modified:     M. Talby               Jun 90
C  Modified      F.Ranjard              Oct 91
C
C!  Subr. ABRREC : Read next record with or without event directories.
C!  Entry ABCLAS : Set event class bits for output.
C!  Entry ABSTRCL: Set event class bits for input.
C!  Entry ABGTCL : Get class word of current event.
C!  Entry ABGTWCL: Get write class word as it is set.
C!  Entry ABGTRCL: Get read class word as it is set.
C!  Entry ABSTCL : Set full output class word MASKW.
C!  Entry ABWEND : Write last output buffer.
C!  Entry ABUNIT : Return logical unit numbers.
C
C   Routines called from ABRREC which can be supplied by the user :
C   ABSKIP called for every skipped event
C   ABOLDR called for the old run before a new run starts.
C
C   +----------------------------------+
C   | CALL ABRREC (ELIST, ULIST, IRET) |
C   +----------------------------------+
C
C   ABRREC reads from the "select" input stream which is the only
C   one which allows for reading event directories. Several files can
C   be read one after the other but only one file at a time.
C   To read data from different files in parallel : use the routine
C   ABREAD (with the same arguments as the BOS routine BREAD) for the
C   2nd, 3rd, ... file. BREAD is not recommended as it might mix up
C   files with and without event directories.
C
C   Input file names are expected to be given on FILI data cards.
C   The files are opened inside ABRREC.
C
C   For input from event directories, class bits allow for selecting
C   events of specific event classes. The data card CLAS denotes the
C   classes to be accepted. For more complicated selection algorithms,
C   the BOS routine BSELEC can be modified. Unfortunately, this feature
C   is not (yet ?) available for input files without event directories.
C
C   NEVT, SEVT, SRUN, IRUN data cards allow for reading N events or for
C   selecting events according to their run/event number.
C
C   Input : ELIST   List name (character * 1) for input banks.
C                   Recommended : 'E'. Forbidden : ' ', 'C', 'R', 'Z'.
C                   N.b.: For run records, the list 'R' (one run record)
C                         and 'C' (accumulated banks of all run records)
C                         is used.
C           ULIST   Character string to steer unpacking. For a complete
C                   description, see routine AUNPCK. Special values are
C                    'AL '   All banks are unpacked. Coordinates are
C                            not sorted.
C                    ' '     No unpacking.
C                    'NODE'  No decompressing, no unpacking.
C
C  Output: IRET
C
C     1 ... 3 : normal return
C
C         1 Event record. Bank names are stored in ...
C             - list ELIST for banks read from input file.
C             - list 'S' for banks created during unpacking.
C             Both ELIST and 'S' are dropped before a new record
C             is read in.
C         2 Run record. The bank names are stored in list 'C'.
C             Banks on list 'C' are dropped before an new run
C             record is read in.
C         3 Other record. Bank names are stored in list ELIST.
C             The class word of event directories is tested. SEVT etc.
C             cards are ignored.
C
C         4  future application.
C         5  End-of-file condition. The next input file is now
C              opened. Call ABRREC again.
C
C     6 ... 16  : End conditions. User response : Stop the program.
C
C         6 No more input files.
C         7 NEVT limit reached.
C         8 End of selected data according to SEVT/SRUN cards.
C             Precondition : The events on the input file(s) must be
C             sorted according to increasing run/event number. If this
C             is not fulfilled : ignore IRET = 8 and call ABRREC again.
C         9 Time limit reached.
C        10 Input file not properly opened.
C        11 Syntax error in SEVT, SRUN, IRUN, NEVT, TIME, CLAS cards.
C        12 Read error on event directory.
C        13 Error in FILI cards.
C        14 Cannot open input file.
C        15 Error on FILO card (n.b.: the output file, if given, is
C        16 Cannot open output file.                      opened here).
C
C    17 ... 21 : Error conditions. Call ABRREC again.
C
C        17 Read error.
C        18 Error in decompressing.
C        19 Not enough space for unpacking.
C        20 Error in TPC unpacking.
C        21 Error during data base reading.
C
C   +----------------------+
C   | CALL ABCLAS (ICLASS) |
C   +----------------------+
C
C   Modifies class bits for output event directories; ignored if there
C   is none. Note that it does NOT cause any output; an additional CALL
C   ABWSEL is necessary. The order of CALL ABCLAS and CALL ABWSEL does
C   not matter. Several CALL ABCLAS for the same event are allowed.
C
C   CALL ABCLAS (0)   sets all class bits to 0.
C   CALL ABCLAS (N)   with 1 <= N <= 30 : Set class bit N.
C   CALL ABCLAS (-1)  Sets all class bits (1 ... 30).
C   no CALL ABCLAS at all : output class bits = input class bits.
C
C   +---------------------+
C   | CALL ABGTCL (ICLAS) |
C   +---------------------+
C
C   Return current class word in ICLAS
C
C   +---------------------+
C   | CALL ABGWTCL (ICLAS) |
C   +---------------------+
C
C   Return write class word MASKW in ICLAS
C
C   +---------------------+
C   | CALL ABGRTCL (ICLAS) |
C   +---------------------+
C
C   Return write class word MASKR in ICLAS
C
C   +---------------------+
C   | CALL ABSTCL (ICLAS) |
C   +---------------------+
C
C   Set output class word MASKW = ICLAS
C
C
C   +---------------------+
C   | CALL ABSTRCL (ICLAS)|
C   +---------------------+
C
C   Set read class word MASKR = MASKR.OR.IBITC(ICLAS)
C   If ICLAS=0 then MASKR = 0
C   IF ICLAS<0 then MASKR = IBITC(ICLAS)
C
C   +-------------+
C   | CALL ABWEND |
C   +-------------+
C
C   Writes the last record if there is any. Should be called at the
C   end of each job although it is unnecessary in most cases.
C
C   +------------------------------------------------------+
C   | CALL ABUNIT (KUNDAT, KUNSEL, KUNSE2, KUTDAT, KUTSEL) |
C   +------------------------------------------------------+
C
C   Returns the logical unit numbers used inside the ABRSEL package.
C   Must not be called before the first CALL ABRSEL.
C
C   Output : KUNDAT : Input data file.
C            KUNSEL : Input event directory.
C            KUNSE2 : future application.
C            KUTDAT : Output data file.
C            KUTSEL : Output event directory.
C
C-----------------------------------------------------------------------
C   Calls :
C   From BOS77 : BCLASR, BCLAST, BCLASW, BDROP, BGARB, BLIST, BREAD,
C                BSELEC, BWRITE, NDROP.
C   From ALEPHLIB : ABREAD, ABCHCK, ALOPEN, AUNPCK, CMPINI, CMPLIS,
C                   DMPLIS, CRFILM.
C   Utilities : TIMAL.
C-----------------------------------------------------------------------
      SAVE TLIMIT, IROLD, NAPTS
      LOGICAL CINIT
      CHARACTER * (*) ELIST, ULIST
      CHARACTER *80  ERLIST
C
*MACRO BOSCOM
C
      COMMON /BCS/IW(1000)
      COMMON /SYSBOS/NSYST,NAMES,NPRIM,IDNAM,IDPTR,
     1               IDFMT,NDUMM,NRESR,NLPLM, NARR,
     2               IARR(10),
     3               IEFMT,TLEFT,
     4               LEPIO,NAMI,INDI,INDJ,IBC,DUMMI(73),
     5               INTA(200), NPTR,NRUN,NEVT,
     6               LUNDAT,LUNSEL,LUNSE2,LUTDAT,MASKR,LMASK,
     7               NRE,NAMERE(3),NUMMRE(3),IRUNRE(3),IEVTRE(3)
C
      COMMON /ABRCOM/ BATCH,INIT,CLOSE1,CLOSE2,FWFILM
     &               ,IUNDAT(2),IUTDAT(5),IUNSEL,IUNSE2,IUTSEL
     &               ,MASKA,MASKW
     &               ,WLIST,TLIST
      LOGICAL BATCH, INIT, CLOSE1, CLOSE2, FWFILM
      CHARACTER*1   TLIST, WLIST
C
      INTEGER IBITC(30)
      DATA    IBITC/1,2,4,8,16,32,64,128,256,512,1024,2048,4096,
     +        8192,16384,32768,65536,131072,262144,524288,1048576,
     +        2097152,4194304,8388608,16777216,33554432,67108864,
     +        134217728,268435456,536870912/
      DATA  CINIT /.TRUE./
C -----------------------------------------------------------------
C
C - 1st entry : initialize COMPRESS package once for ever
C
      IF (CINIT)  THEN
         CINIT  = .FALSE.
         TLIMIT = 2.
         WLIST  = ' '
         IROLD  = 0
         I = NLINK ('COMP', 0)
         IF (I .EQ. 0)  THEN
            I = NBANK ('COMP', 0, 1)
            IW(I+1) = INTCHA ('INTE')
         ENDIF
         CALL CMPINI (' ', I)
C        the return code from CMPINI is ignored.
      ENDIF
C
C - initialize read/write package
C
      IF (INIT)  THEN
         INIT   = .FALSE.
         MASKA = 0
         DO 1 I=1,30
    1    MASKA = IOR (MASKA, IBITC(I))
         NAPTS = NAMIND ('$PTS')
         IND = IW(NAMIND ('TIME'))
         IF(IND .NE. 0)  TLIMIT = FLOAT (IW(IND+1))
         WLIST = ' '
      ENDIF
C
C - next entry
C
      IF (WLIST .NE. ' ')  CALL ABWEVE
C
C       drop
C
   10 TLIST = ELIST
      CALL BDROP (IW, TLIST)
      CALL BLIST (IW, TLIST//'=', '0')
      CALL BDROP (IW, 'S')
      CALL BLIST (IW, 'S=', '0')
      CALL BGARB (IW)
C
      IF (IUNDAT(1) .EQ. 0)  THEN
C       Input file not properly opened :
         IRET = 10
         GO TO 900
      ENDIF
C
      IF (IUNSEL .NE. 0 .OR. IUTDAT(1) .NE. 0)  THEN
C
C       get event directory bank and reset output mask
C
         JPTS = 0
         IPT = IW(NAPTS)
  110    IF (IPT .NE. 0)  THEN
            IF (IW(IPT-2) .EQ. IUNSEL)  THEN
               JPTS = IPT
            ELSE
               IF (IW(IPT-2) .NE. 0 .AND. IUTDAT(1) .NE. 0)
     +            IW(IPT+IW(IPT)-1) = 0
            END IF
            IPT = IW(IPT-1)
            GO TO 110
         ENDIF
      ENDIF
C
      IF (IUNSEL .EQ. 0)  THEN         ! =========================
C
C       sequential read, NO event directory :
C
C       check time
        CALL TIMAL (TLEFT)
        IF (TLEFT .LT. TLIMIT)  GOTO 809
C
C       read header of next record
 130    CONTINUE
        CALL BGARB (IW)
        CALL ABREAD (IW, IUNDAT(1), ' ', *817, *818)
        IPTR = IW(2)
        MASKW = 0
C
C         event record, run record, other record ?
        IF (IW(IW(3)-3) .EQ. NAMERE(1))  THEN
C         event record
           IRET = 1
        ELSE IF (IW(IW(3)-3) .EQ. NAMERE(2) .OR.
     +         IW(IW(3)-3) .EQ. NAMERE(3))  THEN
           IRET = 2
           NEVT = 0
        ELSE
C         other record: read it
           IRET = 3
           NRUN = 0
           NEVT = 0
           GO TO 140
        ENDIF
C
C       event selection (NEVT, SEVT, SRUN, IRUN) :
        CALL ABSEVT (NRUN, NEVT, IFLG)
        IF (IFLG .LE. 0)  GOTO 130
        IF (IFLG .GE. 4)  THEN
C        end of selected data
           IRET = IFLG
           GO TO 900
        ENDIF
        IF (IRET.EQ.2) THEN
C         run record : drop list C = previous run record
           TLIST = 'R'
           IF (IROLD .NE. NRUN)  THEN
             IF (IROLD .NE. 0)  CALL ABOLDR (IROLD)
             IROLD = NRUN
             CALL BDROP (IW, 'C')
             CALL BLIST (IW, 'C=', '0')
           ENDIF
        ENDIF
C
C       read rest of the record and go to decompress:
  140   IRUN = NRUN
        IEVT = NEVT
        CALL ABREAD (IW, IUNDAT(1), TLIST, *817, *818)
        NPTR = IPTR
        NRUN = IRUN
        NEVT = IEVT
C
      ELSE         ! =============================================
C
C       event directories :
C
C       check time
  200   CONTINUE
        CALL TIMAL (TLEFT)
        IF (TLEFT .LT. TLIMIT)  GOTO 809
C
        CALL AREDIR (IEVT,IRUN,NREC,IRET)
        IF (IRET.GT.3) GOTO 900
        NRUN = IRUN
        NEVT = IEVT
C
C         run record : drop 'C'list (previous run record
        IF (IRET.EQ.2) THEN
           IF (IROLD .NE. NRUN)  THEN
              IF (IROLD .NE. 0)  CALL ABOLDR (IROLD)
              IROLD = NRUN
              CALL BDROP (IW, 'C')
              CALL BLIST (IW, 'C=', '0')
           ENDIF
           TLIST = 'R'
        ENDIF
C
C      this entry has been selected : read rest of record
        IW(1) = NREC
        CALL BREAD (IW, IUNDAT, TLIST, *817, *818)
C       why does BREAD set NRUN, NEVT = 0 ???
        NRUN = IRUN
        NEVT = IEVT
        NPTR = IW(2)
C
      ENDIF           ! =====================================
C
C       Run header banks into list 'C'
C       remember TLIST='R' and  IRET=2  for run record
C       Decompress compressed banks
C
  300 IF (ULIST .EQ. 'NODE') THEN
        IF (IRET .EQ. 2)  CALL BLIST (IW, 'C+', 'R')
        GOTO  900
      ENDIF
      CALL DMPLIS (TLIST, ERLIST, IFLG)
      IF(IRET.EQ.2)  CALL BLIST (IW, 'C+', 'R')
      IF (IFLG .NE. 0) CALL UDMPER (IRET,TLIST,ERLIST,IFLG)
C
C       now unpack the event banks if required
C       even if there was a decompression error
C
  350 CONTINUE
      IF (IRET .NE. 1 .OR. ULIST .EQ. ' ')  GO TO 900
      CALL AUNPCK (ELIST, ULIST, IFLG)
      IF (IFLG .LE. 1)  GO TO 900
      IF (IFLG .EQ. 2)  THEN
C       Not enough space for unpacking :
         IRET = 19
      ELSEIF (IFLG .LE. 90)  THEN
C       Error in TPC unpacking :
         IRET = 20
      ELSE
C       Error during data base reading :
         IRET = 21
      ENDIF
      GOTO 900
C
C-----------------------------------------------------------------------
C
      ENTRY ABCLAS (ICLASS)
C
      IF (ICLASS .GT. 0)  THEN
        IF (ICLASS .LE. 30)  MASKW = IOR (MASKW, IBITC(ICLASS))
      ELSE IF (ICLASS .EQ. 0)  THEN
        MASKW = 0
      ELSE
        MASKW = MASKA
      ENDIF
      GO TO 900
C
C-----------------------------------------------------------------------
C
      ENTRY ABSTRCL (ICLASS)
C
      IF (IABS(ICLASS).GT.30) GOTO 900
      IF (ICLASS.LE.0) MASKR = 0
      CALL BCLASR (IABS(ICLASS))
      GO TO 900
C
C-----------------------------------------------------------------------
C
      ENTRY ABGTCL (ICLAS)
C
      ICLAS = LMASK
      GOTO 900
C
C-----------------------------------------------------------------------
C
      ENTRY ABGTWCL (ICLAS)
C
      ICLAS = MASKW
      GOTO 900
C
C--------------------------------------------------------------------
C
      ENTRY ABGTRCL (ICLAS)
C
      ICLAS = MASKR
      GOTO 900
C
C--------------------------------------------------------------------
C
      ENTRY ABSTCL (ICLAS)
C
      MASKW = ICLAS
      GOTO 900
C
C--------------------------------------------------------------------
C
      ENTRY ABUNIT (KUNDAT, KUNSEL, KUNSE2, KUTDAT, KUTSEL)
C
      KUNDAT = IUNDAT(1)
      KUNSEL = IUNSEL
      KUNSE2 = IUNSE2
      KUTDAT = IUTDAT(1)
      KUTSEL = IUTSEL
      GO TO 900
C
C-----------------------------------------------------------------------
C
C        end conditions, error codes etc.
C
C       Time limit :
  809 IRET = 9
      CALL ABWEND
      GOTO 900
C       Read error on data file:
  817 IRET = 17
      GOTO 900
C       EOF on data file
  818 IRET = 5
      GOTO 900
C
  900 RETURN
      END
