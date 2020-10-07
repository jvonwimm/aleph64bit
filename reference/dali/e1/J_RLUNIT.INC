C! LOGICAL-UNIT-NUMBERS
      INTEGER JULINP,JULOUT,NWRTMX,NATOUT,EPIOUT,NOOUTP,JULMON
      PARAMETER (JULINP=20,JULOUT=50,NWRTMX=3,NATOUT=1,EPIOUT=2,
     +  NOOUTP=3,JULMON=18)
      INTEGER MAXFPO
      PARAMETER (MAXFPO=150)
      COMMON /RLUNIT/LINPRL,LOUTRL,LDEBRL,LCOMRL,LPOTRL,LHSTRL,
     +               LRCONS,LRGEOM,LUNIRL(NWRTMX),
     +               JOUTRL(NWRTMX),ANYORL,LMONRL,LSUMRL,LPAS0L
      INTEGER LINPRL,LOUTRL,LDEBRL,LCOMRL,LPOTRL,LHSTRL,LRCONS,
     +               LRGEOM,LUNIRL,JOUTRL,LMONRL,LSUMRL,LPAS0L
      LOGICAL ANYORL
      COMMON /RLUNIC/OUTLIS,FORMRL,FTYPRL,OTYPRL,SUMLIS,PASLIS
C     the character length in the following statement must be
C            4*MAXFPO (parameter defined above)
C               vvv
      CHARACTER*600 OUTLIS(NWRTMX),TEMLIS
      CHARACTER*100 SUMLIS,PASLIS
      CHARACTER*4 FORMRL(3),FTYPRL(3),OTYPRL(NWRTMX)
c#if defined(DOC)
C LINPRL  = raw data input unit
C LOUTRL  = standard print unit for output
C LDEBRL  = debug print unit
C LCOMRL  = BOS command bank input
C LPOTRL  = POT output unit
C LHSTRL  = Histograms storage unit
C LRCONS  = Conditions DAF input unit
C LRGEOM  = Detector geometry DAF input unit
C LUNIRL  = units of actual output streams
C JOUTRL  = Output format-- 1=NATIVE 2=EPIO 3=NO output
C ANYORL  = Flag = true if any output to be written
C LMONRL  = Output unit for monitoring file (VAX only)
C LSUMRL  = Output unit for SUMmaRy file (VAX only)
C LPAS0L  = Output unit for PAS0    file (VAX only)
C
C JULMON  = Logical unit for monitoring file
C JULINP  = Logical unit for input raw data
C JULOUT  = Logical unit for first output stream
C NWRTMX  = Max. number of different output streams
C OUTLIS  = String of banks for the output streams
C TEMLIS  = String of banks on REMD card for dropping
C SUMLIS  = String of banks written to SUMR file
C PASLIS  = String of banks written to PAS0 file
C FORMRL  = Output formats for BOS (1 FORT, 2 EPIO, 3 NO)
C FTYPRL  = Output formats on CARDS(1 NATI, 2 EPIO, 3 NO)
C OTYPRL  = Output types (1 DALI, 2 POT, 3 DST)
C
C#endif
