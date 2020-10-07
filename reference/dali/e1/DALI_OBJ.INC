c****************** COPY IN DIPCHG ************************************
C  Changes were done to data layout by BSN to make things run on DECstations
CBSN  INTEGER IWIW(0:12),IWIP(0:12)     ! WIDTH AND PAGE OF WINDOW
CBSN  REAL    VSTEP                     ! STEP VERTICAL FOR THESE SIZES

      PARAMETER( MAXNO = 250 )          ! MAX. NUMBER OF OBJECTS.
      PARAMETER( NOBJI = 29 )           ! NUMBER OF OBJECT INFORMATIONS
      PARAMETER( NPICT = 12 )           ! NUMBER OF POSSIBLE PICTURES
      CHARACTER*12 TOBJ(MAXNO)          ! NAME OF OBJECT AS IN 'PART'
      ! BANK
      CHARACTER*49 TPH(5)               ! TEXT FOR THE PH COMMAND
      LOGICAL BBANK                     ! BANK USED TO INITIALIZE:
      ! .TRUE. --- FRFT
      ! .FALSE. -- PFRF
      LOGICAL BSORT,BLIST,BSTAR,BJET,BLONG
      LOGICAL BCOLO
      INTEGER ISORT                     ! SORT-ORDER
      INTEGER ISOAF                     ! SORT AFTER
      INTEGER KOLOR                     ! COLOUR
      INTEGER NSOBJ(MAXNO)              ! SELECTED OBJECTS USED.
      REAL*8    OBJ(NOBJI,MAXNO)             ! DETAILS OF OBJECT.
      REAL*8    DIPANG                  ! ANGLE FUNCTION
      REAL*8    REFAD
      REAL DMASS                        ! DELTA-MASS FOR SU COMMAND
      INTEGER IOBJP(MAXNO,0:NPICT)        ! REMEMBER THE SORT-ORDER
      CHARACTER*2 TOBJN(NOBJI)          ! OBJECT NAMES
      CHARACTER*1 TOTY(10)
      INTEGER LSTHI                     ! LAST HIGH OBJECTNUMBER
      INTEGER LSTRE
      COMMON /DIPCMN/ DMASS,
     &  NOBJ,NUMT,NUME,NUMH,NUMR,NUMJ,KOLOR,ISORT,ISOAF,NPART,
     &  JPART,LSTHI,LSTRE,
     &  BBANK,BSORT,BLIST,BCOLO,BSTAR,BJET,BLONG,
     &  TOBJ,TPH,TOTY
      COMMON /DIPLRG/ OBJ,IOBJP


C**********************************************************************
C
C       OBJ --- DETAILS OF OBJECT
C       -------------------------
C
C       1       RE      REFERENCED NUMBER (USED FOR SORTING THE OUTPUT)
C       2       TY      OBJECT TYPE: (AS IN TOTY)
C                       0 - UNDEFINED (SINGLE HIT, F.INST.)
C                       1 - CHARGED TRACK
C                       2 - JET
C                       3 - RECONSTRUCTED
C                       4 - NEUTRAL TRACK ECAL
C                       5 - NEUTRAL TRACK HCAL
C       3       NO      TRACK NUMBER
C       4       RH      RHO - THE RADIUS OF CURVATURE
C       5       TH      THETA
C       6       P0      PHI 0
C       7       D0      D0
C       8       Z0      Z0
C       9       AL      ALPHA
C       10      P       MOMENTUM
C       11      PT      TRANSVERSE MOMENTUM (JET MEMBERS)
C       12      PX      MOMENTUM IN X-DIRECTION
C       13      PY      MOMENTUM IN Y-DIRECTION
C       14      PZ      MOMENTUM IN Z-DIRECTION
C       15      Q       CHARGE
C       16      M       MASS
C       17      ET      TOTAL ENERGY
C       18      JE      JET NUMBER
C       19      PA      PARENT
C       20      ON      OBJECT NUMBER
C       21      RB      RELATION BITS
C       22      EC      ECAL OBJECT
C       23      HC      HCAL OBJECT
C       24      SO      SELECTED OBJECT
C       25      US      USED OBJECT? (0 OR 1)
C       26      AO      ASSOCIATED CAL OBJECT NUMBER
C       27      L1      FIRST  EIGENVALUE OF MOMENTUM TENSOR (JETS ONLY)
C       28      L2      SECOND ...
C       29      L3      THIRD  ... NOTE: L1 < L2 < L3
C

      PARAMETER( BFIELD = 15.0D0 , CLGHT = 29.9792458D0 )   ! 1E9 CM/S

      DATA TOBJN / 'RE','TY','NO','RH','TH','P0','D0','Z0','AL','TP',
     &  'PT','PX','PY','PZ','TQ','IM','TE','JE','PA','ON','RB','EC',
     &  'HC','SO','US','AO','L1','L2','L3' /
CBSN  DATA TOTY / 'U' , 'T' , 'J' , 'R' , 'N' , 'N' , ' ' , ' ' , ' ' , 
CBSN &  ' ' /
C                  0   1   2   3   4   5   6   7   8   9  10 11 12
CBSN  DATA IWIW / 123,041,041,041,041,041,041,083,083,041,41,41,83 /
CBSN  DATA IWIP / 065,032,032,032,032,032,032,032,032,065,65,65,65 /
CBSN  DATA VSTEP / 10.0 /

      DATA REFAD / 5.0D0 /               ! NUMBER TO ADD TO OBJRE
CBSN  DATA DMASS / +0.1000 /
C                          1         2         3         4
C                 1234567890123456789012345678901234567890123456789
CBSN  DATA TPH / 'PH:WW: S=123 T=123 N=123 R=123 J=123 To=123 [123]' ,
CBSN &           'PH: Selected                                     ' ,
CBSN &           'PH:WW: DO what?                                  ' ,
CBSN &           'PH:    Unknown command.                          ' ,
CBSN &           'PH: No object number 123 avaible.                ' /
C                                      22:24

