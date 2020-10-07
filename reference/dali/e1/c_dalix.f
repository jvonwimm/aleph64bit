      BLOCK DATA DALIX
C  Block data for the DALIx.FOR routines necessary because of the Unix
C  Fortran compilers.
C
C  Bjorn S. Nilsson, 23-Apr-1992
C
      PARAMETER( MAXNO = 250 )          ! MAX. NUMBER OF OBJECTS.
      PARAMETER( NOBJI = 29 )           ! NUMBER OF OBJECT INFORMATIONS
      PARAMETER( NPICT = 12 )           ! NUMBER OF POSSIBLE PICTURES
      CHARACTER *12 TOBJ(MAXNO)          ! NAME OF OBJECT AS IN 'PART'
      ! BANK
      CHARACTER *49 TPH(5)               ! TEXT FOR THE PH COMMAND
      LOGICAL BBANK                     ! BANK USED TO INITIALIZE:
      ! .TRUE. --- FRFT
      ! .FALSE. -- PFRF
      LOGICAL BSORT,BLIST,BSTAR,BJET,BLONG
      LOGICAL BCOLO
      INTEGER ISORT                     ! SORT-ORDER
      INTEGER ISOAF                     ! SORT AFTER
      INTEGER KOLOR                     ! COLOUR
      REAL DMASS                        ! DELTA-MASS FOR SU COMMAND
      CHARACTER *1 TOTY(10)
      INTEGER LSTHI                     ! LAST HIGH OBJECTNUMBER
      INTEGER LSTRE
      COMMON /DIPCMN/ DMASS,
     &  NOBJ,NUMT,NUME,NUMH,NUMR,NUMJ,KOLOR,ISORT,ISOAF,NPART,
     &  JPART,LSTHI,LSTRE,
     &  BBANK,BSORT,BLIST,BCOLO,BSTAR,BJET,BLONG,
     &  TOBJ,TPH,TOTY

      DATA KOLOR/8/

      DATA TOTY / 'U' , 'T' , 'J' , 'R' , 'N' , 'N' , ' ' , ' ' , ' ' ,
     &  ' ' /
C                  0   1   2   3   4   5   6   7   8   9  10 11 12

      DATA DMASS / +0.1000 /
C                          1         2         3         4
C                 1234567890123456789012345678901234567890123456789
      DATA TPH / 'PH:WW: S=123 T=123 N=123 R=123 J=123 To=123 [123]' ,
     &           'PH: Selected                                     ' ,
     &           'PH:WW: DO what?                                  ' ,
     &           'PH:    Unknown command.                          ' ,
     &           'PH: No object number 123 avaible.                ' /
C                                      22:24
      END
