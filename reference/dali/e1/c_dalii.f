*DK DIN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DIN
CH
      SUBROUTINE DIN
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *2 TANSW
      CHARACTER *8 TIM
      LOGICAL FCHG
      CHARACTER *49 T1
C
C              123456789 123456789 123456789 123456789 123456789
      DATA T1/'P?:  Introduction'/
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
  930 CALL DTYPT('TYPE',TPICDO, 0    ,  0   ,  0   ,  ' ' ,T1)
  936 FCHG=.FALSE.
      CALL DGZOOM(6,IAREDO,0,0)
      CALL DOPER(1,1,
     &  1,0,' ',0,
     &  1,0,' ',0,
     &  NEXEC,FCHG,TANSW)
      GO TO (910,920,930,930),NEXEC
  910 RETURN
  920 CALL DO_STR('TI: time = ') 
      IF(TANSW.EQ.'TI') THEN
        CALL TIME(TIM)
        CALL DWRT(TIM)
        GO TO 930
      END IF
      IF(TANSW.EQ.'X?') GO TO 930
      CALL DWR_IC(TANSW)
      GO TO 936
      END
      SUBROUTINE DIP
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- Allan Engelhardt       4-JUL-1990
C!   Modified :- Allan Engelhardt      28-AUG-1990  Almost final version.
C!
C!   Inputs:
C!
C!   Outputs:
C!        -

C!
C!   Libraries required: None for DIP. KERNLIB and GENLIB for some of the other
C!   subroutines.
C!
C!   Description
C!   ===========
C!
C!      Physics processor for DALI.
C?
C?      Physics processor.
C?
C?      SUBROUTINE DIP0   - Initialization subroutine.
C?      ENTRY      DIP1   - Main routine.
C?      SUBROUTINE DIPCOM - Handles SA,PI,LO,NO,SF,SN,SO,PA,MS,AL
C?                                  UA,NL,IM,LT,AN commands
C?      SUBROUTINE DIPAD  - Handles AD command
C?      SUBROUTINE DIPJET - Handles JE command
C?      SUBROUTINE DIPOB  - Creates objectlist for each new event
C?      SUBROUTINE DIPCHG - Handles DOPER commands.
C?      SUBROUTINE DIPON  - Find the picked object number.
C?      SUBROUTINE DIPD   - Handles PIcking, MOving and drawing
C?      SUBROUTINE DIPRC  - Recalcultes the objectlist.
C?      SUBROUTINE DIPRO  - Removes an object from the list
C?      SUBROUTINE DIPDI  - Display detailed information about an object
C?      SUBROUTINE DIPNP  - Remove a parent from the objectlist.
C?      ENTRY      DIPSO  - Select an object.
C?      FUNCTION   DIPCHK - Returns the number of loops in suggested data.
C?      FUNCTION   DIPANG - My version of ATAN : gives result in [0°;360°]
C?      SUBROUTINE DIPSU  - Suggestions for an given object.
C?      SUBROUTINE DIPDA  - Displays information about associated objects.
C?
C!======================================================================
*IF .NOT.DOC

      INCLUDE 'DALI_CF.INC'
      INCLUDE 'A_BCS.INC'
C
C      CHARACTER *1 DT1                   ! ******************************
C      CHARACTER *2 DT2                   ! * CHARACTER CONVERSIONS FROM *
C      CHARACTER *3 DT3,DT3Z              ! * DALBT.FOR                  *
C      CHARACTER *4 DT4                   ! * CONVERTS REAL TO THE       *
C      CHARACTER *5 DT5                   ! * DESIRED CHARACTER LENGTH   *
C      CHARACTER *6 DT6                   ! *                            *
C      CHARACTER *7 DT7                   ! *                            *
C      CHARACTER *8 DT8                   ! ******************************
C      PARAMETER (NTRK=9)
C      DIMENSION TRAK(NTRK)
      INTEGER IRUN,IEVT                 ! RUN AND EVENT NUMBER
      COMMON / DIPRUN / IRUN,IEVT       ! ALSO IN DIPD
      DATA IRUN / -1E3 / , IEVT / -1E3 /
C     -------------------------------INITIALIZATION FOR THE 'PH' PROCESSOR
      CALL DQHLP('PH ')                 ! CALL THE HELP MENU
      IF (.NOT.( (IEVT.EQ.IEVTDE(1)).AND.(IRUN.EQ.IRUNDE(1)) )) THEN
        IEVT = IEVTDE(1)                ! THIS ROUTINE IS ALSO IN DIPD
        IRUN = IRUNDE(1)
        CALL DIP0
      ENDIF
C     -----------------------------------------------CALL  MAIN ROUTINE
      CALL DIP1

  999 RETURN
      END
C_______________________________________________________________________
      SUBROUTINE DIP0
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- Allan Engelhardt       4-JUL-1990
C!   Modified :- Allan Engelhardt      28-AUG-1990  Almost final version.
C!
C!   Inputs:
C!
C!   Outputs:
C!        -
C!
C!   Libraries required: DIP0 : None.
C!                 ENTRY DIP1 :  KERNLIB (M107/SORTD ,M103/FLPSOR)
C!                               GENLIB  (M109/SORTDQ,F224/EISRS )
C!
C!   Description
C!   ===========
C!
C!      Initialization for the DALI physics processor 'PH'.
C?      Contains the DIP1 entrypoint.
C?
C!======================================================================
*IF .NOT.DOC
      INCLUDE 'DALI_CF.INC'
      ! *NOTE* THAT THE MODULE IS
      ! TAKEN FROM THE DEFAULT
      ! DIRECTORY.

      PARAMETER( NCMDI = 19 , NCMDD = 12 )
      PARAMETER (NTRK=9)
      DIMENSION TRAK(NTRK)
      REAL    PARD(4,NCMDD)             ! PARAMETERS FROM DOPER
      INTEGER NEXEC                     ! EXECUTABLE LEVEL FROM DOPER:
      ! 1 = GB,GT
      ! 2 = (UNKNOWN TO DOPER)
      ! 3 = <CR>
      ! 4 = DO,'  '

      INTEGER NOBJ,NUMT,NUME,NUMH,NUMR,NUMJ  ! NUMBER OF OBJECTS,
      ! T - CHARGED TRACKS
      ! E - ECAL
      ! H - HCAL
      ! R - RECONSTRUCTED
      ! J - JETS

      CHARACTER *2 TANSW
      CHARACTER *2 TCMDI(NCMDI)
      CHARACTER *2 TCMDD(NCMDD)
      CHARACTER *49 TPHOU                ! OUTPUT FOT PH PROCESSOR
      CHARACTER *2 TCOM(12)
c      CHARACTER *1 DT1                   ! ******************************
c      CHARACTER *2 DT2                   ! * CHARACTER CONVERSIONS FROM *
      CHARACTER *3 DT3,DT3Z              ! * DALBT.FOR                  *
c      CHARACTER *4 DT4                   ! * CONVERTS REAL TO THE       *
c      CHARACTER *5 DT5                   ! * DESIRED CHARACTER LENGTH   *
c      CHARACTER *6 DT6                   ! *                            *
c      CHARACTER *7 DT7                   ! *                            *
c      CHARACTER *8 DT8                   ! ******************************
      CHARACTER *2 TP1

      LOGICAL BCHNG
      LOGICAL FOUND
      INCLUDE 'DALI_OBJ.INC'
      DATA TCMDI / 'SA','PI','LO','NO','SF','SN','TO','AD','JE',
     &   'JM','MS','AL','UA','NL','IM','LB','AN','LC','EO' /
      DATA TCMDD / 'SO','TC','US','DI','RO','HY','PA','SU',
     &  'LA','DA','JB','SL'/ ! DOPER COMMANDS
      DATA TCOM/'EL','PT','ST','ET','DN','DG','DE','DH','TL','MT',
     &          'MN','**'/
      DATA PARD  / 01.00 , -1.00 , MAXNO , -1.00 ,
     &             08.00 , 08.00 , 15.00 , -1.00 ,
     &             01.00 , -1.00 , MAXNO , -1.00 ,
     &             01.00 , -1.00 , MAXNO , -1.00 ,
     &             01.00 , -1.00 , MAXNO , -1.00 ,
     &             01.00 , -1.00 , MAXNO , -1.00 ,
     &             01.00 , -1.00 , MAXNO , -1.00 ,
     &             01.00 , -1.00 , MAXNO , -1.00 ,
     &             01.00 , -1.00 , MAXNO , -1.00 ,
     &             01.00 , -1.00 , MAXNO , -1.00 ,
     &             01.00 , -1.00 , MAXNO , -1.00 ,
     &             00.00 , +10.0 , +100. , -1.00 /
C#ifdef Old
C      DATA KOLOR / 8 /
C      DATA BSORT / .TRUE. / , BSTAR / .TRUE. / , BLIST /.FALSE. / ,
C     &     BJET  / .TRUE. / , BLONG / .TRUE. / , BCOLO / .TRUE. /
C      DATA ISORT / 1 / , ISOAF / 6 /
C#endif

C                  0   1   2   3   4   5   6   7   8   9  10 11 12
      DATA N1/1/
      DATA TP1/'TR'/

      INCLUDE 'A_BCS.INC'
C      INCLUDE 'BCS.INC'
      INCLUDE 'DALI_OBD.INC'
      INCLUDE 'A_BMACRO.INC'
C      INCLUDE 'BMACRO.INC'

      CALL DPITR0(1,TCOM)

C--------------------------------------------------------NITIALIZE OBJ:
      NUMT = 0
      NUME = 0
      NUMH = 0
      NUMR = 0
      NUMJ = 0

      DO 1140 J = 1 , MAXNO
        DO 1120 I = 1 , NOBJI
          OBJ(I,J) = 0.0D0
 1120   CONTINUE
C                  123456789012
        TOBJ(J) = '            '
 1140 CONTINUE

C       ................................... NPICT ?????
      DO 1160 K = 0 , NPICT
        DO 1150 J = 1 , MAXNO
          IOBJP(J,K) = 0
 1150   CONTINUE
 1160 CONTINUE

C      DO 1111 I = 1 , MAXNO
C        OBJ(3,I) = DBLE(MAXNO + 1)
C 1111 CONTINUE
C---------------------------------Create object list ( fill event ):
      CALL DIPOB

C       COPIED FROM CHRIS GRAB'S ROUTINE --- ASK HIM, NOT ME :-)
C       SETS UP THE PART BANK
      LUNDB = JUNIDB(0)
      JPART = MDARD(IW,LUNDB,'PART',0)
      IF ( JPART.EQ.0 ) THEN
        NPART = 0
      ELSE
        NPART = LROWS(JPART)
      ENDIF
      NPART = MIN(800,NPART)            ! CHANGED 500 -> 800
C       PART BANK OK (I HOPE)

      RETURN
C
C_____________________________________________________________________

      ENTRY DIP1
C!  -
C!
C!   Author   :- Allan Engelhardt       4-JUL-1990
C!
C!   Inputs:
C!
C!   Outputs:
C!        -
C!
C!                                  Routines referenced:
C!   Libraries required: KERNLIB  | M107/SORTD ,M103/FLPSOR
C!                       GENLIB   | M109/SORTDQ,F224/EISRS
C!
C!   Description
C!   ===========
C!
C!      Main routine of the DALI physics processor.
C?
C!======================================================================

   10 CONTINUE
      NCC = 0                           ! NUMBER CURRENT COMMAND
      IDIPS = 0
      DO 13 I = 1 , MAXNO
        IF ( OBJUS(I).GT.0.0 ) THEN
          IF ( OBJSO(I).GT.0.0 ) IDIPS = IDIPS + 1
        ENDIF
   13 CONTINUE
      TPH(1)(10:12) = DT3(REAL(IDIPS))
      TPH(1)(16:18) = DT3(REAL(NUMT))
      TPH(1)(22:24) = DT3(REAL(NUME+NUMH))
      TPH(1)(28:30) = DT3(REAL(NUMR))
      TPH(1)(34:36) = DT3(REAL(NUMJ))
      TPH(1)(41:43) = DT3(REAL(NOBJ))
      TPH(1)(46:48) = DT3(REAL(LSTRE))
      CALL DWR_HL_AR(TPH(1))                ! OUTPUT HIGH 2 LETTERS
   11 CONTINUE                          ! AD LOOP AROUND HERE
      I = 0
      TPHOU = TPH(2)
      DO 14 II = 1 , MAXNO
        IF ( OBJSO(II).GT.0.0D0 ) THEN
          IF ( I.GT.9 ) THEN
            TPHOU(46:48) = '...'
            GOTO 15
          ENDIF
          I1 = 14 + 4*I
          I2 = I1 + 2
          TPHOU(I1:I2) = DT3(REAL(OBJNO(II)))
          I = I + 1
        ENDIF
   14 CONTINUE
   15 CONTINUE
      IF ( I.EQ.0 ) TPHOU = 'PH: No selected objects.'
   12 CONTINUE                          ! NEXT KEY INPUT
      DO 20 I = 1 , NCMDD
        IF ( PARD(3,I).EQ.REAL(MAXNO) ) PARD(2,I) = REAL(LSTRE) ! LAST
        ! REFERENCED
        PARD(4,I) = -1.0                ! SET TO CHECK CHANGES
   20 CONTINUE

      BCHNG = .FALSE.                   ! SET TO CHECK CHANGES

      CALL DGZOOM(6,IAREDO,0,0)         ! DRAW LIGHT FRAME
C     ---------------------------------------------------COMMAND INPUT
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_RST'
      CALL DPARAM(19
     &  ,J_RST)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DOPER(0,1,
     &  1,N1,TP1,PARADA(1,J_RST),
     &  1,NCMDD,TCMDD,PARD,
     &  NEXEC,BCHNG,TANSW)
      CALL DGZOOM(6,-1,0,0)             ! REMOVE FRAME.

      IF ( BCHNG ) THEN                 ! ANY VALUES CHANGED?
        DO 22 I = 1 , NCMDD
          IF ( PARD(4,I) .NE. (-1.0) ) THEN
            CALL DIPCHG(TANSW,TRAK,I,PARD(1,I),PARD(2,I),
     &                 PARD(3,I), PARD(4,I))
          ENDIF
   22   CONTINUE
      ELSE
        CONTINUE
      ENDIF
C     _____________________________________________________________________
      GOTO (999,920,930,940) NEXEC
C     -----------------------------------------NEXEC = 2 : UNKNOWN TO DOPER
  920 CONTINUE
      DO 922 I = 1 , NCMDI
        IF( TANSW .EQ. TCMDI(I) ) THEN
          NCC = I
          CALL DIPCOM(TRAK,NCC, *12,*11)
          RETURN
        ENDIF
  922 CONTINUE

      DO 923 I=1,12
        IF (TANSW.EQ.TCOM(I)) THEN
          CALL DPITRK(TANSW,PARADA(2,J_RST),NTRK,TRAK,FOUND,ETRAK)
          BLIST=.FALSE.
          IF (ETRAK.GT.0.) CALL DIPUS(TANSW,TRAK,I,PARD(1,I),PARD(2,I),
     &          PARD(3,I),PARD(4,I))
          IF (TANSW.EQ.TCOM(2).or.
     &        TANSW.EQ.TCOM(3)) CALL DIPSO(TANSW,TRAK,I,PARD(1,I),
     &        PARD(2,I),PARD(3,I),PARD(4,I))
          IF (TANSW.EQ.TCOM(1)) CALL DIPUA(TRAK)
          GOTO 10
        ENDIF
  923 CONTINUE

      CALL DWRT(TPH(4))
      GOTO 12
C     ------------------------------------------------   NEXEC = 3 : <CR>
  930 CONTINUE
      GOTO (935,935,935) (NCC+1)
  935 GOTO 10                           ! RETURN
C     ---------------------------------------------   NEXEC = 4 : GO BACK
  940 CONTINUE
      NCC=3                          ! LIST OBJECTS
      CALL DIPCOM(TRAK,NCC,*12,*11)
C     ----------------------------------------------  NEXEC = 1 : RETURN
  999 RETURN

      END
C_______________________________________________________________________

      SUBROUTINE DIPCOM(TRAK,NCC,*,* )
C----------------------------------------------------------------------
C!
C!
C!   Author   : -Allan Engelhardt
C!   Modified :
C!
C!   Inputs:
C!        - NCC      command index ( from DOPER )
C!
C!   Outputs:
C!        -
C!            RETURN1 :NEXT KEY
C!            RETURN2 :PRINT OUT & KEY
C!
C!   Description
C!   ===========
C!        Handles all the possible commands in the PH processor
C!
C!-----------------------------------------------------------------------

      INCLUDE 'DALI_CF.INC'
      INCLUDE 'DALI_OBJ.INC'
      PARAMETER (PI=3.1415926535)
      PARAMETER (NCMDD=12)
      PARAMETER (NTRK=9)
      DIMENSION TRAK(NTRK)
      REAL*8       P0,P1,P2,P3,PTOT,STH,REF
      REAL PARD(4,NCMDD)
      REAL    P(2,3)                    ! USED FOR AN COMMAND
      LOGICAL JBSO
      CHARACTER *49 TSA(2)               ! TEXT FOR THE SA COMMAND
      CHARACTER *40 TEO
      CHARACTER *40 TLB
      CHARACTER *2 TSAIN                ! INPUT FOR THE SA COMMAND
      CHARACTER *25 TIM(1)               ! TEXT FOR THE IM COMMAND
      CHARACTER *22 TAN(1)               ! TEXT FOR THE AN COMMAND
      CHARACTER *2 TANSW                 ! ANSWER FORM DOPER
      CHARACTER *2 TIN
      CHARACTER *11 TLT                   ! LORENTZ BOOST (HYPOTHESIS)
C      CHARACTER *1 DT1                   ! ******************************
      CHARACTER *2 DT2                   ! * CHARACTER CONVERSIONS FROM *
      CHARACTER *3 DT3,DT3Z              ! * DALBT.FOR                  *
C      CHARACTER *4 DT4                   ! * CONVERTS REAL TO THE       *
      CHARACTER *5 DT5                   ! * DESIRED CHARACTER LENGTH   *
      CHARACTER *6 DT6                   ! *                            *
C      CHARACTER *7 DT7                   ! *                            *
C      CHARACTER *8 DT8                   ! ******************************

C                          1         2         3         4
C                 1234567890123456789012345678901234567890123456789
      DATA TIM / 'Invariant mass: 123456   '/
      DATA TAN / 'Angle=          (deg) '/
      DATA TLB / 'Boosted object 123 creates object 123 . ' /
      DATA TEO / 'Reconstructed object 123 is now erased. ' /
      DATA TSA / 'PH: Sort after (see the list of codes in help)   ' ,
     &           'PH: XX is not a valid code. Try to look in help. ' /
      DATA TLT / 'bo.  in    '/
C                 12345678901
      DATA PARD  / 01.00 , -1.00 , MAXNO , -1.00 ,
     &             08.00 , 08.00 , 15.00 , -1.00 ,
     &             01.00 , -1.00 , MAXNO , -1.00 ,
     &             01.00 , -1.00 , MAXNO , -1.00 ,
     &             01.00 , -1.00 , MAXNO , -1.00 ,
     &             01.00 , -1.00 , MAXNO , -1.00 ,
     &             01.00 , -1.00 , MAXNO , -1.00 ,
     &             01.00 , -1.00 , MAXNO , -1.00 ,
     &             01.00 , -1.00 , MAXNO , -1.00 ,
     &             01.00 , -1.00 , MAXNO , -1.00 ,
     &             01.00 , -1.00 , MAXNO , -1.00 ,
     &             00.00 , +10.0 , +100. , -1.00 /

      INCLUDE 'DALI_OBD.INC'
C     --------------------------------------------------GOTO : COMMAND

C           SA  PI  LO  NO  SF  SN  TO  AD  JE  PA  MS  AL
      GOTO (100,150,200,250,260,265,110,120,130,270,290,300,
     &  310,275,500,800,1000,1100,1200) NCC
C       UA  NL  IM  LT   AN   LC   EO
C     ------------------------------------------------   SA: Sort After
  100 CONTINUE

      CALL DGZOOM(6,IAREDO,0,0)         ! DRAW LIGHT FRAME
      CALL DOPER(0,1,
     &  1,0,' ',0,
     &  1,0,' ',0,
     &  NEXEC,BCHNG,TSAIN)
      CALL DGZOOM(6,-1,0,0)             ! REMOVE FRAME.

      GOTO (999,101,902,200) NEXEC
  101 CONTINUE
      CALL CLTOU(TSAIN)                  ! CONVERT TO UPPERCASE
      IF ( TSAIN.EQ.'  ' ) RETURN 2      ! NOTE 11 HERE
      DO 105 I = 1 , NOBJI
        IF ( TSAIN.EQ.TOBJN(I) ) GOTO 106
  105 CONTINUE
      GOTO 108
  106 CONTINUE
      IF ( I.EQ.24 ) THEN
        DO 107 J = 1 , MAXNO
          S = OBJSO(J)
          IF (S.LT.0.0) OBJ(24,J) = 0.0D0
  107   CONTINUE
      ENDIF
      ISOAF = I                         ! SAVE THE SORTING
      I = I*ISORT                       ! ORDERING, ISORT \in {-1,+1}
      CALL SORTDQ(OBJ,NOBJI,MAXNO,I)
      JBSO = BSORT
      FPIKDP = .FALSE.
      BSORT = .FALSE.
      CALL DIPD                         ! LIST OBJECTS
      BSORT = JBSO
      RETURN 1

  108 CONTINUE
      TSA(2)(5:6) = TSAIN
      CALL DWRT(TSA(2))
      GOTO 100
C     -------------------------------------------   TO: Toggle sort Order.
  110 CONTINUE
      ISORT = -ISORT
      IF ( ISORT.EQ.1 ) THEN
        CALL DWRT('PH: Sort order is now ascending (1.2.3).')
      ELSE
        CALL DWRT('PH: Sort order is now decending (3.2.1).')
      ENDIF
      RETURN 1
C     -----------------------------------------   AD: ADD four vectors etc
  120 CONTINUE
      CALL DIPAD
      IF ( BLIST ) GOTO 200             ! LIST OBJECTS
      RETURN 2                           ! AND FINISH :-) NOTE 11 HERE
C     -------------------------------------------------------- JE: JET
  130 CONTINUE

      CALL DIPJET

      RETURN 1
C     ---------------------------------------------   PI: PIck an object.
  150 CONTINUE

      DPIKDP = 9999.                    ! DISTANCE = HIGH :-)
      TIN = 'PI'
      CALL  DPOS(TIN,REAL1,REAL2,NEXEC,BCHNG)       ! DALIP.FOR
      IF ( TIN.EQ.'ST' ) THEN
        TR = DIPON(NPIKDP,MDLPDP)
      ELSE
        TR = -9999.0
      ENDIF
      IF ( TR.LT.(-1.0) ) THEN
        CALL DWRT('PH: No track was selected.')
      ELSEIF ( TR.EQ.(-1.0) ) THEN
        CALL DWRT('PH: Processor cannot handle this object yet.')
        CALL DWRT('PH: No track was selected.')
      ELSE
        CALL DIPSO(TANSW,TRAK,I,PARD(1,I),PARD(2,I),PARD(3,I),
     &                  PARD(4,I))
      ENDIF
      GOTO (999,901,901,901) NEXEC
      RETURN 1
C     -------------------------------------------   LO: List of Objects
  200 CONTINUE

      FPIKDP = .FALSE.                  ! DRAW, DON'T MOVE OR PICK
      CALL DIPD

  229 RETURN 1
C     ------------------------------   NO: NOrmal ordering of object-list
  250 CONTINUE
      CALL SORTDQ(OBJ,NOBJI,MAXNO,3)    ! QUICKERSORT AFTER TRACK NUMBER
      JBSO = BSORT
      BSORT = .FALSE.
      FPIKDP = .FALSE.
      CALL DIPD
      BSORT = JBSO
      RETURN 1
C     -------------------------------------------------   SF: Sort ofF
  260 CONTINUE
      BSORT = .FALSE.
      CALL DWRT('PH: Automatic sorting of object-list is now OFF')
      RETURN 1
C-------------------------------------------------------   SN: Sort oN
  265 CONTINUE
      BSORT = .TRUE.
      CALL DWRT('PH: Automatic sorting of object-list is now ON')
      RETURN 1
C------------------------------------------------   JM: Toggle Jet Mode
  270 CONTINUE
      IF ( BJET ) THEN
        BJET = .FALSE.
        CALL DWRT('PH: Jet mode is now OFF.')
      ELSE
        BJET = .TRUE.
        CALL DWRT('PH: Jet mode is now ON.')
      ENDIF
      RETURN 1
C     ---------------NL: Notice when the list is too Long for the display
  275 CONTINUE
      IF ( BLONG ) THEN
        BLONG = .FALSE.
        CALL DWRT('PH: No warnings for long objectlist.')
      ELSE
        BLONG = .TRUE.
        CALL DWRT('PH: Warnings for long objectlist.')
      ENDIF
      RETURN 1
C     ----------------------------------------   MS: Mark Selected objects
  290 CONTINUE
      IF ( BSTAR ) THEN
        BSTAR = .FALSE.
        CALL DWRT('PH: Selected objects are no longer marked.')
      ELSE
        BSTAR = .TRUE.
        CALL DWRT('PH: Selected objects now marked with a *')
      ENDIF
      RETURN 1
C     ----------------------   AL: Automatic listing after DO, AD, SO, etc.
  300 CONTINUE
      IF ( BLIST ) THEN
        BLIST = .FALSE.
        CALL DWRT('PH: Automatic listing of objects is now OFF.')
      ELSE
        BLIST = .TRUE.
        CALL DWRT('PH: Automatic listing of objects is now ON.')
      ENDIF
      RETURN 1
C------------------------------------------------------   UA: Unselect All
      ENTRY DIPUA(TRAK)
  310 CONTINUE
      DO 311 I = 1 , MAXNO
        OBJ(24,I) = 0.0D0
  311 CONTINUE
      CALL VZERO(TRAK,NTRK)
      IF ( BLIST ) THEN
        GOTO 200
      ELSE
        RETURN 1
      ENDIF
C     ------------------------------------------------  IM:  Invariant Mass
  500 CONTINUE
      P0=0.0D0
      P1=0.0D0
      P2=0.0D0
      P3=0.0D0
      NUM=1

      DO 510 I=1,MAXNO
        IF (OBJSO(I).GT.0.0D0) THEN
          NUM=NUM+1
          P0=P0+DBLE(OBJET(I))
          P1=P1+DBLE(OBJPX(I))
          P2=P2+DBLE(OBJPY(I))
          P3=P3+DBLE(OBJPZ(I))
        ENDIF
  510 CONTINUE

      NUM=NUM-1
      IF (NUM.EQ.0) THEN
        CALL DWRT('PH:No objects was selected.')
        RETURN 1
      ENDIF

      PTOT=P1*P1+P2*P2+P3*P3
      RMA=REAL(SQRT(ABS(P0*P0-PTOT)))
      TIM(1)(17:22)=DT6(REAL(RMA))
      CALL DWRT(TIM(1))
      RETURN 1
C     -------------------------------------------  LB:   Lorentz Boost
  800 CONTINUE
     REF=0.0D0
 3750 CALL DWRT(' Input rest frame (number of object):')
      CALL DGTINT(IS,LL)
      IF(LL.LT.0) GO TO 3750
      IF (LL.LE.0) RETURN 1
      DO 801 L=1,MAXNO
        IF (IS.EQ.OBJ(3,L))THEN
           IS=L
           GOTO 802
        ENDIF
  801 CONTINUE
  802 CONTINUE
      IF (OBJM(IS).EQ.0.0D0) THEN
        CALL DWRT(' Illegal frame (M=0) ! ')
        GOTO 800
      ENDIF

 3751 CALL DWRT(' Which object to boost ?:')
      CALL DGTINT(I,L)
      IF(L.LT.0) GO TO 3751
      TLB(16:18)=DT3(REAL(I))
      DO 803 L=1,MAXNO
        IF (I.EQ.OBJ(3,L)) THEN
          I=L
          GOTO 804
        ENDIF
  803 CONTINUE

  804 Y=OBJPX(I)*OBJPX(IS)+OBJPY(I)*OBJPY(IS)+OBJPZ(I)*OBJPZ(IS)
      IF (OBJRE(I).GT.REF) REF=OBJRE(I)
      DO 810 IN=1,MAXNO
        IF (OBJUS(IN).EQ.0.0) GOTO 820
  810 CONTINUE
      IN=MAXNO+1
      NOBJ=NOBJ-1
      NUMR=NUMR-1
  820 CONTINUE
      NOBJ=NOBJ+1
      NUMR=NUMR+1
      LSTHI=LSTHI+1                     ! LAST HIGH IS NEW NUMBER
      IF (IN.EQ.(MAXNO+1)) THEN
        CALL DWRT('Cannot handle more objects. Sorry!' )
        GOTO 890
      ENDIF
      OBJ(1,IN)=REF+REFAD               ! REFERENCED
      OBJ(2,IN)=3.0D0                   ! TYPE
      OBJ(3,IN)=DBLE(LSTHI)             ! OBJECT NUMBER
      IF (I.EQ.IS) THEN
        OBJ(12,IN)=OBJPX(I)         ! PX
        OBJ(13,IN)=OBJPY(I)         ! PY
        OBJ(14,IN)=OBJPZ(I)         ! PZ
        OBJ(17,IN)=OBJET(I)         ! ETOT
      ELSE
        OBJ(17,IN)=(OBJET(IS)*OBJET(I)-Y)/OBJM(IS)  ! ETOT
        FAC=-(OBJ(17,IN)+OBJET(I))/(OBJET(IS)+OBJM(IS))
        OBJ(12,IN)=OBJPX(I)+FAC*OBJPX(IS)   ! PX
        OBJ(13,IN)=OBJPY(I)+FAC*OBJPY(IS)   ! PY
        OBJ(14,IN)=OBJPZ(I)+FAC*OBJPZ(IS)   ! PZ
      ENDIF
      OBJ(10,IN)=SQRT(OBJPX(IN)**2+OBJPY(IN)**2+OBJPZ(IN)**2)   ! MOMENTUM
      OBJ(5,IN)=ACOS(OBJPZ(IN)/OBJP(IN))        ! THETA
      STH=SIN(OBJ(5,IN))                ! SIN(THETA)
      OBJ(6,IN)=DIPANG(OBJPX(IN),OBJPY(IN))     ! PHI
      OBJ(7,IN)=0.0D0                   ! D0
      OBJ(8,IN)=0.0D0                   ! Z0
      OBJ(9,IN)=0.0D0                   ! ALPHA
      OBJ(11,IN)=-1.0D0                 ! P-TRANSVERSE
      OBJ(15,IN)=OBJ(15,I)              ! CHARGE
      OBJ(16,IN)=OBJ(16,I)                ! MASS
      OBJ(18,IN)=-1.0D0                 ! JET NUMBER
      OBJ(19,IN)=-1.0D0                 ! PARENT
      OBJ(20,IN)=-1.0D0                 ! OBJET NUMBER
      OBJ(21,IN)=0.0D0                  ! RELATION BITS
      OBJ(22,IN)=-1.0D0                 ! ECAL OBJECT
      OBJ(23,IN)=-1.0D0                 ! HCAL OBJECT
      OBJ(24,IN)=0.0D0                  ! SELECT OBJECT
      OBJ(25,IN)=+1.0D0                 ! USED OBJECT
      OBJ(4,IN)=2.222*OBJ(10,IN)        ! RADIUS OF CURVATURE (m)
      TLT(4:5)=DT2(REAL(OBJ(3,I)))
      TLT(9:10)=DT2(REAL(OBJ(3,IS)))    !
      TOBJ(IN)=TLT                      ! TEXT FOR THE OBJECTLIST
  890 CONTINUE
      TLB(35:37)=DT3(REAL(OBJ(3,IN)))
      CALL DWRT(TLB)
      IF ( BLIST ) GOTO 200             ! LIST OBJECTS
      RETURN 1
C     -------------------------------------  AN:   Angle between vectors
 1000 CONTINUE
      DO 1001 I=1,3
        P(1,I)=0.0
        P(2,I)=0.0
 1001 CONTINUE
      NUM=1
      DO 1010 I=1,MAXNO
        IF(OBJSO(I).GT.0.0D0)THEN
          IF (NUM.GT.2.OR.NUM.LT.2 ) THEN
            CALL DWRT('PH: select only 2 vectors!!')
            RETURN 1
          ENDIF
          P(NUM,1)=DBLE(OBJPX(I))
          P(NUM,2)=DBLE(OBJPY(I))
          P(NUM,3)=DBLE(OBJPZ(I))
          NUM =NUM+1
        ENDIF
 1010 CONTINUE
      NUM=NUM-1
      IF(NUM.EQ.0) THEN
        CALL DWRT('PH: No objects was selected.')
        RETURN 1
      ENDIF
      Z=P(1,1)*P(2,1)+P(1,2)*P(2,2)+P(1,3)*P(2,3)
      CT=Z/(SQRT((P(1,1)**2+P(1,2)**2+P(1,3)**2)*(P(2,1)**2+
     &    P(2,2)**2+P(2,3)**2)))
      T1=ACOS(CT)
      T=T1*180/PI
      TAN(1)(9:14)=DT5(REAL(T))
      CALL DWRT(TAN(1))
      RETURN 1
C     ----------------------------------------------  LC: Colours of List
 1100 CONTINUE
      IF ( BCOLO ) THEN
          BCOLO=.FALSE.
        ELSE
          BCOLO=.TRUE.
      ENDIF
      RETURN 1
C     ------------------------------------  EO: Erase reconstructed Object
 1200 CONTINUE
 3752 CALL DWRT('Which object to erase?')
      CALL DGTINT(II,I)
      IF(I.LT.0) GO TO 3752
      TEO(22:24)=DT3(REAL(II))
      DO 1210 I=1,MAXNO
        IF (II.EQ.OBJ(3,I))THEN
           IF ( II.EQ.LSTHI ) GOTO 1265
           IF ( OBJ(2,I).EQ.3 ) GOTO 1220
           CALL DWRT(' Only reconstructed objects to erase!!!')
           RETURN 1
        ENDIF
 1210 CONTINUE
 1220 CONTINUE
      DO 1230 IL=II,LSTHI-1
        DO 1235 IT=1,MAXNO
          IF ( OBJ(3,IT).EQ.IL ) I1=IT
          IF ( OBJ(3,IT).EQ.IL+1 ) I2=IT
 1235   CONTINUE
       OBJ(3,I2)=OBJ(3,I1)
       DO 1240 L=1,29
          OBJ(L,I1)=OBJ(L,I2)
 1240   CONTINUE
        TOTY(OBJ(3,I1))=TOTY(OBJ(3,I2))
        TOBJ(OBJ(3,I1))=TOBJ(OBJ(3,I2))
 1230 CONTINUE
      I=I2
 1265 CONTINUE
      DO 1250 L=1,29
        OBJ(L,I)=0.
 1250 CONTINUE
      LSTHI=LSTHI-1
      NOBJ=NOBJ-1
      CALL DWRT(TEO)
      RETURN 1
C____________________________________________________________________
 901  RETURN 1
 902  RETURN 2
 999  RETURN

      END
C_________________________________________________________________________

      SUBROUTINE DIPAD
C------------------------------------------------------------------------
C
C!      adds  four - vectors  ( former IM  command )
c
C!      Author : Allen Engelhardt
C------------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      INCLUDE 'DALI_OBJ.INC'
      REAL*8       P0,P1,P2,P3,PTOT2,PTOT,THETA,STH,REF,RE
C                          1         2         3         4
C                 1234567890123456789012345678901234567890123456789
      CHARACTER *49 TAD(3)               ! TEXT FOR THE AD COMMAND
      CHARACTER *10 THY
      CHARACTER *1 DT1                   ! ******************************
      CHARACTER *2 DT2                   ! * CHARACTER CONVERSIONS FROM *
      CHARACTER *3 DT3,DT3Z              ! * DALBT.FOR                  *
C      CHARACTER *4 DT4                   ! * CONVERTS REAL TO THE       *
C      CHARACTER *5 DT5                   ! * DESIRED CHARACTER LENGTH   *
      CHARACTER *6 DT6                   ! *                            *
C      CHARACTER *7 DT7                   ! *                            *
C      CHARACTER *8 DT8                   ! ******************************

      DATA TAD / 'Inv.mass=123456 Charge=12 Object no.=123         ' ,
     &           'PH: Cannot handle more objects. Sorry.           ' ,
     &           'Used 123 objects (123 123 123 123) to create 123.' /
C                 1234567890
      DATA THY / '          '/          ! TEXT FOR OBJECTLIST (HYPOTH.)
      INCLUDE 'DALI_OBD.INC'
C------------------------------------------------------------------------------
      P0 = 0.0D0                          ! FOUR-MOMENTAS
      P1 = 0.0D0                          ! FOR THE RECONSTRUCTED
      P2 = 0.0D0                          ! OBJECT
      P3 = 0.0D0
      Q  = 0.0                          ! CHARGE
      NUM = 1                           ! NUMBER IN NSOBJ
      IA=1
      IS=0
      THY='          '
C------------------------------------------------------------------------------
      DO 121 I = 1 , MAXNO
        NSOBJ(I) = 0
  121 CONTINUE
      REF = 0.0D0
      DO 125 I = 1 , MAXNO
        IF ( OBJSO(I).GT.0.0D0 ) THEN
          NSOBJ(NUM) = INT(OBJNO(I))
C-----------------------------------------------------  TEXT FOR HYPOTHESIS:
          DO 122 L=IA,8
            IF (THY(L:L+1).EQ.'  ') THEN
              IF ( L.EQ.1 ) THEN
                THY(L:L+1)=DT2(REAL(OBJ(3,I)))
              ELSE
                THY(L+1:L+2)=DT2(REAL(OBJ(3,I)))
              ENDIF
              IS=I
              IA=L
              GOTO 123
            ELSEIF ( L.EQ.8 ) THEN
              THY(L+2:L+2)='>'
            ENDIF
  122     CONTINUE
C-----------------------------------------------------
  123     NUM = NUM + 1
          P0 = P0 + DBLE(OBJET(I))
          P1 = P1 + DBLE(OBJPX(I))
          P2 = P2 + DBLE(OBJPY(I))
          P3 = P3 + DBLE(OBJPZ(I))
          Q  = Q  + OBJQ(I)
          OBJ(19,I) = DBLE(LSTHI + 1)   ! SET PARENT
        ENDIF
        RE = OBJRE(I)
        IF ( RE.GT.REF ) REF = RE
  125 CONTINUE
      REF = REF + REFAD
      DO 135 I = 1 , MAXNO
        IF ( OBJSO(I).GT.0.0D0 ) THEN
          OBJ( 1,I) = REF
        ENDIF
  135 CONTINUE
      NUM = NUM - 1
      IF ( NUM.EQ.0 ) THEN
        CALL DWRT('PH: No object was selected.')
        RETURN
      ENDIF
      DO 126 JN = 1 , MAXNO
        IF ( OBJUS(JN).EQ.0.0 ) GOTO 127
  126 CONTINUE
      JN = MAXNO + 1
      NOBJ = NOBJ - 1
      NUMR = NUMR - 1
  127 CONTINUE
      NOBJ = NOBJ + 1
      NUMR = NUMR + 1
      LSTHI = LSTHI + 1                 ! LAST HIGH (LSTHI) IS NEW
      ! NUMBER
      LSTRE = LSTHI                     ! SAVE LAST RE.
      PTOT2 = P1*P1 + P2*P2 + P3*P3     ! DOUBLE
      PTOT = SQRT(PTOT2)                ! DOUBLE
      THETA = ACOS(P3/PTOT)             ! DOUBLE
      STH = SIN(THETA)                  ! DOUBLE
      RMA = REAL(SQRT(ABS(P0*P0 - PTOT2)))
      TAD(1)(10:15) = DT6(REAL(RMA))
      IF ( Q.GT.0 ) THEN
        TAD(1)(24:24) = '+'
      ELSEIF ( Q.LT.0 ) THEN
        TAD(1)(24:24) = '-'
      ELSE
        TAD(1)(24:24) = ' '
      ENDIF
      TAD(1)(25:25) = DT1(REAL(ABS(Q)))
      TAD(1)(38:40) = DT3(REAL(LSTHI))

      CALL DWRT(TAD(1))

      IF ( JN.EQ.(MAXNO+1) ) THEN
        CALL DWRT(TAD(2))              ! CAN'T STORE OBJECT
        GOTO 1291
      ENDIF

      REF = REF + REFAD
      OBJ( 1,JN) = REF                  ! REFERENCED
      OBJ( 2,JN) = 3.0D0                  ! TYPE
      OBJ( 3,JN) = DBLE(LSTHI)                 ! OBJECT NUMBER
      OBJ( 4,JN) = 1.0D5*PTOT*STH/BFIELD/CLGHT ! RHO - RADIUS OF CURVATURE (?)
      OBJ( 5,JN) = THETA                ! THETA
      OBJ( 6,JN) = DIPANG(P1,P2)
      OBJ( 7,JN) = 0.0D0                  ! D0 (?)
      OBJ( 8,JN) = 0.0D0                  ! Z0 (?)
      OBJ( 9,JN) = 0.0D0                  ! ALPHA (?)
      OBJ(10,JN) = PTOT                 ! MOMENTUM
      OBJ(11,JN) = -1.0D0                 ! TRANSVERSE P - ONLY JETS
      OBJ(12,JN) = P1                   ! PX
      OBJ(13,JN) = P2                   ! PY
      OBJ(14,JN) = P3                   ! PZ
      OBJ(15,JN) = DBLE(Q)                    ! CHARGE
      OBJ(16,JN) = RMA                  ! MASS
      OBJ(17,JN) = P0                   ! ETOT
      OBJ(18,JN) = -1.0D0                 ! JET-NUMBER
      OBJ(19,JN) = -1.0D0                 ! PARENT
      OBJ(20,JN) = -1.0D0                 ! OBJECT NUMBER
      OBJ(21,JN) = -0.0D0                 ! RELATION BITS
      OBJ(22,JN) = -1.0D0                 ! ECAL OBJECT
      OBJ(23,JN) = -1.0D0                 ! HCAL OBJECT
      OBJ(25,JN) = +1.0D0                 ! USED OBJECT
      TOBJ(JN) = THY                      ! OBJECT HYPOTHESIS
      TAD(3)(6:8) = DT3(REAL(NUM))
      IF ( NUM.GT.4 ) THEN
C                        123456789012345
        TAD(3)(19:33) = 'to many to list'
        GOTO 129
      ELSEIF ( NUM.EQ.1 ) THEN
        TAD(3)(19:21) = DT3(REAL(NSOBJ(1)))
        TAD(3)(23:33) = '           '
        TAD(3)(22:22) = ' '
        TAD(3)(30:30) = ' '
      ELSEIF ( NUM.EQ.2 ) THEN
        TAD(3)(19:21) = DT3(REAL(NSOBJ(1)))
        TAD(3)(23:25) = DT3(REAL(NSOBJ(2)))
        TAD(3)(27:33) = '       '
        TAD(3)(22:22) = ' '
        TAD(3)(30:30) = ' '
      ELSEIF ( NUM.EQ.3 ) THEN
        TAD(3)(19:21) = DT3(REAL(NSOBJ(1)))
        TAD(3)(23:25) = DT3(REAL(NSOBJ(2)))
        TAD(3)(27:29) = DT3(REAL(NSOBJ(3)))
        TAD(3)(31:33) = '   '
        TAD(3)(22:22) = ' '
        TAD(3)(30:30) = ' '
      ELSEIF ( NUM.EQ.4 ) THEN
        TAD(3)(19:21) = DT3(REAL(NSOBJ(1)))
        TAD(3)(23:25) = DT3(REAL(NSOBJ(2)))
        TAD(3)(27:29) = DT3(REAL(NSOBJ(3)))
        TAD(3)(31:33) = DT3(REAL(NSOBJ(4)))
        TAD(3)(22:22) = ' '
        TAD(3)(30:30) = ' '
      ENDIF


  129 CONTINUE
      TAD(3)(46:48) = DT3(REAL(LSTHI))
      CALL DWRT(TAD(3))
 1291 CONTINUE
      IF ( BLIST ) THEN
        FPIKDP=.FALSE.
        CALL DIPD
      ENDIF
      RETURN

      END
C_______________________________________________________________________
      SUBROUTINE DIPJET
C-----------------------------------------------------------------------
c
C!      treats jets ?
c
C!      Author : Allen Engelhardt
C------------------------------------------------------------------------
      INCLUDE 'DALI_OBJ.INC'
      REAL*8       P0,P1,P2,P3,PTOT2,PTOT,THETA,STH,
     &  PX,PY,PZ,PXYZ,PT,REF
      REAL    QAB(3,3),WR(3),WORK(3)    ! USED FOR F224/EISRS2
      CHARACTER *1 DT1                   ! ******************************
C      CHARACTER *2 DT2                   ! * CHARACTER CONVERSIONS FROM *
      CHARACTER *3 DT3,DT3Z              ! * DALBT.FOR                  *
      CHARACTER *4 DT4                   ! * CONVERTS REAL TO THE       *
C      CHARACTER *5 DT5                   ! * DESIRED CHARACTER LENGTH   *
C      CHARACTER *6 DT6                   ! *                            *
C      CHARACTER *7 DT7                   ! *                            *
C      CHARACTER *8 DT8                   ! ******************************
      CHARACTER *49 TJE(2)               ! TEXT FOR THE JE COMMAND

C                          1         2         3         4
C                 1234567890123456789012345678901234567890123456789
      DATA TJE / '123 objects (123 123 123 123) used for jet 123.  ' ,
     &           'Mass=1234 Q=12 P=1234 Pt=1234 Etot=1234 Ob.No=123' /


      INCLUDE 'DALI_OBD.INC'
C    ---------------------------------------------------JE: Create a JEt

      P0 = 0.0D0                        ! INITIALIZE VARIABLES
      P1 = 0.0D0                        ! FOR ACCUMULATION:
      P2 = 0.0D0                        ! Pi IS FOUR-MOMENTUM
      P3 = 0.0D0
      Q  = 0.0                          ! Q IS CHARGE.
      JET = 0                           ! INDEX TO NSOBJ
      REF = 0.0D0
      DO 132 I = 1 , MAXNO
        NSOBJ(I) = 0
  132 CONTINUE
C     ----------------------------- INITALIZATION FOR F224/EISRS FOLLOWS
      DO 142 I = 1 , 3
        DO 133 J = 1 , 3
          QAB(J,I) = 0.0
  133   CONTINUE
        WR(I) = 0.0
        WORK(I) = 0.0
  142 CONTINUE
      IERR = 0
      DO 134 I = 1 , MAXNO
        IF ( OBJRE(I).GT.REF ) THEN
          REF = INT(OBJRE(I))
        ENDIF
        IF ( OBJSO(I).LE.0.0D0 ) THEN
          GOTO 134
        ENDIF
        IF ( OBJUS(I).LE.0.0D0 ) THEN
          GOTO 134
        ENDIF
        JET = JET + 1
        NSOBJ(JET) = OBJNO(I)
        PX = OBJPX(I)
        PY = OBJPY(I)
        PZ = OBJPZ(I)
        P0 = P0 + DBLE(OBJET(I))
        P1 = P1 + PX
        P2 = P2 + PY
        P3 = P3 + PZ
        Q  = Q  + REAL(OBJQ(I))
        QAB(1,1) = QAB(1,1) + REAL( PX*PX )
        QAB(2,1) = QAB(2,1) + REAL( PY*PX )
        QAB(3,1) = QAB(3,1) + REAL( PZ*PX )
        QAB(1,2) = QAB(1,2) + REAL( PX*PY )     ! SINCE THE MATRIX
        QAB(2,2) = QAB(2,2) + REAL( PY*PY )     ! IS SYMMETRIC, THIS
        QAB(3,2) = QAB(3,2) + REAL( PZ*PY )     ! IS RATHER STUPID. :-)
        QAB(1,3) = QAB(1,3) + REAL( PX*PZ )
        QAB(2,3) = QAB(2,3) + REAL( PY*PZ )
        QAB(3,3) = QAB(3,3) + REAL( PZ*PZ )
  134 CONTINUE
      IF ( JET.EQ.0 ) THEN
        CALL DWRT('PH: No objects were selected.')
        RETURN
      ENDIF
      CALL EISRS2(3,3,QAB,WR,IERR,WORK)  ! CALCULATE EIGENVALUES
      IF ( IERR.NE.0 ) THEN
        CALL DWRT('PH: *ERROR* Eigenvalues are in error')
      ENDIF
      CALL FLPSOR(WR,3)                 ! PROB. NOT NEEDED, BUT...
      ! ASCENDING ORDER: WR(1) <= WR(2) <= WR(3)
C     --------------------------------------------- OK TO CREATE JET NOW.
      NUMJ = NUMJ + 1
      NOBJ = NOBJ + 1
      LSTHI= LSTHI + 1
      LSTRE = LSTHI
      REF = REF + REFAD
      DO 136 IJ = 1 , MAXNO
        IF ( OBJUS(IJ).EQ.0.0D0 ) GOTO 137
  136 CONTINUE
      CALL DWRT('PH: Cannot handle more objects. Sorry.')
      RETURN
  137 CONTINUE
      DO 139 I = 1 , JET
        K = NSOBJ(I)
        DO 138 J = 1 , MAXNO
          IF ( OBJNO(J).EQ.K ) THEN
            OBJ(18,J) = LSTHI
            NSOBJ(I) = J                ! MAKES THINGS EASIER LATER
            GOTO 139
          ENDIF
  138   CONTINUE
  139 CONTINUE
      PTOT2 = P1*P1 + P2*P2 + P3*P3
      PTOT = SQRT(PTOT2)
      THETA = ACOS(P3/PTOT)
      STH = SIN(THETA)
      RMA = REAL(SQRT(ABS(P0*P0 - PTOT2)))
      OBJ( 1,IJ) = REF + 2.0D0*REFAD          ! REFERENCED
      OBJ( 2,IJ) = 2.0D0                ! TYPE
      OBJ( 3,IJ) = DBLE(LSTHI)           ! NUMBER
      OBJ( 4,IJ) = 1.0D5*PTOT*STH/BFIELD/CLGHT ! RHO
      OBJ( 5,IJ) = THETA                ! THETA
      OBJ( 6,IJ) = DIPANG(P1,P2)
      OBJ( 7,IJ) = -9.999D9            ! D0 (?)
      OBJ( 8,IJ) = -9.999D9            ! Z0 (?)
      OBJ( 9,IJ) = -9.999D9            ! ALPHA (?)
      OBJ(10,IJ) = PTOT                 ! MOMENTUM

      DO 140 I = 1 , JET
        PX = OBJPX(NSOBJ(I))
        PY = OBJPY(NSOBJ(I))
        PZ = OBJPZ(NSOBJ(I))
        PXYZ = SQRT( PX**2 + PY**2 + PZ**2 )
        PT = PXYZ * SIN( ACOS( ( P1*PX + P2*PY + P3*PZ ) / ( PTOT*
     &    PXYZ ) ) )                    ! THERE HAS TO BE AN EASIER WAY
        IF ( OBJAO(NSOBJ(I)).GT.0.0D0 ) OBJ(26,IJ) = +1.0D0    !
        ! ASSOCIATED OBJECTS
        OBJ(11,NSOBJ(I)) = PT
        OBJ( 1,NSOBJ(I)) = REF + REFAD
  140 CONTINUE

      OBJ(12,IJ) = P1                   ! PX
      OBJ(13,IJ) = P2                   ! PY
      OBJ(14,IJ) = P3                   ! PZ
      OBJ(15,IJ) = DBLE(Q)              ! CHARGE
      OBJ(16,IJ) = DBLE(RMA)            ! MASS
      OBJ(17,IJ) = P0                   ! E-TOT
      OBJ(18,IJ) = 0.0D0                ! JET NUMBER
      OBJ(19,IJ) = -1.0D0               ! PARENT
      OBJ(20,IJ) = -1.0D0               ! CAL-OBJ NO
      OBJ(21,IJ) = -1.0D0               ! RELATION BITS (!)
      OBJ(22,IJ) = -1.0D0               ! ECAL NO
      OBJ(23,IJ) = -1.0D0               ! HCAL NO
      OBJ(24,IJ) = 0.0D0                ! SELECTED OBJECT
      OBJ(25,IJ) = 1.0D0                ! USED OBJECT
      RTSUM = WR(1) + WR(2) + WR(3)
      OBJ(27,IJ) = DBLE( WR(1) / RTSUM )
      OBJ(28,IJ) = DBLE( WR(2) / RTSUM )
      OBJ(29,IJ) = DBLE( WR(3) / RTSUM )
      TJE(2)(06:09) = DT4(REAL(RMA))
      IF ( Q.GT.0.0 ) THEN
        TJE(2)(13:13) = '+'
      ELSEIF ( Q.LT.0.0 ) THEN
        TJE(2)(13:13) = '-'
      ELSE
        TJE(2)(13:13) = ' '
      ENDIF
      TJE(2)(14:14) = DT1(ABS(Q))
      TJE(2)(18:21) = DT4(REAL(PTOT))
      TJE(2)(26:29) = DT4(REAL(PT))
      TJE(2)(36:39) = DT4(REAL(P0))
      TJE(2)(47:49) = DT3(REAL(LSTHI))

      CALL DWRT(TJE(2))

      TJE(1)(1:3) = DT3(REAL(JET))
      IF ( JET.GT.4 ) THEN
C                        123456789012345
        TJE(1)(14:28) = 'to many to list'
      ELSEIF ( JET.EQ.1 ) THEN
        TJE(1)(14:16) = DT3(REAL(OBJNO(NSOBJ(1))))
        TJE(1)(18:28) = '           '
        TJE(1)(17:17) = ' '
        TJE(1)(25:25) = ' '
      ELSEIF ( JET.EQ.2 ) THEN
        TJE(1)(14:16) = DT3(REAL(OBJNO(NSOBJ(1))))
        TJE(1)(18:20) = DT3(REAL(OBJNO(NSOBJ(2))))
        TJE(1)(22:28) = '       '
        TJE(1)(17:17) = ' '
        TJE(1)(25:25) = ' '
      ELSEIF ( JET.EQ.3 ) THEN
        TJE(1)(14:16) = DT3(REAL(OBJNO(NSOBJ(1))))
        TJE(1)(18:20) = DT3(REAL(OBJNO(NSOBJ(2))))
        TJE(1)(22:24) = DT3(REAL(OBJNO(NSOBJ(3))))
        TJE(1)(26:28) = '   '
        TJE(1)(17:17) = ' '
        TJE(1)(25:25) = ' '
      ELSEIF ( JET.EQ.4 ) THEN
        TJE(1)(14:16) = DT3(REAL(OBJNO(NSOBJ(1))))
        TJE(1)(18:20) = DT3(REAL(OBJNO(NSOBJ(2))))
        TJE(1)(22:24) = DT3(REAL(OBJNO(NSOBJ(3))))
        TJE(1)(26:28) = DT3(REAL(OBJNO(NSOBJ(4))))
        TJE(1)(17:17) = ' '
        TJE(1)(25:25) = ' '
      ENDIF
      TJE(1)(44:46) = DT3(REAL(LSTHI))

      CALL DWRT(TJE(1))

      END
C______________________________________________________________________
      SUBROUTINE DIPOB
C-----------------------------------------------------------------------
c
C!      creates object list for each new event
c
C!      Author : Allen Engelhardt
C------------------------------------------------------------------------
      PARAMETER (RMPI=0.139568)         !
      INCLUDE 'DALI_CF.INC'             ! NEED IEVTDE, IRUNDE
      INCLUDE 'A_BCS.INC'
      INCLUDE 'DALI_OBJ.INC'
      INCLUDE 'DALI_OBD.INC'
      INCLUDE 'A_BMACRO.INC'
C     ---------------------------------------------------  CHARGED TRACKS
      IF( IW(30).NE.12345 ) RETURN      ! ???
      JTRK = IW(NAMIND('FRFT'))
      IF ( JTRK.EQ.0 ) THEN
        JTRK = IW(NAMIND('PFRF'))
        IF ( JTRK.EQ.0 ) THEN
          CALL DWRT('PH: No tracks.')
          NUMT = 0
        ELSE
          NUMT = LROWS(JTRK)
          BBANK = .FALSE.
          CALL DWRT('PH: The FRFT bank is missing.')
        ENDIF
      ELSE
        BBANK = .TRUE.
        NUMT = LROWS(JTRK)
      ENDIF

      DO 1 I = 1 , NUMT
        RIR = -RTABL(JTRK,I,1)          ! INVERSE RADI
        RCS = SIGN(1.0,RIR)             ! CURVATURE + = COUNTER CLOCK
        TL  = RTABL(JTRK,I,2)           ! TAN(\LAMBDA)
        PHI = RTABL(JTRK,I,3)           ! PHI ZERO
        D0  = -RTABL(JTRK,I,4)*RCS      ! D ZERO
        Z0  = RTABL(JTRK,I,5)           ! Z ZERO
        ALP = RTABL(JTRK,I,6)           ! ALPHA
        RHO = 1.0 / ABS(RIR)
        PC  = BFIELD * RHO * CLGHT / 1.0E5
        THETA = ATAN2(1.0,TL)           ! ARCOT (-PI IF TL.LT.0)
        STH = SIN(THETA)
        PTOT = PC / STH
        PX = PTOT * STH * COS(PHI)
        PY = PTOT * STH * SIN(PHI)
        PZ = PTOT * COS(THETA)

        OBJ( 1,I) = 2.0D0*REFAD         ! NUMBER OF TIMES REF'ED
        OBJ( 2,I) = 1.0D0                 ! TYPE = TRACK
        OBJ( 3,I) = DBLE(I)             ! NUMBER = I
        OBJ( 4,I) = RHO                 ! RADIUS OF CURVATURE
        OBJ( 5,I) = THETA
        OBJ( 6,I) = PHI
        OBJ( 7,I) = D0
        OBJ( 8,I) = Z0
        OBJ( 9,I) = ALP                ! ALPHA
        OBJ(10,I) = PTOT               ! TOTAL MOMENTUM
        OBJ(11,I) = -1.0D0                ! TRANSVERSE MOMENTUM: ONLY
C                                         ! FOR JETS
        OBJ(12,I) = PX
        OBJ(13,I) = PY
        OBJ(14,I) = PZ
        OBJ(15,I) = RCS                 ! CHARGE
        OBJ(16,I) = RMPI                 ! MASS = MASS OF PION
        OBJ(17,I) = SQRT( PTOT*PTOT + RMPI*RMPI ) ! TOTAL ENERGY
        OBJ(18,I) = -1.0D0                ! JET NUMBER
        OBJ(19,I) = -1.0D0                ! PARENT
        OBJ(20,I) = -1.0D0                ! CAL OBJECT NUMBER
        OBJ(25,I) = +1.0D0
        IF ( RCS.LT.0 ) THEN
          TOBJ(I) = 'pi-'
        ELSE
          TOBJ(I) = 'pi+'
        ENDIF
    1 CONTINUE
C      --------------------------------------------NEUTRALS,   1: ECAL
      JPECO = IW(NAMIND('PECO'))
      IF ( JPECO.LE.0 ) THEN
        NUME = 0
      ELSE
        NUME = LROWS(JPECO)
      ENDIF

      JPCRL = IW(NAMIND('PCRL'))
      IF ( JPCRL.LE.0 ) THEN
        NUMRE = 0
      ELSE
        NUMRE = LROWS(JPCRL)
      ENDIF

      IE = NUMT

      DO 3 I = 1 , NUME
        DO 2 II = 1 , NUMRE
          K = ITABL(JPCRL,II,2)         ! PECO ECAL-OBJ
          IF ( I.EQ.K ) THEN
            KKK = ITABL(JPCRL,II,3)     ! PFRF FITTED TRACK
            CALOB = REAL(ITABL(JPCRL,II,1))   ! CAL OBJ
            IF ( KKK.GT.0 ) THEN
              OBJ(26,KKK) = DBLE(CALOB)        ! NO ERROR CHECK HERE**
              NUME = NUME - 1             ! NUMBER OF ECAL OBJECTS
              GOTO 3
            ENDIF
            OBJ(20,IE+1) = DBLE(CALOB)
            GOTO 2111
          ENDIF
    2   CONTINUE
 2111   CONTINUE
C    -------------------------------------------------  A 'TRUE' NEUTRAL
        THETA = RTABL(JPECO,I,4)
        PHI = RTABL(JPECO,I,5)
        EC = RTABL(JPECO,I,6)
        STH = SIN(THETA)
        PTOT = EC
        PX = PTOT * STH * COS(PHI)
        PY = PTOT * STH * SIN(PHI)
        PZ = PTOT * COS(THETA)
        IE = IE + 1
        OBJ( 1,IE) = REFAD
        OBJ( 2,IE) = 4.0D0
        OBJ( 3,IE) = DBLE(IE)
        OBJ( 4,IE) = 0.0D0
        OBJ( 5,IE) = THETA
        OBJ( 6,IE) = PHI
        OBJ( 7,IE) = -1.0D6             ! D0
        OBJ( 8,IE) = -1.0D6             ! Z0
        OBJ( 9,IE) = -1.0D6             ! ALPHA
        OBJ(10,IE) = PTOT
        OBJ(11,IE) = -1.0D0               ! P-TRANSVERSE
        OBJ(12,IE) = PX
        OBJ(13,IE) = PY
        OBJ(14,IE) = PZ
        OBJ(15,IE) = 0.0D0                ! CHARGE
        OBJ(16,IE) = 0.0D0                ! MASS
        OBJ(17,IE) = EC
        OBJ(18,IE) = -1.0D0               ! JET NUMBER
        OBJ(19,IE) = -1.0D0               ! PARENT
        OBJ(20,IE) = DBLE(ITABL(JPECO,I,10)) ! CAL OBJECT NUMBER
        OBJ(21,IE) = DBLE(ITABL(JPECO,I,9))  ! RELATION BITS
        OBJ(22,IE) = DBLE(I)           ! ECAL OBJECT NUMBER
        OBJ(23,IE) = -1.0D0               ! HCAL OBJECT
        OBJ(25,IE) = +1.0D0
        TOBJ(IE) = 'gamma'
    3 CONTINUE
C     --------------------------------------------- NEUTRALS,   2: HCAL
      JPHCO = IW(NAMIND('PHCO'))
      IF ( JPHCO.LE.0 ) THEN
        NUMH = 0
      ELSE
        NUMH = LROWS(JPHCO)
      ENDIF
      IH = NUMT + NUME
      DO 6 I = 1 , NUMH
        DO 4 II = 1 , NUMRE
          K = ITABL(JPCRL,II,4)         ! HCAL-OBJ#
          IF ( I.EQ.K ) THEN
            KK = ITABL(JPCRL,II,3)      ! TRACK#
            CALOB = REAL(ITABL(JPCRL,II,1))
            IF ( KK.GT.0 ) THEN
              OBJ(26,KK) = DBLE(CALOB)
              NUMH = NUMH - 1
              GOTO 6
            ENDIF
            OBJ(20,IH+1) = DBLE(CALOB)
            GOTO 4111
          ENDIF
    4   CONTINUE
 4111   CONTINUE

        THETA = RTABL(JPHCO,I,2)
        PHI = RTABL(JPHCO,I,3)
        EC = RTABL(JPHCO,I,4)
        STH = SIN(THETA)
        PTOT = EC
        PX = PTOT * STH * COS(PHI)
        PY = PTOT * STH * SIN(PHI)
        PZ = PTOT * COS(THETA)
        IH = IH + 1
        OBJ( 1,IH) = REFAD
        OBJ( 2,IH) = 5.0D0                ! TYPE
        OBJ( 3,IH) = DBLE(IH)
        OBJ( 4,IH) = 0.0D0                ! RHO
        OBJ( 5,IH) = THETA
        OBJ( 6,IH) = PHI
        OBJ( 7,IH) = -1.0D7             ! D0
        OBJ( 8,IH) = -1.0D7             ! Z0
        OBJ( 9,IH) = -1.0D7             ! ALPHA
        OBJ(10,IH) = PTOT
        OBJ(11,IH) = -1.0D0               ! P-TRANSVERSE
        OBJ(12,IH) = PX
        OBJ(13,IH) = PY
        OBJ(14,IH) = PZ
        OBJ(15,IH) = 0.0D0                ! CHARGE
        OBJ(16,IH) = 0.0D0               ! MASS
        OBJ(17,IH) = EC                 ! TOT. ENERGY
        OBJ(18,IH) = -1.0D0               ! JET NUMBER
        OBJ(19,IH) = -1.0D0               ! PARENT
        OBJ(20,IH) = DBLE(ITABL(JPHCO,I,9))   ! CAL OBJECT #
        OBJ(21,IH) = DBLE(ITABL(JPHCO,I,7))   ! RELATION BITS
        OBJ(22,IH) = -1.0D0               ! ECAL NUMBER
        OBJ(23,IH) = DBLE(I)           ! HCAL NUMBER
        OBJ(25,IH) = +1.0D0
        TOBJ(IH) = 'gamma'
    6 CONTINUE

      NOBJ = NUMT + NUME + NUMH
      LSTHI = NOBJ                      ! LAST HIGH
      LSTRE = 0                         ! LAST REFERENCED

C     -----------------------------------------  LIST OF OBJECTS IS DONE.

      END
      SUBROUTINE DIPCHG(TANSW,TRAK,INUM,P1,P2,P3,P4)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- Allan Engelhardt      12-JUL-1990
C!   Modified :- Allan Engelhardt      28-AUG-1990  Almost final version.
C!
C!   Inputs:
C!        - INUM : The number of the changed parameter.
C!        - Pn   : Parameter from DOPER
C!
C!   Outputs:
C!        - Changes the relevant commons
C!
C!   Libraries required:
C!        KERNLIB (M433/CLTOU)
C!
C!   Description
C!   ===========
C!
C?      Handles all the DOPER commands.
C!======================================================================
C INVOKED BY TANSW.EQ.'PH' (DIPCHG)
*IF .NOT.DOC
      ENTRY DIPSO(TANSW,TRAK,INUM,P1,P2,P3,P4)
      ENTRY DIPUS(TANSW,TRAK,INUM,P1,P2,P3,P4)
      INCLUDE 'DALI_CF.INC'             ! STANDARD DALI INCLUDE MODULE.
      ! *NOTE* THAT THE MODULE IS
      ! TAKEN FROM THE DEFAULT
      ! DIRECTORY.
      INCLUDE 'A_BCS.INC'
      INCLUDE 'DALI_OBJ.INC'
      PARAMETER (NTRK=9)
      DIMENSION TRAK(NTRK)
C      CHARACTER *1 DT1                   ! ******************************
      CHARACTER *2 DT2                   ! * CHARACTER CONVERSIONS FROM *
      CHARACTER *3 DT3,DT3Z              ! * DALBT.FOR                  *
C      CHARACTER *4 DT4                   ! * CONVERTS REAL TO THE       *
      CHARACTER *5 DT5                   ! * DESIRED CHARACTER LENGTH   *
C      CHARACTER *6 DT6                   ! *                            *
C      CHARACTER *7 DT7                   ! *                            *
C      CHARACTER *8 DT8                   ! ******************************
      CHARACTER *4 CHAINT
C      CHARACTER *49 TSO,TCO,TUS,TDI(7)
      CHARACTER *49 TSO,TCO,TUS
C      CHARACTER *132 TUSER
      CHARACTER *49 THY(3)               ! TEXT FOR THE HY COMMAND
      CHARACTER *12 THYIN,THYRD,THYCA    ! INPUT FOR THE HY COMMAND
      CHARACTER *49 TPA(2)               ! TEXT FOR THE PA COMMAND
      CHARACTER *49 TXA(1)               ! TDA/TLA
      CHARACTER *49 TJB(2)               ! TEXT FOR THE JB COMMAND
      CHARACTER *49 TSL(1)               ! TEXT FOR THE SL COMMAND
      CHARACTER *2 TANSW
      INTEGER NSO                       ! NUMBER SELECTED OBJECTS
      INTEGER NARR(3,MAXNO)
      REAL PP

C                 1234567890123456789012345678901234567890123456789
      DATA TSL / 'Suggestion limit was 12345%. New limit is 12345% ' /
      DATA TJB / 'Object number 123 was in jet 123. New jet        ' ,
     &           'Objct number 123 is not a jet. Try another.      ' /
      DATA TSO / 'PH: Object 123 is selected. 123 objects selected.' /
      DATA TCO / 'PH: Text Colour changed to 12.                   ' /
      DATA TUS / 'PH: Object 123 is no longer selected.            ' /
C                 1234567890123456789012345678901234567890123456789
C      DATA TDI / 'Object no. 123  Type: x Jet no. 123 Parent 123   ' ,
C     &           'Ptot=123456 Etot=123456 Q=12 Mass=123456         ' ,
C     &           'Pxy =123456 Hypothesis : xxxxxxxxxxxx            ' ,
C     &           'Px=123456 Py=123456 Pz=123456 Ptransverse=123456 ' ,
C     &           'Rho=12345 Phi0=1234 D0=1234 Z0=1234 Alpha=12345  ' ,
C     &           'Cal Object no. 123 Ecal obj. 123 Hcal obj. 123   ' ,
C     &           'PH: No object number 123 in list.                ' /
      DATA THY / 'PH: No object 123 avaible.                       ' ,
     &           'Object 123 New hypothesis (CR = xxxxxxxxxxxx) ?  ' ,
     &           'Object 123 was xxxxxxxxxxxx is now xxxxxxxxxxxx. ' /
      DATA TPA / 'PH: There is no object number 123.               ' ,
     &           'PH: Object number 123. Parent was 123. New parent' /
      DATA TXA / 'PH: No object 123 is avaible.                    ' /
      INCLUDE 'DALI_OBD.INC'
      INCLUDE 'A_BMACRO.INC'
C------------------------------------------------------------------------
      IF (TANSW.EQ.'PT'.OR.TANSW.EQ.'ST') GOTO 100
      IF (TANSW.EQ.'ET') GOTO 120

C            SO  TC  US  DI  RO  HY  PA  SU  LA  DA  JB  SL
      GOTO (100,110,120,130,140,150,170,180,190,195,200,270) INUM
      CALL DWRT('PH: Unknown parameter.')
      RETURN
C     -----------------------------------------------   SO: Select Object
  100 CONTINUE
      IF (TANSW.EQ.'PT'.OR.TANSW.EQ.'ST') THEN
        PP=TRAK(1)
      ELSE
        PP=P2
      ENDIF
      IF ( PP.LE.0.0 ) RETURN
      DO 101 I = 1 , MAXNO
        NUM = OBJNO(I)
        IF ( NUM.EQ.PP ) THEN
          OBJ(24,I) = 1.0
          OBJ( 1,I) = OBJ( 1,I) + REFAD
          GOTO 102
        ENDIF
  101 CONTINUE
  102 CONTINUE
      NSO = 0
      DO 103 II = 1 , MAXNO              ! COUNT NUMBER OF
        IF ( OBJSO(II) .GT. 0 ) THEN     ! SELECTED OBJECTS.
          NSO = NSO + 1
        ENDIF
  103 CONTINUE
      LSTRE = INT(PP)                   ! SAVE LAST REFERENCED.
      TSO(12:14) = DT3(REAL(PP))
      TSO(29:31) = DT3(REAL(NSO))
      CALL DWRT(TSO)
      IF ( BLIST ) CALL DIPD      ! LO
      IF (TANSW.EQ.'PT'.OR.TANSW.EQ.'ST') RETURN
      IF ( OBJ(2,I).NE.1 ) RETURN
      TRNEW=PP
      TANSW='ST'
      CALL DPITRK(TANSW,TRNEW,NTRK,TRAK,FOUND,ETRAK)
      RETURN                          ! RETURN
C     --------------------------------------------------   TC: Text Colour
  110 CONTINUE
      KOLOR = MOD(INT(P2),16)
      TCO(28:29) = DT2(REAL(KOLOR))
      CALL DWRT(TCO)
      IF ( BLIST ) CALL DIPD      ! LO
      RETURN                          ! RETURN
C     -------------------------------------------   US: Un-Select an object.
  120 CONTINUE
      IF ( P2.EQ.0.0 ) RETURN
      DO 121 I = 1 , MAXNO
        NUM = INT(OBJNO(I))
        IF ( NUM.EQ.P2 ) THEN
          OBJ(24,I) = -ABS(OBJ(24,I))
          REF = OBJ( 1,I)
          IF (REF.GT.REFAD) OBJ( 1,I) = REF - REFAD
          GOTO 122
        ENDIF
  121 CONTINUE
  122 CONTINUE
      IF ( LSTRE.EQ.INT(P2) ) LSTRE = 0
      TUS(12:14) = DT3(REAL(P2))
      CALL DWRT(TUS)
      IF ( BLIST ) CALL DIPD      ! LO
      IF (TANSW.EQ.'ET') RETURN
      TRNEW=P2
      TANSW='ET'
      CALL DPITRK(TANSW,TRNEW,NTRK,TRAK,FOUND,ETRAK)
      RETURN                          ! RETURN
C     ------------------------------------   DI: Detailed object Information
  130 CONTINUE
      LSTRE = INT(P2)
      CALL DIPDI(P2)
      RETURN
C     --------------------------------------------------   RO: Remove Object
  140 CONTINUE
      CALL DIPRO(P2)
      RETURN
C     -------------------------------------------   HY: HYpothesis for object
  150 CONTINUE
      CALL DQHLP('PY ')
      TAN1DH='  '
      ION = INT(P2)
      IF ( ION.EQ.0 ) THEN
        CALL DWRT('PH: No object selected.')
        RETURN
      ENDIF
      THY(3)(16:27) = TOBJ(ION)
      DO 152 IJ = 1 , MAXNO
        IF ( OBJNO(IJ).EQ.ION ) GOTO 153
  152 CONTINUE
      THY(1)(15:17) = DT3(REAL(ION))
      CALL DWRT(THY(1))
      GOTO 169
  153 CONTINUE
      THY(2)(08:10) = DT3(REAL(ION))
      THY(2)(33:44) = TOBJ(OBJNO(IJ))
      CALL DWRT(THY(2))
      CALL DGETLN(THYIN,LCHLIN,12)      ! BJÖRN'S FANCY INPUT ROUTINE
      IF ( LCHLIN.LE.0 ) GOTO 1551
      IF ( THYIN.EQ.'            ' ) GOTO 1551
      DO 155 I = 1 , NPART
        WRITE(THYRD,1541) 
     &    CHAINT(ITABL(JPART,I,2)),
     &    CHAINT(ITABL(JPART,I,3)),
     &    CHAINT(ITABL(JPART,I,4))
 1541   FORMAT (A,A,A)
        THYCA = THYRD
        CALL CLTOU(THYRD)
        IF ( THYRD.EQ.THYIN ) GOTO 156
  155 CONTINUE
      CALL DWRT('PH: Hypothesis not found in bank:')
      CALL DWRT(THYIN)
      CALL DWRT('PH: Check spelling e.g. in the QPAR.BANK file.')
      GOTO 153
 1551 CALL DWRT('PH: Old values are retained.')
      GOTO 169
  156 CONTINUE
      RMASS = RTABL(JPART,I,6)
      Q = RTABL(JPART,I,7)
      TOBJ(OBJNO(IJ)) = THYCA
      OBJ( 1,IJ) = OBJ( 1,IJ) + REFAD
      OBJ(15,IJ) = DBLE(Q)
      OBJ(16,IJ) = DBLE(RMASS)
      OBJ(17,IJ) = SQRT(ABS(OBJP(IJ)**2+DBLE(RMASS)**2))

      CALL DIPRC                        ! RECALCULATE ALL

      THY(3)(08:10) = DT3(REAL(ION))
      THY(3)(36:47) = THYCA
      CALL DWRT(THY(3))

      LSTRE = INT(P2)                   ! SAVE REFERENCED

  169 CONTINUE
      IF ( BLIST ) CALL DIPD             ! LO
      RETURN
C     ------------------------------------------    PA: Change PArent number
  170 CONTINUE
      IOLD = INT(P2)
      IF ( IOLD.EQ.0 ) THEN
        RETURN
      ENDIF
      DO 172 IJ = 1 , MAXNO
        IF ( OBJNO(IJ) .EQ. DBLE(IOLD) ) GOTO 173
  172 CONTINUE
      TPA(1)(31:33)= DT3(REAL(IOLD))
      CALL DWRT(TPA(1))
      RETURN
  173 CONTINUE
      TPA(2)(19:21) = DT3(REAL(IOLD))
      TPA(2)(35:37) = DT3(REAL(OBJPA(IJ)))
 3753 CALL DWRT(TPA(2))
 1731 CALL DGTINT(INEW,II)
      IF(II.LT.0) GO TO 3753
      IF(II.EQ.0) INEW=0
  174 FORMAT (I3)
      IF ( INEW.EQ.IOLD ) THEN
        CALL DWRT('PH: An object cannot be its own parent.')
        GOTO 189
      ENDIF
C---------------------------------------------------*** CHECK PROPOSED DATA
      II = 0
      DO 1742 I = 1 , MAXNO
        IF ( OBJUS(I).GT.0.0D0 ) THEN
          II = II + 1
          NARR(1,II) = INT(OBJNO(I))
          NARR(2,II) = INT(OBJPA(I))
          NARR(3,II) = INT(OBJJE(I))
          IF ( OBJNO(I).EQ.DBLE(IOLD) ) NARR(2,II) = INEW
        ENDIF
 1742 CONTINUE
      N = DIPCHK(NARR,3,II,2,3)
      IF ( N.GT.0 ) THEN
C                   1234567890123456789012345678901234567890123456789
        CALL DWRT('PH: Loop in data detected. WILL NOT CHANGE PARENT')
        RETURN                          ! NOT OK => RETURN
      ENDIF
C     -------------------------------------------  *** PROPOSED DATA OK
      LSTRE = INT(P2)
      IF ( INEW.EQ.0 ) THEN
        OBJ(19,IJ) = 0.0D0
      ELSE
        DO 175 I = 1 , MAXNO
          IF ( OBJNO(I).EQ.DBLE(INEW) ) GOTO 176
  175   CONTINUE
        TPA(1)(31:33)= DT3(REAL(IOLD))
        CALL DWRT(TPA(1))
        RETURN
  176   CONTINUE
        OBJ(19,IJ) = DBLE(INEW)          ! !***
      ENDIF

      CALL DIPRC                        ! RECALCULATE ALL
  189 CONTINUE
      IF ( BLIST ) THEN
        CALL DIPD
      ENDIF
      RETURN
C1730 CONTINUE
C     ---------------------------------------   SU: SUggestion for an object.
  180 CONTINUE
      LSTRE = INT(P2)
      CALL DIPSU(P2)
      RETURN
C     -------------------------------------   LA: List associated information
  190 CONTINUE
      INO = INT(P2)
      DO 192 IND = 1 , MAXNO
        IF ( INO.EQ.INT(OBJNO(IND)) ) GOTO 193
  192 CONTINUE
      TXA(1)(25:27) = DT3( FLOAT(INO) )
      CALL DWRT( TXA(1) )
      RETURN
  193 CONTINUE
      CALL DIPDA( IND,.FALSE. )
      LSTRE = INO
      RETURN
C     ------------------------------------   DA: Display associated information
  195 CONTINUE
      INO = INT(P2)
      DO 197 IND = 1 , MAXNO
        IF ( INO.EQ.INT(OBJNO(IND)) ) GOTO 198
  197 CONTINUE
      TXA(1)(25:27) = DT3( FLOAT(INO) )
      CALL DWRT( TXA(1) )
      RETURN
  198 CONTINUE
      CALL DIPDA( IND,.TRUE. )
      LSTRE = INO
      RETURN
C     ----------------------------------------------------- JB: Jet Belongings
  200 CONTINUE
      INO = INT(P2)
      DO 210 IJ = 1 , MAXNO
        IF ( INO.EQ.INT(OBJNO(IJ)) ) GOTO 220
  210 CONTINUE
      TPH(5)(22:24) = DT3(REAL(INO))
      CALL DWRT( TPH(5) )
      RETURN
  220 CONTINUE
      LSTRE = INO
      IOJE = INT( OBJJE(IJ) )
      TJB(1)(15:17) = DT3( REAL(INO) )
      TJB(1)(30:32) = DT3( REAL(IOJE) )
 3754 CALL DWRT( TJB(1) )
  230 CONTINUE
      CALL DGTINT(INJE,LINJE)
      IF(LINJE.LT.0) GO TO 3754
      IF(LINJE.EQ.0) INJE=0
 2301 FORMAT ( I3 )
      IF ( INJE.LT.0 ) THEN
        CALL DWRT('PH: Old values are retained.')
        RETURN
      ENDIF
      IF ( INJE.EQ.0 ) THEN
        OBJ(11,IJ) = 0.0D0               ! TRANSVERSE MOMENTUM
        OBJ(18,IJ) = 0.0D0               ! JET NUMBER
        GOTO 260
      ENDIF
      DO 240 I = 1 , MAXNO
        IF ( INJE.EQ.INT(OBJNO(I)) ) GOTO 250
  240 CONTINUE
      TPH(5)(22:24) = DT3(REAL(INJE))
      CALL DWRT( TPH(5) )
      CALL DWRT('PH: Please enter a new number.')
      GOTO 230
  250 CONTINUE
      IF ( OBJTY(I).NE.2.0D0 ) THEN
        TJB(2)(14:16) = DT3(REAL(INJE))
        CALL DWRT(  TJB(2) )
        GOTO 230
      ENDIF
      OBJ(18,IJ) = DBLE(INJE)

  260 CONTINUE
      CALL DIPRC
      IF ( BLIST ) THEN
        CALL DIPD
      ENDIF
      RETURN

C2309 CONTINUE
C     ------------------------------------------------   SL: Suggestion Limit
  270 CONTINUE
      IF ( P2.EQ.0.0 ) RETURN
      TSL(1)(22:26) = DT5(DMASS*100.0)
      DMASS = P2 / 100.0
      TSL(1)(43:47) = DT5(DMASS*100.0)
      CALL DWRT( TSL(1) )
      RETURN

      END
C______________________________________________________________________

      FUNCTION DIPON(N,M)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- Allan Engelhardt      27-JUL-1990
C!   Modified :- Allan Engelhardt       2-AUG-1990
C!   Modified :- Allan Engelhardt      28-AUG-1990  Almost final version.
C!
C!   Inputs:
C!        - N:  Value of NPIKDP
C!        - M:  Value of MDLPDP
C!
C!   Outputs:
C!        - DIPON=DIP-Object Number
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!
C?      Find the objectnumber of the picked object. New version.
C!======================================================================
*IF .NOT.DOC
      INCLUDE 'A_BCS.INC'
      INCLUDE 'DALI_CF.INC'
      INCLUDE 'DALI_OBJ.INC'
      REAL    OB                        ! OBJECTNUMBER
      DATA PHCODB / -1.0 / , DIP1DB / -2.0 / , PFRFDB / -3.0 / , DIP2DB
     &  / -3.0 /
      INCLUDE 'DALI_OBD.INC'
      INCLUDE 'A_BMACRO.INC'
C------------------------------------------------------------------------
      OB = -9999.0
      RN = FLOAT(N)
      RM = FLOAT(M)

      IF ( RM.EQ.FRFTDB ) THEN
        OB = RN
        GOTO 998
      ELSEIF ( RM.EQ.PFRFDB ) THEN
        OB = RN
        GOTO 998
      ELSEIF ( RM.EQ.PECODB ) THEN
        DO 10 I = 1 , MAXNO
          IF ( OBJEC(I).EQ.N ) GOTO 11
   10   CONTINUE
        GOTO 998
   11   CONTINUE
        OB = OBJNO(I)
        GOTO 998
      ELSEIF ( RM.EQ.PHCODB ) THEN
        DO 20 I = 1 , MAXNO
          IF ( OBJHC(I).EQ.N ) GOTO 21
   20   CONTINUE
        GOTO 998
   21   CONTINUE
        OB = OBJNO(I)
        GOTO 998
      ELSEIF ( RM.EQ.DIP1DB ) THEN
        OB = RN
      ELSEIF ( RM.EQ.TPCODB ) THEN
        IF ( BBANK ) THEN
          JTPCO = IW(NAMIND('TPCO'))
          OB = FLOAT(ABS(ITABL(JTPCO,N,8)))       ! TRACKNUMBER
          IF ( OB.EQ.0.0 ) OB = -9999.0   !
        ENDIF
        GOTO 998
      ELSEIF ( RM.EQ.DIP2DB ) THEN
        OB = RN
      ENDIF

  998 DIPON = OB
      END
C_______________________________________________________________________

      SUBROUTINE DIPD
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- Allan Engelhardt      31-JUL-1990
C!   Modified :- Allan Engelhardt      28-AUG-1990  Almost final version.
C!
C!   Inputs:
C!
C!   Outputs:
C!        - Selects the object
C!
C!   Libraries required: GENLIB  | M109/SORTD
C!
C!   Description
C!   ===========
C!
C?      handles PIcking, Moving, and display of objectlist.
C!======================================================================
*IF .NOT.DOC
      INCLUDE 'DALI_CF.INC'             ! STANDARD DALI INCLUDE MODULE.
      INCLUDE 'DALI_OBJ.INC'
C      CHARACTER *1 DT1                   ! ******************************
C      CHARACTER *2 DT2                   ! * CHARACTER CONVERSIONS FROM *
      CHARACTER *3 DT3,DT3Z              ! * DALBT.FOR                  *
      CHARACTER *4 DT4                   ! * CONVERTS REAL TO THE       *
C      CHARACTER *5 DT5                   ! * DESIRED CHARACTER LENGTH   *
C      CHARACTER *6 DT6                   ! *                            *
C      CHARACTER *7 DT7                   ! *                            *
C      CHARACTER *8 DT8                   ! ******************************
      CHARACTER *41 TLO(3),TLOD          ! TEXT FOR THE LO COMMAND
      INTEGER IRUN,IEVT                 ! RUN AND EVENT NUMBER
      REAL*8       PTOT,THETA,PHI,STH,PY,PZ,PA

      COMMON / DIPRUN / IRUN,IEVT       ! ALSO IN DIP

C                 12345678901234567890123456789012345678901
      DATA TLO / '-------- List of objects in event -------' ,
     &           'T###    Q P   Pz  Phi The Mass  Hypothe. ',
     &           'X123 123Q123  123 123 123 1234 1234567890'/
      DATA PHCODB / -1.0 / , DIP1DB / -2.0 / , PFRFDB / -3.0 / , DIP2DB
     &  / -3.0 / , II / 0 /
      DATA DH / 40.0 / , N7 / 7 / , N8 / 8 /

      INTEGER IWIW(0:12),IWIP(0:12)     ! WIDTH AND PAGE OF WINDOW
      DATA IWIW / 123,041,041,041,041,041,041,083,083,041,41,41,83 /
      DATA IWIP / 065,032,032,032,032,032,032,032,032,065,65,65,65 /
      DATA VSTEP / 10.0 /
      DATA DVMIN/0./
      INCLUDE 'DALI_OBD.INC'
C-----------------------------------------------------------------------
      CALL DSCTR
      IF ( BCOLO ) GOTO 5
        DO 6  IJ=1,MAXNO
          IF ( (OBJ(2,IJ).EQ.1.) .AND. (NCTRDC(OBJ(3,IJ)).EQ.0.) ) THEN
            BCOLO=.TRUE.
            CALL DWRT('PH:no change of colours for B/W screen !')
            RETURN
          ELSEIF ( II.EQ.3 ) THEN
            GOTO 5
          ENDIF
        II=II+1
    6   CONTINUE
C     --------------------------------***CHECK IF RUN HAS CHANGED. AS IN DIP
    5 IF (.NOT.( (IEVT.EQ.IEVTDE(1)).AND.(IRUN.EQ.IRUNDE(1)) )) THEN
        IEVT = IEVTDE(1)
        IRUN = IRUNDE(1)
        CALL DIP0
      ENDIF
      VH = VHGHDG(IAREDO)               ! I DON'T KNOW :-(
      VM = VMINDG(IAREDO)+0.5*DVMIN     ! I DON'T KNOW :-(
      HM = HMINDG(IAREDO)               ! I DON'T KNOW :-(
      VH = VH - 4.0                     ! BETTER FOR PI AND MO (?)
      VHM = VH - 2.5*VSTEP               ! MIDDLE OF LINE

      IF ( .NOT.FPIKDP ) GOTO 800       ! RETURN
      IF ( FPIMDP ) GOTO 500            ! MOVE
C     -----------------------------------------------------------   PICKING
      N = MAX ( 1 , INT ( ( VHM - VPIKDP ) / VSTEP ) + 1 )
      OB = DBLE(IOBJP(N,IAREDO))              ! OBJECT NUMBER
      DO 10 IJ = 1 , MAXNO
        IF ( OBJNO(IJ).EQ.OB ) GOTO 11
   10 CONTINUE
      CALL DWRT('PH: Internal error in objectnumbers.')
   11 CONTINUE
      IF ( OBJTY(IJ).EQ.1.0 ) THEN
        IJETN = INT(OBJJE(IJ))
        IF ( BJET.AND.(IJETN.GT.0) ) THEN
          MDLPDP = DIP2DB
          NPIKDP = IJETN
          DO 20 K = 1 , IWIP(IAREDO)
            IF ( IOBJP(K,IAREDO).EQ.IJETN ) GOTO 21
   20     CONTINUE
          CALL DWRT('Jet is not on this picture.')
          GOTO 80
   21     CONTINUE                      ! JET IS ON PICTURE
          N = K
          GOTO 80
        ELSE
          IF ( BBANK ) THEN
            MDLPDP = FRFTDB
          ELSE
            MDLPDP = PFRFDB
          ENDIF
          NPIKDP = INT(OBJNO(IJ))
        ENDIF
      ELSEIF ( OBJTY(IJ).EQ.4.0D0 ) THEN
        MDLPDP = PECODB
        NPIKDP = OBJEC(IJ)
      ELSEIF ( OBJTY(IJ).EQ.5.0D0 ) THEN
        MDLPDP = PHCODB
        NPIKDP = OBJHC(IJ)
      ELSEIF ( OBJTY(IJ).EQ.3.0D0 ) THEN
        MDLPDP = DIP1DB
        NPIKDP = OBJNO(IJ)
      ELSEIF ( OBJTY(IJ).EQ.2.0D0 ) THEN
        MDLPDP = DIP2DB
        NPIKDP = OBJNO(IJ)
      ELSE
      ENDIF

   80 CONTINUE
      DPIKDP = 0.0
  100 CONTINUE
      HHPKDP = HM + DH
      VVPKDP = VHM - ( N-1 )*VSTEP

      RETURN
C     ---------------------------------------------------------   MOVE
  500 CONTINUE
      DPIKDP = 9999.0
      ITW = LEN(TLO(1))
      OB = -1.0
      OB = DIPON(NPIKDP,MDLPDP)
      IF ( BJET.AND.(OBJJE(INT(OB)).GT.0.0D0) ) THEN
        OB = REAL( OBJJE(INT(OB)) )
      ENDIF
      IMQQQ = IWIP(IAREDO) - 2
      IF ( IAREDO.EQ.12.OR.IAREDO.EQ.0 ) IMQQQ = IMQQQ * 2
      DO 530 I = 1 , IMQQQ
        IF ( IOBJP(I,IAREDO).EQ.INT(OB) ) GOTO 531
  530 CONTINUE
      CALL DWRT('Object not found on this display.')
      RETURN
  531 CONTINUE
      DPIKDP = 0.0
      HHPKDP = HM + DH
      VVPKDP = VHM - ( I-1 )*VSTEP - 1.0
      IF ( I.GT.(IWIP(IAREDO)-2) ) HM = HM + 8*(ITW)

      RETURN
C     ----------------------------------------------------   DRAW PICTURE
  800 CONTINUE
C       FIRST SORT MATRIX.
      IF (BSORT) CALL SORTD(OBJ,NOBJI,MAXNO,ISORT*ISOAF)     ! PRESERVE THE
                                                      ! ORDERING OF EQUAL LINES.
  801 CONTINUE
      CALL SORTD(OBJ,NOBJI,MAXNO,-25)   ! OUT WITH NOT USED.
      VH = VHGHDG(IAREDO)               ! I DON'T KNOW :-(
      VM = VMINDG(IAREDO)+0.5*DVMIN     ! I DON'T KNOW :-(
      HM = HMINDG(IAREDO)               ! I DON'T KNOW :-(
      VH = VH - 5.0
      IPAGE = IWIP(IAREDO)              ! PAGE LENGTH
      IWDTH = IWIW(IAREDO)              ! PAGE WIDTH
      ITW = LEN(TLO(1))
      ILOOP = MIN(IPAGE-2,NOBJ)
      IF ( ILOOP.EQ.0 ) THEN
        CALL DWRT('PH: No objects to display.')
        RETURN                        ! RETURN
      ENDIF

      DO 805 I = 1 , MAXNO
        IOBJP(I,IAREDO) = INT(OBJNO(I))      ! SAVE THE ORDER
  805 CONTINUE

      CALL DQWIL(0.0)
      CALL DQCL(IAREDO)                ! CLEAR DISPLAY
      CALL DGLEVL(KOLOR)

      VH = VH - VSTEP                    ! MOVE DOWN A BIT
      CALL DGTEXT(HM,VH,TLO(1),ITW)
      VH = VH - VSTEP
      CALL DGTEXT(HM,VH,TLO(2),ITW)
      DO 810 I = 1 , ILOOP
        TLOD = TLO(3)
        TLOD(1:1) = TOTY(INT(OBJTY(I)+1.0))
        TLOD(2:4) = DT3(REAL(OBJNO(I)))
        IF ( BSTAR.AND.(OBJSO(I).EQ.1.0) ) TLOD(5:5) = '*'
        PA = OBJPA(I)
        IF ( PA.GT.0.0 ) THEN
          TLOD(6:8) = DT3(REAL(PA))
        ELSE
          TLOD(6:8) = '   '
        ENDIF
        Q = REAL(OBJQ(I))
        IF ( Q.EQ.0.0 ) THEN
          TLOD(9:9) = ' '
        ELSEIF ( Q.EQ.1.0 ) THEN
          TLOD(9:9) = '+'
        ELSEIF ( Q.EQ.-1.0 ) THEN
          TLOD(9:9) = '-'
        ELSEIF ( Q.EQ.-2.0 ) THEN
          TLOD(9:9) = '='
        ELSEIF ( Q.EQ.+2.0 ) THEN
          TLOD(9:9) = '#'
        ELSE
          TLOD(9:9) = '*'
        ENDIF
        PTOT = OBJP(I)
        PY = OBJPY(I)
        PZ = OBJPZ(I)
        THETA = ACOS(PZ/PTOT)
        STH = SIN(THETA)
        PHI = OBJP0(I)
        THETA = THETA / 3.14159265D0 * 180.0D0
        PHI = PHI / 3.14159265D0 * 180.0D0
        IF ( PHI.LT.0.0D0 ) PHI = PHI + 360.0D0
        TLOD(10:12) = DT3(PTOT)
        IF ( PZ.GT.0.0D0 ) THEN
          TLOD(14:14) = '+'
        ELSEIF ( PZ.LT.0.0D0 ) THEN
          TLOD(14:14) = '-'
        ELSE
          TLOD(14:14) = ' '
        ENDIF
        TLOD(15:17) = DT3(REAL(ABS(PZ)))
        TLOD(19:21) = DT3(REAL(NINT(PHI)))
        TLOD(23:25) = DT3(REAL(NINT(THETA)))
        TLOD(27:30) = DT4(REAL(OBJM(I)))
        TLOD(32:41) = TOBJ(INT(OBJNO(I)))(1:10)
        IF ( TOBJ(INT(OBJNO(I)))(11:12) .EQ. '  ' ) THEN
          CONTINUE
        ELSE
          TLOD(41:41) = '>'
        ENDIF
        VH = VH - VSTEP
        IF ( BCOLO ) GOTO 806
        IF ( OBJTY(I).GE.4.0D0 ) THEN
          CALL DGLEVL(N7)
          CALL DGTEXT(HM,VH,TLOD,ITW)
          CALL DGLEVL( KOLOR )
        ELSEIF ( OBJTY(I).EQ.3.0D0 ) THEN
          CALL DGLEVL(N8)
          CALL DGTEXT(HM,VH,TLOD,ITW)
          CALL DGLEVL( KOLOR )
        ELSEIF ( OBJTY(I).EQ.2.0D0 ) THEN
          CALL DGLEVL(N8)
          CALL DGTEXT(HM,VH,TLOD,ITW)
          CALL DGLEVL( KOLOR )
        ELSE
          CALL DGLEVL(NCTRDC(OBJNO(I)))
          CALL DGTEXT(HM,VH,TLOD,ITW)
        ENDIF
        GOTO 810
  806   CONTINUE
        IF ( OBJSO(I).GT.0.0D0 ) THEN
          CALL DGLEVL( MOD(KOLOR-4,8)+8 )
          CALL DGTEXT(HM,VH,TLOD,ITW)
          CALL DGLEVL(KOLOR)
       ELSEIF ( OBJTY(I).GE.4.0D0 ) THEN
          CALL DGLEVL(MOD(KOLOR-7,8)+8 )
          CALL DGTEXT(HM,VH,TLOD,ITW)
          CALL DGLEVL( KOLOR )
        ELSEIF ( OBJTY(I).EQ.3.0D0 ) THEN
          CALL DGLEVL(MOD(KOLOR-6,8)+8 )
          CALL DGTEXT(HM,VH,TLOD,ITW)
          CALL DGLEVL( KOLOR )
        ELSEIF ( OBJTY(I).EQ.2.0D0 ) THEN
          CALL DGLEVL( MOD(KOLOR-5,8)+8 )
          CALL DGTEXT(HM,VH,TLOD,ITW)
          CALL DGLEVL( KOLOR )
        ELSE
          CALL DGTEXT(HM,VH,TLOD,ITW)
        ENDIF
  810 CONTINUE
      IF ( ( IWDTH.GT.(2*ITW) ) .AND. ( NOBJ.GT.(IPAGE-2) ) ) THEN
        VH = VHGHDG(IAREDO) - 5.0               ! RESTORE
        HM = HM + 8*(ITW)               ! HOPE THIS IS RIGHT.
        ILOOP = MIN(IPAGE-3,NOBJ-IPAGE+1)
        IOLD = I
        VH = VH - VSTEP                    ! MOVE DOWN A BIT
        CALL DGLEVL( KOLOR )
        CALL DGTEXT(HM,VH,TLO(1),ITW)
        VH = VH - VSTEP
        CALL DGTEXT(HM,VH,TLO(2),ITW)
        DO 820 I = IOLD , ILOOP+IOLD
          TLOD = TLO(3)
          TLOD(1:1) = TOTY(INT(OBJTY(I)+1.0))
          TLOD(2:4) = DT3(REAL(OBJNO(I)))
          IF ( BSTAR.AND.(OBJSO(I).EQ.1.0) ) TLOD(5:5) = '*'
          PA = OBJPA(I)
          IF ( PA.GT.0.0 ) THEN
            TLOD(6:8) = DT3(REAL(PA))
          ELSE
            TLOD(6:8) = ' - '
          ENDIF
          Q = REAL(OBJQ(I))
          IF ( Q.EQ.0.0 ) THEN
            TLOD(9:9) = ' '
          ELSEIF ( Q.EQ.1.0 ) THEN
            TLOD(9:9) = '+'
          ELSEIF ( Q.EQ.-1.0 ) THEN
            TLOD(9:9) = '-'
          ELSEIF ( Q.EQ.-2.0 ) THEN
            TLOD(9:9) = '='
          ELSEIF ( Q.EQ.+2.0 ) THEN
            TLOD(9:9) = '#'
          ELSE
            TLOD(9:9) = '*'
          ENDIF
          PTOT = OBJP(I)
          PY = OBJPY(I)
          PZ = OBJPZ(I)
          THETA = ACOS(PZ/PTOT)
          STH = SIN(THETA)
          PHI = OBJP0(I)
          THETA = THETA / 3.14159265D0 * 180.0D0
          PHI = PHI / 3.14159265D0 * 180.0D0
          IF ( PHI.LT.0.0D0 ) PHI = PHI + 360.0D0
          TLOD(10:12) = DT3(PTOT)
          IF ( PZ.GT.0.0D0 ) THEN
            TLOD(14:14) = '+'
          ELSEIF ( PZ.LT.0.0D0 ) THEN
            TLOD(14:14) = '-'
          ELSE
            TLOD(14:14) = ' '
          ENDIF
          TLOD(15:17) = DT3(REAL(ABS(PZ)))
          TLOD(19:21) = DT3(REAL(NINT(PHI)))
          TLOD(23:25) = DT3(REAL(NINT(THETA)))
          TLOD(27:30) = DT4(REAL(OBJM(I)))
          TLOD(32:41) = TOBJ(INT(OBJNO(I)))(1:10)
          IF ( TOBJ(INT(OBJNO(I)))(11:12) .EQ. '    ' ) THEN
            CONTINUE
          ELSE
            TLOD(41:41) = '>'
          ENDIF
          VH = VH - VSTEP
        IF ( BCOLO ) GOTO 807
        IF ( OBJTY(I).GE.4.0D0 ) THEN
          CALL DGLEVL(N7)
          CALL DGTEXT(HM,VH,TLOD,ITW)
          CALL DGLEVL( KOLOR )
        ELSEIF ( OBJTY(I).EQ.3.0D0 ) THEN
          CALL DGLEVL(N8)
          CALL DGTEXT(HM,VH,TLOD,ITW)
          CALL DGLEVL( KOLOR )
        ELSEIF ( OBJTY(I).EQ.2.0D0 ) THEN
          CALL DGLEVL(N8)
          CALL DGTEXT(HM,VH,TLOD,ITW)
          CALL DGLEVL( KOLOR )
        ELSE
          CALL DGLEVL(NCTRDC(OBJNO(I)))
          CALL DGTEXT(HM,VH,TLOD,ITW)
        ENDIF
        GOTO 820
  807   IF ( OBJSO(I).GT.0.0D0 ) THEN
          CALL DGLEVL( MOD(KOLOR+4,16) )
          CALL DGTEXT(HM,VH,TLOD,ITW)
          CALL DGLEVL(KOLOR)
        ELSEIF ( OBJTY(I).GE.4.0D0 ) THEN
          CALL DGLEVL( MOD(KOLOR+1,16) )
          CALL DGTEXT(HM,VH,TLOD,ITW)
          CALL DGLEVL( KOLOR )
        ELSEIF ( OBJTY(I).EQ.3.0D0 ) THEN
          CALL DGLEVL( MOD(KOLOR+2,16) )
          CALL DGTEXT(HM,VH,TLOD,ITW)
          CALL DGLEVL( KOLOR )
        ELSE
          CALL DGTEXT(HM,VH,TLOD,ITW)
        ENDIF
  820   CONTINUE
      ENDIF
      CALL DQFR(IAREDO)                ! DRAW FRAME
      CALL DPCSAR                 ! STORE DRAWING.
      IF ( .NOT.BLONG ) RETURN       ! RETURN
      IF ( NOBJ.GT.126 ) THEN
        CALL DWRT('PH: Too many objects to list.')
      ELSEIF ( (NOBJ.GT.63).AND.(IAREDO.LE.11).AND.(IAREDO.GE.1) ) THEN
        CALL DWRT('PH: Too many objects. Use WS or WW.')
      ELSEIF ( (NOBJ.GT.60).AND.(IAREDO.LE.8).AND.(IAREDO.GE.1) ) THEN
        CALL DWRT('PH: Too many objects. Use WL,WM,WR,WS or WW')
      ELSEIF ( (NOBJ.GT.30).AND.(IAREDO.LE.6).AND.(IAREDO.GE.1) ) THEN
        CALL DWRT(
     &    'PH: Too many objects. Use WU,WD,WL,WM,WR,WS or WW')
C          1234567890123456789012345678901234567890123456789
      ENDIF

      END
C____________________________________________________________________
      SUBROUTINE DIPRC
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- Allan Engelhardt       2-AUG-1990
C!   Modified :- Allan Engelhardt      28-AUG-1990  Almost final version.
C!
C!   Inputs:
C!        - NONE (get all data from the OBJ matrix)
C!
C!   Outputs:
C!        - Modifies the OBJ matrix. Writes to the screen only in case of a
C!        - fatal error (an infinite loop).
C!
C!   Libraries required: KERNLIB | M107/SORTDQ , M103/FLPSOR
C!                       GENLIB  | M109/SORTD  , F224/EISRS2
C!
C!   Description
C!   ===========
C!
C?      Recalculates OBJ(NOBJI,MAXNO).
C?
C?      (Note: I'm afraid the code is now rather ugly. Sorry about that
C?      --- Allan)
C!======================================================================
*IF .NOT.DOC
      INCLUDE 'DALI_CF.INC'
      INCLUDE 'DALI_OBJ.INC'
      REAL*8       P0,P1,P2,P3,PTOT2,PTOT,THETA,STH,
     &  PX,PY,PZ,PXYZ,PT,OBNO
      REAL    QAB(3,3),WR(3),WORK(3)    ! USED FOR F224/EISRS2
      INTEGER IERR                      ! ERROR STATUS FROM F224
      INTEGER LRET                      ! RETURN LABLE
      INTEGER LOOP                      ! CHECK FOR INFINITE LOOP
      LOGICAL BRC(MAXNO)              ! KEEP TRACK OF REWORKED
      ! OBJECTS.
      LOGICAL BRCS,BRCPA,BRCJE
      INCLUDE 'DALI_OBD.INC'
C---------------------------------------------------------------------
      DO 20 I = 1 , NUMT+NUME+NUMH
        BRC(I) = .TRUE.                 ! MESURED OBJECTS ARE DONE
   20 CONTINUE
      DO 19 I = NUMT+NUME+NUMH+1 , MAXNO
        BRC(I) = .FALSE.
   19 CONTINUE
      CALL SORTDQ(OBJ,NOBJI,MAXNO,3)    ! SORT BY NUMBERS
      CALL SORTD(OBJ,NOBJI,MAXNO,-25)   ! USED FIRST
      LOOP = 0
   30 CONTINUE
      LOOP = LOOP + 1
      IF ( LOOP.GT.(2*(MAXNO+1)) ) THEN ! HIGH ENOUGH :-)
C       *** OH SHIT! --- MY DIPCHK DIDN'T WORK, I GUESS :-) ***
        CALL DWRT('PH: ERROR DETECTED --- INFINITE LOOP')
        CALL DWRT('PH: POSSIBLE DATA INCONSISTENCY: LOOP')
        CALL DWRT('PH: IN PARENT/JET RELATIONS.')
        CALL DWRT('PH: *** DATA PRODUCED ARE NOT RELIABLE ***')
        CALL DWRT(
     &   'PH: *** Unpredictable results until loop is removed ***')
        GOTO 301                        ! END
      ENDIF
      DO 200 I = NUMT+NUME+NUMH+1 , NOBJ
        OBNO = OBJNO(I)
        BRCS = .FALSE.
        IF ( BRC(I) ) GOTO 190          ! THIS ONE IS OK AS PArent
        P0 = 0.0D0                      ! RESET ALL VALUES:
        P1 = 0.0D0                      ! Pi IS FOUR-MOMENTUM
        P2 = 0.0D0
        P3 = 0.0D0
        Q  = 0.0                        ! Q IS CHARGE

        BRCPA = .TRUE.
        DO 150 J = 1 , NOBJ
          IF ( OBJPA(J).EQ.OBNO ) THEN
            BRCPA = .FALSE.
            IF ( .NOT.BRC(J) ) THEN
              BRCS = .FALSE.
              GOTO 151
            ENDIF
            BRCS = .TRUE.               ! SOMETHING DONE
            P0 = P0 + DBLE(OBJET(J))          ! ACCUMULATE
            P1 = P1 + DBLE(OBJPX(J))
            P2 = P2 + DBLE(OBJPY(J))
            P3 = P3 + DBLE(OBJPZ(J))
            Q  = Q  + REAL(OBJQ(J))
          ENDIF
  150   CONTINUE                        ! J - LOOP
        IF ( BRCS ) THEN
          LRET=151
          GOTO 400                      ! UPDATE OBJ MATRIX
        ENDIF
  151   CONTINUE

  190   CONTINUE
        BRCJE = .TRUE.
        IF ( OBJTY(I).EQ.2.0D0 ) THEN   ! DO A JET
          P0 = 0.0D0                      ! RESET ALL VALUES:
          P1 = 0.0D0                      ! Pi IS FOUR-MOMENTUM
          P2 = 0.0D0
          P3 = 0.0D0
          Q  = 0.0                        ! Q IS CHARGE
          DO 155 K = 1 , NOBJ
            NSOBJ(K) = 0                  ! NSOBJ IS DEF. IN DIPOBJ.INC
  155     CONTINUE                        ! K - LOOP
          JET = 0                         ! INDEX TO NSOBJ
          DO 165 IL1 = 1 , 3
            DO 156 IL2 = 1 , 3
              QAB(IL2,IL1) = 0.0
  156       CONTINUE
            WR(IL1) = 0.0
            WORK(IL1) = 0.0
  165     CONTINUE
          IERR = 0
          DO 160 J = 1 , NOBJ
            IF ( OBJJE(J).EQ.OBNO ) THEN
              BRCJE = .FALSE.
              IF ( .NOT.BRC(J) ) THEN
                BRCS = .FALSE.
                GOTO 200
              ENDIF
              BRCS = .TRUE.               ! SOMETHING DONE
              JET = JET + 1
              NSOBJ(JET) = J              ! STORE JET-MEMBER
              PX = OBJPX(J)
              PY = OBJPY(J)
              PZ = OBJPZ(J)
              P0 = P0 + DBLE(OBJET(J))          ! ACCUMULATE
              P1 = P1 + PX
              P2 = P2 + PY
              P3 = P3 + PZ
              Q  = Q  + REAL(OBJQ(J))
              QAB(1,1) = QAB(1,1) + REAL( PX*PX )
              QAB(2,1) = QAB(2,1) + REAL( PY*PX )
              QAB(3,1) = QAB(3,1) + REAL( PZ*PX )
              QAB(1,2) = QAB(1,2) + REAL( PX*PY )
              QAB(2,2) = QAB(2,2) + REAL( PY*PY )
              QAB(3,2) = QAB(3,2) + REAL( PZ*PY )
              QAB(1,3) = QAB(1,3) + REAL( PX*PZ )
              QAB(2,3) = QAB(2,3) + REAL( PY*PZ )
              QAB(3,3) = QAB(3,3) + REAL( PZ*PZ )
            ENDIF
  160     CONTINUE                        ! J - LOOP
          IF ( BRCS ) THEN
            LRET=161
            GOTO 400                    ! UPDATE OBJ MATRIX
          ENDIF
  161     CONTINUE
        ENDIF
        IF ( BRCJE.AND.BRCPA ) THEN
          BRC(I) = .TRUE.
        ENDIF
  200 CONTINUE                          ! I - LOOP
      DO 300 I = 1 , NOBJ
        IF ( .NOT.BRC(I) ) GOTO 30
  300 CONTINUE
  301 RETURN
C     ----------------------------   UPDATING OBJ MATRIX. RETURNS TO LRET
  400 CONTINUE
      PTOT2 = P1*P1 + P2*P2 + P3*P3
      PTOT = SQRT(PTOT2)
      THETA = ACOS(P3/PTOT)
      STH = SIN(THETA)
      RMA = SQRT(ABS(P0*P0 - PTOT2))
      IF ( ABS(Q).GT.0.0 ) THEN
        OBJ( 4,I) = 1.0D5*PTOT*STH/BFIELD/CLGHT/DBLE(Q) ! RHO - RADIUS OF
        ! CURVATURE (?)
      ELSE
        OBJ( 4,I) = 0.0D0
      ENDIF
      OBJ( 5,I) = THETA                  ! THETA
      OBJ( 6,I) = DIPANG(P1,P2)
      OBJ( 7,I) = 0.0D0                  ! D0 (?)
      OBJ( 8,I) = 0.0D0                  ! Z0 (?)
      OBJ( 9,I) = 0.0D0                  ! ALPHA (?)
      OBJ(10,I) = PTOT                   ! MOMENTUM
      IF ( OBJTY(I).EQ.2.0D0 ) THEN     ! JET
        DO 170 IJK = 1 , JET
          PX = OBJPX(NSOBJ(IJK))
          PY = OBJPY(NSOBJ(IJK))
          PZ = OBJPZ(NSOBJ(IJK))
          PXYZ = SQRT( PX**2 + PY**2 + PZ**2 )
          PT = PXYZ * SIN( ACOS( ( P1*PX + P2*PY + P3*PZ ) / (
     &      PTOT*PXYZ ) ) )     ! THERE HAS TO BE AN EASIER WAY
          IF ( OBJAO(NSOBJ(IJK)).GT.0.0D0 ) OBJ(26,I) = +1.0D0    !
                                        ! ASSOCIATED OBJECTS
          OBJ(11,NSOBJ(IJK)) = PT
  170   CONTINUE
        CALL EISRS2(3,3,QAB,WR,IERR,WORK)
        IF ( IERR.NE.0 ) THEN
          CALL DWRT('PH: *ERROR* Eigenvalues are in error.')
        ENDIF
        CALL FLPSOR(WR,3)
        RTSUM = WR(1) + WR(2) + WR(3)
        OBJ(27,I) = DBLE( WR(1) / RTSUM )
        OBJ(28,I) = DBLE( WR(2) / RTSUM )
        OBJ(29,I) = DBLE( WR(3) / RTSUM )
      ENDIF
      OBJ(12,I) = P1                     ! PX
      OBJ(13,I) = P2                     ! PY
      OBJ(14,I) = P3                     ! PZ
      IF ( TOBJ(INT(OBJNO(I))).EQ.'            ' ) THEN
        OBJ(15,I) = Q                      ! CHARGE
        OBJ(16,I) = RMA                    ! MASS
      ENDIF
      OBJ(17,I) = P0                     ! ETOT
      OBJ(25,I) = +1.0D0                 ! USED OBJECT
      BRC(I) = .TRUE.                 ! OK TO USE THIS ONE
      IF(LRET.EQ.151) GO TO 151
      IF(LRET.EQ.161) GO TO 161

      END
C________________________________________________________________________
      SUBROUTINE DIPNP(PA)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- Allan Engelhardt       2-AUG-1990
C!   Modified :- Allan Engelhardt      28-AUG-1990  Almost final version.
C!
C!   Inputs:
C!        - PA   Old parent number
C!
C!   Outputs:
C!        - Changes OBJ
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!
C?      Deletes an parent from the OBJ matrix
C!======================================================================
*IF .NOT.DOC
      INCLUDE 'DALI_CF.INC'
      INCLUDE 'DALI_OBJ.INC'
      REAL*8 PA
      INCLUDE 'DALI_OBD.INC'
C------------------------------------------------------------------------
      DO 10 I = 1 , MAXNO
        IF ( OBJPA(I).EQ.PA ) OBJ(19,I) = 0.00D0
   10 CONTINUE
      END
C_______________________________________________________________________
      SUBROUTINE DIPDI(P2)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- Allan Engelhardt       7-AUG-1990
C!   Modified :- Allan Engelhardt      28-AUG-1990  Almost final version.
C!
C!   Inputs:
C!        - P2 : Objectnumber.
C!
C!   Outputs:
C!        - To the screen.
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!
C?      Displays detailed information about an object.
C!======================================================================
*IF .NOT.DOC
      INCLUDE 'DALI_CF.INC'             ! STANDARD DALI INCLUDE MODULE.
      ! *NOTE* THAT THE MODULE IS
      ! TAKEN FROM THE DEFAULT
      ! DIRECTORY.
      INCLUDE 'DALI_OBJ.INC'
      CHARACTER *1 DT1                   ! ******************************
C      CHARACTER *2 DT2                   ! * CHARACTER CONVERSIONS FROM *
      CHARACTER *3 DT3,DT3Z              ! * DALBT.FOR                  *
      CHARACTER *4 DT4                   ! * CONVERTS REAL TO THE       *
      CHARACTER *5 DT5                   ! * DESIRED CHARACTER LENGTH   *
      CHARACTER *6 DT6                   ! *                            *
C      CHARACTER *7 DT7                   ! *                            *
C      CHARACTER *8 DT8                   ! ******************************
C      CHARACTER *49 TSO,TCO,TUS,TDI(9)
      CHARACTER *49 TDI(9)
C      CHARACTER *132 TUSER
      INTEGER NSO                       ! NUMBER SELECTED OBJECTS

C                 1234567890123456789012345678901234567890123456789
C      DATA TSO / 'PH: Object 123 is selected. 123 objects selected.' /
C      DATA TCO / 'PH: Text Colour changed to 1.                    ' /
C      DATA TUS / 'PH: Object 123 is no longer selected.            ' /
      DATA TDI / 'Object no. 123  Type: x Jet no. 123 Parent 123   ' ,
     &           'Ptot=123456 Etot=123456 Q=12 Mass=123456         ' ,
     &           'Pxy =123456 Hypothesis : xxxxxxxxxxxx            ' ,
     &           'Px=123456 Py=123456 Pz=123456 Ptransverse=123456 ' ,
     &           'Rho=12345 Phi0=1234 D0=1234 Z0=1234 Alpha=12345  ' ,
     &           'Cal Object no. 123 Ecal obj. 123 Hcal obj. 123   ' ,
     &           'PH: No object number 123 in list.                ' ,
     &           'Aplanarity=12345 Oblatenes=12345 "Thrust"=12345  ' ,
     &           'Sphericity=12345 Planarity=12345                 ' /
C                 1234567890123456789012345678901234567890123456789
C----------------------------------------------------------------------
      INCLUDE 'DALI_OBD.INC'
      DO 131 IJ = 1 , MAXNO
        IF ( OBJNO(IJ).EQ.P2 ) GOTO 132
  131 CONTINUE
      TDI(7)(22:25) = DT3(REAL(IJ))
      CALL DWRT(TDI(7))
      RETURN
  132 CONTINUE
      IF ( OBJUS(IJ).EQ.0.0 ) THEN
        TDI(7)(22:25) = DT3(REAL(P2))
        CALL DWRT(TDI(7))
        RETURN
      ENDIF
      TDI(1)(12:14) = DT3(REAL(IJ))
      TDI(1)(23:23) = TOTY(OBJTY(IJ)+1)
      TDI(1)(33:35) = DT3(REAL(OBJJE(IJ)))
      TDI(1)(44:46) = DT3(REAL(OBJPA(IJ)))
      CALL DWRT(TDI(1))
      TDI(2)(06:11) = DT6(REAL(OBJP(IJ)))
      TDI(2)(18:23) = DT6(REAL(OBJET(IJ)))
      Q =  REAL(OBJQ(IJ))
      IF ( Q.GT.0 ) THEN
        TDI(2)(27:27) = '+'
      ELSEIF ( Q.LT.0 ) THEN
        TDI(2)(27:27) = '-'
      ELSE
        TDI(2)(27:27) = ' '
      ENDIF
      TDI(2)(28:28) = DT1(REAL(ABS(Q)))
      TDI(2)(35:40) = DT6(REAL(OBJM(IJ)))
      CALL DWRT(TDI(2))
      TDI(3)(06:11) = DT6(REAL(SQRT(OBJPX(IJ)**2+OBJPY(IJ)**2)))
      TDI(3)(26:37) = TOBJ(INT(OBJNO(IJ)))
      CALL DWRT(TDI(3))
      TDI(4)(04:09) = DT6(REAL(OBJPX(IJ)))
      TDI(4)(14:19) = DT6(REAL(OBJPY(IJ)))
      TDI(4)(24:29) = DT6(REAL(OBJPZ(IJ)))
      TDI(4)(43:48) = DT6(REAL(OBJPT(IJ)))
      CALL DWRT(TDI(4))
      TDI(5)(05:09) = DT5(REAL(OBJRH(IJ)))
      TDI(5)(16:19) = DT4(REAL(OBJP0(IJ)))
      TDI(5)(24:27) = DT4(REAL(OBJD0(IJ)))
      TDI(5)(32:35) = DT4(REAL(OBJZ0(IJ)))
      TDI(5)(43:47) = DT5(REAL(OBJAL(IJ)))
      CALL DWRT(TDI(5))
      TDI(6)(16:18) = DT3(REAL(OBJAO(IJ)))
      TDI(6)(30:32) = DT3(REAL(OBJEC(IJ)))
      TDI(6)(44:46) = DT3(REAL(OBJHC(IJ)))
      CALL DWRT(TDI(6))
      CALL DIPDA(IJ,.FALSE.) ! DISPLAY ASSOCIATED INFO. --- SHORT FORM
      IF ( OBJTY(IJ).EQ.2.0D0 ) THEN    ! JETS
        TDI(8)(12:16) = DT5(REAL( 1.5D0 * OBJL1(IJ) ))
        TDI(8)(28:32) = DT5(REAL( OBJL2(IJ) ))
        TDI(8)(43:47) = DT5(REAL( OBJL3(IJ) ))
        CALL DWRT(TDI(8))
        TDI(9)(12:16) = DT5(REAL( 1.5D0 * ( 1.0D0 - OBJL3(IJ) ) ))
        TDI(9)(28:32) = DT5(REAL( ( OBJL3(IJ) - OBJL1(IJ) ) /
     &    SQRT( 3.0D0 ) ))
        CALL DWRT(TDI(9))
      ENDIF

      RETURN
      END
C________________________________________________________________________
      SUBROUTINE DIPRO(P2)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- Allan Engelhardt       7-AUG-1990
C!   Modified :- Allan Engelhardt      28-AUG-1990  Almost final version.
C!
C!   Inputs:
C!        - P2 : Objectnumber
C!
C!   Outputs:
C!        - Modifies the OBJ matrix.
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!
C?      Removes object P2 from the OBJ matrix.
C!======================================================================
*IF .NOT.DOC
      INCLUDE 'DALI_CF.INC'             ! STANDARD DALI INCLUDE MODULE.
      ! *NOTE* THAT THE MODULE IS
      ! TAKEN FROM THE DEFAULT
      ! DIRECTORY.
      INCLUDE 'DALI_OBJ.INC'
C      CHARACTER *1 DT1                   ! ******************************
C      CHARACTER *2 DT2                   ! * CHARACTER CONVERSIONS FROM *
      CHARACTER *3 DT3,DT3Z              ! * DALBT.FOR                  *
C      CHARACTER *4 DT4                   ! * CONVERTS REAL TO THE       *
C      CHARACTER *5 DT5                   ! * DESIRED CHARACTER LENGTH   *
C      CHARACTER *6 DT6                   ! *                            *
C      CHARACTER *7 DT7                   ! *                            *
C      CHARACTER *8 DT8                   ! ******************************
C      CHARACTER *49 TSO,TCO,TUS,TDI(7)
      CHARACTER *49 TDI(7)
      CHARACTER *1 TYN

C                 1234567890123456789012345678901234567890123456789
C      DATA TSO / 'PH: Object 123 is selected. 123 objects selected.' /
C      DATA TCO / 'PH: Text Colour changed to 1.                    ' /
c      DATA TUS / 'PH: Object 123 is no longer selected.            ' /
C                 1234567890123456789012345678901234567890123456789
      DATA TDI / 'Object no. 123  Type: x Jet no. 123 Parent 123   ' ,
     &           'Ptot=123456 Etot=123456 Q=12 Mass=123456         ' ,
     &           'Pxy =123456 Hypothesis : xxxxxxxxxxxx            ' ,
     &           'Px=123456 Py=123456 Pz=123456 Ptransverse=123456 ' ,
     &           'Rho=12345 Phi0=1234 D0=1234 Z0=1234 Alpha=12345  ' ,
     &           'Cal Object no. 123 Ecal obj. 123 Hcal obj. 123   ' ,
     &           'PH: No object number 123 in list.                ' /
C-------------------------------------------------------------------------
      INCLUDE 'DALI_OBD.INC'
      DO 1131 IJ = 1 , MAXNO
        IF ( OBJNO(IJ).EQ.P2 ) GOTO 1132
 1131 CONTINUE
      TDI(7)(22:25) = DT3(REAL(IJ))
      CALL DWRT(TDI(7))
      RETURN
 1132 CONTINUE
      IF ( OBJUS(IJ).NE.1.0 ) THEN
        TDI(7)(22:25) = DT3(REAL(P2))
        CALL DWRT(TDI(7))
        RETURN
      ENDIF

      CALL DIPDI(P2)

C                 1234567890123456789012345678901234567890123456789
      CALL DWRT('PH: SURE you want to remove object (Y/N)? [N]')
      CALL DGETLN(TYN,LCHLIN,1)
      IF ( (TYN(1:1).EQ.'Y').OR.(TYN(1:1).EQ.'y') ) THEN
        DO 142 IJ = 1 , MAXNO
          IF ( OBJNO(IJ).EQ.P2 ) GOTO 143
  142   CONTINUE
        RETURN
  143   CONTINUE
        IF ( OBJTY(IJ).EQ.1.0D0 ) THEN
          NUMT = NUMT - 1
        ELSEIF ( OBJTY(IJ).EQ.2.0D0 ) THEN
          NUMJ = NUMJ - 1
        ELSEIF ( OBJTY(IJ).EQ.3.0D0 ) THEN
          NUMR = NUMR - 1
        ELSEIF ( OBJTY(IJ).EQ.4.0D0 ) THEN
          NUME = NUME - 1
        ELSEIF ( OBJTY(IJ).EQ.5.0D0 ) THEN
          NUMH = NUMH - 1
        ENDIF
        NOBJ = NUMT + NUMJ + NUMR + NUME + NUMH
        CALL DIPNP(OBJNO(IJ))           ! REMOVE PARENT
        DO 150 J = 1 , NOBJI
          OBJ(J,IJ) = 0.0D0               ! DELETE
  150   CONTINUE
        IF ( LSTRE.EQ.INT(P2) ) LSTRE = 0
        CALL DIPRC                      ! RECALCULATE
        IF ( BLIST ) CALL DIPD      ! LO
      ELSE
        CALL DWRT('PH: No objcet was removed by user request.')
        RETURN
      ENDIF
      RETURN

      END
C_____________________________________________________________________
      FUNCTION DIPCHK(NARR,ICOL,IROW,JCOL1,JCOL2)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- Allan Engelhardt      17-AUG-1990
C!   Modified :- Allan Engelhardt      28-AUG-1990  Almost final version.
C!
C!   Inputs:
C!        - NARR(ICOL,IROW) matrix to check for loop
C!        - JCOL1 coloumn with parent data
C!        - JCOL2 coloumn with jet data
C!
C!   Outputs:
C!        - Returns the number of errors.
C!
C!   Libraries required:
C!        -
C!
C!   Description
C!   ===========
C!
C?      This function is used to test for forward loops in the OBJ matrix; i.e.
C?      all loops, that could cause the DIPRC to enter an infinite loop. NARR
C?      is a short version of the OBJ matrix, containing only the needed
C?      information about parents and jets.
C?      NARR is used so that you can check without actually making the change.
C!======================================================================
*IF .NOT.DOC
      INTEGER ICOL,IROW,JCOL1,JCOL2
      INTEGER NARR(ICOL,IROW)
      INTEGER IRMAX                     ! MAX. NUMBER OF ROWS
      PARAMETER( IRMAX = 250 )
      CHARACTER *1 T(IRMAX)              ! USED TO REMEMBER ORDER OF
                                        ! SEARCH
C      CHARACTER *1 TC                    ! CURRENT
      INTEGER NUM                       ! NUMBER OF ERRORS.
C--------------------------------------------------------------------------
C     --------------------------------------   INITIALIZE AND DEFAULT VALUES.
      DO 10 I = 1 , IRMAX
        T(I) = 'P'                      ! T(IRMAX)
   10 CONTINUE
      NUM = 0                           ! NUM
      IF ( JCOL1.EQ.0 ) THEN
        JCOL1 = 2                       ! JCOL1
      ENDIF
      IF ( JCOL2.EQ.0 ) THEN
        JCOL2 = 3                       ! JCOL2
      ENDIF

      DO 500 LROW = 1 , IROW            ! FOR ALL ROWS IN NARR:
        NC = LROW                       ! CURRENT NUMBER = ROW
  102   CONTINUE
        DO 110 I = 1 , IRMAX            ! FOR ALL 'WAYS' IN T:
          IF ( T(I).EQ.'P' ) THEN       ! FIND THE NEW CURRENT NUMBER
            NCN = NARR(JCOL1,NC)
            DO 103 NC = 1 , IROW
              IF ( NARR(1,NC).EQ.NCN ) THEN
                GOTO 104
              ENDIF
  103       CONTINUE
            NC = 0
  104       CONTINUE
          ELSE
            NCN = NARR(JCOL2,NC)
            DO 113 NC = 1 , IROW
              IF ( NARR(1,NC).EQ.NCN ) THEN
                GOTO 114
              ENDIF
  113       CONTINUE
            NC = 0
  114       CONTINUE
          ENDIF
          IF ( NC.LE.0 ) THEN           ! NO CURRENT NUMBER?
            J = I-1                     ! PREVIOUS VALUE OF T
  105       CONTINUE                    ! SHOULD BE CHANGED
            IF ( J.EQ.0 ) GOTO 500      ! NO PREVIOUS=>NEXT ROW
            IF ( T(J).EQ.'P' ) THEN     ! IF PREVIOUS WAS 'P'
              T(J) = 'J'                ! THEN MAKE IT 'J'
              DO 115 K = J+1 , IRMAX
                T(K) = 'P'
  115         CONTINUE
              GOTO 102                  ! AND START AGAIN
            ELSE                        ! IF PREVIOUS VAS 'J'
              J = J-1                   ! STEP ONE BACK
              GOTO 105                  ! AND TEST THIS PREVIOUS
            ENDIF
          ELSEIF ( NC.EQ.NARR(1,LROW) ) THEN    ! ELSE: HAVE WE FOUND A LOOP?
            NUM = NUM + 1               ! ADD ONE TO NUMER OF ERRORS
            GOTO 500

          ENDIF

  110   CONTINUE

  500 CONTINUE

      DIPCHK = NUM
      END
C__________________________________________________________________________
      DOUBLE PRECISION FUNCTION DIPANG(PX,PY)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- Allan Engelhardt      24-AUG-1990
C!   Modified :- Allan Engelhardt      28-AUG-1990  Almost final version.
C!
C!   Inputs:
C!        - Two momentas (REAL*8)
C!
C!   Outputs:
C!        - The phi angle.
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!                           PY     P=(PX,PY)
C?                           |     /
C?                           |    /
C?                           |   /
C?                           |  /
C?                           | /
C?                           |/)phi
C?             -----------------------------   PX
C?                           |
C?                           |
C?                           |
C?                           |
C?                           |
C?                           |
C?      Returns the correct phi value \in [0;2*PI].
C?
C!======================================================================
*IF .NOT.DOC
      DOUBLE PRECISION PX,PY,PHI,PI
      PARAMETER( PI = 3.141592653589793238 )
C------------------------------------------------------------------------
      IF ( PY.EQ.0.0D0 ) THEN
        IF ( PX.GE.0.0D0 ) THEN
          DIPANG = PI/2.0D0
        ELSE
          DIPANG = -PI/2.0D0
        ENDIF
        RETURN
      ENDIF
      IF ( PX.EQ.0.0D0 ) THEN
        IF ( PY.GE.0.0D0 ) THEN
          DIPANG = 0.0D0
        ELSE
          DIPANG = -PI
        ENDIF
        RETURN
      ENDIF
      PHI = ATAN( PY/PX )
      IF     ( PX.GT.0.0D0.AND.PY.GT.0.0D0 ) THEN
        DIPANG = PHI
      ELSEIF ( PX.GT.0.0D0.AND.PY.LT.0.0D0 ) THEN
        DIPANG = PHI + 2.0D0*PI
      ELSE
        DIPANG = PHI + PI
      ENDIF
      END
C______________________________________________________________________
      SUBROUTINE DIPSU(ONUM)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- Allan Engelhardt      24-AUG-1990
C!   Modified :- Allan Engelhardt      28-AUG-1990  Almost final version.
C!
C!   Inputs:
C!        - ONUM: REAL objectnumber
C!
C!   Outputs:
C!        - SUggestions for the object on the screen.
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!
C?      Uses the information in the PART bank.
C?      The limit on the seach is DMASS from the OBJ common. See the SL:
C?      command.
C?
C!======================================================================
*IF .NOT.DOC
      REAL ONUM
      INCLUDE 'DALI_OBJ.INC'
      INCLUDE 'A_BCS.INC'

      CHARACTER *1 DT1                   ! ******************************
C      CHARACTER *2 DT2                   ! * CHARACTER CONVERSIONS FROM *
C      CHARACTER *3 DT3,DT3Z              ! * DALBT.FOR                  *
      CHARACTER *4 DT4                   ! * CONVERTS REAL TO THE       *
      CHARACTER *5 DT5                   ! * DESIRED CHARACTER LENGTH   *
C      CHARACTER *6 DT6                   ! *                            *
      CHARACTER *7 DT7                   ! *                            *
C      CHARACTER *8 DT8                   ! ******************************
      CHARACTER *4 CHAINT
      CHARACTER *49 TSU(3)
      CHARACTER *12 TNAM
      CHARACTER *7 TLIF
      LOGICAL BHEAD

C                          1         2         3         4
C                 1234567890123456789012345678901234567890123456789
      DATA TSU / '   Name       Mass   Q  LifeTime MassWidth dMass%' ,
     &           'xxxxxxxxxxxx 1234567 12  1234567  1234567   1234 ' ,
     &           'PH: No particles matched within mass ± 12.45%    ' /

      INCLUDE 'DALI_OBD.INC'
      INCLUDE 'A_BMACRO.INC'
C-----------------------------------------------------------------------
      BHEAD = .FALSE.                   ! NO HEADER LINE IS SHOWN
      DO 5 INUM = 1 , MAXNO
        IF ( REAL(OBJNO(INUM)).EQ.ONUM ) GOTO 6
    5 CONTINUE
      CALL DWRT('PH: No such object.')
    6 CONTINUE
      RMASS = REAL(OBJM(INUM))
      ICHAR = INT(OBJQ(INUM))
      DO 10 I = 1 , NPART
        ICH = INT(RTABL(JPART,I,7))
        IF ( ICH.EQ.ICHAR ) THEN
          RMA = RTABL(JPART,I,6)
          IF ( RMASS.GT.0.0 ) THEN
            DMA = ABS(RMASS-RMA) / RMASS
          ELSEIF ( RMA.EQ.0.0 ) THEN
            DMA = 0.0
          ELSE
            DMA = 999999.99
          ENDIF
          IF ( DMA.LE.DMASS ) THEN
            IF ( .NOT.BHEAD ) THEN
              CALL DWRT( TSU(1) )
              BHEAD = .TRUE.
            ENDIF
            WRITE(TNAM,9991) 
     &        CHAINT(ITABL(JPART,I,2)),
     &        CHAINT(ITABL(JPART,I,3)),
     &        CHAINT(ITABL(JPART,I,4))
 9991       FORMAT(A,A,A)
            TSU(2)(01:12) = TNAM
            TSU(2)(14:20) = DT7(RTABL(JPART,I,6))
            IF ( ICH.GT.0 ) THEN
              TSU(2)(22:22) = '+'
            ELSEIF ( ICH.LT.0 ) THEN
              TSU(2)(22:22) = '-'
            ELSE
              TSU(2)(22:22) = ' '
            ENDIF
            TSU(2)(23:23) = DT1(REAL(ABS(ICH)))
            RLIF = RTABL(JPART,I,8)
            WRITE(TLIF,9992) RLIF
 9992       FORMAT ( E7.1 )
            TSU(2)(25:32) = TLIF
            TSU(2)(35:41) = DT7(RTABL(JPART,I,9))
            TSU(2)(45:48) = DT4(DMA*100.0)
            CALL DWRT( TSU(2) )
          ENDIF
        ENDIF
   10 CONTINUE
      IF ( .NOT.BHEAD ) THEN
        TSU(3)(40:44) = DT5(DMASS*100.0)
        CALL DWRT( TSU(3) )
      ENDIF

      END
C_____________________________________________________________________
      SUBROUTINE DIPDA(IND,BMAXI)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- Allan Engelhardt      27-AUG-1990
C!   Modified :- Allan Engelhardt      28-AUG-1990  Almost final version.
C!
C!   Inputs:
C!        - IND         Index to the OBJ matrix
C!        - BMAXI       Make a short list (.FALSE.) or a long list (.TRUE.)
C!
C!   Outputs:
C!        - To the screen (calling DT018)
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!
C?      This routine gets the information from the BOS banks, and displays it
C?      on the screen.
C?
C?
C!======================================================================
*IF .NOT.DOC
      INTEGER IND                       ! INDEX TO CURRENT OBJ MATRIX
      LOGICAL BMAXI                     ! MAKE A LONG LIST?

      INCLUDE 'A_BCS.INC'
      INCLUDE 'DALI_OBJ.INC'

      PARAMETER( PI = 3.14159265 )

C      CHARACTER *1 DT1                   ! ******************************
C      CHARACTER *2 DT2                   ! * CHARACTER CONVERSIONS FROM *
      CHARACTER *3 DT3,DT3Z              ! * DALBT.FOR                  *
C      CHARACTER *4 DT4                   ! * CONVERTS REAL TO THE       *
      CHARACTER *5 DT5                   ! * DESIRED CHARACTER LENGTH   *
      CHARACTER *6 DT6                   ! *                            *
C      CHARACTER *7 DT7                   ! *                            *
C      CHARACTER *8 DT8                   ! ******************************
      CHARACTER *2 DQH2                ! CONVERT TO HEXADECIMAL
      CHARACTER *49 TDAO(6)


C                           1         2         3         4
C                  1234567890123456789012345678901234567890123456789
      DATA TDAO / 'E 123 Ecal objects. Total corrected energy=123456' ,
     &            'E ### Phi The Eraw  Ecorr Esta1 Esta2 Rgn CCo Rbi' ,
     &            'E 123 123 123 12345 12345 12345 12345  HH  HH  HH' ,
     &            'H 123 Hcal objects. Total corrected energy=123456' ,
     &            'H ### Phi The Eraw  Ecorr         Noi Rgn CCo Rbi' ,
     &            'H 123 123 123 12345 12345          HH  HH  HH  HH' /
C                  1234567890123456789012345678901234567890123456789
C                           1         2         3         4

      INCLUDE 'DALI_OBD.INC'
      INCLUDE 'A_BMACRO.INC'
C-------------------------------------------------------------------------
      IF ( OBJUS(IND).LE.0.0D0 ) RETURN   ! RETURN IF NOT USED.
      IF ( OBJTY(IND).NE.1.0D0 ) RETURN   ! ONLY FOR TRACKS.
      KTRAC = INT( OBJNO(IND) )             ! KTRAC = TRACKNUMBER.
      IF ( KTRAC.LE.0 ) RETURN            ! RETURN IF NO TRACK.
C     ----------------------------------------------  SET UP THE BOS BANKS
      JPECO = IW(NAMIND('PECO'))        ! Ecal information
      IF ( JPECO.LE.0 ) THEN
        NUMEC = 0
      ELSE
        NUMEC = LROWS(JPECO)
      ENDIF
      JPHCO = IW(NAMIND('PHCO'))        ! Hcal information
      IF ( JPHCO.LE.0 ) THEN
        NUMHC = 0
      ELSE
        NUMHC = LROWS(JPHCO)
      ENDIF
      JPFER = IW(NAMIND('PFER'))        ! Ecal-Track relation
      IF ( JPFER.LE.0 ) THEN
        NUMET = 0
      ELSE
        NUMET = LROWS(JPFER)
      ENDIF
      JPFHR = IW(NAMIND('PFHR'))        ! Hcal-Track relation
      IF ( JPFHR.LE.0 ) THEN
        NUMHT = 0
      ELSE
        NUMHT = LROWS(JPFHR)
      ENDIF
C     -------------------------------------------------  BOS BANKS READY.

C     ----------------------------------------------  FIRST DO E-CAL OBJECTS
      NUEC = 0                          ! NUmber of ECal objects
      ENEC = 0.0                        ! ENergy of ECal objects
      DO 10 I = 1 , NUMET
        ITRAC = ITABL(JPFER,I,2)
        IF ( ITRAC.EQ.KTRAC ) THEN
          NUEC = NUEC + 1
          ENEC = ENEC + RTABL(JPECO,ITABL(JPFER,I,1),6)
        ENDIF
   10 CONTINUE
      TDAO(1)(03:05) = DT3(FLOAT(NUEC))
      TDAO(1)(44:49) = DT6(ENEC)
      CALL DWRT( TDAO(1) )
      IF ( NUEC.EQ.0 )  GOTO 50         ! ANYTHING TO LIST?
      IF ( .NOT.BMAXI ) GOTO 50         ! DO A LONG LIST?
      CALL DWRT( TDAO(2) )             ! DISPLAY HEADERS
      DO 20 I = 1 , NUMET
        ITRAC = ITABL(JPFER,I,2)
        IF ( ITRAC.EQ.KTRAC ) THEN
          IEC = ITABL(JPFER,I,1)
          TDAO(3)(03:05) = DT3( FLOAT(IEC) )
          PHI = RTABL(JPECO,IEC,5)
          PHI = PHI / PI * 180
          TDAO(3)(07:09) = DT3( FLOAT(INT(PHI+.5)) )
          THE = RTABL(JPECO,IEC,4)
          THE = THE / PI * 180
          TDAO(3)(11:13) = DT3( FLOAT(INT(THE+.5)) )
          TDAO(3)(15:19) = DT5( RTABL(JPECO,IEC,1) )
          TDAO(3)(21:25) = DT5( RTABL(JPECO,IEC,6) )
          TDAO(3)(27:31) = DT5( RTABL(JPECO,IEC,2) )
          TDAO(3)(33:37) = DT5( RTABL(JPECO,IEC,3) )
          TDAO(3)(40:41) = DQH2( ITABL(JPECO,IEC,7) )
          TDAO(3)(44:45) = DQH2( ITABL(JPECO,IEC,8) )
          TDAO(3)(48:49) = DQH2( ITABL(JPECO,IEC,9) )
          CALL DWRT( TDAO(3) )
        ENDIF
   20 CONTINUE
C     -----------------------------------------------   NOW DO H-CAL OBJECTS.
   50 CONTINUE
      NUHC = 0                          ! NUmber HCal objects
      ENHC = 0.0                        ! ENergy of HCal objects
      DO 60 I = 1 , NUMHT
        ITRAC = ITABL(JPFHR,I,1)
        IF ( ITRAC.EQ.KTRAC ) THEN
          NUHC = NUHC + 1
          ENHC = ENHC + RTABL(JPHCO,ITABL(JPFHR,I,2),4)
        ENDIF
   60 CONTINUE
      TDAO(4)(03:05) = DT3( FLOAT(NUHC) )
      TDAO(4)(44:49) = DT6( ENHC )
      CALL DWRT( TDAO(4) )
      IF ( NUHC.EQ.0 )  GOTO 100        ! ANYTHING TO LIST?
      IF ( .NOT.BMAXI ) GOTO 100        ! DO A LONG LIST?
      CALL DWRT( TDAO(5) )             ! DISPLAY HEADERS
      DO 70 I = 1 , NUMHT
        ITRAC = ITABL(JPFHR,I,1)
        IF ( ITRAC.EQ.KTRAC ) THEN
          IHC = ITABL(JPFHR,I,2)
          TDAO(6)(03:05) = DT3( FLOAT(IHC) )
          PHI = RTABL(JPHCO,IHC,3)
          PHI = PHI / PI * 180
          TDAO(6)(07:09) = DT3( FLOAT(INT(PHI+.5)) )
          THE = RTABL(JPHCO,IHC,2)
          THE = THE / PI * 180
          TDAO(6)(11:13) = DT3( FLOAT(INT(THE+.5)) )
          TDAO(6)(15:19) = DT5( RTABL(JPHCO,IHC,1) )
          TDAO(6)(21:25) = DT5( RTABL(JPHCO,IHC,4) )
          TDAO(6)(36:37) = DQH2( ITABL(JPHCO,IHC,8) )
          TDAO(6)(40:41) = DQH2( ITABL(JPHCO,IHC,5) )
          TDAO(6)(44:45) = DQH2( ITABL(JPHCO,IHC,7) )
          TDAO(6)(48:49) = DQH2( ITABL(JPHCO,IHC,9) )
          CALL DWRT( TDAO(6) )
        ENDIF
   70 CONTINUE
C     ------------------------------------------------------   ALL DONE NOW
  100 CONTINUE

      END
C_______________________________________________________________________
      CHARACTER *(*) FUNCTION DQH2(IN)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- C.Grab                14-Sep-1989
C!   Modified :- Allan Engelhardt      27-AUG-1990   To 2 character version
C!   Modified :- Allan Engelhardt      28-AUG-1990  Almost final version.
C!
C!   Inputs:
C!        - IN : Integer , 0 <= IN <= 255
C!
C!   Outputs:
C!        - IN in hexadecimal base as a string.
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!
C?     Convert integer numbers into <=8 bits hexadec. character strings
C?     Use internal file I/O :
C?     AS:   string = dth2( ivar )
C?
C!======================================================================
*IF .NOT.DOC
      CHARACTER *2 DTT
C----------------------------------------------------------------------
      DTT = '00'
      WRITE(DTT,1010) IN
      DQH2 = DTT
 1010 FORMAT(Z2)
      RETURN
      END
