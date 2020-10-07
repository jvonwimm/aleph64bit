      INTEGER FUNCTION ALK7OP (LURUK7)
C --------------------------------------------------------
C - F.Ranjard - 900627
C! Open RUNCARTSLIST   file and read it
CKEY ALEF TAPE  RUN / USER
C
C - Input     : LURUK7  / INTE  = open RUNCARTSLIST   on unit LURUK7
C
C - Output    :
C               ALK7OP    / INTE  = error code
C                                 = 0  OK
C                                 = 1  cannot open RUNCARTS file
C                                 = 2  file is empty
C                                 = 3  too many runs, increase ALK7COM
C - open RUNCARTSLIST which contains for each run the number
C   of the CERN cartridges in the order : RAW/POT/DST/MINI/NANO
C   on unit LURUK7
C ---------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C! keep content of RUNCARTS.LIST file
C! keep content of RUNCARTS.LIST file
      PARAMETER(MXLRUN=2500,MXSEG=4,MXTYP=5,LK7=9)
      INTEGER K7LRUN
C - 22500=MXLRUN*LK7 (9 characters per K7 for MXLRUN runs)
      CHARACTER*22500 K7CART
C - 5=MXTYP the number of various data types ('RPDMN')
      CHARACTER*5 K7TYPE
      COMMON/ALK7COM/ K7SEG,K7LINE(MXSEG),K7LRUN(MXLRUN,MXSEG),
     &                K7CART(MXTYP,MXSEG),K7TYPE
      CHARACTER DUMMY*80
      SAVE NK7
      DATA IFI /0/
C
C ------------------------------------------------------------------
C
C - execute only once
      IER = 0
      IF (IFI.EQ.0) THEN
       IFI = 1
       NK7 = (LK7+3)/4          ! # of words to contain LK7 characters
       K7SEG = 0
       K7TYPE = 'RPDMN'
C       open the RUNCARTS.LIST file
       CALL AGTFIL ('FRK7','READ',LURUK7,IER)
       IF (IER.NE.0) THEN
          CALL AOPEN (LURUK7,'RUNCARTSLIST','CARDS','DISK',IER)
       ENDIF
       IF (IER.NE.0) THEN
          IER=1
          GOTO 999
       ENDIF
C
C       read the RUNCARTS.LIST file
       READ (LURUK7,'(A80)')DUMMY
       READ (LURUK7,'(A80)')DUMMY
       READ (LURUK7,'(A80)')DUMMY
       ISEG = 1
       K7LINE(ISEG) = 0
       LINE = 0
 1     CONTINUE
       IF (K7LINE(ISEG).GE.MXLRUN) THEN
          IF (ISEG.GE.MXSEG) THEN
             IER = 3
             CALL ALTELL ('ALK7OP:too many runs,increase ALK7COM',0,
     &                    'RETURN')
             GOTO 3
          ENDIF
          ISEG = ISEG + 1
          K7LINE(ISEG) = 0
       ENDIF
       K7LINE(ISEG) = K7LINE(ISEG)+1
       LINE = LINE+1
       LC = (K7LINE(ISEG)-1)*LK7 + 1
       LCE= LC+LK7-1
       READ (LURUK7,'(1X,I6,5(1X,A9),A23)',END=2)
     &         K7LRUN(K7LINE(ISEG),ISEG),
     &         K7CART(1,ISEG)(LC:LCE),K7CART(2,ISEG)(LC:LCE),
     &         K7CART(3,ISEG)(LC:LCE),
     &         K7CART(4,ISEG)(LC:LCE),K7CART(5,ISEG)(LC:LCE)
       GOTO 1
C - eof on RUNCARTS.LIST: decrement counters
 2     CONTINUE
       LINE = LINE-1
       K7LINE(ISEG) = K7LINE(ISEG)-1
 3     LC = LINE*LK7
       WRITE (IW(6),*)' *** ALK7OP*** RUNCARTS.LIST on unit ',LURUK7,
     &                   ' contains ',LINE,' runs'
C   Parity error or eof : no information on this file
       IF (LINE.LE.0) THEN
          IER = 2
          ISEG = 0
       ENDIF
       K7SEG  = ISEG
      ENDIF
C
 999   ALK7OP = IER
       END
