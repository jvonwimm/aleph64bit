      SUBROUTINE ASKUSE(IDPR,ISTAT,NTRK,NVRT,ECMS,WEIT)
C---------------------------------------------------------------
C! EVENT GENERATION ROUTINE FOR PHOT02
C  This is the main controlling routine for event
C   generation.
C    There are three main stages:
C     1) SPRING is called to generate the 'QED' stage
C         via F(X)
C     2) Various routines are called to set up the LUND
C         arrays, and then LUEXEC is called
C     3) The ALEPH banks are filled from the LUND arrays
C
C OUTPUT parameters :
C          IDPR   : process identification,each digit corresponds to
C          the flavor of the event ( several flavors /event is possible)
C          ISTA   : status flag ( 0 means ok), use it to reject
C                   unwanted events
C          NTRK   : number of tracks generated and kept
C                  (i.e. # KINE banks  written)
C          NVRT   : number of vertices generated
C                   (i.e. # VERT banks written)
C          ECMS   : center of mass energy for the event (may be
C                   different from nominal cms energy)
C          WEIT   : event weight ( not 1 if a weighting method is used)
C
C CALLED BY - KINGAL main routine
C CALLS     -   I'll get back to you on that one.
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------
      LOGICAL KEEP,FIRST
      INTEGER ICALL,IER,F2TYPE
      PARAMETER(F2TYPE=110)
      REAL VRTEX(4),SDVRT(3)
       COMMON / LABVA / EW,E3,E5,E6,E7,PW,P3,P5,P6,P7,GAM1,GAM2,
     *                  AL3,BE5,CTW,STW,CT3,ST3,CT5,ST5,CT6,ST6,
     *                  CT7,ST7,CP3,SP3,CP5,SP5,CP6,SP6,CP7,SP7,
     *                  W,W2
C
C COMMON DATA FOR WHOLE PROGRAM
C
C PI        PI RADIANS
C PIDEG     PI IN DEGREES ( 180)
C RADDEG    CONVERT RADIANS TO DEGREES
C ME        MASS OF ELECTRON
C ME2       ""        ""       SQUARED
C MU2       MASS OF GENERATED PARTICLE **2
C S         2 * EBEAM
C EMP
C CONST     OVERALL CONSTANT FOR F(X) CALC
C
       COMMON / DATA  / PI,PIDEG,RADDEG,ME,ME2,MU2,S,EMP,CONST
       REAL ME,ME2,MU2
C
C--- common for initialization variables ---------------------------
C
       LOGICAL LVDM,LPLUTO,LCONST,LQED,LPSEUD,
     1         MAPIN,MAPOUT,LHIST,LBREIT,LPSHOW,LDEBUG
       REAL    EBEAM,AMASS,WMIN,PTVDM,THMIN,THMAX,XSECT,PTCUT,WMAX,
     1         GAMTOT,SPIN,QFIXED
       INTEGER LUNTYP,MUNIN,MUNOUT,ICOND,ISTATS,
     1         KTYPE,IVERS,IPPART,IQMAX
C
       COMMON / MCGCOM/  LVDM,LHIST,LPLUTO,LCONST,LQED,
     *                   LPSEUD,MAPIN,MAPOUT,LDEBUG,
     *                   EBEAM,AMASS,WMIN,PTVDM,THMIN,THMAX,XSECT,XERR
     *                   ,PTCUT,LUNTYP,MUNIN,MUNOUT,ICOND,IVERS,
     *                   KTYPE,ITYPE,ISTATS(100),WMAX,GAMTOT,
     *                   LBREIT,IPPART,LPSHOW,SPIN,IQMAX,QFIXED

      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (L2PAR=500, L2PARF=2000 )
      PARAMETER (LJNPAR=4000)
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),
     &                KFDP(L2PARF,5)
      COMMON /LUDAT4/ CHAF(L2PAR)
      CHARACTER*8 CHAF
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5)
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      DATA VRTEX/0.0,0.0,0.0,0.0/
      DATA SDVRT/0.0180,0.0010,1.00 /
C CONTENTS OF ISTATS ERROR ARRAY
C 1-9 ERRORS FROM MUMU0
C 20 ERRORS IN SPRING
C 30 TOTAL LUND ERRORS
C 50-59 LUND ERROR CODES FROM MSTU(24)
C 60-69 LUND ERROR CODES FROM MSTU(28)
       DATA  ICALL/0/,FIRST/.TRUE./
       SAVE ICALL,FIRST
C
C ON FIRST CALL INITIALISE SDVRT
C
      IF(ICALL.EQ.0)THEN
       JSVRT = NLINK('SVRT',0)
        IF(JSVRT.NE.0)THEN
          SDVRT(1) = RW(JSVRT+1)
          SDVRT(2) = RW(JSVRT+2)
          SDVRT(3) = RW(JSVRT+3)
        ENDIF
C
C FIND OUT HOW MANY EVENTS TO DEBUG
C
C  DEBUG lout / ndeb1  ndeb2
      IDB1 = 0
      IDB2 = 0
      NADEB = NAMIND('DEBU')
      JDEBU = IW(NADEB)
      IF(JDEBU.NE.0) THEN
         IDB1 = IW(JDEBU+1)
         IDB2 = IW(JDEBU+2)
         LOUT = IW(JDEBU-2)
      ENDIF

      ENDIF
C-------------------------------------------------------------------
C INITIALISE OUTPUT ARGUMENTS
      ISTAT = 0
      NTRK = 0
      NVRT = 1
      ECMS = 2*EBEAM
      WEIT = 1.0
C-------------------------------------------------------------------
C========> PRODUCTION OF WEIGHTED EVENTS
      ICALL = ICALL + 1
      LDEBUG = .FALSE.
      IF(ICALL.GE.IDB1 .AND. ICALL.LE. IDB2) LDEBUG = .TRUE.
C==========> GENERATION
           CALL SPRING(NDIM,*1500,*1800)
           ISTATS(41) = ISTATS(41) + 1
           CALL PHOHIS
C
C    NOW fragment quark 4-vectors according to LUND
C==========> FRAGMENTATION
C FILL LUND ARRAY FROM LABVA ETC.
C
      CALL GAMLUN
C
C SET VALUE OF IDPR
C
      IDPR = ITYPE
      IF(ITYPE.EQ.10)IDPR=LUNTYP
C switch to second random number
      am = amarset(1)
C
C FILLING THE QUARK LINES FOR VDM
C  IS A BIT MORE INVOLVED...
C
      IF(LVDM) THEN
C
C CHOICE OF TWO STYLES
C
              IF(LPLUTO.OR.(W.LT.2.5))THEN
                   CALL PLUTOQ(PTSCAL,IER)
                   IF(IER.NE.0)GOTO 1600
              ELSE
                   CALL MCGRHO(PTSCAL,IER)
                   IF(IER.NE.0)GOTO 1600
              ENDIF
      ENDIF
C
C PARTON SHOWER  FIRST IF REQUESTED
C
      IF(LPSHOW) THEN
C
C CHOICE OF QMAX
C
                 IF(IQMAX.EQ.0)THEN
                    QMAX = PTSCAL
                 ELSEIF(IQMAX.EQ.1)THEN
                    QMAX = W
                 ELSEIF(IQMAX.EQ.2)THEN
                    QMAX = QFIXED
                 ELSE
                    IF(FIRST)WRITE(6,*)
     1              ' INVALID VALUE FOR IQMAX IN ASKUSE',IQMAX
                    FIRST = .FALSE.
                 ENDIF
                 CALL DOLUPS(QMAX)
      ENDIF
C
C FINALLY LUEXEC
C
      CALL LUEXEC
C--- deal with any errors produced by LUND rather than allowing
C--- them to accumulate and thereby stopping program
      MSTU(23) = 0
      IF ( MSTU(24) .NE. 0 ) THEN
               ISTATS(50+MSTU(24)) = ISTATS(50+MSTU(24)) + 1
               MSTU(24) = 0
               ISTATS(50) = ISTATS(50) + 1
               IERR = IERR + 1
               IF ( IERR .EQ. 20 ) THEN
                  WRITE(IW(6),611)
                  MSTU(22) = 2
               ENDIF
C
C--- skip rest of analysis
C              ---------
               GOTO 1000
C              ---------
      ENDIF
C--- count warnings
      IF ( MSTU(28) .NE. 0 ) THEN
               ISTATS(60+MSTU(28)) = ISTATS(60+MSTU(28)) + 1
               ISTATS(60) = ISTATS(60) + 1
      ENDIF
  611 FORMAT(//,' +++++MCGLUN :  MORE THAN 20 LUND ERRORS',
     *              ' - NO MORE MESSAGES WILL BE PRINTED +++++',//)
C
C REJECT EVENT ON BASIS OF LUND 4 VECTORS
C
      CALL AMCSEL(KEEP)
      IF(.NOT.KEEP)GOTO 1700
C
C HISTOGRAM 4 -VECTORS OF FINAL STATE PARTICLES, QUARKS, AND ELECTRONS
      CALL PHOLHS
      IF(LDEBUG)CALL LULIST(3)
C--- COPY EVENT FROM LUND ARRAY TO GALEPH BOS BANKS...
C
C GENERATE PRIMARY VERTEX
C
      CALL RANNOR (RX,RY)
      CALL RANNOR (RZ,DUM)
      VRTEX(1) = RX*SDVRT(1)
      VRTEX(2) = RY*SDVRT(2)
      VRTEX(3) = RZ*SDVRT(3)
      VRTEX(4) = 0.
C
C
      CALL KXL7AL(VRTEX,ISTAT,NVRT,NTRK)
C - Output  : ISTAT = status word ( = 0 means OK)
C                     - 1 means VERT or KINE bank missing
C                     - 2 means not enough space for VERT or KINE
C                     - 3 means too many tracks
C                     - 4 electrons beams not stored as lines 1 and 2
C                     - 5 means Lund status code larger than 4 found
C                     > 0 means unknown LUND particle# ISTAT
       IF(ISTAT.NE.0)THEN
         IF(ISTAT.GT.0)ISTAT = -6
         ISTATS(-ISTAT+70)=ISTATS(-ISTAT+70) + 1
       ELSE
C SUCCESFUL RETURN HERE
         ISTATS(42) = ISTATS(42) + 1
       ENDIF
       RETURN
C------------------------------------------------------------------
C      ERROR IN LUND COMES TO HERE
 1000  CONTINUE
       ISTAT = 3
       ISTATS(30) = ISTATS(30) + 1
       RETURN
C ERROR IN SPRING - RETURNS TO HERE
1500   ISTAT = 2
       ISTATS(20) = ISTATS(20) + 1
       RETURN
C ERROR IN MCGVDM - RETURNS HERE
1600   ISTATS(15) = ISTATS(15)+1
       ISTAT = 4
       RETURN
C EVENT REJECTED BY AMCSEL - RETURNS HERE
1700   ISTATS(40) = ISTATS(40 )+1
       ISTAT = 5
      RETURN
1900  ISTATS(16) = ISTATS(16) + 1
       RETURN
C
C TIME OUT IN SPRING - STOP RUN NOW ( UNFORTUNATELY KINGAL DOESN'T)
C
1800  ISTAT = 999
       RETURN
       END
      SUBROUTINE ASKUSI(IGCOD)
C---------------------------------------------------------------
C! EVENT GENERATOR INTIALISATION FOR PHOT02
C
C INPUT  parameters :
C                    IGCOD - GENERATOR CODE
C
C CALLED BY -   KINGAL ( MAIN ROUTINE )
C CALLS     -
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------
C
      PARAMETER(KINTYP=6007)
      PARAMETER(NVERS=113)
C
      COMMON/BASE4/NOW,NOWARI,K,XJAC,XN,RC,CALLS,FB,FB2,NPG,TI,TSI
     -            ,DV2G,NDM,IRN,ATACC,NSUC
C
C--- common for initialization variables ---------------------------
C
       LOGICAL LVDM,LPLUTO,LCONST,LQED,LPSEUD,
     1         MAPIN,MAPOUT,LHIST,LBREIT,LPSHOW,LDEBUG
       REAL    EBEAM,AMASS,WMIN,PTVDM,THMIN,THMAX,XSECT,PTCUT,WMAX,
     1         GAMTOT,SPIN,QFIXED
       INTEGER LUNTYP,MUNIN,MUNOUT,ICOND,ISTATS,
     1         KTYPE,IVERS,IPPART,IQMAX
C
       COMMON / MCGCOM/  LVDM,LHIST,LPLUTO,LCONST,LQED,
     *                   LPSEUD,MAPIN,MAPOUT,LDEBUG,
     *                   EBEAM,AMASS,WMIN,PTVDM,THMIN,THMAX,XSECT,XERR
     *                   ,PTCUT,LUNTYP,MUNIN,MUNOUT,ICOND,IVERS,
     *                   KTYPE,ITYPE,ISTATS(100),WMAX,GAMTOT,
     *                   LBREIT,IPPART,LPSHOW,SPIN,IQMAX,QFIXED

      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON / LABVAR / AUX(34)
      REAL*8 AUX
      EQUIVALENCE(AUX(1),EW)
      IGCOD = KINTYP
      IVERS = NVERS
C announce myself !
      AVERS = FLOAT(IVERS)/100.0
      WRITE(IW(6),601) IGCOD,AVERS
601   FORMAT(/////,15X,'---------------------------------------'/,
     1             15X,'-                                     -'/,
     1             15X,'-             P H O T 0 2             -'/,
     1             15X,'-            ============             -'/,
     1             15X,'-        generator code ',I5,'         -'/,
     1             15X,'-            version ',F5.2,'            -'/,
     1             15X,'-        interfaced to JETSET 7.4     -'/,
     1             15X,'-     last modification 23 FEb  2001  -'/,
     1             15X,'---------------------------------------',
     1        /////)
C--- ZERO error flag array  and LABVAR common
      CALL VZERO(ISTATS,100)
      DO 15 IAUX=1,34
 15   AUX(IAUX) = 0.D0
C   keep initial random numbers
      call rmarut(i1,i2,i3)
C
C INITIALISE ALEPH PARTICLE BANKS, AND READ ANY LUND CONTROL CARDS
C
      CALL KPARTI
C
C--- read steering cards, and perform initialisation of generator
C     variables
C
       WRITE(6,*)' Analysing data cards ...'
       CALL MCGINI
C
C--- INITIALIZE I/O ROUTINE
C
       CALL MCGDST
C
C EITHER READ IN OLD MAP ( AND OTHER PARAMETERS ) ....
C
      IF(MAPIN)THEN
C
       WRITE(IW(6),600)
600    FORMAT(/,' Reading in map ... ',/
     1 ' ( some parameters set on cards will be overriden.)')
       CALL DSTRED
C
C--- write out variables, and explain them...
C  ( Do it after reading in map ,as some parameters may be overwritten )
C
       CALL MCGCWR(1)

      ELSE

C .. OR
C IF NO MAP TO READ IN, THEN MAKE ONE
C ( MAP WILL BE WRITTEN OUT BY DSTWRT IN BASES IF REQUIRED)
C
C--- write out variables, and explain them...
C     ( Do this before map is created, in case user has made a mistake
C      and want to abort )
C
       CALL MCGCWR(2)
       WRITE(IW(6),*)
       WRITE(IW(6),*)' Generating  new map ...'
       WRITE(IW(6),*)
       CALL BSMAIN(IERR)
       IF ( IERR .NE. 0 ) THEN
        ISTATS(IERR) = ISTATS(IERR) + 1
        WRITE(IW(6),*)' MAP GENERATION  FINISHED WITH ERROR ',IERR
       ELSE
        WRITE(IW(6),*)
        WRITE(IW(6),*)' New map generated succesfully '
        WRITE(IW(6),*)
       CALL MCGCWR(3)
       ENDIF
      ENDIF
      call rmarin(i1,i2,i3)    ! reinit for event generation
C
C STORE INFORMATION IN BANK KPAR   AND RLEP
C
      CALL MCGRUN
       RETURN
       END
      SUBROUTINE KPARTI
C---------------------------------------------------------------
C! Initialise interface from LUND commons to ALEPH banks(PHOT02)
C   Calls KXL74A to allow overriding of LUND commons by data cards
C
C CALLED BY -      ASKUSI
C CALLS     -      KXL74A,EXIT,PRPART,NLINK
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C  ( OK SO I NICKED IT FROM LUND03 ON KINGAL DISK ACTUALLY )
C----------------------------------------------------------------
      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (L2PAR=500, L2PARF=2000 )
      PARAMETER (LJNPAR=4000)
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),
     &                KFDP(L2PARF,5)
      COMMON /LUDAT4/ CHAF(L2PAR)
      CHARACTER*8 CHAF
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5)
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (LPDEC=48)
      INTEGER NODEC(LPDEC)
      PMAS(LUCOMP(25),1)= 100.
      PMAS(LUCOMP( 6),1)= 100.
      PMAS(LUCOMP(23),1)= 91.2
C
C   HIGGS Mass , TOP Mass and Z0 mass defined, can be overwritten by
C   a PMA1 card
C
C -- complete PART bank with LUND  particles
C    use the library routine KXL74A
C
      CALL KXL74A (IPART,IKLIN)
C
      IF (IPART.LE.0 .OR. IKLIN.LE.0) THEN
         WRITE (IW(6),'(1X,''error in PART or KLIN bank - STOP - ''
     +                 ,2I3)') IPART,IKLIN
         CALL EXIT
      ENDIF
C
C -- get list of  particle# which should not be decayed
C    in LUND  because they are decayed in GALEPH.
C    the routines uses the KLIN bank and fills the user array
C    NODEC in the range [1-LPDEC]
      MXDEC = KNODEC (NODEC,LPDEC)
      MXDEC = MIN (MXDEC,LPDEC)
C
C -- inhibit decays in LUND
C    If the user has set some decay channels by data cards they will
C    will not be overwritten
      IF (MXDEC .GT. 0) THEN
         DO 10 I=1,MXDEC
            IF (NODEC(I).GT.0) THEN
               JIDB = NLINK('MDC1',NODEC(I))
               IF (JIDB .EQ. 0) MDCY(LUCOMP(NODEC(I)),1) = 0
            ENDIF
   10    CONTINUE
      ENDIF
      RETURN
      END

       SUBROUTINE MCGINI
C---------------------------------------------------------------
C! Analyses  the DATA cards and sets parameters accordingly (PHOT02)
C
C
C CALLED BY - ASKUSI
C CALLS     - EXIT
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------
C------------------------------------------------------------------
C INITIALISE VARIABLES BASED ON VALUES ON DATA CARD,
C OR USE SENSIBLE DEFAULTS OTHERWISE
C
C A.J.FINCH 24/11/88
C  A.J. Finch last mod AUGUST 1990
C-------------------------------------------------------------------
      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (L2PAR=500, L2PARF=2000 )
      PARAMETER (LJNPAR=4000)
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),
     &                KFDP(L2PARF,5)
      COMMON /LUDAT4/ CHAF(L2PAR)
      CHARACTER*8 CHAF
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5)
C
C
C--- common for initialization variables ---------------------------
C
       LOGICAL LVDM,LPLUTO,LCONST,LQED,LPSEUD,
     1         MAPIN,MAPOUT,LHIST,LBREIT,LPSHOW,LDEBUG
       REAL    EBEAM,AMASS,WMIN,PTVDM,THMIN,THMAX,XSECT,PTCUT,WMAX,
     1         GAMTOT,SPIN,QFIXED
       INTEGER LUNTYP,MUNIN,MUNOUT,ICOND,ISTATS,
     1         KTYPE,IVERS,IPPART,IQMAX
C
       COMMON / MCGCOM/  LVDM,LHIST,LPLUTO,LCONST,LQED,
     *                   LPSEUD,MAPIN,MAPOUT,LDEBUG,
     *                   EBEAM,AMASS,WMIN,PTVDM,THMIN,THMAX,XSECT,XERR
     *                   ,PTCUT,LUNTYP,MUNIN,MUNOUT,ICOND,IVERS,
     *                   KTYPE,ITYPE,ISTATS(100),WMAX,GAMTOT,
     *                   LBREIT,IPPART,LPSHOW,SPIN,IQMAX,QFIXED

       CHARACTER*4 TTYPE,TYPE,CHAINT,KEY,CTEMP
       CHARACTER*16 NAME
      COMMON / BASE1 / NDIM,NCUBES,NTRIAL,ITT1,IGRAPH,IFLAG,NCOND
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C
C
C COMMON DATA FOR WHOLE PROGRAM
C
C PI        PI RADIANS
C PIDEG     PI IN DEGREES ( 180)
C RADDEG    CONVERT RADIANS TO DEGREES
C ME        MASS OF ELECTRON
C ME2       ""        ""       SQUARED
C MU2       MASS OF GENERATED PARTICLE **2
C S         2 * EBEAM
C EMP
C CONST     OVERALL CONSTANT FOR F(X) CALC
C
       COMMON / DATA  / PI,PIDEG,RADDEG,ME,ME2,MU2,S,EMP,CONST
       REAL ME,ME2,MU2

      REAL    PIBY2
      LOGICAL MAPSET
C LUELEC = LUND TYPE CODE FOR ELECTRON
      PARAMETER(LUELEC=11)
      PARAMETER(PIBY2=1.570796327)
      PARAMETER(AM2PI=2*0.1397)
      DATA    MAPSET/.FALSE./
      XSECT = 0.0
CBB   this is confusing as RANMAR is set up by RMAR card !!!
C  RNDM N_RNDM - seed the random number generator
C
C      ISEED = 1234567
C      NARND = NAMIND('RNDM')
C      JRNDM = IW(NARND)
C      IF(JRNDM.NE.0) THEN
C       IF(IW(JRNDM).GE.1) ISEED =IW(JRNDM+1)
C      ENDIF
C      CALL RNSET(ISEED)
C      WRITE(6,*)' Random number seeded with :',ISEED
CBB
C   OVERALL CONTROL - CARD GCON
C
C SET DEFAULTS
C
C ( GENERATE NEW MAP OF VDM AT 46.1 GEV )
C
      INLUN = 0
      MUNIN = 23
      MAPIN = .FALSE.
      MAPOUT = .TRUE.
      MUNOUT = 23
      IHIS  = 1
      ITYPE = 100
      EBEAM = 46.1
      WMIN  = 1.5
      WMAX   = 0.0
      GAMTOT = 0.0
      AMASS = 0.0
      IPPART = 0
C
C LOOK FOR CONTROL CARD 'GCON'
C
      IGCON = IW(NAMIND('GCON'))
      IF(IGCON.GT.0)THEN
       LEN = IW(IGCON)
C
C EXTRACT ALL SET VALUES FROM GCON
C
       IF(LEN.GE.1)INLUN = IW(IGCON+1)
       IF(LEN.GE.2)IHIS  = IW(IGCON+2)
       IF(LEN.GE.3)ITYPE = IW(IGCON+3)
       IF(LEN.GE.4)EBEAM = RW(IGCON+4)
       IF(LEN.GE.5)WMIN  = RW(IGCON+5)
       IF(LEN.GE.6)AMASS = RW(IGCON+6)
       IF(LEN.GE.7)IPPART = IW(IGCON+7)
       IF(LEN.GE.8)WMAX  = RW(IGCON+8)
      ENDIF
C
C - Print PART and KLIN banks
      IF (IPPART.GT.0) CALL PRPART
C
C SET VARIABLES IN COMMON MCGCOM ACCORDINGLY
C
      IF(INLUN.EQ.0) THEN
          MAPIN  = .FALSE.
          MAPOUT = .TRUE.
      ELSEIF(INLUN.EQ.1)THEN
          MAPIN   = .TRUE.
          MAPOUT  = .FALSE.
      ELSEIF(INLUN.EQ.-1)THEN
          MAPIN   = .FALSE.
          MAPOUT  = .FALSE.
      ENDIF
C     Now for usage in the big production machinary, one needs to have some
C uniformity in the way to use data cards and final states :
C   rather input center of mass energy than Ebeam and use the same numbering
C  scheme for final states!
      IGPRO = IW(NAMIND('GPRO'))
      IF(IGPRO.GT.0) then
        EBEAM = 0.5*RW(IGPRO+1)
        JTYPE = IW(IGPRO+2)
        IF(JTYPE.eq.1)  ITYPE = 12 ! ddbar
        IF(JTYPE.eq.2)  ITYPE = 11 ! uubar
        IF(JTYPE.eq.3)  ITYPE = 13 ! ssbar
        IF(JTYPE.eq.4)  ITYPE = 14 ! ccbar
        IF(JTYPE.eq.5)  ITYPE = 15 ! ddbar
        IF(JTYPE.eq.10)  ITYPE = 10 ! udbar
        IF(JTYPE.eq.11)  ITYPE = 1 ! electrons
        IF(JTYPE.eq.13)  ITYPE = 2 ! muons
        IF(JTYPE.eq.15)  ITYPE = 3 ! taus
      ENDIF
C
C SET THE VARIABLE KTYPE DEPENDING ON ITYPE
C
      KTYPE = -1
      IF(ITYPE.EQ.1 )KTYPE=INTCHA('ELEC')
      IF(ITYPE.EQ.2 )KTYPE=INTCHA('MUON')
      IF(ITYPE.EQ.3 )KTYPE=INTCHA('TAU ')
      IF(ITYPE.EQ.10)KTYPE=INTCHA('UD  ')
      IF(ITYPE.EQ.11)KTYPE=INTCHA('U   ')
      IF(ITYPE.EQ.12)KTYPE=INTCHA('D   ')
      IF(ITYPE.EQ.13)KTYPE=INTCHA('S   ')
      IF(ITYPE.EQ.14)KTYPE=INTCHA('C   ')
      IF(ITYPE.EQ.15)KTYPE=INTCHA('B   ')
      IF(ITYPE.EQ.16)KTYPE=INTCHA('T   ')
      IF(ITYPE.EQ.100)KTYPE=INTCHA('VDM ')
      IF(ITYPE.EQ.101)KTYPE=INTCHA('PI0 ')
      IF(ITYPE.EQ.102)KTYPE=INTCHA('ETA ')
      IF(ITYPE.EQ.104)KTYPE=INTCHA('ETAC')
      IF(ITYPE.EQ.105)KTYPE=INTCHA('ETAB')
      IF(ITYPE.EQ.110)KTYPE=INTCHA('F2  ')
      IF(ITYPE.EQ.200)KTYPE=INTCHA('RESO')
      IF(KTYPE.EQ.-1)THEN
        WRITE(IW(6),*)' WARNING FROM MCGINI:'
        WRITE(IW(6),*)' ITYPE OF ',ITYPE,' ON GCON CARD NOT RECOGNISED '
        WRITE(IW(6),*)' TAKING DEFAULT VALUE OF 100 '
        ITYPE = 100
        KTYPE = INTCHA('VDM ')
      ENDIF
C
C IF USER HAS REQUESTED TYPE RESO SHE MUST PROVIDE GRES CARD
C
      IF(ITYPE.EQ.200)THEN
        IGRES = IW(NAMIND('GRES'))
        IF(IGRES.GT.0)LEN = IW(IGRES)
        IF(IGRES.GT.0.AND.LEN.GT.0.AND.AMASS.GT.0.0)THEN
C
C GRES LUNTYP, WIDTH, SPIN
C
           LUNTYP =  IW(IGRES+1)
           CALL LUNAME(LUNTYP,NAME)
           KTYPE=INTCHA(NAME(1:4))
           GAMTOT = 0.01
           IF(LEN.GE.2)GAMTOT = RW(IGRES+2)
           LVDM    = .FALSE.
           LQED    = .FALSE.
           LBREIT  = .FALSE.
           LPSEUD  = .TRUE.
           IF(GAMTOT.GT.0)LBREIT = .TRUE.
           IF(LBREIT)LPSEUD = .FALSE.
           IF(LEN.GE.3)SPIN   = RW(IGRES+3)
           WRITE(6,*)' Resonance parameters :'
           WRITE(6,*)NAME,' Mass ',AMASS,' Width ',GAMTOT,' Spin ',SPIN
        ELSE
         WRITE(6,*)' GCON CARD requested type 200 , but GRES '
         WRITE(6,*)'  card not provided, or zero length ---- STOP '
         CALL EXIT
        ENDIF
      ELSE
C
C NOW SET ALL THE THINGS THAT DEPEND ON THE TYPE
C
        TYPE  = CHAINT(KTYPE)
        LUNTYP = MCGTYP(TYPE)
      ENDIF
C
C IF AMASS NOT SET ON CONTROL CARD
C
      IF(AMASS.EQ.0.0) THEN
            AMASS = ULMASS(LUNTYP)
            IF(ITYPE.EQ.10.OR.ITYPE.EQ.11.OR.ITYPE.EQ.12)AMASS = 0.325
            IF(ITYPE.EQ.13)AMASS= 0.500
            IF(ITYPE.EQ.14)AMASS= 1.6
      ENDIF
C
C IF WMAX NOT SET ON CONTROL CARD SET TO MAX POSSIBLE
C
      IF(WMAX.EQ.0.0) WMAX = 2*(EBEAM-ULMASS(LUELEC))
C
C SET THE VARIOUS LOGICAL FLAGS ( EXCEPT FOR USER DEFINED
C   RESONANCE CASE )
C
C DEFAULT IS QED TYPE FRAGMENTATION
C
      IF(ITYPE.NE.200)THEN
       LVDM    = .FALSE.
       LQED    = .TRUE.
       LPSEUD  = .FALSE.
       LBREIT  = .FALSE.
       IF(TYPE.EQ.'VDM ')THEN
        LVDM = .TRUE.
        LQED = .FALSE.
       ENDIF
       IF(TYPE.EQ.'ETAB'.OR.TYPE.EQ.'ETAC'.OR.TYPE.EQ.'ETA '.OR.TYPE.EQ.
     1           'PI0 ')THEN
        LPSEUD = .TRUE.
        LQED   = .FALSE.
        SPIN   = 0
        GAMTOT = 0.0
       ENDIF
       IF(TYPE.EQ.'F2  ')THEN
         LBREIT = .TRUE.
         LQED   = .FALSE.
         GAMTOT = 0.178
         AMASS  = 1.273
         WMIN   = AMASS - 3*GAMTOT
         WMAX   = AMASS + 3*GAMTOT
C    bug found Alison W. feb 97 , SPIN is 2. not 1.
         SPIN   = 2.0
         IF(WMIN.LT.0.0)WMIN = 0.0
         WTEST =  2*(EBEAM-ULMASS(LUELEC))
         IF(WMAX.GT.WTEST) WMAX = WTEST
       ENDIF
      ENDIF
C
      LHIST = .TRUE.
      IF(IHIS.EQ.0)LHIST = .FALSE.
      IF(IHIS.EQ.1)LHIST = .TRUE.
C
C CHECK WMIN IS VALID FOR THESE SETTINGS
C
      IF( LQED .AND.(WMIN.LT.(2*AMASS)))THEN
        WRITE(IW(6),*)' WMIN WAS SET TO ',WMIN
        WMIN=2*AMASS
        WRITE(IW(6),*)' THIS WAS TOO LOW - HAVE RESET IT TO ',WMIN
      ENDIF
      IF(LQED.AND.(ITYPE.GE.10).AND.(WMIN.LT.(2*AMASS+PARJ(32))))THEN
        WRITE(IW(6),*)' WMIN WAS SET TO ',WMIN
        WMIN=2*AMASS+PARJ(32)
        WRITE(IW(6),*)' THIS WAS TOO LOW - HAVE RESET IT TO ',WMIN
      ENDIF
      IF(LVDM.AND.WMIN.LT.AM2PI)THEN
        WRITE(IW(6),*)' WMIN WAS SET TO ',WMIN
        WMIN=AM2PI
        WRITE(IW(6),*)' THIS WAS TOO LOW - HAVE RESET IT TO ',WMIN
        WRITE(IW(6),*)' ( TWICE THE MASS OF A PION ) '
      ENDIF
      IF(LVDM.AND.WMIN.LT.AM2PI+PARJ(32))THEN
        WRITE(IW(6),*)' WARNING WITH A WMIN THIS LOW, LUND WILL PRODUCE'
     1  ,' WARNING MESSAGES '
        WRITE(IW(6),*)' YOU HAVE BEEN WARNED !'
      ENDIF
C
C 2) GVDM VDM CONTROL CARD
C
      PTVDM  = 2.0
      IWVDM  = 1
      LPLUTO = .FALSE.
C
C LOOK FOR CONTROL CARD 'GVDM'
C
      IGCARD = IW(NAMIND('GVDM'))
      IF(IGCARD.GT.0)THEN
       LEN = IW(IGCARD)
       IF(LEN.GE.1) PTVDM = RW(IGCARD+1)
       IF(LEN.GE.2) IWVDM = IW(IGCARD+2)
       IF(LEN.GE.3) IPLUTO = IW(IGCARD+3)
      ENDIF
      LCONST = .TRUE.
      IF(IWVDM.EQ.1)LCONST = .TRUE.
      IF(IWVDM.EQ.2)LCONST = .FALSE.
      IF(IWVDM.NE.1.AND.IWVDM.NE.2)WRITE(IW(6),*)
     1' MCGINI - WARNING : IWVDM ON GVDM CARD MUST BE 1 OR 2, NOT '
     1,IWVDM
      IF(IPLUTO.EQ.1)LPLUTO = .TRUE.
C
C 3)  GCUT GEOMETRICAL CUTS  CONTROL CARD
C
C SET DEFAULTS
C
C ICOND = 0, THMIN = 0.0, THMAX = PI
C
      ICOND = 0
      THMIN = 0.0
      THMAX  = 1.570796327
C
C LOOK FOR CONTROL CARD 'GCUT'
C
      IGCARD = IW(NAMIND('GCUT'))
      IF(IGCARD.GT.0)THEN
       LEN = IW(IGCARD)
       IF(LEN.GE.1) ICOND = IW(IGCARD+1)
       IF(LEN.GE.2) THMIN = RW(IGCARD+2)
       IF(LEN.GE.3) THMAX = RW(IGCARD+3)
      ENDIF
C
C 4)  GQPM QPM CONTROL CARD
C
C SET DEFAULTS
C
C PTCUT = -1.0 ( NO CUT )
C
      PTCUT = -1.0
C
C LOOK FOR CONTROL CARD 'GQED'
C
      IGCARD = IW(NAMIND('GQED'))
      IF(IGCARD.GT.0)THEN
       LEN = IW(IGCARD)
       IF(LEN.GE.1) PTCUT = RW(IGCARD+1)
      ENDIF
C
C 5)  GPSH CONTROL CARD
C
C SET DEFAULTS
C
C LPSHOW = FALSE
C
      LPSHOW = .FALSE.
      IQMAX  =  0
      QFIXED =  1.0
C
C IF THE TYPE OF PARTICLE CAN'T BE PASSED TO LUSHOW,
C DON'T LOOK FOR THE CARD
C
      IF(ITYPE.GT.3.AND.ITYPE.LT.101)THEN
C
C LOOK FOR CONTROL CARD 'GPSH'
C
      IGCARD = IW(NAMIND('GPSH'))
        IF(IGCARD.GT.0)THEN
         LPSHOW = .TRUE.
         LEN = IW(IGCARD)
         IF(LEN.GE.1) IQMAX  = IW(IGCARD+1)
         IF(LEN.GE.2) QFIXED = RW(IGCARD+2)
C
C PARAMETER IQMAX - CHOOSE SCALE SETTING
C          0 - PT SCALE
C          1 - W
C          2 - FIXED     ( VALUE IS SECOND PARAM IF GIVEN )
        ENDIF
      ENDIF
C-------- END OF PARAMETER SETTING -----------------
C
C CHECK THAT PARAMETERS ARE VALID
C
       IF ( 2*EBEAM .LT. (ME+AMASS) ) THEN
          WRITE(6,*)' YOU CAN NOT GENERATE A MASS OF',AMASS,
     1                      'WITH A BEAM ENERGY OF ',EBEAM
          CALL EXIT
       ENDIF
C
C INITIALISE VARIABLES IN DATA COMMON,
C AND BASES BASE1 COMMON
C
C SET PHYSICS DATA
      PI     = ACOS(-1.0)
      PIDEG  = 180
      RADDEG = PIDEG / PI
      ME     = 0.000511
      ME2    = ME*ME
      S     = 4.0 * EBEAM * EBEAM
      MU2   = AMASS * AMASS
      EMP   = ME2 / ( EBEAM + SQRT( (EBEAM-ME) * (EBEAM+ME) ) )
C SET NUMBER OF DIMENSIONS OF INTEGRATION ( IN F(X))
C FOR BASES
       NDIM = 5
       IF(LQED)NDIM  = 7
       IFLAG = 0
C
1000  RETURN
C 99  CALL EXIT
      END
      SUBROUTINE   MCGRUN
C---------------------------------------------------------------
C!  Build the KPAR and  RLEP bank    (PHOT02)
C
C CALLED BY - ASKUSI
C CALLS     - ALTABL,PRTABL
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------

C --------------------------------------------------------------------
C - AJF ,BBL
      PARAMETER (LMCGCO=20)
      INTEGER ALTABL,ITABL,ALRLEP
      EXTERNAL ALTABL,ALRLEP
      DIMENSION TABL(LMCGCO),ITABL(LMCGCO)
C
C--- common for initialization variables ---------------------------
C
       LOGICAL LVDM,LPLUTO,LCONST,LQED,LPSEUD,
     1         MAPIN,MAPOUT,LHIST,LBREIT,LPSHOW,LDEBUG
       REAL    EBEAM,AMASS,WMIN,PTVDM,THMIN,THMAX,XSECT,PTCUT,WMAX,
     1         GAMTOT,SPIN,QFIXED
       INTEGER LUNTYP,MUNIN,MUNOUT,ICOND,ISTATS,
     1         KTYPE,IVERS,IPPART,IQMAX
C
       COMMON / MCGCOM/  LVDM,LHIST,LPLUTO,LCONST,LQED,
     *                   LPSEUD,MAPIN,MAPOUT,LDEBUG,
     *                   EBEAM,AMASS,WMIN,PTVDM,THMIN,THMAX,XSECT,XERR
     *                   ,PTCUT,LUNTYP,MUNIN,MUNOUT,ICOND,IVERS,
     *                   KTYPE,ITYPE,ISTATS(100),WMAX,GAMTOT,
     *                   LBREIT,IPPART,LPSHOW,SPIN,IQMAX,QFIXED

      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
       EQUIVALENCE (ITABL(1),TABL(1))
       TABL(1)  = FLOAT(ITYPE)
       TABL(2)  = EBEAM
       TABL(3)  = WMIN
       TABL(4)  = PTVDM
C
C PACK FLAGS INTO ONE INTEGER
C
C  STYLE = 1   VDM
C          2   QED
C          3   PSEUOSCALAR
C          4   BREIT WIGNER
C
C   LPSHOW *10
C   LPLUTO *100
C   LCONST *1000
C   IQMAX  *10000
C
       IF(LVDM)ISTYLE = 1
       IF(LQED)ISTYLE = 2
       IF(LPSEUD)ISTYLE = 3
       IF(LBREIT)ISTYLE = 4
       IF(LPSHOW)ISTYLE = ISTYLE   + 10
       IF(LPLUTO)ISTYLE = ISTYLE   + 100
       IF(LCONST)ISTYLE = ISTYLE   + 1000
                 ISTYLE = ISTYLE   + 10000*IQMAX
       TABL(5)  = FLOAT(ISTYLE)
       TABL(6)  = FLOAT(IVERS)
       TABL(7)  = FLOAT(ICOND)
       TABL(8)  = THMIN
       TABL(9)  = THMAX
       TABL(10)  = XSECT
       TABL(11)  = XERR
       TABL(12)  = AMASS
       TABL(13)  = PTCUT

       TABL(14)  = 0.0
       TABL(15)  = 0.0
       TABL(16)  = 0.0
       JSVRT = NLINK('SVRT',0)
       IF(JSVRT.NE.0)THEN
            TABL(14) = RW(JSVRT + 1)
            TABL(15) = RW(JSVRT + 2)
            TABL(16) = RW(JSVRT + 3)
       ENDIF

       TABL(17)  = WMAX
       TABL(18)  = GAMTOT
       TABL(19)  = SPIN
       TABL(20)  = QFIXED

C  Fill the KPAR bank with the generator parameters
       NCOL = 20
       NROW = 1
       JKPAR = ALTABL('KPAR',NCOL,NROW,TABL,'F','C')
       CALL PRTABL('KPAR',0)
C  Fill RLEP bank
       IEBEAM = NINT(EBEAM *1000  )
       JRLEP = ALRLEP(IEBEAM,'    ',0,0,0)
       CALL PRTABL('RLEP',0)
      RETURN
      END
      SUBROUTINE USCJOB
C---------------------------------------------------------------
C! (PHOT02)
C  <purpose>
C
C INPUT  parameters :
C
C OUTPUT parameters :
C
C CALLED BY -
C CALLS     -
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------

C END OF JOB ROUTINE
C
C--- common for initialization variables ---------------------------
C
       LOGICAL LVDM,LPLUTO,LCONST,LQED,LPSEUD,
     1         MAPIN,MAPOUT,LHIST,LBREIT,LPSHOW,LDEBUG
       REAL    EBEAM,AMASS,WMIN,PTVDM,THMIN,THMAX,XSECT,PTCUT,WMAX,
     1         GAMTOT,SPIN,QFIXED
       INTEGER LUNTYP,MUNIN,MUNOUT,ICOND,ISTATS,
     1         KTYPE,IVERS,IPPART,IQMAX
C
       COMMON / MCGCOM/  LVDM,LHIST,LPLUTO,LCONST,LQED,
     *                   LPSEUD,MAPIN,MAPOUT,LDEBUG,
     *                   EBEAM,AMASS,WMIN,PTVDM,THMIN,THMAX,XSECT,XERR
     *                   ,PTCUT,LUNTYP,MUNIN,MUNOUT,ICOND,IVERS,
     *                   KTYPE,ITYPE,ISTATS(100),WMAX,GAMTOT,
     *                   LBREIT,IPPART,LPSHOW,SPIN,IQMAX,QFIXED

      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C-------------------------------------------------------------------
      call ugtsec    ! this will create the KSEC bank sometimes ...
      WRITE(IW(6),62)
62    FORMAT('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! END OF JOB !!!!!
     1!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ')
       WRITE(IW(6),101)
  101  FORMAT(//20X,'EVENTS STATISTICS',
     &         /20X,'*****************')
       WRITE(IW(6),102) ISTATS(41) ,ISTATS(42),ISTATS(41)-ISTATS(42)
  102  FORMAT(/5X,'# OF GENERATED EVENTS                      = ',I10,
     &        /5X,'# OF ACCEPTED  EVENTS                      = ',I10,
     &        /5X,'# OF REJECTED  EVENTS (ISTA # 0 in ASKUSE) = ',I10)
      WRITE(IW(6),63)ISTATS(41)
63    FORMAT('   ',I10,' EVENTS WERE PASSED TO SPRING ')
      WRITE(IW(6),65)ISTATS(40)
65    FORMAT('   ',I10,' EVENTS WERE REJECTED BY AMCSEL')
      WRITE(IW(6),64)ISTATS(42)
64    FORMAT('   ',I10,' EVENTS WERE SUCCESFULLY GENERATED')
      WRITE(IW(6),600)(ISTATS(I),I=1,9)
600   FORMAT(/,/,10X,'   ERRORS ACCUMULATED BY THIS JOB',
     1' IN PHOT02 ROUTINES:'/,/,
     1' ERROR CODE FROM BASES      1     2     3     4     5     6    '
     1,' 7     8     9',/,
     1'                       ',9(2X,I4),/)
      WRITE(IW(6),611)ISTATS(15)
611   FORMAT(' ERROR RETURNS FROM MCGVDM     :',I10,/)
      WRITE(IW(6),601)ISTATS(20)
      WRITE(IW(6),612)ISTATS(16)
612   FORMAT(' ERROR RETURNS FROM MCGF2      :',I10,/)
601   FORMAT(' ERROR RETURNS FROM SPRING     :',I10,/)
      WRITE(IW(6),602)ISTATS(30)
602   FORMAT(' TOTAL ERROR RETURNS FROM LUND :',I10,/)
      WRITE(IW(6),603)(ISTATS(I),I=50,59)
603   FORMAT(' MSTU(24) ERROR CODES (FATAL ERRORS) ',/,
     1'     1     2     3     4     5     6     7     8     9    10',/
     210(2X,I4),/)
      WRITE(IW(6),604)(ISTATS(I),I=60,69)
604   FORMAT(' MSTU(28) ERROR CODES (WARNINGS) ',/,
     1'     1     2     3     4     5     6     7     8     9    10',/
     210(2X,I4),/)
      WRITE(IW(6),605)(ISTATS(I),I=70,79)
605   FORMAT(' KXLUAL ERROR CODES   ',/,
     1'     1     2     3     4     5     6     7     8     9    10',/
     210(2X,I4),/)
      WRITE(IW(6),62)
      RETURN
      END
      SUBROUTINE USKRIN
      RETURN
      END
      SUBROUTINE XKSECT
      RETURN
      END
      SUBROUTINE UGTSEC
C-----------------------------------------
C
C   Author   :- bloch                 28-APR-1995
C
C=========================================
C
C   Purpose   : fill KSEC bank from whatever source available
C   Inputs    :
C   Outputs   :
C
C=========================================
C
C--- common for initialization variables ---------------------------
C
       LOGICAL LVDM,LPLUTO,LCONST,LQED,LPSEUD,
     1         MAPIN,MAPOUT,LHIST,LBREIT,LPSHOW,LDEBUG
       REAL    EBEAM,AMASS,WMIN,PTVDM,THMIN,THMAX,XSECT,PTCUT,WMAX,
     1         GAMTOT,SPIN,QFIXED
       INTEGER LUNTYP,MUNIN,MUNOUT,ICOND,ISTATS,
     1         KTYPE,IVERS,IPPART,IQMAX
C
       COMMON / MCGCOM/  LVDM,LHIST,LPLUTO,LCONST,LQED,
     *                   LPSEUD,MAPIN,MAPOUT,LDEBUG,
     *                   EBEAM,AMASS,WMIN,PTVDM,THMIN,THMAX,XSECT,XERR
     *                   ,PTCUT,LUNTYP,MUNIN,MUNOUT,ICOND,IVERS,
     *                   KTYPE,ITYPE,ISTATS(100),WMAX,GAMTOT,
     *                   LBREIT,IPPART,LPSHOW,SPIN,IQMAX,QFIXED
       COMMON / RESULT / AVGI,SDEV,RDUMMY(2)
C
      PARAMETER(KINTYP=6007)
C
C--------------------------------------------------
      IF (LQED) then
C create cross section bank
         NTOT = ISTATS(42)
         crosec = XSECT
         croser = XERR
         if (XSECT.eq.0.) then   ! try from last integration in bases
           crosec = AVGI
           croser = SDEV
         endif   
         XTOT = crosec*1000.
         RTOT = croser*1000.
         IS = 1
         IDC = KINTYP
         IVER = IVERS
         NACC = NTOT
         XACC = XTOT
         RACC = RTOT
         isec = KSECBK(IS,IDC,IVER,NTOT,NACC,XTOT,RTOT,XACC,RACC)
         call prtabl('KSEC',0)
      else
      PRINT 100
      endif
 100  format(' UGTSEC : THIS DUMMY ROUTINE may be replaced ',
     &       ' sometimes... by the creation of the KSEC bank ',
     &       ' for other options than the QED one')
  999 RETURN
      END
