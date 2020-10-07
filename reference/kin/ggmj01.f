C*HE 07/05/93 12:44:11 C
C*DK ASKUSE
      SUBROUTINE ASKUSE(IDPR,ISTAT,NTRK,NVRT,ECMS,WEIT)
C------------------------------------------------------------
C! Interface routine for GGMJ01 generator
C  Generates one event
C  A.J. Finch last mod 15/2/93
C     structure : subroutine
C     output arguments :
C          IDPR   : process identification,each digit corresponds to
C          the flavor of the evnt ( several flavors /event is possible)
C          ISTA   : status flag ( 0 means ok), use it to reject
C                   unwanted events
C          NTRK   : number of tracks generated and kept
C                  (i.e. # KINE banks  written)
C          NVRT   : number of vertices generated
C                   (i.e. # VERT banks written)
C          ECMS   : center of mass energy for the event (may be
C                   different from nominal cms energy)
C          WEIT   : event weight ( not 1 if a weighting method is used)
C -----------------------------------------------------------------
      REAL*8   FNCMNJ
      EXTERNAL FNCMNJ
      LOGICAL KEEP
      INTEGER ICALL,IER
      REAL VRTEX(4),SDVRT(3)
      INTEGER ISTATS
      COMMON/CMJIF/ISTATS(100),IVERS
C*CA LUNDCOM
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
C*CC LUNDCOM
C*CA EVTPRM
C-------------EVTPRM   file-----------------------------------
      REAL*8          RS, EMINS, EPLUS, EBEAM, XG(25), EGAM1, EGAM2
     >              , CS, EHAT, EGAM10, EGAM20, PTMIN
      INTEGER*4       NRTYPE, NRDUMY, NPRC, INDX
      COMMON /EVTPRM/ RS, EMINS, EPLUS, EBEAM, XG  , EGAM1, EGAM2
     >              , NRTYPE, NRDUMY
     >              , CS, EHAT, EGAM10, EGAM20, PTMIN
     >              , NPRC, INDX
C
C   RS     ; Center of mass  energy
C   EMINS  ; not used
C   EPLUS  ;
C   EBEAM  ; Beam energy
C   XG(0)  ; Randum variables for event generation/
C
C   CS     ; Cos(th) of the event
C   EHAT   ; CMS Energy (Hard Collision)
C
C   EGAM10 ; X1*EBEAM
C   EGAM20 ; X2*EBEAM
C   EGAM1  ; X1*X3*EBEAM
C   EGAM2  ; X2*X4*EBEAM
C   PTMIN  ; Minimum Pt of a jet.
C*CC EVTPRM
C*CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C*CC BCS
C
      DATA MXTRY/2000/
      DATA VRTEX/0.0,0.0,0.0,0.0/
      DATA SDVRT/0.0135,0.0010,1.00 /
      DATA ICREJ/0/
      DATA  ICALL/0/
C-------------------------------------------------------------------
C
C ON FIRST CALL INITIALISE SDVRT
C
C-------------------------------------------------------------------
      IF(ICALL.EQ.0)THEN
       JSVRT = NLINK('SVRT',0)
        IF(JSVRT.NE.0)THEN
          SDVRT(1) = RW(JSVRT+1)
          SDVRT(2) = RW(JSVRT+2)
          SDVRT(3) = RW(JSVRT+3)
        ENDIF
      ENDIF
C-------------------------------------------------------------------
C
C INITIALISE OUTPUT ARGUMENTS
C
C-------------------------------------------------------------------
      ISTAT = 0
      NTRK = 0
      NVRT = 1
      ECMS = 2*EBEAM
      WEIT = 1.0
C-------------------------------------------------------------------
C========> PRODUCTION OF EVENTS
      ICALL = ICALL + 1
C==========> GENERATION
      CALL SPRING(FNCMNJ , MXTRY, IRET)
      ISTATS(41) = ISTATS(41) + 1
C-------------------------------------------------------------------
C
C IF EVENT GENERATED OK ( AT PARTON LEVEL )
C   - PUT IT IN LUND COMMON, AND HADRONIZE WITH LUND
C
C-------------------------------------------------------------------
      IF( IRET .GT. 0 .AND. IRET .LE. MXTRY ) THEN
        CALL SPEVNT
      ELSE
        GOTO 1500
      ENDIF
C
C IDPR FIXED AT 1 ( ALWAYS )
C
      IDPR = 1
C
C--- deal with any errors produced by LUND rather than allowing
C--- them to accumulate and thereby stopping program
C
      IF ( MSTU(24) .NE. 0 ) THEN
        ISTATS(50+MSTU(24)) = ISTATS(50+MSTU(24)) + 1
        MSTU(24) = 0
        ISTATS(50) = ISTATS(50) + 1
        IERR = IERR + 1
        IF ( IERR .EQ. 20 ) THEN
          WRITE(IW(6),611)
          MSTU(25) = 0
        ENDIF
C--- skip rest of analysis
C       --------
        GOTO 1000
C       --------
      ENDIF
C--- count warnings
      IF ( MSTU(28) .NE. 0 ) THEN
        ISTATS(60+MSTU(28)) = ISTATS(60+MSTU(28)) + 1
        ISTATS(60) = ISTATS(60) + 1
      ENDIF
  611 FORMAT(//,' +++++ASKUSE :  MORE THAN 20 LUND ERRORS',
     *' - NO MORE MESSAGES WILL BE PRINTED +++++',//)
C-------------------------------------------------------------------
C
C REJECT EVENT ON BASIS OF LUND 4 VECTORS ( THIS IS A DUMMY ROUTINE
C  IN THE STANDARD LIBRARY BUT USER MAY LINK HIS OWN
C
C-------------------------------------------------------------------
      CALL AMCSEL(KEEP)
C-------------------------------------------------------------------
C
C ( 10 PER CENT OF REJECTED EVENTS ARE KEPT WITH
C   WEIGHT SET TO 10.0 )
C
C-------------------------------------------------------------------
      IF(.NOT.KEEP)THEN
        ICREJ = ICREJ + 1
        IF(ICREJ.LT.10)GOTO 1700
        ICREJ = 0
        WEIT  = 10.0
      ENDIF
C-------------------------------------------------------------------
C
C GENERATE PRIMARY VERTEX
C
C-------------------------------------------------------------------
      CALL RANNOR (RX,RY)
      CALL RANNOR (RZ,DUM)
      VRTEX(1) = RX*SDVRT(1)
      VRTEX(2) = RY*SDVRT(2)
      VRTEX(3) = RZ*SDVRT(3)
      VRTEX(4) = 0.
C-------------------------------------------------------------------
C
C COPY EVENT FROM LUND ARRAY TO ALEPH OUTPUT BANKS
C
C-------------------------------------------------------------------
      CALL KXL7AL (VRTEX,ISTAT,NVRT,NTRK)
      IF(ISTAT.NE.0)THEN
        IF(ISTAT.GT.0)ISTAT = -6
        ISTATS(-ISTAT+70)=ISTATS(-ISTAT+70) + 1
      ELSE
C-------------------------------------------------------------------
C
C SUCCESFUL RETURN HERE
C
C-------------------------------------------------------------------
        ISTATS(42) = ISTATS(42) + 1
          call lutabu(11)
          call lutabu(21)
        RETURN
      ENDIF
C-------------------------------------------------------------------
C
C      ERROR IN LUND COMES TO HERE
C
C-------------------------------------------------------------------
 1000 CONTINUE
      ISTAT = 3
      ISTATS(30) = ISTATS(30) + 1
      RETURN
C-------------------------------------------------------------------
C
C ERROR IN SPRING - RETURNS TO HERE
C
C-------------------------------------------------------------------
1500  ISTAT = 2
      ISTATS(20) = ISTATS(20) + 1
      RETURN
C-------------------------------------------------------------------
C
C EVENT REJECTED BY AMCSEL - RETURNS HERE
C
C-------------------------------------------------------------------
1700  ISTATS(40) = ISTATS(40 )+1
      ISTAT = 5
      RETURN
      END
C*DK ASKUSI
C-------------------------------------------------------------------
      SUBROUTINE ASKUSI(IGCOD)
C------------------------------------------------------------
C  written A.J.FINCH 12/2/1993
C! Interface routine for GGMJ01 generator -Initialisation
C     output arguments :
C          IGCOD : Generator identification code
C------------------------------------------------------------
      PARAMETER(KINTYP=6008)
      PARAMETER(NVERS=100)
      INTEGER ISTATS
      COMMON/CMJIF/ISTATS(100),IVERS
c
c
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
      CHARACTER*80 FNAME
      CHARACTER*4  TYPE,CHAINT,CNAME,DMODE,FDEVI
      CHARACTER*10 ATYPE,DTYPE
      DATA DMODE / 'A   ' /
      IGCOD = KINTYP
      IVERS = NVERS
C announce myself !
      AVERS = FLOAT(IVERS)/100.0
      WRITE(IW(6),601)IGCOD,AVERS
601   FORMAT(/////,15X,'---------------------------------------'/,
     1             15X,'-                                     -'/,
     1             15X,'-             G G M J 0 1             -'/,
     1             15X,'-            ===============          -'/,
     1             15X,'-            generator code ',I5,'     -'/,
     1             15X,'-            version ',F5.2,'            -'/,
     1             15X,'-                                     -'/,
     1             15X,'-  last modification 4  August    1999-'/,
     1             15X,'---------------------------------------',
     1        /////)
C--- ZERO error flag array
      CALL VZERO(ISTATS,100)
C--- Initialise the random number generator
      ISEED= 12345
C  RNDM N_RNDM - seed the random number generator
      ISEED = 1234567
      NARND = NAMIND('RNDM')
      JRNDM = IW(NARND)
      IF(JRNDM.NE.0) THEN
       IF(IW(JRNDM).GE.1) ISEED =IW(JRNDM+1)
      ENDIF
C      WRITE(6,*)' Random number seeded with :',ISEED
C ------------------------------------------------------------------
C
C FIND MAP NAME
C
C ------------------------------------------------------------------
C CARE !!!
C - FNAME IS MACHINE DEPENDANT >>
C
      FNAME = 'GGMJ01.MAP'
C -
      ATYPE = 'NATI'
      FDEVI = 'DISK'
      CNAME = 'GMAP'
      DTYPE = 'NATIVE   '
      CALL ACDARG(CNAME,DTYPE,DMODE,FNAME,ATYPE,FDEVI,IER)
C
C ------------------------------------------------------------------
C
C GET OLD MAP OR GENERATE NEW MAP IF REQUIRED
C
C ------------------------------------------------------------------
      CALL GETMAP(FNAME,ATYPE,FDEVI,IPPART,ISEED)
C ------------------------------------------------------------------
C
C STORE RUN INFORMATION IN BANKS KPAR AND RLEP
C
C ------------------------------------------------------------------
      CALL GGMJRN
C ------------------------------------------------------------------
C
C INITIALISE ALEPH PARTICLE BANKS
C
C ------------------------------------------------------------------
      CALL KPARTI(IPPART)
C ------------------------------------------------------------------
C
C PRINT OUT THE CROSS SECTION
C
C ------------------------------------------------------------------
      CALL SPINIT
       RETURN
       END
C*DK KPARTI
      SUBROUTINE KPARTI(IPPART)
C ------------------------------------------------------------------
C! initialization routine of a  generator   GGMJ01
C ------------------------------------------------------------------
C*CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C*CC BCS
C*CA LUNDCOM
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
C*CC LUNDCOM
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
C    use the library routine KXL7PA
C      CALL KXL7PA (IPART,IKLIN)   !   Jetset 7.3
      CALL KXL74A (IPART,IKLIN)    !   Jetset 7.4
         call lutabu(10)
         call lutabu(20)
      IF (IPART.LE.0 .OR. IKLIN.LE.0) THEN
         WRITE (IW(6),'(1X,''error in PART or KLIN bank - STOP - ''
     +                 ,2I3)') IPART,IKLIN
         CALL EXIT
      ENDIF
C
C - Print PART and KLIN banks
      IF (IPPART.GT.0) CALL PRPART
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
C*DK GGMJRN
      SUBROUTINE   GGMJRN
C --------------------------------------------------------------------
C - AJF ,BBL
C!  Build the KPAR and  RLEP bank      (GGMJ01
C --------------------------------------------------------------------
C*CA MNJPRM
C-------------MNJPRM   file-----------------------------------
C***********************************************************************
C*
C*  common /MNJPRM/
C*
C*  job parameters for mini-jet event generation.
C*
C***********************************************************************
C*
      COMMON /MNJPRM/ NGNTYP, NGNPRC, NQSSRC, NUMFLV,
     >                XXLAM,  XLAM, XXLAM2, XLAM2,
     >                YMAX3, YMAX4,
     >                NDISTR,
     >                NGMINS, NGPLUS,
     >                IHIS
C*
      INTEGER*4  NGNTYP, NGNPRC
C*
C*  NGNTYP = 0  ; Generate all type.
C*         = 1  ; Generate one resolved process.
C*         = 2  ; Generate two resolved process
C*         = 3  ; Generate only one sub-process specified by NGNPRC
C*  NGNPRC = 1 to 12 ; Generate the single subprocess whose process
C*                     ID is NGNPRC
C*
      INTEGER*4  NQSSRC
C*
C*  NQSSRC = 0  ; Q^2 = \hat(S)
C*         = 1  ; Q^2 = Pt^2
C*
      INTEGER*4  NUMFLV
C*
C*  NUMFLV; Number of flavour to generate.
C*         = 0  ; Nflv= 5 when QSQ > 500, =4 when 500 > QSQ > 50
C*         = 3  ; Always NFLV = 3
C*         = 4  ; Always NFLV = 4
C*         = 5  ; Always NFLV = 5
C*
      REAL*8  XXLAM, XLAM
      REAL*8  XXLAM2, XLAM2
C*
C*  XXLAM  ; Lambda to be used to calculate alpha_S
C*  XLAM   ; Lambda to be used to calculate parton density function.
C*
      INTEGER*4  NDISTR
C*
C*    NDISTR ; Parton distribution function of photon
C*      = 0 ; DG, select automatically according to the qsquare.
C*      = 1 ; DG with Nflv=3
C*      = 2 ; DG with Nflv=4
C*      = 3 ; DG with Nflv=5
C*      = 4 ; LAC, SET-I
C*      = 5 ; LAC, SET-II
C*      = 6 ; LAC, SET-III
C*      = 7 ; DO
C*      = 8 ; DO + VMD
C*      = 9 ; Modified DO + VMD
C*
      INTEGER*4 NGMINS, NGPLUS
C
C     NGMINS  : Source of photon beam
C         0=Bremstraulung from e-, 1=Beamstrulung from e-
C     NGPLUS  : Source of photon beam
C         0=Bremstraulung from e+, 1=Beamstrulung from e+
C
      REAL*8    YMAX3, YMAX4
C  YMAX3  : Maximum rapidity to accept event for jet-3
C  YMAX4  : Maximum rapidity to accept event for jet-4
      INTEGER*4 IHIS
C  IHIS   : Flag controlling histogramming 1=YES 0=NO
C*CC MNJPRM
      PARAMETER (NCOL=15)
      INTEGER   ALTABL,ALRLEP
      EXTERNAL  ALTABL,ALRLEP
      DIMENSION TABL(NCOL)
C*CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C*CC BCS
C
C -  BASES COMMONS, FOR THE CROSS SECTION
C
      REAL*8 SI,SI2,SWGT,SCHI,SCALLS,ATACC,WGT
      COMMON / BASE3  / SI,SI2,SWGT,SCHI,SCALLS,ATACC,NSU,IT,WGT
      PARAMETER (ITM = 50)
      REAL*4   TIME,EFF,WRONG,TRSLT,TSTD,PCNT
      REAL*8   RESLT,ACSTD
      COMMON /BASE5/ ITRAT(ITM),TIME(ITM),EFF(ITM),WRONG(ITM),
     .       RESLT(ITM),ACSTD(ITM),TRSLT(ITM),TSTD(ITM),PCNT(ITM)
C
C PHYSICS PARAMETERS
C
C*CA EVTPRM
C-------------EVTPRM   file-----------------------------------
      REAL*8          RS, EMINS, EPLUS, EBEAM, XG(25), EGAM1, EGAM2
     >              , CS, EHAT, EGAM10, EGAM20, PTMIN
      INTEGER*4       NRTYPE, NRDUMY, NPRC, INDX
      COMMON /EVTPRM/ RS, EMINS, EPLUS, EBEAM, XG  , EGAM1, EGAM2
     >              , NRTYPE, NRDUMY
     >              , CS, EHAT, EGAM10, EGAM20, PTMIN
     >              , NPRC, INDX
C
C   RS     ; Center of mass  energy
C   EMINS  ; not used
C   EPLUS  ;
C   EBEAM  ; Beam energy
C   XG(0)  ; Randum variables for event generation/
C
C   CS     ; Cos(th) of the event
C   EHAT   ; CMS Energy (Hard Collision)
C
C   EGAM10 ; X1*EBEAM
C   EGAM20 ; X2*EBEAM
C   EGAM1  ; X1*X3*EBEAM
C   EGAM2  ; X2*X4*EBEAM
C   PTMIN  ; Minimum Pt of a jet.
C*CC EVTPRM
C
C VERSION NUMBER...
C
      COMMON/CMJIF/ISTATS(100),IVERS
        TABL( 1) = RESLT(IT)
        TABL( 2) = EBEAM
        TABL( 3) = PTMIN
        TABL( 4) = FLOAT(NGNTYP)
        TABL( 5) = FLOAT(NGNPRC)
        TABL( 6) = FLOAT(NQSSRC)
        TABL( 7) = FLOAT(NUMFLV)
        TABL( 8) = FLOAT(NDISTR)
        TABL( 9) = FLOAT(NGMINS)
        TABL(10) = FLOAT(NGPLUS)
        TABL(11) = XLAM
        TABL(12) = XXLAM
        TABL(13) = YMAX3
        TABL(14) = YMAX4
        TABL(15) = FLOAT(IVERS)
C  Fill the KPAR bank with the generator parameters
       NROW = 1
       JKPAR = ALTABL('KPAR',NCOL,NROW,TABL,'F','C')
       CALL PRTABL('KPAR',0)
C  Fill RLEP bank
       IEBEAM = NINT(EBEAM *1000  )
       JRLEP = ALRLEP(IEBEAM,'    ',0,0,0)
       CALL PRTABL('RLEP',0)
      RETURN
      END
C*DK USCJOB
C --------------------------------------------------------------------
      SUBROUTINE USCJOB
C --------------------------------------------------------------------
C - AJF ,BBL
C!  USER routine called at end of KINGAL job (GGMJ01)
C   to print out statistics on the generator
C   Written by A.J.Finch 12/2/93
C --------------------------------------------------------------------
C
C*CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C*CC BCS
      INTEGER ISTATS
      COMMON/CMJIF/ISTATS(100),IVERS
C
C-------------------------------------------------------------------
      WRITE(IW(6),62)
62    FORMAT('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! END OF JOB !!!!!
     1!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ')
      WRITE(IW(6),63)ISTATS(41)
63    FORMAT('   ',I10,' EVENTS WERE PASSED TO SPRING ')
      WRITE(IW(6),65)ISTATS(40)
65    FORMAT('   ',I10,' EVENTS WERE REJECTED BY AMCSEL')
      WRITE(IW(6),64)ISTATS(42)
64    FORMAT('   ',I10,' EVENTS WERE SUCCESFULLY GENERATED')
      WRITE(IW(6),602)ISTATS(30)
602   FORMAT(' TOTAL ERROR RETURNS FROM LUND :',I10,/)
      WRITE(IW(6),603)(ISTATS(I),I=50,59)
603   FORMAT(' MSTLU1(25) ERROR CODES (FATAL ERRORS) ',/,
     1'     1     2     3     4     5     6     7     8     9    10',/
     210(2X,I4),/)
      WRITE(IW(6),604)(ISTATS(I),I=60,69)
604   FORMAT(' MSTLU1(26) ERROR CODES (WARNINGS) ',/,
     1'     1     2     3     4     5     6     7     8     9    10',/
     210(2X,I4),/)
      WRITE(IW(6),605)(ISTATS(I),I=70,79)
605   FORMAT(' KXLUAL ERROR CODES   ',/,
     1'     1     2     3     4     5     6     7     8     9    10',/
     210(2X,I4),/)
      WRITE(IW(6),62)
C Create cross section bank
      call ugtsec
      call lutabu(12)
      call lutabu(22)
         
      RETURN
      END
C*DK AMCSEL
C --------------------------------------------------------------------
      SUBROUTINE AMCSEL(KEEP)
C --------------------------------------------------------------------
C
C THIS ROUTINE CAN DECIDE WHETHER TO KEEP AN EVENT
C ON THE BASIS OF THE LUND PARTICLES
C
C HOWEVER THIS IS A DUMMY VERSION - USER MUST PROVIDE HIS OWN
C
C --------------------------------------------------------------------
      LOGICAL KEEP,FIRST
      SAVE    FIRST
      DATA    FIRST/.TRUE./
      IF(FIRST)THEN
C
            WRITE(6,*)' '
            WRITE(6,*)' AMCSEL : This is a ''do nothing'' version from'
            WRITE(6,*)'            GGMJ01.HLB                         '
            WRITE(6,*)'   You may provide your own version to reject  '
            WRITE(6,*)'   events using information in the LUND commons.'
            WRITE(6,*)'   Please consult the documentation for more   '
            WRITE(6,*)'    details.'
            WRITE(6,*)' '
            FIRST=.FALSE.
      ENDIF
      KEEP = .TRUE.
      RETURN
      END
C*DK RNSET
C --------------------------------------------------------------------
      SUBROUTINE RNSET(DUMMY)
C --------------------------------------------------------------------
      CALL RDMIN (DUMMY)
      RETURN
      END
C*DK GETMAP
C --------------------------------------------------------------------
      SUBROUTINE GETMAP(FNAME,ATYPE,DEVI,IPPART,ISEED)
C --------------------------------------------------------------------
C - A. Finch - February 1993
C! Either generate a new map file ,or read existing on
C   This is the interface to the BASES program that
C    generates a 'map' which enables the second
C    stage 'SPRING' to generate events very quickly.
C    Map generation can take a long time ! In its
C    second stage BASES works out the cross section
C    for the process.
C
C     structure : subroutine
C     input arguments :
C            FNAME : Name of file to use for map
C            ATYPE : File type of map
C            DEVI  : Device type of map
C           ISEED  : SEED FOR RANDOM NUMBER GENERATOR
C
C --------------------------------------------------------------------
C*CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C*CC BCS
C
      PARAMETER(LUN=23)
      CHARACTER*80 FNAME
      CHARACTER*4  FDEVI
      CHARACTER*10 ATYPE
      REAL*8   XL,XU,ACC1,ACC2
      REAL*4   SETIM,UTIME,TIM1,TIM2,RTIM
      PARAMETER (MXDIM = 25, NDMX = 50, LENG = 17000)
      COMMON /BASE0/ SETIM,UTIME,IFLAG,NPRINT,IBASES
      COMMON /BASE1/ XL(MXDIM),XU(MXDIM),NDIM,NSNG,
     .               IG(MXDIM),NCALL
      COMMON /BASE2/ ACC1,ACC2,ITMX1,ITMX2
      INTEGER UNIX
      COMMON /BSCNTL/ UNIX,NLOOP,MLOOP
      REAL*8   FNCMNJ
      EXTERNAL FNCMNJ
C*CA MNJPRM
C-------------MNJPRM   file-----------------------------------
C***********************************************************************
C*
C*  common /MNJPRM/
C*
C*  job parameters for mini-jet event generation.
C*
C***********************************************************************
C*
      COMMON /MNJPRM/ NGNTYP, NGNPRC, NQSSRC, NUMFLV,
     >                XXLAM,  XLAM, XXLAM2, XLAM2,
     >                YMAX3, YMAX4,
     >                NDISTR,
     >                NGMINS, NGPLUS,
     >                IHIS
C*
      INTEGER*4  NGNTYP, NGNPRC
C*
C*  NGNTYP = 0  ; Generate all type.
C*         = 1  ; Generate one resolved process.
C*         = 2  ; Generate two resolved process
C*         = 3  ; Generate only one sub-process specified by NGNPRC
C*  NGNPRC = 1 to 12 ; Generate the single subprocess whose process
C*                     ID is NGNPRC
C*
      INTEGER*4  NQSSRC
C*
C*  NQSSRC = 0  ; Q^2 = \hat(S)
C*         = 1  ; Q^2 = Pt^2
C*
      INTEGER*4  NUMFLV
C*
C*  NUMFLV; Number of flavour to generate.
C*         = 0  ; Nflv= 5 when QSQ > 500, =4 when 500 > QSQ > 50
C*         = 3  ; Always NFLV = 3
C*         = 4  ; Always NFLV = 4
C*         = 5  ; Always NFLV = 5
C*
      REAL*8  XXLAM, XLAM
      REAL*8  XXLAM2, XLAM2
C*
C*  XXLAM  ; Lambda to be used to calculate alpha_S
C*  XLAM   ; Lambda to be used to calculate parton density function.
C*
      INTEGER*4  NDISTR
C*
C*    NDISTR ; Parton distribution function of photon
C*      = 0 ; DG, select automatically according to the qsquare.
C*      = 1 ; DG with Nflv=3
C*      = 2 ; DG with Nflv=4
C*      = 3 ; DG with Nflv=5
C*      = 4 ; LAC, SET-I
C*      = 5 ; LAC, SET-II
C*      = 6 ; LAC, SET-III
C*      = 7 ; DO
C*      = 8 ; DO + VMD
C*      = 9 ; Modified DO + VMD
C*
      INTEGER*4 NGMINS, NGPLUS
C
C     NGMINS  : Source of photon beam
C         0=Bremstraulung from e-, 1=Beamstrulung from e-
C     NGPLUS  : Source of photon beam
C         0=Bremstraulung from e+, 1=Beamstrulung from e+
C
      REAL*8    YMAX3, YMAX4
C  YMAX3  : Maximum rapidity to accept event for jet-3
C  YMAX4  : Maximum rapidity to accept event for jet-4
      INTEGER*4 IHIS
C  IHIS   : Flag controlling histogramming 1=YES 0=NO
C*CC MNJPRM
C*CA MNJPID
C-------------MNJPID   file-----------------------------------
      INTEGER*4  NTPJLC
      COMMON / MNJPID / NTPJLC
C
C ... NTPJLC ... Particle ID code.
C     = 0 for TOPAZ ID
C     = 1 for JLC ID
C
C*CC MNJPID
C*CA EVTPRM
C-------------EVTPRM   file-----------------------------------
      REAL*8          RS, EMINS, EPLUS, EBEAM, XG(25), EGAM1, EGAM2
     >              , CS, EHAT, EGAM10, EGAM20, PTMIN
      INTEGER*4       NRTYPE, NRDUMY, NPRC, INDX
      COMMON /EVTPRM/ RS, EMINS, EPLUS, EBEAM, XG  , EGAM1, EGAM2
     >              , NRTYPE, NRDUMY
     >              , CS, EHAT, EGAM10, EGAM20, PTMIN
     >              , NPRC, INDX
C
C   RS     ; Center of mass  energy
C   EMINS  ; not used
C   EPLUS  ;
C   EBEAM  ; Beam energy
C   XG(0)  ; Randum variables for event generation/
C
C   CS     ; Cos(th) of the event
C   EHAT   ; CMS Energy (Hard Collision)
C
C   EGAM10 ; X1*EBEAM
C   EGAM20 ; X2*EBEAM
C   EGAM1  ; X1*X3*EBEAM
C   EGAM2  ; X2*X4*EBEAM
C   PTMIN  ; Minimum Pt of a jet.
C*CC EVTPRM
C --------------------------------------------------------------------
      UNIX   = 0
      INLUN  = 0
      IHIS   = 1
      IPPART = 0
C --------------------------------------------------------------------
C
C LOOK AT THE GCON CONTROL CARD
C
C --------------------------------------------------------------------
      IGCON = IW(NAMIND('GCON'))
      IF(IGCON.GT.0)THEN
       LEN = IW(IGCON)
C --------------------------------------------------------------------
C
C EXTRACT ALL SET VALUES FROM GCON
C
C   INLUN : CONTROLS WHETHER MAP IS TO BE READ IN OR WRITTEN OUT
C   IHIS  : CONTROLS WHETHER HISTOGRAMS ARE PRODUCED
C
C --------------------------------------------------------------------
       IF(LEN.GE.1)INLUN = IW(IGCON+1)
       IF(LEN.GE.2)IHIS  = IW(IGCON+2)
       IF(LEN.GE.3)IPPART  = IW(IGCON+3)
      ENDIF
C --------------------------------------------------------------------
C
C IF NEW MAP GENERATION IS REQUIRED THEN DO IT
C
C --------------------------------------------------------------------
      IF(INLUN.EQ.0)THEN
      CALL DRNSET(ISEED)
C --------------------------------------------------------------------
C
C EXTRACT PARAMETERS CONTROLLING PHYSICS FROM GGMJ CARD
C
C --------------------------------------------------------------------
      EBEAM  = 46.1
      PTMIN  = 2.5
      NGNTYP = 0
      NGNPRC = 0
      NQSSRC = 1
      NUMFLV = 0
      NDISTR = 0
      NGMINS = 0
      NGPLS  = 0
      XLAM   = 0.4
      XXLAM  = 0.4
      YMAX3  = 200.0
      YMAX4  = 200.0
      NTPJLC = 0
C --------------------------------------------------------------------
C
C PHYSICS PARAMETERS MEANING, AND THEIR DEFAULTS:
C46.1    EBEAM : Beam energy
C 2.5    PtMIN : Minimum Pt for the 'High Pt' jets
C  0     NGNTYP:Gen. Type(0=All,1=1Resolved,2=2resolved, 3=1sub. Proc.
C  0     NGNPRC:Subprocess type ( Valid when Gen. Type = 3)
C  1     NQSSRC:How to calculate Q^2(0=Shat, 1=Pt^2)
C  0     NUMFLV:# of flavour(0=Acoording to Q^2, 3=dus, 4=dusc,5=duscb)
C  0     NDISTR:Type of Parton distribution function.
C  0     NGMINS
C  0     NGPLS
C  0.4   XLAM:Lambda to calculate Parton distribution function.
C  0.4   XXLAM:Lambda to calculate Alpha_S
C200.0   YMAX3
C200.0   YMAX4
C ( NTPJLC IS NOT RELEVANT TO ALEPH )
C --------------------------------------------------------------------
        IGGPH = IW(NAMIND('GGPH'))
        IF(IGGPH.GT.0)THEN
           LEN = IW(IGGPH)
           IF(LEN.GE.1)EBEAM = RW(IGGPH+1)
           IF(LEN.GE.2)PTMIN = RW(IGGPH+2)
           IF(LEN.GE.3)NGNTYP= IW(IGGPH+3)
           IF(LEN.GE.4)NGNPRL= IW(IGGPH+4)
           IF(LEN.GE.5)NQSSRC= IW(IGGPH+5)
           IF(LEN.GE.6)NUMFLV= IW(IGGPH+6)
           IF(LEN.GE.7)NDISTR= IW(IGGPH+7)
           IF(LEN.GE.8)NGMINS= IW(IGGPH+8)
           IF(LEN.GE.9)NGPLUS= IW(IGGPH+9)
           IF(LEN.GE.10)XXLAM= RW(IGGPH+10)
           IF(LEN.GE.11)XLAM = RW(IGGPH+11)
           IF(LEN.GE.12)YMAX3= RW(IGGPH+12)
           IF(LEN.GE.13)YMAX4= RW(IGGPH+13)
        ENDIF
C --------------------------------------------------------------------
C
C EXTRACT PARAMETERS CONTROLLING BASES INTEGRATION FROM GBAS CARD
C
C    NCALL : CONTROLS HOW MANY TIMES BASES LOOPS ROUND IN ONE ITERATION
C    ITMX1 : CONTROLS NUMBER OF ITERATIONS FOR 'GRID DEFINING' STEP
C    ITMX2 : CONTROLS NUMBER OF ITERATIONS FOR  CROSS SECTION CALC. STEP
C    ACC1  : ACCURACY REQUIRED IN STEP 1 ( STOPS IF ACHIEVED )
C    ACC2  : ACCURACY REQUIRED IN STEP 2 ( STOPS IF ACHIEVED )
C --------------------------------------------------------------------
         NCALL = 10000
         ITMX1 = 25
         ITMX2 = 25
         ACC1 = 0.2
         ACC2 = 0.2
        IGBAS = IW(NAMIND('GBAS'))
        IF(IGBAS.GT.0)THEN
           LEN = IW(IGBAS)
           IF(LEN.GE.1)NCALL= IW(IGBAS+1)
           IF(LEN.GE.2)ITMX1= IW(IGBAS+2)
           IF(LEN.GE.3)ITMX2= IW(IGBAS+3)
           IF(LEN.GE.4)THEN
              TEMP=  RW(IGBAS+4)
              ACC1 = 0.0
              ACC1 = TEMP
           ENDIF
           IF(LEN.GE.5)THEN
              TEMP = RW(IGBAS+5)
              ACC2 = 0.0
              ACC2 = TEMP
           ENDIF
        ENDIF
C --------------------------------------------------------------------
C  SOME STANDARD PARAMETERS FOR BASES...
C    1,  1  Current loop count,   Max. Loop count
C   -4      Print Flag
C    0      Input Flag
C 1440.     CPU time limit in minutes
C --------------------------------------------------------------------
        NLOOP = 1
        MLOOP = 1
        NPRINT = -4
        IFLAG = 0
        SETIM = 1440
C --------------------------------------------------------------------
C
C OPEN THE MAP FILE FOR OUTPUT
C
C --------------------------------------------------------------------
         CALL AOPENW(LUN,FNAME,ATYPE,FDEVI,IER)
         IF(IER.NE.0)THEN
               WRITE(6,*)' ERROR ',IER,' FROM AOPENW CALLED BY  GETMAP '
     1         ,' WHEN TRYING TO OPEN MAP FILE :',FNAME,
     1          ' FOR WRITING OUT NEW MAP '
               STOP
         ENDIF
C         CALL USERIN
         CALL BSMAIN(FNCMNJ)
      ELSE
C --------------------------------------------------------------------
C
C IF GCON CARD REQUESTED MAP INPUT, OPEN THE MAP FILE FOR INPUT
C AND THEN BSREAD WILL READ IN MAP , AND PHYSICS PARAMETERS
C  (GGPH CARD IGNORED )
C
C --------------------------------------------------------------------
         CALL AOPEN(LUN,FNAME,ATYPE,FDEVI,IER)
         IF(IER.NE.0)THEN
           WRITE(6,*)' ERROR ',IER,' FROM AOPEN CALLED BY  GETMAP '
           IF(IER.EQ.-1)WRITE(6,*)
     1     'THE FILE :',FNAME,' DOES NOT EXIST '
           STOP
         ENDIF
C --------------------------------------------------------------------
C  READ IN THE MAP
C --------------------------------------------------------------------
         CALL BSREAD
C SET RANDOM NUMBER HERE, OTHERWISE IT IS OVERWRITTEN BY READING IN MAP
         CALL DRNSET(ISEED)
      ENDIF
C --------------------------------------------------------------------
C  FINALLY CALL USERIN ( JUST PRINTS OUT INFO NOW )
C --------------------------------------------------------------------
      CALL USERIN
      RETURN
      END
C
C*DK SPEVNT
C --------------------------------------------------------------------
      SUBROUTINE SPEVNT( NRET )
C --------------------------------------------------------------------
C Author A. Miyamoto 28-Aug-1990
C Modified for ALEPH by A.Finch 1993
C! Control hadronisation step of GGMJ01
C
C CALLS MNJMPL which takes information from FNCMNJ to generate partons
C       MNJHAD which puts partons in LUND common then calls LUEXEC
C
C --------------------------------------------------------------------
C
C
C*CA EVTPRM
C-------------EVTPRM   file-----------------------------------
      REAL*8          RS, EMINS, EPLUS, EBEAM, XG(25), EGAM1, EGAM2
     >              , CS, EHAT, EGAM10, EGAM20, PTMIN
      INTEGER*4       NRTYPE, NRDUMY, NPRC, INDX
      COMMON /EVTPRM/ RS, EMINS, EPLUS, EBEAM, XG  , EGAM1, EGAM2
     >              , NRTYPE, NRDUMY
     >              , CS, EHAT, EGAM10, EGAM20, PTMIN
     >              , NPRC, INDX
C
C   RS     ; Center of mass  energy
C   EMINS  ; not used
C   EPLUS  ;
C   EBEAM  ; Beam energy
C   XG(0)  ; Randum variables for event generation/
C
C   CS     ; Cos(th) of the event
C   EHAT   ; CMS Energy (Hard Collision)
C
C   EGAM10 ; X1*EBEAM
C   EGAM20 ; X2*EBEAM
C   EGAM1  ; X1*X3*EBEAM
C   EGAM2  ; X2*X4*EBEAM
C   PTMIN  ; Minimum Pt of a jet.
C*CC EVTPRM
C*CA MNJTYP
C-------------MNJTYP   file-----------------------------------
C MNJTYP file.
C ... sub-process information data.
C
C     NCDATA(0,,) = Cross section formula for the sub-process.
C           (1,,) = Parton ID of initial-1
C           (2,,) =              initial-2
C           (3,,) = Parton ID of final-1
C           (4,,) =              final-2
C
      PARAMETER (MX$PRC=44)
      PARAMETER (MAXPRC=MX$PRC*5)
      INTEGER*4  NCDATA(0:4,5,MX$PRC)
      INTEGER*4  KCDATA(0:4,MAXPRC)
      EQUIVALENCE (KCDATA(0,1), NCDATA(0,1,1))
C
CAJF - I SET NCDATA(0,1:5,1) TO ZERO TOTURN OFF THE QPM COMPONENT
C 1   gamma + gamma --> q + qbar
      DATA (((NCDATA(I,J,K),I=0,4),J=1,5),K=1,7)/
     > 0,22,22,1,-1,   0,22,22,2,-2,  0,22,22,3,-3,
     > 0,22,22,4,-4,   0,22,22,5,-5,
C
C 2a  gamma +q ---> gluon + q
     > 2,22,1,21,1,    2,22,2,21,2,   2,22,3,21,3,
     > 2,22,4,21,4,    2,22,5,21,5,
C
C 2b  gamma + qbar ---> gluon + qbar
     > 2,22,-1,21,-1,  2,22,-2,21,-2,  2,22,-3,21,-3,
     > 2,22,-4,21,-4,  2,22,-5,21,-5,
C
C 2c  q + gamma --> q + gluon
     >-2,1,22,1,21,    -2,2,22,2,21,   -2,3,22,3,21,
     >-2,4,22,4,21,    -2,5,22,5,21,
C
C 2d  qbar + gamma --> qbar + gluon
     >-2,-1,22,-1,21,  -2,-2,22,-2,21,   -2,-3,22,-3,21,
     >-2,-4,22,-4,21,  -2,-5,22,-5,21,
C
C 3a  gamma + gluon --> q + qbar
     > 3,22,21,1,-1,    3,22,21,2,-2,    3,22,21,3,-3,
     > 3,22,21,4,-4,    3,22,21,5,-5,
C
C 3b  gluon + gamma --> q + qbar
     >-3,21,22,1,-1,   -3,21,22,2,-2,    -3,21,22,3,-3 ,
     >-3,21,22,4,-4,   -3,21,22,5,-5          /
C 5/18/92 : costh distribution should be symmetry for replacement of
C            q_i and q_j.
C 4/5 q_i + q_j --> q_i + q_j
C     DATA (((NCDATA(I,J,K),I=0,4),J=1,5),K=8,12)/
C    >4,1,1,1,1,   5,1,2,1,2,   5,1,3,1,3,  5,1,4,1,4,  5,1,5,1,5,
C    >5,2,1,2,1,   4,2,2,2,2,   5,2,3,2,3,  5,2,4,2,4,  5,2,5,2,5,
C    >5,3,1,3,1,   5,3,2,3,2,   4,3,3,3,3,  5,3,4,3,4,  5,3,5,3,5,
C    >5,4,1,4,1,   5,4,2,4,2,   5,4,3,4,3,  4,4,4,4,4,  5,4,5,4,5,
C    >5,5,1,5,1,   5,5,2,5,2,   5,5,3,5,3,  5,5,4,5,4,  4,5,5,5,5/
      DATA (((NCDATA(I,J,K),I=0,4),J=1,5),K=8,12)/
     > 4,1,1,1,1,  5,1,2,1,2,   5,1,3,1,3,  5,1,4,1,4,  5,1,5,1,5,
     >-5,2,1,2,1,  4,2,2,2,2,   5,2,3,2,3,  5,2,4,2,4,  5,2,5,2,5,
     >-5,3,1,3,1, -5,3,2,3,2,   4,3,3,3,3,  5,3,4,3,4,  5,3,5,3,5,
     >-5,4,1,4,1, -5,4,2,4,2,  -5,4,3,4,3,  4,4,4,4,4,  5,4,5,4,5,
     >-5,5,1,5,1, -5,5,2,5,2,  -5,5,3,5,3, -5,5,4,5,4,  4,5,5,5,5/
C
C 6/8 q_i + bar(q_j) --> q_i + bar(q_j)
      DATA (((NCDATA(I,J,K),I=0,4),J=1,5),K=13,17)/
     >6,1,-1,1,-1, 8,1,-2,1,-2, 8,1,-3,1,-3, 8,1,-4,1,-4, 8,1,-5,1,-5,
     >8,2,-1,2,-1, 6,2,-2,2,-2, 8,2,-3,2,-3, 8,2,-4,2,-4, 8,2,-5,2,-5,
     >8,3,-1,3,-1, 8,3,-2,3,-2, 6,3,-3,3,-3, 8,3,-4,3,-4, 8,3,-5,3,-5,
     >8,4,-1,4,-1, 8,4,-2,4,-2, 8,4,-3,4,-3, 6,4,-4,4,-4, 8,4,-5,4,-5,
     >8,5,-1,5,-1, 8,5,-2,5,-2, 8,5,-3,5,-3, 8,5,-4,5,-4, 6,5,-5,5,-5/
C 6/8 bar(q_i) + q_j --> bar(q_i) + q_j
      DATA (((NCDATA(I,J,K),I=0,4),J=1,5),K=18,22)/
     >-6,-1,1,-1,1,-8,-1,2,-1,2,-8,-1,3,-1,3,-8,-1,4,-1,4,-8,-1,5,-1,5,
     >-8,-2,1,-2,1,-6,-2,2,-2,2,-8,-2,3,-2,3,-8,-2,4,-2,4,-8,-2,5,-2,5,
     >-8,-3,1,-3,1,-8,-3,2,-3,2,-6,-3,3,-3,3,-8,-3,4,-3,4,-8,-3,5,-3,5,
     >-8,-4,1,-4,1,-8,-4,2,-4,2,-8,-4,3,-4,3,-6,-4,4,-4,4,-8,-4,5,-4,5,
     >-8,-5,1,-5,1,-8,-5,2,-5,2,-8,-5,3,-5,3,-8,-5,4,-5,4,-6,-5,5,-5,5/
C 5/18/92 : costh distribution should be symmetry for replacement of
C            q_i and q_j.
C 4/5 bar(q_i) + bar(q_j) --> bar(q_i) + bar(q_j)
C     DATA (((NCDATA(I,J,K),I=0,4),J=1,5),K=23,27)/
C    >4,-1,-1,-1,-1,   5,-1,-2,-1,-2,   5,-1,-3,-1,-3,
C    >                 5,-1,-4,-1,-4,   5,-1,-5,-1,-5,
C    >5,-2,-1,-2,-1,   4,-2,-2,-2,-2,   5,-2,-3,-2,-3,
C    >                 5,-2,-4,-2,-4,   5,-2,-5,-2,-5,
C    >5,-3,-1,-3,-1,   5,-3,-2,-3,-2,   4,-3,-3,-3,-3,
C    >                 5,-3,-4,-3,-4,   5,-3,-5,-3,-5,
C    >5,-4,-1,-4,-1,   5,-4,-2,-4,-2,   5,-4,-3,-4,-3,
C    >                 4,-4,-4,-4,-4,   5,-4,-5,-4,-5,
C    >5,-5,-1,-5,-1,   5,-5,-2,-5,-2,   5,-5,-3,-5,-3,
C    >                 5,-5,-4,-5,-4,   4,-5,-5,-5,-5/
      DATA (((NCDATA(I,J,K),I=0,4),J=1,5),K=23,27)/
     > 4,-1,-1,-1,-1,   5,-1,-2,-1,-2,   5,-1,-3,-1,-3,
     >                  5,-1,-4,-1,-4,   5,-1,-5,-1,-5,
     >-5,-2,-1,-2,-1,   4,-2,-2,-2,-2,   5,-2,-3,-2,-3,
     >                  5,-2,-4,-2,-4,   5,-2,-5,-2,-5,
     >-5,-3,-1,-3,-1,  -5,-3,-2,-3,-2,   4,-3,-3,-3,-3,
     >                  5,-3,-4,-3,-4,   5,-3,-5,-3,-5,
     >-5,-4,-1,-4,-1,  -5,-4,-2,-4,-2,  -5,-4,-3,-4,-3,
     >                  4,-4,-4,-4,-4,   5,-4,-5,-4,-5,
     >-5,-5,-1,-5,-1,  -5,-5,-2,-5,-2,  -5,-5,-3,-5,-3,
     >                 -5,-5,-4,-5,-4,   4,-5,-5,-5,-5/
C ... first line is gluon + gluon --> gluon + gluon
C
C 7a  q_i + bar(q_i) --> q_j + bar(q_j)
      DATA (((NCDATA(I,J,K),I=0,4),J=1,5),K=28,32)/
     >12,21,21,21,21, 7,1,-1,2,-2,  7,1,-1,3,-3,
     >                7,1,-1,4,-4,  7,1,-1,5,-5,
     > 7,2,-2,1,-1,   0,0, 0,0, 0,  7,2,-2,3,-3,
     >                7,2,-2,4,-4,  7,2,-2,5,-5,
     > 7,3,-3,1,-1,   7,3,-3,2,-2,  0,0, 0,0, 0,
     >                7,3,-3,4,-4,  7,3,-3,5,-5,
     > 7,4,-4,1,-1,   7,4,-4,2,-2,  7,4,-4,3,-3,
     >                0,0, 0,0, 0,  7,4,-4,5,-4,
     > 7,5,-5,1,-1,   7,5,-5,2,-2,  7,5,-5,3,-3,
     >                7,5,-5,4,-4,  0,0, 0,0, 0 /
C 7b  bar(q_i) + q_i --> bar(q_j) + q_j
      DATA (((NCDATA(I,J,K),I=0,4),J=1,5),K=33,37)/
     > 0,0, 0,0, 0,  -7,-1,1,-2,2,  -7,-1,1,-3,3,
     >               -7,-1,1,-4,4,  -7,-1,1,-5,5,
     >-7,-2,2,-1,1,   0,0, 0,0, 0,  -7,-2,2,-3,3,
     >               -7,-2,2,-4,4,  -7,-2,2,-5,5,
     >-7,-3,3,-1,1,  -7,-3,3,-2,2,   0, 0,0, 0,0,
     >               -7,-3,3,-4,4,  -7,-3,3,-5,5,
     >-7,-4,4,-1,1,  -7,-4,4,-2,2,  -7,-4,4,-3,3,
     >                0,0, 0,0, 0,  -7,-4,4,-5,4,
     >-7,-5,5,-1,1,  -7,-5,5,-2,2,  -7,-5,5,-3,3,
     >               -7,-5,5,-4,4,   0, 0,0, 0,0 /
      DATA (((NCDATA(I,J,K),I=0,4),J=1,5),K=38,43)/
C 9a  q_i + bar(q_i) --> gluon + gluon
     > 9,1,-1,21,21,  9,2,-2,21,21, 9,3,-3,21,21,
     >                9,4,-4,21,21, 9,5,-5,21,21,
C 9b  bar(q_i) + q_i --> gluon + gluon
     >-9,-1,1,21,21,  -9,-2,2,21,21, -9,-3,3,21,21,
     >                -9,-4,4,21,21, -9,-5,5,21,21,
C 10a q_i + gluon ---> q_i + gluon
     > 10,1,21,1,21,  10,2,21,2,21, 10,3,21,3,21,
     >                10,4,21,4,21, 10,5,21,5,21,
C 10b bar(q_i) + gluon ---> bar(q_i) + gluon
     > 10,-1,21,-1,21,  10,-2,21,-2,21, 10,-3,21,-3,21,
     >                  10,-4,21,-4,21, 10,-5,21,-5,21,
C 10c gluon + q_i ---> gluon + q_i
     >-10,21,1,21,1,  -10,21,2,21,2, -10,21,3,21,3,
     >                -10,21,4,21,4, -10,21,5,21,5,
C 10d gluon + bar(q_i) ---> bar(q_i) + gluon
     >-10,21,-1,21,-1,  -10,21,-2,21,-2, -10,21,-3,21,-3,
     >                  -10,21,-4,21,-4, -10,21,-5,21,-5/
      DATA ((NCDATA(I,J,44),I=0,4),J=1,5)/
C 11  gluon + gluon ---> q + qbar
     > 11,21,21,1,-1,  11,21,21,2,-2,  11,21,21,3,-3,
     >                 11,21,21,4,-4,  11,21,21,5,-5 /
C*CC MNJTYP
C
      REAL*4  PVCT(4,20)
      PARAMETER (MX$PRT=50)
      REAL*4  RBUF(20,MX$PRT),FRAME(3,3), GBUF(20)
      INTEGER*4 IBUF(20),ITMP(20)
      REAL*4  EINI(3,3)/1.,0.,0.,  0.,1.,0.,  0.,0.,1./
C
      DATA  NEVENT/0/
C      DATA  ISEED/31293821/
C
C =====< Entry Point >=================================================
C
C --------------------------------------------------------------------
C (1) prepare 4 momentum of decay particle.
C --------------------------------------------------------------------
C
      CALL MNJMPL(  NFINAL, RBUF, GBUF, PVCT )
      IF( NFINAL .LE. 0 ) RETURN
C --------------------------------------------------------------------
C
C Hadronize parton.
C
C --------------------------------------------------------------------
      CALL MNJHAD( NFINAL, RBUF )
      RETURN
      END
C*DK MNJMPL
      SUBROUTINE MNJMPL(  NFINAL, RBUF, GBUF, PVCT)
C --------------------------------------------------------------------
C Author A. Miyamoto 28-Aug-1990
C Modified for ALEPH by A.Finch 1993
C!   Make Parton_List from generated 4 momentum information ( GGMJ01)
C
C (Output)
C   NFINAL    :  Number of final partons, including spectators.
C              = 0 if event is not created by some reason.
C   PARTON(20,4) : Particle_List of final particles.
C                  to be stored in a bank Spring:Parton_List
C                  (1) = Particle serial number.
C                  (2) = Particle ID
C                  (3) = Mass (GeV)
C                  (4) = Charge
C                  (5) = Px(GeV)
C                  (6) = Py(GeV)
C                  (7) = Pz(GeV)
C                  (8) = E(GeV)
C                  (9) =  index of hard scatering particle.
C                 (10) =    not used
C                 (11) =    not used
C                 (12) = # daughter parton
C                 (13) = particle serial # of the 1st daughter
C                 (14) = particle serial # of the parent daughter
C                 (15) =    not used
C                 (16) =    not used
C                 (17) = helicity
C                 (18) = colour single group ID
C                 (19) =    not used
C                 (20) =  10000*KS + KH
C   GBUF(20)   : Generated event information.
C                 ( 1) = Energy of resolved parton from e- side.
C                 ( 2) = Energy of resolved parton from e+ side.
C                 ( 3) = Energy of photon from e-
C                 ( 4) = Energy of photon from e+
C                 ( 5) = sqrt(Shat)
C                 ( 6) = Minijet Pt.
C                 ( 7) = Cos(Theta)^*
C                 ( 8) = Phi^*
C                 ( 9) = W_{gamma-gamma}
C                 (10) = Rapidity of parton -1
C                 (11) = Rapidity of another parton.
C   PVCT(4,3) :  4 momentum of
C                1=Gamma-Gamma system
C                2=Hard scattered q or gluon.
C                3=Hard scattered q-bar or gluon.
C
C **********************************************************************
C
C
      REAL*4  PVCT(4,3), RBUF(20,4), GBUF(20)
C
C*CA EVTPRM
C-------------EVTPRM   file-----------------------------------
      REAL*8          RS, EMINS, EPLUS, EBEAM, XG(25), EGAM1, EGAM2
     >              , CS, EHAT, EGAM10, EGAM20, PTMIN
      INTEGER*4       NRTYPE, NRDUMY, NPRC, INDX
      COMMON /EVTPRM/ RS, EMINS, EPLUS, EBEAM, XG  , EGAM1, EGAM2
     >              , NRTYPE, NRDUMY
     >              , CS, EHAT, EGAM10, EGAM20, PTMIN
     >              , NPRC, INDX
C
C   RS     ; Center of mass  energy
C   EMINS  ; not used
C   EPLUS  ;
C   EBEAM  ; Beam energy
C   XG(0)  ; Randum variables for event generation/
C
C   CS     ; Cos(th) of the event
C   EHAT   ; CMS Energy (Hard Collision)
C
C   EGAM10 ; X1*EBEAM
C   EGAM20 ; X2*EBEAM
C   EGAM1  ; X1*X3*EBEAM
C   EGAM2  ; X2*X4*EBEAM
C   PTMIN  ; Minimum Pt of a jet.
C*CC EVTPRM
C*CA MNJTYP
C-------------MNJTYP   file-----------------------------------
C MNJTYP file.
C ... sub-process information data.
C
C     NCDATA(0,,) = Cross section formula for the sub-process.
C           (1,,) = Parton ID of initial-1
C           (2,,) =              initial-2
C           (3,,) = Parton ID of final-1
C           (4,,) =              final-2
C
      PARAMETER (MX$PRC=44)
      PARAMETER (MAXPRC=MX$PRC*5)
      INTEGER*4  NCDATA(0:4,5,MX$PRC)
      INTEGER*4  KCDATA(0:4,MAXPRC)
      EQUIVALENCE (KCDATA(0,1), NCDATA(0,1,1))
C
CAJF - I SET NCDATA(0,1:5,1) TO ZERO TOTURN OFF THE QPM COMPONENT
C 1   gamma + gamma --> q + qbar
      DATA (((NCDATA(I,J,K),I=0,4),J=1,5),K=1,7)/
     > 0,22,22,1,-1,   0,22,22,2,-2,  0,22,22,3,-3,
     > 0,22,22,4,-4,   0,22,22,5,-5,
C
C 2a  gamma +q ---> gluon + q
     > 2,22,1,21,1,    2,22,2,21,2,   2,22,3,21,3,
     > 2,22,4,21,4,    2,22,5,21,5,
C
C 2b  gamma + qbar ---> gluon + qbar
     > 2,22,-1,21,-1,  2,22,-2,21,-2,  2,22,-3,21,-3,
     > 2,22,-4,21,-4,  2,22,-5,21,-5,
C
C 2c  q + gamma --> q + gluon
     >-2,1,22,1,21,    -2,2,22,2,21,   -2,3,22,3,21,
     >-2,4,22,4,21,    -2,5,22,5,21,
C
C 2d  qbar + gamma --> qbar + gluon
     >-2,-1,22,-1,21,  -2,-2,22,-2,21,   -2,-3,22,-3,21,
     >-2,-4,22,-4,21,  -2,-5,22,-5,21,
C
C 3a  gamma + gluon --> q + qbar
     > 3,22,21,1,-1,    3,22,21,2,-2,    3,22,21,3,-3,
     > 3,22,21,4,-4,    3,22,21,5,-5,
C
C 3b  gluon + gamma --> q + qbar
     >-3,21,22,1,-1,   -3,21,22,2,-2,    -3,21,22,3,-3 ,
     >-3,21,22,4,-4,   -3,21,22,5,-5          /
C 5/18/92 : costh distribution should be symmetry for replacement of
C            q_i and q_j.
C 4/5 q_i + q_j --> q_i + q_j
C     DATA (((NCDATA(I,J,K),I=0,4),J=1,5),K=8,12)/
C    >4,1,1,1,1,   5,1,2,1,2,   5,1,3,1,3,  5,1,4,1,4,  5,1,5,1,5,
C    >5,2,1,2,1,   4,2,2,2,2,   5,2,3,2,3,  5,2,4,2,4,  5,2,5,2,5,
C    >5,3,1,3,1,   5,3,2,3,2,   4,3,3,3,3,  5,3,4,3,4,  5,3,5,3,5,
C    >5,4,1,4,1,   5,4,2,4,2,   5,4,3,4,3,  4,4,4,4,4,  5,4,5,4,5,
C    >5,5,1,5,1,   5,5,2,5,2,   5,5,3,5,3,  5,5,4,5,4,  4,5,5,5,5/
      DATA (((NCDATA(I,J,K),I=0,4),J=1,5),K=8,12)/
     > 4,1,1,1,1,  5,1,2,1,2,   5,1,3,1,3,  5,1,4,1,4,  5,1,5,1,5,
     >-5,2,1,2,1,  4,2,2,2,2,   5,2,3,2,3,  5,2,4,2,4,  5,2,5,2,5,
     >-5,3,1,3,1, -5,3,2,3,2,   4,3,3,3,3,  5,3,4,3,4,  5,3,5,3,5,
     >-5,4,1,4,1, -5,4,2,4,2,  -5,4,3,4,3,  4,4,4,4,4,  5,4,5,4,5,
     >-5,5,1,5,1, -5,5,2,5,2,  -5,5,3,5,3, -5,5,4,5,4,  4,5,5,5,5/
C
C 6/8 q_i + bar(q_j) --> q_i + bar(q_j)
      DATA (((NCDATA(I,J,K),I=0,4),J=1,5),K=13,17)/
     >6,1,-1,1,-1, 8,1,-2,1,-2, 8,1,-3,1,-3, 8,1,-4,1,-4, 8,1,-5,1,-5,
     >8,2,-1,2,-1, 6,2,-2,2,-2, 8,2,-3,2,-3, 8,2,-4,2,-4, 8,2,-5,2,-5,
     >8,3,-1,3,-1, 8,3,-2,3,-2, 6,3,-3,3,-3, 8,3,-4,3,-4, 8,3,-5,3,-5,
     >8,4,-1,4,-1, 8,4,-2,4,-2, 8,4,-3,4,-3, 6,4,-4,4,-4, 8,4,-5,4,-5,
     >8,5,-1,5,-1, 8,5,-2,5,-2, 8,5,-3,5,-3, 8,5,-4,5,-4, 6,5,-5,5,-5/
C 6/8 bar(q_i) + q_j --> bar(q_i) + q_j
      DATA (((NCDATA(I,J,K),I=0,4),J=1,5),K=18,22)/
     >-6,-1,1,-1,1,-8,-1,2,-1,2,-8,-1,3,-1,3,-8,-1,4,-1,4,-8,-1,5,-1,5,
     >-8,-2,1,-2,1,-6,-2,2,-2,2,-8,-2,3,-2,3,-8,-2,4,-2,4,-8,-2,5,-2,5,
     >-8,-3,1,-3,1,-8,-3,2,-3,2,-6,-3,3,-3,3,-8,-3,4,-3,4,-8,-3,5,-3,5,
     >-8,-4,1,-4,1,-8,-4,2,-4,2,-8,-4,3,-4,3,-6,-4,4,-4,4,-8,-4,5,-4,5,
     >-8,-5,1,-5,1,-8,-5,2,-5,2,-8,-5,3,-5,3,-8,-5,4,-5,4,-6,-5,5,-5,5/
C 5/18/92 : costh distribution should be symmetry for replacement of
C            q_i and q_j.
C 4/5 bar(q_i) + bar(q_j) --> bar(q_i) + bar(q_j)
C     DATA (((NCDATA(I,J,K),I=0,4),J=1,5),K=23,27)/
C    >4,-1,-1,-1,-1,   5,-1,-2,-1,-2,   5,-1,-3,-1,-3,
C    >                 5,-1,-4,-1,-4,   5,-1,-5,-1,-5,
C    >5,-2,-1,-2,-1,   4,-2,-2,-2,-2,   5,-2,-3,-2,-3,
C    >                 5,-2,-4,-2,-4,   5,-2,-5,-2,-5,
C    >5,-3,-1,-3,-1,   5,-3,-2,-3,-2,   4,-3,-3,-3,-3,
C    >                 5,-3,-4,-3,-4,   5,-3,-5,-3,-5,
C    >5,-4,-1,-4,-1,   5,-4,-2,-4,-2,   5,-4,-3,-4,-3,
C    >                 4,-4,-4,-4,-4,   5,-4,-5,-4,-5,
C    >5,-5,-1,-5,-1,   5,-5,-2,-5,-2,   5,-5,-3,-5,-3,
C    >                 5,-5,-4,-5,-4,   4,-5,-5,-5,-5/
      DATA (((NCDATA(I,J,K),I=0,4),J=1,5),K=23,27)/
     > 4,-1,-1,-1,-1,   5,-1,-2,-1,-2,   5,-1,-3,-1,-3,
     >                  5,-1,-4,-1,-4,   5,-1,-5,-1,-5,
     >-5,-2,-1,-2,-1,   4,-2,-2,-2,-2,   5,-2,-3,-2,-3,
     >                  5,-2,-4,-2,-4,   5,-2,-5,-2,-5,
     >-5,-3,-1,-3,-1,  -5,-3,-2,-3,-2,   4,-3,-3,-3,-3,
     >                  5,-3,-4,-3,-4,   5,-3,-5,-3,-5,
     >-5,-4,-1,-4,-1,  -5,-4,-2,-4,-2,  -5,-4,-3,-4,-3,
     >                  4,-4,-4,-4,-4,   5,-4,-5,-4,-5,
     >-5,-5,-1,-5,-1,  -5,-5,-2,-5,-2,  -5,-5,-3,-5,-3,
     >                 -5,-5,-4,-5,-4,   4,-5,-5,-5,-5/
C ... first line is gluon + gluon --> gluon + gluon
C
C 7a  q_i + bar(q_i) --> q_j + bar(q_j)
      DATA (((NCDATA(I,J,K),I=0,4),J=1,5),K=28,32)/
     >12,21,21,21,21, 7,1,-1,2,-2,  7,1,-1,3,-3,
     >                7,1,-1,4,-4,  7,1,-1,5,-5,
     > 7,2,-2,1,-1,   0,0, 0,0, 0,  7,2,-2,3,-3,
     >                7,2,-2,4,-4,  7,2,-2,5,-5,
     > 7,3,-3,1,-1,   7,3,-3,2,-2,  0,0, 0,0, 0,
     >                7,3,-3,4,-4,  7,3,-3,5,-5,
     > 7,4,-4,1,-1,   7,4,-4,2,-2,  7,4,-4,3,-3,
     >                0,0, 0,0, 0,  7,4,-4,5,-4,
     > 7,5,-5,1,-1,   7,5,-5,2,-2,  7,5,-5,3,-3,
     >                7,5,-5,4,-4,  0,0, 0,0, 0 /
C 7b  bar(q_i) + q_i --> bar(q_j) + q_j
      DATA (((NCDATA(I,J,K),I=0,4),J=1,5),K=33,37)/
     > 0,0, 0,0, 0,  -7,-1,1,-2,2,  -7,-1,1,-3,3,
     >               -7,-1,1,-4,4,  -7,-1,1,-5,5,
     >-7,-2,2,-1,1,   0,0, 0,0, 0,  -7,-2,2,-3,3,
     >               -7,-2,2,-4,4,  -7,-2,2,-5,5,
     >-7,-3,3,-1,1,  -7,-3,3,-2,2,   0, 0,0, 0,0,
     >               -7,-3,3,-4,4,  -7,-3,3,-5,5,
     >-7,-4,4,-1,1,  -7,-4,4,-2,2,  -7,-4,4,-3,3,
     >                0,0, 0,0, 0,  -7,-4,4,-5,4,
     >-7,-5,5,-1,1,  -7,-5,5,-2,2,  -7,-5,5,-3,3,
     >               -7,-5,5,-4,4,   0, 0,0, 0,0 /
      DATA (((NCDATA(I,J,K),I=0,4),J=1,5),K=38,43)/
C 9a  q_i + bar(q_i) --> gluon + gluon
     > 9,1,-1,21,21,  9,2,-2,21,21, 9,3,-3,21,21,
     >                9,4,-4,21,21, 9,5,-5,21,21,
C 9b  bar(q_i) + q_i --> gluon + gluon
     >-9,-1,1,21,21,  -9,-2,2,21,21, -9,-3,3,21,21,
     >                -9,-4,4,21,21, -9,-5,5,21,21,
C 10a q_i + gluon ---> q_i + gluon
     > 10,1,21,1,21,  10,2,21,2,21, 10,3,21,3,21,
     >                10,4,21,4,21, 10,5,21,5,21,
C 10b bar(q_i) + gluon ---> bar(q_i) + gluon
     > 10,-1,21,-1,21,  10,-2,21,-2,21, 10,-3,21,-3,21,
     >                  10,-4,21,-4,21, 10,-5,21,-5,21,
C 10c gluon + q_i ---> gluon + q_i
     >-10,21,1,21,1,  -10,21,2,21,2, -10,21,3,21,3,
     >                -10,21,4,21,4, -10,21,5,21,5,
C 10d gluon + bar(q_i) ---> bar(q_i) + gluon
     >-10,21,-1,21,-1,  -10,21,-2,21,-2, -10,21,-3,21,-3,
     >                  -10,21,-4,21,-4, -10,21,-5,21,-5/
      DATA ((NCDATA(I,J,44),I=0,4),J=1,5)/
C 11  gluon + gluon ---> q + qbar
     > 11,21,21,1,-1,  11,21,21,2,-2,  11,21,21,3,-3,
     >                 11,21,21,4,-4,  11,21,21,5,-5 /
C*CC MNJTYP
C
C
      DATA  PI   /3.141592653E0/
      REAL*4  PCM3(4)
C
C =====< Entry Point >=================================================
C
C
C Know process type and determine final particles.
C
C
      NFINAL = 0
      IAPRC  = IABS(NPRC)
      IF( IAPRC.EQ.0 ) RETURN
C
C .. Set CM momentum
C
      E1 = EGAM1
      E2 = EGAM2
C ... PVCT(,1) ; CM vector.
      PVCT(1,1) = 0
      PVCT(2,1) = 0
      PVCT(3,1) = E1-E2
      PVCT(4,1) = E1+E2
      WGG       = 2.*SQRT(E1*E2)
C
C Set 4 momenta according to the mode.
C
      TWOPI = PI*2.
      SN    = SQRT((1.-CS)*(1.+CS))
      PHI   = TWOPI*DRN(DUMMY)
      PVCT(1,2) = EHAT*SN*COS(PHI)
      PVCT(2,2) = EHAT*SN*SIN(PHI)
      PVCT(3,2) = EHAT*CS
      PVCT(4,2) = EHAT
      PVCT(1,3) = -PVCT(1,2)
      PVCT(2,3) = -PVCT(2,2)
      PVCT(3,3) = -PVCT(3,2)
      PVCT(4,3) = EHAT
      PTEVT     = EHAT*SN
      GBUF(5)   = 2.*EHAT
      GBUF(6)   = PTEVT
      GBUF(8)   = PHI
      WGG00     = 2.*SQRT(EGAM10*EGAM20)
      GBUF(9)   = WGG00
C
C .. In case of Direct process
C
       CALL UVZERO( 80 , RBUF)
      XF = 1.0
      IF( IAPRC .EQ. 1 ) THEN
        NFINAL = 2
        RBUF(2,1) = KCDATA(3,INDX)
        RBUF(2,2) = KCDATA(4,INDX)
        DO 200 I = 1, NFINAL
          CALL UBSTBK( PVCT(1, I+1), PVCT(1,1), RBUF(5,I))
200     CONTINUE
        RBUF(20,1) = 10000.
        RBUF(20,2) = 0.
        IP1= 1
        IP2= 2
C
C ... 1 Resolved, gamma+q --> gluon + q case.
C       NPRC = +2, emit q from e+
C       NPRC = -2, emit q from e-
C
      ELSEIF( IAPRC .EQ. 2 ) THEN
        NFINAL = 3
        IF( NPRC.GT.0 ) THEN
          RBUF(2,1) = KCDATA(3,INDX)
          RBUF(2,2) = KCDATA(4,INDX)
          RBUF(2,3) =-KCDATA(4,INDX)
          CALL UBSTBK( PVCT(1, 2), PVCT(1,1), RBUF(5,1))
          CALL UBSTBK( PVCT(1, 3), PVCT(1,1), RBUF(5,2))
          ESPEC     = EGAM20*(1-XG(24))* XF
          RBUF(7,3) = -ESPEC
          RBUF(8,3) =  ESPEC
          RBUF(20,1)= 10000.
          RBUF(20,2)= 10000.
          RBUF(20,3)= 0.
          IP1= 1
          IP2= 2
        ELSE
          RBUF(2,3) = KCDATA(3,INDX)
          RBUF(2,2) = KCDATA(4,INDX)
          RBUF(2,1) =-KCDATA(3,INDX)
          CALL UBSTBK( PVCT(1, 2), PVCT(1,1), RBUF(5,2))
          CALL UBSTBK( PVCT(1, 3), PVCT(1,1), RBUF(5,3))
          ESPEC     = EGAM10*(1-XG(23))*XF
          RBUF(7,1) = ESPEC
          RBUF(8,1) = ESPEC
          RBUF(20,1)= 10000.
          RBUF(20,2)= 10000.
          RBUF(20,3)= 0.
          IP1= 2
          IP2= 3
        ENDIF
C
C ... 1 Resolved, gamma + gluon --> q qbar case
C       NPRC = +3, emit q from e+
C       NPRC = -3, emit q from e-
C     Assumes always u-ubar spectator.
C
      ELSEIF( IAPRC .EQ. 3 ) THEN
        NFINAL = 3
        IF( NPRC.GT.0 ) THEN
          RBUF(2,1) = KCDATA(3,INDX)
          RBUF(2,2) = 21
          RBUF(2,3) = KCDATA(4,INDX)
          CALL UBSTBK( PVCT(1, 2), PVCT(1,1), RBUF(5,1))
          CALL UBSTBK( PVCT(1, 3), PVCT(1,1), RBUF(5,3))
          ESPEC     = EGAM20*(1-XG(24))*XF
          RBUF(7,2) = -ESPEC
          RBUF(8,2) = ESPEC
          RBUF(20,1)= 10000.
          RBUF(20,2)= 10000.
          RBUF(20,3)= 0.
          IP1= 1
          IP2= 3
        ELSE
          RBUF(2,1) = KCDATA(3,INDX)
          RBUF(2,2) =21
          RBUF(2,3) = KCDATA(4,INDX)
          CALL UBSTBK( PVCT(1, 2), PVCT(1,1), RBUF(5,1))
          CALL UBSTBK( PVCT(1, 3), PVCT(1,1), RBUF(5,3))
          ESPEC     = EGAM10*(1-XG(23))*XF
          RBUF(7,2) = ESPEC
          RBUF(8,2) = ESPEC
          RBUF(20,1)= 10000.
          RBUF(20,2)= 10000.
          RBUF(20,3)= 0.0
          IP1= 1
          IP2= 3
        ENDIF
C
C ... 2 Resolved
C
      ELSE
        IAD1 = IABS(KCDATA(1,INDX))
        IAD2 = IABS(KCDATA(2,INDX))
        IAD3 = IABS(KCDATA(3,INDX))
        IAD4 = IABS(KCDATA(4,INDX))
        IF( IAD1.LT.10 .AND. IAD2.LT.10 ) THEN
          NFINAL = 4
          RBUF(2,1) =-KCDATA(1,INDX)
          RBUF(2,2) = KCDATA(3,INDX)
          RBUF(2,3) = KCDATA(4,INDX)
          RBUF(2,4) =-KCDATA(2,INDX)
C--
C Symmetrize color flow for q_i q_i --> q_i q_i scattering.
C--
            IF(IAPRC.EQ.4 ) THEN
              IF(DRN(DUMMY).GT.0.5)  NPRC = - NPRC
             ENDIF
          IF(NPRC.GT.0)  THEN
          CALL UBSTBK( PVCT(1, 2), PVCT(1,1), RBUF(5,2))
          CALL UBSTBK( PVCT(1, 3), PVCT(1,1), RBUF(5,3))
          IP1= 2
          IP2= 3
          ELSE
          CALL UBSTBK( PVCT(1, 2), PVCT(1,1), RBUF(5,3))
          CALL UBSTBK( PVCT(1, 3), PVCT(1,1), RBUF(5,2))
          IP1= 3
          IP2= 2
          ENDIF
          ESPEC1    = EGAM10*(1-XG(23))*XF
          ESPEC2    = EGAM20*(1-XG(24))*XF
          RBUF(7,1) = ESPEC1
          RBUF(7,4) =-ESPEC2
          RBUF(8,1) = ESPEC1
          RBUF(8,4) = ESPEC2
          RBUF(20,1)= 10000.
          RBUF(20,2)= 0.
          RBUF(20,3)= 10000.
          RBUF(20,4)= 0.
C--   q_i q_ibar --> g g case.
          IF(IAPRC.EQ.9)  THEN
          RBUF(20,1)= 10000.
          RBUF(20,2)= 10000.
          RBUF(20,3)= 10000.
          RBUF(20,4)= 0.
          ENDIF
        ELSEIF( IAD1.LT.10 .AND. IAD2.GT.10 ) THEN
          NFINAL = 4
          RBUF(2,1) =-KCDATA(1,INDX)
          RBUF(2,4) = KCDATA(3,INDX)
          RBUF(2,2) = KCDATA(4,INDX)
          RBUF(2,3) = 21
          IF(NPRC.GT.0)  THEN
          CALL UBSTBK( PVCT(1, 2), PVCT(1,1), RBUF(5,4))
          CALL UBSTBK( PVCT(1, 3), PVCT(1,1), RBUF(5,2))
          IP1= 4
          IP2= 2
          ELSE
          CALL UBSTBK( PVCT(1, 2), PVCT(1,1), RBUF(5,2))
          CALL UBSTBK( PVCT(1, 3), PVCT(1,1), RBUF(5,4))
          IP1= 2
          IP2= 4
          ENDIF
          ESPEC1    = EGAM10*(1-XG(23))*XF
          ESPEC2    = EGAM20*(1-XG(24))*XF
          RBUF(7,1) = ESPEC1
          RBUF(7,3) =-ESPEC2
          RBUF(8,1) = ESPEC1
          RBUF(8,3) = ESPEC2
          RBUF(20,1)= 10000.
          RBUF(20,2)= 10000.
          RBUF(20,3)= 10000.
          RBUF(20,4)= 0.
        ELSEIF( IAD1.GT.10 .AND. IAD2.LT.10 ) THEN
          NFINAL = 4
          RBUF(2,3) = 21
          RBUF(2,2) = KCDATA(3,INDX)
          RBUF(2,1) = KCDATA(4,INDX)
          RBUF(2,4) =-KCDATA(2,INDX)
          IF(NPRC.GT.0) THEN
          CALL UBSTBK( PVCT(1, 2), PVCT(1,1), RBUF(5,2))
          CALL UBSTBK( PVCT(1, 3), PVCT(1,1), RBUF(5,1))
          IP1= 2
          IP2= 1
          ELSE
          CALL UBSTBK( PVCT(1, 2), PVCT(1,1), RBUF(5,1))
          CALL UBSTBK( PVCT(1, 3), PVCT(1,1), RBUF(5,2))
          IP1= 1
          IP2= 2
          ENDIF
          ESPEC1    = EGAM10*(1-XG(23))*XF
          ESPEC2    = EGAM20*(1-XG(24))*XF
          RBUF(7,3) = ESPEC1
          RBUF(7,4) =-ESPEC2
          RBUF(8,3) = ESPEC1
          RBUF(8,4) = ESPEC2
          RBUF(20,1)= 10000.
          RBUF(20,2)= 10000.
          RBUF(20,3)= 10000.
          RBUF(20,4)= 0.
        ELSE
C  gluon + gluon --> gluon + gluon case.
          NFINAL = 4
          RBUF(2,1) = 1
          RBUF(2,2) = KCDATA(3,INDX)
          RBUF(2,3) = KCDATA(4,INDX)
          RBUF(2,4) = -1
C--
C Symmetrize color flow for g  g    -->  g  g   scattering.
C--
            IF(DRN(DUMMY).GT.0.5)  NPRC = - NPRC
          IF(NPRC.GT.0 ) THEN
          CALL UBSTBK( PVCT(1, 2), PVCT(1,1), RBUF(5,2))
          CALL UBSTBK( PVCT(1, 3), PVCT(1,1), RBUF(5,3))
          IP1= 2
          IP2= 3
          ELSE
          CALL UBSTBK( PVCT(1, 2), PVCT(1,1), RBUF(5,3))
          CALL UBSTBK( PVCT(1, 3), PVCT(1,1), RBUF(5,2))
          IP1= 3
          IP2= 2
          ENDIF
          ESPEC1    = EGAM10*(1-XG(23))*XF
          ESPEC2    = EGAM20*(1-XG(24))*XF
          RBUF(7,1) = ESPEC1
          RBUF(7,4) =-ESPEC2
          RBUF(8,1) = ESPEC1
          RBUF(8,4) = ESPEC2
          RBUF(20,1)= 10000.
          RBUF(20,2)= 10000.
          RBUF(20,3)= 10000.
          RBUF(20,4)= 0.
C  gluon + gluon --> q + qbar case.
          IF(IAPRC.EQ.11) THEN
          RBUF(2,1) = -1
          RBUF(2,2) = KCDATA(3,INDX)
          RBUF(2,3) = KCDATA(4,INDX)
          RBUF(2,4) =  1
          RBUF(20,1)= 10000.
          RBUF(20,2)= 0.
          RBUF(20,3)= 10000.
          RBUF(20,4)= 0.
          ENDIF
        ENDIF
      ENDIF
          RBUF(9,IP1) = 3
          RBUF(9,IP2) = 4
C-- CALICULATE RAPIDITY
       Y1 = .5 * ALOG((RBUF(8,IP1) + RBUF(7,IP1))/
     +                (RBUF(8,IP1) - RBUF(7,IP1))  )
       Y2 = .5 * ALOG((RBUF(8,IP2) + RBUF(7,IP2))/
     +                (RBUF(8,IP2) - RBUF(7,IP2))  )
       GBUF(10) = Y1
       GBUF(11) = Y2
C--
C     Boost to hard scattering C-M system to obation conTH*
C     GBUF(7)   = CS
          CALL UBSTFD( RBUF(5,IP1), PVCT(1,1), PCM3(1)   )
        PP     = SQRT(PCM3(1)**2 + PCM3(2)**2 + PCM3(3)**2)
        GBUF(7) = PCM3(3)/PP
CD      IF(NDMP.LE.5  ) THEN
CD       PRINT *,' MNJSPR   IAPRC =',IAPRC,' IP1=',IP1
CD       PRINT *,' PVCT  =', PVCT(1,2),PVCT(2,2),PVCT(3,2),PVCT(4,2)
CD       PRINT *,' PCM3  =', PCM3(1),PCM3(2),PCM3(3),PCM3(4)
CD       NDMP = NDMP + 1
CD      ENDIF
C
      RETURN
      END
C*DK MNJHAD
      SUBROUTINE MNJHAD( NPART, RBUF )
C *********************************************************************
C (Author)
C   A. Miyamoto   22-Nov-1991    Original version.
C (Modified)
C   A.J.Finch      1-Jun-1992    Converted for use in ALEPH
C   A.J.Finch     13-Feb-1993    Converted to use LUND 7.3
C!   Copy Parton momentum into /LUJETS/ common and hadronise them.
C
C Input
C   NPART  ; Number of Input partons
C   RBUF(20,i) ; Particle information, as of SPRING;Parton_List
C                Format.
C Output
C   Kept in /LUJETS/ common.
C
C *********************************************************************
C
      REAL*4 RBUF(20,NPART)
C*CA LUNDCOM
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
C*CC LUNDCOM
      INTEGER*4        NPID(6)
      DATA NPID/502, 501, 503, 504, 505, 506 /
C
C*CA MNJPID
C-------------MNJPID   file-----------------------------------
      INTEGER*4  NTPJLC
      COMMON / MNJPID / NTPJLC
C
C ... NTPJLC ... Particle ID code.
C     = 0 for TOPAZ ID
C     = 1 for JLC ID
C
C*CC MNJPID
CJLC          DATA  NTPJLC/1/
CTOPAZ        DATA  NTPJLC/0/
C
C =====< Entry Point >==================================
C
C --------------------------
C (1) Put beam and scattered electrons, into the top of LUND common
C---------------------------
      N7LU=0
      CALL MJLUEL
C
C --------------------------
C (2) Write Parton information in to /LUJETS/ common
C --------------------------
C
      N7LU = 4
      DO 100 I = 1, NPART
        N7LU = N7LU + 1
      IF(RBUF(20,I).EQ.10000)THEN
        K7LU(N7LU,1) = 2
      ELSEIF(RBUF(20,I).EQ.0)THEN
        K7LU(N7LU,1) = 1
      ELSE
        WRITE(6,*)' WHAT IS THIS THEN ?',RBUF(20,I)
        K7LU(N7LU,1) = 2
      ENDIF
        K7LU(N7LU,2) = INT(RBUF(2,I))
        K7LU(N7LU,3) = 0
        K7LU(N7LU,4) = 0
        K7LU(N7LU,5) = 0
        P7LU(N7LU,1) = RBUF(5,I)
        P7LU(N7LU,2) = RBUF(6,I)
        P7LU(N7LU,3) = RBUF(7,I)
        P7LU(N7LU,5) = ULMASS(K7LU(N7LU,2))
        PMOM2  = RBUF(5,I)**2 + RBUF(6,I)**2 + RBUF(7,I)**2
        P7LU(N7LU,4) = SQRT( PMOM2 + P7LU(N7LU,5)**2 )
        DO  99 J = 1,5
99      V7LU(N7LU,J) = 0.0
100   CONTINUE
C      MST(1) = 0
C      MST(2) = 0
C      MST(3) = 0
C -----------------------------------------------------------------
C (3) DO LUEXEC for fragmentation.
C -----------------------------------------------------------------
      CALL LUEXEC
      RETURN
      END
C*DK UVZERO
      SUBROUTINE UVZERO(N,ARRAY)
C------------------------------------------------------------
C!  ZERO AN ARRAY
C  A.J. Finch last mod 15/2/93
C     structure : subroutine
C     INPUT
C      N : array size
C  ARRAY : array to zero
C -----------------------------------------------------------------
C
      REAL ARRAY(N)
      DO 1 I = 1,N
        ARRAY(I) = 0.0
1     CONTINUE
      RETURN
      END
C*DK MJLUEL
      SUBROUTINE MJLUEL
C------------------------------------------------------------
C! Put scattered electrons in LUJETS ( GGMJ01 )
C  A.J. Finch last mod 15/2/93
C     structure : subroutine
C -----------------------------------------------------------------
      REAL ME
C*CA LUNDCOM
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
C*CC LUNDCOM
C*CA EVTPRM
C-------------EVTPRM   file-----------------------------------
      REAL*8          RS, EMINS, EPLUS, EBEAM, XG(25), EGAM1, EGAM2
     >              , CS, EHAT, EGAM10, EGAM20, PTMIN
      INTEGER*4       NRTYPE, NRDUMY, NPRC, INDX
      COMMON /EVTPRM/ RS, EMINS, EPLUS, EBEAM, XG  , EGAM1, EGAM2
     >              , NRTYPE, NRDUMY
     >              , CS, EHAT, EGAM10, EGAM20, PTMIN
     >              , NPRC, INDX
C
C   RS     ; Center of mass  energy
C   EMINS  ; not used
C   EPLUS  ;
C   EBEAM  ; Beam energy
C   XG(0)  ; Randum variables for event generation/
C
C   CS     ; Cos(th) of the event
C   EHAT   ; CMS Energy (Hard Collision)
C
C   EGAM10 ; X1*EBEAM
C   EGAM20 ; X2*EBEAM
C   EGAM1  ; X1*X3*EBEAM
C   EGAM2  ; X2*X4*EBEAM
C   PTMIN  ; Minimum Pt of a jet.
C*CC EVTPRM
C
C THE FIRST TWO LINES IN LUJETS ARE THE BEAM ELECTRONS
C  THE NEXT TWO ARE THE SCATTERED ELECTRONS
C  NOTE - THEY ARE EXPLICITLY 0 Q**2 ,THERE IS NO SCATTERING !
C          THE PHYSICS IN FNCMNJ ASSUMES THIS
C
      ME = ULMASS(11)
      P7LU(1,4) = EBEAM
      K7LU(1,1) = 21
      P7LU(2,4) = EBEAM
      K7LU(2,1) = 21
      P7LU(3,4) = EBEAM - EGAM10
      K7LU(3,1) = 1
      P7LU(4,4) = EBEAM - EGAM20
      K7LU(4,1) = 1
      DO 1 NLU = 1,4
        P7LU(NLU,5) = ME
        P7LU(NLU,3) = SQRT(P7LU(NLU,4)**2-P7LU(NLU,5)**2)
        P7LU(NLU,2) = 0.0
        P7LU(NLU,1) = 0.0
        K7LU(NLU,2) = 11*(2*MOD(NLU,2)-1)
1     CONTINUE
      P7LU(2,3) = -P7LU(2,3)
      P7LU(4,3) = -P7LU(4,3)
      RETURN
      END
C*DK SPINIT
      SUBROUTINE SPINIT
C -----------------------------------------------------------------
C    A.J.Finch 12/2/93
C!   Initialization routine for SPRING. - just write out Xsection.
C     so that no one can miss it
C    modified 28/6/93
C     - Print out Accumulated result, not result of last
C        Iintegration. Also print out error.
C -----------------------------------------------------------------
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 SI,SI2,SWGT,SCHI,SCALLS,ATACC,WGT
      COMMON / BASE3  / SI,SI2,SWGT,SCHI,SCALLS,ATACC,NSU,IT,WGT
      PARAMETER (ITM = 50)
      REAL*4   TIME,EFF,WRONG,TRSLT,TSTD,PCNT
      REAL*8   RESLT,ACSTD
      COMMON /BASE5/ ITRAT(ITM),TIME(ITM),EFF(ITM),WRONG(ITM),
     .       RESLT(ITM),ACSTD(ITM),TRSLT(ITM),TSTD(ITM),PCNT(ITM)
C
      INTEGER ISTATS,IVERS
      COMMON/CMJIF/ISTATS(100),IVERS
      REAL*4 XTOT,XACC,RTOT,RACC
      INTEGER NTOT,NACC,KINTYP,IDC,IS,ISEC,KSECBK
      EXTERNAL KSECBK
      PARAMETER(KINTYP=6008)
C Get total cross section.
       TOTSIG =  RESLT(IT)
       ERRSIG = ACSTD(IT)
       WRITE(6,600) REAL(TOTSIG),REAL(ERRSIG)
600   FORMAT(/////,15X,'---------------------------------------'/,
     1             15X,'-                                     -'/,
     1             15X,'-             G G M J 0 1             -'/,
     1             15X,'-                                     -'/,
     1             15X,'-      Total Cross section for        -'/,
     1             15X,'-        this process is              -'/,
     1             15X,'-   ',E15.5,' +/-               -'/,
     1             15X,'-   ',E15.5,' (pb)              -'/,
     1             15X,'---------------------------------------',
     1        /////)

      RETURN
      entry ugtsec
        print *,it,scalls,RESLT(IT),ACSTD(IT)
      IDC = KINTYP
      IS = 1
      NTOT = ISTATS(42) 
      XTOT = real(RESLT(IT))
      RTOT = real(ACSTD(IT))
      NACC = NTOT
      XACC = XTOT
      RACC = RTOT
      isec = KSECBK(IS,IDC,IVERS,NTOT,NACC,XTOT,RTOT,XACC,RACC)
      CALL PRTABL('KSEC',0)

      END
