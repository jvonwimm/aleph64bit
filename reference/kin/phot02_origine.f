C-----------------------------------------------------------------------
C  A  L  E  P  H   I  N  S  T  A  L  L  A  T  I  O  N    N  O  T  E  S |
C                                                                      |
C    original code : PHOPHO from A.FINCH                               |
C    trasmitted by : A. FINCH  Last mods from MAY 1993                 |
C    This file contains the origine code of PHOT02 and some interface  |
C   routines from the phot02 package that can loosely be described as  |
C  interface ,i.e. they perform no physics action                      |
C     Modifications:B. Bloch September 1993                            |
C                   BSMAIN(IERR) : IERR was not initialized            |
C                    this caused trouble on IBM when tested after call |
C                   MCGDST       : PARAMETER MCGCOM renamed to LMCGCOM |
C                              to avoid confusion with common /MCGCOM/ |
C-                  A.Finch august 96 : in MCGRHO , call               |
C               MCFPTQ(0.5*PQRK,PTVDM) instead of MCFPTQ(0.5*PQRK,5.)  |
C-                  A.Finch may 1997 : allow generated resonance       |
C                                     masses to vary with the width.   |
C-                  B.Bloch February 2001 to port code to Linux        |
C                   GAMLUN: make RN arg real instead of integer        |
C                   DSTWR : call rmarut instead of RNSAVE              |
C                   PLUTOQ: make RN arg real instead of integer        |
C                   MCGRHO: give correct number of args to HBOOK1 calls|
C                           make RN arg real instead of integer        |
C                   PERIPH: declare RN arg REAL*4                      |
C-                  A.Finch February 2001 fix uninitialized variable   |
C                   FUNQED : x(8) was not defined but used later       |
C-                  A.Finch August 2001 add some declarations in       |
CC                  GAMLUN: more save statements                       |
C                   MCGWR : add common/result/                         |
C-----------------------------------------------------------------------
      SUBROUTINE GAMLUN
C---------------------------------------------------------------
C! (PHOT02)  FILL THE LUND ARRAY FROM LABVA COMMMON
C
C THIS ROUTINE FILLS THE LUND ARRAY FROM THE LABVA
C COMMON
C NOTE THAT IT BEHAVES DIFFERENTLY FOR THE THREE CASES
C A) QED B) VDM C) PSEUDOSCALAR GENERATION
C IN ALL CASES THE SCATTERED ELECTRONS ARE COPIED OVER
C A) QED - FINAL STATE LEPTONS ALSO COPIED
C B)     - NOTHING ELSE DONE HERE ( SEE MCGVDM)
C C) PSEUDOS - FINAL STATE PARTICLE ADDED ( CALCULATED FROM
C              SCATTERED ELECTRONS USING CONSERVATION LAWS )
C
C CALLED BY - ASKUSE
C
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C            April    1997 corrected to allow generated resonance
C                          masses to vary with the width.
C----------------------------------------------------------------
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
       COMMON / LABVA / EW,E3,E5,E6,E7,PW,P3,P5,P6,P7,GAM1,GAM2,
     *                  AL3,BE5,CTW,STW,CT3,ST3,CT5,ST5,CT6,ST6,
     *                  CT7,ST7,CP3,SP3,CP5,SP5,CP6,SP6,CP7,SP7,
     *                  W,W2
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
C-------------------------------------------------------------------
C COMMON DATA FOR WHOLE PROGRAM
C
C PI        PI RADIANS
C PIDEG     PI IN DEGREES ( 180)   RADDEG    CONVERT RADIANS TO DEGREES
C ME        MASS OF ELECTRON       ME2       ""        ""       SQUARED
C MU2       MASS OF GENERATED PARTICLE **2
C S         2 * EBEAM              EMP
C CONST     OVERALL CONSTANT FOR F(X) CALC
C
       COMMON / DATA  / PI,PIDEG,RADDEG,ME,ME2,MU2,S,EMP,CONST
       REAL ME,ME2,MU2
C----------------------------------------------------------------
       CHARACTER*4  CHAINT,TYPE
       DIMENSION ELE(5),POS(5),ELE1(5),POS1(5),QRK1(5),QRK2(5)
       LOGICAL LFIRST
       SAVE    SAVE   IZERO,THMAS,TYPE,ELE,POS,LUELEC,LFIRST
       DATA    LFIRST/.TRUE./
C------<ENTRY POINT>---------------------------------------------
C--- if first event, then get general properties such as Ebeam etc.
       IF ( LFIRST ) THEN
          IZERO  = 0
          THMAS = 2.0 * AMASS + 0.1
          TYPE  = CHAINT(KTYPE)
C--- save information about beam electrons
C     ( assumed same for all events )
          ELE( 1) = 0.0
          ELE( 2) = 0.0
          ELE( 3) = -1.0*SQRT(EBEAM*EBEAM - ME2)
          ELE( 4) = EBEAM
          ELE( 5) = ME
          POS( 1) = 0.0
          POS( 2) = 0.0
          POS( 3) = SQRT(EBEAM*EBEAM - ME2)
          POS( 4) = EBEAM
          POS( 5) = ME
          LUELEC =  MCGTYP('ELEC')
          LFIRST = .FALSE.
       ENDIF
C--- decide on quark type - mix u and d's in ratio u:d=16:1
C           (FOR UD OR VDM TYPE GENERATION)
C
        IF(TYPE.EQ.'UD  '.OR.TYPE.EQ.'VDM ')THEN
             LUNTYP    = 2
             RUS     = 17.0 * RN(0.)
             IF ( RUS .GT. 16.0 ) LUNTYP = 1
        ENDIF
C--- decide on phi for electrons and quarks
C--- ( necessary for electrons but also done
C--- for quarks for good measure )
       PHI4 = PI*( 1. - 2.*RN( 0. ) )
       SPH4 = SIN( PHI4 )
       CPH4 = COS( PHI4 )
C--- scattered electron/positron
       R3   = P3
       PX   = R3*ST3*CP3
       PY   = R3*ST3*SP3
       POS1( 1 ) = PX*CPH4 - PY*SPH4
       POS1( 2 ) = PX*SPH4 + PY*CPH4
       POS1( 3 ) = R3*CT3
       POS1( 4 ) = E3
       POS1( 5 ) = ME
       R5   = P5
       PX   = R5*ST5*CP5
       PY   = R5*ST5*SP5
       ELE1( 1 ) = PX*CPH4 - PY*SPH4
       ELE1( 2 ) = PX*SPH4 + PY*CPH4
       ELE1( 3 ) = R5*CT5
       ELE1( 4 ) = E5
       ELE1( 5 ) = ME
C--- fill LUND vectors ---------------------------------------------
C     for scattered electrons only at first
C--- beam electrons k(i,1)=40000
       K7LU(1,1) = 21
       K7LU(1,2) = LUELEC
       P7LU(1, 1)= POS(1)
       P7LU(1, 2)= POS(2)
       P7LU(1, 3)= POS(3)
       P7LU(1, 4)= POS(4)
       P7LU(1, 5)= POS(5)
       K7LU(2,1) = 21
       K7LU(2,2) = -LUELEC
       P7LU(2, 1)= ELE(1)
       P7LU(2, 2)= ELE(2)
       P7LU(2, 3)= ELE(3)
       P7LU(2, 4)= ELE(4)
       P7LU(2, 5)= ELE(5)
C--- scattered electrons
       K7LU(3,1) = 1
       K7LU(3,2) = LUELEC
       P7LU(3, 1)= POS1(1)
       P7LU(3, 2)= POS1(2)
       P7LU(3, 3)= POS1(3)
       P7LU(3, 4)= POS1(4)
       P7LU(3, 5)= POS1(5)
       K7LU(4,1) = 1
       K7LU(4,2) = -LUELEC
       P7LU(4, 1)= ELE1(1)
       P7LU(4, 2)= ELE1(2)
       P7LU(4, 3)= ELE1(3)
       P7LU(4, 4)= ELE1(4)
       P7LU(4, 5)= ELE1(5)
       N7LU = 4
       IF(LQED)THEN
C--- quarks
       R6   = P6
       PX   = R6*ST6*CP6
       PY   = R6*ST6*SP6
       QRK1( 1 ) = PX*CPH4 - PY*SPH4
       QRK1( 2 ) = PX*SPH4 + PY*CPH4
       QRK1( 3 ) = R6*CT6
       QRK1( 4 ) = E6
       QRK1( 5 ) = AMASS
       R7   = P7
       PX   = R7*ST7*CP7
       PY   = R7*ST7*SP7
       QRK2( 1 ) = PX*CPH4 - PY*SPH4
       QRK2( 2 ) = PX*SPH4 + PY*CPH4
       QRK2( 3 ) = R7*CT7
       QRK2( 4 ) = E7
       QRK2( 5 ) = AMASS
C     COPY TO LUND
C--- quarks or leptons for QED case
       K7LU(5,1) = 1
       K7LU(5,2) = LUNTYP
       P7LU(5, 1)= QRK1(1)
       P7LU(5, 2)= QRK1(2)
       P7LU(5, 3)= QRK1(3)
       P7LU(5, 4)= QRK1(4)
       P7LU(5, 5)= QRK1(5)
       IF(LUNTYP.LT.10) K7LU(5,1)=2
       K7LU(6,1) = 1
       K7LU(6,2) = -K7LU(5,2)
       P7LU(6, 1)= QRK2(1)
       P7LU(6, 2)= QRK2(2)
       P7LU(6, 3)= QRK2(3)
       P7LU(6, 4)= QRK2(4)
       P7LU(6, 5)= QRK2(5)
       N7LU = 6
       ENDIF
       IF(LPSEUD.OR.LBREIT)THEN
C--- fill LUND vectors ---------------------------------------------
C single resonance - calculate properties directly from
C scattered electrons - only AMASS is taken from common
C CALCULATE 4 VECTOR OF CENTRE OF MASS
C  BY SUBTRACTING 4 VECTOR OF SCATTERED ELECTRONS
C  FROM ORIGINAL ELECTRONS
C-------------------------------------------------------------------
       K7LU(5,1) = 1
       K7LU(5,2) = LUNTYP
       P7LU(5, 1)= -(P7LU(3,1)+P7LU(4,1))
       P7LU(5, 2)= -(P7LU(3,2)+P7LU(4,2))
       P7LU(5, 3)= -(P7LU(3,3)+P7LU(4,3))
C remove the fixed mass 
C       P7LU(5, 5)= AMASS
C       P7LU(5, 4)= SQRT(P7LU(5,1)**2+P7LU(5,2)**2+P7LU(5,3)**2+
C     1                  P7LU(5,5)**2)
C                                     NEW 23/4/97
C             VARY MASS ASWELL
C
       P7LU(5, 4)= P7LU(1,4)+P7LU(2,4)
     1            -(P7LU(3,4)+P7LU(4,4))
       P7LU(5, 5)= SQRT(P7LU(5,4)**2 -
     1 P7LU(5,1)**2 - P7LU(5,2)**2 - P7LU(5,3)**2)
       N7LU = 5
       ENDIF
       RETURN
       END
      SUBROUTINE MCGCWR(IFLAG)
C---------------------------------------------------------------
C! Write out the contents of the (PHOT02) common for the user.
C
C INPUT  parameters :
C                   IFLAG - control printing
C                    1       print everything
C                    2       Drop cross section
C                    3       Only cross section
C CALLED BY - MCGDST
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
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
      CHARACTER*4 TYPE,CHAINT
      TYPE = CHAINT(KTYPE)
        IF(IFLAG.EQ.1.OR.IFLAG.EQ.2)THEN
         WRITE(6,611) TYPE,
     *              EBEAM,AMASS,WMIN,WMAX,PTVDM,PTCUT,THMIN,THMAX,
     *              LUNTYP,ICOND,IVERS,GAMTOT,SPIN,IQMAX,QFIXED
C      IF(MAPIN)WRITE(6,*)'  Map to be read in '
C      IF(MAPOUT)WRITE(6,*)'  Map to be written out '
      WRITE(6,*)'  '
      WRITE(6,*)'  '
      WRITE(6,*)'  Here is some further explanation of '
      WRITE(6,*)'   the options selected...            '
      WRITE(6,*)'  '
      WRITE(6,*)'  The physics process selected is '
      IF(LVDM)THEN
       WRITE(6,*)'  VDM production of Hadrons. '
         IF(LCONST)THEN
            WRITE(6,*)'  with cross section independent of W '
         ELSE
            WRITE(6,*)'  with 1/W cross section '
         ENDIF
         IF(LPLUTO)THEN
            WRITE(6,*)' The final state invariant mass is '
            WRITE(6,*)' transformed into two quarks with '
            WRITE(6,*)' transverse momentum relative to  '
            WRITE(6,*)' the gamma gamma direction controlled'
            WRITE(6,*)' by the parameter Pt_VDM .'
            WRITE(6,*)' This currently has the value ',PTVDM
            WRITE(6,*)' This is the so called ''PLUTO'' model '
         ELSE
            WRITE(6,*)' The final state invariant mass is '
            WRITE(6,*)' transformed into four quarks with '
            WRITE(6,*)' transverse momentum relative to  '
            WRITE(6,*)' the gamma gamma direction controlled'
            WRITE(6,*)' by the parameter Pt_VDM .'
            WRITE(6,*)' This currently has the value ',PTVDM
            WRITE(6,*)' This is the so called ''RHO RHO'' model '
         ENDIF
      ENDIF
      IF(LQED)WRITE(6,612)TYPE,TYPE,TYPE,AMASS
      IF(LPSEUD)WRITE(6,614)TYPE
      IF(LBREIT)THEN
         WRITE(6,613)TYPE
         WRITE(6,*)' of mass ',AMASS,' with total width ',GAMTOT
         WRITE(6,*)' and spin ',SPIN
      ENDIF
      WRITE(6,*)'  '
      WRITE(6,*)'  '
      IF(MSTJ(1).EQ.0)THEN
            WRITE(6,*)'  LUND fragmentation DISABLED by MSTJ(2) '
      ELSE
           WRITE(6,*)'  LUND fragmentation will use  '
        IF(LPSHOW)THEN
          WRITE(6,*)' the Parton Shower process. '
          IF(IQMAX.EQ.0)THEN
              WRITE(6,*)' QMAX taken from Pt of quarks in event '
          ELSEIF(IQMAX.EQ.1)THEN
              WRITE(6,*)' QMAX taken from mass of final state '
              IF(LVDM)THEN
               WRITE(6,*)' This is NOT now recommended for VDM  '
               WRITE(6,*)' as Pt is not limited , however it does '
               WRITE(6,*)'  fit ALEPH data !'
              ENDIF
          ELSEIF(IQMAX.EQ.2)THEN
              WRITE(6,*)' QMAX is  at a fixed value of ',QFIXED
          ENDIF
        ELSE
          IF(MSTJ(1).EQ.1)THEN
           WRITE(6,*)'  LUND string fragmentation. '
          ELSEIF(MSTJ(1).EQ.2)THEN
           WRITE(6,*)'  independent jet fragmentation. '
          ENDIF
        ENDIF
      ENDIF
      WRITE(6,*)'  '
      WRITE(6,*)'  '
      IF(ICOND.EQ.0)THEN
            WRITE(6,*)' No cuts are applied to the scattered electron '
      ELSEIF(ICOND.EQ.1)THEN
            WRITE(6,*)' At least one electron must be scattered into an'
            WRITE(6,*)' angle between ',THMIN,' and ',THMAX,
     1               ' ( radians )'
      ELSEIF(ICOND.EQ.2)THEN
            WRITE(6,*)' BOTH  electrons must be scattered into an'
            WRITE(6,*)' angle between ',THMIN,' and ',THMAX,
     1              ' ( radians )'
      ENDIF
      WRITE(6,*)'  '
      WRITE(6,*)' The final state must have a mass (W) that lies '
      WRITE(6,*)' between ',WMIN,' and ',WMAX
      WRITE(6,*)'  '
      ENDIF
      IF(IFLAG.EQ.1.OR.IFLAG.EQ.3)THEN
      WRITE(6,*)'*********************************************',
     1'**************************'
      WRITE(6,*)'*                                            ',
     1'                         *'
      WRITE(6,*)'*                                            ',
     1'                         *'
      XSECT = AVGI
      XERR  = SDEV
        IF(LQED)THEN
                 WRITE(6,615)XSECT,XERR
                 WRITE(6,619)
        ELSE IF(LVDM)THEN
                 WRITE(6,616)XSECT,XERR
                 WRITE(6,617)300*XSECT
        ELSE IF(LPSEUD.OR.LBREIT)THEN
                 WRITE(6,618)XSECT,XERR
        ENDIF
       WRITE(6,*)'*                                            ',
     1'                         *'
      WRITE(6,*)'*                                            ',
     1'                         *'
      WRITE(6,*)'*********************************************',
     1'**************************'
      ENDIF
      RETURN
611   FORMAT(///,' +++++ PHOT02 COMMON ''/MCGCOM/'': VALUES OF'
     *,' VARIABLES ARE :',/,
     *       '  Type of particle to generate: ',a4,/,
     *       '  Beam energy       = ',f6.2,' GeV '/,
     *       '  Mass of particle  = ',f12.8,' GeV/c**2 (irrelevant for'
     *,' vdm )',/,'  Minimum invariant mass  = ',f6.2,' GeV ( ',
     *'irrelevant for psuedoscalar production) ',/,
     *'  Maximum invariant mass = ',F6.2,' GeV ',/,
     *       '  Pt_VDM  = ',F6.2,' GeV ( controls Pt distribution for',
     *' VDM) ',/,'  Ptcut   = ',F6.2,' GeV ( minimum quark Pt for',
     *' QPM )',/,'  Minumum angle for tagging  = ',F6.2,' radians ',/,
     *       '  Maximum angle for tagging  = ',F6.2,' radians ',/,
     *          '  Lund type code for produced particle   = ',I3/,
     *       '  Geometry control flag I_cond        = ',I3,/,
     *       '  Program version number              = ',I3,/
     *       '  Resonance - Width :',F6.2,' Spin :',F6.2,/
     *       ' QMAX control flag = ',I3,/
     *       '  Fixed value for QMAX in LUSHOW ',F6.2)
612   FORMAT('  QED production of pair of particles. '
     *,          ' e+e- --> e+e- ',A4,' ',A4/' where each ',
     *A4,' has a mass of ',E10.3)
613   FORMAT('  Production of resonance with Breit Wigner distribution '
     *,          ' e+e- --> e+e- ',A4)
614   FORMAT('  Production of Pseudoscalar resonance. '
     *,          ' e+e- --> e+e- ',A4)
615   FORMAT(' * Total cross section for this process = ',E10.3,
     1       '+/- ',E10.3 ,'     *')
619   FORMAT(' *                             microbarns ',
     1       '                             *')
616   FORMAT(' * Luminosity function for this process = ',E10.3,
     1       ' +/- ',E10.3,'    *')
617   FORMAT(' * so for a gamma gamma cross section of 300 nb ',
     1' the cross section     *'/' * is ',E10.3,' nb                ',
     1'                                    *')
618   FORMAT(' * Total cross section for this process = ',E10.3,
     1       ' +/- ',E10.3,'    *',/,
     1       ' *    times ( gamma gamma partial w',
     1       'idth in Gev) in microbarns          *')
      END
      SUBROUTINE BSMAIN(IERR)
C---------------------------------------------------------------
C! Controlling routine for BASES integration package (PHOT02)
C  Calls the BASES package for its two steps,
C  1) To define GRID to optimise integration
C  2) To calculate cross section
C
C INPUT  parameters :
C                    An optional card GBAS can be used to override
C                    the default control settings
C OUTPUT parameters :
C                    IERR - INTEGER error code
C                         0 - success
C                         5 - time out
C
C CALLED BY -  ASKUSI
C CALLS     -  BASES
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------
       EXTERNAL F
       COMMON/BASE1      /NDIM,NCUB1,NTRIAL,ITMX,IGRAPH,IFLAG,NOMAX
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
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
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
         NTRIAL = 3
         ITMX1  = 50
         ITMX2  = 50
         ACC1   = 1.0
         ACC2   = 1.0
        IGBAS = IW(NAMIND('GBAS'))
        IF(IGBAS.GT.0)THEN
           LEN = IW(IGBAS)
           IF(LEN.GE.1)NTRIAL= IW(IGBAS+1)
           IF(LEN.GE.2)ITMX1= IW(IGBAS+2)
           IF(LEN.GE.3)ITMX2= IW(IGBAS+3)
           IF(LEN.GE.4)ACC1=  RW(IGBAS+4)
           IF(LEN.GE.5)ACC2 = RW(IGBAS+5)
        ENDIF
C =======================
       IERR   = 0
       ITMX   = ITMX1
       BCC    = ACC1
       NCOND  = 0
       IFLAG  = 0
       WRITE(6,*)' Grid definition step, requesting accuracy ',
     1' of ',BCC,' percent in at most ',ITMX,' steps '
       CALL BASES(F,BCC)
       IF ( IFLAG     .NE. 1 ) GOTO 5000
       IF ( LFTIME(5) .EQ. 0 ) GOTO 5000
C =======================
       ITMX   = ITMX2
       BCC    = ACC2
       NTRIAL = 2
       WRITE(6,*)'    '
       WRITE(6,*)'    '
       WRITE(6,*)'    '
       WRITE(6,*)'    '
       WRITE(6,*)'    '
       WRITE(6,*)' Cross section calculation step, requesting'
     1 ,' accuracy of ',BCC,' percent in at most ',ITMX,' steps '
       CALL BASES(F,BCC)
C =======================
 3000  CONTINUE
       RETURN
 5000  CONTINUE
       WRITE(6,602)
  602  FORMAT(//,'+++++BSMAIN :  TIME-OUT +++++',//)
       IERR = 5
       RETURN
       END
      INTEGER FUNCTION MCGTYP(TYPE)
C---------------------------------------------------------------
C! INTEGER FUNCTION TO TRANSLATE NAME INTO LUND CODE(PHOT02)
C
C THIS ROUTINE RETURNS LUND TYPE CODE FOR
C A LIMITED LIST OF TYPES NAMED BY THE
C STRING 'TYPE'
C INPUT  parameters :
C                     TYPE : CHARACTER*4 Name of particle/parton
C OUTPUT parameters :
C                   MCGTYP : INTEGER*4 Equivalent LUND KF code
C CALLED BY - MCGINI,GAMLUN
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------
      PARAMETER (NTYPE=16)
      CHARACTER*4 TYPE
      INTEGER ITYPE(NTYPE)
      CHARACTER*4 LIST(NTYPE)
C NB TYPES 'UD', AND VDM  ARE SPECIAL CASES - PICKED UP AT LATER STAGE
      DATA LIST/'VDM ','UD  ','U   ','D   ','S   ','C   ','B   ',
     1          'T   ','ELEC','MUON','TAU ','ETAC','ETAB','ETA ',
     1          'PI0 ','F2  '/
      DATA ITYPE/2    ,2     ,2   ,1   ,3   ,4   ,5   ,
     1           6    ,11     ,13     ,15    ,441   ,551    ,221    ,
     1           111  ,225    /
      DO 10 I = 1,NTYPE
      IF(TYPE.NE.LIST(I))GOTO 10
            MCGTYP = ITYPE(I)
            RETURN
10    CONTINUE
      WRITE(6,600)TYPE
600   FORMAT(' MCGTYP UNABLE TO FIND TYPE CODE FOR TYPE :',A4)
      MCGTYP = 0
      RETURN
      END
      SUBROUTINE PHOHIS
C---------------------------------------------------------------
C! BOOK AND FILL HISTOGRAMS OF QED STAGE (PHOT02)
C
C NOTE THIS ROUTINE HAS TWO ALTERNATE ENTRY POINTS
C CALLED BY -
C                 PHOHIS  called by ASKUSE
C        ENTRY    PHOFIL  called by F(X)
C        ENTRY    PHOZER  called by F(X)
C CALLS     - HBOOK2,HMINIM,HIDOPT,HFILL
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------
 
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
C
C-------------------------------------------------------------------
C
C-------------------------------------------------------------------
C COMMON DATA FOR WHOLE PROGRAM
C
C PI        PI RADIANS
C PIDEG     PI IN DEGREES ( 180)   RADDEG    CONVERT RADIANS TO DEGREES
C ME        MASS OF ELECTRON       ME2       ""        ""       SQUARED
C MU2       MASS OF GENERATED PARTICLE **2
C S         2 * EBEAM              EMP
C CONST     OVERALL CONSTANT FOR F(X) CALC
       COMMON / DATA  / PI,PIDEG,RADDEG,ME,ME2,MU2,S,EMP,CONST
       REAL ME,ME2,MU2
       COMMON / LABVA / EW,E3,E5,E6,E7,PW,P3,P5,P6,P7,GAM1,GAM2,
     *                  AL3,BE5,CTW,STW,CT3,ST3,CT5,ST5,CT6,ST6,
     *                  CT7,ST7,CP3,SP3,CP5,SP5,CP6,SP6,CP7,SP7,
     *                  W,W2
      REAL    XL(25)
      LOGICAL FIRST
      SAVE    FIRST
      DATA    FIRST/.TRUE./
      DATA    IB/10000/
C----------------------------------------------------------------
C--- Book histograms
C----------------------------------------------------------------
      IF(.NOT.LHIST)RETURN
      IF(FIRST)THEN
       EJMN1 = 1.2*EBEAM
       QJMN12 = EJMN1*EJMN1*SIN(THMAX/2)**2
       IF(ICOND.EQ.0)QJMN12 = 1.0
       CALL HBOOK1( IB+1,' ENERGY OF SCATTERED ELECTRON'
     1 ,80,0.0,EJMN1,0.)
       CALL HBOOK1( IB+2,'THETA OF SCATTERED ELECTRON (DEGREES)'
     1,100,0.0,60.0,0.)
       CALL HBOOK1( IB+3,'ENERGY OF OTHER ELECTRON '
     1,80,0.0,EJMN1,0.)
       CALL HBOOK1( IB+4,'THETA OF OTHER ELECTRON ( DEGREES )'
     1,100,120.0,180.0,0.)
       CALL HBOOK1( IB+5,'Q**2 OF SCATTERED ELECTRON'
     1,100,0.0,QJMN12,0.)
       CALL HBOOK1( IB+6,'Q**2 OF OTHER SCATTERED ELECTRON '
     1,100,0.0,QJMN12,0.)
       CALL HBOOK1( IB+7,'ACOLINEARITY BETWEEN ELECTRONS '
     1,72,0.0,5.0,0.)
       AMAX = EJMN1
       IF(ITYPE.GT.100)AMAX = 1.5*(AMASS+2*GAMTOT)
       CALL HBOOK1( IB+8,'SQUARE OF INVARIANT MASS OF FINAL STATE '
     1,80,0.0,AMAX*AMAX,0.)
       CALL HBOOK1( IB+9,'INVARIANT MASS OF FINAL STATE '
     1,40,0.0,AMAX,0.)
       CALL HBOOK1( IB+10,'MOMENTUM BALANCE',50,-5.0,5.0,0.)
       CALL HBOOK1( IB+11,'TRANSVERSE MOMENTUM OF FINAL STATE'
     1,50,0.0,0.05*EJMN1,0.)
       DO 2 I = 1,11
       CALL HMINIM(IB + I,1.0)
2      CALL HIDOPT(IB + I,'LOGY')
       FIRST = .FALSE.
      ENDIF
C----------------------------------------------------------------
C--- Fill histograms from the XL array
C----------------------------------------------------------------
       DO 1 I = 1,11
1      CALL HFILL(IB + I,XL(I),0.0,1.0)
       RETURN
C----------------------------------------------------------------
       ENTRY    PHOFIL
C----------------------------------------------------------------
C
C Fill the XL array
C
C----------------------------------------------------------------
       PRD    = ST3*CP3*ST5*CP5+ST3*SP3*ST5*SP5+CT3*CT5
       ACOL   = PIDEG - ACOS( PRD ) * RADDEG
       Q32    = EBEAM*(E3-P3*CT3)-ME2
       Q32    = Q32*2.
       IF(Q32.LT.0.0)Q32 = 0.0
       Q52    = EBEAM*(E5+P5*CT5)-ME2
       Q52    = Q52*2.
       IF(Q52.LT.0.0)Q52 = 0.0
       BALMOM = P6*CT6 + P7*CT7
       PTM    = PW*STW
       TH3    = RADDEG * ACOS( CT3 )
       TH5    = RADDEG * ACOS( CT5 )
       XL( 1) = E3
       XL( 2) = TH3
       IF(XL(2).GE.180.00)XL(2)=179.99
       XL( 3) = E5
       XL( 4) = TH5
       IF(XL(4).GE.180.00)XL(4)=179.99
       XL( 5) = Q32
       XL( 6) = Q52
       XL( 7) = ACOL
       XL( 8) = W2
       XL( 9) = W
       XL(10) = BALMOM
       XL(11) = PTM
       RETURN
C----------------------------------------------------------------
       ENTRY    PHOZER
C----------------------------------------------------------------
C
C Clear the XL array
C
C----------------------------------------------------------------
       CALL     VZERO(XL,25)
       RETURN
       END
      SUBROUTINE PHOLHS
C---------------------------------------------------------------
C! Fill histogram of properties from LUND common (PHOT02)
C
C CALLED BY - ASKUSE
C CALLS     - HBOOK1,HIDOPT,HFILL
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
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
C-------------------------------------------------------------------
C
C COMMON DATA FOR WHOLE PROGRAM
C
C PI        PI RADIANS
C PIDEG     PI IN DEGREES ( 180)   RADDEG    CONVERT RADIANS TO DEGREES
C ME        MASS OF ELECTRON       ME2       ""        ""       SQUARED
C MU2       MASS OF GENERATED PARTICLE **2
C S         2 * EBEAM              EMP
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
C
      CHARACTER*4 TYPE,CHAINT
      LOGICAL FIRST,LEPTON,RESON
      SAVE    FIRST,LEPTON,RESON
      DATA IB/10020/
      DATA    FIRST/.TRUE./,LEPTON/.FALSE./
C----------------------------------------------------------------
      IF(.NOT.LHIST)RETURN
      IF(FIRST)THEN
       TYPE = CHAINT(KTYPE)
       IF(TYPE.EQ.'ELEC'.OR.TYPE.EQ.'MUON'.OR.TYPE.EQ.'TAU')
     1 LEPTON = .TRUE.
       IF(TYPE.EQ.'PI0 '.OR.TYPE.EQ.'ETA '.OR.TYPE.EQ.'ETAC'
     1 .OR.TYPE.EQ.'ETAB'.OR.TYPE.EQ.'F2  ')
     1 RESON = .TRUE.
      IF(ITYPE.EQ.200)RESON = .TRUE.
       CALL HBOOK1(IB+1,' ANGLE OF FINAL STATE PARTICLES',
     1            100,0.0,180.0,0.0)
       CALL HBOOK1(IB+2,' ANGLE OF ELECTRONS ',
     1            100,0.0,180.0,0.0)
       IF(.NOT.LEPTON.AND..NOT.RESON)THEN
         CALL HBOOK1(IB+3,' ANGLE OF PARTONS ',
     1              100,0.0,180.0,0.0)
         CALL HBOOK1(IB+5,' PT OF PARTONS ',
     1            20,0.0,10.0,0.0)
         CALL HIDOPT(IB+5,'LOGY')
       ENDIF
       PTMAX = 10.0
       IF(RESON)PTMAX = AMASS
       CALL HBOOK1(IB+4,' PT OF FINAL STATE PARTICLES',
     1            100,0.0,PTMAX,0.0)
       CALL HIDOPT(IB+4,'LOGY')
      FIRST = .FALSE.
      ENDIF
C----------------------------------------------------------------
      IFOUND = 0
C
      DO 10 I = 3, N7LU
        KS = K7LU(I,1)
        IK7 = ABS(K7LU(I,2))
C
C SKIP ALL THE INFORMATIONAL LINES
C
        IF(KS.GE.13)GOTO 10
C
        PTOT = SQRT(P7LU(I,1)**2+P7LU(I,2)**2+P7LU(I,3)**2)
        PT = SQRT(P7LU(I,1)**2+P7LU(I,2)**2)
        THETA = RADDEG*ACOS(P7LU(I,3)/PTOT)
        IF(THETA.EQ.180.0000)THETA = 179.9
        IF(I.EQ.3.OR.I.EQ.4)THEN
C
C SCATTERED ELECTRONS
C
            CALL HFILL(IB+2,THETA,0.0,1.0)
        ELSEIF(KS.EQ.1)THEN
C
C FINAL STATE PARTICLES
C
             CALL HFILL(IB+1,THETA,0.0,1.0)
             CALL HFILL(IB+4,PT,0.0,1.0)
        ELSEIF(KS.GT.10.AND.
     1  (IK7.LT.11.OR.IK7.EQ.21))THEN
C
C QUARKS AND GLUONS
C
             CALL HFILL(IB+3,THETA,0.0,1.0)
             CALL HFILL(IB+5,PT,0.0,1.0)
        ENDIF
10    CONTINUE
      RETURN
      END
      REAL FUNCTION RN(DUMMY)
C---------------------------------------------------------------
C! Interface from RN to RNDM (PHOT02)
C
C INPUT  parameters : DUMMY argument
C CALLED BY - various
C CALLS     - RNDM
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------
      RN  = RNDM(DUMMY)
      RETURN
      END
      SUBROUTINE RNSAVE(DUMMY)
C---------------------------------------------------------------
C! INTERFACE FROM RNSAVE TO RDMOUT (PHOT02)
C
C INPUT  parameters : DUMMY
C
C CALLED BY - VARIOUS
C CALLS     - RDMOUT
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------
      CALL RDMOUT (DUMMY)
      RETURN
      END
      SUBROUTINE RNSET(DUMMY)
C---------------------------------------------------------------
C! INTERFACE FROM RNSET TO RDMIN (PHOT02)
C
C INPUT  parameters : DUMMY
C
C CALLED BY - VARIOUS
C CALLS     - RDMIN
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------
      CALL RDMIN (DUMMY)
      RETURN
      END
      INTEGER FUNCTION NTIME(DUMMY)
C---------------------------------------------------------------
C! RETURN TIME REMAINING TO JOB (PHOT02)
C
C INPUT  parameters : DUMMY
C
C OUTPUT parameters :
C                     NTIME : Time remaining in 1/100 sec
C
C CALLED BY - BASES, SPRING
C CALLS     - TIMEL ( in KERNLIB )
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------
      REAL T
      CALL TIMEL(T)
      NTIME = INT(T*100)
      RETURN
      END
      SUBROUTINE AMCSEL(KEEP)
C---------------------------------------------------------------
C! THIS ROUTINE CAN DECIDE WHETHER TO KEEP AN EVENT  (PHOT02)
C  ON THE BASIS OF THE LUND PARTICLES
C HOWEVER THIS IS A DUMMY VERSION - USER MUST PROVIDE HIS OWN
C
C
C OUTPUT parameters :
C         KEEP      LOGICAL   Whether to keep event or not.
C CALLED BY - ASKUSE
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------
C
      LOGICAL KEEP,FIRST
      SAVE    FIRST
      DATA    FIRST/.TRUE./
      IF(FIRST)THEN
C
            WRITE(6,*)' '
            WRITE(6,*)' AMCSEL : This is a ''do nothing'' version from'
            WRITE(6,*)'            PHOT02 ORIGINE                     '
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
      SUBROUTINE MCGDST
C---------------------------------------------------------------
C! I/O package for (PHOT02)
C   ENTRY point MCGDST finds map name from cards,and opens the
C      file
C   ENTRY DSTRDX does nothing, should never be called
C   ENTRY DSTRED reads in an old map, with checks on parameters
C   ENTRY DSTWRT writes out a new map
C
C CALLED BY - ASKUSI, and BASES
C CALLS     - MCGWBL,MCGRBL
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------
C
C LREC HERE IS NUMBER OF I*4 WORDS  IN NATIVE BOS FILE RECORDS
C AJF
      PARAMETER(LREC=8010)
      INTEGER BUF(LREC-1)
C
       LOGICAL LTEMP1,LTEMP2,LTEMP3,LTEMP4
       INTEGER ITEMP1,ITEMP2
       CHARACTER*80 FNAME
       CHARACTER*4 TYPE,CHAINT,CNAME,DMODE,FDEVI
       CHARACTER*10 ATYPE,DTYPE
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
            INTEGER LMCGCOM
C
C LMCGCOM - NUMBER OF WORDS IN MCGCOM COMMON
C
            PARAMETER(LMCGCOM=132)
C-------------------------------------------------------------------
       DIMENSION IMCGC(1)
       EQUIVALENCE (IMCGC(1),LVDM)
C--- commons for DST routines called by BASES and SPRING
C       real*8 SI,SI2,SWGT,SCHI,SCALLS,XI
C       real*8 XND,DXG,DXD,DXP
C       real*8 XJAC,XN,RC,FB,FB2,TI,TSI,DV2G
C       real*8 XIN,R,DX,DT,DI,D
       COMMON / BASE1 / NDIM,NCUBES,NTRIAL,ITMX,IGRAPH,IFLAG,ICONDX
       COMMON / BASE2 / NDO,IT,SI,SI2,SWGT,SCHI,XI(50,10),SCALLS
       COMMON / BASE3 / ND,XND,NG,NSP,DXG,KG(10),MA(10),DXD(17000),
     *                  DXP(17000)
       COMMON / BASE4 / NOW,OWARI,K,XJAC,XN,RC,CALLS,FB,FB2,NPG,TI,
     *                  TSI,DV2G,NDM,IRN,ATACC,NSUC
       COMMON / BASE5 / DI(50,10),D(50,10),XIN(50),R(50),DX(10),
     *                  IA(10),DT(10)
       COMMON / RESULT / AVGI,SDEV,RDUMMY(2)
       COMMON / PLOT1 / NP1(4737)
       COMMON / PLOT2 / NP2(216)
       COMMON / PLOT3 / NP3(7)
       COMMON / PARMX / ND6(20)
       DIMENSION ND1(7),ND2(507),ND3(34025),ND4(17),ND5(1130)
       DIMENSION AND3(5),AND2(4)
       EQUIVALENCE (NDIM,ND1(1)),(NDO,ND2(1)),(ND,ND3(1)),(NOW,ND4(1))
       EQUIVALENCE (DI(1,1),ND5(1))
       EQUIVALENCE (ND2(3),AND2(1))
       EQUIVALENCE (AND3(1),ND3(1))
       DATA CNAME / 'GMAP' /
       DATA DTYPE / 'NATIVE    '/
       DATA DMODE / 'A   ' /
C---
       IF(MAPIN)THEN
         CALL ACDARG(CNAME,DTYPE,DMODE,FNAME,ATYPE,FDEVI,IER)
         IF(IER.NE.0)THEN
               WRITE(6,*)' ERROR ',IER,' FROM ACDARG CALLED BY  MCGDST '
               WRITE(6,*)' Did you provide a valid GMAP card ? '
               STOP
         ENDIF
         CALL AOPEN(MUNIN,FNAME,ATYPE,FDEVI,IER)
         IF(IER.NE.0)THEN
               WRITE(6,*)' ERROR ',IER,' FROM AOPEN CALLED BY  MCGDST '
               IF(IER.EQ.-1)WRITE(6,*)
     1         ' THE FILE :',FNAME,' DOES NOT EXIST '
               STOP
         ENDIF
       ELSEIF(MAPOUT)THEN
         CALL ACDARG(CNAME,DTYPE,DMODE,FNAME,ATYPE,FDEVI,IER)
         IF(IER.NE.0)THEN
               WRITE(6,*)' ERROR ',IER,' FROM ACDARG CALLED BY  MCGDST '
               WRITE(6,*)' Did you provide a valid GMAP card ? '
               STOP
         ENDIF
         CALL AOPENW(MUNOUT,FNAME,ATYPE,FDEVI,IER)
         IF(IER.NE.0)THEN
               WRITE(6,*)' ERROR ',IER,' FROM AOPENW CALLED BY  MCGDST '
     1         ,' WHEN TRYING TO OPEN MAP FILE :',FNAME,
     1          ' FOR WRITING OUT NEW MAP '
               STOP
         ENDIF
       ELSE
C         WRITE(6,*)' ERROR IN MCGDST, NEITHER MAPIN NOR MAPOUT ARE TRUE
C         STOP
C THIS IS NOW ALLOWED !
C
        RETURN
       ENDIF
      IF(MAPIN)THEN
       WRITE(6,*)
       WRITE(6,*)' MAP will be read from file  :',FNAME
      ENDIF
      IF(MAPOUT)THEN
       WRITE(6,*)' MAP will be written to  file : ',FNAME
       WRITE(6,*)
      ENDIF
       RETURN
C      ------------
       ENTRY DSTRDX
C      ------------
       WRITE(6,*)' DSTRDX HAS BEEN CALLED - THIS IS NOT LEGAL '
       RETURN
C      ------------
       ENTRY DSTRED
C      ------------
      IF(MAPIN)THEN
       CALL  MCGRBL  (MUNIN,ND1,LREC,7,BUF)
       CALL  MCGRBL  (MUNIN,ND2,LREC,507,BUF)
       CALL  MCGRBL  (MUNIN,ND3,LREC,34025,BUF)
       CALL  MCGRBL  (MUNIN,ND4,LREC,17,BUF)
       CALL  MCGRBL  (MUNIN,ND5,LREC,1130,BUF)
       CALL  MCGRBL  (MUNIN,NP1,LREC,4737,BUF)
       CALL  MCGRBL  (MUNIN,NP2,LREC,216,BUF)
       CALL  MCGRBL  (MUNIN,NP3,LREC,7,BUF)
       CALL  MCGRBL  (MUNIN,ND6,LREC,20,BUF)
C
C TO ALLOW FOR CHANGES WITH VERSION NUMBER
C - FIRST READ LENGTH OF COMMON BLOCK IN THIS VERSION
C
       CALL  MCGRBL  (MUNIN,ILENG,LREC,1,BUF)
C
C IT MUST BE LESS THAN OR EQUAL !
C
       IF(ILENG.GT.LMCGCOM)THEN
             WRITE(6,6101)MUNIN,ILENG,LMCGCOM
6101        FORMAT('  INPUT FILE ON UNIT ',I3,
     1             ' IS INCOMPATIBLE - LENGTH READ :',I3,
     1             ' SHOULD BE ',I3,/,' ***** ',
     1             ' PROGRAM STOPPED  ***** ')
6102         FORMAT(/,/,' ******** WARNING THIS MAP WAS WRITTEN WITH'
     1,' VERSION ',I4,' OF PROGRAM, BUT THIS IS VERSION ',I4,
     1 ' ********* ',/,/)
 603  FORMAT(' +++++MCGDST :  I/P UNIT = ',I3,'  O/P UNIT = ',I3,//)
           CALL EXIT
       ELSE
C
C SAVE ALL VARIABLES THAT ARE NOT TO BE OVERWRITTEN BY READ IN VALUES
C
             LTEMP1 = MAPIN
             LTEMP2 = MAPOUT
             LTEMP3 = LHIST
             LTEMP4 = LPLUTO
             ITEMP1 = MUNIN
             ITEMP2 = MUNOUT
             ITEMP3 = IVERS
             ATEMP1 = PTVDM
C
C FOR BACKWARDS COMPATIBILITY
C SET VARIABLES NOT IN PRE VERSION 1.05
C
             WMAX   = 0.0
             GAMTOT = 0.0
             LBREIT = .FALSE.
             CALL  MCGRBL  (MUNIN,IMCGC,LREC,ILENG,BUF)
             IF(WMAX.EQ.0.0)WMAX = 2*(EBEAM-ULMASS(7))
C
C RESTORE SAVED SETTINGS
C
             MAPIN  = LTEMP1
             MAPOUT = LTEMP2
             LHIST  = LTEMP3
             LPLUTO = LTEMP4
             MUNIN  = ITEMP1
             MUNOUT = ITEMP2
C
C IF PTVDM WAS SET ON CONTROL CARDS IT IS USED INSTEAD OF
C READ IN VALUE
C
             IF(ATEMP1.NE.0.0.AND.(PTVDM.NE.ATEMP1))THEN
                  WRITE(6,*)' '
                  WRITE(6,*)' DSTRED:     -  WARNING - '
                  WRITE(6,*)' '
                  WRITE(6,*)' Using value of Pt_VDM from card GVDM :',
     1                                                            ATEMP1
                  WRITE(6,*)' rather than value on MAP file of :',PTVDM
                  PTVDM  = ATEMP1
                  WRITE(6,*)' '
              ENDIF
             IF(IVERS.NE.ITEMP3)WRITE(6,6102)IVERS,ITEMP3
       ENDIF
c       WRITE(6,615)
c  615  FORMAT(1X,'=====  RESULTS FROM DISK')
c       WRITE(6,616)NDIM,CALLS,IT,ITMX,ND,NG
c  616  FORMAT(37H0INPUT PARAMETERS FOR SPRING :  NDIM=,I3
c     1 ,8H  NCALL=,F8.0/29X,6H   IT=,I5,7H  ITMX=,I5/29X
c     2 ,6H   ND=,I4,6H   NG=,I4)
C       WRITE(6,612) (ND2(I),I=1,2),AND2
C       WRITE(6,613) ND3(1),AND3(2),ND3(3),ND3(4),AND3(5)
      ELSE
       WRITE(6,6021)
6021   FORMAT(' ERROR DSTRED CALLED BUT MAPIN IS FALSE ')
      ENDIF
       RETURN
C----------------------------------------------------------------
C=============================
       ENTRY DSTWRT
C=============================
      IF(MAPOUT)THEN
C       CALL RNSAVE(irn)
       call rmarut(irn,idu1,idu2)
       WRITE(6,630)MUNOUT
630    FORMAT(/,/,/,'  WRITING OUT NEW MAP ON UNIT ',I3,/,/)
C       WRITE(6,618)NDIM,CALLS,IT,ITMX,ND,NG,IRN
C  618  FORMAT(37H0OUTPUT PARAMETERS FROM BASES:  NDIM=,I3
C     1 ,8H  NCALL=,F8.0/29X,6H   IT=,I5,7H  ITMX=,I5/29X
C     2 ,6H   ND=,I4,6H   NG=,I4,6H   RN=,I15)
       REWIND MUNOUT
       XSECT = AVGI
       XERR  = SDEV
       CALL  MCGWBL  (MUNOUT,ND1,LREC,7,BUF)
       CALL  MCGWBL  (MUNOUT,ND2,LREC,507,BUF)
       CALL  MCGWBL  (MUNOUT,ND3,LREC,34025,BUF)
       CALL  MCGWBL  (MUNOUT,ND4,LREC,17,BUF)
       CALL  MCGWBL  (MUNOUT,ND5,LREC,1130,BUF)
       CALL  MCGWBL  (MUNOUT,NP1,LREC,4737,BUF)
       CALL  MCGWBL  (MUNOUT,NP2,LREC,216,BUF)
       CALL  MCGWBL  (MUNOUT,NP3,LREC,7,BUF)
       CALL  MCGWBL  (MUNOUT,ND6,LREC,20,BUF)
       CALL  MCGWBL  (MUNOUT,LMCGCOM,LREC,1,BUF)
       CALL  MCGWBL  (MUNOUT,IMCGC,LREC,LMCGCOM,BUF)
       CALL  MCGWFL  (MUNOUT,BUF,LREC)
C      ELSE
C       WRITE(6,6022)
C6022   FORMAT(' ERROR - DSTWRT CALLED BUT MAPOUT IS FALSE',/,
C     1' NO MAP PRODUCED ')
C       RETURN
      ENDIF
       RETURN
      END
      SUBROUTINE  MCGRBL(IUNIT,IARRAY,LREC,LEN,BUF)
C
C READ IN IARRAY IN LREC LENGTH CHUNKS
C  The idea is to pack data into standard NATIVE file
C to save space
C
      INTEGER IUNIT,IARRAY(LEN),BUF(LREC-1),LEN
      INTEGER IPOINT
      SAVE    IPOINT
      DATA    IPOINT/0/
C
C READ IN THE FIRST RECORD
C
      IF(IPOINT.EQ.0)READ(IUNIT)J,(BUF(K),K=1,J)
      IF(LEN.GT.0)THEN
C
C LOOP OVER ARRAY TO BE FILLED
C
            DO 10 I = 1,LEN
                  IPOINT = IPOINT + 1
C
C IF BUFFER IS EXHAUSTED
C
                  IF(IPOINT.GT.LREC-1)THEN
C
C READ NEXT RECORD
C
                        READ(IUNIT)J,(BUF(K),K=1,J)
                        IPOINT = 1
                  ENDIF
C
C FILL ELEMENTS OF ARRAY
C
                  IARRAY(I) = BUF(IPOINT)
10          CONTINUE
      ENDIF
      RETURN
      END
      SUBROUTINE  MCGWBL(IUNIT,IARRAY,LREC,LEN,BUF)
C WRITE OUT IARRAY IN <= LREC LENGTH CHUNKS
      INTEGER IUNIT,IARRAY(LEN),BUF(LREC-1),LEN
      INTEGER IPOINT
      SAVE    IPOINT
      DATA    IPOINT/0/
      IF(LEN.GT.0)THEN
C
C LOOP OVER ARRAY BEING WRITTEN
C
            DO 10 I = 1,LEN
                  IPOINT = IPOINT + 1
C
C IF BUFFER IS FULL
C
                  IF(IPOINT.GT.LREC-1)THEN
C
C WRITE IT OUT, AND RESET POINTER TO
C BUFFER
                        WRITE(IUNIT)LREC-1,BUF
                        IPOINT = 1
                  ENDIF
                  BUF(IPOINT) = IARRAY(I)
10          CONTINUE
      ENDIF
      RETURN
      ENTRY MCGWFL(IUNIT,BUF,LREC)
C
C FLUSH BUFFER TO FILE
C
            WRITE(IUNIT)IPOINT,(BUF(J),J=1,IPOINT)
      RETURN
      END
      FUNCTION F(X)
C---------------------------------------------------------------
C! Function called by BASES/SPRING to calculate QED part of XSECT (PHOT0
C
C INPUT  parameters :
C                    X(10) array of up to 10 random variables
C                          their precise interpretation depends
C                          on process being generated
C CALLED BY - SPRING, BASES
C CALLS     -
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------
C
       DIMENSION X(10)
       LOGICAL FIRST,LKEEP
       SAVE    FIRST
       INTEGER    F2TYPE,PI0TYP
       PARAMETER (F2TYPE=110)
       PARAMETER (PI0TYP=23)
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
C-------------------------------------------------------------------
C COMMON DATA FOR WHOLE PROGRAM
C
C PI        PI RADIANS
C PIDEG     PI IN DEGREES ( 180)   RADDEG    CONVERT RADIANS TO DEGREES
C ME        MASS OF ELECTRON       ME2       ""        ""       SQUARED
C MU2       MASS OF GENERATED PARTICLE **2
C S         2 * EBEAM              EMP
C CONST     OVERALL CONSTANT FOR F(X) CALC
C
       COMMON / DATA  / PI,PIDEG,RADDEG,ME,ME2,MU2,S,EMP,CONST
       REAL ME,ME2,MU2
       COMMON / LABVA / EW,E3,E5,E6,E7,PW,P3,P5,P6,P7,GAM1,GAM2,
     *                  AL3,BE5,CTW,STW,CT3,ST3,CT5,ST5,CT6,ST6,
     *                  CT7,ST7,CP3,SP3,CP5,SP5,CP6,SP6,CP7,SP7,
     *                  W,W2
       DATA    FIRST/.TRUE./
C
       IF ( LQED ) THEN
        CALL FUNQED(X,FX)
       ELSEIF(LPSEUD)THEN
        CALL FUNPSE(X,FX)
       ELSEIF(LBREIT)THEN
        CALL FUNBWR(X,FX)
       ELSEIF(LVDM)THEN
        CALL FUNVDM(X,FX)
       ELSE
         IF(FIRST)WRITE(6,*)
     1   ' NO INTEGRAND FUNCTION REQUESTED  - STOP -'
         FIRST = .FALSE.
        F = 1.0
        CALL EXIT
        RETURN
       ENDIF
       FIRST = .FALSE.
       F = FX
      IF(F.GT.0)THEN
C
C--- apply geometrical cuts
C
             CALL MCGEOM(LKEEP)
             IF (LKEEP )THEN
C
C--- good event, so calc properties for histograms
C
                   CALL   PHOFIL
                   RETURN
              ENDIF
       ENDIF
C
C--- error or unsuccessful sampling
C
       F = 0.0
       CALL PHOZER
C
       RETURN
       END
      SUBROUTINE FUNQED(X,FX)
C---------------------------------------------------------------
C! Calculate e+e->e+e-l+l- ( VERMASSEREN ) (PHOT02)
C
C INPUT  parameters :
C                    X(10) array of up to 10 random variables
C                          their precise interpretation depends
C                          on process being generated
C OUTPUT parameters :
C                      FX REAL value of cross section for these
C                         values of X()
C CALLED BY - F
C CALLS     - EEMUMU,PERIPH
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------
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
C-------------------------------------------------------------------
C COMMON DATA FOR WHOLE PROGRAM
C
C PI        PI RADIANS
C PIDEG     PI IN DEGREES ( 180)   RADDEG    CONVERT RADIANS TO DEGREES
C ME        MASS OF ELECTRON       ME2       ""        ""       SQUARED
C MU2       MASS OF GENERATED PARTICLE **2
C S         2 * EBEAM              EMP
C CONST     OVERALL CONSTANT FOR F(X) CALC
C
       COMMON / DATA  / PI,PIDEG,RADDEG,ME,ME2,MU2,S,EMP,CONST
       REAL ME,ME2,MU2
       COMMON / LABVA / EW,E3,E5,E6,E7,PW,P3,P5,P6,P7,GAM1,GAM2,
     *                  AL3,BE5,CTW,STW,CT3,ST3,CT5,ST5,CT6,ST6,
     *                  CT7,ST7,CP3,SP3,CP5,SP5,CP6,SP6,CP7,SP7,
     *                  W,W2
       DIMENSION X(10)
       LOGICAL     FIRST,LKEEP
       CHARACTER*4 CHAINT,TYPE
       EXTERNAL CHAINT
       SAVE    FIRST
       DATA    FIRST/.TRUE./
C----------------------------------------------------------------
C        Q E D
C----------------------------------------------------------------
        IF(FIRST)THEN
          ALF    = 1.0 / 137.04
          CONST  = 19.733**2 * ALF**4 / ( 2.0*S*PI**3 )
C ( IN PHOT01 THIS WAS DONE IN DSTWR, NOW MOVED HERE  WHICH IS MORE
C   SENSIBLE !)
          CHARGE = ABS(LUCHGE(LUNTYP))
          CFACT = 1.0
          TYPE = CHAINT(KTYPE)
          IF(TYPE.EQ.'UD  ')THEN
           CFACT = 17.0/27.0
          ELSE IF(CHARGE.EQ.2)THEN
           CFACT = 16.0/27.0
          ELSE IF(CHARGE.EQ.1)THEN
           CFACT = 1.0/27.0
          ENDIF
          IF(CFACT.NE.1.0)WRITE(6,*)'  APPLYING COLOUR/CHARGE ',
     1 'FACTOR OF ',CFACT
          CONST = CONST*CFACT
          FIRST = .FALSE.
        ENDIF
C
C--- qed calculation
C
C AJF 21/2/2001 - added 8th random number
        X(8) = RNDM(dum)
        CALL EEMUMU(WMIN,WMAX,NERR,DJDX,X,0,
     1PI,ME,ME2,AMASS,MU2,EBEAM,S,ENP,CONST)
        IF ( NERR .NE. 0 ) THEN
          ISTATS(NERR) = ISTATS(NERR) + 1
          GOTO 999
        ENDIF
C
C--- throw away events with low Pt quarks
C
      IF(P7*ST7.LT.PTCUT.OR.P6*ST6.LT.PTCUT)GOTO 999
C
C--- good sampling
C
       FX = DJDX * CONST * PERIPH(DUM)
       IF(FX.GT.0)RETURN
999    FX = 0.0
       RETURN
       END
      SUBROUTINE FUNPSE(X,FX)
C---------------------------------------------------------------
C! Calculate e+e->e+e-X ( PSEUDOSCALAR RESONANCE ) (PHOT02)
C
C INPUT  parameters :
C                    X(10) array of up to 10 random variables
C                          their precise interpretation depends
C                          on process being generated
C OUTPUT parameters :
C                      FX REAL value of cross section for these
C                         values of X()
C CALLED BY - F
C CALLS     - LMFUN
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------
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
C-------------------------------------------------------------------
C COMMON DATA FOR WHOLE PROGRAM
C
C PI        PI RADIANS
C PIDEG     PI IN DEGREES ( 180)   RADDEG    CONVERT RADIANS TO DEGREES
C ME        MASS OF ELECTRON       ME2       ""        ""       SQUARED
C MU2       MASS OF GENERATED PARTICLE **2
C S         2 * EBEAM              EMP
C CONST     OVERALL CONSTANT FOR F(X) CALC
C
       COMMON / DATA  / PI,PIDEG,RADDEG,ME,ME2,MU2,S,EMP,CONST
       REAL ME,ME2,MU2
       COMMON / LABVA / EW,E3,E5,E6,E7,PW,P3,P5,P6,P7,GAM1,GAM2,
     *                  AL3,BE5,CTW,STW,CT3,ST3,CT5,ST5,CT6,ST6,
     *                  CT7,ST7,CP3,SP3,CP5,SP5,CP6,SP6,CP7,SP7,
     *                  W,W2
       DIMENSION X(10)
       LOGICAL FIRST,LKEEP
       DATA FIRST/.TRUE./
       SAVE    FIRST
C----------------------------------------------------------------
C        P S E U D O S C A L A R
C----------------------------------------------------------------
      IF(FIRST)THEN
          ALF    = 1.0 / 137.04
          CONST  =  ( ALF**2 )  /  ( 32.0 * PI**3 )
C
C FURTHER CONSTANT CONVERTS A PARTIAL WIDTH IN GEV INTO
C  A TOTAL E+ E- CROSS SECTION IN MILIBARNS
C-PSUEDOSCALAR - EXTRA FACTOR 2
C
          CONST2 = (4 /EBEAM) * ( (PI*19.733) /AMASS)**2
C
C-STANDARD...  CONST2 = (2 /EBEAM) * ( (PI*19.733) /AMASS)**2
C
          CONST  = CONST * CONST2
          ZS = AMASS/(2*EBEAM)
          FIRST = .FALSE.
      ENDIF
C
C--- qed calculation
C
       CALL LMFUN(NERR,X,DJDX,SIG,ZS,QQ1,QQ2,
     1PI,ME,ME2,AMASS,MU2,EBEAM,S,ENP,CONST)
       IF ( NERR .NE. 0 )   GOTO 901
C
C--- (2)  Q**2 dependence - rho formfactors
C RHO FORM FACTOR WEIGHTING  (.6 = RHOMASS**2)
C DUE TO DIFFERENT SIGN CONVENTIONS, NEGATE QQ1,QQ2
C
      QQQ1 = - QQ1
      QQQ2 = - QQ2
      SF2 = 1. - QQQ1/0.6
      SF3 = 1. - QQQ2/0.6
      SIG2 = 1./(SF2*SF2)
      SIG3 = 1./(SF3*SF3)
C
C  PSEUDOSCALAR COUPLING KINEMATICS
C  MAXIMUM VALUE (AT Q2=0) IS 0.5
C
      FNY = .25*(QQQ1-QQQ2)**2
      XMOE = .25*(MU2 - QQQ1-QQQ2)**2 - QQQ1*QQQ2
      QTIL2 = .25*(-MU2 + 2.*QQQ1+2.*QQQ2)
      SIG4 = (FNY/MU2 - QTIL2)/SQRT(XMOE)
C
C--- good sampling
C
       FX = DJDX * CONST * SIG *  SIG2 * SIG3 * SIG4
       IF(FX.GT.0)RETURN
901    FX = 0.0
       RETURN
       END
      SUBROUTINE FUNBWR(X,FX)
C---------------------------------------------------------------
C! Calculate e+e->e+e-X BREIT WIGNER RESONANCE (PHOT02)
C
C INPUT  parameters :
C                    X(10) array of up to 10 random variables
C                          their precise interpretation depends
C                          on process being generated
C OUTPUT parameters :
C                      FX REAL value of cross section for these
C                         values of X()
C CALLED BY - F
C CALLS     - MCGWID,LMFUN
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------
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
C-------------------------------------------------------------------
C COMMON DATA FOR WHOLE PROGRAM
C
C PI        PI RADIANS
C PIDEG     PI IN DEGREES ( 180)   RADDEG    CONVERT RADIANS TO DEGREES
C ME        MASS OF ELECTRON       ME2       ""        ""       SQUARED
C MU2       MASS OF GENERATED PARTICLE **2
C S         2 * EBEAM              EMP
C CONST     OVERALL CONSTANT FOR F(X) CALC
C
       COMMON / DATA  / PI,PIDEG,RADDEG,ME,ME2,MU2,S,EMP,CONST
       REAL ME,ME2,MU2
       INTEGER  PI0TYP
       PARAMETER(PI0TYP=111)
       COMMON / LABVA / EW,E3,E5,E6,E7,PW,P3,P5,P6,P7,GAM1,GAM2,
     *                  AL3,BE5,CTW,STW,CT3,ST3,CT5,ST5,CT6,ST6,
     *                  CT7,ST7,CP3,SP3,CP5,SP5,CP6,SP6,CP7,SP7,
     *                  W,W2
       CHARACTER*16 NAME1,NAME2,NAME3
       DIMENSION X(10)
       LOGICAL FIRST,LKEEP
       DATA FIRST/.TRUE./
       SAVE    FIRST
C----------------------------------------------------------------
C        BREIT WIGNER RESONANCE
C----------------------------------------------------------------
            IF(FIRST)THEN
              ALF    = 1.0 / 137.04
              CONST  =  ( ALF**2 )  /  ( 32.0 * PI**3 )
              AMASS2 = AMASS*AMASS
              AMGAM2 = AMASS2*GAMTOT*GAMTOT
C              SPIN   = 0.0
C              IF(ITYPE.EQ.F2TYPE)SPIN   = 2.0
              FERMI  = 1.0
C
C FMAS1, FMAS2 ARE THE MASSES OF THE PARTICLES TO WHICH RESONANCE DECAYS
C
C DEFAULT..
              FMAS1  = ULMASS(PI0TYP)
              FMAS2  = ULMASS(PI0TYP)
C LOOK UP DECAY PRODUCTS IN LUND ARRAY
C
                KC = LUCOMP(LUNTYP)
                ISTART = MDCY(KC,2)
                IEND   = MDCY(KC,2)+MDCY(KC,3)-1
 
C
C FIND THE FIRST CHANNEL THAT IS NOT TURNED OFF
C
                 DO 10 IDEC = ISTART,IEND
                    IF(MDME(IDEC,1).NE.0)THEN
C
C JUST TAKE THE FIRST TWO DECAY PRODUCTS
C  OK SO THIS IS NOT VERY CLEVER, IT'LL DO FOR MOST NOW
C
                        FMAS1 = ULMASS(KFDP(IDEC,1))
                        FMAS2 = ULMASS(KFDP(IDEC,2))
                        CALL LUNAME(KFDP(IDEC,1),NAME1)
                        CALL LUNAME(KFDP(IDEC,2),NAME2)
                        CALL LUNAME(LUNTYP,NAME3)
                        WRITE(6,*)' ',NAME3,' -> ',
     1                                NAME1,'  ',
     1                                NAME2
                       GOTO 11
                    ENDIF
10               CONTINUE
11             CONTINUE
C
C   FURTHER CONSTANT INCLUDES 8PI(2J+1) AND
C   CONVERTS A PARTIAL WIDTH IN GEV INTO
C   A TOTAL E+ E- CROSS SECTION IN MICROBARNS
C
              CONST2 = 8*PI*(2*SPIN+1)*19.733**2
              CONST  = CONST * CONST2
C
              ZC   = AMASS/EBEAM*0.5
              ZMIN = WMIN/EBEAM*0.5
              ZMAX = WMAX/EBEAM*0.5
              FIRST = .FALSE.
      ENDIF
C
C PICK A W (ZS) IN THE BREIT WIGNER DISTRIBUTION
C
      CALL MCGWID(X,PROB,ZMIN,ZMAX,GAMTOT,SPIN,FERMI,EBEAM,ZC,
     1                  FMAS1,FMAS2,ZS)
C
C CALCULATE THE LUMINOSITY FUNCTION
C
       CALL LMFUN(NERR,X,DJDX,SIG,ZS,QQ1,QQ2,
     1PI,ME,ME2,AMASS,MU2,EBEAM,S,ENP,CONST)
       IF ( NERR .NE. 0 ) GOTO 901
C
C--- (2)  Q**2 dependence - rho formfactors
C RHO FORM FACTOR WEIGHTING  (.6 = RHOMASS**2)
C DUE TO DIFFERENT SIGN CONVENTIONS, NEGATE QQ1,QQ2
C
      QQQ1 = - QQ1
      QQQ2 = - QQ2
      SF2 = 1. - QQQ1/0.6
      SF3 = 1. - QQQ2/0.6
      SIG2 = 1./(SF2*SF2)
      SIG3 = 1./(SF3*SF3)
C--- good sampling
       FX = DJDX * CONST * SIG * PROB * SIG2 * SIG3
       IF(FX.GT.0)RETURN
901    FX = 0.0
       RETURN
       END
      SUBROUTINE MCGWID(Y,PROB,ZMIN4,ZMAX4,GAM4,SPIN4,FERMI4,EBEAM4,ZC4,
     1                  FMA14,FMA24,ZS4)
C---------------------------------------------------------------
C! Calculate a Spin Dependant Breit Wigner width (PHOT02)
C  <purpose>
C
C INPUT  parameters :
C                          Y - array of random variables, number 5 is us
C                      ZMIN4 - Minimum value for Z
C                      ZMAX4 - Maximum value for Z
C                       GAM4 - Width of resonance
C                      SPIN4 - Spin of resonance
C                     FERMI4 - Size of resonance
C                     EBEAM4 - Beam energy
C                        ZC4 - Z of resonance central point
C                      FMA14 - Mass of one decay product
C                      FMA24 - Mass of other decay product
C
C OUTPUT parameters :
C                       PROB - value of the function
C                        ZS4 - Z chosen
C
C CALLED BY -  FUNBWR
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------
C
      IMPLICIT REAL*8 (A - H,O - Z)
      LOGICAL FIRST
C
      REAL*4 Y,PROB,ZMIN4,ZMAX4,GAM4,SPIN4,FERMI4,EBEAM4,ZC4,ZS4,FMA14,
     1       FMA24
C
      DIMENSION Y(10)
C
      DATA FIRST/.TRUE./
C                      REDEFINITION OF DOUBLE PRECISION FORTRAN FUNCTION
CBB   if arg and result are real*8 , generic sqrt is dsqrt
CBB      SQRT(XXX) = DSQRT(XXX)
C
C ON FIRST CALL CONVERT EXTERNAL REAL*4 VARIABLES TO REAL*8
C
      IF (FIRST)THEN
        E     = EBEAM4
        ZC    = ZC4
        WS    = 2.*ZC*E
        GAM0  = GAM4
        SPIN  = SPIN4
        FERMI = FERMI4 * 5.068
        SS    = 2.*SPIN + 1.
        WS2   = WS*WS
C-
C SPIN DEPENDENT WIDTH, CALCULATE NOMINAL MOMENTUM FOR DECAY PRODUCTS
C
        Q0 = (WS2 - (FMA14**2 + FMA24**2))**2 - (2.*FMA14*FMA24)**2
        Q0 = Q0/(4.*WS2)
        Q0 = SQRT(Q0)
C                             NOMINAL X FOR BARRIER PENETRATION FUNCTION
        XBPF0 = Q0 * FERMI
        BPF0 = 1.
        IF (SPIN .GT. 0.) BPF0 = 1. + XBPF0*XBPF0
        IF(SPIN.GT.1.) BPF0 = 9.+ 3.*XBPF0*XBPF0 +
     1                   XBPF0*XBPF0*XBPF0*XBPF0
C
        ZMIN  = ZMIN4
        ZMAX  = ZMAX4
        DZ    = ZMAX-ZMIN
C
        FIRST = .FALSE.
      ENDIF
C------------------------------------------ END OF REAL*4 TO REAL*8 CONV
C
C      CALL MAPX1(ZS,Y(5),ZMIN,ZMAX,DZ)
       ZS     = ZMIN+DZ*Y(5)
C
      ZS4   = ZS
C
      W     = 2.*E*ZS
      W2    = W*W
      DIF2  = WS2 - W2
      DIF2  = DIF2*DIF2
C                                     DECAY PRODUCT MOMENTUM
C
      Q1 = (W2 - (FMA14**2 + FMA24**2))**2 - (2.*FMA14*FMA24)**2
      Q1 = Q1/(4.*W2)
      Q1 = SQRT(Q1)
C
      GAM = GAM0*((Q1/Q0)**SS)
C
C BARRIER PENETRATION FACTOR
C
      XBPF = Q1 * FERMI
      BPF = 1.
      IF (SPIN .GT. 0.) BPF = 1. + XBPF*XBPF
      IF(SPIN.GT.1.) BPF = 9. + 3.*XBPF*XBPF + XBPF*XBPF*XBPF*XBPF
C
      XBRAT = BPF0/BPF
C
      GAM = GAM*XBRAT
C--
      GWS2  = GAM*GAM*WS2
      PROB  = GAM*DZ/(DIF2 + GWS2)
C
      RETURN
      END
      SUBROUTINE FUNVDM(X,FX)
C---------------------------------------------------------------
C! Calculate e+e->e+e-X VDM production (PHOT02)
C
C INPUT  parameters :
C                    X(10) array of up to 10 random variables
C                          their precise interpretation depends
C                          on process being generated
C OUTPUT parameters :
C                      FX REAL value of cross section for these
C                         values of X()
C CALLED BY - F
C CALLS     - WSAMPL,LMFUN
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------
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
C-------------------------------------------------------------------
C COMMON DATA FOR WHOLE PROGRAM
C
C PI        PI RADIANS
C PIDEG     PI IN DEGREES ( 180)   RADDEG    CONVERT RADIANS TO DEGREES
C ME        MASS OF ELECTRON       ME2       ""        ""       SQUARED
C MU2       MASS OF GENERATED PARTICLE **2
C S         2 * EBEAM              EMP
C CONST     OVERALL CONSTANT FOR F(X) CALC
C
       COMMON / DATA  / PI,PIDEG,RADDEG,ME,ME2,MU2,S,EMP,CONST
       REAL ME,ME2,MU2
       COMMON / LABVA / EW,E3,E5,E6,E7,PW,P3,P5,P6,P7,GAM1,GAM2,
     *                  AL3,BE5,CTW,STW,CT3,ST3,CT5,ST5,CT6,ST6,
     *                  CT7,ST7,CP3,SP3,CP5,SP5,CP6,SP6,CP7,SP7,
     *                  W,W2
       DIMENSION X(10)
       LOGICAL FIRST,LKEEP
       DATA    FIRST/.TRUE./
       SAVE    FIRST
C----------------------------------------------------------------
C        V D M
C----------------------------------------------------------------
       IF(FIRST)THEN
          ALF    = 1.0 / 137.04
          CONST  =  ( ALF**2 )  /  ( 32.0 * PI**3 )
          ZMIN   = WMIN / EBEAM * 0.5
          ZMAX   = 1.0
          IF(WMAX.GT.WMIN) ZMAX   = WMAX / EBEAM * 0.5
          IF(ZMAX.GT.1.0)ZMAX = 1.0
       ENDIF
C
C--- qed calculation
C
       IF(.NOT.LCONST)THEN
        CALL WSAMPL(X,PROB,ZMIN,ZMAX,W,ZS)
       ELSE
        PROB   = ZMAX-ZMIN
        ZS     = ZMIN+PROB*X(5)
        W      = 2*EBEAM*ZS
       ENDIF
        CALL LMFUN(NERR,X,DJDX,SIG,ZS,QQ1,QQ2,
     1  PI,ME,ME2,AMASS,MU2,EBEAM,S,ENP,CONST)
       IF ( NERR .NE. 0 ) THEN
         GOTO 901
      ENDIF
C
C--- calc probabilities for given W and Q2 dependencies
C--- (1)  W dependence
C
       IF ( LCONST ) THEN
          SIG1 = 1.0
       ELSE
          SIG1 = 1.0 / ( W + 1.0E-6 )
       ENDIF
C
C--- (2)  Q**2 dependence - rho formfactors
C
       IF ( (QQ1.LE.0.0) .OR. (QQ2.LE.0.0) ) THEN
          ISTATS(22) = ISTATS(22) + 1
          GOTO 901
       ENDIF
       IF ( (QQ1.GT.1.0E+8) .OR. (QQ2.GT.1.0E+8) ) THEN
          ISTATS(23) = ISTATS(23) + 1
          GOTO 901
       ENDIF
C--- (2a)  Q**2 dependence - rho formfactors 0<Q2<1 , constant for Q2>1
CCC       IF ( QQ1 .GE. 1.0 ) QQ1 = 1.
CCC       IF ( QQ2 .GE. 1.0 ) QQ2 = 1.
C--- (2b)  Q**2 dependence - rho formfactors for whole Q2 range
C---  RHO-PROPAGATOR FOR WHOLE Q**2-RANGE
CCC       SF2  = 1.0 + QQ1/0.6
CCC       SF3  = 1.0 + QQ2/0.6
CCC       SIG2 = 1.0 / (SF2*SF2)
CCC       SIG3 = 1.0 / (SF3*SF3)
C--- (2c)  Q**2 dependence - GVDM formfactors
       SIG2 = FGVDM(QQ1)
       SIG3 = FGVDM(QQ2)
C--- (2d)  NO Q**2 dependence
CCC       SIG2 = 1.0
CCC       SIG3 = 1.0
C--- good sampling
       FX = DJDX * CONST * PROB * SIG *  SIG1 * SIG2 * SIG3
       IF(FX.GT.0)RETURN
901    FX = 0.0
       RETURN
       END
      REAL FUNCTION FGVDM(QQ)
C---------------------------------------------------------------
C! Return the GVDM form factor (PHOT02)
C  <purpose>
C
C INPUT  parameters :
C                      QQ : Q squared of photon
C OUTPUT parameters :
C                   FGVDM : Value of form factor
C CALLED BY -  FUNVDM
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------
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
C-------------------------------------------------------------------
C COMMON DATA FOR WHOLE PROGRAM
C
C PI        PI RADIANS
C PIDEG     PI IN DEGREES ( 180)   RADDEG    CONVERT RADIANS TO DEGREES
C ME        MASS OF ELECTRON       ME2       ""        ""       SQUARED
C MU2       MASS OF GENERATED PARTICLE **2
C S         2 * EBEAM              EMP
C CONST     OVERALL CONSTANT FOR F(X) CALC
C
       COMMON / DATA  / PI,PIDEG,RADDEG,ME,ME2,MU2,S,EMP,CONST
       REAL ME,ME2,MU2
       COMMON / LABVA / EW,E3,E5,E6,E7,PW,P3,P5,P6,P7,GAM1,GAM2,
     *                  AL3,BE5,CTW,STW,CT3,ST3,CT5,ST5,CT6,ST6,
     *                  CT7,ST7,CP3,SP3,CP5,SP5,CP6,SP6,CP7,SP7,
     *                  W,W2
       DIMENSION X(10)
       LOGICAL FIRST,LKEEP
       DATA    FIRST/.TRUE./
       SAVE    FIRST
C
C---  CALCULATE THE GVDM-FF ACCORDING TO GINZBURG & SERBO,
C     PHYS. LETT. 109B(1982), NO.3, P.231FF, EQ.(2)
C     H.WRIEDT    25.11.82    11:45
C     LAST MOD    25.11.82    11:45
C
      DIMENSION R(3),XMQ(3)
      DATA R/0.65,0.08,0.05/,XMQ/0.591,0.612,1.04/
C
      FGVDM = 1.
      IF (QQ.EQ.0.) RETURN
      SUM = 0.
        DO 10 I = 1,3
        F = QQ/XMQ(I)
        T = R(I)*(1.+0.25*F)/((1.+F)*(1.+F))
   10   SUM = SUM + T
      T0 = 0.22/(1.+QQ/1.96)
      FGVDM = SUM + T0
      RETURN
      END
      SUBROUTINE LMFUN(NERR,Y,DJDX,SIG,ZS4,QQ1,QQ2,
     1 PI4,ME4,ME24,MU4,MU24,E4,S4,EMP4,CONST4)
C---------------------------------------------------------------
C! Calculate the Luminosity Function in gamma gamma event (PHOT02)
C
C INPUT  parameters :
C                      Y : random variables
C                    PI4 : PI !
C                    ME4 : electron mass
C                    MU4 : not used
C                    E4  : beam energy
C                    S4  : shat of final state
C                    ZS4 : Z of final state
C                  CONST4: constant
C
C
C OUTPUT parameters :
C                    NERR : error code 0 = success
C                                      1 = failure
C                    QQ1,QQ2 : Q**2 of photons
C                    DJDZ  : jacobian of integration
C                   Values in LABVA are also set.
C
C CALLED BY - FUNVDM,FUNPSE,FUNBWR
C CALLS     - MAPX1,CONV
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------
 
C      THIS PROGRAM CALCULATES DL/DZ OF LUMINOSITY FUNCTION
C      WRITTEN BY S.KAWABATA
C      MODIFIED BY H.WRIEDT     COPIED     12.03.81   19:55
C      MODIFIED BY H.WRIEDT     LAST MOD   14.03.81   16:15
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       DOUBLE PRECISION ME,ME2,MU,MU2
       REAL QQ1,QQ2
       REAL Y(10),DJDX,SIG
       REAL PI4,ME4,ME24,MU4,MU24,E4,S4,EMP4,CONST4
       REAL RPAI4,ZS4,Z24,XMIN4,TMIN4,TMAX4
       REAL ZMIN4,ZMAX4
      COMMON/LABVAR/EW,E3,E5,E6,E7,PW,P3,P5,P6,P7,GAM1,GAM2,AL3,BE5
     1,CTW,STW,CT3,ST3,CT5,ST5,CT6,ST6,CT7,ST7,CP3,SP3,CP5,SP5,CP6,SP6
     2,CP7,SP7,W,W2
       DATA NFL,XMAX / 1, 1./
C      SIN(XXX)    = DSIN(XXX)
C      COS(XXX)    = DCOS(XXX)
C      COTAN(XXX)  = DCOTAN(XXX)
C      ABS(XXX)    = DABS(XXX)
C      SQRT(XXX)   = DSQRT(XXX)
C      ALOG10(XXX) = DLOG10(XXX)
C      ARCOS(XXX)  = DARCOS(XXX)
      NERR  = 0
      IF(NFL.EQ.0) GO TO 10
      PI    =DBLE( PI4)
      ME    =DBLE( ME4)
      ME2   =DBLE( ME24)
      MU    =DBLE( MU4)
      MU2   =DBLE( MU24)
      E     =DBLE( E4)
      S     =DBLE( S4)
      EMP   =DBLE( EMP4)
      CONST =DBLE( CONST4)
C     RPAI  =DBLE( RPAI4)
      FME   = ME/E
      FME2  = FME*FME
      FMA   = (1.-FME)*(1.+FME)
      XMAX  = 1.D0-FME
      TMIN  = 1.0E-10
      TMAX   =DBLE( PI)
      E2    = E*E
      PB    = (E-ME)*(E+ME)
      PB    = SQRT(PB)
      PB    = E- ME2/(E+PB)
      RAMD  = S*(S-4.*ME2)
      RAMD  = 1./SQRT(RAMD)
      DJ    = 2.*PI
      NFL   = 0
  10  CONTINUE
      ZS    =DBLE( ZS4)
      Z2    = ZS*ZS
      XMIN  = (Z2+0.5*FME+0.5*FME2)/(1.D0-0.5*FME)
C            X1    : Y(1)
C            TH1   : Y(2)
C            TH2   : Y(3)
C            PH    : Y(4)
C==========> SAMPLE X1
C FOR CONSISTENCY WITH EEMUMU USE ALL DOUBLE PRECISION ARGUMENTS
C ( YTEMP - DOUBLE PRECISION, Y - REAL*4)
      YTEMP =DBLE( Y(1))
      CALL MAPX1(X1,YTEMP,XMIN,XMAX,DX1)
C==========> SAMPLE THETA 1
      YTEMP =DBLE( Y(2))
      CALL MAPX1(TH1,YTEMP,TMIN,TMAX,DT1)
      CT1   = COS(TH1)
      ST1   = SIN(TH1)
      DT1   = DT1*ST1
C++++++++++++++++++++++++++++++++
C     CT1   = 2.*Y(2)-1.
C     ST1   = (1.-CT1)*(1.+CT1)
C     ST1   = SQRT(ST1)
C     DT1   = 2.
C++++++++++++++++++++++++++++++++
C==========> SAMPLE THETA 2
      YTEMP =DBLE( Y(3))
      CALL MAPX1(TH2,YTEMP,TMIN,TMAX,DT2)
      CT2   = COS(TH2)
      ST2   = SIN(TH2)
      DT2   = DT2*ST2
C++++++++++++++++++++++++++++++++
C     CT2   = 2.*Y(3)-1.
C     ST2   = (1.-CT2)*(1.+CT2)
C     ST2   = SQRT(ST2)
C     DT2   = 2.
C++++++++++++++++++++++++++++++++
C==========> SAMPLE PHAI
      PH    = 2.*PI*DBLE(Y(4))
      CPH   = COS(PH)
C==========> DEFINE X2
      CT    = ST1*ST2*CPH-CT1*CT2
      IF(ABS(CT).GT.1.) GO TO 1000
      CTSQ  = CT*CT
      X1MM  = (1.-X1)
      X1MP  = (1.+X1)
      FAC1  = (X1MM+FME)*(X1MM-FME)
      FAC2  = 1.-X1+2.*Z2-FME2
      A     = FAC1*CTSQ - X1MP*X1MP
      B     = FAC1*CTSQ - X1MP*FAC2
      C     = FMA*FAC1*CTSQ - FAC2*FAC2
      D     = B*B - A*C
C              WRITE(6,8000) X1,CT1,CT2,PH
C8000 FORMAT(1X,'***** X1,CT1,CT2,PH =',4E14.4)
C              WRITE(6,8100) A,B,C,D
C8100 FORMAT(1X,'      A, B, C, D    =',4E14.4)
C-----------------------------------------
      IF(D.LT.0.) GO TO 1000
      PXX   = (1.-X1)**2-FME2
      IF(PXX.LT.0.) GO TO 1000
      PX1   = SQRT(PXX)*CT
      PY1   = X1-1.-2.*Z2+FME2
      SD    = SQRT(D)
      X2    = (B + SD)/A
      IF(X2.LT.XMIN.OR.X2.GT.XMAX) GO TO 500
      PXX   = (1.-X2)**2-FME2
      IF(PXX.LT.0.) GO TO 1000
      PXT   = SQRT(PXX)*PX1
      PYT   = X2+X1*X2+PY1
      IF(PXT*PYT.GE.0.) GO TO 600
  500 X2    = (B - SD)/A
      PXX   = (1.-X2)**2-FME2
      IF(PXX.LT.0.) GO TO 1000
      PXT   = SQRT(PXX)*PX1
      PYT   = X2+X1*X2+PY1
      IF(PXT*PYT.LT.0.) GO TO 1000
  600 CONTINUE
C              WRITE(6,9000) CT,X2,TH1,TH2
 9000 FORMAT(1X,'===>  CT,X2,TH1,TH2 =',4E14.4)
      IF(X2.LT.XMIN.OR.X2.GT.XMAX) GO TO 1000
C------------------------------
      E3    = E-E*X1
      P3    = (E3-ME)*(E3+ME)
      IF(P3.LT.0.) GO TO 1000
      P3    = SQRT(P3)
      P3    = E3- ME2/(E3+P3)
      CT3   = CT1
      ST3   = ST1
      CP3   = 1.
      SP3   = 0.
      E5    = E-E*X2
      P5    = (E5-ME)*(E5+ME)
      IF(P5.LT.0.) GO TO 1000
      P5    = SQRT(P5)
      P5    = E5- ME2/(E5+P5)
      CT5   = -CT2
      ST5   = ST2
      CP5   = CPH
      SP5   = SIN(PH)
C-----------------------
      EW    = 2.*E-E3-E5
      W2    = 4.*E2*Z2
      PW    = EW*EW-W2
      IF(PW.LT.0.) GO TO 1000
      PW    = SQRT(PW)
      PW    = EW-W2/(EW+PW)
      W     = SQRT(W2)
      PWX   = -(P3*ST3*CP3+P5*ST5*CP5)
      PWY   = -(P3*ST3*SP3+P5*ST5*SP5)
      PWZ   = -(P3*CT3+P5*CT5)
      CTW   = PWZ/PW
      IF(ABS(CTW).GT.1.) GO TO 1000
      STW   = SQRT((1.-CTW)*(1.+CTW))
      PWXY  = PW*STW
      IF(PWXY.NE.0.) GO TO 700
      CPW   = 1.D0
      SPW   = 0.D0
      GO TO 750
  700 CPW   = PWX/PWXY
      SPW   = PWY/PWXY
C-----------------------
  750 Q12   = -2.*E*E3+2.*PB*P3*CT3
      Q12   = Q12+2.*ME2
      QQ1 = -Q12
      IF(Q12.GE.0.) GO TO 1000
      Q22   = -2.*E*E5-2.*PB*P5*CT5
      Q22   = Q22+2.*ME2
      QQ2 = -Q22
      IF(Q22.GE.0.) GO TO 1000
      Q12 = -0.25*Q12/E2
      Q22 = -0.25*Q22/E2
CCCCCC-------------------
C     WRITE(6,9100) Q12,Q22
 9100 FORMAT(1X,'Q12 ,Q22  =',2E14.4)
C======================================================================
      XK = Z2 + Q12 + Q22
C     ODR1 = ALOG10(Q12)+ALOG10(Q22)
C     ODR2 = ALOG10(XK)*2.-8.
C     IF(ODR2.GT.ODR1) GO TO 100
      CH12 = XK*XK - 4.*Q12*Q22
C     GO TO 101
C 100 CH12 = XK*XK
  101 CH = SQRT(CH12)
CCCCCC-------------------
C     WRITE(6,9150) XK,CH12
 9150 FORMAT(1X,'XK,CH12   =',2E14.4)
      XMEQ1 = ME2/(E2*Q12)
      XMEQ2 = ME2/(E2*Q22)
      C1 = 1. - XMEQ1
      C2 = 1. - XMEQ2
      FR1 = XK - 2.*(X2 + Q22)
      FR2 = XK - 2.*(X1 + Q12)
      FR11 = FR1*FR1
      FR22 = FR2*FR2
CCCCCC-------------------
C     WRITE(6,9300) FR11,FR22
 9300 FORMAT(1X,'FR11,FR22 =',2E14.4)
      BRA1 = FR11/CH12 + C1
      BRA2 = FR22/CH12 + C2
CCCCCC-------------------
C     WRITE(6,9400) BRA1,BRA2
 9400 FORMAT(1X,'BRA1,BRA2 =',2E14.4)
      COT1   = P3*DT1/Q12
      COT2   = P5*DT2/Q22
CCCCCC-------------------
C     WRITE(6,9500) COT1,COT2
 9500 FORMAT(1X,'COT1,COT2 =',2E14.4)
      X2MM  = 1.-X2
      FAC   = SQRT(FAC1/((X2MM-FME)*(X2MM+FME)))
      FAC   = X1MP+X2MM*FAC*CT
      FACT  = 4.*ZS*RAMD/FAC
      FL1 = CH*COT1*BRA1
      FL2 = FACT*COT2*BRA2
C     ODR1 = ALOG10(ABS(FL1))+ALOG10(ABS(FL2))
CCCCCC-------------------
C     WRITE(6,9600) FL1,FL2,ODR1
 9600 FORMAT(1X,'FL1 ,FL2 ,ODR1 =',3E14.4)
C     IF(ODR1.GT.70.) GO TO 1000
      SIG = FL1*FL2
C======================================================================
      IF(SIG.LE.0.) GO TO 1000
      DJDX  = DX1*DJ
C              WRITE(6,9200) SIG,DJDX
9200  FORMAT(1X,'      SIG,DJDX      =',2E14.4)
      CALL CONV
      RETURN
 1000 NERR  = 1
      RETURN
      END
      SUBROUTINE MAPVAR(T,X,XMIN,XMAX,DX)
C---------------------------------------------------------------
C! MAP routine for integrating an exponential (PHOT02)
C
C INPUT  parameters : X,XMIN,XMAX
C
C OUTPUT parameters : T,DX
C
C CALLED BY - various
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------
      ENTRY      MAPX1 (T,X,XMIN,XMAX,DX)
      ENTRY      MAPS2 (T,X,XMIN,XMAX,DX)
      ENTRY      MAPW2 (T,X,XMIN,XMAX,DX)
      IMPLICIT DOUBLE PRECISION (A-Z)
      Y     = XMAX/XMIN
      T     = XMIN*Y**X
      DX    = T*LOG(Y)
      RETURN
      END
      SUBROUTINE MAPT1(T,X,XMIN,XMAX,DX)
      ENTRY      MAPT2(T,X,XMIN,XMAX,DX)
C---------------------------------------------------------------
C! MAP routine for integrating an exponential (PHOT02)
C
C INPUT  parameters : X,XMIN,XMAX
C
C OUTPUT parameters : T,DX
C
C CALLED BY - INITIA
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------
 
      IMPLICIT DOUBLE PRECISION (A-Z)
      Y     = XMAX/XMIN
      T     = XMIN*Y**X
      DX    = -T*LOG(Y)
      RETURN
      END
      SUBROUTINE MCGEOM(LKEEP)
C---------------------------------------------------------------
C! Apply gemoterical cuts to electrons in gamma gamma event (PHOT02)
C INPUT  parameters : ( looks at LABVA COMMON )
C
C OUTPUT parameters :
C                LOGICAL :LKEEP  whether to keep event
C
C CALLED BY - F(X)
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------
       COMMON / LABVA / EW,E3,E5,E6,E7,PW,P3,P5,P6,P7,GAM1,GAM2,
     *                  AL3,BE5,CTW,STW,CT3,ST3,CT5,ST5,CT6,ST6,
     *                  CT7,ST7,CP3,SP3,CP5,SP5,CP6,SP6,CP7,SP7,
     *                  W,W2
       LOGICAL LKEEP
       LOGICAL LFIRST
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
       SAVE LFIRST
       DATA    LFIRST  / .TRUE. /
C----------------------------------------------------------------
       IF ( LFIRST ) THEN
          CTMAX = COS( THMIN )
          CTMIN = COS( THMAX )
          IF(ICOND.NE.0)
     1    WRITE(6,604) ICOND,THMIN,THMAX
          LFIRST = .FALSE.
          IGEOM  = 1
       ENDIF
  601  FORMAT(///,' +++++MCGEOM :  GEOMETRIC CUTS           +++++')
  602  FORMAT(  /,' +++++MCGEOM :  STORED CUTS WILL BE USED +++++')
  603  FORMAT(  /,' +++++MCGEOM :  CUTDAT WILL BE CALLED    +++++')
  604  FORMAT(///,
 
     *  /,1X,9('=========='),
     $  /,10X,'ICOND(DETECTOR CONDITION) =',I3,
     1  /,15X,'ICOND = 0 ; NO GEOMETRICAL CUTS',
     2  /,15X,'ICOND = 1 ; AT LEAST SINGLE TAG',
     3  /,15X,'ICOND = 2 ; DOUBLE TAG',
     9  /,10X,'GEOMETRICAL CONSTANTS :',
     1  /,15X,'TAGG THETA MIN & MAX (RADIANS)  ',2(5X,F8.4),
     4  /, 1X,9('=========='),/)
 1100  CONTINUE
C--- apply cuts
       LKEEP = .FALSE.
       CTHE  = CT3
       CTHP  = -1.0 * CT5
C--- count the number of small (NTAG) angle tags
C--- small angle :  thmin <= thtag <= thmax
       IF ( ICOND .EQ. 0 ) THEN
C---                                  0 : no cuts
          LKEEP = .TRUE.
          RETURN
       ELSE
          NTAG  = 0
          IF ( (CTHE.GE.CTMIN) .AND. (CTHE.LE.CTMAX) ) NTAG = NTAG + 1
          IF ( (CTHP.GE.CTMIN) .AND. (CTHP.LE.CTMAX) ) NTAG = NTAG + 1
       ENDIF
C--- apply various cuts
       IF     ( ICOND .EQ. 1 ) THEN
C---                                  1 : single s.a. tag
          IF ( NTAG .GT. 0 ) LKEEP = .TRUE.
       ELSEIF ( ICOND .EQ. 2 ) THEN
C---                                  2 : double s.a. tag
          IF ( NTAG .GE. 2 ) LKEEP = .TRUE.
       ELSE
C---                                 >2 : no cuts
          LKEEP = .TRUE.
       ENDIF
       RETURN
       END
      SUBROUTINE PLUTOQ(PTSCAL,IER)
C---------------------------------------------------------------
C! Turn gamma gamma final state into two quarks (PLUTO STYLE) (PHOT02)
C
C INPUT  - uses electron vectors from LUND array
C
C OUTPUT parameters :
C                  PTSCAL - PT OF INTERACTION, FOR USE IN CALL
C                               TO LUSHOW
C                    IER  return code
C                            0 success
C                            1 failure
C
C CALLED BY - ASKUSE
C CALLS     - HBOOK1,HFILL,LOREN4,THQRK(function)
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
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
C-------------------------------------------------------------------
C COMMON DATA FOR WHOLE PROGRAM
C
C PI        PI RADIANS
C PIDEG     PI IN DEGREES ( 180)   RADDEG    CONVERT RADIANS TO DEGREES
C ME        MASS OF ELECTRON       ME2       ""        ""       SQUARED
C MU2       MASS OF GENERATED PARTICLE **2
C S         2 * EBEAM              EMP
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
       REAL ELE(4),POS(4),ELE1(4),POS1(4)
       REAL QRK1(4),QRK2(4),QRK1P(4),QRK2P(4)
       REAL GAM1(4),GAM2(4),GAM1P(4),GAM2P(4)
       REAL LAB(4),CMS(4)
       INTEGER IER
       LOGICAL LFIRST
       SAVE    LFIRST
       DATA    LFIRST  /.TRUE./
C--- first call to PLUTOQ - book histograms
       IF ( LFIRST ) THEN
          LFIRST = .FALSE.
          IF(LHIST)THEN
            CALL HBOOK1(10131,'PLUTOQ - THETAQ$',50,0.0,PI,0.)
            CALL HBOOK1(10132,'PLUTOQ - THETAG$',50,0.0,PI,0.)
          ENDIF
       ENDIF
       IER = 0
C--- get beam electron and positron vectors
       ELE(1) = P7LU(1,1)
       ELE(2) = P7LU(1,2)
       ELE(3) = P7LU(1,3)
       ELE(4) = P7LU(1,4)
       POS(1) = P7LU(2,1)
       POS(2) = P7LU(2,2)
       POS(3) = P7LU(2,3)
       POS(4) = P7LU(2,4)
C--- get scattered electron and positron vectors
       ELE1(1) = P7LU(3,1)
       ELE1(2) = P7LU(3,2)
       ELE1(3) = P7LU(3,3)
       ELE1(4) = P7LU(3,4)
       POS1(1) = P7LU(4,1)
       POS1(2) = P7LU(4,2)
       POS1(3) = P7LU(4,3)
       POS1(4) = P7LU(4,4)
C--- calc gamma vectors
       GAM1(1) = ELE(1) - ELE1(1)
       GAM1(2) = ELE(2) - ELE1(2)
       GAM1(3) = ELE(3) - ELE1(3)
       GAM1(4) = ELE(4) - ELE1(4)
       GAM2(1) = POS(1) - POS1(1)
       GAM2(2) = POS(2) - POS1(2)
       GAM2(3) = POS(3) - POS1(3)
       GAM2(4) = POS(4) - POS1(4)
C--- calculate gamma-gamma cms
       CMS(1) = GAM1(1) + GAM2(1)
       CMS(2) = GAM1(2) + GAM2(2)
       CMS(3) = GAM1(3) + GAM2(3)
       CMS(4) = GAM1(4) + GAM2(4)
       LAB(1) = - CMS(1)
       LAB(2) = - CMS(2)
       LAB(3) = - CMS(3)
       LAB(4) =   CMS(4)
C--- boost gammas into CMS
C
C-- for w close to zero , rounding errors may sometimes
C   produce -ve W, not good so avoid it
C
       WTEST = CMS(4)**2-CMS(1)**2-CMS(2)**2-CMS(3)**2
       IF(WTEST.LT.0.0000001)THEN
            CMS(4)=SQRT(CMS(1)**2+CMS(2)**2+CMS(3)**2)
            DO 12 IJK = 1,4
                  GAM1P(IJK) = GAM1(IJK)
                  GAM2P(IJK) = GAM2(IJK)
12          CONTINUE
       ELSE
             CALL LOREN4(CMS,GAM1,GAM1P)
             CALL LOREN4(CMS,GAM2,GAM2P)
       ENDIF
C       IF ( LDEBUG ) THEN
C          WRITE(6,601)ELE,ELE1,POS,POS1,
C     *                GAM1,GAM2,GAM1P,GAM2P,
C     *                CMS,LAB,
C     *                QRK1,QRK2,QRK1P,QRK2P
C       ENDIF
C--- calculate W_true and give energy and momentum to quarks
       GGW2  = CMS(4)**2 - CMS(1)**2 - CMS(2)**2 - CMS(3)**2
C
C CHECK FOR LEGAL W ( >= 2*AMASS )
C
       IF(GGW2.LT.4*AMASS*AMASS)THEN
            IER = 1
            RETURN
       ENDIF
       PQRK2 = 0.25 * GGW2 - AMASS*AMASS
       PQRK  = SQRT( PQRK2 )
C       IF ( LDEBUG ) THEN
C          GGW2A = (GAM1P(4) + GAM2P(4))**2
C     *          - (GAM1P(1) + GAM2P(1))**2
C     *          - (GAM1P(2) + GAM2P(2))**2
C     *          - (GAM1P(3) + GAM2P(3))**2
C          GGW2B = (GAM1(4) + GAM2(4))**2
C    *          - (GAM1(1) + GAM2(1))**2
C     *          - (GAM1(2) + GAM2(2))**2
C     *          - (GAM1(3) + GAM2(3))**2
C          WRITE(6,*)'W2 FROM CMS,GAM1P,GAM1 = ',GGW2,GGW2A,GGW2B
C          QENRGY = 0.5 * SQRT( GGW2 )
C          QMOM   = SQRT(QENRGY**2 - QMASS**2)
C          WRITE(6,*)'QM,QE,QM,PQ2,PQ',QMASS,QENRGY,QMOM,PQRK2,PQRK
C       ENDIF
C                                     C
C--- sample exp and set quark direction w.r.t. gamma-gamma axis
       THETAQ = THQRK(PQRK,PTVDM) + 1.0E-10
C       THETAQ = 0.0
       PHIQ = PI*( 1. - 2.*RN( 0. ) )
       SPHQ = SIN( PHIQ )
       CPHQ = COS( PHIQ )
C--- rotate quark 4-vector into gamma-gamma cms coordinate system
       PTQ  = PQRK * SIN( THETAQ )
       PTSCAL = PTQ
       PXQ  = PTQ * COS( PHIQ )
       PYQ  = PTQ * SIN( PHIQ )
       PZQ  = PQRK * COS( THETAQ )
       PTG2 = GAM1P(1)**2 + GAM1P(2)**2
       PG2  = PTG2 + GAM1P(3)**2
       PTG  = SQRT( PTG2 + 1.0E-20 )
       PG   = SQRT( PG2  + 1.0E-20 )
       CTHG = GAM1P(3) / PG
       STHG = PTG / PG
       CPHG = GAM1P(1) / PTG
       SPHG = GAM1P(2) / PTG
       QRK1P(1) = CTHG*CPHG*PXQ - SPHG*PYQ + STHG*CPHG*PZQ
       QRK1P(2) = CTHG*SPHG*PXQ + CPHG*PYQ + STHG*SPHG*PZQ
       QRK1P(3) =    -STHG*PXQ           + CTHG*PZQ
       QRK1P(4) = SQRT(PQRK2 + AMASS*AMASS)
       QRK2P(1) = - QRK1P(1)
       QRK2P(2) = - QRK1P(2)
       QRK2P(3) = - QRK1P(3)
       QRK2P(4) = QRK1P(4)
C--- boost quarks back into LAB
       IF(WTEST.GT.0.0000001)THEN
             CALL LOREN4(LAB,QRK1P,QRK1)
             CALL LOREN4(LAB,QRK2P,QRK2)
       ENDIF
C       IF ( LDEBUG ) THEN
C          CALL LOREN4(LAB,GAM1P,GAM1)
C          WRITE(6,603)GAM1
C          WRITE(6,602)PQRK2,PQRK,THETAQ,PHIQ,PLQ,PTQ,
C     *                QRK1P,QRK2P,QRK1,QRK2
C       ENDIF
C--- set LUND quark vectors
       P7LU(5,1) = QRK1(1)
       P7LU(5,2) = QRK1(2)
       P7LU(5,3) = QRK1(3)
       P7LU(5,4) = QRK1(4)
       P7LU(5,5) = AMASS
       K7LU(5,1) = 2
       K7LU(5,2) = LUNTYP
       P7LU(6,1) = QRK2(1)
       P7LU(6,2) = QRK2(2)
       P7LU(6,3) = QRK2(3)
       P7LU(6,4) = QRK2(4)
       P7LU(6,5) = AMASS
       K7LU(6,1) = 1
       K7LU(6,2) = -K7LU(5,2)
       N7lU = 6
       THETAG = ASIN(STHG)
       IF(LHIST)THEN
        CALL HFILL(10131,THETAQ,0.0,1.0)
        CALL HFILL(10132,THETAG,0.0,1.0)
       ENDIF
       RETURN
  601  FORMAT(//,' +++++PLUTOQ : DEBUG START',/,
     *        ' INITIAL VECTORS :',/,
     *        '    ELE  (LAB) = ',4(F7.3,1X),
     *        '    ELE1 (LAB) = ',4(F7.3,1X),/,
     *        '    POS  (LAB) = ',4(F7.3,1X),
     *        '    POS1 (LAB) = ',4(F7.3,1X),/,
     *        '    GAM1 (LAB) = ',4(F7.3,1X),
     *        '    GAM2 (LAB) = ',4(F7.3,1X),/,
     *        '    GAM1 (CMS) = ',4(F7.3,1X),
     *        '    GAM2 (CMS) = ',4(F7.3,1X),/,
     *        '    CMS        = ',4(F7.3,1X),
     *        '    LAB        = ',4(F7.3,1X),/,
     *        '    QRK1 (LAB) = ',4(F7.3,1X),
     *        '    QRK2 (LAB) = ',4(F7.3,1X),/,
     *        '    QRK1 (CMS) = ',4(F7.3,1X),
     *        '    QRK2 (CMS) = ',4(F7.3,1X)   )
  602  FORMAT(' AFTER SETTING THETA-QUARK :',/,
     *        '    P2,P,THQRK,PHIQ,PLQ,PTQ = ',6(F7.3,1X),/,
     *        '    QRK1 (CMS) = ',4(F7.3,1X),
     *        '    QRK2 (CMS) = ',4(F7.3,1X),/,
     *        '    QRK1 (LAB) = ',4(F7.3,1X),
     *        '    QRK2 (LAB) = ',4(F7.3,1X),/,
     *        ' +++++PLUTOQ : DEBUG END.',//)
  603  FORMAT(' LORENTZ TEST - GAM1 BACK INTO LAB = ',4(F7.3,1X))
       END
       REAL FUNCTION THQRK(PQUARK,A)
C---------------------------------------------------------------
C! Choose a THETA for the quark according to exp(-A*pt**2) (PHOT02)
C  <purpose>
C
C INPUT  parameters :
C                     PQUARK - momentum of quark
C                      A     - slope parameter
C
C OUTPUT parameters :
C                      THQRK : Theta for quark
C
C CALLED BY -         PLUTOQ
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------
C
C ----------------------------------------------------------------------
C     CALCULATES THETA FOR VDM ACCORDING TO DN/D(PT**2)=EXP(-A.PT**2)
C     AS SUGGESTED BY IOS 21/11/82
C     CHANGED TO IMPROVE EFFICIENCY A.J.FINCH 22/6/88
C     protection added for too large argument in YMAX B.Bloch april 89
C ----------------------------------------------------------------------
       PARAMETER ( BIG = 90.)
       DATA PI / 3.1415927 /
       DATA IERR / 0 /
C ------------------------------------- FIRST CALCULATE PT**2
       IF ( PQUARK .LE. 0.0 ) THEN
          IERR = IERR + 1
          IF ( IERR .LT. 20 )
     *       WRITE(6,*)'+++++THQRK : ERROR ! PQUARK,A = ',PQUARK,A
          IF ( IERR .EQ. 20 )
     *       WRITE(6,*)'+++++THQRK : NO MORE ERROR MESSAGES !'
          THQRK = 0.0
       ENDIF
       PT2MAX = PQUARK**2
       IF (A*PT2MAX.GT.BIG) THEN
         YMAX =0.
       ELSE
         YMAX = EXP(-A*PT2MAX)
       ENDIF
    1  CONTINUE
          Y =  RN(X)*(1-YMAX)
          PT2    = -1.0 / A   *   ALOG( 1 - Y )
          IF ( PT2 .LT. 0.0 ) PT2 = 0.0
          IF ( PT2 .GT. PT2MAX ) GOTO 1
C ------------------------------------- HENCE THETA
       SINTH = SQRT( PT2 / PT2MAX )
       IF ( SINTH .GT. 1.0 ) SINTH = 1.0
       THQRK = ASIN( SINTH )
       IF ( RN(X) .GT. 0.5 ) THQRK = PI - THQRK
       RETURN
       END
      SUBROUTINE WSAMPL(X,PROB,ZMIN,ZMAX,W,ZS4)
C---------------------------------------------------------------
C! Pick a value for W of final state (PHOT02)
C
C INPUT  parameters :
C                      X array of random variables
C                         ( 5th one is used here )
C                      ZMIN,ZMAX range of Z
C
C OUTPUT parameters :
C                      ZS4 : Z of final state
C                        W : W of final state
C                      PROB : jacobian for integration.
C
C CALLED BY -     FUNVDM
C CALLS     -     MAPVAR
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------
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
        DOUBLE PRECISION ZS,DZ,ZMIN8,ZMAX8,X8
       DIMENSION X(10)
       ZMIN8 =DBLE( ZMIN)
       ZMAX8 =DBLE( ZMAX)
       X8    =DBLE( X(5))
       CALL MAPVAR(ZS,X8,ZMIN8,ZMAX8,DZ)
       ZS4   =SNGL( ZS)
       W     = 2.*EBEAM*ZS
       PROB  =SNGL( DZ)
       RETURN
       END
      SUBROUTINE DOLUPS(QMAX)
C---------------------------------------------------------------
C! Interface to LUND Parton Shower (PHOT02)
C  CALLS PARTON SHOWER IN THE CASE WHERE THERE ARE TWO
C   QUARKS TO DECAY
C
C INPUT  parameters :
C                     QMAX - SCALE FOR THE SHOWER
C
C CALLED BY -     ASKUSE
C CALLS     -     LUJOIN,LUSHOW
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------
C DO LUnd Parton Shower on quarks...
C
       INTEGER IJOIN1(2),IJOIN2(2),IJOIN3(2)
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
       DATA IJOIN1/5,6/
       DATA IJOIN2/5,7/
       DATA IJOIN3/6,8/
C
C THERE ARE TWO POSSIBLE CONFIGURATIONS , EITHER
C  TWO QUARKS IN LINES 5 AND 6
C OR FOUR IN LINES 5 6 7 8
C IN WHICH CASE LINES 5 AND 6 ARE ONE 'RHO', 7 AND 8 ARE THE OTHER
C
      IF(N7LU.EQ.6)THEN
            CALL LUJOIN(2,IJOIN1)
            CALL LUSHOW(5,6,QMAX)
      ELSE
            CALL LUJOIN(2,IJOIN2)
            CALL LUJOIN(2,IJOIN3)
            CALL LUSHOW(5,7,QMAX)
            CALL LUSHOW(6,8,QMAX)
      ENDIF
         RETURN
         END
      SUBROUTINE MCGRHO(PTSCAL,IER)
C---------------------------------------------------------------
C! Convert gamma gamma CMS into 4 quarks 'rho rho ' model(PHOT02)
C
C INPUT  parameters :
C
C OUTPUT parameters :
C                  PTSCAL - PT OF INTERACTION, FOR USE IN CALL
C                               TO LUSHOW
C                    IER  - ERROR CODE
C                            0 SUCCESS
C                            1 FAILURE
C CALLED BY -       ASKUSE
C CALLS     -       HBOOK1,HFILL,LOREN4,MCGRHX,MCFPTQ
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
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
C-------------------------------------------------------------------
C COMMON DATA FOR WHOLE PROGRAM
C
C PI        PI RADIANS
C PIDEG     PI IN DEGREES ( 180)   RADDEG    CONVERT RADIANS TO DEGREES
C ME        MASS OF ELECTRON       ME2       ""        ""       SQUARED
C MU2       MASS OF GENERATED PARTICLE **2
C S         2 * EBEAM              EMP
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
       REAL ELE(4),POS(4),ELE1(4),POS1(4)
       REAL QRK1(4),QRK2(4),QRK1P(4),QRK2P(4)
       REAL GAM1(4),GAM2(4),GAM1P(4),GAM2P(4)
       REAL LAB(4),CMS(4),PTOT(4),PTOT2(4)
       REAL PXQ(4),PYQ(4),PZQ(4),EN(4)
       REAL      MCFPTQ,MCGRHX
       REAL XQRK(4),PT(4),PLQRK(4)
       INTEGER IER
       INTEGER ILIST(4)
       LOGICAL LFIRST
       SAVE    LFIRST,NEV
       DATA    NEV/0/,ILIST/5,6,7,8/
C       DATA    NEV/0/,ILIST/5,7,9,11/
       DATA    LFIRST  /.TRUE./
C----------------------------------------------------------------
       IF(LFIRST)THEN
C--- first call to MCGRHO
           LFIRST = .FALSE.
           IF(LHIST)THEN
           CALL HBOOK1(300,' PT OF QUARK IN MCGRHO '
     1,100,0.0,5.0,0.)
           CALL HBOOK1(301,' X OF QUARK IN MCGRHO '
     1,100,0.0,1.0,0.)
           CALL HBOOK1(302,' PT OF 4th QUARK IN MCGRHO '
     1,100,0.0,5.0,0.)
        ENDIF
        ENDIF
       IER = 0
C--- get beam electron and positron vectors
       ELE(1) = P7LU(1,1)
       ELE(2) = P7LU(1,2)
       ELE(3) = P7LU(1,3)
       ELE(4) = P7LU(1,4)
       POS(1) = P7LU(2,1)
       POS(2) = P7LU(2,2)
       POS(3) = P7LU(2,3)
       POS(4) = P7LU(2,4)
C--- get scattered electron and positron vectors
       ELE1(1) = P7LU(3,1)
       ELE1(2) = P7LU(3,2)
       ELE1(3) = P7LU(3,3)
       ELE1(4) = P7LU(3,4)
       POS1(1) = P7LU(4,1)
       POS1(2) = P7LU(4,2)
       POS1(3) = P7LU(4,3)
       POS1(4) = P7LU(4,4)
C--- calc gamma vectors
       GAM1(1) = ELE(1) - ELE1(1)
       GAM1(2) = ELE(2) - ELE1(2)
       GAM1(3) = ELE(3) - ELE1(3)
       GAM1(4) = ELE(4) - ELE1(4)
       GAM2(1) = POS(1) - POS1(1)
       GAM2(2) = POS(2) - POS1(2)
       GAM2(3) = POS(3) - POS1(3)
       GAM2(4) = POS(4) - POS1(4)
C--- calculate gamma-gamma cms
       CMS(1) = GAM1(1) + GAM2(1)
       CMS(2) = GAM1(2) + GAM2(2)
       CMS(3) = GAM1(3) + GAM2(3)
       CMS(4) = GAM1(4) + GAM2(4)
       LAB(1) = - CMS(1)
       LAB(2) = - CMS(2)
       LAB(3) = - CMS(3)
       LAB(4) =   CMS(4)
C--- boost gammas into CMS
C
C-- for w close to zero , rounding errors may sometimes
C   produce -ve W, not good so avoid it
C
       WTEST = CMS(4)**2-CMS(1)**2-CMS(2)**2-CMS(3)**2
       IF(WTEST.LT.0.0000001)THEN
            CMS(4)=SQRT(CMS(1)**2+CMS(2)**2+CMS(3)**2)
            DO 12 IJK = 1,4
                  GAM1P(IJK) = GAM1(IJK)
                  GAM2P(IJK) = GAM2(IJK)
12          CONTINUE
       ELSE
             CALL LOREN4(CMS,GAM1,GAM1P)
             CALL LOREN4(CMS,GAM2,GAM2P)
       ENDIF
       GGW2  = CMS(4)**2 - CMS(1)**2 - CMS(2)**2 - CMS(3)**2
C
C CHECK FOR LEGAL W ( >= 4*AMASS )
C
       IF(GGW2.LT.16*AMASS*AMASS)THEN
            IER = 1
            RETURN
       ENDIF
       GGW = SQRT(GGW2)
C       IF(GGW.LT.2.5)GOTO 2000
C
C TOTAL MOMENTUM AVAILABLE TO ONE RHO
C IS SQRT(W/2**2-2M**2)
       ERHO = 0.5*SQRT(GGW2)
       PQRK2 = 0.25 * GGW2 - 4*AMASS*AMASS
       PQRK  = SQRT( PQRK2 )
      PTOT2(1) = 0.0
      PTOT2(2) = 0.0
      PTOT2(3) = 0.0
      PTOT2(4) = 0.0
      SUMPZ = 0.0
      IQUARK = 0
C--- CHOOSE X FOR THIS QUARK
       X  = MCGRHX(DUMMY)
C       X = RN(DUMMY)
       XQRK(1) = X
       XQRK(2) = 1-X
       IF(LHIST)THEN
       CALL HFILL(301,XQRK(1),0.0,1.0)
       CALL HFILL(301,XQRK(2),0.0,1.0)
       ENDIF
       X  = MCGRHX(DUMMY)
C       X = RN(DUMMY)
       XQRK(3) = X
       XQRK(4) = 1-X
       IF(LHIST)THEN
       CALL HFILL(301,XQRK(3),0.0,1.0)
       CALL HFILL(301,XQRK(4),0.0,1.0)
       ENDIF
C
C CHOOSE THE TRANSVERSE COMPONENTS
C
       PXTOT = 0
       PYTOT =0
       DO 1 I = 1,4
C--- CHOOSE PT FOR ALL QUARKS
C   fix by Alex 28-aug-1996
        PT(I) = MCFPTQ(0.5*PQRK,PTVDM)
        PHIQ = PI*( 1. - 2.*RN( 0. ) )
        PXQ(I)  = PT(I) * COS( PHIQ )
        PYQ(I)  = PT(I) * SIN( PHIQ )
        PXTOT = PXTOT + PXQ(I)
        PYTOT = PYTOT + PYQ(I)
 1     CONTINUE
       PTSCAL = 0.0
       DO 2 I = 1,4
           PXQ(I) = PXQ(I) - PXTOT/4
           PYQ(I) = PYQ(I) - PYTOT/4
           PT(I)  = SQRT(PXQ(I)**2+PYQ(I)**2)
           PTSCAL = MAX(PT(I),PTSCAL)
           IF(LHIST)CALL HFILL(300,PT(I),0.0,1.0)
2      CONTINUE
C
C CALC REMAINING MOMENTUM ALONG Z FOR EACH QUARK PAIR
C ( -VE SIGN FOR 3 AND 4 )
C
       PLQRK(1) = SQRT( PQRK**2 - PT(1)**2 - PT(2)**2)
       PLQRK(2) = PLQRK(1)
       PLQRK(3) = -SQRT( PQRK**2 - PT(3)**2 - PT(4)**2)
       PLQRK(4) = PLQRK(3)
C
C FILL IN PL , AND CALC E FOR EACH QUARK
C
      DO 3 I = 1, 4
        PZQ(I)  = XQRK(I)*PLQRK(I)
       EN(I)   = SQRT(PXQ(I)**2+PYQ(I)**2+PZQ(I)**2+
     1                     AMASS**2)
  3   CONTINUE
 
C
C NOW ROTATE AND BOOST EACH QUARK TO THE GG FRAME
C
C
C NOW ENFORCE ENERGY CONSERVATION
C
      ETOT = 0
      DO 11 I = 1,4
11    ETOT = ETOT + EN(I)
 
C      WRITE(6,*)' ETOT BEFORE CORRECTION ',ETOT
 
      FACTOR = GGW/ETOT
 
      PTOT(1) = 0.0
      PTOT(2) = 0.0
      PTOT(3) = 0.0
      PTOT(4) = 0.0
      DO 13 I = 1,4
            PXQ(I) = PXQ(I)*FACTOR
            PYQ(I) = PYQ(I)*FACTOR
            PZQ(I) = PZQ(I)*FACTOR
            EN(I) = EN(I)*FACTOR
            PTOT(1) = PTOT(1) + PXQ(I)
            PTOT(2) = PTOT(2) + PYQ(I)
            PTOT(3) = PTOT(3) + PZQ(I)
            PTOT(4) = PTOT(4) + EN(I)
13    CONTINUE
 
 
      GGWNEW = SQRT(PTOT(4)**2-PTOT(3)**2-PTOT(2)**2-PTOT(1)**2)
C      IF(NCALL.LT.20)THEN
C           WRITE(6,*)' W = ',GGW,' W ',GGWNEW
C      ENDIF
C
C CALC ANGLE OF GAMMA
C
        PTG2 = GAM1P(1)**2 + GAM1P(2)**2
        PG2  = PTG2 + GAM1P(3)**2
        PTG  = SQRT( PTG2 + 1.0E-20 )
        PG   = SQRT( PG2  + 1.0E-20 )
        CTHG = GAM1P(3) / PG
        STHG = PTG / PG
        CPHG = GAM1P(1) / PTG
        SPHG = GAM1P(2) / PTG
C
C ROTATE ALL 4 QUARKS INTO GAMMA GAMMA DIRECTION
C
      DO 10 IQUARK = 1,4
 
       QRK1P(1) = CTHG*CPHG*PXQ(IQUARK) - SPHG*PYQ(IQUARK)
     1                                  + STHG*CPHG*PZQ(IQUARK)
       QRK1P(2) = CTHG*SPHG*PXQ(IQUARK) + CPHG*PYQ(IQUARK)
     1                                + STHG*SPHG*PZQ(IQUARK)
       QRK1P(3) =    -STHG*PXQ(IQUARK)           + CTHG*PZQ(IQUARK)
       QRK1P(4) = EN(IQUARK)
C
C AND BOOST INTO LAB
C
       CALL LOREN4(LAB,QRK1P,QRK1)
 
C--- set LUND quark vectors
       N7LU = ILIST(IQUARK)
 
       P7LU(N7LU,1) = QRK1(1)
       P7LU(N7LU,2) = QRK1(2)
       P7LU(N7LU,3) = QRK1(3)
       P7LU(N7LU,4) = QRK1(4)
C
C PUT QUARK ON MASS SHELL - BY CHANGING ITS MASS !
C
       ARG = QRK1(4)**2-QRK1(1)**2-QRK1(2)**2- QRK1(3)**2
       IF(ARG.GT.0)THEN
        P7LU(N7LU,5) = SQRT(ARG)
       ELSE
        P7LU(N7LU,5) = 0.0
       ENDIF
       K7LU(N7LU,1) = 2
10     CONTINUE
 
C
C SET UP A VALID COMBINATION OF QUARKS
C
       K7LU(ILIST(1),2) = LUNTYP
       K7LU(ILIST(2),2) = -LUNTYP
       K7LU(ILIST(3),2) = -LUNTYP
       K7LU(ILIST(4),2) = LUNTYP
       K7LU(ILIST(4),1) = 1
         RETURN
         END
         REAL FUNCTION MCFPTQ(PQUARK,A)
C---------------------------------------------------------------
C! CHOOSE PT FOR QUARK ACCORDING TO DN/D(PT**2)=EXP(-A.PT**2)(PHOT02)
C
C INPUT  parameters :
C                     PQUARK - momentum of quark
C                      A     - slope parameter
C
C OUTPUT parameters :
C                      MCFPTQ : PT for quark
C
C CALLED BY - MCGRHO
C CALLS     - RN
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------
 
C ----------------------------------------------------------------------
C     CALCULATES  PT FOR VDM ACCORDING TO DN/D(PT**2)=EXP(-A.PT**2)
C     AS SUGGESTED BY IOS 21/11/82
C     CHANGED TO IMPROVE EFFICIENCY A.J.FINCH 22/6/88
C     protection added for too large argument in YMAX B.Bloch april 89
C ----------------------------------------------------------------------
       PARAMETER ( BIG = 90.)
       DATA PI / 3.1415927 /
       DATA IERR / 0 /
C ------------------------------------- FIRST CALCULATE PT**2
       IF ( PQUARK .LE. 0.0 ) THEN
          IERR = IERR + 1
          IF ( IERR .LT. 20 )
     *       WRITE(6,*)'+++++MCFPTQ : ERROR ! PQUARK,A = ',PQUARK,A
          IF ( IERR .EQ. 20 )
     *       WRITE(6,*)'+++++MCFPTQ : NO MORE ERROR MESSAGES !'
          MCFPTQ = 0.0
       ENDIF
       PT2MAX = PQUARK**2
       IF (A*PT2MAX.GT.BIG) THEN
         YMAX =0.
       ELSE
         YMAX = EXP(-A*PT2MAX)
       ENDIF
    1  CONTINUE
          Y =  RN(X)*(1-YMAX)
          PT2    = -1.0 / A   *   ALOG( 1 - Y )
          IF ( PT2 .LT. 0.0 ) PT2 = 0.0
          IF ( PT2 .GT. PT2MAX ) GOTO 1
          MCFPTQ = SQRT(PT2)
       RETURN
       END
      REAL FUNCTION MCGRHX(DUMMY)
C---------------------------------------------------------------
C! CHOOSE AN X FOR QUARK IN MESON  (PHOT02)
C
C INPUT  parameters :
C                    DUMMY
C OUTPUT parameters :
C                    MCGRHX - CHOSEN X
C CALLED BY -        MCGRHO
C CALLS     -        RN
C  A.J.Finch February 1993 adapted to PHOT02 from PHOT01 version
C----------------------------------------------------------------
C
C
C CHOOSE AN X FOR THE QUARK IN THE RHO
C  TOTALLY AD HOC AT PRESENT
C
      DATA YMAX/1.0/
1     CONTINUE
      X = RN(DOM)
      TEST = RN(DOM)*YMAX
C      IF(TEST.GT.(EXP(-15*X)*X))GOTO 1
C      IF(TEST.GT.(X*EXP(-15*X)/0.025))GOTO 1
      IF(TEST.GT.(1-X))GOTO 1
C       IF(TEST.GT.SIN(6.28*X)**2)GOTO 1
C      TEST2 = RN(DOM)
C      IF(TEST2.GT.0.5)X = 1 - X
 
      MCGRHX = X
      RETURN
      END
      SUBROUTINE BASES(FXN,BCC)
C=====================================
C     SUBROUTINE PERFORMS N-DIMENSIONAL MONTE CARLO INTEGRATION
C     FOR FOUR VECTOR GENERATION OF SIMULATED EVENTS
C     CODED BY S.KAWABATA   AT JULY,1980
C     CHANGED BY S.KAWABATA AT JAN. 1981
CB      real*8 SI,SI2,SWGT,SCHI,SCALLS,XI
CB      real*8 XND,DXG,DXD,DXP
CB      real*8 XJAC,XN,RC,FB,FB2,TI,TSI,DV2G
CB      real*8 XIN,R,DX,DT,DI,D
      COMMON/BASE1/NDIM,NCUB1,NTRIAL,ITMX,IGRAPH,IFLAG,NOMAX
      COMMON/BASE2/NDO,IT,SI,SI2,SWGT,SCHI,XI(50,10),SCALLS
      COMMON/BASE3/ND,XND,NG,NSP,DXG,KG(10),MA(10),DXD(17000),DXP(17000)
      COMMON/BASE4/NOW,NOWARI,K,XJAC,XN,RC,CALLS,FB,FB2,NPG,TI,TSI
     -            ,DV2G,NDM,IRN,ATACC,NSUC
      COMMON/BASE5/DI(50,10),D(50,10),XIN(50),R(50),DX(10),IA(10),DT(10)
      COMMON/RESULT/S1,S2,S3,S4
      COMMON/WEIGHT/WGT,ITS
      CHARACTER*50 ICH
      DIMENSION XL(10),XU(10)
      DIMENSION QRAN(10),X(10)
      DIMENSION BIPER(7)
      real*8 f2b,one,sd,xo,RR
      DATA BIPER/ 1.,2.,5.,10.,20., 50., 100./
      DATA XL,XU/10*0.,10*1./
      DATA NDMX,ALPH,ONE,MDS/50,1.5,1.D0,1/
      DATA LENG/ 17000/
      DATA NCMAX/ 6000/
CBB      DSQRT(XXX)=SQRT(XXX)
CBB      DLOG(XXX)=ALOG(XXX)
      ACC   = BCC
C=========> BRANCH
C        IFLAG =-1 ; ONLY INTEGRATION
C        IFLAG = 0 ; FIRST TRIAL OF DEFINING GRID
C        IFLAG = 1 ; FIRST TRIAL OF DATA ACCUMULATION
C        IFLAG = 2 ; SECOND TRIAL OF DEFINING GRID
C        IFLAG = 3 ; SECOND TRIAL OF DATA ACCUMULATION
      IHAT    = 0
      IF(IFLAG.LE.0) GO TO 1000
      IF(IFLAG.NE.1) GO TO 30
      DO 10 J=1,NSP
      DXD(J)= 0.
      DXP(J)= 0.
   10 CONTINUE
      GO TO 2000
C======> READ TEMPORARY RESULT FROM DISK FILE
   30 CONTINUE
      CALL DSTRDX
      IF(IFLAG.NE.2) GO TO 50
      IFLAG = 0
      GO TO 200
   50 IF(IFLAG.NE.3) STOP
      IFLAG = 1
      GO TO 200
C======================
 1000 IF(NOMAX.GT.NCMAX.AND.NOMAX.LE.LENG) NCMAX=NOMAX
      NCUB1 = (LENG)**(1./NDIM)
      IF(NCUB1.GT.50) NCUB1 = 50
      DO 70 I=2,NCUB1
      NGX   = I**NDIM
      IF(NGX.GT.NCMAX) GO TO 75
      NG    = I
   70 CONTINUE
   75 CONTINUE
      NCUB1 = NG
      NDO   = 1
      DO 80 I=1,25
      NDX   = NG*I
      IF(NDX.GT.NDMX) GO TO 85
      ND    = NDX
   80 CONTINUE
   85 CONTINUE
      DO 90 J=1,NDIM
   90 XI(1,J)= ONE
C======================================================
 2000 NOWARI= 0
      IF(IFLAG.LE.0) GO TO 100
      NG    = NCUB1
      ALPH  = 0.
  100 ATACC = 0.
      NOW   = IGRAPH
C                    ----------------------
CCC      IF(IFLAG.LE.0) CALL INHIST(NOW,F1,W)
CCC      IF(IFLAG.EQ.1) CALL CLHIST(NOW,F1,W)
C                    ----------------------
      IT    = 0
      SI    = 0.
      SI2   = SI
      SWGT  = SI
      SCHI  = SI
      SCALLS= SI
      MDS   = 1
      IF((2*NG-ND).LT.0)GO TO 110
      ND    = NDMX
      NG    = ND/2
      ND    = 2*NG
  110 NSP   = NG**NDIM
      NCALL = NSP*NTRIAL
      IF(IFLAG.NE.1) GO TO 120
C      WRITE(6,*)NDMX,NDIM,NG,ND,NSP,NTRIAL,NCALL
      DO 115 J=1,NDIM
      MA(J) = NG**(J-1)
  115 CONTINUE
  120 CONTINUE
C      WRITE(6,*)NDMX,NDIM,NG,ND,NSP,NTRIAL,NCALL
      K     = NG**NDIM
      NPG   = NCALL/K
      IF(NPG.LT.2) NPG = 2
      CALLS = NPG*K
      DXG   = ONE/NG
      DV2G  = DXG**(2*NDIM)/NPG/NPG/(NPG-ONE)
      XND   = ND
      NDM   = ND-1
      DXG   = DXG*XND
      XJAC  = ONE
      DO 130 J=1,NDIM
      DX(J) = XU(J)-XL(J)
  130 XJAC  = XJAC*DX(J)
      IF(ND.EQ.NDO)GO TO 160
      RC    = NDO/XND
      DO 155 J=1,NDIM
      K     = 0
      XN    = 0.
      DR    = XN
      I     = K
  140 K     = K+1
      DR    = DR+ONE
      XO    = XN
      XN    = XI(K,J)
  145 IF(RC.GT.DR)GO TO 140
      I     = I+1
      DR    = DR-RC
      XIN(I)=XN-(XN-XO)*DR
      IF(I.LT.NDM)GO TO 145
      DO 150 I=1,NDM
  150 XI(I,J)=XIN(I)
  155 XI(ND,J)=ONE
      NDO   = ND
  160 continue
c  160 WRITE(6,9160)NDIM,CALLS,IT,ITMX,ACC,MDS,ND,NG
c 9160 FORMAT(35H0INPUT PARAMETERS FOR BASES:  NDIM=,I3
c     1,8H  NCALL=,F8.0/28X,5H  IT=,I5,7H  ITMX=,I5/28X
c     2,6H  ACC=,G9.3/28X,6H  MDS=,I3,6H   ND=,I4,6H   NG=,I4//)
C==========>    MAIN INTEGRATION LOOP
  200 IT    = IT+1
      HATA  = 0.
      NTIM1 = NTIME(DUMM)
      ITS   = IT
      TI    = 0.
      TSI   = TI
C                    ----------------------
CCC      IF(IGRAPH.GT.0) CALL REHIST(NOW,F1,W)
C                    ----------------------
      DO 230 J=1,NDIM
      KG(J) = 1
      DO 230 I=1,ND
      D(I,J)= TI
  230 DI(I,J)=TI
  250 FB    = 0.
      F2B   = FB
      K     = 0
C=========== ACCUMULATE EVENTS
  270 K     = K+1
      DO 280 J=1,NDIM
  280 QRAN(J)=RN(DUMY)
      WGT   = XJAC
      DO 310 J=1,NDIM
      XN    = (KG(J)-QRAN(J))*DXG+ONE
      IA(J) = XN
      IAJ   = IA(J)
      IF(IAJ.GT.1)GO TO 290
      XO    = XI(IAJ,J)
      RC    = (XN-IA(J))*XO
      GO TO 300
  290 XO    = XI(IAJ,J)-XI(IAJ-1,J)
      RC    = XI(IAJ-1,J)+(XN-IAJ)*XO
  300 X(J)  = XL(J)+RC*DX(J)
  310 WGT   = WGT*XO*XND
      FXG   = FXN(X)
      F     = FXG*WGT
      F1    = F/CALLS
      W     = WGT/CALLS
      IF(F.GT.0.) HATA=HATA+1.
C                   -----------------------
CCC      IF(IGRAPH.GT.0) CALL XLHIST(NOW,F1,W)
C                   -----------------------
      F2    = F*F
      FB    = FB+F
      F2B   = F2B+F2
C                  -------------------------
      IF(IFLAG.EQ.1)  CALL DSFIL(NDIM,FXG,WGT,*8888)
C                  -------------------------
      DO 320 J=1,NDIM
      IAJ   = IA(J)
      DI(IAJ,J)= DI(IAJ,J)+F/CALLS
  320 IF(MDS.GE.0) D(IAJ,J)= D(IAJ,J)+F2
      IF(K.LT.NPG)GO TO 270
      F2B   = SQRT(F2B*NPG)
      F2B   = (F2B-FB)*(F2B+FB)
      TI    = TI+FB
      TSI   = TSI+F2B
      IF(MDS.GE.0)GO TO 340
      DO 330 J=1,NDIM
      IAJ   = IA(J)
  330 D(IAJ,J)=D(IAJ,J)+F2B
  340 K     = NDIM
  350 KG(K) = MOD(KG(K),NG)+1
      IF(KG(K).NE.1)GO TO 250
      K     = K-1
      IF(K.GT.0)GO TO 350
C                  -------------------------
      IF(IFLAG.EQ.1)  CALL DSCOR
C                  -------------------------
C========== COMPUTE RESULTS OF THIS ITERATION
      TI    = TI/CALLS
      TSI   = TSI*DV2G
      TI2   = TI*TI
      WGT   = TI2/TSI
      SI    = SI+TI*WGT
      SI2   = SI2+TI2
      SWGT  = SWGT+WGT
      SCHI  = SCHI+TI2*WGT
      SCALLS=SCALLS+CALLS
      AVGI  = SI/SWGT
      SD    = SWGT*IT/SI2
      CHI2A = 0.
      IF(IT.GT.1) CHI2A=SD*(SCHI/SWGT-AVGI*AVGI)/(IT-.999)
      SD    = SQRT(ONE/SD)
      TSI   =SQRT(TSI)
      PERCNT= SD/AVGI*100.
C     IF(IFLAG.EQ.1) GO TO 450
      IF(IHAT.GT.0) GO TO 420
      DO 400 KK=1,7
      PERMAX= BIPER(KK)
      DPER  = PERMAX/50.
      IF(PERMAX.GE.PERCNT) GO TO 410
  400 CONTINUE
  410 WRITE(6,9410) PERMAX
 9410 FORMAT(1H1,/1H ,4H  IT,6X,'TIME',6X,'RATE',4X,'RESULT',4X,'ST-DEV'
     -,5X,'T-RES',5X,'T-ACC',7X,'ACC',10X,'MAXIMUM OF DIST =',G11.3/1X
     -,12('----------'),'------+')
  420 IHAT=IHAT+1
      IPER  = PERCNT/DPER
      IF(IPER.GT.50) IPER=50
      DO 430 KK=1,50
      ICH(KK:KK)= ' '
      IF(KK.LE.IPER) ICH(KK:KK)= '*'
  430 CONTINUE
      TACC  = TSI/TI*100.
      RATE  = HATA/CALLS
      NTIM2 = NTIME(DUMM)
      WRITE(6,9430) IT,NTIM2,RATE,AVGI,SD,TI,TACC,PERCNT,ICH
 9430 FORMAT(1X,I4,I10,6G10.3,' I',A50,'I')
      NDTIM = (NTIM1-NTIM2)*1.1
      IGXX  = 0
      IF(IFLAG.GT.0) GO TO 440
C------<< TEST TEMPORARY ACCURACY >>-------------
      AACC = ABS(TACC-ATACC)/TACC
      IF(AACC.LE.0.10) NSUC=NSUC+1
      IF(AACC.GT.0.10) NSUC= 0
      ATACC = TACC
      IF(NSUC.GT.3) GO TO 460
C------<<<  CHECK REMAINING CPU TIME FOR THE CASE OF FLAG = 0 >>--------
  440 IF(NTIM2.GT.NDTIM) GO TO 450
      IGXX  = 1
      GO TO 460
  450 CONTINUE
      SDAV  = SD/AVGI
C
C AJF HAS BEEN HERE ! - I CHANGED THE TEST TO BE
C ON THE PERCENTAGE ERROR RATHER THAN THE ABSOLUTE ERROR
C - WHICH IS MORE USEFUL I THINK
C
C - OLD VERSION - IF(ABS(SDAV).GT.ACC.AND.IT.LT.ITMX) GO TO 470
      IF(ABS(PERCNT).GT.ACC.AND.IT.LT.ITMX) GO TO 470
  460 NOW   = 2
      NOWARI= 2
  470 S1    = AVGI
      S2    = SD
      S3    = TI
      S4    = TSI
C---> PRINT OUT THE RESULT
      IF(NOW.EQ.2) WRITE(6,9480)
 9480 FORMAT(1X,12('----------'),'------+')
      IF(NOW.EQ.2) WRITE(6,9490)IT,TI,TSI,AVGI,SD,CHI2A,HATA
 9490 FORMAT(///21H0INTEGRATION BY BASES /14H0ITERATION NO. ,I3,
     1 14H    INTEGRAL =,G14.8/21X,10HSTD DEV  =,G10.4 /
     2 34H ACCUMULATED RESULTS    INTEGRAL = ,G14.8/
     3 24X,10HSTD DEV  =,G10.4/24X,22HCHI**2 PER ITERATION =,G10.4/
     4 24X,10H# OF TRY =,G10.4)
C                                    -----------------------
CCC      IF(IGRAPH.GT.0) CALL PRHIST(NOW,F1,W)
C                                    -----------------------
      IF(IGXX.EQ.1) GO TO 600
      IF(NOWARI.NE.2) GO TO 700
C=========== END OF BASES
C      DO 500 J=1,NDIM
C  500 WRITE(6,9500) J,(XI(I,J),DI(I,J),D(I,J),I=1,ND)
 9500 FORMAT(14H0DATA FOR AXIS,I2/7X,1HX,9X,6HDELT I,3X,11HCONVERGENCE,
     111X,1HX,9X,6HDELT I,3X,11HCONVERGENCE,11X,1HX,9X,6HDELT I,3X,
     2 11HCONVERGENCE/(1H ,3G12.4,5X,3G12.4,5X,3G12.4))
      S1    = AVGI
      S2    = SD
      S3    = CHI2A
      WRITE(6,9510)
 9510 FORMAT(1X,'*********** END OF BASES *************')
      IF(IFLAG.EQ.1) GO TO 650
      IF(IFLAG.EQ.-1) RETURN
      IFLAG = 1
      RETURN
C========> TIME OUT
  600 NOW   = 0
      NOWARI= 0
      IF(IFLAG.EQ.-1) GO TO 660
      IF(IFLAG.EQ.0) IFLAG = 2
      IF(IFLAG.EQ.1) IFLAG = 3
C========> WRITE TEMPORARY RESULTS ON DISK
C                   -----------------------
  650                  CALL DSTWRT
C                   -----------------------
      IF(NOWARI.EQ.2) RETURN
  660 WRITE(6,9600) IFLAG
 9600 FORMAT(1X,'********** CPU TIME OUT ; CODE =',I3,' *************')
      RETURN
C======= SMOOTHING THE FUNCTION D(I,J)
  700 CONTINUE
      DO 720 J=1,NDIM
      XO    = D(1,J)
      XN    = D(2,J)
      D(1,J)= (XO+XN)/2.
      DT(J) = D(1,J)
      DO 710 I=2,NDM
      D(I,J)= XO+XN
      XO    = XN
      XN    = D(I+1,J)
      D(I,J)= (D(I,J)+XN)/3.
  710 DT(J) = DT(J)+D(I,J)
      D(ND,J)= (XN+XO)/2.
  720 DT(J) = DT(J)+D(ND,J)
C=========== REDEFINE THE GRID
      DO 780 J=1,NDIM
      RC    = 0.
      DO 730 I=1,ND
      R(I)  = 0.
      IF(D(I,J).LE.0)GO TO 730
      XO    = DT(J)/D(I,J)
      rr = ((XO-ONE)/XO/LOG(XO))**ALPH
      R(I)  = rr
CBB      R(I)  = ((XO-ONE)/XO/LOG(XO))**ALPH
  730 RC    = RC+R(I)
      RC    = RC/XND
      K     = 0
      XN    = 0
      DR    = XN
      I     = K
  740 K     = K+1
      DR    = DR+R(K)
      XO    = XN
      XN    = XI(K,J)
  750 IF(RC.GT.DR)GO TO 740
      I     = I+1
      DR    = DR-RC
      XIN(I)= XN-(XN-XO)*DR/R(K)
      IF(I.LT.NDM)GO TO 750
      DO 760 I=1,NDM
  760 XI(I,J)= XIN(I)
  780 XI(ND,J)= ONE
      SDAV  = SD/AVGI
      GO TO 200
C//////////////////////
 8888 WRITE(6,9888)
 9888 FORMAT(1X,'**** ERROR IN BASES ; DIMENSION OVERFLOW')
      RETURN
C//////////////////////
      END
      SUBROUTINE CONV
      DOUBLE PRECISION X(34)
      REAL Y(34)
      COMMON/LABVAR/X
      COMMON/LABVA /Y
      DO 1 I=1,34
    1 Y(I)=X(I)
      RETURN
      END
      SUBROUTINE DSFIL(NDIM,FX,WGT,*)
C==========================================
CB      real*8 DXO
CB      real*8 SI,SI2,SWGT,SCHI,SCALLS,XI
CB      real*8 XND,DXG,DXD,DXP
      COMMON/BASE2/NDO,IT,SI,SI2,SWGT,SCHI,XI(50,10),SCALLS
      COMMON/BASE3/ND,XND,NG,NSP,DXG,KG(10),MA(10),DXD(17000),DXP(17000)
      DIMENSION DXO(17000)
      DATA IHATA/0/
      IF(IHATA.EQ.1) GO TO 50
      IHATA = 1
      DO 20 I=1,NSP
      DXO(I)= 0.
 20   CONTINUE
  50  IAD   = 1
      DO 100 I=1,NDIM
      IAD   = IAD+(KG(I)-1)*MA(I)
 100  CONTINUE
      FXG   = FX*WGT
      IF(IAD.LT.17001) GO TO 120
      WRITE(6,9000) IAD,KG,FXG,FX,WGT
 9000 FORMAT(1X,'IAD,KG,FX,WGT =',I10,10I4,3E15.7)
      RETURN1
 120  DXO(IAD)= DXO(IAD)+FXG
      IF(FXG.GT.DXP(IAD)) DXP(IAD)= FXG
      RETURN
       ENTRY DSCOR
       IF(IT.GT.1) GO TO 300
       DO 200 I=1,NSP
       DXD(I)= DXO(I)
 200   CONTINUE
       GO TO 400
 300   DO 350 I=1,NSP
       DXD(I)= ((IT-1)*DXD(I)+DXO(I))/IT
 350   CONTINUE
 400   DO 450 I=1,NSP
       DXO(I)= 0.
 450   CONTINUE
       RETURN
      END
      SUBROUTINE EEMUMU(WMIN,WMAX,NERR,DJDX,X,J,
     1PI,NE,NE2,NU,NU2,F,R,SNP,CONS)
      IMPLICIT DOUBLE PRECISION(A-H,M-Z)
      INTEGER NERR
      REAL X(8),WMIN,WMAX,DJDX,PI,NE,NE2,NU,NU2,F,R,SNP,CONS
      DIMENSION Y(4)
C      COMMON/DATA/PI,NE,NE2,NU,NU2,F,R,SNP,CONS
      COMMON/DOTP/P12,P13,P14,P15,P23,P24,P25,P34,P35,P45,Q1,Q2,Q3,Q4
      COMMON/DOTM/P1DQ,P2DQ,P3DQ,P5DQ,Q1DQ
      COMMON/LABVAR/EW,E3,E5,E6,E7,PW,P3,P5,P6,P7,GAM1,GAM2,AL3,BE5
     1,CTW,STW,CT3,ST3,CT5,ST5,CT6,ST6,CT7,ST7,CP3,SP3,CP5,SP5,CP6,SP6
     2,CP7,SP7,W,W2
      COMMON/MOM/RT1,RT2,ME2,MU2
      COMMON/DELTA/DELTA
      COMMON/RELINV/ T1,T2,S1,S2
      DATA BCC/.1D-08/
      COS(XXX)=DCOS(XXX)
      SIN(XXX)=DSIN(XXX)
CBB      SQRT(XXX)=DSQRT(XXX)   ! automatic with generic sqrt
      ABS(XXX)=DABS(XXX)
      ACC=BCC*10.**J
      E=F
      S=E
      S=4.*S*S
      CONST=CONS
      NERR=0
      MU=NU
      ME=NE
      MU2=MU*MU
      ME2=ME*ME
      E2=E*E
      E2ME2= E2-ME2
      IF(E2ME2.LT.0.) GO TO 19
      EMP=ME2/(E+SQRT(E2ME2))
      P=E-EMP
C
C MAXIMUM POSSIBLE W IS ALL AVAILABEL ENERGY
C
      WWMAX=2.*(E-ME)
C
C MINIMUM POSSIBLE W IS 2* MASS OF PARTICLES TO CREATE
C
      WWMIN=2.*MU
      IF(WWMIN.LT.WMIN)WWMIN=WMIN
      IF(WWMIN.GT.WWMAX)GO TO 11
C
C IF WMAX PASSED AS ARGUMENT IS VALID THEN USE IT
C
      IF(WMAX.GT.WMIN.AND.WMAX.LT.WWMAX)WWMAX=WMAX
      W2MAX=WWMAX*WWMAX
      W2MIN=WWMIN*WWMIN
      XW=X(5)
      CALL MAPW2(W2,XW,W2MIN,W2MAX,DW)
      UM2W= 1.-4.*MU2/W2
      IF(UM2W.LT.0.) GO TO 19
      EQP=SQRT(UM2W)
      DW=DW*EQP/8.
C     THIS INCLUDES THE FACTORS DUE TO THE QUASI TWO BODY DECAY
      W=SQRT(W2)
      DO 1 I=1,4
    1 Y(I)=X(I)
      CALL INITIA(S,ME,ME,ME,W,ME,0,0,DW)
      CALL PICK4(S1,S2,T1,T2,C2,2,Y)
      IF(C2.EQ.0)GO TO 12
      DJDX=-C2
      RT1=T1
      RT2=T2
      EW=.25*(S1+S2-2.*ME2)/E
      EWMW= (EW-W)*(EW+W)
      IF(EWMW.LT.0.) GO TO 19
      PW=SQRT(EWMW)
      EWMPW=W2/(EW+PW)
      PW=EW-EWMPW
      EP1=DELTA+ME2*(S1+S2+4.*PW*PW-ME2)
      EP2=EP1/(S2-S1-4.*PW*P)
      EP1=EP1/(S1-S2-4.*PW*P)
      GAM1=(.5*(T1-T2)+EP1)/(P*PW)
      GAM2=(.5*(T2-T1)+EP2)/(P*PW)
      IF(GAM1.LT.1.)GAM2=2.-GAM1
      IF(GAM2.LT.1.)GAM1=2.-GAM2
      IF(GAM1.LT.1.)CTW=1.-GAM1
      IF(GAM2.LT.1.)CTW=GAM2-1.
      GAM12= GAM1*GAM2
      IF(GAM12.LT.0.) GO TO 19
      STW=SQRT(GAM12)
C     POSSIBLE PEAKING COULD BE ALONG EITHER BEAM
C     IF S1.GT.S2 USE GAM2 BECAUSE CTW IS NEAR -1.
C     IF S2.GT.S1 USE GAM1 BECAUSE CTW IS NEAR +1.
      EME3=.25*(S2-ME2)/E
      EME5=.25*(S1-ME2)/E
      E3=E-EME3
      E5=E-EME5
      E3ME= (E3-ME)*(E3+ME)
      IF(E3ME.LT.0.) GO TO 19
      P3=SQRT(E3ME)
      E5ME= (E5-ME)*(E5+ME)
      IF(E5ME.LT.0.) GO TO 19
      P5=SQRT(E5ME)
      E3MP3=ME2/(E3+P3)
      E5MP5=ME2/(E5+P5)
      P3=E3-E3MP3
      P5=E5-E5MP5
      EPA=-ME2*EME3*EME3/(E*E3+P*P3-ME2)
      EPB=-ME2*EME5*EME5/(E*E5+P*P5-ME2)
      AL3=(EPA-.5*T1)/(P*P3)
      BE5=(EPB-.5*T2)/(P*P5)
      IF(AL3.LT.0.OR.BE5.LT.0)GO TO 13
      CT3=1.-AL3
      CT5=-1.+BE5
      AL23= AL3*(2.-AL3)
      IF(AL23.LT.0.) GO TO 19
      ST3=SQRT(AL23)
      BE25= BE5*(2.-BE5)
      IF(BE25.LT.0.) GO TO 19
      ST5=SQRT(BE25)
      IF(S1.GT.S2)GO TO 5
      Z13=((P3*AL3+PW*GAM1)*((.5*T2+PW*EMP-EPA)+2.*E*(EMP-E3MP3)-EMP*
     1 (EWMPW+P3*AL3+PW*GAM1))/(2.*E)+PW*E3MP3-(2.*E-EWMPW)*(.5*T2+PW
     2 *EMP-EPA)/(2.*E)-.5*(PW*STW-P3*ST3)**2)/(P3*PW*ST3*STW)
      Z15=((PW*GAM1-P5*BE5)*((EPB-.5*T1+PW*EMP)-2.*E*(EMP-E5MP5)+EMP*
     1 (EW+PW-PW*GAM1+P5*BE5))/(2.*E)-PW*E5MP5-.5*(PW*STW-P5*ST5)**2
     2 +(2.*E-EW-PW)*(EPB-.5*T1+PW*EMP)/(2.*E))/(PW*P5*STW*ST5)
      GO TO 6
    5 Z13=((PW*GAM2-P3*AL3)*((EPA-.5*T2+PW*EMP)-2.*E*(EMP-E3MP3)+EMP*
     1 (EW+PW-PW*GAM2+P3*AL3))/(2.*E)-PW*E3MP3-.5*(PW*STW-P3*ST3)**2
     2 +(2.*E-EW-PW)*(EPA-.5*T2+PW*EMP)/(2.*E))/(P3*PW*ST3*STW)
      Z15=((PW*GAM2+P5*BE5)*((.5*T1+PW*EMP-EPB)+2.*E*(EMP-E5MP5)-EMP*
     1 (EWMPW+P5*BE5+PW*GAM2))/(2.*E)+PW*E5MP5-.5*(PW*STW-P5*ST5)**2
     2 -(2.*E-EWMPW)*(.5*T1+PW*EMP-EPB)/(2.*E))/(PW*P5*STW*ST5)
    6 CONTINUE
      CP3=Z13-1.
      CP5=Z15-1.
      IF(ABS(CP3).GT.1..OR.ABS(CP5).GT.1.)GO TO 14
      SP=-E3MP3*P5-E5MP5*E3-P3*P5*(AL3+BE5-AL3*BE5+ST3*ST5*CP3*CP5)
     1 +(ME2*(2.*S-S1-S2+ME2)-DELTA)/(2.*S)
      SP=SP/(P3*P5*ST3*ST5)
      IF(Z13.LT.Z15)GO TO 7
      Z213= Z13*(2.-Z13)
      IF(Z213.LT.0.) GO TO 19
      SP3=SQRT(Z213)
      IF(X(8).LT..5)SP3=-SP3
      SP5=SP/SP3
      ACC1=ABS(SP5*SP5+CP5*CP5-1.)
      GO TO 8
    7 Z215= Z15*(2.-Z15)
      IF(Z215.LT.0.) GO TO 19
      SP5=SQRT(Z215)
      IF(X(8).LT..5)SP5=-SP5
      SP3=SP/SP5
      ACC1=ABS(SP3*SP3+CP3*CP3-1.)
    8 CONTINUE
      IF(ACC1.GT.ACC)GO TO 17
      CT1P=1.-2.*X(6)
      C1T1P= (1.-CT1P)*(1.+CT1P)
      IF(C1T1P.LT.0.) GO TO 19
      ST1P=SQRT(C1T1P)
      PHP=2.*PI*X(7)
      CPHP=COS(PHP)
      SPHP=SIN(PHP)
      EQ=EQP*PW*CT1P
      QX=EQP*(W*CTW*ST1P*CPHP+EW*CT1P*STW)
      QY=EQP*W*ST1P*SPHP
      QZ=EQP*(EW*CT1P*CTW-W*STW*ST1P*CPHP)
      P1DQ=E*EQ-P*QZ
      P2DQ=E*EQ+P*QZ
      P3DQ=E3*EQ-P3*(QX*ST3*CP3+QY*ST3*SP3+QZ*CT3)
      P5DQ=E5*EQ-P5*(QX*ST5*CP5+QY*ST5*SP5+QZ*CT5)
      Q1DQ=EME3*EQ+(CP3*QX+SP3*QY)*P3*ST3-(EME3-EMP+E3MP3+P3*AL3)*QZ
      E6=.5*(EW+EQ)
      E7=.5*(EW-EQ)
      E6MU= (E6+MU)*(E6-MU)
      IF(E6MU.LT.0.) GO TO 19
      P6=SQRT(E6MU)
      E7MU= (E7+MU)*(E7-MU)
      IF(E7MU.LT.0.) GO TO 19
      P7=SQRT(E7MU)
      CT6=.5*(PW*CTW+QZ)/P6
      CT7=.5*(PW*CTW-QZ)/P7
      IF(ABS(CT6).GT.1.)GO TO 15
      IF(ABS(CT7).GT.1.)GO TO 16
      C1T6= (1.-CT6)*(1.+CT6)
      IF(C1T6.LT.0.) GO TO 19
      ST6=SQRT(C1T6)
      C1T7= (1.-CT7)*(1.+CT7)
      IF(C1T7.LT.0.) GO TO 19
      ST7=SQRT(C1T7)
      CP6=.5*(PW*STW+QX)/(P6*ST6)
      CP7=.5*(PW*STW-QX)/(P7*ST7)
      SP6= .5*QY/(P6*ST6)
      SP7=-.5*QY/(P7*ST7)
      ACC6=ABS(CP6*CP6+SP6*SP6-1.)
      ACC7=ABS(CP7*CP7+SP7*SP7-1.)
      IF(ACC6.GT.ACC*100.)GO TO 18
      IF(ACC7.GT.ACC*100.)GO TO 19
      CALL CONV
      RETURN
   19 NERR=NERR+1
   18 NERR=NERR+1
   17 NERR=NERR+1
   16 NERR=NERR+1
   15 NERR=NERR+1
   14 NERR=NERR+1
   13 NERR=NERR+1
   12 NERR=NERR+1
   11 NERR=NERR+1
      DJDX=0.
      RETURN
      END
      SUBROUTINE INITIA(SS,W1,W2,W3,W4,W5,INFO,ISEL,CONST)
      IMPLICIT DOUBLE PRECISION(A-H,M,O-Z)
      COMMON/INIT/S,V1,V2,V3,V4,V5,M1,M2,M3,M4,M5,M13,M45,MM4,Q,V13,SQ1,
     1SW,SQM,SWM,SUBS,TMIN,TMAX,DT1,B1(5),AL(14),BE(14),GA(3),DE(9)
CBB      SQRT(XXX)=DSQRT(XXX)
      V1=W1
      V2=W2
      V3=W3
      V4=W4
      V5=W5
      M1=V1*V1
      M2=V2*V2
      M3=V3*V3
      M4=V4*V4
      M5=V5*V5
      IF(W2.LT.0)M2=W2
      M45=(V4+V5)**2
      M13=M1+M3
      MM4=4.*M2
      V13=4.*M1*M3
      M12=M1*M2
      M23=M2*M3
      M15=M1*M5
      S=SS
      IF(ISEL.EQ.1)S=M1+M2+2.*V1*SS
      IF(ISEL.EQ.2)S=M1+M2+2.*V2*SS
      P=S-M3
      Q=S+M3
      R=S-M5
      T=M5-M4
      U=S-M2
      V=S-M1
      W=P-T
      X=M3-M4
      Y=R-M1
      Z=P-M2
      E=M3-M1
      H=S+M5
      SW=U+M1
      UP=M2+M4
      UV=V-M2
      VP=M13+M2
      X24=M2*M4
      X123=M12-M3*M5
      YP=M2-M5
      ZP=M2-M1
      WP=M2+M5
      SL1=SW*SW-4.*S*M1
      SQ1=SQRT(SL1)
      ST=Q-M45
      SL2=ST*ST-4.*S*M3
      SQ2=SQRT(SL2)
      AL(1)=S*M5
      AL(2)=S*YP-M5*SW
      AL(3)=S*(M5*(U-VP+T)-X24)-M3*M5*ZP
      AL(4)=M12-YP*(SW+M1)
      AL(5)=S*(YP*(S-YP)+2.*M5*(M3+M4)-M2*M13)-M4*ZP*WP+YP*(M1*M3
     1-M2*M5)-M15*(E+M5)-M2*M23
      AL(6)=S*(S*(-M5*YP-M4*WP)+X24*(VP+UP)-M5*(M3*T-M2*YP-M1*(UP+YP)))+
     1(M23*M5+ZP*(M5*(M2+YP)+M4*WP))*M3
      AL(7)=M1*YP
      AL(8)=YP*(M4+E)*UV+M1*(YP*(YP+3.*(M4-M2))-2.*X24)
      AL(9)=S*(YP*(-S*M4-E*WP)+M4*(2.*X123-X24))+M4*(ZP*(M2*(M1+UP)-M5*
     1ZP)+2.*M3*(M2*M2+M1*M5))+YP*(M3*(M23+M2*WP-M1*YP)+M15*ZP)
      AL(10)=S*(S*(M4+YP)*X24+M2*(YP*E*M5-M4*(M4*VP+M2*YP+2.*X123)))
     1-M23*(M4*(ZP*(UP+M2)+M3*WP)+YP*(E+M2)*M5)
      AL(11)=-M1*M4*YP
      AL(12)=M4*(M12*(M4+3.*YP)-E*YP*UV)
      AL(13)=X24*(E*(S*(M4+2.*YP)-M1*M4)-M13*(X24+E*YP)-M2*YP*(M3+M13))
      AL(14)=M2*X24*(M3*X24+YP*M12-E*(M4*P+YP*Z))
      BE(1)=-S
      BE(2)=-S*T
      BE(3)=2.*S-ZP
      BE(4)=S*(X-M4-V)+ZP*(M3-T)
      BE(5)=-T*(S*(U-VP)-M3*ZP)
      BE(6)=-SW-M1
      BE(7)=S*(P-X+M5)-ZP*ZP+2.*M1*(M3+M4)-M2*(M4+M5)
      BE(8)=M2*(H*M2-P*P)-M4*(U*U-M1*M1)-M5*V*V+2.*M3*(M4*V+M2*M5)
      BE(9)=M2*T*(S*(V-M1-M2)-M3*(M3+2.*ZP))
      BE(10)=M1
      BE(11)=V*E-M23-M1*(UP+WP)
      BE(12)=-S*E*(UP+WP)+M4*(E*M1+M23)+E*(M15+M2*(M13-YP))
     1+3.*M2*(M15+M23)
      BE(13)=M2*(S*E*(M5+WP)+M4*(M12-E*E)-M2*(M23+M5*(M3+M13))
     1-E*(M23+M13*M5))
      BE(14)=-M2*M2*T*(P*E-M23)
      GA(1)=M2*(M23-E*P)
      GA(2)=E*V-M2*M13
      GA(3)=V*P-Q*M2
      B1(5)=2.
      B1(3)=-2.
      B1(4)=2.*P
      B1(1)=-M2*B1(4)
      B1(2)=2.*(S+E+ZP)
      DE(9)=-2.*H
      DE(8)=2.*M4*S
      DE(7)=2.*(H+M1)-MM4
      DE(6)=2.*((YP-M4-M5)*S+M4*ZP+VP*M5+M23)
      DE(5)=-4.*M2*M4*S
      DE(4)=-2.*M1
      DE(3)=2.*(M2*M13+M4*(M1+M2)-E*M5)
      DE(2)=-2.*M2*((Q+E+M2)*M4+M23-E*M5)
      DE(1)=-.5*M2*DE(5)
      IF(INFO.GE.1)PRINT 103,V1,V2,V3,V4,V5,S
      YMA=-.5*(SW*ST+SQ1*SQ2)/S
      YMM=-2.*V1*V3
      T1MM=YMM+M13
      TMAX=YMA+M13
      IF(V1.EQ.0)GO TO 30
      SQM=.5*SQ1/M1
      SWM=.5*SW/M1
      SMM=Q+SWM*YMM
      IF(SMM.LE.M45)GO TO 10
      TMIN=T1MM
      IF(INFO.GE.1)PRINT 101,TMIN,TMAX
      GO TO 20
   30 SQM=SW*M3
      SWM=S/SW
   10 TMIN=((M45-M2)*E+(M45-M2-E)*(M1*M45-M23)/S)/TMAX
      IF(INFO.GE.1)PRINT 100,TMIN,TMAX
   20 DT1=TMAX-TMIN
      SUBS=CONST*3.141592*.25/SQ1
      IF(INFO.GE.2)PRINT 104,S,V1,V2,V3,V4,V5,M1,M2,M3,M4,M5,M13,M45,MM4
     1,Q,V13,SQ1,SW,SQM,SWM,SUBS,TMIN,TMAX,DT1,B1,AL,BE,GA,DE
  100 FORMAT(//9H   TMIN =,G18.10/9H   TMAX =,G18.10/)
  101 FORMAT(//9H   TMIN =,G18.10/9H   TMAX =,G18.10//
     132H   TMIN IS NOT OBTAINED AT S2MIN/)
  103 FORMAT(9H1MASS 1 =,G18.10/9H MASS 2 =,G18.10/9H MASS 3 =,G18.10
     1/9H MASS 4 =,G18.10/9H MASS 5 =,G18.10/9H    S   =,G18.10//)
  104 FORMAT(1H ,'VARIABLES IN COMMON',/6G20.10/5G20.10/5G20.10/4G20.10
     1/G20.10/3G20.10/5G20.10/4(7G18.10/),3G18.10/7G18.10/2G18.10/)
      RETURN
      END
       INTEGER FUNCTION LFTIME(MINTIM)
C               ***  K E R N L I B  V E R S I O N  ***
C   Function  LFTIME  :  Returns the time left for a job.
C                        If this time is less than the supplied
C                        parameter (MINTIM) then 0 is returned.
C                        All times are measured in seconds.
C                        LFTIME can be changed to use machine
C                        dependent timing routines.
       REAL T
       INTEGER MINTIM
C--- get time left in secs
       CALL TIMEL(T)
       LFTIME = INT(T*100)
C--- is this less than the specified time limit ?
       IF ( LFTIME .LT. MINTIM ) LFTIME = 0
       RETURN
       END
      FUNCTION PERIPH(DUM)
      IMPLICIT DOUBLE PRECISION(A-Z)
      REAL PERIPH,dum
      COMMON/MOM/T1,T2,ME2,MU2
      COMMON/DOTM/P1DQ,P2DQ,P3DQ,P5DQ,Q1DQ
      COMMON/DELTA/DELTA
      COMMON/DOTP/P12,P13,P14,P15,P23,P24,P25,P34,P35,P45,Q1,Q2,Q3,Q4
      COMMON/LABVAR/XX(33),W2
      DATA UNDFL/ 0.30D-38/
      Q1DQ2=.5*(W2-T1-T2)
      M12=ME2
      M32=ME2
      M22=MU2
      EP=.25*(DELTA-(T1+T2)*2.*P12-2.*P34*(T2-T1-M32)-2.*P45*(T1-T2
     1-M12)+M12*M32+(T1-T2)**2)
      M3222=M32+M22
      M1222=M12+M22
      M1232=M12+M32
      B=Q1DQ*Q1DQ-Q1DQ2*Q1DQ2
      A2=P24*Q1DQ+P2DQ*Q1DQ2
      A1=P14*Q1DQ-P1DQ*Q1DQ2
      TEMP =-512.*(P12*Q1DQ**2+P14*P2DQ*Q1DQ-P1DQ*P24*Q1DQ-P1DQ*P2DQ
     1*Q1DQ2-EP*Q1DQ2-.5*T1*(P24*Q1DQ2+P2DQ*Q1DQ)-.5*T2*(P14*Q1DQ2
     2-P1DQ*Q1DQ))**2
      AA = TEMP
     3-128.*(T1*T2*(M1232*B*W2+M22*(Q1DQ**2*(T1+T2)+2.*Q1DQ2*B))+Q1DQ**2
     4*(M12*T2+M32*T1)*(T1*T2+B)+B*T1*M32*(2.*P14-T1-Q1DQ2)**2
     5+B*T2*M12*(2.*P24-T2-Q1DQ2)**2)
     6-512.*(T1*M1222*(A2-.5*T2*Q1DQ)**2+T2*M3222*(A1-.5*T1*Q1DQ)**2
     7+(T1*T2*M1222*M3222+M12*M32*B)*Q1DQ2**2)
     8-128.*T1*T2*B*((2.*P12-P14-P24+Q1DQ2)**2+P1DQ**2+P2DQ**2
     9+.5*(T1*T1+T2*(T1+T2))+(2.*P12+Q1DQ2)*(T1+T2)-T2*P24-T1*P14)
     O+64.*T1*T2*A1*A2-128.*(T1*A2+T2*A1-T1*T2*Q1DQ)**2
     1-64.*T1*T2*((2.*B+P14*Q1DQ2-P1DQ*Q1DQ)*(2.*B+P24*Q1DQ2+P2DQ
     2*Q1DQ)-3.*B*(B+P14*P24)+B*P1DQ*P2DQ)
      IF(DABS(B).LT.UNDFL) GO TO 100
      PERIP1= AA/B/B
      WEIGHT= PERIP1
      IF(DABS(T1).LT.UNDFL) GO TO 100
      PERIP1= PERIP1/T1/T1
      IF(DABS(T2).LT.UNDFL) GO TO 100
      PERIPH= PERIP1/T2/T2
      RETURN
  100 PERIPH= 0.
C     WRITE(6,9100) B,T1,T2
C9100 FORMAT(1X,'******** DEVIDE CHECK ERROR ******',3E15.4)
      RETURN
      END
      SUBROUTINE PICK4(S1,S2,T1,T2,CON,DOT,X)
      IMPLICIT DOUBLE PRECISION(A-H,M,O-Z)
      INTEGER DOT
      COMMON/INIT/S,V1,V2,V3,V4,V5,M1,M2,M3,M4,M5,M13,M45,MM4,Q,V13,SQ1,
     1SW,SQM,SWM,SUBS,TMIN,TMAX,DT1,B1(5),AL(14),BE(14),GA(3),DE(9)
      COMMON/DOTP/P12,P13,P14,P15,P23,P24,P25,P34,P35,P45,Q1,Q2,Q3,Q4
      COMMON/DELTA/DELTA
      DIMENSION  X(4)
CBB      SQRT(XXX)=DSQRT(XXX)
CBB      ALOG(XXX)=DLOG(XXX)
      XT1=X(1)
      CALL MAPT1(T1,XT1,TMIN,TMAX,DT1)
      Y=T1-M13
      IF(Y.EQ.0.) GO TO 1
      SM=M45
      IF(V1.NE.0)GO TO 2
      SMAX=Q+Y*SWM+SQM/Y
      GO TO 3
    2 YA=Y*Y-V13
      IF(YA.LT.0)GO TO 1
      YB=SQM*SQRT(YA)
      YC=Q+SWM*Y
      SMAX=YC+YB
      IF(YC-YB.GT.M45)SM=YC-YB
    3 XS2=X(2)
      CALL MAPS2(S2,XS2,SM,SMAX,DS2)
      IF(S2.EQ.0.) GO TO 1
      A=(S2-T1+M2)**2-MM4*S2
      IF(A.EQ.0.) GO TO 1
      PB1=B1(1)+S2*(B1(2)+S2*B1(3)+T1*B1(5))+T1*B1(4)
      PA=16.*((S*T1-SW*S2+GA(3))*T1+(M1*S2+GA(2))*S2+GA(1))*S2
      PB=16.*((((BE(1)*S2+BE(2))*T1+(BE(3)*S2+BE(4))*S2+BE(5))*T1
     1+((BE(6)*S2+BE(7))*S2+BE(8))*S2+BE(9))*T1
     2+(((BE(10)*S2+BE(11))*S2+BE(12))*S2+BE(13))*S2+BE(14))
      PC=16.*((((T1*AL(1)+S2*AL(2)+AL(3))*T1+(S2*AL(4)+AL(5))*S2+AL(6))
     1*T1+((S2*AL(7)+AL(8))*S2+AL(9))*S2+AL(10))*T1+((S2*AL(11)+AL(12)
     2)*S2+AL(13))*S2+AL(14))
      IF(PA.GE.0)GO TO 1
C A.J.FINCH ADDITION TO AVOID ARITHMETIC OVERFLOW
      IF(ABS(PB).GT.1E19)THEN
             PD = (PB/1E9)**2-4E-18*PA*PC
             PS = 1E9*SQRT(PD)
      ELSE
             PD=PB*PB-4.*PA*PC
             IF(PD.LT.0)GO TO 1
             PS=SQRT(PD)
      ENDIF
      IF(PD.LT.0)GO TO 1
      PS=SQRT(PD)
      T2MAX=-.5*(PB-PS)/PA
      T2MIN=PC/(PA*T2MAX)
      XT2=X(3)
      CALL MAPT2(T2,XT2,T2MIN,T2MAX,DT2)
      D=PA*(T2-T2MAX)*(T2-T2MIN)
      IF(D.LE.0)GO TO 1
      PS=SQRT(D)
      DELTA=DE(1)+S2*(DE(2)+S2*(DE(3)+S2*DE(4)))
     1 +T1*((DE(5)+S2*(DE(6)+S2*DE(7)))+T1*(DE(8)+S2*DE(9)))
      DELMIN=(DELTA+S2*(T2*PB1-PS))/(2.*A)
      DDEL=S2*PS/A
      DELTA=DELMIN+DDEL*(1.-X(4))
      S1=(S*M4-DELTA)/S2
      DS1=DDEL/S2
      GRAM=A*(DELTA-DELMIN)*(DELMIN+DDEL-DELTA)/(S2*S2)
      IF(GRAM.EQ.0.) GO TO 1
      CON=DT1*DT2*DS1*DS2*SUBS/SQRT(GRAM)
      CON=-CON
      IF(DOT.EQ.0)RETURN
      P12=.5*(S-M1-M2)
      P13=.5*(M13-T1)
      P14=.5*(S1+T1-T2-M3)
      P15=.5*(S+T2-S1-M2)
      P23=.5*(S+T1-S2-M1)
      P24=.5*(S2+T2-T1-M5)
      P25=.5*(M2+M5-T2)
      P34=.5*(S1-M3-M4)
      P35=.5*(S+M4-S1-S2)
      P45=.5*(S2-M4-M5)
      IF(DOT.EQ.1)RETURN
      Q1=.5*(T2-S1+M1)
      Q2=.5*(M1+M4-S1-T1)
      Q3=.5*(T1-S2+M2)
      Q4=.5*(M2+M4-S2-T2)
      RETURN
    1 CON=0.
      RETURN
      END
      SUBROUTINE SPRING(IPRNT,*,*)
C===================================
C      GENERATION OF EVENTS ACCORDING TO THE PROBABILTY DENSITY
C      WHICH IS STORED IN A DISK FILE.
C     CODED BY S.KAWABATA   AT JULY,1980
C     CHANGED BY S.KAWABATA AT JAN. 1981
CB      DOUBLE PRECISION RX,DXD
CB      real*8 SI,SI2,SWGT,SCHI,SCALLS,XI
CB      real*8 XND,DXG,DXO,DXP
      COMMON/BASE1/NDIM,NCUBES,NTRIAL,ITMX,IGRAPH,IFLAG,ICOND
      COMMON/BASE2/NDO,IT,SI,SI2,SWGT,SCHI,XI(50,10),SCALLS
      COMMON/BASE3/ND,XND,NG,NSP,DXG,KG(10),MA(10),DXO(17000),DXP(17000)
      DIMENSION QRAN(10),Y(10),DXD(17000)
      DATA ONE/1./,NEVNT/0/
      DATA IHATA/0/
C//////////
      IF(IHATA.EQ.1) GO TO 100
      IHATA = 1
      RX    = 0.D0
      DO 50 I=1,NSP
      RX    = RX+DXO(I)
      DXD(I)= RX
  50  CONTINUE
C     RATIO = 1.
  100 RX    = RN(DUMY)*DXD(NSP)
      IPMIN = 1
      IPMAX = NSP
 120  IC    = (IPMIN+IPMAX)/2
      IF(RX.GE.DXD(IC)) GO TO 122
      IPMAX = IC
      GO TO 124
 122  IPMIN = IC
 124  CONTINUE
      IF(IPMAX-IPMIN.GT.10) GO TO 120
      IC    = IPMIN-1
 126  IC    = IC+1
      IF(DXD(IC).LT.RX) GO TO 126
      IX    = IC-1
C     IF(IC.NE.1) GO TO 135
C     FMAX  = DXD(IC)*RATIO
C     GO TO 136
C135  FMAX  = (DXD(IC)-DXD(IC-1))*RATIO
      FMAX  = DXP(IC)
 136  IFLG  = 0
      DO 160 I=1,NDIM
      NO    = NDIM+1-I
      IJ    = IX/MA(NO)
      IF(IJ.GT.NG) IFLG = IFLG+1
      KG(NO)= IJ+1
      IF(IJ) 140,160,140
  140 IX    = IX-IJ*MA(NO)
  160 CONTINUE
      IF(IFLG) 600, 180, 600
C========== CONVERSION OF PARAMETER
  180 NTRX  = 0
  190 CONTINUE
CTEMP 1000 -> 10000
      IF(NTIME(DUMM).LT.1000) GO TO 700
      DO 200 J=1,NDIM
  200 QRAN(J)=RN(DUMY)
      WGT   = 1.
      DO 240 J=1,NDIM
      XN    = (KG(J)-QRAN(J))*DXG+ONE
      IAJ   = XN
      IF(IAJ.GT.1)GO TO 210
      XO    = XI(IAJ,J)
      RC    = (XN-IAJ)*XO
      GO TO 214
  210 XO    = XI(IAJ,J)-XI(IAJ-1,J)
      RC    = XI(IAJ-1,J)+(XN-IAJ)*XO
  214 Y(J)  = RC
      WGT   = WGT*XO*XND
  240 CONTINUE
      FX    = F(Y)
      IF(FX.LE.0.) GO TO 190
      FUNCT = FX*WGT/FMAX
      NTRX  = NTRX+1
C     IF(NEVNT.LT.IPRNT) WRITE(6,9240) NTRX,FX,WGT,FMAX,FUNCT
C9240 FORMAT(1X,I10,'-TH TRIAL',4E15.4)
      IF(RN(DUMY).GT.FUNCT) GO TO 190
      NEVNT = NEVNT+1
C     IF(NEVNT.LE.IPRNT) WRITE(6,9300) NEVNT,FUNCT
C9300 FORMAT(1X,'GENERATION OF',I10,'-TH EVENT',E15.4)
      RETURN
 600  WRITE(6,9600) KG
 9600 FORMAT(1X,'*** PRODUCTION ERROR IA =',10I5)
      RETURN1
 700  WRITE(6,9700)
 9700 FORMAT(1X,'*** TIME LIMIT IN THE SPRING ****')
      RETURN2
      END
