      SUBROUTINE ASKUSI(IGCOD)                                          ASKUSI 2
C --------------------------------------------------------------------  ASKUSI 3
C Initialization for UBAB02    from H. Meinhard , P. Comas /B.Bloch may ASKUSI 4
C --------------------------------------------------------------------  ASKUSI 5
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12         ALCONS 2
      REAL RADEG, DEGRA                                                 ALCONS 3
      REAL CLGHT, ALDEDX                                                ALCONS 4
      INTEGER NBITW, NBYTW, LCHAR                                       ALCONS 5
      PARAMETER (PI=3.141592653589)                                     ALCONS 6
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)                          ALCONS 7
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          ALCONS 8
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         ALCONS 9
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         ALCONS10
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)                 ALCONS11
      INTEGER       LBCS, IW                                            BCS    2
      PARAMETER     (LBCS=1000)                                         BCS    3
      COMMON        /BCS/ IW(LBCS)                                      BCS    4
      REAL          RW(LBCS)                                            BCS    5
      EQUIVALENCE   (RW(1),IW(1))                                       BCS    6
      INTEGER       NCNT, NEVCNT, ITFRE                                 CDGENE 2
      PARAMETER     (NCNT = 5)                                          CDGENE 3
      REAL          SDVRT, VPOS                                         CDGENE 4
      COMMON        /CDGENE/ SDVRT(3), VPOS(3), NEVCNT(NCNT), ITFRE     CDGENE 5
      INTEGER       NINP, NOUT, NOUT2                                   INOUT  2
      COMMON        /INOUT/ NINP, NOUT, NOUT2                           INOUT  3
      double precision ahpla                                            UBPCOM 2
      common /ubpcom/  ahpla                                            UBPCOM 3
      double precision alpha                                            UBPCOM 4
      common /ubpcom/  alpha                                            UBPCOM 5
      double precision mass1e                                           UBPCOM 6
      common /ubpcom/  mass1e                                           UBPCOM 7
      double precision mass2e                                           UBPCOM 8
      common /ubpcom/  mass2e                                           UBPCOM 9
      double precision mass1z                                           UBPCOM10
      common /ubpcom/  mass1z                                           UBPCOM11
      double precision mass2z                                           UBPCOM12
      common /ubpcom/  mass2z                                           UBPCOM13
      double precision mass1t                                           UBPCOM14
      common /ubpcom/  mass1t                                           UBPCOM15
      double precision mass1h                                           UBPCOM16
      common /ubpcom/  mass1h                                           UBPCOM17
      double precision mass1w                                           UBPCOM18
      common /ubpcom/  mass1w                                           UBPCOM19
      double precision gamm1z                                           UBPCOM20
      common /ubpcom/  gamm1z                                           UBPCOM21
      double precision gamm2z                                           UBPCOM22
      common /ubpcom/  gamm2z                                           UBPCOM23
      double precision sin2w                                            UBPCOM24
      common /ubpcom/  sin2w                                            UBPCOM25
      double precision alphas                                           UBPCOM26
      common /ubpcom/  alphas                                           UBPCOM27
      double precision ebeam                                            UBPCOM28
      common /ubpcom/  ebeam                                            UBPCOM29
      double precision epol                                             UBPCOM30
      common /ubpcom/  epol                                             UBPCOM31
      double precision ppol                                             UBPCOM32
      common /ubpcom/  ppol                                             UBPCOM33
      double precision ctsmin                                           UBPCOM34
      common /ubpcom/  ctsmin                                           UBPCOM35
      double precision ctsmax                                           UBPCOM36
      common /ubpcom/  ctsmax                                           UBPCOM37
      double precision ecut                                             UBPCOM38
      common /ubpcom/  ecut                                             UBPCOM39
      double precision acocut                                           UBPCOM40
      common /ubpcom/  acocut                                           UBPCOM41
      double precision evisct                                           UBPCOM42
      common /ubpcom/  evisct                                           UBPCOM43
      integer          nevent, pad022                                   UBPCOM44
      common /ubpcom/  nevent, pad022                                   UBPCOM45
      integer          rseed , pad023                                   UBPCOM46
      common /ubpcom/  rseed , pad023                                   UBPCOM47
      logical          tchann, pad024                                   UBPCOM48
      common /ubpcom/  tchann, pad024                                   UBPCOM49
      logical          qedvtx, pad025                                   UBPCOM50
      common /ubpcom/  qedvtx, pad025                                   UBPCOM51
      logical          qedbox, pad026                                   UBPCOM52
      common /ubpcom/  qedbox, pad026                                   UBPCOM53
      logical          weak  , pad027                                   UBPCOM54
      common /ubpcom/  weak  , pad027                                   UBPCOM55
      logical          boxes , pad028                                   UBPCOM56
      common /ubpcom/  boxes , pad028                                   UBPCOM57
      integer          isrtyp, pad029                                   UBPCOM58
      common /ubpcom/  isrtyp, pad029                                   UBPCOM59
      integer          fsrtyp, pad030                                   UBPCOM60
      common /ubpcom/  fsrtyp, pad030                                   UBPCOM61
      double precision epsiln                                           UBPCOM62
      common /ubpcom/  epsiln                                           UBPCOM63
      double precision taumin                                           UBPCOM64
      common /ubpcom/  taumin                                           UBPCOM65
      double precision taumax                                           UBPCOM66
      common /ubpcom/  taumax                                           UBPCOM67
      double precision emin                                             UBPCOM68
      common /ubpcom/  emin                                             UBPCOM69
      double precision eps2                                             UBPCOM70
      common /ubpcom/  eps2                                             UBPCOM71
      double precision qsq                                              UBPCOM72
      common /ubpcom/  qsq                                              UBPCOM73
      integer          bstyle, pad037                                   UBPCOM74
      common /ubpcom/  bstyle, pad037                                   UBPCOM75
      logical          nonlog, pad038                                   UBPCOM76
      common /ubpcom/  nonlog, pad038                                   UBPCOM77
      integer          stdin , pad039                                   UBPCOM78
      common /ubpcom/  stdin , pad039                                   UBPCOM79
      integer          stdout, pad040                                   UBPCOM80
      common /ubpcom/  stdout, pad040                                   UBPCOM81
      integer          stderr, pad041                                   UBPCOM82
      common /ubpcom/  stderr, pad041                                   UBPCOM83
      integer          errcnt, pad042                                   UBPCOM84
      common /ubpcom/  errcnt, pad042                                   UBPCOM85
      integer          errmax, pad043                                   UBPCOM86
      common /ubpcom/  errmax, pad043                                   UBPCOM87
      integer          verbos, pad044                                   UBPCOM88
      common /ubpcom/  verbos, pad044                                   UBPCOM89
      integer          status, pad045                                   UBPCOM90
      common /ubpcom/  status, pad045                                   UBPCOM91
      integer          hepdat, pad046                                   UBPCOM92
      common /ubpcom/  hepdat, pad046                                   UBPCOM93
      integer          heprev, pad047                                   UBPCOM94
      common /ubpcom/  heprev, pad047                                   UBPCOM95
      integer          runid , pad048                                   UBPCOM96
      common /ubpcom/  runid , pad048                                   UBPCOM97
      logical          dbgcol, pad049                                   UBPCOM98
      common /ubpcom/  dbgcol, pad049                                   UBPCOM99
      logical          dbghep, pad050                                   UBPCO100
      common /ubpcom/  dbghep, pad050                                   UBPCO101
      logical          dbgini, pad051                                   UBPCO102
      common /ubpcom/  dbgini, pad051                                   UBPCO103
      logical          dbgmas, pad052                                   UBPCO104
      common /ubpcom/  dbgmas, pad052                                   UBPCO105
      logical          dbgmup, pad053                                   UBPCO106
      common /ubpcom/  dbgmup, pad053                                   UBPCO107
      DOUBLE PRECISION BRNMAX                                           UBPCO108
      COMMON /UBCBRN/  BRNMAX                                           UBPCO109
      double precision maxws, maxwt,                                    UBPCO110
     &                 maxwnl, wnlvs, wnlhrd, wnlavh, wnlbare           UBPCO111
      common /ubcsta/  maxws, maxwt,                                    UBPCO112
     &                 maxwnl, wnlvs, wnlhrd, wnlavh, wnlbare           UBPCO113
                                                                        UBPCO114
      integer          isbad, iscall, isfail, isrej, callct, acccut     UBPCO115
      common /ubcsta/  isbad, iscall, isfail, isrej, callct, acccut     UBPCO116
                                                                        UBPCO117
      integer          itbad, itcall, itfail, nlbad, nlcall, nlfail     UBPCO118
      common /ubcsta/  itbad, itcall, itfail, nlbad, nlcall, nlfail     UBPCO119
                                                                        UBPCO120
      integer          nlvs, nlhrd                                      UBPCO121
      common /ubcsta/  nlvs, nlhrd                                      UBPCO122
      CHARACTER     VERSQQ*8                                            ASKUSI 7
      INTEGER       ALTABL, ALRLEP, IDATQQ, IGCO, IGCOD, NAMIND, JGPAR, ASKUSI 8
     +              ITCH, IWEAK, IBOXES, IBST, JSVRT, JXVRT, NCOL, NROW,ASKUSI 9
     +              JKPAR, IEBEAM, JRLEP, ITAB(19)                      ASKUSI10
      EXTERNAL      ALTABL, ALRLEP, NAMIND                              ASKUSI11
      REAL          TABL(19), AMZ, AMH, AMT, ECMS, CTSMN, CTSMX, EFERM, ASKUSI12
     +              ACOLMX                                              ASKUSI13
      PARAMETER     (IGCO = 2011)                                       ASKUSI14
      EQUIVALENCE   (TABL(1), ITAB(1))                                  ASKUSI15
C --------------------------------------------------------------------  ASKUSI16
C                                                                       ASKUSI17
C   Set the generator code as defined in the KINGAL library             ASKUSI18
C                                                                       ASKUSI19
      IGCOD = IGCO                                                      ASKUSI20
      NOUT  = IW(6)                                                     ASKUSI21
C                                                                       ASKUSI22
C   Default parameters                                                  ASKUSI23
C                                                                       ASKUSI24
      AMZ       = 91.187                                                ASKUSI25
      AMH       = 250.0                                                 ASKUSI26
      AMT       = 150.0                                                 ASKUSI27
      ECMS      = 92.0                                                  ASKUSI28
      CTSMN     = -0.9                                                  ASKUSI29
      CTSMX     = +0.9                                                  ASKUSI30
      EFERM     = 20.0                                                  ASKUSI31
      ACOLMX    = 20.0                                                  ASKUSI32
      ITCH      = 1                                                     ASKUSI33
      IWEAK     = 1                                                     ASKUSI34
      IBOXES    = 1                                                     ASKUSI35
      IBST      = 3                                                     ASKUSI36
      ITFRE     = 1000                                                  ASKUSI37
      SDVRT(1)  = 0.0110                                                ASKUSI38
      SDVRT(2)  = 0.0005                                                ASKUSI39
      SDVRT(3)  = 0.70                                                  ASKUSI40
      VPOS(1)   = 0.0                                                   ASKUSI41
      VPOS(2)   = 0.0                                                   ASKUSI42
      VPOS(3)   = 0.0                                                   ASKUSI43
C                                                                       ASKUSI44
C   The default values can be changed by data cards GPAR, SVRT, XVRT    ASKUSI45
C                                                                       ASKUSI46
C   various generator-dependent parameters                              ASKUSI47
      JGPAR = IW(NAMIND('GPAR'))                                        ASKUSI48
      IF (JGPAR .NE. 0) THEN                                            ASKUSI49
        AMZ    = RW(JGPAR+ 1)                                           ASKUSI50
        AMH    = RW(JGPAR+ 2)                                           ASKUSI51
        AMT    = RW(JGPAR+ 3)                                           ASKUSI52
        ECMS   = RW(JGPAR+ 4)                                           ASKUSI53
        CTSMN  = RW(JGPAR+ 5)                                           ASKUSI54
        CTSMX  = RW(JGPAR+ 6)                                           ASKUSI55
        EFERM  = RW(JGPAR+ 7)                                           ASKUSI56
        ACOLMX = RW(JGPAR+ 8)                                           ASKUSI57
        ITCH   = IW(JGPAR+ 9)                                           ASKUSI58
        IWEAK  = IW(JGPAR+10)                                           ASKUSI59
        IBOXES = IW(JGPAR+11)                                           ASKUSI60
        IBST   = IW(JGPAR+12)                                           ASKUSI61
        ITFRE  = IW(JGPAR+13)                                           ASKUSI62
      END IF                                                            ASKUSI63
C   main vertex smearing                                                ASKUSI64
      JSVRT = IW(NAMIND('SVRT'))                                        ASKUSI65
      IF (JSVRT .NE. 0) THEN                                            ASKUSI66
        SDVRT(1) = RW(JSVRT+1)                                          ASKUSI67
        SDVRT(2) = RW(JSVRT+2)                                          ASKUSI68
        SDVRT(3) = RW(JSVRT+3)                                          ASKUSI69
      END IF                                                            ASKUSI70
C   main vertex mean position                                           ASKUSI71
      JXVRT = IW(NAMIND('XVRT'))                                        ASKUSI72
      IF (JXVRT .NE. 0) THEN                                            ASKUSI73
        VPOS(1)  = RW(JXVRT+1)                                          ASKUSI74
        VPOS(2)  = RW(JXVRT+2)                                          ASKUSI75
        VPOS(3)  = RW(JXVRT+3)                                          ASKUSI76
      END IF                                                            ASKUSI77
C                                                                       ASKUSI78
C  Fill the KPAR bank with the generator parameters                     ASKUSI79
C                                                                       ASKUSI80
      TABL( 1) = AMZ                                                    ASKUSI81
      TABL( 2) = AMH                                                    ASKUSI82
      TABL( 3) = AMT                                                    ASKUSI83
      TABL( 4) = ECMS                                                   ASKUSI84
      TABL( 5) = CTSMN                                                  ASKUSI85
      TABL( 6) = CTSMX                                                  ASKUSI86
      TABL( 7) = EFERM                                                  ASKUSI87
      TABL( 8) = ACOLMX                                                 ASKUSI88
      TABL( 9) = SDVRT(1)                                               ASKUSI89
      TABL(10) = SDVRT(2)                                               ASKUSI90
      TABL(11) = SDVRT(3)                                               ASKUSI91
      TABL(12) = VPOS(1)                                                ASKUSI92
      TABL(13) = VPOS(2)                                                ASKUSI93
      TABL(14) = VPOS(3)                                                ASKUSI94
      ITAB(15) = ITCH                                                   ASKUSI95
      ITAB(16) = IWEAK                                                  ASKUSI96
      ITAB(17) = IBOXES                                                 ASKUSI97
      ITAB(18) = IBST                                                   ASKUSI98
      ITAB(19) = ITFRE                                                  ASKUSI99
      NCOL = 19                                                         ASKUS100
      NROW = 1                                                          ASKUS101
      JKPAR = ALTABL('KPAR',NCOL,NROW,TABL,'2I,(14F,5I)','C')           ASKUS102
C                                                                       ASKUS103
C  Fill RLEP bank                                                       ASKUS104
C                                                                       ASKUS105
      IEBEAM = NINT(0.5*ECMS*1000.0)                                    ASKUS106
      JRLEP = ALRLEP(IEBEAM,'    ',0,0,0)                               ASKUS107
C                                                                       ASKUS108
C  Generator initialization                                             ASKUS109
C                                                                       ASKUS110
      MASS1Z = DBLE(AMZ)                                                ASKUS111
      MASS1H = DBLE(AMH)                                                ASKUS112
      MASS1T = DBLE(AMT)                                                ASKUS113
      EBEAM  = DBLE(ECMS) * 0.5D0                                       ASKUS114
      CTSMIN = DBLE(CTSMN)                                              ASKUS115
      CTSMAX = DBLE(CTSMX)                                              ASKUS116
      ECUT   = DBLE(EFERM)                                              ASKUS117
      ACOCUT = DBLE(ACOLMX)                                             ASKUS118
      TCHANN = .TRUE.                                                   ASKUS119
      IF (ITCH .EQ. 0) TCHANN = .FALSE.                                 ASKUS120
      WEAK   = .TRUE.                                                   ASKUS121
      IF (IWEAK .EQ. 0) WEAK = .FALSE.                                  ASKUS122
      BOXES  = .TRUE.                                                   ASKUS123
      IF (IBOXES .EQ. 0) BOXES = .FALSE.                                ASKUS124
      BSTYLE = IBST                                                     ASKUS125
      CALL UBINIT(IDATQQ, VERSQQ)                                       ASKUS126
C                                                                       ASKUS127
C   say hello and print version number and date                         ASKUS128
C                                                                       ASKUS129
      WRITE(NOUT,101) IGCOD ,VERSQQ,IDATQQ                              ASKUS130
 101  FORMAT(/,10X,                                                     ASKUS131
     &       'UBAB02 - CODE NUMBER = ',I4,', VERSION ',A8,', RELEASED ',ASKUS132
     &       I6,/,10X,' Interface last modified on May 9  1996',        ASKUS133
     & /,10X,'********************************************************',ASKUS134
     &       '******',//)                                               ASKUS135
C                                                                       ASKUS136
C   Print PART and KLIN banks                                           ASKUS137
C                                                                       ASKUS138
      CALL PRPART                                                       ASKUS139
      CALL PRTABL('KPAR',0)                                             ASKUS140
      CALL PRTABL('RLEP',0)                                             ASKUS141
C                                                                       ASKUS142
C   Reset the event counters                                            ASKUS143
C                                                                       ASKUS144
      CALL VZERO(NEVCNT, NCNT)                                          ASKUS145
C                                                                       ASKUS146
C   book a few standard histograms                                      ASKUS147
C                                                                       ASKUS148
      CALL HBOOK1(10001, 'COS(THETA-STAR)', 100, -1.0, +1.0, 0.0)       ASKUS149
      CALL HBOOK1(10002, 'ACOLLINEARITY', 90, 0.0, PI, 0.0)             ASKUS150
      CALL HBOOK1(10003, 'NO OF STORED PHOTONS', 100, 0.0, 100.0, 0.0)  ASKUS151
      CALL HBOOK1(10004,                                                ASKUS152
     +  'ANGLE OF HIGHEST MOM PHOTON WRT TO RADIATING LEPTON',          ASKUS153
     +  90, 0.0, PIBY2, 0.0)                                            ASKUS154
      CALL HBOOK1(10005, 'ENERGY OF HIGHEST MOM PHOTON / EBEAM', 50,    ASKUS155
     +  0.0, 1.0, 0.0)                                                  ASKUS156
      CALL HIDOPT(10002,'LOGY')                                         ASKUS157
      CALL HIDOPT(10004,'LOGY')                                         ASKUS158
      CALL HIDOPT(10005,'LOGY')                                         ASKUS159
C                                                                       ASKUS160
      RETURN                                                            ASKUS161
      END                                                               ASKUS162
      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)                 ASKUSE 2
C --------------------------------------------------------------------  ASKUSE 3
C Event generation for UBAB01           as from H. Meinhard October 1993ASKUSE 4
C --------------------------------------------------------------------  ASKUSE 5
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12         ALCONS 2
      REAL RADEG, DEGRA                                                 ALCONS 3
      REAL CLGHT, ALDEDX                                                ALCONS 4
      INTEGER NBITW, NBYTW, LCHAR                                       ALCONS 5
      PARAMETER (PI=3.141592653589)                                     ALCONS 6
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)                          ALCONS 7
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          ALCONS 8
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         ALCONS 9
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         ALCONS10
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)                 ALCONS11
      INTEGER       LBCS, IW                                            BCS    2
      PARAMETER     (LBCS=1000)                                         BCS    3
      COMMON        /BCS/ IW(LBCS)                                      BCS    4
      REAL          RW(LBCS)                                            BCS    5
      EQUIVALENCE   (RW(1),IW(1))                                       BCS    6
      INTEGER       NCNT, NEVCNT, ITFRE                                 CDGENE 2
      PARAMETER     (NCNT = 5)                                          CDGENE 3
      REAL          SDVRT, VPOS                                         CDGENE 4
      COMMON        /CDGENE/ SDVRT(3), VPOS(3), NEVCNT(NCNT), ITFRE     CDGENE 5
      INTEGER       NMXHEP                                              HEPEVT 2
      PARAMETER     (NMXHEP = 2000)                                     HEPEVT 3
      INTEGER       NEVHEP, NHEP, ISTHEP, IDHEP, JMOHEP, JDAHEP         HEPEVT 4
      REAL          PHEP, VHEP, SHEP                                    HEPEVT 5
      COMMON        /HEPEVT/ NEVHEP, NHEP, ISTHEP(NMXHEP),              HEPEVT 6
     +                       IDHEP(NMXHEP), JMOHEP(2,NMXHEP),           HEPEVT 7
     +                       JDAHEP(2,NMXHEP), PHEP(5,NMXHEP),          HEPEVT 8
     +                       VHEP(5,NMXHEP)                             HEPEVT 9
      COMMON        /HEPSPN/ SHEP(5,NMXHEP)                             HEPEVT10
c Quarks                                                                PDGCODE2
      INTEGER PDGDWN, PDGUP, PDGSTR, PDGCHM, PDGBOT, PDGTOP             PDGCODE3
      PARAMETER (PDGDWN =    1, PDGUP  =    2, PDGSTR =    3,           PDGCODE4
     $           PDGCHM =    4, PDGBOT =    5, PDGTOP =    6)           PDGCODE5
c Leptons                                                               PDGCODE6
      INTEGER PDGELE, PDGNUE, PDGMU, PDGNUM, PDGTAU, PDGNUT             PDGCODE7
      PARAMETER (PDGELE =   11, PDGNUE =   12, PDGMU =    13,           PDGCODE8
     $           PDGNUM =   14, PDGTAU =   15, PDGNUT =   16)           PDGCODE9
c Bosons                                                                PDGCOD10
      INTEGER PDGGLU, PDGGLV, PDGGAM, PDGZ0, PDGWPL, PDGH0              PDGCOD11
      PARAMETER (PDGGLU =   21, PDGGLV =    9, PDGGAM =   22,           PDGCOD12
     $           PDGZ0  =   23, PDGWPL =   24, PDGH0  =   25)           PDGCOD13
c Mesons                                                                PDGCOD14
      INTEGER PDGPIP, PDGPI0, PDGETA, PDGKPL, PDGK0, PDGK0S, PDGK0L     PDGCOD15
      PARAMETER (PDGPIP =  211, PDGPI0 =  111, PDGETA =  221,           PDGCOD16
     $           PDGKPL =  321, PDGK0  =  311, PDGK0S =  310,           PDGCOD17
     $           PDGK0L =  130)                                         PDGCOD18
c Baryons                                                               PDGCOD19
      INTEGER PDGPRO, PDGNEU                                            PDGCOD20
      PARAMETER (PDGPRO = 2212, PDGNEU = 2112)                          PDGCOD21
      double precision ahpla                                            UBPCOM 2
      common /ubpcom/  ahpla                                            UBPCOM 3
      double precision alpha                                            UBPCOM 4
      common /ubpcom/  alpha                                            UBPCOM 5
      double precision mass1e                                           UBPCOM 6
      common /ubpcom/  mass1e                                           UBPCOM 7
      double precision mass2e                                           UBPCOM 8
      common /ubpcom/  mass2e                                           UBPCOM 9
      double precision mass1z                                           UBPCOM10
      common /ubpcom/  mass1z                                           UBPCOM11
      double precision mass2z                                           UBPCOM12
      common /ubpcom/  mass2z                                           UBPCOM13
      double precision mass1t                                           UBPCOM14
      common /ubpcom/  mass1t                                           UBPCOM15
      double precision mass1h                                           UBPCOM16
      common /ubpcom/  mass1h                                           UBPCOM17
      double precision mass1w                                           UBPCOM18
      common /ubpcom/  mass1w                                           UBPCOM19
      double precision gamm1z                                           UBPCOM20
      common /ubpcom/  gamm1z                                           UBPCOM21
      double precision gamm2z                                           UBPCOM22
      common /ubpcom/  gamm2z                                           UBPCOM23
      double precision sin2w                                            UBPCOM24
      common /ubpcom/  sin2w                                            UBPCOM25
      double precision alphas                                           UBPCOM26
      common /ubpcom/  alphas                                           UBPCOM27
      double precision ebeam                                            UBPCOM28
      common /ubpcom/  ebeam                                            UBPCOM29
      double precision epol                                             UBPCOM30
      common /ubpcom/  epol                                             UBPCOM31
      double precision ppol                                             UBPCOM32
      common /ubpcom/  ppol                                             UBPCOM33
      double precision ctsmin                                           UBPCOM34
      common /ubpcom/  ctsmin                                           UBPCOM35
      double precision ctsmax                                           UBPCOM36
      common /ubpcom/  ctsmax                                           UBPCOM37
      double precision ecut                                             UBPCOM38
      common /ubpcom/  ecut                                             UBPCOM39
      double precision acocut                                           UBPCOM40
      common /ubpcom/  acocut                                           UBPCOM41
      double precision evisct                                           UBPCOM42
      common /ubpcom/  evisct                                           UBPCOM43
      integer          nevent, pad022                                   UBPCOM44
      common /ubpcom/  nevent, pad022                                   UBPCOM45
      integer          rseed , pad023                                   UBPCOM46
      common /ubpcom/  rseed , pad023                                   UBPCOM47
      logical          tchann, pad024                                   UBPCOM48
      common /ubpcom/  tchann, pad024                                   UBPCOM49
      logical          qedvtx, pad025                                   UBPCOM50
      common /ubpcom/  qedvtx, pad025                                   UBPCOM51
      logical          qedbox, pad026                                   UBPCOM52
      common /ubpcom/  qedbox, pad026                                   UBPCOM53
      logical          weak  , pad027                                   UBPCOM54
      common /ubpcom/  weak  , pad027                                   UBPCOM55
      logical          boxes , pad028                                   UBPCOM56
      common /ubpcom/  boxes , pad028                                   UBPCOM57
      integer          isrtyp, pad029                                   UBPCOM58
      common /ubpcom/  isrtyp, pad029                                   UBPCOM59
      integer          fsrtyp, pad030                                   UBPCOM60
      common /ubpcom/  fsrtyp, pad030                                   UBPCOM61
      double precision epsiln                                           UBPCOM62
      common /ubpcom/  epsiln                                           UBPCOM63
      double precision taumin                                           UBPCOM64
      common /ubpcom/  taumin                                           UBPCOM65
      double precision taumax                                           UBPCOM66
      common /ubpcom/  taumax                                           UBPCOM67
      double precision emin                                             UBPCOM68
      common /ubpcom/  emin                                             UBPCOM69
      double precision eps2                                             UBPCOM70
      common /ubpcom/  eps2                                             UBPCOM71
      double precision qsq                                              UBPCOM72
      common /ubpcom/  qsq                                              UBPCOM73
      integer          bstyle, pad037                                   UBPCOM74
      common /ubpcom/  bstyle, pad037                                   UBPCOM75
      logical          nonlog, pad038                                   UBPCOM76
      common /ubpcom/  nonlog, pad038                                   UBPCOM77
      integer          stdin , pad039                                   UBPCOM78
      common /ubpcom/  stdin , pad039                                   UBPCOM79
      integer          stdout, pad040                                   UBPCOM80
      common /ubpcom/  stdout, pad040                                   UBPCOM81
      integer          stderr, pad041                                   UBPCOM82
      common /ubpcom/  stderr, pad041                                   UBPCOM83
      integer          errcnt, pad042                                   UBPCOM84
      common /ubpcom/  errcnt, pad042                                   UBPCOM85
      integer          errmax, pad043                                   UBPCOM86
      common /ubpcom/  errmax, pad043                                   UBPCOM87
      integer          verbos, pad044                                   UBPCOM88
      common /ubpcom/  verbos, pad044                                   UBPCOM89
      integer          status, pad045                                   UBPCOM90
      common /ubpcom/  status, pad045                                   UBPCOM91
      integer          hepdat, pad046                                   UBPCOM92
      common /ubpcom/  hepdat, pad046                                   UBPCOM93
      integer          heprev, pad047                                   UBPCOM94
      common /ubpcom/  heprev, pad047                                   UBPCOM95
      integer          runid , pad048                                   UBPCOM96
      common /ubpcom/  runid , pad048                                   UBPCOM97
      logical          dbgcol, pad049                                   UBPCOM98
      common /ubpcom/  dbgcol, pad049                                   UBPCOM99
      logical          dbghep, pad050                                   UBPCO100
      common /ubpcom/  dbghep, pad050                                   UBPCO101
      logical          dbgini, pad051                                   UBPCO102
      common /ubpcom/  dbgini, pad051                                   UBPCO103
      logical          dbgmas, pad052                                   UBPCO104
      common /ubpcom/  dbgmas, pad052                                   UBPCO105
      logical          dbgmup, pad053                                   UBPCO106
      common /ubpcom/  dbgmup, pad053                                   UBPCO107
      DOUBLE PRECISION BRNMAX                                           UBPCO108
      COMMON /UBCBRN/  BRNMAX                                           UBPCO109
      double precision maxws, maxwt,                                    UBPCO110
     &                 maxwnl, wnlvs, wnlhrd, wnlavh, wnlbare           UBPCO111
      common /ubcsta/  maxws, maxwt,                                    UBPCO112
     &                 maxwnl, wnlvs, wnlhrd, wnlavh, wnlbare           UBPCO113
                                                                        UBPCO114
      integer          isbad, iscall, isfail, isrej, callct, acccut     UBPCO115
      common /ubcsta/  isbad, iscall, isfail, isrej, callct, acccut     UBPCO116
                                                                        UBPCO117
      integer          itbad, itcall, itfail, nlbad, nlcall, nlfail     UBPCO118
      common /ubcsta/  itbad, itcall, itfail, nlbad, nlcall, nlfail     UBPCO119
                                                                        UBPCO120
      integer          nlvs, nlhrd                                      UBPCO121
      common /ubcsta/  nlvs, nlhrd                                      UBPCO122
      INTEGER       IDPR, ISTA, NTRK, NVRT, ITAB(NMXHEP), ALTABL, IVMAI,ASKUSE 7
     +              JVERT, KBVERT, IHEP, IDIM, JKINE, KBKINE, JKHIS,    ASKUSE 8
     +              JKSUM, NEGMX, NINEL, NINPO, NOUTEL, NOUTPO          ASKUSE 9
      REAL          ECMS, WEIT, VRTEX(4), TABL(17), RN1, RN2, RN3, DUM, ASKUSE10
     +              EGMX, COSTS, ACOLL, THETE, THETP, DUM2, DUM3,       ASKUSE11
     +              ACIE, ACIP, ACOE, ACOP, ANGMX                       ASKUSE12
      EXTERNAL      ALTABL, KBVERT, KBKINE                              ASKUSE13
      EQUIVALENCE   (TABL(1), ITAB(1))                                  ASKUSE14
C --------------------------------------------------------------------  ASKUSE15
C                                                                       ASKUSE16
C   increment counter                                                   ASKUSE17
C                                                                       ASKUSE18
      NEVCNT(1) = NEVCNT(1) + 1                                         ASKUSE19
C                                                                       ASKUSE20
C   Generate primary vertex                                             ASKUSE21
C                                                                       ASKUSE22
      CALL RANNOR(RN1, RN2)                                             ASKUSE23
      CALL RANNOR(RN3, DUM)                                             ASKUSE24
      VRTEX(1) = VPOS(1) + RN1*SDVRT(1)                                 ASKUSE25
      VRTEX(2) = VPOS(2) + RN2*SDVRT(2)                                 ASKUSE26
      VRTEX(3) = VPOS(3) + RN3*SDVRT(3)                                 ASKUSE27
      VRTEX(4) = 0.0                                                    ASKUSE28
C                                                                       ASKUSE29
C   Fill 'VERT' bank                                                    ASKUSE30
C                                                                       ASKUSE31
      IVMAI = 1                                                         ASKUSE32
      JVERT = KBVERT(IVMAI, VRTEX, 0)                                   ASKUSE33
      IF (JVERT .EQ. 0) GO TO 98                                        ASKUSE34
C                                                                       ASKUSE35
C   Event generation                                                    ASKUSE36
C                                                                       ASKUSE37
      NEVCNT(2) = NEVCNT(2) + 1                                         ASKUSE38
      CALL UBGEN(NEVCNT(2))                                             ASKUSE39
C                                                                       ASKUSE40
C   set return values                                                   ASKUSE41
C                                                                       ASKUSE42
      IDPR = 0                                                          ASKUSE43
      ISTA = 0                                                          ASKUSE44
C   NTRK is increased later on by the no of stored photons              ASKUSE45
      NTRK = 2                                                          ASKUSE46
      NVRT = 1                                                          ASKUSE47
      ECMS = 2.0 * REAL(EBEAM)                                          ASKUSE48
      WEIT = 1.0                                                        ASKUSE49
C                                                                       ASKUSE50
C   scan HEPEVT common for particles to be stored                       ASKUSE51
C                                                                       ASKUSE52
      NINEL  = 0                                                        ASKUSE53
      NINPO  = 0                                                        ASKUSE54
      NOUTEL = 0                                                        ASKUSE55
      NOUTPO = 0                                                        ASKUSE56
      NEGMX  = 0                                                        ASKUSE57
      EGMX   = -999.99                                                  ASKUSE58
      DO 310 IHEP = 1, NHEP                                             ASKUSE59
        DO 300 IDIM = 1, 3                                              ASKUSE60
          TABL(IDIM) = PHEP(IDIM,IHEP)                                  ASKUSE61
  300   CONTINUE                                                        ASKUSE62
C   Let KBKINE calculate the energy of the particle                     ASKUSE63
        TABL(4) = 0.0                                                   ASKUSE64
        IF (ISTHEP(IHEP) .EQ. 102 .AND. IDHEP(IHEP) .EQ. -PDGELE) THEN  ASKUSE65
C   incoming positron                                                   ASKUSE66
          NINPO  = IHEP                                                 ASKUSE67
          JKINE  = KBKINE(-1, TABL, 2, 0)                               ASKUSE68
          IF (JKINE .EQ. 0) GO TO 98                                    ASKUSE69
        ELSE IF (ISTHEP(IHEP) .EQ. 101 .AND. IDHEP(IHEP) .EQ. PDGELE)   ASKUSE70
     +    THEN                                                          ASKUSE71
C   incoming electron                                                   ASKUSE72
          NINEL  = IHEP                                                 ASKUSE73
          JKINE  = KBKINE(-2, TABL, 3, 0)                               ASKUSE74
          IF (JKINE .EQ. 0) GO TO 98                                    ASKUSE75
        ELSE IF (ISTHEP(IHEP) .EQ. 1 .AND. IDHEP(IHEP) .EQ. -PDGELE)    ASKUSE76
     +    THEN                                                          ASKUSE77
C   outgoing positron                                                   ASKUSE78
          NOUTPO = IHEP                                                 ASKUSE79
          JKINE  = KBKINE(1, TABL, 2, IVMAI)                            ASKUSE80
          IF (JKINE .EQ. 0) GO TO 98                                    ASKUSE81
        ELSE IF (ISTHEP(IHEP) .EQ. 1 .AND. IDHEP(IHEP) .EQ. PDGELE) THENASKUSE82
C   outgoing electron                                                   ASKUSE83
          NOUTEL = IHEP                                                 ASKUSE84
          JKINE  = KBKINE(2, TABL, 3, IVMAI)                            ASKUSE85
          IF (JKINE .EQ. 0) GO TO 98                                    ASKUSE86
        ELSE IF (ISTHEP(IHEP) .EQ. 1 .AND. IDHEP(IHEP) .EQ. PDGGAM) THENASKUSE87
C   outgoing photon - require a reasonable minimum energy               ASKUSE88
          IF (PHEP(4,IHEP) .GT. 1.0E-6) THEN                            ASKUSE89
            IF (PHEP(4,IHEP) .GT. EGMX) THEN                            ASKUSE90
              EGMX  = PHEP(4,IHEP)                                      ASKUSE91
              NEGMX = IHEP                                              ASKUSE92
            END IF                                                      ASKUSE93
            NTRK   = NTRK + 1                                           ASKUSE94
            JKINE  = KBKINE(NTRK, TABL, 1, IVMAI)                       ASKUSE95
            IF (JKINE .EQ. 0) GO TO 98                                  ASKUSE96
          END IF                                                        ASKUSE97
        ELSE                                                            ASKUSE98
C   intermediate particle - don't store                                 ASKUSE99
        END IF                                                          ASKUS100
  310 CONTINUE                                                          ASKUS101
C                                                                       ASKUS102
C   check whether we found all relevant particles                       ASKUS103
C                                                                       ASKUS104
      IF (NINPO*NINEL*NOUTPO*NOUTEL .EQ. 0) GO TO 99                    ASKUS105
C                                                                       ASKUS106
C     update process number as the number of photons                    ASKUS107
C                                                                       ASKUS108
      IDPR = NTRK-2                                                     ASKUS109
C                                                                       ASKUS110
C   Fill history with 'KHIS' bank                                       ASKUS111
C                                                                       ASKUS112
      CALL VZERO(ITAB, NTRK)                                            ASKUS113
      JKHIS = ALTABL('KHIS', 1, NTRK, ITAB, 'I', 'E')                   ASKUS114
      IF (JKHIS .EQ. 0) GO TO 98                                        ASKUS115
C                                                                       ASKUS116
C   write a summary bank, if appropriate                                ASKUS117
C                                                                       ASKUS118
      IF (MOD(NEVCNT(2), ITFRE) .EQ. 0) THEN                            ASKUS119
        CALL UBCLOS(NEVCNT(2))                                          ASKUS120
        TABL( 1) = PHEP(1,1)                                            ASKUS121
        TABL( 2) = PHEP(1,2)                                            ASKUS122
        TABL( 3) = REAL(BRNMAX)                                         ASKUS123
cb        TABL( 4) = REAL(SUMW)                                         ASKUS124
cb        TABL( 5) = REAL(SUMW2)                                        ASKUS125
        TABL( 4) = 0.                                                   ASKUS126
        TABL( 5) = 0.                                                   ASKUS127
        TABL( 6) = REAL(MAXWS)                                          ASKUS128
        TABL( 7) = REAL(MAXWT)                                          ASKUS129
        ITAB( 8) = ISTHEP(1)                                            ASKUS130
        ITAB( 9) = ISBAD                                                ASKUS131
        ITAB(10) = ISCALL                                               ASKUS132
        ITAB(11) = ISFAIL                                               ASKUS133
        ITAB(12) = ITBAD                                                ASKUS134
        ITAB(13) = ITCALL                                               ASKUS135
        ITAB(14) = ITFAIL                                               ASKUS136
        ITAB(15) = ISREJ                                                ASKUS137
        ITAB(16) = CALLCT                                               ASKUS138
        ITAB(17) = ACCCUT                                               ASKUS139
        JKSUM = ALTABL('KSUM', 1, 17, TABL, '2I,(7F,10I)', 'E')         ASKUS140
        IF (JKSUM .EQ. 0) GO TO 98                                      ASKUS141
      END IF                                                            ASKUS142
C                                                                       ASKUS143
C   fill histograms                                                     ASKUS144
C                                                                       ASKUS145
      CALL ANGLES(PHEP(1,NOUTEL), PHEP(1,NOUTPO), COSTS, ACOLL,         ASKUS146
     +            THETE, THETP)                                         ASKUS147
      CALL HFILL(10001, COSTS, 0.0, 1.0)                                ASKUS148
      CALL HFILL(10002, ACOLL, 0.0, 1.0)                                ASKUS149
      CALL HFILL(10003, REAL(NTRK-2) + 0.5, 0.0, 1.0)                   ASKUS150
      IF (NEGMX .GT. 0) THEN                                            ASKUS151
        CALL ANGLES(PHEP(1,NINEL),  PHEP(1,NEGMX),  DUM,   ACIE,        ASKUS152
     +              DUM2,  DUM3)                                        ASKUS153
        CALL ANGLES(PHEP(1,NINPO),  PHEP(1,NEGMX),  DUM,   ACIP,        ASKUS154
     +              DUM2,  DUM3)                                        ASKUS155
        CALL ANGLES(PHEP(1,NOUTEL), PHEP(1,NEGMX),  DUM,   ACOE,        ASKUS156
     +              DUM2,  DUM3)                                        ASKUS157
        CALL ANGLES(PHEP(1,NOUTPO), PHEP(1,NEGMX),  DUM,   ACOP,        ASKUS158
     +              DUM2,  DUM3)                                        ASKUS159
        ANGMX = PI - MAX(ACIE, ACIP, ACOE, ACOP)                        ASKUS160
        CALL HFILL(10004, ANGMX, 0.0, 1.0)                              ASKUS161
        CALL HFILL(10005, EGMX / REAL(EBEAM), 0.0, 1.0)                 ASKUS162
      END IF                                                            ASKUS163
C                                                                       ASKUS164
C   that's all for the normal event                                     ASKUS165
C                                                                       ASKUS166
      NEVCNT(3) = NEVCNT(3) + 1                                         ASKUS167
      GO TO 999                                                         ASKUS168
C                                                                       ASKUS169
C   error processing                                                    ASKUS170
C                                                                       ASKUS171
C   not enough space to book new bank                                   ASKUS172
   98 CONTINUE                                                          ASKUS173
      ISTA = 1                                                          ASKUS174
      NEVCNT(4) = NEVCNT(4) + 1                                         ASKUS175
      GO TO 999                                                         ASKUS176
C                                                                       ASKUS177
C   not all particles found in /HEPEVT/                                 ASKUS178
   99 CONTINUE                                                          ASKUS179
      ISTA = 2                                                          ASKUS180
      NEVCNT(5) = NEVCNT(5) + 1                                         ASKUS181
      GO TO 999                                                         ASKUS182
C                                                                       ASKUS183
  999 CONTINUE                                                          ASKUS184
      RETURN                                                            ASKUS185
      END                                                               ASKUS186
      SUBROUTINE USCJOB                                                 USCJOB 2
C --------------------------------------------------------------------  USCJOB 3
C Job termination for UBAB01        as from  H. Meinhard October 1993   USCJOB 4
C --------------------------------------------------------------------  USCJOB 5
      INTEGER       NCNT, NEVCNT, ITFRE                                 CDGENE 2
      PARAMETER     (NCNT = 5)                                          CDGENE 3
      REAL          SDVRT, VPOS                                         CDGENE 4
      COMMON        /CDGENE/ SDVRT(3), VPOS(3), NEVCNT(NCNT), ITFRE     CDGENE 5
      INTEGER       NINP, NOUT, NOUT2                                   INOUT  2
      COMMON        /INOUT/ NINP, NOUT, NOUT2                           INOUT  3
      INTEGER       NMXHEP                                              HEPEVT 2
      PARAMETER     (NMXHEP = 2000)                                     HEPEVT 3
      INTEGER       NEVHEP, NHEP, ISTHEP, IDHEP, JMOHEP, JDAHEP         HEPEVT 4
      REAL          PHEP, VHEP, SHEP                                    HEPEVT 5
      COMMON        /HEPEVT/ NEVHEP, NHEP, ISTHEP(NMXHEP),              HEPEVT 6
     +                       IDHEP(NMXHEP), JMOHEP(2,NMXHEP),           HEPEVT 7
     +                       JDAHEP(2,NMXHEP), PHEP(5,NMXHEP),          HEPEVT 8
     +                       VHEP(5,NMXHEP)                             HEPEVT 9
      COMMON        /HEPSPN/ SHEP(5,NMXHEP)                             HEPEVT10
      double precision ahpla                                            UBPCOM 2
      common /ubpcom/  ahpla                                            UBPCOM 3
      double precision alpha                                            UBPCOM 4
      common /ubpcom/  alpha                                            UBPCOM 5
      double precision mass1e                                           UBPCOM 6
      common /ubpcom/  mass1e                                           UBPCOM 7
      double precision mass2e                                           UBPCOM 8
      common /ubpcom/  mass2e                                           UBPCOM 9
      double precision mass1z                                           UBPCOM10
      common /ubpcom/  mass1z                                           UBPCOM11
      double precision mass2z                                           UBPCOM12
      common /ubpcom/  mass2z                                           UBPCOM13
      double precision mass1t                                           UBPCOM14
      common /ubpcom/  mass1t                                           UBPCOM15
      double precision mass1h                                           UBPCOM16
      common /ubpcom/  mass1h                                           UBPCOM17
      double precision mass1w                                           UBPCOM18
      common /ubpcom/  mass1w                                           UBPCOM19
      double precision gamm1z                                           UBPCOM20
      common /ubpcom/  gamm1z                                           UBPCOM21
      double precision gamm2z                                           UBPCOM22
      common /ubpcom/  gamm2z                                           UBPCOM23
      double precision sin2w                                            UBPCOM24
      common /ubpcom/  sin2w                                            UBPCOM25
      double precision alphas                                           UBPCOM26
      common /ubpcom/  alphas                                           UBPCOM27
      double precision ebeam                                            UBPCOM28
      common /ubpcom/  ebeam                                            UBPCOM29
      double precision epol                                             UBPCOM30
      common /ubpcom/  epol                                             UBPCOM31
      double precision ppol                                             UBPCOM32
      common /ubpcom/  ppol                                             UBPCOM33
      double precision ctsmin                                           UBPCOM34
      common /ubpcom/  ctsmin                                           UBPCOM35
      double precision ctsmax                                           UBPCOM36
      common /ubpcom/  ctsmax                                           UBPCOM37
      double precision ecut                                             UBPCOM38
      common /ubpcom/  ecut                                             UBPCOM39
      double precision acocut                                           UBPCOM40
      common /ubpcom/  acocut                                           UBPCOM41
      double precision evisct                                           UBPCOM42
      common /ubpcom/  evisct                                           UBPCOM43
      integer          nevent, pad022                                   UBPCOM44
      common /ubpcom/  nevent, pad022                                   UBPCOM45
      integer          rseed , pad023                                   UBPCOM46
      common /ubpcom/  rseed , pad023                                   UBPCOM47
      logical          tchann, pad024                                   UBPCOM48
      common /ubpcom/  tchann, pad024                                   UBPCOM49
      logical          qedvtx, pad025                                   UBPCOM50
      common /ubpcom/  qedvtx, pad025                                   UBPCOM51
      logical          qedbox, pad026                                   UBPCOM52
      common /ubpcom/  qedbox, pad026                                   UBPCOM53
      logical          weak  , pad027                                   UBPCOM54
      common /ubpcom/  weak  , pad027                                   UBPCOM55
      logical          boxes , pad028                                   UBPCOM56
      common /ubpcom/  boxes , pad028                                   UBPCOM57
      integer          isrtyp, pad029                                   UBPCOM58
      common /ubpcom/  isrtyp, pad029                                   UBPCOM59
      integer          fsrtyp, pad030                                   UBPCOM60
      common /ubpcom/  fsrtyp, pad030                                   UBPCOM61
      double precision epsiln                                           UBPCOM62
      common /ubpcom/  epsiln                                           UBPCOM63
      double precision taumin                                           UBPCOM64
      common /ubpcom/  taumin                                           UBPCOM65
      double precision taumax                                           UBPCOM66
      common /ubpcom/  taumax                                           UBPCOM67
      double precision emin                                             UBPCOM68
      common /ubpcom/  emin                                             UBPCOM69
      double precision eps2                                             UBPCOM70
      common /ubpcom/  eps2                                             UBPCOM71
      double precision qsq                                              UBPCOM72
      common /ubpcom/  qsq                                              UBPCOM73
      integer          bstyle, pad037                                   UBPCOM74
      common /ubpcom/  bstyle, pad037                                   UBPCOM75
      logical          nonlog, pad038                                   UBPCOM76
      common /ubpcom/  nonlog, pad038                                   UBPCOM77
      integer          stdin , pad039                                   UBPCOM78
      common /ubpcom/  stdin , pad039                                   UBPCOM79
      integer          stdout, pad040                                   UBPCOM80
      common /ubpcom/  stdout, pad040                                   UBPCOM81
      integer          stderr, pad041                                   UBPCOM82
      common /ubpcom/  stderr, pad041                                   UBPCOM83
      integer          errcnt, pad042                                   UBPCOM84
      common /ubpcom/  errcnt, pad042                                   UBPCOM85
      integer          errmax, pad043                                   UBPCOM86
      common /ubpcom/  errmax, pad043                                   UBPCOM87
      integer          verbos, pad044                                   UBPCOM88
      common /ubpcom/  verbos, pad044                                   UBPCOM89
      integer          status, pad045                                   UBPCOM90
      common /ubpcom/  status, pad045                                   UBPCOM91
      integer          hepdat, pad046                                   UBPCOM92
      common /ubpcom/  hepdat, pad046                                   UBPCOM93
      integer          heprev, pad047                                   UBPCOM94
      common /ubpcom/  heprev, pad047                                   UBPCOM95
      integer          runid , pad048                                   UBPCOM96
      common /ubpcom/  runid , pad048                                   UBPCOM97
      logical          dbgcol, pad049                                   UBPCOM98
      common /ubpcom/  dbgcol, pad049                                   UBPCOM99
      logical          dbghep, pad050                                   UBPCO100
      common /ubpcom/  dbghep, pad050                                   UBPCO101
      logical          dbgini, pad051                                   UBPCO102
      common /ubpcom/  dbgini, pad051                                   UBPCO103
      logical          dbgmas, pad052                                   UBPCO104
      common /ubpcom/  dbgmas, pad052                                   UBPCO105
      logical          dbgmup, pad053                                   UBPCO106
      common /ubpcom/  dbgmup, pad053                                   UBPCO107
      DOUBLE PRECISION BRNMAX                                           UBPCO108
      COMMON /UBCBRN/  BRNMAX                                           UBPCO109
      double precision maxws, maxwt,                                    UBPCO110
     &                 maxwnl, wnlvs, wnlhrd, wnlavh, wnlbare           UBPCO111
      common /ubcsta/  maxws, maxwt,                                    UBPCO112
     &                 maxwnl, wnlvs, wnlhrd, wnlavh, wnlbare           UBPCO113
                                                                        UBPCO114
      integer          isbad, iscall, isfail, isrej, callct, acccut     UBPCO115
      common /ubcsta/  isbad, iscall, isfail, isrej, callct, acccut     UBPCO116
                                                                        UBPCO117
      integer          itbad, itcall, itfail, nlbad, nlcall, nlfail     UBPCO118
      common /ubcsta/  itbad, itcall, itfail, nlbad, nlcall, nlfail     UBPCO119
                                                                        UBPCO120
      integer          nlvs, nlhrd                                      UBPCO121
      common /ubcsta/  nlvs, nlhrd                                      UBPCO122
      INTEGER       ITAB(17), ALTABL, JKSUM                             USCJOB 7
      REAL          TABL(17)                                            USCJOB 8
      EXTERNAL      ALTABL                                              USCJOB 9
      EQUIVALENCE   (TABL(1), ITAB(1))                                  USCJOB10
C --------------------------------------------------------------------  USCJOB11
C                                                                       USCJOB12
C   call generator termination                                          USCJOB13
C                                                                       USCJOB14
      CALL UBCLOS(NEVCNT(2))                                            USCJOB15
C                                                                       USCJOB16
C   write a summary bank, if appropriate. However, I'm afraid this      USCJOB17
C   bank will never appear on the output because of the hardwiring      USCJOB18
C   in the KEEND routine (ALEPHLIB).                                    USCJOB19
C                                                                       USCJOB20
      TABL( 1) = PHEP(1,1)                                              USCJOB21
      TABL( 2) = PHEP(1,2)                                              USCJOB22
      TABL( 3) = REAL(BRNMAX)                                           USCJOB23
CB      TABL( 4) = REAL(SUMW)                                           USCJOB24
CB      TABL( 5) = REAL(SUMW2)                                          USCJOB25
      TABL( 4) = 0.                                                     USCJOB26
      TABL( 5) = 0.                                                     USCJOB27
      TABL( 6) = REAL(MAXWS)                                            USCJOB28
      TABL( 7) = REAL(MAXWT)                                            USCJOB29
      ITAB( 8) = ISTHEP(1)                                              USCJOB30
      ITAB( 9) = ISBAD                                                  USCJOB31
      ITAB(10) = ISCALL                                                 USCJOB32
      ITAB(11) = ISFAIL                                                 USCJOB33
      ITAB(12) = ITBAD                                                  USCJOB34
      ITAB(13) = ITCALL                                                 USCJOB35
      ITAB(14) = ITFAIL                                                 USCJOB36
      ITAB(15) = ISREJ                                                  USCJOB37
      ITAB(16) = CALLCT                                                 USCJOB38
      ITAB(17) = ACCCUT                                                 USCJOB39
      JKSUM = ALTABL('KSUM', 1, 17, TABL, '2I,(7F,10I)', 'C')           USCJOB40
      IF (JKSUM .EQ. 0) NEVCNT(4) = NEVCNT(4) + 1                       USCJOB41
C                                                                       USCJOB42
C   write current cross section status                                  USCJOB43
C                                                                       USCJOB44
      WRITE (NOUT, 103)                                                 USCJOB45
  103 FORMAT (//20X,'X-SECTION STATISTICS',                             USCJOB46
     +         /20X,'********************')                             USCJOB47
      WRITE (NOUT, 104) (TABL(II)*1.E06,II=1,2),TABL(3)*1.E-03,         USCJOB48
     $                  (TABL(II),II=4,7),(ITAB(II),II=8,17)            USCJOB49
  104 FORMAT (/5X,'Generated x-section (nbarn)             = ',F10.4,   USCJOB50
     &        /5X,'Error on generated x-section ( nbarn)   = ',F10.4,   USCJOB51
     &        /5X,'Maximum  Born x-section      ( nbarn)   = ',F10.4,   USCJOB52
     &        /5X,'Sum of events weights                   = ',F10.4,   USCJOB53
     &        /5X,'Sum of squares of events weights        = ',F10.4,   USCJOB54
     &        /5X,'Maximum weight in s rejection           = ',F10.4,   USCJOB55
     &        /5X,'Maximum weight in t rejection           = ',F10.4,   USCJOB56
     &        /5X,'# of events generated                   = ',I10,     USCJOB57
     &        /5X,'# of events with too large s weight     = ',I10,     USCJOB58
     &        /5X,'# of tries in s generation              = ',I10,     USCJOB59
     &        /5X,'# of events w/ unphysical E hard scatter= ',I10,     USCJOB60
     &        /5X,'# of events with too large t weight     = ',I10,     USCJOB61
     &        /5X,'# of tries in t generation              = ',I10,     USCJOB62
     &        /5X,'# of events rejected in t loop          = ',I10,     USCJOB63
     &        /5X,'# of events rejected in s loop          = ',I10,     USCJOB64
     &        /5X,'# of events passing all internal cuts   = ',I10,     USCJOB65
     &        /5X,'# of events after all int. & ext. cuts  = ',I10)     USCJOB66
C                                                                       USCJOB67
C   write statistics                                                    USCJOB68
C                                                                       USCJOB69
      WRITE (NOUT, 101)                                                 USCJOB70
  101 FORMAT (//20X,'RECORD STATISTICS',                                USCJOB71
     +         /20X,'*****************')                                USCJOB72
      WRITE (NOUT, 102) NEVCNT                                          USCJOB73
  102 FORMAT (/5X,'# of calls to ASKUSE                    = ',I10,     USCJOB74
     &        /5X,'# of calls to UBGEN - evt generation    = ',I10,     USCJOB75
     &        /5X,'# of error-free events                  = ',I10,     USCJOB76
     &        /5X,'# of records with BOS bank space pb     = ',I10,     USCJOB77
     &        /5X,'# of records with inconsistent HEPEVT   = ',I10)     USCJOB78
C                                                                       USCJOB79
C That's it                                                             USCJOB80
C                                                                       USCJOB81
      RETURN                                                            USCJOB82
      END                                                               USCJOB83
      SUBROUTINE ANGLES(PE,PP,COSTS,ACOLL,THETE,THETP)                  ANGLES 2
C---------------------------------------------------------------------- ANGLES 3
C! Calculate cos(theta-star) and acollinearity                          ANGLES 4
C!                                                                      ANGLES 5
C!    Author:     H. Meinhard       30-Jun-1993                         ANGLES 6
C!                                                                      ANGLES 7
C!    Input:      - PE(3)     /R    electron 3-momentum                 ANGLES 8
C!                - PP(3)     /R    positron 3-momentum                 ANGLES 9
C!    Output:     - COSTS     /R    cos(theta-star)                     ANGLES10
C!                - ACOLL     /R    acollinearity                       ANGLES11
C!                - THETE     /R    electron theta                      ANGLES12
C!                - THETP     /R    positron theta                      ANGLES13
C!                                                                      ANGLES14
C---------------------------------------------------------------------- ANGLES15
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12         ALCONS 2
      REAL RADEG, DEGRA                                                 ALCONS 3
      REAL CLGHT, ALDEDX                                                ALCONS 4
      INTEGER NBITW, NBYTW, LCHAR                                       ALCONS 5
      PARAMETER (PI=3.141592653589)                                     ALCONS 6
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)                          ALCONS 7
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          ALCONS 8
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         ALCONS 9
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         ALCONS10
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)                 ALCONS11
      REAL        PE(*), PP(*), COSTS, ACOLL, THETE, THETP              ANGLES17
      REAL        PEPP, PEPE, PPPP, COSEP                               ANGLES18
      INTEGER     I                                                     ANGLES19
C---------------------------------------------------------------------- ANGLES20
      IF (PE(3) .EQ. 0.0) THEN                                          ANGLES21
        THETE = PIBY2                                                   ANGLES22
      ELSE                                                              ANGLES23
        THETE = ATAN(SQRT(PE(1)**2+PE(2)**2) / PE(3))                   ANGLES24
        IF (THETE .LT. 0.0) THETE = THETE + PI                          ANGLES25
      END IF                                                            ANGLES26
      IF (PP(3) .EQ. 0.0) THEN                                          ANGLES27
        THETP = PIBY2                                                   ANGLES28
      ELSE                                                              ANGLES29
        THETP = ATAN(SQRT(PP(1)**2+PP(2)**2) / PP(3))                   ANGLES30
        IF (THETP .LT. 0.0) THETP = THETP + PI                          ANGLES31
      END IF                                                            ANGLES32
      COSTS = COS(0.5*(THETE+PI-THETP)) / COS(0.5*(THETE-PI+THETP))     ANGLES33
      PEPP  = 0.0                                                       ANGLES34
      PEPE  = 0.0                                                       ANGLES35
      PPPP  = 0.0                                                       ANGLES36
      DO 300 I = 1, 3                                                   ANGLES37
        PEPP = PEPP + PE(I) * PP(I)                                     ANGLES38
        PEPE = PEPE + PE(I) * PE(I)                                     ANGLES39
        PPPP = PPPP + PP(I) * PP(I)                                     ANGLES40
  300 CONTINUE                                                          ANGLES41
      COSEP = PEPP / SQRT(PEPE*PPPP)                                    ANGLES42
      COSEP = MAX(COSEP, -1.0)                                          ANGLES43
      COSEP = MIN(COSEP, +1.0)                                          ANGLES44
      ACOLL = PI - ACOS(COSEP)                                          ANGLES45
      END                                                               ANGLES46
      SUBROUTINE USKRIN(ECM)                                            USKRIN 2
      REAL ECM                                                          USKRIN 3
      END                                                               USKRIN 4
      REAL FUNCTION XKSECT(ECM)                                         XKSECT 2
      REAL ECM                                                          XKSECT 3
      XKSECT = 0.0                                                      XKSECT 4
      END                                                               XKSECT 5
