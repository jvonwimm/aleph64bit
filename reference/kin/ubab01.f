      SUBROUTINE ASKUSI(IGCOD)                                          ASKUSI 2
C --------------------------------------------------------------------  ASKUSI 3
C Initialization for UBAB01                H. Meinhard October 1993     ASKUSI 4
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
      DOUBLE PRECISION AHPLA , ALPHA , MASS1E, MASS2E, MASS1Z, MASS2Z,  UBPCOM 2
     +                 MASS1T, MASS1H, MASS1W, GAMM1Z, GAMM2Z, SIN2W ,  UBPCOM 3
     +                 EBEAM , CTSMIN, CTSMAX, ECUT  , ACOCUT           UBPCOM 4
      INTEGER          NEVENT, PAD018, RSEED , PAD019                   UBPCOM 5
      LOGICAL          TCHANN, PAD020, WEAK  , PAD021, BOXES , PAD022   UBPCOM 6
      DOUBLE PRECISION TAUMIN, TAUMAX, EMIN  , EPSILN, QSQ              UBPCOM 7
      INTEGER          BSTYLE, PAD028, STDIN , PAD029, STDOUT, PAD030,  UBPCOM 8
     +                 STDERR, PAD031, ERRCNT, PAD032, ERRMAX, PAD033,  UBPCOM 9
     +                 VERBOS, PAD034, STATUS, PAD035, HEPDAT, PAD036,  UBPCOM10
     +                 HEPREV, PAD037, RUNID , PAD038                   UBPCOM11
      LOGICAL          DBGCOL, PAD039, DBGHEP, PAD040, DBGINI, PAD041,  UBPCOM12
     +                 DBGMUP, PAD042, DBGSCA, PAD043                   UBPCOM13
      COMMON /UBPCOM/  AHPLA , ALPHA , MASS1E, MASS2E, MASS1Z, MASS2Z,  UBPCOM14
     +                 MASS1T, MASS1H, MASS1W, GAMM1Z, GAMM2Z, SIN2W ,  UBPCOM15
     +                 EBEAM , CTSMIN, CTSMAX, ECUT  , ACOCUT,          UBPCOM16
     +                 NEVENT, PAD018, RSEED , PAD019,                  UBPCOM17
     +                 TCHANN, PAD020, WEAK  , PAD021, BOXES , PAD022,  UBPCOM18
     +                 TAUMIN, TAUMAX, EMIN  , EPSILN, QSQ   ,          UBPCOM19
     +                 BSTYLE, PAD028, STDIN , PAD029, STDOUT, PAD030,  UBPCOM20
     +                 STDERR, PAD031, ERRCNT, PAD032, ERRMAX, PAD033,  UBPCOM21
     +                 VERBOS, PAD034, STATUS, PAD035, HEPDAT, PAD036,  UBPCOM22
     +                 HEPREV, PAD037, RUNID , PAD038,                  UBPCOM23
     +                 DBGCOL, PAD039, DBGHEP, PAD040, DBGINI, PAD041,  UBPCOM24
     +                 DBGMUP, PAD042, DBGSCA, PAD043                   UBPCOM25
      DOUBLE PRECISION BRNMAX                                           UBPCOM26
      COMMON /UBCBRN/  BRNMAX                                           UBPCOM27
      DOUBLE PRECISION SUMW, SUMW2, MAXWS, MAXWT                        UBPCOM28
      INTEGER          ISBAD, ISCALL, ISFAIL, ISREJ, ITBAD, ITCALL,     UBPCOM29
     +                 ITFAIL, CALLCT, ACCCUT                           UBPCOM30
      COMMON /UBCSTA/  SUMW, SUMW2, MAXWS, MAXWT, ISBAD, ISCALL, ISFAIL,UBPCOM31
     +                 ISREJ, ITBAD, ITCALL, ITFAIL, CALLCT, ACCCUT     UBPCOM32
      CHARACTER     VERSQQ*8                                            ASKUSI 7
      INTEGER       ALTABL, ALRLEP, IDATQQ, IGCO, IGCOD, NAMIND, JGPAR, ASKUSI 8
     +              ITCH, IWEAK, IBOXES, IBST, JSVRT, JXVRT, NCOL, NROW,ASKUSI 9
     +              JKPAR, IEBEAM, JRLEP, ITAB(19)                      ASKUSI10
      EXTERNAL      ALTABL, ALRLEP, NAMIND                              ASKUSI11
      REAL          TABL(19), AMZ, AMH, AMT, ECMS, CTSMN, CTSMX, EFERM, ASKUSI12
     +              ACOLMX                                              ASKUSI13
      PARAMETER     (IGCO = 2010)                                       ASKUSI14
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
      MASS1Z = DBLE(AMZ)                                                ASKUS113
      MASS1H = DBLE(AMH)                                                ASKUS114
      MASS1T = DBLE(AMT)                                                ASKUS115
      EBEAM  = DBLE(ECMS) * 0.5D0                                       ASKUS116
      CTSMIN = DBLE(CTSMN)                                              ASKUS117
      CTSMAX = DBLE(CTSMX)                                              ASKUS118
      ECUT   = DBLE(EFERM)                                              ASKUS119
      ACOCUT = DBLE(ACOLMX)                                             ASKUS120
      TCHANN = .TRUE.                                                   ASKUS121
      IF (ITCH .EQ. 0) TCHANN = .FALSE.                                 ASKUS122
      WEAK   = .TRUE.                                                   ASKUS123
      IF (IWEAK .EQ. 0) WEAK = .FALSE.                                  ASKUS124
      BOXES  = .TRUE.                                                   ASKUS125
      IF (IBOXES .EQ. 0) BOXES = .FALSE.                                ASKUS126
      BSTYLE = IBST                                                     ASKUS127
      CALL UBINIT(IDATQQ, VERSQQ)                                       ASKUS128
C                                                                       ASKUS129
C   say hello and print version number and date                         ASKUS130
C                                                                       ASKUS131
      WRITE(NOUT,101) IGCOD ,VERSQQ,IDATQQ                              ASKUS132
 101  FORMAT(/,10X,                                                     ASKUS133
     &       'UBAB01 - CODE NUMBER = ',I4,', VERSION ',A8,', RELEASED ',ASKUS134
     &       I6,/,10X,' Interface last modified on October 9  1995',    BBL001 1
     & /,10X,'********************************************************',ASKUS136
     &       '******',//)                                               ASKUS137
C                                                                       ASKUS138
C   Print PART and KLIN banks                                           ASKUS139
C                                                                       ASKUS140
      CALL PRPART                                                       ASKUS141
      CALL PRTABL('KPAR',0)                                             ASKUS142
      CALL PRTABL('RLEP',0)                                             ASKUS143
C                                                                       ASKUS144
C   Reset the event counters                                            ASKUS145
C                                                                       ASKUS146
      CALL VZERO(NEVCNT, NCNT)                                          ASKUS147
C                                                                       ASKUS148
C   book a few standard histograms                                      ASKUS149
C                                                                       ASKUS150
      CALL HBOOK1(10001, 'COS(THETA-STAR)', 100, -1.0, +1.0, 0.0)       ASKUS151
      CALL HBOOK1(10002, 'ACOLLINEARITY', 90, 0.0, PI, 0.0)             ASKUS152
      CALL HBOOK1(10003, 'NO OF STORED PHOTONS', 100, 0.0, 100.0, 0.0)  ASKUS153
      CALL HBOOK1(10004,                                                ASKUS154
     +  'ANGLE OF HIGHEST MOM PHOTON WRT TO RADIATING LEPTON',          ASKUS155
     +  90, 0.0, PIBY2, 0.0)                                            ASKUS156
      CALL HBOOK1(10005, 'ENERGY OF HIGHEST MOM PHOTON / EBEAM', 50,    ASKUS157
     +  0.0, 1.0, 0.0)                                                  ASKUS158
      CALL HIDOPT(10002,'LOGY')                                         ASKUS159
      CALL HIDOPT(10004,'LOGY')                                         ASKUS160
      CALL HIDOPT(10005,'LOGY')                                         ASKUS161
C                                                                       ASKUS162
      RETURN                                                            ASKUS163
      END                                                               ASKUS164
      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)                 ASKUSE 2
C --------------------------------------------------------------------  ASKUSE 3
C Event generation for UBAB01              H. Meinhard October 1993     ASKUSE 4
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
      DOUBLE PRECISION AHPLA , ALPHA , MASS1E, MASS2E, MASS1Z, MASS2Z,  UBPCOM 2
     +                 MASS1T, MASS1H, MASS1W, GAMM1Z, GAMM2Z, SIN2W ,  UBPCOM 3
     +                 EBEAM , CTSMIN, CTSMAX, ECUT  , ACOCUT           UBPCOM 4
      INTEGER          NEVENT, PAD018, RSEED , PAD019                   UBPCOM 5
      LOGICAL          TCHANN, PAD020, WEAK  , PAD021, BOXES , PAD022   UBPCOM 6
      DOUBLE PRECISION TAUMIN, TAUMAX, EMIN  , EPSILN, QSQ              UBPCOM 7
      INTEGER          BSTYLE, PAD028, STDIN , PAD029, STDOUT, PAD030,  UBPCOM 8
     +                 STDERR, PAD031, ERRCNT, PAD032, ERRMAX, PAD033,  UBPCOM 9
     +                 VERBOS, PAD034, STATUS, PAD035, HEPDAT, PAD036,  UBPCOM10
     +                 HEPREV, PAD037, RUNID , PAD038                   UBPCOM11
      LOGICAL          DBGCOL, PAD039, DBGHEP, PAD040, DBGINI, PAD041,  UBPCOM12
     +                 DBGMUP, PAD042, DBGSCA, PAD043                   UBPCOM13
      COMMON /UBPCOM/  AHPLA , ALPHA , MASS1E, MASS2E, MASS1Z, MASS2Z,  UBPCOM14
     +                 MASS1T, MASS1H, MASS1W, GAMM1Z, GAMM2Z, SIN2W ,  UBPCOM15
     +                 EBEAM , CTSMIN, CTSMAX, ECUT  , ACOCUT,          UBPCOM16
     +                 NEVENT, PAD018, RSEED , PAD019,                  UBPCOM17
     +                 TCHANN, PAD020, WEAK  , PAD021, BOXES , PAD022,  UBPCOM18
     +                 TAUMIN, TAUMAX, EMIN  , EPSILN, QSQ   ,          UBPCOM19
     +                 BSTYLE, PAD028, STDIN , PAD029, STDOUT, PAD030,  UBPCOM20
     +                 STDERR, PAD031, ERRCNT, PAD032, ERRMAX, PAD033,  UBPCOM21
     +                 VERBOS, PAD034, STATUS, PAD035, HEPDAT, PAD036,  UBPCOM22
     +                 HEPREV, PAD037, RUNID , PAD038,                  UBPCOM23
     +                 DBGCOL, PAD039, DBGHEP, PAD040, DBGINI, PAD041,  UBPCOM24
     +                 DBGMUP, PAD042, DBGSCA, PAD043                   UBPCOM25
      DOUBLE PRECISION BRNMAX                                           UBPCOM26
      COMMON /UBCBRN/  BRNMAX                                           UBPCOM27
      DOUBLE PRECISION SUMW, SUMW2, MAXWS, MAXWT                        UBPCOM28
      INTEGER          ISBAD, ISCALL, ISFAIL, ISREJ, ITBAD, ITCALL,     UBPCOM29
     +                 ITFAIL, CALLCT, ACCCUT                           UBPCOM30
      COMMON /UBCSTA/  SUMW, SUMW2, MAXWS, MAXWT, ISBAD, ISCALL, ISFAIL,UBPCOM31
     +                 ISREJ, ITBAD, ITCALL, ITFAIL, CALLCT, ACCCUT     UBPCOM32
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
        TABL( 4) = REAL(SUMW)                                           ASKUS124
        TABL( 5) = REAL(SUMW2)                                          ASKUS125
        TABL( 6) = REAL(MAXWS)                                          ASKUS126
        TABL( 7) = REAL(MAXWT)                                          ASKUS127
        ITAB( 8) = ISTHEP(1)                                            ASKUS128
        ITAB( 9) = ISBAD                                                ASKUS129
        ITAB(10) = ISCALL                                               ASKUS130
        ITAB(11) = ISFAIL                                               ASKUS131
        ITAB(12) = ITBAD                                                ASKUS132
        ITAB(13) = ITCALL                                               ASKUS133
        ITAB(14) = ITFAIL                                               ASKUS134
        ITAB(15) = ISREJ                                                ASKUS135
        ITAB(16) = CALLCT                                               ASKUS136
        ITAB(17) = ACCCUT                                               ASKUS137
        JKSUM = ALTABL('KSUM', 1, 17, TABL, '2I,(7F,10I)', 'E')         ASKUS138
        IF (JKSUM .EQ. 0) GO TO 98                                      ASKUS139
      END IF                                                            ASKUS140
C                                                                       ASKUS141
C   fill histograms                                                     ASKUS142
C                                                                       ASKUS143
      CALL ANGLES(PHEP(1,NOUTEL), PHEP(1,NOUTPO), COSTS, ACOLL,         ASKUS144
     +            THETE, THETP)                                         ASKUS145
      CALL HFILL(10001, COSTS, 0.0, 1.0)                                ASKUS146
      CALL HFILL(10002, ACOLL, 0.0, 1.0)                                ASKUS147
      CALL HFILL(10003, REAL(NTRK-2) + 0.5, 0.0, 1.0)                   ASKUS148
      IF (NEGMX .GT. 0) THEN                                            ASKUS149
        CALL ANGLES(PHEP(1,NINEL),  PHEP(1,NEGMX),  DUM,   ACIE,        ASKUS150
     +              DUM2,  DUM3)                                        ASKUS151
        CALL ANGLES(PHEP(1,NINPO),  PHEP(1,NEGMX),  DUM,   ACIP,        ASKUS152
     +              DUM2,  DUM3)                                        ASKUS153
        CALL ANGLES(PHEP(1,NOUTEL), PHEP(1,NEGMX),  DUM,   ACOE,        ASKUS154
     +              DUM2,  DUM3)                                        ASKUS155
        CALL ANGLES(PHEP(1,NOUTPO), PHEP(1,NEGMX),  DUM,   ACOP,        ASKUS156
     +              DUM2,  DUM3)                                        ASKUS157
        ANGMX = PI - MAX(ACIE, ACIP, ACOE, ACOP)                        ASKUS158
        CALL HFILL(10004, ANGMX, 0.0, 1.0)                              ASKUS159
        CALL HFILL(10005, EGMX / REAL(EBEAM), 0.0, 1.0)                 ASKUS160
      END IF                                                            ASKUS161
C                                                                       ASKUS162
C   that's all for the normal event                                     ASKUS163
C                                                                       ASKUS164
      NEVCNT(3) = NEVCNT(3) + 1                                         ASKUS165
      GO TO 999                                                         ASKUS166
C                                                                       ASKUS167
C   error processing                                                    ASKUS168
C                                                                       ASKUS169
C   not enough space to book new bank                                   ASKUS170
   98 CONTINUE                                                          ASKUS171
      ISTA = 1                                                          ASKUS172
      NEVCNT(4) = NEVCNT(4) + 1                                         ASKUS173
      GO TO 999                                                         ASKUS174
C                                                                       ASKUS175
C   not all particles found in /HEPEVT/                                 ASKUS176
   99 CONTINUE                                                          ASKUS177
      ISTA = 2                                                          ASKUS178
      NEVCNT(5) = NEVCNT(5) + 1                                         ASKUS179
      GO TO 999                                                         ASKUS180
C                                                                       ASKUS181
  999 CONTINUE                                                          ASKUS182
      RETURN                                                            ASKUS183
      END                                                               ASKUS184
      SUBROUTINE USCJOB                                                 USCJOB 2
C --------------------------------------------------------------------  USCJOB 3
C Job termination for UBAB01               H. Meinhard October 1993     USCJOB 4
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
      DOUBLE PRECISION AHPLA , ALPHA , MASS1E, MASS2E, MASS1Z, MASS2Z,  UBPCOM 2
     +                 MASS1T, MASS1H, MASS1W, GAMM1Z, GAMM2Z, SIN2W ,  UBPCOM 3
     +                 EBEAM , CTSMIN, CTSMAX, ECUT  , ACOCUT           UBPCOM 4
      INTEGER          NEVENT, PAD018, RSEED , PAD019                   UBPCOM 5
      LOGICAL          TCHANN, PAD020, WEAK  , PAD021, BOXES , PAD022   UBPCOM 6
      DOUBLE PRECISION TAUMIN, TAUMAX, EMIN  , EPSILN, QSQ              UBPCOM 7
      INTEGER          BSTYLE, PAD028, STDIN , PAD029, STDOUT, PAD030,  UBPCOM 8
     +                 STDERR, PAD031, ERRCNT, PAD032, ERRMAX, PAD033,  UBPCOM 9
     +                 VERBOS, PAD034, STATUS, PAD035, HEPDAT, PAD036,  UBPCOM10
     +                 HEPREV, PAD037, RUNID , PAD038                   UBPCOM11
      LOGICAL          DBGCOL, PAD039, DBGHEP, PAD040, DBGINI, PAD041,  UBPCOM12
     +                 DBGMUP, PAD042, DBGSCA, PAD043                   UBPCOM13
      COMMON /UBPCOM/  AHPLA , ALPHA , MASS1E, MASS2E, MASS1Z, MASS2Z,  UBPCOM14
     +                 MASS1T, MASS1H, MASS1W, GAMM1Z, GAMM2Z, SIN2W ,  UBPCOM15
     +                 EBEAM , CTSMIN, CTSMAX, ECUT  , ACOCUT,          UBPCOM16
     +                 NEVENT, PAD018, RSEED , PAD019,                  UBPCOM17
     +                 TCHANN, PAD020, WEAK  , PAD021, BOXES , PAD022,  UBPCOM18
     +                 TAUMIN, TAUMAX, EMIN  , EPSILN, QSQ   ,          UBPCOM19
     +                 BSTYLE, PAD028, STDIN , PAD029, STDOUT, PAD030,  UBPCOM20
     +                 STDERR, PAD031, ERRCNT, PAD032, ERRMAX, PAD033,  UBPCOM21
     +                 VERBOS, PAD034, STATUS, PAD035, HEPDAT, PAD036,  UBPCOM22
     +                 HEPREV, PAD037, RUNID , PAD038,                  UBPCOM23
     +                 DBGCOL, PAD039, DBGHEP, PAD040, DBGINI, PAD041,  UBPCOM24
     +                 DBGMUP, PAD042, DBGSCA, PAD043                   UBPCOM25
      DOUBLE PRECISION BRNMAX                                           UBPCOM26
      COMMON /UBCBRN/  BRNMAX                                           UBPCOM27
      DOUBLE PRECISION SUMW, SUMW2, MAXWS, MAXWT                        UBPCOM28
      INTEGER          ISBAD, ISCALL, ISFAIL, ISREJ, ITBAD, ITCALL,     UBPCOM29
     +                 ITFAIL, CALLCT, ACCCUT                           UBPCOM30
      COMMON /UBCSTA/  SUMW, SUMW2, MAXWS, MAXWT, ISBAD, ISCALL, ISFAIL,UBPCOM31
     +                 ISREJ, ITBAD, ITCALL, ITFAIL, CALLCT, ACCCUT     UBPCOM32
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
      TABL( 4) = REAL(SUMW)                                             USCJOB24
      TABL( 5) = REAL(SUMW2)                                            USCJOB25
      TABL( 6) = REAL(MAXWS)                                            USCJOB26
      TABL( 7) = REAL(MAXWT)                                            USCJOB27
      ITAB( 8) = ISTHEP(1)                                              USCJOB28
      ITAB( 9) = ISBAD                                                  USCJOB29
      ITAB(10) = ISCALL                                                 USCJOB30
      ITAB(11) = ISFAIL                                                 USCJOB31
      ITAB(12) = ITBAD                                                  USCJOB32
      ITAB(13) = ITCALL                                                 USCJOB33
      ITAB(14) = ITFAIL                                                 USCJOB34
      ITAB(15) = ISREJ                                                  USCJOB35
      ITAB(16) = CALLCT                                                 USCJOB36
      ITAB(17) = ACCCUT                                                 USCJOB37
      JKSUM = ALTABL('KSUM', 1, 17, TABL, '2I,(7F,10I)', 'C')           USCJOB38
      IF (JKSUM .EQ. 0) NEVCNT(4) = NEVCNT(4) + 1                       USCJOB39
C                                                                       USCJOB40
C   write current cross section status                                  USCJOB41
C                                                                       USCJOB42
      WRITE (NOUT, 103)                                                 USCJOB43
  103 FORMAT (//20X,'X-SECTION STATISTICS',                             USCJOB44
     +         /20X,'********************')                             USCJOB45
      WRITE (NOUT, 104) (TABL(II)*1.E06,II=1,2),TABL(3)*1.E-03,         USCJOB46
     $                  (TABL(II),II=4,7),(ITAB(II),II=8,17)            USCJOB47
  104 FORMAT (/5X,'Generated x-section (nbarn)             = ',F10.4,   USCJOB48
     &        /5X,'Error on generated x-section ( nbarn)   = ',F10.4,   USCJOB49
     &        /5X,'Maximum  Born x-section      ( nbarn)   = ',F10.4,   USCJOB50
     &        /5X,'Sum of events weights                   = ',F10.4,   USCJOB51
     &        /5X,'Sum of squares of events weights        = ',F10.4,   USCJOB52
     &        /5X,'Maximum weight in s rejection           = ',F10.4,   USCJOB53
     &        /5X,'Maximum weight in t rejection           = ',F10.4,   USCJOB54
     &        /5X,'# of events generated                   = ',I10,     USCJOB55
     &        /5X,'# of events with too large s weight     = ',I10,     USCJOB56
     &        /5X,'# of tries in s generation              = ',I10,     USCJOB57
     &        /5X,'# of events w/ unphysical E hard scatter= ',I10,     USCJOB58
     &        /5X,'# of events with too large t weight     = ',I10,     USCJOB59
     &        /5X,'# of tries in t generation              = ',I10,     USCJOB60
     &        /5X,'# of events rejected in t loop          = ',I10,     USCJOB61
     &        /5X,'# of events rejected in s loop          = ',I10,     USCJOB62
     &        /5X,'# of events passing all internal cuts   = ',I10,     USCJOB63
     &        /5X,'# of events after all int. & ext. cuts  = ',I10)     USCJOB64
C                                                                       USCJOB65
C   write statistics                                                    USCJOB66
C                                                                       USCJOB67
      WRITE (NOUT, 101)                                                 USCJOB68
  101 FORMAT (//20X,'RECORD STATISTICS',                                USCJOB69
     +         /20X,'*****************')                                USCJOB70
      WRITE (NOUT, 102) NEVCNT                                          USCJOB71
  102 FORMAT (/5X,'# of calls to ASKUSE                    = ',I10,     USCJOB72
     &        /5X,'# of calls to UBGEN - evt generation    = ',I10,     USCJOB73
     &        /5X,'# of error-free events                  = ',I10,     USCJOB74
     &        /5X,'# of records with BOS bank space pb     = ',I10,     USCJOB75
     &        /5X,'# of records with inconsistent HEPEVT   = ',I10)     USCJOB76
C                                                                       USCJOB77
C That's it                                                             USCJOB78
C                                                                       USCJOB79
      RETURN                                                            USCJOB80
      END                                                               USCJOB81
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
