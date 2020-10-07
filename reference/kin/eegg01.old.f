      SUBROUTINE ASKUSI(IGCOD)                                          ASKUSI 2
C--------------------------------------------------------------------   ASKUSI 3
C                P. Gay       May 30, 1988                              ASKUSI 4
C                                                                       ASKUSI 5
C        modified G. Bonneaud June, 1988                                ASKUSI 6
C        modified B.Bloch January 1990                                  ASKUSI 7
C                                                                       ASKUSI 8
C        initialization routine for EEGINI                              ASKUSI 9
C                                                                       ASKUSI10
C--------------------------------------------------------------------   ASKUSI11
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      PARAMETER (LBCS=50000,LCHAR=4)                                    BCS    5
      COMMON/BCS/ IW(LBCS)                                              BCS    6
      INTEGER IW                                                        BCS    7
      REAL RW(LBCS)                                                     BCS    8
      EQUIVALENCE (RW(1),IW(1))                                         BCS    9
C                                                                       BCS   10
C                                                                       ASKUSI13
      PARAMETER ( IGCO = 2003)                                          ASKUSI14
C                                                                       ASKUSI15
      COMMON / COUNTR / NEVENT(3),LWRITE                                ASKUSI16
      COMMON / GENPAR / GPAR(24)                                        ASKUSI17
      DIMENSION SVRT(3)                                                 ASKUSI18
      INTEGER ALTABL,ALRLEP                                             ASKUSI19
      EXTERNAL ALTABL,ALRLEP                                            ASKUSI20
C                                                                       ASKUSI21
      LOGICAL VALID                                                     ASKUSI22
C                                                                       ASKUSI23
C  Initialization flag                                                  ASKUSI24
      VALID = .FALSE.                                                   ASKUSI25
C                                                                       ASKUSI26
C   Return the generator code as defined in the KINGAL library          ASKUSI27
      IGCOD = IGCO                                                      ASKUSI28
C                                                                       ASKUSI29
C indicate the OUTPUT file number                                       ASKUSI30
      LWRITE = IW(6)                                                    ASKUSI31
      WRITE(LWRITE,100) IGCOD                                           ASKUSI32
 100  FORMAT(///,' GENERATOR CODE ',I9,' Last modification January 1990'ASKUSI33
     &,/,' ********************************************************',/) ASKUSI34
C                                                                       ASKUSI35
C  Initialization : the subroutine EEGINI calls the subroutine RDATA    ASKUSI36
C  which reads (eventually) the data cards.                             ASKUSI37
      CALL EEGINI(VALID)                                                ASKUSI38
C                                                                       ASKUSI39
      IF(VALID) THEN                                                    ASKUSI40
       WRITE(LWRITE,1001)                                               ASKUSI41
 1001  FORMAT(///,' INITIALIZATION O.K. --- START OF GENERATION',/      ASKUSI42
     &           ,' *******************************************',///)   ASKUSI43
      ELSE                                                              ASKUSI44
       WRITE(LWRITE,1002)                                               ASKUSI45
 1002  FORMAT(///,' INITIALIZATION NOT O.K. - SEE MESSAGES - STOP',/    ASKUSI46
     &           ,' *********************************************')     ASKUSI47
       STOP                                                             ASKUSI48
      ENDIF                                                             ASKUSI49
C                                                                       ASKUSI50
C  Main vertex smearing                                                 ASKUSI51
      SVRT(1) = 0.035                                                   ASKUSI52
      SVRT(2) = 0.0012                                                  ASKUSI53
      SVRT(3) = 1.28                                                    ASKUSI54
      JSVRT=NLINK('SVRT',0)                                             ASKUSI55
      IF (JSVRT.NE.0) THEN                                              ASKUSI56
       SVRT(1)=RW(JSVRT+1)                                              ASKUSI57
       SVRT(2)=RW(JSVRT+2)                                              ASKUSI58
       SVRT(3)=RW(JSVRT+3)                                              ASKUSI59
      ENDIF                                                             ASKUSI60
      GPAR(22) = SVRT(1)                                                ASKUSI61
      GPAR(23) = SVRT(2)                                                ASKUSI62
      GPAR(24) = SVRT(3)                                                ASKUSI63
C                                                                       ASKUSI64
C  Fill the KPAR bank                                                   ASKUSI65
      NCOL = 24                                                         ASKUSI66
      NROW = 1                                                          ASKUSI67
      JKPAR = ALTABL('KPAR',NCOL,NROW,GPAR,'2I,(F)','C')                ASKUSI68
C  Fill RLEP bank                                                       ASKUSI69
      IEBEAM = NINT(GPAR(1)*1000.)                                      ASKUSI70
      JRLEP = ALRLEP(IEBEAM,'    ',0,0,0)                               ASKUSI71
C                                                                       ASKUSI72
C  Initialize counters                                                  ASKUSI73
      DO 10 I = 1,3                                                     ASKUSI74
   10  NEVENT(I) = 0                                                    ASKUSI75
C                                                                       ASKUSI76
C  Print PART and KPAR banks                                            ASKUSI77
      CALL PRPART                                                       ASKUSI78
C                                                                       ASKUSI79
      CALL PRTABL('KPAR',0)                                             ASKUSI80
      CALL PRTABL('RLEP',0)                                             ASKUSI81
C                                                                       ASKUSI82
      RETURN                                                            ASKUSI83
      END                                                               ASKUSI84
      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)                 ASKUSE 2
C--------------------------------------------------------------------   ASKUSE 3
C                                                                       ASKUSE 4
C                 P. Gay       May 30, 1988                             ASKUSE 5
C                                                                       ASKUSE 6
C         modified by G. Bonneaud June 1988                             ASKUSE 7
C                                                                       ASKUSE 8
C - call your generator routine to generate one event                   ASKUSE 9
C - fill KINE , VERT , KHIS banks                                       ASKUSE10
C                                                                       ASKUSE11
C     output    : 6 arguments                                           ASKUSE12
C          IDPR   : process identification                              ASKUSE13
C          ISTA   : status flag ( 0 means ok)                           ASKUSE14
C          NTRK   : number of tracks generated and kept                 ASKUSE15
C                  (i.e. # KINE banks  written)                         ASKUSE16
C          NVRT   : number of vertices generated                        ASKUSE17
C                   (i.e. # VERT banks written)                         ASKUSE18
C          ECMS   : center of mass energy for the event                 ASKUSE19
C          WEIT   : event weight                                        ASKUSE20
C--------------------------------------------------------------------   ASKUSE21
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      PARAMETER (LBCS=50000,LCHAR=4)                                    BCS    5
      COMMON/BCS/ IW(LBCS)                                              BCS    6
      INTEGER IW                                                        BCS    7
      REAL RW(LBCS)                                                     BCS    8
      EQUIVALENCE (RW(1),IW(1))                                         BCS    9
C                                                                       BCS   10
C                                                                       ASKUSE23
      COMMON / COUNTR / NEVENT(3),LWRITE                                ASKUSE24
      DIMENSION SVRT(3),VERT(4),TABL(4),Q(4,4),ITABL(4)                 ASKUSE25
      INTEGER ALTABL                                                    ASKUSE26
      DATA IFIRST / 0 /                                                 ASKUSE27
C                                                                       ASKUSE28
C  Generate vertex position                                             ASKUSE29
C                                                                       ASKUSE30
      CALL RANNOR(RX,RY)                                                ASKUSE31
      CALL RANNOR(RZ,DUM)                                               ASKUSE32
      IF(IFIRST.EQ.0) THEN                                              ASKUSE33
       IFIRST = 1                                                       ASKUSE34
       JKPAR = NLINK('KPAR',0)                                          ASKUSE35
       EBEAM   = RW(JKPAR+LMHLEN+1)                                     ASKUSE36
       SVRT(1) = RW(JKPAR+LMHLEN+22)                                    ASKUSE37
       SVRT(2) = RW(JKPAR+LMHLEN+23)                                    ASKUSE38
       SVRT(3) = RW(JKPAR+LMHLEN+24)                                    ASKUSE39
       IDPR    = RW(JKPAR+LMHLEN+3)                                     ASKUSE40
      ENDIF                                                             ASKUSE41
      VERT(1)=RX*SVRT(1)                                                ASKUSE42
      VERT(2)=RY*SVRT(2)                                                ASKUSE43
      VERT(3)=RZ*SVRT(3)                                                ASKUSE44
      VERT(4)=0.                                                        ASKUSE45
C                                                                       ASKUSE46
C  Book VERT bank #1 (main vertex)                                      ASKUSE47
C                                                                       ASKUSE48
      IND=KBVERT(1,VERT,0)                                              ASKUSE49
      IF (IND.LE.0) GO TO 60                                            ASKUSE50
C                                                                       ASKUSE51
C  Event generation                                                     ASKUSE52
C                                                                       ASKUSE53
      NEVENT(1) = NEVENT(1) + 1                                         ASKUSE54
      CALL EEGGEN(NBRE,Q,POIDS)                                         ASKUSE55
C                                                                       ASKUSE56
      NTRK = NBRE                                                       ASKUSE57
      NVRT = 1                                                          ASKUSE58
      ISTA = 0                                                          ASKUSE59
      ECMS = EBEAM * 2.                                                 ASKUSE60
      WEIT = POIDS                                                      ASKUSE61
C                                                                       ASKUSE62
C   Book KINE banks for beam electrons                                  ASKUSE63
C                                                                       ASKUSE64
      TABL(1) = 0.                                                      ASKUSE65
      TABL(2) = 0.                                                      ASKUSE66
      TABL(3) = -EBEAM                                                  ASKUSE67
      TABL(4) =  0.                                                     ASKUSE68
      IND=KBKINE(-1,TABL,2,0)                                           ASKUSE69
      TABL(3) =  EBEAM                                                  ASKUSE70
      JND=KBKINE(-2,TABL,3,0)                                           ASKUSE71
      IF (IND*JND.EQ.0) GO TO 60                                        ASKUSE72
C                                                                       ASKUSE73
C   Book KINE banks for outgoing particles                              ASKUSE74
C                                                                       ASKUSE75
      DO 10 I = 1,3                                                     ASKUSE76
   10 TABL(I) = ((-1)**I)*Q(1,I)                                        ASKUSE77
      IND = KBKINE(1,TABL(1),2,1)                                       ASKUSE78
      IF(IND.EQ.0) GO TO 60                                             ASKUSE79
      DO 20 I = 1,3                                                     ASKUSE80
   20 TABL(I) = ((-1)**I)*Q(2,I)                                        ASKUSE81
      IND = KBKINE(2,TABL(1),3,1)                                       ASKUSE82
      IF(IND.EQ.0) GO TO 60                                             ASKUSE83
      DO 30 I = 1,3                                                     ASKUSE84
   30 TABL(I) = ((-1)**I)*Q(3,I)                                        ASKUSE85
      IND = KBKINE(3,TABL(1),1,1)                                       ASKUSE86
      IF(IND.EQ.0) GO TO 60                                             ASKUSE87
      IF (NBRE.EQ.4) THEN                                               ASKUSE88
       DO 40 I = 1,3                                                    ASKUSE89
   40  TABL(I) = ((-1)**I)*Q(4,I)                                       ASKUSE90
       JND = KBKINE(4,TABL(1),1,1)                                      ASKUSE91
       IF(JND.EQ.0) GO TO 60                                            ASKUSE92
      ENDIF                                                             ASKUSE93
C                                                                       ASKUSE94
C    Fill the history bank KHIS                                         ASKUSE95
C                                                                       ASKUSE96
      DO 50 I=1,NTRK                                                    ASKUSE97
   50 ITABL(I)=0                                                        ASKUSE98
      IND=ALTABL('KHIS',1,NTRK,ITABL,'I','E')                           ASKUSE99
      IF (IND.LE.0) GO TO 60                                            ASKUS100
C                                                                       ASKUS101
C  End of event generation                                              ASKUS102
C                                                                       ASKUS103
      NEVENT(2) = NEVENT(2) + 1                                         ASKUS104
      RETURN                                                            ASKUS105
   60 ISTA=1                                                            ASKUS106
      NEVENT(3) = NEVENT(3) + 1                                         ASKUS107
      RETURN                                                            ASKUS108
      END                                                               ASKUS109
      SUBROUTINE USCJOB                                                 USCJOB 2
C-------------------------------------------------------------------    USCJOB 3
C  End of job routine                                                   USCJOB 4
C                                                                       USCJOB 5
C------------------------------------------------------------------     USCJOB 6
      COMMON / COUNTR / NEVENT(3),LWRITE                                USCJOB 7
C                                                                       USCJOB 8
C  Calculate efficiencies and total cross section                       USCJOB 9
C                                                                       USCJOB10
      CALL TEEGGC                                                       USCJOB11
C                                                                       USCJOB12
C  Print out a summary header of the parameters and results             USCJOB13
C                                                                       USCJOB14
      CALL TEEGGP(LWRITE)                                               USCJOB15
C                                                                       USCJOB16
      WRITE(LWRITE,101)                                                 USCJOB17
  101 FORMAT(//20X,'EVENTS AND ERRORS STATISTICS',                      USCJOB18
     &        /20X,'****************************')                      USCJOB19
      WRITE(LWRITE,102)NEVENT(1),NEVENT(2),NEVENT(3)                    USCJOB20
  102 FORMAT(/5X,'# OF GENERATED EVENTS                       = ',I10,  USCJOB21
     &       /5X,'# OF ACCEPTED  EVENTS                       = ',I10,  USCJOB22
     &       /5X,'# OF REJECTED  EVENTS (ISTA # 0 IN ASKUSE)  = ',I10)  USCJOB23
C                                                                       USCJOB24
C  Print histos                                                         USCJOB25
C                                                                       USCJOB26
C     CALL HPRINT(0)                                                    USCJOB27
C     CALL HDELET(0)                                                    USCJOB28
C                                                                       USCJOB29
      RETURN                                                            USCJOB30
      END                                                               USCJOB31
      SUBROUTINE RDATA                                                  RDATA  2
C====================================================================   RDATA  3
C    P. GAY (Clermont fd 5/88)                                          RDATA  4
C                                                                       RDATA  5
C    modified by G. Bonneaud June 1988.                                 RDATA  6
C                                                                       RDATA  7
C    Set up the parameters values.                                      RDATA  8
C    Read the DATA cards file, and fill the commons                     RDATA  9
C                                                                       RDATA 10
C=====================================================================  RDATA 11
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      PARAMETER (LBCS=50000,LCHAR=4)                                    BCS    5
      COMMON/BCS/ IW(LBCS)                                              BCS    6
      INTEGER IW                                                        BCS    7
      REAL RW(LBCS)                                                     BCS    8
      EQUIVALENCE (RW(1),IW(1))                                         BCS    9
C                                                                       BCS   10
C                                                                       RDATA 13
C                                                                       RDATA 14
      REAL*8 ALPHA,ALPHA3,ALPHA4,M,PBARN,PI,TWOPI                       RDATA 15
C                                                                       RDATA 16
      PARAMETER                                                         RDATA 17
     >(          ALPHA = 1.D0/137.036D0, ALPHA3=ALPHA**3,ALPHA4=ALPHA**4RDATA 18
     >,          M     = 0.5110034 D-3                                  RDATA 19
     >,          PBARN = .389386 D9                                     RDATA 20
     >,          PI    = 3.14159265358979 D0 , TWOPI=2.0D0*PI           RDATA 21
     >)                                                                 RDATA 22
C                                                                       RDATA 23
C                                                                       RDATA 24
      INTEGER*4 HARD,SOFT,NONE    , EGAMMA,GAMMA,ETRON,GAMMAE           RDATA 25
     >,         BK,BKM2,TCHAN,EPA , EPADC,BEEGG,MEEGG,HEEGG             RDATA 26
      PARAMETER                                                         RDATA 27
     >(          HARD  = 1 , EGAMMA = 11 , BK    = 21 , EPADC = 31      RDATA 28
     >,          SOFT  = 2 , ETRON  = 12 , BKM2  = 22 , BEEGG = 32      RDATA 29
     >,          NONE  = 3 , GAMMA  = 13 , TCHAN = 23 , MEEGG = 33      RDATA 30
     >,                      GAMMAE = 14 , EPA   = 24 , HEEGG = 34      RDATA 31
     >)                                                                 RDATA 32
C                                                                       RDATA 33
C                                                                       RDATA 34
      REAL*8        EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN            RDATA 35
     >,             PEGMIN,EEVETO,EGVETO,PHVETO,CUTOFF,EPS              RDATA 36
     >,             WGHT1M,WGHTMX                                       RDATA 37
      INTEGER*4     ISEED, RADCOR,CONFIG,MATRIX,MTRXGG,LNWGHT           RDATA 38
      LOGICAL*4     UNWGHT                                              RDATA 39
C                                                                       RDATA 40
      COMMON/TINPAR/EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN            RDATA 41
     >,             PEGMIN,EEVETO,EGVETO,PHVETO,CUTOFF,EPS              RDATA 42
     >,             WGHT1M,WGHTMX                                       RDATA 43
     >,             ISEED, RADCOR,CONFIG,MATRIX,MTRXGG, UNWGHT          RDATA 44
C                                                                       RDATA 45
C                                                                       RDATA 46
      INTEGER*4 NRNDMX                                                  RDATA 47
      PARAMETER (NRNDMX=20)                                             RDATA 48
C                                                                       RDATA 49
      REAL*4 RND(NRNDMX)                                                RDATA 50
      INTEGER*4 SEED,NXSEED,BSEED                                       RDATA 51
      COMMON/TRND/RND,SEED,NXSEED,BSEED                                 RDATA 52
      REAL*8 S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK          RDATA 53
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC RDATA 54
      COMMON/TCONST/                                                    RDATA 55
     >       S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK          RDATA 56
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC RDATA 57
C                                                                       RDATA 58
C                                                                       RDATA 59
      REAL*8 P(16),RSIGN                                                RDATA 60
      COMMON/TEVENT/P,RSIGN                                             RDATA 61
      REAL*8 T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                 RDATA 62
      COMMON/TEVQUA/T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF          RDATA 63
C                                                                       RDATA 64
C                                                                       RDATA 65
      REAL*8 EFFIC,SIGE,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX                 RDATA 66
     >,      SUMW1,SUMW12,SUMWGT,SUMW2,CONVER                           RDATA 67
      INTEGER*4 NTRIAL,NPASSQ,NACC                                      RDATA 68
      COMMON/TSMMRY/EFFIC,SIGE,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX          RDATA 69
     >,             SUMW1,SUMW12,SUMWGT,SUMW2,CONVER                    RDATA 70
     >,             NTRIAL,NPASSQ,NACC                                  RDATA 71
C                                                                       RDATA 72
C                                                                       RDATA 73
      REAL*4 GPAR(24)                                                   RDATA 74
      COMMON / GENPAR / GPAR                                            RDATA 75
C                                                                       RDATA 76
      JGENE = NLINK('GENE',0)                                           RDATA 77
      IF(JGENE.NE.0) THEN                                               RDATA 78
       EB     = DBLE(RW(JGENE+1))                                       RDATA 79
       RADCOR = IW(JGENE+2)                                             RDATA 80
       CONFIG = IW(JGENE+3)                                             RDATA 81
       MATRIX = IW(JGENE+4)                                             RDATA 82
       MTRXGG = IW(JGENE+5)                                             RDATA 83
       CUTOFF = DBLE(RW(JGENE+6))                                       RDATA 84
       WGHT1M = DBLE(RW(JGENE+7))                                       RDATA 85
       WGHTMX = DBLE(RW(JGENE+8))                                       RDATA 86
       ISEED  =      IW(JGENE+9)                                        RDATA 87
       EPS    = DBLE(RW(JGENE+10))                                      RDATA 88
       LNWGHT =      IW(JGENE+11)                                       RDATA 89
      ENDIF                                                             RDATA 90
C                                                                       RDATA 91
      JGCUT = NLINK('GCUT',0)                                           RDATA 92
      IF(JGCUT.NE.0) THEN                                               RDATA 93
       TEVETO = DBLE(RW(JGCUT+1))                                       RDATA 94
       TEMIN  = DBLE(RW(JGCUT+2))                                       RDATA 95
       TGVETO = DBLE(RW(JGCUT+3))                                       RDATA 96
       TGMIN  = DBLE(RW(JGCUT+4))                                       RDATA 97
       PHVETO = DBLE(RW(JGCUT+5))                                       RDATA 98
       PEGMIN = DBLE(RW(JGCUT+6))                                       RDATA 99
       EEMIN  = DBLE(RW(JGCUT+7))                                       RDATA100
       EGMIN  = DBLE(RW(JGCUT+8))                                       RDATA101
       EEVETO = DBLE(RW(JGCUT+9))                                       RDATA102
       EGVETO = DBLE(RW(JGCUT+10))                                      RDATA103
      ENDIF                                                             RDATA104
C                                                                       RDATA105
      IF(LNWGHT.EQ.1) UNWGHT=.TRUE.                                     RDATA106
      IF(LNWGHT.EQ.0) UNWGHT=.FALSE.                                    RDATA107
C                                                                       RDATA108
C  Fill the GPAR table                                                  RDATA109
      GPAR(1)   = SNGL(EB)                                              RDATA110
      GPAR(2)   =      RADCOR                                           RDATA111
      GPAR(3)   =      CONFIG                                           RDATA112
      GPAR(4)   =      MATRIX                                           RDATA113
      GPAR(5)   =      MTRXGG                                           RDATA114
      GPAR(6)   = SNGL(CUTOFF)                                          RDATA115
      GPAR(7)   = SNGL(WGHT1M)                                          RDATA116
      GPAR(8)   = SNGL(WGHTMX)                                          RDATA117
      GPAR(9)   =       ISEED                                           RDATA118
      GPAR(10)  = SNGL(   EPS)                                          RDATA119
      GPAR(11)  =      LNWGHT                                           RDATA120
      GPAR(12)  = SNGL(TEVETO)                                          RDATA121
      GPAR(13)  = SNGL( TEMIN)                                          RDATA122
      GPAR(14)  = SNGL(TGVETO)                                          RDATA123
      GPAR(15)  = SNGL( TGMIN)                                          RDATA124
      GPAR(16)  = SNGL(PHVETO)                                          RDATA125
      GPAR(17)  = SNGL(PEGMIN)                                          RDATA126
      GPAR(18)  = SNGL( EEMIN)                                          RDATA127
      GPAR(19)  = SNGL( EGMIN)                                          RDATA128
      GPAR(20)  = SNGL(EEVETO)                                          RDATA129
      GPAR(21)  = SNGL(EGVETO)                                          RDATA130
C                                                                       RDATA131
      RETURN                                                            RDATA132
      END                                                               RDATA133
      SUBROUTINE EEGINI(VALID)                                          EEGINI 2
C====================================================================   EEGINI 3
C    P. GAY (Clermont fd 5/88)                                          EEGINI 4
C                                                                       EEGINI 5
C    Init. the D. Karlen generator                                      EEGINI 6
C=====================================================================  EEGINI 7
      IMPLICIT LOGICAL*1(A-Z)                                           EEGINI 8
C                                                                       EEGINI 9
      REAL*8 ALPHA,ALPHA3,ALPHA4,M,PBARN,PI,TWOPI                       EEGINI10
C                                                                       EEGINI11
      PARAMETER                                                         EEGINI12
     >(          ALPHA = 1.D0/137.036D0, ALPHA3=ALPHA**3,ALPHA4=ALPHA**4EEGINI13
     >,          M     = 0.5110034 D-3                                  EEGINI14
     >,          PBARN = .389386 D9                                     EEGINI15
     >,          PI    = 3.14159265358979 D0 , TWOPI=2.0D0*PI           EEGINI16
     >)                                                                 EEGINI17
C                                                                       EEGINI18
      INTEGER*4 HARD,SOFT,NONE    , EGAMMA,GAMMA,ETRON,GAMMAE           EEGINI19
     >,         BK,BKM2,TCHAN,EPA , EPADC,BEEGG,MEEGG,HEEGG             EEGINI20
      PARAMETER                                                         EEGINI21
     >(          HARD  = 1 , EGAMMA = 11 , BK    = 21 , EPADC = 31      EEGINI22
     >,          SOFT  = 2 , ETRON  = 12 , BKM2  = 22 , BEEGG = 32      EEGINI23
     >,          NONE  = 3 , GAMMA  = 13 , TCHAN = 23 , MEEGG = 33      EEGINI24
     >,                      GAMMAE = 14 , EPA   = 24 , HEEGG = 34      EEGINI25
     >)                                                                 EEGINI26
C                                                                       EEGINI27
      REAL*8        EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN            EEGINI28
     >,             PEGMIN,EEVETO,EGVETO,PHVETO,CUTOFF,EPS              EEGINI29
     >,             WGHT1M,WGHTMX                                       EEGINI30
      INTEGER*4     ISEED, RADCOR,CONFIG,MATRIX,MTRXGG                  EEGINI31
      LOGICAL*4     UNWGHT                                              EEGINI32
C                                                                       EEGINI33
      COMMON/TINPAR/EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN            EEGINI34
     >,             PEGMIN,EEVETO,EGVETO,PHVETO,CUTOFF,EPS              EEGINI35
     >,             WGHT1M,WGHTMX                                       EEGINI36
     >,             ISEED, RADCOR,CONFIG,MATRIX,MTRXGG, UNWGHT          EEGINI37
C                                                                       EEGINI38
C                                                                       EEGINI39
      INTEGER*4 NRNDMX                                                  EEGINI40
      PARAMETER (NRNDMX=20)                                             EEGINI41
C                                                                       EEGINI42
      REAL*4 RND(NRNDMX)                                                EEGINI43
      INTEGER*4 SEED,NXSEED,BSEED                                       EEGINI44
      COMMON/TRND/RND,SEED,NXSEED,BSEED                                 EEGINI45
      REAL*8 S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK          EEGINI46
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC EEGINI47
      COMMON/TCONST/                                                    EEGINI48
     >       S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK          EEGINI49
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC EEGINI50
C                                                                       EEGINI51
C                                                                       EEGINI52
      REAL*8 P(16),RSIGN                                                EEGINI53
      COMMON/TEVENT/P,RSIGN                                             EEGINI54
      REAL*8 T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                 EEGINI55
      COMMON/TEVQUA/T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF          EEGINI56
C                                                                       EEGINI57
C                                                                       EEGINI58
      REAL*8 EFFIC,SIGE,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX                 EEGINI59
     >,      SUMW1,SUMW12,SUMWGT,SUMW2,CONVER                           EEGINI60
      INTEGER*4 NTRIAL,NPASSQ,NACC                                      EEGINI61
      COMMON/TSMMRY/EFFIC,SIGE,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX          EEGINI62
     >,             SUMW1,SUMW12,SUMWGT,SUMW2,CONVER                    EEGINI63
     >,             NTRIAL,NPASSQ,NACC                                  EEGINI64
C                                                                       EEGINI65
      INTEGER*4 NEVENT(3),LWRITE                                        EEGINI66
      COMMON / COUNTR / NEVENT,LWRITE                                   EEGINI67
C                                                                       EEGINI68
C Set the default values for all the parameters                         EEGINI69
C                                                                       EEGINI70
      CALL TEEGGI                                                       EEGINI71
C                                                                       EEGINI72
C modify the parameters as desired                                      EEGINI73
C                                                                       EEGINI74
      CALL RDATA                                                        EEGINI75
C                                                                       EEGINI76
C Initialize the random number                                          EEGINI77
C                                                                       EEGINI78
      CALL RNDIN(ISEED)                                                 EEGINI79
C                                                                       EEGINI80
C Checks the validity of the parameters and calculates some constants   EEGINI81
      VALID = .FALSE.                                                   EEGINI82
      IF(TEEGGL(LWRITE)) VALID =.TRUE.                                  EEGINI83
C                                                                       EEGINI84
C Histogramms init.                                                     EEGINI85
      IF(VALID) CALL YHINIT                                             EEGINI86
C                                                                       EEGINI87
      RETURN                                                            EEGINI88
      END                                                               EEGINI89
      SUBROUTINE EEGGEN(NBRE,Q,POIDS)                                   EEGGEN 2
C====================================================================   EEGGEN 3
C    P. GAY (Clermont fd 5/88)                                          EEGGEN 4
C    modified by G. Bonneaud  June 1988                                 EEGGEN 5
C    Generate an event                                                  EEGGEN 6
C=====================================================================  EEGGEN 7
      IMPLICIT LOGICAL*1(A-Z)                                           EEGGEN 8
C                                                                       EEGGEN 9
      INTEGER*4 NBRE,I,J                                                EEGGEN10
      REAL*4 Q(4,4)                                                     EEGGEN11
      REAL*4 POIDS                                                      EEGGEN12
C                                                                       EEGGEN13
      REAL*8 ALPHA,ALPHA3,ALPHA4,M,PBARN,PI,TWOPI                       EEGGEN14
C                                                                       EEGGEN15
      PARAMETER                                                         EEGGEN16
     >(          ALPHA = 1.D0/137.036D0, ALPHA3=ALPHA**3,ALPHA4=ALPHA**4EEGGEN17
     >,          M     = 0.5110034 D-3                                  EEGGEN18
     >,          PBARN = .389386 D9                                     EEGGEN19
     >,          PI    = 3.14159265358979 D0 , TWOPI=2.0D0*PI           EEGGEN20
     >)                                                                 EEGGEN21
C                                                                       EEGGEN22
      INTEGER*4 HARD,SOFT,NONE    , EGAMMA,GAMMA,ETRON,GAMMAE           EEGGEN23
     >,         BK,BKM2,TCHAN,EPA , EPADC,BEEGG,MEEGG,HEEGG             EEGGEN24
      PARAMETER                                                         EEGGEN25
     >(          HARD  = 1 , EGAMMA = 11 , BK    = 21 , EPADC = 31      EEGGEN26
     >,          SOFT  = 2 , ETRON  = 12 , BKM2  = 22 , BEEGG = 32      EEGGEN27
     >,          NONE  = 3 , GAMMA  = 13 , TCHAN = 23 , MEEGG = 33      EEGGEN28
     >,                      GAMMAE = 14 , EPA   = 24 , HEEGG = 34      EEGGEN29
     >)                                                                 EEGGEN30
C                                                                       EEGGEN31
      REAL*8        EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN            EEGGEN32
     >,             PEGMIN,EEVETO,EGVETO,PHVETO,CUTOFF,EPS              EEGGEN33
     >,             WGHT1M,WGHTMX                                       EEGGEN34
      INTEGER*4     ISEED, RADCOR,CONFIG,MATRIX,MTRXGG                  EEGGEN35
      LOGICAL*4     UNWGHT                                              EEGGEN36
C                                                                       EEGGEN37
      COMMON/TINPAR/EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN            EEGGEN38
     >,             PEGMIN,EEVETO,EGVETO,PHVETO,CUTOFF,EPS              EEGGEN39
     >,             WGHT1M,WGHTMX                                       EEGGEN40
     >,             ISEED, RADCOR,CONFIG,MATRIX,MTRXGG, UNWGHT          EEGGEN41
C                                                                       EEGGEN42
C                                                                       EEGGEN43
      INTEGER*4 NRNDMX                                                  EEGGEN44
      PARAMETER (NRNDMX=20)                                             EEGGEN45
C                                                                       EEGGEN46
      REAL*4 RND(NRNDMX)                                                EEGGEN47
      INTEGER*4 SEED,NXSEED,BSEED                                       EEGGEN48
      COMMON/TRND/RND,SEED,NXSEED,BSEED                                 EEGGEN49
      REAL*8 S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK          EEGGEN50
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC EEGGEN51
      COMMON/TCONST/                                                    EEGGEN52
     >       S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK          EEGGEN53
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC EEGGEN54
C                                                                       EEGGEN55
C                                                                       EEGGEN56
      REAL*8 P(16),RSIGN                                                EEGGEN57
      COMMON/TEVENT/P,RSIGN                                             EEGGEN58
      REAL*8 T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                 EEGGEN59
      COMMON/TEVQUA/T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF          EEGGEN60
C                                                                       EEGGEN61
C                                                                       EEGGEN62
      REAL*8 EFFIC,SIGE,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX                 EEGGEN63
     >,      SUMW1,SUMW12,SUMWGT,SUMW2,CONVER                           EEGGEN64
      INTEGER*4 NTRIAL,NPASSQ,NACC                                      EEGGEN65
      COMMON/TSMMRY/EFFIC,SIGE,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX          EEGGEN66
     >,             SUMW1,SUMW12,SUMWGT,SUMW2,CONVER                    EEGGEN67
     >,             NTRIAL,NPASSQ,NACC                                  EEGGEN68
C                                                                       EEGGEN69
C Call the generating routine. (Generates one event).                   EEGGEN70
      CALL TEEGG7                                                       EEGGEN71
C                                                                       EEGGEN72
C  set the weight of the event                                          EEGGEN73
C                                                                       EEGGEN74
      IF(UNWGHT)      POIDS=1.                                          EEGGEN75
      IF(.NOT.UNWGHT) POIDS=SNGL(WGHT)                                  EEGGEN76
C                                                                       EEGGEN77
C Fill the histograms                                                   EEGGEN78
      CALL YHFILL(POIDS)                                                EEGGEN79
C                                                                       EEGGEN80
C  NBRE  number of particules in the final state                        EEGGEN81
C  Q(I = 1,4 , J = 1,4) are the 4-vectors respectively of the           EEGGEN82
C  positron,the electron and the two photons (I = 1,4), in the P_x, P_y,EEGGEN83
C  P_z, E order (J = 1,4).                                              EEGGEN84
C                                                                       EEGGEN85
      NBRE = 3                                                          EEGGEN86
      IF(RADCOR.EQ.HARD) NBRE = 4                                       EEGGEN87
C                                                                       EEGGEN88
       DO 20 I = 1,NBRE                                                 EEGGEN89
        DO 10 J = 1,4                                                   EEGGEN90
   10    Q(I,J) = SNGL(P((I-1)*4+J))                                    EEGGEN91
   20 CONTINUE                                                          EEGGEN92
C                                                                       EEGGEN93
      RETURN                                                            EEGGEN94
      END                                                               EEGGEN95
      SUBROUTINE YHINIT                                                 YHINIT 2
C======================                                                 YHINIT 3
C     init. all the histograms                                          YHINIT 4
C    P. GAY (Clermont fd 5/88)                                          YHINIT 5
C                                                                       YHINIT 6
C======================                                                 YHINIT 7
      REAL*8 EMAX , BAS , HAUT ,FAC                                     YHINIT 8
      REAL*8 ALPHA,ALPHA3,ALPHA4,M,PBARN,PI,TWOPI                       YHINIT 9
C                                                                       YHINIT10
      PARAMETER                                                         YHINIT11
     >(          ALPHA = 1.D0/137.036D0, ALPHA3=ALPHA**3,ALPHA4=ALPHA**4YHINIT12
     >,          M     = 0.5110034 D-3                                  YHINIT13
     >,          PBARN = .389386 D9                                     YHINIT14
     >,          PI    = 3.14159265358979 D0 , TWOPI=2.0D0*PI           YHINIT15
     >)                                                                 YHINIT16
C                                                                       YHINIT17
      INTEGER*4 HARD,SOFT,NONE    , EGAMMA,GAMMA,ETRON,GAMMAE           YHINIT18
     >,         BK,BKM2,TCHAN,EPA , EPADC,BEEGG,MEEGG,HEEGG             YHINIT19
      PARAMETER                                                         YHINIT20
     >(          HARD  = 1 , EGAMMA = 11 , BK    = 21 , EPADC = 31      YHINIT21
     >,          SOFT  = 2 , ETRON  = 12 , BKM2  = 22 , BEEGG = 32      YHINIT22
     >,          NONE  = 3 , GAMMA  = 13 , TCHAN = 23 , MEEGG = 33      YHINIT23
     >,                      GAMMAE = 14 , EPA   = 24 , HEEGG = 34      YHINIT24
     >)                                                                 YHINIT25
C                                                                       YHINIT26
      REAL*8        EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN            YHINIT27
     >,             PEGMIN,EEVETO,EGVETO,PHVETO,CUTOFF,EPS              YHINIT28
     >,             WGHT1M,WGHTMX                                       YHINIT29
      INTEGER*4     ISEED, RADCOR,CONFIG,MATRIX,MTRXGG                  YHINIT30
      LOGICAL*4     UNWGHT                                              YHINIT31
C                                                                       YHINIT32
      COMMON/TINPAR/EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN            YHINIT33
     >,             PEGMIN,EEVETO,EGVETO,PHVETO,CUTOFF,EPS              YHINIT34
     >,             WGHT1M,WGHTMX                                       YHINIT35
     >,             ISEED, RADCOR,CONFIG,MATRIX,MTRXGG, UNWGHT          YHINIT36
C                                                                       YHINIT37
C                                                                       YHINIT38
      INTEGER*4 NRNDMX                                                  YHINIT39
      PARAMETER (NRNDMX=20)                                             YHINIT40
C                                                                       YHINIT41
      REAL*4 RND(NRNDMX)                                                YHINIT42
      INTEGER*4 SEED,NXSEED,BSEED                                       YHINIT43
      COMMON/TRND/RND,SEED,NXSEED,BSEED                                 YHINIT44
      REAL*8 S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK          YHINIT45
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC YHINIT46
      COMMON/TCONST/                                                    YHINIT47
     >       S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK          YHINIT48
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC YHINIT49
C                                                                       YHINIT50
C                                                                       YHINIT51
      REAL*8 P(16),RSIGN                                                YHINIT52
      COMMON/TEVENT/P,RSIGN                                             YHINIT53
      REAL*8 T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                 YHINIT54
      COMMON/TEVQUA/T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF          YHINIT55
C                                                                       YHINIT56
C                                                                       YHINIT57
      REAL*8 EFFIC,SIGE,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX                 YHINIT58
     >,      SUMW1,SUMW12,SUMWGT,SUMW2,CONVER                           YHINIT59
      INTEGER*4 NTRIAL,NPASSQ,NACC                                      YHINIT60
      COMMON/TSMMRY/EFFIC,SIGE,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX          YHINIT61
     >,             SUMW1,SUMW12,SUMWGT,SUMW2,CONVER                    YHINIT62
     >,             NTRIAL,NPASSQ,NACC                                  YHINIT63
C                                                                       YHINIT64
C                                                                       YHINIT65
C                                                                       YHINIT66
C     CALL HDELET(0)                                                    YHINIT67
C                                                                       YHINIT68
      BAS   = 0.D0                                                      YHINIT69
      HAUT  = DABS(WGHTMX)                                              YHINIT70
      IF(RADCOR.NE.HARD)THEN                                            YHINIT71
       CALL HBOOK1(9001,' Weight WGHT T3BODY  $',50,BAS,HAUT,0.)        YHINIT72
      ELSE                                                              YHINIT73
       CALL HBOOK1(9002,' Weight WGHT T4BODY  $',50,BAS,HAUT,0.)        YHINIT74
      ENDIF                                                             YHINIT75
C                                                                       YHINIT76
      BAS  = 0.D0                                                       YHINIT77
      HAUT = DABS(WGHT1M)                                               YHINIT78
      IF(RADCOR.NE.HARD)THEN                                            YHINIT79
       CALL HBOOK1(9003,' Weight WGHT1 T3BODY  $',50,BAS,HAUT,0.)       YHINIT80
      ELSE                                                              YHINIT81
       CALL HBOOK1(9004,' Weight WGHT1 T4BODY  $',50,BAS,HAUT,0.)       YHINIT82
      ENDIF                                                             YHINIT83
C                                                                       YHINIT84
      EMAX      =       EB                                              YHINIT85
C                                                                       YHINIT86
      CALL HBOOK1(10001,' Photon1 energy (GeV)$',50,0.,EMAX,0.)         YHINIT87
      IF(RADCOR.EQ.HARD)THEN                                            YHINIT88
       CALL HBOOK1(10002,' Photon2 energy (GeV)$',50,0.,EMAX,0.)        YHINIT89
      ENDIF                                                             YHINIT90
C                                                                       YHINIT91
      CALL HBOOK1(10003,' Electron energy (GeV)$',50,0.,EMAX,0.)        YHINIT92
      CALL HBOOK1(10004,' Positron energy (GeV)$',50,0.,EMAX,0.)        YHINIT93
      CALL HBOOK1(10005,' cos photon1 angle $',50,-1.,1.,0.)            YHINIT94
      IF(RADCOR.EQ.HARD)THEN                                            YHINIT95
       CALL HBOOK1(10006,' cos photon2 angle $',50,-1.,1.,0.)           YHINIT96
      ENDIF                                                             YHINIT97
C                                                                       YHINIT98
      CALL HBOOK1(10007,' cos e-  angle $',50,-1.,1.,0.)                YHINIT99
      CALL HBOOK1(10008,' e-  angle (degrees) $',50,0.,90.,0.)          YHINI100
      CALL HBOOK1(10009,' cos e+  angle $',50,-1.,1.,0.)                YHINI101
      CALL HBOOK1(10010,' 180 - e+ angle (degrees) $',50,0.,90.,0.)     YHINI102
C                                                                       YHINI103
C                                                                       YHINI104
      RETURN                                                            YHINI105
      END                                                               YHINI106
      SUBROUTINE YHFILL(POIDS)                                          YHFILL 2
C======================                                                 YHFILL 3
C    P. GAY (Clermont fd 5/88)                                          YHFILL 4
C                                                                       YHFILL 5
C     Fill  the histograms with kinematic parameter                     YHFILL 6
C======================                                                 YHFILL 7
      REAL*8 ALPHA,ALPHA3,ALPHA4,M,PBARN,PI,TWOPI                       YHFILL 8
C                                                                       YHFILL 9
      PARAMETER                                                         YHFILL10
     >(          ALPHA = 1.D0/137.036D0, ALPHA3=ALPHA**3,ALPHA4=ALPHA**4YHFILL11
     >,          M     = 0.5110034 D-3                                  YHFILL12
     >,          PBARN = .389386 D9                                     YHFILL13
     >,          PI    = 3.14159265358979 D0 , TWOPI=2.0D0*PI           YHFILL14
     >)                                                                 YHFILL15
C                                                                       YHFILL16
      INTEGER*4 HARD,SOFT,NONE    , EGAMMA,GAMMA,ETRON,GAMMAE           YHFILL17
     >,         BK,BKM2,TCHAN,EPA , EPADC,BEEGG,MEEGG,HEEGG             YHFILL18
      PARAMETER                                                         YHFILL19
     >(          HARD  = 1 , EGAMMA = 11 , BK    = 21 , EPADC = 31      YHFILL20
     >,          SOFT  = 2 , ETRON  = 12 , BKM2  = 22 , BEEGG = 32      YHFILL21
     >,          NONE  = 3 , GAMMA  = 13 , TCHAN = 23 , MEEGG = 33      YHFILL22
     >,                      GAMMAE = 14 , EPA   = 24 , HEEGG = 34      YHFILL23
     >)                                                                 YHFILL24
C                                                                       YHFILL25
      REAL*8        EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN            YHFILL26
     >,             PEGMIN,EEVETO,EGVETO,PHVETO,CUTOFF,EPS              YHFILL27
     >,             WGHT1M,WGHTMX                                       YHFILL28
      INTEGER*4     ISEED, RADCOR,CONFIG,MATRIX,MTRXGG                  YHFILL29
      LOGICAL*4     UNWGHT                                              YHFILL30
C                                                                       YHFILL31
      COMMON/TINPAR/EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN            YHFILL32
     >,             PEGMIN,EEVETO,EGVETO,PHVETO,CUTOFF,EPS              YHFILL33
     >,             WGHT1M,WGHTMX                                       YHFILL34
     >,             ISEED, RADCOR,CONFIG,MATRIX,MTRXGG, UNWGHT          YHFILL35
C                                                                       YHFILL36
C                                                                       YHFILL37
      INTEGER*4 NRNDMX                                                  YHFILL38
      PARAMETER (NRNDMX=20)                                             YHFILL39
C                                                                       YHFILL40
C     REAL*4 RND(NRNDMX)                                                YHFILL41
      INTEGER*4 SEED,NXSEED,BSEED                                       YHFILL42
      COMMON/TRND/RND,SEED,NXSEED,BSEED                                 YHFILL43
      REAL*8 S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK          YHFILL44
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC YHFILL45
      COMMON/TCONST/                                                    YHFILL46
     >       S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK          YHFILL47
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC YHFILL48
C                                                                       YHFILL49
C                                                                       YHFILL50
      REAL*8 P(16),RSIGN                                                YHFILL51
      COMMON/TEVENT/P,RSIGN                                             YHFILL52
      REAL*8 T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF                 YHFILL53
      COMMON/TEVQUA/T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF          YHFILL54
      INTEGER*4 NEVENT(3),LWRITE                                        YHFILL55
      COMMON / COUNTR / NEVENT,LWRITE                                   YHFILL56
C                                                                       YHFILL57
C                                                                       YHFILL58
      REAL*8 EFFIC,SIGE,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX                 YHFILL59
     >,      SUMW1,SUMW12,SUMWGT,SUMW2,CONVER                           YHFILL60
      INTEGER*4 NTRIAL,NPASSQ,NACC                                      YHFILL61
      COMMON/TSMMRY/EFFIC,SIGE,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX          YHFILL62
     >,             SUMW1,SUMW12,SUMWGT,SUMW2,CONVER                    YHFILL63
     >,             NTRIAL,NPASSQ,NACC                                  YHFILL64
C                                                                       YHFILL65
      REAL*8 PMOD,CT,ANGLE                                              YHFILL66
C                                                                       YHFILL67
C Fill the gamma1, gamma2, (e- or e+)  energy                           YHFILL68
C                                                                       YHFILL69
      CALL HFILL(10001,P(12),0.,POIDS)                                  YHFILL70
      CALL HFILL(10002,P(16),0.,POIDS)                                  YHFILL71
      CALL HFILL(10003,P(8),0.,POIDS)                                   YHFILL72
      CALL HFILL(10004,P(4),0.,POIDS)                                   YHFILL73
C                                                                       YHFILL74
C Fill the gamma1, gamma2, (e- or e+)  cosinus angle                    YHFILL75
C                                                                       YHFILL76
      PMOD=P( 9)**2+P(10)**2+P(11)**2                                   YHFILL77
      PMOD=DSQRT(PMOD)                                                  YHFILL78
      IF(PMOD.EQ.0.D0) GOTO 100                                         YHFILL79
      CT=P(11)/PMOD                                                     YHFILL80
      CALL HFILL(10005,CT,0.,POIDS)                                     YHFILL81
 100  CONTINUE                                                          YHFILL82
C                                                                       YHFILL83
      PMOD=P(13)**2+P(14)**2+P(15)**2                                   YHFILL84
      PMOD=DSQRT(PMOD)                                                  YHFILL85
      IF(PMOD.EQ.0.D0) GOTO 200                                         YHFILL86
      CT=P(15)/PMOD                                                     YHFILL87
      CALL HFILL(10006,CT,0.,POIDS)                                     YHFILL88
 200  CONTINUE                                                          YHFILL89
C                                                                       YHFILL90
      PMOD=P( 5)**2+P( 6)**2+P( 7)**2                                   YHFILL91
      PMOD=DSQRT(PMOD)                                                  YHFILL92
      IF(PMOD.EQ.0.D0) GOTO 300                                         YHFILL93
      CT=-P( 7)/PMOD                                                    YHFILL94
      ANGLE=DACOS(DABS(CT))*180.D0/PI                                   YHFILL95
C     WRITE(LWRITE,123)CT,ANGLE                                         YHFILL96
      CALL HFILL(10008,ANGLE,0.,POIDS)                                  YHFILL97
      CALL HFILL(10007,CT,0.,POIDS)                                     YHFILL98
 300  CONTINUE                                                          YHFILL99
C                                                                       YHFIL100
      PMOD=P( 1)**2+P( 2)**2+P( 3)**2                                   YHFIL101
      PMOD=DSQRT(PMOD)                                                  YHFIL102
      IF(PMOD.EQ.0.D0) GOTO 300                                         YHFIL103
      CT=-P( 3)/PMOD                                                    YHFIL104
      ANGLE=DACOS(DABS(CT))*180.D0/PI                                   YHFIL105
C     WRITE(LWRITE,123)CT,ANGLE                                         YHFIL106
      CALL HFILL(10010,ANGLE,0.,POIDS)                                  YHFIL107
      CALL HFILL(10009,CT,0.,POIDS)                                     YHFIL108
 400  CONTINUE                                                          YHFIL109
C                                                                       YHFIL110
 123  FORMAT(2X,F16.9,4X,F16.9)                                         YHFIL111
C                                                                       YHFIL112
      RETURN                                                            YHFIL113
      END                                                               YHFIL114
