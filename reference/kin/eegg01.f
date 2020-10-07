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
      common/opal/IRUNOPAL,IEVOFF
      integer IRUNOPAL,IEVOFF
*
      COMMON / COUNTR / NEVENT(3),LWRITE                                ASKUSI16
      COMMON / GENPAR / GPAR(28)                                        ASKUSI17
      COMMON / NSAVE / NSAVEK                                           EGAM   1
      DIMENSION SVRT(3),VPOS(3)
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
      VPOS(1) = 0.000
      VPOS(2) = 0.000
      VPOS(3) = 0.000
      JSVRT=NLINK('SVRT',0)                                             ASKUSI55
      IF (JSVRT.NE.0) THEN                                              ASKUSI56
       SVRT(1)=RW(JSVRT+1)                                              ASKUSI57
       SVRT(2)=RW(JSVRT+2)                                              ASKUSI58
       SVRT(3)=RW(JSVRT+3)                                              ASKUSI59
      ENDIF                                                             ASKUSI60
      GPAR(23) = SVRT(1)                                                ASKUSI61
      GPAR(24) = SVRT(2)                                                ASKUSI62
      GPAR(25) = SVRT(3)                                                ASKUSI63
C
C Main vertex mean position
C
      JXVRT = IW(NAMIND('XVRT'))
      IF (JXVRT .NE. 0) THEN
        VPOS(1) = RW(JXVRT+1)
        VPOS(2) = RW(JXVRT+2)
        VPOS(3) = RW(JXVRT+3)
      ENDIF
      GPAR(26) = VPOS(1)                                                ASKUSI61
      GPAR(27) = VPOS(2)                                                ASKUSI62
      GPAR(28) = VPOS(3)       
C                                                                       ASKUSI64
C  Fill the KPAR bank                                                   ASKUSI65
      NCOL = 28
      NROW = 1                                                          ASKUSI67
      JKPAR = ALTABL('KPAR',NCOL,NROW,GPAR,'2I,(F)','C')                ASKUSI68
C  Fill RLEP bank                                                       ASKUSI69
      IEBEAM = NINT(GPAR(1)*1000.)                                      ASKUSI70
      JRLEP = ALRLEP(IEBEAM,'    ',0,0,0)                               ASKUSI71
*
C  Initialize counters                                                  ASKUSI73
      DO 10 I = 1,3                                                     ASKUSI74
   10  NEVENT(I) = 0                                                    ASKUSI75
C                                                                       ASKUSI76
      NSAVEK = 0                                                        EGAM   2
      CALL HBOOK1(1001,'Angle 1 Beam $',40,-1.,1.,0.)                   EGAM   3
      CALL HBOOK1(1002,'Angle 2 Beam   $',40,-1.,1.,0.)                 EGAM   4
      CALL HBOOK1(1003,'Angle Gamma Beam$',40,-1.,1.,0.)                EGAM   5
      CALL HBOOK1(1004,'Photon energy$',40,0.,60.,0.)                   EGAM   6
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
      COMMON / NSAVE / NSAVEK                                           EGAM   7
      DIMENSION SVRT(3),VPOS(3),VRTX(4),TABL(4),Q(4,4),ITABL(4)
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
       SVRT(1) = RW(JKPAR+LMHLEN+23)                                    ASKUSE37
       SVRT(2) = RW(JKPAR+LMHLEN+24)                                    ASKUSE38
       SVRT(3) = RW(JKPAR+LMHLEN+25)                                    ASKUSE39
       VPOS(1) = RW(JKPAR+LMHLEN+26)
       VPOS(2) = RW(JKPAR+LMHLEN+27)
       VPOS(3) = RW(JKPAR+LMHLEN+28)
       IDPR    = RW(JKPAR+LMHLEN+3)                                     ASKUSE40
      ENDIF                                                             ASKUSE41
      VRTX(1)=VPOS(1)+RX*SVRT(1)
      VRTX(2)=VPOS(2)+RY*SVRT(2)
      VRTX(3)=VPOS(3)+RZ*SVRT(3)
      VRTX(4)=0.                                                        ASKUSE45
C                                                                       ASKUSE46
C  Book VERT bank #1 (main vertex)                                      ASKUSE47
C                                                                       ASKUSE48
C
C   Fill 'VERT' bank
C
      IND=KBVERT(1,VRTX,0)                                              ASKUSE49
      IF (IND.EQ.0) GO TO 60                                            ASKUSE50
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
*      IF(NBRE.EQ.2) THEN                                               EGAM   8
*       ISTA = 101                                                      EGAM   9
*       NEVENT(3) = NEVENT(3) + 1                                       EGAM  10
*       RETURN                                                          EGAM  11
*      ENDIF                                                            EGAM  12
*      NHARD = 0                                                        EGAM  13
*      DO 70 N = 3,4                                                    EGAM  14
*       IF(Q(N,4).GT.5.) THEN                                           EGAM  15
*        NHARD = NHARD + 1                                              EGAM  16
*        IDPH  = N                                                      EGAM  17
*       ENDIF                                                           EGAM  18
*   70 CONTINUE                                                         EGAM  19
*      IF(NHARD.NE.1) THEN                                              EGAM  20
*       ISTA = 101                                                      EGAM  21
*       NEVENT(3) = NEVENT(3) + 1                                       EGAM  22
*       RETURN                                                          EGAM  23
*      ENDIF                                                            EGAM  24
*      THETA1  = Q(1,3) / Q(1,4)                                        EGAM  25
*      THETA2  = Q(2,3) / Q(2,4)                                        EGAM  26
*      THETAK  = Q(IDPH,3)/Q(IDPH,4)                                    EGAM  27
c     IF(NSAVEK.GE.2000) THEN                                           EGAM  28
c      ISTA = 99999                                                     EGAM  29
c      RETURN                                                           EGAM  30
c     ENDIF                                                             EGAM  31
c     IACCEP1 = 0                                                       EGAM  32
c     IF(THETA1.LT.0.95.AND.Q(1,4).GT.10.) IACCEP1 = 1                  EGAM  33
c     IACCEP2 = 0                                                       EGAM  34
c     IF(THETA2.LT.0.95.AND.Q(2,4).GT.10.) IACCEP2 = 1                  EGAM  35
c     IF(IACCEP1.EQ.0.AND.IACCEP2.EQ.0) THEN                            EGAM  36
c      ISTA = 101                                                       EGAM  37
c      NEVENT(3) = NEVENT(3) + 1                                        EGAM  38
c      RETURN                                                           EGAM  39
c     ENDIF                                                             EGAM  40
c     IF(IACCEP1.EQ.1.AND.IACCEP2.EQ.1) THEN                            EGAM  41
c      ISTA = 101                                                       EGAM  42
c      NEVENT(3) = NEVENT(3) + 1                                        EGAM  43
c      RETURN                                                           EGAM  44
c     ENDIF                                                             EGAM  45
c     IF(THETAK.LT.0.95.AND.Q(IDPH,4).GT.5.) THEN                       EGAM  46
c      NSAVEK = NSAVEK + 1                                              EGAM  47
c      CALL HFILL(1001,THETA1,0.,1.)                                    EGAM  48
c      CALL HFILL(1002,THETA2,0.,1.)                                    EGAM  49
c      CALL HFILL(1003,THETAK,0.,1.)                                    EGAM  50
c      CALL HFILL(1004,Q(IDPH,4),0.,1.)                                 EGAM  51
c     ELSE                                                              EGAM  52
c      ISTA = 101                                                       EGAM  53
c      NEVENT(3) = NEVENT(3) + 1                                        EGAM  54
c      RETURN                                                           EGAM  55
c     ENDIF                                                             EGAM  56
C                                                                       EGAM  57
C    Fill the history bank KHIS                                         ASKUSE95
C                                                                       ASKUSE96
      DO 50 I=1,NTRK                                                    ASKUSE97
   50 ITABL(I)=0                                                        ASKUSE98
      IND=ALTABL('KHIS',1,NTRK,ITABL,'I','E')                           ASKUSE99
c      write(6,*)'ntrk,ind,itabl',ntrk,ind,itabl
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
      SUBROUTINE USCJOB
C-------------------------------------------------------------------
C  End of job routine
C
C------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    
      PARAMETER (LBCS=50000,LCHAR=4)                                    BCS    
      COMMON/BCS/ IW(LBCS)
      INTEGER IW 
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
      COMMON / COUNTR / NEVENT(3),LWRITE
C
C  Calculate efficiencies and total cross section
C
      CALL TEEGGC
C
C  Print out a summary header of the parameters and results
C
      CALL TEEGGP(LWRITE)
C
      WRITE(LWRITE,101)
  101 FORMAT(//20X,'EVENTS AND ERRORS STATISTICS',
     &        /20X,'****************************')
      WRITE(LWRITE,102)NEVENT(1),NEVENT(2),NEVENT(3)
  102 FORMAT(/5X,'# OF GENERATED EVENTS                       = ',I10,
     &       /5X,'# OF ACCEPTED  EVENTS                       = ',I10,
     &       /5X,'# OF REJECTED  EVENTS (ISTA # 0 IN ASKUSE)  = ',I10)
C
C  Print histos
C
C     CALL HPRINT(0)
C     CALL HDELET(0)
C
      RETURN
      END
*DK RDATA
      SUBROUTINE RDATA
C====================================================================
C    P. GAY (Clermont fd 5/88)
C
C    modified by G. Bonneaud June 1988.
C
C    Set up the parameters values.
C    Read the DATA cards file, and fill the commons
C
C=====================================================================
*	CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
      PARAMETER (LBCS=50000,LCHAR=4)
      COMMON/BCS/ IW(LBCS)
      INTEGER IW
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
C
C
      REAL*8 ALPHA,ALPHA3,ALPHA4,M,PBARN,PI,TWOPI
C
      PARAMETER
     >(          ALPHA = 1.D0/137.036D0, ALPHA3=ALPHA**3,ALPHA4=ALPHA**4
     >,          M     = 0.5110034 D-3
     >,          PBARN = .389386 D9
     >,          PI    = 3.14159265358979 D0 , TWOPI=2.0D0*PI
     >)
C
C
      INTEGER*4 HARD,SOFT,NONE    , EGAMMA,GAMMA,ETRON,GAMMAE
     >,         BK,BKM2,TCHAN,EPA , EPADC,BEEGG,MEEGG,HEEGG
      PARAMETER
     >(          HARD  = 1 , EGAMMA = 11 , BK    = 21 , EPADC = 31
     >,          SOFT  = 2 , ETRON  = 12 , BKM2  = 22 , BEEGG = 32
     >,          NONE  = 3 , GAMMA  = 13 , TCHAN = 23 , MEEGG = 33
     >,                      GAMMAE = 14 , EPA   = 24 , HEEGG = 34
     >)
C
C
      REAL*8        EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS
     >,             WGHT1M,WGHTMX
      INTEGER*4     ISEED, RADCOR,CONFIG,MATRIX,MTRXGG,LNWGHT
      LOGICAL*4     UNWGHT
C
      COMMON/TINPAR/EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS
     >,             WGHT1M,WGHTMX
     >,             ISEED, RADCOR,CONFIG,MATRIX,MTRXGG, UNWGHT
C
C
      INTEGER*4 NRNDMX
      PARAMETER (NRNDMX=20)
C
      REAL*4 RND(NRNDMX)
      INTEGER*4 SEED,NXSEED,BSEED
      COMMON/TRND/RND,SEED,NXSEED,BSEED
      REAL*8 S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC
      COMMON/TCONST/
     >       S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC
C
C
      REAL*8 P(16),RSIGN
      COMMON/TEVENT/P,RSIGN
      REAL*8 T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF
      COMMON/TEVQUA/T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF
C
C
      REAL*8 EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX
     >,      SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER
      INTEGER*4 NTRIAL,NPASSQ,NACC
      COMMON/TSMMRY/EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX
     >,             SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER
     >,             NTRIAL,NPASSQ,NACC
C
C
      REAL*4 GPAR(28)
      COMMON / GENPAR / GPAR
C
      JGENE = NLINK('GENE',0)
      IF(JGENE.NE.0) THEN
       EB     = DBLE(RW(JGENE+1))
       RADCOR = IW(JGENE+2)
       CONFIG = IW(JGENE+3)
       MATRIX = IW(JGENE+4)
       MTRXGG = IW(JGENE+5)
       CUTOFF = DBLE(RW(JGENE+6))
       WGHT1M = DBLE(RW(JGENE+7))
       WGHTMX = DBLE(RW(JGENE+8))
       ISEED  =      IW(JGENE+9)
       EPS    = DBLE(RW(JGENE+10))
       LNWGHT =      IW(JGENE+11)
      ENDIF
C
      JGCUT = NLINK('GCUT',0)
      IF(JGCUT.NE.0) THEN
       TEVETO = DBLE(RW(JGCUT+1))
       TEMIN  = DBLE(RW(JGCUT+2))
       TGVETO = DBLE(RW(JGCUT+3))
       TGMIN  = DBLE(RW(JGCUT+4))
       PHVETO = DBLE(RW(JGCUT+5))
       PEGMIN = DBLE(RW(JGCUT+6))
       CEGMAX = DBLE(RW(JGCUT+7))
       EEMIN  = DBLE(RW(JGCUT+8))
       EGMIN  = DBLE(RW(JGCUT+9))
       EEVETO = DBLE(RW(JGCUT+10))
       EGVETO = DBLE(RW(JGCUT+11))
      ENDIF
C
      IF(LNWGHT.EQ.1) UNWGHT=.TRUE.
      IF(LNWGHT.EQ.0) UNWGHT=.FALSE.
C
C  Fill the GPAR table
      GPAR(1)   = SNGL(EB)
      GPAR(2)   =      RADCOR
      GPAR(3)   =      CONFIG
      GPAR(4)   =      MATRIX
      GPAR(5)   =      MTRXGG
      GPAR(6)   = SNGL(CUTOFF)
      GPAR(7)   = SNGL(WGHT1M)
      GPAR(8)   = SNGL(WGHTMX)
      GPAR(9)   =       ISEED
      GPAR(10)  = SNGL(   EPS)
      GPAR(11)  =      LNWGHT
      GPAR(12)  = SNGL(TEVETO)
      GPAR(13)  = SNGL( TEMIN)
      GPAR(14)  = SNGL(TGVETO)
      GPAR(15)  = SNGL( TGMIN)
      GPAR(16)  = SNGL(PHVETO)
      GPAR(17)  = SNGL(PEGMIN)
      GPAR(18)  = SNGL(CEGMAX)
      GPAR(19)  = SNGL( EEMIN)
      GPAR(20)  = SNGL( EGMIN)
      GPAR(21)  = SNGL(EEVETO)
      GPAR(22)  = SNGL(EGVETO)
C
      RETURN
      END
*DK EEGINI
      SUBROUTINE EEGINI(VALID)
C====================================================================
C    P. GAY (Clermont fd 5/88)
C
C    Init. the D. Karlen generator
C=====================================================================
C
      LOGICAL VALID
      EXTERNAL TEEGGL
      LOGICAL TEEGGL
      REAL*8 ALPHA,ALPHA3,ALPHA4,M,PBARN,PI,TWOPI
C
      PARAMETER
     >(          ALPHA = 1.D0/137.036D0, ALPHA3=ALPHA**3,ALPHA4=ALPHA**4
     >,          M     = 0.5110034 D-3
     >,          PBARN = .389386 D9
     >,          PI    = 3.14159265358979 D0 , TWOPI=2.0D0*PI
     >)
C
      INTEGER*4 HARD,SOFT,NONE    , EGAMMA,GAMMA,ETRON,GAMMAE
     >,         BK,BKM2,TCHAN,EPA , EPADC,BEEGG,MEEGG,HEEGG
      PARAMETER
     >(          HARD  = 1 , EGAMMA = 11 , BK    = 21 , EPADC = 31
     >,          SOFT  = 2 , ETRON  = 12 , BKM2  = 22 , BEEGG = 32
     >,          NONE  = 3 , GAMMA  = 13 , TCHAN = 23 , MEEGG = 33
     >,                      GAMMAE = 14 , EPA   = 24 , HEEGG = 34
     >)
C
      REAL*8        EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS
     >,             WGHT1M,WGHTMX
      INTEGER*4     ISEED, RADCOR,CONFIG,MATRIX,MTRXGG
      LOGICAL*4     UNWGHT
C
      COMMON/TINPAR/EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS
     >,             WGHT1M,WGHTMX
     >,             ISEED, RADCOR,CONFIG,MATRIX,MTRXGG, UNWGHT
C
C
      INTEGER*4 NRNDMX
      PARAMETER (NRNDMX=20)
C
      REAL*4 RND(NRNDMX)
      INTEGER*4 SEED,NXSEED,BSEED
      COMMON/TRND/RND,SEED,NXSEED,BSEED
      REAL*8 S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC
      COMMON/TCONST/
     >       S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC
C
C
      REAL*8 P(16),RSIGN
      COMMON/TEVENT/P,RSIGN
      REAL*8 T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF
      COMMON/TEVQUA/T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF
C
C
      REAL*8 EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX
     >,      SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER
      INTEGER*4 NTRIAL,NPASSQ,NACC
      COMMON/TSMMRY/EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX
     >,             SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER
     >,             NTRIAL,NPASSQ,NACC
C
      INTEGER*4 NEVENT(3),LWRITE
      COMMON / COUNTR / NEVENT,LWRITE
C
C Set the default values for all the parameters
C
      CALL TEEGGI
C
C modify the parameters as desired
C
      CALL RDATA
C
C Initialize the random number
C
*      CALL RNDIN(ISEED)
      WRITE(6,*)'NOT calling RNDIN - ISEED no longer used'
      WRITE(6,*)'Using RANMAR instead with seeds from RMAR card'
C
C Checks the validity of the parameters and calculates some constants
      VALID = .FALSE.
      IF(TEEGGL(LWRITE)) VALID =.TRUE.
C
C Histogramms init.
      IF(VALID) CALL YHINIT
C
      RETURN
      END
*DK EEGGEN
      SUBROUTINE EEGGEN(NBRE,Q,POIDS)
C====================================================================
C    P. GAY (Clermont fd 5/88)
C    modified by G. Bonneaud  June 1988
C    Generate an event
C=====================================================================
      IMPLICIT LOGICAL*1(A-Z)
C
      INTEGER*4 NBRE,I,J
      REAL*4 Q(4,4)
      REAL*4 POIDS
C
      REAL*8 ALPHA,ALPHA3,ALPHA4,M,PBARN,PI,TWOPI
C
      PARAMETER
     >(          ALPHA = 1.D0/137.036D0, ALPHA3=ALPHA**3,ALPHA4=ALPHA**4
     >,          M     = 0.5110034 D-3
     >,          PBARN = .389386 D9
     >,          PI    = 3.14159265358979 D0 , TWOPI=2.0D0*PI
     >)
C
      INTEGER*4 HARD,SOFT,NONE    , EGAMMA,GAMMA,ETRON,GAMMAE
     >,         BK,BKM2,TCHAN,EPA , EPADC,BEEGG,MEEGG,HEEGG
      PARAMETER
     >(          HARD  = 1 , EGAMMA = 11 , BK    = 21 , EPADC = 31
     >,          SOFT  = 2 , ETRON  = 12 , BKM2  = 22 , BEEGG = 32
     >,          NONE  = 3 , GAMMA  = 13 , TCHAN = 23 , MEEGG = 33
     >,                      GAMMAE = 14 , EPA   = 24 , HEEGG = 34
     >)
C
      REAL*8        EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS
     >,             WGHT1M,WGHTMX
      INTEGER*4     ISEED, RADCOR,CONFIG,MATRIX,MTRXGG
      LOGICAL*4     UNWGHT
C
      COMMON/TINPAR/EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS
     >,             WGHT1M,WGHTMX
     >,             ISEED, RADCOR,CONFIG,MATRIX,MTRXGG, UNWGHT
C
C
      INTEGER*4 NRNDMX
      PARAMETER (NRNDMX=20)
C
      REAL*4 RND(NRNDMX)
      INTEGER*4 SEED,NXSEED,BSEED
      COMMON/TRND/RND,SEED,NXSEED,BSEED
      REAL*8 S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC
      COMMON/TCONST/
     >       S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC
C
C
      REAL*8 P(16),RSIGN
      COMMON/TEVENT/P,RSIGN
      REAL*8 T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF
      COMMON/TEVQUA/T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF
C
C
      REAL*8 EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX
     >,      SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER
      INTEGER*4 NTRIAL,NPASSQ,NACC
      COMMON/TSMMRY/EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX
     >,             SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER
     >,             NTRIAL,NPASSQ,NACC
C
C Call the generating routine. (Generates one event).
      CALL TEEGG7
C
C  set the weight of the event
C
      IF(UNWGHT)      POIDS=1.
      IF(.NOT.UNWGHT) POIDS=SNGL(WGHT)
C
C Fill the histograms
      CALL YHFILL(POIDS)
C
C  NBRE  number of particules in the final state
C  Q(I = 1,4 , J = 1,4) are the 4-vectors respectively of the
C  positron,the electron and the two photons (I = 1,4), in the P_x, P_y,
C  P_z, E order (J = 1,4).
C
      NBRE = 3
      IF(RADCOR.EQ.HARD) NBRE = 4
C
       DO 20 I = 1,NBRE
        DO 10 J = 1,4
   10    Q(I,J) = SNGL(P((I-1)*4+J))
   20 CONTINUE
C
      RETURN
      END
*DK YHINIT
      SUBROUTINE YHINIT
C======================
C     init. all the histograms
C    P. GAY (Clermont fd 5/88)
C
C======================
      REAL*8 EMAX , BAS , HAUT ,FAC
      REAL*8 ALPHA,ALPHA3,ALPHA4,M,PBARN,PI,TWOPI
C
      PARAMETER
     >(          ALPHA = 1.D0/137.036D0, ALPHA3=ALPHA**3,ALPHA4=ALPHA**4
     >,          M     = 0.5110034 D-3
     >,          PBARN = .389386 D9
     >,          PI    = 3.14159265358979 D0 , TWOPI=2.0D0*PI
     >)
C
      INTEGER*4 HARD,SOFT,NONE    , EGAMMA,GAMMA,ETRON,GAMMAE
     >,         BK,BKM2,TCHAN,EPA , EPADC,BEEGG,MEEGG,HEEGG
      PARAMETER
     >(          HARD  = 1 , EGAMMA = 11 , BK    = 21 , EPADC = 31
     >,          SOFT  = 2 , ETRON  = 12 , BKM2  = 22 , BEEGG = 32
     >,          NONE  = 3 , GAMMA  = 13 , TCHAN = 23 , MEEGG = 33
     >,                      GAMMAE = 14 , EPA   = 24 , HEEGG = 34
     >)
C
      REAL*8        EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS
     >,             WGHT1M,WGHTMX
      INTEGER*4     ISEED, RADCOR,CONFIG,MATRIX,MTRXGG
      LOGICAL*4     UNWGHT
C
      COMMON/TINPAR/EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS
     >,             WGHT1M,WGHTMX
     >,             ISEED, RADCOR,CONFIG,MATRIX,MTRXGG, UNWGHT
C
C
      INTEGER*4 NRNDMX
      PARAMETER (NRNDMX=20)
C
      REAL*4 RND(NRNDMX)
      INTEGER*4 SEED,NXSEED,BSEED
      COMMON/TRND/RND,SEED,NXSEED,BSEED
      REAL*8 S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC
      COMMON/TCONST/
     >       S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC
C
C
      REAL*8 P(16),RSIGN
      COMMON/TEVENT/P,RSIGN
      REAL*8 T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF
      COMMON/TEVQUA/T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF
C
C
      REAL*8 EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX
     >,      SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER
      INTEGER*4 NTRIAL,NPASSQ,NACC
      COMMON/TSMMRY/EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX
     >,             SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER
     >,             NTRIAL,NPASSQ,NACC
C
C
C
C     CALL HDELET(0)
C
      BAS   = 0.D0
      HAUT  = DABS(WGHTMX)
      IF(RADCOR.NE.HARD)THEN
       CALL HBOOK1(9001,' Wgt WGHT T3BODY',50,real(BAS),real(HAUT),0.)
      ELSE
       CALL HBOOK1(9002,' Wgt WGHT T4BODY',50,real(BAS),real(HAUT),0.)
      ENDIF
C
      BAS  = 0.D0
      HAUT = DABS(WGHT1M)
      IF(RADCOR.NE.HARD)THEN
       CALL HBOOK1(9003,'Wgt WGHT1 T3BODY',50,real(BAS),real(HAUT),0.)
      ELSE
       CALL HBOOK1(9004,'Wgt WGHT1 T4BODY',50,real(BAS),real(HAUT),0.)
      ENDIF
C
      EMAX      =       EB
C
      CALL HBOOK1(10001,' Photon1 energy (GeV)$',50,0.,real(EMAX),0.)
      IF(RADCOR.EQ.HARD)THEN
       CALL HBOOK1(10002,' Photon2 energy (GeV)$',50,0.,real(EMAX),0.)
      ENDIF
C
      CALL HBOOK1(10003,' Electron energy (GeV)$',50,0.,real(EMAX),0.)
      CALL HBOOK1(10004,' Positron energy (GeV)$',50,0.,real(EMAX),0.)
      CALL HBOOK1(10005,' cos photon1 angle $',50,-1.,1.,0.)
      IF(RADCOR.EQ.HARD)THEN
       CALL HBOOK1(10006,' cos photon2 angle $',50,-1.,1.,0.)
      ENDIF
C
      CALL HBOOK1(10007,' cos e-  angle $',50,-1.,1.,0.)
      CALL HBOOK1(10008,' e-  angle (degrees) $',50,0.,90.,0.)
      CALL HBOOK1(10009,' cos e+  angle $',50,-1.,1.,0.)
      CALL HBOOK1(10010,' 180 - e+ angle (degrees) $',50,0.,90.,0.)
C
C
      RETURN
      END
*DK YHFILL
      SUBROUTINE YHFILL(POIDS)
C======================
C    P. GAY (Clermont fd 5/88)
C
C     Fill  the histograms with kinematic parameter
C======================
      REAL*8 ALPHA,ALPHA3,ALPHA4,M,PBARN,PI,TWOPI
C
      PARAMETER
     >(          ALPHA = 1.D0/137.036D0, ALPHA3=ALPHA**3,ALPHA4=ALPHA**4
     >,          M     = 0.5110034 D-3
     >,          PBARN = .389386 D9
     >,          PI    = 3.14159265358979 D0 , TWOPI=2.0D0*PI
     >)
C
      INTEGER*4 HARD,SOFT,NONE    , EGAMMA,GAMMA,ETRON,GAMMAE
     >,         BK,BKM2,TCHAN,EPA , EPADC,BEEGG,MEEGG,HEEGG
      PARAMETER
     >(          HARD  = 1 , EGAMMA = 11 , BK    = 21 , EPADC = 31
     >,          SOFT  = 2 , ETRON  = 12 , BKM2  = 22 , BEEGG = 32
     >,          NONE  = 3 , GAMMA  = 13 , TCHAN = 23 , MEEGG = 33
     >,                      GAMMAE = 14 , EPA   = 24 , HEEGG = 34
     >)
C
      REAL*8        EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS
     >,             WGHT1M,WGHTMX
      INTEGER*4     ISEED, RADCOR,CONFIG,MATRIX,MTRXGG
      LOGICAL*4     UNWGHT
C
      COMMON/TINPAR/EB,TEVETO,TEMIN,TGMIN,TGVETO,EEMIN,EGMIN
     >,             PEGMIN,CEGMAX,EEVETO,EGVETO,PHVETO,CUTOFF,EPS
     >,             WGHT1M,WGHTMX
     >,             ISEED, RADCOR,CONFIG,MATRIX,MTRXGG, UNWGHT
C
C
      INTEGER*4 NRNDMX
      PARAMETER (NRNDMX=20)
C
      REAL*4 RND(NRNDMX)
      INTEGER*4 SEED,NXSEED,BSEED
      COMMON/TRND/RND,SEED,NXSEED,BSEED
      REAL*8 S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC
      COMMON/TCONST/
     >       S,SQRTS,EBP,EPSLON,CDELT,CDELT1,CTGVT1,ACTEM,ACTK
     >,      FQPMAX,QP0MIN,ZMAX,LOGZ0M,LOGRSM,FACT3,FACT7,CTGM1M,ASOFTC
C
C
      REAL*8 P(16),RSIGN
      COMMON/TEVENT/P,RSIGN
      REAL*8 T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF
      COMMON/TEVQUA/T,TP,SP,U,UP,X1,X2,Y1,Y2,DSIGE,WGHT,WGHTSF
      INTEGER*4 NEVENT(3),LWRITE
      COMMON / COUNTR / NEVENT,LWRITE
C
C
      REAL*8 EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX
     >,      SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER
      INTEGER*4 NTRIAL,NPASSQ,NACC
      COMMON/TSMMRY/EFFIC,SIGE,SIGEO,ERSIGE,W1MAX,WMAX,WMINSF,Q2W2MX
     >,             SUMW1,SUMW12,SUMWGT,SUMWGTO,SUMW2,CONVER
     >,             NTRIAL,NPASSQ,NACC
C
      REAL*8 PMOD,CT,ANGLE
C
C Fill the gamma1, gamma2, (e- or e+)  energy
C
      CALL HFILL(10001,real(P(12)),0.,POIDS)
      CALL HFILL(10002,real(P(16)),0.,POIDS)
      CALL HFILL(10003,real(P(8)),0.,POIDS)
      CALL HFILL(10004,real(P(4)),0.,POIDS)
C
C Fill the gamma1, gamma2, (e- or e+)  cosinus angle
C
      PMOD=P( 9)**2+P(10)**2+P(11)**2
      PMOD=DSQRT(PMOD)
      IF(PMOD.EQ.0.D0) GOTO 100
      CT=P(11)/PMOD
      CALL HFILL(10005,real(CT),0.,POIDS)
 100  CONTINUE
C
      PMOD=P(13)**2+P(14)**2+P(15)**2
      PMOD=DSQRT(PMOD)
      IF(PMOD.EQ.0.D0) GOTO 200
      CT=P(15)/PMOD
      CALL HFILL(10006,real(CT),0.,POIDS)
 200  CONTINUE
C
      PMOD=P( 5)**2+P( 6)**2+P( 7)**2
      PMOD=DSQRT(PMOD)
      IF(PMOD.EQ.0.D0) GOTO 300
      CT=-P( 7)/PMOD
      ANGLE=DACOS(DABS(CT))*180.D0/PI
C     WRITE(LWRITE,123)CT,ANGLE
      CALL HFILL(10008,real(ANGLE),0.,POIDS)
      CALL HFILL(10007,real(CT),0.,POIDS)
 300  CONTINUE
C
      PMOD=P( 1)**2+P( 2)**2+P( 3)**2
      PMOD=DSQRT(PMOD)
      IF(PMOD.EQ.0.D0) GOTO 300
      CT=-P( 3)/PMOD
      ANGLE=DACOS(DABS(CT))*180.D0/PI
C     WRITE(LWRITE,123)CT,ANGLE
      CALL HFILL(10010,real(ANGLE),0.,POIDS)
      CALL HFILL(10009,reAL(CT),0.,POIDS)
 400  CONTINUE
C
 123  FORMAT(2X,F16.9,4X,F16.9)
C
      RETURN
      END
