      SUBROUTINE ASKUSI(IGCOD)                                          ASKUSI 2
C--------------------------------------------------------------------   ASKUSI 3
C                E. Milotti                                             ASKUSI 4
C                G. Bonneaud  April 26, 1988                            ASKUSI 5
C                             April  3, 1989                            ASKUSI 6
C                                                                       ASKUSI 7
C                B.Bloch march 96 , create RLEP bank                    BBL001 1
C        initialization routine for EVT3GA                              ASKUSI 8
C                                                                       ASKUSI 9
C--------------------------------------------------------------------   ASKUSI10
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      PARAMETER (LBCS=50000,LCHAR=4)                                    BCS    5
      COMMON/BCS/ IW(LBCS)                                              BCS    6
      INTEGER IW                                                        BCS    7
      REAL RW(LBCS)                                                     BCS    8
      EQUIVALENCE (RW(1),IW(1))                                         BCS    9
      COMMON / COUNTR / NEVENT(3),LWRITE                                COUNTR 2
C                                                                       ASKUSI13
      PARAMETER ( IGCO = 1005)                                          ASKUSI14
C                                                                       ASKUSI15
      DIMENSION SVRT(3),GPAR(6)                                         ASKUSI16
      INTEGER ALTABL ,ALRLEP                                            BBL001 2
C                                                                       ASKUSI18
      INTEGER ONLY                                                      ASKUSI19
      DIMENSION Q(4,3)                                                  ASKUSI20
C                                                                       ASKUSI21
C   Return the generator code as defined in the KINGAL library          ASKUSI22
C                                                                       ASKUSI23
      IGCOD = IGCO                                                      ASKUSI24
C                                                                       ASKUSI25
      LWRITE = IW(6)                                                    ASKUSI26
      WRITE(LWRITE,101) IGCOD                                           ASKUSI27
 101  FORMAT(/,10X,                                                     ASKUSI28
     &       'MGAM01 - CODE NUMBER =',I4,' Last mod. March 5 ,1996',    BBL001 3
     & /,10X,'*************************************************',//)    ASKUSI30
C                                                                       ASKUSI31
C   get Generator parameters ( from a data card if you want             ASKUSI32
C    or by default values if you prefer)                                ASKUSI33
C                                                                       ASKUSI34
      EBEAM = 46.1                                                      ASKUSI35
      EMIN  =  0.2                                                      ASKUSI36
      ONLY  =  0                                                        ASKUSI37
      JGENE=NLINK('GENE',0)                                             ASKUSI38
      IF (JGENE.NE.0) THEN                                              ASKUSI39
       EBEAM = RW(JGENE+1)                                              ASKUSI40
       EMIN  = RW(JGENE+2)                                              ASKUSI41
       ONLY  = IW(JGENE+3)                                              ASKUSI42
      ENDIF                                                             ASKUSI43
      GPAR(1) = EBEAM                                                   ASKUSI44
      GPAR(2) = EMIN                                                    ASKUSI45
      GPAR(3) = ONLY                                                    ASKUSI46
C                                                                       ASKUSI47
C  Main vertex smearing                                                 ASKUSI48
C                                                                       ASKUSI49
      SVRT(1) = 0.035                                                   ASKUSI50
      SVRT(2) = 0.0012                                                  ASKUSI51
      SVRT(3) = 1.28                                                    ASKUSI52
      JSVRT=NLINK('SVRT',0)                                             ASKUSI53
      IF (JSVRT.NE.0) THEN                                              ASKUSI54
       SVRT(1)=RW(JSVRT+1)                                              ASKUSI55
       SVRT(2)=RW(JSVRT+2)                                              ASKUSI56
       SVRT(3)=RW(JSVRT+3)                                              ASKUSI57
      ENDIF                                                             ASKUSI58
      GPAR(4) = SVRT(1)                                                 ASKUSI59
      GPAR(5) = SVRT(2)                                                 ASKUSI60
      GPAR(6) = SVRT(3)                                                 ASKUSI61
C                                                                       ASKUSI62
C  Fill the KPAR bank                                                   ASKUSI63
C                                                                       ASKUSI64
      NCOL = 6                                                          ASKUSI65
      NROW = 1                                                          ASKUSI66
      JKPAR = ALTABL('KPAR',NCOL,NROW,GPAR,'2I,(F)','C')                ASKUSI67
C                                                                       ASKUSI68
C  Initialize counters                                                  ASKUSI69
C                                                                       ASKUSI70
      DO 10 I = 1,3                                                     ASKUSI71
   10  NEVENT(I) = 0                                                    ASKUSI72
C                                                                       ASKUSI73
C  Then init the generator with needed parameters                       ASKUSI74
C                                                                       ASKUSI75
C------------------------------------------------------------------     ASKUSI76
C                                                                       ASKUSI77
      LENTRY = -1                                                       ASKUSI78
      CALL EVT3GA(EBEAM,EMIN,ONLY,LWRITE,LENTRY,NGA,Q)                  ASKUSI79
C                                                                       ASKUSI80
C---------------------------------------------------------------------- ASKUSI81
C  Print PART and KLIN banks                                            ASKUSI82
C                                                                       ASKUSI83
      CALL PRPART                                                       ASKUSI84
C                                                                       ASKUSI85
      CALL PRTABL('KPAR',0)                                             ASKUSI86
C                                                                       ASKUSI87
      IEBEAM = NINT(EBEAM*1000)                                         BBL001 4
      JRLEP = ALRLEP(IEBEAM,'   ',0,0,0)                                BBL001 5
      CALL PRTABL('RLEP',0)                                             BBL001 6
C                                                                       BBL001 7
      RETURN                                                            ASKUSI88
      END                                                               ASKUSI89
      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)                 ASKUSE 2
C--------------------------------------------------------------------   ASKUSE 3
C                                                                       ASKUSE 4
C                 E. Milotti                                            ASKUSE 5
C                 G. Bonneaud  April 26, 1988                           ASKUSE 6
C                              April  3, 1989                           ASKUSE 7
C                                                                       ASKUSE 8
C - call generator routine to generate one event                        ASKUSE 9
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
      COMMON / COUNTR / NEVENT(3),LWRITE                                COUNTR 2
C                                                                       ASKUSE24
      DIMENSION SVRT(3),VERT(4),TABL(4)                                 ASKUSE25
      DIMENSION Q(4,3)                                                  ASKUSE26
      INTEGER ALTABL                                                    ASKUSE27
      INTEGER ONLY                                                      ASKUSE28
      DATA IFIRST / 0 /                                                 ASKUSE29
C                                                                       ASKUSE30
      LENTRY = 0                                                        ASKUSE31
C                                                                       ASKUSE32
C  Generate vertex position                                             ASKUSE33
C                                                                       ASKUSE34
      CALL RANNOR(RX,RY)                                                ASKUSE35
      CALL RANNOR(RZ,DUM)                                               ASKUSE36
      IF(IFIRST.EQ.0) THEN                                              ASKUSE37
       IFIRST = 1                                                       ASKUSE38
       JKPAR = NLINK('KPAR',0)                                          ASKUSE39
       SVRT(1)=RW(JKPAR+4+LMHLEN)                                       ASKUSE40
       SVRT(2)=RW(JKPAR+5+LMHLEN)                                       ASKUSE41
       SVRT(3)=RW(JKPAR+6+LMHLEN)                                       ASKUSE42
      ENDIF                                                             ASKUSE43
      VERT(1)=RX*SVRT(1)                                                ASKUSE44
      VERT(2)=RY*SVRT(2)                                                ASKUSE45
      VERT(3)=RZ*SVRT(3)                                                ASKUSE46
      VERT(4)=0.                                                        ASKUSE47
C                                                                       ASKUSE48
C   Book VERT bank #1                                                   ASKUSE49
C                                                                       ASKUSE50
      IND=KBVERT(1,VERT,0)                                              ASKUSE51
      IF (IND.LE.0) GO TO 20                                            ASKUSE52
C                                                                       ASKUSE53
C   This generator returns either 2 or 3 photons                        ASKUSE54
C   Initial electrons beams are E1( e+) and E2 (e-)                     ASKUSE55
C                                                                       ASKUSE56
C---------------------------------------------------------------------- ASKUSE57
C     we get an event generated by the following call                   ASKUSE58
      NEVENT(1) = NEVENT(1) + 1                                         ASKUSE59
      CALL EVT3GA(EBEAM,EMIN,ONLY,LWRITE,LENTRY,NGA,Q)                  ASKUSE60
C                                                                       ASKUSE61
C---------------------------------------------------------------------- ASKUSE62
      IDPR = NGA                                                        ASKUSE63
      NTRK = NGA                                                        ASKUSE64
      NVRT = 1                                                          ASKUSE65
      ISTA = 0                                                          ASKUSE66
      ECMS = EBEAM*2                                                    ASKUSE67
      WEIT = 1.                                                         ASKUSE68
C                                                                       ASKUSE69
C   Book KINE banks for beam electrons                                  ASKUSE70
C                                                                       ASKUSE71
      TABL(1) = 0.                                                      ASKUSE72
      TABL(2) = 0.                                                      ASKUSE73
      TABL(3) = -EBEAM                                                  ASKUSE74
      TABL(4) = 0.                                                      ASKUSE75
      IND=KBKINE(-1,TABL,2,0)                                           ASKUSE76
      TABL(3) =  EBEAM                                                  ASKUSE77
      TABL(4) = 0.                                                      BBL001 8
      JND=KBKINE(-2,TABL,3,0)                                           ASKUSE78
      IF (IND*JND.EQ.0) GO TO 20                                        ASKUSE79
C                                                                       ASKUSE80
C   Book KINE banks for outgoing particles                              ASKUSE81
C                                                                       ASKUSE82
      DO 5,I=1,NTRK                                                     ASKUSE83
       DO 4 J = 1,3                                                     ASKUSE84
    4  TABL(J) = Q(J,I)                                                 ASKUSE85
       TABL(4) = 0.                                                     ASKUSE86
       IND=KBKINE(I,TABL,1,1)                                           ASKUSE87
       IF (IND.EQ.0) GO TO 20                                           ASKUSE88
    5 CONTINUE                                                          ASKUSE89
C                                                                       ASKUSE90
C    Fill the history bank KHIS                                         ASKUSE91
C   Here the history is simple as all 3 outgoing particles come from    ASKUSE92
C   the original interaction. Use an array(3) filled with zeroes        ASKUSE93
C                                                                       ASKUSE94
      DO 10 I=1,NTRK                                                    ASKUSE95
   10 TABL(I)=0.                                                        ASKUSE96
      IND=ALTABL('KHIS',1,NTRK,TABL,'I','E')                            ASKUSE97
      IF (IND.LE.0) GO TO 20                                            ASKUSE98
      NEVENT(2) = NEVENT(2) + 1                                         ASKUSE99
      RETURN                                                            ASKUS100
   20 ISTA=1                                                            ASKUS101
      NEVENT(3) = NEVENT(3) + 1                                         ASKUS102
      RETURN                                                            ASKUS103
      END                                                               ASKUS104
      SUBROUTINE USCJOB                                                 USCJOB 2
C-------------------------------------------------------------------    USCJOB 3
C  End of job routine                                                   USCJOB 4
C                                                                       USCJOB 5
C------------------------------------------------------------------     USCJOB 6
      COMMON / COUNTR / NEVENT(3),LWRITE                                COUNTR 2
C                                                                       USCJOB 8
      DIMENSION Q(4,3)                                                  USCJOB 9
      INTEGER ONLY                                                      USCJOB10
C                                                                       USCJOB11
C  Final printout                                                       USCJOB12
C                                                                       USCJOB13
      LENTRY = 1                                                        USCJOB14
      CALL EVT3GA(EBEAM,EMIN,ONLY,LWRITE,LENTRY,NGA,Q)                  USCJOB15
C                                                                       USCJOB16
       WRITE(LWRITE,101)                                                USCJOB17
  101  FORMAT(//20X,'EVENTS AND ERRORS STATISTICS',                     USCJOB18
     &         /20X,'****************************')                     USCJOB19
       WRITE(LWRITE,102)NEVENT(1),NEVENT(2),NEVENT(3)                   USCJOB20
  102  FORMAT(/5X,'# OF GENERATED EVENTS                       = ',I10, USCJOB21
     &        /5X,'# OF ACCEPTED  EVENTS                       = ',I10, USCJOB22
     &        /5X,'# OF REJECTED  EVENTS (ISTA # 0 IN ASKUSE)  = ',I10) USCJOB23
      RETURN                                                            USCJOB24
      END                                                               USCJOB25
      SUBROUTINE EVT3GA(EBM,EMN,IONLY,LWRITE,MODE,NGA,Q)                EVT3GA 2
C                                                                       EVT3GA 3
C--------------------------------------------------------------------   EVT3GA 4
C                                                                       EVT3GA 5
C  MC routine for the generation of two and three photon events         EVT3GA 6
C  Theory and formulae are in :                                         EVT3GA 7
C  1. Eidelman & Kuraev Nucl.Phys. B143 (1978), 353                     EVT3GA 8
C  2. Berends & Kleiss Nucl.Phys. B186 (1981), 22                       EVT3GA 9
C                                                                       EVT3GA10
C  Author: Edoardo Milotti/Univ.di Trieste-Italy                        EVT3GA11
C                                                                       EVT3GA12
C  Input:  EBM     beam energy (and then the CM energy is 2E)           EVT3GA13
C          EMN     is the energy limit to photon detection              EVT3GA14
C          IONLY   selects the kind of event generation:                EVT3GA15
C                  ONLY=2 gives two-photon events only, while           EVT3GA16
C                  ONLY=3 gives three-photon events only.               EVT3GA17
C                  All other values give both 2- and 3-photon evts.     EVT3GA18
C          LWRITE  logical unit for output                              EVT3GA19
C          MODE    =-1 initialization (must be called before event loop)EVT3GA20
C                  = 0 normal event mode (returns one event)            EVT3GA21
C                  = 1 final print-out (optional)                       EVT3GA22
C                  the above input variables have meaning only for      EVT3GA23
C                  initialization mode; in the other modes they become  EVT3GA24
C                  output variables with the same value as at           EVT3GA25
C                  the initialisation step                              EVT3GA26
C  Output: NGA     number of emitted photons (2 or 3)                   EVT3GA27
C          Qi      photon four-vector momenta (Qi(4)=photon energy)     EVT3GA28
C                  the output variables have meaning only for normal    EVT3GA29
C                  mode                                                 EVT3GA30
C                                                                       EVT3GA31
C  All energies in GeV                                                  EVT3GA32
C                                                                       EVT3GA33
C  The routine uses the following subroutines from the CERN library     EVT3GA34
C  RNDM  VDOT                                                           EVT3GA35
C                                                                       EVT3GA36
C                                                                       EVT3GA37
C                                                                       EVT3GA38
      IMPLICIT REAL*8 (A-H,O-Z)                                         EVT3GA39
C                                                                       EVT3GA40
      COMMON /CONS3G/ PI,ALFA,EMASS                                     EVT3GA41
      REAL*8 M,K,KP                                                     EVT3GA42
      COMMON /VAR3G/ EBEAM,PBEAM,E,S,M,V,K,KP,X1,X2,X3,C1,C2,C3,WEIGHT  EVT3GA43
C                                                                       EVT3GA44
      REAL*4 EBM,EMN                                                    EVT3GA45
      DIMENSION IGAM(3)                                                 EVT3GA46
      INTEGER ONLY,MODE,LWRITE,NGA                                      EVT3GA47
      REAL*4 Q(4,3),XCOS,XCOS1,XCOS2                                    EVT3GA48
      REAL*4 RNDM,DUMMY,VDOT                                            BBL001 9
      EXTERNAL RNDM,rnd1dd,vdot                                         BBL00110
C                                                                       EVT3GA51
      EXTERNAL XSEC1,XSEC2,XSEC3,XSEC4                                  EVT3GA52
      PARAMETER (EPS=1D-4)                                              EVT3GA53
C  Conversion factor       GeV**(-2) ---> nbarn conversion              EVT3GA54
      DATA HC/3.89385D5/                                                EVT3GA55
C  Stirling's approximation                                             EVT3GA56
      FACT(J)=SQRT(2*3.1415926*J)*(J/2.718281828)**J                    EVT3GA57
C                                                                       EVT3GA58
C  Initialization                                                       EVT3GA59
C                                                                       EVT3GA60
      IF(MODE.EQ.-1) THEN                                               EVT3GA61
C                                                                       EVT3GA62
        ALFA = 0.00729735D0                                             EVT3GA63
        PI = 3.14159265358979D0                                         EVT3GA64
        EMASS = 0.5110034D-3                                            EVT3GA65
        NEV=0                                                           EVT3GA66
        N2=0                                                            EVT3GA67
        N3=0                                                            EVT3GA68
        NT2=0                                                           EVT3GA69
        NT3=0                                                           EVT3GA70
C                                                                       EVT3GA71
        LU=LWRITE                                                       EVT3GA72
C                                                                       EVT3GA73
        ONLY=IONLY                                                      EVT3GA74
        EMIN=DBLE(EMN)                                                  EVT3GA75
        EBEAM=DBLE(EBM)                                                 EVT3GA76
        PBEAM=SQRT(EBEAM**2-EMASS**2)                                   EVT3GA77
C  Normalized energy cut                                                EVT3GA78
        K=EMIN/PBEAM                                                    EVT3GA79
C  1/beta for original electrons                                        EVT3GA80
        E=EBEAM/PBEAM                                                   EVT3GA81
        KP=E-K                                                          EVT3GA82
        S=4*EBEAM**2                                                    EVT3GA83
        M=EMASS/PBEAM                                                   EVT3GA84
        V=LOG(2/M)                                                      EVT3GA85
C                                                                       EVT3GA86
        CSEC0=XSEC1(1.D0)                                               EVT3GA87
        CSEC2=CSEC0+(2*ALFA**3/S)*(2*(2*V-1)**2*LOG(K)+                 EVT3GA88
     +      4./3.*V**3+3*V*V+(2./3.*PI*PI-6.)*V-PI*PI/12.)              EVT3GA89
        IF(CSEC2.LT.0) WRITE(LU,*) '*** WARNING: CSEC2 LT 0 ***'        EVT3GA90
        CSEC3=XSEC2(E)                                                  EVT3GA91
        IF(CSEC3.LT.0) WRITE(LU,*) '*** WARNING: CSEC3 LT 0 ***'        EVT3GA92
        CSEC3T=(2*ALFA**3/S)*(-(2*V-1)**2*(2*LOG(K)-1.)+3.)             EVT3GA93
        CSECTO=CSEC0+(2*ALFA**3/S)*(4./3.*V**3-V**2+                    EVT3GA94
     +         2*V*(PI*PI/3-1)+6.606-PI*PI/3)+                          EVT3GA95
     +         2*ALFA**4/3/PI/S*V**5                                    EVT3GA96
        DL=2*ALFA*V**2/PI                                               EVT3GA97
        CMULT=8*PI*ALFA**2*V/S                                          EVT3GA98
        J=3                                                             EVT3GA99
   20   CONTINUE                                                        EVT3G100
        CORR=CMULT*DL**J/FACT(J)/FACT(J+2)                              EVT3G101
        CSECTO=CSECTO+CORR                                              EVT3G102
        J=J+1                                                           EVT3G103
        IF(CORR.GT.0.001*CSEC0) GOTO 20                                 EVT3G104
C                                                                       EVT3G105
C GB    WRITE(LU,'(/1X,''EVT3GA initialization: '')')                   EVT3G106
        WRITE(LU,'(/1X,''EVT3GA initialization: '',/1X,                 EVT3G107
     &                 ''**********************'')')                    EVT3G108
        WRITE(LU,'(/1X,''Beam energy                        : '',F9.5,  EVT3G109
     &                                       ''    GeV'')') REAL(EBEAM) EVT3G110
        WRITE(LU,'(1X,''Threshold energy                   : '',F9.5,   EVT3G111
     +                                       ''    GeV'')') REAL(EMIN)  EVT3G112
        WRITE(LU,'(1X,''First-order cross-section          : '',F9.5,   EVT3G113
     +        '' nbarns'')') REAL(CSEC0*HC)                             EVT3G114
        WRITE(LU,'(1X,''Cross-section for 2-photon evts.   : '',F9.5,   EVT3G115
     +        '' nbarns'')') REAL(CSEC2*HC)                             EVT3G116
        WRITE(LU,'(1X,''Cross-section for 3-photon evts.   : '',F9.5,   EVT3G117
     +        '' nbarns'')') REAL(CSEC3T*HC)                            EVT3G118
        WRITE(LU,'(1X,''Approx. X-sect. for 3-photon evts. : '',F9.5,   EVT3G119
     +        '' nbarns'')') REAL(CSEC3*HC)                             EVT3G120
        WRITE(LU,'(1X,''Total estimated cross-section      : '',F9.5,   EVT3G121
     +        '' nbarns'')') REAL(CSECTO*HC)                            EVT3G122
        IF(ONLY.EQ.2.OR.ONLY.EQ.3)                                      EVT3G123
     +    WRITE(LU,'(1X,''Only '',I1,''-photon events'')') ONLY         EVT3G124
        IF(ONLY.EQ.2) CSEC3=0                                           EVT3G125
        IF(ONLY.EQ.3) CSEC2=0                                           EVT3G126
        CSECT=CSEC2+CSEC3                                               EVT3G127
C        WRITE(LU,'(/1X,''Other quantities of interest: '')')           EVT3G128
C        WRITE(LU,'('' E-1: '',G11.5)') REAL(E-1)                       EVT3G129
C        WRITE(LU,'('' K: '',G11.5)') REAL(K)                           EVT3G130
C        WRITE(LU,'('' M: '',G11.5)') REAL(M)                           EVT3G131
C        WRITE(LU,'(//1X,''dsigma3/dx'',/)')                            EVT3G132
C          DO 6,I=1,10                                                  EVT3G133
C            X3=1D-1*I                                                  EVT3G134
C            IF(X3.LT.K) GOTO 6                                         EVT3G135
C            XSEC=XSEC2(X3)-XSEC2(K)                                    EVT3G136
C            WRITE(LU,'(/2X,G11.5,5X,G11.5)') REAL(X3),REAL(XSEC*HC)    EVT3G137
C            DO 6,J=-10,10                                              EVT3G138
C              C1=1D-1*J                                                EVT3G139
C              XSEC=XSEC3(C1)-XSEC3(-1D0)                               EVT3G140
C              WRITE(LU,'(5X,G11.5,5X,G11.5)') REAL(C1),REAL(XSEC*HC)   EVT3G141
C    6     CONTINUE                                                     EVT3G142
C                                                                       EVT3G143
C  Booking of some standard histogrammes                                EVT3G144
C                                                                       EVT3G145
        CALL HBOOK1(10001,'Cosine of the polar angle : final gammas$',  EVT3G146
     &                                           40,-1.,1.,0.)          EVT3G147
        CALL HIDOPT(10001,'LOGY')                                       EVT3G148
        CALL HBOOK2(10002,'Cosine gamma#1 versus Cosine gamma#2$',      EVT3G149
     &                                  40,-1.,1.,40,-1.,1.,0.)         EVT3G150
        CALL HBOOK1(10003,'Energy distribution : final gamma1$',        EVT3G151
     &                                           30,0.,60.,0.)          EVT3G152
        CALL HIDOPT(10003,'LOGY')                                       EVT3G153
        CALL HBOOK1(10004,'Energy distribution : final gamma2$',        EVT3G154
     &                                           30,0.,60.,0.)          EVT3G155
        CALL HIDOPT(10004,'LOGY')                                       EVT3G156
        CALL HBOOK2(10005,'Energy gamma#1 versus Energy gamma#2$',      EVT3G157
     &                                   30,0.,60.,30,0.,60.,0.)        EVT3G158
        CALL HBOOK1(10006,'Energy distribution : final gamma3$',        EVT3G159
     &                                           30,0.,60.,0.)          EVT3G160
        CALL HBOOK1(10007,'3-photons events : Acolinearity distribution EVT3G161
     &gamma #1 - gamma #2 (Degrees)$',40,0.,80.,0.)                     EVT3G162
        CALL HIDOPT(10007,'LOGY')                                       EVT3G163
        RETURN                                                          EVT3G164
C                                                                       EVT3G165
      END IF                                                            EVT3G166
C                                                                       EVT3G167
C  Final printout                                                       EVT3G168
C                                                                       EVT3G169
      IF(MODE.EQ.1) THEN                                                EVT3G170
        WRITE(LU,'(///1X,''EVT3GA final report: '',/1X,                 EVT3G171
     &                   ''******************** '')')                   EVT3G172
        WRITE(LU,'(/1X,I9,'' events have been generated'')') NEV        EVT3G173
        WRITE(LU,'(/1X,I9,'' 2-photon events have been generated'')') N2EVT3G174
        WRITE(LU,'(12X,''out of '',I9,'' 2-photon events tried'')') NT2 EVT3G175
        WRITE(LU,'(/1X,I9,'' 3-photon events have been generated'')') N3EVT3G176
        WRITE(LU,'(12X,''out of '',I9,'' 3-photon events tried'')') NT3 EVT3G177
C                                                                       EVT3G178
        RETURN                                                          EVT3G179
C                                                                       EVT3G180
      END IF                                                            EVT3G181
C                                                                       EVT3G182
C  normal event mode                                                    EVT3G183
C                                                                       EVT3G184
      NEV=NEV+1                                                         EVT3G185
      EBM=REAL(EBEAM)                                                   EVT3G186
      EMN=REAL(EMIN)                                                    EVT3G187
      ONLY=IONLY                                                        EVT3G188
      LWRITE=LU                                                         EVT3G189
C                                                                       EVT3G190
      IF(RNDM(DUMMY).LT.CSEC2/CSECT) THEN                               EVT3G191
C  2-photon event                                                       EVT3G192
    1   CONTINUE                                                        EVT3G193
        NT2=NT2+1                                                       EVT3G194
        C=RND1DD(XSEC1,-1D0,1D0,EPS,IERR)                               EVT3G195
        C1=C                                                            EVT3G196
        WEIGHT=WEI2(C1)                                                 EVT3G197
        IF(2*RNDM(DUMMY).GT.WEIGHT) GOTO 1                              EVT3G198
        NGA=2                                                           EVT3G199
        N2=N2+1                                                         EVT3G200
C                                                                       EVT3G201
        S1=SQRT(1-C1*C1)                                                EVT3G202
        C2=-C1                                                          EVT3G203
        X1=E                                                            EVT3G204
        X2=E                                                            EVT3G205
        PHI=2.*PI*RNDM(DUMMY)                                           BBL00111
C                                                                       EVT3G207
        Q(1,1)=EBEAM*S1*COS(PHI)                                        EVT3G208
        Q(2,1)=EBEAM*S1*SIN(PHI)                                        EVT3G209
        Q(3,1)=EBEAM*C1                                                 EVT3G210
        Q(4,1)=EBEAM                                                    EVT3G211
C                                                                       EVT3G212
        Q(1,2)=-Q(1,1)                                                  EVT3G213
        Q(2,2)=-Q(2,1)                                                  EVT3G214
        Q(3,2)=-Q(3,1)                                                  EVT3G215
        Q(4,2)=EBEAM                                                    EVT3G216
C                                                                       EVT3G217
      ELSE                                                              EVT3G218
C  3-photon event                                                       EVT3G219
C                                                                       EVT3G220
    2   CONTINUE                                                        EVT3G221
        NT3=NT3+1                                                       EVT3G222
        X=RND1DD(XSEC2,K,E,EPS,IERR)                                    EVT3G223
        X3=X                                                            EVT3G224
        C=RND1DD(XSEC3,-1D0,1D0,EPS,IERR)                               EVT3G225
        C1=C                                                            EVT3G226
        C=RND1DD(XSEC4,-1D0,1D0,EPS,IERR)                               EVT3G227
        C3=C                                                            EVT3G228
C                                                                       EVT3G229
        S1=SQRT(1-C1*C1)                                                EVT3G230
        S3=SQRT(1-C3*C3)                                                EVT3G231
        PH1=2*PI*RNDM(DUMMY)                                            EVT3G232
C                                                                       EVT3G233
C  unit vectors                                                         EVT3G234
        Q(1,1)=S1*COS(PH1)                                              EVT3G235
        Q(2,1)=S1*SIN(PH1)                                              EVT3G236
        Q(3,1)=C1                                                       EVT3G237
        Q(4,1)=1.                                                       EVT3G238
        Q(1,3)=S3                                                       EVT3G239
        Q(2,3)=0.                                                       EVT3G240
        Q(3,3)=C3                                                       EVT3G241
        Q(4,3)=1.                                                       EVT3G242
C                                                                       EVT3G243
C  compute X1,X2                                                        EVT3G244
        X1=2.*E*(E-X3)/(2.*E-X3+X3*VDOT(Q(1,1),Q(1,3),3))               EVT3G245
C        IF(X1.LT.K) GOTO 2                                             EVT3G246
        X2=2.*E-X1-X3                                                   EVT3G247
        C2=-(C1*X1+C3*X3)/X2                                            EVT3G248
C                                                                       EVT3G249
C  find Q's                                                             EVT3G250
        DO 5 I=1,3                                                      EVT3G251
          Q(I,1)=PBEAM*X1*Q(I,1)                                        EVT3G252
          Q(I,3)=PBEAM*X3*Q(I,3)                                        EVT3G253
          Q(I,2)=-Q(I,1)-Q(I,3)                                         EVT3G254
    5   CONTINUE                                                        EVT3G255
        Q(4,1)=PBEAM*X1                                                 EVT3G256
        Q(4,2)=PBEAM*X2                                                 EVT3G257
        Q(4,3)=PBEAM*X3                                                 EVT3G258
C                                                                       EVT3G259
        WEIGHT=WEI3(Q)                                                  EVT3G260
        IF(2*RNDM(DUMMY).GT.WEIGHT) GOTO 2                              EVT3G261
C                                                                       EVT3G262
C  now that the event has been accepted, random rotation about beam axisEVT3G263
        PHI=2.*PI*RNDM(DUMMY)                                           EVT3G264
        SP=SIN(PHI)                                                     EVT3G265
        CP=COS(PHI)                                                     EVT3G266
        DO 10 I=1,3                                                     EVT3G267
          Q1=     CP*Q(1,I)+SP*Q(2,I)                                   EVT3G268
          Q(2,I)=-SP*Q(1,I)+CP*Q(2,I)                                   EVT3G269
          Q(1,I)=Q1                                                     EVT3G270
   10   CONTINUE                                                        EVT3G271
C                                                                       EVT3G272
        NGA=3                                                           EVT3G273
        N3=N3+1                                                         EVT3G274
C                                                                       EVT3G275
      END IF                                                            EVT3G276
C                                                                       EVT3G277
C  Fill the histograms                                                  EVT3G278
C                                                                       EVT3G279
C  search for the lowest energy photon IGAM(3)                          EVT3G280
      DO 30 I = 1,NGA                                                   EVT3G281
      XCOS = Q(3,I) / Q(4,I)                                            EVT3G282
      IF(XCOS.GE.+1.) XCOS=+0.999999                                    EVT3G283
      IF(XCOS.LE.-1.) XCOS=-0.999999                                    EVT3G284
   30 CALL HFILL(10001,XCOS,0.,1.)                                      EVT3G285
      DO 40 I = 1,3                                                     EVT3G286
   40 IGAM(I) = I                                                       EVT3G287
      IF(NGA.EQ.3) THEN                                                 EVT3G288
       DO 50 I = 1,2                                                    EVT3G289
       IF(Q(4,I).LT.Q(4,IGAM(3))) THEN                                  EVT3G290
        IGAM(I) = IGAM(3)                                               EVT3G291
        IGAM(3) = I                                                     EVT3G292
       ENDIF                                                            EVT3G293
   50  CONTINUE                                                         EVT3G294
       CALL HFILL(10006,REAL(Q(4,IGAM(3))),0.,1.)                       EVT3G295
      ENDIF                                                             EVT3G296
      XCOS1 = Q(3,IGAM(1))/Q(4,IGAM(1))                                 EVT3G297
      IF(XCOS1.GE.+1.) XCOS1=+0.999999                                  EVT3G298
      IF(XCOS1.LE.-1.) XCOS1=-0.999999                                  EVT3G299
      XCOS2 = Q(3,IGAM(2))/Q(4,IGAM(2))                                 EVT3G300
      IF(XCOS2.GE.+1.) XCOS2=+0.999999                                  EVT3G301
      IF(XCOS2.LE.-1.) XCOS2=-0.999999                                  EVT3G302
      CALL HFILL(10002,XCOS1,XCOS2,1.)                                  EVT3G303
      CALL HFILL(10003,REAL(Q(4,IGAM(1))),0.,1.)                        EVT3G304
      CALL HFILL(10004,REAL(Q(4,IGAM(2))),0.,1.)                        EVT3G305
      CALL HFILL(10005,REAL(Q(4,IGAM(1))),REAL(Q(4,IGAM(2))),1.)        EVT3G306
      IF(NGA.EQ.3) THEN                                                 EVT3G307
       ACOLIN =      -(Q(1,IGAM(1))*Q(1,IGAM(2))+                       EVT3G308
     &                 Q(2,IGAM(1))*Q(2,IGAM(2))+                       EVT3G309
     &                 Q(3,IGAM(1))*Q(3,IGAM(2)))/(                     EVT3G310
     &                 Q(4,IGAM(1))*Q(4,IGAM(2)))                       EVT3G311
       IF(DABS(ACOLIN).GE.1.0D0) ACOLIN = ACOLIN*0.99999999/DABS(ACOLIN)EVT3G312
       ACOLIN = ACOS(ACOLIN)*180./3.14159                               EVT3G313
       CALL HFILL(10007,ACOLIN,0.,1.)                                   EVT3G314
      ENDIF                                                             EVT3G315
C                                                                       EVT3G316
      RETURN                                                            EVT3G317
      END                                                               EVT3G318
