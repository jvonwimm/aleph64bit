      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)                 ASKUSE 2
C ------------------------------------------------------------          ASKUSE 3
C -  B. Bloch  - March 1992                                             ASKUSE 4
C! get an event from ARIADNE 4.01                                       ASKUSE 5
C  then transfer the information into KINE and VERT banks.              ASKUSE 6
C                                                                       ASKUSE 7
C                                                                       ASKUSE 8
C     structure : subroutine                                            ASKUSE 9
C     output arguments :                                                ASKUSE10
C          IDPR   : process identification,each digit corresponds to    ASKUSE11
C          the flavor of the evnt ( several flavors /event is possible) ASKUSE12
C          ISTA   : status flag ( 0 means ok), use it to reject         ASKUSE13
C                   unwanted events                                     ASKUSE14
C          NTRK   : number of tracks generated and kept                 ASKUSE15
C                  (i.e. # KINE banks  written)                         ASKUSE16
C          NVRT   : number of vertices generated                        ASKUSE17
C                   (i.e. # VERT banks written)                         ASKUSE18
C          ECMS   : center of mass energy for the event (may be         ASKUSE19
C                   different from nominal cms energy)                  ASKUSE20
C          WEIT   : event weight ( not 1 if a weighting method is used) ASKUSE21
C -----------------------------------------------------------------     ASKUSE22
      REAL VRTX(4)                                                      ASKUSE23
      INTEGER KFL(8)                                                    ASKUSE24
C                                                                       ASKUSE25
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      PARAMETER (L1MST=200, L1PAR=200)                                  LUNDCOM2
      PARAMETER (L2PAR=500, L2PARF=2000 )                               LUNDCOM3
      PARAMETER (LJNPAR=4000)                                           LUNDCOM4
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)   LUNDCOM5
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)LUNDCOM6
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),        LUNDCOM7
     &                KFDP(L2PARF,5)                                    LUNDCOM8
      COMMON /LUDAT4/ CHAF(L2PAR)                                       LUNDCOM9
      CHARACTER*8 CHAF                                                  LUNDCO10
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5) LUNDCO11
C                                                                       LUNDCO12
      COMMON / GLUPAR / SVERT(3),IFL,ECM,IPROC                          GLUCOM 2
C     IFL      : LUND flavour , set to 0 by default, can be changed     GLUCOM 3
C     ECM      : nominal cms energy                                     GLUCOM 4
C     SVERT    : vertex smearing, set to 0. by default, can be changed  GLUCOM 5
C     IPROC    : process number : 1= ARIADNE , 2 = pure JETSET 7.3      GLUCOM 6
C                                 3= PYTHIA  , 4 = LEPTO                GLUCOM 7
      COMMON / GLUSTA / ICOULU(10)                                      GLUCOM 8
      DATA IFI / 0/                                                     ASKUSE29
C initialize photon counter                                             ASKUSE30
      NPH=0                                                             ASKUSE31
C ------------------------------------------------------------------    ASKUSE32
C                                                                       ASKUSE33
      ISTA = 0                                                          ASKUSE34
C                                                                       ASKUSE35
C   get an event from the generator                                     ASKUSE36
C                                                                       ASKUSE37
      IFI  =  IFI + 1                                                   ASKUSE38
C - set the process identification IDPR = IFL                           ASKUSE39
      IDPR = IFL                                                        ASKUSE40
C - set the cms energy for this event ECMS = ECM                        ASKUSE41
      ECMS = ECM                                                        ASKUSE42
      WEIT = 1.                                                         ASKUSE43
C - get an event from ARIADNE generator                                 ASKUSE44
      IF (IPROC.EQ.1 .OR. IPROC.EQ.2) THEN                              ASKUSE45
        CALL LUEEVT (IFL,ECMS)                                          ASKUSE46
      ENDIF                                                             ASKUSE47
C count number of photons emitted                                       ASKUSE48
      DO 200 I=1,N7LU                                                   ASKUSE49
        IF(K7LU(I,2).EQ.22) NPH=NPH+1                                   ASKUSE50
200   CONTINUE                                                          ASKUSE51
      CALL HFILL(10022,FLOAT(NPH),0.,1.)                                ASKUSE52
C do fragmentation                                                      ASKUSE53
      IF (IPROC.EQ.1) THEN                                              ASKUSE54
         CALL AREXEC                                                    ASKUSE55
      ELSE IF ( IPROC.EQ.2) THEN                                        ASKUSE56
         CALL LUEXEC                                                    ASKUSE57
      ENDIF                                                             ASKUSE58
C                                                                       ASKUSE59
C   fill BOS banks                                                      ASKUSE60
C                                                                       ASKUSE61
C - get the primary vertex                                              ASKUSE62
      CALL RANNOR (RX,RY)                                               ASKUSE63
      CALL RANNOR (RZ,DUM)                                              ASKUSE64
      VRTX(1) = RX*SVERT(1)                                             ASKUSE65
      VRTX(2) = RY*SVERT(2)                                             ASKUSE66
      VRTX(3) = RZ*SVERT(3)                                             ASKUSE67
      VRTX(4) = 0.                                                      ASKUSE68
      IF (IFI.LE.5) CALL LULIST(1)                                      ASKUSE69
      DO 30 ITR = 1 , N7LU                                              ASKUSE70
        IMOTH = K7LU(ITR,3)                                             ASKUSE71
        IMTYP = IABS(K7LU(IMOTH,2))                                     ASKUSE72
        ILUN = K7LU(ITR,2)                                              ASKUSE73
C   Final state radiation photon comes from a quark                     ASKUSE74
        IF (ILUN.EQ.22 .AND. IMTYP.GE.1 .AND. IMTYP.LE.6)               ASKUSE75
     &       CALL HFILL(10023,P7LU(ITR,4),DUM,1.)                       ASKUSE76
C  Main vertex particles                                                ASKUSE77
        IF (IMOTH.LE.1.AND. (ILUN.EQ.22))THEN                           ASKUSE78
C    There is a photon                                                  ASKUSE79
           CALL HFILL(10003,P7LU(ITR,4),DUM,1.)                         ASKUSE80
        ELSEIF (ILUN.GT.0 .AND. ILUN.LE.6.AND.IMTYP.EQ.23) THEN         ASKUSE81
C   fermion                                                             ASKUSE82
           CALL HFILL(10001,P7LU(ITR,4),DUM,1.)                         ASKUSE83
           EE=P7LU(ITR,3)/SQRT(P7LU(ITR,1)**2+P7LU(ITR,2)**2+           ASKUSE84
     $       P7LU(ITR,3)**2)                                            ASKUSE85
           CALL HFILL(10004,EE,DUM,1.)                                  ASKUSE86
        ELSEIF (ILUN.LT.0 .AND. ILUN.GE.-6.AND.IMTYP.EQ.23)  THEN       ASKUSE87
C   anti-fermion                                                        ASKUSE88
           CALL HFILL(10002,P7LU(ITR,4),DUM,1.)                         ASKUSE89
           EE= P7LU(ITR,3)/SQRT(P7LU(ITR,1)**2+P7LU(ITR,2)**2+          ASKUSE90
     &     P7LU(ITR,3)**2)                                              ASKUSE91
           CALL HFILL(10005,EE,DUM,1.)                                  ASKUSE92
      ENDIF                                                             ASKUSE93
  30  CONTINUE                                                          ASKUSE94
      EE=N7LU                                                           ASKUSE95
      CALL HFILL(10006,EE,DUM,1.)                                       ASKUSE96
C                                                                       ASKUSE97
C      Call the specific routine KXL7AL to fill BOS banks               ASKUSE98
C      the secondary vertices are propagated                            ASKUSE99
        CALL KXL7AL (VRTX,ISTA,NVRT,NTRK)                               ASKUS100
C                                                                       ASKUS101
C   Update IDPR                                                         ASKUS102
C                                                                       ASKUS103
        IDPR = 0                                                        ASKUS104
C Look for flavor generated                                             ASKUS105
        DO 5 I=1,8                                                      ASKUS106
 5      KFL(I)=0                                                        ASKUS107
        NFL=0                                                           ASKUS108
        DO 40 I=1,N7LU                                                  ASKUS109
           ITYP=ABS(KLU(I,9))                                           ASKUS110
           IF (ITYP.GT.8 .OR. ITYP.EQ.0) GO TO 40                       ASKUS111
           IF ( NFL.GT.0) THEN                                          ASKUS112
              DO 41 J=1,NFL                                             ASKUS113
              IF (ITYP.EQ.KFL(J)) GO TO 40                              ASKUS114
  41          CONTINUE                                                  ASKUS115
           ENDIF                                                        ASKUS116
           NFL=NFL+1                                                    ASKUS117
           KFL(NFL)=ITYP                                                ASKUS118
           IDPR=10*IDPR+ITYP                                            ASKUS119
  40    CONTINUE                                                        ASKUS120
C                                                                       ASKUS121
      IF (MSTU(24).NE.0) THEN                                           ASKUS122
        WRITE(6,'(''  ---ERROR LUEXEC AT EVENT #  '',I10)') IFI         ASKUS123
        CALL LULIST(1)                                                  ASKUS124
        ISTA = -8                                                       ASKUS125
      ENDIF                                                             ASKUS126
      IF (ISTA.EQ.0 ) THEN                                              ASKUS127
         ICOULU(10) = ICOULU(10)+1                                      ASKUS128
      ELSEIF (ISTA.GT.0) THEN                                           ASKUS129
         ICOULU(1) = ICOULU(1) +1                                       ASKUS130
         ICOULU(9) = ICOULU(9) +1                                       ASKUS131
      ELSEIF ( ISTA.LT.0) THEN                                          ASKUS132
         ICOULU(-ISTA) = ICOULU(-ISTA) +1                               ASKUS133
         ICOULU(9) = ICOULU(9) +1                                       ASKUS134
      ENDIF                                                             ASKUS135
      RETURN                                                            ASKUS136
      END                                                               ASKUS137
      SUBROUTINE ASKUSI(IGCOD)                                          ASKUSI 2
C ------------------------------------------------------------------    ASKUSI 3
C - B. Bloch  - September 1990                                          ASKUSI 4
C! Initialization routine of ARIADNE 3.3  generator                     ASKUSI 5
C ------------------------------------------------------------------    ASKUSI 6
      PARAMETER (L1MST=200, L1PAR=200)                                  LUNDCOM2
      PARAMETER (L2PAR=500, L2PARF=2000 )                               LUNDCOM3
      PARAMETER (LJNPAR=4000)                                           LUNDCOM4
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)   LUNDCOM5
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)LUNDCOM6
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),        LUNDCOM7
     &                KFDP(L2PARF,5)                                    LUNDCOM8
      COMMON /LUDAT4/ CHAF(L2PAR)                                       LUNDCOM9
      CHARACTER*8 CHAF                                                  LUNDCO10
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5) LUNDCO11
C                                                                       LUNDCO12
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      COMMON / GLUPAR / SVERT(3),IFL,ECM,IPROC                          GLUCOM 2
C     IFL      : LUND flavour , set to 0 by default, can be changed     GLUCOM 3
C     ECM      : nominal cms energy                                     GLUCOM 4
C     SVERT    : vertex smearing, set to 0. by default, can be changed  GLUCOM 5
C     IPROC    : process number : 1= ARIADNE , 2 = pure JETSET 7.3      GLUCOM 6
C                                 3= PYTHIA  , 4 = LEPTO                GLUCOM 7
      COMMON / GLUSTA / ICOULU(10)                                      GLUCOM 8
      COMMON /ARDAT1/ PARA(40),MSTA(40)                                 ARDAT1 2
      SAVE /ARDAT1/                                                     ARDAT1 3
      PARAMETER (LPDEC=48)                                              ASKUSI11
      INTEGER NODEC(LPDEC)                                              ASKUSI12
      INTEGER ALTABL,ALRLEP                                             ASKUSI13
      EXTERNAL ALTABL,ALRLEP                                            ASKUSI14
      DIMENSION TABL(25),MODE(4)                                        ASKUSI15
      CHARACTER*6  MODE                                                 ASKUSI16
      CHARACTER*4  CHAINT,LEPM                                          BBL001 2
C    IGCOD  for ARIADNE 4.01                                            ASKUSI17
      PARAMETER ( IGCO  =  5018)                                        ASKUSI18
      PARAMETER ( IPMX  =  2   )                                        ASKUSI19
      DATA MODE /'JETSET','JETSET','PYTHIA','LEPTO'/                    ASKUSI20
C - # of words/row in bank with index ID                                BMACRO 2
      LCOLS(ID) = IW(ID+1)                                              BMACRO 3
C - # of rows in bank with index ID                                     BMACRO 4
      LROWS(ID) = IW(ID+2)                                              BMACRO 5
C - index of next row in the bank with index ID                         BMACRO 6
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)                       BMACRO 7
C - index of row # NRBOS in the bank with index ID                      BMACRO 8
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)                 BMACRO 9
C - # of free words in the bank with index ID                           BMACRO10
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)                              BMACRO11
C - # of free rows in the bank with index ID                            BMACRO12
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)                               BMACRO13
C - Lth integer element of the NRBOSth row of the bank with index ID    BMACRO14
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)            BMACRO15
C - Lth real element of the NRBOSth row of the bank with index ID       BMACRO16
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)            BMACRO17
C                                                                       BMACRO18
C -----------------------------------------------------------------     ASKUSI22
C - get the LUND flavour IFL if given on data card                      ASKUSI23
         NLUND = NAMIND ('GARI')                                        ASKUSI24
         JLUND = IW(NLUND)                                              ASKUSI25
         IF (JLUND .NE. 0) THEN                                         ASKUSI26
            IFL = IW(JLUND+1)                                           ASKUSI27
            ECM  =  RW(JLUND+2)                                         ASKUSI28
            IPRT = IW(JLUND+3)                                          ASKUSI29
            IPROC= IW(JLUND+4)                                          ASKUSI30
            IF ( IPROC.GT.IPMX)THEN                                     ASKUSI31
               WRITE (IUT,'(''   ++++++++++ THIS OPTION IS NOT YET IMPLEASKUSI32
     $MENTED.....MAX OPTION IS '',I10)') IPMX                           ASKUSI33
               CALL EXIT                                                ASKUSI34
            ENDIF                                                       ASKUSI35
            LEPM = '    '                                               BBL001 3
            IF (JLUND.GT.4 ) LEPM   = CHAINT(IW(JLUND+5))               BBL001 4
         ELSE                                                           ASKUSI36
            IFL = 0                                                     ASKUSI37
            ECM = 91.2                                                  ASKUSI38
            IPRT = 0                                                    ASKUSI39
            IPROC= 1                                                    ASKUSI40
         ENDIF                                                          ASKUSI41
C - make use of a smearing of the vertex                                ASKUSI42
C   if it is given                                                      ASKUSI43
         NSVER = NAMIND ('SVRT')                                        ASKUSI44
         JSVER = IW(NSVER)                                              ASKUSI45
         IF (JSVER .NE. 0) THEN                                         ASKUSI46
            SVERT(1) = RW(JSVER+1)                                      ASKUSI47
            SVERT(2) = RW(JSVER+2)                                      ASKUSI48
            SVERT(3) = RW(JSVER+3)                                      ASKUSI49
         ELSE                                                           ASKUSI50
            SVERT(1) = 0.035                                            ASKUSI51
            SVERT(2) = 0.0012                                           ASKUSI52
            SVERT(3) = 1.28                                             ASKUSI53
         ENDIF                                                          ASKUSI54
C                                                                       ASKUSI55
      IUT = IW(6)                                                       ASKUSI56
C                                                                       ASKUSI57
C   Return generator code IGCOD                                         ASKUSI58
C                                                                       ASKUSI59
      IGCOD = IGCO                                                      ASKUSI60
      WRITE(IUT,101) IGCOD                                              ASKUSI61
 101  FORMAT(/,10X,                                                     ASKUSI62
     &       'ARIA02 - CODE NUMBER =',I4,' Last modification ',         ASKUSI63
     $ 'October  9,1995'                                                BBL002 1
     & ,/,10X,'****************************************************',//)ASKUSI65
C                                                                       ASKUSI66
C   Issue the relevant parameters                                       ASKUSI67
C                                                                       ASKUSI68
      WRITE (IUT,1000) IPROC,MODE(IPROC)                                ASKUSI69
      WRITE (IUT,1007)                                                  ASKUSI70
 1000 FORMAT(1X,88('*'),/,/,10X,'WELCOME TO ARIADNE version 4,  ARIA02',BBL001 5
     $ /,1X,'*',25X,' Process chosen :',I8 ,' initial state provided by'ASKUSI72
     $ ,A10,'   ',10X,'*')                                              ASKUSI73
 1007 FORMAT (1X,88('*') )                                              ASKUSI74
C                                                                       ASKUSI75
      DO 5  K=1,10                                                      ASKUSI76
 5    ICOULU(K)=0                                                       ASKUSI77
C  Set up some default values for masses and initial conditions         ASKUSI80
      PMAS(LUCOMP(25),1)= 100.                                          ASKUSI81
      PMAS(LUCOMP( 6),1)= 100.                                          ASKUSI82
      PMAS(LUCOMP(23),1)= 91.2                                          ASKUSI83
C   HIGGS Mass , TOP Mass and Z0 mass defined, can be overwritten by    ASKUSI84
C   a PMA1 card                                                         ASKUSI85
C    init user's JETSET                                                 ASKUSI86
C -- complete PART bank with LUND  particles                            ASKUSI87
C    use the library routine KXL7PA                                     ASKUSI88
      CALL KXL7PA (IPART,IKLIN)                                         ASKUSI89
      IF (IPART.LE.0 .OR. IKLIN.LE.0) THEN                              ASKUSI90
         WRITE (IW(6),'(1X,''error in PART or KLIN bank - STOP - ''     ASKUSI91
     +                 ,2I3)') IPART,IKLIN                              ASKUSI92
         CALL EXIT                                                      ASKUSI93
      ENDIF                                                             ASKUSI94
      IF ( IPROC.NE.2 ) THEN                                            ASKUSI95
C    init gene                                                          ASKUSI96
          CALL ARINIT(MODE(IPROC))                                      ASKUSI97
          IF ( LEPM.EQ.'ALEP') CALL ARTUNE('ALEPH')                     BBL001 6
          IF ( LEPM.EQ.'DELP') CALL ARTUNE('DELPHI')                    BBL001 7
          IF ( LEPM.EQ.'OPAL') CALL ARTUNE('OPAL')                      BBL001 8
C    init user's ARIADNE                                                ASKUSI98
          CALL KXARCO(LAPAR)                                            ASKUSI99
      ENDIF                                                             ASKUS100
C   Make sure that masses in PART bank are consistent                   ASKUS101
      NAPAR = NAMIND('PART')                                            ASKUS102
C This is the aleph number of the Z0(lund code=23),top (6) and Higgs(25)ASKUS103
C function KGPART returns the ALEPH code corresponding to the LUND code ASKUS104
C required.                                                             ASKUS105
      JPART = IW(NAPAR)                                                 ASKUS106
      IZPART = KGPART(23)                                               ASKUS107
      IF (IZPART.GT.0)  THEN                                            ASKUS108
        ZMAS = PMAS(LUCOMP(23),1)                                       ASKUS109
        KPART = KROW(JPART,IZPART)                                      ASKUS110
        RW(KPART+6)=ZMAS                                                ASKUS111
        IANTI = ITABL(JPART,IZPART,10)                                  ASKUS112
        IF (IANTI.NE.IZPART) THEN                                       ASKUS113
          KAPAR = KROW(JPART,IANTI)                                     ASKUS114
          RW(KAPAR+6)=ZMAS                                              ASKUS115
        ENDIF                                                           ASKUS116
      ENDIF                                                             ASKUS117
      ITPART = KGPART(6)                                                ASKUS118
      IF (ITPART.GT.0)  THEN                                            ASKUS119
        ZMAS = PMAS(LUCOMP( 6),1)                                       ASKUS120
        KPART = KROW(JPART,ITPART)                                      ASKUS121
        RW(KPART+6)=ZMAS                                                ASKUS122
        IANTI = ITABL(JPART,ITPART,10)                                  ASKUS123
        IF (IANTI.NE.ITPART) THEN                                       ASKUS124
          KAPAR = KROW(JPART,IANTI)                                     ASKUS125
          RW(KAPAR+6)=ZMAS                                              ASKUS126
        ENDIF                                                           ASKUS127
      ENDIF                                                             ASKUS128
      IHPART = KGPART(25)                                               ASKUS129
      IF (IHPART.GT.0)  THEN                                            ASKUS130
        ZMAS = PMAS(LUCOMP(25),1)                                       ASKUS131
        KPART = KROW(JPART,IHPART)                                      ASKUS132
        RW(KPART+6)=ZMAS                                                ASKUS133
        IANTI = ITABL(JPART,IHPART,10)                                  ASKUS134
        IF (IANTI.NE.IHPART) THEN                                       ASKUS135
          KAPAR = KROW(JPART,IANTI)                                     ASKUS136
          RW(KAPAR+6)=ZMAS                                              ASKUS137
        ENDIF                                                           ASKUS138
      ENDIF                                                             ASKUS139
C                                                                       ASKUS140
C - Print PART and KLIN banks                                           ASKUS141
      IF (IPRT.GT.0) CALL PRPART                                        ASKUS142
C                                                                       ASKUS143
C -- get list of  particle# which should not be decayed                 ASKUS144
C    in LUND  because they are decayed in GALEPH.                       ASKUS145
C    the routines uses the KLIN bank and fills the user array           ASKUS146
C    NODEC in the range [1-LPDEC]                                       ASKUS147
      MXDEC = KNODEC (NODEC,LPDEC)                                      ASKUS148
      MXDEC = MIN (MXDEC,LPDEC)                                         ASKUS149
C                                                                       ASKUS150
C -- inhibit decays in LUND                                             ASKUS151
C    If the user has set some decay channels by data cards they will    ASKUS152
C    will not be overwritten                                            ASKUS153
      IF (MXDEC .GT. 0) THEN                                            ASKUS154
         DO 10 I=1,MXDEC                                                ASKUS155
            IF (NODEC(I).GT.0) THEN                                     ASKUS156
               JIDB = NLINK('MDC1',NODEC(I))                            ASKUS157
               IF (JIDB .EQ. 0) MDCY(LUCOMP(NODEC(I)),1) = 0            ASKUS158
            ENDIF                                                       ASKUS159
   10    CONTINUE                                                       ASKUS160
      ENDIF                                                             ASKUS161
C                                                                       ASKUS162
      CALL HBOOK1(10001,'OUTGOING FERMION ENERGY$',50,0.,50.,0.)        ASKUS163
      CALL HBOOK1(10002,'OUTGOING ANTIFERMION ENERGY$',50,0.,50.,0.)    ASKUS164
      CALL HBOOK1(10003,'ISR PHOTON ENERGY IF ANY$',50,0.,20.,0.)       ASKUS165
      CALL HBOOK1(10004,'POLAR ANGLE FERMION$',50,-1.,1.,0.)            ASKUS166
      CALL HBOOK1(10005,'POLAR ANGLE ANTIFERMION$',50,-1.,1.,0.)        ASKUS167
      CALL HBOOK1(10006,'FINAL MULTIPLICITY GENERATED$',50,20.,120.,0.) ASKUS168
      CALL HBOOK1(10023,'FSR PHOTON ENERGIES IF ANY      ',50,0.,20.,0.)ASKUS169
      CALL HBOOK1(10022,'# OF PHOTON PER EVENT    ',50,0.,50.,0.)       ASKUS170
C                                                                       ASKUS171
C   dump the generator parameters for this run in a bank                ASKUS172
C assume all parameters are real and stored as a single row             ASKUS173
      TABL(1) = FLOAT(IFL)                                              ASKUS174
      TABL(2) = ECM                                                     ASKUS175
      TABL(3) = FLOAT(IPROC)                                            ASKUS176
      DO 11 I=1,3                                                       ASKUS177
 11   TABL(3+I) = SVERT(I)                                              ASKUS178
      TABL(7) = INTCHA(LEPM)                                            BBL001 9
      NWB = 7                                                           BBL00110
      IND = ALTABL('KPAR',NWB,1,TABL,'2I,(F)','C')                      ASKUS180
C                                                                       ASKUS181
C  Fill RLEP bank                                                       ASKUS182
      IEBEAM = NINT(ECM* 500  )                                         ASKUS183
      JRLEP = ALRLEP(IEBEAM,'    ',0,0,0)                               ASKUS184
      CALL PRTABL('RLEP',0)                                             ASKUS185
      CALL PRTABL('KPAR',0)                                             ASKUS186
      IF ( IPROC.NE.2) CALL ARPRDA                                      ASKUS187
      RETURN                                                            ASKUS188
      END                                                               ASKUS189
      SUBROUTINE USCJOB                                                 USCJOB 2
C-------------------------------------------------------------------    USCJOB 3
C! End of job routine    ARIADNE 4.01                                   USCJOB 4
C                                                                       USCJOB 5
C   To be filled by user to print any relevant info                     USCJOB 6
C                                                                       USCJOB 7
C------------------------------------------------------------------     USCJOB 8
      PARAMETER (L1MST=200, L1PAR=200)                                  LUNDCOM2
      PARAMETER (L2PAR=500, L2PARF=2000 )                               LUNDCOM3
      PARAMETER (LJNPAR=4000)                                           LUNDCOM4
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)   LUNDCOM5
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)LUNDCOM6
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),        LUNDCOM7
     &                KFDP(L2PARF,5)                                    LUNDCOM8
      COMMON /LUDAT4/ CHAF(L2PAR)                                       LUNDCOM9
      CHARACTER*8 CHAF                                                  LUNDCO10
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5) LUNDCO11
C                                                                       LUNDCO12
      COMMON / GLUPAR / SVERT(3),IFL,ECM,IPROC                          GLUCOM 2
C     IFL      : LUND flavour , set to 0 by default, can be changed     GLUCOM 3
C     ECM      : nominal cms energy                                     GLUCOM 4
C     SVERT    : vertex smearing, set to 0. by default, can be changed  GLUCOM 5
C     IPROC    : process number : 1= ARIADNE , 2 = pure JETSET 7.3      GLUCOM 6
C                                 3= PYTHIA  , 4 = LEPTO                GLUCOM 7
      COMMON / GLUSTA / ICOULU(10)                                      GLUCOM 8
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
C.......................................................................USCJOB12
       IUT=IW(6)                                                        USCJOB13
       WRITE(IUT,101)                                                   USCJOB14
  101  FORMAT(//20X,'EVENTS STATISTICS',                                USCJOB15
     &         /20X,'*****************')                                USCJOB16
       WRITE(IUT,102) ICOULU(9)+ICOULU(10),ICOULU(10),ICOULU(9)         USCJOB17
  102  FORMAT(/5X,'# OF GENERATED EVENTS                = ',I10,        USCJOB18
     &        /5X,'# OF ACCEPTED  EVENTS                = ',I10,        USCJOB19
     &        /5X,'# OF REJECTED  EVENTS                = ',I10)        USCJOB20
       WRITE(IUT,103)                                                   USCJOB21
  103  FORMAT(//20X,'REJECT STATISTICS',                                USCJOB22
     &         /20X,'*****************')                                USCJOB23
       WRITE(IUT,104) (ICOULU(I),I=1,8)                                 USCJOB24
  104  FORMAT(/10X,'IR= 1 LUND ERROR unknown part    # OF REJECT =',I10,USCJOB25
     &        /10X,'IR= 2 BOS  ERROR KINE/VERT       # OF REJECT =',I10,USCJOB26
     &        /10X,'IR= 3 LUND ERROR too many tracks # OF REJECT =',I10,USCJOB27
     &        /10X,'IR= 4 LUND ERROR Beam wrong pos  # OF REJECT =',I10,USCJOB28
     &        /10X,'IR= 5 LUND ERROR status code >5  # OF REJECT =',I10,USCJOB29
     &        /10X,'IR= 6 free for user              # OF REJECT =',I10,USCJOB30
     &        /10X,'IR= 7 free for user              # OF REJECT =',I10,USCJOB31
     &        /10X,'IR= 8 LUND error in LUEXEC       # OF REJECT =',I10)USCJOB32
        WRITE (IUT,105) (PARJ(II),II=141,148)                           USCJOB33
 105    FORMAT(//,20X,'FINAL RESULTS FROM LUND GENERATION',             USCJOB34
     $          /,20X,'**********************************',             USCJOB35
     $          /,10X,'R value as given in massless QED   :',F10.5,     USCJOB36
     $          /,10X,'R value including weak effects     :',F10.5,     USCJOB37
     $          /,10X,'R value including QCD corrections  :',F10.5,     USCJOB38
     $          /,10X,'R value including I.S.R. effects   :',F10.5,     USCJOB39
     $          /,10X,'Absolute cross sections in nb      :',           USCJOB40
     $          /,10X,'As given  in massless QED   :',F10.5,            USCJOB41
     $          /,10X,'Including  weak effects     :',F10.5,            USCJOB42
     $          /,10X,'Including  QCD corrections  :',F10.5,            USCJOB43
     $          /,10X,'Including  I.S.R. effects   :',F10.5)            USCJOB44
      RETURN                                                            USCJOB45
      END                                                               USCJOB46
      SUBROUTINE KXARCO(LAPAR)                                          KXARCO 2
C-------------------------------------------------------------------    KXARCO 3
C                                                                       KXARCO 5
C - Modified for Ariadne 4.01       B.Bloch   - 920330                  KXARCO 6
C                                                                       KXARCO 7
C! Set ARIADNE parameters by data cards                                 KXARCO 8
CKEY KINE KINGAL   /  USER INTERNAL                                     KXARCO 9
C  Every ARIADNE parameter is a BOS data card keyword,the index of the  KXARCO10
C  parameter is the bank number.                                        KXARCO11
C                                                                       KXARCO12
C  the list of keywords with their format is given below:               KXARCO13
C                                                                       KXARCO14
C 'MSTA'(I),'PARA'(F)                                                   KXARCO15
C                                                                       KXARCO16
C                                                                       KXARCO17
C    KEY  i  /  ival     ====>  KEY(i)=ival                             KXARCO18
C    RKEY i  /  value    ====>  RKEY(i)=value                           KXARCO19
C                                                                       KXARCO20
C - structure: SUBROUTINE subprogram                                    KXARCO21
C              User Entry Name: KXARCO                                  KXARCO22
C              External References: NAMIND/BKFMT/BLIST(BOS77)           KXARCO23
C              Comdecks referenced: BCS,ARDAT1                          KXARCO24
C                                                                       KXARCO25
C - usage    : CALL KXARCO(LUPAR)                                       KXARCO26
C - output  : LUPAR=No. of read data cards                              KXARCO27
C                                                                       KXARCO28
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      COMMON /ARDAT1/ PARA(40),MSTA(40)                                 ARDAT1 2
      SAVE /ARDAT1/                                                     ARDAT1 3
C                                                                       KXARCO31
      PARAMETER (LKEYS=2)                                               KXARCO32
      CHARACTER*4 KEY(LKEYS),CHAINT                                     KXARCO33
      CHARACTER*1 FMT(LKEYS)                                            KXARCO34
      DATA KEY / 'MSTA','PARA'/                                         KXARCO35
      DATA FMT /'I','F'/                                                KXARCO36
      LUPAR=0                                                           KXARCO37
      DO 50 I=1,LKEYS                                                   KXARCO38
         NAMI=NAMIND(KEY(I))                                            KXARCO39
         IF (IW(NAMI).EQ.0) GOTO 50                                     KXARCO40
         KIND=NAMI+1                                                    KXARCO41
   15    KIND=IW(KIND-1)                                                KXARCO42
         IF (KIND.EQ.0) GOTO 49                                         KXARCO43
         LUPAR = LUPAR+1                                                KXARCO44
         J = IW(KIND-2)                                                 KXARCO45
         GOTO (21,22 ) I                                                KXARCO46
   21    MSTA(J) = IW(KIND+1)                                           KXARCO47
       GOTO 15                                                          KXARCO48
   22    PARA(J) = RW(KIND+1)                                           KXARCO49
       GOTO 15                                                          KXARCO50
   49    CONTINUE                                                       KXARCO51
         CALL BKFMT (KEY(I),FMT(I))                                     KXARCO52
         CALL BLIST (IW,'C+',KEY(I))                                    KXARCO53
   50 CONTINUE                                                          KXARCO54
      RETURN                                                            KXARCO55
      END                                                               KXARCO56
      SUBROUTINE USKRIN(EI)                                             USKRIN 2
C                                                                       USKRIN 3
C! Reinitialise the generator with energy  EI and reinit some quantitiesUSKRIN 4
C! ARIA02 version  B.Bloch 30 MARCH 1992                                USKRIN 5
C                                                                       USKRIN 6
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      COMMON / GLUPAR / SVERT(3),IFL,ECM,IPROC                          GLUCOM 2
C     IFL      : LUND flavour , set to 0 by default, can be changed     GLUCOM 3
C     ECM      : nominal cms energy                                     GLUCOM 4
C     SVERT    : vertex smearing, set to 0. by default, can be changed  GLUCOM 5
C     IPROC    : process number : 1= ARIADNE , 2 = pure JETSET 7.3      GLUCOM 6
C                                 3= PYTHIA  , 4 = LEPTO                GLUCOM 7
      COMMON / GLUSTA / ICOULU(10)                                      GLUCOM 8
      DIMENSION TABL(25)                                                USKRIN 9
      INTEGER ALTABL                                                    USKRIN10
      EXTERNAL ALTABL                                                   USKRIN11
      ECM = EI                                                          USKRIN12
C Get kpar bank, modify                                                 USKRIN13
      IKPAR= NLINK('KPAR',0)                                            USKRIN14
      IF (IKPAR.GT.0) THEN                                              USKRIN15
         NCO = IW(IKPAR+LMHCOL)                                         USKRIN16
         CALL UCOPY(RW(IKPAR+LMHLEN+1),TABL(1),NCO)                     USKRIN17
         CALL BDROP(IW ,'KPAR')                                         USKRIN18
         TABL(2) = EI                                                   USKRIN19
         IND = ALTABL('KPAR',NCO,1,TABL,'2I,(F)','C')                   USKRIN20
         CALL PRTABL('KPAR',0)                                          USKRIN21
      ENDIF                                                             USKRIN22
      CALL VZERO(ICOULU,10)                                             USKRIN23
      RETURN                                                            USKRIN24
      END                                                               USKRIN25
      FUNCTION XKSECT(ECMI)                                             XKSECT 2
C                                                                       XKSECT 3
C! Returns the cross section value in nb for energy ECM                 XKSECT 4
C! ARIA02 version  B.Bloch 30 MARCH 1992                                XKSECT 5
C                                                                       XKSECT 6
      COMMON / GLUPAR / SVERT(3),IFL,ECM,IPROC                          GLUCOM 2
C     IFL      : LUND flavour , set to 0 by default, can be changed     GLUCOM 3
C     ECM      : nominal cms energy                                     GLUCOM 4
C     SVERT    : vertex smearing, set to 0. by default, can be changed  GLUCOM 5
C     IPROC    : process number : 1= ARIADNE , 2 = pure JETSET 7.3      GLUCOM 6
C                                 3= PYTHIA  , 4 = LEPTO                GLUCOM 7
      COMMON / GLUSTA / ICOULU(10)                                      GLUCOM 8
      CALL LUXTOT(IFL,ECMI,XTOT)                                        XKSECT 8
      XKSECT = XTOT                                                     XKSECT 9
      ECM = ECMI                                                        XKSECT10
      RETURN                                                            XKSECT11
      END                                                               XKSECT12
