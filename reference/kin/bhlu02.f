      SUBROUTINE ASKUSI(IGCOD)                                          ASKUSI 2
C --------------------------------------------------------------------  ASKUSI 3
C Initialization for BHLU01                G. Bonneaud November 1988.   ASKUSI 4
C Modified for BHLU02                   Bolek Pietrzyk January  1992.   ASKUSI 5
C --------------------------------------------------------------------  ASKUSI 6
      PARAMETER (LBCS=1000,LCHAR=4)                                     BCS    2
      COMMON/BCS/ IW(LBCS)                                              BCS    3
      REAL RW(LBCS)                                                     BCS    4
      EQUIVALENCE (RW(1),IW(1))                                         BCS    5
      COMMON  / CDGENE /SDVRT(3),VPOS(3),NEVENT(12),NEVPHO(2),VANG(2)   BBL003 1
      COMMON / INOUT  / NINP,NOUT,NOUT2                                 INOUT  2
      INTEGER ALTABL,ALRLEP                                             ASKUSI10
      EXTERNAL ALTABL ,ALRLEP                                           ASKUSI11
      COMMON /INTBHL/CMSENE,THMIN,THMAX,EPSCM,WTMAX,KEYRAD,KEYOPT       BBL002 1
      REAL *8 CMSENE,THMIN,THMAX,EPSCM                                  BBL002 2
      COMMON / CDBHGO / R0A,R0B,CORMIN,CORMAX,Z0A,Z0B                   CDBHGO 2
      real*4 R0A,R0B,CORMIN,CORMAX,Z0A,Z0B                              CDBHGO 3
      COMMON/ CDECUT /E1CUT,E2CUT                                       CDBHGO 4
      real*4 E1CUT,E2CUT                                                CDBHGO 5
      COMMON / KGCOMM / IST,NTR,IDP,ECM,WEI,ISICA                       BBL003 2
      DIMENSION TABL(20)                                                BBL003 5
      PARAMETER ( IGCO = 2009)                                          ASKUSI15
C                                                                       ASKUSI16
C   Return the generator code as defined in the KINGAL library          ASKUSI17
C                                                                       ASKUSI18
      IGCOD = IGCO                                                      ASKUSI19
      NOUT  = IW(6)                                                     ASKUSI20
      WRITE(NOUT,101) IGCOD                                             ASKUSI21
 101  FORMAT(/,10X,                                                     ASKUSI22
     &       'BHLU02 - CODE NUMBER =',I4,' Last mod. November  28,1995',BBL003 6
     & /,10X,'**************************************************',//)   ASKUSI24
C                                                                       ASKUSI25
C   Input parameters (see description in BHLU02)                        ASKUSI26
C                                                                       ASKUSI27
      CMSENE = 92.0                                                     ASKUSI28
      THMIN  = 2.177                                                    ASKUSI29
      THMAX  = 10.31                                                    ASKUSI30
      EPSCM  = 0.0001                                                   ASKUSI31
      KEYRAD = 1                                                        ASKUSI32
      KEYOPT = 3001                                                     ASKUSI33
      WTMAX = 2.8                                                       BBL002 5
C                                                                       ASKUSI34
C  The default values can be changed by the DATA CARD GBHL              BBL001 2
C                                                                       BBL001 3
      JGBGO = NLINK('GBGO',0)                                           BBL003 7
      IF(JGBGO.NE.0) THEN                                               BBL003 8
       R0A = RW(JGBGO+1)                                                BBL003 9
       R0B  = RW(JGBGO+2)                                               BBL00310
       CORMIN  = RW(JGBGO+3)                                            BBL00311
       CORMAX = RW(JGBGO+4)                                             BBL00312
       Z0A = RW(JGBGO+5)                                                BBL00313
       Z0B = RW(JGBGO+6)                                                BBL00314
       E1CUT  = RW(JGBGO+7)                                             BBL00315
       E2CUT  = RW(JGBGO+8)                                             BBL00316
      ENDIF                                                             BBL00317
      ISICA = 0                                                         BBL00318
      JGENE = NLINK('GBHL',0)                                           BBL001 4
      IF(JGENE.NE.0) THEN                                               ASKUSI38
       CMSENE = RW(JGENE+1)                                             ASKUSI39
       THMIN  = RW(JGENE+2)                                             ASKUSI40
       THMAX  = RW(JGENE+3)                                             ASKUSI41
       EPSCM  = RW(JGENE+4)                                             ASKUSI42
       KEYRAD = IW(JGENE+5)                                             ASKUSI43
       KEYOPT = IW(JGENE+6)                                             ASKUSI44
       IF ( IW(JGENE).GT.6)  WTMAX  = RW(JGENE+7)                       BBL002 6
       IF ( IW(JGENE).GT.7)  ISICA  = IW(JGENE+8)                       BBL00319
      ENDIF                                                             ASKUSI45
      TABL(1) = CMSENE                                                  ASKUSI46
      TABL(2) = THMIN                                                   ASKUSI47
      TABL(3) = THMAX                                                   ASKUSI48
      TABL(4) = EPSCM                                                   ASKUSI49
      TABL(5) = KEYRAD                                                  ASKUSI50
      TABL(6) = KEYOPT                                                  ASKUSI51
C                                                                       ASKUSI52
C  Main vertex generation                                               ASKUSI53
C                                                                       ASKUSI54
      SDVRT(1) = 0.0180                                                 ASKUSI55
      SDVRT(2) = 0.0010                                                 ASKUSI56
      SDVRT(3) = 1.00                                                   ASKUSI57
      VPOS(1)  = 0.                                                     ASKUSI58
      VPOS(2)  = 0.                                                     ASKUSI59
      VPOS(3)  = 0.                                                     ASKUSI60
      VANG(1)  = 0.                                                     BBL00320
      VANG(2)  = 0.                                                     BBL00321
      JSVRT = NLINK('SVRT',0)                                           ASKUSI61
      IF(JSVRT.NE.0) THEN                                               ASKUSI62
       SDVRT(1) = RW(JSVRT+1)                                           ASKUSI63
       SDVRT(2) = RW(JSVRT+2)                                           ASKUSI64
       SDVRT(3) = RW(JSVRT+3)                                           ASKUSI65
      ENDIF                                                             BBL001 5
      JXVRT = NLINK('XVRT',0)                                           BBL001 6
      IF(JXVRT.NE.0) THEN                                               BBL002 7
       VPOS(1)  = RW(JXVRT+1)                                           BBL002 8
       VPOS(2)  = RW(JXVRT+2)                                           BBL002 9
       VPOS(3)  = RW(JXVRT+3)                                           BBL00210
      ENDIF                                                             ASKUSI69
      JAVRT = NLINK('AVRT',0)                                           BBL00322
      IF(JAVRT.NE.0) THEN                                               BBL00323
       VANG(1)  = RW(JAVRT+1)                                           BBL00324
       VANG(2)  = RW(JAVRT+2)                                           BBL00325
      ENDIF                                                             BBL00326
      TABL(7) = SDVRT(1)                                                ASKUSI70
      TABL(8) = SDVRT(2)                                                ASKUSI71
      TABL(9) = SDVRT(3)                                                ASKUSI72
      TABL(10) = VPOS(1)                                                ASKUSI73
      TABL(11)= VPOS(2)                                                 ASKUSI74
      TABL(12)= VPOS(3)                                                 ASKUSI75
      TABL(13)= WTMAX                                                   BBL00211
      TABL(14)= VANG(1)                                                 BBL00327
      TABL(15)= VANG(2)                                                 BBL00328
      TABL(16)= ISICA                                                   BBL00329
C                                                                       ASKUSI76
C  Fill the KPAR bank with the generator parameters                     ASKUSI77
C                                                                       ASKUSI78
      NCOL = 16                                                         BBL00330
      NROW = 1                                                          ASKUSI80
      JKPAR = ALTABL('KPAR',NCOL,NROW,TABL,'2I,(F)','C')                ASKUSI81
C  Fill RLEP bank                                                       ASKUSI82
       EB = 0.5*CMSENE                                                  ASKUSI83
       IEBEAM = NINT(EB *1000  )                                        ASKUSI84
       JRLEP = ALRLEP(IEBEAM,'    ',0,0,0)                              ASKUSI85
C                                                                       ASKUSI86
C  Initialize events counters                                           ASKUSI87
      DO 10 I = 1,12                                                    ASKUSI88
   10 NEVENT(I) = 0                                                     ASKUSI89
      DO 11 I = 1,2                                                     ASKUSI90
   11 NEVPHO(I) = 0                                                     ASKUSI91
C                                                                       ASKUSI92
C Booking of some standard histogrammes                                 ASKUSI93
C                                                                       ASKUSI94
      CALL HBOOK1(10001,'Energy distribution : final e+$',              ASKUSI95
     &                50,0.,1.1*EB,0.)                                  BBL00331
      CALL HIDOPT(10001,'LOGY')                                         ASKUSI97
      CALL HBOOK1(10002,'Polar angle : final e+ (Degrees)$',            ASKUSI98
     &                                                   40,0.,180.,0.) ASKUSI99
      CALL HIDOPT(10002,'LOGY')                                         ASKUS100
      CALL HBOOK1(10003,'Energy distribution : final e-$',              ASKUS101
     &                50,0.,1.1*EB,0.)                                  BBL00332
      CALL HIDOPT(10003,'LOGY')                                         ASKUS103
      CALL HBOOK1(10004,'Polar angle : final e- (Degrees)$',            ASKUS104
     &                                                   40,0.,180.,0.) ASKUS105
      CALL HIDOPT(10004,'LOGY')                                         ASKUS106
      CALL HBOOK1(10005,'Photons multiplicity$',40,0.,40.,0.)           ASKUS107
      CALL HBOOK1(10006,'Energy dist. of photons',50,0.,1.1*EB,0.)      BBL00333
      CALL HIDOPT(10006,'LOGY')                                         ASKUS109
      CALL HBOOK1(10007,'Average energy distribution : photons$',       ASKUS110
     &                50,0.,EB,0.)                                      BBL00334
      CALL HIDOPT(10007,'LOGY')                                         ASKUS112
      CALL HBOOK1(10008,'Accolinearity between e+ and e-$',             ASKUS113
     &                                                   40,0.,10.,0.)  ASKUS114
      CALL HIDOPT(10008,'LOGY')                                         ASKUS115
      CALL HBOOK1(10009,'Accoplanarity between e+ and e-$',             ASKUS116
     &                                                   40,0.,40.,0.)  ASKUS117
      CALL HIDOPT(10009,'LOGY')                                         ASKUS118
      CALL HBOOK1(10010,'Evisible(Ee+ + Ee-) GeV',50,0.,2.2*EB,0.)      BBL00335
      CALL HIDOPT(10010,'LOGY')                                         ASKUS120
      IF (ISICA.LE.0) THEN                                              BBL00336
      CALL HBOOK1(10051,'Dist - M8 except nonfid-cut$',50,-1.,4.,0.)    ASKUS121
      CALL HBOOK2(10052,'E2 vs E1 - M8 except energy-cuts$',            ASKUS122
     &  50,0.,50.,50,0.,50.,0.)                                         ASKUS123
      CALL HBOOK1(10053,'Dphi - M8 except dphi-cut$',90,0.,180.,0.)     ASKUS124
      CALL HBOOK1(10054,'Xmin - M8 except energy-cuts$',50,0.,1.,0.)    ASKUS125
      CALL HBOOK1(10055,'Xsum - M8 except esum-cut$',50,0.,1.,0.)       ASKUS126
      CALL HBOOK1(10056,'Theta - M8$',80,45.,125.,0.)                   ASKUS127
      CALL HBOOK1(10057,'Theta - M9$',80,45.,125.,0.)                   ASKUS128
      CALL HIDOPT(10053,'LOGY')                                         ASKUS129
      CALL HIDOPT(10054,'LOGY')                                         ASKUS130
      CALL HIDOPT(10055,'LOGY')                                         ASKUS131
      ELSE                                                              BBL00337
        CALL HBOOK1(10053,'Theta each particle (GeV)',50,0.,10.,0.)     BBL00338
        CALL HIDOPT(10053,'LOGY')                                       BBL00339
        CALL HBOOK1(10054,'Hemisphere energy (GeV)',50,0.,1.1*EB,0.)    BBL00340
        CALL HIDOPT(10054,'LOGY')                                       BBL00341
        CALL HBOOK2(10055,'EA vs EB (GeV)',                             BBL00342
     &  50,0.,EB+5.,50,0.,EB+5.,0.)                                     BBL00343
      ENDIF                                                             BBL00344
C                                                                       ASKUS132
C  Generator initialization                                             ASKUS133
C                                                                       ASKUS134
      LENTRY = 1                                                        ASKUS135
      CALL BHLU02(LENTRY)                                               ASKUS136
C                                                                       ASKUS137
C  Print PART and KLIN banks                                            ASKUS138
C                                                                       ASKUS139
CC-RM      CALL PRPART                                                  BBL00345
      CALL PRTABL('KPAR',0)                                             ASKUS141
      CALL PRTABL('RLEP',0)                                             ASKUS142
        ncol = 8                                                        BBL00346
        TABL(1) = R0A                                                   BBL00347
        TABL(2) = R0B                                                   BBL00348
        TABL(3) = CORMIN                                                BBL00349
        TABL(4) = CORMAX                                                BBL00350
        TABL(5) = Z0A                                                   BBL00351
        TABL(6) = Z0B                                                   BBL00352
        TABL(7) = E1CUT                                                 BBL00353
        TABL(8) = E2CUT                                                 BBL00354
        JKBGO = ALTABL('KBGO',NCOL,1,TABL,'2I,(F)','C')                 BBL00355
        CALL PRTABL('KBGO',0)                                           BBL00356
C                                                                       ASKUS143
      RETURN                                                            ASKUS144
      END                                                               ASKUS145
      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)                 ASKUSE 2
C --------------------------------------------------------------------  ASKUSE 3
C Generation for BHLU01                    G. Bonneaud November 1988.   ASKUSE 4
C Modified for BHLU02                   Bolek Pietrzyk January  1992.   ASKUSE 5
C --------------------------------------------------------------------  ASKUSE 6
      PARAMETER (LBCS=1000,LCHAR=4)                                     BCS    2
      COMMON/BCS/ IW(LBCS)                                              BCS    3
      REAL RW(LBCS)                                                     BCS    4
      EQUIVALENCE (RW(1),IW(1))                                         BCS    5
      COMMON  / CDGENE /SDVRT(3),VPOS(3),NEVENT(12),NEVPHO(2),VANG(2)   BBL003 1
      COMMON / KGCOMM / IST,NTR,IDP,ECM,WEI,ISICA                       BBL003 2
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT       ASKUSE10
      REAL*8 P1,P2,Q1,Q2,PHOT                                           ASKUSE11
      REAL*8 QAUX(4)                                                    BBL00357
      DIMENSION VRTEX(4),TABK(4),ITAB(3)                                ASKUSE12
      INTEGER ALTABL                                                    ASKUSE13
      LOGICAL REJFL,ACCFL                                               ASKUSE14
      EXTERNAL ALTABL                                                   ASKUSE15
C                                                                       ASKUSE16
C  Generate primary vertex                                              ASKUSE17
C                                                                       ASKUSE18
      CALL RANNOR (RN1,RN2)                                             ASKUSE19
      CALL RANNOR (RN3,DUM)                                             ASKUSE20
      VRTEX(1) = RN1*SDVRT(1) + VPOS(1)                                 ASKUSE21
      VRTEX(2) = RN2*SDVRT(2) + VPOS(2)                                 ASKUSE22
      VRTEX(3) = RN3*SDVRT(3) + VPOS(3)                                 ASKUSE23
      VRTEX(4) = 0.                                                     ASKUSE24
C                                                                       ASKUSE25
C  Fill 'VERT' bank                                                     ASKUSE26
C                                                                       ASKUSE27
      IVMAI = 1                                                         ASKUSE28
      JVERT = KBVERT(IVMAI,VRTEX,0)                                     ASKUSE29
      IF(JVERT.EQ.0) THEN                                               ASKUSE30
       ISTA = 1                                                         ASKUSE31
       NEVENT(2) = NEVENT(2) + 1                                        ASKUSE32
       GO TO 98                                                         ASKUSE33
      ENDIF                                                             ASKUSE34
C                                                                       ASKUSE35
C  Event generation                                                     ASKUSE36
C                                                                       ASKUSE37
      LENTRY = 2                                                        ASKUSE38
    1 NEVENT(1) = NEVENT(1) + 1                                         ASKUSE39
      CALL BHLU02(LENTRY)                                               ASKUSE40
CC-RM                                                                   BBL00358
C Perform rotation from beam to Aleph (or rather SiCAL) system,         BBL00359
C that is, transform coordinates from the first system to the second    BBL00360
C                                                                       BBL00361
      CALL RBTOA (P1)                                                   BBL00362
      CALL RBTOA (Q1)                                                   BBL00363
      CALL RBTOA (P2)                                                   BBL00364
      CALL RBTOA (Q2)                                                   BBL00365
      IF ( NPHOT.GT.0) THEN                                             BBL00366
      DO 44 I=1,NPHOT                                                   BBL00367
        DO 45 J=1,4                                                     BBL00368
 45     QAUX(J) = PHOT(I,J)                                             BBL00369
        CALL RBTOA(QAUX)                                                BBL00370
        DO 46 J=1,4                                                     BBL00371
 46     PHOT(I,J) = QAUX(J)                                             BBL00372
 44   CONTINUE                                                          BBL00373
      ENDIF                                                             BBL00374
CC-RM                                                                   BBL00375
C                                                                       ASKUSE41
C  Reject events manifestly outside acceptance                          ASKUSE42
C                                                                       ASKUSE43
      IF ( ISICA.LE.0) THEN                                             BBL00376
      CALL REJEVT(P2,Q2,PHOT,NPHOT,VRTEX,REJFL,ACCFL)                   ASKUSE44
      ELSE                                                              BBL00377
         CALL REJSEV(P2,Q2,PHOT,NPHOT,VRTEX,REJFL)                      BBL00378
      ENDIF                                                             BBL00379
      IF(REJFL) GO TO 1                                                 ASKUSE45
C                                                                       ASKUSE46
      IF ( ISICA.LE.0) THEN                                             BBL00380
C  Count Method 8 events on parton level by REJECT routine              ASKUSE47
C                                                                       ASKUSE48
      IF(ACCFL) NEVENT(11) = NEVENT(11) + 1                             ASKUSE49
      ENDIF                                                             BBL00381
      NVRT = 1                                                          ASKUSE50
      ISTA = IST                                                        ASKUSE51
      IDPR = IDP                                                        ASKUSE52
      ECMS = ECM                                                        ASKUSE53
      WEIT = WEI                                                        ASKUSE54
C                                                                       ASKUSE55
C  book 'KINE' for beam electrons (-1 and -2)                           ASKUSE56
C                                                                       ASKUSE57
      TABK(1) = Q1(1)                                                   BBL00382
      TABK(2) = Q1(2)                                                   BBL00383
      TABK(3) = Q1(3)                                                   ASKUSE60
      TABK(4) = 0.                                                      ASKUSE61
      JKINE = KBKINE(-1,TABK,2,0)                                       ASKUSE62
      IF(JKINE.EQ.0) THEN                                               ASKUSE63
       ISTA = 2                                                         ASKUSE64
       NEVENT(3) = NEVENT(3) + 1                                        ASKUSE65
       GO TO 98                                                         ASKUSE66
      ENDIF                                                             ASKUSE67
      TABK(1) = P1(1)                                                   BBL00384
      TABK(2) = P1(2)                                                   BBL00385
      TABK(3) = P1(3)                                                   ASKUSE68
      TABK(4) = 0.                                                      ASKUSE69
      JKINE = KBKINE(-2,TABK,3,0)                                       ASKUSE70
      IF(JKINE.EQ.0) THEN                                               ASKUSE71
       ISTA = 3                                                         ASKUSE72
       NEVENT(4) = NEVENT(4) + 1                                        ASKUSE73
       GO TO 98                                                         ASKUSE74
      ENDIF                                                             ASKUSE75
C  book 'KINE' for final state particles                                ASKUSE76
      TABK(4) = 0.                                                      ASKUSE77
      DO 92 I = 1,3                                                     ASKUSE78
       TABK(I) = Q2(I)                                                  ASKUSE79
   92 CONTINUE                                                          ASKUSE80
      JKINE = KBKINE(1,TABK,2,IVMAI)                                    ASKUSE81
      IF(JKINE.EQ.0) THEN                                               ASKUSE82
       ISTA = 4                                                         ASKUSE83
       NEVENT(5) = NEVENT(5) + 1                                        ASKUSE84
       GO TO 98                                                         ASKUSE85
      ENDIF                                                             ASKUSE86
      TABK(4) = 0.                                                      ASKUSE87
      DO 93 I = 1,3                                                     ASKUSE88
       TABK(I) = P2(I)                                                  ASKUSE89
   93 CONTINUE                                                          ASKUSE90
      JKINE = KBKINE(2,TABK,3,IVMAI)                                    ASKUSE91
      IF(JKINE.EQ.0) THEN                                               ASKUSE92
       ISTA = 5                                                         ASKUSE93
       NEVENT(6) = NEVENT(6) + 1                                        ASKUSE94
       GO TO 98                                                         ASKUSE95
      ENDIF                                                             ASKUSE96
C                                                                       ASKUSE97
C Now we book the photons (if any...)                                   ASKUSE98
C                                                                       ASKUSE99
      NTR = 2 + NPHOT                                                   ASKUS100
      IF(NPHOT.EQ.0) GO TO 96                                           ASKUS101
      JJ = 0                                                            ASKUS102
      DO 95 J = 1,NPHOT                                                 ASKUS103
C We did not book the radiated photon if the energy is equal to zero    ASKUS104
       IF(PHOT(J,4).LT.1.E-06) THEN                                     ASKUS105
        NTR = NTR - 1                                                   ASKUS106
        GO TO 95                                                        ASKUS107
       ENDIF                                                            ASKUS108
      TABK(4) = 0.                                                      ASKUS109
       DO 94 I = 1,3                                                    ASKUS110
        TABK(I) = PHOT(J,I)                                             ASKUS111
   94  CONTINUE                                                         ASKUS112
       JJ = JJ + 1                                                      ASKUS113
       JKINE = KBKINE(2+JJ,TABK,1,IVMAI)                                ASKUS114
       IF(JKINE.EQ.0) THEN                                              ASKUS115
        ISTA = 6                                                        ASKUS116
        NEVENT(7) = NEVENT(7) + 1                                       ASKUS117
        GO TO 98                                                        ASKUS118
       ENDIF                                                            ASKUS119
   95  CONTINUE                                                         ASKUS120
C                                                                       ASKUS121
C  Fill history with 'KHIS' bank                                        ASKUS122
C                                                                       ASKUS123
   96 DO 97 I = 1,NTR                                                   ASKUS124
   97 ITAB(I) = 0                                                       ASKUS125
      JKHIS = ALTABL('KHIS',1,NTR,ITAB,'I','E')                         ASKUS126
      IF(JKHIS.EQ.0) THEN                                               ASKUS127
       ISTA = 7                                                         ASKUS128
       NEVENT(8) = NEVENT(8) + 1                                        ASKUS129
      ENDIF                                                             ASKUS130
C                                                                       ASKUS131
   98 IF(ISTA.NE.0) NEVENT(9) = NEVENT(9) + 1                           ASKUS132
      IF(ISTA.EQ.0) THEN                                                ASKUS133
       NEVENT(10) = NEVENT(10) + 1                                      ASKUS134
       IF(NTR.EQ.2) NEVPHO(1) = NEVPHO(1) +1                            ASKUS135
       IF(NTR.NE.2) NEVPHO(2) = NEVPHO(2) +1                            ASKUS136
       CALL HFILL(10001,REAL(Q2(4)),0.,WEIT)                            ASKUS137
       QPTHET = Q2(3)/DSQRT(Q2(1)**2+Q2(2)**2+Q2(3)**2)                 ASKUS138
       QPTHET = ACOS(QPTHET)*180./3.14159                               ASKUS139
       CALL HFILL(10002,QPTHET,0.,WEIT)                                 ASKUS140
       CALL HFILL(10003,REAL(P2(4)),0.,WEIT)                            ASKUS141
       QMTHET = P2(3)/DSQRT(P2(1)**2+P2(2)**2+P2(3)**2)                 ASKUS142
       QMTHET = ACOS(QMTHET)*180./3.14159                               ASKUS143
       CALL HFILL(10004,QMTHET,0.,WEIT)                                 ASKUS144
       XNPHOT = NPHOT                                                   ASKUS145
       CALL HFILL(10005,XNPHOT,0.,WEIT)                                 ASKUS146
       IF(NPHOT.NE.0) THEN                                              ASKUS147
        EAVE = 0.                                                       ASKUS148
        DO 99 I = 1,NPHOT                                               ASKUS149
         EGAM = PHOT(I,4)                                               ASKUS150
         EAVE = EAVE + EGAM                                             ASKUS151
   99    CALL HFILL(10006,EGAM,0.,WEIT)                                 ASKUS152
        EAVE = EAVE / NPHOT                                             ASKUS153
        CALL HFILL(10007,EAVE,0.,WEIT)                                  ASKUS154
       ENDIF                                                            ASKUS155
       ACCOL = P2(1)*Q2(1)+P2(2)*Q2(2)+P2(3)*Q2(3)                      ASKUS156
       ACCOL = ACCOL / (DSQRT(P2(1)**2+P2(2)**2+P2(3)**2) *             ASKUS157
     &                  DSQRT(Q2(1)**2+Q2(2)**2+Q2(3)**2))              ASKUS158
       ACCOL = ACOS(ACCOL)*180./3.14159                                 ASKUS159
       ACCOL = 180. - ACCOL                                             ASKUS160
       CALL HFILL(10008,ACCOL,0.,WEIT)                                  ASKUS161
       ACCOP = P2(1)*Q2(1)+P2(2)*Q2(2)                                  ASKUS162
       ACCOP = ACCOP / (DSQRT(P2(1)**2+P2(2)**2) *                      ASKUS163
     &                  DSQRT(Q2(1)**2+Q2(2)**2))                       ASKUS164
       ACCOP = ACOS(ACCOP)*180./3.14159                                 ASKUS165
       ACCOP = 180. - ACCOP                                             ASKUS166
       CALL HFILL(10009,ACCOP,0.,WEIT)                                  ASKUS167
       EVISEE = P2(4) + Q2(4)                                           ASKUS168
       CALL HFILL(10010,EVISEE,0.,WEIT)                                 ASKUS169
      ENDIF                                                             ASKUS170
C                                                                       ASKUS171
      IF ( ISICA.LE.0) THEN                                             BBL00386
      if ( mod(nevent(10),100000).eq.0) call boker8(1)                  BBL00387
      ELSE                                                              BBL00388
      if ( mod(nevent(10),100000).eq.0) call bokerS(1)                  BBL00389
      ENDIF                                                             BBL00390
      NTRK = NTR                                                        ASKUS172
C                                                                       ASKUS173
      RETURN                                                            ASKUS174
      END                                                               ASKUS175
      SUBROUTINE USCJOB                                                 USCJOB 2
C --------------------------------------------------------------------  USCJOB 3
C End of generation for BHLU01             G. Bonneaud November 1988.   USCJOB 4
C Modified for BHLU02                   Bolek Pietrzyk January  1992.   USCJOB 5
C --------------------------------------------------------------------  USCJOB 6
      COMMON  / CDGENE /SDVRT(3),VPOS(3),NEVENT(12),NEVPHO(2),VANG(2)   BBL003 1
      COMMON / INOUT  / NINP,NOUT,NOUT2                                 INOUT  2
      COMMON / KGCOMM / IST,NTR,IDP,ECM,WEI,ISICA                       BBL003 2
C                                                                       USCJOB 9
      LENTRY = 3                                                        USCJOB10
      CALL BHLU02(LENTRY)                                               USCJOB11
C                                                                       USCJOB12
       WRITE(NOUT,101)                                                  USCJOB13
  101  FORMAT(//20X,'EVENTS STATISTICS',                                USCJOB14
     &         /20X,'*****************')                                USCJOB15
      IF ( ISICA.LE.0) THEN                                             BBL00392
       WRITE(NOUT,102)NEVENT(1),NEVENT(10),NEVPHO(1),NEVPHO(2),NEVENT(9)USCJOB16
     &               ,NEVENT(11)                                        USCJOB17
      ELSE                                                              BBL00393
       WRITE(NOUT,102)NEVENT(1),NEVENT(10),NEVPHO(1),NEVPHO(2),NEVENT(9)BBL00394
      ENDIF                                                             BBL00395
  102  FORMAT(/5X,'# OF GENERATED UNWEIGHTED EVENTS     = ',I10,        USCJOB18
     &        /5X,'# OF ACCEPTED  EVENTS LOOSE ACCEPT.  = ',I10,        USCJOB19
     &        /5X,'# OF ACCEPTED  EVENTS WITHOUT PHOTON = ',I10,        USCJOB20
     &        /5X,'# OF ACCEPTED  EVENTS WITH PHOTON(S) = ',I10,        USCJOB21
     &        /5X,'# OF REJECTED  EVENTS (BOS ERROR)    = ',I10,        USCJOB22
     &        /5X,'# OF M8 ACCEPTED EVENTS              = ',I10)        USCJOB23
       WRITE(NOUT,103)                                                  USCJOB24
  103  FORMAT(//20X,'ERRORS STATISTICS',                                USCJOB25
     &         /20X,'*****************')                                USCJOB26
       WRITE(NOUT,104)NEVENT(2),NEVENT(3),NEVENT(4),NEVENT(5),NEVENT(6),USCJOB27
     &               NEVENT(7),NEVENT(8)                                USCJOB28
  104  FORMAT(/10X,'ISTA = 1 BOS ERROR VERT     # OF REJECT = ',I10,    USCJOB29
     &        /10X,'ISTA = 2 BOS ERROR KINE e+  # OF REJECT = ',I10,    USCJOB30
     &        /10X,'ISTA = 3 BOS ERROR KINE e-  # OF REJECT = ',I10,    USCJOB31
     &        /10X,'ISTA = 4 BOS ERROR KINE f+  # OF REJECT = ',I10,    USCJOB32
     &        /10X,'ISTA = 5 BOS ERROR KINE f-  # OF REJECT = ',I10,    USCJOB33
     &        /10X,'ISTA = 6 BOS ERROR KINE gam # OF REJECT = ',I10,    USCJOB34
     &        /10X,'ISTA = 7 BOS ERROR KHIS     # OF REJECT = ',I10)    USCJOB35
C                                                                       USCJOB36
      RETURN                                                            USCJOB37
      END                                                               USCJOB38
      SUBROUTINE BHLU02(LENTRY)                                         BHLU02 2
C --------------------------------------------------------------------  BHLU02 3
C Interface for BHLU01                     G. Bonneaud November 1988.   BHLU02 4
C Modified for BHLU02                   Bolek Pietrzyk January  1992.   BHLU02 5
C --------------------------------------------------------------------  BHLU02 6
      COMMON  / CDGENE /SDVRT(3),VPOS(3),NEVENT(12),NEVPHO(2),VANG(2)   BBL003 1
      COMMON / INOUT  / NINP,NOUT,NOUT2                                 INOUT  2
      COMMON / KGCOMM / IST,NTR,IDP,ECM,WEI,ISICA                       BBL003 2
      PARAMETER( PI = 3.1415926535897932D0 )                            BHLU0210
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT       BHLU0211
      REAL *8 P1,P2,Q1,Q2,PHOT                                          BHLU0212
      COMMON /INTBHL/CMSENE,THMIN,THMAX,EPSCM,WTMAX,KEYRAD,KEYOPT       BBL00213
      REAL *8 CMSENE,THMIN,THMAX,EPSCM                                  BHLU0214
      REAL *8 TRMIN,TRMAX                                               BHLU0215
      REAL *8  XPAR(100)                                                BBL00214
      INTEGER NPAR(100)                                                 BBL00215
C                                                                       BHLU0218
C  INITIALIZATION            *********************                      BHLU0219
C                                                                       BHLU0220
      IF(LENTRY.EQ.1) THEN                                              BHLU0221
C                                                                       BHLU0222
C  Parameters initialization                                            BHLU0223
C                                                                       BHLU0224
C                                                                       BHLU0225
       TRMIN = CMSENE**2*(1D0-COS(THMIN*PI/180D0))/2D0                  BHLU0226
       TRMAX = CMSENE**2*(1D0-COS(THMAX*PI/180D0))/2D0                  BHLU0227
       XPAR(1)=CMSENE                                                   BHLU0228
       XPAR(2)=TRMIN                                                    BHLU0229
       XPAR(3)=TRMAX                                                    BHLU0230
       XPAR(4)=EPSCM                                                    BHLU0231
       XPAR(7)=WTMAX                                                    BBL00216
       NPAR(1)=KEYOPT                                                   BHLU0232
       NPAR(2)=KEYRAD                                                   BHLU0233
C                                                                       BHLU0234
       CALL BHLUMI(-1,XPAR,NPAR)                                        BHLU0235
        IF ( ISICA.LE.0) THEN                                           BBL00396
       CALL BOKER8(-1)                                                  BHLU0236
        ELSE                                                            BBL00397
          CALL BOKERS(-1)                                               BBL00398
        ENDIF                                                           BBL00399
C                                                                       BHLU0237
C                                                                       BHLU0238
C  EVENT GENERATION          *********************                      BHLU0239
C                                                                       BHLU0240
       ELSE IF(LENTRY.EQ.2) THEN                                        BHLU0241
C                                                                       BHLU0242
C  Event status (0 = O.K.)                                              BHLU0243
C                                                                       BHLU0244
       IST = 0                                                          BHLU0245
C                                                                       BHLU0246
       CALL BHLUMI(0,XPAR,NPAR)                                         BHLU0247
        IF ( ISICA.LE.0) THEN                                           BBL00100
       CALL BOKER8(0)                                                   BHLU0248
        ELSE                                                            BBL00101
           CALL BOKERS(0)                                               BBL00102
        ENDIF                                                           BBL00103
C                                                                       BHLU0249
       IDP  = NPHOT                                                     BHLU0250
       WEI  = 1.                                                        BHLU0251
       ECM  = CMSENE                                                    BHLU0252
C                                                                       BHLU0253
C  END OF GENERATION         *********************                      BHLU0254
C                                                                       BHLU0255
      ELSE IF(LENTRY.EQ.3) THEN                                         BHLU0256
C                                                                       BHLU0257
       CALL BHLUMI(2,XPAR,NPAR)                                         BHLU0258
        IF ( ISICA.LE.0) THEN                                           BBL00104
       CALL BOKER8(1)                                                   BHLU0259
        ELSE                                                            BBL00105
           CALL BOKERS(1)                                               BBL00106
        ENDIF                                                           BBL00107
C                                                                       BHLU0260
      ENDIF                                                             BHLU0261
C                                                                       BHLU0262
      RETURN                                                            BHLU0263
      END                                                               BHLU0264
      SUBROUTINE REJEVT(QM,QP,QK,NPHOT,VRTEX,REJFL,ACCFL)               REJEVT 2
C---------------------------------------------------------------------- REJEVT 3
C!  - Make a preselection on generator level                            REJEVT 4
C!                                                                      REJEVT 5
C!   Author   :- Peter H. Hansen       27-NOV-1990  for BABAMC          REJEVT 6
C!   Modified :- Bolek Pietrzyk and Peter H. Hansen for BHLU02          REJEVT 7
C                                      17-JAN-1992                      REJEVT 8
C!                                                                      REJEVT 9
C!   Inputs: QM,QP,QK fourvectors of electron, positron and photon      REJEVT10
C!           NPHOT    number of photons                                 REJEVT11
C!           VRTEX(4) vertex                                            REJEVT12
C!   Output: REJFL = .TRUE. if rejected                                 REJEVT13
C!                 = .FALSE. if accepted                                REJEVT14
C!           ACCFL = .TRUE. if Meth 8 accepted                          REJEVT15
C!                 = .FALSE. if Meth 8 rejected                         REJEVT16
C!   DESCRIPTION                                                        REJEVT17
C!   ===========                                                        REJEVT18
C?   The purpose of this is to avoid waisting GALEPH time on            REJEVT19
C?   events that do not have particles in the acceptance.               REJEVT20
C?                                                                      REJEVT21
C?   Check whether a particle on each side has hit the active           REJEVT22
C?   region defined as the edge at z=ZBACK (the last plane)             REJEVT23
C?   extended by a margin of XEXTRA. If so REJFL = .FALSE.              REJEVT24
C?   Here the alignment, vertex and B-field is taken into account.      REJEVT25
C?                                                                      REJEVT26
C?   Furthermore check whether the event passes Meth8.                  REJEVT27
C?   This is usefull for various studies of the acceptance at           REJEVT28
C?   parton level.                                                      REJEVT29
C?                                                                      REJEVT30
C!======================================================================REJEVT31
C Acceptance in horizontal plane (cm)                                   REJEVT32
      PARAMETER (XEDG   = 11.9)                                         REJEVT33
C y of "corner"                                                         REJEVT34
      PARAMETER (YEDG   = 8.4+1.2)                                      REJEVT35
C z of last plane                                                       REJEVT36
      PARAMETER (ZBACK  = 302.0)                                        REJEVT37
C z of shower max                                                       REJEVT38
      PARAMETER (ZAVER  = 280.0)                                        REJEVT39
C z of origin of local coordinate system                                REJEVT40
      PARAMETER (ZORIG  = 262.5)                                        REJEVT41
C margin allowing for transverse shower smear                           REJEVT42
      PARAMETER (XEXTRA = -.5)                                          REJEVT43
      PARAMETER (YEXTRA = -.5)                                          REJEVT44
C maximum theta of acceptance                                           REJEVT45
      PARAMETER (TMAX = 0.170)                                          REJEVT46
C separation when two clusters are formed                               REJEVT47
      PARAMETER (CLSEP = 15.0)                                          REJEVT48
C phi rotation of 45.5 GeV particle in 1.5 T field                      REJEVT49
      PARAMETER (PHROT = 0.014)                                         REJEVT50
C zero suppression                                                      REJEVT51
      PARAMETER (EZERO = 0.050)                                         REJEVT52
C M8 effective dist cut on non-fiducial side                            REJEVT53
      PARAMETER (DICUT = 0.8)                                           REJEVT54
C M8 maximal theta cut                                                  REJEVT55
      PARAMETER (THCUT = 0.125)                                         REJEVT56
C M8 Dphi cut                                                           REJEVT57
      PARAMETER (DPCUT = 170.)                                          REJEVT58
C M8 cluster energy cut                                                 REJEVT59
      PARAMETER (ECCUT = 0.22)                                          REJEVT60
C M8 energy sum cut                                                     REJEVT61
      PARAMETER (ESCUT = 0.6)                                           REJEVT62
C                                                                       REJEVT63
C                                                                       REJEVT64
      COMMON /INTBHL/CMSENE,THMIN,THMAX,EPSCM,WTMAX,KEYRAD,KEYOPT       BBL00217
      REAL *8 CMSENE,THMIN,THMAX,EPSCM                                  REJEVT66
      DIMENSION DXN(4),DYN(4),DZN(4),OMXN(4),OMYN(4),OMZN(4)            REJEVT67
      REAL*8 QM(4),QP(4),QK(100,4)                                      REJEVT68
      REAL*4 QA(4,102),VRTEX(4)                                         REJEVT69
      REAL *8 DXA,DXB,DYA,DYB                                           REJEVT70
      DIMENSION DIST(2)                                                 REJEVT71
      LOGICAL REJFL,ACCFL,AFLAG,BFLAG,KEEPIT                            REJEVT72
C                                                                       REJEVT73
      DATA PI/3.1415927/                                                REJEVT74
      DATA IEVT/0/                                                      REJEVT75
C 1990 Alignment constants                                              REJEVT76
C      DATA DXN/ -0.045, -0.045,  0.195,  0.195/                        REJEVT77
C      DATA DYN/ -0.093, -0.093, -0.047, -0.047/                        REJEVT78
C      DATA DZN/ -0.141, -0.170,  0.596,  0.387/                        REJEVT79
C      DATA OMXN/  0.00363, 0.00355,  0.00170, -0.00146/                REJEVT80
C      DATA OMYN/ -0.00440, 0.00275,  0.00302,  0.00232/                REJEVT81
C      DATA OMZN/  0.00235, 0.00235, -0.00272, -0.00272/                REJEVT82
C 1991 Alignment constants                                              REJEVT83
       DATA DXN/ -0.096, -0.118,  0.039,  0.027/                        REJEVT84
       DATA DYN/ -0.310, -0.012, -0.026, -0.100/                        REJEVT85
       DATA DZN/  0.134,  0.164,  0.317,  0.320/                        REJEVT86
       DATA OMXN/ -0.00019, 0.00007, -0.00309, -0.00353/                REJEVT87
       DATA OMYN/ -0.00119, -0.00043, -0.00030, -0.00260/               REJEVT88
       DATA OMZN/  0.01248, 0.01248, -0.00148, -0.00148/                REJEVT89
C-----------------------------------------------------------------------REJEVT90
      REJFL = .FALSE.                                                   REJEVT91
      ACCFL = .FALSE.                                                   REJEVT92
      AFLAG = .FALSE.                                                   REJEVT93
      BFLAG = .FALSE.                                                   REJEVT94
      EA = 0.                                                           REJEVT95
      EB = 0.                                                           REJEVT96
      XA = 100.                                                         REJEVT97
      YA = 100.                                                         REJEVT98
      XB = 100.                                                         REJEVT99
      YB = 100.                                                         REJEV100
C                                                                       REJEV101
C alternate kflg between 0 and 1 at each event                          REJEV102
      IEVT = IEVT+1                                                     REJEV103
      KFLG = MOD(IEVT,2)                                                REJEV104
C                                                                       REJEV105
C electron, positron and gamma                                          REJEV106
      DO 10 I=1,4                                                       REJEV107
        QA(I,1)=QM(I)                                                   REJEV108
        QA(I,2)=QP(I)                                                   REJEV109
      DO 10 J = 1,NPHOT                                                 REJEV110
        QA(I,J+2)=QK(J,I)                                               REJEV111
   10 CONTINUE                                                          REJEV112
C                                                                       REJEV113
C loop over the 3 partons                                               REJEV114
      NPART = 2+NPHOT                                                   REJEV115
      DO 100 J=1,NPART                                                  REJEV116
        IF(QA(4,J).LT.EZERO) GOTO 100                                   REJEV117
        PT = QA(1,J)*QA(1,J)+QA(2,J)*QA(2,J)                            REJEV118
        PTOT = SQRT(PT+QA(3,J)*QA(3,J))                                 REJEV119
        IF(PTOT.LT.EZERO) GOTO 100                                      REJEV120
        PT = SQRT(PT)                                                   REJEV121
        THETA = ASIN(PT/PTOT)                                           REJEV122
        IF(QA(3,J).LT.0.) THETA = PI-THETA                              REJEV123
        PHI = ATAN2(QA(2,J),QA(1,J))                                    REJEV124
        IF(PHI.LT.0.) PHI=PHI+2.*PI                                     REJEV125
C                                                                       REJEV126
C Sign z and module number                                              REJEV127
        IF(THETA.GT.1.) THEN                                            REJEV128
          SIG  =-1.                                                     REJEV129
          MODU = 2                                                      REJEV130
        ELSE                                                            REJEV131
          SIG  = 1.                                                     REJEV132
          MODU = 4                                                      REJEV133
        ENDIF                                                           REJEV134
        IF(COS(PHI).LT.0.) MODU=MODU-1                                  REJEV135
C                                                                       REJEV136
C Turn phi for electron and positron                                    REJEV137
        IF(J.LE.2) PHI = PHI - SIG*PHROT*CMSENE/(2.*PTOT)               REJEV138
C                                                                       REJEV139
C X and Y in global system                                              REJEV140
C                                                                       REJEV141
        X = VRTEX(1) + SIG*(ZAVER-SIG*VRTEX(3))*COS(PHI)*TAN(THETA)     REJEV142
        Y = VRTEX(2) + SIG*(ZAVER-SIG*VRTEX(3))*SIN(PHI)*TAN(THETA)     REJEV143
C                                                                       REJEV144
C Local system                                                          REJEV145
        ZL = SIG*(ZAVER-ZORIG)                                          REJEV146
        XL = X - DXN(MODU) - OMZN(MODU)*Y + OMYN(MODU)*ZL               REJEV147
        YL = Y - DYN(MODU) - OMXN(MODU)*ZL + OMZN(MODU)*X               REJEV148
C                                                                       REJEV149
C Fold into first quadrant and extrapolate to last plane                REJEV150
        XLOC = ABS(XL)                                                  REJEV151
        YLOC = ABS(YL)                                                  REJEV152
C                                                                       REJEV153
C Find distance to edges AT THE LAST PLANE in Lcal                      REJEV154
C (similarly to LCLUTW)                                                 REJEV155
        XBACK = XLOC*ZBACK/ZAVER                                        REJEV156
        YBACK = YLOC*ZBACK/ZAVER                                        REJEV157
        DIST(1) = XBACK - 1.9                                           REJEV158
        DIST(2) = 24.5                                                  REJEV159
        IF(YBACK.GT.YEDG) THEN                                          REJEV160
          DIS = XBACK-(17.5-YBACK)/0.75                                 REJEV161
          IF(DIS.LT.DIST(1)) DIST(1)=DIS                                REJEV162
          IF(XBACK.LT.XEDG) DIST(2) = YBACK-(17.5-XBACK*0.75)           REJEV163
        ELSE                                                            REJEV164
          DIST(1) = XBACK-XEDG                                          REJEV165
        ENDIF                                                           REJEV166
C                                                                       REJEV167
C Sum up the energy in the active region (defined at last plane)        REJEV168
C find maximum energy clusters EA and EB                                REJEV169
C two particles form separate clusters if more than 15cm apart.         REJEV170
        TH = THETA                                                      REJEV171
        IF(THETA.GT.1.) TH = PI-THETA                                   REJEV172
        IF(DIST(1).GT.XEXTRA.AND.DIST(2).GT.YEXTRA.AND.                 REJEV173
     &     TH.LT.TMAX.AND.QA(4,J).GT.EZERO) THEN                        REJEV174
C                                                                       REJEV175
C energy deposit decreases linearly to zero when dist goes              REJEV176
C from 1.2 cm to zero                                                   REJEV177
          EJ = QA(4,J)*AMIN1(DIST(1),1.2)/1.2                           REJEV178
          EJ = AMAX1(EJ,0.)                                             REJEV179
C                                                                       REJEV180
C raise flag if any side is hit by a particle                           REJEV181
C A side                                                                REJEV182
          IF(SIG.GT.0.) THEN                                            REJEV183
            AFLAG = .TRUE.                                              REJEV184
            IF(ABS(X-XA).GT.CLSEP.OR.ABS(Y-YA).GT.CLSEP) THEN           REJEV185
              IF(EJ.GT.EA) THEN                                         REJEV186
                EA=EJ                                                   REJEV187
                XA = XL                                                 REJEV188
                YA = YL                                                 REJEV189
              ENDIF                                                     REJEV190
            ELSE                                                        REJEV191
              XA = XA*EA+X*EJ                                           REJEV192
              YA = YA*EA+Y*EJ                                           REJEV193
              EA= EA+EJ                                                 REJEV194
              XA = XA/EA                                                REJEV195
              YA = YA/EA                                                REJEV196
            ENDIF                                                       REJEV197
C B side                                                                REJEV198
          ELSE                                                          REJEV199
            BFLAG = .TRUE.                                              REJEV200
            IF(ABS(X-XB).GT.CLSEP.OR.ABS(Y-YB).GT.CLSEP) THEN           REJEV201
              IF(EJ.GT.EB) THEN                                         REJEV202
                EB=EJ                                                   REJEV203
                XB = XL                                                 REJEV204
                YB = YL                                                 REJEV205
              ENDIF                                                     REJEV206
            ELSE                                                        REJEV207
              XB = XB*EB+X*EJ                                           REJEV208
              YB = YB*EB+Y*EJ                                           REJEV209
              EB= EB+EJ                                                 REJEV210
              XB = XB/EB                                                REJEV211
              YB = YB/EB                                                REJEV212
            ENDIF                                                       REJEV213
          ENDIF                                                         REJEV214
        ENDIF                                                           REJEV215
  100 CONTINUE                                                          REJEV216
C                                                                       REJEV217
C Make a trigger requirement: active region hit by particle             REJEV218
C                                                                       REJEV219
      IF(.NOT.AFLAG.OR..NOT.BFLAG) THEN                                 REJEV220
         REJFL = .TRUE.                                                 REJEV221
         GOTO 999                                                       REJEV222
      ENDIF                                                             REJEV223
C                                                                       REJEV224
C Global system                                                         REJEV225
        ZL = SIG*(ZAVER-ZORIG)                                          REJEV226
        XGA = XA + DXN(MODU) + OMZN(MODU)*YA - OMYN(MODU)*ZL            REJEV227
        YGA = YA + DYN(MODU) + OMXN(MODU)*ZL - OMZN(MODU)*XA            REJEV228
        XGB = XB + DXN(MODU) + OMZN(MODU)*YB - OMYN(MODU)*ZL            REJEV229
        YGB = YB + DYN(MODU) + OMXN(MODU)*ZL - OMZN(MODU)*XB            REJEV230
C                                                                       REJEV231
C Fiducial cut                                                          REJEV232
      M8OK = 0                                                          REJEV233
      M9OK = 0                                                          REJEV234
      IF(KFLG.EQ.0) THEN                                                REJEV235
        DXB = XB*10.                                                    REJEV236
        DYB = YB*10.                                                    REJEV237
        IF(KEEPIT(DXB,DYB,5)) M8OK = M8OK+1                             REJEV238
        IF(KEEPIT(DXB,DYB,6)) M9OK = M9OK+1                             REJEV239
        XLOC = ABS(XA)                                                  REJEV240
        YLOC = ABS(YA)                                                  REJEV241
        XG  = XGB                                                       REJEV242
        YG  = YGB                                                       REJEV243
      ELSE                                                              REJEV244
        DXA = XA*10.                                                    REJEV245
        DYA = YA*10.                                                    REJEV246
        IF(KEEPIT(DXA,DYA,5)) M8OK = M8OK+1                             REJEV247
        IF(KEEPIT(DXA,DYA,6)) M9OK = M9OK+1                             REJEV248
        XLOC = ABS(XB)                                                  REJEV249
        YLOC = ABS(YB)                                                  REJEV250
        XG = XGA                                                        REJEV251
        YG = YGA                                                        REJEV252
      ENDIF                                                             REJEV253
      TH = ATAN(SQRT(XG**2+YG**2)/ZAVER)                                REJEV254
C                                                                       REJEV255
C Non fiducial cut (again with hardwired constants)                     REJEV256
        DIST(1) = XLOC - 1.9                                            REJEV257
        DIST(2) = 24.5                                                  REJEV258
        IF(YLOC.GT.YEDG) THEN                                           REJEV259
          DIS = XLOC-(17.5-YLOC)/0.75                                   REJEV260
          IF(DIS.LT.DIST(1)) DIST(1)=DIS                                REJEV261
          IF(XLOC.LT.XEDG) DIST(2) = YLOC-(17.5-XLOC*0.75)              REJEV262
        ELSE                                                            REJEV263
          DIST(1) = XLOC-XEDG                                           REJEV264
        ENDIF                                                           REJEV265
C                                                                       REJEV266
C it is effectively 0.8cm (instead of 1cm) because of lateral leakage.  REJEV267
        IF(DIST(1).GT.DICUT.AND.TH.LT.THCUT) M8OK = M8OK + 2            REJEV268
        IF(DIST(1).GT.1.5.AND.TH.LT.THCUT) M9OK = M9OK + 2              REJEV269
C                                                                       REJEV270
C Phi cut                                                               REJEV271
        PHIA = ATAN2(YGA,XGA)                                           REJEV272
        IF(PHIA.LT.0.) PHIA=PHIA+2.*PI                                  REJEV273
        PHIB = ATAN2(YGB,XGB)                                           REJEV274
        IF(PHIB.LT.0.) PHIB=PHIB+2.*PI                                  REJEV275
        DPHI = ABS(PHIA-PHIB)*180./PI                                   REJEV276
        DPHI = AMIN1(DPHI,360.-DPHI)                                    REJEV277
        IF(DPHI.GT.DPCUT) M8OK = M8OK + 4                               REJEV278
C                                                                       REJEV279
C Energy cut                                                            REJEV280
      EMIN = AMIN1(EA,EB)                                               REJEV281
      IF(EA/CMSENE.GT.ECCUT.AND.EB/CMSENE.GT.ECCUT.AND.                 REJEV282
     &   (EA+EB)/CMSENE.GT.ESCUT) M8OK = M8OK + 8                       REJEV283
C                                                                       REJEV284
C Now histogram the various cut quantities for Meth 8 events            REJEV285
      IF(M8OK.EQ.15) ACCFL = .TRUE.                                     REJEV286
      IF(ACCFL)  CALL HF1(10056,TH*1000.,1.)                            REJEV287
      IF(ACCFL.AND.M9OK.EQ.3)  CALL HF1(10057,TH*1000.,1.)              REJEV288
      IF(ACCFL.OR.M8OK.EQ.13)  CALL HF1(10051,DIST(1),1.)               REJEV289
      IF(ACCFL.OR.M8OK.EQ.11)  CALL HF1(10053,DPHI,1.)                  REJEV290
      IF(ACCFL.OR.M8OK.EQ.7)   CALL HF2(10052,EA,EB,1.)                 REJEV291
      IF(ACCFL.OR.M8OK.EQ.7)   CALL HF1(10054,EMIN/CMSENE,1.)           REJEV292
      IF((ACCFL.OR.M8OK.EQ.7).AND.EMIN/CMSENE.GT.ECCUT)                 REJEV293
     &      CALL HF1(10055,(EA+EB)/CMSENE,1.)                           REJEV294
C                                                                       REJEV295
  999 CONTINUE                                                          REJEV296
      END                                                               REJEV297
      SUBROUTINE REJSEV(QM,QP,QK,NPHOT,VRTEX,REJFL)                     REJSEV 2
C---------------------------------------------------------------------- REJSEV 3
C!  - Make a preselection on generator level                            REJSEV 4
C!                                                                      REJSEV 5
C!                                                                      REJSEV 6
C!   Inputs: QM,QP,QK fourvectors of electron, positron and photon      REJSEV 7
C!           NPHOT    number of photons                                 REJSEV 8
C!           VRTEX(4) vertex                                            REJSEV 9
C!   Output: REJFL = .TRUE. if rejected                                 REJSEV10
C!                 = .FALSE. if accepted                                REJSEV11
C!   DESCRIPTION                                                        REJSEV12
C!   ===========                                                        REJSEV13
C?   The purpose of this is to avoid waisting GALEPH time on            REJSEV14
C?   events that do not have particles in the acceptance.               REJSEV15
C?                                                                      REJSEV16
C?   Check whether a particle on each side has hit the active           REJSEV17
C?   region defined as theta = THMIBK (the last plane)                  REJSEV18
C?   extended by a margin of THXTRA. If so REJFL = .FALSE.              REJSEV19
C?                           RM 1992                                    REJSEV20
C!======================================================================REJSEV21
      PARAMETER (PI = 3.141592654)                                      REJSEV22
C theta of last plane                                                   REJSEV23
      PARAMETER (THMIBK = 1.33)                                         REJSEV24
C theta of first tungsten                                               REJSEV25
      PARAMETER (THMAFR = 3.356)                                        REJSEV26
C safety margin                                                         REJSEV27
      PARAMETER (THXTRA = 0.13)                                         REJSEV28
C theta min for enlarged acceptance                                     REJSEV29
      PARAMETER (THMIAC = (THMIBK - THXTRA) * PI /180.)                 REJSEV30
C theta max for enlarged acceptance                                     REJSEV31
      PARAMETER (THMAAC = (THMAFR + THXTRA) * PI /180.)                 REJSEV32
C zero suppression                                                      REJSEV33
      PARAMETER (EZERO = 0.050)                                         REJSEV34
C energy cut                                                            REJSEV35
      PARAMETER (ECUT  = 0.500)                                         REJSEV36
C depth at which the fiducial cut is done (cm)                          REJSEV37
cc      PARAMETER (Z0 = 252.78)                                         REJSEV38
      COMMON / CDBHGO / R0A,R0B,CORMIN,CORMAX,Z0A,Z0B                   CDBHGO 2
      real*4 R0A,R0B,CORMIN,CORMAX,Z0A,Z0B                              CDBHGO 3
      COMMON/ CDECUT /E1CUT,E2CUT                                       CDBHGO 4
      real*4 E1CUT,E2CUT                                                CDBHGO 5
cc      PARAMETER (Z0 = 252.83)                                         REJSEV40
C                                                                       REJSEV41
C                                                                       REJSEV42
      REAL*8 QM(4),QP(4),QK(100,4)                                      REJSEV43
      REAL*4 QA(4,102),VRTEX(4)                                         REJSEV44
      LOGICAL REJFL                                                     REJSEV45
C                                                                       REJSEV46
C-----------------------------------------------------------------------REJSEV47
      REJFL = .FALSE.                                                   REJSEV48
C                                                                       REJSEV49
C electron, positron and gamma                                          REJSEV50
      DO 10 I = 1,4                                                     REJSEV51
        QA(I,1)=QM(I)                                                   REJSEV52
        QA(I,2)=QP(I)                                                   REJSEV53
        DO 10 J = 1,NPHOT                                               REJSEV54
          QA(I,J+2)=QK(J,I)                                             REJSEV55
   10 CONTINUE                                                          REJSEV56
C                                                                       REJSEV57
C loop over all partons                                                 REJSEV58
      EA = 0.                                                           REJSEV59
      EB = 0.                                                           REJSEV60
      NPART = 2 + NPHOT                                                 REJSEV61
      DO 100 J = 1,NPART                                                REJSEV62
        PT2   = QA(1,J)*QA(1,J)+QA(2,J)*QA(2,J)                         REJSEV63
        PTOT  = SQRT(PT2+QA(3,J)*QA(3,J))                               REJSEV64
        PT    = SQRT(PT2)                                               REJSEV65
        THETA = ASIN(PT/PTOT)                                           REJSEV66
CC-RM                                                                   REJSEV67
CC        PHI = ATAN2(QA(2,J),QA(1,J))                                  REJSEV68
CC        IF(PHI.LT.0.) PHI=PHI+2.*PI                                   REJSEV69
CC        THETA = THETA*(1.-VRTEX(3)/Z0) + VRTEX(1)/Z0*COS(PHI)         REJSEV70
CC     .                                 + VRTEX(2)/Z0*SIN(PHI)         REJSEV71
CC        IF ( QA(3,J) .LT. 0.) THETA = THETA + 2.*VRTEX(3)*Z0          REJSEV72
CC-RM                                                                   REJSEV73
        IF ( QA(3,J) .LT. 0.) THETA = PI-THETA                          REJSEV74
        IF (THETA .LT. 1.) CALL HF1(10053,THETA*180./PI,QA(4,J))        REJSEV75
C                                                                       REJSEV76
        IF ( QA(4,J) .LT. EZERO ) GOTO 100                              REJSEV77
C                                                                       REJSEV78
        IF     ( (THETA .GT. THMIAC)                                    REJSEV79
     &                  .AND.                                           REJSEV80
     &           (THETA .LT. THMAAC) )      THEN                        REJSEV81
          EA = EA + QA(4,J)                                             REJSEV82
        ELSEIF ( (THETA .LT. (PI-THMIAC))                               REJSEV83
     &                  .AND.                                           REJSEV84
     &           (THETA .GT. (PI-THMAAC)) ) THEN                        REJSEV85
          EB = EB + QA(4,J)                                             REJSEV86
        ENDIF                                                           REJSEV87
C                                                                       REJSEV88
  100 CONTINUE                                                          REJSEV89
cc-rm      IF ( (EA .LT. ECUT) .AND. (EB .LT. ECUT) )  REJFL = .TRUE.   REJSEV90
      IF ( (EA .LT. ECUT) .OR. (EB .LT. ECUT) )  REJFL = .TRUE.         REJSEV91
      CALL HF1(10054,EA,1.)                                             REJSEV92
      CALL HF2(10055,EA,EB,1.)                                          REJSEV93
C                                                                       REJSEV94
  999 CONTINUE                                                          REJSEV95
      END                                                               REJSEV96
      SUBROUTINE BOKER8(MODE)                                           BOKER8 2
C*****************************************************************      BOKER8 3
C Calulate cross-section with UNWEIGHTED events within geometrical      BOKER8 4
C acceptance of ALEPH                                                   BOKER8 5
C                                  Bolek Pietrzyk January 1992          BOKER8 6
C*****************************************************************      BOKER8 7
      IMPLICIT REAL*8(A-H,O-Z)                                          BOKER8 8
      PARAMETER( PI = 3.1415926535897932D0 )                            BOKER8 9
      CHARACTER*80      BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G BOKER810
      PARAMETER(                                                        BOKER811
     $BXOPE =  '(//1X,15(5H=====)    )',                                BOKER812
     $BXTXT =  '(1X,1H=,                  A48,25X,    1H=)',            BOKER813
     $BXL1I =  '(1X,1H=,I17,                 16X, A20,A12,A7, 1X,1H=)', BOKER814
     $BXL1F =  '(1X,1H=,F17.8,               16X, A20,A12,A7, 1X,1H=)', BOKER815
     $BXL2F =  '(1X,1H=,F17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H=)', BOKER816
     $BXL1G =  '(1X,1H=,G17.8,               16X, A20,A12,A7, 1X,1H=)', BOKER817
     $BXL2G =  '(1X,1H=,G17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H=)', BOKER818
     $BXCLO =  '(1X,15(5H=====)/   )'    )                              BOKER819
      COMMON / INOUT  / NINP,NOUT,NOUT2                                 INOUT  2
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(100)                  BOKER821
      COMMON / PARGEN / CMSENE,TMING,RAXIG,VMAXG,XK0,KEYOPT,KEYRAD      BOKER822
      COMMON / PAROBL / TMINW,RAXIW,TMINN,RAXIN,VMAXE,KEYTRI            BOKER823
C THIS IS COMMON BLOCK FROM INSIDE GENERATOR !!!!!!!                    BOKER824
      COMMON / TRANSR / TRAN,TRMIN,TRMAX                                BOKER825
C !!!!!!!                                                               BOKER826
      LOGICAL LWIDE,LNARR,LMIX1,LMIX2                                   BOKER827
      DIMENSION NPAR(100),XPAR(100)                                     BOKER828
      DIMENSION NACC(4)                                                 BOKER829
      IF(MODE.EQ.-1) THEN                                               BOKER830
*     *******************                                               BOKER831
      XMIN= 0.1                                                         BOKER832
C.....initialize scalers                                                BOKER833
      DO 10 K = 1,4                                                     BOKER834
   10 NACC(K) = 0                                                       BOKER835
      NEVGEN  = 0                                                       BOKER836
      ELSEIF(MODE.EQ.0) THEN                                            BOKER837
*     **********************                                            BOKER838
      NEVGEN=NEVGEN+1                                                   BOKER839
      LWIDE  = .FALSE.                                                  BOKER840
      LNARR  = .FALSE.                                                  BOKER841
      LMIX1  = .FALSE.                                                  BOKER842
      LMIX2  = .FALSE.                                                  BOKER843
c Three triggers TRIGA2 = ALEPH                                         BOKER844
      CALL TRIGA2(TH1N,TH2N,TH1W,TH2W,NPHI,XWIDE,XNARR,XMIX1,XMIX2)     BOKER845
       LWIDE= XWIDE.GT.XMIN                                             BOKER846
       LNARR= XNARR.GT.XMIN                                             BOKER847
       LMIX1= XMIX1.GT.XMIN                                             BOKER848
       LMIX2= XMIX2.GT.XMIN                                             BOKER849
C BHLUM2 total... with vac_pol and Z                                    BOKER850
      IF(LWIDE) NACC(1) = NACC(1) + 1                                   BOKER851
      IF(LNARR) NACC(2) = NACC(2) + 1                                   BOKER852
      IF(LMIX1) NACC(3) = NACC(3) + 1                                   BOKER853
      IF(LMIX2) NACC(4) = NACC(4) + 1                                   BOKER854
      ELSEIF(MODE.EQ.1) THEN                                            BOKER855
*     ***********************                                           BOKER856
      WRITE(NOUT,BXOPE)                                                 BOKER857
      WRITE(NOUT,BXTXT) '============ BOKER8 ============='             BOKER858
      WRITE(NOUT,BXTXT) '      BHLUM2 related tests       '             BOKER859
      CALL BHLUMI(   1,XPAR,NPAR)                                       BOKER860
      NEVT  = NPAR(20)                                                  BOKER861
      XCRU  = XPAR(20)                                                  BOKER862
      WRITE(NOUT,BXTXT) '*********************************'             BOKER863
      WRITE(NOUT,BXTXT) 'Integrated x-sect. total best    '             BOKER864
      WRITE(NOUT,BXTXT) '(with vac., with Z, with s-chan.)'             BOKER865
      WRITE(NOUT,BXTXT) 'UNWEIGHTED EVENTS                '             BOKER866
      WRITE(NOUT,BXTXT) '*********************************'             BOKER867
      WRITE(NOUT,BXL1I) NEVGEN,     'generated events   ','NEVGEN','  ' BOKER868
      WRITE(NOUT,BXL1I) NACC(1),    'wide-wide accepted ','NACC1 ','  ' BOKER869
      WRITE(NOUT,BXL1I) NACC(2),    'narrow-narrow acc. ','NACC2 ','  ' BOKER870
      WRITE(NOUT,BXL1I) NACC(3),    'narrow-wide accept.','NACC3 ','  ' BOKER871
      WRITE(NOUT,BXL1I) NACC(4),    'wide-narrow accept.','NACC4 ','  ' BOKER872
      XSMCNB = XPAR(20)                                                 BOKER873
      ERMC   = XSMCNB*XPAR(21)                                          BOKER874
      FACC1 = FLOAT(NACC(1))                                            BOKER875
      FACC2 = FLOAT(NACC(2))                                            BOKER876
      FACC3 = FLOAT(NACC(3))                                            BOKER877
      FACC4 = FLOAT(NACC(4))                                            BOKER878
      FACCNW= (FACC3+FACC4)/2D0                                         BOKER879
      FEVGEN= FLOAT(NEVGEN)                                             BOKER880
      XWW    = XSMCNB*FACC1/FEVGEN                                      BOKER881
      XNN    = XSMCNB*FACC2/FEVGEN                                      BOKER882
      XNW    = XSMCNB*FACCNW/FEVGEN                                     BOKER883
      DXWW   = SQRT((FACC1/FEVGEN)**2*ERMC**2+                          BOKER884
     & (XSMCNB/FEVGEN)**2*FACC1+(XSMCNB*FACC1/FEVGEN**2)**2*FEVGEN)     BOKER885
      DXNN   = SQRT((FACC2/FEVGEN)**2*ERMC**2+                          BOKER886
     & (XSMCNB/FEVGEN)**2*FACC2+(XSMCNB*FACC2/FEVGEN**2)**2*FEVGEN)     BOKER887
      DXNW   = SQRT((FACCNW/FEVGEN)**2*ERMC**2+                         BOKER888
     & (XSMCNB/FEVGEN)**2*FACCNW/2D0+                                   BOKER889
     & (XSMCNB*FACCNW/FEVGEN**2)**2*FEVGEN)                             BOKER890
      WRITE(NOUT,BXL2G) XWW,DXWW,   'wide-wide accepted ','XWW   ','  ' BOKER891
      WRITE(NOUT,BXL2G) XNN,DXNN,   'narrow-narrow acc. ','XNN   ','  ' BOKER892
      WRITE(NOUT,BXL2G) XNW,DXNW,   'narrow-wide accept.','XNW   ','  ' BOKER893
C  type values of rundom generator on the end of gereration             BOKER894
      CALL RMARUT(I1OUT,N1OUT,N2OUT)                                    BOKER895
 1234 FORMAT(1X,'RANMAR at the end of generation',3I12)                 BOKER896
      WRITE(NOUT,1234) I1OUT,N1OUT,N2OUT                                BOKER897
      ENDIF                                                             BOKER898
*     *****                                                             BOKER899
      END                                                               BOKER100
      SUBROUTINE BOKERS(MODE)                                           BOKERS 2
C*****************************************************************      BOKERS 3
C Calulate cross-section with UNWEIGHTED events within geometrical      BOKERS 4
C acceptance of ALEPH                                                   BOKERS 5
C                                  Ramon Miquel   novemb 1992           BOKERS 6
C*****************************************************************      BOKERS 7
      IMPLICIT REAL*8(A-H,O-Z)                                          BOKERS 8
      PARAMETER( PI = 3.1415926535897932D0 )                            BOKERS 9
      CHARACTER*80      BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G BOKERS10
      PARAMETER(                                                        BOKERS11
     $BXOPE =  '(//1X,15(5H=====)    )',                                BOKERS12
     $BXTXT =  '(1X,1H=,                  A48,25X,    1H=)',            BOKERS13
     $BXL1I =  '(1X,1H=,I17,                 16X, A20,A12,A7, 1X,1H=)', BOKERS14
     $BXL1F =  '(1X,1H=,F17.8,               16X, A20,A12,A7, 1X,1H=)', BOKERS15
     $BXL2F =  '(1X,1H=,F17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H=)', BOKERS16
     $BXL1G =  '(1X,1H=,G17.8,               16X, A20,A12,A7, 1X,1H=)', BOKERS17
     $BXL2G =  '(1X,1H=,G17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H=)', BOKERS18
     $BXCLO =  '(1X,15(5H=====)/   )'    )                              BOKERS19
      COMMON / INOUT  / NINP,NOUT,NOUT2                                 INOUT  2
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(100)                  BOKERS21
      COMMON / PARGEN / CMSENE,TMING,RAXIG,VMAXG,XK0,KEYOPT,KEYRAD      BOKERS22
      COMMON / PAROBL / TMINW,RAXIW,TMINN,RAXIN,VMAXE,KEYTRI            BOKERS23
C THIS IS COMMON BLOCK FROM INSIDE GENERATOR !!!!!!!                    BOKERS24
      COMMON / TRANSR / TRAN,TRMIN,TRMAX                                BOKERS25
C !!!!!!!                                                               BOKERS26
      LOGICAL LWIDE,LNARR,LMIX1,LMIX2                                   BOKERS27
      DIMENSION NPAR(100),XPAR(100)                                     BOKERS28
      DIMENSION NACC(4)                                                 BOKERS29
cc-rm                                                                   BOKERS30
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT       BOKERS31
      parameter (conv=pi/180.)                                          BOKERS32
cc-rm                                                                   BOKERS33
      IF(MODE.EQ.-1) THEN                                               BOKERS34
*     *******************                                               BOKERS35
cc-rm                                                                   BOKERS36
        nback  = 0                                                      BOKERS37
        nbackp = 0                                                      BOKERS38
        nbackt = 0                                                      BOKERS39
        trmist = 91.284d0**2 * (1.d0 - cos(0.6d0*pi/180.d0)) / 2.d0     BOKERS40
        trmast = 91.284d0**2 * (1.d0 - cos(4.1d0*pi/180.d0)) / 2.d0     BOKERS41
cc-rm                                                                   BOKERS42
                                                                        BOKERS43
        XMIN= 0.1                                                       BOKERS44
C.....initialize scalers                                                BOKERS45
        DO 10 K = 1,4                                                   BOKERS46
   10   NACC(K) = 0                                                     BOKERS47
        NEVGEN  = 0                                                     BOKERS48
      ELSEIF(MODE.EQ.0) THEN                                            BOKERS49
*     **********************                                            BOKERS50
        NEVGEN=NEVGEN+1                                                 BOKERS51
        LWIDE  = .FALSE.                                                BOKERS52
        LNARR  = .FALSE.                                                BOKERS53
        LMIX1  = .FALSE.                                                BOKERS54
        LMIX2  = .FALSE.                                                BOKERS55
C Three triggers TRIGAS = ALEPH    SICAL                                BOKERS56
        CALL TRIGAS(TH1N,TH2N,TH1W,TH2W,NPHI,XWIDE,XNARR,XMIX1,XMIX2)   BOKERS57
C       WRITE (6,*) ' X wide narr,mix1 mix2 ' , XWIDE,XNARR,XMIX1,XMIX2 BOKERS58
        LWIDE= XWIDE.GT.XMIN                                            BOKERS59
        LNARR= XNARR.GT.XMIN                                            BOKERS60
        LMIX1= XMIX1.GT.XMIN                                            BOKERS61
        LMIX2= XMIX2.GT.XMIN                                            BOKERS62
C BHLUM2 total... with vac_pol and Z                                    BOKERS63
        IF(LWIDE) NACC(1) = NACC(1) + 1                                 BOKERS64
        IF(LNARR) NACC(2) = NACC(2) + 1                                 BOKERS65
        IF(LMIX1) NACC(3) = NACC(3) + 1                                 BOKERS66
        IF(LMIX2) NACC(4) = NACC(4) + 1                                 BOKERS67
cc-rm                                                                   BOKERS68
        if (   (lmix1.or.lmix2) .and.                                   BOKERS69
     .      ( (abs(p2(3)/p2(4)).lt.cos(conv*4.1)) .or.                  BOKERS70
     .        (abs(p2(3)/p2(4)).gt.cos(conv*0.6)) .or.                  BOKERS71
     .        (abs(q2(3)/q2(4)).lt.cos(conv*4.1)) .or.                  BOKERS72
     .        (abs(q2(3)/q2(4)).gt.cos(conv*0.6)) ) ) nback = nback + 1 BOKERS73
        if (   (lmix1.or.lmix2) .and.                                   BOKERS74
     .      (tran.lt.trmist .or. tran.gt.trmast)  ) nbackp = nbackp + 1 BOKERS75
        if  (tran.lt.trmist .or. tran.gt.trmast)  nbackt = nbackt + 1   BOKERS76
cc-rm                                                                   BOKERS77
      ELSEIF(MODE.EQ.1) THEN                                            BOKERS78
*     ***********************                                           BOKERS79
cc-rm                                                                   BOKERS80
        write (6,*) 'nback = ', nback                                   BOKERS81
        write (6,*) 'nbackp= ', nbackp                                  BOKERS82
        write (6,*) 'nbackt= ', nbackt                                  BOKERS83
cc-rm                                                                   BOKERS84
        WRITE(NOUT,BXOPE)                                               BOKERS85
        WRITE(NOUT,BXTXT) '============ BOKERS ============='           BOKERS86
        WRITE(NOUT,BXTXT) '      BHLUM2 related tests       '           BOKERS87
        CALL BHLUMI(   1,XPAR,NPAR)                                     BOKERS88
        NEVT  = NPAR(20)                                                BOKERS89
        XCRU  = XPAR(20)                                                BOKERS90
        WRITE(NOUT,BXTXT) '*********************************'           BOKERS91
        WRITE(NOUT,BXTXT) 'Integrated x-sect. total best    '           BOKERS92
        WRITE(NOUT,BXTXT) '(with vac., with Z, with s-chan.)'           BOKERS93
        WRITE(NOUT,BXTXT) 'UNWEIGHTED EVENTS                '           BOKERS94
        WRITE(NOUT,BXTXT) '*********************************'           BOKERS95
        WRITE(NOUT,BXL1I) NEVGEN,     'generated events   ','NEVGEN',   BOKERS96
     &    '  '                                                          BOKERS97
        WRITE(NOUT,BXL1I) NACC(1),    'wide-wide accepted ','NACC1 ',   BOKERS98
     &    '  '                                                          BOKERS99
        WRITE(NOUT,BXL1I) NACC(2),    'narrow-narrow acc. ','NACC2 ',   BOKER100
     &    '  '                                                          BOKER101
        WRITE(NOUT,BXL1I) NACC(3),    'narrow-wide accept.','NACC3 ',   BOKER102
     &    '  '                                                          BOKER103
        WRITE(NOUT,BXL1I) NACC(4),    'wide-narrow accept.','NACC4 ',   BOKER104
     &    '  '                                                          BOKER105
        XSMCNB = XPAR(20)                                               BOKER106
        ERMC   = XSMCNB*XPAR(21)                                        BOKER107
        FACC1 = FLOAT(NACC(1))                                          BOKER108
        FACC2 = FLOAT(NACC(2))                                          BOKER109
        FACC3 = FLOAT(NACC(3))                                          BOKER110
        FACC4 = FLOAT(NACC(4))                                          BOKER111
        FACCNW= (FACC3+FACC4)/2D0                                       BOKER112
        FEVGEN= FLOAT(NEVGEN)                                           BOKER113
        XWW    = XSMCNB*FACC1/FEVGEN                                    BOKER114
        XNN    = XSMCNB*FACC2/FEVGEN                                    BOKER115
        XNW    = XSMCNB*FACCNW/FEVGEN                                   BOKER116
cc        DXWW   = SQRT((FACC1/FEVGEN)**2*ERMC**2+                      BOKER117
cc     &    (XSMCNB/FEVGEN)**2*FACC1+(XSMCNB*FACC1/FEVGEN**2)**2*FEVGEN)BOKER118
cc        DXNN   = SQRT((FACC2/FEVGEN)**2*ERMC**2+                      BOKER119
cc     &    (XSMCNB/FEVGEN)**2*FACC2+(XSMCNB*FACC2/FEVGEN**2)**2*FEVGEN)BOKER120
cc        DXNW   = SQRT((FACCNW/FEVGEN)**2*ERMC**2+                     BOKER121
cc     &    (XSMCNB/FEVGEN)**2*FACCNW/2D0+                              BOKER122
cc     &    (XSMCNB*FACCNW/FEVGEN**2)**2*FEVGEN)                        BOKER123
cc-rm                                                                   BOKER124
        DXWW   = SQRT((FACC1/FEVGEN)**2*ERMC**2+                        BOKER125
     &    (XSMCNB/FEVGEN)**2*FACC1-(XSMCNB*FACC1/FEVGEN**2)**2*FEVGEN)  BOKER126
        DXNN   = SQRT((FACC2/FEVGEN)**2*ERMC**2+                        BOKER127
     &    (XSMCNB/FEVGEN)**2*FACC2-(XSMCNB*FACC2/FEVGEN**2)**2*FEVGEN)  BOKER128
        DXNW   = SQRT((FACCNW/FEVGEN)**2*ERMC**2+                       BOKER129
     &    (XSMCNB/FEVGEN)**2*FACCNW -                                   BOKER130
     &    (XSMCNB*FACCNW/FEVGEN**2)**2*FEVGEN)                          BOKER131
cc-rm                                                                   BOKER132
        WRITE(NOUT,BXL2G) XWW,DXWW,   'wide-wide accepted ','XWW   ',   BOKER133
     &    '  '                                                          BOKER134
        WRITE(NOUT,BXL2G) XNN,DXNN,   'narrow-narrow acc. ','XNN   ',   BOKER135
     &    '  '                                                          BOKER136
        WRITE(NOUT,BXL2G) XNW,DXNW,   'narrow-wide accept.','XNW   ',   BOKER137
     &    '  '                                                          BOKER138
C  type values of rundom generator on the end of gereration             BOKER139
        CALL RMARUT(I1OUT,N1OUT,N2OUT)                                  BOKER140
 1234   FORMAT(1X,'RANMAR at the end of generation',3I12)               BOKER141
        WRITE(NOUT,1234) I1OUT,N1OUT,N2OUT                              BOKER142
      ENDIF                                                             BOKER143
*     *****                                                             BOKER144
      END                                                               BOKER145
      SUBROUTINE TRIGA2 (TH1N,TH2N,TH1W,TH2W,NPHI,XWIDE,XNARR,          TRIGA2 2
     $                  XMIX1,XMIX2)                                    TRIGA2 3
C     **********************************************************        TRIGA2 4
C MODIFIED TRIGA1 FOR ALEPH GEOMETRICAL CUTS                            TRIGA2 5
C Idealized exper. CALORIMETRIC trigger on dressed final electrons.     TRIGA2 6
C Electrons and photons not distinguished!                              TRIGA2 7
C     ******************************************                        TRIGA2 8
      IMPLICIT REAL*8(A-H,O-Z)                                          TRIGA2 9
      PARAMETER( PI = 3.1415926535897932D0 )                            TRIGA210
      LOGICAL LANGW,LANGN,LPHI                                          TRIGA211
      LOGICAL LNARROW,LWIDE,KEEPIT                                      TRIGA212
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT       TRIGA213
      REAL *4 X4,Y4                                                     TRIGA214
      DIMENSION PC(100,4)                                               TRIGA215
      DIMENSION PCW1(4),PCN1(4)                                         TRIGA216
      DIMENSION PCW2(4),PCN2(4)                                         TRIGA217
      DATA ICONT /0/                                                    TRIGA218
C Beam energy                                                           TRIGA219
      ENE = P1(4)                                                       TRIGA220
C Final electrons and photons not distinguished                         TRIGA221
      DO 10 K=1,4                                                       TRIGA222
      PC(1,K)=P2(K)                                                     TRIGA223
   10 PC(2,K)=Q2(K)                                                     TRIGA224
      DO 20 I=1,NPHOT                                                   TRIGA225
      DO 20 K=1,4                                                       TRIGA226
   20 PC(2+I,K)=PHOT(I,K)                                               TRIGA227
      NP = NPHOT+2                                                      TRIGA228
      DO 40 K=1,4                                                       TRIGA229
      PCW1(K)=0D0                                                       TRIGA230
      PCN1(K)=0D0                                                       TRIGA231
      PCW2(K)=0D0                                                       TRIGA232
   40 PCN2(K)=0D0                                                       TRIGA233
C                                                                       TRIGA234
C Collecting energies in calorimeter sectors                            TRIGA235
C                                                                       TRIGA236
      Z0 = 2800.D0                                                      TRIGA237
      DO 100 I=1,NP                                                     TRIGA238
C wide/narrow sectors forward                                           TRIGA239
C Staszek's angels                                                      TRIGA240
      THETA=ANGFI(ABS(PC(I,3)),DSQRT(PC(I,1)**2+PC(I,2)**2))            TRIGA241
      PHI  =ANGFI(PC(I,1),PC(I,2))                                      TRIGA242
C Bolek's angels                                                        TRIGA243
c****      THETA = ACOS(ABS(PC(I,3))/PC(I,4))                           TRIGA244
C***      PHIA  = ATG( PC(I,1),PC(I,2)) - PI/2.                         TRIGA245
      X     = Z0*PC(I,1)/ABS(PC(I,3))                                   TRIGA246
      Y     = Z0*PC(I,2)/ABS(PC(I,3))                                   TRIGA247
      LNARROW = .FALSE.                                                 TRIGA248
      LWIDE   = .TRUE.                                                  TRIGA249
      IF( KEEPIT(X,Y,5) ) LNARROW = .TRUE.                              TRIGA250
      CALL LOOSEC(THETA,PHI,LWIDE)                                      TRIGA251
      LANGW = .NOT. LWIDE                                               TRIGA252
      LANGN = LNARROW                                                   TRIGA253
C**      X4 = X                                                         TRIGA254
C**      Y4 = Y                                                         TRIGA255
C**      IF(LANGN) CALL HF2(100,X4,Y4,1.)                               TRIGA256
C**      IF(LANGW) CALL HF2(200,X4,Y4,1.)                               TRIGA257
C**      TYPE *,I,PC(I,1),PC(I,2),THETA,THET,PHIA,PHI,X,Y,              TRIGA258
C**     &       LNARROW,LWIDE,langw,LANGN                               TRIGA259
      IF(PC(I,3) .GT. 0D0) THEN                                         TRIGA260
      IF(LANGW) THEN                                                    TRIGA261
        DO 50 K=1,4                                                     TRIGA262
   50   PCW1(K)=PCW1(K)+ PC(I,K)                                        TRIGA263
      ENDIF                                                             TRIGA264
      IF(LANGN) THEN                                                    TRIGA265
        DO 51 K=1,4                                                     TRIGA266
   51   PCN1(K)=PCN1(K)+ PC(I,K)                                        TRIGA267
      ENDIF                                                             TRIGA268
      ELSE                                                              TRIGA269
C wide/narrow sectors backward                                          TRIGA270
      IF(LANGW) THEN                                                    TRIGA271
        DO 70 K=1,4                                                     TRIGA272
   70   PCW2(K)=PCW2(K)+ PC(I,K)                                        TRIGA273
      ENDIF                                                             TRIGA274
      IF(LANGN) THEN                                                    TRIGA275
        DO 71 K=1,4                                                     TRIGA276
   71   PCN2(K)=PCN2(K)+ PC(I,K)                                        TRIGA277
      ENDIF                                                             TRIGA278
      ENDIF                                                             TRIGA279
C**      TYPE *,PCW1(4),PCW2(4),PCN1(4),PCN2(4)                         TRIGA280
  100 CONTINUE                                                          TRIGA281
C at least one coincidences in a pair of opposite calorimetric blocks   TRIGA282
      XWIDE= 0D0                                                        TRIGA283
      XNARR= 0D0                                                        TRIGA284
      XMIX1= 0D0                                                        TRIGA285
      XMIX2= 0D0                                                        TRIGA286
      IF(PCW1(4)/ENE .GT. 0.44D0  .AND.                                 TRIGA287
     &   PCW2(4)/ENE .GT. 0.44D0  .AND.                                 TRIGA288
     &  (PCW1(4)+PCW2(4))/(2D0*ENE) .GT. 0.6D0) XWIDE = PCW1(4)+PCW2(4) TRIGA289
      IF(PCN1(4)/ENE .GT. 0.44D0  .AND.                                 TRIGA290
     &   PCN2(4)/ENE .GT. 0.44D0  .AND.                                 TRIGA291
     &  (PCN1(4)+PCN2(4))/(2D0*ENE) .GT. 0.6D0) XNARR = PCN1(4)+PCN2(4) TRIGA292
      IF(PCW1(4)/ENE .GT. 0.44D0  .AND.                                 TRIGA293
     &   PCN2(4)/ENE .GT. 0.44D0  .AND.                                 TRIGA294
     &  (PCW1(4)+PCN2(4))/(2D0*ENE) .GT. 0.6D0) XMIX1 = PCW1(4)+PCN2(4) TRIGA295
      IF(PCN1(4)/ENE .GT. 0.44D0  .AND.                                 TRIGA296
     &   PCW2(4)/ENE .GT. 0.44D0  .AND.                                 TRIGA297
     &  (PCN1(4)+PCW2(4))/(2D0*ENE) .GT. 0.6D0) XMIX2 = PCN1(4)+PCW2(4) TRIGA298
      END                                                               TRIGA299
      SUBROUTINE TRIGAS (TH1N,TH2N,TH1W,TH2W,NPHI,XWIDE,XNARR,          TRIGAS 2
     $                  XMIX1,XMIX2)                                    TRIGAS 3
C     **********************************************************        TRIGAS 4
C MODIFIED TRIGA1 FOR ALEPH GEOMETRICAL CUTS                            TRIGAS 5
C Idealized exper. CALORIMETRIC trigger on dressed final electrons.     TRIGAS 6
C Electrons and photons not distinguished!                              TRIGAS 7
C     ******************************************                        TRIGAS 8
      IMPLICIT REAL*8(A-H,O-Z)                                          TRIGAS 9
      real*4 xxf,yyf,xxb,yyb,atg                                        TRIGAS10
      external atg                                                      TRIGAS11
      PARAMETER( PI = 3.1415 26535897932D0 )                            TRIGAS12
C Depth at which the fiducial cut is done (mm)                          TRIGAS13
cc-This is the number that was used for the tests                       TRIGAS14
cc-   PARAMETER (Z0 = 2527.8)                                           TRIGAS15
cc      PARAMETER (Z0 = 2528.31)                                        TRIGAS16
      COMMON / CDBHGO / R0A,R0B,CORMIN,CORMAX,Z0A,Z0B                   CDBHGO 2
      real*4 R0A,R0B,CORMIN,CORMAX,Z0A,Z0B                              CDBHGO 3
      COMMON/ CDECUT /E1CUT,E2CUT                                       CDBHGO 4
      real*4 E1CUT,E2CUT                                                CDBHGO 5
C Cuts in energy in one side and sum of both sides                      TRIGAS18
C     PARAMETER (E1CUT = 0.44, E2CUT = 0.60)                            TRIGAS19
C     PARAMETER (E1CUT = 0.44, E2CUT = 0.713)                           TRIGAS20
C Delta phi cut                                                         TRIGAS21
      PARAMETER (PHICUT = PI/6.)                                        TRIGAS22
      LOGICAL KEEPN,KEEPW,KEEPS                                         TRIGAS23
      LOGICAL LANGW,LANGN,LANGS                                         TRIGAS24
      LOGICAL LANGFW,LANGFN,LANGBN,LANGBW                               TRIGAS25
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT       TRIGAS26
      DIMENSION PC(100,4)                                               TRIGAS27
      DIMENSION PCW1(4),PCN1(4)                                         TRIGAS28
      DIMENSION PCW2(4),PCN2(4)                                         TRIGAS29
      DIMENSION PCB(4),PCF(4)                                           TRIGAS30
      DATA ICONT /0/                                                    TRIGAS31
C Beam energy                                                           TRIGAS32
      ENE = P1(4)                                                       TRIGAS33
C Final electrons and photons not distinguished                         TRIGAS34
      DO K=1,4                                                          TRIGAS35
        PC(1,K)=P2(K)                                                   TRIGAS36
        PC(2,K)=Q2(K)                                                   TRIGAS37
      ENDDO                                                             TRIGAS38
      DO I=1,NPHOT                                                      TRIGAS39
        DO K=1,4                                                        TRIGAS40
          PC(2+I,K)=PHOT(I,K)                                           TRIGAS41
        ENDDO                                                           TRIGAS42
      ENDDO                                                             TRIGAS43
C                                                                       TRIGAS44
      DO K=1,4                                                          TRIGAS45
        PCW1(K)=0D0                                                     TRIGAS46
        PCN1(K)=0D0                                                     TRIGAS47
        PCW2(K)=0D0                                                     TRIGAS48
        PCN2(K)=0D0                                                     TRIGAS49
        PCB(K)=0D0                                                      TRIGAS50
        PCF(K)=0D0                                                      TRIGAS51
      ENDDO                                                             TRIGAS52
C                                                                       TRIGAS53
C Collecting energies in calorimeter sectors                            TRIGAS54
C                                                                       TRIGAS55
      NP = NPHOT+2                                                      TRIGAS56
      DO 100 I= 1,NP                                                    TRIGAS57
CCC        THETA = ANGFI(ABS(PC(I,3)),DSQRT(PC(I,1)**2+PC(I,2)**2))     TRIGAS58
CCC        PHI   = ANGFI(PC(I,1),PC(I,2))                               TRIGAS59
        z    = pc(i,3)                                                  TRIGAS60
        if ( z.gt.0.) z0=z0a                                            TRIGAS61
        if ( z.lt.0.) z0=z0b                                            TRIGAS62
        X     = Z0*PC(I,1)/ABS(PC(I,3))                                 TRIGAS63
        Y     = Z0*PC(I,2)/ABS(PC(I,3))                                 TRIGAS64
C       WRITE ( 6,*) ' I  , X Y Z0' , I,X,Y,Z0                          TRIGAS65
        LANGS = .FALSE.                                                 TRIGAS66
        IF( KEEPS(X,Y,z) ) LANGS = .TRUE.                               TRIGAS67
C                                                                       TRIGAS68
        if (.not.langs) go to 100                                       TRIGAS69
C forward                                                               TRIGAS70
        IF(PC(I,3) .GT. 0D0) THEN                                       TRIGAS71
          IM = 1                                                        TRIGAS72
          IF(LANGS) THEN                                                TRIGAS73
            DO 50 K=1,4                                                 TRIGAS74
   50       PCF(K)=PCF(K)+ PC(I,K)                                      TRIGAS75
          ENDIF                                                         TRIGAS76
        ELSE                                                            TRIGAS77
C backward                                                              TRIGAS78
          IM = 2                                                        TRIGAS79
          IF(LANGS) THEN                                                TRIGAS80
            DO 70 K=1,4                                                 TRIGAS81
   70       PCB(K)=PCB(K)+ PC(I,K)                                      TRIGAS82
          ENDIF                                                         TRIGAS83
        ENDIF                                                           TRIGAS84
  100 CONTINUE                                                          TRIGAS85
        xf= 0.                                                          TRIGAS86
        yf = 0.                                                         TRIGAS87
        xb = 0.                                                         TRIGAS88
        yb = 0.                                                         TRIGAS89
C     WRITE ( 6,* ) ' PCF ' , PCF                                       TRIGAS90
C     WRITE ( 6,* ) ' PCB ' , PCB                                       TRIGAS91
      if (pcf(3).ne.0.) then                                            TRIGAS92
        XF    = Z0A*PCF(1)/ABS(PCF(3))                                  TRIGAS93
        YF    = Z0A*PCF(2)/ABS(PCF(3))                                  TRIGAS94
C     WRITE ( 6,* ) ' XF,YF ', XF,YF                                    TRIGAS95
      endif                                                             TRIGAS96
      if (pcb(3).ne.0.) then                                            TRIGAS97
        XB    = Z0B*PCB(1)/ABS(PCB(3))                                  TRIGAS98
        YB    = Z0B*PCB(2)/ABS(PCB(3))                                  TRIGAS99
C     WRITE ( 6,* ) ' XB,YB ', XB,YB                                    TRIGA100
      endif                                                             TRIGA101
C at least one coincidence in a pair of opposite calorimetric blocks    TRIGA102
      XWIDE= 0D0                                                        TRIGA103
      XNARR= 0D0                                                        TRIGA104
      XMIX1= 0D0                                                        TRIGA105
      XMIX2= 0D0                                                        TRIGA106
      LANGFN = .FALSE.                                                  TRIGA107
      LANGFW = .FALSE.                                                  TRIGA108
      LANGBN = .FALSE.                                                  TRIGA109
      LANGBW = .FALSE.                                                  TRIGA110
      METHOD= 2                                                         TRIGA111
      IF( KEEPN(XF,YF,IM,METHOD) ) LANGFN = .TRUE.                      TRIGA112
      IF( KEEPW(XF,YF,IM,METHOD) ) LANGFW = .TRUE.                      TRIGA113
      IF( KEEPN(XB,YB,IM,METHOD) ) LANGBN = .TRUE.                      TRIGA114
      IF( KEEPW(XB,YB,IM,METHOD) ) LANGBW = .TRUE.                      TRIGA115
      xxf = xf                                                          TRIGA116
      xxb = xb                                                          TRIGA117
      yyf = yf                                                          TRIGA118
      yyb = yb                                                          TRIGA119
      if (yf.ne.0.)  PHIF = ATG(YYF,XXF)                                TRIGA120
      if (yb.ne.0.)  PHIB = ATG(YYB,XXB)                                TRIGA121
C     write ( 6,*)  'phif phib ' , PHIF , PHIB                          TRIGA122
C     write ( 6,*)  'pcf pcb(4)' , PCF(4),PCB(4)                        TRIGA123
      IF(LANGFW .AND. LANGBW    .AND.                                   TRIGA124
     &   ABS(ABS(PHIF-PHIB)-PI) .LT. PHICUT  .AND.                      TRIGA125
     &   PCF(4)/ENE .GT. E1CUT  .AND.                                   TRIGA126
     &   PCB(4)/ENE .GT. E1CUT  .AND.                                   TRIGA127
     &   (PCF(4)+PCB(4))/(2D0*ENE) .GT. E2CUT) XWIDE = PCF(4)+PCB(4)    TRIGA128
      IF(LANGFN .AND. LANGBN    .AND.                                   TRIGA129
     &   ABS(ABS(PHIF-PHIB)-PI) .LT. PHICUT  .AND.                      TRIGA130
     &   PCF(4)/ENE .GT. E1CUT  .AND.                                   TRIGA131
     &   PCB(4)/ENE .GT. E1CUT  .AND.                                   TRIGA132
     &   (PCF(4)+PCB(4))/(2D0*ENE) .GT. E2CUT) XNARR = PCF(4)+PCB(4)    TRIGA133
      IF(LANGFW .AND. LANGBN    .AND.                                   TRIGA134
     &   ABS(ABS(PHIF-PHIB)-PI) .LT. PHICUT  .AND.                      TRIGA135
     &   PCF(4)/ENE .GT. E1CUT  .AND.                                   TRIGA136
     &   PCB(4)/ENE .GT. E1CUT  .AND.                                   TRIGA137
     &   (PCF(4)+PCB(4))/(2D0*ENE) .GT. E2CUT) XMIX1 = PCF(4)+PCB(4)    TRIGA138
      IF(LANGFN .AND. LANGBW    .AND.                                   TRIGA139
     &   ABS(ABS(PHIF-PHIB)-PI) .LT. PHICUT  .AND.                      TRIGA140
     &   PCF(4)/ENE .GT. E1CUT  .AND.                                   TRIGA141
     &   PCB(4)/ENE .GT. E1CUT  .AND.                                   TRIGA142
     &   (PCF(4)+PCB(4))/(2D0*ENE) .GT. E2CUT) XMIX2 = PCF(4)+PCB(4)    TRIGA143
 99   return                                                            TRIGA144
      END                                                               TRIGA145
      LOGICAL FUNCTION KEEPIT(X,Y,METH)                                 KEEPIT 2
C---------------------------------------------------------------------- KEEPIT 3
C!  -                                                                   KEEPIT 4
C!                                                                      KEEPIT 5
C!   Author   :- John Renner Hansen    23-JAN-1990                      KEEPIT 6
C!                                                                      KEEPIT 7
C!   Inputs:Method number:  METHOD = (5,6)                              KEEPIT 8
C!          X,Y of particle                                             KEEPIT 9
C!        -                                                             KEEPIT10
C!                                                                      KEEPIT11
C!   Outputs: KEEPIT                                                    KEEPIT12
C!        -                                                             KEEPIT13
C!                                                                      KEEPIT14
C!   Libraries required:                                                KEEPIT15
C!                                                                      KEEPIT16
C!   Description                                                        KEEPIT17
C!   ===========                                                        KEEPIT18
C!                                                                      KEEPIT19
C?                                                                      KEEPIT20
      IMPLICIT REAL*8(A-H,O-Z)                                          KEEPIT21
      REAL *8 PADS                                                      KEEPIT22
      REAL *8 X,Y,XP,YP                                                 KEEPIT23
      REAL *8 LXP5(9)                                                   KEEPIT24
      REAL *8 HXP(9)                                                    KEEPIT25
      REAL *8 LYP(10)                                                   KEEPIT26
      REAL *8 LXP6(9)                                                   KEEPIT27
      DATA PADS/29.75D0/                                                KEEPIT28
      DATA LXP5/4.5D0,4.5D0,3.5D0,2.5D0,5*1.5D0/                        KEEPIT29
      DATA HXP/10.5D0,9.5D0,9.5D0,9.5D0,8.5D0,7.5D0                     KEEPIT30
     &              ,6.5D0,5.5D0,3.5D0/                                 KEEPIT31
      DATA LYP/0D0,3.D0,4.D0,5.D0,6.D0,7.D0                             KEEPIT32
     &               ,8.D0,9.D0,10.D0,11.D0/                            KEEPIT33
      DATA LXP6/5.5D0,5.5D0,4.5D0,2.5D0,5*1.5D0/                        KEEPIT34
      INTEGER*4 METH                                                    KEEPIT35
      LOGICAL LOG1,LOG2,LOG3,LOG4                                       KEEPIT36
C!======================================================================KEEPIT37
      KEEPIT = .FALSE.                                                  KEEPIT38
C                                                                       KEEPIT39
C     DEFINE ACTIVE REGION                                              KEEPIT40
      XP  =  ABS(X/PADS)                                                KEEPIT41
      YP  =  ABS(Y/PADS)                                                KEEPIT42
C                                                                       KEEPIT43
      IF(METH.EQ.5) THEN                                                KEEPIT44
      DO I = 1,9                                                        KEEPIT45
        LOG1 = XP.GT.LXP5(I)                                            KEEPIT46
        LOG2 = XP.LT.HXP(I)                                             KEEPIT47
        LOG3 = YP.GE.LYP(I)                                             KEEPIT48
        LOG4 = YP.LE.LYP(I+1)                                           KEEPIT49
        IF(LOG1.AND.LOG2.AND.LOG3.AND.LOG4) THEN                        KEEPIT50
          KEEPIT = .TRUE.                                               KEEPIT51
          GOTO 999                                                      KEEPIT52
        ENDIF                                                           KEEPIT53
      ENDDO                                                             KEEPIT54
      ELSE                                                              KEEPIT55
      DO I = 1,9                                                        KEEPIT56
        LOG1 = XP.GT.LXP6(I)                                            KEEPIT57
        LOG2 = XP.LT.HXP(I)                                             KEEPIT58
        LOG3 = YP.GT.LYP(I)                                             KEEPIT59
        LOG4 = YP.LT.LYP(I+1)                                           KEEPIT60
        IF(LOG1.AND.LOG2.AND.LOG3.AND.LOG4) THEN                        KEEPIT61
          KEEPIT = .TRUE.                                               KEEPIT62
          GOTO 999                                                      KEEPIT63
        ENDIF                                                           KEEPIT64
      ENDDO                                                             KEEPIT65
      ENDIF                                                             KEEPIT66
  999 RETURN                                                            KEEPIT67
      END                                                               KEEPIT68
      LOGICAL FUNCTION KEEPN(X,Y,IM,METH)                               KEEPN  2
C---------------------------------------------------------------------- KEEPN  3
C!  -                                                                   KEEPN  4
C!                                                                      KEEPN  5
C!   Inputs:Method number:  METH = (1,2) for the moment                 KEEPN  6
C!          X,Y of particle at Z of fiducial cut                        KEEPN  7
C!        -                                                             KEEPN  8
C!     R. MIQUEL  1992                                                  KEEPN  9
C?                                                                      KEEPN 10
C---------------------------------------------------------------------- KEEPN 11
      IMPLICIT REAL*8(A-H,O-Z)                                          KEEPN 12
C inner radius, radius of one pad, correction for asymmetry (mm)        KEEPN 13
C      PARAMETER (R0   = 60.93, RPAD = 5.225, CORR = 0.028)             KEEPN 14
      PARAMETER ( RPAD = 5.225)                                         KEEPN 15
      COMMON / CDBHGO / R0A,R0B,CORMIN,CORMAX,Z0A,Z0B                   CDBHGO 2
      real*4 R0A,R0B,CORMIN,CORMAX,Z0A,Z0B                              CDBHGO 3
      COMMON/ CDECUT /E1CUT,E2CUT                                       CDBHGO 4
      real*4 E1CUT,E2CUT                                                CDBHGO 5
C                                                                       KEEPN 17
C!======================================================================KEEPN 18
      KEEPN = .FALSE.                                                   KEEPN 19
C                                                                       KEEPN 20
      R   = SQRT(X**2+Y**2)                                             KEEPN 21
      IF ( IM.EQ.1) R0=R0A                                              KEEPN 22
      IF ( IM.EQ.2) R0=R0B                                              KEEPN 23
C                                                                       KEEPN 24
C min, max pad for fiducial cut                                         KEEPN 25
      IF     (METH.EQ.1) THEN                                           KEEPN 26
        PMIN = 2.                                                       KEEPN 27
        PMAX = 12.                                                      KEEPN 28
      ELSEIF (METH.EQ.2) THEN                                           KEEPN 29
        PMIN = 3.                                                       KEEPN 30
        PMAX = 12.                                                      KEEPN 31
      ENDIF                                                             KEEPN 32
C min, max radius for fiducial cut                                      KEEPN 33
      RMIN = R0 + PMIN*RPAD - CORMIN                                    KEEPN 34
      RMAX = R0 + PMAX*RPAD - CORMAX                                    KEEPN 35
C     WRITE (6,*) 'R,R0,RMIN,RMAX' ,  R,R0,RMIN,RMAX                    KEEPN 36
      IF ( (R .GT. RMIN) .AND. (R .LT. RMAX) ) THEN                     KEEPN 37
        KEEPN = .TRUE.                                                  KEEPN 38
      ENDIF                                                             KEEPN 39
C     WRITE (6,* ) 'KEEPN ' , KEEPN                                     KEEPN 40
  999 RETURN                                                            KEEPN 41
      END                                                               KEEPN 42
      LOGICAL FUNCTION KEEPW(X,Y,im,METH)                               KEEPW  2
C---------------------------------------------------------------------- KEEPW  3
C!  -                                                                   KEEPW  4
C!                                                                      KEEPW  5
C!   Inputs:Method number:  METH = (1,2) for the moment                 KEEPW  6
C!          X,Y of particle at Z of fiducial cut                        KEEPW  7
C!        -                                                             KEEPW  8
C!    R. MIQUEL 1992                                                    KEEPW  9
C?                                                                      KEEPW 10
C---------------------------------------------------------------------- KEEPW 11
      IMPLICIT REAL*8(A-H,O-Z)                                          KEEPW 12
C inner radius, radius of one pad                                       KEEPW 13
      COMMON / CDBHGO / R0A,R0B,CORMIN,CORMAX,Z0A,Z0B                   CDBHGO 2
      real*4 R0A,R0B,CORMIN,CORMAX,Z0A,Z0B                              CDBHGO 3
      COMMON/ CDECUT /E1CUT,E2CUT                                       CDBHGO 4
      real*4 E1CUT,E2CUT                                                CDBHGO 5
C      PARAMETER (R0   = 60.93, RPAD = 5.225, CORR = 0.028)             KEEPW 15
      PARAMETER ( RPAD = 5.225 )                                        KEEPW 16
C!======================================================================KEEPW 17
      KEEPW = .FALSE.                                                   KEEPW 18
C                                                                       KEEPW 19
      R   = SQRT(X**2+Y**2)                                             KEEPW 20
      IF ( IM.EQ.1) R0=R0A                                              KEEPW 21
      IF ( IM.EQ.2) R0=R0B                                              KEEPW 22
C                                                                       KEEPW 23
C min, max pad for non-fiducial cut                                     KEEPW 24
      IF ( (METH.EQ.1) .OR. (METH.EQ.2) ) THEN                          KEEPW 25
        PMIN = 1.                                                       KEEPW 26
        PMAX = 15.                                                      KEEPW 27
      ENDIF                                                             KEEPW 28
C min, max radius for non-fiducial cut                                  KEEPW 29
      RMIN = R0 + PMIN*RPAD - CORMIN                                    KEEPW 30
      RMAX = R0 + PMAX*RPAD - CORMAX                                    KEEPW 31
C     WRITE (6,*) 'R,R0,RMIN,RMAX' ,  R,R0,RMIN,RMAX                    KEEPW 32
      IF ( (R .GT. RMIN) .AND. (R .LT. RMAX) ) THEN                     KEEPW 33
        KEEPW = .TRUE.                                                  KEEPW 34
      ENDIF                                                             KEEPW 35
C     WRITE (6,* ) 'KEEPW ' , KEEPW                                     KEEPW 36
  999 RETURN                                                            KEEPW 37
      END                                                               KEEPW 38
      LOGICAL FUNCTION KEEPS(X,Y,Z)                                     KEEPS  2
C---------------------------------------------------------------------- KEEPS  3
C!  -                                                                   KEEPS  4
C!                                                                      KEEPS  5
C!   Inputs: X,Y of particle at Z of fiducial cut                       KEEPS  6
C!                                                                      KEEPS  7
C?      R. MIQUEL 1992                                                  KEEPS  8
C---------------------------------------------------------------------- KEEPS  9
      IMPLICIT REAL*8(A-H,O-Z)                                          KEEPS 10
C inner radius, radius of one pad                                       KEEPS 11
C      PARAMETER (R0   = 60.93, RPAD = 5.225)                           KEEPS 12
      COMMON / CDBHGO / R0A,R0B,CORMIN,CORMAX,Z0A,Z0B                   CDBHGO 2
      real*4 R0A,R0B,CORMIN,CORMAX,Z0A,Z0B                              CDBHGO 3
      COMMON/ CDECUT /E1CUT,E2CUT                                       CDBHGO 4
      real*4 E1CUT,E2CUT                                                CDBHGO 5
C      PARAMETER (R0   = 60.93, RPAD = 5.225, CORR = 0.028)             KEEPS 14
      PARAMETER ( RPAD = 5.225 )                                        KEEPS 15
C!======================================================================KEEPS 16
      KEEPS = .FALSE.                                                   KEEPS 17
C                                                                       KEEPS 18
      R   = SQRT(X**2+Y**2)                                             KEEPS 19
      if(z.gt.0.) r0 = r0a                                              KEEPS 20
      if(z.lt.0.) r0 = r0b                                              KEEPS 21
C                                                                       KEEPS 22
C min, max pad for being inside SiCAL                                   KEEPS 23
      PMIN = 0.                                                         KEEPS 24
      PMAX = 16.                                                        KEEPS 25
C min, max radius for non-fiducial cut                                  KEEPS 26
      RMIN = R0 + PMIN*RPAD                                             KEEPS 27
      RMAX = R0 + PMAX*RPAD                                             KEEPS 28
C     WRITE (6,*) 'R,R0,RMIN,RMAX' ,  R,R0,RMIN,RMAX                    KEEPS 29
      IF ( (R .GT. RMIN) .AND. (R .LT. RMAX) ) THEN                     KEEPS 30
        KEEPS = .TRUE.                                                  KEEPS 31
      ENDIF                                                             KEEPS 32
C     WRITE (6,* ) 'KEEPS ' , KEEPS                                     KEEPS 33
  999 RETURN                                                            KEEPS 34
      END                                                               KEEPS 35
      SUBROUTINE LOOSEC(THETA,PHI,REJFL)                                LOOSEC 2
C---------------------------------------------------------------------- LOOSEC 3
C!  - Bhabha selection method 5 on loose side                           LOOSEC 4
C!                                                                      LOOSEC 5
C!   Author   :- Peter H. Hansen       27-NOV-1990                      LOOSEC 6
C!                                                                      LOOSEC 7
C!   Inputs: THETA,PHI of cluster centroid in global system             LOOSEC 8
C!   Output: REJFL = .TRUE. if rejected                                 LOOSEC 9
C!                 = .FALSE. if accepted                                LOOSEC10
C?                                                                      LOOSEC11
C!======================================================================LOOSEC12
      IMPLICIT REAL*8(A-H,O-Z)                                          LOOSEC13
      DIMENSION DXN(4),DYN(4),DZN(4),OMXN(4),OMYN(4),OMZN(4)            LOOSEC14
      DIMENSION DIST(2)                                                 LOOSEC15
      LOGICAL REJFL                                                     LOOSEC16
C 1991 Alignment constants                                              LOOSEC17
       DATA DXN/ -0.096, -0.118,  0.039,  0.027/                        LOOSEC18
       DATA DYN/ -0.310, -0.012, -0.026, -0.100/                        LOOSEC19
       DATA DZN/  0.134,  0.164,  0.317,  0.320/                        LOOSEC20
       DATA OMXN/ -0.00019, 0.00007, -0.00309, -0.00353/                LOOSEC21
       DATA OMYN/ -0.00119, -0.00043, -0.00030, -0.00260/               LOOSEC22
       DATA OMZN/  0.01248, 0.01248, -0.00148, -0.00148/                LOOSEC23
C-----------------------------------------------------------------------LOOSEC24
      REJFL = .TRUE.                                                    LOOSEC25
C                                                                       LOOSEC26
C Sign z and module number                                              LOOSEC27
      IF(THETA.GT.1.D0) THEN                                            LOOSEC28
        SIG  =-1.D0                                                     LOOSEC29
        MODU = 2                                                        LOOSEC30
      ELSE                                                              LOOSEC31
        SIG  = 1.D0                                                     LOOSEC32
        MODU = 4                                                        LOOSEC33
      ENDIF                                                             LOOSEC34
      IF(COS(PHI).LT.0.D0) MODU=MODU-1                                  LOOSEC35
C                                                                       LOOSEC36
C X and Y in global system                                              LOOSEC37
C                                                                       LOOSEC38
      X = SIG*280.D0*COS(PHI)*TAN(THETA)                                LOOSEC39
      Y = SIG*280.D0*SIN(PHI)*TAN(THETA)                                LOOSEC40
C                                                                       LOOSEC41
C Local system                                                          LOOSEC42
      ZL = SIG*17.5D0                                                   LOOSEC43
      XLOC = X - DXN(MODU) - OMZN(MODU)*Y + OMYN(MODU)*ZL               LOOSEC44
      YLOC = Y - DYN(MODU) - OMXN(MODU)*ZL + OMZN(MODU)*X               LOOSEC45
C                                                                       LOOSEC46
C Fold into first quadrant                                              LOOSEC47
      XLOC = ABS(XLOC)                                                  LOOSEC48
      YLOC = ABS(YLOC)                                                  LOOSEC49
C                                                                       LOOSEC50
C Find distance to edges (as in LCLUTW, but database hardwired)         LOOSEC51
      DIST(1) = XLOC - 1.9D0                                            LOOSEC52
      DIST(2) = 24.5D0                                                  LOOSEC53
      IF(YLOC.GT.8.4D0+1.2D0) THEN                                      LOOSEC54
        DIS = XLOC-(17.5D0-YLOC)/0.75D0                                 LOOSEC55
        IF(DIS.LT.DIST(1)) DIST(1)=DIS                                  LOOSEC56
        DIST(2) = YLOC-(17.5D0-XLOC*0.75D0)                             LOOSEC57
      ELSE                                                              LOOSEC58
        DIST(1) = XLOC-11.9D0                                           LOOSEC59
      ENDIF                                                             LOOSEC60
C                                                                       LOOSEC61
C Now make the cut on the inner boundary                                LOOSEC62
C and on the outer boundary                                             LOOSEC63
      IF(THETA.GT.1.D0) THETA = 3.14159D0-THETA                         LOOSEC64
      IF(DIST(1).GT.1.D0  .AND.                                         LOOSEC65
     &   DIST(2).GT.0.75D0  .AND.                                       LOOSEC66
     &   THETA.LT.0.125D0)  REJFL = .FALSE.                             LOOSEC67
C**      TYPE *,DIST(1),DIST(2),THETA,REJFL                             LOOSEC68
C                                                                       LOOSEC69
  999 CONTINUE                                                          LOOSEC70
      END                                                               LOOSEC71
      SUBROUTINE RBTOA(P)                                               RBTOA  2
      REAL*8 P(4),Q(3)                                                  RBTOA  3
C 92  PARAMETER(AX=-0.59E-3,AY=+0.20E-3)                                RBTOA  4
C 93  PARAMETER(AX= 0.131E-3,AY=-0.266E-3)                              RBTOA  5
      COMMON  / CDGENE /SDVRT(3),VPOS(3),NEVENT(12),NEVPHO(2),VANG(2)   BBL003 1
      AX = VANG(1)                                                      RBTOA  7
      AY = VANG(2)                                                      RBTOA  8
C                                                                       RBTOA  9
      DO 111 I = 1, 3                                                   RBTOA 10
 111  Q(I) = P(I)                                                       RBTOA 11
C                                                                       RBTOA 12
      P(1) =     Q(1)            + AX*Q(3)                              RBTOA 13
      P(2) =               Q(2)  + AY*Q(3)                              RBTOA 14
      P(3) = -AX*Q(1)  -AY*Q(2)  +    Q(3)                              RBTOA 15
      END                                                               RBTOA 16
CC-RM                                                                   RBTOA 17