      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)                 ASKUSE 2
C --------------------------------------------------------------------  ASKUSE 3
C                                                                       ASKUSE 4
C --------------------------------------------------------------------  ASKUSE 5
      COMMON /KGCOMM/ IST,NTR,IDP,ECM,WEI                               ASKUSE 6
      LENTRY = 2                                                        ASKUSE 7
      CALL BHAB01(LENTRY)                                               ASKUSE 8
      IDPR = IDP                                                        ASKUSE 9
      NTRK = NTR                                                        ASKUSE10
      NVRT = 1                                                          ASKUSE11
      ISTA = IST                                                        ASKUSE12
      ECMS = ECM                                                        ASKUSE13
      WEIT = WEI                                                        ASKUSE14
      RETURN                                                            ASKUSE15
      END                                                               ASKUSE16
      SUBROUTINE ASKUSI(IGCOD)                                          ASKUSI 2
C --------------------------------------------------------------------  ASKUSI 3
C                                                                       ASKUSI 4
C --------------------------------------------------------------------  ASKUSI 5
      PARAMETER (LBCS=1000,LCHAR=4)                                     BCS    2
      COMMON/BCS/ IW(LBCS)                                              BCS    3
      REAL RW(LBCS)                                                     BCS    4
      EQUIVALENCE (RW(1),IW(1))                                         BCS    5
      COMMON / UNICOM / IIN,IUT                                         UNICOM 2
      PARAMETER ( IGCO = 2002)                                          ASKUSI 8
C                                                                       ASKUSI 9
C   Return the generator code as defined in the KINGAL library          ASKUSI10
C                                                                       ASKUSI11
      IGCOD = IGCO                                                      ASKUSI12
      IUT = IW(6)                                                       ASKUSI13
      WRITE(IUT,101) IGCOD                                              ASKUSI14
 101  FORMAT(/,10X,                                                     ASKUSI15
     &       'BHAB01 - CODE NUMBER =',I4,' Last mod. February,1990',    BBL02  1
     & /,10X,'***********************************************',//)      ASKUSI17
C                                                                       ASKUSI18
      LENTRY = 1                                                        ASKUSI19
C                                                                       ASKUSI20
C  CORRECTION TO BE DONE TO TAKE STANDARD ALEPH MASSES DEFINITIONS      ASKUSI21
C                                                                       ASKUSI22
      CALL BHAB01(LENTRY)                                               ASKUSI23
C  Print PART and KLIN banks                                            ASKUSI24
C                                                                       ASKUSI25
      CALL PRPART                                                       ASKUSI26
C                                                                       ASKUSI27
      CALL PRTABL('RLEP',0)                                             BBL01  2
      CALL PRTABL('KPAR',0)                                             ASKUSI28
C                                                                       ASKUSI29
      RETURN                                                            ASKUSI30
      END                                                               ASKUSI31
      SUBROUTINE BHAB01(LENTRY)                                         BHAB01 2
C***************************************************************        BHAB01 3
C                                                                       BHAB01 4
C     MONTE CARLO EVENT GENERATOR FOR THE PROCESSES                     BHAB01 5
C                                                                       BHAB01 6
C         E+(PP) E-(PM)   ---->  E+(QP) E-(QM)                          BHAB01 7
C                                                                       BHAB01 8
C     AND                                                               BHAB01 9
C                                                                       BHAB0110
C         E+(PP) E-(PM)   ----> E+(QP) E-(QM) PHOTON(QK)                BHAB0111
C                                                                       BHAB0112
C                                                                       BHAB0113
C  AUTHORS: R. KLEISS, CERN                                             BHAB0114
C           F.A. BERENDS, LEIDEN UNIVERSITY, LEIDEN, HOLLAND            BHAB0115
C           W. HOLLIK, HAMBURG UNIVERSITY, HAMBURG, GERMANY             BHAB0116
C                                                                       BHAB0117
C                                                                       BHAB0118
C  THE INPUT QUANTITIES ARE                                             BHAB0119
C  EB    = BEAM ENERGY, IN GEV;                                         BHAB0120
C  XMZ   = MASS OF THE Z0 BOSON, IN GEV;                                BHAB0121
C  XMH   = MASS OF THE HIGGS BOSON, IN GEV;                             BHAB0122
C  XMT   = MASS OF THE TOP QUARK, IN GEV;                               BHAB0123
C  THMIN = MINIMUM POLAR SCATTERING ANGLE OF THE E+,E-, IN              BHAB0124
C          DEGREES: IT MUST BE LARGER THAN 0 DEGREES BUT                BHAB0125
C          SMALLER THAN THMAX;                                          BHAB0126
C  THMAX = MAXIMUM POLAR SCATTERING ANGLE OF THE E+,E-, IN              BHAB0127
C          DEGREES: IT MUST BE SMALLER THAN 180 DEGREES BUT             BHAB0128
C          LARGER THAN THMIN;                                           BHAB0129
C  XKMAX = MAXIMUM ENERGY OF THE BREMSSTRAHLUNG PHOTON, AS A            BHAB0130
C          FRACTION OF THE BEAM ENERGY E: IT MAY BE SET                 BHAB0131
C          TO 1, IN WHICH CASE THE PROGRAM CHANGES ITS VALUE            BHAB0132
C          TO THE ACTUAL KINEMATIC LIMIT.                               BHAB0133
C                                                                       BHAB0134
C                                                                       BHAB0135
C  IN THE PRESENT PROGRAM THE W MASS AND THE ELECTROWEAK MIXING ANGLE   BHAB0136
C  ARE CALCULATED FROM THE (AS YET) MORE ACCURATELY KNOWN VALUE         BHAB0137
C  OF THEMUON LIFETIME. THIS IS DONE BY ROUTINE FINDMW                  BHAB0138
C                                                                       BHAB0139
C***************************************************************        BHAB0140
C  THIS IS THE MAIN PROGRAM, CONSISTING OF:                             BHAB0141
C 1) INITIALIZATION OF THE GENERATOR;                                   BHAB0142
C 2) GENERATION OF AN EVENT SAMPLE,                                     BHAB0143
C    AND SUBSEQUENT ANALYSIS OF THE EVENTS;                             BHAB0144
C 3) EVALUATION OF THE TOTAL GENERATED CROSS SECTION                    BHAB0145
C                                                                       BHAB0146
C                                                                       BHAB0147
      PARAMETER (LBCS=1000,LCHAR=4)                                     BCS    2
      COMMON/BCS/ IW(LBCS)                                              BCS    3
      REAL RW(LBCS)                                                     BCS    4
      EQUIVALENCE (RW(1),IW(1))                                         BCS    5
      COMMON / UNICOM / IIN,IUT                                         UNICOM 2
C                                                                       BHAB0150
      COMMON /KGCOMM/ ISTA,NTR,IDPR,ECMS,WEIT                           BHAB0151
      REAL*8 EB,XMZ,XMT,XMH,THMIN,THMAX,XKMAX,W,S2W                     BHAB0152
      REAL*8 SIGTOT,ERRTOT                                              BHAB0153
      REAL*8 PP(4),PM(4),QP(4),QM(4),QK(4)                              BHAB0154
      DIMENSION SDVRT(3),VRTEX(4),TABL(12),TABK(4),ITAB(3)              BBL01  3
      DIMENSION NEVENT(10),NEVPHO(2)                                    BHAB0156
      COMMON / INPUT1 / EB                                              BHAB0157
      COMMON / INPUT2 / XMZ,S2W,XMH,XMT                                 BHAB0158
      COMMON / INPUT3 / THMIN,THMAX,XKMAX                               BHAB0159
      INTEGER ALTABL,ALRLEP                                             BBL01  4
      EXTERNAL ALTABL ,ALRLEP                                           BBL01  5
C                                                                       BHAB0162
C  INITIALIZATION            *********************                      BHAB0163
C                                                                       BHAB0164
      IF(LENTRY.EQ.1) THEN                                              BHAB0165
C                                                                       BHAB0166
C THE SETUP PHASE: ASK FOR THE INPUT PARAMETERS                         BHAB0167
C                                                                       BHAB0168
       IIN=5                                                            BHAB0169
       EB      =   46.1D0                                               BHAB0170
       XMZ     =   92.0D0                                               BHAB0171
       XMT     =   60.0D0                                               BHAB0172
       XMH     =  100.0D0                                               BHAB0173
       THMIN   =   10.0D0                                               BHAB0174
       THMAX   =  170.0D0                                               BHAB0175
       XKMAX   =    1.0D0                                               BHAB0176
       WEIOPT  =    1.                                                  BBL01  6
       WTMAX   =    1.7                                                 BBL01  7
C                                                                       BHAB0177
C  The default values can be changed by the DATA CARD GENE              BHAB0178
       JGENE = NLINK('GENE',0)                                          BHAB0179
       IF(JGENE.NE.0) THEN                                              BHAB0180
        EB = RW(JGENE+1)                                                BHAB0181
        XMZ = RW(JGENE+2)                                               BHAB0182
        XMH = RW(JGENE+3)                                               BHAB0183
        XMT = RW(JGENE+4)                                               BHAB0184
        THMIN = RW(JGENE+5)                                             BHAB0185
        THMAX = RW(JGENE+6)                                             BHAB0186
        XKMAX = RW(JGENE+7)                                             BHAB0187
        WEIOPT  = RW(JGENE+8)                                           BBL01  8
        WTMAX   = RW(JGENE+9)                                           BBL01  9
       ENDIF                                                            BHAB0188
       TABL(1) = EB                                                     BHAB0189
       TABL(2) = XMZ                                                    BHAB0190
       TABL(3) = XMT                                                    BHAB0191
       TABL(4) = XMH                                                    BHAB0192
       TABL(5) = THMIN                                                  BHAB0193
       TABL(6) = THMAX                                                  BHAB0194
       TABL(7) = XKMAX                                                  BHAB0195
       TABL(8) = WEIOPT                                                 BBL01 10
       TABL(9) = WTMAX                                                  BBL01 11
C                                                                       BHAB0196
C  Main vertex generation                                               BHAB0197
       SDVRT(1) = 0.035                                                 BHAB0198
       SDVRT(2) = 0.0012                                                BHAB0199
       SDVRT(3) = 1.28                                                  BHAB0100
       JSVRT = NLINK('SVRT',0)                                          BHAB0101
       IF(JSVRT.NE.0) THEN                                              BHAB0102
        SDVRT(1) = RW(JSVRT+1)                                          BHAB0103
        SDVRT(2) = RW(JSVRT+2)                                          BHAB0104
        SDVRT(3) = RW(JSVRT+3)                                          BHAB0105
       ENDIF                                                            BHAB0106
       TABL(10)= SDVRT(1)                                               BBL01 12
       TABL(11)= SDVRT(2)                                               BBL01 13
       TABL(12)= SDVRT(3)                                               BBL01 14
C                                                                       BBL01 15
C  Fill the KPAR bank with the generator parameters                     BBL01 16
       NCOL = 12                                                        BBL01 17
       NROW = 1                                                         BBL01 18
       JKPAR = ALTABL('KPAR',NCOL,NROW,TABL,'2I,(F)','C')               BBL01 19
C  Fill RLEP bank                                                       BBL01 20
       IEBEAM = NINT(EB *1000  )                                        BBL01 21
       JRLEP = ALRLEP(IEBEAM,'    ',0,0,0)                              BBL01 22
C                                                                       BHAB0115
C  Initialize events counters                                           BHAB0116
       DO 10 I = 1,10                                                   BHAB0117
   10  NEVENT(I) = 0                                                    BHAB0118
       DO 11 I = 1,2                                                    BHAB0119
   11  NEVPHO(I) = 0                                                    BHAB0120
C                                                                       BHAB0121
C Booking of some standard histogrammes                                 BHAB0122
       CALL HBOOK1(10001,'Energy distribution : final e+$',30,0.,60.,0.)BHAB0123
       CALL HIDOPT(10001,'LOGY')                                        BHAB0124
       CALL HBOOK1(10002,'Energy distribution : final e-$',30,0.,60.,0.)BHAB0125
       CALL HIDOPT(10002,'LOGY')                                        BHAB0126
       CALL HBOOK1(10003,'Energy distribution : gamma$',30,0.,60.,0.)   BHAB0127
       CALL HIDOPT(10003,'LOGY')                                        BHAB0128
       CALL HBOOK1(10004,'Energy distribution : gamma < 1. GeV$',       BHAB0129
     &                                                     40,0.,1.,0.) BHAB0130
       CALL HIDOPT(10004,'LOGY')                                        BHAB0131
       CALL HBOOK1(10005,'Polar angle distribution : final e+$',        BHAB0132
     &                                                     40,-1.,1.,0.)BHAB0133
       CALL HIDOPT(10005,'LOGY')                                        BHAB0134
       CALL HBOOK1(10006,'Polar angle distribution : final e-$',        BHAB0135
     &                                                     40,-1.,1.,0.)BHAB0136
       CALL HIDOPT(10006,'LOGY')                                        BHAB0137
       CALL HBOOK1(10007,'Weight distribution for hard events$',        BHAB0138
     &                                                     40,0.,2.,0.) BHAB0139
       CALL HBOOK2(10008,'Hard events : weight distribution    versus   BHAB0140
     &photon energy$',30,0.,60.,40,0.,2.,0.)                            BHAB0141
C                                                                       BHAB0142
C THE INITIALIZATION STEP OF THE PROGRAM                                BHAB0143
       CALL SETBAB(EB,XMZ,XMH,XMT,THMIN,THMAX,XKMAX)                    BHAB0144
C GB   CALL OUTCRY('GENBAB')                                            BHAB0145
       RETURN                                                           BHAB0146
      ENDIF                                                             BHAB0147
C                                                                       BHAB0148
C  EVENT GENERATION          *********************                      BHAB0149
C                                                                       BHAB0150
      IF(LENTRY.EQ.2) THEN                                              BHAB0151
C                                                                       BHAB0152
C  EVENT STATUS (0 = O.K.)                                              BHAB0153
       ISTA = 0                                                         BHAB0154
C                                                                       BHAB0155
C  Initialize the track number (radiated photon included)               BHAB0156
       NTR = 3                                                          BHAB0157
C                                                                       BHAB0158
C  Generate primary vertex                                              BHAB0159
       CALL RANNOR (RN1,RN2)                                            BHAB0160
       CALL RANNOR (RN3,DUM)                                            BHAB0161
       VRTEX(1) = RN1*SDVRT(1)                                          BHAB0162
       VRTEX(2) = RN2*SDVRT(2)                                          BHAB0163
       VRTEX(3) = RN3*SDVRT(3)                                          BHAB0164
       VRTEX(4) = 0.                                                    BHAB0165
C                                                                       BHAB0166
C GB   CALL TELLER(K,1000,'EVENT LOOP')                                 BHAB0167
 1     NEVENT(1) = NEVENT(1) + 1                                        BBL01 23
       CALL GENBAB(PP,PM,QP,QM,QK,W,ICON)                               BHAB0169
       CALL CANCUT(QP,QM,QK,W)                                          BHAB0170
       IDPR = ICON                                                      BHAB0171
       WEIT = W                                                         BHAB0172
       ECMS = 2.*EB                                                     BHAB0173
       WTMAX = MAX(WEIT,WTMAX)                                          BBL01 24
       IF (WEIOPT.EQ.1.) THEN                                           BBL01 25
          IF (WEIT/WTMAX .LT. RNDM(DUM)) THEN                           BBL01 26
            GO TO 1                                                     BBL01 27
          ENDIF                                                         BBL01 28
          WEIT = 1.                                                     BBL01 29
       ENDIF                                                            BBL01 30
C                                                                       BHAB0174
C  Now fill 'KINE' and 'VERT' banks                                     BHAB0175
C                                                                       BHAB0176
       IVMAI = 1                                                        BHAB0177
       JVERT = KBVERT(IVMAI,VRTEX,0)                                    BHAB0178
       IF(JVERT.EQ.0) THEN                                              BHAB0179
        ISTA = 1                                                        BHAB0180
        NEVENT(2) = NEVENT(2) + 1                                       BHAB0181
        GO TO 97                                                        BHAB0182
       ENDIF                                                            BHAB0183
C  book 'KINE' for beam electrons (-1 and -2)                           BHAB0184
       DO 90 I = 1,3                                                    BHAB0185
   90   TABK(I) = (-1.**I)*PP(I)                                        BHAB0186
       TABK(4) = 0.                                                     BHAB0187
       JKINE = KBKINE(-1,TABK,2,0)                                      BHAB0188
       IF(JKINE.EQ.0) THEN                                              BHAB0189
        ISTA = 2                                                        BHAB0190
        NEVENT(3) = NEVENT(3) + 1                                       BHAB0191
        GO TO 97                                                        BHAB0192
       ENDIF                                                            BHAB0193
       DO 91 I = 1,3                                                    BHAB0194
   91   TABK(I) = (-1.**I)*PM(I)                                        BHAB0195
       TABK(4) = 0.                                                     BHAB0196
       JKINE = KBKINE(-2,TABK,3,0)                                      BHAB0197
       IF(JKINE.EQ.0) THEN                                              BHAB0198
        ISTA = 3                                                        BHAB0199
        NEVENT(4) = NEVENT(4) + 1                                       BHAB0200
        GO TO 97                                                        BHAB0201
       ENDIF                                                            BHAB0202
C  book 'KINE' for final state particles                                BHAB0203
       DO 92 I = 1,3                                                    BHAB0204
   92   TABK(I) = (-1.**I)*QP(I)                                        BHAB0205
       TABK(4) = 0.                                                     BHAB0206
       JKINE = KBKINE(1,TABK,2,IVMAI)                                   BHAB0207
       IF(JKINE.EQ.0) THEN                                              BHAB0208
        ISTA = 4                                                        BHAB0209
        NEVENT(5) = NEVENT(5) + 1                                       BHAB0210
        GO TO 97                                                        BHAB0211
       ENDIF                                                            BHAB0212
       DO 93 I = 1,3                                                    BHAB0213
   93   TABK(I) = (-1.**I)*QM(I)                                        BHAB0214
       TABK(4) = 0.                                                     BHAB0215
       JKINE = KBKINE(2,TABK,3,IVMAI)                                   BHAB0216
       IF(JKINE.EQ.0) THEN                                              BHAB0217
        ISTA = 5                                                        BHAB0218
        NEVENT(6) = NEVENT(6) + 1                                       BHAB0219
        GO TO 97                                                        BHAB0220
       ENDIF                                                            BHAB0221
C We did not book the radiated photon if the energy is equal to zero    BHAB0222
       IF(QK(4).LT.1.E-06) THEN                                         BHAB0223
        NTR = 2                                                         BHAB0224
        GO TO 95                                                        BHAB0225
       ENDIF                                                            BHAB0226
       DO 94 I = 1,3                                                    BHAB0227
   94   TABK(I) = (-1.**I)*QK(I)                                        BHAB0228
       TABK(4) = 0.                                                     BHAB0229
       JKINE = KBKINE(3,TABK,1,IVMAI)                                   BHAB0230
       IF(JKINE.EQ.0) THEN                                              BHAB0231
        ISTA = 6                                                        BHAB0232
        NEVENT(7) = NEVENT(7) + 1                                       BHAB0233
        GO TO 97                                                        BHAB0234
       ENDIF                                                            BHAB0235
C                                                                       BHAB0236
C  Fill history with 'KHIS' bank                                        BHAB0237
   95  DO 96 I = 1,NTR                                                  BHAB0238
   96  ITAB(I) = 0                                                      BHAB0239
       JKHIS = ALTABL('KHIS',1,NTR,ITAB,'I','E')                        BHAB0240
       IF(JKHIS.EQ.0) THEN                                              BHAB0241
        ISTA = 7                                                        BHAB0242
        NEVENT(8) = NEVENT(8) + 1                                       BHAB0243
       ENDIF                                                            BHAB0244
C                                                                       BHAB0245
   97  IF(ISTA.NE.0) NEVENT(9) = NEVENT(9) + 1                          BHAB0246
       IF(ISTA.EQ.0) THEN                                               BHAB0247
        NEVENT(10) = NEVENT(10) + 1                                     BHAB0248
        IF(NTR.EQ.2) NEVPHO(1) = NEVPHO(1) +1                           BHAB0249
        IF(NTR.EQ.3) NEVPHO(2) = NEVPHO(2) +1                           
        qp4= QP(4)
        qm4= Qm(4)
        qk4= Qk(4)

        CALL HFILL(10001,QP4,0.,WEIT)                                 
        CALL HFILL(10002,QM4,0.,WEIT)                                 
        CALL HFILL(10003,QK4,0.,WEIT)                                 
        IF(QK(4).LT.1.0D0) CALL HFILL(10004,QK4,0.,WEIT)              
C        ISIGNE = +1                                                    J9001H 2
C        IF(QP(3).GE.0.D0) ISIGNE=-1                                    J9001H 3
        QPTHET = -1.*QP(3)/SQRT(QP(1)**2+QP(2)**2+QP(3)**2)             J9001H 4
        CALL HFILL(10005,QPTHET,0.,WEIT)                                J9001H 5
        QMTHET = -1.*QM(3)/SQRT(QM(1)**2+QM(2)**2+QM(3)**2)             J9001H 6
        CALL HFILL(10006,QMTHET,0.,WEIT)                                BHAB0260
        IF(QK(4).NE.0.0D0) THEN                                         BHAB0261
         CALL HFILL(10007,WEIT,0.,1.)                                   BHAB0262
         CALL HFILL(10008,QK4,WEIT,1.)                                
        ENDIF                                                           BHAB0264
       ENDIF                                                            BHAB0265
C                                                                       BHAB0266
       RETURN                                                           BHAB0267
      ENDIF                                                             BHAB0268
C                                                                       BHAB0269
C  END OF GENERATION         *********************                      BHAB0270
C                                                                       BHAB0271
      IF(LENTRY.EQ.3) THEN                                              BHAB0272
C                                                                       BHAB0273
C EVALUATION OF THE GENERATED CROSS SECTION                             BHAB0274
       CALL ENDBAB(SIGTOT,ERRTOT)                                       BHAB0275
       CALL EFFCIT                                                      BHAB0276
       CALL ENDCUT(SIGTOT)                                              BHAB0277
       WRITE(IUT,'(''   ********MAXIMUM WEIGHT REACHED '',F12.4)')WTMAX BBL01 31
       WRITE(IUT,101)                                                   BHAB0278
  101  FORMAT(//20X,'EVENTS STATISTICS',                                BHAB0279
     &         /20X,'*****************')                                BHAB0280
       WRITE(IUT,102)NEVENT(1),NEVENT(10),NEVPHO(1),NEVPHO(2),NEVENT(9) BHAB0281
  102  FORMAT(/5X,'# OF GENERATED EVENTS                = ',I10,        BHAB0282
     &        /5X,'# OF ACCEPTED  EVENTS                = ',I10,        BHAB0283
     &        /5X,'# OF ACCEPTED  EVENTS WITHOUT PHOTON = ',I10,        BHAB0284
     &        /5X,'# OF ACCEPTED  EVENTS WITH PHOTON    = ',I10,        BHAB0285
     &        /5X,'# OF REJECTED  EVENTS                = ',I10)        BHAB0286
       WRITE(IUT,103)                                                   BHAB0287
  103  FORMAT(//20X,'ERRORS STATISTICS',                                BHAB0288
     &         /20X,'*****************')                                BHAB0289
       WRITE(IUT,104)NEVENT(2),NEVENT(3),NEVENT(4),NEVENT(5),NEVENT(6), BHAB0290
     &               NEVENT(7),NEVENT(8)                                BHAB0291
  104  FORMAT(/10X,'ISTA = 1 BOS ERROR VERT     # OF REJECT = ',I10,    BHAB0292
     &        /10X,'ISTA = 2 BOS ERROR KINE e+  # OF REJECT = ',I10,    BHAB0293
     &        /10X,'ISTA = 3 BOS ERROR KINE e-  # OF REJECT = ',I10,    BHAB0294
     &        /10X,'ISTA = 4 BOS ERROR KINE f+  # OF REJECT = ',I10,    BHAB0295
     &        /10X,'ISTA = 5 BOS ERROR KINE f-  # OF REJECT = ',I10,    BHAB0296
     &        /10X,'ISTA = 6 BOS ERROR KINE gam # OF REJECT = ',I10,    BHAB0297
     &        /10X,'ISTA = 7 BOS ERROR KHIS     # OF REJECT = ',I10)    BHAB0298
      ENDIF                                                             BHAB0299
C                                                                       BHAB0300
      RETURN                                                            BHAB0301
      END                                                               BHAB0302
      SUBROUTINE USCJOB                                                 USCJOB 2
C --------------------------------------------------------------------  USCJOB 3
C                                                                       USCJOB 4
C --------------------------------------------------------------------  USCJOB 5
      LENTRY = 3                                                        USCJOB 6
      CALL BHAB01(LENTRY)                                               USCJOB 7
      RETURN                                                            USCJOB 8
      END                                                               USCJOB 9
