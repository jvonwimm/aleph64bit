cFrom BLOCH@alws.cern.ch Fri Feb 13 16:20:28 2004
cDate: Fri, 13 Feb 2004 16:16:20 +0100
cFrom: BLOCH@alws.cern.ch
cTo: BLOCH@alws.cern.ch

      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)                 ASKUSE 2
C --------------------------------------------------------------------  ASKUSE 3
C                                                                       ASKUSE 4
C --------------------------------------------------------------------  ASKUSE 5
      COMMON /KGCOMM/ IST,NTR,IDP,NVR,ECM,WEI                           ASKUSE 6
      LENTRY = 2                                                        ASKUSE 7
      CALL DIGZ01(LENTRY)                                               ASKUSE 8
      IDPR = IDP                                                        ASKUSE 9
      NTRK = NTR                                                        ASKUSE10
      NVRT = NVR                                                        ASKUSE11
      ISTA = IST                                                        ASKUSE12
      ECMS = ECM                                                        ASKUSE13
      WEIT = WEI                                                        ASKUSE14
      RETURN                                                            ASKUSE15
      END                                                               ASKUSE16
      SUBROUTINE ASKUSI(IGCOD)                                          ASKUSI 2
C --------------------------------------------------------------------  ASKUSI 3
C                                                                       ASKUSI 4
C --------------------------------------------------------------------  ASKUSI 5
      INTEGER LMHLEN,LMHCOL,LMHROW                                      BCS    2
      PARAMETER (LMHLEN=2 ,LMHCOL=1 ,LMHROW=2)                          BCS    3
      PARAMETER (LBCS=1000,LCHAR=4)                                     BCS    4
      COMMON/BCS/ IW(LBCS)                                              BCS    5
      REAL RW(LBCS)                                                     BCS    6
      EQUIVALENCE (RW(1),IW(1))                                         BCS    7
      PARAMETER ( IGCO = 6004)                                          ASKUSI 7
C                                                                       ASKUSI 8
C   Return the generator code as defined in the KINGAL library          ASKUSI 9
C                                                                       ASKUSI10
      IGCOD = IGCO                                                      ASKUSI11
      IOUT=IW(6)                                                        ASKUSI12
C                                                                       ASKUSI13
      WRITE (IOUT,'(30X,''W E L C O M E   T O   D I A G 3 6 Z N  '',/,  ASKUSI14
     $                30X,''**********************************'',/,     ASKUSI15
     $                30X,'' Generator code  is  # '',I10       ,/,     ASKUSI16
     $           30x,'' last modification '',                           ASKUSI17
     $''July      23,1991''                                             ASKUSI18
     $ ,/)') IGCOD                                                      ASKUSI19
      LENTRY = 1                                                        ASKUSI20
C                                                                       ASKUSI21
C  CORRECTION TO BE DONE TO TAKE STANDARD ALEPH MASSES DEFINITIONS      ASKUSI22
C                                                                       ASKUSI23
      CALL DIGZ01(LENTRY)                                               ASKUSI24
C  Print PART and KLIN banks                                            ASKUSI25
C                                                                       ASKUSI26
      CALL PRPART                                                       ASKUSI27
C                                                                       ASKUSI28
      CALL PRTABL('KPAR',0)                                             ASKUSI29
      CALL PRTABL('RLEP',0)                                             ASKUSI30
C                                                                       ASKUSI31
      RETURN                                                            ASKUSI32
      END                                                               ASKUSI33
      SUBROUTINE DIGZ01(LENTRY)                                         DIGZ01 2
C***************************************************************        DIGZ01 3
C                                                                       DIGZ01 4
C***************************************************************        DIGZ01 5
C  THIS IS THE MAIN PROGRAM, CONSISTING OF:                             DIGZ01 6
C 1) INITIALIZATION OF THE GENERATOR;                                   DIGZ01 7
C 2) GENERATION OF AN EVENT SAMPLE,                                     DIGZ01 8
C    AND SUBSEQUENT ANALYSIS OF THE EVENTS;                             DIGZ01 9
C 3) EVALUATION OF THE TOTAL GENERATED CROSS SECTION                    DIGZ0110
C                                                                       DIGZ0111
C                                                                       DIGZ0112
      IMPLICIT REAL*8(A-H,O-Z)                                          DIGZ0113
      INTEGER LMHLEN,LMHCOL,LMHROW                                      BCS    2
      PARAMETER (LMHLEN=2 ,LMHCOL=1 ,LMHROW=2)                          BCS    3
      PARAMETER (LBCS=1000,LCHAR=4)                                     BCS    4
      COMMON/BCS/ IW(LBCS)                                              BCS    5
      REAL RW(LBCS)                                                     BCS    6
      EQUIVALENCE (RW(1),IW(1))                                         BCS    7
      COMMON /ANGLE / THPMIN,THPMAX,THMMIN,THMMAX,THMUMI,THMUMA         DIGZ0115
      COMMON /BOUND / W2MIN,W2MAX                                       DIGZ0116
      COMMON /CENINI/ THETA0,C0                                         DIGZ0117
      COMMON /CHARGE/ QCHARG,QCHRG2,QCHRG3,QCHRG4                       DIGZ0118
      PARAMETER ( PI = 3.14159  , DEGRA =PI/180.  , RADEG = 180./PI)    DIGZ0119
      COMMON /EDGE  / WDMIN,WDMAX,SDMIN,SDMAX                           DIGZ0120
      COMMON /FACTOR/ FACE,FACL,FACM,PROC                               DIGZ0121
      COMMON /FTSTAT/ SUMFT,SUMFT2,FTMAX,IEEN                           DIGZ0122
      COMMON /GENC  / XLC1(4),XLC2(4),SA3(4),EA3(3)                     DIGZ0123
      COMMON /GEND  / XLD1(4),SAPD(4),SA4(2),EA4                        DIGZ0124
      COMMON /INIT  / PB,ET,EP(3),ECH(3)                                DIGZ0125
      COMMON /INPUT / EB,ESWE,ESFT,WAP(4),WBP(4),VAP(4)                 DIGZ0126
      COMMON /LOGCM / OUTFL(4)                                          DIGZ0127
      COMMON /MASSES/ XM,XMU,XML,XM2,XMU2,XML2                          DIGZ0128
      COMMON /MOMENZ/ Q1(5),Q2(5),Q3(5),Q4(5),Q5(5),Q6(6)               DIGZ0129
      COMMON /PROPAR/ ID                                                DIGZ0130
      COMMON /SELECT/ IC,ICH                                            DIGZ0131
      COMMON /VECTOR/ P1(4),P2(4),QM(4),QP(4),PM(4),PP(4)               DIGZ0132
      COMMON /WESTAT/ SWE(4),SWEK(4),MWE(4),SUM,SUMK,MAXWE,IWE(4),IGEN  DIGZ0133
      COMMON /WECOUN/ IFAIL(4),IACC(4),INUL(4),ICHG(4),IUNDFL(4),       DIGZ0134
     .                INEG,IONE,IZERO                                   DIGZ0135
      COMMON /WEIGHC/ WEIGHT(4),WEEV,IEVACC                             DIGZ0136
      DIMENSION IDUMP(4)                                                DIGZ0137
      REAL*8 MWE,MAXWE                                                  DIGZ0138
      LOGICAL OUTFL                                                     DIGZ0139
C                                                                       DIGZ0140
      COMMON /KGCOMM/ ISTA,NTR,IDPR,NVRT,ECMS,WEIT                      DIGZ0141
      REAL*4 SDVRT(3),VRTEX(4),TABL(34),TABK(4),ECMS,WEIT,RN1,RN2,RN3   DIGZ0142
      REAL*4 RNDM ,RNF100,DUM,QMTHET,QPTHET,ZMAS,ZWID,SIN2              DIGZ0143
      REAL*4 PMOM(5,4)                                                  DIGZ0144
      DIMENSION ITAB(4),NEVENT(10)                                      DIGZ0145
      INTEGER ALTABL,ALRLEP                                             DIGZ0146
      EXTERNAL ALTABL,ALRLEP,RNDM,RNF100                                DIGZ0147
      DATA IDIA /0/                                                     DIGZ0148
      DATA PIOM /.135D0/                                                DIGZ0149
C                                                                       DIGZ0150
C  INITIALIZATION            *********************                      DIGZ0151
C                                                                       DIGZ0152
      IF(LENTRY.EQ.1) THEN                                              DIGZ0153
        CALL INIDAT                                                     DIGZ0154
      IUT= IW(6)                                                        DIGZ0155
      JTRIG = NLINK('TRIG',0)                                           DIGZ0156
      IF (JTRIG.NE.0) THEN                                              DIGZ0157
        ITOT = IW(JTRIG+2)                                              DIGZ0158
      ELSE                                                              DIGZ0159
        ITOT = 1000                                                     DIGZ0160
      ENDIF                                                             DIGZ0161
C                                                                       DIGZ0162
C THE SETUP PHASE: ASK FOR THE INPUT PARAMETERS                         DIGZ0163
C                                                                       DIGZ0164
C-------------------------------------------------------------          DIGZ0165
C.....EBEAM,PROCE5S CLASS                                               DIGZ0166
      EB =45.6                                                          DIGZ0167
      ITYP = 11                                                         DIGZ0168
C.....IREJEC,ESTIM. MAX. WEIGHTS,INFORMATION LEVEL,INITIAL RANDOM       DIGZ0169
C     NUMBER & CENTRAL DETECTOR THETA ANGLE                             DIGZ0170
C     WEIGTHED OR UN WEIGHTED EVENTS                                    DIGZ0171
      IREJEC = 2                                                        DIGZ0172
      ESWE = 1.1D0                                                      DIGZ0173
      ESFT = 1.3D0                                                      DIGZ0174
      INFO = 1                                                          DIGZ0175
      THETA0 = 25.                                                      DIGZ0176
C.....CUTS ON INVARIANT MASS SQUARED BOTH PAIRS                         DIGZ0177
      W1MIN = 0.                                                        DIGZ0178
      W1MAX = 0.                                                        DIGZ0179
      W2MIN = 0.                                                        DIGZ0180
      W2MAX = 0.                                                        DIGZ0181
C.....CUTS IN ENERGY AND COS(THETA) FOR OUTPUT PARTICLE 1               DIGZ0182
      E1PCUT = 0.                                                       DIGZ0183
      E1MCUT = 0.                                                       DIGZ0184
      C1PCUT = 0.                                                       DIGZ0185
      C1MCUT = 0.                                                       DIGZ0186
C.....CUTS IN ENERGY AND COS(THETA) FOR OUTPUT PARTICLE 2               DIGZ0187
      E2PCUT = 0.                                                       DIGZ0188
      E2MCUT = 0.                                                       DIGZ0189
      C2PCUT = 0.                                                       DIGZ0190
      C2MCUT = 0.                                                       DIGZ0191
C.....RELATIVE IMPORTANCE OF SUBGENERATORS A,B,C AND D........          DIGZ0192
      WAP(1) = 1.                                                       DIGZ0193
      WAP(2) = 1.                                                       DIGZ0194
      WAP(3) = 1.                                                       DIGZ0195
      WAP(4) = 1.                                                       DIGZ0196
      IDUMP(1) = 0                                                      DIGZ0197
      IDUMP(2) = 0                                                      DIGZ0198
      IDUMP(3) = 0                                                      DIGZ0199
      IDUMP(4) = 0                                                      DIGZ0100
C.....IMPORTANCE FACTORS                                                DIGZ0101
      WBP(1) = 1.                                                       DIGZ0102
      WBP(2) = 1.                                                       DIGZ0103
      WBP(3) = 2.31                                                     DIGZ0104
      WBP(4) = 4.39                                                     DIGZ0105
C-------------------------------------------------------------          DIGZ0106
C  The default values can be changed by the DATA CARD GENE              DIGZ0107
       JGENE = NLINK('GENE',0)                                          DIGZ0108
       IF(JGENE.NE.0) THEN                                              DIGZ0109
        EB = RW(JGENE+1)                                                DIGZ0110
        ITYP = IW(JGENE+2)                                              DIGZ0111
        IREJEC = IW(JGENE+3)                                            DIGZ0112
        ESWE = RW(JGENE+4)                                              DIGZ0113
        ESFT = RW(JGENE+5)                                              DIGZ0114
        INFO  = IW(JGENE+6)                                             DIGZ0115
        THETA0 = RW(JGENE+7)                                            DIGZ0116
        ZMAS = RW(JGENE+8)                                              DIGZ0117
        ZWID = RW(JGENE+9)                                              DIGZ0118
        SIN2 = RW(JGENE+10)                                             DIGZ0119
       ENDIF                                                            DIGZ0120
       TABL(1) = EB                                                     DIGZ0121
       TABL(2) = ITYP                                                   DIGZ0122
       TABL(3) = IREJEC                                                 DIGZ0123
       TABL(4) = ESWE                                                   DIGZ0124
       TABL(5) = ESFT                                                   DIGZ0125
       TABL(6) = THETA0                                                 DIGZ0126
       TABL(7) = ZMAS                                                   DIGZ0127
       TABL(8) = ZWID                                                   DIGZ0128
       TABL(9) = SIN2                                                   DIGZ0129
C                                                                       DIGZ0130
C  Main vertex generation                                               DIGZ0131
       SDVRT(1) = 0.0180                                                DIGZ0132
       SDVRT(2) = 0.0010                                                DIGZ0133
       SDVRT(3) = 1.00                                                  DIGZ0134
       JSVRT = NLINK('SVRT',0)                                          DIGZ0135
       IF(JSVRT.NE.0) THEN                                              DIGZ0136
        SDVRT(1) = RW(JSVRT+1)                                          DIGZ0137
        SDVRT(2) = RW(JSVRT+2)                                          DIGZ0138
        SDVRT(3) = RW(JSVRT+3)                                          DIGZ0139
       ENDIF                                                            DIGZ0140
       TABL(30) = SDVRT(1)                                              DIGZ0141
       TABl(31) = SDVRT(2)                                              DIGZ0142
       TABL(32) = SDVRT(3)                                              DIGZ0143
C  The default values can be changed by the DATA CARD GCUT              DIGZ0144
       JGCUT = NLINK('GCUT',0)                                          DIGZ0145
       IF(JGCUT.NE.0) THEN                                              DIGZ0146
        W1MIN = RW(JGCUT+1)                                             DIGZ0147
        W1MAX = RW(JGCUT+2)                                             DIGZ0148
        W2MIN = RW(JGCUT+3)                                             DIGZ0149
        W2MAX = RW(JGCUT+4)                                             DIGZ0150
        E1PCUT = RW(JGCUT+5)                                            DIGZ0151
        E1MCUT = RW(JGCUT+6)                                            DIGZ0152
        C1PCUT = RW(JGCUT+7)                                            DIGZ0153
        C1MCUT = RW(JGCUT+8)                                            DIGZ0154
        E2PCUT = RW(JGCUT+9)                                            DIGZ0155
        E2MCUT = RW(JGCUT+10)                                           DIGZ0156
        C2PCUT = RW(JGCUT+11)                                           DIGZ0157
        C2MCUT = RW(JGCUT+12)                                           DIGZ0158
       ENDIF                                                            DIGZ0159
       TABL(10) = W1MIN                                                 DIGZ0160
       TABL(11) = W1MAX                                                 DIGZ0161
       TABL(12) = W2MIN                                                 DIGZ0162
       TABL(13) = W2MAX                                                 DIGZ0163
       TABL(14) = E1PCUT                                                DIGZ0164
       TABL(15) = E1MCUT                                                DIGZ0165
       TABL(16) = C1PCUT                                                DIGZ0166
       TABL(17) = C1MCUT                                                DIGZ0167
       TABL(18) = E2PCUT                                                DIGZ0168
       TABL(19) = E2MCUT                                                DIGZ0169
       TABL(20) = C2PCUT                                                DIGZ0170
       TABL(21) = C2MCUT                                                DIGZ0171
C  The default values can be changed by the DATA CARD GIMP              DIGZ0172
       JGIMP = NLINK('GIMP',0)                                          DIGZ0173
       IF(JGIMP.NE.0) THEN                                              DIGZ0174
        DO 18 II = 1,4                                                  DIGZ0175
 18    WAP(II) = RW(JGIMP+II)                                           DIGZ0176
        DO 11 II= 1,4                                                   DIGZ0177
 11     IDUMP(II) = IW(JGIMP+4+II)                                      DIGZ0178
        DO 12 II = 1,4                                                  DIGZ0179
 12     WBP(II)= RW(JGIMP+8+II)                                         DIGZ0180
        ENDIF                                                           DIGZ0181
      DO 20 II = 1,4                                                    DIGZ0182
        TABL(21+II) = WAP(II)                                           DIGZ0183
 20     TABL(21+II+4) = WBP(II)                                         DIGZ0184
C                                                                       DIGZ0185
C  Fill the KPAR bank with the generator parameters                     DIGZ0186
       NCOL = 32                                                        DIGZ0187
       NROW = 1                                                         DIGZ0188
       JKPAR = ALTABL('KPAR',NCOL,NROW,TABL,'2I,(F)','C')               DIGZ0189
C  Fill RLEP bank                                                       DIGZ0190
      IEBEAM = NINT(EB *1000. )                                         DIGZ0191
      JRLEP = ALRLEP(IEBEAM,'    ',0,0,0)                               DIGZ0192
C                                                                       DIGZ0193
C  Initialize events counters                                           DIGZ0194
       DO 19 I = 1,10                                                   DIGZ0195
   19  NEVENT(I) = 0                                                    DIGZ0196
C                                                                       DIGZ0197
C Booking of some standard histogrammes                                 DIGZ0198
       CALL HBOOK1(10009,'Weight distribution $',40,0.,2.,0.)           DIGZ0199
       CALL HBOOK1(10001,'Energy distribution final f1+$',30,0.,60.,0.) DIGZ0200
       CALL HIDOPT(10001,'LOGY')                                        DIGZ0201
       CALL HBOOK1(10002,'Energy distribution final f1-$',30,0.,60.,0.) DIGZ0202
       CALL HIDOPT(10002,'LOGY')                                        DIGZ0203
       CALL HBOOK1(10003,'Energy distribution final f2+$',30,0.,60.,0.) DIGZ0204
       CALL HIDOPT(10003,'LOGY')                                        DIGZ0205
       CALL HBOOK1(10004,'Energy distribution final f2-$',30,0.,60.,0.) DIGZ0206
       CALL HIDOPT(10004,'LOGY')                                        DIGZ0207
       CALL HBOOK1(10005,'costeta distribution final f1+$',40,-1.,1.,0.)DIGZ0208
       CALL HIDOPT(10005,'LOGY')                                        DIGZ0209
       CALL HBOOK1(10006,'costeta distribution final f1-$',40,-1.,1.,0.)DIGZ0210
       CALL HIDOPT(10006,'LOGY')                                        DIGZ0211
       CALL HBOOK1(10007,'costeta distribution final f2+$',40,-1.,1.,0.)DIGZ0212
       CALL HIDOPT(10007,'LOGY')                                        DIGZ0213
       CALL HBOOK1(10008,'costeta distribution final f2-$',40,-1.,1.,0.)DIGZ0214
       CALL HIDOPT(10008,'LOGY')                                        DIGZ0215
C                                                                       DIGZ0216
C THE INITIALIZATION STEP OF THE PROGRAM                                DIGZ0217
C==MM=                                                                  DIGZ0218
      I1=MOD(ITYP,10)                                                   DIGZ0219
      I2=ITYP/10                                                        DIGZ0220
      IQ2= MAX0(I1,I2)                                                  DIGZ0221
      IQ1= MIN0(I1,I2)                                                  DIGZ0222
      E1PCUT=E1PCUT/EB                                                  DIGZ0223
      E1MCUT=E1MCUT/EB                                                  DIGZ0224
      E2PCUT=E2PCUT/EB                                                  DIGZ0225
      E2MCUT=E2MCUT/EB                                                  DIGZ0226
      C1PCUT= COS(C1PCUT*DEGRA)                                         DIGZ0227
      C1MCUT= COS(C1MCUT*DEGRA)                                         DIGZ0228
      C2PCUT= COS(C2PCUT*DEGRA)                                         DIGZ0229
      C2MCUT= COS(C2MCUT*DEGRA)                                         DIGZ0230
C==MM=                                                                  DIGZ0231
      PIOM=PIOM/EB                                                      DIGZ0232
      W2MIN  = W2MIN*W2MIN/EB/EB                                        DIGZ0233
      W1MIN  = W1MIN*W1MIN/EB/EB                                        DIGZ0234
      X2M=XMU                                                           DIGZ0235
      X1M=XML                                                           DIGZ0236
      IF(IQ2.GT.4.AND.X2M.LT.PIOM)X2M=PIOM                              DIGZ0237
      IF(IQ1.GT.4.AND.X1M.LT.PIOM)X1M=PIOM                              DIGZ0238
C==MM=                                                                  DIGZ0239
      W2MINO = 4.D0*X2M*X2M                                             DIGZ0240
      W2MAXO = 4.D0*(1.D0-X1M)*(1.D0-X1M)                               DIGZ0241
      IF(W2MIN.LT.W2MINO.OR.W2MIN.EQ.0.) W2MIN = W2MINO                 DIGZ0242
      IF(W2MAX.GT.W2MAXO.OR.W2MAX.EQ.0.) W2MAX = W2MAXO                 DIGZ0243
      R2MIN=W2MIN                                                       DIGZ0244
      R2MAX=W2MAX                                                       DIGZ0245
C==MM=                                                                  DIGZ0246
      W1MINO = 4.D0*X1M*X1M                                             DIGZ0247
      W1MAXO = 4.D0*(1.D0-X2M)*(1.-X2M)                                 DIGZ0248
      IF(W1MIN.LT.W1MINO.OR.W1MIN.EQ.0.) W1MIN = W1MINO                 DIGZ0249
      IF(W1MAX.GT.W1MAXO.OR.W1MAX.EQ.0.) W1MAX = W1MAXO                 DIGZ0250
C                                                                       DIGZ0251
C==MM=  SELECTING THE PROCESS CLASS                                     DIGZ0252
C                                                                       DIGZ0253
      IPROC=0                                                           DIGZ0254
      IF (IQ1.EQ.1) THEN                                                DIGZ0255
         IF (IQ2.EQ.1) IPROC = 5                                        DIGZ0256
         IF (IQ2.EQ.2) IPROC = 3                                        DIGZ0257
         IF (IQ2.EQ.3) IPROC = 4                                        DIGZ0258
      ELSE IF ( IQ1.EQ.2) THEN                                          DIGZ0259
         IF (IQ2.EQ.2) IPROC = 2                                        DIGZ0260
         IF (IQ2.EQ.3) IPROC = 1                                        DIGZ0261
      ELSE IF ( IQ1.EQ.3) THEN                                          DIGZ0262
         IF (IQ2.EQ.3) IPROC = 6                                        DIGZ0263
      ENDIF                                                             DIGZ0264
      IF(IPROC.NE.0) GO TO 37                                           DIGZ0265
      WRITE(IUT,98) IQ1,IQ2                                             DIGZ0266
      STOP                                                              DIGZ0267
 111  FORMAT(/,1F10.3,4I10)                                             DIGZ0268
 122  FORMAT(/,4F10.3)                                                  DIGZ0269
 133  FORMAT(/,4F10.3,4I5)                                              DIGZ0270
 144  FORMAT(/,I10,2D10.2,2I10,F10.3)                                   DIGZ0271
 166  FORMAT(/,4D10.2)                                                  DIGZ0272
  98  FORMAT(/,'#############',2I1,' ----> NOT COMPUTABLE PROCESS')     DIGZ0273
C-------------------------------------------------------------          DIGZ0274
  37  CONTINUE                                                          DIGZ0275
      CALL KXLUCO(LUPAR)                                                DIGZ0276
      CALL LUTAUD(IFL)                                                  DIGZ0277
      IF (IFL.NE.0) CALL EXIT                                           DIGZ0278
      CALL KLINBK(IFL)                                                  DIGZ0279
      IF (IFL.LE.0) CALL EXIT                                           DIGZ0280
      WRITE(IUT,1000)  IPROC                                            DIGZ0281
 1000 FORMAT('0PROCESS NUMBER',I5,' HAS BEEN SELECTED'//                DIGZ0282
     .       ' NO-TAGGING'//)                                           DIGZ0283
C                                                                       DIGZ0284
C.....MASS OF BEAM PARTICLES (ELECTRON MASS)..................          DIGZ0285
      XM     = 0.511D-3/EB                                              DIGZ0286
C                                                                       DIGZ0287
C.....MASS OF PRODUCED PARTICLES..............................          DIGZ0288
      XMU    = 0.1057D0/EB                                              DIGZ0289
C                                                                       DIGZ0290
C.....MASS OF SCATTERED OR PRODUCED PARICLES..................          DIGZ0291
      XML    = 1.784D0/EB                                               DIGZ0292
C                                                                       DIGZ0293
C.....MINIMUM AND MAXIMUM ANGLE SCATTERED POSITRON                      DIGZ0294
C.....IN CASE IPROC=2 5 OR 6 THESE PARAMETERS MUST BE EQUAL TO          DIGZ0295
C.....RESPECTIVELY 0 AND 180 DEGREES                                    DIGZ0296
C.....(DUE TO SYMMETRIZATION PROCEDURE).                                DIGZ0297
      THPMIN = 0.D0                                                     DIGZ0298
      THPMAX = 180.                                                     DIGZ0299
C                                                                       DIGZ0300
C.....MINIMUM AND MAXIMUM ANGLE SCATTERED ELECTRON                      DIGZ0301
C.....IN CASE IPROC=2 OR 5 OR 6 THESE PARAMETERS MUST BE EQUAL TO       DIGZ0302
C.....RESPECTIVELY 0 AND 180 DEGREES                                    DIGZ0303
C.....(DUE TO SYMMETRIZATION PROCEDURE).                                DIGZ0304
      THMMIN = 0.D0                                                     DIGZ0305
      THMMAX = 180.                                                     DIGZ0306
C                                                                       DIGZ0307
C.....CUTS ON MUON ANGLES                                               DIGZ0308
      THMUMI = 0.D0                                                     DIGZ0309
      THMUMA = 180.                                                     DIGZ0310
C                                                                       DIGZ0311
      FACE   = 1.D+03                                                   DIGZ0312
      FACL   = 1.D+03                                                   DIGZ0313
      FACM   = 1.D+03                                                   DIGZ0314
      PROC   = 1.D+06                                                   DIGZ0315
C                                                                       DIGZ0316
      GOTO (1001,1002,1003,1004,1005,1006), IPROC                       DIGZ0317
 1001 CONTINUE                                                          DIGZ0318
C E E --> MU MU L L                                                     DIGZ0319
C THE MU MU PAIR MAY BE A QUARK ANTI QUARK PAIR WITH CHARGE QCHARG      DIGZ0320
C QM = FOUR MOMENTUM L-   MASS = XML                                    DIGZ0321
C QP = FOUR MOMENTUM L+   MASS = XML                                    DIGZ0322
C PM = FOUR MOMENTUM MU-  MASS = XMU                                    DIGZ0323
C PP = FOUR MOMENTUM MU+  MASS = XMU                                    DIGZ0324
C ONLY MCC AND MCD CONTRIBUTE                                           DIGZ0325
      WAP(1) = 0.D0                                                     DIGZ0326
      WAP(2) = 0.D0                                                     DIGZ0327
      VAP(1) = 0.D0                                                     DIGZ0328
      VAP(2) = 0.D0                                                     DIGZ0329
      VAP(3) = 1.D0                                                     DIGZ0330
      VAP(4) = 1.D0                                                     DIGZ0331
C LEPTONS      : QCHARG = 1    , ID = 1                                 DIGZ0332
C U,C,T QUARKS : QCHARG = -2/3 , ID = 2                                 DIGZ0333
C D,S,B QUARKS : QCHARG = 1/3  , ID = 3                                 DIGZ0334
C QCHARG = CHARGE CORRESPONDING TO THE LINES WITH 4MOMENTUM PM AND PP   DIGZ0335
C CONSEQUENTLY IN EE --> L L QUARK ANTI-QUARK CASE THE QUARK 4MOMENTA   DIGZ0336
C ARE PM AND PP, WHEREAS THE QM AND QP ARE THE L L 4MOMENTA. NOW THE    DIGZ0337
C XMU IS THE QUARK MASS AND XML IS THE LEPTON MASS.                     DIGZ0338
      ID     = 1                                                        DIGZ0339
      QCHARG = 1.D0                                                     DIGZ0340
      WRITE(IUT,1011)                                                   DIGZ0341
 1011 FORMAT('0',130(1H*)//                                             DIGZ0342
     .       ' MONTE CARLO SIMULATION OF THE PROCESS : ',               DIGZ0343
     .       'E+ E- ---> MU+ MU- L+ L-'//' ',130(1H*)//)                DIGZ0344
      GOTO 1007                                                         DIGZ0345
 1002 CONTINUE                                                          DIGZ0346
C E E --> MU MU MU MU                                                   DIGZ0347
C QM = FOUR MOMENTUM MU-  MASS = XMU                                    DIGZ0348
C QP = FOUR MOMENTUM MU+  MASS = XMU                                    DIGZ0349
C PM = FOUR MOMENTUM MU-  MASS = XMU                                    DIGZ0350
C PP = FOUR MOMENTUM MU+  MASS = XMU                                    DIGZ0351
C ONLY MCC AND MCD CONTRIBUTE                                           DIGZ0352
      WAP(1) = 0.D0                                                     DIGZ0353
      WAP(2) = 0.D0                                                     DIGZ0354
      VAP(1) = 0.D0                                                     DIGZ0355
      VAP(2) = 0.D0                                                     DIGZ0356
      VAP(3) = 0.5D0                                                    DIGZ0357
      VAP(4) = 0.5D0                                                    DIGZ0358
      XML    = XMU                                                      DIGZ0359
C QCHARG MAY NOT BE CHANGED                                             DIGZ0360
      ID     = 1                                                        DIGZ0361
      QCHARG = 1.D0                                                     DIGZ0362
      WRITE(IUT,1021)                                                   DIGZ0363
 1021 FORMAT('0',130(1H*)//                                             DIGZ0364
     .       ' MONTE CARLO SIMULATION OF THE PROCESS : ',               DIGZ0365
     .       'E+ E- ---> MU+ MU- MU+ MU-'//' ',130(1H*)//)              DIGZ0366
      GOTO 1007                                                         DIGZ0367
 1003 CONTINUE                                                          DIGZ0368
C E E --> E E MU MU                                                     DIGZ0369
C QM = FOUR MOMENTUM E-   MASS = XM                                     DIGZ0370
C QP = FOUR MOMENTUM E+   MASS = XM                                     DIGZ0371
C PM = FOUR MOMENTUM MU-  MASS = XMU                                    DIGZ0372
C PP = FOUR MOMENTUM MU+  MASS = XMU                                    DIGZ0373
C MCA MCB MCC AND MCD CONTRIBUTE                                        DIGZ0374
      VAP(1) = 1.D0                                                     DIGZ0375
      VAP(2) = 1.D0                                                     DIGZ0376
      VAP(3) = 1.D0                                                     DIGZ0377
      VAP(4) = 1.D0                                                     DIGZ0378
      XML    = XM                                                       DIGZ0379
C QCHARG MAY NOT BE CHANGED                                             DIGZ0380
      ID     = 1                                                        DIGZ0381
      QCHARG = 1.D0                                                     DIGZ0382
      WRITE(IUT,1031)                                                   DIGZ0383
 1031 FORMAT('0',130(1H*)//                                             DIGZ0384
     .       ' MONTE CARLO SIMULATION OF THE PROCESS : ',               DIGZ0385
     .       'E+ E- ---> E+ E- MU+ MU-'//' ',130(1H*)//)                DIGZ0386
      GOTO 1007                                                         DIGZ0387
 1004 CONTINUE                                                          DIGZ0388
C E E --> E E L  L                                                      DIGZ0389
C THE MU MU PAIR MAY BE A QUARK ANTI QUARK PAIR WITH CHARGE QCHARG      DIGZ0390
C QM = FOUR MOMENTUM E-  MASS = XM                                      DIGZ0391
C QP = FOUR MOMENTUM E+  MASS = XM                                      DIGZ0392
C PM = FOUR MOMENTUM L -  MASS = XML                                    DIGZ0393
C PP = FOUR MOMENTUM L +  MASS = XML                                    DIGZ0394
C MCA MCB MCC AND MCD CONTRIBUTE                                        DIGZ0395
      VAP(1) = 1.D0                                                     DIGZ0396
      VAP(2) = 1.D0                                                     DIGZ0397
      VAP(3) = 1.D0                                                     DIGZ0398
      VAP(4) = 1.D0                                                     DIGZ0399
      XMU    = XML                                                      DIGZ0400
      XML    = XM                                                       DIGZ0401
C LEPTONS      : QCHARG = 1                                             DIGZ0402
C U,C,T QUARKS : QCHARG = -2/3                                          DIGZ0403
C D,S,B QUARKS : QCHARG = 1/3                                           DIGZ0404
C QCHARG = CHARGE CORRESPONDING TO THE LINES WITH 4MOMENTUM PM AND PP   DIGZ0405
      ID     = 1                                                        DIGZ0406
      QCHARG = 1.D0                                                     DIGZ0407
      WRITE(IUT,1041)                                                   DIGZ0408
 1041 FORMAT('0',130(1H*)//                                             DIGZ0409
     .       ' MONTE CARLO SIMULATION OF THE PROCESS : ',               DIGZ0410
     .       'E+ E- ---> E+ E- L+ L-'//' ',130(1H*)//)                  DIGZ0411
      GOTO 1007                                                         DIGZ0412
 1005 VAP(1) = 1.D0                                                     DIGZ0413
      VAP(2) = 1.D0                                                     DIGZ0414
      VAP(3) = 0.5D0                                                    DIGZ0415
      VAP(4) = 0.5D0                                                    DIGZ0416
      XML    = XM                                                       DIGZ0417
      XMU    = XM                                                       DIGZ0418
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC               DIGZ0419
C-RM      W2MIN  = 1.D0/(EB*EB)                                         DIGZ0420
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC               DIGZ0421
C QCHARG MAY NOT BE CHANGED                                             DIGZ0422
      ID     = 1                                                        DIGZ0423
      QCHARG = 1.D0                                                     DIGZ0424
      WRITE(IUT,1051)                                                   DIGZ0425
 1051 FORMAT('0',130(1H*)//                                             DIGZ0426
     .       ' MONTE CARLO SIMULATION OF THE PROCESS : ',               DIGZ0427
     .       'E+ E- ---> E+ E- E+ E-'//' ',130(1H*)//)                  DIGZ0428
      GOTO 1007                                                         DIGZ0429
 1006 CONTINUE                                                          DIGZ0430
C E E --> L  L  L L                                                     DIGZ0431
C THE MU MU PAIR MAY BE A QUARK ANTI QUARK PAIR WITH CHARGE QCHARG      DIGZ0432
C QM = FOUR MOMENTUM L-   MASS = XML                                    DIGZ0433
C QP = FOUR MOMENTUM L+   MASS = XML                                    DIGZ0434
C PM = FOUR MOMENTUM L -  MASS = XML                                    DIGZ0435
C PP = FOUR MOMENTUM L +  MASS = XML                                    DIGZ0436
C ONLY MCC AND MCD CONTRIBUTE                                           DIGZ0437
      WAP(1) = 0.D0                                                     DIGZ0438
      WAP(2) = 0.D0                                                     DIGZ0439
      VAP(1) = 0.D0                                                     DIGZ0440
      VAP(2) = 0.D0                                                     DIGZ0441
      VAP(3) = .5D0                                                     DIGZ0442
      VAP(4) = .5D0                                                     DIGZ0443
      XMU  =  XML                                                       DIGZ0444
C LEPTONS      : QCHARG = 1    , ID = 1                                 DIGZ0445
C U,C,T QUARKS : QCHARG = -2/3 , ID = 2                                 DIGZ0446
C D,S,B QUARKS : QCHARG = 1/3  , ID = 3                                 DIGZ0447
C QCHARG = CHARGE CORRESPONDING TO THE LINES WITH 4MOMENTUM PM AND PP   DIGZ0448
C CONSEQUENTLY IN EE --> L L QUARK ANTI-QUARK CASE THE QUARK 4MOMENTA   DIGZ0449
C ARE PM AND PP, WHEREAS THE QM AND QP ARE THE L L 4MOMENTA. NOW THE    DIGZ0450
C XMU IS THE QUARK MASS AND XML IS THE LEPTON MASS.                     DIGZ0451
      ID     = 1                                                        DIGZ0452
      QCHARG = 1.D0                                                     DIGZ0453
      WRITE(IUT,1061)                                                   DIGZ0454
 1061 FORMAT('0',130(1H*)//                                             DIGZ0455
     .       ' MONTE CARLO SIMULATION OF THE PROCESS : ',               DIGZ0456
     .       'E+ E- ---> L + L - L+ L-'//' ',130(1H*)//)                DIGZ0457
      GOTO 1007                                                         DIGZ0458
 1007 CONTINUE                                                          DIGZ0459
C                                                                       DIGZ0460
C...W2MIN W2MAX FREE TO CHOOSE                                          DIGZ0461
C...W2MIN = MINIMUM (PM+PP)**2                                          DIGZ0462
      W2MIN  = 4.D0*XMU*XMU                                             DIGZ0463
      W2MAX  = 4.D0*(1.D0-XML)*(1.D0-XML)                               DIGZ0464
      XM2    = XM*XM                                                    DIGZ0465
      XMU2   = XMU*XMU                                                  DIGZ0466
      XML2   = XML*XML                                                  DIGZ0467
      QCHRG2 = QCHARG*QCHARG                                            DIGZ0468
      QCHRG3 = QCHARG*QCHRG2                                            DIGZ0469
      QCHRG4 = QCHRG2*QCHRG2                                            DIGZ0470
      WRITE(IUT,1012)  XM,XMU,XML,QCHARG,W2MIN,W2MAX,WAP,WBP,VAP        DIGZ0471
 1012 FORMAT(' ',6X,'XM     = ',D19.6/                                  DIGZ0472
     .       ' ',6X,'XMU    = ',D19.6/                                  DIGZ0473
     .       ' ',6X,'XML    = ',D19.6/                                  DIGZ0474
     .       ' ',6X,'QCHARG = ',D19.6/                                  DIGZ0475
     .       ' ',6X,'W2MIN  = ',D19.6/                                  DIGZ0476
     .       ' ',6X,'W2MAX  = ',D19.6/                                  DIGZ0477
     .       ' ',6X,'ARRAY WAP : ',4(D19.6,2X)/                         DIGZ0478
     .       ' ',6X,'ARRAY WBP : ',4(D19.6,2X)/                         DIGZ0479
     .       ' ',6X,'ARRAY VAP : ',4(D19.6,2X))                         DIGZ0480
C                                                                       DIGZ0481
      CALL START(ZMAS,ZWID,SIN2,IPROC,ITOT)                             DIGZ0482
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC                  DIGZ0483
      DUMM   = RNDM(1.)                                                 DIGZ0484
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC                  DIGZ0485
      IF (IREJEC.EQ.2) X = 1.D0                                         DIGZ0486
C                                                                       DIGZ0487
C                                                                       DIGZ0488
C  EVENT GENERATION          *********************                      DIGZ0489
C                                                                       DIGZ0490
      ELSE IF(LENTRY.EQ.2) THEN                                         DIGZ0491
C                                                                       DIGZ0492
C  EVENT STATUS (0 = O.K.)                                              DIGZ0493
       ISTA = 0                                                         DIGZ0494
C                                                                       DIGZ0495
C  Initialize the  number of tracks                                     DIGZ0496
       NTR = 4                                                          DIGZ0497
C                                                                       DIGZ0498
C  Generate primary vertex                                              DIGZ0499
       CALL RANNOR (RN1,RN2)                                            DIGZ0500
       CALL RANNOR (RN3,DUM)                                            DIGZ0501
       VRTEX(1) = RN1*SDVRT(1)                                          DIGZ0502
       VRTEX(2) = RN2*SDVRT(2)                                          DIGZ0503
       VRTEX(3) = RN3*SDVRT(3)                                          DIGZ0504
       VRTEX(4) = 0.                                                    DIGZ0505
C                                                                       DIGZ0506
   10 CONTINUE                                                          DIGZ0507
      NEVENT(1) = NEVENT(1) + 1                                         DIGZ0508
      IS     = 1                                                        DIGZ0509
      ETA0   = RNF100(12)                                               DIGZ0510
      IF (ETA0.LT.EP(2)) IS = -1                                        DIGZ0511
      IF (ETA0.LT.EP(2+IS)) GOTO 1                                      DIGZ0512
      IC     = 3 + IS                                                   DIGZ0513
      GOTO 2                                                            DIGZ0514
    1 IC     = 2 + IS                                                   DIGZ0515
    2 CONTINUE                                                          DIGZ0516
C                                                                       DIGZ0517
C     IDUMP(IC) = 0                                                     DIGZ0518
C                                                                       DIGZ0519
      IF (IC.EQ.1) THEN                                                 DIGZ0520
        CALL MCA(IPROC,IDUMP(1))                                        DIGZ0521
      ELSE IF (IC.EQ.2) THEN                                            DIGZ0522
        CALL MCB(IPROC,IDUMP(2))                                        DIGZ0523
      ELSE IF (IC.EQ.3) THEN                                            DIGZ0524
        ETA = RNF100(12)                                                DIGZ0525
        IF (ETA.LT.EA3(1)) IDEC=1                                       DIGZ0526
        IF (ETA.LT.EA3(2).AND.ETA.GT.EA3(1)) IDEC=2                     DIGZ0527
        IF (ETA.LT.EA3(3).AND.ETA.GT.EA3(2)) IDEC=3                     DIGZ0528
        IF (ETA.GT.EA3(3)) IDEC=4                                       DIGZ0529
        IF (IDEC.NE.1.AND.IDEC.NE.2.AND.                                DIGZ0530
     .    IDEC.NE.3.AND.IDEC.NE.4) WRITE(IUT,666)                       DIGZ0531
  666 FORMAT(' $$$$ERROR$$$$ IN MAIN IDEC .NE. 1,2,3,4')                DIGZ0532
        CALL MCC(IPROC,IDEC,IDUMP(3))                                   DIGZ0533
      ELSE IF (IC.EQ.4) THEN                                            DIGZ0534
        ETA = RNF100(12)                                                DIGZ0535
        IDEC=1                                                          DIGZ0536
        IF (ETA.GT.EA4) IDEC=2                                          DIGZ0537
        CALL MCD(IPROC,IDEC,IDUMP(4))                                   DIGZ0538
      ENDIF                                                             DIGZ0539
C                                                                       DIGZ0540
      IF (OUTFL(IC)) GOTO 800                                           DIGZ0541
      PMV       =  SQRT(PM(1)*PM(1)+PM(2)*PM(2)+PM(3)*PM(3))            DIGZ0542
      PPV       =  SQRT(PP(1)*PP(1)+PP(2)*PP(2)+PP(3)*PP(3))            DIGZ0543
      QMV       =  SQRT(QM(1)*QM(1)+QM(2)*QM(2)+QM(3)*QM(3))            DIGZ0544
      QPV       =  SQRT(QP(1)*QP(1)+QP(2)*QP(2)+QP(3)*QP(3))            DIGZ0545
      CSPM      =  PM(3)/PMV                                            DIGZ0546
      CSPP      =  PP(3)/PPV                                            DIGZ0547
      CSQM      =  QM(3)/QMV                                            DIGZ0548
      CSQP      = -QP(3)/QPV                                            DIGZ0549
      THQM      = DACOS(CSQM)*RADEG                                     DIGZ0550
      THQP      = DACOS(CSQP)*RADEG                                     DIGZ0551
      THPM      = DACOS(CSPM)*RADEG                                     DIGZ0552
      THPP      = DACOS(CSPP)*RADEG                                     DIGZ0553
      IF (THPM.LT.THMUMI.OR.THPM.GT.THMUMA.OR.                          DIGZ0554
     .    THPP.LT.THMUMI.OR.THPP.GT.THMUMA) GOTO 800                    DIGZ0555
      IF (THQM.LT.THMMIN.OR.THQM.GT.THMMAX.OR.                          DIGZ0556
     .    THQP.LT.THPMIN.OR.THQP.GT.THPMAX) GOTO 800                    DIGZ0557
      W2MU      = 2.D0*DOT(PM,PP)+2.D0*XMU2                             DIGZ0558
      W2MU2     = 2.D0*DOT(QM,QP)+2.D0*XMU2                             DIGZ0559
      W2MU3     = 2.D0*DOT(QM,PP)+2.D0*XMU2                             DIGZ0560
      W2MU4     = 2.D0*DOT(QP,PM)+2.D0*XMU2                             DIGZ0561
      IF (W2MU.LT.W2MIN.OR.W2MU.GT.W2MAX) GOTO 800                      DIGZ0562
      IF (IPROC.NE.2.AND.IPROC.NE.5.AND.IPROC.NE.6) GOTO 801            DIGZ0563
      IF (W2MU2.LT.W2MIN.OR.W2MU2.GT.W2MAX) GOTO 800                    DIGZ0564
      IF (W2MU3.LT.W2MIN.OR.W2MU3.GT.W2MAX) GOTO 800                    DIGZ0565
      IF (W2MU4.LT.W2MIN.OR.W2MU4.GT.W2MAX) GOTO 800                    DIGZ0566
      GOTO 801                                                          DIGZ0567
  800 CONTINUE                                                          DIGZ0568
      WEIGHT(IC) = 0.D0                                                 DIGZ0569
      WEEV       = 0.D0                                                 DIGZ0570
      FT         = 0.D0                                                 DIGZ0571
      INUL(IC)   = INUL(IC) + 1                                         DIGZ0572
      IZERO      = IZERO    + 1                                         DIGZ0573
  801 CONTINUE                                                          DIGZ0574
C                                                                       DIGZ0575
      IEVACC    = 0                                                     DIGZ0576
      IWE(IC)   = IWE(IC)  + 1                                          DIGZ0577
      IGEN      = IGEN     + 1                                          DIGZ0578
      IF (WEIGHT(IC).LE.0.D0.OR.WEIGHT(IC).GT.1.D-30) GOTO 17           DIGZ0579
      WEIGHT(IC) = 0.D0                                                 DIGZ0580
      WEEV       = 0.D0                                                 DIGZ0581
      FT         = 0.D0                                                 DIGZ0582
   17 CONTINUE                                                          DIGZ0583
      SWE(IC)   = SWE(IC)  + WEIGHT(IC)                                 DIGZ0584
      SUM       = SUM      + WEEV                                       DIGZ0585
      SWEK(IC)  = SWEK(IC) + WEIGHT(IC)*WEIGHT(IC)                      DIGZ0586
      SUMK      = SUMK     + WEEV*WEEV                                  DIGZ0587
      IF (MWE(IC).LT.WEIGHT(IC)) MWE(IC) = WEIGHT(IC)                   DIGZ0588
      IF (MAXWE.LT.WEEV) MAXWE = WEEV                                   DIGZ0589
C                                                                       DIGZ0590
C-----PRODUCTION OF EVENTS WITH WEIGHT 1-------------------------       DIGZ0591
      IF(IREJEC.EQ.2) THEN                                              DIGZ0592
      ETA1   = RNF100(11)                                               DIGZ0593
      IF (ETA1*ESWE.GT.WEIGHT(IC)) GOTO 8                               DIGZ0594
      ELSE IF (IREJEC.EQ.1) THEN                                        DIGZ0595
      X      = WEEV                                                     DIGZ0596
      ENDIF                                                             DIGZ0597
      IEVACC    = 1                                                     DIGZ0598
      IACC(IC)  = IACC(IC) + 1                                          DIGZ0599
      IONE      = IONE     + 1                                          DIGZ0600
    8 CONTINUE                                                          DIGZ0601
      CALL HISTO1(1,8HWEIGHT   ,20,0.D0,2.D0,WEEV,1.D0)                 DIGZ0602
      IF (IEVACC.EQ.0) GOTO 10                                          DIGZ0603
      IF (X.EQ.0.D0) GOTO 28                                            DIGZ0604
      IDIA  = IDIA + 1                                                  DIGZ0605
      Q1(5) = -XM                                                       DIGZ0606
      Q2(5) =  XM                                                       DIGZ0607
      Q3(5) = -XML                                                      DIGZ0608
      Q4(5) =  XML                                                      DIGZ0609
      Q5(5) = -XMU                                                      DIGZ0610
      Q6(5) =  XMU                                                      DIGZ0611
      DO 13 IV=1,4                                                      DIGZ0612
      Q1(IV) = P2(IV)                                                   DIGZ0613
      Q2(IV) = P1(IV)                                                   DIGZ0614
      Q3(IV) = QP(IV)                                                   DIGZ0615
      Q4(IV) = QM(IV)                                                   DIGZ0616
      Q5(IV) = PP(IV)                                                   DIGZ0617
      Q6(IV) = PM(IV)                                                   DIGZ0618
   13 CONTINUE                                                          DIGZ0619
      INFO  = -1                                                        DIGZ0620
C----GA & AL---                                                         DIGZ0621
C     IF (IDIA.EQ.1) INFO = 1                                           DIGZ0622
C---GA & AL---                                                          DIGZ0623
      FT    = DIAM(IPROC,INFO)                                          DIGZ0624
      FT    = X*FT                                                      DIGZ0625
   28 CONTINUE                                                          DIGZ0626
      CALL CENDEC(FT)                                                   DIGZ0627
      CALL HISTO1(2,8HFT      ,20,0.D0,2.D0,FT,1.D0)                    DIGZ0628
      SUMFT = SUMFT  + FT                                               DIGZ0629
      SUMFT2= SUMFT2 + FT*FT                                            DIGZ0630
      IF (FTMAX.LT.FT) FTMAX = FT                                       DIGZ0631
C                                                                       DIGZ0632
C REJECTION ALGORITHM WHICH PRODUCES UNWEIGHTED EVENTS                  DIGZ0633
      ETA2  = RNF100(2)                                                 DIGZ0634
      IF (ETA2*ESFT.GT.FT) GOTO 10                                      DIGZ0635
      IEEN  = IEEN + 1                                                  DIGZ0636
 200   CONTINUE                                                         DIGZ0637
       IDPR = IC+100*ITYP                                               DIGZ0638
       WEIT = 1.                                                        DIGZ0639
       ECMS = 2.*EB                                                     DIGZ0640
C                                                                       DIGZ0641
C  Now fill 'KINE' and 'VERT' banks                                     DIGZ0642
C                                                                       DIGZ0643
       TABK(4) = 0.                                                     DIGZ0644
C  book 'KINE' for beam electrons (-1 and -2)                           DIGZ0645
       DO 90 I = 1,3                                                    DIGZ0646
        TABK(I) = P2(I)*EB                                              DIGZ0647
   90  CONTINUE                                                         DIGZ0648
       JKINE = KBKINE(-1,TABK,2,0)                                      DIGZ0649
       IF(JKINE.EQ.0) THEN                                              DIGZ0650
        ISTA = 2                                                        DIGZ0651
        GO TO 97                                                        DIGZ0652
       ENDIF                                                            DIGZ0653
       DO 91 I = 1,3                                                    DIGZ0654
        TABK(I) = P1(I)*EB                                              DIGZ0655
   91  CONTINUE                                                         DIGZ0656
       TABK(4)=0.                                                       DIGZ0657
       JKINE = KBKINE(-2,TABK,3,0)                                      DIGZ0658
       IF(JKINE.EQ.0) THEN                                              DIGZ0659
        ISTA = 2                                                        DIGZ0660
        GO TO 97                                                        DIGZ0661
       ENDIF                                                            DIGZ0662
C  book 'KINE' for final state particles                                DIGZ0663
C                                                                       DIGZ0664
C Final fermions                                                        DIGZ0665
C                                                                       DIGZ0666
      DO 533 IDL = 1, 4                                                 DIGZ0667
  533    PMOM(IDL,1) = REAL(Q3(IDL)*EB)                                 DIGZ0668
      PMOM(5,1) = REAL(ABS(Q3(5))*EB)                                   DIGZ0669
      DO 534 IDL = 1, 4                                                 DIGZ0670
  534    PMOM(IDL,2) = REAL(Q4(IDL)*EB)                                 DIGZ0671
      PMOM(5,2) = REAL(ABS(Q4(5))*EB)                                   DIGZ0672
      DO 535 IDL = 1, 4                                                 DIGZ0673
  535    PMOM(IDL,3) = REAL(Q5(IDL)*EB)                                 DIGZ0674
      PMOM(5,3) = REAL(ABS(Q5(5))*EB)                                   DIGZ0675
      DO 536 IDL = 1, 4                                                 DIGZ0676
  536    PMOM(IDL,4) = REAL(Q6(IDL)*EB)                                 DIGZ0677
      PMOM(5,4) = REAL(ABS(Q6(5))*EB)                                   DIGZ0678
C                                                                       DIGZ0679
      CALL LUFILL(PMOM)                                                 DIGZ0680
      CALL KXLUAL(VRTEX,ISTA,NVRT,NTR)                                  DIGZ0681
C                                                                       DIGZ0682
   97 IF(ISTA.NE.0) THEN                                                DIGZ0683
          NEVENT(9) = NEVENT(9) + 1                                     DIGZ0684
          ICO = MIN ( 8,ABS(ISTA))                                      DIGZ0685
          NEVENT(ICO) = NEVENT(ICO) + 1                                 DIGZ0686
      ELSE                                                              DIGZ0687
        NEVENT(10) = NEVENT(10) + 1                                     DIGZ0688
        TABK(4) = QP(4)*EB                                              DIGZ0689
        CALL HFILL(10001,TABK(4),0.,WEIT)                               DIGZ0690
        TABK(4) = QM(4)*EB                                              DIGZ0691
        CALL HFILL(10002,TABK(4),0.,WEIT)                               DIGZ0692
        TABK(4) = PP(4)*EB                                              DIGZ0693
        CALL HFILL(10003,TABK(4),0.,WEIT)                               DIGZ0694
        TABK(4) = PM(4)*EB                                              DIGZ0695
        CALL HFILL(10004,TABK(4),0.,WEIT)                               DIGZ0696
        QPTHET = QP(3)/SQRT(QP(1)**2+QP(2)**2+QP(3)**2)                 DIGZ0697
        CALL HFILL(10005,QPTHET,0.,WEIT)                                DIGZ0698
        QMTHET = QM(3)/SQRT(QM(1)**2+QM(2)**2+QM(3)**2)                 DIGZ0699
        CALL HFILL(10006,QMTHET,0.,WEIT)                                DIGZ0700
        QPTHET = PP(3)/SQRT(PP(1)**2+PP(2)**2+PP(3)**2)                 DIGZ0701
        CALL HFILL(10007,QPTHET,0.,WEIT)                                DIGZ0702
        QMTHET = PM(3)/SQRT(PM(1)**2+PM(2)**2+PM(3)**2)                 DIGZ0703
        CALL HFILL(10008,QMTHET,0.,WEIT)                                DIGZ0704
        CALL HFILL(10009,WEIT,0.,1.)                                    DIGZ0705
      ENDIF                                                             DIGZ0706
C                                                                       DIGZ0707
C  END OF GENERATION         *********************                      DIGZ0708
C                                                                       DIGZ0709
      ELSE IF(LENTRY.EQ.3) THEN                                         DIGZ0710
C                                                                       DIGZ0711
C-----RESULTS----------------------------------------------------       DIGZ0712
      IF (IREJEC.EQ.1) THEN                                             DIGZ0713
      WRITE(IUT,120)                                                    DIGZ0714
  120  FORMAT('0',6X,'REQUESTED NUMBER OF EVENTS WITH WEIGHT 1 REACHED')DIGZ0715
      ENDIF                                                             DIGZ0716
      CALL FINISH(IPROC,ITOT,IREJEC)                                    DIGZ0717
       WRITE(IUT,101)                                                   DIGZ0718
  101  FORMAT(//,20X,'EVENTS STATISTICS',                               DIGZ0719
     &         /,20X,'*****************')                               DIGZ0720
       WRITE(IUT,102)NEVENT(1),NEVENT(10),NEVENT(9)                     DIGZ0721
  102  FORMAT(/,5X,'# OF GENERATED EVENTS                = ',I10,       DIGZ0722
     &        /,5X,'# OF ACCEPTED  EVENTS                = ',I10,       DIGZ0723
     &        /,5X,'# OF REJECTED  EVENTS                = ',I10)       DIGZ0724
       WRITE(IUT,103)                                                   DIGZ0725
  103  FORMAT(//,20X,'ERRORS STATISTICS',                               DIGZ0726
     &         /20X,'*****************')                                DIGZ0727
       WRITE(IUT,104)NEVENT(2),NEVENT(3),NEVENT(4),NEVENT(5),NEVENT(6), DIGZ0728
     &               NEVENT(7),NEVENT(8)                                DIGZ0729
  104  FORMAT(/,10X,'ISTA = 2 BOS ERROR VERT/KINE# OF REJECT = ',I10,   DIGZ0730
     &        /,10X,'ISTA = 3 Too many tracks    # OF REJECT = ',I10,   DIGZ0731
     &        /,10X,'ISTA = 4 Beam wrong pos     # OF REJECT = ',I10,   DIGZ0732
     &        /,10X,'ISTA = 5 Wrong status code  # OF REJECT = ',I10,   DIGZ0733
     &        /,10X,'ISTA = 6 unknown part code  # OF REJECT = ',I10,   DIGZ0734
     &        /,10X,'ISTA = 7 Bos error KHIS     # OF REJECT = ',I10,   DIGZ0735
     &        /,10X,'ISTA = 8 unused             # OF REJECT = ',I10)   DIGZ0736
      ENDIF                                                             DIGZ0737
C                                                                       DIGZ0738
      RETURN                                                            DIGZ0739
      END                                                               DIGZ0740
      SUBROUTINE USCJOB                                                 USCJOB 2
C --------------------------------------------------------------------  USCJOB 3
C                                                                       USCJOB 4
C --------------------------------------------------------------------  USCJOB 5
      LENTRY = 3                                                        USCJOB 6
      CALL DIGZ01(LENTRY)                                               USCJOB 7
      RETURN                                                            USCJOB 8
      END                                                               USCJOB 9
      SUBROUTINE LUFILL(PMOM)                                           LUFILL 2
C --------------------------------------------------------------------  LUFILL 3
C                                                                       LUFILL 4
C --------------------------------------------------------------------  LUFILL 5
      COMMON/LUJETS/  NPARLU,KODELU(2000,2),PARTLU(2000,5)              LUNDCOM2
      REAL*4 PMOM(5,4)                                                  LUFILL 7
      DO 10 IT=1,4                                                      LUFILL 8
        XM = PMOM(5,IT)                                                 LUFILL 9
        IF (XM.LT.0.01) THEN                                            LUFILL10
           IPCOD = 7                                                    LUFILL11
        ELSEIF (XM.LT.0.5) THEN                                         LUFILL12
           IPCOD = 9                                                    LUFILL13
        ELSEIF (XM.LT.2.) THEN                                          LUFILL14
           IPCOD = 11                                                   LUFILL15
        ENDIF                                                           LUFILL16
        DO 11  J=1,5                                                    LUFILL17
  11    PARTLU(IT,J)= PMOM(J,IT)                                        LUFILL18
        KODELU(IT,1)= 0                                                 LUFILL19
        KODELU(IT,2)= IPCOD*(-1)**IT                                    LUFILL20
  10  CONTINUE                                                          LUFILL21
      NPARLU = 4                                                        LUFILL22
      CALL LUEXEC                                                       LUFILL23
      RETURN                                                            LUFILL24
      END                                                               LUFILL25
       SUBROUTINE KLINBK (JKLIN)                                        KLINBK 2
C --------------------------------------------------                    KLINBK 3
C - B.Bloch - 910310                                                    KLINBK 4
C! fill 'KLIN' bank FOR fisrt 90  LUND particles                        KLINBK 5
CKEY KINE KINGAL LUND KLIN  /  USER                                     KLINBK 6
C  Get  the NOtracking marker word NOTRK from KRUN bank                 KLINBK 7
C  Fill KLIN bank with LUND particle# which correspond                  KLINBK 8
C       to GEANT particles ( or ALEPH particles)                        KLINBK 9
C  Reduce PART and KLIN banks to their normal size                      KLINBK10
C                                                                       KLINBK11
C - structure: SUBROUTINE subprogram                                    KLINBK12
C              User Entry Name: KLINBK                                  KLINBK13
C              External References: NAMIND(BOS77)                       KLINBK14
C                                   KBKLIN/AUBPRS                       KLINBK15
C                                   (ALEPHLIB)                          KLINBK16
C              Comdecks referenced: BCS,PARTJJ                          KLINBK17
C                                                                       KLINBK18
C - Usage    : CALL KLINBK (JKLIN)                                      KLINBK19
C - Output   : JKLIN  gt. 0 means OK                                    KLINBK20
      INTEGER LMHLEN,LMHCOL,LMHROW                                      BCS    2
      PARAMETER (LMHLEN=2 ,LMHCOL=1 ,LMHROW=2)                          BCS    3
      PARAMETER (LBCS=1000,LCHAR=4)                                     BCS    4
      COMMON/BCS/ IW(LBCS)                                              BCS    5
      REAL RW(LBCS)                                                     BCS    6
      EQUIVALENCE (RW(1),IW(1))                                         BCS    7
C     ILUGE (LLUGE) are the LUND numbers corresponding to the first     KLINBK22
C                   part of PART bank ( used by GEANT)                  KLINBK23
C     ILUAL (LLUAL) are the LUND numbers corresponding to the rest of   KLINBK24
C                   the PART bank                                       KLINBK25
      PARAMETER ( LLUGE=52 ,   LLUAL =315)                              KLINBK26
      INTEGER ILUGE(LLUGE),ILUAL(LLUAL)                                 KLINBK27
      EXTERNAL NAMIND                                                   KLINBK28
      CHARACTER TNAM*12                                                 KLINBK29
      DATA ILUGE /1,-7,7,0,-9,9,23,17,-17,38,                           KLINBK30
     &           18,-18,42,41,-41,37,24,57,43,44,                       KLINBK31
     &           45,46,47,70,-42,-57,-43,-44,-45,-46,                   KLINBK32
     &          -47,-70,-11,11,21,-21,20,-20,22,-22,                    KLINBK33
     &           58,3,-3,2,0,0,0,0,0,0,0,0/                             KLINBK34
      DATA ILUAL/-58, 4,83,19,-19, 8,-8,10,-10, 12,-12, 94,-94, 95, 0,  KLINBK35
     $ 0,25, 3*0,36,96,26, 27,-27, 28,-28, 29,-29,30,-30,31,-31,32,-32, KLINBK36
     $ 33,34,35, 3*0 , 87,  0, 84, 3*0,500,501,502,503,504,505,506,-501,KLINBK37
     $ -502,-503,-504,-505,-506,0,0,101,-101,102,-102,103,-103,104,-104,KLINBK38
     $123,-123,124,-124,125,-125,126,-126,105,-105,106,-106,107,-107,108KLINBK39
     $,-108,109,-109,127,-127,128,-128,129,-129,130,-130,131,-131,61,-61KLINBK40
     $,62,-62,63,-63,64,-64,65,-65,66,-66,67,-67,68,-68,69,-69,48,-48,49KLINBK41
     $,-49,50,-50,51,-51,52,-52,53,-53,59,-59,60,-60,71,-71,72,-72,73,  KLINBK42
     $-73,74,-74,75,-75,76,-76,54,-54,55,-55,56,-56,77,-77,78,-78,79,-79KLINBK43
     $,80,-80,145,-145,146,-146,147,-147,148,-148,149,-149,150,-150,151,KLINBK44
     $-151,152,-152,153,-153,154,-154,155,-155,156,-156,157,-157,158,   KLINBK45
     $-158,159,-159,160,-160,161,-161,162,-162,163,-163,164,-164,165,   KLINBK46
     $-165,166,-166,167,-167,168,-168,241,-241,242,-242,243,-243,244,   KLINBK47
     $-244,245,-245,246,-246,247,-247,248,-248,249,-249,250,-250,251,   KLINBK48
     $-251,252,-252,293,-293,294,-294,295,-295,296,-296,297,-297,298,   KLINBK49
     $-298,299,-299,300,-300,301,-301,302,-302,308,-308,309,-309,310,   KLINBK50
     $-310,311,-311,312,-312,313,-313,314,-314,315,-315,316,-316,317,   KLINBK51
     $-317,318,-318,319,-319,320,-320,321,-321,322,-322,169,-169,170,   KLINBK52
     $-170,171,-171,172,-172,173,-173,253,-253,254,-254,255,-255,256,   KLINBK53
     $-256,303,-303,304,-304,305,-305,306,-306,307,-307,-7,7,5*0/       KLINBK54
C                                                                       KLINBK55
      ITABL(ID,NR,L) = IW(ID+LMHLEN+(NR-1)*IW(ID+1)+L)                  KLINBK56
C ------------------------------------------------------                KLINBK57
C - Get NAPAR name-index of PART bank                                   KLINBK58
      NAPAR = NAMIND ('PART')                                           KLINBK59
C - Get number of columns in PART bank                                  KLINBK60
      IDPAR = IW(NAPAR)                                                 KLINBK61
C                                                                       KLINBK62
C - NOtrack marker word stored in KRUN bank                             KLINBK63
      KNOTRK = ITABL (IW(NAMIND('KRUN')),1,2)                           KLINBK64
C                                                                       KLINBK65
C - Fill KLIN with LUGEN particle# for the GEANT particles              KLINBK66
C   which are the 1st LLUGE particles of PART                           KLINBK67
C                                                                       KLINBK68
      JKLIN = 0                                                         KLINBK69
      DO 1 IPART=1,LLUGE                                                KLINBK70
         JKLIN = KBKLIN (IPART,ILUGE(IPART))                            KLINBK71
         IF (JKLIN .LE. 0) GOTO 998                                     KLINBK72
 1    CONTINUE                                                          KLINBK73
C -  extend the KLIN bank                                               KLINBK74
        DO 2 IPART = LLUGE+1,LLUAL+LLUGE                                KLINBK75
         JKLIN = KBKLIN (IPART,ILUAL(IPART-LLUGE))                      KLINBK76
         IF (JKLIN .LE. 0) GOTO 998                                     KLINBK77
  2     CONTINUE                                                        KLINBK78
C                                                                       KLINBK79
      CALL AUBPRS ('PARTKLIN')                                          KLINBK80
C                                                                       KLINBK81
      GOTO 999                                                          KLINBK82
C - not enough space                                                    KLINBK83
 998  CONTINUE                                                          KLINBK84
C - End                                                                 KLINBK85
 999  CONTINUE                                                          KLINBK86
C                                                                       KLINBK87
      END                                                               KLINBK88
