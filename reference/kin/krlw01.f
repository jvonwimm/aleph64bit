      SUBROUTINE ASKUSI(IGCOD)                                          ASKUSI 2
C--------------------------------------------------------------------   ASKUSI 3
C Initialization                 P.Perez August 1995                    ASKUSI 4
C                                B.Bloch November 1995.                 ASKUSI 5
C--------------------------------------------------------------------   ASKUSI 6
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
      COMMON / KGCOMM / ISTA,VRTEX(4),SDVRT(3),NEVENT(8),ECMI           KGCOMM 2
      COMMON / DECAYS / IFLAV(4),AMDEC(4),BRRAT(2),BREL                 KGCOMM 3
      COMMON / WGTALL / WTCRUD,WTMOD,WTSET(100)                         KGCOMM 4
      REAL *8 WTCRUD,WTMOD,WTSET,AMDEC,BRRAT,BREL                       KGCOMM 5
      PARAMETER (L1MST=200, L1PAR=200)                                  LUN7COM2
      PARAMETER (L2PAR=500, L2PARF=2000 )                               LUN7COM3
      PARAMETER (LJNPAR=4000)                                           LUN7COM4
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)   LUN7COM5
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)LUN7COM6
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),        LUN7COM7
     &                KFDP(L2PARF,5)                                    LUN7COM8
      COMMON /LUDAT4/ CHAF(L2PAR)                                       LUN7COM9
      CHARACTER*8 CHAF                                                  LUN7CO10
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5) LUN7CO11
      COMMON / INOUT / INUT,IOUT                                        INOUT  2
      COMMON / TAUBRA / GAMPRT(30),JLIST(30),NCHAN                      TAUOLAC2
      COMMON / TAUKLE / BRA1,BRK0,BRK0B,BRKS                            TAUOLAC3
      REAL*4            BRA1,BRK0,BRK0B,BRKS                            TAUOLAC4
      DIMENSION TABL(40)                                                ASKUSI12
      DOUBLE PRECISION XPAR(100)                                        ASKUSI13
      DIMENSION NPAR(100)                                               ASKUSI14
C                                                                       ASKUSI15
C Generator code (see KINLIB DOC)                                       ASKUSI16
C                                                                       ASKUSI17
      PARAMETER ( IGCO = 5033 )                                         ASKUSI18
      PARAMETER ( IVER = 101  )                                         ASKUSI19
C                                                                       ASKUSI20
      PARAMETER (LPDEC = 48)                                            ASKUSI21
      INTEGER NODEC(LPDEC)                                              ASKUSI22
      INTEGER ALTABL,ALRLEP                                             ASKUSI23
      EXTERNAL ALTABL ,ALRLEP                                           ASKUSI24
      CHARACTER TNAM*12                                                 ASKUSI25
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
C                                                                       ASKUSI27
C   Return generator code                                               ASKUSI28
C                                                                       ASKUSI29
      IGCOD= IGCO                                                       ASKUSI30
      INUT = IW(5)                                                      ASKUSI31
      IOUT = IW(6)                                                      ASKUSI32
      WRITE(IOUT,101) IGCOD ,IVER                                       ASKUSI33
  101 FORMAT(/,10X,'KRLW01 - CODE NUMBER =',I4,                         ASKUSI34
     &       /,10X,'**************************',                        ASKUSI35
     &       /,10X,' SUBVERSION  :',I10 ,                               ASKUSI36
     &       /,10X,'Last mod =  November 15 1995  ')                    ASKUSI37
C                                                                       ASKUSI38
C Input parameters for the generator (see subroutine KORALW for comment ASKUSI39
C                                                                       ASKUSI40
      CALL GLIMIT(20000)                                                ASKUSI41
      CALL GOUTPU(IOUT)                                                 ASKUSI42
      KEYRAD = 1                                                        ASKUSI43
      KEYPHY = 1011                                                     ASKUSI44
      KEYTEK = 10                                                       ASKUSI45
      KEYMIS = 0                                                        ASKUSI46
      KEYDWM = 0                                                        ASKUSI47
      KEYDWP = 0                                                        ASKUSI48
      JAK1   = 0                                                        ASKUSI49
      JAK2   = 0                                                        ASKUSI50
      ITDKRC = 1                                                        ASKUSI51
      IFPHOT = 1                                                        ASKUSI52
      IFHADM = 1                                                        ASKUSI53
      IFHADP = 1                                                        ASKUSI54
      NOUT   = IOUT                                                     ASKUSI55
C                                                                       ASKUSI56
      CMSENE = 176.                                                     ASKUSI57
      GFERMI    = 1.16639E-5                                            ASKUSI58
      ALFWIN = 128.07                                                   ASKUSI59
      AMAZ   = 91.1888                                                  ASKUSI60
      GAMMZ  = 2.4974                                                   ASKUSI61
      AMAW   = 80.23                                                    ASKUSI62
      GAMMW  = -2.03                                                    ASKUSI63
      VVMIN  = 0.000001                                                 ASKUSI64
      VVMAX  = 0.99                                                     ASKUSI65
      WTMAX  = -1.                                                      ASKUSI66
C                                                                       ASKUSI67
C                                                                       ASKUSI68
C  The default values can be changed by the DATA CARDS: GKRW & GTAU     ASKUSI69
C                                                                       ASKUSI70
      NAGKOR = NAMIND('GKRW')                                           ASKUSI71
      JGENE = IW(NAGKOR)                                                ASKUSI72
      IF(JGENE.NE.0) THEN                                               ASKUSI73
        KEYRAD = IW(JGENE+1)                                            ASKUSI74
        KEYPHY = IW(JGENE+2)                                            ASKUSI75
        KEYTEK = IW(JGENE+3)                                            ASKUSI76
        KEYMIS = IW(JGENE+4)                                            ASKUSI77
        KEYDWM = IW(JGENE+5)                                            ASKUSI78
        KEYDWP = IW(JGENE+6)                                            ASKUSI79
C                                                                       ASKUSI80
        CMSENE = RW(JGENE+7)                                            ASKUSI81
        GFERMI = RW(JGENE+8)                                            ASKUSI82
        ALFWIN = RW(JGENE+9)                                            ASKUSI83
        AMAZ   = RW(JGENE+10)                                           ASKUSI84
        GAMMZ  = RW(JGENE+11)                                           ASKUSI85
        AMAW   = RW(JGENE+12)                                           ASKUSI86
        GAMMW  = RW(JGENE+13)                                           ASKUSI87
        VVMIN  = RW(JGENE+14)                                           ASKUSI88
        VVMAX  = RW(JGENE+15)                                           ASKUSI89
        WTMAX  = RW(JGENE+16)                                           ASKUSI90
      ENDIF                                                             ASKUSI91
C                                                                       ASKUSI92
      NAGTAU = NAMIND('GTDK')                                           ASKUSI93
      JGTAU = IW(NAGTAU)                                                ASKUSI94
      IF(JGTAU.NE.0) THEN                                               ASKUSI95
        JAK1   = IW(JGTAU+1)                                            ASKUSI96
        JAK2   = IW(JGTAU+2)                                            ASKUSI97
        ITDKRC = IW(JGTAU+3)                                            ASKUSI98
        IFPHOT = IW(JGTAU+4)                                            ASKUSI99
        IFHADM = IW(JGTAU+5)                                            ASKUS100
        IFHADP = IW(JGTAU+6)                                            ASKUS101
      ENDIF                                                             ASKUS102
C                                                                       ASKUS103
C  All the parameters are stored in TABL(I)                             ASKUS104
C                                                                       ASKUS105
      TABL(1)  = KEYRAD                                                 ASKUS106
      TABL(2)  = KEYPHY                                                 ASKUS107
      TABL(3)  = KEYTEK                                                 ASKUS108
      TABL(4)  = KEYMIS                                                 ASKUS109
      TABL(5)  = KEYDWM                                                 ASKUS110
      TABL(6)  = KEYDWP                                                 ASKUS111
      TABL(7)  = IOUT                                                   ASKUS112
      TABL(11) = JAK1                                                   ASKUS113
      TABL(12) = JAK2                                                   ASKUS114
      TABL(13) = ITDKRC                                                 ASKUS115
      TABL(14) = IFPHOT                                                 ASKUS116
      TABL(15) = IFHADM                                                 ASKUS117
      TABL(16) = IFHADP                                                 ASKUS118
                                                                        ASKUS119
      TABL(21) = CMSENE                                                 ASKUS120
      TABL(22) = GFERMI                                                 ASKUS121
      TABL(23) = ALFWIN                                                 ASKUS122
      TABL(24) = AMAZ                                                   ASKUS123
      TABL(25) = GAMMZ                                                  ASKUS124
      TABL(26) = AMAW                                                   ASKUS125
      TABL(27) = GAMMW                                                  ASKUS126
      TABL(28) = VVMIN                                                  ASKUS127
      TABL(29) = VVMAX                                                  ASKUS128
      TABL(30) = WTMAX                                                  ASKUS129
C                                                                       ASKUS130
C  Main vertex initialization                                           ASKUS131
C                                                                       ASKUS132
      SDVRT(1) = 0.0185                                                 ASKUS133
      SDVRT(2) = 0.0008                                                 ASKUS134
      SDVRT(3) = 1.02                                                   ASKUS135
      NASVRT = NAMIND('SVRT')                                           ASKUS136
      JSVRT = IW(NASVRT)                                                ASKUS137
      IF(JSVRT.NE.0) THEN                                               ASKUS138
        SDVRT(1) = RW(JSVRT+1)                                          ASKUS139
        SDVRT(2) = RW(JSVRT+2)                                          ASKUS140
        SDVRT(3) = RW(JSVRT+3)                                          ASKUS141
      ENDIF                                                             ASKUS142
      TABL(31) = SDVRT(1)                                               ASKUS143
      TABL(32) = SDVRT(2)                                               ASKUS144
      TABL(33) = SDVRT(3)                                               ASKUS145
C                                                                       ASKUS146
      ECMI = CMSENE                                                     ASKUS147
C  Fill the KPAR bank with the generator parameters                     ASKUS148
C                                                                       ASKUS149
      NCOL = 33                                                         ASKUS150
      NROW = 1                                                          ASKUS151
      JKPAR = ALTABL('KPAR',NCOL,NROW,TABL,'2I,(F)','C')                ASKUS152
C                                                                       ASKUS153
C  Fill RLEP bank                                                       ASKUS154
      IEBEAM = NINT(CMSENE * 500.  )                                    ASKUS155
      JRLEP = ALRLEP(IEBEAM,'    ',0,0,0)                               ASKUS156
C                                                                       ASKUS157
C Initialization event counters                                         ASKUS158
C                                                                       ASKUS159
      DO 20 I = 1,8                                                     ASKUS160
        NEVENT(I) = 0                                                   ASKUS161
   20 CONTINUE                                                          ASKUS162
C                                                                       ASKUS163
C Initialization particle data                                          ASKUS164
C                                                                       ASKUS165
      CALL KXL74A (IPART,IKLIN)                                         ASKUS166
      IF (IPART.LE.0 .OR. IKLIN.LE.0) THEN                              ASKUS167
        WRITE (IOUT,                                                    ASKUS168
     &    '(1X,''ASKUSI :error in PART or KLIN bank - STOP - ''         ASKUS169
     &                 ,2I3)') IPART,IKLIN                              ASKUS170
        STOP                                                            ASKUS171
      ENDIF                                                             ASKUS172
C                                                                       ASKUS173
C  modify Lund masses according to input masses                         ASKUS174
      PMAS(LUCOMP(23),1)= AMAZ                                          ASKUS175
      PMAS(LUCOMP(24),1)= AMAW                                          ASKUS176
Cc      PMAS(LUCOMP(16),1)= AMNUTA                                      ASKUS177
Cc      PMAS(LUCOMP( 6),1)= AMTOP                                       ASKUS178
      PMAS(LUCOMP( 7),1)= 150.                                          ASKUS179
      PMAS(LUCOMP( 8),1)= 300.                                          ASKUS180
      IA1=20213                                  !jetset7.3 code for a1 ASKUS181
      PMAS(LUCOMP(IA1),1)= 1.251                                        ASKUS182
      PMAS(LUCOMP(IA1),2)= 0.599                                        ASKUS183
C                                                                       ASKUS184
C   Make sure that masses and width in PART bank are consistent         ASKUS185
C function KGPART returns the ALEPH code corresponding to the LUND code ASKUS186
C required.                                                             ASKUS187
C Z0(lund code=23) top (lund code=6)  Higgs (lund code=25)              ASKUS188
C a1(lund code=20213)                                                   ASKUS189
      NAPAR = NAMIND('PART')                                            ASKUS190
      JPART = IW(NAPAR)                                                 ASKUS191
      IZPART = KGPART(23)                                               ASKUS192
      IF (IZPART.GT.0)  THEN                                            ASKUS193
        ZMAS = PMAS(LUCOMP(23),1)                                       ASKUS194
        KPART = KROW(JPART,IZPART)                                      ASKUS195
        RW(KPART+6)=ZMAS                                                ASKUS196
        IANTI = ITABL(JPART,IZPART,10)                                  ASKUS197
        IF (IANTI.NE.IZPART) THEN                                       ASKUS198
          KAPAR = KROW(JPART,IANTI)                                     ASKUS199
          RW(KAPAR+6)=ZMAS                                              ASKUS200
        ENDIF                                                           ASKUS201
      ENDIF                                                             ASKUS202
      ITPART = KGPART(6)                                                ASKUS203
      IF (ITPART.GT.0)  THEN                                            ASKUS204
        ZMAS = PMAS(LUCOMP( 6),1)                                       ASKUS205
        KPART = KROW(JPART,ITPART)                                      ASKUS206
        RW(KPART+6)=ZMAS                                                ASKUS207
        IANTI = ITABL(JPART,ITPART,10)                                  ASKUS208
        IF (IANTI.NE.ITPART) THEN                                       ASKUS209
          KAPAR = KROW(JPART,IANTI)                                     ASKUS210
          RW(KAPAR+6)=ZMAS                                              ASKUS211
        ENDIF                                                           ASKUS212
      ENDIF                                                             ASKUS213
      IHPART = KGPART(25)                                               ASKUS214
      IF (IHPART.GT.0)  THEN                                            ASKUS215
        ZMAS = PMAS(LUCOMP(25),1)                                       ASKUS216
        KPART = KROW(JPART,IHPART)                                      ASKUS217
        RW(KPART+6)=ZMAS                                                ASKUS218
        IANTI = ITABL(JPART,IHPART,10)                                  ASKUS219
        IF (IANTI.NE.IHPART) THEN                                       ASKUS220
          KAPAR = KROW(JPART,IANTI)                                     ASKUS221
          RW(KAPAR+6)=ZMAS                                              ASKUS222
        ENDIF                                                           ASKUS223
      ENDIF                                                             ASKUS224
                                                                        ASKUS225
      IHPART = KGPART(20213)                                            ASKUS226
      IF (IHPART.GT.0)  THEN                                            ASKUS227
        ZMAS = PMAS(LUCOMP(20213),1)                                    ASKUS228
        ZWID = PMAS(LUCOMP(20213),2)                                    ASKUS229
        KPART = KROW(JPART,IHPART)                                      ASKUS230
        RW(KPART+6)=ZMAS                                                ASKUS231
        RW(KPART+9)=ZWID                                                ASKUS232
        IANTI = ITABL(JPART,IHPART,10)                                  ASKUS233
        IF (IANTI.NE.IHPART) THEN                                       ASKUS234
          KAPAR = KROW(JPART,IANTI)                                     ASKUS235
          RW(KAPAR+6)=ZMAS                                              ASKUS236
          RW(KAPAR+9)=ZWID                                              ASKUS237
        ENDIF                                                           ASKUS238
      ENDIF                                                             ASKUS239
C                                                                       ASKUS240
C                                                                       ASKUS241
C   Inhibit decays                                                      ASKUS242
C                                                                       ASKUS243
      MXDEC=KNODEC(NODEC,LPDEC)                                         ASKUS244
      MXDEC=MIN(MXDEC,LPDEC)                                            ASKUS245
      IF (MXDEC.GT.0) THEN                                              ASKUS246
        DO 50 I=1,MXDEC                                                 ASKUS247
          IF (NODEC(I).GT.0) THEN                                       ASKUS248
            JIDB = NLINK('MDC1',NODEC(I))                               ASKUS249
            IF (JIDB .EQ. 0) MDCY(LUCOMP(NODEC(I)),1) = 0               ASKUS250
          ENDIF                                                         ASKUS251
   50   CONTINUE                                                        ASKUS252
      ENDIF                                                             ASKUS253
C                                                                       ASKUS254
C  Generator initialization                                             ASKUS255
      NPAR(1)=KEYRAD ! KeyRad =1000*KeyCul+100*KeyNLL+10*KeyFSR+KeyISR  ASKUS256
      NPAR(2)=KEYPHY ! KeyPhy =10000*KeyRed+1000*KeySpn+100*KeyZet+10*KeASKUS257
C                    !         KeyBr                                    ASKUS258
      NPAR(3)=KEYTEK ! KeyTek =10*KeyRnd+KeyWgt                         ASKUS259
      NPAR(4)=KEYMIS ! KeyMis =  KeyMix                                 ASKUS260
      NPAR(5)=KEYDWM ! KEYDWM    W- decay: 7=(ev), 0=all ch.            ASKUS261
      NPAR(6)=KEYDWP ! KEYDWP    W+ decay: 7=(ev), 0=all ch.            ASKUS262
      NPAR(7)=NOUT   ! NOUT      Output unit number, for Nout.LE.0, NoutASKUS263
C                                                                       ASKUS264
      NPAR(21)=JAK1   ! JAK1      Decay mode tau+                       ASKUS265
      NPAR(22)=JAK2   ! JAK2      Decay mode tau-                       ASKUS266
      NPAR(23)=ITDKRC ! ITDKRC    Bremsstrahlung switch in Tauola       ASKUS267
      NPAR(24)=IFPHOT ! IFPHOT    PHOTOS switch                         ASKUS268
      NPAR(25)=IFHADM ! IFHADM    Hadronisation W-                      ASKUS269
      NPAR(26)=IFHADP ! IFHADP    Hadronisation W+IFHADP                ASKUS270
                                                                        ASKUS271
      XPAR(1)=CMSENE  ! CMSENE CMS total energy                         ASKUS272
      XPAR(2)=GFERMI  ! GFERMI Fermi Constant                           ASKUS273
      XPAR(3)=ALFWIN  ! ALFWIN alpha QED at WW tresh. scale (inverse)   ASKUS274
      XPAR(4)=AMAZ    ! AMAZ   Z mass                                   ASKUS275
      XPAR(5)=GAMMZ   ! GAMMZ  Z width                                  ASKUS276
      XPAR(6)=AMAW    ! AMAW   W mass                                   ASKUS277
      XPAR(7)=GAMMW   ! GAMMW  W with, For GAMMW<0 RECALCULATED inside pASKUS278
      XPAR(8)=VVMIN   ! VVMIN  Photon spectrum parameter                ASKUS279
      XPAR(9)=VVMAX   ! VVMAX  Photon spectrum parameter                ASKUS280
      XPAR(10)=WTMAX  ! WTMAX  max weight for reject.                   ASKUS281
C                     !        WTMAX<0 = default setting                ASKUS282
C                                                                       ASKUS283
      LENTRY = -1                                                       ASKUS284
      CALL KORALW(LENTRY,XPAR,NPAR)                                     ASKUS285
C                                                                       ASKUS286
C  Print PART and KPAR banks                                            ASKUS287
C                                                                       ASKUS288
C     CALL LULIST(12)                                                   ASKUS289
C     CALL PRPART                                                       ASKUS290
      CALL PRTABL('RLEP',0)                                             ASKUS291
      CALL PRTABL('KPAR',0)                                             ASKUS292
C                                                                       ASKUS293
C                                                                       ASKUS294
C    possibly update branching ratios  with card GKBR                   ASKUS295
C                                                                       ASKUS296
      NAGKBR = NAMIND('GKBR')                                           ASKUS297
      JGKBR = IW(NAGKBR)                                                ASKUS298
      IF(JGKBR.NE.0) THEN                                               ASKUS299
C check consitency of length                                            ASKUS300
        NLEN = IW(JGKBR)                                                ASKUS301
        IF ( NLEN .NE.NCHAN+4 ) THEN                                    ASKUS302
            WRITE (IW(6),'(1X,'' Inconsistent number of Brs should be'',ASKUS303
     $                    I5,'' is '',I5)') NCHAN,NLEN-4                ASKUS304
            CALL EXIT                                                   ASKUS305
        ENDIF                                                           ASKUS306
        BRA1   = RW(JGKBR+1)                                            ASKUS307
        BRK0   = RW(JGKBR+2)                                            ASKUS308
        BRK0B  = RW(JGKBR+3)                                            ASKUS309
        BRKS   = RW(JGKBR+4)                                            ASKUS310
        DO 51 I = 1,NCHAN                                               ASKUS311
           GAMPRT(I) = RW(JGKBR+4+I)                                    ASKUS312
 51     CONTINUE                                                        ASKUS313
        IF ( GAMPRT(1).NE.1.) THEN                                      ASKUS314
         DO 52 I = 1, NCHAN                                             ASKUS315
           GAMPRT(I) = GAMPRT(I)/GAMPRT(1)                              ASKUS316
 52      CONTINUE                                                       ASKUS317
        ENDIF                                                           ASKUS318
      ENDIF                                                             ASKUS319
C                                                                       ASKUS320
C   Store the version used in the job and the branching ratios in       ASKUS321
C   header bank  KORL                                                   ASKUS322
      NCOL = NCHAN+5                                                    ASKUS323
      NROW = 1                                                          ASKUS324
      TABL(1) = IVER                                                    ASKUS325
      TABL(2) = BRA1                                                    ASKUS326
      TABL(3) = BRK0                                                    ASKUS327
      TABL(4) = BRK0B                                                   ASKUS328
      TABL(5) = BRKS                                                    ASKUS329
      DO 57 IBR = 1,NCHAN                                               ASKUS330
          TABL(5+IBR) = GAMPRT(IBR)                                     ASKUS331
 57   CONTINUE                                                          ASKUS332
      JKORL = ALTABL('KORL',NCOL,NROW,TABL,'2I,(F)','C')                ASKUS333
      CALL PRTABL('KORL',0)                                             ASKUS334
      RETURN                                                            ASKUS335
      END                                                               ASKUS336
      SUBROUTINE ASKUSE (IDP,IST,NTRK,NVRT,ECM,WEI)                     ASKUSE 2
C --------------------------------------------------------------------  ASKUSE 3
C Generation                     P.Perez August 1995.                   ASKUSE 4
C                                B.Bloch November 1995                  ASKUSE 5
C --------------------------------------------------------------------  ASKUSE 6
C--------------------------------------------------------------------   ASKUSE 7
C     input     : none                                                  ASKUSE 8
C                                                                       ASKUSE 9
C     output    : 6 arguments                                           ASKUSE10
C          IDP    : process identification                              ASKUSE11
C          IST    : status flag ( 0 means ok)                           ASKUSE12
C          NTRK   : number of tracks generated and kept                 ASKUSE13
C          NVRT   : number of vertices generated                        ASKUSE14
C          ECM    : center of mass energy for the event                 ASKUSE15
C          WEI    : event weight always equal to 1                      ASKUSE16
C--------------------------------------------------------------------   ASKUSE17
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
      COMMON / KGCOMM / ISTA,VRTEX(4),SDVRT(3),NEVENT(8),ECMI           KGCOMM 2
      COMMON / DECAYS / IFLAV(4),AMDEC(4),BRRAT(2),BREL                 KGCOMM 3
      COMMON / WGTALL / WTCRUD,WTMOD,WTSET(100)                         KGCOMM 4
      REAL *8 WTCRUD,WTMOD,WTSET,AMDEC,BRRAT,BREL                       KGCOMM 5
      PARAMETER (L1MST=200, L1PAR=200)                                  LUN7COM2
      PARAMETER (L2PAR=500, L2PARF=2000 )                               LUN7COM3
      PARAMETER (LJNPAR=4000)                                           LUN7COM4
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)   LUN7COM5
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)LUN7COM6
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),        LUN7COM7
     &                KFDP(L2PARF,5)                                    LUN7COM8
      COMMON /LUDAT4/ CHAF(L2PAR)                                       LUN7COM9
      CHARACTER*8 CHAF                                                  LUN7CO10
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5) LUN7CO11
      DOUBLE PRECISION XPAR(100)                                        ASKUSE21
      DIMENSION NPAR(100)                                               ASKUSE22
      DIMENSION E1(3),E2(3)                                             ASKUSE23
C                                                                       ASKUSE24
      IST  = 0                                                          ASKUSE25
      IDP  = 0                                                          ASKUSE26
      ECM  = 0.                                                         ASKUSE27
      WEI  = 0.                                                         ASKUSE28
C                                                                       ASKUSE29
C  Generate primary vertex                                              ASKUSE30
C                                                                       ASKUSE31
      CALL RANNOR (RN1,RN2)                                             ASKUSE32
      CALL RANNOR (RN3,DUM)                                             ASKUSE33
      VRTEX(1) = RN1*SDVRT(1)                                           ASKUSE34
      VRTEX(2) = RN2*SDVRT(2)                                           ASKUSE35
      VRTEX(3) = RN3*SDVRT(3)                                           ASKUSE36
      VRTEX(4) = 0.                                                     ASKUSE37
C                                                                       ASKUSE38
C  Event generation                                                     ASKUSE39
C                                                                       ASKUSE40
      NEVENT(1) = NEVENT(1) + 1                                         ASKUSE41
      CALL KORALW(0,XPAR,NPAR)                                          ASKUSE42
      IDP  = abs(IFLAV(1))+100*abs(IFLAV(2))+10000*abs(IFLAV(3))        ASKUSE43
     $        +1000000*abs(IFLAV(4))                                    ASKUSE44
      ECM  = ECMI                                                       ASKUSE45
      WEI  = WTMOD                                                      ASKUSE46
      IF(IST.NE.0) THEN                                                 ASKUSE47
        NEVENT(4) = NEVENT(4) + 1                                       ASKUSE48
        GO TO 20                                                        ASKUSE49
      ENDIF                                                             ASKUSE50
C  decay remaining pi0's                                                ASKUSE51
      CALL LUEXEC                                                       ASKUSE52
C  Book all banks                                                       ASKUSE53
C                                                                       ASKUSE54
      CALL KXL7AL(VRTEX,ISTA,NVRT,NTRK)                                 ASKUSE55
      IST = ISTA                                                        ASKUSE56
      IF(IST.NE.0) THEN                                                 ASKUSE57
        NEVENT(5) = NEVENT(5) + 1                                       ASKUSE58
        GO TO 20                                                        ASKUSE59
      ENDIF                                                             ASKUSE60
C                                                                       ASKUSE61
C  Event counters                                                       ASKUSE62
C                                                                       ASKUSE63
      IF(IST.EQ.0) THEN                                                 ASKUSE64
        NEVENT(2) = NEVENT(2) + 1                                       ASKUSE65
        DO 10 IP = 1,N7LU                                               ASKUSE66
          IF(K7LU(IP,2).EQ.22) THEN                                     ASKUSE67
            NEVENT(8) = NEVENT(8) + 1                                   ASKUSE68
            GO TO 30                                                    ASKUSE69
          ENDIF                                                         ASKUSE70
   10   CONTINUE                                                        ASKUSE71
        NEVENT(7) = NEVENT(7) + 1                                       ASKUSE72
      ENDIF                                                             ASKUSE73
   20 IF(IST.NE.0) NEVENT(3) = NEVENT(3) + 1                            ASKUSE74
C                                                                       ASKUSE75
   30 RETURN                                                            ASKUSE76
      END                                                               ASKUSE77
      SUBROUTINE USCJOB                                                 USCJOB 2
C --------------------------------------------------------------------  USCJOB 3
C End of generation              P.Perez August 1995.                   USCJOB 4
C --------------------------------------------------------------------  USCJOB 5
      COMMON / INOUT / INUT,IOUT                                        INOUT  2
      COMMON / KGCOMM / ISTA,VRTEX(4),SDVRT(3),NEVENT(8),ECMI           KGCOMM 2
      COMMON / DECAYS / IFLAV(4),AMDEC(4),BRRAT(2),BREL                 KGCOMM 3
      COMMON / WGTALL / WTCRUD,WTMOD,WTSET(100)                         KGCOMM 4
      REAL *8 WTCRUD,WTMOD,WTSET,AMDEC,BRRAT,BREL                       KGCOMM 5
      DOUBLE PRECISION XPAR(100)                                        USCJOB 8
      DIMENSION NPAR(100)                                               USCJOB 9
C                                                                       USCJOB10
C End of generation                                                     USCJOB11
C                                                                       USCJOB12
      LENTRY = 1                                                        USCJOB13
      CALL KORALW(LENTRY,XPAR,NPAR)                                     USCJOB14
C                                                                       USCJOB15
C Print event counters                                                  USCJOB16
C                                                                       USCJOB17
       WRITE(IOUT,101)                                                  USCJOB18
  101  FORMAT(//20X,'EVENTS STATISTICS',                                USCJOB19
     &         /20X,'*****************')                                USCJOB20
       WRITE(IOUT,102) NEVENT(1),NEVENT(2),NEVENT(3),                   USCJOB21
     &                 NEVENT(7),NEVENT(8)                              USCJOB22
  102  FORMAT(/5X,'# OF GENERATED EVENTS                      = ',I10,  USCJOB23
     &        /5X,'# OF ACCEPTED  EVENTS                      = ',I10,  USCJOB24
     &        /5X,'# OF REJECTED  EVENTS (ISTA # 0 in ASKUSE) = ',I10,  USCJOB25
     &        /5X,'# OF EVENTS WITHOUT PHOTON                 = ',I10,  USCJOB26
     &        /5X,'# OF EVENTS WITH PHOTON                    = ',I10)  USCJOB27
       WRITE(IOUT,103)                                                  USCJOB28
  103  FORMAT(//20X,'ERRORS STATISTICS',                                USCJOB29
     &         /20X,'*****************')                                USCJOB30
       WRITE(IOUT,104) NEVENT(4),NEVENT(5)                              USCJOB31
  104  FORMAT(/10X,'ISTA # 0 FROM KORW01        # OF REJECT = ',I10,    USCJOB32
     &        /10X,'ISTA # 0 FROM KXL7AL        # OF REJECT = ',I10)    USCJOB33
C                                                                       USCJOB34
      RETURN                                                            USCJOB35
      END                                                               USCJOB36
