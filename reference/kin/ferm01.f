      SUBROUTINE ASKUSI(ICODE)                                          ASKUSI 2
C-----------------------------------------------------------------------ASKUSI 3
C!   Init routine for fermisv P. Janot October 94                       ASKUSI 4
C                                                                       ASKUSI 5
C-----------------------------------------------------------------------ASKUSI 6
C                                                                       ASKUSI 8
      PARAMETER(lpdec=48)                                               ASKUSI 9
      PARAMETER(igcod=5027)                                             ASKUSI10
      INTEGER nodec(lpdec)                                              ASKUSI11
      INTEGER altabl,namind,alrlep                                      ASKUSI12
      EXTERNAL altabl,namind,alrlep                                     ASKUSI13
      CHARACTER*2 alf(11)                                               ASKUSI14
      CHARACTER*4 chaint                                                ASKUSI15
      INTEGER charg(11)                                                 ASKUSI16
      DATA  ALF/'EL','NE','MU','NM','TO','NT','DQ','UQ','SQ','CQ','BQ'/ ASKUSI17
      DATA CHARG/-3 ,  0 , -3 ,  0 , -3 ,  0 , -1 , +2 , -1 , +2 , -1 / ASKUSI18
C The quark and lepton masses                                           QMASSES2
      REAL*8 amu, amd, ams, amc, amb, amt                               QMASSES3
      REAL*8 amel, ammu, amto, amne, amnm, amnt                         QMASSES4
      PARAMETER ( amu = .005D0, amd = .010d0, ams = .150d0 )            QMASSES5
      PARAMETER ( amc = 1.37d0, amb = 4.70d0, amt = 170.d0 )            QMASSES6
      PARAMETER ( amel= .511D-3, ammu= .1057D0, amto = 1.777d0 )        QMASSES7
      PARAMETER ( amne= 1D-4, amnm= 1D-4, amnt = 1D-4 )                 QMASSES8
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C BRANCH commons                                                        BRANCHC2
      INTEGER LCHMX,PAR1,PAR2,PAR3,PAR4,PAR5,PAR6,NEVTGN,NRJGN,         BRANCHC3
     &   NCHAN,NCHANT,GENRJC,LGENR,JCHAN,CHACOU,CHANLS,                 BRANCHC4
     &   LANNIH,LBREMF,LBREMB,LCONV1,LCONV2,LCON1Z,LCON2Z,LMULTI,LRAMBO BRANCHC5
      PARAMETER (LCHMX = 100, LGENR = 9, LANNIH = 1, LBREMF = 2,        BRANCHC6
     &   LBREMB = 3, LCONV1 = 4, LCONV2 = 5,  LCON1Z = 6, LCON2Z = 7,   BRANCHC7
     &   LMULTI = 8, LRAMBO =9)                                         BRANCHC8
      REAL*8 SMINP,SMAXP,SMX3,SKMIN,SKMAX,BRNCH,FRAC,CGEN               BRANCHC9
      COMMON /BRANCH/ PAR1(LCHMX),PAR2(LCHMX),PAR3(LCHMX),PAR4(LCHMX),  BRANCH10
     &   PAR5(LCHMX),PAR6(LCHMX),NEVTGN(LGENR),NRJGN(LGENR),NCHAN,      BRANCH11
     &   NCHANT,GENRJC(LCHMX),JCHAN(LBUNCH),CHACOU(0:LGENR),            BRANCH12
     &   CHANLS(LGENR),                                                 BRANCH13
     &   SKMIN,SKMAX,SMINP(LCHMX),SMAXP(LCHMX),SMX3(LCHMX),             BRANCH14
     &   BRNCH(0:LCHMX),FRAC(LCHMX),CGEN(LGENR)                         BRANCH15
      CHARACTER*16 GNNAME(LGENR)                                        BRANCH16
      COMMON /GNNAMS/ GNNAME                                            BRANCH17
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
C setup commons                                                         SETUPCO2
      REAL*8 ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                          SETUPCO3
      COMMON /SETUP/ ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                  SETUPCO4
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C Event writing block                                                   WRITING2
      INTEGER IWRITE,CODE,NWRITN,NWRIMX                                 WRITING3
      REAL*8 XMAXWT                                                     WRITING4
      COMMON /EWRITE/ XMAXWT,IWRITE,CODE(11),NWRITN,NWRIMX              WRITING5
C Some physics constants                                                CONSTPH2
      REAL*8 ALPHA,RMZ,RGZ,SW2,RMZ2,RMZRGZ,RMZGZ2                       CONSTPH3
      COMMON / FISIKX / ALPHA,RMZ,RGZ,SW2,RMZ2,RMZRGZ,RMZGZ2            CONSTPH4
      REAL*8 XME,XM34,XM56,THETCN                                       PROSECO2
      REAL*4 SCGEN                                                      PROSECO3
      INTEGER IISR,IFSR,IAUTOW,CH334,CH356,QCDFLG                       PROSECO4
      CHARACTER*4 CDIAGS                                                PROSECO5
      COMMON / copro1 / xme,xm34,xm56,thetcn                            PROSECO6
      COMMON / copro2 / iisr,ifsr,iautow,ch334,ch356,qcdflg             PROSECO7
      COMMON / copro3 / cdiags                                          PROSECO8
      COMMON / copro4 / scgen(lgenr)                                    PROSECO9
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      COMMON / BCS / IW(1000)                                           BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(1000)                                                     BCS    7
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
      COMMON / miscl / loutbe,ecms,idb1,idb2,irad,ibeams,               MISCL  2
     &                 sdvrt(3),vrtx(4),tabl(26),nnvent(11)             MISCL  3
      INTEGER loutbe,idb1,idb2,nnvent                                   MISCL  4
      INTEGER kabl(26)                                                  MISCL  5
      REAL*4 sdvrt,vrtx,tabl                                            MISCL  6
      EQUIVALENCE(tabl(1),kabl(1))                                      MISCL  7
C TAUOLA commons                                                        TAUCOM 2
      COMMON / jaki / jak1,jak2,jakp,jakm,ktom                          TAUCOM 3
      COMMON / idfc / idff                                              TAUCOM 4
      COMMON /taurad/ xk0dec,itdkrc                                     TAUCOM 5
      REAL*8 xk0dec                                                     TAUCOM 6
      COMMON /qedprm/ alfinv,alfpi,xk0                                  TAUCOM 7
      REAL*8 alfinv,alfpi,xk0                                           TAUCOM 8
      COMMON / inout/ inut,iout                                         TAUCOM 9
      COMMON /decpar/ gfermi,gv,ga,ccabib,scabib,gamel                  TAUCOM10
      COMMON /parmas/ amtau_k,amnuta_k,amel_k,amnue_k,ammu_k,           TAUCOM11
     *                amnumu_k,ampiz_k,ampi_k,amro_k,gamro_k,           TAUCOM12
     *                ama1_k,gama1_k,amk_k,amkz_k,amkst_k,gamkst_k      TAUCOM13
C                                                                       TAUCOM14
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
C                                                                       ASKUSI33
      loutbe = IW(6)                                                    ASKUSI34
C                                                                       ASKUSI35
C...Set generator code                                                  ASKUSI36
C                                                                       ASKUSI37
      icode = igcod                                                     ASKUSI38
      WRITE (loutbe,'(30X,''W E L C O M E   T O   F E R M 0 1 '',/,     ASKUSI39
     $                30X,''**********************************'',/,     ASKUSI40
     $                30X,'' Generator code  is  # '',I10       ,/,30x, ASKUSI41
     $                 '' last modification '',                         ASKUSI42
     $                 ''September 19, 1995''                           BB950911
     $                 ,/)') ICODE                                      ASKUSI44
C                                                                       ASKUSI45
C...Create the KLIN bank and complete the PART bank                     ASKUSI46
C                                                                       ASKUSI47
      CALL kxl74a(ipart,iklin)                                          ASKUSI48
      IF( ipart .LE . 0 .OR. iklin .LE. 0 ) THEN                        ASKUSI49
        WRITE(loutbe,1000) ipart,iklin                                  ASKUSI50
 1000   FORMAT(1X,'+++ASKUSI+++ ERROR FILLING PART OR KLIN---STOP'      ASKUSI51
     $  ,2I5)                                                           ASKUSI52
        call exit                                                       ASKUSI53
      ENDIF                                                             ASKUSI54
C                                                                       ASKUSI55
C Read data cards and initialize Hilgart's program                      ASKUSI56
C                                                                       ASKUSI57
      nagene = NAMIND('GENE')                                           ASKUSI58
      igene = IW(nagene)                                                ASKUSI59
      IF ( igene .GT. 0 ) THEN                                          ASKUSI60
        fla(1) = 'EL'                                                   ASKUSI61
        fla(2) = 'EL'                                                   ASKUSI62
        fla(3) = alf(IW(igene+ 1))                                      ASKUSI63
        fla(4) = fla(3)                                                 ASKUSI64
        fla(5) = alf(IW(igene+ 2))                                      ASKUSI65
        fla(6) = fla(5)                                                 ASKUSI66
        ch334  = charg(IW(igene+ 1))                                    ASKUSI67
        ch356  = charg(IW(igene+ 2))                                    ASKUSI68
        xme    = RW(igene+ 3)                                           ASKUSI69
        xm34   = RW(igene+ 4)                                           ASKUSI70
        xm56   = RW(igene+ 5)                                           ASKUSI71
        xmin34 = RW(igene+ 6)                                           ASKUSI72
        xmin56 = RW(igene+ 7)                                           ASKUSI73
        xmax34 = RW(igene+ 8)                                           ASKUSI74
        xmax56 = RW(igene+ 9)                                           ASKUSI75
        xmom34 = RW(igene+10)                                           ASKUSI76
        xmom56 = RW(igene+11)                                           ASKUSI77
        thetcn = RW(igene+12)                                           ASKUSI78
        ecm    = RW(igene+13)                                           ASKUSI79
        rmz    = RW(igene+14)                                           ASKUSI80
        rgz    = RW(igene+15)                                           ASKUSI81
        sw2    = RW(igene+16)                                           ASKUSI82
        iisr   = IW(igene+17)                                           ASKUSI83
        kgmax  = RW(igene+18)                                           ASKUSI84
        ifsr   = IW(igene+19)                                           ASKUSI85
        nofsr  = IW(igene+20)                                           ASKUSI86
        iwrite = IW(igene+21)                                           ASKUSI87
        xmaxwt = RW(igene+22)                                           ASKUSI88
        qcdflg = IW(igene+23)                                           ASKUSI89
        debglv = IW(igene+24)                                           ASKUSI90
        nevpri = IW(igene+25)                                           ASKUSI91
      ELSE                                                              ASKUSI92
        fla(1) = 'EL'                                                   ASKUSI93
        fla(2) = 'EL'                                                   ASKUSI94
        fla(3) = 'MU'                                                   ASKUSI95
        fla(4) = 'MU'                                                   ASKUSI96
        fla(5) = 'MU'                                                   ASKUSI97
        fla(6) = 'MU'                                                   ASKUSI98
        ch334  = -3                                                     ASKUSI99
        ch356  = -3                                                     ASKUS100
        xm34   = ammu                                                   ASKUS101
        xm56   = ammu                                                   ASKUS102
        xmin34 = 0D0                                                    ASKUS103
        xmin56 = 0D0                                                    ASKUS104
        xmax34 = 0D0                                                    ASKUS105
        xmax56 = 0D0                                                    ASKUS106
        xmom34 = 0D0                                                    ASKUS107
        xmom56 = 0D0                                                    ASKUS108
        thetcn = 0D0                                                    ASKUS109
        ecm    = 91.2D0                                                 ASKUS110
        rmz    = 91.1890D0                                              ASKUS111
        grz    = 2.497D0                                                ASKUS112
        sw2    = 0.2317                                                 ASKUS113
        iisr   = 1                                                      ASKUS114
        kgmax  = 0.80D0                                                 ASKUS115
        ifsr   = 1                                                      ASKUS116
        nofsr  = 4                                                      ASKUS117
        iwrite = 0                                                      ASKUS118
        xmaxwt = 0D0                                                    ASKUS119
        qcdflg = 1                                                      ASKUS120
        debglv = 1                                                      ASKUS121
        nevpri = 6                                                      ASKUS122
      ENDIF                                                             ASKUS123
      ecms = ecm                                                        ASKUS124
C                                                                       ASKUS125
      KABL( 1) = INTCHA(fla(3)//fla(4))                                 ASKUS126
      KABL( 2) = INTCHA(fla(5)//fla(6))                                 ASKUS127
      TABL( 3) = xme                                                    ASKUS128
      TABL( 4) = xm34                                                   ASKUS129
      TABL( 5) = xm56                                                   ASKUS130
      TABL( 6) = xmin34                                                 ASKUS131
      TABL( 7) = xmin56                                                 ASKUS132
      TABL( 8) = xmax34                                                 ASKUS133
      TABL( 9) = xmax56                                                 ASKUS134
      TABL(10) = xmom34                                                 ASKUS135
      TABL(11) = xmom56                                                 ASKUS136
      TABL(12) = thetcn                                                 ASKUS137
      TABL(13) = ecm                                                    ASKUS138
      TABL(14) = rmz                                                    ASKUS139
      TABL(15) = rgz                                                    ASKUS140
      TABL(16) = sw2                                                    ASKUS141
      KABL(17) = iisr                                                   ASKUS142
      TABL(18) = kgmax                                                  ASKUS143
      KABL(19) = ifsr                                                   ASKUS144
      KABL(20) = nofsr                                                  ASKUS145
      KABL(21) = iwrite                                                 ASKUS146
      TABL(22) = xmaxwt                                                 ASKUS147
      KABL(23) = qcdflg                                                 ASKUS148
C                                                                       ASKUS149
      nadiag = NAMIND('GDIA')                                           ASKUS150
      idiag = IW(nadiag)                                                ASKUS151
      IF ( idiag .GT. 0 ) THEN                                          ASKUS152
        iautow = IW(idiag+ 1)                                           ASKUS153
        DO i = 1, lgenr                                                 ASKUS154
          scgen(i) = RW(idiag+i+1)                                      ASKUS155
        ENDDO                                                           ASKUS156
        cdiags = CHAINT(IW(idiag+lgenr+2))                              ASKUS157
      ELSE                                                              ASKUS158
        iautow = 1                                                      ASKUS159
        cdiags = 'MCBA'                                                 ASKUS160
      ENDIF                                                             ASKUS161
C                                                                       ASKUS162
      nevtmx = 1D9                                                      ASKUS163
      nwrimx = 1D9                                                      ASKUS164
C                                                                       ASKUS165
      CALL PROSET                                                       ASKUS166
C                                                                       ASKUS167
C Kleiss's histogramming initialization                                 ASKUS168
C                                                                       ASKUS169
      CALL HISTO3(0)                                                    ASKUS170
C                                                                       ASKUS171
      mxdec = KNODEC(nodec,lpdec)                                       ASKUS172
      mxdec = MIN(mxdec,lpdec)                                          ASKUS173
C...Inhibit lund decays which should be done in galeph                  ASKUS174
      IF ( mxdec .GT. 0 ) THEN                                          ASKUS175
        DO 10 i=1,mxdec                                                 ASKUS176
          IF ( nodec(i) .GT. 0 ) THEN                                   ASKUS177
            jidb = NLINK('MDC1',nodec(i))                               ASKUS178
            IF ( jidb .eq. 0 ) mdcy(lucomp(nodec(i)),1) = 0             ASKUS179
          ENDIF                                                         ASKUS180
   10   CONTINUE                                                        ASKUS181
      ENDIF                                                             ASKUS182
C...Vertex smearing                                                     ASKUS183
      sdvrt(1) = 0.035                                                  ASKUS184
      sdvrt(2) = 0.0012                                                 ASKUS185
      sdvrt(3) = 1.28                                                   ASKUS186
      jsvrt = NLINK('SVRT',0)                                           ASKUS187
      IF ( jsvrt .NE. 0 ) THEN                                          ASKUS188
        sdvrt(1) = RW(jsvrt+1)                                          ASKUS189
        sdvrt(2) = RW(jsvrt+2)                                          ASKUS190
        sdvrt(3) = RW(jsvrt+3)                                          ASKUS191
      ENDIF                                                             ASKUS192
      tabl(24) = sdvrt(1)                                               ASKUS193
      tabl(25) = sdvrt(2)                                               ASKUS194
      tabl(26) = sdvrt(3)                                               ASKUS195
C                                                                       ASKUS196
C  Fill the KPAR bank with the generator parameters                     ASKUS197
C                                                                       ASKUS198
       jkpar = altabl('KPAR',26,1,tabl,'2I,(2A,14F,I,F,3I,F,I,4F)','C') ASKUS199
C                                                                       ASKUS200
C  Fill RLEP bank                                                       ASKUS201
C                                                                       ASKUS202
       iebeam = NINT(ecms*500)                                          ASKUS203
       jrlep = alrlep(iebeam,'    ',0,0,0)                              ASKUS204
C                                                                       ASKUS205
C...Debug flags                                                         ASKUS206
C                                                                       ASKUS207
      jdebu = IW(NAMIND('DEBU'))                                        ASKUS208
      IF ( jdebu .GT. 0 ) THEN                                          ASKUS209
        idb1 = IW(jdebu+1)                                              ASKUS210
        idb2 = IW(jdebu+2)                                              ASKUS211
      ENDIF                                                             ASKUS212
C                                                                       ASKUS213
C  Initialize events counters                                           ASKUS214
C                                                                       ASKUS215
       DO 11 i = 1,11                                                   ASKUS216
   11  nnvent(i) = 0                                                    ASKUS217
C                                                                       ASKUS218
C  Print PART and KLIN banks                                            ASKUS219
C                                                                       ASKUS220
C     CALL PRPART                                                       ASKUS221
C                                                                       ASKUS222
      CALL PRTABL('KPAR',0)                                             ASKUS223
      CALL PRTABL('RLEP',0)                                             ASKUS224
C                                                                       ASKUS225
C TAUOLA initialization                                                 ASKUS226
C                                                                       ASKUS227
C     ikorl = NLINK('GKRL',0)                                           ASKUS228
C     IF ( ikorl .NE. 0 ) THEN                                          ASKUS229
C Initialize TAUOLA                                                     ASKUS230
C       igtau = NLINK('GTDK',0)                                         ASKUS231
C       IF ( igtau .NE. 0 ) THEN                                        ASKUS232
C Parameters in KORLxx 'GTDK' card first                                ASKUS233
C         jak1     = IW(igtau+1)                                        ASKUS234
C         jak2     = IW(igtau+2)                                        ASKUS235
C         itdkrc   = IW(igtau+3)                                        ASKUS236
C         xk0dec   = RW(igtau+4)                                        ASKUS237
C         amnuta_k = RW(igtau+5)                                        ASKUS238
C       ELSE                                                            ASKUS239
C Defaults                                                              ASKUS240
C         jak1     = 0                                                  ASKUS241
C         jak2     = 0                                                  ASKUS242
C         itdkrc   = 1                                                  ASKUS243
C         xk0dec   = 0.001                                              ASKUS244
C         amnuta_k = 0.001                                              ASKUS245
C       ENDIF                                                           ASKUS246
C       xk0 = xk0dec                                                    ASKUS247
C       alfinv = 137.03604d0                                            ASKUS248
C       alfpi = 1.0d0 / ( alfinv * 4.0d0 * atan(1.0d0) )                ASKUS249
C       gv = 1.                                                         ASKUS250
C       ga = -1.                                                        ASKUS251
C       iout = loutbe                                                   ASKUS252
C       idff = -15                                                      ASKUS253
C       CALL inimas                                                     ASKUS254
C       CALL initdk                                                     ASKUS255
C     ELSE                                                              ASKUS256
C                                                                       ASKUS257
C Choose LUND for tau decaus                                            ASKUS258
C                                                                       ASKUS259
C       jak1 = -1                                                       ASKUS260
C       jak2 = -1                                                       ASKUS261
C     ENDIF                                                             ASKUS262
C                                                                       ASKUS263
      RETURN                                                            ASKUS264
      END                                                               ASKUS265
      SUBROUTINE ASKUSE(IDPR,ISTA,NITR,NIVX,ECM,WEIT)                   ASKUSE 2
C-----------------------------------------------------------------------ASKUSE 3
C Event generation                                                      ASKUSE 4
C-----------------------------------------------------------------------ASKUSE 5
      REAL dummy(4)                                                     ASKUSE 6
C                                                                       ASKUSE 7
      DIMENSION ptrak(4,2)                                              ASKUSE 8
      REAL*8 wght                                                       ASKUSE 9
      DIMENSION icode(11), ijoin(2), jndqq(8)                           ASKUSE10
      LOGICAL checkres                                                  ASKUSE11
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      COMMON / BCS / IW(1000)                                           BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(1000)                                                     BCS    7
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
      COMMON / miscl / loutbe,ecms,idb1,idb2,irad,ibeams,               MISCL  2
     &                 sdvrt(3),vrtx(4),tabl(26),nnvent(11)             MISCL  3
      INTEGER loutbe,idb1,idb2,nnvent                                   MISCL  4
      INTEGER kabl(26)                                                  MISCL  5
      REAL*4 sdvrt,vrtx,tabl                                            MISCL  6
      EQUIVALENCE(tabl(1),kabl(1))                                      MISCL  7
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C BRANCH commons                                                        BRANCHC2
      INTEGER LCHMX,PAR1,PAR2,PAR3,PAR4,PAR5,PAR6,NEVTGN,NRJGN,         BRANCHC3
     &   NCHAN,NCHANT,GENRJC,LGENR,JCHAN,CHACOU,CHANLS,                 BRANCHC4
     &   LANNIH,LBREMF,LBREMB,LCONV1,LCONV2,LCON1Z,LCON2Z,LMULTI,LRAMBO BRANCHC5
      PARAMETER (LCHMX = 100, LGENR = 9, LANNIH = 1, LBREMF = 2,        BRANCHC6
     &   LBREMB = 3, LCONV1 = 4, LCONV2 = 5,  LCON1Z = 6, LCON2Z = 7,   BRANCHC7
     &   LMULTI = 8, LRAMBO =9)                                         BRANCHC8
      REAL*8 SMINP,SMAXP,SMX3,SKMIN,SKMAX,BRNCH,FRAC,CGEN               BRANCHC9
      COMMON /BRANCH/ PAR1(LCHMX),PAR2(LCHMX),PAR3(LCHMX),PAR4(LCHMX),  BRANCH10
     &   PAR5(LCHMX),PAR6(LCHMX),NEVTGN(LGENR),NRJGN(LGENR),NCHAN,      BRANCH11
     &   NCHANT,GENRJC(LCHMX),JCHAN(LBUNCH),CHACOU(0:LGENR),            BRANCH12
     &   CHANLS(LGENR),                                                 BRANCH13
     &   SKMIN,SKMAX,SMINP(LCHMX),SMAXP(LCHMX),SMX3(LCHMX),             BRANCH14
     &   BRNCH(0:LCHMX),FRAC(LCHMX),CGEN(LGENR)                         BRANCH15
      CHARACTER*16 GNNAME(LGENR)                                        BRANCH16
      COMMON /GNNAMS/ GNNAME                                            BRANCH17
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
      REAL*8 XME,XM34,XM56,THETCN                                       PROSECO2
      REAL*4 SCGEN                                                      PROSECO3
      INTEGER IISR,IFSR,IAUTOW,CH334,CH356,QCDFLG                       PROSECO4
      CHARACTER*4 CDIAGS                                                PROSECO5
      COMMON / copro1 / xme,xm34,xm56,thetcn                            PROSECO6
      COMMON / copro2 / iisr,ifsr,iautow,ch334,ch356,qcdflg             PROSECO7
      COMMON / copro3 / cdiags                                          PROSECO8
      COMMON / copro4 / scgen(lgenr)                                    PROSECO9
      DATA icode/11,12,13,14,15,16,1,2,3,4,5/                           ASKUSE21
      DATA jndqq/2,1,3,4,5,6,7,8/,kstrn/92/,kfg/22/                     ASKUSE22
      DATA nevbunch/0/                                                  ASKUSE23
C                                                                       ASKUSE24
C  Initialization ASKUSE's arguments                                    ASKUSE25
C                                                                       ASKUSE26
      ista = 0                                                          ASKUSE27
      nitr = 0                                                          ASKUSE28
      nivx = 0                                                          ASKUSE29
      ecm  = ecms                                                       ASKUSE30
      weit = 1.                                                         ASKUSE31
      sbeam = ecm**2                                                    ASKUSE32
      n7lu = 0                                                          ASKUSE33
C                                                                       ASKUSE34
C  Produce a bunch of events, if needed                                 ASKUSE35
C                                                                       ASKUSE36
    1 CONTINUE                                                          ASKUSE37
      IF ( nevbunch .EQ. 0 ) THEN                                       ASKUSE38
        CALL fermis                                                     ASKUSE39
        nevbunch = lbunch                                               ASKUSE40
      ENDIF                                                             ASKUSE41
C                                                                       ASKUSE42
C  Else proceed with the current bunch                                  ASKUSE43
C                                                                       ASKUSE44
      nevbunch = nevbunch - 1                                           ASKUSE45
      iev = lbunch - nevbunch                                           ASKUSE46
C                                                                       ASKUSE47
C  Keep only unwt'd events, if required                                 ASKUSE48
C                                                                       ASKUSE49
      CALL wtdevt(iev,wght)                                             ASKUSE50
      weit = wght                                                       ASKUSE51
      IF ( wght .EQ. 0D0 ) GOTO 1                                       ASKUSE52
C                                                                       ASKUSE53
C  Store beam particles also in bos banks                               ASKUSE54
C                                                                       ASKUSE55
      ipart = kgpart(11)                                                ASKUSE56
      DO 2 itr = 1,2                                                    ASKUSE57
         DO 9 i=1,4                                                     ASKUSE58
 9       ptrak(i,itr) = 0.                                              ASKUSE59
         ipart = kgpart(11)                                             ASKUSE60
         ptrak(3,itr) = 0.5*ecm                                         ASKUSE61
         IF ( itr .EQ. 2 ) THEN                                         ASKUSE62
           ipart = kgpart(-11)                                          ASKUSE63
           ptrak(3,itr) =- 0.5*ecm                                      ASKUSE64
         ENDIF                                                          ASKUSE65
         ist=kbkine(-itr,ptrak(1,itr),ipart,0)                          ASKUSE66
         IF ( ist .LE. 0 ) THEN                                         ASKUSE67
            ista = -2                                                   ASKUSE68
            GO TO 998                                                   ASKUSE69
         ENDIF                                                          ASKUSE70
  2   CONTINUE                                                          ASKUSE71
C                                                                       ASKUSE72
C  Radiative photon in the initial state                                ASKUSE73
C                                                                       ASKUSE74
      IF ( iisr .EQ. 1 ) THEN                                           ASKUSE75
        IF ( qmom(iev,4,7) .GT. 1D-4 ) THEN                             ASKUSE76
          n7lu = n7lu + 1                                               ASKUSE77
          CALL hhlu1(n7lu,kfg,qmom(iev,1,7),qmom(iev,2,7),              ASKUSE78
     .                        qmom(iev,3,7),qmom(iev,4,7))              ASKUSE79
          k7lu(n7lu,1) = 1                                              ASKUSE80
          k7lu(n7lu,3) = 0                                              ASKUSE81
          nrad = 1                                                      ASKUSE82
        ENDIF                                                           ASKUSE83
      ENDIF                                                             ASKUSE84
C                                                                       ASKUSE85
C  Fill the 4-fermions                                                  ASKUSE86
C                                                                       ASKUSE87
      nagene = NAMIND('GENE')                                           ASKUSE88
      igene = IW(nagene)                                                ASKUSE89
      isg  = -1                                                         ASKUSE90
      DO idl = 3, 6                                                     ASKUSE91
        ifl = (idl-1)/2                                                 ASKUSE92
        isg = -isg                                                      ASKUSE93
        kff = isg * icode(IW(igene+ifl))                                ASKUSE94
        n7lu = n7lu + 1                                                 ASKUSE95
        CALL hhlu1(n7lu,kff,qmom(iev,1,idl),qmom(iev,2,idl),            ASKUSE96
     .                      qmom(iev,3,idl),qmom(iev,4,idl))            ASKUSE97
        k7lu(n7lu,1) = 1                                                ASKUSE98
        k7lu(n7lu,3) = 0                                                ASKUSE99
      ENDDO                                                             ASKUS100
      npal = n7lu                                                       ASKUS101
C                                                                       ASKUS102
C Quark final states                                                    ASKUS103
C                                                                       ASKUS104
      do 10 ipp=1,2                                                     ASKUS105
         kff = icode(IW(igene+ipp))                                     ASKUS106
         IF ( kff .LE. 8 ) THEN                                         ASKUS107
            xm2 = ( qmom(iev,4,2*ipp+1)+qmom(iev,4,2*ipp+2) ) **2       ASKUS108
     .      - ( qmom(iev,1,2*ipp+1)+qmom(iev,1,2*ipp+2) ) **2           ASKUS109
     .      - ( qmom(iev,2,2*ipp+1)+qmom(iev,2,2*ipp+2) ) **2           ASKUS110
     .      - ( qmom(iev,3,2*ipp+1)+qmom(iev,3,2*ipp+2) ) **2           ASKUS111
            xmm = SQRT(xm2)                                             ASKUS112
C                                                                       ASKUS113
C                                                                       ASKUS114
C Kind of quarks                                                        ASKUS115
C                                                                       ASKUS116
            indqq = jndqq(kff)                                          ASKUS117
C                                                                       ASKUS118
C Is the qqbar system a (1s) resonance ?                                ASKUS119
C                                                                       ASKUS120
            IF ( qcdflg .EQ. 1 .AND. checkres(xm2,indqq,ires) ) THEN    ASKUS121
C The rho                                                               ASKUS122
            IF     ( indqq .LE. 2 .AND. ires .EQ. 0 ) THEN              ASKUS123
               kkcod = 113                                              ASKUS124
C The omega                                                             ASKUS125
            ELSEIF ( indqq .LE. 2 .AND. ires .EQ. 1 ) THEN              ASKUS126
               kkcod = 223                                              ASKUS127
C The Phi                                                               ASKUS128
            ELSEIF ( indqq .EQ. 3 ) THEN                                ASKUS129
               kkcod = 333                                              ASKUS130
C The J/Psi                                                             ASKUS131
            ELSEIF ( indqq .EQ. 4 ) THEN                                ASKUS132
               kkcod = 443                                              ASKUS133
C The Upsilon(1s)                                                       ASKUS134
            ELSEIF ( indqq .EQ. 5 ) THEN                                ASKUS135
               kkcod = 553                                              ASKUS136
C Here we go                                                            ASKUS137
            ENDIF                                                       ASKUS138
C                                                                       ASKUS139
            k7lu(npal+2*ipp-4,1) = 11                                   ASKUS140
            k7lu(npal+2*ipp-5,1) = 12                                   ASKUS141
            n7lu = n7lu + 1                                             ASKUS142
         CALL hhlu1(n7lu,kstrn,qmom(iev,1,2*ipp+1)+qmom(iev,1,2*ipp+2), ASKUS143
     .                         qmom(iev,2,2*ipp+1)+qmom(iev,2,2*ipp+2), ASKUS144
     .                         qmom(iev,3,2*ipp+1)+qmom(iev,3,2*ipp+2), ASKUS145
     .                         qmom(iev,4,2*ipp+1)+qmom(iev,4,2*ipp+2)) ASKUS146
            k7lu(n7lu,1) = 11                                           ASKUS147
            k7lu(n7lu,3) = npal+2*ipp-5                                 ASKUS148
            k7lu(n7lu,4) = n7lu+1                                       ASKUS149
            k7lu(n7lu,5) = n7lu+1                                       ASKUS150
            n7lu = n7lu + 1                                             ASKUS151
         CALL hhlu1(n7lu,kkcod,qmom(iev,1,2*ipp+1)+qmom(iev,1,2*ipp+2), ASKUS152
     .                         qmom(iev,2,2*ipp+1)+qmom(iev,2,2*ipp+2), ASKUS153
     .                         qmom(iev,3,2*ipp+1)+qmom(iev,3,2*ipp+2), ASKUS154
     .                         qmom(iev,4,2*ipp+1)+qmom(iev,4,2*ipp+2)) ASKUS155
            k7lu(n7lu,1) = 1                                            ASKUS156
            k7lu(n7lu,3) = n7lu-1                                       ASKUS157
C                                                                       ASKUS158
C If not, call LUSHOW                                                   ASKUS159
C                                                                       ASKUS160
        ELSE                                                            ASKUS161
                                                                        ASKUS162
          ijoin(1) = npal +2*ipp-5                                      ASKUS163
          ijoin(2) = npal +2*ipp-4                                      ASKUS164
          k7lu(ijoin(1),1) = 2                                          ASKUS165
          njoin = 2                                                     ASKUS166
          CALL lujoin(njoin,ijoin)                                      ASKUS167
          CALL lushow(ijoin(1), ijoin(2), xmm)                          ASKUS168
        ENDIF                                                           ASKUS169
C                                                                       ASKUS170
      ENDIF                                                             ASKUS171
  10  continue                                                          ASKUS172
C                                                                       ASKUS173
C  Radiative photon in the final state                                  ASKUS174
C                                                                       ASKUS175
      emax = 0.                                                         ASKUS176
      DO i = 8, nopart                                                  ASKUS177
        IF ( qmom(iev,4,i) .GT. 1D-4 ) THEN                             ASKUS178
          n7lu = n7lu + 1                                               ASKUS179
          CALL hhlu1(n7lu,kfg,qmom(iev,1,i),qmom(iev,2,i),              ASKUS180
     .                        qmom(iev,3,i),qmom(iev,4,i))              ASKUS181
          k7lu(n7lu,1) = 1                                              ASKUS182
          k7lu(n7lu,3) = 0                                              ASKUS183
          IF ( qmom(iev,4,i) .GT. emax ) emax = qmom(iev,4,i)           ASKUS184
        ENDIF                                                           ASKUS185
      ENDDO                                                             ASKUS186
C                                                                       ASKUS187
C Decay all taus with TAUOLA                                            ASKUS188
C                                                                       ASKUS189
C     ikorl = NLINK('GKRL',0)                                           ASKUS190
C     IF ( ikorl .GT. 0 ) THEN                                          ASKUS191
C       dummy(1) = 0.                                                   ASKUS192
C       CALL dexay(0,dummy)                                             ASKUS193
C     ENDIF                                                             ASKUS194
C                                                                       ASKUS195
C Execute LUND                                                          ASKUS196
C                                                                       ASKUS197
      CALL luexec                                                       ASKUS198
C                                                                       ASKUS199
C  Listing of the event                                                 ASKUS200
C                                                                       ASKUS201
      nnvent(1) = nnvent(1) + 1                                         ASKUS202
      IF ( nnvent(1) .GE. idb1 .AND.                                    ASKUS203
     .     nnvent(1) .LE. idb2 ) CALL lulist(1)                         ASKUS204
C                                                                       ASKUS205
C  Smear vertex position                                                ASKUS206
C                                                                       ASKUS207
      CALL rannor (rx,ry)                                               ASKUS208
      CALL rannor (rz,dum)                                              ASKUS209
      vrtx(1) = rx*sdvrt(1)                                             ASKUS210
      vrtx(2) = ry*sdvrt(2)                                             ASKUS211
      vrtx(3) = rz*sdvrt(3)                                             ASKUS212
      vrtx(4) = 0.                                                      ASKUS213
C                                                                       ASKUS214
C  Event header (Yet undefined)                                         ASKUS215
C                                                                       ASKUS216
      idpr = 0                                                          ASKUS217
C                                                                       ASKUS218
C      Call the specific routine KXL7AL to fill BOS banks               ASKUS219
C                                                                       ASKUS220
      CALL kxl7al (vrtx,ist,nivx,nitr)                                  ASKUS221
      IF ( ist .NE. 0 ) GOTO 998                                        ASKUS222
      IF ( emax .LE. -1. ) ist = -6                                     ASKUS223
      ista = ist                                                        ASKUS224
C                                                                       ASKUS225
C  Event counters                                                       ASKUS226
C                                                                       ASKUS227
  998 IF ( ist .EQ. 0 ) nnvent(2) = nnvent(2) + 1                       ASKUS228
      IF ( ist .GT. 0) THEN                                             ASKUS229
        nnvent(3) = nnvent(3) + 1                                       ASKUS230
        nnvent(4) = nnvent(4) + 1                                       ASKUS231
        WRITE(6,*) 'Evt ',nnvent(1),' ist = ',ist                       ASKUS232
        CALL lulist(1)                                                  ASKUS233
      ELSEIF ( ist .LT. 0) THEN                                         ASKUS234
        nnvent(3) = nnvent(3) + 1                                       ASKUS235
        nnvent(4-ist) = nnvent(4-ist) + 1                               ASKUS236
      ENDIF                                                             ASKUS237
C                                                                       ASKUS238
      RETURN                                                            ASKUS239
      END                                                               ASKUS240
      SUBROUTINE USCJOB                                                 USCJOB 2
C-----------------------------------------------------------------------USCJOB 3
C                                                                       USCJOB 4
C   Routine for printout at the end of a run                            USCJOB 5
C                                                                       USCJOB 6
C-----------------------------------------------------------------------USCJOB 7
      COMMON / miscl / loutbe,ecms,idb1,idb2,irad,ibeams,               MISCL  2
     &                 sdvrt(3),vrtx(4),tabl(26),nnvent(11)             MISCL  3
      INTEGER loutbe,idb1,idb2,nnvent                                   MISCL  4
      INTEGER kabl(26)                                                  MISCL  5
      REAL*4 sdvrt,vrtx,tabl                                            MISCL  6
      EQUIVALENCE(tabl(1),kabl(1))                                      MISCL  7
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      COMMON / BCS / IW(1000)                                           BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(1000)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      REAL DUMMY(4)                                                     USCJOB10
C                                                                       USCJOB11
C Tabulate cross section and sub-divide into physics process            USCJOB12
C                                                                       USCJOB13
      CALL FINISH(0)                                                    USCJOB14
C                                                                       USCJOB15
C TAUOLA summary                                                        USCJOB16
C                                                                       USCJOB17
C     ikorl = NLINK('GKRL',0)                                           USCJOB18
C     IF ( ikorl .GT. 0 ) CALL dexay(1,dummy)                           USCJOB19
C                                                                       USCJOB20
C Print event counters                                                  USCJOB21
C                                                                       USCJOB22
       WRITE(LOUTBE,101)                                                USCJOB23
  101  FORMAT(//20X,'EVENTS STATISTICS',                                USCJOB24
     &         /20X,'*****************')                                USCJOB25
       WRITE(LOUTBE,102) NnVENT(1),NnVENT(2),NnVENT(3)                  USCJOB26
  102  FORMAT(/5X,'# OF GENERATED EVENTS                      = ',I10,  USCJOB27
     &        /5X,'# OF ACCEPTED  EVENTS                      = ',I10,  USCJOB28
     &        /5X,'# OF REJECTED  EVENTS (ISTA # 0 in ASKUSE) = ',I10)  USCJOB29
       WRITE(LOUTBE,103)                                                USCJOB30
  103  FORMAT(//20X,'ERRORS STATISTICS',                                USCJOB31
     &         /20X,'*****************')                                USCJOB32
       WRITE(LOUTBE,104) (NnVENT(I),I=4,11)                             USCJOB33
  104  FORMAT(/10X,'IR= 1 LUND ERROR unknown part   # OF REJECT = ',I10,USCJOB34
     &        /10X,'IR= 2 KINE/VERT banks missing   # OF REJECT = ',I10,USCJOB35
     &        /10X,'IR= 3 no space for VERT/KINE    # OF REJECT = ',I10,USCJOB36
     &        /10X,'IR= 4 LUND ERROR too many tracks# OF REJECT = ',I10,USCJOB37
     &        /10X,'IR= 5 LUND ERROR Beam wrong pos # OF REJECT = ',I10,USCJOB38
     &        /10X,'IR= 6 LUND ERROR Status code >5 # OF REJECT = ',I10,USCJOB39
     &        /10X,'IR= 7 free for user             # OF REJECT = ',I10,USCJOB40
     &        /10X,'IR= 8 free for user             # OF REJECT = ',I10)USCJOB41
C                                                                       USCJOB42
      RETURN                                                            USCJOB43
      END                                                               USCJOB44
      FUNCTION checkres(xs,ind,jres)                                    CHECKRE2
C--------------------------------------------------------------------   CHECKRE3
C! Check whether or not the qqbar system is a resonance (1s)            CHECKRE4
C                                                                       CHECKRE5
C Input :                                                               CHECKRE6
C    o xs   : the qqbar invariant mass squared                          CHECKRE7
C    o ind  : the kind of quark                                         CHECKRE8
C                                                                       CHECKRE9
C Output :                                                              CHECKR10
C                                                                       CHECKR11
C    o jres : the resonance label                                       CHECKR12
C                                                                       CHECKR13
C--------------------------------------------------------------------   CHECKR14
      IMPLICIT REAL*8(A-H,O-Z)                                          CHECKR15
C The quark and lepton masses                                           QMASSES2
      REAL*8 amu, amd, ams, amc, amb, amt                               QMASSES3
      REAL*8 amel, ammu, amto, amne, amnm, amnt                         QMASSES4
      PARAMETER ( amu = .005D0, amd = .010d0, ams = .150d0 )            QMASSES5
      PARAMETER ( amc = 1.37d0, amb = 4.70d0, amt = 170.d0 )            QMASSES6
      PARAMETER ( amel= .511D-3, ammu= .1057D0, amto = 1.777d0 )        QMASSES7
      PARAMETER ( amne= 1D-4, amnm= 1D-4, amnt = 1D-4 )                 QMASSES8
C The pi value, you shouldn't touch !                                   RESONA 2
      PARAMETER ( pi = 3.1415926353 )                                   RESONA 3
C Alpha QED                                                             RESONA 4
      PARAMETER ( alpha = 1./137.0 )                                    RESONA 5
C The normalized mu+mu- cross section                                   RESONA 6
      PARAMETER ( cmumu = 4.*pi*alpha**2/3. )                           RESONA 7
C The mu,pi, K, D0  masses                                              RESONA 8
      PARAMETER ( xmu  = 0.10565839D0, xmu2  = xmu **2 )                RESONA 9
      PARAMETER ( xmpi = 0.13956755D0, xmpi2 = xmpi**2 )                RESONA10
      PARAMETER ( xmka = 0.493646D0  , xmka2 = xmka**2 )                RESONA11
      PARAMETER ( xmd0 = 1.8695D0    , xmd02 = xmd0**2 )                RESONA12
      PARAMETER ( xmb0 = 5.2776D0    , xmb02 = xmb0**2 )                RESONA13
C The pion form factor                                                  RESONA14
      PARAMETER ( a1 = 0.29020D0, a2 = -2.301D0 )                       RESONA15
      PARAMETER ( a3 = -0.0121D0, a4 =  1.849D0 )                       RESONA16
      PARAMETER ( xm = 1.2D0, xm2 = xm**2, xg = 0.15D0, pow = 0.22D0 )  RESONA17
C The omega resonance                                                   RESONA18
      PARAMETER ( xmom = 0.78195D0 , xmom2 = xmom**2 )                  RESONA19
      PARAMETER ( gmom = 0.00843D0 , gmome = 0.0000006 )                RESONA20
C The omega(1390) resonance                                             RESONA21
      PARAMETER ( x1390 = 1.3910D0  , x13902 = x1390**2 )               RESONA22
      PARAMETER ( g1390 = 0.2240D0  , g1390e = 0.0000002D0 )            RESONA23
C The rho(1450) resonance                                               RESONA24
      PARAMETER ( x1450 = 1.4500D0  , x14502 = x1450**2 )               RESONA25
C     PARAMETER ( g1450 = 0.2370D0  , g1450e = 0.0000025D0 ) ! ???      RESONA26
      PARAMETER ( g1450 = 0.2370D0  , g1450e = 0.0000010D0 )            RESONA27
C The omega(1600) resonance                                             RESONA28
      PARAMETER ( x1600 = 1.5940D0  , x16002 = x1600**2 )               RESONA29
      PARAMETER ( g1600 = 0.1000D0  , g1600e = 0.0000002D0 )            RESONA30
C The rho(1700) resonance                                               RESONA31
      PARAMETER ( x1700 = 1.7120D0  , x17002 = x1700**2 )               RESONA32
C     PARAMETER ( g1700 = 0.2130D0  , g1700e = 0.0000035 ) ! ???        RESONA33
      PARAMETER ( g1700 = 0.2130D0  , g1700e = 0.0000012 )              RESONA34
C The Janot(2600) resonance                                             RESONA35
      PARAMETER ( x2600 = 2.6000D0  , x26002 = x2600**2 )               RESONA36
      PARAMETER ( g2600 = 0.7000D0  , g2600e = 0.0000055 )              RESONA37
C The Phi(1020) resonance                                               RESONA38
      PARAMETER ( x1020 = 1.0194D0  , x10202 = x1020**2 )               RESONA39
      PARAMETER ( g1020 = 0.0044D0  , g1020e = 0.00000137D0 )           RESONA40
C The Phi(1680) resonance                                               RESONA41
      PARAMETER ( x1680 = 1.6800D0  , x16802 = x1680**2 )               RESONA42
      PARAMETER ( g1680 = 0.1500D0  , g1680e = 0.00000045D0 )           RESONA43
C The J/Psi(1s) resonance                                               RESONA44
      PARAMETER ( x3097 = 3.0969D0  , x30972 = x3097**2 )               RESONA45
      PARAMETER ( g3097 = 0.000068D0, g3097e = 0.00000507D0 )           RESONA46
C The J/Psi(2s) resonance                                               RESONA47
      PARAMETER ( x3685 = 3.6860D0  , x36852 = x3685**2 )               RESONA48
      PARAMETER ( g3685 = 0.000243D0, g3685e = 0.00000214D0 )           RESONA49
C The Psi(3770) resonance                                               RESONA50
      PARAMETER ( x3770 = 3.7699D0  , x37702 = x3770**2 )               RESONA51
      PARAMETER ( g3770 = 0.0236D0  , g3770e = 0.00000024D0 )           RESONA52
C The Psi(4040) resonance                                               RESONA53
      PARAMETER ( x4040 = 4.0400D0  , x40402 = x4040**2 )               RESONA54
      PARAMETER ( g4040 = 0.0520D0  , g4040e = 0.00000075D0 )           RESONA55
C The Psi(4160) resonance                                               RESONA56
      PARAMETER ( x4160 = 4.1590D0  , x41602 = x4160**2 )               RESONA57
      PARAMETER ( g4160 = 0.0780D0  , g4160e = 0.00000077D0 )           RESONA58
C The Psi(4415) resonance                                               RESONA59
      PARAMETER ( x4415 = 4.4150D0  , x44152 = x4415**2 )               RESONA60
      PARAMETER ( g4415 = 0.0430D0  , g4415e = 0.00000047D0 )           RESONA61
C The Janot(7000) resonance                                             RESONA62
      PARAMETER ( x7000 = 6.6000D0  , x70002 = x7000**2 )               RESONA63
      PARAMETER ( g7000 = 1.2500D0  , g7000e = 0.00000450D0 )           RESONA64
C The Upsilon(1s)(9460) resonance                                       RESONA65
      PARAMETER ( x9460 = 9.4603D0  , x94602 = x9460**2 )               RESONA66
      PARAMETER ( g9460 = 0.000052D0, g9460e = 0.00000134D0 )           RESONA67
C The Upsilon(2s)(10023) resonance                                      RESONA68
      PARAMETER ( x0023 = 10.023D0  , x00232 = x0023**2 )               RESONA69
      PARAMETER ( g0023 = 0.000043D0, g0023e = 0.00000059D0 )           RESONA70
C The Upsilon(3s)(10355) resonance                                      RESONA71
      PARAMETER ( x0355 = 10.355D0  , x03552 = x0355**2 )               RESONA72
      PARAMETER ( g0355 = 0.000024D0, g0355e = 0.00000044D0 )           RESONA73
C The Upsilon(4s)(10580) resonance                                      RESONA74
      PARAMETER ( x0580 = 10.580D0  , x05802 = x0580**2 )               RESONA75
      PARAMETER ( g0580 = 0.0238D0  , g0580e = 0.00000024D0 )           RESONA76
C The Upsilon(10860) resonance                                          RESONA77
      PARAMETER ( x0860 = 10.865D0  , x08602 = x0860**2 )               RESONA78
      PARAMETER ( g0860 = 0.1100D0  , g0860e = 0.00000031D0 )           RESONA79
C The Upsilon(11020) resonance                                          RESONA80
      PARAMETER ( x1111 = 11.019D0  , x11112 = x1111**2 )               RESONA81
      PARAMETER ( g1111 = 0.0790D0  , g1111e = 0.00000013D0 )           RESONA82
C And the common y affering                                             RESONA83
      COMMON / resonance / rparam(4,10,5), nres(5)                      RESONA84
      COMMON / extrmes   / rmin(10), rmax(10)                           RESONA85
      REAL*4 xs                                                         CHECKR18
      LOGICAL checkres, first                                           CHECKR19
      DATA first/.TRUE./                                                CHECKR20
C                                                                       CHECKR21
      checkres = .FALSE.                                                CHECKR22
C                                                                       CHECKR23
      IF ( ind .GT. 5 .OR. ind .LE. 0 ) GOTO 999                        CHECKR24
C                                                                       CHECKR25
      s  = xs                                                           CHECKR26
      ss = DSQRT(s)                                                     CHECKR27
      cumul = 0D0                                                       CHECKR28
      rtot = rratio(s,ind)                                              CHECKR29
      alea = RNDM(dummy)                                                CHECKR30
C                                                                       CHECKR31
      GOTO (1,1,3,4,3) ind                                              CHECKR32
C                                                                       CHECKR33
    1 facmul = 3D0/5D0                                                  CHECKR34
C                                                                       CHECKR35
C Check for the rho                                                     CHECKR36
C                                                                       CHECKR37
      beta = SQRT(1.-4.*xmpi2/s)                                        CHECKR38
      IF ( ss .LT. 1.5D0 ) THEN                                         CHECKR39
        cumul  = facmul*piform(s)*beta**3/4D0                           CHECKR40
C       WRITE(6,*) ss,alea,cumul,rtot                                   CHECKR41
        IF ( alea .LE. cumul/rtot ) THEN                                CHECKR42
          jres = 0                                                      CHECKR43
          checkres = .TRUE.                                             CHECKR44
          GOTO 999                                                      CHECKR45
        ENDIF                                                           CHECKR46
      ENDIF                                                             CHECKR47
      GOTO 5                                                            CHECKR48
    3 facmul = 3D0                                                      CHECKR49
      GOTO 5                                                            CHECKR50
    4 facmul = 3D0/4D0                                                  CHECKR51
    5 CONTINUE                                                          CHECKR52
C                                                                       CHECKR53
C Check only the 1st resonance [omega, phi, J/psi and Upsilon(1s)]      CHECKR54
C                                                                       CHECKR55
      jres = 1                                                          CHECKR56
      IF ( DABS(ss-rparam(1,jres,ind)) .LT.                             CHECKR57
     .         40.*rparam(2,jres,ind)       ) THEN                      CHECKR58
        res = 3.*pi*rparam(2,jres,ind)*rparam(3,jres,ind)               CHECKR59
     .     / ( (ss-rparam(1,jres,ind))**2                               CHECKR60
     .            +rparam(2,jres,ind) **2/4.)                           CHECKR61
     .     / cmumu                                                      CHECKR62
      ELSE                                                              CHECKR63
        res = 0D0                                                       CHECKR64
      ENDIF                                                             CHECKR65
      cumul = cumul + facmul * res                                      CHECKR66
C     WRITE(6,*) ss,alea,cumul,rtot                                     CHECKR67
      IF ( alea .LE. cumul/rtot ) THEN                                  CHECKR68
        checkres = .TRUE.                                               CHECKR69
        GOTO 999                                                        CHECKR70
      ENDIF                                                             CHECKR71
C                                                                       CHECKR72
 999  RETURN                                                            CHECKR73
      END                                                               CHECKR74
      SUBROUTINE hhlu1(ipa,kf,px,py,pz,pe)                              HHLU1  2
C------------------------------------------------------------------     HHLU1  3
C  Add one entry to the LUND event record                               HHLU1  4
C                                                                       HHLU1  5
C  Patrick Janot -- 26 Aug 1991                                         HHLU1  6
C------------------------------------------------------------------     HHLU1  7
      REAL*8 px,py,pz,pe,pm                                             HHLU1  8
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
C                                                                       HHLU1 10
      pm2 = pe**2-px**2-py**2-pz**2                                     HHLU1 11
      IF ( pm2 .GE. 0D0 ) THEN                                          HHLU1 12
        pm = SQRT(pm2)                                                  HHLU1 13
      ELSE                                                              HHLU1 14
        pm = 0D0                                                        HHLU1 15
      ENDIF                                                             HHLU1 16
      DO 100 J=1,5                                                      HHLU1 17
      k7lu(ipa,j)=0                                                     HHLU1 18
      p7lu(ipa,j)=0.                                                    HHLU1 19
  100 v7lu(ipa,j)=0.                                                    HHLU1 20
C...Store parton/particle in K and P vectors.                           HHLU1 21
      k7lu(ipa,1)=1                                                     HHLU1 22
      k7lu(ipa,2)=kf                                                    HHLU1 23
      p7lu(ipa,5)=pm                                                    HHLU1 24
      p7lu(ipa,4)=pe                                                    HHLU1 25
      p7lu(ipa,1)=px                                                    HHLU1 26
      p7lu(ipa,2)=py                                                    HHLU1 27
      p7lu(ipa,3)=pz                                                    HHLU1 28
C                                                                       HHLU1 29
  999 RETURN                                                            HHLU1 30
      END                                                               HHLU1 31
      DOUBLE PRECISION FUNCTION zratio(s,ind1)                          ZRATIO 2
C-----------------------------------------------------------------------ZRATIO 3
C! The R ratio parametrization !                                        ZRATIO 4
C                                                                       ZRATIO 5
C Input...                                                              ZRATIO 6
C o S    : the value of the qqbar invariant mass squared                ZRATIO 7
C o IND1 : = 1 for uubar,                                               ZRATIO 8
C          = 2 for ddbar,                                               ZRATIO 9
C          = 3 for ssbar,                                               ZRATIO10
C          = 4 for ccbar,                                               ZRATIO11
C          = 5 for bbbar,                                               ZRATIO12
C                                                                       ZRATIO13
C  Patrick Janot -- 01 Apr 1994                                         ZRATIO14
C                                                                       ZRATIO15
C-----------------------------------------------------------------------ZRATIO16
      IMPLICIT REAL*8 (A-H,O-Z)                                         ZRATIO17
C The quark and lepton masses                                           QMASSES2
      REAL*8 amu, amd, ams, amc, amb, amt                               QMASSES3
      REAL*8 amel, ammu, amto, amne, amnm, amnt                         QMASSES4
      PARAMETER ( amu = .005D0, amd = .010d0, ams = .150d0 )            QMASSES5
      PARAMETER ( amc = 1.37d0, amb = 4.70d0, amt = 170.d0 )            QMASSES6
      PARAMETER ( amel= .511D-3, ammu= .1057D0, amto = 1.777d0 )        QMASSES7
      PARAMETER ( amne= 1D-4, amnm= 1D-4, amnt = 1D-4 )                 QMASSES8
C The pi value, you shouldn't touch !                                   RESONA 2
      PARAMETER ( pi = 3.1415926353 )                                   RESONA 3
C Alpha QED                                                             RESONA 4
      PARAMETER ( alpha = 1./137.0 )                                    RESONA 5
C The normalized mu+mu- cross section                                   RESONA 6
      PARAMETER ( cmumu = 4.*pi*alpha**2/3. )                           RESONA 7
C The mu,pi, K, D0  masses                                              RESONA 8
      PARAMETER ( xmu  = 0.10565839D0, xmu2  = xmu **2 )                RESONA 9
      PARAMETER ( xmpi = 0.13956755D0, xmpi2 = xmpi**2 )                RESONA10
      PARAMETER ( xmka = 0.493646D0  , xmka2 = xmka**2 )                RESONA11
      PARAMETER ( xmd0 = 1.8695D0    , xmd02 = xmd0**2 )                RESONA12
      PARAMETER ( xmb0 = 5.2776D0    , xmb02 = xmb0**2 )                RESONA13
C The pion form factor                                                  RESONA14
      PARAMETER ( a1 = 0.29020D0, a2 = -2.301D0 )                       RESONA15
      PARAMETER ( a3 = -0.0121D0, a4 =  1.849D0 )                       RESONA16
      PARAMETER ( xm = 1.2D0, xm2 = xm**2, xg = 0.15D0, pow = 0.22D0 )  RESONA17
C The omega resonance                                                   RESONA18
      PARAMETER ( xmom = 0.78195D0 , xmom2 = xmom**2 )                  RESONA19
      PARAMETER ( gmom = 0.00843D0 , gmome = 0.0000006 )                RESONA20
C The omega(1390) resonance                                             RESONA21
      PARAMETER ( x1390 = 1.3910D0  , x13902 = x1390**2 )               RESONA22
      PARAMETER ( g1390 = 0.2240D0  , g1390e = 0.0000002D0 )            RESONA23
C The rho(1450) resonance                                               RESONA24
      PARAMETER ( x1450 = 1.4500D0  , x14502 = x1450**2 )               RESONA25
C     PARAMETER ( g1450 = 0.2370D0  , g1450e = 0.0000025D0 ) ! ???      RESONA26
      PARAMETER ( g1450 = 0.2370D0  , g1450e = 0.0000010D0 )            RESONA27
C The omega(1600) resonance                                             RESONA28
      PARAMETER ( x1600 = 1.5940D0  , x16002 = x1600**2 )               RESONA29
      PARAMETER ( g1600 = 0.1000D0  , g1600e = 0.0000002D0 )            RESONA30
C The rho(1700) resonance                                               RESONA31
      PARAMETER ( x1700 = 1.7120D0  , x17002 = x1700**2 )               RESONA32
C     PARAMETER ( g1700 = 0.2130D0  , g1700e = 0.0000035 ) ! ???        RESONA33
      PARAMETER ( g1700 = 0.2130D0  , g1700e = 0.0000012 )              RESONA34
C The Janot(2600) resonance                                             RESONA35
      PARAMETER ( x2600 = 2.6000D0  , x26002 = x2600**2 )               RESONA36
      PARAMETER ( g2600 = 0.7000D0  , g2600e = 0.0000055 )              RESONA37
C The Phi(1020) resonance                                               RESONA38
      PARAMETER ( x1020 = 1.0194D0  , x10202 = x1020**2 )               RESONA39
      PARAMETER ( g1020 = 0.0044D0  , g1020e = 0.00000137D0 )           RESONA40
C The Phi(1680) resonance                                               RESONA41
      PARAMETER ( x1680 = 1.6800D0  , x16802 = x1680**2 )               RESONA42
      PARAMETER ( g1680 = 0.1500D0  , g1680e = 0.00000045D0 )           RESONA43
C The J/Psi(1s) resonance                                               RESONA44
      PARAMETER ( x3097 = 3.0969D0  , x30972 = x3097**2 )               RESONA45
      PARAMETER ( g3097 = 0.000068D0, g3097e = 0.00000507D0 )           RESONA46
C The J/Psi(2s) resonance                                               RESONA47
      PARAMETER ( x3685 = 3.6860D0  , x36852 = x3685**2 )               RESONA48
      PARAMETER ( g3685 = 0.000243D0, g3685e = 0.00000214D0 )           RESONA49
C The Psi(3770) resonance                                               RESONA50
      PARAMETER ( x3770 = 3.7699D0  , x37702 = x3770**2 )               RESONA51
      PARAMETER ( g3770 = 0.0236D0  , g3770e = 0.00000024D0 )           RESONA52
C The Psi(4040) resonance                                               RESONA53
      PARAMETER ( x4040 = 4.0400D0  , x40402 = x4040**2 )               RESONA54
      PARAMETER ( g4040 = 0.0520D0  , g4040e = 0.00000075D0 )           RESONA55
C The Psi(4160) resonance                                               RESONA56
      PARAMETER ( x4160 = 4.1590D0  , x41602 = x4160**2 )               RESONA57
      PARAMETER ( g4160 = 0.0780D0  , g4160e = 0.00000077D0 )           RESONA58
C The Psi(4415) resonance                                               RESONA59
      PARAMETER ( x4415 = 4.4150D0  , x44152 = x4415**2 )               RESONA60
      PARAMETER ( g4415 = 0.0430D0  , g4415e = 0.00000047D0 )           RESONA61
C The Janot(7000) resonance                                             RESONA62
      PARAMETER ( x7000 = 6.6000D0  , x70002 = x7000**2 )               RESONA63
      PARAMETER ( g7000 = 1.2500D0  , g7000e = 0.00000450D0 )           RESONA64
C The Upsilon(1s)(9460) resonance                                       RESONA65
      PARAMETER ( x9460 = 9.4603D0  , x94602 = x9460**2 )               RESONA66
      PARAMETER ( g9460 = 0.000052D0, g9460e = 0.00000134D0 )           RESONA67
C The Upsilon(2s)(10023) resonance                                      RESONA68
      PARAMETER ( x0023 = 10.023D0  , x00232 = x0023**2 )               RESONA69
      PARAMETER ( g0023 = 0.000043D0, g0023e = 0.00000059D0 )           RESONA70
C The Upsilon(3s)(10355) resonance                                      RESONA71
      PARAMETER ( x0355 = 10.355D0  , x03552 = x0355**2 )               RESONA72
      PARAMETER ( g0355 = 0.000024D0, g0355e = 0.00000044D0 )           RESONA73
C The Upsilon(4s)(10580) resonance                                      RESONA74
      PARAMETER ( x0580 = 10.580D0  , x05802 = x0580**2 )               RESONA75
      PARAMETER ( g0580 = 0.0238D0  , g0580e = 0.00000024D0 )           RESONA76
C The Upsilon(10860) resonance                                          RESONA77
      PARAMETER ( x0860 = 10.865D0  , x08602 = x0860**2 )               RESONA78
      PARAMETER ( g0860 = 0.1100D0  , g0860e = 0.00000031D0 )           RESONA79
C The Upsilon(11020) resonance                                          RESONA80
      PARAMETER ( x1111 = 11.019D0  , x11112 = x1111**2 )               RESONA81
      PARAMETER ( g1111 = 0.0790D0  , g1111e = 0.00000013D0 )           RESONA82
C And the common y affering                                             RESONA83
      COMMON / resonance / rparam(4,10,5), nres(5)                      RESONA84
      COMMON / extrmes   / rmin(10), rmax(10)                           RESONA85
C                                                                       ZRATIO20
      DIMENSION res(10)                                                 ZRATIO21
C                                                                       ZRATIO22
      ss = DSQRT(s)                                                     ZRATIO23
      rratio = 0.                                                       ZRATIO24
      CALL vzero(res(1),20)                                             ZRATIO25
C                                                                       ZRATIO26
      DO ires = 1, nres(ind1)                                           ZRATIO27
        IF ( DABS(ss-rparam(1,ires,ind1)) .LT.                          ZRATIO28
     .           40.*rparam(2,ires,ind1)       ) THEN                   ZRATIO29
          res(ires) = 3.*pi*rparam(2,ires,ind1)*rparam(3,ires,ind1)     ZRATIO30
     .            / ( (ss-rparam(1,ires,ind1))**2                       ZRATIO31
     .                   +rparam(2,ires,ind1) **2/4.)                   ZRATIO32
        ENDIF                                                           ZRATIO33
        IF ( ind1 .LE. 2 .AND. ires .GE. 2 .AND. ss .LE. .922D0 )       ZRATIO34
     .  res(ires) = 0D0                                                 ZRATIO35
        rratio = rratio + res(ires)/cmumu                               ZRATIO36
      ENDDO                                                             ZRATIO37
C                                                                       ZRATIO38
      GOTO (1,1,3,4,5) ind1                                             ZRATIO39
C                                                                       ZRATIO40
C The uubar/ddbar contribution                                          ZRATIO41
C                                                                       ZRATIO42
    1 CONTINUE                                                          ZRATIO43
      IF ( ss .LT. 2*xmpi ) THEN                                        ZRATIO44
        rratio = 0.                                                     ZRATIO45
        GOTO 999                                                        ZRATIO46
      ENDIF                                                             ZRATIO47
C                                                                       ZRATIO48
      beta = SQRT(1.-4.*xmpi2/s)                                        ZRATIO49
      rhores  = 0.                                                      ZRATIO50
      IF (ss .LT. 1.5) rhores = piform(s) * beta**3 / 4.                ZRATIO51
      IF (ss .LT. .922D0) THEN                                          ZRATIO52
        beta = 0D0                                                      ZRATIO53
      ELSE                                                              ZRATIO54
        beta = SQRT(1D0-.922D0**2/s)                                    ZRATIO55
      ENDIF                                                             ZRATIO56
      rratio = (beta + 3./5.*(rratio + rhores))                         ZRATIO57
      GOTO 999                                                          ZRATIO58
C                                                                       ZRATIO59
C The ssbar contribution                                                ZRATIO60
C                                                                       ZRATIO61
    3 CONTINUE                                                          ZRATIO62
      IF ( ss .LT. 2*xmka ) THEN                                        ZRATIO63
        rratio = 0.                                                     ZRATIO64
        GOTO 999                                                        ZRATIO65
      ENDIF                                                             ZRATIO66
C                                                                       ZRATIO67
      beta = 0D0                                                        ZRATIO68
      IF ( ss .GT. 4.*xmka ) beta = SQRT(1.-4.*xmka2/s)                 ZRATIO69
      rratio = (beta   +3D0*rratio)                                     ZRATIO70
      GOTO 999                                                          ZRATIO71
C                                                                       ZRATIO72
C The ccbar contribution                                                ZRATIO73
C                                                                       ZRATIO74
    4 CONTINUE                                                          ZRATIO75
      IF ( ss .LE. rmin(4) ) THEN                                       ZRATIO76
        rratio = 0.                                                     ZRATIO77
        GOTO 999                                                        ZRATIO78
      ENDIF                                                             ZRATIO79
C                                                                       ZRATIO80
      betad0 = 0.D0                                                     ZRATIO81
      IF ( ss .GT. 2D0*xmd0 ) betad0 = SQRT(1.-4D0*xmd0**2/s)           ZRATIO82
      rratio = (betad0   +3D0/4D0*rratio)                               ZRATIO83
      GOTO 999                                                          ZRATIO84
C                                                                       ZRATIO85
C The bbbar contribution                                                ZRATIO86
C                                                                       ZRATIO87
    5 CONTINUE                                                          ZRATIO88
      IF ( ss .LE. rmin(5) ) THEN                                       ZRATIO89
        rratio = 0.                                                     ZRATIO90
        GOTO 999                                                        ZRATIO91
      ENDIF                                                             ZRATIO92
C                                                                       ZRATIO93
      betab0 = 0.D0                                                     ZRATIO94
      IF ( ss .GT. x1111 ) THEN                                         ZRATIO95
        betab0 = 1D0                                                    ZRATIO96
      ELSEIF ( ss .GT. x9460 ) THEN                                     ZRATIO97
        betab0 = 0.1                                                    ZRATIO98
      ENDIF                                                             ZRATIO99
      rratio = (betab0   +3D0*rratio)                                   ZRATI100
C                                                                       ZRATI101
  999 CONTINUE                                                          ZRATI102
      zratio = rratio                                                   ZRATI103
C                                                                       ZRATI104
      RETURN                                                            ZRATI105
      END                                                               ZRATI106
      SUBROUTINE FERMIS                                                 FERMIS 2
C---------------------------------------------------------------------  FERMIS 3
C! Modified steering routine for FERMISV                                FERMIS 4
C                                                                       FERMIS 5
C Patrick Janot -- 17 Oct 1994                                          FERMIS 6
C---------------------------------------------------------------------  FERMIS 7
C *** VECTOR (and scalar) MACHINE VERSION ***                           FERMIS 8
C PROGRAM TREE:                                                         FERMIS 9
C                                                                       FERMIS10
C PROSET -  set kinematics, flavours, charges, weak couplings, and JOB  FERMIS11
C |-> BRNCHS - set up channels used by ps generators                    FERMIS12
C |-> PSWTS  - set up ps generator fractions                            FERMIS13
C HISTO3 - initialize Kleiss histogram package                          FERMIS14
C DOISR  - ISR generation and related variables                         FERMIS15
C |-> GETBOO - build boost 4-vector from gamma 4-momentum               FERMIS16
C |-> GETLAB - Lorentz boost routine to return to lab frame             FERMIS17
C |-> INITIG - generate ISR photon                                      FERMIS18
C |-> PSWTS  - set up ps generator fractions                            FERMIS19
C |-> CSCENV - Update cen. det. angle cut-off for events w/ ISR         FERMIS20
C PHASE -  Steering for phase space.  Choose generator and form p.s. denFERMIS21
C |-> ANNIHI - Annih. style p.s. generator                              FERMIS22
C     |-> DECA2X                                                        FERMIS23
C     |-> DECAY2                                                        FERMIS24
C |-> BREMF - Bremss. fwd leg radiating p.s. generator                  FERMIS25
C     |-> DECAY2                                                        FERMIS26
C |-> BREMB - Bremss. bck leg radiating p.s. generator                  FERMIS27
C     |-> DECA2X                                                        FERMIS28
C     |-> DECAY2                                                        FERMIS29
C |-> CONVS1 - Convs. style 1 whisker (soft ISR) p.s. generator         FERMIS30
C     |-> DECAY2                                                        FERMIS31
C |-> CONVS2 -  Convs. style 2 whiskers p.s. generator                  FERMIS32
C     |-> DECAY2                                                        FERMIS33
C |-> CONV1Z - Convs. style 1 whisker (resonant ISR) p.s. generator     FERMIS34
C     |-> DECAY2                                                        FERMIS35
C |-> CONV2Z - Convs. style 2 resonanting particles p.s. generator      FERMIS36
C     |-> DECAY2                                                        FERMIS37
C |-> MULTIP - multip. style p.s. generator                             FERMIS38
C     |-> DECAY2                                                        FERMIS39
C |-> RANEVT                                                            FERMIS40
C     |-> RAMBO - generate random phase space event                     FERMIS41
C |-> DOFSR - FSR generation                                            FERMIS42
C     |-> FINALG - FSR generation                                       FERMIS43
C |-> ACCEPT - check if generated 4-vectors acceptable                  FERMIS44
C |-> RAMDEN - p.s. density for RAMBO                                   FERMIS45
C |-> DENSAN - p.s. density for ANNIHI                                  FERMIS46
C |-> DENSBF - p.s. density for BREMF                                   FERMIS47
C |-> DENSBB - p.s. density for BREMB                                   FERMIS48
C |-> DENSC1 - p.s. density for CONVS1                                  FERMIS49
C |-> DENSC2 - p.s. density for CONVS2                                  FERMIS50
C |-> DENC1Z - p.s. density for CONV1Z                                  FERMIS51
C |-> DENC2Z - p.s. density for CONV2Z                                  FERMIS52
C |-> DENSMU - p.s. density for MULTIP                                  FERMIS53
C SCTOVE - scalar to vector translation                                 FERMIS54
C XSECN  - event cross section                                          FERMIS55
C |-> DIAGFV - compute the feynman diagram for a given helicity configurFERMIS56
C     |-> GFUNV compute the functions H1,H2,H3,H4                       FERMIS57
C |-> STATS - Update statistics and record kinematics of extreme weight FERMIS58
C BOOKEM - fill all the histograms with this event's info               FERMIS59
C |-> HISTO1 - put entry into a histogram                               FERMIS60
C WRITE - write out unweighted events                                   FERMIS61
C FINISH - print out cross sections and other stats                     FERMIS62
C |-> HISTO2 - print out histograms                                     FERMIS63
C                                                                       FERMIS64
C J.H. Fixes :                                                          FERMIS65
C KINT uninitialized                                                    FERMIS66
C Indices switched in QMOM(i,j,k) when calculating S( , ) etc           FERMIS67
C masses of anti-particles 4 and 6 set negative                         FERMIS68
C H2 +++- call in GFUN had the wrong sign for the last argument         FERMIS69
C Add fermi sign in XSECN                                               FERMIS70
C Particle masses, ECM in PROSET                                        FERMIS71
C Replace RANEVT with modified RAMBO                                    FERMIS72
C                                                                       FERMIS73
C Additional modifications for the CRAY:                                FERMIS74
C The CDABS, DCONJG, and DCMPLX of the original Kleiss code were not recFERMIS75
C by the CRAY compiler as intrinsic functions, hence they were replaced FERMIS76
C generic functions ABS, CONJG, and CMPLX.  On VAX the ABS and CONJG funFERMIS77
C will in any case work as desired as generic functions, BUT CMPLX DOES FERMIS78
C as intended as a generic replacement for DCMPLX.  If the arguments of FERMIS79
C are REAL*8, the complex number is only COMPLEX*8, not COMPLEX*16 as deFERMIS80
C                                                                       FERMIS81
C To aid in vectorization, an inner-most loop over MBUNCH bunches of LBUFERMIS82
C events                                                                FERMIS83
C is made, necessitating an additional argument for event-dependent variFERMIS84
C such as momenta to indicate event number.  The choice LBUNCH = 127 helFERMIS85
C use the CRAY more efficiently.                                        FERMIS86
C                                                                       FERMIS87
C ISR and FSR addition : June, 1991 by JH                               FERMIS88
      IMPLICIT NONE                                                     FERMIS89
      SAVE                                                              FERMIS90
C local variables                                                       FERMIS91
      INTEGER IREJ,NEVT5,NITER,NOUT,MBUNCH                              FERMIS92
      LOGICAL first                                                     FERMIS93
      DATA first/.TRUE./                                                FERMIS94
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C Event writing block                                                   WRITING2
      INTEGER IWRITE,CODE,NWRITN,NWRIMX                                 WRITING3
      REAL*8 XMAXWT                                                     WRITING4
      COMMON /EWRITE/ XMAXWT,IWRITE,CODE(11),NWRITN,NWRIMX              WRITING5
C Phase space weights                                                   PHASSPA2
      REAL*8 PSWT,PSWTEV,DENS                                           PHASSPA3
      COMMON /PHASEP/ PSWTEV,PSWT(LBUNCH),DENS(100,LBUNCH)              PHASSPA4
C                                                                       FERMI100
C Initializations                                                       FERMI101
C                                                                       FERMI102
      IF ( first ) THEN                                                 FERMI103
        MBUNCH = LBUNCH                                                 FERMI104
        NEVENT = 0                                                      FERMI105
        NBUNCH = 0                                                      FERMI106
        first = .FALSE.                                                 FERMI107
      ENDIF                                                             FERMI108
C                                                                       FERMI109
      NEVTIN = 0                                                        FERMI110
      NBUNCH = NBUNCH + 1                                               FERMI111
    2 CONTINUE                                                          FERMI112
C                                                                       FERMI113
C Event loop:                                                           FERMI114
C Generate enough events to make a `BUNCH'                              FERMI115
C                                                                       FERMI116
      NEVENT = NEVENT + 1                                               FERMI117
C                                                                       FERMI118
C ISR generation and related variables                                  FERMI119
C                                                                       FERMI120
      CALL DOISR                                                        FERMI121
C                                                                       FERMI122
C Phase space generator and p.s. weight.; FSR; topological rejection    FERMI123
C                                                                       FERMI124
      CALL PHASE(IREJ)                                                  FERMI125
      IF (IREJ.EQ.1)  THEN                                              FERMI126
         NEVTRJ = NEVTRJ + 1                                            FERMI127
      ELSE                                                              FERMI128
C                                                                       FERMI129
C Transfer of "scalar" variables to "vector" arrays                     FERMI130
C                                                                       FERMI131
         CALL SCTOVE                                                    FERMI132
         NEVTAC = NEVTAC + 1                                            FERMI133
         NEVTIN = NEVTIN + 1                                            FERMI134
      ENDIF                                                             FERMI135
      IF ( NEVTIN .LT. MBUNCH ) GOTO 2                                  FERMI136
C                                                                       FERMI137
C End of event loop                                                     FERMI138
C                                                                       FERMI139
C Compute the cross section of accepted events                          FERMI140
C                                                                       FERMI141
      CALL XSECN                                                        FERMI142
C                                                                       FERMI143
C Fill histograms.                                                      FERMI144
C                                                                       FERMI145
      CALL BOOKEM                                                       FERMI146
C                                                                       FERMI147
      RETURN                                                            FERMI148
      END                                                               FERMI149
      FUNCTION ACCEPT(DUM)                                              ACCEPT 2
C-------------------------------------------------------------------    ACCEPT 3
C After the 4 vectors have been generated, they must be checked against ACCEPT 4
C the cuts.  Here we have taken the trouble to check everything after   ACCEPT 5
C final- and initial-state radiation.                                   ACCEPT 6
C-------------------------------------------------------------------    ACCEPT 7
      IMPLICIT NONE                                                     ACCEPT 8
      SAVE                                                              ACCEPT 9
C Local variables                                                       ACCEPT10
      REAL*8 XMAS2,DUM,PMOM(3:11),BOOST(4)                              ACCEPT11
      INTEGER I,ICEN,IP,IPLACE                                          ACCEPT12
      LOGICAL ACCEPT                                                    ACCEPT13
C                                                                       ACCEPT14
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C setup commons                                                         SETUPCO2
      REAL*8 ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                          SETUPCO3
      COMMON /SETUP/ ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                  SETUPCO4
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C                                                                       ACCEPT21
C Check that the generated momenta are acceptable                       ACCEPT22
C                                                                       ACCEPT23
      ACCEPT = .FALSE.                                                  ACCEPT24
C                                                                       ACCEPT25
      IF (ISRFL) THEN                                                   ACCEPT26
C Rejection if Egam is above cut value                                  ACCEPT27
         IF (GAMISR(4)/EBEAM .GT. KGMAX)  GOTO 999                      ACCEPT28
C Get the boost that will put us back in the lab frame.                 ACCEPT29
         CALL GETBOO(GAMISR,EBEAM,BOOST)                                ACCEPT30
      ENDIF                                                             ACCEPT31
      ICEN = 0                                                          ACCEPT32
C Construct a consistent set of 4-momenta in the lab frame.  Put resultsACCEPT33
C into POUT.                                                            ACCEPT34
C                                                                       ACCEPT35
      DO 1 I = 3, 6+NOFSR                                               ACCEPT36
C First boost particles to lab frame in the ISR case                    ACCEPT37
         IF (ISRFL) THEN                                                ACCEPT38
            IF (I.GT.6)  THEN                                           ACCEPT39
               IPLACE = I + 1                                           ACCEPT40
            ELSE                                                        ACCEPT41
               IPLACE = I                                               ACCEPT42
            ENDIF                                                       ACCEPT43
            IF (FSRFL) THEN                                             ACCEPT44
               CALL GETLAB(BOOST,PEVTFS(1,I),POUT(1,IPLACE))            ACCEPT45
            ELSE                                                        ACCEPT46
               CALL GETLAB(BOOST,PEVT(1,I),POUT(1,IPLACE))              ACCEPT47
            ENDIF                                                       ACCEPT48
         ELSEIF ((.NOT.ISRFL).AND.FSRFL) THEN                           ACCEPT49
            DO 2 IP = 1, 4                                              ACCEPT50
               POUT(IP,I) = PEVTFS(IP,I)                                ACCEPT51
 2          CONTINUE                                                    ACCEPT52
         ELSE                                                           ACCEPT53
C NO FSR, NO ISR :                                                      ACCEPT54
            DO 3 IP = 1, 4                                              ACCEPT55
               POUT(IP,I) = PEVT(IP,I)                                  ACCEPT56
 3          CONTINUE                                                    ACCEPT57
         ENDIF                                                          ACCEPT58
         IF (I.LE.6) THEN                                               ACCEPT59
            PMOM(I) = DSQRT(POUT(4,I)**2-MASS2(I))                      ACCEPT60
            IF (DABS(POUT(3,I)/PMOM(I)) .LE. CSCEN                      ACCEPT61
C No angular cut for neutrinos                                          ACCEPT62
     &              .OR. DCHA(I).EQ.0.D0                                ACCEPT63
C No angular cut for quarks                                             ACCEPT64
     &              .OR. kindqq(i) .NE. 0 )  ICEN = ICEN + 1            ACCEPT65
         ENDIF                                                          ACCEPT66
 1    CONTINUE                                                          ACCEPT67
      IF (ICEN.LT.NTKCEN) GOTO 999                                      ACCEPT68
C put the ISR photon in POUT if there is one                            ACCEPT69
      IF (ISRFL) THEN                                                   ACCEPT70
         DO 11 IP = 1, 4                                                ACCEPT71
            POUT(IP,LGMISR) = GAMISR(IP)                                ACCEPT72
 11      CONTINUE                                                       ACCEPT73
      ENDIF                                                             ACCEPT74
C                                                                       ACCEPT75
      IF (XMAS2(3,4).LE.X2MN34 .OR. XMAS2(3,4).GE.X2MX34)  GOTO 999     ACCEPT76
      IF (PMOM(3).LE.XMOM34 .OR. PMOM(4).LE.XMOM34)    GOTO 999         ACCEPT77
C                                                                       ACCEPT78
C Use X2MN34 as the single minimum mass requirement if there are 2 pairsACCEPT79
C identical outgoing particles, and XMOM34 as the single momentum requirACCEPT80
C                                                                       ACCEPT81
      IF (FLA(3).EQ.FLA(5)) THEN                                        ACCEPT82
         IF (XMAS2(5,6).LE.X2MN34 .OR. XMAS2(5,6).GE.X2MX34)  GOTO 999  ACCEPT83
         IF (XMAS2(3,6).LE.X2MN34 .OR. XMAS2(3,6).GE.X2MX34)  GOTO 999  ACCEPT84
         IF (XMAS2(5,4).LE.X2MN34 .OR. XMAS2(5,4).GE.X2MX34)  GOTO 999  ACCEPT85
         IF (PMOM(5).LE.XMOM34 .OR. PMOM(6).LE.XMOM34)    GOTO 999      ACCEPT86
      ELSE                                                              ACCEPT87
         IF (XMAS2(5,6).LE.X2MN56 .OR. XMAS2(5,6).GE.X2MX56)  GOTO 999  ACCEPT88
         IF (PMOM(5).LE.XMOM56 .OR. PMOM(6).LE.XMOM56)    GOTO 999      ACCEPT89
      ENDIF                                                             ACCEPT90
C                                                                       ACCEPT91
C All checks passed, accept event.                                      ACCEPT92
      ACCEPT = .TRUE.                                                   ACCEPT93
  999 CONTINUE                                                          ACCEPT94
      END                                                               ACCEPT95
      SUBROUTINE ANNIHI(SPMIN,SPMAX,STRP,P1,P2,Q1,Q2,IACC)              ANNIHI 2
C----------------------------------------------------------             ANNIHI 3
C Generate phase space suited for an annihilation event.  Assume p1+p2  ANNIHI 4
C is peaked at low invariant mass, and that p1+p2+q1 is also            ANNIHI 5
C peaked at low invariant mass.                                         ANNIHI 6
C This is the routine designed for 1/s behaviour.                       ANNIHI 7
C INPUTS :                                                              ANNIHI 8
C  SPMIN  - minimum value for (p1 + p2)**2                              ANNIHI 9
C  SPMAX   - maximum value for (p1 + p2)**2                             ANNIHI10
C  STRP   - maximum value for (p1 + p2 + q1)**2 - MQ**2                 ANNIHI11
C  P1,P2,Q1,Q2 - 4-momentum labels                                      ANNIHI12
C OUTPUT :                                                              ANNIHI13
C  IACC     = 0 if event not accepted                                   ANNIHI14
C                                                                       ANNIHI15
C Modification : Implement the R ratio for a qqbar pair                 ANNIHI16
C                Patrick Janot -- 05 Apr 1994                           ANNIHI17
C----------------------------------------------------------             ANNIHI18
      IMPLICIT NONE                                                     ANNIHI19
      SAVE                                                              ANNIHI20
C Local variables                                                       ANNIHI21
      REAL*8 SPMIN,SPMAX,STRP,SP,                                       ANNIHI22
     &   SQ,PCM(4),Q3BOD(4),Q2BOD(4),LAMBDA,LAM,B1,C,                   ANNIHI23
     &   SQMIN,SQS,SPQ,MQS,MQQ,LAM1,LAM2,CSTH,                          ANNIHI24
     &   HILGTX                                                         ANNIHI25
      INTEGER P1,P2,Q1,Q2,IACC                                          ANNIHI26
      LOGICAL DECAY2,DECA2X                                             ANNIHI27
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C Power laws for various distributions in our ps generators             POWERLW2
      REAL*8 ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ,BBCS2,   POWERLW3
     &       BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF             POWERLW4
      COMMON /DPWRS/ ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ, POWERLW5
     &      BBCS2, BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF       POWERLW6
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C setup commons                                                         SETUPCO2
      REAL*8 ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                          SETUPCO3
      COMMON /SETUP/ ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                  SETUPCO4
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
      LAMBDA(B1,C) = 1.D0 + B1*B1 + C*C - 2.D0*(B1 + C + B1*C)          ANNIHI35
C                                                                       ANNIHI36
C Initialiation                                                         ANNIHI37
      PCM(1) = 0.D0                                                     ANNIHI38
      PCM(2) = 0.D0                                                     ANNIHI39
      PCM(3) = 0.D0                                                     ANNIHI40
      PCM(4) = ECMISR                                                   ANNIHI41
      IACC = 0                                                          ANNIHI42
C                                                                       ANNIHI43
C First choose SP, the invariant mass**2 of the (p1,p2) system, assumingANNIHI44
C a 1/SP**ANSP behavior                                                 ANNIHI45
C                                                                       ANNIHI46
C This behaviour is multiplied by the ratio R (e+e- -> qqbar) in case ofANNIHI47
C a qqbar system (P. Janot)                                             ANNIHI48
C                                                                       ANNIHI49
      IF ( kindqq(p1) .EQ. 0 .AND. kindqq(p2) .EQ. 0 ) THEN             ANNIHI50
        SP = HILGTX(0.D0,ANSP,SPMIN,SPMAX)                              ANNIHI51
      ELSE                                                              ANNIHI52
        CALL qcd2(kindqq(p1),1,spmin,spmax,sp)                          ANNIHI53
      ENDIF                                                             ANNIHI54
C                                                                       ANNIHI55
C Next choose SQ, the inv. mass**2 of the (q1,p1,p2) system, assuming a ANNIHI56
C 1/SQ**ANSQ behavior                                                   ANNIHI57
      SQMIN = (DSQRT(SP)+DMASS(Q1))**2                                  ANNIHI58
      SQ = HILGTX(0.D0,ANSQ,SQMIN,STRP)                                 ANNIHI59
C We now treat the first 2-body problem : q2 against (q1+p1+p2)         ANNIHI60
      IF (.NOT.DECAY2(PCM,SISR,MASS2(Q2),SQ,PEVT(1,Q2),Q3BOD,LAM1))     ANNIHI61
     &   GOTO 999                                                       ANNIHI62
C                                                                       ANNIHI63
C Next choose cos(theta), the angle of decay of the sp system w/r/t     ANNIHI64
C the sq system's direction in the lab frame.                           ANNIHI65
      SQS = SQ/SISR                                                     ANNIHI66
      SPQ = SP/SQ                                                       ANNIHI67
      MQS = MASS2(Q1)/SISR                                              ANNIHI68
      MQQ = MASS2(Q1)/SQ                                                ANNIHI69
      LAM2 = LAMBDA(SPQ,MQQ)                                            ANNIHI70
      C = (1.D0+SQS-MQS)*(1.D0+SPQ-MQQ) /                               ANNIHI71
     &   (DSQRT(LAM1*LAM2))                                             ANNIHI72
C Throw costh of sp system as 1/(C+costh)**ANCOST                       ANNIHI73
      CSTH = HILGTX(C,ANCOST,-1.D0,1.D0)                                ANNIHI74
C                                                                       ANNIHI75
C We now treat the 2nd 2-body problem : q1 against (p1+p2), using a specANNIHI76
C routine                                                               ANNIHI77
      IF (.NOT.DECA2X(CSTH,Q3BOD,SQ,MASS2(Q1),SP,PEVT(1,Q1),Q2BOD))     ANNIHI78
     &   GOTO 999                                                       ANNIHI79
C                                                                       ANNIHI80
C And now the last 2-body problem : p1 against p2                       ANNIHI81
      IF (.NOT.DECAY2(Q2BOD,SP,MASS2(P1),MASS2(P2),PEVT(1,P1),          ANNIHI82
     &   PEVT(1,P2),LAM))  GOTO 999                                     ANNIHI83
C                                                                       ANNIHI84
C Printout                                                              ANNIHI85
      IF (NEVTIN.LT.NEVPRI.AND.NBUNCH.EQ.1.AND.DEBGLV.GE.1) THEN        ANNIHI86
         PRINT'(/'' Annihi. SP,SQ,SQRT(SP),SQRT(SQ)'',4F9.2)',SP,SQ,    ANNIHI87
     &      SQRT(SP),SQRT(SQ)                                           ANNIHI88
      ENDIF                                                             ANNIHI89
      IACC = 1                                                          ANNIHI90
  999 CONTINUE                                                          ANNIHI91
      END                                                               ANNIHI92
      SUBROUTINE BOOKEM                                                 BOOKEM 2
C--------------------------------------------------------               BOOKEM 3
C Make histogram entries after an accepted event                        BOOKEM 4
C--------------------------------------------------------               BOOKEM 5
      IMPLICIT NONE                                                     BOOKEM 6
      SAVE                                                              BOOKEM 7
C local variables                                                       BOOKEM 8
      INTEGER ID,I,IN,NBIN,ITYP,IP                                      BOOKEM 9
      REAL*8 QSUM34(4),QSUM56(4),QSUM36(4),QSUM45(4),DOT3,PTOT3,        BOOKEM10
     &   CS34,CS56,CS36,CS45,RN,EMAX                                    BOOKEM11
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C Power laws for various distributions in our ps generators             POWERLW2
      REAL*8 ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ,BBCS2,   POWERLW3
     &       BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF             POWERLW4
      COMMON /DPWRS/ ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ, POWERLW5
     &      BBCS2, BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF       POWERLW6
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
C setup commons                                                         SETUPCO2
      REAL*8 ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                          SETUPCO3
      COMMON /SETUP/ ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                  SETUPCO4
      DATA NBIN /20/                                                    BOOKEM19
C                                                                       BOOKEM20
      DO 1 IN = 1, NEVTIN                                               BOOKEM21
      CALL HISTO1(1,NBIN,0.D0,100.D0,CROSS(IN),1.D0)                    BOOKEM22
C                                                                       BOOKEM23
      IF (FLA(3).EQ.FLA(5)) THEN                                        BOOKEM24
         ID = 0                                                         BOOKEM25
      ELSE                                                              BOOKEM26
         ID = 1                                                         BOOKEM27
      ENDIF                                                             BOOKEM28
      CALL HISTO1(2,NBIN,0.D0,SECM,D(IN,3,4)+MASS2(3)+MASS2(4),         BOOKEM29
     &      CROSS(IN))                                                  BOOKEM30
      CALL HISTO1(2+ID,NBIN,0.D0,SECM,D(IN,5,6)+MASS2(5)+MASS2(6),      BOOKEM31
     &   CROSS(IN))                                                     BOOKEM32
      IF (FLA(3).EQ.FLA(5)) THEN                                        BOOKEM33
         CALL HISTO1(2,NBIN,0.D0,SECM,D(IN,3,6)+MASS2(3)+MASS2(6),      BOOKEM34
     &      CROSS(IN))                                                  BOOKEM35
         CALL HISTO1(2,NBIN,0.D0,SECM,D(IN,4,5)+MASS2(4)+MASS2(5),      BOOKEM36
     &      CROSS(IN))                                                  BOOKEM37
      ENDIF                                                             BOOKEM38
C                                                                       BOOKEM39
C                                                                       BOOKEM40
      CALL HISTO1(4,NBIN,0.D0,QMOM(IN,4,1),QMOM(IN,4,3),CROSS(IN))      BOOKEM41
      CALL HISTO1(4,NBIN,0.D0,QMOM(IN,4,1),QMOM(IN,4,4),CROSS(IN))      BOOKEM42
      CALL HISTO1(4+ID,NBIN,0.D0,QMOM(IN,4,1),QMOM(IN,4,5),CROSS(IN))   BOOKEM43
      CALL HISTO1(4+ID,NBIN,0.D0,QMOM(IN,4,1),QMOM(IN,4,6),CROSS(IN))   BOOKEM44
C                                                                       BOOKEM45
C angle between a ferm-anti-ferm pair and nearest track                 BOOKEM46
      DO 11 I = 1, 4                                                    BOOKEM47
         QSUM34(I) = QMOM(IN,I,3) + QMOM(IN,I,4)                        BOOKEM48
         QSUM56(I) = QMOM(IN,I,5) + QMOM(IN,I,6)                        BOOKEM49
         IF (FLA(3).EQ.FLA(5)) THEN                                     BOOKEM50
            QSUM36(I) = QMOM(IN,I,3) + QMOM(IN,I,6)                     BOOKEM51
            QSUM45(I) = QMOM(IN,I,4) + QMOM(IN,I,5)                     BOOKEM52
         ENDIF                                                          BOOKEM53
   11 CONTINUE                                                          BOOKEM54
C                                                                       BOOKEM55
      CS34 = DMAX1(DOT3(QSUM34,QMOM(IN,1,5))/PTOT3(QSUM34)/             BOOKEM56
     &   PTOT3(QMOM(IN,1,5)),DOT3(QSUM34,QMOM(IN,1,6))/PTOT3(QSUM34)/   BOOKEM57
     &   PTOT3(QMOM(IN,1,6)) )                                          BOOKEM58
      CS56 = DMAX1(DOT3(QSUM56,QMOM(IN,1,3))/PTOT3(QSUM56)/             BOOKEM59
     &   PTOT3(QMOM(IN,1,3)),                                           BOOKEM60
     &   DOT3(QSUM56,QMOM(IN,1,3))/PTOT3(QSUM56)/PTOT3(QMOM(IN,1,4)) )  BOOKEM61
C                                                                       BOOKEM62
      CALL HISTO1(6,NBIN,-1.D0,1.D0,CS34,CROSS(IN))                     BOOKEM63
      CALL HISTO1(6+ID,NBIN,-1.D0,1.D0,CS56,CROSS(IN))                  BOOKEM64
      IF (FLA(3).EQ.FLA(5)) THEN                                        BOOKEM65
         CS36 = DMAX1(DOT3(QSUM36,QMOM(IN,1,5))/PTOT3(QSUM36)/          BOOKEM66
     &      PTOT3(QMOM(IN,1,5))                                         BOOKEM67
     &   , DOT3(QSUM36,QMOM(IN,1,4))/PTOT3(QSUM36)/PTOT3(QMOM(IN,1,4))) BOOKEM68
         CS45 = DMAX1(DOT3(QSUM45,QMOM(IN,1,3))/PTOT3(QSUM45)/          BOOKEM69
     &      PTOT3(QMOM(IN,1,3))                                         BOOKEM70
     &   , DOT3(QSUM45,QMOM(IN,1,6))/PTOT3(QSUM45)/PTOT3(QMOM(IN,1,6))) BOOKEM71
         CALL HISTO1(6,NBIN,-1.D0,1.D0,CS36,CROSS(IN))                  BOOKEM72
         CALL HISTO1(6,NBIN,-1.D0,1.D0,CS45,CROSS(IN))                  BOOKEM73
      ENDIF                                                             BOOKEM74
C                                                                       BOOKEM75
C photon variables                                                      BOOKEM76
      IF (ISRFL)                                                        BOOKEM77
     &   CALL HISTO1(8,NBIN,0.D0,EBEAM,QMOM(IN,4,LGMISR),CROSS(IN))     BOOKEM78
C behavior of typical and high-energy photon                            BOOKEM79
      IF (FSRFL) THEN                                                   BOOKEM80
         ITYP = NOFSR*RN(1) + NOPART - NOFSR + 1                        BOOKEM81
         CALL HISTO1(9,NBIN,0.D0,EBEAM,QMOM(IN,4,ITYP),CROSS(IN))       BOOKEM82
         EMAX = -1.D0                                                   BOOKEM83
         DO 21 IP = NOPART - NOFSR + 1, NOPART                          BOOKEM84
            EMAX = MAX(EMAX,QMOM(IN,4,IP))                              BOOKEM85
 21      CONTINUE                                                       BOOKEM86
         CALL HISTO1(10,NBIN,0.D0,EBEAM,EMAX,CROSS(IN))                 BOOKEM87
      ENDIF                                                             BOOKEM88
    1 CONTINUE                                                          BOOKEM89
C                                                                       BOOKEM90
      END                                                               BOOKEM91
      SUBROUTINE BREMB(SKMIN,SKMAX,STRP,P1,P2,K1,K2,Q1,Q2,IACC)         BREMB  2
C----------------------------------------------------------             BREMB  3
C Generate phase space suited for an annihilation event.  Assume p1+p2  BREMB  4
C is peaked at low invariant mass, and that p1+p2+q1 is also            BREMB  5
C peaked at low invariant mass.                                         BREMB  6
C This is the routine designed for radiation off a final leg.           BREMB  7
C INPUTS :                                                              BREMB  8
C  P1,P2,K1,K2,Q1,Q2 - 4-momentum labels                                BREMB  9
C SKMIN - min. (k1,k2) mass                                             BREMB 10
C SKMAX - max. (k1,k2) mass                                             BREMB 11
C OUTPUT :                                                              BREMB 12
C  IACC     = 0 if event not accepted                                   BREMB 13
C                                                                       BREMB 14
C Modification : Implement the R ratio for a qqbar pair                 BREMB 15
C                Patrick Janot -- 05 Apr 1994                           BREMB 16
C----------------------------------------------------------             BREMB 17
      IMPLICIT NONE                                                     BREMB 18
      SAVE                                                              BREMB 19
C Local variables                                                       BREMB 20
      REAL*8 SKMIN,SKMAX,STRP,PCM(4),TWOPI,SK,SQMIN,SQ,                 BREMB 21
     &    C,CSTH,CSTH2,PHI,KVEC(4),PHI2,SNTH2,Q20,Q2MOM,                BREMB 22
     &    Q3BOD(4),RN,LAM1,HILGTX                                       BREMB 23
      INTEGER P1,P2,K1,K2,Q1,Q2,IACC,IONC,ID                            BREMB 24
      LOGICAL DECAY2,DECA2X                                             BREMB 25
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C Power laws for various distributions in our ps generators             POWERLW2
      REAL*8 ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ,BBCS2,   POWERLW3
     &       BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF             POWERLW4
      COMMON /DPWRS/ ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ, POWERLW5
     &      BBCS2, BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF       POWERLW6
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C setup commons                                                         SETUPCO2
      REAL*8 ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                          SETUPCO3
      COMMON /SETUP/ ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                  SETUPCO4
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
      DATA IONC /0/                                                     BREMB 33
C                                                                       BREMB 34
C Initialization                                                        BREMB 35
      IF (IONC.EQ.0) THEN                                               BREMB 36
         PCM(1) = 0.D0                                                  BREMB 37
         PCM(2) = 0.D0                                                  BREMB 38
         PCM(3) = 0.D0                                                  BREMB 39
         TWOPI=8.D0*DATAN(1.D0)                                         BREMB 40
         IONC = 1                                                       BREMB 41
      ENDIF                                                             BREMB 42
      PCM(4) = ECMISR                                                   BREMB 43
      IACC = 0                                                          BREMB 44
C                                                                       BREMB 45
C First choose SK, the invariant mass**2 of the (k1,k2) system, assumingBREMB 46
C a 1/SK**BBSK behavior                                                 BREMB 47
C                                                                       BREMB 48
C This behaviour is multiplied by the ratio R (e+e- -> qqbar) in case ofBREMB 49
C a qqbar system (P. Janot)                                             BREMB 50
C                                                                       BREMB 51
      IF ( kindqq(k1) .EQ. 0 .AND. kindqq(k2) .EQ. 0 ) THEN             BREMB 52
        SK = HILGTX(0.D0,BBSK,SKMIN,SKMAX)                              BREMB 53
      ELSE                                                              BREMB 54
        CALL qcd2(kindqq(k1),3,skmin,skmax,sk)                          BREMB 55
      ENDIF                                                             BREMB 56
C Next  choose SQ, the invariant mass**2 of the (k1,k2,q1) system, assumBREMB 57
C a 1/SQ**BBSQ behavior                                                 BREMB 58
      SQMIN = (DSQRT(SK)+DMASS(Q1))**2                                  BREMB 59
      SQ = HILGTX(0.D0,BBSQ,SQMIN,STRP)                                 BREMB 60
C Finally choose costh2, the angle between (p2,q2) as 1/(1-costh2)**BBCSBREMB 61
C with CSCNEV as cutoff                                                 BREMB 62
      CSTH2 = -HILGTX(1.D0,BBCS2,CSCNEV,-CSCNEV)                        BREMB 63
C Put CSTH2 into lab system:                                            BREMB 64
      IF (PEVT(3,P2).LT.0.D0)  CSTH2 = -CSTH2                           BREMB 65
C Choose phi2, the azimuthal angle of q2                                BREMB 66
      PHI2 = TWOPI*RN(4)                                                BREMB 67
C                                                                       BREMB 68
C Next choose costh, the angle between the (k1,k2) system and particle qBREMB 69
C according to the distribution 1/(C + costh)**BBCOST, in the SQ rest frBREMB 70
      C = (SISR+SQ)*(SQ+SK)/((SISR-SQ)*(SQ-SK))                         BREMB 71
      CSTH = HILGTX(C,BBCOST,-1.D0,1.D0)                                BREMB 72
C                                                                       BREMB 73
C Find energy of q2 and build its 4-vector.                             BREMB 74
      SNTH2 = DSQRT(1.D0-CSTH2*CSTH2)                                   BREMB 75
      Q20 = (SISR+MASS2(Q2)-SQ)/(2.D0*ECMISR)                           BREMB 76
      Q2MOM = DSQRT(Q20*Q20-MASS2(Q2))                                  BREMB 77
      PEVT(1,Q2) = Q2MOM*SNTH2*COS(PHI2)                                BREMB 78
      PEVT(2,Q2) = Q2MOM*SNTH2*SIN(PHI2)                                BREMB 79
      PEVT(3,Q2) = Q2MOM*CSTH2                                          BREMB 80
      PEVT(4,Q2) = Q20                                                  BREMB 81
C                                                                       BREMB 82
C Build 3-body vector                                                   BREMB 83
      DO 10 ID = 1, 4                                                   BREMB 84
 10      Q3BOD(ID) = PCM(ID) - PEVT(ID,Q2)                              BREMB 85
C We now treat a 2-body problem : q1 against (k1+k2), using a special   BREMB 86
C routine                                                               BREMB 87
      IF (.NOT.DECA2X(CSTH,Q3BOD,SQ,MASS2(Q1),SK,PEVT(1,Q1),KVEC))      BREMB 88
     &   GOTO 999                                                       BREMB 89
C                                                                       BREMB 90
C Now choose phi, the azimuthl angle of the (k1,k2) system, with a flat BREMB 91
C probability                                                           BREMB 92
      PHI = TWOPI*RN(6)                                                 BREMB 93
C                                                                       BREMB 94
C Decay KVEC uniformly:                                                 BREMB 95
      IF (.NOT.DECAY2(KVEC,SK,MASS2(K1),MASS2(K2),PEVT(1,K1),           BREMB 96
     &   PEVT(1,K2),LAM1)) GOTO 999                                     BREMB 97
C                                                                       BREMB 98
C 4-momenta of q1 can be built from energy-momentum conservation        BREMB 99
      DO 11 ID = 1, 4                                                   BREMB100
   11    PEVT(ID,Q1) = Q3BOD(ID) - KVEC(ID)                             BREMB101
C                                                                       BREMB102
      IACC = 1                                                          BREMB103
C Printout                                                              BREMB104
      IF (NEVTIN.LT.NEVPRI.AND.NBUNCH.EQ.1.AND.DEBGLV.GE.1)             BREMB105
     &   PRINT'(/'' BREMS. SK,SQ,CS(q1,K),CS(p2,q2)'',4F9.2)',SK,SQ,    BREMB106
     &      CSTH,CSTH2                                                  BREMB107
  999 CONTINUE                                                          BREMB108
      END                                                               BREMB109
      SUBROUTINE BREMF(SKMIN,SKMAX,P1,P2,K1,K2,Q1,Q2,IACC)              BREMF  2
C----------------------------------------------------------             BREMF  3
C Generate phase space suited for an annihilation event.  Assume p1+p2  BREMF  4
C is peaked at low invariant mass, and that p1+p2+q1 is also            BREMF  5
C peaked at low invariant mass.                                         BREMF  6
C This is the routine designed for radiation off an initial leg.        BREMF  7
C INPUTS :                                                              BREMF  8
C  P1,P2,K1,K2,Q1,Q2 - 4-momentum labels                                BREMF  9
C SKMIN - min. (k1,k2) mass                                             BREMF 10
C SKMAX - max. (k1,k2) mass                                             BREMF 11
C OUTPUT :                                                              BREMF 12
C  IACC     = 0 if event not accepted                                   BREMF 13
C                                                                       BREMF 14
C Modification : Implement the R ratio for a qqbar pair                 BREMF 15
C                Patrick Janot -- 05 Apr 1994                           BREMF 16
C----------------------------------------------------------             BREMF 17
      IMPLICIT NONE                                                     BREMF 18
      SAVE                                                              BREMF 19
C Local variables                                                       BREMF 20
      REAL*8 RN,K,K0,KMOM,TWOPI,KVEC(4),                                BREMF 21
     &   PHI,PHI2,CSTH,CSTH2,LAM1,CSQ2K,SNTH,SNTH2,Q2MOM,Q20,           BREMF 22
     &   PCM(4),SK,C,SKMIN,SKMAX,CSTHO,CSTH2O,K0MAX,K01MAX,CSTHMN,BETA2,BREMF 23
     &   THK1,THKMN,K02,HILGTX                                          BREMF 24
      INTEGER IONC,P1,P2,K1,K2,Q1,Q2,IACC,ID                            BREMF 25
      LOGICAL DECAY2                                                    BREMF 26
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C Power laws for various distributions in our ps generators             POWERLW2
      REAL*8 ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ,BBCS2,   POWERLW3
     &       BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF             POWERLW4
      COMMON /DPWRS/ ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ, POWERLW5
     &      BBCS2, BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF       POWERLW6
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C setup commons                                                         SETUPCO2
      REAL*8 ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                          SETUPCO3
      COMMON /SETUP/ ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                  SETUPCO4
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
      DATA IONC /0/                                                     BREMF 34
C                                                                       BREMF 35
C Initialization                                                        BREMF 36
      IF (IONC.EQ.0) THEN                                               BREMF 37
         PCM(1) = 0.D0                                                  BREMF 38
         PCM(2) = 0.D0                                                  BREMF 39
         PCM(3) = 0.D0                                                  BREMF 40
         TWOPI=8.D0*DATAN(1.D0)                                         BREMF 41
         IONC = 1                                                       BREMF 42
      ENDIF                                                             BREMF 43
      PCM(4) = ECMISR                                                   BREMF 44
      IACC = 0                                                          BREMF 45
C                                                                       BREMF 46
C Assume 1/SK**BFSK    behaviour                                        BREMF 47
C                                                                       BREMF 48
C This behaviour is multiplied by the ratio R (e+e- -> qqbar) in case ofBREMF 49
C a qqbar system (P. Janot)                                             BREMF 50
C                                                                       BREMF 51
      IF ( kindqq(k1) .EQ. 0 .AND. kindqq(k2) .EQ. 0 ) THEN             BREMF 52
        SK = HILGTX(0.D0,BFSK,SKMIN,SKMAX)                              BREMF 53
      ELSE                                                              BREMF 54
        CALL qcd2(kindqq(k1),2,skmin,skmax,sk)                          BREMF 55
      ENDIF                                                             BREMF 56
C                                                                       BREMF 57
C Next choose K0, the energy of the (k1,k2) system, assuming a 1/K0**BFKBREMF 58
C behavior                                                              BREMF 59
C The choice of limits must be done with care.                          BREMF 60
      K01MAX = (SISR+SK-Q2MN)/(2.D0*ECMISR)                             BREMF 61
      K = DSQRT(SK)                                                     BREMF 62
      K0MAX = KBOD2*(K01MAX - K) + K                                    BREMF 63
      K0 = HILGTX(0.D0,BFK0,K,K0MAX)                                    BREMF 64
C                                                                       BREMF 65
C Throw costh s.t. angle  of (k1,k2) system goes as 1/(C - COSTH)**BFCOSBREMF 66
C First find efficient limits                                           BREMF 67
      K02 = K0*K0                                                       BREMF 68
      KMOM = DSQRT(K02 - SK)                                            BREMF 69
      C = (K0*ECMISR - SK)/(KMOM*ECMISR)                                BREMF 70
      BETA2 = 1.D0 - 4.D0*MASS2(K1)/K02                                 BREMF 71
      THK1 = THCNEV - 0.5D0*ACOS(1.D0/BETA2*(1.D0 -                     BREMF 72
     &    2.D0*(SK - 2.D0*MASS2(K1))/K02))                              BREMF 73
      THKMN = MAX(THK1,0.D0)                                            BREMF 74
      CSTHMN = COS(THKMN)                                               BREMF 75
C If final state is neutral, remove cos theta cutoff                    BREMF 76
      IF (DCHA(K1).EQ.0.D0)  CSTHMN = 0.999D0                           BREMF 77
      CSTH = -HILGTX(C,BFCOST,CSTHMN,-CSTHMN)                           BREMF 78
C Finally choose costh2, the angle between (p2,q2) as 1/(1-costh2)**BFCSBREMF 79
C with CSCNEV as cutoff                                                 BREMF 80
      CSTH2 = -HILGTX(1.D0,BFCS2,CSCNEV,-CSCNEV)                        BREMF 81
C                                                                       BREMF 82
C Put CSTH and CSTH2 into the same system: one gets rotated by 180 degreBREMF 83
      CSTHO = CSTH                                                      BREMF 84
      CSTH2O = CSTH2                                                    BREMF 85
      IF (PEVT(3,P1).LT.0.D0) THEN                                      BREMF 86
         CSTH  = -CSTH                                                  BREMF 87
      ELSE                                                              BREMF 88
         CSTH2 = -CSTH2                                                 BREMF 89
      ENDIF                                                             BREMF 90
C                                                                       BREMF 91
C Now choose phi, the azimuthl angle of the (k1,k2) system, with a flat BREMF 92
C probability                                                           BREMF 93
      PHI = TWOPI*RN(5)                                                 BREMF 94
C                                                                       BREMF 95
C Build the K vector and decay it uniformly into (k1,k2)                BREMF 96
      SNTH = DSQRT(1.D0-CSTH*CSTH)                                      BREMF 97
      KVEC(1) = KMOM*SNTH*COS(PHI)                                      BREMF 98
      KVEC(2) = KMOM*SNTH*SIN(PHI)                                      BREMF 99
      KVEC(3) = KMOM*CSTH                                               BREMF100
      KVEC(4) = K0                                                      BREMF101
      IF (.NOT.DECAY2(KVEC,SK,MASS2(K1),MASS2(K2),PEVT(1,K1),           BREMF102
     &   PEVT(1,K2),LAM1)) GOTO 999                                     BREMF103
C                                                                       BREMF104
C Choose phi2, the azimuthal angle of q2                                BREMF105
      PHI2 = TWOPI*RN(6)                                                BREMF106
C                                                                       BREMF107
C The kinematics is completely constrained now.  Find energy of q2.     BREMF108
      SNTH2 = DSQRT(1.D0-CSTH2*CSTH2)                                   BREMF109
      CSQ2K = CSTH*CSTH2 + SNTH*SNTH2*COS(PHI-PHI2)                     BREMF110
      Q20 = (SISR+SK-2.D0*ECMISR*K0)/                                   BREMF111
     &   2.D0/(ECMISR-K0+KMOM*CSQ2K)                                    BREMF112
C      IF (Q20.LE.DMASS(Q2))  GOTO 999                                  BREMF113
      Q2MOM = DSQRT(Q20*Q20-MASS2(Q2))                                  BREMF114
      PEVT(1,Q2) = Q2MOM*SNTH2*COS(PHI2)                                BREMF115
      PEVT(2,Q2) = Q2MOM*SNTH2*SIN(PHI2)                                BREMF116
      PEVT(3,Q2) = Q2MOM*CSTH2                                          BREMF117
      PEVT(4,Q2) = Q20                                                  BREMF118
C                                                                       BREMF119
C 4-momenta of q1 can be built from energy-momentum conservation        BREMF120
      DO 11 ID = 1, 4                                                   BREMF121
   11    PEVT(ID,Q1) = PCM(ID) - KVEC(ID) - PEVT(ID,Q2)                 BREMF122
C                                                                       BREMF123
      IACC = 1                                                          BREMF124
C Printout                                                              BREMF125
      IF (NEVTIN.LT.NEVPRI.AND.NBUNCH.EQ.1.AND.DEBGLV.GE.1)             BREMF126
     &   PRINT'(/'' BREMS. SK,K0,CS(p1,K),CS(p2,q2)'',4F9.3)',SK,K0,    BREMF127
     &      CSTHO,CSTH2O                                                BREMF128
  999 CONTINUE                                                          BREMF129
      END                                                               BREMF130
      SUBROUTINE BRNCHS                                                 BRNCHS 2
C-------------------------------------------------------------------    BRNCHS 3
C Set up open channels which are used by the phase space generators.    BRNCHS 4
C-------------------------------------------------------------------    BRNCHS 5
      IMPLICIT NONE                                                     BRNCHS 6
      SAVE                                                              BRNCHS 7
C Local variables                                                       BRNCHS 8
      INTEGER I,IBR3(8),IBR4(8),IBR5(8),IBR6(8),IOFF,MOFF,IDO,IBEG,IEND,BRNCHS 9
     &   IUSE,ID,JCPS                                                   BRNCHS10
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
C BRANCH commons                                                        BRANCHC2
      INTEGER LCHMX,PAR1,PAR2,PAR3,PAR4,PAR5,PAR6,NEVTGN,NRJGN,         BRANCHC3
     &   NCHAN,NCHANT,GENRJC,LGENR,JCHAN,CHACOU,CHANLS,                 BRANCHC4
     &   LANNIH,LBREMF,LBREMB,LCONV1,LCONV2,LCON1Z,LCON2Z,LMULTI,LRAMBO BRANCHC5
      PARAMETER (LCHMX = 100, LGENR = 9, LANNIH = 1, LBREMF = 2,        BRANCHC6
     &   LBREMB = 3, LCONV1 = 4, LCONV2 = 5,  LCON1Z = 6, LCON2Z = 7,   BRANCHC7
     &   LMULTI = 8, LRAMBO =9)                                         BRANCHC8
      REAL*8 SMINP,SMAXP,SMX3,SKMIN,SKMAX,BRNCH,FRAC,CGEN               BRANCHC9
      COMMON /BRANCH/ PAR1(LCHMX),PAR2(LCHMX),PAR3(LCHMX),PAR4(LCHMX),  BRANCH10
     &   PAR5(LCHMX),PAR6(LCHMX),NEVTGN(LGENR),NRJGN(LGENR),NCHAN,      BRANCH11
     &   NCHANT,GENRJC(LCHMX),JCHAN(LBUNCH),CHACOU(0:LGENR),            BRANCH12
     &   CHANLS(LGENR),                                                 BRANCH13
     &   SKMIN,SKMAX,SMINP(LCHMX),SMAXP(LCHMX),SMX3(LCHMX),             BRANCH14
     &   BRNCH(0:LCHMX),FRAC(LCHMX),CGEN(LGENR)                         BRANCH15
      CHARACTER*16 GNNAME(LGENR)                                        BRANCH16
      COMMON /GNNAMS/ GNNAME                                            BRANCH17
C                                                                       BRNCHS14
C IBR3 and IBR4 are the momentum labels of the pair coming off the photoBRNCHS15
C The physics is invariant to their interchange.  But IBR5 is the fermioBRNCHS16
C which is to be combined with IBR3 and IBR4 to form a low-inv.-mass triBRNCHS17
      DATA IBR3 / 3, 3, 5, 5, 3, 3, 5, 5/                               BRNCHS18
      DATA IBR4 / 4, 4, 6, 6, 6, 6, 4, 4/                               BRNCHS19
      DATA IBR5 / 5, 6, 3, 4, 4, 5, 3, 6/                               BRNCHS20
      DATA IBR6 / 6, 5, 4, 3, 5, 4, 6, 3/                               BRNCHS21
C                                                                       BRNCHS22
C                                                                       BRNCHS23
C Initialization                                                        BRNCHS24
      CHACOU(0) = 0                                                     BRNCHS25
C                                                                       BRNCHS26
C First the annihilation graphs                                         BRNCHS27
      IF (FLA(3).EQ.FLA(5)) THEN                                        BRNCHS28
         CHANLS(LANNIH) = 8                                             BRNCHS29
      ELSE                                                              BRNCHS30
         CHANLS(LANNIH) = 4                                             BRNCHS31
      ENDIF                                                             BRNCHS32
      CHACOU(LANNIH) = CHACOU(LANNIH-1) + CHANLS(LANNIH)                BRNCHS33
C                                                                       BRNCHS34
      DO 11 ID = 1, CHANLS(LANNIH)                                      BRNCHS35
         I = CHACOU(LANNIH-1)+ID                                        BRNCHS36
         IUSE = ID                                                      BRNCHS37
         PAR3(I) = IBR3(IUSE)                                           BRNCHS38
         PAR4(I) = IBR4(IUSE)                                           BRNCHS39
         PAR5(I) = IBR5(IUSE)                                           BRNCHS40
         PAR6(I) = IBR6(IUSE)                                           BRNCHS41
   11 CONTINUE                                                          BRNCHS42
C                                                                       BRNCHS43
C Next Bremsstrahlung graphs                                            BRNCHS44
      IOFF = 0                                                          BRNCHS45
      IF (FLA(1).EQ.FLA(3) .AND. FLA(1).EQ.FLA(5)) THEN                 BRNCHS46
         CHANLS(LBREMF) = 8                                             BRNCHS47
      ELSEIF (FLA(1).EQ.FLA(3)) THEN                                    BRNCHS48
         CHANLS(LBREMF) = 2                                             BRNCHS49
         IOFF = 2                                                       BRNCHS50
      ELSEIF (FLA(1).EQ.FLA(5)) THEN                                    BRNCHS51
         CHANLS(LBREMF) = 2                                             BRNCHS52
      ELSE                                                              BRNCHS53
C No beam particle fermions in final state => no brems. possible        BRNCHS54
         CHANLS(LBREMF) = 0                                             BRNCHS55
      ENDIF                                                             BRNCHS56
      CHANLS(LBREMB) = CHANLS(LBREMF)                                   BRNCHS57
      CHACOU(LBREMF) = CHACOU(LBREMF-1) + CHANLS(LBREMF)                BRNCHS58
      CHACOU(LBREMB) = CHACOU(LBREMB-1) + CHANLS(LBREMB)                BRNCHS59
C                                                                       BRNCHS60
      DO 21 ID = 1, 2                                                   BRNCHS61
         IDO = CHANLS(LBREMF)*(ID-1)                                    BRNCHS62
         IBEG = CHACOU(LBREMF-1)+1+IDO                                  BRNCHS63
         IEND = IBEG + CHANLS(LBREMF) - 1                               BRNCHS64
         DO 22 I = IBEG, IEND                                           BRNCHS65
            IUSE = (I-IBEG+1) + IOFF                                    BRNCHS66
            PAR3(I) = IBR3(IUSE)                                        BRNCHS67
            PAR4(I) = IBR4(IUSE)                                        BRNCHS68
            PAR5(I) = IBR5(IUSE)                                        BRNCHS69
            PAR6(I) = IBR6(IUSE)                                        BRNCHS70
   22    CONTINUE                                                       BRNCHS71
   21 CONTINUE                                                          BRNCHS72
C                                                                       BRNCHS73
C Next the conversion graphs                                            BRNCHS74
      IF (FLA(3).EQ.FLA(5)) THEN                                        BRNCHS75
         CHANLS(LCONV1) = 4                                             BRNCHS76
      ELSE                                                              BRNCHS77
         CHANLS(LCONV1) = 2                                             BRNCHS78
      ENDIF                                                             BRNCHS79
      CHANLS(LCONV2) = CHANLS(LCONV1)                                   BRNCHS80
      CHANLS(LCON1Z) = CHANLS(LCONV1)                                   BRNCHS81
      CHANLS(LCON2Z) = CHANLS(LCONV1)                                   BRNCHS82
      CHACOU(LCONV1) = CHACOU(LCONV1-1) + CHANLS(LCONV1)                BRNCHS83
      CHACOU(LCONV2) = CHACOU(LCONV2-1) + CHANLS(LCONV2)                BRNCHS84
      CHACOU(LCON1Z) = CHACOU(LCON1Z-1) + CHANLS(LCON1Z)                BRNCHS85
      CHACOU(LCON2Z) = CHACOU(LCON2Z-1) + CHANLS(LCON2Z)                BRNCHS86
C                                                                       BRNCHS87
      DO 25 ID = 1, CHANLS(LCONV1)                                      BRNCHS88
         I = CHACOU(LCONV1-1)+ID                                        BRNCHS89
         IUSE = (ID-1)*2 + 1                                            BRNCHS90
         PAR3(I) = IBR3(IUSE)                                           BRNCHS91
         PAR4(I) = IBR4(IUSE)                                           BRNCHS92
         PAR5(I) = IBR5(IUSE)                                           BRNCHS93
         PAR6(I) = IBR6(IUSE)                                           BRNCHS94
 25   CONTINUE                                                          BRNCHS95
      DO 26 ID = 1, CHANLS(LCONV2)                                      BRNCHS96
         I = CHACOU(LCONV2-1)+ID                                        BRNCHS97
         IUSE = (ID-1)*2 + 1                                            BRNCHS98
         PAR3(I) = IBR3(IUSE)                                           BRNCHS99
         PAR4(I) = IBR4(IUSE)                                           BRNCH100
         PAR5(I) = IBR5(IUSE)                                           BRNCH101
         PAR6(I) = IBR6(IUSE)                                           BRNCH102
 26   CONTINUE                                                          BRNCH103
      DO 27 ID = 1, CHANLS(LCON1Z)                                      BRNCH104
         I = CHACOU(LCON1Z-1)+ID                                        BRNCH105
         IUSE = (ID-1)*2 + 1                                            BRNCH106
         PAR3(I) = IBR3(IUSE)                                           BRNCH107
         PAR4(I) = IBR4(IUSE)                                           BRNCH108
         PAR5(I) = IBR5(IUSE)                                           BRNCH109
         PAR6(I) = IBR6(IUSE)                                           BRNCH110
 27   CONTINUE                                                          BRNCH111
      DO 28 ID = 1, CHANLS(LCON2Z)                                      BRNCH112
         I = CHACOU(LCON2Z-1)+ID                                        BRNCH113
         IUSE = (ID-1)*2 + 1                                            BRNCH114
         PAR3(I) = IBR3(IUSE)                                           BRNCH115
         PAR4(I) = IBR4(IUSE)                                           BRNCH116
         PAR5(I) = IBR5(IUSE)                                           BRNCH117
         PAR6(I) = IBR6(IUSE)                                           BRNCH118
 28   CONTINUE                                                          BRNCH119
C                                                                       BRNCH120
C Next the multiperipheral graphs                                       BRNCH121
      MOFF = 0                                                          BRNCH122
      IF (FLA(1).EQ.FLA(3) .AND. FLA(1).EQ.FLA(5)) THEN                 BRNCH123
         CHANLS(LMULTI) = 4                                             BRNCH124
      ELSEIF (FLA(1).EQ.FLA(3)) THEN                                    BRNCH125
         CHANLS(LMULTI) = 1                                             BRNCH126
         MOFF = 2                                                       BRNCH127
      ELSEIF (FLA(1).EQ.FLA(5)) THEN                                    BRNCH128
         CHANLS(LMULTI) = 1                                             BRNCH129
      ELSE                                                              BRNCH130
         CHANLS(LMULTI) = 0                                             BRNCH131
      ENDIF                                                             BRNCH132
      CHACOU(LMULTI) = CHACOU(LMULTI-1) + CHANLS(LMULTI)                BRNCH133
      DO 31 ID = 1, CHANLS(LMULTI)                                      BRNCH134
         I = CHACOU(LMULTI-1)+ID                                        BRNCH135
         IUSE = (ID-1)*2 + 1 + MOFF                                     BRNCH136
         PAR3(I) = IBR3(IUSE)                                           BRNCH137
         PAR4(I) = IBR4(IUSE)                                           BRNCH138
         PAR5(I) = IBR5(IUSE)                                           BRNCH139
         PAR6(I) = IBR6(IUSE)                                           BRNCH140
 31   CONTINUE                                                          BRNCH141
C                                                                       BRNCH142
C RAMBO - one channel only                                              BRNCH143
      CHANLS(LRAMBO) = 1                                                BRNCH144
      CHACOU(LRAMBO) = CHACOU(LRAMBO-1) + CHANLS(LRAMBO)                BRNCH145
C                                                                       BRNCH146
      IF (FLA(3).EQ.FLA(1) .OR. FLA(5).EQ.FLA(1)) THEN                  BRNCH147
C                                                                       BRNCH148
C Also properly identify the first two particles.                       BRNCH149
         DO 51 I = CHACOU(LBREMF-1)+1, CHACOU(LBREMB)                   BRNCH150
            IF (MOD(PAR5(I),2).EQ.0) THEN                               BRNCH151
               PAR1(I) = 1                                              BRNCH152
               PAR2(I) = 2                                              BRNCH153
            ELSE                                                        BRNCH154
               PAR1(I) = 2                                              BRNCH155
               PAR2(I) = 1                                              BRNCH156
            ENDIF                                                       BRNCH157
            IF (I.LE.CHACOU(LBREMF))  GOTO 51                           BRNCH158
   51    CONTINUE                                                       BRNCH159
      ENDIF                                                             BRNCH160
C                                                                       BRNCH161
C Switch for phase-space generator                                      BRNCH162
      NCHANT = CHACOU(LGENR)                                            BRNCH163
      DO 75 JCPS = 1, NCHANT                                            BRNCH164
         DO 76 I = 1, LGENR-1                                           BRNCH165
            IF (JCPS.LE.CHACOU(I))  THEN                                BRNCH166
               GENRJC(JCPS) = I                                         BRNCH167
               GOTO 75                                                  BRNCH168
            ELSE                                                        BRNCH169
               GENRJC(JCPS) = LGENR                                     BRNCH170
            ENDIF                                                       BRNCH171
 76      CONTINUE                                                       BRNCH172
 75   CONTINUE                                                          BRNCH173
C                                                                       BRNCH174
      END                                                               BRNCH175
      SUBROUTINE CONV1Z(K1,K2,Q1,Q2,IACC)                               CONV1Z 2
C----------------------------------------------------------             CONV1Z 3
C Conversion phase space generator (one cat whisker topology, radiating CONV1Z 4
C down to make s' = Mz                                                  CONV1Z 5
C JH w/ RK, 11/91                                                       CONV1Z 6
C                                                                       CONV1Z 7
C Modification : Implement the R ratio for a qqbar pair                 CONV1Z 8
C                Patrick Janot -- 05 Apr 1994                           CONV1Z 9
C----------------------------------------------------------             CONV1Z10
      IMPLICIT NONE                                                     CONV1Z11
      SAVE                                                              CONV1Z12
C local variables                                                       CONV1Z13
      INTEGER K1,K2,Q1,Q2,IACC,IONC,ID                                  CONV1Z14
      REAL*8 PCM(4),SKMIN,SKMAX,SK,SQ,K,                                CONV1Z15
     &    K0,K02,KMOM,BETA2,THK1,THKMN,CSTHMN,                          CONV1Z16
     &    A,B,L,EFAC,CSK,PHK,SNK,KVEC(4),QVEC(4),                       CONV1Z17
     &    PI,TWOPI,RN,LAM1,LAM2,HILGTX,YMIN,YMAX,FAC,SPRMIN             CONV1Z18
      LOGICAL DECAY2                                                    CONV1Z19
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C Power laws for various distributions in our ps generators             POWERLW2
      REAL*8 ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ,BBCS2,   POWERLW3
     &       BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF             POWERLW4
      COMMON /DPWRS/ ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ, POWERLW5
     &      BBCS2, BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF       POWERLW6
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C setup commons                                                         SETUPCO2
      REAL*8 ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                          SETUPCO3
      COMMON /SETUP/ ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                  SETUPCO4
C Some physics constants                                                CONSTPH2
      REAL*8 ALPHA,RMZ,RGZ,SW2,RMZ2,RMZRGZ,RMZGZ2                       CONSTPH3
      COMMON / FISIKX / ALPHA,RMZ,RGZ,SW2,RMZ2,RMZRGZ,RMZGZ2            CONSTPH4
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
      DATA IONC /0/                                                     CONV1Z28
C                                                                       CONV1Z29
C Initialization                                                        CONV1Z30
      IACC = 0                                                          CONV1Z31
      PCM(1) = 0.D0                                                     CONV1Z32
      PCM(2) = 0.D0                                                     CONV1Z33
      PCM(3) = 0.D0                                                     CONV1Z34
      PCM(4) = ECMISR                                                   CONV1Z35
      IF (IONC.EQ.0) THEN                                               CONV1Z36
         PI=4.D0*DATAN(1.D0)                                            CONV1Z37
         TWOPI=2.D0*PI                                                  CONV1Z38
         IONC = 1                                                       CONV1Z39
      ENDIF                                                             CONV1Z40
      IF (K1.EQ.3) THEN                                                 CONV1Z41
         SKMIN = X2MN34                                                 CONV1Z42
         SKMAX = Y2MX34                                                 CONV1Z43
         SPRMIN = X2MN56                                                CONV1Z44
      ELSE                                                              CONV1Z45
         SKMIN = X2MN56                                                 CONV1Z46
         SKMAX = Y2MX56                                                 CONV1Z47
         SPRMIN = X2MN34                                                CONV1Z48
      ENDIF                                                             CONV1Z49
C                                                                       CONV1Z50
C First choose SK, the invariant mass**2 of the (k1,k2) system, assumingCONV1Z51
C a 1/SK**C1SK behavior                                                 CONV1Z52
C                                                                       CONV1Z53
C This behaviour is multiplied by the ratio R (e+e- -> qqbar) in case ofCONV1Z54
C a qqbar system (P. Janot)                                             CONV1Z55
C                                                                       CONV1Z56
      IF ( kindqq(k1) .EQ. 0 .AND. kindqq(k2) .EQ. 0 ) THEN             CONV1Z57
        SK = HILGTX(0.D0,C1SK,SKMIN,SKMAX)                              CONV1Z58
      ELSE                                                              CONV1Z59
        CALL qcd2(kindqq(k1),6,skmin,skmax,sk)                          CONV1Z60
      ENDIF                                                             CONV1Z61
C Next choose K0, the energy of the (k1,k2) system, assuming a          CONV1Z62
C behavior 1/((s'-m**2)**2 + m**2*gam**2)                               CONV1Z63
      K = DSQRT(SK)                                                     CONV1Z64
      YMIN = (SPRMIN - RMZ2)/RMZRGZ                                     CONV1Z65
      YMAX = ((ECMISR - K)**2  - RMZ2)/RMZRGZ                           CONV1Z66
      FAC = ATAN(YMIN) - ATAN(YMAX)                                     CONV1Z67
      K0 = ( -RMZRGZ * TAN(ATAN(YMAX) + RN(2)*FAC)                      CONV1Z68
     &         + SISR + SK - RMZ2)/(2.D0*ECMISR)                        CONV1Z69
C                                                                       CONV1Z70
C Generate cos angular distribution of the (k1,k2) system               CONV1Z71
C First find efficient limits                                           CONV1Z72
      K02 = K0*K0                                                       CONV1Z73
      KMOM = DSQRT(K02 - SK)                                            CONV1Z74
      BETA2 = 1.D0 - 4.D0*MASS2(K1)/K02                                 CONV1Z75
      THK1 = THCNEV - 0.5D0*ACOS(1.D0/BETA2*(1.D0 -                     CONV1Z76
     &    2.D0*(SK - 2.D0*MASS2(K1))/K02))                              CONV1Z77
      THKMN = MAX(THK1,0.D0)                                            CONV1Z78
C Now generate angle according to 1/(A**2 - B**2*cos**2)                CONV1Z79
      CSTHMN = COS(THKMN)                                               CONV1Z80
C If final state is neutral, remove cos theta cutoff                    CONV1Z81
      IF (DCHA(K1).EQ.0.D0)  CSTHMN = 0.999D0                           CONV1Z82
      A = SK - ECMISR*K0                                                CONV1Z83
      B = ECMISR*KMOM                                                   CONV1Z84
      L = DLOG( (A/B + CSTHMN)/(A/B - CSTHMN) )                         CONV1Z85
      EFAC = EXP((2.D0*RN(3)-1.D0)*L)                                   CONV1Z86
      CSK =  (EFAC - 1.D0)*A/B/(1.D0 + EFAC)                            CONV1Z87
      PHK =  TWOPI*RN(4)                                                CONV1Z88
C And build the K and Q vectors                                         CONV1Z89
      SNK = DSQRT(1.D0 - CSK*CSK)                                       CONV1Z90
      KVEC(1) = KMOM*SNK*COS(PHK)                                       CONV1Z91
      KVEC(2) = KMOM*SNK*SIN(PHK)                                       CONV1Z92
      KVEC(3) = KMOM*CSK                                                CONV1Z93
      KVEC(4) = K0                                                      CONV1Z94
      DO 11 ID = 1, 4                                                   CONV1Z95
 11      QVEC(ID) = PCM(ID) - KVEC(ID)                                  CONV1Z96
      SQ = QVEC(4)**2 - KMOM**2                                         CONV1Z97
C Now decay the Q and K vectors uniformly in their rest frames          CONV1Z98
      IF (.NOT.DECAY2(KVEC,SK,MASS2(K1),MASS2(K2),PEVT(1,K1),           CONV1Z99
     &   PEVT(1,K2),LAM1)) GOTO 999                                     CONV1100
      IF (.NOT.DECAY2(QVEC,SQ,MASS2(Q1),MASS2(Q2),PEVT(1,Q1),           CONV1101
     &   PEVT(1,Q2),LAM2)) GOTO 999                                     CONV1102
      IACC = 1                                                          CONV1103
C printout                                                              CONV1104
      IF (NEVTIN.LE.NEVPRI.AND.NBUNCH.EQ.1.AND.DEBGLV.GE.1) THEN        CONV1105
         PRINT'('' K parts,SK,K0,COS(K,beam),SQ '',2I2,4F10.3)',        CONV1106
     &        K1,K2,SK,K0,CSK,SQ                                        CONV1107
      ENDIF                                                             CONV1108
 999  CONTINUE                                                          CONV1109
      END                                                               CONV1110
      SUBROUTINE CONV2Z(K1,K2,P1,P2,IACC)                               CONV2Z 2
C----------------------------------------------------------             CONV2Z 3
C Conversion phase space generator (2 Z's: the LEP 200 ps)              CONV2Z 4
C JH w/ RK, 11/91                                                       CONV2Z 5
C----------------------------------------------------------             CONV2Z 6
      IMPLICIT NONE                                                     CONV2Z 7
      SAVE                                                              CONV2Z 8
C local variables                                                       CONV2Z 9
      INTEGER K1,K2,P1,P2,IACC,IONC,ID                                  CONV2Z10
      REAL*8 PCM(4),SKMIN,SKMAX,SPMIN,SPMAX,SK,SP,                      CONV2Z11
     &    K0,K02,KMOM,P0,P02,BETA2,THK1,THK2,THKMN,CSTHMN,              CONV2Z12
     &    A,B,C,CSK,PHK,SNK,KVEC(4),PVEC(4),                            CONV2Z13
     &    PI,TWOPI,RN,LAM1,LAM2,HILGTX,LLIM,ULIM,YMIN,YMAX              CONV2Z14
      LOGICAL DECAY2                                                    CONV2Z15
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C Power laws for various distributions in our ps generators             POWERLW2
      REAL*8 ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ,BBCS2,   POWERLW3
     &       BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF             POWERLW4
      COMMON /DPWRS/ ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ, POWERLW5
     &      BBCS2, BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF       POWERLW6
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C setup commons                                                         SETUPCO2
      REAL*8 ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                          SETUPCO3
      COMMON /SETUP/ ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                  SETUPCO4
C Some physics constants                                                CONSTPH2
      REAL*8 ALPHA,RMZ,RGZ,SW2,RMZ2,RMZRGZ,RMZGZ2                       CONSTPH3
      COMMON / FISIKX / ALPHA,RMZ,RGZ,SW2,RMZ2,RMZRGZ,RMZGZ2            CONSTPH4
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
      DATA IONC /0/                                                     CONV2Z24
C                                                                       CONV2Z25
C Initialization                                                        CONV2Z26
      IACC = 0                                                          CONV2Z27
      PCM(1) = 0.D0                                                     CONV2Z28
      PCM(2) = 0.D0                                                     CONV2Z29
      PCM(3) = 0.D0                                                     CONV2Z30
      PCM(4) = ECMISR                                                   CONV2Z31
      IF (IONC.EQ.0) THEN                                               CONV2Z32
         PI=4.D0*DATAN(1.D0)                                            CONV2Z33
         TWOPI=2.D0*PI                                                  CONV2Z34
         IONC = 1                                                       CONV2Z35
         LLIM = (RMZ - GFAC2Z*RGZ)**2                                   CONV2Z36
         ULIM = (RMZ + GFAC2Z*RGZ)**2                                   CONV2Z37
      ENDIF                                                             CONV2Z38
      IF (K1.EQ.3) THEN                                                 CONV2Z39
         SKMIN = MAX(X2MN34,LLIM)                                       CONV2Z40
         SKMAX = MIN(X2MX34,ULIM)                                       CONV2Z41
         SPMIN = MAX(X2MN56,LLIM)                                       CONV2Z42
         SPMAX = MIN(X2MX56,ULIM)                                       CONV2Z43
      ELSE                                                              CONV2Z44
         SKMIN = MAX(X2MN56,LLIM)                                       CONV2Z45
         SKMAX = MIN(X2MX56,ULIM)                                       CONV2Z46
         SPMIN = MAX(X2MN34,LLIM)                                       CONV2Z47
         SPMAX = MIN(X2MX34,ULIM)                                       CONV2Z48
      ENDIF                                                             CONV2Z49
      IF (SKMAX.LE.SKMIN .OR. SPMAX.LE.SPMIN)  GOTO 999                 CONV2Z50
C                                                                       CONV2Z51
C First choose SK, the invariant mass**2 of the (k1,k2) system, assumingCONV2Z52
C a breit-wigner behavior                                               CONV2Z53
      YMIN = (SKMIN-RMZ2)/RMZRGZ                                        CONV2Z54
      YMAX = (SKMAX-RMZ2)/RMZRGZ                                        CONV2Z55
      SK = RMZRGZ*TAN(ATAN(YMIN) + RN(1)*(ATAN(YMAX) - ATAN(YMIN))) +   CONV2Z56
     &       RMZ2                                                       CONV2Z57
C Recalculate spmax, based on sk                                        CONV2Z58
      SPMAX = MIN((ECMISR-DSQRT(SK))**2,SPMAX)                          CONV2Z59
      IF (SPMAX.LE.SPMIN)  GOTO 999                                     CONV2Z60
C                                                                       CONV2Z61
C Next choose SP, the invariant mass**2 of the (p1,p2) system, assuming CONV2Z62
C a breit-wigner behavior                                               CONV2Z63
      YMIN = (SPMIN-RMZ2)/RMZRGZ                                        CONV2Z64
      YMAX = (SPMAX-RMZ2)/RMZRGZ                                        CONV2Z65
      SP = RMZRGZ*TAN(ATAN(YMIN) + RN(2)*(ATAN(YMAX) - ATAN(YMIN))) +   CONV2Z66
     &       RMZ2                                                       CONV2Z67
C We now have a 2-body decay, so energy and momenta of the 2 bodies     CONV2Z68
C are determined:                                                       CONV2Z69
      K0 = (SISR + SK - SP)/(2.D0*ECMISR)                               CONV2Z70
      K02 = K0*K0                                                       CONV2Z71
      KMOM  = DSQRT(K02 - SK)                                           CONV2Z72
C                                                                       CONV2Z73
C Next choose cos (k1+k2 , beam particle)                               CONV2Z74
C                                                                       CONV2Z75
C First find efficient limits                                           CONV2Z76
      BETA2 = 1.D0 - 4.D0*MASS2(K1)/K02                                 CONV2Z77
      THK1 = THCNEV - 0.5D0*ACOS(1.D0/BETA2*(1.D0 -                     CONV2Z78
     &    2.D0*(SK - 2.D0*MASS2(K1))/K02))                              CONV2Z79
      P0 = PCM(4) - K0                                                  CONV2Z80
      P02 = P0*P0                                                       CONV2Z81
      BETA2 = 1.D0 - 4.D0*MASS2(P1)/P02                                 CONV2Z82
      THK2 = THCNEV - 0.5D0*ACOS(1.D0/BETA2*(1.D0 -                     CONV2Z83
     &    2.D0*(SP - 2.D0*MASS2(P1))/P02))                              CONV2Z84
C If final state is neutral, remove angular cutoff                      CONV2Z85
      IF (DCHA(K1).EQ.0.D0)  THK1 = 1.4D-2                              CONV2Z86
      IF (DCHA(P1).EQ.0.D0)  THK2 = 1.4D-2                              CONV2Z87
      THKMN = MAX(THK1,THK2,0.D0)                                       CONV2Z88
      CSTHMN = COS(THKMN)                                               CONV2Z89
      A = ECMISR*K0 - SK                                                CONV2Z90
      B = ECMISR*KMOM                                                   CONV2Z91
      C = A/B                                                           CONV2Z92
      CSK = -HILGTX(C,C2CSK,CSTHMN,-CSTHMN)                             CONV2Z93
C Make phi (k1+k2) uniform                                              CONV2Z94
      PHK = TWOPI*RN(4)                                                 CONV2Z95
C And build the K and P vectors                                         CONV2Z96
      SNK = DSQRT(1.D0 - CSK*CSK)                                       CONV2Z97
      KVEC(1) = KMOM*SNK*COS(PHK)                                       CONV2Z98
      KVEC(2) = KMOM*SNK*SIN(PHK)                                       CONV2Z99
      KVEC(3) = KMOM*CSK                                                CONV2100
      KVEC(4) = K0                                                      CONV2101
      DO 11 ID = 1, 4                                                   CONV2102
 11      PVEC(ID) = PCM(ID) - KVEC(ID)                                  CONV2103
C                                                                       CONV2104
C Now decay the P and K vectors uniformly in their rest frames          CONV2105
      IF (.NOT.DECAY2(KVEC,SK,MASS2(K1),MASS2(K2),PEVT(1,K1),           CONV2106
     &   PEVT(1,K2),LAM1)) GOTO 999                                     CONV2107
      IF (.NOT.DECAY2(PVEC,SP,MASS2(P1),MASS2(P2),PEVT(1,P1),           CONV2108
     &   PEVT(1,P2),LAM2)) GOTO 999                                     CONV2109
      IACC = 1                                                          CONV2110
C printout                                                              CONV2111
      IF (NEVTIN.LE.NEVPRI.AND.NBUNCH.EQ.1.AND.DEBGLV.GE.1) THEN        CONV2112
         PRINT'('' SK,SP,COS(K,beam) '',3F10.3)',SK,SP,CSK              CONV2113
      ENDIF                                                             CONV2114
 999  CONTINUE                                                          CONV2115
      END                                                               CONV2116
      SUBROUTINE CONVS1(K1,K2,Q1,Q2,IACC)                               CONVS1 2
C----------------------------------------------------------             CONVS1 3
C Conversion phase space generator (one cat whisker topology)           CONVS1 4
C JH w/ RK, 10/91                                                       CONVS1 5
C                                                                       CONVS1 6
C Modification : Implement the R ratio for a qqbar pair                 CONVS1 7
C                Patrick Janot -- 05 Apr 1994                           CONVS1 8
C----------------------------------------------------------             CONVS1 9
      IMPLICIT NONE                                                     CONVS110
      SAVE                                                              CONVS111
C local variables                                                       CONVS112
      INTEGER K1,K2,Q1,Q2,IACC,IONC,ID                                  CONVS113
      REAL*8 PCM(4),SKMIN,SKMAX,SK,SQ,K,K0MAX,                          CONVS114
     &    K0,K02,KMOM,BETA2,THK1,THKMN,CSTHMN,                          CONVS115
     &    A,B,L,EFAC,CSK,PHK,SNK,KVEC(4),QVEC(4),SPRMIN,                CONVS116
     &    PI,TWOPI,RN,LAM1,LAM2,HILGTX                                  CONVS117
      LOGICAL DECAY2                                                    CONVS118
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C Power laws for various distributions in our ps generators             POWERLW2
      REAL*8 ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ,BBCS2,   POWERLW3
     &       BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF             POWERLW4
      COMMON /DPWRS/ ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ, POWERLW5
     &      BBCS2, BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF       POWERLW6
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C setup commons                                                         SETUPCO2
      REAL*8 ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                          SETUPCO3
      COMMON /SETUP/ ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                  SETUPCO4
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
      DATA IONC /0/                                                     CONVS126
C                                                                       CONVS127
C Initialization                                                        CONVS128
      IACC = 0                                                          CONVS129
      PCM(1) = 0.D0                                                     CONVS130
      PCM(2) = 0.D0                                                     CONVS131
      PCM(3) = 0.D0                                                     CONVS132
      PCM(4) = ECMISR                                                   CONVS133
      IF (IONC.EQ.0) THEN                                               CONVS134
         PI=4.D0*DATAN(1.D0)                                            CONVS135
         TWOPI=2.D0*PI                                                  CONVS136
         IONC = 1                                                       CONVS137
      ENDIF                                                             CONVS138
      IF (K1.EQ.3) THEN                                                 CONVS139
         SKMIN = X2MN34                                                 CONVS140
         SKMAX = Y2MX34                                                 CONVS141
         SPRMIN = X2MN56                                                CONVS142
      ELSE                                                              CONVS143
         SKMIN = X2MN56                                                 CONVS144
         SKMAX = Y2MX56                                                 CONVS145
         SPRMIN = X2MN34                                                CONVS146
      ENDIF                                                             CONVS147
C                                                                       CONVS148
C First choose SK, the invariant mass**2 of the (k1,k2) system, assumingCONVS149
C a 1/SK**C1SK behavior                                                 CONVS150
C                                                                       CONVS151
C This behaviour is multiplied by the ratio R (e+e- -> qqbar) in case ofCONVS152
C a qqbar system (P. Janot)                                             CONVS153
C                                                                       CONVS154
      IF ( kindqq(k1) .EQ. 0 .AND. kindqq(k2) .EQ. 0 ) THEN             CONVS155
        SK = HILGTX(0.D0,C1SK,SKMIN,SKMAX)                              CONVS156
      ELSE                                                              CONVS157
        CALL qcd2(kindqq(k1),4,skmin,skmax,sk)                          CONVS158
      ENDIF                                                             CONVS159
C Next choose K0, the energy of the (k1,k2) system, assuming a 1/K0**C1KCONVS160
C behavior                                                              CONVS161
      K = DSQRT(SK)                                                     CONVS162
C The choice of limits must be done with care.                          CONVS163
      K0MAX = (SISR+SK-SPRMIN)/(2.D0*ECMISR)                            CONVS164
      K0 = HILGTX(0.D0,C1K0,K,K0MAX)                                    CONVS165
C                                                                       CONVS166
C Generate cos angular distribution of the (k1,k2) system               CONVS167
C First find efficient limits                                           CONVS168
      K02 = K0*K0                                                       CONVS169
      KMOM = DSQRT(K02 - SK)                                            CONVS170
      BETA2 = 1.D0 - 4.D0*MASS2(K1)/K02                                 CONVS171
      THK1 = THCNEV - 0.5D0*ACOS(1.D0/BETA2*(1.D0 -                     CONVS172
     &    2.D0*(SK - 2.D0*MASS2(K1))/K02))                              CONVS173
      THKMN = MAX(THK1,0.D0)                                            CONVS174
C Now generate angle according to 1/(A**2 - B**2*cos**2)                CONVS175
      CSTHMN = COS(THKMN)                                               CONVS176
C If final state is neutral, remove cos theta cutoff                    CONVS177
      IF (DCHA(K1).EQ.0.D0)  CSTHMN = 0.999D0                           CONVS178
      A = SK - ECMISR*K0                                                CONVS179
      B = ECMISR*KMOM                                                   CONVS180
      L = DLOG( (A/B + CSTHMN)/(A/B - CSTHMN) )                         CONVS181
      EFAC = EXP((2.D0*RN(3)-1.D0)*L)                                   CONVS182
      CSK =  (EFAC - 1.D0)*A/B/(1.D0 + EFAC)                            CONVS183
      PHK =  TWOPI*RN(4)                                                CONVS184
C And build the K and Q vectors                                         CONVS185
      SNK = DSQRT(1.D0 - CSK*CSK)                                       CONVS186
      KVEC(1) = KMOM*SNK*COS(PHK)                                       CONVS187
      KVEC(2) = KMOM*SNK*SIN(PHK)                                       CONVS188
      KVEC(3) = KMOM*CSK                                                CONVS189
      KVEC(4) = K0                                                      CONVS190
      DO 11 ID = 1, 4                                                   CONVS191
 11      QVEC(ID) = PCM(ID) - KVEC(ID)                                  CONVS192
      SQ = QVEC(4)**2 - KMOM**2                                         CONVS193
C Now decay the Q and K vectors uniformly in their rest frames          CONVS194
      IF (.NOT.DECAY2(KVEC,SK,MASS2(K1),MASS2(K2),PEVT(1,K1),           CONVS195
     &   PEVT(1,K2),LAM1)) GOTO 999                                     CONVS196
      IF (.NOT.DECAY2(QVEC,SQ,MASS2(Q1),MASS2(Q2),PEVT(1,Q1),           CONVS197
     &   PEVT(1,Q2),LAM2)) GOTO 999                                     CONVS198
      IACC = 1                                                          CONVS199
C printout                                                              CONVS100
      IF (NEVTIN.LE.NEVPRI.AND.NBUNCH.EQ.1.AND.DEBGLV.GE.1) THEN        CONVS101
         PRINT'('' K parts,SK,K0,COS(K,beam),SQ '',2I2,4F10.3)',        CONVS102
     &        K1,K2,SK,K0,CSK,SQ                                        CONVS103
      ENDIF                                                             CONVS104
 999  CONTINUE                                                          CONVS105
      END                                                               CONVS106
      SUBROUTINE CONVS2(K1,K2,P1,P2,IACC)                               CONVS2 2
C----------------------------------------------------------             CONVS2 3
C Conversion phase space generator (cat whiskers topology)              CONVS2 4
C JH w/ RK, 7/91                                                        CONVS2 5
C                                                                       CONVS2 6
C Modification : Implement the R ratio for a qqbar pair                 CONVS2 7
C                Patrick Janot -- 05 Apr 1994                           CONVS2 8
C----------------------------------------------------------             CONVS2 9
      IMPLICIT NONE                                                     CONVS210
      SAVE                                                              CONVS211
C local variables                                                       CONVS212
      INTEGER K1,K2,P1,P2,IACC,IONC,ID                                  CONVS213
      REAL*8 PCM(4),SKMIN,SKMAX,SPMIN,SPMAX,SK,SP,                      CONVS214
     &    K0,K02,KMOM,P0,P02,BETA2,THK1,THK2,THKMN,CSTHMN,              CONVS215
     &    A,B,C,CSK,PHK,SNK,KVEC(4),PVEC(4),                            CONVS216
     &    PI,TWOPI,RN,LAM1,LAM2,HILGTX                                  CONVS217
      LOGICAL DECAY2                                                    CONVS218
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C Power laws for various distributions in our ps generators             POWERLW2
      REAL*8 ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ,BBCS2,   POWERLW3
     &       BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF             POWERLW4
      COMMON /DPWRS/ ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ, POWERLW5
     &      BBCS2, BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF       POWERLW6
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C setup commons                                                         SETUPCO2
      REAL*8 ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                          SETUPCO3
      COMMON /SETUP/ ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                  SETUPCO4
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
      DATA IONC /0/                                                     CONVS226
C                                                                       CONVS227
C Initialization                                                        CONVS228
      IACC = 0                                                          CONVS229
      PCM(1) = 0.D0                                                     CONVS230
      PCM(2) = 0.D0                                                     CONVS231
      PCM(3) = 0.D0                                                     CONVS232
      PCM(4) = ECMISR                                                   CONVS233
      IF (IONC.EQ.0) THEN                                               CONVS234
         PI=4.D0*DATAN(1.D0)                                            CONVS235
         TWOPI=2.D0*PI                                                  CONVS236
         IONC = 1                                                       CONVS237
      ENDIF                                                             CONVS238
      IF (K1.EQ.3) THEN                                                 CONVS239
         SKMIN = X2MN34                                                 CONVS240
         SKMAX = Y2MX34                                                 CONVS241
         SPMIN = X2MN56                                                 CONVS242
         SPMAX = Y2MX56                                                 CONVS243
      ELSE                                                              CONVS244
         SKMIN = X2MN56                                                 CONVS245
         SKMAX = Y2MX56                                                 CONVS246
         SPMIN = X2MN34                                                 CONVS247
         SPMAX = Y2MX34                                                 CONVS248
      ENDIF                                                             CONVS249
C                                                                       CONVS250
C First choose SK, the invariant mass**2 of the (k1,k2) system, assumingCONVS251
C a 1/SK**C2SKP behavior                                                CONVS252
C                                                                       CONVS253
C This behaviour is multiplied by the ratio R (e+e- -> qqbar) in case ofCONVS254
C a qqbar system (P. Janot)                                             CONVS255
C                                                                       CONVS256
      IF ( kindqq(k1) .EQ. 0 .AND. kindqq(k2) .EQ. 0 ) THEN             CONVS257
        SK = HILGTX(0.D0,C2SKP,SKMIN,SKMAX)                             CONVS258
      ELSE                                                              CONVS259
        CALL qcd2(kindqq(k1),5,skmin,skmax,sk)                          CONVS260
      ENDIF                                                             CONVS261
C                                                                       CONVS262
C Next choose SP, the invariant mass**2 of the (p1,p2) system, assuming CONVS263
C a 1/SP**C2SKP behavior                                                CONVS264
C                                                                       CONVS265
C This behaviour is multiplied by the ratio R (e+e- -> qqbar) in case ofCONVS266
C a qqbar system (P. Janot)                                             CONVS267
C                                                                       CONVS268
      IF ( kindqq(p1) .EQ. 0 .AND. kindqq(p2) .EQ. 0 ) THEN             CONVS269
        SP = HILGTX(0.D0,C2SKP,SPMIN,SPMAX)                             CONVS270
      ELSE                                                              CONVS271
        CALL qcd2(kindqq(p1),5,spmin,spmax,sp)                          CONVS272
      ENDIF                                                             CONVS273
C We now have a 2-body decay, so energy and momenta of the 2 bodies     CONVS274
C are determined:                                                       CONVS275
      K0 = (SISR + SK - SP)/(2.D0*ECMISR)                               CONVS276
      K02 = K0*K0                                                       CONVS277
      KMOM  = DSQRT(K02 - SK)                                           CONVS278
C                                                                       CONVS279
C Next choose cos (k1+k2 , beam particle)                               CONVS280
C                                                                       CONVS281
C First find efficient limits                                           CONVS282
      BETA2 = 1.D0 - 4.D0*MASS2(K1)/K02                                 CONVS283
      THK1 = THCNEV - 0.5D0*ACOS(1.D0/BETA2*(1.D0 -                     CONVS284
     &    2.D0*(SK - 2.D0*MASS2(K1))/K02))                              CONVS285
      P0 = PCM(4) - K0                                                  CONVS286
      P02 = P0*P0                                                       CONVS287
      BETA2 = 1.D0 - 4.D0*MASS2(P1)/P02                                 CONVS288
      THK2 = THCNEV - 0.5D0*ACOS(1.D0/BETA2*(1.D0 -                     CONVS289
     &    2.D0*(SP - 2.D0*MASS2(P1))/P02))                              CONVS290
C If final state is neutral, remove angular cutoff                      CONVS291
      IF (DCHA(K1).EQ.0.D0)  THK1 = 1.4D-2                              CONVS292
      IF (DCHA(P1).EQ.0.D0)  THK2 = 1.4D-2                              CONVS293
      THKMN = MAX(THK1,THK2,0.D0)                                       CONVS294
      CSTHMN = COS(THKMN)                                               CONVS295
      A = ECMISR*K0 - SK                                                CONVS296
      B = ECMISR*KMOM                                                   CONVS297
      C = A/B                                                           CONVS298
      CSK = -HILGTX(C,C2CSK,CSTHMN,-CSTHMN)                             CONVS299
C Make phi (k1+k2) uniform                                              CONVS100
      PHK = TWOPI*RN(4)                                                 CONVS101
C And build the K and P vectors                                         CONVS102
      SNK = DSQRT(1.D0 - CSK*CSK)                                       CONVS103
      KVEC(1) = KMOM*SNK*COS(PHK)                                       CONVS104
      KVEC(2) = KMOM*SNK*SIN(PHK)                                       CONVS105
      KVEC(3) = KMOM*CSK                                                CONVS106
      KVEC(4) = K0                                                      CONVS107
      DO 11 ID = 1, 4                                                   CONVS108
 11      PVEC(ID) = PCM(ID) - KVEC(ID)                                  CONVS109
C                                                                       CONVS110
C Now decay the P and K vectors uniformly in their rest frames          CONVS111
      IF (.NOT.DECAY2(KVEC,SK,MASS2(K1),MASS2(K2),PEVT(1,K1),           CONVS112
     &   PEVT(1,K2),LAM1)) GOTO 999                                     CONVS113
      IF (.NOT.DECAY2(PVEC,SP,MASS2(P1),MASS2(P2),PEVT(1,P1),           CONVS114
     &   PEVT(1,P2),LAM2)) GOTO 999                                     CONVS115
      IACC = 1                                                          CONVS116
C printout                                                              CONVS117
      IF (NEVTIN.LE.NEVPRI.AND.NBUNCH.EQ.1.AND.DEBGLV.GE.1) THEN        CONVS118
         PRINT'('' SK,K0,COS(K,beam) '',3F10.3)',SK,K0,CSK              CONVS119
      ENDIF                                                             CONVS120
 999  CONTINUE                                                          CONVS121
      END                                                               CONVS122
      FUNCTION COSPRI(QL,SQ,PL)                                         COSPRI 2
C----------------------------------------------------------             COSPRI 3
C Find cos between vectors QL and PL after boosting to QL rest frame.   COSPRI 4
C----------------------------------------------------------             COSPRI 5
      IMPLICIT NONE                                                     COSPRI 6
      SAVE                                                              COSPRI 7
C Local variables                                                       COSPRI 8
      REAL*8 COSPRI,QL(4),QLMOM,SQ,PL(4),BETA,GAMMA,PPARL,PPER,         COSPRI 9
     &   PLMOM2,PPARR                                                   COSPRI10
C                                                                       COSPRI11
      QLMOM = DSQRT(QL(4)**2 - SQ)                                      COSPRI12
      BETA = QLMOM/QL(4)                                                COSPRI13
      GAMMA = QL(4)/DSQRT(SQ)                                           COSPRI14
C The parallel momentum of PL in the lab frame                          COSPRI15
      PPARL = (PL(1)*QL(1) + PL(2)*QL(2) + PL(3)*QL(3))/QLMOM           COSPRI16
C Boost the parallel momentum                                           COSPRI17
      PPARR = GAMMA*(PPARL - BETA*PL(4))                                COSPRI18
      PLMOM2 = PL(1)**2 + PL(2)**2 + PL(3)**2                           COSPRI19
C the perpendiculr momentum is of course invariant                      COSPRI20
C Occasional pathologies exist, so this protection is needed:           COSPRI21
      PPER = DSQRT(MAX(PLMOM2 - PPARL*PPARL, 1.D-6))                    COSPRI22
C                                                                       COSPRI23
      COSPRI = PPARR/DSQRT(PPARR*PPARR + PPER*PPER)                     COSPRI24
C                                                                       COSPRI25
      END                                                               COSPRI26
*CSCENV                                                                 COSPRI27
      SUBROUTINE CSCENV                                                 COSPRI28
C----------------------------------------------------------             COSPRI29
C Update cen. det. angle cut-off for events w/ ISR                      COSPRI30
C----------------------------------------------------------             COSPRI31
      IMPLICIT NONE                                                     COSPRI32
      SAVE                                                              COSPRI33
C local variables                                                       COSPRI34
      REAL*8 PCEN(4,2),BOOST(4),PBOOS(4),CS1,CS2                        COSPRI35
      INTEGER IONC                                                      COSPRI36
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C setup commons                                                         SETUPCO2
      REAL*8 ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                          SETUPCO3
      COMMON /SETUP/ ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                  SETUPCO4
C                                                                       COSPRI42
      DATA IONC /0/                                                     COSPRI43
      IF (IONC.EQ.0) THEN                                               COSPRI44
C Fill cen. det. vector                                                 COSPRI45
         PCEN(1,1) = SQRT(1.D0-CSCEN*CSCEN)                             COSPRI46
         PCEN(1,2) = SQRT(1.D0-CSCEN*CSCEN)                             COSPRI47
         PCEN(2,1) = 0.D0                                               COSPRI48
         PCEN(2,2) = 0.D0                                               COSPRI49
         PCEN(3,1) = CSCEN                                              COSPRI50
         PCEN(3,2) = -CSCEN                                             COSPRI51
         PCEN(4,1) = 1.D0                                               COSPRI52
         PCEN(4,2) = 1.D0                                               COSPRI53
         IONC = 1                                                       COSPRI54
      ENDIF                                                             COSPRI55
      CSCNEV = CSCEN                                                    COSPRI56
C Get boost                                                             COSPRI57
      CALL GETBOO(GAMISR,EBEAM,BOOST)                                   COSPRI58
C Boost the beam particle into lab frame.                               COSPRI59
      CALL GETLAB(BOOST,PCEN(1,1),PBOOS)                                COSPRI60
      CS1 = ABS(PBOOS(3))/PBOOS(4)                                      COSPRI61
      CALL GETLAB(BOOST,PCEN(1,2),PBOOS)                                COSPRI62
      CS2 = ABS(PBOOS(3))/PBOOS(4)                                      COSPRI63
      CSCNEV = MAX(CS1,CS2)                                             COSPRI64
      IF (CSCNEV.LT.1.D0) THEN                                          COSPRI65
         THCNEV = ACOS(CSCNEV)                                          COSPRI66
      ELSE                                                              COSPRI67
         THCNEV = 0.D0                                                  COSPRI68
      ENDIF                                                             COSPRI69
C printout:                                                             COSPRI70
      IF (NEVTIN.LE.NEVPRI.AND.NBUNCH.EQ.1.AND.DEBGLV.GE.1) THEN        COSPRI71
         PRINT'('' AFter ISR, cen. det. angle in 4-fermion rest'',      COSPRI72
     &    '' frame is '',F8.3,'' degrees'')',THCNEV*180.D0/3.14159      COSPRI73
      ENDIF                                                             COSPRI74
C                                                                       COSPRI75
      END                                                               COSPRI76
      FUNCTION DECA2X(CSTH,K,M2,M21,M22,K1,K2)                          DECA2X 2
C----------------------------------------------------------             DECA2X 3
C Decay two particles and boost to parent's frame.                      DECA2X 4
C INPUTS :                                                              DECA2X 5
C CSTH  - cos of K1 w/r/t parent's lab direction                        DECA2X 6
C  K(i) - parent momenta                                                DECA2X 7
C  M2  - mass**2 of parent                                              DECA2X 8
C  M21 - mass**2 of particle 1                                          DECA2X 9
C  M22 - mass**2 of particle 2                                          DECA2X10
C OUTPUTS :                                                             DECA2X11
C  K1(i) - daughter 1's momenta                                         DECA2X12
C  K2(i) - daughter 2's momenta                                         DECA2X13
C----------------------------------------------------------             DECA2X14
      IMPLICIT NONE                                                     DECA2X15
      SAVE                                                              DECA2X16
C Local variables                                                       DECA2X17
      REAL*8 PI,TWOPI,CSTH,K(4),M2,M21,M22,K1(4),K2(4),ROT(3,3),        DECA2X18
     &   RP(4),RRP(3),PHI,RN,SN,LAM,LAMBDA,KIMOM,PHK,CSK,SNK,QR,        DECA2X19
     &   EP,B,C                                                         DECA2X20
      INTEGER IONC,IROW,ICOL,I                                          DECA2X21
      LOGICAL DECA2X                                                    DECA2X22
C                                                                       DECA2X23
      DATA IONC /0/                                                     DECA2X24
      LAMBDA(B,C) = 1.D0 + B*B + C*C - 2.D0*(B + C + B*C)               DECA2X25
C                                                                       DECA2X26
      IF (IONC.EQ.0) THEN                                               DECA2X27
         PI=4.D0*DATAN(1.D0)                                            DECA2X28
         TWOPI=2.D0*PI                                                  DECA2X29
         IONC = 1                                                       DECA2X30
      ENDIF                                                             DECA2X31
      DECA2X = .FALSE.                                                  DECA2X32
      PHI = TWOPI*RN(1)                                                 DECA2X33
      SN = DSQRT(1.D0 - CSTH*CSTH)                                      DECA2X34
C                                                                       DECA2X35
      LAM = LAMBDA(M21/M2,M22/M2)                                       DECA2X36
C                                                                       DECA2X37
C Guard against non-physical inputs :                                   DECA2X38
      IF (LAM.LE.0.D0)  GOTO 999                                        DECA2X39
      KIMOM = DSQRT(M2*LAM)/2.D0                                        DECA2X40
      EP = DSQRT(M2)                                                    DECA2X41
C                                                                       DECA2X42
C Momemtum of K1 in K's (rotated) rest frame                            DECA2X43
      RRP(1) = KIMOM*SN*SIN(PHI)                                        DECA2X44
      RRP(2) = KIMOM*SN*COS(PHI)                                        DECA2X45
C The minus sign is needed because K1 refers to the single particle!    DECA2X46
      RRP(3) = -KIMOM*CSTH                                              DECA2X47
      RP(4) = DSQRT(M21 + KIMOM**2)                                     DECA2X48
C                                                                       DECA2X49
C Rotate K1 into K's actual rest frame                                  DECA2X50
      PHK = DATAN2(K(1),K(2))                                           DECA2X51
      CSK = K(3)/DSQRT(K(1)**2 + K(2)**2 + K(3)**2)                     DECA2X52
      SNK = DSQRT(1.D0 - CSK**2)                                        DECA2X53
C                                                                       DECA2X54
C Build rotation matrix                                                 DECA2X55
      ROT(1,1) = COS(PHK)                                               DECA2X56
      ROT(1,2) = CSK*SIN(PHK)                                           DECA2X57
      ROT(1,3) = SNK*SIN(PHK)                                           DECA2X58
      ROT(2,1) = -SIN(PHK)                                              DECA2X59
      ROT(2,2) = ROT(1,1)*CSK                                           DECA2X60
      ROT(2,3) = COS(PHK)*SNK                                           DECA2X61
      ROT(3,1) = 0.D0                                                   DECA2X62
      ROT(3,2) = -SNK                                                   DECA2X63
      ROT(3,3) = CSK                                                    DECA2X64
C                                                                       DECA2X65
C Do rotation                                                           DECA2X66
      DO 11 IROW = 1, 3                                                 DECA2X67
         RP(IROW) = 0.D0                                                DECA2X68
         DO 11 ICOL = 1, 3                                              DECA2X69
   11       RP(IROW) = RP(IROW) + ROT(IROW,ICOL)*RRP(ICOL)              DECA2X70
C                                                                       DECA2X71
C Now proceed as in DECAY2                                              DECA2X72
C BOOST K1 MOMENTA TO LAB FRAME AND CONSTRUCT K2 MOMENTUM               DECA2X73
      K1(4)=(K(4)*RP(4)+K(1)*RP(1)+K(2)*RP(2)+K(3)*RP(3))/EP            DECA2X74
      QR   =(RP(4)+K1(4))/(EP+K(4))                                     DECA2X75
      DO 5 I=1,3                                                        DECA2X76
         K1(I)=RP(I)+K(I)*QR                                            DECA2X77
  5      K2(I)=K(I)-K1(I)                                               DECA2X78
      K2(4)=K(4)-K1(4)                                                  DECA2X79
C                                                                       DECA2X80
      DECA2X = .TRUE.                                                   DECA2X81
C                                                                       DECA2X82
  999 CONTINUE                                                          DECA2X83
      END                                                               DECA2X84
      FUNCTION DECAY2(K,M2,M21,M22,K1,K2,LAM)                           DECAY2 2
C----------------------------------------------------------             DECAY2 3
C Decay two particles uniformly and boost to parent's frame.            DECAY2 4
C INPUTS :                                                              DECAY2 5
C  K(i) - parent momenta                                                DECAY2 6
C  M2  - mass**2 of parent                                              DECAY2 7
C  M21 - mass**2 of particle 1                                          DECAY2 8
C  M22 - mass**2 of particle 2                                          DECAY2 9
C OUTPUTS :                                                             DECAY210
C  K1(i) - daughter 1's momenta                                         DECAY211
C  K2(i) - daughter 2's momenta                                         DECAY212
C  LAM - normalized 2-body phase space factor                           DECAY213
C----------------------------------------------------------             DECAY214
      IMPLICIT NONE                                                     DECAY215
      SAVE                                                              DECAY216
C Local variables                                                       DECAY217
      REAL*8 K(4),M2,M21,M22,K1(4),K2(4),RP(4),EP,LAMBDA,B,C,           DECAY218
     &   KIMOM,PI,TWOPI,CS,SN,RN,PHI,QR,LAM                             DECAY219
      INTEGER IONC,I                                                    DECAY220
      LOGICAL DECAY2                                                    DECAY221
      DATA IONC /0/                                                     DECAY222
      LAMBDA(B,C) = 1.D0 + B*B + C*C - 2.D0*(B + C + B*C)               DECAY223
C                                                                       DECAY224
      IF (IONC.EQ.0) THEN                                               DECAY225
         PI=4.D0*DATAN(1.D0)                                            DECAY226
         TWOPI=2.D0*PI                                                  DECAY227
         IONC = 1                                                       DECAY228
      ENDIF                                                             DECAY229
      DECAY2 = .FALSE.                                                  DECAY230
      EP = DSQRT(M2)                                                    DECAY231
      CS = 2.D0*RN(1) - 1.D0                                            DECAY232
      PHI = TWOPI*RN(2)                                                 DECAY233
      SN = DSQRT(1.D0 - CS*CS)                                          DECAY234
C                                                                       DECAY235
      LAM = LAMBDA(M21/M2,M22/M2)                                       DECAY236
C                                                                       DECAY237
C Guard against non-physical inputs :                                   DECAY238
      IF (LAM.LE.0.D0)  GOTO 999                                        DECAY239
      KIMOM = DSQRT(M2*LAM)/2.D0                                        DECAY240
C                                                                       DECAY241
C Momemtum of K1 in K's rest frame                                      DECAY242
      RP(1) = KIMOM*SN*COS(PHI)                                         DECAY243
      RP(2) = KIMOM*SN*SIN(PHI)                                         DECAY244
      RP(3) = KIMOM*CS                                                  DECAY245
      RP(4) = DSQRT(M21 + KIMOM**2)                                     DECAY246
C                                                                       DECAY247
C BOOST K1 MOMENTA TO LAB FRAME AND CONSTRUCT K2 MOMENTUM               DECAY248
      K1(4)=(K(4)*RP(4)+K(1)*RP(1)+K(2)*RP(2)+K(3)*RP(3))/EP            DECAY249
      QR   =(RP(4)+K1(4))/(EP+K(4))                                     DECAY250
      DO 5 I=1,3                                                        DECAY251
         K1(I)=RP(I)+K(I)*QR                                            DECAY252
  5      K2(I)=K(I)-K1(I)                                               DECAY253
      K2(4)=K(4)-K1(4)                                                  DECAY254
C                                                                       DECAY255
      DECAY2 = .TRUE.                                                   DECAY256
C                                                                       DECAY257
  999 CONTINUE                                                          DECAY258
      END                                                               DECAY259
      FUNCTION DENC1Z(K1,K2,Q1,Q2,NEWEV,LERR)                           DENC1Z 2
C-------------------------------------------------------------------    DENC1Z 3
C Find the a posteriori densities expressing the probability for having DENC1Z 4
C generated a given phase space configuration with one of our           DENC1Z 5
C conversion phase-space generators.                                    DENC1Z 6
C INPUT :                                                               DENC1Z 7
C K1,K2,Q1,Q2 - 4-momentum labels                                       DENC1Z 8
C NEWEV = .TRUE. if first call of new event                             DENC1Z 9
C OUTPUT :                                                              DENC1Z10
C   LERR = .TRUE. if some non-physical situation is present             DENC1Z11
C-------------------------------------------------------------------    DENC1Z12
      IMPLICIT NONE                                                     DENC1Z13
      SAVE                                                              DENC1Z14
C Local variables                                                       DENC1Z15
      INTEGER K1,K2,Q1,Q2,IONC,N,M                                      DENC1Z16
      REAL*8 DENC1Z,PI,PIBY2,PIFAC,SKMAX,SKMIN,SK,KMASS,                DENC1Z17
     &   SQ,FAC1,FAC2,FAC3,FAC4,FAC5,FAC6,FAC7,FAC8,K0,KMOM,CSK,        DENC1Z18
     &   BETA2,THK1,K02,THKMN,K0MAX,SPRMIN,                             DENC1Z19
     &   A,B,C,CSTHMX,CSTHMN,F1,F2,DFAC,LAMBDA,XM2,HILGTN,SPR,YMIN,YMAX DENC1Z20
      LOGICAL NEWEV,LERR                                                DENC1Z21
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C Power laws for various distributions in our ps generators             POWERLW2
      REAL*8 ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ,BBCS2,   POWERLW3
     &       BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF             POWERLW4
      COMMON /DPWRS/ ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ, POWERLW5
     &      BBCS2, BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF       POWERLW6
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C setup commons                                                         SETUPCO2
      REAL*8 ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                          SETUPCO3
      COMMON /SETUP/ ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                  SETUPCO4
C Some physics constants                                                CONSTPH2
      REAL*8 ALPHA,RMZ,RGZ,SW2,RMZ2,RMZRGZ,RMZGZ2                       CONSTPH3
      COMMON / FISIKX / ALPHA,RMZ,RGZ,SW2,RMZ2,RMZRGZ,RMZGZ2            CONSTPH4
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
C                                                                       DENC1Z29
      DATA IONC /0/                                                     DENC1Z30
      LAMBDA(B,C) = 1.D0 + B*B + C*C - 2.D0*(B + C + B*C)               DENC1Z31
      XM2(N,M) = MASS2(N) + MASS2(M) + DEVT(N,M)                        DENC1Z32
C                                                                       DENC1Z33
      LERR = .FALSE.                                                    DENC1Z34
C                                                                       DENC1Z35
C Once-per-job initialization.                                          DENC1Z36
C                                                                       DENC1Z37
      IF (IONC.EQ.0) THEN                                               DENC1Z38
         PIBY2=2.D0*DATAN(1.D0)                                         DENC1Z39
         PI=4.D0*DATAN(1.D0)                                            DENC1Z40
         PIFAC = PIBY2**2*PI*2.D0                                       DENC1Z41
         IONC = 1                                                       DENC1Z42
      ENDIF                                                             DENC1Z43
      IF (K1.EQ.3) THEN                                                 DENC1Z44
         SKMAX = Y2MX34                                                 DENC1Z45
         SKMIN = X2MN34                                                 DENC1Z46
         SPRMIN = X2MN56                                                DENC1Z47
      ELSE                                                              DENC1Z48
         SKMAX = Y2MX56                                                 DENC1Z49
         SKMIN = X2MN56                                                 DENC1Z50
         SPRMIN = X2MN34                                                DENC1Z51
      ENDIF                                                             DENC1Z52
      SK = XM2(K1,K2)                                                   DENC1Z53
C Check inv. mass of (k1,k2) system                                     DENC1Z54
      IF (SK.GT.SKMAX)  GOTO 999                                        DENC1Z55
C Check energy  of (k1,k2) system                                       DENC1Z56
      KMASS = DSQRT(SK)                                                 DENC1Z57
      K0   = PEVT(4,K1)+PEVT(4,K2)                                      DENC1Z58
      K0MAX = (SISR+SK-SPRMIN)/(2.D0*ECMISR)                            DENC1Z59
      IF (K0.GT.K0MAX)  GOTO 999                                        DENC1Z60
      K02  = K0*K0                                                      DENC1Z61
      KMOM = DSQRT(K02 - SK)                                            DENC1Z62
      FAC1 = SK**C1SK                                                   DENC1Z63
      IF ( kindqq(k1) .EQ. 0 ) THEN                                     DENC1Z64
        FAC2 = HILGTN(0.D0,C1SK,SKMIN,SKMAX)                            DENC1Z65
      ELSE                                                              DENC1Z66
        CALL qcd3(kindqq(k1),6,skmin,skmax,fac2)                        DENC1Z67
        fac1 = fac1 / rratio(sk,kindqq(k1))                             DENC1Z68
      ENDIF                                                             DENC1Z69
C      FAC3 = .5D0*KMOM*K0**C1K0                                        DENC1Z70
C      FAC4 = HILGTN(0.D0,C1K0,KMASS,K0MAX)                             DENC1Z71
      SPR = SISR + SK - 2.D0*K0*ECMISR                                  DENC1Z72
      FAC3 = .5D0*KMOM*((SPR-RMZ2)**2 + RMZGZ2)                         DENC1Z73
      YMIN = (SPRMIN - RMZ2)/RMZRGZ                                     DENC1Z74
      YMAX = ((ECMISR - KMASS)**2  - RMZ2)/RMZRGZ                       DENC1Z75
      FAC4 = 1./(2.D0*ECMISR*RMZRGZ)*(ATAN(YMAX) - ATAN(YMIN))          DENC1Z76
      A = SK - ECMISR*K0                                                DENC1Z77
      B = ECMISR*KMOM                                                   DENC1Z78
      CSK  = (PEVT(3,K1)+PEVT(3,K2))/KMOM                               DENC1Z79
      FAC5 = (A*A - (B*CSK)**2)                                         DENC1Z80
C First find efficient limits                                           DENC1Z81
      BETA2 = 1.D0 - 4.D0*MASS2(K1)/K02                                 DENC1Z82
      THK1 = THCNEV - 0.5D0*ACOS(1.D0/BETA2*(1.D0 -                     DENC1Z83
     &    2.D0*(SK - 2.D0*MASS2(K1))/K02))                              DENC1Z84
      THKMN = MAX(THK1,0.D0)                                            DENC1Z85
      CSTHMN = COS(THKMN)                                               DENC1Z86
C If final state is neutral, remove cos theta cutoff                    DENC1Z87
      IF (DCHA(K1).EQ.0.D0)  CSTHMN = 0.999D0                           DENC1Z88
      CSTHMX = -CSTHMN                                                  DENC1Z89
      IF (CSK.GT.CSTHMN .OR. CSK.LT.CSTHMX)  GOTO 999                   DENC1Z90
C ld                                                                    DENC1Z91
C     if ( (A+B*CSTHMN)/(A-B*CSTHMN) .lt. 1e-70 ) then                  DENC1Z92
C       print *,'+++DENC1Z+++ something went wrong: log ',a,b,CSTHMN    DENC1Z93
C       goto 999                                                        DENC1Z94
C     endif                                                             DENC1Z95
C ld                                                                    DENC1Z96
      FAC6 = DLOG( (A+B*CSTHMN)/(A-B*CSTHMN) )/(A*B)                    DENC1Z97
C                                                                       DENC1Z98
      SQ = (ECMISR - K0)**2 - KMOM*KMOM                                 DENC1Z99
      F1 = MASS2(K1)/SK                                                 DENC1100
      F2 = MASS2(Q1)/SQ                                                 DENC1101
      FAC7 = LAMBDA(F1,F1)                                              DENC1102
      FAC8 = LAMBDA(F2,F2)                                              DENC1103
C Put it all together                                                   DENC1104
      DFAC = PIFAC*FAC1*FAC2*FAC3*FAC4*FAC5*FAC6*DSQRT(FAC7*FAC8)       DENC1105
      DENC1Z = 1.D0/DFAC                                                DENC1106
      RETURN                                                            DENC1107
 999  CONTINUE                                                          DENC1108
      DENC1Z = 0.D0                                                     DENC1109
      END                                                               DENC1110
      FUNCTION DENC2Z(K1,K2,P1,P2,NEWEV,LERR)                           DENC2Z 2
C-------------------------------------------------------------------    DENC2Z 3
C Find the a posteriori densities expressing the probability for having DENC2Z 4
C generated a given phase space configuration with one of our           DENC2Z 5
C conversion phase-space generators.                                    DENC2Z 6
C INPUT :                                                               DENC2Z 7
C K1,K2,P1,P2 - 4-momentum labels                                       DENC2Z 8
C NEWEV = .TRUE. if first call of new event                             DENC2Z 9
C OUTPUT :                                                              DENC2Z10
C   LERR = .TRUE. if some non-physical situation is present             DENC2Z11
C-------------------------------------------------------------------    DENC2Z12
      IMPLICIT NONE                                                     DENC2Z13
      SAVE                                                              DENC2Z14
C Local variables                                                       DENC2Z15
      INTEGER K1,K2,P1,P2,IONC,N,M                                      DENC2Z16
      REAL*8 DENC2Z,PI,PIBY2,PIFAC,F7F8,SKMIN,SKMAX,SK,SP,SPMIN,SPMAX,  DENC2Z17
     &   SKS,SPS,FAC1,FAC2,FAC3,FAC4,FAC5,FAC6,FAC9,K0,KMOM,CSK,        DENC2Z18
     &   BETA2,THK1,K02,P0,P02,THK2,THKMN,                              DENC2Z19
     &   B,C,CSTHMN,DFAC,LAMBDA,XM2,HILGTN,LLIM,ULIM,YKMAX,YKMIN,       DENC2Z20
     &   YPMAX,YPMIN                                                    DENC2Z21
      LOGICAL NEWEV,LERR                                                DENC2Z22
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C Power laws for various distributions in our ps generators             POWERLW2
      REAL*8 ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ,BBCS2,   POWERLW3
     &       BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF             POWERLW4
      COMMON /DPWRS/ ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ, POWERLW5
     &      BBCS2, BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF       POWERLW6
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C setup commons                                                         SETUPCO2
      REAL*8 ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                          SETUPCO3
      COMMON /SETUP/ ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                  SETUPCO4
C Some physics constants                                                CONSTPH2
      REAL*8 ALPHA,RMZ,RGZ,SW2,RMZ2,RMZRGZ,RMZGZ2                       CONSTPH3
      COMMON / FISIKX / ALPHA,RMZ,RGZ,SW2,RMZ2,RMZRGZ,RMZGZ2            CONSTPH4
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
C                                                                       DENC2Z30
      DATA IONC /0/                                                     DENC2Z31
      LAMBDA(B,C) = 1.D0 + B*B + C*C - 2.D0*(B + C + B*C)               DENC2Z32
      XM2(N,M) = MASS2(N) + MASS2(M) + DEVT(N,M)                        DENC2Z33
C                                                                       DENC2Z34
      LERR = .FALSE.                                                    DENC2Z35
C                                                                       DENC2Z36
C Once-per-job initialization.                                          DENC2Z37
C                                                                       DENC2Z38
      IF (IONC.EQ.0) THEN                                               DENC2Z39
         PIBY2=2.D0*DATAN(1.D0)                                         DENC2Z40
         PI=4.D0*DATAN(1.D0)                                            DENC2Z41
         PIFAC = PIBY2**2*PI/4.D0                                       DENC2Z42
         LLIM = (RMZ - GFAC2Z*RGZ)**2                                   DENC2Z43
         ULIM = (RMZ + GFAC2Z*RGZ)**2                                   DENC2Z44
         IONC = 1                                                       DENC2Z45
      ENDIF                                                             DENC2Z46
C Once-per-event initialization.                                        DENC2Z47
      IF (K1.EQ.3) THEN                                                 DENC2Z48
         SKMIN = MAX(X2MN34,LLIM)                                       DENC2Z49
         SKMAX = MIN(X2MX34,ULIM)                                       DENC2Z50
         SPMIN = MAX(X2MN56,LLIM)                                       DENC2Z51
         SPMAX = MIN(X2MX56,ULIM)                                       DENC2Z52
      ELSE                                                              DENC2Z53
         SKMIN = MAX(X2MN56,LLIM)                                       DENC2Z54
         SKMAX = MIN(X2MX56,ULIM)                                       DENC2Z55
         SPMIN = MAX(X2MN34,LLIM)                                       DENC2Z56
         SPMAX = MIN(X2MX34,ULIM)                                       DENC2Z57
      ENDIF                                                             DENC2Z58
      IF (NEWEV) THEN                                                   DENC2Z59
         NEWEV = .FALSE.                                                DENC2Z60
      ENDIF                                                             DENC2Z61
      SK = XM2(K1,K2)                                                   DENC2Z62
C Recalculate spmax, based on sk                                        DENC2Z63
      SPMAX = MIN((ECMISR-DSQRT(SK))**2,SPMAX)                          DENC2Z64
      IF (SKMAX.LE.SKMIN .OR. SPMAX.LE.SPMIN)  GOTO 999                 DENC2Z65
      SP = XM2(P1,P2)                                                   DENC2Z66
      IF (SK.GT.SKMAX .OR. SP.GT.SPMAX)  GOTO 999                       DENC2Z67
      IF (SK.LT.SKMIN .OR. SP.LT.SPMIN)  GOTO 999                       DENC2Z68
      YKMAX = (SKMAX - RMZ2)/RMZRGZ                                     DENC2Z69
      YKMIN = (SKMIN - RMZ2)/RMZRGZ                                     DENC2Z70
      YPMAX = (SPMAX - RMZ2)/RMZRGZ                                     DENC2Z71
      YPMIN = (SPMIN - RMZ2)/RMZRGZ                                     DENC2Z72
      F7F8 = PIFAC/RMZGZ2*(ATAN(YKMAX) - ATAN(YKMIN))*                  DENC2Z73
     &        (ATAN(YPMAX) - ATAN(YPMIN))                               DENC2Z74
      SKS = SK/SISR                                                     DENC2Z75
      SPS = SP/SISR                                                     DENC2Z76
      FAC1 = LAMBDA(SKS,SPS)                                            DENC2Z77
      FAC2 = 1.D0-4.D0*MASS2(K1)/SK                                     DENC2Z78
      FAC3 = 1.D0-4.D0*MASS2(P1)/SP                                     DENC2Z79
      K0   = PEVT(4,K1)+PEVT(4,K2)                                      DENC2Z80
      K02  = K0*K0                                                      DENC2Z81
      KMOM = DSQRT(K02 - SK)                                            DENC2Z82
      CSK  = (PEVT(3,K1)+PEVT(3,K2))/KMOM                               DENC2Z83
C First find efficient limits                                           DENC2Z84
      BETA2 = 1.D0 - 4.D0*MASS2(K1)/K02                                 DENC2Z85
      THK1 = THCNEV - 0.5D0*ACOS(1.D0/BETA2*(1.D0 -                     DENC2Z86
     &    2.D0*(SK - 2.D0*MASS2(K1))/K02))                              DENC2Z87
      P0 =   PEVT(4,P1)+PEVT(4,P2)                                      DENC2Z88
      P02 = P0*P0                                                       DENC2Z89
      BETA2 = 1.D0 - 4.D0*MASS2(P1)/P02                                 DENC2Z90
      THK2 = THCNEV - 0.5D0*ACOS(1.D0/BETA2*(1.D0 -                     DENC2Z91
     &    2.D0*(SP - 2.D0*MASS2(P1))/P02))                              DENC2Z92
C If final state is neutral, remove angular cutoff                      DENC2Z93
      IF (DCHA(K1).EQ.0.D0)  THK1 = 1.4D-2                              DENC2Z94
      IF (DCHA(P1).EQ.0.D0)  THK2 = 1.4D-2                              DENC2Z95
      THKMN = MAX(THK1,THK2,0.D0)                                       DENC2Z96
      CSTHMN = COS(THKMN)                                               DENC2Z97
C                                                                       DENC2Z98
      IF (CSK.GT.CSTHMN .OR. CSK.LT.-CSTHMN)  GOTO 999                  DENC2Z99
      B    = ECMISR*KMOM                                                DENC2100
      C  = (ECMISR*K0 - SK)/B                                           DENC2101
      FAC4 = (C-CSK)**C2CSK                                             DENC2102
      FAC5 = (SK-RMZ2)**2 + RMZGZ2                                      DENC2103
      FAC6 = (SP-RMZ2)**2 + RMZGZ2                                      DENC2104
      FAC9 = -HILGTN(C,C2CSK,CSTHMN,-CSTHMN)                            DENC2105
C Put it all together                                                   DENC2106
      DFAC = DSQRT(FAC1*FAC2*FAC3)*FAC4*FAC5*FAC6*F7F8*FAC9             DENC2107
      DENC2Z = 1.D0/DFAC                                                DENC2108
      RETURN                                                            DENC2109
 999  CONTINUE                                                          DENC2110
      DENC2Z = 0.D0                                                     DENC2111
      END                                                               DENC2112
      FUNCTION DENSAN(IPROP,PR1,PR2,INIT,NEWEV,LERR)                    DENSAN 2
C-------------------------------------------------------------------    DENSAN 3
C Find the a posteriori densities expressing the probability for having DENSAN 4
C generated a given phase space configuration with one of our           DENSAN 5
C annihilation phase-space generators.                                  DENSAN 6
C INPUT :                                                               DENSAN 7
C   IPROP - the momentum label of the propagator                        DENSAN 8
C   PR1   - the momentum label of the 1st paired particle               DENSAN 9
C   PR2   - the momentum label of the 2nd paired particle               DENSAN10
C   INIT = 0 no initialization                                          DENSAN11
C        = 1 initialize for fla(3) .ne. fla(5) case                     DENSAN12
C        = 2 initialize for fla(3) .eq. fla(5) case                     DENSAN13
C OUTPUT :                                                              DENSAN14
C   LERR = .TRUE. if some non-physical situation is present             DENSAN15
C-------------------------------------------------------------------    DENSAN16
      IMPLICIT NONE                                                     DENSAN17
      SAVE                                                              DENSAN18
C Local variables                                                       DENSAN19
      INTEGER IPROP,PR1,PR2,INIT,L,M,N,IONC,                            DENSAN20
     & ID                                                               DENSAN21
      REAL*8 XM2,XM3,DENSAN,SQ,                                         DENSAN22
     &   DFAC,FAC1,FAC2,FAC3,FAC4,FAC5,FAC6,FAC7,FAC8,FAC9,             DENSAN23
     &   LAMBDA,PIBY2,PIFAC,B,C,SQMIN,SQMAX,                            DENSAN24
     &   PL(4),QL(4),COSTH,COSPRI,SP,SPMIN,SPMAX,HILGTN                 DENSAN25
      LOGICAL LERR,NEWEV                                                DENSAN26
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C Power laws for various distributions in our ps generators             POWERLW2
      REAL*8 ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ,BBCS2,   POWERLW3
     &       BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF             POWERLW4
      COMMON /DPWRS/ ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ, POWERLW5
     &      BBCS2, BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF       POWERLW6
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C setup commons                                                         SETUPCO2
      REAL*8 ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                          SETUPCO3
      COMMON /SETUP/ ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                  SETUPCO4
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
      DATA IONC /0/                                                     DENSAN34
C                                                                       DENSAN35
      XM2(N,M) = MASS2(N) + MASS2(M) + DEVT(N,M)                        DENSAN36
      XM3(L,N,M) = MASS2(L) + MASS2(N) + MASS2(M) + DEVT(L,N) +         DENSAN37
     &   DEVT(L,M) + DEVT(N,M)                                          DENSAN38
      LAMBDA(B,C) = 1.D0 + B*B + C*C - 2.D0*(B + C + B*C)               DENSAN39
C                                                                       DENSAN40
      LERR = .TRUE.                                                     DENSAN41
C                                                                       DENSAN42
C Once-per-job initialization.                                          DENSAN43
C                                                                       DENSAN44
      IF (IONC.EQ.0) THEN                                               DENSAN45
         PIBY2=2.D0*DATAN(1.D0)                                         DENSAN46
         PIFAC = PIBY2**3                                               DENSAN47
         IONC = 1                                                       DENSAN48
      ENDIF                                                             DENSAN49
C Once-per-event initialization.                                        DENSAN50
      IF (NEWEV) THEN                                                   DENSAN51
         NEWEV = .FALSE.                                                DENSAN52
      ENDIF                                                             DENSAN53
      IF (PR1.EQ.3) THEN                                                DENSAN54
         SPMIN = X2MN34                                                 DENSAN55
         SPMAX = Y2MX34                                                 DENSAN56
         SQMAX = YTRP34                                                 DENSAN57
      ELSE                                                              DENSAN58
         SPMIN = X2MN56                                                 DENSAN59
         SPMAX = Y2MX56                                                 DENSAN60
         SQMAX = YTRP56                                                 DENSAN61
      ENDIF                                                             DENSAN62
C                                                                       DENSAN63
      LERR = .FALSE.                                                    DENSAN64
      SP = XM2(PR1,PR2)                                                 DENSAN65
      FAC1 = SP**ANSP                                                   DENSAN66
      IF ( kindqq(pr1) .EQ. 0 ) THEN                                    DENSAN67
        FAC3 = HILGTN(0.D0,ANSP,SPMIN,SPMAX)                            DENSAN68
      ELSE                                                              DENSAN69
        CALL qcd3(kindqq(pr1),1,spmin,spmax,fac3)                       DENSAN70
        fac1 = fac1 / rratio(sp,kindqq(pr1))                            DENSAN71
      ENDIF                                                             DENSAN72
C                                                                       DENSAN73
C Check range of 2-particle invariant mass.  Set DENS =0 if outside limiDENSAN74
      IF (SP.GT.SPMAX)  GOTO 999                                        DENSAN75
C                                                                       DENSAN76
C Check range of 3-particle invariant mass.  Set DENS =0 if outside limiDENSAN77
      SQ = XM3(PR1,PR2,IPROP)                                           DENSAN78
      IF (SQ.GT.SQMAX)  GOTO 999                                        DENSAN79
      FAC2 = SQ**ANSQ                                                   DENSAN80
C                                                                       DENSAN81
      SQMIN = (DSQRT(SP)+DMASS(IPROP))**2                               DENSAN82
      FAC4 = HILGTN(0.D0,ANSQ,SQMIN,SQMAX)                              DENSAN83
C                                                                       DENSAN84
C normalized 2-particle phase-space densities                           DENSAN85
      FAC7 = LAMBDA(SQ/SISR,MASS2(IPROP)/SISR)                          DENSAN86
      FAC8 = LAMBDA(SP/SQ,MASS2(IPROP)/SQ)                              DENSAN87
      FAC9 = LAMBDA(MASS2(PR1)/SP,MASS2(PR2)/SP)                        DENSAN88
C Now we handle the sp angular spectrum                                 DENSAN89
      C  =                                                              DENSAN90
     & (1.D0 + SQ/SISR - MASS2(IPROP)/SISR)*                            DENSAN91
     & (1.D0 + SP/SQ - MASS2(IPROP)/SQ) /                               DENSAN92
     & DSQRT(FAC7*FAC8)                                                 DENSAN93
C                                                                       DENSAN94
      DO 32 ID = 1, 4                                                   DENSAN95
         PL(ID) = PEVT(ID,PR1) + PEVT(ID,PR2)                           DENSAN96
         QL(ID) = PEVT(ID,IPROP) + PL(ID)                               DENSAN97
   32 CONTINUE                                                          DENSAN98
C                                                                       DENSAN99
C Learn cosine in QL rest frame between PL and QL                       DENSA100
      COSTH = COSPRI(QL,SQ,PL)                                          DENSA101
C Throw costh of sp system as 1/(c+costh)**ANCOST                       DENSA102
      FAC5 = (C+COSTH)**ANCOST                                          DENSA103
      FAC6 = HILGTN(C,ANCOST,-1.D0,1.D0)/2.D0                           DENSA104
C                                                                       DENSA105
      DFAC = PIFAC*FAC1*FAC2*FAC3*FAC4*FAC5*FAC6*                       DENSA106
     &        DSQRT(FAC7*FAC8*FAC9)                                     DENSA107
      DENSAN = 1.D0/DFAC                                                DENSA108
      RETURN                                                            DENSA109
C                                                                       DENSA110
  999 CONTINUE                                                          DENSA111
      DENSAN = 0.D0                                                     DENSA112
      END                                                               DENSA113
      FUNCTION DENSBB(P2,K1,K2,Q1,Q2,NEWEV,LERR)                        DENSBB 2
C-------------------------------------------------------------------    DENSBB 3
C Find the a posteriori densities expressing the probability for having DENSBB 4
C generated a given phase space configuration with our                  DENSBB 5
C backwards bremsstrahlung phase-space generators.                      DENSBB 6
C INPUT :                                                               DENSBB 7
C   P2   - the momentum label of the beam particle w/o fermion leg      DENSBB 8
C   K1   - the momentum label of the 1st paired particle                DENSBB 9
C   K2   - the momentum label of the 2nd paired particle                DENSBB10
C   Q1   - the momentum label of the `outgoing beam particle' w/        DENSBB11
C           fermion leg                                                 DENSBB12
C   Q2   - the momentum label of the `outgoing beam particle' w/o       DENSBB13
C           fermion leg                                                 DENSBB14
C OUTPUT :                                                              DENSBB15
C   LERR = .TRUE. if some non-physical situation is present             DENSBB16
C-------------------------------------------------------------------    DENSBB17
      IMPLICIT NONE                                                     DENSBB18
      SAVE                                                              DENSBB19
C Local variables                                                       DENSBB20
      LOGICAL LERR,NEWEV                                                DENSBB21
      INTEGER P2,K1,K2,Q1,Q2,N,M,L,ID,IONC                              DENSBB22
      REAL*8 DENSBB,B,C,XM2,XM3,LAMBDA,PIBY2,PI,PIFAC,FAC1,FAC2,FAC3,   DENSBB23
     &   FAC4,FAC5,FAC6,FAC7,FAC8,FAC9,FAC10,FAC11,F8F11,STRP,SK,CSQ2,  DENSBB24
     &   KVEC(4),Q3BOD(4),COSTH,DFAC,COSPRI,SQ,SQMIN,HILGTN             DENSBB25
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C BRANCH commons                                                        BRANCHC2
      INTEGER LCHMX,PAR1,PAR2,PAR3,PAR4,PAR5,PAR6,NEVTGN,NRJGN,         BRANCHC3
     &   NCHAN,NCHANT,GENRJC,LGENR,JCHAN,CHACOU,CHANLS,                 BRANCHC4
     &   LANNIH,LBREMF,LBREMB,LCONV1,LCONV2,LCON1Z,LCON2Z,LMULTI,LRAMBO BRANCHC5
      PARAMETER (LCHMX = 100, LGENR = 9, LANNIH = 1, LBREMF = 2,        BRANCHC6
     &   LBREMB = 3, LCONV1 = 4, LCONV2 = 5,  LCON1Z = 6, LCON2Z = 7,   BRANCHC7
     &   LMULTI = 8, LRAMBO =9)                                         BRANCHC8
      REAL*8 SMINP,SMAXP,SMX3,SKMIN,SKMAX,BRNCH,FRAC,CGEN               BRANCHC9
      COMMON /BRANCH/ PAR1(LCHMX),PAR2(LCHMX),PAR3(LCHMX),PAR4(LCHMX),  BRANCH10
     &   PAR5(LCHMX),PAR6(LCHMX),NEVTGN(LGENR),NRJGN(LGENR),NCHAN,      BRANCH11
     &   NCHANT,GENRJC(LCHMX),JCHAN(LBUNCH),CHACOU(0:LGENR),            BRANCH12
     &   CHANLS(LGENR),                                                 BRANCH13
     &   SKMIN,SKMAX,SMINP(LCHMX),SMAXP(LCHMX),SMX3(LCHMX),             BRANCH14
     &   BRNCH(0:LCHMX),FRAC(LCHMX),CGEN(LGENR)                         BRANCH15
      CHARACTER*16 GNNAME(LGENR)                                        BRANCH16
      COMMON /GNNAMS/ GNNAME                                            BRANCH17
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C Power laws for various distributions in our ps generators             POWERLW2
      REAL*8 ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ,BBCS2,   POWERLW3
     &       BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF             POWERLW4
      COMMON /DPWRS/ ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ, POWERLW5
     &      BBCS2, BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF       POWERLW6
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C setup commons                                                         SETUPCO2
      REAL*8 ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                          SETUPCO3
      COMMON /SETUP/ ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                  SETUPCO4
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
      DATA IONC /0/                                                     DENSBB34
C                                                                       DENSBB35
      XM2(N,M) = MASS2(N) + MASS2(M) + DEVT(N,M)                        DENSBB36
      XM3(L,N,M) = MASS2(L) + MASS2(N) + MASS2(M) + DEVT(L,N) +         DENSBB37
     &   DEVT(L,M) + DEVT(N,M)                                          DENSBB38
      LAMBDA(B,C) = 1.D0 + B*B + C*C - 2.D0*(B + C + B*C)               DENSBB39
C                                                                       DENSBB40
      LERR = .FALSE.                                                    DENSBB41
C                                                                       DENSBB42
C Once-per-job initialization.                                          DENSBB43
C                                                                       DENSBB44
      IF (IONC.EQ.0) THEN                                               DENSBB45
         PIBY2=2.D0*DATAN(1.D0)                                         DENSBB46
         PI=4.D0*DATAN(1.D0)                                            DENSBB47
         PIFAC = (PI/4.D0)**2*PIBY2                                     DENSBB48
         IONC = 1                                                       DENSBB49
      ENDIF                                                             DENSBB50
C Once-per-event initialization.                                        DENSBB51
      IF (NEWEV) THEN                                                   DENSBB52
         IF ( kindqq(k1) .EQ. 0 ) THEN                                  DENSBB53
           FAC8 = HILGTN(0.D0,BBSK,SKMIN,SKMAX)                         DENSBB54
         ELSE                                                           DENSBB55
           CALL qcd3(kindqq(k1),3,skmin,skmax,fac8)                     DENSBB56
         ENDIF                                                          DENSBB57
         FAC11 = -HILGTN(1.D0,BBCS2,CSCNEV,-CSCNEV)                     DENSBB58
         F8F11  = PIFAC*FAC8*FAC11                                      DENSBB59
         NEWEV = .FALSE.                                                DENSBB60
      ENDIF                                                             DENSBB61
      IF (K1.EQ.3) THEN                                                 DENSBB62
         STRP  = YTRP34                                                 DENSBB63
      ELSE                                                              DENSBB64
         STRP  = YTRP56                                                 DENSBB65
      ENDIF                                                             DENSBB66
      SK = XM2(K1,K2)                                                   DENSBB67
      IF (SK.GT.SKMAX) GOTO 999                                         DENSBB68
      FAC4 = SK**BBSK                                                   DENSBB69
      IF ( kindqq(k1) .NE. 0 ) fac4 = fac4 / rratio(sk,kindqq(k1))      DENSBB70
      SQ = XM3(K1,K2,Q1)                                                DENSBB71
      FAC5 = SQ**BBSQ                                                   DENSBB72
      IF (SQ.GT.STRP)  GOTO 999                                         DENSBB73
      FAC1 = 1.D0 - 4.D0*MASS2(K1)/SK                                   DENSBB74
      FAC2 = LAMBDA(SK/SQ,MASS2(Q1)/SQ)                                 DENSBB75
      FAC3 = LAMBDA(SQ/SISR,MASS2(Q1)/SISR)                             DENSBB76
      CSQ2 = PEVT(3,Q2)/DSQRT(PEVT(4,Q2)**2-MASS2(Q2))                  DENSBB77
C Account for beam direction                                            DENSBB78
      IF (PEVT(3,P2).LT.0.D0)  CSQ2 = -CSQ2                             DENSBB79
      FAC6 = (1.D0 - CSQ2)**BBCS2                                       DENSBB80
C                                                                       DENSBB81
C Reconstruct KVEC (k1,k2) and  (k1,k2,q1) momentum                     DENSBB82
      DO 11 ID = 1, 4                                                   DENSBB83
         KVEC(ID) = PEVT(ID,K1) + PEVT(ID,K2)                           DENSBB84
         Q3BOD(ID) = KVEC(ID) + PEVT(ID,Q1)                             DENSBB85
 11   CONTINUE                                                          DENSBB86
C Learn cosine in Q3BOD rest frame between KVEC and Q3BOD               DENSBB87
      COSTH = COSPRI(Q3BOD,SQ,KVEC)                                     DENSBB88
      C = (SISR+SQ)*(SQ+SK)/((SISR-SQ)*(SQ-SK))                         DENSBB89
      FAC7 = (C + COSTH)**BBCOST                                        DENSBB90
      SQMIN = (DSQRT(SK)+DMASS(Q1))**2                                  DENSBB91
      FAC9 = HILGTN(0.D0,BBSQ,SQMIN,STRP)                               DENSBB92
      FAC10 = HILGTN(C,BBCOST,-1.D0,1.D0)                               DENSBB93
      DFAC = DSQRT(FAC1*FAC2*FAC3)*FAC4*FAC5*FAC6*FAC7*FAC9*FAC10*      DENSBB94
     &   F8F11                                                          DENSBB95
      DENSBB = 1.D0/DFAC                                                DENSBB96
C                                                                       DENSBB97
      RETURN                                                            DENSBB98
  999 CONTINUE                                                          DENSBB99
      DENSBB = 0.D0                                                     DENSB100
      END                                                               DENSB101
      FUNCTION DENSBF(P1,P2,K1,K2,Q2,NEWEV,LERR)                        DENSBF 2
C-------------------------------------------------------------------    DENSBF 3
C Find the a posteriori densities expressing the probability for having DENSBF 4
C generated a given phase space configuration with one of our           DENSBF 5
C bremsstrahlung phase-space generators.                                DENSBF 6
C INPUT :                                                               DENSBF 7
C   P1   - the momentum label of the beam particle with fermion leg     DENSBF 8
C   P2   - the momentum label of the beam particle w/o fermion leg      DENSBF 9
C   K1   - the momentum label of the 1st paired particle                DENSBF10
C   K2   - the momentum label of the 2nd paired particle                DENSBF11
C   Q2   - the momentum label of the `outgoing beam particle' w/o       DENSBF12
C           fermion leg                                                 DENSBF13
C OUTPUT :                                                              DENSBF14
C   LERR = .TRUE. if some non-physical situation is present             DENSBF15
C-------------------------------------------------------------------    DENSBF16
      IMPLICIT NONE                                                     DENSBF17
      SAVE                                                              DENSBF18
C Local variables                                                       DENSBF19
      INTEGER P1,P2,K1,K2,Q2,M,N,IONC,                                  DENSBF20
     & ID                                                               DENSBF21
      REAL*8 XM2,XMM2,K(4),TWOPI,KMOM2,KMOM,CSQ2K,FAC1,FAC2,FAC3,       DENSBF22
     &   FAC4,FAC5,FAC6,FAC7,FAC8,FAC9,RFAC10,DENSBF,DFAC,SK,A,B,PIBY2, DENSBF23
     &   PIFAC,F2F9,KMASS,K0MAX,K01MAX,CSK,C,K02,BETA2,THK1,THKMN,      DENSBF24
     &   CSTHMN,CS2,HILGTN                                              DENSBF25
      LOGICAL LERR,NEWEV                                                DENSBF26
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C BRANCH commons                                                        BRANCHC2
      INTEGER LCHMX,PAR1,PAR2,PAR3,PAR4,PAR5,PAR6,NEVTGN,NRJGN,         BRANCHC3
     &   NCHAN,NCHANT,GENRJC,LGENR,JCHAN,CHACOU,CHANLS,                 BRANCHC4
     &   LANNIH,LBREMF,LBREMB,LCONV1,LCONV2,LCON1Z,LCON2Z,LMULTI,LRAMBO BRANCHC5
      PARAMETER (LCHMX = 100, LGENR = 9, LANNIH = 1, LBREMF = 2,        BRANCHC6
     &   LBREMB = 3, LCONV1 = 4, LCONV2 = 5,  LCON1Z = 6, LCON2Z = 7,   BRANCHC7
     &   LMULTI = 8, LRAMBO =9)                                         BRANCHC8
      REAL*8 SMINP,SMAXP,SMX3,SKMIN,SKMAX,BRNCH,FRAC,CGEN               BRANCHC9
      COMMON /BRANCH/ PAR1(LCHMX),PAR2(LCHMX),PAR3(LCHMX),PAR4(LCHMX),  BRANCH10
     &   PAR5(LCHMX),PAR6(LCHMX),NEVTGN(LGENR),NRJGN(LGENR),NCHAN,      BRANCH11
     &   NCHANT,GENRJC(LCHMX),JCHAN(LBUNCH),CHACOU(0:LGENR),            BRANCH12
     &   CHANLS(LGENR),                                                 BRANCH13
     &   SKMIN,SKMAX,SMINP(LCHMX),SMAXP(LCHMX),SMX3(LCHMX),             BRANCH14
     &   BRNCH(0:LCHMX),FRAC(LCHMX),CGEN(LGENR)                         BRANCH15
      CHARACTER*16 GNNAME(LGENR)                                        BRANCH16
      COMMON /GNNAMS/ GNNAME                                            BRANCH17
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C Power laws for various distributions in our ps generators             POWERLW2
      REAL*8 ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ,BBCS2,   POWERLW3
     &       BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF             POWERLW4
      COMMON /DPWRS/ ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ, POWERLW5
     &      BBCS2, BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF       POWERLW6
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C setup commons                                                         SETUPCO2
      REAL*8 ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                          SETUPCO3
      COMMON /SETUP/ ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                  SETUPCO4
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
      DATA IONC /0/                                                     DENSBF35
C                                                                       DENSBF36
      XM2(N,M) = MASS2(N) + MASS2(M) + DEVT(N,M)                        DENSBF37
      XMM2(N,M) = MASS2(N) + MASS2(M) - DEVT(N,M)                       DENSBF38
C                                                                       DENSBF39
      LERR = .FALSE.                                                    DENSBF40
C                                                                       DENSBF41
C Once-per-job initialization.                                          DENSBF42
C                                                                       DENSBF43
      IF (IONC.EQ.0) THEN                                               DENSBF44
         PIBY2=2.D0*DATAN(1.D0)                                         DENSBF45
         TWOPI=8.D0*DATAN(1.D0)                                         DENSBF46
         PIFAC = TWOPI**2*PIBY2                                         DENSBF47
         IONC = 1                                                       DENSBF48
      ENDIF                                                             DENSBF49
C Once-per-event initialization.                                        DENSBF50
      IF (NEWEV) THEN                                                   DENSBF51
         IF ( kindqq(k1) .EQ. 0 ) THEN                                  DENSBF52
           FAC2 = HILGTN(0.D0,BFSK,SKMIN,SKMAX)                         DENSBF53
         ELSE                                                           DENSBF54
           CALL qcd3(kindqq(k1),2,skmin,skmax,fac2)                     DENSBF55
         ENDIF                                                          DENSBF56
         FAC9 = -HILGTN(1.D0,BFCS2,CSCNEV,-CSCNEV)                      DENSBF57
         F2F9  = PIFAC*FAC2*FAC9                                        DENSBF58
         NEWEV = .FALSE.                                                DENSBF59
      ENDIF                                                             DENSBF60
      SK = XM2(K1,K2)                                                   DENSBF61
      IF (SK.GT.SKMAX) GOTO 999                                         DENSBF62
      FAC1 = SK**BFSK                                                   DENSBF63
      IF ( kindqq(k1) .NE. 0 ) fac1 = fac1 / rratio(sk,kindqq(k1))      DENSBF64
      KMASS = DSQRT(SK)                                                 DENSBF65
C                                                                       DENSBF66
C Reconstruct K's momentum                                              DENSBF67
      KMOM2 = 0.D0                                                      DENSBF68
      DO 11 ID = 1, 4                                                   DENSBF69
   11    K(ID) = PEVT(ID,K1) + PEVT(ID,K2)                              DENSBF70
      K01MAX = (SISR+SK-Q2MN)/(2.D0*ECMISR)                             DENSBF71
      K0MAX = KBOD2*(K01MAX - KMASS) + KMASS                            DENSBF72
      IF (K(4).GT.K0MAX)  GOTO 999                                      DENSBF73
      KMOM2 = K(4)*K(4) - SK                                            DENSBF74
      KMOM = DSQRT(KMOM2)                                               DENSBF75
C                                                                       DENSBF76
C     FAC3 = DABS(XMM2(P2,Q2))                                          DENSBF77
C Try this one!                                                         DENSBF78
      CS2 = PEVT(3,Q2)/DSQRT(PEVT(4,Q2)**2 - MASS2(Q2))                 DENSBF79
      IF (PEVT(3,P1).GT.0.D0)  CS2 = -CS2                               DENSBF80
      FAC3 = ECMISR*PEVT(4,Q2)*(1.D0-CS2)**BFCS2                        DENSBF81
      FAC4 = K(4)**BFK0                                                 DENSBF82
      FAC5 = HILGTN(0.D0,BFK0,KMASS,K0MAX)                              DENSBF83
C                                                                       DENSBF84
C Explicitly assume that the beam particles are along +/- z axis        DENSBF85
      CSK = K(3)/DSQRT(K(1)**2+K(2)**2+K(3)**2)                         DENSBF86
      IF (PEVT(3,P1).LT.0.D0)  CSK = -CSK                               DENSBF87
      A = ECMISR*K(4) - SK                                              DENSBF88
      B = -ECMISR*KMOM                                                  DENSBF89
      C = -A/B                                                          DENSBF90
C First find efficient limits                                           DENSBF91
      K02 = K(4)*K(4)                                                   DENSBF92
      BETA2 = 1.D0 - 4.D0*MASS2(K1)/K02                                 DENSBF93
      THK1 = THCNEV - 0.5D0*ACOS(1.D0/BETA2*(1.D0 -                     DENSBF94
     &    2.D0*(SK - 2.D0*MASS2(K1))/K02))                              DENSBF95
      THKMN = MAX(THK1,0.D0)                                            DENSBF96
      CSTHMN = COS(THKMN)                                               DENSBF97
C If final state is neutral, remove cos theta cutoff                    DENSBF98
      IF (DCHA(K1).EQ.0.D0)  CSTHMN = 0.999D0                           DENSBF99
      FAC6 = (C-CSK)**BFCOST                                            DENSB100
      FAC7 = -HILGTN(C,BFCOST,CSTHMN,-CSTHMN)*KMOM/(4.D0*ECMISR)        DENSB101
      FAC8 = DSQRT(1.D0 - 4.D0*MASS2(K1)/SK)                            DENSB102
C                                                                       DENSB103
      CSQ2K = (K(1)*PEVT(1,Q2)+K(2)*PEVT(2,Q2)+K(3)*PEVT(3,Q2))         DENSB104
     &   /DSQRT(KMOM2*(PEVT(4,Q2)**2 - MASS2(Q2)))                      DENSB105
      RFAC10 = 2.D0*(ECMISR + KMOM*CSQ2K - K(4))                        DENSB106
      DFAC = F2F9*FAC1*FAC3*FAC4*FAC5*FAC6*FAC7*FAC8                    DENSB107
      DENSBF = RFAC10/DFAC                                              DENSB108
C                                                                       DENSB109
      RETURN                                                            DENSB110
  999 CONTINUE                                                          DENSB111
      DENSBF = 0.D0                                                     DENSB112
      END                                                               DENSB113
      FUNCTION DENSC1(K1,K2,Q1,Q2,NEWEV,LERR)                           DENSC1 2
C-------------------------------------------------------------------    DENSC1 3
C Find the a posteriori densities expressing the probability for having DENSC1 4
C generated a given phase space configuration with one of our           DENSC1 5
C conversion phase-space generators.                                    DENSC1 6
C INPUT :                                                               DENSC1 7
C K1,K2,Q1,Q2 - 4-momentum labels                                       DENSC1 8
C NEWEV = .TRUE. if first call of new event                             DENSC1 9
C OUTPUT :                                                              DENSC110
C   LERR = .TRUE. if some non-physical situation is present             DENSC111
C-------------------------------------------------------------------    DENSC112
      IMPLICIT NONE                                                     DENSC113
      SAVE                                                              DENSC114
C Local variables                                                       DENSC115
      INTEGER K1,K2,Q1,Q2,IONC,N,M                                      DENSC116
      REAL*8 DENSC1,PI,PIBY2,PIFAC,SKMAX,SKMIN,SK,KMASS,                DENSC117
     &   SQ,FAC1,FAC2,FAC3,FAC4,FAC5,FAC6,FAC7,FAC8,K0,KMOM,CSK,        DENSC118
     &   BETA2,THK1,K02,THKMN,K0MAX,SPRMIN,                             DENSC119
     &   A,B,C,CSTHMX,CSTHMN,F1,F2,DFAC,LAMBDA,XM2,HILGTN               DENSC120
      LOGICAL NEWEV,LERR                                                DENSC121
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C Power laws for various distributions in our ps generators             POWERLW2
      REAL*8 ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ,BBCS2,   POWERLW3
     &       BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF             POWERLW4
      COMMON /DPWRS/ ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ, POWERLW5
     &      BBCS2, BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF       POWERLW6
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C setup commons                                                         SETUPCO2
      REAL*8 ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                          SETUPCO3
      COMMON /SETUP/ ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                  SETUPCO4
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
C                                                                       DENSC128
      DATA IONC /0/                                                     DENSC129
      LAMBDA(B,C) = 1.D0 + B*B + C*C - 2.D0*(B + C + B*C)               DENSC130
      XM2(N,M) = MASS2(N) + MASS2(M) + DEVT(N,M)                        DENSC131
C                                                                       DENSC132
      LERR = .FALSE.                                                    DENSC133
C                                                                       DENSC134
C Once-per-job initialization.                                          DENSC135
C                                                                       DENSC136
      IF (IONC.EQ.0) THEN                                               DENSC137
         PIBY2=2.D0*DATAN(1.D0)                                         DENSC138
         PI=4.D0*DATAN(1.D0)                                            DENSC139
         PIFAC = PIBY2**2*PI*2.D0                                       DENSC140
         IONC = 1                                                       DENSC141
      ENDIF                                                             DENSC142
      IF (K1.EQ.3) THEN                                                 DENSC143
         SKMAX = Y2MX34                                                 DENSC144
         SKMIN = X2MN34                                                 DENSC145
         SPRMIN = X2MN56                                                DENSC146
      ELSE                                                              DENSC147
         SKMAX = Y2MX56                                                 DENSC148
         SKMIN = X2MN56                                                 DENSC149
         SPRMIN = X2MN34                                                DENSC150
      ENDIF                                                             DENSC151
      SK = XM2(K1,K2)                                                   DENSC152
C Check inv. mass of (k1,k2) system                                     DENSC153
      IF (SK.GT.SKMAX)  GOTO 999                                        DENSC154
C Check energy  of (k1,k2) system                                       DENSC155
      KMASS = DSQRT(SK)                                                 DENSC156
      K0   = PEVT(4,K1)+PEVT(4,K2)                                      DENSC157
      K0MAX = (SISR+SK-SPRMIN)/(2.D0*ECMISR)                            DENSC158
      IF (K0.GT.K0MAX)  GOTO 999                                        DENSC159
      K02  = K0*K0                                                      DENSC160
      KMOM = DSQRT(K02 - SK)                                            DENSC161
      FAC1 = SK**C1SK                                                   DENSC162
      IF ( kindqq(k1) .EQ. 0 ) THEN                                     DENSC163
        FAC2 = HILGTN(0.D0,C1SK,SKMIN,SKMAX)                            DENSC164
      ELSE                                                              DENSC165
        CALL qcd3(kindqq(k1),4,skmin,skmax,fac2)                        DENSC166
        fac1 = fac1 / rratio(sk,kindqq(k1))                             DENSC167
      ENDIF                                                             DENSC168
      FAC3 = .5D0*KMOM*K0**C1K0                                         DENSC169
      FAC4 = HILGTN(0.D0,C1K0,KMASS,K0MAX)                              DENSC170
      A = SK - ECMISR*K0                                                DENSC171
      B = ECMISR*KMOM                                                   DENSC172
      CSK  = (PEVT(3,K1)+PEVT(3,K2))/KMOM                               DENSC173
      FAC5 = (A*A - (B*CSK)**2)                                         DENSC174
C First find efficient limits                                           DENSC175
      BETA2 = 1.D0 - 4.D0*MASS2(K1)/K02                                 DENSC176
      THK1 = THCNEV - 0.5D0*ACOS(1.D0/BETA2*(1.D0 -                     DENSC177
     &    2.D0*(SK - 2.D0*MASS2(K1))/K02))                              DENSC178
      THKMN = MAX(THK1,0.D0)                                            DENSC179
      CSTHMN = COS(THKMN)                                               DENSC180
C If final state is neutral, remove cos theta cutoff                    DENSC181
      IF (DCHA(K1).EQ.0.D0)  CSTHMN = 0.999D0                           DENSC182
      CSTHMX = -CSTHMN                                                  DENSC183
      IF (CSK.GT.CSTHMN .OR. CSK.LT.CSTHMX)  GOTO 999                   DENSC184
C ld                                                                    DENSC185
C     if ( (A+B*CSTHMN)/(A-B*CSTHMN) .lt. 1e-70 ) then                  DENSC186
C       print *,'+++DENSC1+++ something went wrong: log ',a,b,CSTHMN    DENSC187
C       goto 999                                                        DENSC188
C     endif                                                             DENSC189
C ld                                                                    DENSC190
      FAC6 = DLOG( (A+B*CSTHMN)/(A-B*CSTHMN) )/(A*B)                    DENSC191
C                                                                       DENSC192
      SQ = (ECMISR - K0)**2 - KMOM*KMOM                                 DENSC193
      F1 = MASS2(K1)/SK                                                 DENSC194
      F2 = MASS2(Q1)/SQ                                                 DENSC195
      FAC7 = LAMBDA(F1,F1)                                              DENSC196
      FAC8 = LAMBDA(F2,F2)                                              DENSC197
C Put it all together                                                   DENSC198
      DFAC = PIFAC*FAC1*FAC2*FAC3*FAC4*FAC5*FAC6*DSQRT(FAC7*FAC8)       DENSC199
      DENSC1 = 1.D0/DFAC                                                DENSC100
      RETURN                                                            DENSC101
 999  CONTINUE                                                          DENSC102
      DENSC1 = 0.D0                                                     DENSC103
      END                                                               DENSC104
      FUNCTION DENSC2(K1,K2,P1,P2,NEWEV,LERR)                           DENSC2 2
C-------------------------------------------------------------------    DENSC2 3
C Find the a posteriori densities expressing the probability for having DENSC2 4
C generated a given phase space configuration with one of our           DENSC2 5
C conversion phase-space generators.                                    DENSC2 6
C INPUT :                                                               DENSC2 7
C K1,K2,P1,P2 - 4-momentum labels                                       DENSC2 8
C NEWEV = .TRUE. if first call of new event                             DENSC2 9
C OUTPUT :                                                              DENSC210
C   LERR = .TRUE. if some non-physical situation is present             DENSC211
C-------------------------------------------------------------------    DENSC212
      IMPLICIT NONE                                                     DENSC213
      SAVE                                                              DENSC214
C Local variables                                                       DENSC215
      INTEGER K1,K2,P1,P2,IONC,N,M                                      DENSC216
      REAL*8 DENSC2,PI,PIBY2,PIFAC,F7F8,SKMAX,SPMAX,SK,                 DENSC217
     &   SP,SKS,SPS,FAC1,FAC2,FAC3,FAC4,FAC5,FAC6,FAC9,K0,KMOM,CSK,     DENSC218
     &   BETA2,THK1,K02,P0,P02,THK2,THKMN,                              DENSC219
     &   B,C,CSTHMN,DFAC,LAMBDA,XM2,HILGTN                              DENSC220
      LOGICAL NEWEV,LERR                                                DENSC221
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C Power laws for various distributions in our ps generators             POWERLW2
      REAL*8 ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ,BBCS2,   POWERLW3
     &       BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF             POWERLW4
      COMMON /DPWRS/ ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ, POWERLW5
     &      BBCS2, BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF       POWERLW6
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C setup commons                                                         SETUPCO2
      REAL*8 ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                          SETUPCO3
      COMMON /SETUP/ ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                  SETUPCO4
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
C                                                                       DENSC228
      DATA IONC /0/                                                     DENSC229
      LAMBDA(B,C) = 1.D0 + B*B + C*C - 2.D0*(B + C + B*C)               DENSC230
      XM2(N,M) = MASS2(N) + MASS2(M) + DEVT(N,M)                        DENSC231
C                                                                       DENSC232
      LERR = .FALSE.                                                    DENSC233
C                                                                       DENSC234
C Once-per-job initialization.                                          DENSC235
C                                                                       DENSC236
      IF (IONC.EQ.0) THEN                                               DENSC237
         PIBY2=2.D0*DATAN(1.D0)                                         DENSC238
         PI=4.D0*DATAN(1.D0)                                            DENSC239
         PIFAC = PIBY2**2*PI/4.D0                                       DENSC240
         IONC = 1                                                       DENSC241
      ENDIF                                                             DENSC242
C Once-per-event initialization.                                        DENSC243
      IF (NEWEV) THEN                                                   DENSC244
         NEWEV = .FALSE.                                                DENSC245
         IF     ( kindqq(k1) .EQ. 0 .AND. kindqq(p1) .EQ. 0 ) THEN      DENSC246
           F7F8 = PIFAC*HILGTN(0.D0,C2SKP,X2MN34,Y2MX34)*               DENSC247
     &                  HILGTN(0.D0,C2SKP,X2MN56,Y2MX56)                DENSC248
         ELSEIF ( kindqq(k1) .NE. 0 ) THEN                              DENSC249
           CALL qcd3(kindqq(k1),5,x2mn34,y2mx34,f7f8)                   DENSC250
           F7F8 = F7F8*PIFAC*HILGTN(0.D0,C2SKP,X2MN56,Y2MX56)           DENSC251
         ELSEIF ( kindqq(p1) .NE. 0 ) THEN                              DENSC252
           CALL qcd3(kindqq(p1),5,x2mn56,y2mx56,f7f8)                   DENSC253
           F7F8 = F7F8*PIFAC*HILGTN(0.D0,C2SKP,X2MN34,Y2MX34)           DENSC254
         ENDIF                                                          DENSC255
      ENDIF                                                             DENSC256
      IF (K1.EQ.3) THEN                                                 DENSC257
         SKMAX = Y2MX34                                                 DENSC258
         SPMAX = Y2MX56                                                 DENSC259
      ELSE                                                              DENSC260
         SKMAX = Y2MX56                                                 DENSC261
         SPMAX = Y2MX34                                                 DENSC262
      ENDIF                                                             DENSC263
      SK = XM2(K1,K2)                                                   DENSC264
      SP = XM2(P1,P2)                                                   DENSC265
      IF (SK.GT.SKMAX .OR. SP.GT.SPMAX)  GOTO 999                       DENSC266
      SKS = SK/SISR                                                     DENSC267
      SPS = SP/SISR                                                     DENSC268
      FAC1 = LAMBDA(SKS,SPS)                                            DENSC269
      FAC2 = 1.D0-4.D0*MASS2(K1)/SK                                     DENSC270
      FAC3 = 1.D0-4.D0*MASS2(P1)/SP                                     DENSC271
      K0   = PEVT(4,K1)+PEVT(4,K2)                                      DENSC272
      K02  = K0*K0                                                      DENSC273
      KMOM = DSQRT(K02 - SK)                                            DENSC274
      CSK  = (PEVT(3,K1)+PEVT(3,K2))/KMOM                               DENSC275
C First find efficient limits                                           DENSC276
      BETA2 = 1.D0 - 4.D0*MASS2(K1)/K02                                 DENSC277
      THK1 = THCNEV - 0.5D0*ACOS(1.D0/BETA2*(1.D0 -                     DENSC278
     &    2.D0*(SK - 2.D0*MASS2(K1))/K02))                              DENSC279
      P0 =   PEVT(4,P1)+PEVT(4,P2)                                      DENSC280
      P02 = P0*P0                                                       DENSC281
      BETA2 = 1.D0 - 4.D0*MASS2(P1)/P02                                 DENSC282
      THK2 = THCNEV - 0.5D0*ACOS(1.D0/BETA2*(1.D0 -                     DENSC283
     &    2.D0*(SP - 2.D0*MASS2(P1))/P02))                              DENSC284
C If final state is neutral, remove angular cutoff                      DENSC285
      IF (DCHA(K1).EQ.0.D0)  THK1 = 1.4D-2                              DENSC286
      IF (DCHA(P1).EQ.0.D0)  THK2 = 1.4D-2                              DENSC287
      THKMN = MAX(THK1,THK2,0.D0)                                       DENSC288
      CSTHMN = COS(THKMN)                                               DENSC289
C                                                                       DENSC290
      IF (CSK.GT.CSTHMN .OR. CSK.LT.-CSTHMN)  GOTO 999                  DENSC291
      B    = ECMISR*KMOM                                                DENSC292
      C  = (ECMISR*K0 - SK)/B                                           DENSC293
      FAC4 = (C-CSK)**C2CSK                                             DENSC294
      FAC5 = SK**C2SKP                                                  DENSC295
      FAC6 = SP**C2SKP                                                  DENSC296
      IF ( kindqq(k1) .NE. 0 ) fac5 = fac5 / rratio(sk,kindqq(k1))      DENSC297
      IF ( kindqq(p1) .NE. 0 ) fac6 = fac6 / rratio(sp,kindqq(p1))      DENSC298
      FAC9 = -HILGTN(C,C2CSK,CSTHMN,-CSTHMN)                            DENSC299
C Put it all together                                                   DENSC100
      DFAC = DSQRT(FAC1*FAC2*FAC3)*FAC4*FAC5*FAC6*F7F8*FAC9             DENSC101
      DENSC2 = 1.D0/DFAC                                                DENSC102
      RETURN                                                            DENSC103
 999  CONTINUE                                                          DENSC104
      DENSC2 = 0.D0                                                     DENSC105
      END                                                               DENSC106
      FUNCTION DENSMU(K1,K2,Q1,Q2,NEWEV,LERR)                           DENSMU 2
C-------------------------------------------------------------------    DENSMU 3
C Find the a posteriori densities expressing the probability for having DENSMU 4
C generated a given phase space configuration with  our                 DENSMU 5
C multiperipheral phase-space generator.                                DENSMU 6
C INPUT :                                                               DENSMU 7
C K1,K2,Q1,Q2 - 4-momentum labels                                       DENSMU 8
C NEWEV = .TRUE. if first call of new event                             DENSMU 9
C OUTPUT :                                                              DENSMU10
C   LERR = .TRUE. if some non-physical situation is present             DENSMU11
C-------------------------------------------------------------------    DENSMU12
      IMPLICIT NONE                                                     DENSMU13
      SAVE                                                              DENSMU14
C Local variables                                                       DENSMU15
      INTEGER K1,K2,Q1,Q2,IONC,N,M                                      DENSMU16
      REAL*8 DENSMU,PI,PIBY2,TWOPI,PIFAC,SKMIN,SK,                      DENSMU17
     &   FAC1,FAC2,FAC3,FAC4,FAC5,FAC6,FAC7,FAC8,FAC9,                  DENSMU18
     &   DOT3,BETA,TAU,Z,CS1,CS2,Q10,Q20,                               DENSMU19
     &   F1,DFAC,LAMBDA,XM2,B,C,HILGTN                                  DENSMU20
      LOGICAL NEWEV,LERR                                                DENSMU21
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C Power laws for various distributions in our ps generators             POWERLW2
      REAL*8 ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ,BBCS2,   POWERLW3
     &       BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF             POWERLW4
      COMMON /DPWRS/ ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ, POWERLW5
     &      BBCS2, BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF       POWERLW6
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C setup commons                                                         SETUPCO2
      REAL*8 ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                          SETUPCO3
      COMMON /SETUP/ ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                  SETUPCO4
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
C                                                                       DENSMU28
      DATA IONC /0/                                                     DENSMU29
C     DATA SKOFF /50.D0/                                                DENSMU30
      LAMBDA(B,C) = 1.D0 + B*B + C*C - 2.D0*(B + C + B*C)               DENSMU31
      XM2(N,M) = MASS2(N) + MASS2(M) + DEVT(N,M)                        DENSMU32
C                                                                       DENSMU33
      LERR = .FALSE.                                                    DENSMU34
C                                                                       DENSMU35
C Once-per-job initialization.                                          DENSMU36
C                                                                       DENSMU37
      IF (IONC.EQ.0) THEN                                               DENSMU38
         PIBY2=2.D0*DATAN(1.D0)                                         DENSMU39
         PI=4.D0*DATAN(1.D0)                                            DENSMU40
         TWOPI = 2.D0*PI                                                DENSMU41
         PIFAC = PIBY2*TWOPI**2                                         DENSMU42
         IONC = 1                                                       DENSMU43
      ENDIF                                                             DENSMU44
      IF (NEWEV) THEN                                                   DENSMU45
         FAC6 = (-HILGTN(1.D0,MUCOST,CSCNEV,-CSCNEV))**2                DENSMU46
         IF (K1.EQ.3) THEN                                              DENSMU47
            SKMIN = X2MN34                                              DENSMU48
         ELSE                                                           DENSMU49
            SKMIN = X2MN56                                              DENSMU50
         ENDIF                                                          DENSMU51
         NEWEV = .FALSE.                                                DENSMU52
      ENDIF                                                             DENSMU53
      SK = XM2(K1,K2)                                                   DENSMU54
C Check inv. mass of (k1,k2) system                                     DENSMU55
      IF (SK.GT.Y2MXKM)  GOTO 999                                       DENSMU56
C Check angles of Q1 and Q2. Explicitly assume massless particles.      DENSMU57
      CS1 = PEVT(3,Q1)/PEVT(4,Q1)                                       DENSMU58
      CS2 = PEVT(3,Q2)/PEVT(4,Q2)                                       DENSMU59
C Flip Q2's angle                                                       DENSMU60
      CS2 = -CS2                                                        DENSMU61
      FAC1 = (SK + SKOFF)**MUSK                                         DENSMU62
      IF ( kindqq(k1) .EQ. 0 ) THEN                                     DENSMU63
        FAC2 = HILGTN(SKOFF,MUSK,SKMIN,Y2MXKM)                          DENSMU64
      ELSE                                                              DENSMU65
        CALL qcd3(kindqq(k1),8,skmin,y2mxkm,fac2)                       DENSMU66
        fac1 = fac1 / rratio(sk,kindqq(k1))                             DENSMU67
      ENDIF                                                             DENSMU68
      Q10 = PEVT(4,Q1)                                                  DENSMU69
      Q20 = PEVT(4,Q2)                                                  DENSMU70
      FAC3 = .5D0*Q10                                                   DENSMU71
      FAC4 = .5D0*Q20                                                   DENSMU72
      FAC5 = (1.D0 - CS1)**MUCOST                                       DENSMU73
      FAC7 = (1.D0 - CS2)**MUCOST                                       DENSMU74
      Z    = DOT3(PEVT(1,Q1),PEVT(1,Q2))/(Q10*Q20)                      DENSMU75
      TAU  = (1.D0 - Z)/2.D0                                            DENSMU76
      BETA = 1.D0 - SK/SISR                                             DENSMU77
      FAC8 = -DLOG(1.D0 - BETA*TAU)/(4.D0*TAU)                          DENSMU78
      F1 = MASS2(K1)/SK                                                 DENSMU79
      FAC9 = LAMBDA(F1,F1)                                              DENSMU80
C Put it all together                                                   DENSMU81
      DFAC = PIFAC*FAC1*FAC2*FAC3*FAC4*FAC5*FAC6*FAC7*FAC8*DSQRT(FAC9)  DENSMU82
      DENSMU = 1.D0/DFAC                                                DENSMU83
      RETURN                                                            DENSMU84
 999  CONTINUE                                                          DENSMU85
      DENSMU = 0.D0                                                     DENSMU86
      END                                                               DENSMU87
      SUBROUTINE DIAGFV(DIAGFN)                                         DIAGFV 2
C-----------------------------------------------------------------------DIAGFV 3
C COMPUTE THE FEYNMAN DIAGRAM for a given helicity configuration        DIAGFV 4
C *** WARNING *** Watch out for CMPLX vs DCMPLX here                    DIAGFV 5
C-----------------------------------------------------------------------DIAGFV 6
      IMPLICIT NONE                                                     DIAGFV 7
      SAVE                                                              DIAGFV 8
C Local variables                                                       DIAGFV 9
      INTEGER IN,K,KI,LI                                                DIAGFV10
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
C Some physics constants                                                CONSTPH2
      REAL*8 ALPHA,RMZ,RGZ,SW2,RMZ2,RMZRGZ,RMZGZ2                       CONSTPH3
      COMMON / FISIKX / ALPHA,RMZ,RGZ,SW2,RMZ2,RMZRGZ,RMZGZ2            CONSTPH4
C DIAGFV commons                                                        DIAGFVC2
      INTEGER K1,K2,K3,K4,K5,K6                                         DIAGFVC3
      LOGICAL Z1FL,Z2FL,QMASK                                           DIAGFVC4
      COMMON /DIACOM/ K1,K2,K3,K4,K5,K6,Z1FL(LBUNCH),Z2FL(LBUNCH),      DIAGFVC5
     &   QMASK(LBUNCH)                                                  DIAGFVC6
C GFUNV commons                                                         GFUNVC 2
      LOGICAL VMASK                                                     GFUNVC 3
      COMMON /GFNCOM/ VMASK(LBUNCH)                                     GFUNVC 4
      COMPLEX*16 DIAGFN(LBUNCH)                                         DIAGFV18
C The helicities of the fermions                                        HELICOF2
      INTEGER HELI(6)                                                   HELICOF3
      COMMON / HELICO / HELI                                            HELICOF4
C                                                                       DIAGFV20
      REAL*8 FPROP(LBUNCH),S12(LBUNCH),S56(LBUNCH),PHS12(LBUNCH),       DIAGFV21
     &   PHS56(LBUNCH)                                                  DIAGFV22
      COMPLEX*16 Z0S12(LBUNCH),Z0S56(LBUNCH)                            DIAGFV23
      COMPLEX*16 GFUN1(LBUNCH),GFUN2(LBUNCH),ZZDIAG(LBUNCH),            DIAGFV24
     &   ZGDIAG(LBUNCH),GZDIAG(LBUNCH),GGDIAG(LBUNCH)                   DIAGFV25
C                                                                       DIAGFV26
C THE PHOTON AND Z0 PROPAGATORS OF THE 1/2 AND 5/6 CURRENTS             DIAGFV27
      DO 11 IN = 1, NEVTIN                                              DIAGFV28
         S12(IN)=MASS2(K1)+MASS2(K2)+BFAC(K1)*BFAC(K2)*D(IN,K1,K2)      DIAGFV29
         S56(IN)=MASS2(K5)+MASS2(K6)+BFAC(K5)*BFAC(K6)*D(IN,K5,K6)      DIAGFV30
         PHS12(IN) = S12(IN)                                            DIAGFV31
         Z0S12(IN) =  CMPLX(S12(IN)-RMZ2,RMZRGZ)                        DIAGFV32
         PHS56(IN) = S56(IN)                                            DIAGFV33
         Z0S56(IN) =  CMPLX(S56(IN)-RMZ2,RMZRGZ)                        DIAGFV34
C                                                                       DIAGFV35
C FERMION PROPAGATOR                                                    DIAGFV36
         FPROP(IN) =                                                    DIAGFV37
     &     MASS(K1)*MASS(K1) + MASS(K2)*MASS(K2) +                      DIAGFV38
     &     BFAC(K1)*BFAC(K2)*D(IN,K1,K2) + BFAC(K1)*BFAC(K3)*D(IN,K1,K3)DIAGFV39
     &     + BFAC(K2)*BFAC(K3)*D(IN,K2,K3)                              DIAGFV40
   11 CONTINUE                                                          DIAGFV41
C                                                                       DIAGFV42
C THE NUMERATOR OF THE ZZ DIAGRAMS                                      DIAGFV43
      DO 60 IN = 1, NEVTIN                                              DIAGFV44
   60    ZZDIAG(IN)=(0.D0,0.D0)                                         DIAGFV45
C Build vector mask                                                     DIAGFV46
      DO 65 IN = 1, NEVTIN                                              DIAGFV47
   65    VMASK(IN) = QMASK(IN).OR.Z1FL(IN).OR.Z2FL(IN)                  DIAGFV48
      DO 62 K=1,3                                                       DIAGFV49
        IF(K.EQ.1) KI=K1                                                DIAGFV50
        IF(K.EQ.2) KI=K2                                                DIAGFV51
        IF(K.EQ.3) KI=K3                                                DIAGFV52
        DO 61 LI=1,-1,-2                                                DIAGFV53
           CALL GFUNV(K1,HELI(K1),K2,HELI(K2),VF(K1),AF(K1),            DIAGFV54
     .           K3,HELI(K3),KI,LI      ,VF(K3),AF(K3),GFUN1)           DIAGFV55
           CALL GFUNV(KI,LI      ,K4,HELI(K4),VF(K4),AF(K4),            DIAGFV56
     .           K5,HELI(K5),K6,HELI(K6),VF(K5),AF(K5),GFUN2)           DIAGFV57
           DO 61 IN = 1, NEVTIN                                         DIAGFV58
   61         IF (.NOT.VMASK(IN)) ZZDIAG(IN) = ZZDIAG(IN) +             DIAGFV59
     &           BFAC(KI)*GFUN1(IN)*GFUN2(IN)                           DIAGFV60
   62 CONTINUE                                                          DIAGFV61
C                                                                       DIAGFV62
C THE NUMERATOR OF THE GZ DIAGRAMS                                      DIAGFV63
      DO 70 IN = 1, NEVTIN                                              DIAGFV64
   70    GZDIAG(IN)=(0.D0,0.D0)                                         DIAGFV65
C Build vector mask                                                     DIAGFV66
      DO 75 IN = 1, NEVTIN                                              DIAGFV67
   75    VMASK(IN) = QMASK(IN).OR.Z2FL(IN)                              DIAGFV68
      DO 72 K=1,3                                                       DIAGFV69
        IF(K.EQ.1) KI=K1                                                DIAGFV70
        IF(K.EQ.2) KI=K2                                                DIAGFV71
        IF(K.EQ.3) KI=K3                                                DIAGFV72
        DO 71 LI=1,-1,-2                                                DIAGFV73
           CALL GFUNV(K1,HELI(K1),K2,HELI(K2),CHA(K1),0.D0,             DIAGFV74
     .           K3,HELI(K3),KI,LI      ,CHA(K3),0.D0,GFUN1)            DIAGFV75
           CALL GFUNV(KI,LI      ,K4,HELI(K4),VF(K4),AF(K4),            DIAGFV76
     .           K5,HELI(K5),K6,HELI(K6),VF(K5),AF(K5),GFUN2)           DIAGFV77
           DO 71 IN = 1, NEVTIN                                         DIAGFV78
   71         IF (.NOT.VMASK(IN)) GZDIAG(IN) = GZDIAG(IN) +             DIAGFV79
     &           BFAC(KI)*GFUN1(IN)*GFUN2(IN)                           DIAGFV80
   72 CONTINUE                                                          DIAGFV81
C                                                                       DIAGFV82
C THE NUMERATOR OF THE ZG DIAGRAMS                                      DIAGFV83
      DO 80 IN = 1, NEVTIN                                              DIAGFV84
   80    ZGDIAG(IN)=(0.D0,0.D0)                                         DIAGFV85
C Build vector mask                                                     DIAGFV86
      DO 85 IN = 1, NEVTIN                                              DIAGFV87
   85    VMASK(IN) = QMASK(IN).OR.Z1FL(IN)                              DIAGFV88
      DO 82 K=1,3                                                       DIAGFV89
        IF(K.EQ.1) KI=K1                                                DIAGFV90
        IF(K.EQ.2) KI=K2                                                DIAGFV91
        IF(K.EQ.3) KI=K3                                                DIAGFV92
        DO 81 LI=1,-1,-2                                                DIAGFV93
           CALL GFUNV(K1,HELI(K1),K2,HELI(K2),VF(K1),AF(K1),            DIAGFV94
     .           K3,HELI(K3),KI,LI      ,VF(K3),AF(K3),GFUN1)           DIAGFV95
           CALL GFUNV(KI,LI      ,K4,HELI(K4),CHA(K4),0.D0,             DIAGFV96
     .           K5,HELI(K5),K6,HELI(K6),CHA(K5),0.D0,GFUN2)            DIAGFV97
           DO 81 IN = 1, NEVTIN                                         DIAGFV98
   81         IF (.NOT.VMASK(IN)) ZGDIAG(IN) = ZGDIAG(IN) +             DIAGFV99
     &           BFAC(KI)*GFUN1(IN)*GFUN2(IN)                           DIAGF100
   82 CONTINUE                                                          DIAGF101
C                                                                       DIAGF102
C THE NUMERATOR OF THE GG DIAGRAMS                                      DIAGF103
      DO 90 IN = 1, NEVTIN                                              DIAGF104
   90    GGDIAG(IN)=(0.D0,0.D0)                                         DIAGF105
C Build vector mask                                                     DIAGF106
      DO 95 IN = 1, NEVTIN                                              DIAGF107
   95    VMASK(IN) = QMASK(IN)                                          DIAGF108
      DO 92 K=1,3                                                       DIAGF109
        IF(K.EQ.1) KI=K1                                                DIAGF110
        IF(K.EQ.2) KI=K2                                                DIAGF111
        IF(K.EQ.3) KI=K3                                                DIAGF112
        DO 91 LI=1,-1,-2                                                DIAGF113
           CALL GFUNV(K1,HELI(K1),K2,HELI(K2),CHA(K1),0.D0,             DIAGF114
     .           K3,HELI(K3),KI,LI      ,CHA(K3),0.D0,GFUN1)            DIAGF115
           CALL GFUNV(KI,LI      ,K4,HELI(K4),CHA(K4),0.D0,             DIAGF116
     .           K5,HELI(K5),K6,HELI(K6),CHA(K5),0.D0,GFUN2)            DIAGF117
          DO 91 IN = 1, NEVTIN                                          DIAGF118
   91        IF (.NOT.VMASK(IN)) GGDIAG(IN) = GGDIAG(IN) +              DIAGF119
     &          BFAC(KI)*GFUN1(IN)*GFUN2(IN)                            DIAGF120
   92 CONTINUE                                                          DIAGF121
C                                                                       DIAGF122
      DO 100 IN = 1, NEVTIN                                             DIAGF123
  100    DIAGFN(IN)=(0.D0,-1.D0)/FPROP(IN)*(                            DIAGF124
     .   ZZDIAG(IN)/(Z0S12(IN)*Z0S56(IN)) + ZGDIAG(IN)/(Z0S12(IN)*      DIAGF125
     .   PHS56(IN)) + GZDIAG(IN)/(PHS12(IN)*Z0S56(IN)) +                DIAGF126
     .   GGDIAG(IN)/(PHS12(IN)*PHS56(IN)) )                             DIAGF127
C                                                                       DIAGF128
      END                                                               DIAGF129
      SUBROUTINE DOFSR                                                  DOFSR  2
C------------------------------------------------------------------     DOFSR  3
C FSR generation and related variables                                  DOFSR  4
C------------------------------------------------------------------     DOFSR  5
      IMPLICIT NONE                                                     DOFSR  6
      SAVE                                                              DOFSR  7
C Local variables                                                       DOFSR  8
      INTEGER IN,IFER,IP,IONC,ICO,IPAR,JFER                             DOFSR  9
      REAL*8 KFER(50), p1(4), p2(4), betax, betay, betaz                DOFSR 10
C                                                                       DOFSR 11
C FINALG commons                                                        DOFSR 12
      REAL*8 WMAXFS,WMXX,POUFSR,PIN                                     DOFSR 13
      INTEGER INQ,NCHAR,NNEUT,IDEBFS,NNG                                DOFSR 14
      COMMON/TALK/WMAXFS,WMXX,POUFSR(50,6),PIN(50,6),INQ(50)            DOFSR 15
     *           ,NCHAR,NNEUT,IDEBFS,NNG                                DOFSR 16
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C                                                                       DOFSR 22
      DATA IONC /0/                                                     DOFSR 23
C                                                                       DOFSR 24
C 1-time init for interface to FINALG                                   DOFSR 25
      IF (IONC.EQ.0) THEN                                               DOFSR 26
C NNG - no of FSR gammas; WMAXFS - a max weight; NCHAR - no. of ch'd parDOFSR 27
C INQ - charge of each particle                                         DOFSR 28
         NNG = NOFSR                                                    DOFSR 29
C WMXX will be filled with the max. observed weight.                    DOFSR 30
         WMXX = 0.D0                                                    DOFSR 31
C preset max. weight:                                                   DOFSR 32
         WMAXFS = 3.D0*(1+NNG)                                          DOFSR 33
C take care of beam particles                                           DOFSR 34
         NCHAR = 2                                                      DOFSR 35
         NNEUT = 0                                                      DOFSR 36
         INQ(1) = 0                                                     DOFSR 37
         INQ(2) = 0                                                     DOFSR 38
C Note that this treatment is not correct for quarks.                   DOFSR 39
         DO 5 ICO = 3, 6                                                DOFSR 40
            IF (DCHA(ICO).GT.0.01D0 .AND. kindqq(ico).EQ.0 )  THEN      DOFSR 41
               NCHAR = NCHAR + 1                                        DOFSR 42
               INQ(NCHAR) = (-1)**ICO                                   DOFSR 43
            ELSEIF ( kindqq(ico) .EQ. 0 ) THEN                          DOFSR 44
               NCHAR = NCHAR + 1                                        DOFSR 45
               INQ(NCHAR) = 0                                           DOFSR 46
            ENDIF                                                       DOFSR 47
 5       CONTINUE                                                       DOFSR 48
C Keep untouched the invariant masses of quark-resonances !             DOFSR 49
         DO 6 ICO = 3, 6                                                DOFSR 50
            IF ( kindqq(ico) .NE. 0 ) THEN                              DOFSR 51
               NNEUT = nneut + 1                                        DOFSR 52
               INQ(NCHAR+nneut) = 0                                     DOFSR 53
            ENDIF                                                       DOFSR 54
 6       CONTINUE                                                       DOFSR 55
         IF ( nneut .EQ. 2 ) nneut = 1                                  DOFSR 56
C IDEBFS - debug flag                                                   DOFSR 57
         IDEBFS = 0                                                     DOFSR 58
         IONC = 1                                                       DOFSR 59
      ENDIF                                                             DOFSR 60
C                                                                       DOFSR 61
C Transfer relevant momenta to FINALG's commons                         DOFSR 62
      IN = NEVTIN + 1                                                   DOFSR 63
      JFER = 0                                                          DOFSR 64
      DO 11 IFER = 1, 6                                                 DOFSR 65
        IF ( KINDQQ(IFER) .EQ. 0 ) THEN                                 DOFSR 66
          JFER = JFER + 1                                               DOFSR 67
          DO 12 IP = 1, 4                                               DOFSR 68
             PIN(JFER,IP) = PEVT(IP,IFER)                               DOFSR 69
 12       CONTINUE                                                      DOFSR 70
          PIN(JFER,5) = DMASS(IFER)                                     DOFSR 71
          KFER(IFER) = JFER                                             DOFSR 72
        ENDIF                                                           DOFSR 73
 11   CONTINUE                                                          DOFSR 74
C                                                                       DOFSR 75
      IF ( nneut .NE. 0 ) THEN                                          DOFSR 76
        DO 13 ifer = 3, 6                                               DOFSR 77
          IF ( kindqq(ifer) .NE. 0 ) THEN                               DOFSR 78
            jfer = jfer + 1                                             DOFSR 79
            kfer(ifer) = jfer                                           DOFSR 80
            DO 14 ip = 1, 4                                             DOFSR 81
              pin(jfer,ip) = pevt(ip,ifer)                              DOFSR 82
 14         CONTINUE                                                    DOFSR 83
          ENDIF                                                         DOFSR 84
 13     CONTINUE                                                        DOFSR 85
        DO 15 ip = 1, 4                                                 DOFSR 86
          p1(   ip) = pin(5,ip)                                         DOFSR 87
          p2(   ip) = pin(6,ip)                                         DOFSR 88
          pin(5,ip) = pin(5,ip)+pin(6,ip)                               DOFSR 89
          pin(6,ip) = 0D0                                               DOFSR 90
 15     CONTINUE                                                        DOFSR 91
        pin(5,5) = DSQRT(pin(5,4)**2                                    DOFSR 92
     .           -       pin(5,3)**2                                    DOFSR 93
     .           -       pin(5,2)**2                                    DOFSR 94
     .           -       pin(5,1)**2)                                   DOFSR 95
        betax = (p1(1)+p2(1)) / (p1(4)+p2(4))                           DOFSR 96
        betay = (p1(2)+p2(2)) / (p1(4)+p2(4))                           DOFSR 97
        betaz = (p1(3)+p2(3)) / (p1(4)+p2(4))                           DOFSR 98
        CALL boost(betax,betay,betaz,p1)                                DOFSR 99
        CALL boost(betax,betay,betaz,p2)                                DOFSR100
      ENDIF                                                             DOFSR101
C                                                                       DOFSR102
C Now the call to Le Dediberder's FSR routine                           DOFSR103
C                                                                       DOFSR104
      CALL FINALG                                                       DOFSR105
C                                                                       DOFSR106
C Put the momenta into the FSR area, skipping over the ISR photon slot  DOFSR107
C                                                                       DOFSR108
      IF ( nneut .NE. 0 ) THEN                                          DOFSR109
        DO 16 ipar = 5+NOFSR, 6, -1                                     DOFSR110
          DO 17 ip = 1,4                                                DOFSR111
            poufsr(ipar+1,ip) = poufsr(ipar,ip)                         DOFSR112
 17       CONTINUE                                                      DOFSR113
 16     CONTINUE                                                        DOFSR114
        betax = -poufsr(5,1)/poufsr(5,4)                                DOFSR115
        betay = -poufsr(5,2)/poufsr(5,4)                                DOFSR116
        betaz = -poufsr(5,3)/poufsr(5,4)                                DOFSR117
        CALL boost(betax,betay,betaz,p1)                                DOFSR118
        CALL boost(betax,betay,betaz,p2)                                DOFSR119
        DO 18 ip = 1, 4                                                 DOFSR120
          poufsr(5,ip) = p1(ip)                                         DOFSR121
          poufsr(6,ip) = p2(ip)                                         DOFSR122
 18       CONTINUE                                                      DOFSR123
      ENDIF                                                             DOFSR124
C                                                                       DOFSR125
      DO 21 IPAR = 3, 6+NOFSR                                           DOFSR126
         DO 22 IP = 1, 4                                                DOFSR127
            IF ( ipar .LE. 6 ) THEN                                     DOFSR128
              PEVTFS(IP,IPAR) = POUFSR(kfer(IPAR),IP)                   DOFSR129
            ELSE                                                        DOFSR130
              PEVTFS(IP,IPAR) = POUFSR(     IPAR,IP)                    DOFSR131
            ENDIF                                                       DOFSR132
 22      CONTINUE                                                       DOFSR133
 21   CONTINUE                                                          DOFSR134
C                                                                       DOFSR135
 1000 FORMAT(4(2x,f10.5))                                               DOFSR136
C                                                                       DOFSR137
      END                                                               DOFSR138
      SUBROUTINE DOISR                                                  DOISR  2
C------------------------------------------------------------------     DOISR  3
C ISR generation and related variables                                  DOISR  4
C------------------------------------------------------------------     DOISR  5
      IMPLICIT NONE                                                     DOISR  6
      SAVE                                                              DOISR  7
C Local variables                                                       DOISR  8
      INTEGER I,IN,ID,J,K                                               DOISR  9
      REAL*8 RN,BOOST(4)                                                DOISR 10
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C setup commons                                                         SETUPCO2
      REAL*8 ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                          SETUPCO3
      COMMON /SETUP/ ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                  SETUPCO4
C BRANCH commons                                                        BRANCHC2
      INTEGER LCHMX,PAR1,PAR2,PAR3,PAR4,PAR5,PAR6,NEVTGN,NRJGN,         BRANCHC3
     &   NCHAN,NCHANT,GENRJC,LGENR,JCHAN,CHACOU,CHANLS,                 BRANCHC4
     &   LANNIH,LBREMF,LBREMB,LCONV1,LCONV2,LCON1Z,LCON2Z,LMULTI,LRAMBO BRANCHC5
      PARAMETER (LCHMX = 100, LGENR = 9, LANNIH = 1, LBREMF = 2,        BRANCHC6
     &   LBREMB = 3, LCONV1 = 4, LCONV2 = 5,  LCON1Z = 6, LCON2Z = 7,   BRANCHC7
     &   LMULTI = 8, LRAMBO =9)                                         BRANCHC8
      REAL*8 SMINP,SMAXP,SMX3,SKMIN,SKMAX,BRNCH,FRAC,CGEN               BRANCHC9
      COMMON /BRANCH/ PAR1(LCHMX),PAR2(LCHMX),PAR3(LCHMX),PAR4(LCHMX),  BRANCH10
     &   PAR5(LCHMX),PAR6(LCHMX),NEVTGN(LGENR),NRJGN(LGENR),NCHAN,      BRANCH11
     &   NCHANT,GENRJC(LCHMX),JCHAN(LBUNCH),CHACOU(0:LGENR),            BRANCH12
     &   CHANLS(LGENR),                                                 BRANCH13
     &   SKMIN,SKMAX,SMINP(LCHMX),SMAXP(LCHMX),SMX3(LCHMX),             BRANCH14
     &   BRNCH(0:LCHMX),FRAC(LCHMX),CGEN(LGENR)                         BRANCH15
      CHARACTER*16 GNNAME(LGENR)                                        BRANCH16
      COMMON /GNNAMS/ GNNAME                                            BRANCH17
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C                                                                       DOISR 18
C Here is an example of how to read in an external event from unit 20 : DOISR 19
C Allow for the possibility to read in the kinematics from a file.      DOISR 20
C Read in event if switch is set                                        DOISR 21
      IF (EXTSOU) THEN                                                  DOISR 22
         OPEN(20,STATUS='UNKNOWN')                                      DOISR 23
         READ(20,*)                                                     DOISR 24
         READ(20,*) ((PEVT(J,K),J=1,4),K=1,NOPART)                      DOISR 25
C Do some extra for ISR events                                          DOISR 26
         IF (ISRFL) THEN                                                DOISR 27
            DO 5 ID = 1, 4                                              DOISR 28
 5             GAMISR(ID) = PEVT(ID,LGMISR)                             DOISR 29
C Reverse boost the other particles into their rest frame               DOISR 30
            CALL GETBOO(GAMISR,EBEAM,BOOST)                             DOISR 31
            BOOST(3) = -BOOST(3)                                        DOISR 32
C Boost the particles into rest frame.                                  DOISR 33
            DO 6 ID = 3, 6                                              DOISR 34
 6             CALL GETLAB(BOOST,PEVT(1,ID),PEVT(1,ID))                 DOISR 35
         ENDIF                                                          DOISR 36
      ENDIF                                                             DOISR 37
C Do ISR if requested                                                   DOISR 38
      IN = NEVTIN + 1                                                   DOISR 39
      IF (ISRFL)  THEN                                                  DOISR 40
         IF (.NOT.EXTSOU) THEN                                          DOISR 41
            CALL INITIG(GAMISR,EBEAM,KG1,KGMAX,KRATIO,ISRWT(IN))        DOISR 42
C Constrain the photon to be parallel to the z-axis                     DOISR 43
            GAMISR(1) = 0.D0                                            DOISR 44
            GAMISR(2) = 0.D0                                            DOISR 45
            IF (RN(1).GT.0.5D0) THEN                                    DOISR 46
               GAMISR(3) = GAMISR(4)                                    DOISR 47
            ELSE                                                        DOISR 48
               GAMISR(3) = -GAMISR(4)                                   DOISR 49
            ENDIF                                                       DOISR 50
         ELSE                                                           DOISR 51
            ISRWT(IN) = 1.D0                                            DOISR 52
         ENDIF                                                          DOISR 53
         SISR = SECM - 2.D0*GAMISR(4)*ECM                               DOISR 54
         ECMISR = SQRT(SISR)                                            DOISR 55
         IF (GAMISR(4).LT.EGMSOF)  NEVSOF = NEVSOF + 1                  DOISR 56
         IF (DEBGLV.GE.1 .AND. NEVTAC.LE.NEVPRI)                        DOISR 57
     &      PRINT'('' Weight returned by INITIG :'',G15.7)',ISRWT(IN)   DOISR 58
         CALL PSWTS(ECMISR)                                             DOISR 59
      ELSE                                                              DOISR 60
         SISR = SECM                                                    DOISR 61
         ECMISR = ECM                                                   DOISR 62
         ISRWT(IN) = 1.D0                                               DOISR 63
      ENDIF                                                             DOISR 64
C                                                                       DOISR 65
C THE BEAM MOMENTA                                                      DOISR 66
      EBEISR = ECMISR/2.D0                                              DOISR 67
      PEVT(1,1) = 0.D0                                                  DOISR 68
      PEVT(2,1) = 0.D0                                                  DOISR 69
      PEVT(3,1) = -EBEISR                                               DOISR 70
      PEVT(4,1) = EBEISR                                                DOISR 71
C The electron goes along +z by ALEPH convention, I think. J.H.         DOISR 72
      PEVT(1,2) = 0.D0                                                  DOISR 73
      PEVT(2,2) = 0.D0                                                  DOISR 74
      PEVT(3,2) = EBEISR                                                DOISR 75
      PEVT(4,2) = EBEISR                                                DOISR 76
C                                                                       DOISR 77
C Update the phase space generator parameters :                         DOISR 78
C                                                                       DOISR 79
      IF (FLA(3).EQ.FLA(5)) THEN                                        DOISR 80
C Maximum pair inv mass**2 to be used in most phase-space generators    DOISR 81
         Y2MX34 = ((ECMISR-2.D0*XMIN34)*FBOD2+XMIN34)**2                DOISR 82
C Make value non-physical if the user-supplied cut-off is higher than ouDOISR 83
C preferred value.                                                      DOISR 84
         IF (Y2MX34.LT.X2MN34)  Y2MX34 = -1.D0                          DOISR 85
C Maximum triplet inv mass**2 to be used in phase-space generators      DOISR 86
         YTRP34 = DMIN1((ECMISR-DMASS(5))**2,(XMAX34+DMASS(5))**2,      DOISR 87
     &      ((ECMISR-XMIN34-DMASS(5))*FBOD3+XMIN34+DMASS(5))**2)        DOISR 88
         Y2MX56 = Y2MX34                                                DOISR 89
         YTRP56 = YTRP34                                                DOISR 90
      ELSE                                                              DOISR 91
C Maximum pair inv mass**2 to be used in phase-space generators         DOISR 92
         Y2MX34 = ((ECMISR-XMIN34-XMIN56)*FBOD2+XMIN34)**2              DOISR 93
         Y2MX56 = ((ECMISR-XMIN56-XMIN34)*FBOD2+XMIN56)**2              DOISR 94
C Maximum triplet inv mass**2 to be used in phase-space generators      DOISR 95
         YTRP34 = DMIN1((ECMISR-DMASS(5))**2,(XMAX34+DMASS(5))**2,      DOISR 96
     &      ((ECMISR-XMIN34-DMASS(5))*FBOD3+XMIN34+DMASS(5))**2)        DOISR 97
         YTRP56 = DMIN1((ECMISR-DMASS(3))**2,(XMAX56+DMASS(3))**2,      DOISR 98
     &      ((ECMISR-XMIN56-DMASS(3))*FBOD3+XMIN56+DMASS(3))**2)        DOISR 99
      ENDIF                                                             DOISR100
C Maximum pair inv mass**2 to be used in multip. phase-space generator  DOISR101
      IF (FLA(1).EQ.FLA(3)) THEN                                        DOISR102
         Y2MXKM = ((ECMISR-2.D0*XMIN56)*FBOD2M+XMIN56)**2               DOISR103
      ELSEIF (FLA(1).EQ.FLA(5)) THEN                                    DOISR104
         Y2MXKM = ((ECMISR-2.D0*XMIN34)*FBOD2M+XMIN34)**2               DOISR105
      ENDIF                                                             DOISR106
C                                                                       DOISR107
C Next take care of some kinematics constants                           DOISR108
      IF (FLA(3).EQ.FLA(5)) THEN                                        DOISR109
         DO 31 I = CHACOU(LANNIH-1)+1, CHACOU(LANNIH)                   DOISR110
            SMINP(I) = X2MN34                                           DOISR111
            SMAXP(I) = Y2MX34                                           DOISR112
            SMX3(I)  = YTRP34                                           DOISR113
   31    CONTINUE                                                       DOISR114
      ELSE                                                              DOISR115
         DO 41 I = CHACOU(LANNIH-1)+1, CHACOU(LANNIH)                   DOISR116
            IF (PAR3(I).EQ.3) THEN                                      DOISR117
               SMINP(I) = X2MN34                                        DOISR118
               SMAXP(I) = Y2MX34                                        DOISR119
               SMX3(I)  = YTRP34                                        DOISR120
            ELSE                                                        DOISR121
               SMINP(I) = X2MN56                                        DOISR122
               SMAXP(I) = Y2MX56                                        DOISR123
               SMX3(I)  = YTRP56                                        DOISR124
            ENDIF                                                       DOISR125
   41    CONTINUE                                                       DOISR126
      ENDIF                                                             DOISR127
C SKMAX and SKMIN are used by the bremsstrahlung generator              DOISR128
      IF (FLA(3).EQ.FLA(1) .OR. FLA(5).EQ.FLA(1)) THEN                  DOISR129
         IF (FLA(5).EQ.FLA(1)) THEN                                     DOISR130
            SKMAX = Y2MX34                                              DOISR131
            SKMIN = X2MN34                                              DOISR132
         ELSE                                                           DOISR133
            SKMAX = Y2MX56                                              DOISR134
            SKMIN = X2MN56                                              DOISR135
         ENDIF                                                          DOISR136
         DO 51 I = CHACOU(LBREMB-1)+1, CHACOU(LBREMB)                   DOISR137
            IF (PAR3(I).EQ.3) THEN                                      DOISR138
               SMX3(I)  = YTRP34                                        DOISR139
            ELSE                                                        DOISR140
               SMX3(I)  = YTRP56                                        DOISR141
            ENDIF                                                       DOISR142
 51      CONTINUE                                                       DOISR143
      ENDIF                                                             DOISR144
C Update the cen. det. angle cut-off to reflect the boosted cms frame   DOISR145
      IF (ISRFL) THEN                                                   DOISR146
         CALL CSCENV                                                    DOISR147
      ELSE                                                              DOISR148
         CSCNEV = CSCEN                                                 DOISR149
      ENDIF                                                             DOISR150
C                                                                       DOISR151
      END                                                               DOISR152
      SUBROUTINE DOTBIL                                                 DOTBIL 2
C--------------------------------------------------------               DOTBIL 3
C Build dot-product array for use by DENS routines                      DOTBIL 4
C--------------------------------------------------------               DOTBIL 5
      IMPLICIT NONE                                                     DOTBIL 6
      SAVE                                                              DOTBIL 7
C local commons                                                         DOTBIL 8
      INTEGER IO,IN                                                     DOTBIL 9
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C                                                                       DOTBIL12
      DO 11 IO = 1, 6                                                   DOTBIL13
         DEVT(IO,IO) = 2.D0*MASS2(IO)                                   DOTBIL14
         DO 12 IN = 1, IO-1                                             DOTBIL15
            DEVT(IO,IN) = 2.D0*(PEVT(4,IO)*PEVT(4,IN) - (               DOTBIL16
     &          PEVT(1,IN)*PEVT(1,IO) + PEVT(2,IN)*PEVT(2,IO)           DOTBIL17
     &          + PEVT(3,IN)*PEVT(3,IO)))                               DOTBIL18
            DEVT(IN,IO) = DEVT(IO,IN)                                   DOTBIL19
 12      CONTINUE                                                       DOTBIL20
 11   CONTINUE                                                          DOTBIL21
C                                                                       DOTBIL22
      END                                                               DOTBIL23
      FUNCTION DOT3(P1,P2)                                              DOT3   2
      IMPLICIT NONE                                                     DOT3   3
      REAL*8 DOT3,P1(3),P2(3)                                           DOT3   4
      DOT3 = P1(1)*P2(1) + P1(2)*P2(2) + P1(3)*P2(3)                    DOT3   5
      END                                                               DOT3   6
      FUNCTION DOT4(P1,P2)                                              DOT4   2
C--------------------------------------------------------               DOT4   3
      IMPLICIT NONE                                                     DOT4   4
      REAL*8 DOT4,P1(4),P2(4)                                           DOT4   5
      DOT4 = P1(4)*P2(4) -(P1(1)*P2(1)+ P1(2)*P2(2) + P1(3)*P2(3))      DOT4   6
      END                                                               DOT4   7
      SUBROUTINE FINISH(NITER)                                          FINISH 2
C------------------------------------------------------------------     FINISH 3
C Print out cross-section for the job and other quantities of interest. FINISH 4
C------------------------------------------------------------------     FINISH 5
      IMPLICIT NONE                                                     FINISH 6
      SAVE                                                              FINISH 7
C Local variables                                                       FINISH 8
      REAL*8 WTSUM,CROS,ERR,EFF,RAT,VT1,VT2,VT41(4),VT42(4),VTMN,VTMX,  FINISH 9
     &   VTMN4(4),VTMX4(4),XINT,VT1S,VT2S,VT41S(4),VT42S(4),CROSF,ERRS  FINISH10
      INTEGER NITER,I,N,K,ID,IEV,IOD,IEVMN,IODMN,IEVMX,IODMX,IDLLM,IP   FINISH11
      CHARACTER*45 TITL                                                 FINISH12
      CHARACTER*4  CLIM,CANTI                                           FINISH13
      REAL*8 FAC,QTOT(4),QSUM(3),CSMX,PTOT3,DOT3,MASMN,MAS(3:6,3:6),    FINISH14
     &   DOT4,CROST,MEAVG1,MEAVG2,MESD1,MESD2                           FINISH15
      INTEGER HELPRP                                                    FINISH16
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C BRANCH commons                                                        BRANCHC2
      INTEGER LCHMX,PAR1,PAR2,PAR3,PAR4,PAR5,PAR6,NEVTGN,NRJGN,         BRANCHC3
     &   NCHAN,NCHANT,GENRJC,LGENR,JCHAN,CHACOU,CHANLS,                 BRANCHC4
     &   LANNIH,LBREMF,LBREMB,LCONV1,LCONV2,LCON1Z,LCON2Z,LMULTI,LRAMBO BRANCHC5
      PARAMETER (LCHMX = 100, LGENR = 9, LANNIH = 1, LBREMF = 2,        BRANCHC6
     &   LBREMB = 3, LCONV1 = 4, LCONV2 = 5,  LCON1Z = 6, LCON2Z = 7,   BRANCHC7
     &   LMULTI = 8, LRAMBO =9)                                         BRANCHC8
      REAL*8 SMINP,SMAXP,SMX3,SKMIN,SKMAX,BRNCH,FRAC,CGEN               BRANCHC9
      COMMON /BRANCH/ PAR1(LCHMX),PAR2(LCHMX),PAR3(LCHMX),PAR4(LCHMX),  BRANCH10
     &   PAR5(LCHMX),PAR6(LCHMX),NEVTGN(LGENR),NRJGN(LGENR),NCHAN,      BRANCH11
     &   NCHANT,GENRJC(LCHMX),JCHAN(LBUNCH),CHACOU(0:LGENR),            BRANCH12
     &   CHANLS(LGENR),                                                 BRANCH13
     &   SKMIN,SKMAX,SMINP(LCHMX),SMAXP(LCHMX),SMX3(LCHMX),             BRANCH14
     &   BRNCH(0:LCHMX),FRAC(LCHMX),CGEN(LGENR)                         BRANCH15
      CHARACTER*16 GNNAME(LGENR)                                        BRANCH16
      COMMON /GNNAMS/ GNNAME                                            BRANCH17
C Event writing block                                                   WRITING2
      INTEGER IWRITE,CODE,NWRITN,NWRIMX                                 WRITING3
      REAL*8 XMAXWT                                                     WRITING4
      COMMON /EWRITE/ XMAXWT,IWRITE,CODE(11),NWRITN,NWRIMX              WRITING5
      CHARACTER*16  TYPENAME(4)                                         FINISH24
C                                                                       FINISH25
      DATA TYPENAME /'MULTIPERIPHERAL', 'BREMSSTRAHLUNG', 'CONVERSION', FINISH26
     &   'ANNIHILIATION'/                                               FINISH27
C                                                                       FINISH28
C Put the proper constants in                                           FINISH29
      FAC = PSFACT*SYMME*COLFAC*3.89379D8/4.D0                          FINISH30
      HELPRP = 2**6*2**2                                                FINISH31
C                                                                       FINISH32
C Begin printout                                                        FINISH33
      IF (NITER.GT.0) THEN                                              FINISH34
         IF (DEBGLV.EQ.0)  RETURN                                       FINISH35
         PRINT'(/''  Results from iteration '',I2,'' :'')',NITER        FINISH36
      ELSE                                                              FINISH37
         PRINT'(/''  +++ FERMIS HAS FINISHED +++ ''/)'                  FINISH38
         IF (IWRITE.EQ.1 .AND. WTMX*FAC .GT. XMAXWT)                    FINISH39
     &    PRINT'(/''  Maximum weight exceeded !! Max weight = '',F9.3,  FINISH40
     &      ''  event weight = '',F9.3/                                 FINISH41
     &      ''  Please raise weight for next run.''/)',XMAXWT,WTMX*FAC  FINISH42
      ENDIF                                                             FINISH43
      PRINT'(''  No. of generated events   :        '',I10/             FINISH44
     &       ''  No. of rejected events    :        '',I10/             FINISH45
     &       ''  No. of accepted events    :        '',I10/             FINISH46
     &       ''  No. of events written out :        '',I10/             FINISH47
     &       ''  Sum of weights**1         :        '',G20.11/          FINISH48
     &       ''  Sum of weights**2         :        '',G20.11           FINISH49
     &       )',NEVENT,                                                 FINISH50
     &       NEVTRJ,NEVTAC,NWRITN,WT1*FAC,WT2*FAC*FAC                   FINISH51
      IF (ISRFL) THEN                                                   FINISH52
         PRINT'(''  Soft photon energy cutoff       :  '',F8.3/         FINISH53
     &          ''  No. of generated soft phot evts :  '',I10/          FINISH54
     &          ''  No. of accepted soft phot evts  :  '',I10)',        FINISH55
     &   EGMSOF,NEVSOF,NACSOF                                           FINISH56
      ENDIF                                                             FINISH57
C                                                                       FINISH58
      WTSUM = 0.D0                                                      FINISH59
      CROST = 0.D0                                                      FINISH60
C                                                                       FINISH61
C Calculate quantities for the relevant processes                       FINISH62
C                                                                       FINISH63
      IF (FLA(1).EQ.FLA(3) .OR. FLA(1).EQ.FLA(5)) THEN                  FINISH64
         IDLLM = 1                                                      FINISH65
      ELSE                                                              FINISH66
         IDLLM = 3                                                      FINISH67
      ENDIF                                                             FINISH68
      DO 1 I = IDLLM, 4                                                 FINISH69
    1    WTSUM = WTSUM + WT41(I)*FAC                                    FINISH70
      DO 11 I = IDLLM, 4                                                FINISH71
         IF (.NOT.DWANT(I))  GOTO 11                                    FINISH72
         VT41(I) = WT41(I)*FAC                                          FINISH73
         VT42(I) = WT42(I)*FAC*FAC                                      FINISH74
         VT41S(I) = WT41S(I)*FAC                                        FINISH75
         VT42S(I) = WT42S(I)*FAC*FAC                                    FINISH76
         VTMX4(I) = WTMX4(I)*FAC                                        FINISH77
         VTMN4(I) = WTMN4(I)*FAC                                        FINISH78
C                                                                       FINISH79
         CROS = VT41(I)/NEVENT                                          FINISH80
         CROST = CROST + CROS                                           FINISH81
         IF (NEVENT.GT.1) THEN                                          FINISH82
            ERR = DSQRT(VT42(I) - CROS**2*NEVENT)/NEVENT                FINISH83
         ELSE                                                           FINISH84
            ERR = CROS                                                  FINISH85
         ENDIF                                                          FINISH86
         IF (VTMX4(I).GT.1.D-20) THEN                                   FINISH87
            EFF = CROS/VTMX4(I)*100.D0                                  FINISH88
         ELSE                                                           FINISH89
            EFF = 0.D0                                                  FINISH90
         ENDIF                                                          FINISH91
         IF (WTSUM.GT.1.D-20) THEN                                      FINISH92
            RAT = VT41(I)/WTSUM*100.D0                                  FINISH93
         ELSE                                                           FINISH94
            RAT = 0.D0                                                  FINISH95
         ENDIF                                                          FINISH96
C                                                                       FINISH97
         PRINT'(/''  The '',A16,'' diagrams.'')',TYPENAME(I)            FINISH98
         PRINT'(''  Min. weight                 '',G20.11,'' pb''/      FINISH99
     &          ''  Max. weight                 '',G20.11,'' pb''/      FINIS100
     &          ''  Efficiency (=sigma/max wt)  '',F9.4,'' %''/         FINIS101
     &          ''  *** CROSS SECTION ***       '',G20.11,'' pb +/- ''  FINIS102
     &             ,G20.11,'' pb''/                                     FINIS103
     &          ''  Est. contrib. to total x-sec'',F7.2,'' %'')',       FINIS104
     &          VTMN4(I),VTMX4(I),EFF,CROS,ERR,RAT                      FINIS105
         IF (ISRFL.AND.NEVSOF.GT.0) THEN                                FINIS106
            CROSF =VT41S(I)/NEVSOF                                      FINIS107
            ERRS = DSQRT(VT42S(I)-CROSF**2*NEVSOF)/NEVSOF               FINIS108
            PRINT'(''  Cross section w/ soft phots '',G20.11,           FINIS109
     &        '' pb +/- '',G20.11,'' pb'')',CROSF,ERRS                  FINIS110
         ENDIF                                                          FINIS111
   11 CONTINUE                                                          FINIS112
C                                                                       FINIS113
C Combined results                                                      FINIS114
      VT1 = WT1*FAC                                                     FINIS115
      VT2 = WT2*FAC*FAC                                                 FINIS116
      VT1S = WT1S*FAC                                                   FINIS117
      VT2S = WT2S*FAC*FAC                                               FINIS118
      VTMX = WTMX*FAC                                                   FINIS119
      VTMN = WTMN*FAC                                                   FINIS120
      CROS = VT1/NEVENT                                                 FINIS121
      IF (NEVENT.GT.1) THEN                                             FINIS122
         ERR = DSQRT(VT2 - CROS**2*NEVENT)/NEVENT                       FINIS123
      ELSE                                                              FINIS124
         ERR = CROS                                                     FINIS125
      ENDIF                                                             FINIS126
      EFF = CROS/VTMX*100.D0                                            FINIS127
C                                                                       FINIS128
      IF (NITER.GT.0) THEN                                              FINIS129
         PRINT'(/''  +++ THE COMBINED RESULT +++ '')'                   FINIS130
      ELSE                                                              FINIS131
         PRINT'(/''  +++ THE FINAL COMBINED RESULT +++ '')'             FINIS132
      ENDIF                                                             FINIS133
      XINT = (CROS - CROST)/CROS*100.D0                                 FINIS134
      PRINT'(''  Min. weight                 '',G20.11,'' pb''/         FINIS135
     &       ''  Max. weight                 '',G20.11,'' pb''/         FINIS136
     &       ''  Efficiency (=sigma/max wt)  '',F9.4,'' %''/            FINIS137
     &       ''  *** CROSS SECTION ***       '',G20.11,'' pb +/- ''     FINIS138
     &           ,G20.11,'' pb''/                                       FINIS139
     &       ''  Interference                '',F9.4,'' %'')',          FINIS140
     &       VTMN,VTMX,EFF,CROS,ERR,XINT                                FINIS141
      IF (ISRFL) THEN                                                   FINIS142
         CROSF = VT1S/NEVSOF                                            FINIS143
         ERRS  = SQRT(VT2S - CROSF**2*NEVSOF)/NEVSOF                    FINIS144
         PRINT'(''  Cross section w/ soft phots '',G20.11,              FINIS145
     &        '' pb +/- '',G20.11,'' pb'')',CROSF,ERRS                  FINIS146
      ENDIF                                                             FINIS147
      PRINT'(/''  +++++++++++++++++++++++++ ''/)'                       FINIS148
C                                                                       FINIS149
      IF (NITER.GT.0)  RETURN                                           FINIS150
      IF (DEBGLV.GT.0 .AND. NEVTGN(LGENR).GT.0) THEN                    FINIS151
C Matrix element statistics                                             FINIS152
         ME1    = ME1*FAC                                               FINIS153
         ME2    = ME2*FAC*FAC                                           FINIS154
         MEMN   = MEMN*FAC                                              FINIS155
         MEMX   = MEMX*FAC                                              FINIS156
C                                                                       FINIS157
C We have 2 means to calculate the average and st. dev. :               FINIS158
C 1) Use flat phase-space events only (RAMBO)                           FINIS159
C 2) get avg. from sigma/wt(RAMBO), use this in variance formula        FINIS160
         MEAVG1 = ME1/NEVTGN(LRAMBO)                                    FINIS161
         MESD1  = SQRT(ME2/NEVTGN(LRAMBO) - MEAVG1*MEAVG1)              FINIS162
         MEAVG2 = CROS/(.5D0*(PSWMN+PSWMX))                             FINIS163
C         MESD2  = SQRT(ME2/NEVTGN(LRAMBO) - MEAVG2*MEAVG2)             FINIS164
         PRINT'('' Matrix element**2 statistics :'')'                   FINIS165
         PRINT'(''  Min. |M.E.|**2              '',G20.11/              FINIS166
     &          ''  Max. |M.E.|**2              '',G20.11/              FINIS167
     &          ''  * Method 1 : Use flat phase-space events :''/       FINIS168
     &          ''    avg  |M.E.|**2                '',G20.11/          FINIS169
     &          ''    resolution=st.dev./avg        '',G20.11,'' %''/   FINIS170
     &          ''  * Method 2 : Use correct answer for average :''/    FINIS171
     &          ''    avg  |M.E.|**2 = sig/wt(RAMBO)'',G20.11/          FINIS172
     &          ''    effic=avg/max                 '',G20.11,'' %'')'  FINIS173
     &    ,MEMN,MEMX,MEAVG1,100.*MESD1/MEAVG1,MEAVG2                    FINIS174
     &    ,100.*MEAVG2/MEMX                                             FINIS175
C                                                                       FINIS176
         PRINT'(/'' p.s. weight of Min. |M.E.|**2 evt '',G20.11/        FINIS177
     &          '' p.s. weight of Max. |M.E.|**2 evt '',G20.11)'        FINIS178
     &       ,PSWMN,PSWMX                                               FINIS179
      ENDIF                                                             FINIS180
C Print some phase space statistics                                     FINIS181
      PRINT'(/'' Phase-space generator statistics :'')'                 FINIS182
      DO 15 I = 1, LGENR                                                FINIS183
         IF (NEVTGN(I).GT.0)  PRINT'(2X,A16,'' generator.'',I10,        FINIS184
     &      '' events generated,'',I10,'' events accepted.'')',         FINIS185
     &      GNNAME(I),NEVTGN(I),NEVTGN(I)-NRJGN(I)                      FINIS186
   15 CONTINUE                                                          FINIS187
C                                                                       FINIS188
C                                                                       FINIS189
C Printout of extreme weight events                                     FINIS190
      IF (DEBGLV.EQ.0)  GOTO 20                                         FINIS191
C                                                                       FINIS192
      DO 21 I = 1, 2                                                    FINIS193
         IF (I.EQ.1) THEN                                               FINIS194
            CLIM = 'high'                                               FINIS195
         ELSE                                                           FINIS196
            CLIM = ' low'                                               FINIS197
         ENDIF                                                          FINIS198
         PRINT'(/''  Information on the '',A4,''est weight'',           FINIS199
     &    '' event : '')',CLIM                                          FINIS200
         WRITE(*,*) ' MOMENTA:'                                         FINIS201
         WRITE(*,91) 'LBL','X','Y','Z','ENERGY','MASS','TYPE'           FINIS202
   91    FORMAT(A4,6A12)                                                FINIS203
         DO 3 N=1, NOPART                                               FINIS204
           IF (N.EQ.1 .OR. N.EQ.4 .OR. N.EQ.6) THEN                     FINIS205
              CANTI = 'ANTI'                                            FINIS206
           ELSE                                                         FINIS207
              CANTI = '    '                                            FINIS208
           ENDIF                                                        FINIS209
           IF (N.LE.2) THEN                                             FINIS210
              WRITE(*,92) N,(QMOM(1,K,N),K=1,4),MASS(N),CANTI,FLA(N)    FINIS211
           ELSE                                                         FINIS212
              WRITE(*,92) N,(QSAV(K,N,I),K=1,4),MASS(N),CANTI,FLA(N)    FINIS213
           ENDIF                                                        FINIS214
   92      FORMAT(I4,5F12.5,6X,A4,A2)                                   FINIS215
    3    CONTINUE                                                       FINIS216
         DO 4 K=1,4                                                     FINIS217
           QTOT(K)=QMOM(1,K,1)+QMOM(1,K,2)                              FINIS218
           DO 4 IP = 3, NOPART                                          FINIS219
              QTOT(K) = QTOT(K) - QSAV(K,IP,I)                          FINIS220
    4    CONTINUE                                                       FINIS221
         WRITE(*,93) ' TOTS:',(QTOT(K),K=1,4)                           FINIS222
   93    FORMAT(A6,1X,4F12.5)                                           FINIS223
         MASMN = 1.D10                                                  FINIS224
         DO 25 IOD = 3, 5, 2                                            FINIS225
            DO 26 IEV = 4, 6, 2                                         FINIS226
               MAS(IOD,IEV) = DSQRT(MASS2(IOD)+MASS2(IEV)+              FINIS227
     &           2.D0*DOT4(QSAV(1,IOD,I),QSAV(1,IEV,I)))                FINIS228
               IF (MAS(IOD,IEV).LT.MASMN) THEN                          FINIS229
                  IF ((FLA(3).NE.FLA(5) .AND. IOD.EQ.IEV-1) .OR.        FINIS230
     &                FLA(3).EQ.FLA(5)) THEN                            FINIS231
                     MASMN = MAS(IOD,IEV)                               FINIS232
                     IODMN = IOD                                        FINIS233
                     IEVMN = IEV                                        FINIS234
                  ENDIF                                                 FINIS235
               ENDIF                                                    FINIS236
 26         CONTINUE                                                    FINIS237
 25      CONTINUE                                                       FINIS238
         PRINT'('' Min. mass pair is ('',I1,'','',I1,''), with mass '', FINIS239
     &          F9.5,'' GeV'')',MIN(IODMN,IEVMN),MAX(IODMN,IEVMN),      FINIS240
     &          MAS(IODMN,IEVMN)                                        FINIS241
         IODMX = 8 - IODMN                                              FINIS242
         IEVMX = 10 - IEVMN                                             FINIS243
         PRINT'('' Mass of other pair ('',I1,'','',I1,''), is '',       FINIS244
     &          F9.5,'' GeV'')',MIN(IODMX,IEVMX),MAX(IODMX,IEVMX),      FINIS245
     &          MAS(IODMX,IEVMX)                                        FINIS246
C                                                                       FINIS247
C Now get the angle between low-inv.-mass pair and nearest track.       FINIS248
         DO 27 ID = 1, 3                                                FINIS249
 27         QSUM(ID) = QSAV(ID,IODMN,I) + QSAV(ID,IEVMN,I)              FINIS250
         CSMX = DMAX1(DOT3(QSUM,QSAV(1,IODMX,I))/PTOT3(QSUM)/           FINIS251
     &      PTOT3(QSAV(1,IODMX,I)),DOT3(QSUM,QSAV(1,IEVMX,I))/          FINIS252
     &      PTOT3(QSUM)/PTOT3(QSAV(1,IEVMX,I)) )                        FINIS253
         PRINT'('' Cos between min. mass pair and nearest tk : '',      FINIS254
     &         F9.6,'' radians ''/)',CSMX                               FINIS255
C                                                                       FINIS256
C Breakdown of weight for this event by process                         FINIS257
         DO 28 ID = IDLLM, 4                                            FINIS258
 28         PRINT'(2X,A16,'', weight= '',G20.12)',TYPENAME(ID),         FINIS259
     &         WTSAV(ID,I)*FAC                                          FINIS260
         PRINT'(/'' Event p.s. weight : '',G20.12)',PSWSAV(I)           FINIS261
         PRINT'('' Ph. space densities :''/6(7(1X,G10.4)/))',           FINIS262
     &      (DENSSV(ID,I),ID=1,NCHANT)                                  FINIS263
 21   CONTINUE                                                          FINIS264
C Diagram evaluation count                                              FINIS265
      PRINT'(/'' Diagram evaluation ratio = '',F6.2,''%'')',            FINIS266
     &         100.0*DIATOT/(DIACOU*HELPRP*NEVTAC)                      FINIS267
   20 CONTINUE                                                          FINIS268
C                                                                       FINIS269
C Printout of histograms the last time through                          FINIS270
C                                                                       FINIS271
      PRINT'(/'' *** KLEISS HISTOGRAM PACKAGE PRINTOUT *** ''/)'        FINIS272
C                                                                       FINIS273
      CALL HISTO2(1,0,'Event weights')                                  FINIS274
      TITL = '('//FLA(3)//',ANTI-'//FLA(3)//') mass**2'                 FINIS275
      CALL HISTO2(2,0,TITL)                                             FINIS276
      IF (FLA(3).NE.FLA(5)) THEN                                        FINIS277
         TITL = '('//FLA(5)//',ANTI-'//FLA(5)//') mass**2'              FINIS278
         CALL HISTO2(3,0,TITL)                                          FINIS279
      ENDIF                                                             FINIS280
      TITL = FLA(3)//',ANTI-'//FLA(3)//' energy'                        FINIS281
      CALL HISTO2(4,0,TITL)                                             FINIS282
      IF (FLA(3).NE.FLA(5)) THEN                                        FINIS283
         TITL = FLA(5)//',ANTI-'//FLA(5)//' energy'                     FINIS284
         CALL HISTO2(5,0,TITL)                                          FINIS285
      ENDIF                                                             FINIS286
      TITL = FLA(3)//',ANTI-'//FLA(3)//                                 FINIS287
     &   ' min(cos) ang w.r.t. other 2 tks'                             FINIS288
      CALL HISTO2(6,0,TITL)                                             FINIS289
      IF (FLA(3).NE.FLA(5)) THEN                                        FINIS290
         TITL = FLA(5)//',ANTI-'//FLA(5)//                              FINIS291
     &   ' min(cos) ang w.r.t. other 2 tks'                             FINIS292
         CALL HISTO2(7,0,TITL)                                          FINIS293
      ENDIF                                                             FINIS294
      IF (ISRFL)  CALL HISTO2(8,0,'ISR photon energy')                  FINIS295
      IF (FSRFL) THEN                                                   FINIS296
          CALL HISTO2(9,0,'typical FSR photon energy')                  FINIS297
          CALL HISTO2(10,0,'max FSR photon energy in evt')              FINIS298
      ENDIF                                                             FINIS299
      RETURN                                                            FINIS300
      END                                                               FINIS301
      SUBROUTINE GFUNV(P1,L1,P2,L2,V1,A1,P3,L3,P4,L4,V3,A3,GFUN)        GFUNV  2
C-----------------------------------------------------------------------GFUNV  3
      IMPLICIT NONE                                                     GFUNV  4
      SAVE                                                              GFUNV  5
C COMPUTE THE FUNCTION                                                  GFUNV  6
C UBAR(P1,L1)*(V1+A1*GAMMA5)*GAMMA(MU)*U(P2,L2) X                       GFUNV  7
C UBAR(P3,L3)*(V3+A3*GAMMA5)*GAMMA(MU)*U(P4,L4)                         GFUNV  8
C THIS IS DONE BY EXPRESSING IT IN TERMS OF THE FUNCTIONS H1,H2,H3,H4   GFUNV  9
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
      COMPLEX*16 GFUN(LBUNCH)                                           GFUNV 12
      INTEGER P1,P2,P3,P4,L1,L2,L3,L4,I,IN                              GFUNV 13
      REAL*8 V1,A1,V3,A3                                                GFUNV 14
C 4-vector products                                                     VEC4PRO2
      COMPLEX*16 S(LBUNCH,6,6),T(LBUNCH,6,6)                            VEC4PRO3
      REAL*8 ETA(LBUNCH,6),RMU(LBUNCH,6)                                VEC4PRO4
      COMMON / SPPROD / S,T,ETA,RMU                                     VEC4PRO5
C                                                                       GFUNV 16
C GO THROUGH THE LOGIC TO FIGURE OUT WHICH OF THE H(I) TO CALL          GFUNV 17
      IF(L1.EQ.L2) THEN                                                 GFUNV 18
        IF(L3.EQ.L4) THEN                                               GFUNV 19
          IF(L1.EQ.1.AND.L3.EQ.1) THEN                                  GFUNV 20
C           WRITE(*,*) ' ++++',L1,L2,L3,L4                              GFUNV 21
             CALL H1(P1,P2,P3,P4,V1,A1,V3,A3,GFUN)                      GFUNV 22
C           WRITE(*,*) GFUN                                             GFUNV 23
          ELSEIF(L1.EQ.1.AND.L3.EQ.-1) THEN                             GFUNV 24
C           WRITE(*,*) ' ++--',L1,L2,L3,L4                              GFUNV 25
             CALL H1(P1,P2,P4,P3,V1,A1,V3,-A3,GFUN)                     GFUNV 26
C           WRITE(*,*) GFUN                                             GFUNV 27
          ELSEIF(L1.EQ.-1.AND.L3.EQ.1) THEN                             GFUNV 28
C           WRITE(*,*) ' --++',L1,L2,L3,L4                              GFUNV 29
             CALL H1(P2,P1,P3,P4,V1,-A1,V3,A3,GFUN)                     GFUNV 30
C           WRITE(*,*) GFUN                                             GFUNV 31
          ELSEIF(L1.EQ.-1.AND.L3.EQ.-1) THEN                            GFUNV 32
C           WRITE(*,*) ' ----',L1,L2,L3,L4                              GFUNV 33
             CALL H1(P2,P1,P4,P3,V1,-A1,V3,-A3,GFUN)                    GFUNV 34
C           WRITE(*,*) GFUN                                             GFUNV 35
          ELSE                                                          GFUNV 36
            WRITE(*,*) ' CASE 1 ERROR:',L1,L2,L3,L4                     GFUNV 37
            STOP 'CASE 1'                                               GFUNV 38
          ENDIF                                                         GFUNV 39
        ELSEIF(L3.EQ.(-L4)) THEN                                        GFUNV 40
          IF(L1.EQ.1.AND.L3.EQ.1) THEN                                  GFUNV 41
C           WRITE(*,*) ' +++-',L1,L2,L3,L4                              GFUNV 42
             CALL H2(P1,P2,P3,P4,V1,A1,V3,A3,GFUN)                      GFUNV 43
C           WRITE(*,*) GFUN                                             GFUNV 44
          ELSEIF(L1.EQ.1.AND.L3.EQ.-1) THEN                             GFUNV 45
C           WRITE(*,*) ' ++-+',L1,L2,L3,L4                              GFUNV 46
             CALL H3(P1,P2,P3,P4,V1,A1,V3,A3,GFUN)                      GFUNV 47
C           WRITE(*,*) GFUN                                             GFUNV 48
          ELSEIF(L1.EQ.-1.AND.L3.EQ.1) THEN                             GFUNV 49
C           WRITE(*,*) ' --+-',L1,L2,L3,L4                              GFUNV 50
             CALL H2(P2,P1,P3,P4,V1,-A1,V3,A3,GFUN)                     GFUNV 51
C           WRITE(*,*) GFUN                                             GFUNV 52
          ELSEIF(L1.EQ.-1.AND.L3.EQ.-1) THEN                            GFUNV 53
C           WRITE(*,*) ' ---+',L1,L2,L3,L4                              GFUNV 54
             CALL H3(P2,P1,P3,P4,V1,-A1,V3,A3,GFUN)                     GFUNV 55
C           WRITE(*,*) GFUN                                             GFUNV 56
          ELSE                                                          GFUNV 57
            WRITE(*,*) ' CASE 2 ERROR:',L1,L2,L3,L4                     GFUNV 58
            STOP 'CASE 2'                                               GFUNV 59
          ENDIF                                                         GFUNV 60
        ELSE                                                            GFUNV 61
          WRITE(*,*) ' CASE 3 ERROR:',L1,L2,L3,L4                       GFUNV 62
          STOP 'CASE 3'                                                 GFUNV 63
        ENDIF                                                           GFUNV 64
      ELSEIF(L1.EQ.(-L2)) THEN                                          GFUNV 65
        IF(L3.EQ.L4) THEN                                               GFUNV 66
          IF(L1.EQ.1.AND.L3.EQ.1) THEN                                  GFUNV 67
C           WRITE(*,*) ' +-++',L1,L2,L3,L4                              GFUNV 68
             CALL H2(P3,P4,P1,P2,V3,A3,V1,A1,GFUN)                      GFUNV 69
C           WRITE(*,*) GFUN                                             GFUNV 70
          ELSEIF(L1.EQ.1.AND.L3.EQ.-1) THEN                             GFUNV 71
C           WRITE(*,*) ' +---',L1,L2,L3,L4                              GFUNV 72
C            CALL H2(P4,P3,P1,P2,V3,-A3,V1,-A1)                         GFUNV 73
C J.H. wrong sign in front of A1                                        GFUNV 74
             CALL H2(P4,P3,P1,P2,V3,-A3,V1,A1,GFUN)                     GFUNV 75
C           WRITE(*,*) GFUN                                             GFUNV 76
          ELSEIF(L1.EQ.-1.AND.L3.EQ.1) THEN                             GFUNV 77
C           WRITE(*,*) ' -+++',L1,L2,L3,L4                              GFUNV 78
             CALL H3(P3,P4,P1,P2,V3,A3,V1,A1,GFUN)                      GFUNV 79
C           WRITE(*,*) GFUN                                             GFUNV 80
          ELSEIF(L1.EQ.-1.AND.L3.EQ.-1) THEN                            GFUNV 81
C           WRITE(*,*) ' -+--',L1,L2,L3,L4                              GFUNV 82
             CALL H3(P4,P3,P1,P2,V3,-A3,V1,A1,GFUN)                     GFUNV 83
C           WRITE(*,*) GFUN                                             GFUNV 84
          ELSE                                                          GFUNV 85
            WRITE(*,*) ' CASE 4 ERROR:',L1,L2,L3,L4                     GFUNV 86
            STOP 'CASE 4'                                               GFUNV 87
          ENDIF                                                         GFUNV 88
        ELSEIF(L3.EQ.(-L4)) THEN                                        GFUNV 89
          IF(L1.EQ.1.AND.L3.EQ.1) THEN                                  GFUNV 90
C           WRITE(*,*) ' +-+-',L1,L2,L3,L4                              GFUNV 91
             DO 13 IN = 1, NEVTIN                                       GFUNV 92
   13           GFUN(IN)=(0.D0,0.D0)                                    GFUNV 93
C           WRITE(*,*) GFUN                                             GFUNV 94
          ELSEIF(L1.EQ.1.AND.L3.EQ.-1) THEN                             GFUNV 95
C           WRITE(*,*) ' +--+',L1,L2,L3,L4                              GFUNV 96
             CALL H4(P1,P2,P3,P4,V1,A1,V3,A3,GFUN)                      GFUNV 97
C           WRITE(*,*) GFUN                                             GFUNV 98
          ELSEIF(L1.EQ.-1.AND.L3.EQ.1) THEN                             GFUNV 99
C           WRITE(*,*) ' -++-',L1,L2,L3,L4                              GFUNV100
             CALL H4(P3,P4,P1,P2,V3,A3,V1,A1,GFUN)                      GFUNV101
C           WRITE(*,*) GFUN                                             GFUNV102
          ELSEIF(L1.EQ.-1.AND.L3.EQ.-1) THEN                            GFUNV103
C           WRITE(*,*) ' -+-+',L1,L2,L3,L4                              GFUNV104
             DO 16 IN = 1, NEVTIN                                       GFUNV105
   16           GFUN(IN)=(0.D0,0.D0)                                    GFUNV106
C           WRITE(*,*) GFUN                                             GFUNV107
          ELSE                                                          GFUNV108
            WRITE(*,*) ' CASE 5 ERROR:',L1,L2,L3,L4                     GFUNV109
            STOP 'CASE 5'                                               GFUNV110
          ENDIF                                                         GFUNV111
        ELSE                                                            GFUNV112
          WRITE(*,*) ' CASE 6 ERROR:',L1,L2,L3,L4                       GFUNV113
          STOP 'CASE 6'                                                 GFUNV114
        ENDIF                                                           GFUNV115
      ELSE                                                              GFUNV116
        WRITE(*,*) ' CASE 7 ERROR:',L1,L2,L3,L4                         GFUNV117
        STOP 'CASE 7'                                                   GFUNV118
      ENDIF                                                             GFUNV119
C                                                                       GFUNV120
      END                                                               GFUNV121
      SUBROUTINE H1(K1,K2,K3,K4,W1,B1,W3,B3,GFUN)                       H1     2
C-----------------------------------------------------------------------H1     3
C                                                                       H1     4
      IMPLICIT NONE                                                     H1     5
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C 4-vector products                                                     VEC4PRO2
      COMPLEX*16 S(LBUNCH,6,6),T(LBUNCH,6,6)                            VEC4PRO3
      REAL*8 ETA(LBUNCH,6),RMU(LBUNCH,6)                                VEC4PRO4
      COMMON / SPPROD / S,T,ETA,RMU                                     VEC4PRO5
C Local variables                                                       H1     9
      INTEGER K1,K2,K3,K4,IN                                            H1    10
      REAL*8 W1,W3,B1,B3                                                H1    11
      COMPLEX*16 GFUN(LBUNCH)                                           H1    12
C GFUNV commons                                                         GFUNVC 2
      LOGICAL VMASK                                                     GFUNVC 3
      COMMON /GFNCOM/ VMASK(LBUNCH)                                     GFUNVC 4
C                                                                       H1    14
      DO 10 IN = 1, NEVTIN                                              H1    15
   10    IF (.NOT.VMASK(IN)) GFUN(IN) =                                 H1    16
     .   2.D0*( (W1-B1)*(W3-B3)*S(IN,K1,K3)*T(IN,K4,K2)                 H1    17
     .   +(W1+B1)*(W3-B3)*RMU(IN,K1)*RMU(IN,K2)*ETA(IN,K3)*ETA(IN,K4)   H1    18
     .   +(W1-B1)*(W3+B3)*RMU(IN,K3)*RMU(IN,K4)*ETA(IN,K1)*ETA(IN,K2))  H1    19
C                                                                       H1    20
      END                                                               H1    21
      SUBROUTINE H2(K1,K2,K3,K4,W1,B1,W3,B3,GFUN)                       H2     2
C-----------------------------------------------------------------------H2     3
      IMPLICIT NONE                                                     H2     4
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C 4-vector products                                                     VEC4PRO2
      COMPLEX*16 S(LBUNCH,6,6),T(LBUNCH,6,6)                            VEC4PRO3
      REAL*8 ETA(LBUNCH,6),RMU(LBUNCH,6)                                VEC4PRO4
      COMMON / SPPROD / S,T,ETA,RMU                                     VEC4PRO5
C Local variables                                                       H2     8
      COMPLEX*16 GFUN(LBUNCH)                                           H2     9
      INTEGER K1,K2,K3,K4,IN                                            H2    10
      REAL*8 W1,W3,B1,B3                                                H2    11
C GFUNV commons                                                         GFUNVC 2
      LOGICAL VMASK                                                     GFUNVC 3
      COMMON /GFNCOM/ VMASK(LBUNCH)                                     GFUNVC 4
C                                                                       H2    13
      DO 10 IN = 1, NEVTIN                                              H2    14
   10    IF (.NOT.VMASK(IN)) GFUN(IN) =                                 H2    15
     .   2.D0*(W1-B1)*ETA(IN,K2)*( (W3+B3)*RMU(IN,K3)*S(IN,K1,K4)       H2    16
     .                        - (W3-B3)*RMU(IN,K4)*S(IN,K1,K3) )        H2    17
      END                                                               H2    18
      SUBROUTINE H3(K1,K2,K3,K4,W1,B1,W3,B3,GFUN)                       H3     2
C-----------------------------------------------------------------------H3     3
      IMPLICIT NONE                                                     H3     4
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C 4-vector products                                                     VEC4PRO2
      COMPLEX*16 S(LBUNCH,6,6),T(LBUNCH,6,6)                            VEC4PRO3
      REAL*8 ETA(LBUNCH,6),RMU(LBUNCH,6)                                VEC4PRO4
      COMMON / SPPROD / S,T,ETA,RMU                                     VEC4PRO5
C Local variables                                                       H3     8
      COMPLEX*16 GFUN(LBUNCH)                                           H3     9
      INTEGER K1,K2,K3,K4,IN                                            H3    10
      REAL*8 W1,W3,B1,B3                                                H3    11
C GFUNV commons                                                         GFUNVC 2
      LOGICAL VMASK                                                     GFUNVC 3
      COMMON /GFNCOM/ VMASK(LBUNCH)                                     GFUNVC 4
C                                                                       H3    13
      DO 10 IN = 1, NEVTIN                                              H3    14
   10    IF (.NOT.VMASK(IN)) GFUN(IN) =                                 H3    15
     .    2.D0*(W1-B1)*ETA(IN,K1)*( (W3-B3)*RMU(IN,K3)*T(IN,K2,K4)      H3    16
     .                        - (W3+B3)*RMU(IN,K4)*T(IN,K2,K3) )        H3    17
C                                                                       H3    18
      END                                                               H3    19
      SUBROUTINE H4(K1,K2,K3,K4,W1,B1,W3,B3,GFUN)                       H4     2
C-----------------------------------------------------------------------H4     3
      IMPLICIT NONE                                                     H4     4
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C 4-vector products                                                     VEC4PRO2
      COMPLEX*16 S(LBUNCH,6,6),T(LBUNCH,6,6)                            VEC4PRO3
      REAL*8 ETA(LBUNCH,6),RMU(LBUNCH,6)                                VEC4PRO4
      COMMON / SPPROD / S,T,ETA,RMU                                     VEC4PRO5
C Local variables                                                       H4     8
      COMPLEX*16 GFUN(LBUNCH)                                           H4     9
      INTEGER K1,K2,K3,K4,IN                                            H4    10
      REAL*8 W1,W3,B1,B3                                                H4    11
C GFUNV commons                                                         GFUNVC 2
      LOGICAL VMASK                                                     GFUNVC 3
      COMMON /GFNCOM/ VMASK(LBUNCH)                                     GFUNVC 4
C                                                                       H4    13
      DO 10 IN = 1, NEVTIN                                              H4    14
   10    IF (.NOT.VMASK(IN)) GFUN(IN) =                                 H4    15
     .2.D0*((W1+B1)*(W3-B3)*RMU(IN,K1)*RMU(IN,K3)*ETA(IN,K2)*ETA(IN,K4) H4    16
     .   - (W1+B1)*(W3+B3)*RMU(IN,K1)*RMU(IN,K4)*ETA(IN,K2)*ETA(IN,K3)  H4    17
     .   - (W1-B1)*(W3-B3)*RMU(IN,K2)*RMU(IN,K3)*ETA(IN,K1)*ETA(IN,K4)  H4    18
     .   + (W1-B1)*(W3+B3)*RMU(IN,K2)*RMU(IN,K4)*ETA(IN,K1)*ETA(IN,K3)) H4    19
C                                                                       H4    20
      END                                                               H4    21
      FUNCTION HILGTN(C,ALPHA,XMIN,XMAX)                                HILGTN 2
C--------------------------------------------------------               HILGTN 3
C Return distrib. normalization for a variable distributed according to HILGTN 4
C 1/(C + x)**alpha                                                      HILGTN 5
C INPUTS:                                                               HILGTN 6
C C - the constant offset                                               HILGTN 7
C XMIN - the min. x value                                               HILGTN 8
C XMAX - the max. x value                                               HILGTN 9
C ALPHA - the power                                                     HILGTN10
C--------------------------------------------------------               HILGTN11
      IMPLICIT NONE                                                     HILGTN12
      REAL*8 HILGTN,C,ALPHA,XMIN,XMAX,ALP1                              HILGTN13
C                                                                       HILGTN14
C There are 2 kinds of distributions to consider                        HILGTN15
      IF (ALPHA.EQ.1.D0) THEN                                           HILGTN16
         HILGTN = DLOG((C+XMAX)/(C+XMIN))                               HILGTN17
      ELSE                                                              HILGTN18
         ALP1 = 1.D0 - ALPHA                                            HILGTN19
         HILGTN = 1.D0/ALP1*((C+XMAX)**ALP1 - (C+XMIN)**ALP1)           HILGTN20
      ENDIF                                                             HILGTN21
C                                                                       HILGTN22
      END                                                               HILGTN23
      FUNCTION HILGTX(C,ALPHA,XMIN,XMAX)                                HILGTX 2
C--------------------------------------------------------               HILGTX 3
C Return x value for a variable distributed according to 1/(C + x)**alphHILGTX 4
C                                                                       HILGTX 5
C INPUTS:                                                               HILGTX 6
C C - the constant offset                                               HILGTX 7
C XMIN - the min. x value                                               HILGTX 8
C XMAX - the max. x value                                               HILGTX 9
C ALPHA - the power                                                     HILGTX10
C--------------------------------------------------------               HILGTX11
      IMPLICIT NONE                                                     HILGTX12
      REAL*8 HILGTX,C,ALPHA,XMIN,XMAX,RN,ALP1                           HILGTX13
      INTEGER K                                                         HILGTX14
C                                                                       HILGTX15
C There are 2 kinds of distributions to consider                        HILGTX16
      IF (ALPHA.EQ.1.D0) THEN                                           HILGTX17
         HILGTX = -C + (C+XMIN)*EXP(RN(K)*DLOG((C+XMAX)/(C+XMIN)))      HILGTX18
      ELSE                                                              HILGTX19
         ALP1 = 1.D0 - ALPHA                                            HILGTX20
         HILGTX = -C + ( (C+XMIN)**ALP1 + RN(K)*((C+XMAX)**ALP1 -       HILGTX21
     &     (C+XMIN)**ALP1) )**(1.D0/ALP1)                               HILGTX22
      ENDIF                                                             HILGTX23
C                                                                       HILGTX24
      END                                                               HILGTX25
      SUBROUTINE HISTO1(IH,IB,X0,X1,X,W)                                HISTO1 2
C--------------------------------------------------------               HISTO1 3
C The infamous Kleiss histogramming package. Includes routines          HISTO1 4
C HISTO1 - add entry to histogram                                       HISTO1 5
C HISTO2 - print out histogram                                          HISTO1 6
C HISTO3 - initialize histogram                                         HISTO1 7
C INPUT arguments to HISTO1 :                                           HISTO1 8
C IH - histogram ID no.                                                 HISTO1 9
C IB - no. of bins                                                      HISTO110
C X0 - low value                                                        HISTO111
C X1 - high value                                                       HISTO112
C X  - entry's value                                                    HISTO113
C W  - entry's weight                                                   HISTO114
C--------------------------------------------------------               HISTO115
C STORE IN HISTOGRAM                                                    HISTO116
      IMPLICIT REAL*8(A-H,O-Z)                                          HISTO117
      SAVE                                                              HISTO118
C Histogram commons                                                     HISCOCO2
      PARAMETER(JH=20,JB=50,JR=30)                                      HISCOCO3
      COMMON / HISCO1 /H(JH,JB),NB(JH),XL(JH),XU(JH),XM(JH),            HISCOCO4
     .                 II(JH),IU(JH),IO(JH)                             HISCOCO5
      NB(IH)=IB                                                         HISTO120
      XL(IH)=X0                                                         HISTO121
      XU(IH)=X1                                                         HISTO122
      IF(X.LT.X0) THEN                                                  HISTO123
        IU(IH)=IU(IH)+1                                                 HISTO124
      ELSEIF(X.GT.X1) THEN                                              HISTO125
        IO(IH)=IO(IH)+1                                                 HISTO126
      ELSE                                                              HISTO127
        II(IH)=II(IH)+1                                                 HISTO128
        K=INT((X-X0)/(X1-X0)*IB)+1                                      HISTO129
        H(IH,K)=H(IH,K)+W                                               HISTO130
        IF(H(IH,K).GT.XM(IH)) XM(IH)=H(IH,K)                            HISTO131
      ENDIF                                                             HISTO132
      END                                                               HISTO133
      SUBROUTINE HISTO2(IH,M,TITLE)                                     HISTO2 2
C--------------------------------------------------------               HISTO2 3
C PRINT HISTOGRAM                                                       HISTO2 4
C INPUT ARGUMENTS :                                                     HISTO2 5
C IH - ID of histogram no. to print                                     HISTO2 6
C M  - 0 for linear scale, 1 for log scale                              HISTO2 7
C TITLE - character string                                              HISTO2 8
C--------------------------------------------------------               HISTO2 9
      IMPLICIT REAL*8(A-H,O-Z)                                          HISTO210
      SAVE                                                              HISTO211
C Histogram commons                                                     HISCOCO2
      PARAMETER(JH=20,JB=50,JR=30)                                      HISCOCO3
      COMMON / HISCO1 /H(JH,JB),NB(JH),XL(JH),XU(JH),XM(JH),            HISCOCO4
     .                 II(JH),IU(JH),IO(JH)                             HISCOCO5
      CHARACTER*(*) TITLE                                               HISTO213
      CHARACTER*3 SCM(0:1)                                              HISTO214
      CHARACTER*1 REGEL(JR),STAR,BLANK                                  HISTO215
      CHARACTER*10 PIECE                                                HISTO216
      DATA SCM/'LIN','LOG'/,REGEL/30*' '/,STAR/'*'/,BLANK/' '/          HISTO217
      DATA PIECE/'----+----I'/                                          HISTO218
      IF(II(IH).NE.0) THEN                                              HISTO219
        WRITE(6,9001) IH,TITLE                                          HISTO220
        WRITE(6,9002) SCM(M),IU(IH),II(IH),IO(IH)                       HISTO221
        WRITE(6,9003) (PIECE,K=1,3)                                     HISTO222
        XMX=XM(IH)*1.00001                                              HISTO223
        BSZ=(XU(IH)-XL(IH))/FLOAT(NB(IH))                               HISTO224
        DO 1 K=1,NB(IH)                                                 HISTO225
          BUP=XL(IH)+BSZ*K                                              HISTO226
          IF(M.EQ.0) THEN                                               HISTO227
            J=INT(H(IH,K)/XMX*JR)+1                                     HISTO228
          ELSE                                                          HISTO229
            IF(H(IH,K).LE.0.) THEN                                      HISTO230
              J=0                                                       HISTO231
            ELSE                                                        HISTO232
              J=INT(DLOG(H(IH,K))/DLOG(XMX)*JR)+1                       HISTO233
            ENDIF                                                       HISTO234
          ENDIF                                                         HISTO235
          IF(J.GT.0) REGEL(J)=STAR                                      HISTO236
          WRITE(6,9004) BUP,H(IH,K),(REGEL(JJ),JJ=1,JR)                 HISTO237
          IF(J.GT.0) REGEL(J)=BLANK                                     HISTO238
    1   CONTINUE                                                        HISTO239
        WRITE(6,9003) (PIECE,K=1,3)                                     HISTO240
      ELSE                                                              HISTO241
        WRITE(6,9009) IH,IU(IH),IO(IH)                                  HISTO242
      ENDIF                                                             HISTO243
 9001 FORMAT(' '/,I3,': ',A45)                                          HISTO244
 9002 FORMAT(' ',A3,' SCALE;  ENTRIES UNDER,INSIDE,OVER :',3I10)        HISTO245
 9003 FORMAT(30X,'I',3A10)                                              HISTO246
 9004 FORMAT(' ',2E12.4,5X,'I',30A1,'I')                                HISTO247
 9009 FORMAT(' '/,' HISTOGRAM #',I2,' IS EMPTY: ',                      HISTO248
     . ' UNDERFLOW ',I10,',     OVERFLOW',I10)                          HISTO249
      END                                                               HISTO250
      SUBROUTINE HISTO3(IH)                                             HISTO3 2
C--------------------------------------------------------               HISTO3 3
C INITIALIZE HISTOGRAMS (IH=0: ALL HISTOGRAMS)                          HISTO3 4
C--------------------------------------------------------               HISTO3 5
      IMPLICIT REAL*8(A-H,O-Z)                                          HISTO3 6
      SAVE                                                              HISTO3 7
C Histogram commons                                                     HISCOCO2
      PARAMETER(JH=20,JB=50,JR=30)                                      HISCOCO3
      COMMON / HISCO1 /H(JH,JB),NB(JH),XL(JH),XU(JH),XM(JH),            HISCOCO4
     .                 II(JH),IU(JH),IO(JH)                             HISCOCO5
      IF(IH.EQ.0) THEN                                                  HISTO3 9
        IH1=1                                                           HISTO310
        IH2=JH                                                          HISTO311
      ELSE                                                              HISTO312
        IH1=IH                                                          HISTO313
        IH2=IH                                                          HISTO314
      ENDIF                                                             HISTO315
      DO 2 J=IH1,IH2                                                    HISTO316
        DO 1 K=1,JB                                                     HISTO317
          H(J,K)=0.                                                     HISTO318
    1   CONTINUE                                                        HISTO319
        XM(J)=0.                                                        HISTO320
        II(J)=0                                                         HISTO321
        IU(J)=0                                                         HISTO322
        IO(J)=0                                                         HISTO323
    2 CONTINUE                                                          HISTO324
      END                                                               HISTO325
      SUBROUTINE LTRANS(CHAR1,ITYPE,IERR)                               LTRANS 2
C-------------------------------------------------------------------    LTRANS 3
C! Associate a number to the first letter of the diagram.               LTRANS 4
C INPUTS : CHAR1 - character                                            LTRANS 5
C OUTPUTS : ITYPE - number of diagram                                   LTRANS 6
C           IERR  - # 0 if there is an unexpected letter                LTRANS 7
C-------------------------------------------------------------------    LTRANS 8
C Local variables                                                       LTRANS 9
      CHARACTER*1 CHAR1                                                 LTRANS10
      INTEGER ITYPE,IERR                                                LTRANS11
C                                                                       LTRANS12
      IERR = 0                                                          LTRANS13
      IF (CHAR1.EQ.'M') THEN                                            LTRANS14
         ITYPE = 1                                                      LTRANS15
      ELSEIF (CHAR1.EQ.'B') THEN                                        LTRANS16
         ITYPE = 2                                                      LTRANS17
      ELSEIF (CHAR1.EQ.'C') THEN                                        LTRANS18
         ITYPE = 3                                                      LTRANS19
      ELSEIF (CHAR1.EQ.'A') THEN                                        LTRANS20
         ITYPE = 4                                                      LTRANS21
      ELSE                                                              LTRANS22
C Unexpected letter.                                                    LTRANS23
         IERR = 1                                                       LTRANS24
      ENDIF                                                             LTRANS25
C                                                                       LTRANS26
      END                                                               LTRANS27
      SUBROUTINE MULTIP(K1,K2,Q1,Q2,IACC)                               MULTIP 2
C----------------------------------------------------------             MULTIP 3
C Multiperipheral phase-space generator                                 MULTIP 4
C JH w/ RK, 10/91                                                       MULTIP 5
C----------------------------------------------------------             MULTIP 6
      IMPLICIT NONE                                                     MULTIP 7
      SAVE                                                              MULTIP 8
C local variables                                                       MULTIP 9
      INTEGER K1,K2,Q1,Q2,IACC,IONC,ID                                  MULTIP10
      REAL*8 PCM(4),SKMIN,SK,CON3,                                      MULTIP11
     &    Z,BETA,TAU,X1,X2,Q10,Q20,CS1,CS2,PH1,PH2,TH1,TH2,             MULTIP12
     &    KVEC(4),                                                      MULTIP13
     &    PI,TWOPI,RN,LAM1,HILGTX                                       MULTIP14
      LOGICAL DECAY2                                                    MULTIP15
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C Power laws for various distributions in our ps generators             POWERLW2
      REAL*8 ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ,BBCS2,   POWERLW3
     &       BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF             POWERLW4
      COMMON /DPWRS/ ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ, POWERLW5
     &      BBCS2, BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF       POWERLW6
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C setup commons                                                         SETUPCO2
      REAL*8 ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                          SETUPCO3
      COMMON /SETUP/ ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                  SETUPCO4
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
      DATA IONC /0/                                                     MULTIP23
C     DATA SKOFF /50.D0/                                                MULTIP24
C                                                                       MULTIP25
C Initialization                                                        MULTIP26
      IACC = 0                                                          MULTIP27
      PCM(1) = 0.D0                                                     MULTIP28
      PCM(2) = 0.D0                                                     MULTIP29
      PCM(3) = 0.D0                                                     MULTIP30
      PCM(4) = ECMISR                                                   MULTIP31
      IF (IONC.EQ.0) THEN                                               MULTIP32
         PI=4.D0*DATAN(1.D0)                                            MULTIP33
         TWOPI=2.D0*PI                                                  MULTIP34
         IONC = 1                                                       MULTIP35
      ENDIF                                                             MULTIP36
      IF (K1.EQ.3) THEN                                                 MULTIP37
         SKMIN = X2MN34                                                 MULTIP38
      ELSE                                                              MULTIP39
         SKMIN = X2MN56                                                 MULTIP40
      ENDIF                                                             MULTIP41
C                                                                       MULTIP42
C First choose SK, the invariant mass**2 of the (k1,k2) system, assumingMULTIP43
C a 1/(SK+SKOFF)**MUSK behavior                                         MULTIP44
C                                                                       MULTIP45
C This behaviour is multiplied by the ratio R (e+e- -> qqbar) in case ofMULTIP46
C a qqbar system (P. Janot)                                             MULTIP47
C                                                                       MULTIP48
      IF ( kindqq(k1) .EQ. 0 .AND. kindqq(k2) .EQ. 0 ) THEN             MULTIP49
        SK = HILGTX(SKOFF,MUSK,SKMIN,Y2MXKM)                            MULTIP50
      ELSE                                                              MULTIP51
        CALL qcd2(kindqq(k1),8,skmin,y2mxkm,sk)                         MULTIP52
      ENDIF                                                             MULTIP53
C                                                                       MULTIP54
C Next, the angle of q1 and q2 w/r/t beam                               MULTIP55
C Generate according to 1/(1-cos)**MUCOST                               MULTIP56
      CS1 = -HILGTX(1.D0,MUCOST,CSCNEV,-CSCNEV)                         MULTIP57
      CS2 = -HILGTX(1.D0,MUCOST,CSCNEV,-CSCNEV)                         MULTIP58
C Flip CS2 around!                                                      MULTIP59
      CS2 = -CS2                                                        MULTIP60
C Azimuthal angles                                                      MULTIP61
      PH1 = TWOPI*RN(4)                                                 MULTIP62
      PH2 = TWOPI*RN(5)                                                 MULTIP63
C                                                                       MULTIP64
C To generate the energy of Q1 requires that we build a few parameters  MULTIP65
C Angle between Q1 and Q2 :                                             MULTIP66
      TH1 = ACOS(CS1)                                                   MULTIP67
      TH2 = ACOS(CS2)                                                   MULTIP68
      Z   = CS1*CS2 + SIN(TH1)*SIN(TH2)*COS(PH1-PH2)                    MULTIP69
      TAU = (1.D0 - Z)/2.D0                                             MULTIP70
      BETA = 1.D0 - SK/SISR                                             MULTIP71
C Energy of Q1                                                          MULTIP72
      CON3 = DLOG(1.D0 - BETA*TAU)                                      MULTIP73
      X1   = (1.D0 - EXP(RN(6)*CON3))/TAU                               MULTIP74
      Q10  = X1*EBEISR                                                  MULTIP75
C Energy-mom conservation fixes Q2                                      MULTIP76
      X2   = (BETA - X1)/(1.D0 - X1*TAU)                                MULTIP77
      Q20  = X2*EBEISR                                                  MULTIP78
C Build the q vectors.  ASSUME electron mass = 0                        MULTIP79
      PEVT(1,Q1) = Q10*SIN(TH1)*COS(PH1)                                MULTIP80
      PEVT(2,Q1) = Q10*SIN(TH1)*SIN(PH1)                                MULTIP81
      PEVT(3,Q1) = Q10*CS1                                              MULTIP82
      PEVT(4,Q1) = Q10                                                  MULTIP83
C                                                                       MULTIP84
      PEVT(1,Q2) = Q20*SIN(TH2)*COS(PH2)                                MULTIP85
      PEVT(2,Q2) = Q20*SIN(TH2)*SIN(PH2)                                MULTIP86
      PEVT(3,Q2) = Q20*CS2                                              MULTIP87
      PEVT(4,Q2) = Q20                                                  MULTIP88
C And build the K vector from energy-mom cons.                          MULTIP89
      DO 11 ID = 1, 4                                                   MULTIP90
 11      KVEC(ID) = PCM(ID) - PEVT(ID,Q1) - PEVT(ID,Q2)                 MULTIP91
C Now decay the K vector uniformly in its rest frame                    MULTIP92
      IF (.NOT.DECAY2(KVEC,SK,MASS2(K1),MASS2(K2),PEVT(1,K1),           MULTIP93
     &   PEVT(1,K2),LAM1)) GOTO 999                                     MULTIP94
      IACC = 1                                                          MULTIP95
C printout                                                              MULTIP96
      IF (NEVTIN.LE.NEVPRI.AND.NBUNCH.EQ.1.AND.DEBGLV.GE.1) THEN        MULTIP97
         PRINT'('' K parts,SK,CS1,CS2'',2I2,1X,3F10.3)',                MULTIP98
     &        K1,K2,SK,CS1,CS2                                          MULTIP99
      ENDIF                                                             MULTI100
 999  CONTINUE                                                          MULTI101
      END                                                               MULTI102
      SUBROUTINE PHASE(IREJ)                                            PHASE  2
C-------------------------------------------------------------------    PHASE  3
C Steering for phase space.  Choose generator and form p.s. densities.  PHASE  4
C-------------------------------------------------------------------    PHASE  5
      IMPLICIT NONE                                                     PHASE  6
      SAVE                                                              PHASE  7
C Local variables                                                       PHASE  8
      INTEGER IACC,ICALC,IREJ,INIT,I,J,N,K,K1,GENR,IN,JC,IG             PHASE  9
      REAL*8 R,RN,DENSR,DENST,DENSAN,DENSBF,DENSBB,DENSC1,DENSC2,       PHASE 10
     &       DENC1Z,DENC2Z,DENSMU,RAMDEN,QTOT(4),DUM,EPS                PHASE 11
      LOGICAL LERR,ACCEPT                                               PHASE 12
      LOGICAL LNAN,PRINTE                                               PHASE 13
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C BRANCH commons                                                        BRANCHC2
      INTEGER LCHMX,PAR1,PAR2,PAR3,PAR4,PAR5,PAR6,NEVTGN,NRJGN,         BRANCHC3
     &   NCHAN,NCHANT,GENRJC,LGENR,JCHAN,CHACOU,CHANLS,                 BRANCHC4
     &   LANNIH,LBREMF,LBREMB,LCONV1,LCONV2,LCON1Z,LCON2Z,LMULTI,LRAMBO BRANCHC5
      PARAMETER (LCHMX = 100, LGENR = 9, LANNIH = 1, LBREMF = 2,        BRANCHC6
     &   LBREMB = 3, LCONV1 = 4, LCONV2 = 5,  LCON1Z = 6, LCON2Z = 7,   BRANCHC7
     &   LMULTI = 8, LRAMBO =9)                                         BRANCHC8
      REAL*8 SMINP,SMAXP,SMX3,SKMIN,SKMAX,BRNCH,FRAC,CGEN               BRANCHC9
      COMMON /BRANCH/ PAR1(LCHMX),PAR2(LCHMX),PAR3(LCHMX),PAR4(LCHMX),  BRANCH10
     &   PAR5(LCHMX),PAR6(LCHMX),NEVTGN(LGENR),NRJGN(LGENR),NCHAN,      BRANCH11
     &   NCHANT,GENRJC(LCHMX),JCHAN(LBUNCH),CHACOU(0:LGENR),            BRANCH12
     &   CHANLS(LGENR),                                                 BRANCH13
     &   SKMIN,SKMAX,SMINP(LCHMX),SMAXP(LCHMX),SMX3(LCHMX),             BRANCH14
     &   BRNCH(0:LCHMX),FRAC(LCHMX),CGEN(LGENR)                         BRANCH15
      CHARACTER*16 GNNAME(LGENR)                                        BRANCH16
      COMMON /GNNAMS/ GNNAME                                            BRANCH17
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C Phase space weights                                                   PHASSPA2
      REAL*8 PSWT,PSWTEV,DENS                                           PHASSPA3
      COMMON /PHASEP/ PSWTEV,PSWT(LBUNCH),DENS(100,LBUNCH)              PHASSPA4
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
      LOGICAL NEWEV(LGENR)                                              PHASE 21
C                                                                       PHASE 22
      DATA EPS /1.D-5/                                                  PHASE 23
C                                                                       PHASE 24
      IREJ = 1                                                          PHASE 25
      IN = NEVTIN + 1                                                   PHASE 26
C                                                                       PHASE 27
      IF (EXTSOU) THEN                                                  PHASE 28
         JC = NCHANT                                                    PHASE 29
         GENR = LRAMBO                                                  PHASE 30
         IACC = 1                                                       PHASE 31
         GOTO 21                                                        PHASE 32
      ENDIF                                                             PHASE 33
C                                                                       PHASE 34
C Choose a channel                                                      PHASE 35
      R = RN(1)                                                         PHASE 36
      DO 118 JC = 1, NCHANT                                             PHASE 37
  118     IF (R.LE.BRNCH(JC))  GOTO 119                                 PHASE 38
  119 CONTINUE                                                          PHASE 39
C                                                                       PHASE 40
      GENR = GENRJC(JC)                                                 PHASE 41
      GOTO (1,2,3,4,5,6,7,8,9) GENR                                     PHASE 42
C Call annihilation phase-space generator                               PHASE 43
 1       CALL ANNIHI(SMINP(JC),SMAXP(JC),SMX3(JC),                      PHASE 44
     &     PAR3(JC),PAR4(JC),PAR5(JC),PAR6(JC),IACC)                    PHASE 45
      GOTO 19                                                           PHASE 46
C Call Bremsstrahlung forward phase-space generator                     PHASE 47
 2       CALL BREMF(SKMIN,SKMAX,PAR1(JC),PAR2(JC),                      PHASE 48
     &     PAR3(JC),PAR4(JC),PAR5(JC),PAR6(JC),IACC)                    PHASE 49
      GOTO 19                                                           PHASE 50
C Call Bremsstrahlung backward phase-space generator                    PHASE 51
 3       CALL BREMB(SKMIN,SKMAX,SMX3(JC),PAR1(JC),PAR2(JC),             PHASE 52
     &     PAR3(JC),PAR4(JC),PAR5(JC),PAR6(JC),IACC)                    PHASE 53
      GOTO 19                                                           PHASE 54
C Call Conversion cat-whiskers phase-space generator                    PHASE 55
 4       CALL CONVS1(PAR3(JC),PAR4(JC),PAR5(JC),PAR6(JC),IACC)          PHASE 56
      GOTO 19                                                           PHASE 57
C Call Conversion one whisker phase-space generator                     PHASE 58
 5       CALL CONVS2(PAR3(JC),PAR4(JC),PAR5(JC),PAR6(JC),IACC)          PHASE 59
      GOTO 19                                                           PHASE 60
C Call Conversion one whisker phase-space generator                     PHASE 61
 6       CALL CONV1Z(PAR3(JC),PAR4(JC),PAR5(JC),PAR6(JC),IACC)          PHASE 62
      GOTO 19                                                           PHASE 63
C Call Conversion 2 Z  phase-space generator                            PHASE 64
 7       CALL CONV2Z(PAR3(JC),PAR4(JC),PAR5(JC),PAR6(JC),IACC)          PHASE 65
      GOTO 19                                                           PHASE 66
C Call Multiperipheral phase-space gernator                             PHASE 67
 8       CALL MULTIP(PAR3(JC),PAR4(JC),PAR5(JC),PAR6(JC),IACC)          PHASE 68
      GOTO 19                                                           PHASE 69
C Call RAMBO (flat phase space generator)                               PHASE 70
 9       CALL RANEVT(DENSR,IACC)                                        PHASE 71
 19   CONTINUE                                                          PHASE 72
C                                                                       PHASE 73
C Add final-state radiation photons if requested                        PHASE 74
      IF (FSRFL)  CALL DOFSR                                            PHASE 75
 21   NEVTGN(GENR) = NEVTGN(GENR) + 1                                   PHASE 76
C                                                                       PHASE 77
C Were there problems with the generation?                              PHASE 78
      IF (IACC.EQ.0)  THEN                                              PHASE 79
         NRJGN(GENR) = NRJGN(GENR) + 1                                  PHASE 80
         GOTO 999                                                       PHASE 81
C Is the event any good topologically-wise?                             PHASE 82
      ELSEIF (.NOT.ACCEPT(DUM)) THEN                                    PHASE 83
         NRJGN(GENR) = NRJGN(GENR) + 1                                  PHASE 84
         GOTO 999                                                       PHASE 85
      ENDIF                                                             PHASE 86
C                                                                       PHASE 87
C Build dot-product array for use by DENS routines                      PHASE 88
      CALL DOTBIL                                                       PHASE 89
      IN = NEVTIN+1                                                     PHASE 90
      JCHAN(IN) = JC                                                    PHASE 91
C Get RAMBO's density                                                   PHASE 92
      IF (GENR.LE.LGENR-1 .OR. EXTSOU)  DENSR = RAMDEN(PEVT,ICALC)      PHASE 93
C                                                                       PHASE 94
C Form overall density                                                  PHASE 95
      IF (FLA(3).EQ.FLA(5)) THEN                                        PHASE 96
         INIT = 2                                                       PHASE 97
      ELSE                                                              PHASE 98
         INIT = 1                                                       PHASE 99
      ENDIF                                                             PHASE100
      DENST = 0.D0                                                      PHASE101
C                                                                       PHASE102
      DO 23 I = 1, LGENR-1                                              PHASE103
 23      NEWEV(I) = .TRUE.                                              PHASE104
      DO 25 I = 1, NCHANT-1                                             PHASE105
         IG = GENRJC(I)                                                 PHASE106
         IF (FRAC(I).LT.EPS)  GOTO 25                                   PHASE107
C Fisrt call does initialization                                        PHASE108
         IF (I.GT.CHACOU(LANNIH-1).AND.I.LE.CHACOU(LANNIH)) THEN        PHASE109
            DENS(I,IN) = DENSAN(PAR5(I),PAR3(I),PAR4(I),INIT,           PHASE110
     &         NEWEV(IG),LERR)                                          PHASE111
         ELSEIF (I.GT.CHACOU(LBREMF-1).AND.I.LE.CHACOU(LBREMF)) THEN    PHASE112
            DENS(I,IN) = DENSBF(PAR1(I),PAR2(I),PAR3(I),PAR4(I),        PHASE113
     &         PAR6(I),NEWEV(IG),LERR)                                  PHASE114
         ELSEIF (I.GT.CHACOU(LBREMB-1).AND.I.LE.CHACOU(LBREMB)) THEN    PHASE115
            DENS(I,IN) = DENSBB(PAR2(I),PAR3(I),PAR4(I),PAR5(I),        PHASE116
     &         PAR6(I),NEWEV(IG),LERR)                                  PHASE117
         ELSEIF (I.GT.CHACOU(LCONV1-1).AND.I.LE.CHACOU(LCONV1)) THEN    PHASE118
            DENS(I,IN) = DENSC1(PAR3(I),PAR4(I),PAR5(I),PAR6(I),        PHASE119
     &         NEWEV(IG),LERR)                                          PHASE120
         ELSEIF (I.GT.CHACOU(LCONV2-1).AND.I.LE.CHACOU(LCONV2)) THEN    PHASE121
            DENS(I,IN) = DENSC2(PAR3(I),PAR4(I),PAR5(I),PAR6(I),        PHASE122
     &         NEWEV(IG),LERR)                                          PHASE123
         ELSEIF (I.GT.CHACOU(LCON1Z-1).AND.I.LE.CHACOU(LCON1Z)) THEN    PHASE124
            DENS(I,IN) = DENC1Z(PAR3(I),PAR4(I),PAR5(I),PAR6(I),        PHASE125
     &         NEWEV(IG),LERR)                                          PHASE126
         ELSEIF (I.GT.CHACOU(LCON2Z-1).AND.I.LE.CHACOU(LCON2Z)) THEN    PHASE127
            DENS(I,IN) = DENC2Z(PAR3(I),PAR4(I),PAR5(I),PAR6(I),        PHASE128
     &         NEWEV(IG),LERR)                                          PHASE129
         ELSEIF (I.GT.CHACOU(LMULTI-1).AND.I.LE.CHACOU(LMULTI)) THEN    PHASE130
            DENS(I,IN) = DENSMU(PAR3(I),PAR4(I),PAR5(I),PAR6(I),        PHASE131
     &         NEWEV(IG),LERR)                                          PHASE132
         ENDIF                                                          PHASE133
         IF (LERR)  THEN                                                PHASE134
            PRINT'(/'' Unexpected error from DENS.  Continue.''/)'      PHASE135
            GOTO 999                                                    PHASE136
         ENDIF                                                          PHASE137
         DENST = DENST + FRAC(I)*DENS(I,IN)                             PHASE138
         INIT = 0                                                       PHASE139
   25 CONTINUE                                                          PHASE140
   26 CONTINUE                                                          PHASE141
      DENS(NCHANT,IN) = DENSR                                           PHASE142
      IF (DENS(JC,IN).LE.1.D-20) THEN                                   PHASE143
         PRINT'('' *** Density in NEVENT='',I8,'' is wrong''/           PHASE144
     &       '' ps generator='',I2,'' JC='',I2,'' DENS='',G16.9/        PHASE145
     &       '' Action: Contact expert ! ''/)',                         PHASE146
     &       NEVENT,GENR,JC,DENS(JC,IN)                                 PHASE147
         PRINTE = .TRUE.                                                PHASE148
      ELSE                                                              PHASE149
         PRINTE = .FALSE.                                               PHASE150
      ENDIF                                                             PHASE151
      DENST = DENST + FRAC(NCHANT)*DENS(NCHANT,IN)                      PHASE152
C                                                                       PHASE153
      PSWTEV = 1.D0/DENST                                               PHASE154
C                                                                       PHASE155
C *** debug ***                                                         PHASE156
C     LNAN = .NOT.(PSWTEV.GT.0 .OR. PSWTEV.LE.0)                        PHASE157
      IREJ = 0                                                          PHASE158
      IF (NEVTIN.LE.NEVPRI.AND.NBUNCH.EQ.1.AND.DEBGLV.GE.1) THEN        PHASE159
C     IF ((NEVTIN.LE.NEVPRI.AND.NBUNCH.EQ.1.AND.DEBGLV.GE.1).OR.        PHASE160
C    &        LNAN .OR. PRINTE) THEN                                    PHASE161
C                                                                       PHASE162
C PRINT out momenta                                                     PHASE163
C        PRINT'('' Bunch is '',I4)',NBUNCH                              PHASE164
         PRINT'('' Accepted event'',I6)',IN                             PHASE165
         WRITE(*,*) ' MOMENTA:'                                         PHASE166
         WRITE(*,91) 'LBL','X','Y','Z','ENERGY','MASS'                  PHASE167
   91    FORMAT(A4,5A12)                                                PHASE168
         DO 31 N=1, NOPART                                              PHASE169
           WRITE(*,92) N,(POUT(K,N),K=1,4),MASS(N)                      PHASE170
   92      FORMAT(I4,5F12.5)                                            PHASE171
   31    CONTINUE                                                       PHASE172
         DO 41 K=1,4                                                    PHASE173
            QTOT(K)=POUT(K,1)+POUT(K,2)                                 PHASE174
            DO 42 K1 = 3, NOPART                                        PHASE175
               QTOT(K) = QTOT(K) - POUT(K,K1)                           PHASE176
   42       CONTINUE                                                    PHASE177
   41    CONTINUE                                                       PHASE178
         WRITE(*,93) ' TOTS:',(QTOT(K),K=1,4)                           PHASE179
   93    FORMAT(A6,1X,4F12.5)                                           PHASE180
         PRINT'(/'' Event p.s. weight : '',G20.12)',PSWTEV              PHASE181
         PRINT'(/'' PS generator :'',A17,'', channel chosen :'',I3)',   PHASE182
     &      GNNAME(GENR),JC                                             PHASE183
         PRINT'('' Ph. space densities :''/7(7(1X,G10.4)/))',           PHASE184
     &      (DENS(I,IN),I=1,NCHANT)                                     PHASE185
C        IF (LNAN)  STOP                                                PHASE186
      ENDIF                                                             PHASE187
C                                                                       PHASE188
  999 CONTINUE                                                          PHASE189
      END                                                               PHASE190
      SUBROUTINE PROSET                                                 PROSET 2
C-----------------------------------------------------------------------PROSET 3
C SET KINEMATICS, FLAVOURS, CHARGES AND WEAK COUPLINGS AND JOB STUFF    PROSET 4
C BFAC(I)=-1 FOR I IN INITIAL STATE, BFAC(I)=+1 FOR I IN FINAL STATE    PROSET 5
C CONVENTIONS: 1,3,5 MUST BE OUTFLOWING FERMIONS (INCOMING E+, F OUT)   PROSET 6
C              2,4,6 MUST BE INFLOWING FERMIONS  (INCOMING E-, FBAR OUT)PROSET 7
C-----------------------------------------------------------------------PROSET 8
      IMPLICIT NONE                                                     PROSET 9
      SAVE                                                              PROSET10
C                                                                       PROSET11
C Local variables                                                       PROSET12
      REAL*8 PI,ELEM,ELEZ,SMIN,SMAX,ALPHAZ,CGENT,PMAX                   PROSET13
      INTEGER K,I,ISEED,IN,LENOCC,ITYPE,IERR,ID,PART,ICM                PROSET14
      CHARACTER*4 SPACES,CTEMP                                          PROSET15
C THE TYPE OF A GVEN DIAGRAM                                            PROSET16
      CHARACTER*16  TYPENAME(4)                                         PROSET17
C                                                                       PROSET18
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C The quark and lepton masses                                           QMASSES2
      REAL*8 amu, amd, ams, amc, amb, amt                               QMASSES3
      REAL*8 amel, ammu, amto, amne, amnm, amnt                         QMASSES4
      PARAMETER ( amu = .005D0, amd = .010d0, ams = .150d0 )            QMASSES5
      PARAMETER ( amc = 1.37d0, amb = 4.70d0, amt = 170.d0 )            QMASSES6
      PARAMETER ( amel= .511D-3, ammu= .1057D0, amto = 1.777d0 )        QMASSES7
      PARAMETER ( amne= 1D-4, amnm= 1D-4, amnt = 1D-4 )                 QMASSES8
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
C setup commons                                                         SETUPCO2
      REAL*8 ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                          SETUPCO3
      COMMON /SETUP/ ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                  SETUPCO4
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C Power laws for various distributions in our ps generators             POWERLW2
      REAL*8 ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ,BBCS2,   POWERLW3
     &       BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF             POWERLW4
      COMMON /DPWRS/ ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ, POWERLW5
     &      BBCS2, BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF       POWERLW6
C BRANCH commons                                                        BRANCHC2
      INTEGER LCHMX,PAR1,PAR2,PAR3,PAR4,PAR5,PAR6,NEVTGN,NRJGN,         BRANCHC3
     &   NCHAN,NCHANT,GENRJC,LGENR,JCHAN,CHACOU,CHANLS,                 BRANCHC4
     &   LANNIH,LBREMF,LBREMB,LCONV1,LCONV2,LCON1Z,LCON2Z,LMULTI,LRAMBO BRANCHC5
      PARAMETER (LCHMX = 100, LGENR = 9, LANNIH = 1, LBREMF = 2,        BRANCHC6
     &   LBREMB = 3, LCONV1 = 4, LCONV2 = 5,  LCON1Z = 6, LCON2Z = 7,   BRANCHC7
     &   LMULTI = 8, LRAMBO =9)                                         BRANCHC8
      REAL*8 SMINP,SMAXP,SMX3,SKMIN,SKMAX,BRNCH,FRAC,CGEN               BRANCHC9
      COMMON /BRANCH/ PAR1(LCHMX),PAR2(LCHMX),PAR3(LCHMX),PAR4(LCHMX),  BRANCH10
     &   PAR5(LCHMX),PAR6(LCHMX),NEVTGN(LGENR),NRJGN(LGENR),NCHAN,      BRANCH11
     &   NCHANT,GENRJC(LCHMX),JCHAN(LBUNCH),CHACOU(0:LGENR),            BRANCH12
     &   CHANLS(LGENR),                                                 BRANCH13
     &   SKMIN,SKMAX,SMINP(LCHMX),SMAXP(LCHMX),SMX3(LCHMX),             BRANCH14
     &   BRNCH(0:LCHMX),FRAC(LCHMX),CGEN(LGENR)                         BRANCH15
      CHARACTER*16 GNNAME(LGENR)                                        BRANCH16
      COMMON /GNNAMS/ GNNAME                                            BRANCH17
C Event writing block                                                   WRITING2
      INTEGER IWRITE,CODE,NWRITN,NWRIMX                                 WRITING3
      REAL*8 XMAXWT                                                     WRITING4
      COMMON /EWRITE/ XMAXWT,IWRITE,CODE(11),NWRITN,NWRIMX              WRITING5
C Some physics constants                                                CONSTPH2
      REAL*8 ALPHA,RMZ,RGZ,SW2,RMZ2,RMZRGZ,RMZGZ2                       CONSTPH3
      COMMON / FISIKX / ALPHA,RMZ,RGZ,SW2,RMZ2,RMZRGZ,RMZGZ2            CONSTPH4
C ps generator weights for different cms energies                       PSGENWG2
      INTEGER LENERS,LENERD                                             PSGENWG3
      PARAMETER (LENERS = 19, LENERD = LENERS+1)                        PSGENWG4
      REAL*8 ANPSWT,CVPSWT,BRPSWT,MUPSWT,CMSPSW                         PSGENWG5
      COMMON /WTVSEN/ ANPSWT(0:LENERD,2),CVPSWT(0:LENERD,2),            PSGENWG6
     &       BRPSWT(0:LENERD,2),MUPSWT(0:LENERD,2),CMSPSW(0:LENERS)     PSGENWG7
      REAL*8 XME,XM34,XM56,THETCN                                       PROSECO2
      REAL*4 SCGEN                                                      PROSECO3
      INTEGER IISR,IFSR,IAUTOW,CH334,CH356,QCDFLG                       PROSECO4
      CHARACTER*4 CDIAGS                                                PROSECO5
      COMMON / copro1 / xme,xm34,xm56,thetcn                            PROSECO6
      COMMON / copro2 / iisr,ifsr,iautow,ch334,ch356,qcdflg             PROSECO7
      COMMON / copro3 / cdiags                                          PROSECO8
      COMMON / copro4 / scgen(lgenr)                                    PROSECO9
      REAL*8 ANNIWT(LENERS,2),CONVWT(LENERS,2),BREMWT(LENERS,2),        PROSET32
     &       CMSENS(LENERS)                                             PROSET33
C                                                                       PROSET34
      CHARACTER*5 ANTI                                                  PROSET35
C                                                                       PROSET36
      DATA TYPENAME /'MULTIPERPIPHERAL', 'BREMSSTRAHLUNG', 'CONVERSION',PROSET37
     &   'ANNIHILIATION'/                                               PROSET38
C These wts are in percentages, and have been determined in trial runs  PROSET39
C w/ Mz=91.2, cen.det.ang=15.0, pmin=0.0, m(f,fbar)>50MeV.  Users are   PROSET40
C encouraged to supply their own tables for their own special purposes. PROSET41
C                                                                       PROSET42
C eemm wts first, then eeee wts.                                        PROSET43
C                   10,  20   30   40   50   60   70   80   88    89    PROSET44
C                    90   91.2 92   94   100  110  130  150  180        PROSET45
      DATA ANNIWT / 3.1, 3.2, 3.2, 3.3, 3.2, 3.3, 3.8, 7.1, 41.3, 56.5, PROSET46
     &               71.3,79.2,69.6,35.9,11.5,5.7, 3.8, 3.7, 3.0,       PROSET47
     &              1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.3, 2.6, 18.8, 27.8, PROSET48
     &               46.9,60.6,49.7,21.7,5.1, 2.3, 1.6, 1.2, 1.2/       PROSET49
      DATA CONVWT / 7.4, 7.0, 6.2, 5.7, 5.7, 5.4, 5.1, 5.5, 9.1,  9.4,  PROSET50
     &               9.0, 10.4,17.0,32.0,33.0,26.1,16.7,15.6,14.8,      PROSET51
     &              2.0, 1.7, 1.8, 1.7, 1.6, 1.6, 1.7, 1.7, 4.3,  5.2,  PROSET52
     &               6.6, 9.2, 13.9,18.2,11.7,8.3, 5.6, 3.3, 3.8/       PROSET53
      DATA BREMWT / 60.9,75.2,73.7,78.8,80.7,83.5,83.4,80.9,45.2,31.4,  PROSET54
     &               18.2,9.5, 12.3,29.2,50.8,63.0,74.9,75.8,77.7,      PROSET55
     &              83.9,88.2,90.8,90.4,91.6,92.7,92.8,91.3,73.4, 63.6, PROSET56
     &               44.7,29.1,34.9,57.6,80.2,86.3,90.1,91.6,91.8/      PROSET57
      DATA CMSENS / 10.,  20., 30., 40., 50., 60., 70., 80., 88., 89.,  PROSET58
     &               90., 91.2,92., 94., 100.,110.,130.,150.,180./      PROSET59
C                                                                       PROSET60
C Transfer of p.s. wt table into common arrays                          PROSET61
      CMSPSW(0) = 0.D0                                                  PROSET62
      CMSPSW(0) = 2.D3                                                  PROSET63
      DO 21 ID = 1, 2                                                   PROSET64
         DO 22 ICM = 1, LENERS                                          PROSET65
            ANPSWT(ICM,ID) = ANNIWT(ICM,ID)                             PROSET66
            CVPSWT(ICM,ID) = CONVWT(ICM,ID)                             PROSET67
            BRPSWT(ICM,ID) = BREMWT(ICM,ID)                             PROSET68
            MUPSWT(ICM,ID) = 100.D0 - ANNIWT(ICM,ID) - CONVWT(ICM,ID) - PROSET69
     &          BREMWT(ICM,ID)                                          PROSET70
            IF (ID.EQ.1)  CMSPSW(ICM) = CMSENS(ICM)                     PROSET71
 22      CONTINUE                                                       PROSET72
         ANPSWT(0,ID) = ANPSWT(1,ID)                                    PROSET73
         CVPSWT(0,ID) = CVPSWT(1,ID)                                    PROSET74
         BRPSWT(0,ID) = BRPSWT(1,ID)                                    PROSET75
         MUPSWT(0,ID) = MUPSWT(1,ID)                                    PROSET76
         ANPSWT(LENERD,ID) = ANPSWT(LENERS,ID)                          PROSET77
         BRPSWT(LENERD,ID) = BRPSWT(LENERS,ID)                          PROSET78
         CVPSWT(LENERD,ID) = CVPSWT(LENERS,ID)                          PROSET79
         MUPSWT(LENERD,ID) = MUPSWT(LENERS,ID)                          PROSET80
 21   CONTINUE                                                          PROSET81
C                                                                       PROSET82
      ALPHA=1.D0/137.0D0                                                PROSET83
      ALPHAZ=1.D0/128.87D0                                              PROSET84
      PI=4.D0*DATAN(1.D0)                                               PROSET85
      ELEM=DSQRT(4.D0*PI*ALPHA)                                         PROSET86
      ELEZ=DSQRT(4.D0*PI*ALPHAZ)                                        PROSET87
C                                                                       PROSET88
      BFAC(1)=-1.D0                                                     PROSET89
      BFAC(2)=-1.D0                                                     PROSET90
      BFAC(3)=1.D0                                                      PROSET91
      BFAC(4)=1.D0                                                      PROSET92
      BFAC(5)=1.D0                                                      PROSET93
      BFAC(6)=1.D0                                                      PROSET94
      NTKCEN = 4                                                        PROSET95
C                                                                       PROSET96
      PRINT'(/'' +++ THE FERMIS GENERATOR +++ ''/)'                     PROSET97
      PRINT*,' PHYSICS PARAMETERS:'                                     PROSET98
      PRINT*,' 1/ALPHA          =',1.D0/ALPHA                           PROSET99
      PRINT*,' 1/ALPHA(M_z)     =',1.D0/ALPHAZ                          PROSE100
      PRINT*,' Z MASS           =',RMZ,' GEV'                           PROSE101
      PRINT*,' Z WIDTH          =',RGZ,' GEV'                           PROSE102
      PRINT*,' SIN**2 THETA_W   =',SW2                                  PROSE103
      PRINT'(/''  CMS energy is '',F8.3,'' GeV'')',ECM                  PROSE104
      PRINT'(/''  The initial random number seed is '',I11)',ISEED      PROSE105
      NEVPRI = MIN(NEVPRI,LBUNCH,NEVTMX)                                PROSE106
      PRINT'(/''  No of requested events  '',I11)',NEVTMX               PROSE107
      PRINT'(/''  Detail of print-out is = '',I3,'',  for'',I5,         PROSE108
     &   '' events'')',DEBGLV,NEVPRI                                    PROSE109
C                                                                       PROSE110
C Set type and mass for quarks for the ratio R, if required             PROSE111
C                                                                       PROSE112
      CALL vzero(kindqq(1),6)                                           PROSE113
      IF ( qcdflg .EQ. 1 ) THEN                                         PROSE114
        IF     ( fla(3) .EQ. 'UQ' ) THEN                                PROSE115
          xm34  = amu                                                   PROSE116
          kindqq(3) = 1                                                 PROSE117
          kindqq(4) = 1                                                 PROSE118
        ELSEIF ( fla(3) .EQ. 'DQ' ) THEN                                PROSE119
          xm34  = amd                                                   PROSE120
          kindqq(3) = 2                                                 PROSE121
          kindqq(4) = 2                                                 PROSE122
        ELSEIF ( fla(3) .EQ. 'CQ' ) THEN                                PROSE123
          xm34  = amc                                                   PROSE124
          kindqq(3) = 4                                                 PROSE125
          kindqq(4) = 4                                                 PROSE126
        ELSEIF ( fla(3) .EQ. 'SQ' ) THEN                                PROSE127
          xm34  = ams                                                   PROSE128
          kindqq(3) = 3                                                 PROSE129
          kindqq(4) = 3                                                 PROSE130
        ELSEIF ( fla(3) .EQ. 'BQ' ) THEN                                PROSE131
          xm34  = amb                                                   PROSE132
          kindqq(3) = 5                                                 PROSE133
          kindqq(4) = 5                                                 PROSE134
        ENDIF                                                           PROSE135
C                                                                       PROSE136
        IF     ( fla(5) .EQ. 'UQ' ) THEN                                PROSE137
          xm56  = amu                                                   PROSE138
          kindqq(5) = 1                                                 PROSE139
          kindqq(6) = 1                                                 PROSE140
        ELSEIF ( fla(5) .EQ. 'DQ' ) THEN                                PROSE141
          xm56  = amd                                                   PROSE142
          kindqq(5) = 2                                                 PROSE143
          kindqq(6) = 2                                                 PROSE144
        ELSEIF ( fla(5) .EQ. 'CQ' ) THEN                                PROSE145
          xm56  = amc                                                   PROSE146
          kindqq(5) = 4                                                 PROSE147
          kindqq(6) = 4                                                 PROSE148
        ELSEIF ( fla(5) .EQ. 'SQ' ) THEN                                PROSE149
          xm56  = ams                                                   PROSE150
          kindqq(5) = 3                                                 PROSE151
          kindqq(6) = 3                                                 PROSE152
        ELSEIF ( fla(5) .EQ. 'BQ' ) THEN                                PROSE153
          xm56  = amb                                                   PROSE154
          kindqq(5) = 5                                                 PROSE155
          kindqq(6) = 5                                                 PROSE156
        ENDIF                                                           PROSE157
C                                                                       PROSE158
      ENDIF                                                             PROSE159
C Note that in case of identical pairs of outgoing particles, only the  PROSE160
C cuts on 3-4 are applied.                                              PROSE161
C XMIN34 - min. inv. mass of 3-4 pair, in GeV                           PROSE162
      XMIN34 = MAX(XMIN34,2.D0*XM34,1.D-6)                              PROSE163
C XMIN56 - min. inv. mass of 5-6 pair, in GeV                           PROSE164
      XMIN56 = MAX(XMIN56,2.D0*XM56,1.D-6)                              PROSE165
C XMAX34 - max. inv. mass of 3-4 pair, in GeV                           PROSE166
      XMAX34 = MIN(XMAX34,ECM-XMIN56)                                   PROSE167
C XMAX56 - max. inv. mass of 3-4 pair, in GeV                           PROSE168
      XMAX56 = MIN(XMAX56,ECM-XMIN34)                                   PROSE169
C                                                                       PROSE170
C Notify user if contradictory cuts are imposed                         PROSE171
C                                                                       PROSE172
      IF (XMAX56.LE.XMIN56 .OR. XMAX34.LE.XMIN34) THEN                  PROSE173
         PRINT'('' Contradictory cuts: XMAX34,XMIN34,XMAX56,XMIN56''    PROSE174
     &       ,4F10.4)',XMAX34,XMIN34,XMAX56,XMIN56                      PROSE175
         STOP                                                           PROSE176
      ENDIF                                                             PROSE177
C                                                                       PROSE178
C Here are various power laws of the ps generators.                     PROSE179
C                                                                       PROSE180
      ANSP   = 1.3D0                                                    PROSE181
      ANSQ   = 1.D0                                                     PROSE182
      ANCOST = 1.5D0                                                    PROSE183
      BFSK   = 1.4D0                                                    PROSE184
      BFK0   = 0.5D0                                                    PROSE185
      BFCOST = 1.7D0                                                    PROSE186
      BFCS2  = 1.7D0                                                    PROSE187
      BBSK   = BFSK                                                     PROSE188
      BBSQ   = 1.D0                                                     PROSE189
      BBCS2  = BFCS2                                                    PROSE190
      BBCOST = 1.D0                                                     PROSE191
      C1SK   = 1.5D0                                                    PROSE192
      C1K0   = 1.5D0                                                    PROSE193
      C2SKP  = 1.D0                                                     PROSE194
      C2CSK  = 1.D0                                                     PROSE195
      MUSK   = 1.D0                                                     PROSE196
      MUCOST = 1.5D0                                                    PROSE197
C FBOD2M is used for the 2-bod inv mass cut-off in multip. gen.         PROSE198
      FBOD2M = 0.85D0                                                   PROSE199
C                                                                       PROSE200
C Initialize the ratio R part, if needed                                PROSE201
C                                                                       PROSE202
      DO i = 1, 9                                                       PROSE203
        qcdf(i) = 1D0                                                   PROSE204
      ENDDO                                                             PROSE205
      IF     ( kindqq(3).NE.0 .AND. kindqq(4).NE.0 ) THEN               PROSE206
        DO i = 1, 8                                                     PROSE207
          IF ( i .NE. 7 ) THEN                                          PROSE208
            smin = xmin34**2                                            PROSE209
            smax = xmax34**2                                            PROSE210
            CALL qcd1(smin,smax,kindqq(3),i,qcdf(i))                    PROSE211
            xmin34 = DSQRT(smin)                                        PROSE212
            xmax34 = DSQRT(smax)                                        PROSE213
          ENDIF                                                         PROSE214
        ENDDO                                                           PROSE215
      ELSEIF ( kindqq(5).NE.0 .AND. kindqq(6).NE.0 ) THEN               PROSE216
        DO i = 1, 8                                                     PROSE217
          IF ( i .NE. 7 ) THEN                                          PROSE218
            smin = xmin56**2                                            PROSE219
            smax = xmax56**2                                            PROSE220
            CALL qcd1(smin,smax,kindqq(5),i,qcdf(i))                    PROSE221
            xmin56 = DSQRT(smin)                                        PROSE222
            xmax56 = DSQRT(smax)                                        PROSE223
          ENDIF                                                         PROSE224
        ENDDO                                                           PROSE225
      ENDIF                                                             PROSE226
C                                                                       PROSE227
C Check again if contradictory cuts are imposed                         PROSE228
C                                                                       PROSE229
      IF (XMAX56.LE.XMIN56 .OR. XMAX34.LE.XMIN34) THEN                  PROSE230
         PRINT'('' Contradictory cuts: XMAX34,XMIN34,XMAX56,XMIN56''    PROSE231
     &       ,4F10.4)',XMAX34,XMIN34,XMAX56,XMIN56                      PROSE232
         STOP                                                           PROSE233
      ENDIF                                                             PROSE234
C                                                                       PROSE235
      ISRFL = IISR.NE.0                                                 PROSE236
      FSRFL = IFSR.NE.0                                                 PROSE237
      IF (.NOT.FSRFL)  NOFSR = 0                                        PROSE238
      IF (FSRFL .AND. NOFSR.EQ.0)  THEN                                 PROSE239
         PRINT'(/'' *** FSR requested, but no. of FSR photons = 0'',    PROSE240
     &         '' => no FSR'')'                                         PROSE241
         FSRFL = .FALSE.                                                PROSE242
      ENDIF                                                             PROSE243
      PRINT'(/''  Initial-state radiation flag = '',L1)',ISRFL          PROSE244
      IF (ISRFL) THEN                                                   PROSE245
C check if KGMAX requested was too high                                 PROSE246
         PMAX = 1.D0 - (XMIN34+XMIN56+1.D-3)**2/ECM/ECM                 PROSE247
         IF (PMAX.LT.KGMAX) THEN                                        PROSE248
            PRINT'(''  Requested KGMAX of '',F7.5,'' is too high.''/    PROSE249
     &             ''  It has been readjusted to '',F7.5)',KGMAX,PMAX   PROSE250
            KGMAX = PMAX                                                PROSE251
         ELSE                                                           PROSE252
            PRINT'(''  Maximum value of k=Egam/Ebeam is '',F7.5)',      PROSE253
     &        KGMAX                                                     PROSE254
C These numbers are a first guess                                       PROSE255
         ENDIF                                                          PROSE256
         KG1 = 0.8D0*KGMAX                                              PROSE257
         KRATIO = -1.D0                                                 PROSE258
         PRINT'(''  Other internal details of ISR:''/                   PROSE259
     &          10X,''For the ISR spectrum with k<'',F5.3/              PROSE260
     &          10X,''the usual distribution is followed.  But''/       PROSE261
     &          10X,''for '',F5.3,''<k<'',F5.3,'' the spectrum is'',    PROSE262
     &          '' (k(1-k))**(-1).''/                                   PROSE263
     &          10X,''The frequency ratio of generation in these'',     PROSE264
     &          '' regions is'',F7.3)',KG1,KG1,KGMAX,KRATIO             PROSE265
         PRINT'(10X,''The ISR photon is produced parallel to''/         PROSE266
     &      '' the beam.'')'                                            PROSE267
      ENDIF                                                             PROSE268
      PRINT'(''  Final-state   radiation flag = '',L1)',FSRFL           PROSE269
      IF (FSRFL) PRINT'(''    with'',I5,'' FSR photons requested.'')',  PROSE270
     &       NOFSR                                                      PROSE271
      IF (FSRFL .AND. NOFSR.GT.4) THEN                                  PROSE272
         PRINT'('' *** TOO MANY FSR PHOTONS REQUESTED *** '')'          PROSE273
         STOP                                                           PROSE274
      ENDIF                                                             PROSE275
      IF (ISRFL .AND. KGMAX.LE.1.D-5) THEN                              PROSE276
         PRINT'('' *** KGMAX IS TOO SMALL *** '')'                      PROSE277
         STOP                                                           PROSE278
      ENDIF                                                             PROSE279
C No. of particles in each event                                        PROSE280
      NOPART = 6 + NOFSR                                                PROSE281
      IF (ISRFL)  NOPART = NOPART + 1                                   PROSE282
C                                                                       PROSE283
      PRINT'('' IWRITE='',I2)',IWRITE                                   PROSE284
      EXTSOU = .FALSE.                                                  PROSE285
      IF (IWRITE.EQ.0) THEN                                             PROSE286
         PRINT'(/''  Do not write out unweighted events. '')'           PROSE287
      ELSEIF (IWRITE.EQ.1) THEN                                         PROSE288
         PRINT'(/''  Write out unweighted events.''/                    PROSE289
     &      ''  The maximum weight used in the rejection is '',F9.3/    PROSE290
     &      ''  The maximum no. of events to write out is   '',I6)',    PROSE291
     &      XMAXWT,NWRIMX                                               PROSE292
      ELSEIF (IWRITE.EQ.2) THEN                                         PROSE293
         PRINT'(/''  Write out weighted events.''/                      PROSE294
     &      ''  The maximum no. of events to write out is   '',I6)',    PROSE295
     &      NWRIMX                                                      PROSE296
      ELSEIF (IWRITE.EQ.10) THEN                                        PROSE297
         PRINT'('' Read in events from unit 20.  Experts only.'')'      PROSE298
         EXTSOU = .TRUE.                                                PROSE299
      ENDIF                                                             PROSE300
      NWRITN = 0                                                        PROSE301
C     CALL RDMIN(ISEED)                                                 PROSE302
C Photons at the end                                                    PROSE303
      DO 41 PART = 7, NOPART                                            PROSE304
         FLA(PART)='GA'                                                 PROSE305
   41 CONTINUE                                                          PROSE306
C                                                                       PROSE307
C charged leptons : CHA = -1    * ELEM                                  PROSE308
C neutral leptons : CHA = 0                                             PROSE309
C up-type quarks  : CHA = +2/3  * ELEM                                  PROSE310
C down-type quarks: CHA = -1/3  * ELEM                                  PROSE311
      CHA(1)=-ELEM                                                      PROSE312
      CHA(2)=-ELEM                                                      PROSE313
      CHA(3)= (ELEM*CH334)/3                                            PROSE314
      CHA(4)= (ELEM*CH334)/3                                            PROSE315
      CHA(5)= (ELEM*CH356)/3                                            PROSE316
      CHA(6)= (ELEM*CH356)/3                                            PROSE317
      DO 45 ID = 1, 6                                                   PROSE318
 45      DCHA(ID) = ABS(CHA(ID))                                        PROSE319
C                                                                       PROSE320
C Learn which diagrams are desired, and which are possible!             PROSE321
      DO 50 ID = 1, 4                                                   PROSE322
   50    DWANT(ID) = .FALSE.                                            PROSE323
C remove spaces, capitalize                                             PROSE324
      CTEMP = SPACES(CDIAGS,0)                                          PROSE325
      CALL CLTOU(CTEMP)                                                 PROSE326
      IF (CTEMP(1:3).EQ.'ALL') THEN                                     PROSE327
         DO 51 ID = 1, 2                                                PROSE328
            DWANT(ID) = FLA(1).EQ.FLA(3) .OR. FLA(1).EQ.FLA(5)          PROSE329
   51    CONTINUE                                                       PROSE330
         DWANT(3) = .TRUE.                                              PROSE331
         DWANT(4) = .TRUE.                                              PROSE332
         GOTO 62                                                        PROSE333
      ENDIF                                                             PROSE334
C                                                                       PROSE335
      DO 61 ID = 1, LENOCC(CTEMP)                                       PROSE336
         CALL LTRANS(CTEMP(ID:ID),ITYPE,IERR)                           PROSE337
         IF (IERR.GT.0) THEN                                            PROSE338
            PRINT'('' Unrecognized diagram letter requested :'',A1)',   PROSE339
     &         CTEMP(ID:ID)                                             PROSE340
            STOP                                                        PROSE341
         ENDIF                                                          PROSE342
         IF (ITYPE.LE.2) THEN                                           PROSE343
            DWANT(ITYPE) = FLA(1).EQ.FLA(3) .OR. FLA(1).EQ.FLA(5)       PROSE344
         ELSE                                                           PROSE345
            DWANT(ITYPE) = .TRUE.                                       PROSE346
         ENDIF                                                          PROSE347
   61 CONTINUE                                                          PROSE348
   62 CONTINUE                                                          PROSE349
      PRINT'(/'' Diagrams which will be used : '')'                     PROSE350
      DO 63 ID = 1, 4                                                   PROSE351
   63    IF (DWANT(ID)) PRINT'(1X,A16)',TYPENAME(ID)                    PROSE352
C                                                                       PROSE353
C Specifiy kinematic cuts here.                                         PROSE354
C                                                                       PROSE355
      PRINT'(/''  Kinematic cuts imposed on the events : ''/            PROSE356
     &   ''  Min. ('',A2,'' ANTI-'',A2,'') inv. mass : '',              PROSE357
     &   F7.3,'' GeV''/                                                 PROSE358
     &   ''  Max. ('',A2,'' ANTI-'',A2,'') inv. mass : '',              PROSE359
     &   F7.3,'' GeV''/                                                 PROSE360
     &   ''  Min. '',A2,'' and ANTI-'',A2,'' momentum : '',F7.3,'' GeV''PROSE361
     &   )',FLA(3),FLA(3),XMIN34,FLA(3),FLA(3),XMAX34,FLA(3),FLA(3),    PROSE362
     &   XMOM34                                                         PROSE363
      IF (FLA(3).NE.FLA(5))  PRINT'(                                    PROSE364
     &   ''  Min. ('',A2,'' ANTI-'',A2,'') inv. mass : '',              PROSE365
     &   F7.3,'' GeV''/                                                 PROSE366
     &   ''  Max. ('',A2,'' ANTI-'',A2,'') inv. mass : '',              PROSE367
     &   F7.3,'' GeV''/                                                 PROSE368
     &   ''  Min. '',A2,'' and ANTI-'',A2,'' momentum : '',F7.3,'' GeV''PROSE369
     &   )',FLA(5),FLA(5),XMIN56,FLA(5),FLA(5),XMAX56,FLA(5),FLA(5),    PROSE370
     &   XMOM56                                                         PROSE371
      PRINT'(''  Central detector angle : '',F8.3,'' degrees '')',      PROSE372
     &   THETCN                                                         PROSE373
C                                                                       PROSE374
C Derived quantities                                                    PROSE375
      X2MN34 = XMIN34*XMIN34                                            PROSE376
      X2MN56 = XMIN56*XMIN56                                            PROSE377
      X2MX34 = XMAX34*XMAX34                                            PROSE378
      X2MX56 = XMAX56*XMAX56                                            PROSE379
      CSCEN  = DCOS(THETCN*PI/180.D0)                                   PROSE380
C Protection from user stupidity:                                       PROSE381
      IF (CSCEN.GT.0.9999D0 .AND. (FLA(3).EQ.FLA(1) .OR.                PROSE382
     &   FLA(5).EQ.FLA(1))) THEN                                        PROSE383
         PRINT'('' THETCN is too close to 0 for this final state.'')'   PROSE384
         STOP                                                           PROSE385
      ENDIF                                                             PROSE386
C In radians                                                            PROSE387
      THCNEV = THETCN*PI/180.D0                                         PROSE388
      RMZ2   = RMZ*RMZ                                                  PROSE389
      RMZRGZ = RMZ*RGZ                                                  PROSE390
      RMZGZ2 = RMZRGZ**2                                                PROSE391
C                                                                       PROSE392
C anti-particles have negative mass in order to do the spinor algebra   PROSE393
      MASS(1) = -XME                                                    PROSE394
      MASS(2) = XME                                                     PROSE395
      MASS(3) = XM34                                                    PROSE396
      MASS(4) = -XM34                                                   PROSE397
      MASS(5) = XM56                                                    PROSE398
      MASS(6) = -XM56                                                   PROSE399
C Next the photon masses and particle type                              PROSE400
      DO 200 I = 7, NOPART                                              PROSE401
         MASS(I) = 0.D0                                                 PROSE402
         CODE(I) = CODE(7)                                              PROSE403
  200 CONTINUE                                                          PROSE404
      DO 201 I = 1, NOPART                                              PROSE405
         DMASS(I) = DABS(MASS(I))                                       PROSE406
         MASS2(I) = MASS(I)*MASS(I)                                     PROSE407
  201 CONTINUE                                                          PROSE408
C                                                                       PROSE409
C THE BEAM MOMENTA                                                      PROSE410
      EBEAM=ECM/2.D0                                                    PROSE411
      DO 202 IN = 1, LBUNCH                                             PROSE412
         QMOM(IN,1,1) = 0.D0                                            PROSE413
         QMOM(IN,2,1) = 0.D0                                            PROSE414
         QMOM(IN,3,1) = -EBEAM                                          PROSE415
         QMOM(IN,4,1) = EBEAM                                           PROSE416
C The electron goes along +z by ALEPH convention, I think. J.H.         PROSE417
         QMOM(IN,1,2) = 0.D0                                            PROSE418
         QMOM(IN,2,2) = 0.D0                                            PROSE419
         QMOM(IN,3,2) = EBEAM                                           PROSE420
         QMOM(IN,4,2) = EBEAM                                           PROSE421
  202 CONTINUE                                                          PROSE422
      DO 203 I = 1, 4                                                   PROSE423
         POUT(I,1) = QMOM(1,I,1)                                        PROSE424
         POUT(I,2) = QMOM(1,I,2)                                        PROSE425
  203 CONTINUE                                                          PROSE426
C                                                                       PROSE427
C fermi symmetry factor and color factors                               PROSE428
      SYMME=1.D0                                                        PROSE429
      IF(FLA(3).EQ.FLA(5)) SYMME=SYMME/2.D0                             PROSE430
      IF(FLA(4).EQ.FLA(6)) SYMME=SYMME/2.D0                             PROSE431
      COLFAC = 1                                                        PROSE432
      IF (ABS(CH334) .GT.0     .AND.ABS(CH334).LT.3) THEN               PROSE433
         PRINT'(/'' The (3,4) pair are assumed to be quarks since the'' PROSE434
     &       ,'' charge ('',F5.2,'') is non-integral.''/                PROSE435
     &       '' ==> Color factor will be applied,'')',FLOAT(CH334)/3.D0 PROSE436
         COLFAC = COLFAC*3                                              PROSE437
         PRINT'('' but no alpha_s correction is applied.''/)'           PROSE438
      ENDIF                                                             PROSE439
      IF (ABS(CH356) .GT.0     .AND.ABS(CH356).LT.3) THEN               PROSE440
         PRINT'(/'' The (5,6) pair are assumed to be quarks since the'' PROSE441
     &       ,'' charge ('',F5.2,'') is non-integral.''/                PROSE442
     &       '' ==> Color factor will be applied,'')',FLOAT(CH356)/3.D0 PROSE443
         PRINT'('' but no alpha_s correction is applied.''/)'           PROSE444
         COLFAC = COLFAC*3                                              PROSE445
      ENDIF                                                             PROSE446
C Watch out for this delicate situation:                                PROSE447
      IF (COLFAC.EQ.9 .AND. FLA(3).EQ.FLA(5))  THEN                     PROSE448
         PRINT'('' You have picked indentical outgoing quarks.  ''/     PROSE449
     &       '' The program cannot handle this (symmetrization too'',   PROSE450
     &       '' complicated).  '')'                                     PROSE451
         STOP                                                           PROSE452
      ENDIF                                                             PROSE453
C                                                                       PROSE454
C neutral lepton advisory                                               PROSE455
      IF (ABS(CH334).EQ.0)  THEN                                        PROSE456
         PRINT'(/'' The (3,4) pair are assumed to be neutrinos since''  PROSE457
     &       ,'' the charge ('',F5.2,'') is zero.''/                    PROSE458
     &       '' ==> Central det. cut will NOT be applied to them,'')',  PROSE459
     &       FLOAT(CH334)/3.D0                                          PROSE460
         PRINT'('' and no. of neutrino families considered = 1.'')'     PROSE461
      ENDIF                                                             PROSE462
      IF (ABS(CH356).EQ.0)  THEN                                        PROSE463
         PRINT'(/'' The (5,6) pair are assumed to be neutrinos since''  PROSE464
     &       ,'' the charge ('',F5.2,'') is zero.''/                    PROSE465
     &       '' ==> Central det. cut will NOT be applied to them,'')',  PROSE466
     &       FLOAT(CH356)/3.D0                                          PROSE467
         PRINT'('' and no. of neutrino families considered = 1.'')'     PROSE468
      ENDIF                                                             PROSE469
C                                                                       PROSE470
C quark plus FSR advisory                                               PROSE471
      IF (((ABS(CH334).GT.0 .AND. ABS(CH334).LT.3) .OR. (ABS(CH356).GT.0PROSE472
     &     .AND.ABS(CH356).LT.3)) .AND. FSRFL) THEN                     PROSE473
         PRINT'(/'' ==> The FSR routine will treat your final state ''  PROSE474
     &      ,''quarks as though they were charge=1 leptons.'')'         PROSE475
      ENDIF                                                             PROSE476
C                                                                       PROSE477
C pi factors in phase space                                             PROSE478
      SECM   = ECM*ECM                                                  PROSE479
      PSFACT =(1.D0/(2.D0*PI))**(3*4-4)                                 PROSE480
C                                                                       PROSE481
      DO 1 K=1,6                                                        PROSE482
        AF(K)=ELEZ/4.D0/DSQRT(SW2*(1.D0-SW2))                           PROSE483
        IF(CHA(K).LT.0.D0) AF(K)=-AF(K)                                 PROSE484
        VF(K)=AF(K)*(1.D0-4.D0*SW2*DABS(CHA(K)/ELEM))                   PROSE485
    1 CONTINUE                                                          PROSE486
C                                                                       PROSE487
      WRITE(*,*) ' FERMION PARAMETERS'                                  PROSE488
      WRITE(*,2) 'LABEL','TYPE','KIN','CHARGE','VECTOR','AXIAL','MASS'  PROSE489
    2 FORMAT(A6,A11,A8,5X,A8,3A13)                                      PROSE490
      DO 4 K=1,6                                                        PROSE491
        IF(K.EQ.1) ANTI='ANTI-'                                         PROSE492
        IF(K.EQ.2) ANTI='     '                                         PROSE493
        IF(K.EQ.3) ANTI='     '                                         PROSE494
        IF(K.EQ.4) ANTI='ANTI-'                                         PROSE495
        IF(K.EQ.5) ANTI='     '                                         PROSE496
        IF(K.EQ.6) ANTI='ANTI-'                                         PROSE497
        WRITE(*,3) K,ANTI,FLA(K),BFAC(K),CHA(K),VF(K),AF(K),MASS(K)     PROSE498
    3   FORMAT(I6,A9,A2,F8.1,4F13.5)                                    PROSE499
    4 CONTINUE                                                          PROSE500
      WRITE(*,*) ' FERMI SYMMETRY FACTOR = ',SYMME                      PROSE501
      WRITE(*,*) ' Color factor = ',COLFAC                              PROSE502
C                                                                       PROSE503
C Initialize phase psace generator constants                            PROSE504
C                                                                       PROSE505
C FBOD2 and FBOD3 can be changed to optimize the annihilation           PROSE506
C phase-space generator (but they are used elsewhere, as well!)         PROSE507
C KBOD2 can be changed to optimize the brems. fwd generator             PROSE508
      FBOD2 = 0.4D0                                                     PROSE509
      FBOD3 = 0.70D0                                                    PROSE510
      KBOD2 = 0.95D0                                                    PROSE511
C GFAC2Z is used by CONV2Z genertaor as no. of widths over which to intePROSE512
      GFAC2Z = 11.D0                                                    PROSE513
C                                                                       PROSE514
      IF (FLA(3).EQ.FLA(5)) THEN                                        PROSE515
         IF (ABS(XMIN34-XMIN56).GT.1.D-3.OR.ABS(XMAX34-XMAX56).GT.1.D-3 PROSE516
     &     .OR. ABS(XMOM34-XMOM56).GT.1.D-3) THEN                       PROSE517
            PRINT'('' *** You have chosen an inconsistent set of''/     PROSE518
     &         ''  parameters.  Check the following: XM34,XM56 ''/      PROSE519
     &         ''  XMIN34,XMIN56, XMAX34,XMAX56 *** '')'                PROSE520
            STOP                                                        PROSE521
         ENDIF                                                          PROSE522
C Maximum pair inv mass**2 to be used in phase-space generators         PROSE523
         Y2MX34 = ((ECM-2.D0*XMIN34)*FBOD2+XMIN34)**2                   PROSE524
C Make value non-physical if the user-supplied cut-off is higher than ouPROSE525
C preferred value.                                                      PROSE526
         IF (Y2MX34.LT.X2MN34)  Y2MX34 = -1.D0                          PROSE527
C Maximum triplet inv mass**2 to be used in phase-space generators      PROSE528
         YTRP34 = DMIN1((ECM-DMASS(5))**2,(XMAX34+DMASS(5))**2,         PROSE529
     &      ((ECM-XMIN34-DMASS(5))*FBOD3+XMIN34+DMASS(5))**2)           PROSE530
         Y2MX56 = Y2MX34                                                PROSE531
         YTRP56 = YTRP34                                                PROSE532
C Q2MN aids in calculating K0MAX for bremstrahlung graphs               PROSE533
         Q2MN   = X2MN34                                                PROSE534
C                                                                       PROSE535
      ELSE                                                              PROSE536
C Maximum pair inv mass**2 to be used in phase-space generators         PROSE537
         Y2MX34 = ((ECM-XMIN34-XMIN56)*FBOD2+XMIN34)**2                 PROSE538
         IF (Y2MX34.LT.X2MN34)  Y2MX34 = -1.D0                          PROSE539
         Y2MX56 = ((ECM-XMIN56-XMIN34)*FBOD2+XMIN56)**2                 PROSE540
         IF (Y2MX56.LE.X2MN56)  Y2MX56 = -1.D0                          PROSE541
C Maximum triplet inv mass**2 to be used in phase-space generators      PROSE542
         YTRP34 = DMIN1((ECM-DMASS(5))**2,(XMAX34+DMASS(5))**2,         PROSE543
     &      ((ECM-XMIN34-DMASS(5))*FBOD3+XMIN34+DMASS(5))**2)           PROSE544
         YTRP56 = DMIN1((ECM-DMASS(3))**2,(XMAX56+DMASS(3))**2,         PROSE545
     &      ((ECM-XMIN56-DMASS(3))*FBOD3+XMIN56+DMASS(3))**2)           PROSE546
C Q2MN aids in calculating K0MAX for bremstrahlung graphs               PROSE547
         IF (FLA(3).EQ.FLA(1)) THEN                                     PROSE548
            Q2MN   = X2MN34                                             PROSE549
         ELSEIF (FLA(5).EQ.FLA(1)) THEN                                 PROSE550
            Q2MN   = X2MN56                                             PROSE551
         ENDIF                                                          PROSE552
      ENDIF                                                             PROSE553
C                                                                       PROSE554
C Check against non-physical phase-space requests                       PROSE555
      IF (Y2MX34.LT.0.D0 .AND. Y2MX56.LT.0.D0) THEN                     PROSE556
         PRINT'('' Non-physical phase-space requested.'',               PROSE557
     &      ''  Please reconsider.'')'                                  PROSE558
         STOP                                                           PROSE559
      ENDIF                                                             PROSE560
C                                                                       PROSE561
C Phase space generator fractions                                       PROSE562
      AUTOWT = IAUTOW.NE.0                                              PROSE563
      IF (.NOT.AUTOWT) THEN                                             PROSE564
         CGENT = 0.D0                                                   PROSE565
         DO 5 I = 1, LGENR                                              PROSE566
            IF (SCGEN(I).LT.1.E-3)  SCGEN(I) = 0.0                      PROSE567
    5       CGENT = CGENT + DBLE(SCGEN(I))                              PROSE568
         DO 6 I = 1, LGENR                                              PROSE569
    6       CGEN(I) = DBLE(SCGEN(I))/CGENT                              PROSE570
C Issue warning if RAMBO is not requested, or if it is under-utilized   PROSE571
         IF (CGEN(LGENR).LT.1.D-5) THEN                                 PROSE572
            PRINT'(''  Choose RAMBO with a greater proportion. '')'     PROSE573
            STOP                                                        PROSE574
         ELSEIF (CGEN(LRAMBO).LT.1.D-2) THEN                            PROSE575
            PRINT'(''  RAMBO chosen with a rather small proportion. ''/ PROSE576
     &          ''  Perhaps reconsider?''/)'                            PROSE577
         ENDIF                                                          PROSE578
         PRINT'(/''  PS generator weights determined from card file.'')'PROSE579
      ELSE                                                              PROSE580
         PRINT'(/''  PS generator weights determined from'',            PROSE581
     &         '' internal look-up table.'')'                           PROSE582
      ENDIF                                                             PROSE583
C                                                                       PROSE584
C Calculation accuracy                                                  PROSE585
      ACCU = 8.D-4                                                      PROSE586
      PRINT'(''  Calculation accuracy factor = '',F8.4)',ACCU           PROSE587
C                                                                       PROSE588
C Initialize variables                                                  PROSE589
      WTMN = 1.D20                                                      PROSE590
      WTMX = 1.D-20                                                     PROSE591
      WT1  = 0.D0                                                       PROSE592
      WT2  = 0.D0                                                       PROSE593
      WT1S = 0.D0                                                       PROSE594
      WT2S = 0.D0                                                       PROSE595
      MEMN = 1.D20                                                      PROSE596
      MEMX = 1.D-20                                                     PROSE597
      ME1  = 0.D0                                                       PROSE598
      ME2  = 0.D0                                                       PROSE599
      DO 11 K = 1, 4                                                    PROSE600
         WTMN4(K) = 1.D20                                               PROSE601
         WTMX4(K) = 1.D-20                                              PROSE602
         WT41(K) = 0.D0                                                 PROSE603
         WT42(K) = 0.D0                                                 PROSE604
         WT41S(K) = 0.D0                                                PROSE605
         WT42S(K) = 0.D0                                                PROSE606
         NEVTGN(K) = 0                                                  PROSE607
         NRJGN(K)  = 0                                                  PROSE608
   11 CONTINUE                                                          PROSE609
C                                                                       PROSE610
      NEVTAC = 0                                                        PROSE611
      NEVTRJ = 0                                                        PROSE612
      NEVTW1 = 0                                                        PROSE613
      DIATOT = 0                                                        PROSE614
      DIACOU = 0                                                        PROSE615
C EGMSOF is the `soft' photon cutoff used to compare x-sections at the ePROSE616
      EGMSOF = 0.1D0                                                    PROSE617
      NEVSOF = 0                                                        PROSE618
      NACSOF = 0                                                        PROSE619
C                                                                       PROSE620
C Phase space generator names                                           PROSE621
C                                                                       PROSE622
      GNNAME(LANNIH) = 'ANNIHILIATION  '                                PROSE623
      GNNAME(LBREMF) = 'BREMS. FORW.   '                                PROSE624
      GNNAME(LBREMB) = 'BREMS. BACKW   '                                PROSE625
      GNNAME(LCONV1) = 'CONVERSION 1   '                                PROSE626
      GNNAME(LCONV2) = 'CONVERSION 2   '                                PROSE627
      GNNAME(LCON1Z) = 'CONVERSION 1Z  '                                PROSE628
      GNNAME(LCON2Z) = 'CONVERSION 2Z  '                                PROSE629
      GNNAME(LMULTI) = 'MULTIPERIPHERAL'                                PROSE630
      GNNAME(LRAMBO) = 'RAMBO          '                                PROSE631
C Set up open channels which are used by the phase space generators.    PROSE632
      CALL BRNCHS                                                       PROSE633
C Build relative importance of each phase-space generator.              PROSE634
      CALL PSWTS(ECM)                                                   PROSE635
C                                                                       PROSE636
      END                                                               PROSE637
      SUBROUTINE PSWTS(CMS)                                             PSWTS  2
C-------------------------------------------------------------------    PSWTS  3
C CMS - center of mass energy                                           PSWTS  4
C Phase space weights                                                   PSWTS  5
C-------------------------------------------------------------------    PSWTS  6
      IMPLICIT NONE                                                     PSWTS  7
      SAVE                                                              PSWTS  8
C Local variables                                                       PSWTS  9
      INTEGER I,IGEN,IONC,ITAKE,IB                                      PSWTS 10
      REAL*8 RAT(100),CUM(0:100),TOT,FRA,RATOT,EPS,CMS,EFRAC            PSWTS 11
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C BRANCH commons                                                        BRANCHC2
      INTEGER LCHMX,PAR1,PAR2,PAR3,PAR4,PAR5,PAR6,NEVTGN,NRJGN,         BRANCHC3
     &   NCHAN,NCHANT,GENRJC,LGENR,JCHAN,CHACOU,CHANLS,                 BRANCHC4
     &   LANNIH,LBREMF,LBREMB,LCONV1,LCONV2,LCON1Z,LCON2Z,LMULTI,LRAMBO BRANCHC5
      PARAMETER (LCHMX = 100, LGENR = 9, LANNIH = 1, LBREMF = 2,        BRANCHC6
     &   LBREMB = 3, LCONV1 = 4, LCONV2 = 5,  LCON1Z = 6, LCON2Z = 7,   BRANCHC7
     &   LMULTI = 8, LRAMBO =9)                                         BRANCHC8
      REAL*8 SMINP,SMAXP,SMX3,SKMIN,SKMAX,BRNCH,FRAC,CGEN               BRANCHC9
      COMMON /BRANCH/ PAR1(LCHMX),PAR2(LCHMX),PAR3(LCHMX),PAR4(LCHMX),  BRANCH10
     &   PAR5(LCHMX),PAR6(LCHMX),NEVTGN(LGENR),NRJGN(LGENR),NCHAN,      BRANCH11
     &   NCHANT,GENRJC(LCHMX),JCHAN(LBUNCH),CHACOU(0:LGENR),            BRANCH12
     &   CHANLS(LGENR),                                                 BRANCH13
     &   SKMIN,SKMAX,SMINP(LCHMX),SMAXP(LCHMX),SMX3(LCHMX),             BRANCH14
     &   BRNCH(0:LCHMX),FRAC(LCHMX),CGEN(LGENR)                         BRANCH15
      CHARACTER*16 GNNAME(LGENR)                                        BRANCH16
      COMMON /GNNAMS/ GNNAME                                            BRANCH17
C ps generator weights for different cms energies                       PSGENWG2
      INTEGER LENERS,LENERD                                             PSGENWG3
      PARAMETER (LENERS = 19, LENERD = LENERS+1)                        PSGENWG4
      REAL*8 ANPSWT,CVPSWT,BRPSWT,MUPSWT,CMSPSW                         PSGENWG5
      COMMON /WTVSEN/ ANPSWT(0:LENERD,2),CVPSWT(0:LENERD,2),            PSGENWG6
     &       BRPSWT(0:LENERD,2),MUPSWT(0:LENERD,2),CMSPSW(0:LENERS)     PSGENWG7
C Some physics constants                                                CONSTPH2
      REAL*8 ALPHA,RMZ,RGZ,SW2,RMZ2,RMZRGZ,RMZGZ2                       CONSTPH3
      COMMON / FISIKX / ALPHA,RMZ,RGZ,SW2,RMZ2,RMZRGZ,RMZGZ2            CONSTPH4
      REAL*8 HEUR(LGENR),FRACS(LGENR)                                   PSWTS 19
      DATA EPS /1.D-5/                                                  PSWTS 20
      DATA IONC /0/                                                     PSWTS 21
C                                                                       PSWTS 22
C Next take care of some cut-dependent constants                        PSWTS 23
      IF (IONC.EQ.0) THEN                                               PSWTS 24
         IF (FLA(3).EQ.FLA(5)) THEN                                     PSWTS 25
            DO 31 I = CHACOU(LANNIH-1), CHACOU(LANNIH)                  PSWTS 26
               RAT(I) = 1.D0/CHANLS(LANNIH)                             PSWTS 27
   31       CONTINUE                                                    PSWTS 28
            RATOT = 1.D0                                                PSWTS 29
         ELSE                                                           PSWTS 30
            RATOT = 0.D0                                                PSWTS 31
            DO 41 I = CHACOU(LANNIH-1)+1, CHACOU(LANNIH)                PSWTS 32
               IF (PAR3(I).EQ.3) THEN                                   PSWTS 33
                  RAT(I) = (X2MN56/X2MN34)**0.3D0                       PSWTS 34
               ELSE                                                     PSWTS 35
                  RAT(I) = 1.D0                                         PSWTS 36
               ENDIF                                                    PSWTS 37
               RATOT = RATOT + RAT(I)                                   PSWTS 38
   41       CONTINUE                                                    PSWTS 39
         ENDIF                                                          PSWTS 40
C Everything is eemumu-like, unless it is eeee:                         PSWTS 41
         IF (FLA(1).EQ.FLA(3) .AND. FLA(1).EQ.FLA(5)) THEN              PSWTS 42
            ITAKE = 2                                                   PSWTS 43
         ELSE                                                           PSWTS 44
            ITAKE = 1                                                   PSWTS 45
         ENDIF                                                          PSWTS 46
C                                                                       PSWTS 47
C The following fractions represent some heuristic rules applied to the PSWTS 48
C ps weights, since not all the ps generators are equally well optimizedPSWTS 49
         IF (AUTOWT) THEN                                               PSWTS 50
            HEUR(LANNIH) = 1.D0                                         PSWTS 51
            HEUR(LBREMF) = 0.5D0                                        PSWTS 52
            HEUR(LBREMB) = 0.5D0                                        PSWTS 53
            HEUR(LCONV1) = 0.95D0                                       PSWTS 54
            HEUR(LCONV2) = 0.10D0                                       PSWTS 55
C The multip. ps generator is somewhat less efficient                   PSWTS 56
            HEUR(LMULTI) = 1.1D0                                        PSWTS 57
            HEUR(LRAMBO) = 0.025D0                                      PSWTS 58
            IF (.NOT.(FLA(1).EQ.FLA(3) .OR. FLA(1).EQ.FLA(5))) THEN     PSWTS 59
C Eliminate brem and multiper.                                          PSWTS 60
               HEUR(LBREMF) = 0.D0                                      PSWTS 61
               HEUR(LBREMB) = 0.D0                                      PSWTS 62
               HEUR(LMULTI) = 0.D0                                      PSWTS 63
            ENDIF                                                       PSWTS 64
         ELSE                                                           PSWTS 65
            DO 45 I = 1, LGENR                                          PSWTS 66
 45            HEUR(I) = 1.D0                                           PSWTS 67
         ENDIF                                                          PSWTS 68
      ENDIF                                                             PSWTS 69
C                                                                       PSWTS 70
C Some behaviour is different above the Z resonance                     PSWTS 71
      IF (AUTOWT) THEN                                                  PSWTS 72
         IF (CMS.GT.RMZ+2.0*RGZ) THEN                                   PSWTS 73
            IF (CMS.GT.2.D0*RMZ-GFAC2Z*RGZ) THEN                        PSWTS 74
               HEUR(LCON2Z) = 0.35D0                                    PSWTS 75
            ELSE                                                        PSWTS 76
               HEUR(LCON2Z) = 0.0D0                                     PSWTS 77
            ENDIF                                                       PSWTS 78
            HEUR(LCONV1) = 0.05D0*INT((CMS - RMZ)/50.D0)                PSWTS 79
            HEUR(LCON1Z) = 1.D0 - HEUR(LCON2Z) - HEUR(LCONV1)           PSWTS 80
C Increase RAMBO since not all peaks have been included, yet            PSWTS 81
            HEUR(LRAMBO) = 0.04D0                                       PSWTS 82
         ELSE                                                           PSWTS 83
            HEUR(LCON1Z) = 0.1D0                                        PSWTS 84
            HEUR(LCON2Z) = 0.D0                                         PSWTS 85
            HEUR(LCONV1) = 0.85D0                                       PSWTS 86
            HEUR(LRAMBO) = 0.025D0                                      PSWTS 87
         ENDIF                                                          PSWTS 88
      ENDIF                                                             PSWTS 89
C                                                                       PSWTS 90
C The bin we are in is determined by the CMS energy                     PSWTS 91
      DO 51 IB = 1, LENERS                                              PSWTS 92
 51      IF (CMSPSW(IB).GT.CMS)  GOTO 52                                PSWTS 93
 52   CONTINUE                                                          PSWTS 94
      IF (AUTOWT) THEN                                                  PSWTS 95
C Use interpolation between the table values                            PSWTS 96
         EFRAC = (CMS - CMSPSW(IB-1))/(CMSPSW(IB) - CMSPSW(IB-1))       PSWTS 97
         CGEN(LANNIH) = HEUR(LANNIH)*(ANPSWT(IB-1,ITAKE) +              PSWTS 98
     &        (ANPSWT(IB,ITAKE) - ANPSWT(IB-1,ITAKE))*EFRAC)            PSWTS 99
         CGEN(LBREMF) = HEUR(LBREMF)*(BRPSWT(IB-1,ITAKE) +              PSWTS100
     &        (BRPSWT(IB,ITAKE) - BRPSWT(IB-1,ITAKE))*EFRAC)            PSWTS101
         CGEN(LBREMB) = HEUR(LBREMB)*(BRPSWT(IB-1,ITAKE) +              PSWTS102
     &        (BRPSWT(IB,ITAKE) - BRPSWT(IB-1,ITAKE))*EFRAC)            PSWTS103
         CGEN(LCONV1) = HEUR(LCONV1)*(CVPSWT(IB-1,ITAKE) +              PSWTS104
     &        (CVPSWT(IB,ITAKE) - CVPSWT(IB-1,ITAKE))*EFRAC)            PSWTS105
         CGEN(LCONV2) = HEUR(LCONV2)*(CVPSWT(IB-1,ITAKE) +              PSWTS106
     &        (CVPSWT(IB,ITAKE) - CVPSWT(IB-1,ITAKE))*EFRAC)            PSWTS107
         CGEN(LCON1Z) = HEUR(LCON1Z)*(CVPSWT(IB-1,ITAKE) +              PSWTS108
     &        (CVPSWT(IB,ITAKE) - CVPSWT(IB-1,ITAKE))*EFRAC)            PSWTS109
         CGEN(LCON2Z) = HEUR(LCON2Z)*(CVPSWT(IB-1,ITAKE) +              PSWTS110
     &        (CVPSWT(IB,ITAKE) - CVPSWT(IB-1,ITAKE))*EFRAC)            PSWTS111
         CGEN(LMULTI) = HEUR(LMULTI)*(MUPSWT(IB-1,ITAKE) +              PSWTS112
     &        (MUPSWT(IB,ITAKE) - MUPSWT(IB-1,ITAKE))*EFRAC)            PSWTS113
      ENDIF                                                             PSWTS114
      TOT = 0.D0                                                        PSWTS115
      CUM(0) = 0.D0                                                     PSWTS116
      BRNCH(0) = 0.D0                                                   PSWTS117
      DO 81 I = 1, CHACOU(LGENR)                                        PSWTS118
         IGEN = GENRJC(I)                                               PSWTS119
         IF (IGEN.EQ.LRAMBO .AND. AUTOWT)                               PSWTS120
     &        CGEN(LRAMBO) = HEUR(LRAMBO)*CUM(I-1)                      PSWTS121
         IF (I.GT.CHACOU(LANNIH-1) .AND. I.LE.CHACOU(LANNIH)) THEN      PSWTS122
            FRA = RAT(I)/RATOT*CGEN(LANNIH)                             PSWTS123
         ELSE                                                           PSWTS124
            FRA = CGEN(IGEN)/CHANLS(IGEN)                               PSWTS125
         ENDIF                                                          PSWTS126
         CUM(I) = CUM(I-1) + FRA                                        PSWTS127
         IF (CUM(I)-CUM(I-1).LT.EPS)  CUM(I) = CUM(I-1)                 PSWTS128
   81 CONTINUE                                                          PSWTS129
C                                                                       PSWTS130
C Build branching fraction table                                        PSWTS131
      TOT = CUM(NCHANT)                                                 PSWTS132
      DO 85 I = 1, LGENR                                                PSWTS133
 85      FRACS(I) = 0.D0                                                PSWTS134
      DO 91 I = 1, NCHANT                                               PSWTS135
         BRNCH(I) = CUM(I)/TOT                                          PSWTS136
         FRAC(I)  = BRNCH(I)-BRNCH(I-1)                                 PSWTS137
         IGEN = GENRJC(I)                                               PSWTS138
         FRACS(IGEN) = FRACS(IGEN) + FRAC(I)                            PSWTS139
   91 CONTINUE                                                          PSWTS140
C                                                                       PSWTS141
C Print-out 1st time through or during major debugging                  PSWTS142
      IF (IONC.EQ.0 .OR.                                                PSWTS143
     &  (NEVTIN.LE.NEVPRI.AND.NBUNCH.EQ.1.AND.DEBGLV.GE.2)) THEN        PSWTS144
      PRINT'(/''  sub-generator  Relative importance     no. channels''/PSWTS145
     &     ''  -------------+-----------------------+------------- ''/  PSWTS146
     &    9(A17,F8.4,20X,I3/))',                                        PSWTS147
     &    (GNNAME(I),FRACS(I),CHANLS(I),I=1,LGENR)                      PSWTS148
C                                                                       PSWTS149
         IONC = 1                                                       PSWTS150
      ENDIF                                                             PSWTS151
C                                                                       PSWTS152
      END                                                               PSWTS153
      FUNCTION PTOT3(P1)                                                PTOT3  2
      IMPLICIT NONE                                                     PTOT3  3
      REAL*8 PTOT3,P1(3)                                                PTOT3  4
      PTOT3 = DSQRT(P1(1)**2 + P1(2)**2+ P1(3)**2)                      PTOT3  5
      END                                                               PTOT3  6
      DOUBLE PRECISION FUNCTION RN(DUMMY)                               RN     2
C------------------------------------------------------------------     RN     3
C!  Random number generator in double precision                         RN     4
C                                                                       RN     5
C   P. Janot -- 04 nov 1988                                             RN     6
C                                                                       RN     7
C Corrections:                                                          RN     8
C     o P. Janot 21 Oct 1994 : This does not work on AXP.               RN     9
C------------------------------------------------------------------     RN    10
      IMPLICIT REAL*8(A-H,M-Z)                                          RN    11
      REAL*4 RNDM,Z(2)                                                  RN    12
      EQUIVALENCE(Z(1),ZZ)                                              RN    13
C     Z(1)  = RNDM(Z(1))                                                RN    14
C     Z(2)  = RNDM(Z(2))                                                RN    15
C     RN = ZZ                                                           RN    16
      RN = DBLE(RNDM(Z(1)))                                             RN    17
      RETURN                                                            RN    18
      END                                                               RN    19
      SUBROUTINE RAMBO(N,ET,XM,P,WT)                                    RAMBO  2
C------------------------------------------------------                 RAMBO  3
C                                                                       RAMBO  4
C                       RAMBO                                           RAMBO  5
C                                                                       RAMBO  6
C    RA(NDOM)  M(OMENTA)  BO(OSTER)                                     RAMBO  7
C                                                                       RAMBO  8
C    A DEMOCRATIC MULTI-PARTICLE PHASE SPACE GENERATOR                  RAMBO  9
C    AUTHORS:  S.D. ELLIS,  R. KLEISS,  W.J. STIRLING                   RAMBO 10
C    THIS IS VERSION 1.0 -  WRITTEN BY R. KLEISS                        RAMBO 11
C                                                                       RAMBO 12
C    N  = NUMBER OF PARTICLES (>1, IN THIS VERSION <101)                RAMBO 13
C    ET = TOTAL CENTRE-OF-MASS ENERGY                                   RAMBO 14
C    XM = PARTICLE MASSES ( DIM=N )                                     RAMBO 15
C    P  = PARTICLE MOMENTA ( DIM=(4,N) )                                RAMBO 16
C    WT = WEIGHT OF THE EVENT                                           RAMBO 17
C                                                                       RAMBO 18
C------------------------------------------------------                 RAMBO 19
      IMPLICIT REAL*8(A-H,O-Z)                                          RAMBO 20
      SAVE                                                              RAMBO 21
      DIMENSION XM(N),P(4,N),Q(4,100),Z(100),R(4),                      RAMBO 22
     .   B(3),P2(100),XM2(100),E(100),V(100),IWARN(5)                   RAMBO 23
      DATA ACC/1.D-14/,ITMAX/6/,IBEGIN/0/,IWARN/5*0/                    RAMBO 24
C                                                                       RAMBO 25
C INITIALIZATION STEP: FACTORIALS FOR THE PHASE SPACE WEIGHT            RAMBO 26
      IF(IBEGIN.NE.0) GOTO 103                                          RAMBO 27
      IBEGIN=1                                                          RAMBO 28
      TWOPI=8.*DATAN(1.D0)                                              RAMBO 29
      PO2LOG=DLOG(TWOPI/4.)                                             RAMBO 30
      Z(2)=PO2LOG                                                       RAMBO 31
      DO 101 K=3,100                                                    RAMBO 32
  101 Z(K)=Z(K-1)+PO2LOG-2.*DLOG(DFLOAT(K-2))                           RAMBO 33
      DO 102 K=3,100                                                    RAMBO 34
  102 Z(K)=(Z(K)-DLOG(DFLOAT(K-1)))                                     RAMBO 35
C                                                                       RAMBO 36
C CHECK ON THE NUMBER OF PARTICLES                                      RAMBO 37
  103 IF(N.GT.1.AND.N.LT.101) GOTO 104                                  RAMBO 38
      PRINT 1001,N                                                      RAMBO 39
      STOP                                                              RAMBO 40
C                                                                       RAMBO 41
C CHECK WHETHER TOTAL ENERGY IS SUFFICIENT; COUNT NONZERO MASSES        RAMBO 42
  104 XMT=0.                                                            RAMBO 43
      NM=0                                                              RAMBO 44
      DO 105 I=1,N                                                      RAMBO 45
      IF(XM(I).NE.0.D0) NM=NM+1                                         RAMBO 46
  105 XMT=XMT+DABS(XM(I))                                               RAMBO 47
      IF(XMT.LE.ET) GOTO 201                                            RAMBO 48
      PRINT 1002,XMT,ET                                                 RAMBO 49
      STOP                                                              RAMBO 50
C                                                                       RAMBO 51
C THE PARAMETER VALUES ARE NOW ACCEPTED                                 RAMBO 52
C                                                                       RAMBO 53
C GENERATE N MASSLESS MOMENTA IN INFINITE PHASE SPACE                   RAMBO 54
  201 DO 202 I=1,N                                                      RAMBO 55
      C=2.*RN(1)-1.                                                     RAMBO 56
      S=DSQRT(1.-C*C)                                                   RAMBO 57
      F=TWOPI*RN(2)                                                     RAMBO 58
      Q(4,I)=-DLOG(RN(3)*RN(4))                                         RAMBO 59
      Q(3,I)=Q(4,I)*C                                                   RAMBO 60
      Q(2,I)=Q(4,I)*S*DCOS(F)                                           RAMBO 61
  202 Q(1,I)=Q(4,I)*S*DSIN(F)                                           RAMBO 62
C                                                                       RAMBO 63
C CALCULATE THE PARAMETERS OF THE CONFORMAL TRANSFORMATION              RAMBO 64
      DO 203 I=1,4                                                      RAMBO 65
  203 R(I)=0.                                                           RAMBO 66
      DO 204 I=1,N                                                      RAMBO 67
      DO 204 K=1,4                                                      RAMBO 68
  204 R(K)=R(K)+Q(K,I)                                                  RAMBO 69
      RMAS=DSQRT(R(4)**2-R(3)**2-R(2)**2-R(1)**2)                       RAMBO 70
      DO 205 K=1,3                                                      RAMBO 71
  205 B(K)=-R(K)/RMAS                                                   RAMBO 72
      G=R(4)/RMAS                                                       RAMBO 73
      A=1./(1.+G)                                                       RAMBO 74
      X=ET/RMAS                                                         RAMBO 75
C                                                                       RAMBO 76
C TRANSFORM THE Q'S CONFORMALLY INTO THE P'S                            RAMBO 77
      DO 207 I=1,N                                                      RAMBO 78
      BQ=B(1)*Q(1,I)+B(2)*Q(2,I)+B(3)*Q(3,I)                            RAMBO 79
      DO 206 K=1,3                                                      RAMBO 80
  206 P(K,I)=X*(Q(K,I)+B(K)*(Q(4,I)+A*BQ))                              RAMBO 81
  207 P(4,I)=X*(G*Q(4,I)+BQ)                                            RAMBO 82
C                                                                       RAMBO 83
C CALCULATE WEIGHT AND POSSIBLE WARNINGS                                RAMBO 84
      WT=PO2LOG                                                         RAMBO 85
      IF(N.NE.2) WT=(2.*N-4.)*DLOG(ET)+Z(N)                             RAMBO 86
      IF(WT.GE.-180.D0) GOTO 208                                        RAMBO 87
      IF(IWARN(1).LE.5) PRINT 1004,WT                                   RAMBO 88
      IWARN(1)=IWARN(1)+1                                               RAMBO 89
  208 IF(WT.LE. 174.D0) GOTO 209                                        RAMBO 90
      IF(IWARN(2).LE.5) PRINT 1005,WT                                   RAMBO 91
      IWARN(2)=IWARN(2)+1                                               RAMBO 92
C                                                                       RAMBO 93
C RETURN FOR WEIGHTED MASSLESS MOMENTA                                  RAMBO 94
  209 IF(NM.NE.0) GOTO 210                                              RAMBO 95
      WT=DEXP(WT)                                                       RAMBO 96
      RETURN                                                            RAMBO 97
C                                                                       RAMBO 98
C MASSIVE PARTICLES: RESCALE THE MOMENTA BY A FACTOR X                  RAMBO 99
  210 XMAX=DSQRT(1.-(XMT/ET)**2)                                        RAMBO100
      DO 301 I=1,N                                                      RAMBO101
      XM2(I)=XM(I)**2                                                   RAMBO102
  301 P2(I)=P(4,I)**2                                                   RAMBO103
      ITER=0                                                            RAMBO104
      X=XMAX                                                            RAMBO105
      ACCU=ET*ACC                                                       RAMBO106
  302 F0=-ET                                                            RAMBO107
      G0=0.                                                             RAMBO108
      X2=X*X                                                            RAMBO109
      DO 303 I=1,N                                                      RAMBO110
      E(I)=DSQRT(XM2(I)+X2*P2(I))                                       RAMBO111
      F0=F0+E(I)                                                        RAMBO112
  303 G0=G0+P2(I)/E(I)                                                  RAMBO113
      IF(DABS(F0).LE.ACCU) GOTO 305                                     RAMBO114
      ITER=ITER+1                                                       RAMBO115
      IF(ITER.LE.ITMAX) GOTO 304                                        RAMBO116
      PRINT 1006,ITMAX                                                  RAMBO117
      GOTO 305                                                          RAMBO118
  304 X=X-F0/(X*G0)                                                     RAMBO119
      GOTO 302                                                          RAMBO120
  305 DO 307 I=1,N                                                      RAMBO121
      V(I)=X*P(4,I)                                                     RAMBO122
      DO 306 K=1,3                                                      RAMBO123
  306 P(K,I)=X*P(K,I)                                                   RAMBO124
  307 P(4,I)=E(I)                                                       RAMBO125
C                                                                       RAMBO126
C CALCULATE THE MASS-EFFECT WEIGHT FACTOR                               RAMBO127
      WT2=1.D0                                                          RAMBO128
      WT3=0.D0                                                          RAMBO129
      DO 308 I=1,N                                                      RAMBO130
      WT2=WT2*V(I)/E(I)                                                 RAMBO131
  308 WT3=WT3+V(I)**2/E(I)                                              RAMBO132
      WTM=(2.D0*N-3.D0)*DLOG(X)+DLOG(WT2/WT3*ET)                        RAMBO133
C                                                                       RAMBO134
C RETURN FOR  WEIGHTED MASSIVE MOMENTA                                  RAMBO135
      WT=WT+WTM                                                         RAMBO136
      IF(WT.GE.-180.D0) GOTO 309                                        RAMBO137
      IF(IWARN(3).LE.5) PRINT 1004,WT                                   RAMBO138
      IWARN(3)=IWARN(3)+1                                               RAMBO139
  309 IF(WT.LE. 174.D0) GOTO 310                                        RAMBO140
      IF(IWARN(4).LE.5) PRINT 1005,WT                                   RAMBO141
      IWARN(4)=IWARN(4)+1                                               RAMBO142
  310 WT=DEXP(WT)                                                       RAMBO143
      RETURN                                                            RAMBO144
C                                                                       RAMBO145
 1001 FORMAT(' RAMBO FAILS: # OF PARTICLES =',I5,' IS NOT ALLOWED')     RAMBO146
 1002 FORMAT(' RAMBO FAILS: TOTAL MASS =',D15.6,' IS NOT',              RAMBO147
     . ' SMALLER THAN TOTAL ENERGY =',D15.6)                            RAMBO148
 1004 FORMAT(' RAMBO WARNS: WEIGHT = EXP(',F20.9,') MAY UNDERFLOW')     RAMBO149
 1005 FORMAT(' RAMBO WARNS: WEIGHT = EXP(',F20.9,') MAY  OVERFLOW')     RAMBO150
 1006 FORMAT(' RAMBO WARNS:',I3,' ITERATIONS DID NOT GIVE THE',         RAMBO151
     . ' DESIRED ACCURACY =',D15.6)                                     RAMBO152
      END                                                               RAMBO153
      FUNCTION RAMDEN(PEV,ICALC)                                        RAMDEN 2
C-------------------------------------------------------------------    RAMDEN 3
C Calculate RAMBO's phase-space density                                 RAMDEN 4
C INPUTS :                                                              RAMDEN 5
C   PEV(4,6) : 4-vectors of fermions                                    RAMDEN 6
C OUTPUTS :                                                             RAMDEN 7
C  ICALC = 0 if phase space density not actually calculated from scratchRAMDEN 8
C-------------------------------------------------------------------    RAMDEN 9
      IMPLICIT NONE                                                     RAMDEN10
      SAVE                                                              RAMDEN11
C Local variables                                                       RAMDEN12
      INTEGER ICALC,IONC,NOUT,I,IP                                      RAMDEN13
C No. of outgoing particles                                             RAMDEN14
      PARAMETER (NOUT = 4)                                              RAMDEN15
      REAL*8 RAMDEN,PIBY2,GAM(NOUT),PSVOL,DENS,P(3:6),PTOT,KSI,WLOG,    RAMDEN16
     &   X2,X3,VOLCON,PEV(4,6),ECMEV                                    RAMDEN17
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C                                                                       RAMDEN21
      DATA IONC /0/                                                     RAMDEN22
      DATA GAM /1.D0, 1.D0, 2.D0, 6.D0/                                 RAMDEN23
C                                                                       RAMDEN24
      IF (IONC.EQ.0) THEN                                               RAMDEN25
         PIBY2=2.D0*DATAN(1.D0)                                         RAMDEN26
         VOLCON = PIBY2**(NOUT-1)/GAM(NOUT)/GAM(NOUT-1)                 RAMDEN27
         IONC = 1                                                       RAMDEN28
      ENDIF                                                             RAMDEN29
      ECMEV = PEV(4,1)+PEV(4,2)                                         RAMDEN30
      PSVOL = VOLCON*ECMEV**(2*NOUT-4)                                  RAMDEN31
      DENS = 1.D0/PSVOL                                                 RAMDEN32
C                                                                       RAMDEN33
C Now calculate ksi :                                                   RAMDEN34
      PTOT = 0.D0                                                       RAMDEN35
      DO 11 I = 1, NOUT                                                 RAMDEN36
         IP = I+2                                                       RAMDEN37
         P(IP) = DSQRT(PEV(4,IP)**2-MASS2(IP))                          RAMDEN38
         PTOT = PTOT + P(IP)                                            RAMDEN39
   11 CONTINUE                                                          RAMDEN40
C                                                                       RAMDEN41
      KSI = PTOT/ECMEV                                                  RAMDEN42
C                                                                       RAMDEN43
C Test if this ksi is close enough to 1.0 for our desired accuracy      RAMDEN44
      IF (DABS(KSI-1.D0).LT.ACCU) THEN                                  RAMDEN45
         RAMDEN = DENS                                                  RAMDEN46
         ICALC = 0                                                      RAMDEN47
         GOTO 999                                                       RAMDEN48
      ELSE                                                              RAMDEN49
         X2 = 1.D0                                                      RAMDEN50
         X3 = 0.D0                                                      RAMDEN51
         DO 21 I = 1, NOUT                                              RAMDEN52
            IP = I+2                                                    RAMDEN53
            X2 = X2*P(IP)/PEV(4,IP)                                     RAMDEN54
            X3 = X3 + (PEV(4,IP)**2-MASS2(IP))/PEV(4,IP)                RAMDEN55
   21    CONTINUE                                                       RAMDEN56
C                                                                       RAMDEN57
         WLOG = (2.D0*NOUT-3.D0)*DLOG(KSI)+DLOG(X2*ECMEV/X3)            RAMDEN58
         RAMDEN = 1.D0/(PSVOL*EXP(WLOG))                                RAMDEN59
         ICALC = 1                                                      RAMDEN60
      ENDIF                                                             RAMDEN61
C                                                                       RAMDEN62
  999 CONTINUE                                                          RAMDEN63
      END                                                               RAMDEN64
      SUBROUTINE RANEVT(DENSR,IACC)                                     RANEVT 2
C------------------------------------------------------                 RANEVT 3
C Setup call to RAMBO for 4 outgoing particles.                         RANEVT 4
C OUTPUT:                                                               RANEVT 5
C DENS - phase space density of event                                   RANEVT 6
C IACC = 0 if event not accepted                                        RANEVT 7
C------------------------------------------------------                 RANEVT 8
      IMPLICIT NONE                                                     RANEVT 9
      SAVE                                                              RANEVT10
C Local variables                                                       RANEVT11
      REAL*8 DENSR,WT,XM(4)                                             RANEVT12
      INTEGER NOUT,IACC,I,IONC                                          RANEVT13
      PARAMETER (NOUT = 4)                                              RANEVT14
C                                                                       RANEVT15
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C setup commons                                                         SETUPCO2
      REAL*8 ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                          SETUPCO3
      COMMON /SETUP/ ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                  SETUPCO4
C Phase space weights                                                   PHASSPA2
      REAL*8 PSWT,PSWTEV,DENS                                           PHASSPA3
      COMMON /PHASEP/ PSWTEV,PSWT(LBUNCH),DENS(100,LBUNCH)              PHASSPA4
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C                                                                       RANEVT22
      DATA IONC /0/                                                     RANEVT23
C                                                                       RANEVT24
C Initialize XM array with its masses.                                  RANEVT25
      IF (IONC.EQ.0) THEN                                               RANEVT26
         DO 1 I = 1, 4                                                  RANEVT27
    1       XM(I) = DMASS(I+2)                                          RANEVT28
         IONC = 1                                                       RANEVT29
      ENDIF                                                             RANEVT30
      IACC = 0                                                          RANEVT31
C                                                                       RANEVT32
C Get a phase space event                                               RANEVT33
      CALL RAMBO(NOUT,ECMISR,XM,PEVT(1,3),WT)                           RANEVT34
C                                                                       RANEVT35
      DENSR = 1.D0/WT                                                   RANEVT36
      IACC = 1                                                          RANEVT37
  999 CONTINUE                                                          RANEVT38
      END                                                               RANEVT39
      SUBROUTINE SCTOVE                                                 SCTOVE 2
C-----------------------------------------------------------------------SCTOVE 3
C Transfer of scalar variables to vector commons                        SCTOVE 4
C-----------------------------------------------------------------------SCTOVE 5
      IMPLICIT NONE                                                     SCTOVE 6
      SAVE                                                              SCTOVE 7
C Local variables                                                       SCTOVE 8
      INTEGER IN,N,M,IP,PART,PART1,PART2                                SCTOVE 9
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C setup commons                                                         SETUPCO2
      REAL*8 ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                          SETUPCO3
      COMMON /SETUP/ ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                  SETUPCO4
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C Phase space weights                                                   PHASSPA2
      REAL*8 PSWT,PSWTEV,DENS                                           PHASSPA3
      COMMON /PHASEP/ PSWTEV,PSWT(LBUNCH),DENS(100,LBUNCH)              PHASSPA4
C 4-vector products                                                     VEC4PRO2
      COMPLEX*16 S(LBUNCH,6,6),T(LBUNCH,6,6)                            VEC4PRO3
      REAL*8 ETA(LBUNCH,6),RMU(LBUNCH,6)                                VEC4PRO4
      COMMON / SPPROD / S,T,ETA,RMU                                     VEC4PRO5
C                                                                       SCTOVE18
      IN = NEVTIN + 1                                                   SCTOVE19
      PSWT(IN) = PSWTEV                                                 SCTOVE20
C The flux factor                                                       SCTOVE21
      WFLUX(IN) = 1.D0/(2.D0*SISR)                                      SCTOVE22
C                                                                       SCTOVE23
C CALCULATE THE SPINOR PRODUCTS, ETA AND RMU                            SCTOVE24
C THE 4 MOMENTA ARE IN A BLOCK QMOM(IN,4,6)                             SCTOVE25
C THE FIRST INDEX IS EQUAL TO THE LABEL OF THE INNER LOOP EVENT NUMBER  SCTOVE26
C THE MIDDLE INDEX:1,2,3,4=X,Y,Z,ENERGY. The particle masses are in MASSSCTOVE27
C THE LAST INDEX IS EQUAL TO THE LABEL OF THE MOMENTUM                  SCTOVE28
C *** WARNING *** Watch out for use of CMPLX and CONJG here (CRAY usage)SCTOVE29
C                                                                       SCTOVE30
      DO 1 N=1,6                                                        SCTOVE31
         ETA(IN,N)=DSQRT(2.D0*(PEVT(4,N)-PEVT(1,N)))                    SCTOVE32
         RMU(IN,N)=MASS(N)/ETA(IN,N)                                    SCTOVE33
    1 CONTINUE                                                          SCTOVE34
      DO 3 M=1,6                                                        SCTOVE35
        DO 2 N=1,M                                                      SCTOVE36
             S(IN,M,N)= CMPLX(PEVT(2,M),PEVT(3,M))*ETA(IN,N)/           SCTOVE37
     &          ETA(IN,M)                                               SCTOVE38
     .          - CMPLX(PEVT(2,N),PEVT(3,N))*ETA(IN,M)/ETA(IN,N)        SCTOVE39
             S(IN,N,M)=-S(IN,M,N)                                       SCTOVE40
             T(IN,M,N)=-CONJG(S(IN,M,N))                                SCTOVE41
             T(IN,N,M)=-T(IN,M,N)                                       SCTOVE42
    2   CONTINUE                                                        SCTOVE43
    3 CONTINUE                                                          SCTOVE44
C                                                                       SCTOVE45
C Transferral of lab-frame momenta and dot product commons              SCTOVE46
      DO 11 PART = 3, NOPART                                            SCTOVE47
         DO 12 IP = 1, 4                                                SCTOVE48
            QMOM(IN,IP,PART) =  POUT(IP,PART)                           SCTOVE49
 12      CONTINUE                                                       SCTOVE50
 11   CONTINUE                                                          SCTOVE51
      DO 13 PART1 = 1, 6                                                SCTOVE52
         DO 14 PART2 = 1, 6                                             SCTOVE53
            D(IN,PART2,PART1) = DEVT(PART2,PART1)                       SCTOVE54
   14    CONTINUE                                                       SCTOVE55
   13 CONTINUE                                                          SCTOVE56
C                                                                       SCTOVE57
      IF (NBUNCH.EQ.1.AND.DEBGLV.GE.2.AND.IN.LE.NEVPRI) THEN            SCTOVE58
            PRINT'('' Event '',I3)',IN                                  SCTOVE59
            WRITE(*,*) ' S(+) MATRIX:'                                  SCTOVE60
            WRITE(*,91) ((S(IN,M,N),N=1,6),M=1,6)                       SCTOVE61
            WRITE(*,*) ' S(-) MATRIX:'                                  SCTOVE62
            WRITE(*,91) ((T(IN,M,N),N=1,6),M=1,6)                       SCTOVE63
   91       FORMAT(6(' ',2F6.2))                                        SCTOVE64
            WRITE(*,*) ' ETA ARRAY:'                                    SCTOVE65
            WRITE(*,92) (ETA(IN,N),N=1,6)                               SCTOVE66
            WRITE(*,*) ' RMU ARRAY:'                                    SCTOVE67
            WRITE(*,92) (RMU(IN,N),N=1,6)                               SCTOVE68
   92       FORMAT(6F13.5)                                              SCTOVE69
            WRITE(*,*) ' D MATRIX:'                                     SCTOVE70
            WRITE(*,92) ((D(IN,M,N),M=1,6),N=1,6)                       SCTOVE71
      ENDIF                                                             SCTOVE72
C                                                                       SCTOVE73
      END                                                               SCTOVE74
      SUBROUTINE STATS                                                  STATS  2
C-----------------------------------------------------------------------STATS  3
C Record event weight stats including min, max, variance                STATS  4
C-----------------------------------------------------------------------STATS  5
      IMPLICIT NONE                                                     STATS  6
      SAVE                                                              STATS  7
C local variables                                                       STATS  8
      INTEGER IN,I,IO,ID,ICALC,PART,IP                                  STATS  9
      REAL*8 RAMDEN,PEV(4,6),ETEMP                                      STATS 10
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C BRANCH commons                                                        BRANCHC2
      INTEGER LCHMX,PAR1,PAR2,PAR3,PAR4,PAR5,PAR6,NEVTGN,NRJGN,         BRANCHC3
     &   NCHAN,NCHANT,GENRJC,LGENR,JCHAN,CHACOU,CHANLS,                 BRANCHC4
     &   LANNIH,LBREMF,LBREMB,LCONV1,LCONV2,LCON1Z,LCON2Z,LMULTI,LRAMBO BRANCHC5
      PARAMETER (LCHMX = 100, LGENR = 9, LANNIH = 1, LBREMF = 2,        BRANCHC6
     &   LBREMB = 3, LCONV1 = 4, LCONV2 = 5,  LCON1Z = 6, LCON2Z = 7,   BRANCHC7
     &   LMULTI = 8, LRAMBO =9)                                         BRANCHC8
      REAL*8 SMINP,SMAXP,SMX3,SKMIN,SKMAX,BRNCH,FRAC,CGEN               BRANCHC9
      COMMON /BRANCH/ PAR1(LCHMX),PAR2(LCHMX),PAR3(LCHMX),PAR4(LCHMX),  BRANCH10
     &   PAR5(LCHMX),PAR6(LCHMX),NEVTGN(LGENR),NRJGN(LGENR),NCHAN,      BRANCH11
     &   NCHANT,GENRJC(LCHMX),JCHAN(LBUNCH),CHACOU(0:LGENR),            BRANCH12
     &   CHANLS(LGENR),                                                 BRANCH13
     &   SKMIN,SKMAX,SMINP(LCHMX),SMAXP(LCHMX),SMX3(LCHMX),             BRANCH14
     &   BRNCH(0:LCHMX),FRAC(LCHMX),CGEN(LGENR)                         BRANCH15
      CHARACTER*16 GNNAME(LGENR)                                        BRANCH16
      COMMON /GNNAMS/ GNNAME                                            BRANCH17
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
C Phase space weights                                                   PHASSPA2
      REAL*8 PSWT,PSWTEV,DENS                                           PHASSPA3
      COMMON /PHASEP/ PSWTEV,PSWT(LBUNCH),DENS(100,LBUNCH)              PHASSPA4
C                                                                       STATS 18
      DO 11 IN = 1, NEVTIN                                              STATS 19
C  First treat the matrix element**2 w/o the phase space weight         STATS 20
C  The extremes of the matrix element:                                  STATS 21
         IF (CROSS(IN).LT.MEMN .OR. CROSS(IN).GT.MEMX) THEN             STATS 22
            ETEMP = 0.D0                                                STATS 23
            DO 12 PART = 3, 6                                           STATS 24
               DO 13 IP = 1, 4                                          STATS 25
                  PEV(IP,PART) = QMOM(IN,IP,PART)                       STATS 26
   13          CONTINUE                                                 STATS 27
               ETEMP = ETEMP + PEV(4,PART)                              STATS 28
   12       CONTINUE                                                    STATS 29
            PEV(4,1) = ETEMP/2.D0                                       STATS 30
            PEV(4,2) = ETEMP/2.D0                                       STATS 31
         ENDIF                                                          STATS 32
         IF (CROSS(IN).LT.MEMN) THEN                                    STATS 33
            MEMN = CROSS(IN)                                            STATS 34
            PSWMN = WFLUX(IN)/RAMDEN(PEV,ICALC)                         STATS 35
         ELSEIF (CROSS(IN).GT.MEMX) THEN                                STATS 36
            MEMX = CROSS(IN)                                            STATS 37
            PSWMX = WFLUX(IN)/RAMDEN(PEV,ICALC)                         STATS 38
         ENDIF                                                          STATS 39
C  The variance of the matrix element, but only when RAMBO has been callSTATS 40
         IF (GENRJC(JCHAN(IN)).EQ.LGENR) THEN                           STATS 41
            ME1 = ME1 + CROSS(IN)                                       STATS 42
            ME2 = ME2 + CROSS(IN)*CROSS(IN)                             STATS 43
         ENDIF                                                          STATS 44
  11  CONTINUE                                                          STATS 45
C                                                                       STATS 46
      IF (NBUNCH.EQ.1.AND.DEBGLV.GE.1)  THEN                            STATS 47
         DO 115 IN = 1, NEVPRI                                          STATS 48
 115        PRINT'(/'' Event '',I3,'' ME**2:'',G16.7)',IN,CROSS(IN)     STATS 49
      ENDIF                                                             STATS 50
C Now treat the M.E.**2 * p.s.(wt) statistics                           STATS 51
      DO 15 IN = 1, NEVTIN                                              STATS 52
  15     CROSS(IN) = CROSS(IN)*PSWT(IN)*WFLUX(IN)*ISRWT(IN)             STATS 53
C                                                                       STATS 54
      DO 16 IN = 1, NEVTIN                                              STATS 55
         I = 0                                                          STATS 56
         IF (CROSS(IN).GT.WTMX) THEN                                    STATS 57
            WTMX = CROSS(IN)                                            STATS 58
            I = 1                                                       STATS 59
         ELSEIF (CROSS(IN).LT.WTMN) THEN                                STATS 60
            WTMN = CROSS(IN)                                            STATS 61
            I = 2                                                       STATS 62
         ENDIF                                                          STATS 63
         IF (I.GT.0) THEN                                               STATS 64
C Record kinematics of extreme events                                   STATS 65
C Record momentum                                                       STATS 66
            DO 17 IO = 3, NOPART                                        STATS 67
               DO 18 ID = 1, 4                                          STATS 68
                  QSAV(ID,IO,I) = QMOM(IN,ID,IO)                        STATS 69
 18            CONTINUE                                                 STATS 70
 17         CONTINUE                                                    STATS 71
C                                                                       STATS 72
C Record weights                                                        STATS 73
            DO 14 ID = 1, 4                                             STATS 74
               WTSAV(ID,I) = CROSS4(IN,ID)*PSWT(IN)*WFLUX(IN)*ISRWT(IN) STATS 75
 14         CONTINUE                                                    STATS 76
            PSWSAV(I) = PSWT(IN)                                        STATS 77
C Record separate phase space densities                                 STATS 78
            DO 19 ID = 1, NCHANT                                        STATS 79
 19            DENSSV(ID,I) = DENS(ID,IN)                               STATS 80
         ENDIF                                                          STATS 81
C                                                                       STATS 82
         WT1 = WT1 + CROSS(IN)                                          STATS 83
         WT2 = WT2 + CROSS(IN)*CROSS(IN)                                STATS 84
  16  CONTINUE                                                          STATS 85
C                                                                       STATS 86
      DO 21 I = 1, 4                                                    STATS 87
         DO 22 IN = 1, NEVTIN                                           STATS 88
            CROSS4(IN,I) = CROSS4(IN,I)*PSWT(IN)*WFLUX(IN)*ISRWT(IN)    STATS 89
            WTMN4(I) = DMIN1(WTMN4(I),CROSS4(IN,I))                     STATS 90
            WTMX4(I) = DMAX1(WTMX4(I),CROSS4(IN,I))                     STATS 91
            WT41(I) = WT41(I) + CROSS4(IN,I)                            STATS 92
            IF ( cross4(in,i) .GT. 1D-23 )                              STATS 93
     .      WT42(I) = WT42(I) + CROSS4(IN,I)**2                         STATS 94
 22      CONTINUE                                                       STATS 95
 21   CONTINUE                                                          STATS 96
C                                                                       STATS 97
C Now record weight sums if there was ISR and it is soft                STATS 98
      IF (ISRFL) THEN                                                   STATS 99
         DO 31 IN = 1, NEVTIN                                           STATS100
            IF (QMOM(IN,4,LGMISR) .LT. EGMSOF) THEN                     STATS101
               NACSOF = NACSOF + 1                                      STATS102
               WT1S = WT1S + CROSS(IN)                                  STATS103
               WT2S = WT2S + CROSS(IN)*CROSS(IN)                        STATS104
               DO 32 I = 1, 4                                           STATS105
                  WT41S(I) = WT41S(I) + CROSS4(IN,I)                    STATS106
                  IF ( cross4(in,i) .GT. 1D-23 )                        STATS107
     .            WT42S(I) = WT42S(I) + CROSS4(IN,I)*CROSS4(IN,I)       STATS108
 32            CONTINUE                                                 STATS109
            ENDIF                                                       STATS110
 31      CONTINUE                                                       STATS111
      ENDIF                                                             STATS112
C                                                                       STATS113
      END                                                               STATS114
      SUBROUTINE WTDEVT(in,weit)                                        WTDEVT 2
C-----------------------------------------------------------------      WTDEVT 3
C Compute the event weight and return unwt'd events, if required        WTDEVT 4
C-----------------------------------------------------------------      WTDEVT 5
      IMPLICIT NONE                                                     WTDEVT 6
      SAVE                                                              WTDEVT 7
C Local variables                                                       WTDEVT 8
      INTEGER J,IN,IDL,IONC,I1,I2                                       WTDEVT 9
      REAL*8 DUM,RAT,RN,FAC,LMAXW,WEIT                                  WTDEVT10
      LOGICAL first                                                     WTDEVT11
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C Event writing block                                                   WRITING2
      INTEGER IWRITE,CODE,NWRITN,NWRIMX                                 WRITING3
      REAL*8 XMAXWT                                                     WRITING4
      COMMON /EWRITE/ XMAXWT,IWRITE,CODE(11),NWRITN,NWRIMX              WRITING5
C                                                                       WTDEVT18
      DATA first/.TRUE./                                                WTDEVT19
C                                                                       WTDEVT20
      IF ( first ) THEN                                                 WTDEVT21
C                                                                       WTDEVT22
C Take the proper constants out (they are only used at the end)         WTDEVT23
C                                                                       WTDEVT24
        fac = psfact*symme*colfac*3.89379d8/4D0                         WTDEVT25
        lmaxw = xmaxwt/fac                                              WTDEVT26
        first = .FALSE.                                                 WTDEVT27
C                                                                       WTDEVT28
      ENDIF                                                             WTDEVT29
C                                                                       WTDEVT30
C Note that IWRITE=1 for unweighted events                              WTDEVT31
C                                                                       WTDEVT32
      RAT = CROSS(IN)/LMAXW                                             WTDEVT33
      IF ( iwrite .EQ. 1 ) THEN                                         WTDEVT34
        IF ( rat .GT. 1.D0 ) THEN                                       WTDEVT35
          PRINT'(/''  Maximum weight exceeded !! Max weight = '',F9.3,  WTDEVT36
     &      ''  event weight = '',F9.3/                                 WTDEVT37
     &      ''  Please raise weight for next run.''/)',                 WTDEVT38
     &      XMAXWT,CROSS(IN)*FAC                                        WTDEVT39
        ELSEIF ( rat .GT. RN(rat) ) THEN                                WTDEVT40
          weit = 1D0                                                    WTDEVT41
        ELSE                                                            WTDEVT42
          weit = 0D0                                                    WTDEVT43
        ENDIF                                                           WTDEVT44
      ELSE                                                              WTDEVT45
        weit = cross(in)*fac                                            WTDEVT46
      ENDIF                                                             WTDEVT47
C                                                                       WTDEVT48
  999 RETURN                                                            WTDEVT49
      END                                                               WTDEVT50
      FUNCTION XMAS2(I1,I2)                                             XMAS2  2
C-----------------------------------------------------------------------XMAS2  3
C 2-particle invariant mass using POUT.  Used in ACCEPT.                XMAS2  4
C-----------------------------------------------------------------------XMAS2  5
      IMPLICIT NONE                                                     XMAS2  6
C local variables                                                       XMAS2  7
      INTEGER I1,I2                                                     XMAS2  8
      REAL*8 XMAS2                                                      XMAS2  9
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C                                                                       XMAS2 12
      XMAS2 = MASS2(I1) + MASS2(I2) + 2.D0*(POUT(4,I1)*POUT(4,I2)       XMAS2 13
     &   -POUT(1,I1)*POUT(1,I2)-POUT(2,I1)*POUT(2,I2)                   XMAS2 14
     &   -POUT(3,I1)*POUT(3,I2))                                        XMAS2 15
C                                                                       XMAS2 16
      END                                                               XMAS2 17
      SUBROUTINE XSECN                                                  XSECN  2
C-----------------------------------------------------------------------XSECN  3
C COMPUTE THE AMPLITUDE BY SUMMING OVER DIAGRAMS, THEN OVER HELICITIES  XSECN  4
C                                                                       XSECN  5
C This is the relevant picture of the diagrams and permutations         XSECN  6
C                                                                       XSECN  7
C           particle label  momentum label  helicity label              XSECN  8
C           --------------+---------------+---------------              XSECN  9
C        /  1               K1              HELI(K1)                    XSECN 10
C       /                                                               XSECN 11
C      /                                                                XSECN 12
C     |\                                                                XSECN 13
C       \                                                               XSECN 14
C     |  \  2               K2              HELI(K2)                    XSECN 15
C                                                                       XSECN 16
C     |                                                                 XSECN 17
C        /  3               K3              HELI(K3)                    XSECN 18
C     | /                                                               XSECN 19
C      /                                                                XSECN 20
C      |                                                                XSECN 21
C      |                                                                XSECN 22
C      \                                                                XSECN 23
C     | \                                                               XSECN 24
C        \  4               K4              HELI(K4)                    XSECN 25
C     |                                                                 XSECN 26
C                                                                       XSECN 27
C     |  /  5               K5              HELI(K5)                    XSECN 28
C       /                                                               XSECN 29
C     |/                                                                XSECN 30
C      \                                                                XSECN 31
C       \                                                               XSECN 32
C        \  6               K6              HELI(K6)                    XSECN 33
C-----------------------------------------------------------------------XSECN 34
      IMPLICIT NONE                                                     XSECN 35
      SAVE                                                              XSECN 36
      INTEGER LBUNCH                                                    BUNCHLG2
      PARAMETER (LBUNCH = 127)                                          BUNCHLG3
C Momentum commons                                                      MOMENTU2
      INTEGER LPARMX,LGMISR                                             MOMENTU3
      PARAMETER (LPARMX = 11, LGMISR=7)                                 MOMENTU4
      REAL*8 QMOM,PEVT,MASS,DMASS,MASS2,D,DEVT,PEVTFS,GAMISR,POUT,QSAV  MOMENTU5
      COMMON / MOMCOM / QMOM(LBUNCH,4,LPARMX),MASS(LPARMX),DMASS(LPARMX)MOMENTU6
     &   ,MASS2(LPARMX),D(LBUNCH,6,6),DEVT(6,6),PEVT(4,LPARMX),         MOMENTU7
     &   PEVTFS(4,LPARMX),GAMISR(4),POUT(4,LPARMX),QSAV(4,3:LPARMX,2)   MOMENTU8
C FER1CO and FER2CO commons - fermion characteristics                   FER12CO2
      INTEGER COLFAC,KINDQQ                                             FER12CO3
      REAL*8 BFAC,CHA,DCHA,VF,AF,SYMME,PSFACT,WFLUX,QCDF,RRATIO,ZRATIO  FER12CO4
      COMMON / FER1CO / BFAC(6),CHA(6),DCHA(6),VF(6),AF(6),SYMME,PSFACT,FER12CO5
     &        WFLUX(LBUNCH),QCDF(9),COLFAC,KINDQQ(6)                    FER12CO6
      CHARACTER*2 FLA                                                   FER12CO7
      COMMON / FER2CO / FLA(11)                                         FER12CO8
C Phase space weights                                                   PHASSPA2
      REAL*8 PSWT,PSWTEV,DENS                                           PHASSPA3
      COMMON /PHASEP/ PSWTEV,PSWT(LBUNCH),DENS(100,LBUNCH)              PHASSPA4
C Bookkeeping block                                                     BOOKEEP2
      INTEGER NEVENT,NEVTRJ,NEVTAC,NEVTW1,NBUNCH,NEVTIN,                BOOKEEP3
     &   NEVSOF,NACSOF,DIACOU,DIATOT                                    BOOKEEP4
      REAL*8 WT1,WT2,WT41,WT42,WTMN,WTMX,WTMN4,WTMX4,CROSS,CROSS4,WIJ,  BOOKEEP5
     &   WTSAV,PSWSAV,MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT1S,WT2S,   BOOKEEP6
     &   WT41S,WT42S,DENSSV,ISRWT                                       BOOKEEP7
      COMMON /BOOKIE/ WT1,WT2,WT41(4),WT42(4),WTMN,WTMX,WTMN4(4),       BOOKEEP8
     &   WTMX4(4),CROSS(LBUNCH),CROSS4(LBUNCH,4),WIJ(LBUNCH,4,4),       BOOKEEP9
     &   MEMN,MEMX,ME1,ME2,PSWMN,PSWMX,EGMSOF,WT41S(4),WT42S(4),WT1S,   BOOKEE10
     &   WT2S,WTSAV(4,2),PSWSAV(2),DENSSV(100,2),ISRWT(LBUNCH),         BOOKEE11
     &   NEVENT,NEVTRJ,NEVTAC,                                          BOOKEE12
     &   NEVTW1,NBUNCH,NEVTIN,NEVSOF,NACSOF,DIACOU,DIATOT               BOOKEE13
C control block                                                         CONTROL2
      REAL*8 X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,Y2MX34,    CONTROL3
     &   Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,FBOD3,FBOD2M,      CONTROL4
     &   XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,THCNEV,KGMAX,  CONTROL5
     &   KG1,KRATIO                                                     CONTROL6
      INTEGER NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART                  CONTROL7
      LOGICAL DWANT,ISRFL,FSRFL,EXTSOU,AUTOWT                           CONTROL8
      COMMON /CONTRL/ X2MN34,X2MN56,X2MX34,X2MX56,CSCEN,XMOM34,XMOM56,  CONTROL9
     &   Y2MX34,Y2MX56,Y2MXKM,YTRP34,YTRP56,Q2MN,ACCU,FBOD2,            CONTRO10
     &   FBOD3,FBOD2M,XMIN34,XMIN56,XMAX34,XMAX56,KBOD2,GFAC2Z,CSCNEV,  CONTRO11
     &   THCNEV,KGMAX,KG1,KRATIO,                                       CONTRO12
     &   NEVPRI,NEVTMX,NTKCEN,DEBGLV,NOFSR,NOPART,DWANT(4),ISRFL,FSRFL, CONTRO13
     &   EXTSOU,AUTOWT                                                  CONTRO14
C Some physics constants                                                CONSTPH2
      REAL*8 ALPHA,RMZ,RGZ,SW2,RMZ2,RMZRGZ,RMZGZ2                       CONSTPH3
      COMMON / FISIKX / ALPHA,RMZ,RGZ,SW2,RMZ2,RMZRGZ,RMZGZ2            CONSTPH4
C Local variables                                                       XSECN 44
      INTEGER IN,INFCNT,L1,L2,L3,L4,L5,L6,K,N1,N2,N3,IONC,HELPRP,       XSECN 45
     &   NPERM1,NPERM2,ITYPE,I,I1,I2,HID(-1:1),LI1,LI2,LI3,LI4,         XSECN 46
     &   LI5,LI6,LI56,DIAEV                                             XSECN 47
      REAL*8 FERMS,CRSPRI,CS1234(LBUNCH,4,4),XM2,XM3,Q1,Q3              XSECN 48
      CHARACTER*8 CHTEM(4)                                              XSECN 49
      LOGICAL Q1FL(LBUNCH),Q2FL(LBUNCH),Q3FL(LBUNCH)                    XSECN 50
C                                                                       XSECN 51
C THE TYPE OF A GVEN DIAGRAM                                            XSECN 52
      CHARACTER*16  TYPENAME(4)                                         XSECN 53
C                                                                       XSECN 54
C THE DIAGRAMS FOR THE VARIOUS HELICITIES, THE HELICITY AMP, AND THE X SXSECN 55
      COMPLEX*16 DIAGRM(LBUNCH),DIAGFN(LBUNCH),M(LBUNCH,2,2,2,2,2,2),   XSECN 56
     &   M4(LBUNCH,4,2,2,2,2,4)                                         XSECN 57
C Unfortunately, 7 is the max. no. of subscripts on the VAX             XSECN 58
C DIAGFV commons                                                        DIAGFVC2
      INTEGER K1,K2,K3,K4,K5,K6                                         DIAGFVC3
      LOGICAL Z1FL,Z2FL,QMASK                                           DIAGFVC4
      COMMON /DIACOM/ K1,K2,K3,K4,K5,K6,Z1FL(LBUNCH),Z2FL(LBUNCH),      DIAGFVC5
     &   QMASK(LBUNCH)                                                  DIAGFVC6
C The helicities of the fermions                                        HELICOF2
      INTEGER HELI(6)                                                   HELICOF3
      COMMON / HELICO / HELI                                            HELICOF4
C                                                                       XSECN 61
C THE PERMUTATIONS OF THE FERMIONS                                      XSECN 62
      INTEGER KPERM1(6,4),KPERM2(6,4)                                   XSECN 63
      DATA KPERM1/1, 3, 5, 5, 1, 3,                                     XSECN 64
     &            3, 5, 1, 3, 5, 1,                                     XSECN 65
     &            5, 1, 3, 1, 3, 5,                                     XSECN 66
     &            1, 1, 1,-1,-1,-1/                                     XSECN 67
      DATA KPERM2/2, 4, 6, 6, 2, 4,                                     XSECN 68
     &            4, 6, 2, 4, 6, 2,                                     XSECN 69
     &            6, 2, 4, 2, 4, 6,                                     XSECN 70
     &            1, 1, 1,-1,-1,-1/                                     XSECN 71
      DATA INFCNT /0/                                                   XSECN 72
      DATA IONC /0/                                                     XSECN 73
C                                                                       XSECN 74
      DATA TYPENAME /'MULTIPERPIPHERAL', 'BREMSSTRAHLUNG', 'CONVERSION',XSECN 75
     &   'ANNIHILIATION'/                                               XSECN 76
C Helicity index translator                                             XSECN 77
      DATA HID /1,0,2/                                                  XSECN 78
C                                                                       XSECN 79
      XM2(IN,N1,N2) = MASS2(N1) + MASS2(N2) +                           XSECN 80
     &   BFAC(N1)*BFAC(N2)*D(IN,N1,N2)                                  XSECN 81
      XM3(IN,N1,N2,N3) = MASS2(N1) + MASS2(N2) + MASS2(N3) +            XSECN 82
     &   BFAC(N1)*BFAC(N2)*D(IN,N1,N2) + BFAC(N1)*BFAC(N3)*D(IN,N1,N3)  XSECN 83
     &   + BFAC(N2)*BFAC(N3)*D(IN,N2,N3)                                XSECN 84
C                                                                       XSECN 85
C                                                                       XSECN 86
C INITIALIZATION                                                        XSECN 87
      IF (IONC.EQ.0) THEN                                               XSECN 88
C Max. number of diagrams, excluding permutations => count of           XSECN 89
C helicity and propagator combinations                                  XSECN 90
         HELPRP = 2**6*2**2                                             XSECN 91
         IONC = 1                                                       XSECN 92
      ENDIF                                                             XSECN 93
      DO 6 IN = 1, NEVTIN                                               XSECN 94
6        CROSS(IN)=0.D0                                                 XSECN 95
      DO 1 I1 = 1, 4                                                    XSECN 96
         DO 2 IN = 1, NEVTIN                                            XSECN 97
    2       CROSS4(IN,I1) = 0.D0                                        XSECN 98
         DO 3 I2 = I1+1, 4                                              XSECN 99
            DO 4 IN = 1, NEVTIN                                         XSECN100
    4          CS1234(IN,I2,I1) = 0.D0                                  XSECN101
    3    CONTINUE                                                       XSECN102
    1 CONTINUE                                                          XSECN103
C Loop over helicities                                                  XSECN104
      DO 11 LI6 = 1, 2                                                  XSECN105
      DO 11 LI5 = 1, 2                                                  XSECN106
      LI56 = (LI5-1)*2 + LI6                                            XSECN107
      DO 11 LI4 = 1, 2                                                  XSECN108
      DO 11 LI3 = 1, 2                                                  XSECN109
      DO 11 LI2 = 1, 2                                                  XSECN110
      DO 11 LI1 = 1, 2                                                  XSECN111
      DO 12 IN = 1, NEVTIN                                              XSECN112
   12    M(IN,LI1,LI2,LI3,LI4,LI5,LI6) = (0.D0,0.D0)                    XSECN113
         DO 11 I1 = 1, 4                                                XSECN114
            DO 13 IN = 1, NEVTIN                                        XSECN115
   13          M4(IN,I1,LI1,LI2,LI3,LI4,LI56) = (0.D0,0.D0)             XSECN116
   11 CONTINUE                                                          XSECN117
C A BIG LOOP OVER ALL POSSIBLE DIAGRAM PERMUTATIONS                     XSECN118
      DO 101 NPERM1=1,6                                                 XSECN119
      K1=KPERM1(NPERM1,1)                                               XSECN120
      K3=KPERM1(NPERM1,2)                                               XSECN121
      K5=KPERM1(NPERM1,3)                                               XSECN122
      DO 101 NPERM2=1,6                                                 XSECN123
         K2=KPERM2(NPERM2,1)                                            XSECN124
         K4=KPERM2(NPERM2,2)                                            XSECN125
         K6=KPERM2(NPERM2,3)                                            XSECN126
C                                                                       XSECN127
C NOW THE DIAGRAM IS (IN PRINCIPLE) DEFINED                             XSECN128
C CHECK ON FLAVOUR CONSERVATION ALONG THE LINES                         XSECN129
         IF(FLA(K1).NE.FLA(K2)) GOTO 101                                XSECN130
         IF(FLA(K3).NE.FLA(K4)) GOTO 101                                XSECN131
         IF(FLA(K5).NE.FLA(K6)) GOTO 101                                XSECN132
C DETERMINE THE TYPE OF THE DIAGRAM                                     XSECN133
         IF((K1.EQ.1.AND.K2.EQ.2).OR.                                   XSECN134
     .       (K5.EQ.1.AND.K6.EQ.2)) THEN                                XSECN135
C Annihilation                                                          XSECN136
           ITYPE=4                                                      XSECN137
         ELSEIF(K3.EQ.1.AND.K4.EQ.2) THEN                               XSECN138
C Conversion                                                            XSECN139
           ITYPE=3                                                      XSECN140
         ELSEIF((K1.EQ.1.AND.K6.EQ.2).OR.                               XSECN141
     .           (K5.EQ.1.AND.K2.EQ.2)) THEN                            XSECN142
C Multiperipheral                                                       XSECN143
           ITYPE=1                                                      XSECN144
         ELSE                                                           XSECN145
C Bremsstrahlung                                                        XSECN146
           ITYPE=2                                                      XSECN147
         ENDIF                                                          XSECN148
C Do we want this type of diagram?                                      XSECN149
         IF (.NOT.DWANT(ITYPE))  GOTO 101                               XSECN150
         IF (NBUNCH.EQ.1)  DIACOU = DIACOU + 1                          XSECN151
C                                                                       XSECN152
C FERMI SIGN                                                            XSECN153
         FERMS=DFLOAT(KPERM1(NPERM1,4)*KPERM2(NPERM2,4))                XSECN154
C Kinematic tests to see if we can ignore the helicity non-conserving   XSECN155
C amplitudes as though our particles were massless.                     XSECN156
         DO 105 IN = 1, NEVTIN                                          XSECN157
            Q1   = ABS(XM2(IN,K1,K2))                                   XSECN158
            Q1FL(IN) = MASS2(K1)/Q1 .LT. ACCU                           XSECN159
            Q3   = ABS(XM2(IN,K5,K6))                                   XSECN160
            Q3FL(IN) = MASS2(K5)/Q3 .LT. ACCU                           XSECN161
C The Q2 test is more delicate because we must ensure our test is gauge XSECN162
C invariant.                                                            XSECN163
            Q2FL(IN) = MASS2(K3)/ABS(XM3(IN,K1,K2,K3)) .LT. ACCU .AND.  XSECN164
     &             MASS2(K3)/ABS(XM3(IN,K5,K6,K3)) .LT. ACCU            XSECN165
C Can we ignore either of the Z propagators?                            XSECN166
            Z1FL(IN) = Q1/RMZ2 .LT. ACCU .AND. DCHA(K1).GT.0.01D0       XSECN167
            Z2FL(IN) = Q3/RMZ2 .LT. ACCU .AND. DCHA(K5).GT.0.01D0       XSECN168
  105    CONTINUE                                                       XSECN169
C Count of number of diagrams evaluated                                 XSECN170
         DO 107 IN = 1, NEVTIN                                          XSECN171
            DIAEV = HELPRP                                              XSECN172
            IF (Q1FL(IN))  DIAEV = DIAEV/2                              XSECN173
            IF (Q2FL(IN))  DIAEV = DIAEV/2                              XSECN174
            IF (Q3FL(IN))  DIAEV = DIAEV/2                              XSECN175
            IF (Z1FL(IN))  DIAEV = DIAEV/2                              XSECN176
            IF (Z1FL(IN))  DIAEV = DIAEV/2                              XSECN177
            DIATOT = DIATOT + DIAEV                                     XSECN178
  107    CONTINUE                                                       XSECN179
C A BIG LOOP OVER THE HELICITIES                                        XSECN180
         DO 201 L1=1,-1,-2                                              XSECN181
         HELI(1)=L1                                                     XSECN182
         LI1 = HID(L1)                                                  XSECN183
         DO 201 L2=1,-1,-2                                              XSECN184
         HELI(2)=L2                                                     XSECN185
         LI2 = HID(L2)                                                  XSECN186
         DO 201 L3=1,-1,-2                                              XSECN187
         HELI(3)=L3                                                     XSECN188
         LI3 = HID(L3)                                                  XSECN189
         DO 201 L4=1,-1,-2                                              XSECN190
         HELI(4)=L4                                                     XSECN191
         LI4 = HID(L4)                                                  XSECN192
         DO 201 L5=1,-1,-2                                              XSECN193
         HELI(5)=L5                                                     XSECN194
         LI5 = HID(L5)                                                  XSECN195
         DO 201 L6=1,-1,-2                                              XSECN196
         HELI(6)=L6                                                     XSECN197
         LI6 = HID(L6)                                                  XSECN198
         LI56 = (LI5-1)*2 + LI6                                         XSECN199
C Set up vector mask                                                    XSECN200
         DO 205 IN = 1, NEVTIN                                          XSECN201
            QMASK(IN) = .FALSE.                                         XSECN202
            IF (Q1FL(IN) .AND. HELI(K1).NE.HELI(K2))  QMASK(IN) = .TRUE.XSECN203
            IF (Q2FL(IN) .AND. HELI(K3).NE.HELI(K4))  QMASK(IN) = .TRUE.XSECN204
            IF (Q3FL(IN) .AND. HELI(K5).NE.HELI(K6))  QMASK(IN) = .TRUE.XSECN205
 205     CONTINUE                                                       XSECN206
C COMPUTE THE GIVEN DIAGRAM FOR THE GIVEN HELICITY CONFIGURATION        XSECN207
          CALL DIAGFV(DIAGFN)                                           XSECN208
          DO 209 IN = 1, NEVTIN                                         XSECN209
             DIAGRM(IN)=FERMS*DIAGFN(IN)                                XSECN210
C                                                                       XSECN211
C The ratio R again !                                                   XSECN212
C                                                                       XSECN213
             IF     ( kindqq(k5).NE.0 .AND. kindqq(k6).NE.0 ) THEN      XSECN214
               diagrm(in) = diagrm(in)                                  XSECN215
     .                    * SQRT(rratio(xm2(in,k5,k6),kindqq(k5)))      XSECN216
             ELSEIF ( kindqq(k1).NE.0 .AND. kindqq(k2).NE.0 ) THEN      XSECN217
               diagrm(in) = diagrm(in)                                  XSECN218
     .                    * SQRT(rratio(xm2(in,k1,k2),kindqq(k1)))      XSECN219
             ELSEIF ( kindqq(k3).NE.0 .AND. kindqq(k4).NE.0 ) THEN      XSECN220
               diagrm(in) = diagrm(in)                                  XSECN221
     .                    * 1D0          ! Multiperipheral              XSECN222
             ENDIF                                                      XSECN223
C                                                                       XSECN224
C ADD THE RESULT TO THE CURRENT HELICITY AMPLITUDE                      XSECN225
  209        M(IN,LI1,LI2,LI3,LI4,LI5,LI6) =                            XSECN226
     &          M(IN,LI1,LI2,LI3,LI4,LI5,LI6) + DIAGRM(IN)              XSECN227
C Separate helicity amplitudes by diagram type                          XSECN228
          DO 211 IN = 1, NEVTIN                                         XSECN229
  211           M4(IN,ITYPE,LI1,LI2,LI3,LI4,LI56) =                     XSECN230
     &          M4(IN,ITYPE,LI1,LI2,LI3,LI4,LI56) + DIAGRM(IN)          XSECN231
C                                                                       XSECN232
C Bookkeeping part of loop:                                             XSECN233
C                                                                       XSECN234
          INFCNT=INFCNT+1                                               XSECN235
          IF (INFCNT.LE.4.AND.DEBGLV.GE.2)                              XSECN236
     &       WRITE(*,*) ' DIAGRAM :',DIAGRM(1)                          XSECN237
C PRINT OPTIONAL INFO ON THE DIAGRAM                                    XSECN238
          IF (INFCNT.LE.4.AND.DEBGLV.GE.2) WRITE(*,901)                 XSECN239
     .      FLA(K1),K1,HELI(K1),  FLA(K2),K2,HELI(K2),                  XSECN240
     .      FLA(K3),K3,HELI(K3),  FLA(K4),K4,HELI(K4),                  XSECN241
     .      FLA(K5),K5,HELI(K5),  FLA(K6),K6,HELI(K6), TYPENAME(ITYPE), XSECN242
     .      NINT(FERMS)                                                 XSECN243
  901     FORMAT(                                                       XSECN244
     .      3('  [',A2,'(',I1,',',I2,') ',A2,'(',I1,',',I2,')]'),1X,A16,XSECN245
     .      I3)                                                         XSECN246
C                                                                       XSECN247
C END OF THE LOOP OVER HELICITIES                                       XSECN248
  201    CONTINUE                                                       XSECN249
C END OF THE LOOP OVER DIAGRAMS                                         XSECN250
  101 CONTINUE                                                          XSECN251
C Now add the HEL**2, and separate into diagram groups                  XSECN252
      DO 301 LI6 = 1, 2                                                 XSECN253
      DO 301 LI5 = 1, 2                                                 XSECN254
      LI56 = (LI5-1)*2 + LI6                                            XSECN255
      DO 301 LI4 = 1, 2                                                 XSECN256
      DO 301 LI3 = 1, 2                                                 XSECN257
      DO 301 LI2 = 1, 2                                                 XSECN258
      DO 301 LI1 = 1, 2                                                 XSECN259
C ADD THE HELICITY AMPLITUDE SQUARED TO THE CROSS SECTION               XSECN260
         DO 302 IN = 1, NEVTIN                                          XSECN261
  302       CROSS(IN) = CROSS(IN) +                                     XSECN262
     &         ABS(M(IN,LI1,LI2,LI3,LI4,LI5,LI6))**2                    XSECN263
         DO 301 I1 = 1, 4                                               XSECN264
C Calculate  matrix elements**2 in the diagram groups                   XSECN265
            DO 303 IN = 1, NEVTIN                                       XSECN266
  303          CROSS4(IN,I1) = CROSS4(IN,I1) +                          XSECN267
     &            ABS(M4(IN,I1,LI1,LI2,LI3,LI4,LI56))**2                XSECN268
            DO 301 I2 = I1+1, 4                                         XSECN269
C Now calulate cross terms                                              XSECN270
               DO 304 IN = 1, NEVTIN                                    XSECN271
  304             CS1234(IN,I2,I1) = CS1234(IN,I2,I1) + ABS(            XSECN272
     &          M4(IN,I1,LI1,LI2,LI3,LI4,LI56) +                        XSECN273
     &          M4(IN,I2,LI1,LI2,LI3,LI4,LI56))**2                      XSECN274
  301 CONTINUE                                                          XSECN275
C                                                                       XSECN276
C Now build weights.  Note that Wij summed in the upper diag. = 1       XSECN277
      DO 111 I = 1, 4                                                   XSECN278
         DO 112 IN = 1, NEVTIN                                          XSECN279
  112       WIJ(IN,I,I) = CROSS4(IN,I)/CROSS(IN)                        XSECN280
  111 CONTINUE                                                          XSECN281
C                                                                       XSECN282
      DO 121 I1 = 1, 4                                                  XSECN283
         DO 122 I2 = I1+1, 4                                            XSECN284
            DO 123 IN = 1, NEVTIN                                       XSECN285
  123          WIJ(IN,I2,I1) = CS1234(IN,I2,I1)/CROSS(IN) -             XSECN286
     &            WIJ(IN,I1,I1) - WIJ(IN,I2,I2)                         XSECN287
  122    CONTINUE                                                       XSECN288
  121 CONTINUE                                                          XSECN289
C                                                                       XSECN290
C Do some stats on various weights of interest                          XSECN291
      CALL STATS                                                        XSECN292
C                                                                       XSECN293
      IF (NBUNCH.EQ.1.AND.DEBGLV.GE.1)  THEN                            XSECN294
C Diagram evaluation count                                              XSECN295
            PRINT'(1X,I6,'' /'',I7,'' ('',F6.2,''%), of the diagrams'', XSECN296
     &         '' were evaluated in this bunch.'')',DIATOT,             XSECN297
     &         NEVTIN*DIACOU*HELPRP,100.0*DIATOT/(DIACOU*HELPRP*        XSECN298
     &         NEVTIN)                                                  XSECN299
C                                                                       XSECN300
C ADDITIONAL POSSIBLE SYMMETRY FACTOR FOR IDENTICAL OUTGOING PARTICLES  XSECN301
C plus other constants.                                                 XSECN302
         DO 131 I = 1, NEVPRI                                           XSECN303
            PRINT'(/'' Event '',I4,'' :'')',I                           XSECN304
C ADDITIONAL SPIN AVERAGING FACTOR FOR INCOMING E+E- SPINS              XSECN305
            CRSPRI=CROSS(I)*PSFACT*SYMME*COLFAC*3.89379D8/4.D0          XSECN306
            PRINT*,' CROSS :',CRSPRI                                    XSECN307
            PRINT'('' ME weights :''/1X,4I10)',(I1,I1=1,4)              XSECN308
            DO 135 I1 = 1, 4                                            XSECN309
               DO 136 I2 = 1, 4                                         XSECN310
                  IF (I2.LT.I1) THEN                                    XSECN311
                     CHTEM(I2) = ' '                                    XSECN312
                  ELSE                                                  XSECN313
C Internal write                                                        XSECN314
                     WRITE(CHTEM(I2),'(F8.4)') WIJ(I,I2,I1)             XSECN315
                  ENDIF                                                 XSECN316
  136          CONTINUE                                                 XSECN317
               PRINT'(1X,I1,4(A8,2X))',I1,(CHTEM(I2),I2=1,4)            XSECN318
  135       CONTINUE                                                    XSECN319
  131    CONTINUE                                                       XSECN320
      ENDIF                                                             XSECN321
C                                                                       XSECN322
      END                                                               XSECN323
      SUBROUTINE FINALG                                                 FINALG 2
C***********************************************************************FINALG 3
C                                                                       FINALG 4
C AUTHOR : FRANCOIS LE DIBERDER (1991)                                  FINALG 5
C          DIBERDER@FRCPN11                                             FINALG 6
C          LAL: (33-1) 64 46 89 15                                      FINALG 7
C                                                                       FINALG 8
C***********************************************************************FINALG 9
C-----------------------------------------------------------------------FINALG10
C RADIATIVE CORRECTIONS TO THE REACTION                                 FINALG11
C              X Y  ->- CHARGED AND NEUTRAL PARTICLES                   FINALG12
C-----------------------------------------------------------------------FINALG13
C COMMUNICATION WITH THE PROGRAM IS DONE THROUGH THE COMMON : TALK      FINALG14
C-----------------------------------------------------------------------FINALG15
C INPUT :                                                               FINALG16
C        PIN(50,6)  AT MOST 50 TRACKS (INCLUDING PHOTONS TO BE RADIATED)FINALG17
C                   THE FIRST TWO TRACKS ARE X AND Y                    FINALG18
C        PIN(  ,1): PX                                                  FINALG19
C               2 : PY                                                  FINALG20
C               3 : PZ                                                  FINALG21
C               4 : E   (RECALCULATED)                                  FINALG22
C               5 : M                                                   FINALG23
C               6 : P   (RECALCULATED)                                  FINALG24
C        INQ(50)    CHARGE   (RADIATING TRACKS)                         FINALG25
C                   TO KEEP A PAIR OF TRACKS FROM RADIATING             FINALG26
C                   SET THEIR INQ() TO 0                                FINALG27
C        NC         NUMBER OF CHARGED TRACKS                            FINALG28
C        NN         NUMBER OF NEUTRAL (AFTER THE CHARGED TRACKS)        FINALG29
C        IDEB       PRINTING OPTION (=1 => PRINT)                       FINALG30
C        WMAX       MAXIMUM WEIGHT                                      FINALG31
C        NNG        NUMBER OF PHOTONS TO ADD TO THE FINAL STATE         FINALG32
C                   THE MINIMUM ENERGY OF EACH PHOTON IS SET TO         FINALG33
C                   KM*EBEAM IN ORDER TO AVOID TROUBLE WITH DIVISIONS   FINALG34
C-----------------------------------------------------------------------FINALG35
C OUTPUT:                                                               FINALG36
C        POUT(50,6) OUTPUT 4-MOMENTA,  INCLUDING THE NEW PHOTON(S)      FINALG37
C                   WHICH ARE ADDED AFTER THE INPUT PARTICLES           FINALG38
C        WMXX       MAXIMUM WEIGHT WHICH WAS SEEN                       FINALG39
C-----------------------------------------------------------------------FINALG40
C-----------------------------------------------------------------------FINALG41
C**************************************                                 FINALG42
C INTERNAL PARAMETERS [CAN BE MODIFIED]                                 FINALG43
C**************************************                                 FINALG44
C        KM         MINIMUM VALUE OF EGAM/EBEAM ( 0 < KM < KMAX )       FINALG45
C        NTRMAX     MAXIMUM NUMBER OF TRIALS. IF NTRIAL>NTRMAX THE      FINALG46
C                   THE CURRENT PHOTON GENERATION IS RESTARTED.         FINALG47
C        IW         FLAG TO SELECT BETWEEN VARIOUS 'FRAGMENTATION'      FINALG48
C                   SCHEMES.                                            FINALG49
C             =0    IS THE RECOMMENDED VALUE                            FINALG50
C             =1    DEPOPULATES SLIGHTLY THE HIGH-ENERGY PART OF THE    FINALG51
C                   PHOTON ENERGY SPECTRUM                              FINALG52
C             =2    DEMOCRATIC OVERALL BOOSTING OF THE FINAL STATE      FINALG53
C             =3    1 AND 2                                             FINALG54
C-----------------------------------------------------------------------FINALG55
      IMPLICIT REAL*8(A-H,K,O-Z)                                        FINALG56
      SAVE                                                              FINALG57
C Final state radiation common                                          RADFINA2
      COMMON/RADFIN/P(50,6),K(4),RADLOG(50,4),ZC(50)                    RADFINA3
     *,ALFA,PI,CST,B,WCOR,KMAX,REDUCE                                   RADFINA4
     *,NCHA,NTOTAL,JDEBUG,NTRIAL,KM,IRAD,ICH(50),IW,IFLAG               RADFINA5
      COMMON/TALK/WMAX,WMXX,POUT(50,6),PIN(50,6),INQ(50)                FINALG59
     *           ,NC,NN,IDEB,NNG                                        FINALG60
      DATA IPAS/0/                                                      FINALG61
      JDEBUG=IDEB                                                       FINALG62
      REDUCE=1.D+00/NNG                                                 FINALG63
C INITIALIZATION DONE ONCE FOR ALL                                      FINALG64
      IF(IPAS.EQ.0)             THEN                                    FINALG65
         IPAS =  1                                                      FINALG66
      PI    =  3.141592653D+00                                          FINALG67
      ALFA  =  1.D+00/(137.035989D+00*PI)                               FINALG68
      CST0  =  ALFA*0.5D+00                                             FINALG69
C SET THE INTERNAL PARAMETERS                                           FINALG70
      KM    =  1.D-06                                                   FINALG71
      NTRMAX=  1000                                                     FINALG72
      IW    =  0                                                        FINALG73
                                END IF                                  FINALG74
      EBEAM =PIN(1,4)                                                   FINALG75
      NCHA  =NC                                                         FINALG76
      NTOTAL=NC+NN                                                      FINALG77
         DO 21 I=1,NTOTAL                                               FINALG78
         DO 22 J=1,6                                                    FINALG79
         POUT(I,J)=PIN(I,J)                                             FINALG80
 22      CONTINUE                                                       FINALG81
 21      CONTINUE                                                       FINALG82
                    SUMASS=0.D+00                                       FINALG83
         DO 31 I=1,NTOTAL                                               FINALG84
         ICH(I)=INQ(I)                                                  FINALG85
         IF(I.GE.2) SUMASS=SUMASS+POUT(I,5)/EBEAM                       FINALG86
 31      CONTINUE                                                       FINALG87
C THE MAXIMAL VALUE OF KMAX IS DEFINED BY THE MASSES                    FINALG88
      KMAX =1.D+00-SUMASS**2*0.25D+00                                   FINALG89
C LOOP OVER THE TO BE PRODUCED PHOTONS                                  FINALG90
      DO 11111 NGAM=1,NNG                                               FINALG91
      NTRIAL=0                                                          FINALG92
C RETURN LINE TO PRODUCE UN-WEIGHTED EVENTS                             FINALG93
10000 CONTINUE                                                          FINALG94
      NTRIAL=NTRIAL+1                                                   FINALG95
C RESET THE GENERATION IF TOO MANY TRIALS                               FINALG96
      IF(NTRIAL.GT.NTRMAX) NTRIAL=1                                     FINALG97
C (RE)-LOAD THE DATA                                                    FINALG98
      DO 10 I=1,NTOTAL                                                  FINALG99
                       PTOT=0.D+00                                      FINAL100
          DO 41 J=1,3                                                   FINAL101
          P(I,J)=POUT(I,J)/EBEAM                                        FINAL102
                       PTOT=PTOT+P(I,J)**2                              FINAL103
 41       CONTINUE                                                      FINAL104
          P(I,5)=POUT(I,5)/EBEAM                                        FINAL105
          P(I,6)=DSQRT(PTOT)                                            FINAL106
          P(I,4)=DSQRT(PTOT+P(I,5)**2)                                  FINAL107
 10   CONTINUE                                                          FINAL108
          IF(NTRIAL.EQ.1) THEN                                          FINAL109
            RADSUM          =0.D+00                                     FINAL110
            DO 51 I=1,NTOTAL                                            FINAL111
                 RADLOG(I,3)=0.D+00                                     FINAL112
                 RADLOG(I,4)=0.D+00                                     FINAL113
            IF(ICH(I).NE.0) THEN                                        FINAL114
                 RADLOG(I,3)=2.D+00*DLOG((P(I,4)+P(I,6))/P(I,5))        FINAL115
                 RADLOG(I,4)=RADLOG(I,3)*P(I,4)/P(I,6)                  FINAL116
            RADSUM          =RADSUM+RADLOG(I,4)                         FINAL117
                            END IF                                      FINAL118
 51       CONTINUE                                                      FINAL119
      CST=CST0*RADSUM                                                   FINAL120
            RADSUM          =1.D+00/RADSUM                              FINAL121
                                           RAD0=0.D+00                  FINAL122
            DO 61 I=1,NTOTAL                                            FINAL123
            RADLOG(I,2)=RADLOG(I,4)*RADSUM                              FINAL124
            RADLOG(I,1)=RADLOG(I,4)*RADSUM+RAD0                         FINAL125
                                           RAD0=RADLOG(I,1)             FINAL126
 61       CONTINUE                                                      FINAL127
            IF(JDEBUG.EQ.1)     THEN                                    FINAL128
            PRINT *,'RADLOG-2'                                          FINAL129
            PRINT *,(RADLOG(I,2),I=1,NTOTAL)                            FINAL130
                                END IF                                  FINAL131
                          END IF                                        FINAL132
       IF(JDEBUG.EQ.1.AND.NTRIAL.EQ.1) THEN                             FINAL133
       PRINT *,' INPUT PARTICLES '                                      FINAL134
         DO 71 I=1,NTOTAL                                               FINAL135
         PRINT *,' PARTICLE ',I,' PX-Y-Z [Q= ',ICH(I),' ] '             FINAL136
         PRINT *,(P(I,J),J=1,3)                                         FINAL137
         PRINT *,'            ','  E-M-P '                              FINAL138
         PRINT *,(P(I,J),J=4,6)                                         FINAL139
 71       CONTINUE                                                      FINAL140
                                       END IF                           FINAL141
C **************************************                                FINAL142
C GO FOR IT !!!!!!!!!!!!!!!!!!!!!!!!!!!!                                FINAL143
C **************************************                                FINAL144
      CALL RADCOR                                                       FINAL145
C **************************************                                FINAL146
      IF(WCOR.GT.WMXX) WMXX=WCOR                                        FINAL147
      IF(WMAX*RN(1).GT.WCOR) GO TO 10000                                FINAL148
C LOAD THE MODIFIED 4-VECTORS                                           FINAL149
           DO 81 J=1,6                                                  FINAL150
          DO 11 I=3,NTOTAL                                              FINAL151
          POUT(I,J)=P(I,J)*EBEAM                                        FINAL152
 11       CONTINUE                                                      FINAL153
 81       CONTINUE                                                      FINAL154
      NTOTAL=NTOTAL+1                                                   FINAL155
           DO 91 J=1,4                                                  FINAL156
           POUT(NTOTAL,J)=K(J)*EBEAM                                    FINAL157
 91       CONTINUE                                                      FINAL158
           POUT(NTOTAL,5)=0.D+00                                        FINAL159
           POUT(NTOTAL,6)=POUT(NTOTAL,4)                                FINAL160
           ICH(NTOTAL)=0                                                FINAL161
11111 CONTINUE                                                          FINAL162
      RETURN                                                            FINAL163
      END                                                               FINAL164
      SUBROUTINE GETBET                                                 GETBET 2
      IMPLICIT REAL*8(A-H,K,O-Z)                                        GETBET 3
      SAVE                                                              GETBET 4
C Final state radiation common                                          RADFINA2
      COMMON/RADFIN/P(50,6),K(4),RADLOG(50,4),ZC(50)                    RADFINA3
     *,ALFA,PI,CST,B,WCOR,KMAX,REDUCE                                   RADFINA4
     *,NCHA,NTOTAL,JDEBUG,NTRIAL,KM,IRAD,ICH(50),IW,IFLAG               RADFINA5
      B=0.D+00                                                          GETBET 6
      DO 10 I=1  ,NCHA-1                                                GETBET 7
         DO 21 J=I+1,NCHA                                               GETBET 8
         EMIJ=P(I,5)*P(J,5)                                             GETBET 9
         PIPJ=P(I,4)*P(J,4)-P(I,1)*P(J,1)-P(I,2)*P(J,2)-P(I,3)*P(J,3)   GETBET10
         PIPJ=DABS(PIPJ)                                                GETBET11
         SQIJ=DSQRT(PIPJ**2-EMIJ**2)                                    GETBET12
         FIJ=PIPJ/SQIJ*DLOG((PIPJ+SQIJ)/EMIJ)-1.D+00                    GETBET13
         B=B-FIJ*ZC(I)*ZC(J)                                            GETBET14
 21      CONTINUE                                                       GETBET15
   10 CONTINUE                                                          GETBET16
      B=B*2.D+00*ALFA*REDUCE                                            GETBET17
      IF(JDEBUG.EQ.1) PRINT *,' BETA FACTOR = ',B                       GETBET18
      RETURN                                                            GETBET19
      END                                                               GETBET20
      SUBROUTINE GETBOO(GAMMA,EBEAM,BOOST)                              GETBOO 2
      IMPLICIT REAL*8(A-H,O-Z)                                          GETBOO 3
      SAVE                                                              GETBOO 4
      DIMENSION GAMMA(4),BOOST(4)                                       GETBOO 5
      DATA ONE/1.D+00/                                                  GETBOO 6
        BOOST(4)=ONE                                                    GETBOO 7
        DO 371 I=1,3                                                    GETBOO 8
        BOOST(I)=0.D+00                                                 GETBOO 9
 371    CONTINUE                                                        GETBOO10
      IF(GAMMA(4).LT.1.E-20*EBEAM) RETURN                               GETBOO11
        BOOST(4)=0.D+00                                                 GETBOO12
        DO 381 I=1,3                                                    GETBOO13
        BOOST(I)=-GAMMA(I)/(2.D+00*EBEAM-GAMMA(4))                      GETBOO14
        BOOST(4)=BOOST(4)+BOOST(I)*BOOST(I)                             GETBOO15
 381    CONTINUE                                                        GETBOO16
      BOOST(4)=ONE/DSQRT(ONE-BOOST(4))                                  GETBOO17
        DO 391 I=1,3                                                    GETBOO18
        BOOST(I)=BOOST(I)*BOOST(4)                                      GETBOO19
 391    CONTINUE                                                        GETBOO20
      RETURN                                                            GETBOO21
      END                                                               GETBOO22
      SUBROUTINE GETLAB(BOOST,PIN,POUT)                                 GETLAB 2
      IMPLICIT REAL*8(A-H,O-Z)                                          GETLAB 3
      SAVE                                                              GETLAB 4
      DIMENSION BOOST(4),PIN(4),POUT(4)                                 GETLAB 5
      DATA ONE/1.D+00/                                                  GETLAB 6
        BIN=0.D+00                                                      GETLAB 7
        DO 421 I=1,3                                                    GETLAB 8
        BIN=BIN+BOOST(I)*PIN(I)                                         GETLAB 9
 421    CONTINUE                                                        GETLAB10
      DO 431 I=1,3                                                      GETLAB11
      POUT(I)=PIN(I)+BOOST(I)*( BIN/(BOOST(4)+ONE)+PIN(4))              GETLAB12
 431  CONTINUE                                                          GETLAB13
      POUT(4)=PIN(4)*BOOST(4) + BIN                                     GETLAB14
      RETURN                                                            GETLAB15
      END                                                               GETLAB16
      SUBROUTINE GETMAT(ELEM)                                           GETMAT 2
      IMPLICIT REAL*8(A-H,K,O-Z)                                        GETMAT 3
      SAVE                                                              GETMAT 4
C Final state radiation common                                          RADFINA2
      COMMON/RADFIN/P(50,6),K(4),RADLOG(50,4),ZC(50)                    RADFINA3
     *,ALFA,PI,CST,B,WCOR,KMAX,REDUCE                                   RADFINA4
     *,NCHA,NTOTAL,JDEBUG,NTRIAL,KM,IRAD,ICH(50),IW,IFLAG               RADFINA5
      ELEM=0.D+00                                                       GETMAT 6
      DO 20 J=1,3                                                       GETMAT 7
                SUM=0.D+00                                              GETMAT 8
                DO 331 I=1,NCHA                                         GETMAT 9
                VL=K(1)*P(I,1)+K(2)*P(I,2)+K(3)*P(I,3)                  GETMAT10
                VP=P(I,J)*K(4)-VL*K(J)/K(4)                             GETMAT11
                SUM=SUM+VP/(K(4)*P(I,4)-VL)*ZC(I)                       GETMAT12
 331            CONTINUE                                                GETMAT13
      ELEM=ELEM+SUM*SUM                                                 GETMAT14
   20 CONTINUE                                                          GETMAT15
      RETURN                                                            GETMAT16
      END                                                               GETMAT17
      SUBROUTINE GETWEI                                                 GETWEI 2
      IMPLICIT REAL*8(A-H,K,O-Z)                                        GETWEI 3
      SAVE                                                              GETWEI 4
C Final state radiation common                                          RADFINA2
      COMMON/RADFIN/P(50,6),K(4),RADLOG(50,4),ZC(50)                    RADFINA3
     *,ALFA,PI,CST,B,WCOR,KMAX,REDUCE                                   RADFINA4
     *,NCHA,NTOTAL,JDEBUG,NTRIAL,KM,IRAD,ICH(50),IW,IFLAG               RADFINA5
         WSSS=0.D+00                                                    GETWEI 6
         DO 21 I=1,NCHA                                                 GETWEI 7
         IF(ICH(I).NE.0) THEN                                           GETWEI 8
         VL=(K(1)*P(I,1)+K(2)*P(I,2)+K(3)*P(I,3))/K(4)/P(I,4)           GETWEI 9
         WSSS=WSSS+1.D+00/(1.D+00-VL)                                   GETWEI10
                         END IF                                         GETWEI11
 21      CONTINUE                                                       GETWEI12
      CALL MOVE                                                         GETWEI13
      CALL GETMAT(ELEM)                                                 GETWEI14
      WCOR=CST*ELEM/WSSS/B*REDUCE                                       GETWEI15
      RETURN                                                            GETWEI16
      END                                                               GETWEI17
      SUBROUTINE INITIG(GAMMA,EBEAM,GK1,GK2,RATIO,WEIGHT)               INITG  2
C***********************************************************************INITG  3
C                                                                       INITG  4
C AUTHOR : FRANCOIS LE DIBERDER (1991)                                  INITG  5
C          DIBERDER@FRCPN11                                             INITG  6
C          LAL: (33-1) 64 46 89 15                                      INITG  7
C                                                                       INITG  8
C***********************************************************************INITG  9
C                                                                       INITG 10
C INITIAL STATE RADIATION TO THE PROCESS                                INITG 11
C                                                                       INITG 12
C    E+  E-  --->---- FINAL STATE                                       INITG 13
C                                                                       INITG 14
C-----------------------------------------------------------------------INITG 15
C INPUT  : EBEAM = BEAM ENERGY                                          INITG 16
C          GK1   = PHOTON ENERGY (UNIT OF EBEAM) BELOW WHICH THE        INITG 17
C                  ENERGY GENERATION YIELDS A CONSTANT WEIGHT.          INITG 18
C     ABOVE GK1    ENERGY GENERATION IS DONE ACCORDING TO THE LAW       INITG 19
C                  1/(K0*(1-K0)). IN THAT REGION THE WEIGHT THEN        INITG 20
C                  DEPENDS ON THE PHOTON ENERGY. THIS PECULIARITY       INITG 21
C                  ALLOWS ONE TO HANDLE IN A MORE CONFORTABLE WAY       INITG 22
C                  A POSSIBLE QED-POLE OF THE CROSS-SECTION.            INITG 23
C          GK2   = MAXIMUM PHOTON ENERGY. IT CANNOT BE SET TO 1         INITG 24
C                  IF THE 2ND GENERATOR IS USED.                        INITG 25
C          RATIO = IF POSITIVE, IS TAKEN AS THE FRACTION OF THE         INITG 26
C                  GENERATED EVENTS THE USER WANTS TO PRODUCE BELOW     INITG 27
C                  GK1. IF RATIO IS NEGATIVE, IT IS TAKEN TO MEAN       INITG 28
C                  THAT THE USER WANTS THIS FRACTION TO BE CALCULA-     INITG 29
C                  TED BY INITIG ON THE BASIS OF THE RADIATION ALONE.   INITG 30
C                  ANY VALUE OF RATIO (<1 ...) IS ALLOWED, SINCE THE    INITG 31
C                  WEIGHT WILL ACCOUNT FOR IT.                          INITG 32
C-----------------------------------------------------------------------INITG 33
C OUTPUT : GAMMA = 4-MOMEMTUM OF THE GENERATED PHOTON                   INITG 34
C          WEIGHT= WEIGHT OF THE GENERATION. ONE SHOULD MULTIPLY        INITG 35
C                  THE USER-GENERATOR-WEIGHT BY THIS VALUE.             INITG 36
C   ---------->--  TO DEAL ONLY WITH UNITY-WEIGHT EVENTS ONE CAN        INITG 37
C                  SET GK1<0. IN SUCH A CASE THE 2ND GENERATOR IS       INITG 38
C                  SKIPPED.                                             INITG 39
C-----------------------------------------------------------------------INITG 40
C A)THE GENERATED PHOTON IS COMING OUT THE ROUTINE WITH WEIGHT UNITY    INITG 41
C   ->>>- IF GK1 < 0. -------<<<------------------------------------    INITG 42
C B)GENERATION ACCOUNTS FOR INTERFERENCES BETWEEN THE E+ AND E- LINES   INITG 43
C C)THE ENERGY DISTRIBUTION IS TAKEN FROM THE REFERENCES GIVEN BELOW    INITG 44
C              (THERE IS NOTHING SPECIAL ABOUT IT).                     INITG 45
C-----------------------------------------------------------------------INITG 46
C REF : R.CAHN                 [PHYS.REV.D 36      (1987) PAGE. 2666]   INITG 47
C     : E.A. KURAEV V.S.FADIN  [SOV.J.NUCL.PHYS.41 (1985) PAGE.  466]   INITG 48
C***********************************************************************INITG 49
C TYPICAL EXAMPLE OF USE :                                              INITG 50
C-----------------------------------------------------------------------INITG 51
C 1) GET THE PHOTON 4-MOMENTUM                                          INITG 52
C     CALL INITIG(GAMMA,EBEAM,GK1,GK2,RATIO,WEIGHT)                     INITG 53
C 2) SIMULATE THE PROCESS EE->-X IN ITS CMS                             INITG 54
C    WITH S_NEW=4.EBEAM**2*(1-GAMMA(4)/EBEAM)                           INITG 55
C     CALL ......                                                       INITG 56
C 3) GET THE BOOST WHICH LINKS THE ABOVE CMS WITH THE LAB               INITG 57
C     CALL GETBOO(GAMMA,EBEAM,BOOST)                                    INITG 58
C 4) LOOP OVER THE FINAL STATE TRACKS TO OBTAIN THE LAB MOMENTA [POUT]  INITG 59
C     CALL GETLAB(BOOST,PIN,POUT)                                       INITG 60
C***********************************************************************INITG 61
      IMPLICIT REAL*8(A-H,K,O-Z)                                        INITG 62
      SAVE                                                              INITG 63
      DIMENSION GAMMA(4)                                                INITG 64
C DIB                                                                   INITG 65
      DIMENSION ID(100)                                                 INITG 66
      DATA ID/100*0/                                                    INITG 67
      DATA IPAS/0/                                                      INITG 68
C INITIALIZATION DONE ONCE FOR ALL                                      INITG 69
      IF(IPAS.EQ.0)             THEN                                    INITG 70
         IPAS =  1                                                      INITG 71
      ONE   =  1.            D+00                                       INITG 72
      TWO   =  2.            D+00                                       INITG 73
      PI    =  3.141592653   D+00                                       INITG 74
      TWOPI =  TWO*PI                                                   INITG 75
      XME   =  0.000510999   D+00                                       INITG 76
      V2    =  ONE-(XME/EBEAM)**2                                       INITG 77
      V     =  DSQRT(V2)                                                INITG 78
      RADLOG=  TWO*DLOG((ONE+V)*EBEAM/XME)                              INITG 79
      BETA  =  TWO/(137.035989D+00*PI)*(RADLOG-ONE)                     INITG 80
      CST   =  ONE+0.75D+00*BETA                                        INITG 81
C CHECK THE VALIDITY OF GK1 AND GK2                                     INITG 82
      IF(  GK1.LT.ONE   .AND.GK2.LT.ONE                                 INITG 83
     *.AND.GK1.GT.0.D+00.AND.GK2.GT.0.D+00                              INITG 84
     *.AND.GK1.LT.GK2)                                                  INITG 85
     *                       THEN                                       INITG 86
                             NORM=1                                     INITG 87
      GK1B  =  GK1**BETA                                                INITG 88
      GK2B  =  GK2**BETA                                                INITG 89
      PROB1 =  CST*GK1B-BETA*GK1*(ONE-0.25D+00*GK1)                     INITG 90
      PROB2 =  CST*GK2B-BETA*GK2*(ONE-0.25D+00*GK2)                     INITG 91
      IF(RATIO.LT.0.D+00) RATIO =  PROB1/PROB2                          INITG 92
      WZONE1=  PROB1/           RATIO                                   INITG 93
      GK1LOG=  DLOG(GK1/(ONE-GK1))                                      INITG 94
      GK2LOG=  DLOG(GK2/(ONE-GK2))                                      INITG 95
      DELLOG=  GK2LOG-GK1LOG                                            INITG 96
      WZONE2=  BETA*DELLOG/(ONE-RATIO)                                  INITG 97
      HK1   =  GK1                                                      INITG 98
                             ELSE                                       INITG 99
                             NORM=0                                     INITG100
      WZONE1=  ONE                                                      INITG101
      GK1B  =  ONE                                                      INITG102
      HK1   =  ONE                                                      INITG103
      RATIO =  ONE                                                      INITG104
                             END IF                                     INITG105
C SET THE LOWEST PHOTON ENERGY VALUE (IN BEAM ENERGY UNIT)              INITG106
      KM    =  1.D-10                                                   INITG107
      KMB   =  KM**BETA                                                 INITG108
                                END IF                                  INITG109
C................................                                       INITG110
C GENERATION OF THE PHOTON ENERGY                                       INITG111
C................................                                       INITG112
      IF(NORM.EQ.0..OR.RN(7).LT.RATIO)               THEN               INITG113
C GENERATE ACCORDING TO THE RIGHT DISTRIBUTION.                         INITG114
      WEIGHT=WZONE1                                                     INITG115
10000                                        CONTINUE                   INITG116
      K0=KM                                                             INITG117
      AR=RN(8)                                                          INITG118
      IF(AR.GT.KMB) THEN                                                INITG119
      K0=HK1*DEXP(DLOG(AR)/BETA)                                        INITG120
      WK0=CST-K0/(GK1B*AR)*(ONE-0.5D+00*K0)                             INITG121
      IF(RN(9)*CST.GT.WK0)                GO TO 10000                   INITG122
                    END IF                                              INITG123
                                                        ELSE            INITG124
C GENERATE ACCORDING TO 1/[K(1-K)].                                     INITG125
      AR=RN(9)                                                          INITG126
      K0=ONE-ONE/(ONE+DEXP(DELLOG*AR+GK1LOG))                           INITG127
      WEIGHT=WZONE2                                                     INITG128
     *      *(CST*K0**BETA-K0*(ONE-0.5D+00*K0))                         INITG129
     *      *(ONE-K0)                                                   INITG130
                                                        END IF          INITG131
C...................................                                    INITG132
C GENERATION OF THE PHOTON DIRECTION                                    INITG133
C...................................                                    INITG134
C----GENERATION OF PHI                                                  INITG135
      PHI=TWOPI*RN(10)                                                  INITG136
C----GENERATION OF COS(THE)                                             INITG137
10001                                        CONTINUE                   INITG138
      RN2=RN(11)                                                        INITG139
      CTH=(ONE-(ONE+V)/DEXP(RADLOG*RN2))/V                              INITG140
      CT2=CTH*CTH                                                       INITG141
      WEI=(ONE-CT2)/(ONE-V2*CT2)                                        INITG142
      IF(RN(11).GT.WEI)                   GO TO 10001                   INITG143
      SIDE=RN(12)                                                       INITG144
      IF(SIDE.LT.0.5D+00) CTH=-CTH                                      INITG145
      STH=DSQRT(ONE-CT2)                                                INITG146
C............................                                           INITG147
C BUILT THE PHOTON 4-MOMEMTUM                                           INITG148
C............................                                           INITG149
      GAMMA(1)=EBEAM*K0*STH*DCOS(PHI)                                   INITG150
      GAMMA(2)=EBEAM*K0*STH*DSIN(PHI)                                   INITG151
      GAMMA(3)=EBEAM*K0*CTH                                             INITG152
      GAMMA(4)=EBEAM*K0                                                 INITG153
      RETURN                                                            INITG154
      END                                                               INITG155
      SUBROUTINE LORENZ(X,A,B)                                          LORENZ 2
      IMPLICIT REAL*8(A-H,O-Z)                                          LORENZ 3
      DIMENSION X(4),A(4),B(4)                                          LORENZ 4
      XA=X(1)*A(1)+X(2)*A(2)+X(3)*A(3)                                  LORENZ 5
        DO 341 I=1,3                                                    LORENZ 6
        B(I)=A(I)+X(I)*X(4)*(X(4)/(X(4)+1.D+00)*XA+A(4))                LORENZ 7
 341    CONTINUE                                                        LORENZ 8
      B(4)=X(4)*(A(4)+XA)                                               LORENZ 9
      RETURN                                                            LORENZ10
      END                                                               LORENZ11
      SUBROUTINE MOVE                                                   MOVE   2
      IMPLICIT REAL*8(A-H,K,O-Z)                                        MOVE   3
      SAVE                                                              MOVE   4
C Final state radiation common                                          RADFINA2
      COMMON/RADFIN/P(50,6),K(4),RADLOG(50,4),ZC(50)                    RADFINA3
     *,ALFA,PI,CST,B,WCOR,KMAX,REDUCE                                   RADFINA4
     *,NCHA,NTOTAL,JDEBUG,NTRIAL,KM,IRAD,ICH(50),IW,IFLAG               RADFINA5
      COMMON/LOOK/PRO,EGAM,K0,PART(50),TR(4)                            MOVE   6
      DIMENSION PIN(4),POUT(4)                                          MOVE   7
      DIMENSION PTOT(4)                                                 MOVE   8
C THE PURPOSE OF THIS ROUTINE IS TO ENSURE ENERGY-MOMENTUM CONSERVATION MOVE   9
C WHICH WAS EXPLICITELY VIOLATED ABOVE BY THE ADJONCTION OF A PHOTON TO MOVE  10
C THE FINAL STATE.                                                      MOVE  11
C THE PROCEDURE TO RESTORE (EXACTLY) P_IN=P_OUT IS AS FOLLOWS.          MOVE  12
C  0) REMOVE THE PHOTON FROM THE RADIATING PARTICLES (IW=2 OR 3)        MOVE  13
C                3_P(I)=3_P(I)-3_P(PHOTON)*PART(I)                      MOVE  14
C     IN MOST CASES THIS STEP ALREADY ALMOST ENSURES  P_INI=P_FIN       MOVE  15
C  1) RESCALE THE FINAL STATE MOMENTA TO SATISFY :                      MOVE  16
C              P(FINAL STATE)**2=S*(1-K0)                               MOVE  17
C     IN DOING SO, THE MASSES OF THE PARTICLES ARE ALSO SCALED DOWN.    MOVE  18
C  2) APPLY A BOOST TO THE FINAL STATE TO SATISFY :                     MOVE  19
C            3_P(FINAL STATE)   = - 3_P(PHOTON)                         MOVE  20
C  3) MODIFY THE ENERGIES OF THE FINAL STATE PARTICLES TO SATISFY :     MOVE  21
C            MASSE(PARTICLE)**2 = E(PARTICLE)**2- 3_P(PARTICLE)**2      MOVE  22
C  4) RESCALE THE PHOTON 4-MOMENTA (K0 ->- EGAM) TO RECOVER :           MOVE  23
C              P(FINAL STATE)**2=S*(1-EGAM)                             MOVE  24
C  5) RE-APPLY A BOOST TO THE FINAL STATE TO RECOVER :                  MOVE  25
C            3_P(FINAL STATE) + 3_P(PHOTON) =0.                         MOVE  26
C----                                                                   MOVE  27
C[0]-                                                                   MOVE  28
C----                                                                   MOVE  29
      IF(IW.EQ.0.OR.IW.EQ.1)                                            MOVE  30
C                                                                       MOVE  31
     *THEN                                                              MOVE  32
C                                                                       MOVE  33
C  REMOVE THE PHOTON FROM THE RADIATING PARTICLES                       MOVE  34
C GET THE RELATIVE RADIATION PROBABILITIES                              MOVE  35
      PARTOT=0.D+00                                                     MOVE  36
      DO 31 I=1,NTOTAL                                                  MOVE  37
       COSTET=0.D+00                                                    MOVE  38
         DO 21 J=1,3                                                    MOVE  39
         COSTET=COSTET+P(I,J)*K(J)                                      MOVE  40
 21      CONTINUE                                                       MOVE  41
C COSTET(PARTICLE-PHOTON) AFTER FULL SUBTRACTION : [(P-K).K/(P-K)*K]    MOVE  42
       COSTET=(COSTET-K(4)**2)                                          MOVE  43
     *       /(DSQRT(P(I,6)**2+K(4)**2-2.D+00*COSTET)*K(4))             MOVE  44
       VELOCI=P(I,6)/P(I,4)                                             MOVE  45
       PART(I)=VELOCI**2*(1.D+00-COSTET**2)/(1.D+00-VELOCI*COSTET)**2   MOVE  46
     *        *ZC(I)**2                                                 MOVE  47
       PARTOT=PARTOT+PART(I)                                            MOVE  48
 31    CONTINUE                                                         MOVE  49
C REMOVE THE PHOTON BY PART FROM THE FINAL STATE ONLY                   MOVE  50
      DO 51 I=3,NTOTAL                                                  MOVE  51
      P(I,6)=0.D+00                                                     MOVE  52
      PART(I)=PART(I)/PARTOT                                            MOVE  53
        DO 41 J=1,3                                                     MOVE  54
        P(I,J)=P(I,J)-K(J)*PART(I)                                      MOVE  55
        P(I,6)=P(I,6)+P(I,J)**2                                         MOVE  56
 41     CONTINUE                                                        MOVE  57
      P(I,4)=DSQRT(P(I,6)+P(I,5)**2)                                    MOVE  58
      P(I,6)=DSQRT(P(I,6))                                              MOVE  59
 51   CONTINUE                                                          MOVE  60
C GET THE FINAL STATE INVARIANT MASS                                    MOVE  61
      DO 71 J=1,4                                                       MOVE  62
      PTOT(J)=0.D+00                                                    MOVE  63
         DO 61 I=3,NTOTAL                                               MOVE  64
         PTOT(J)=PTOT(J)+P(I,J)                                         MOVE  65
 61      CONTINUE                                                       MOVE  66
 71   CONTINUE                                                          MOVE  67
      WTOT=PTOT(4)**2-PTOT(1)**2-PTOT(2)**2-PTOT(3)**2                  MOVE  68
      PRO=DSQRT(4.D+00*(1.D+00-K(4))/WTOT)                              MOVE  69
C                                                                       MOVE  70
      ELSE                                                              MOVE  71
C                                                                       MOVE  72
      PRO=DSQRT(1.D+00-K(4))                                            MOVE  73
C                                                                       MOVE  74
      END IF                                                            MOVE  75
C                                                                       MOVE  76
C----                                                                   MOVE  77
C[1]-                                                                   MOVE  78
C----                                                                   MOVE  79
C  RESCALE THE FINAL STATE MOMENTA                                      MOVE  80
      DO 91 I=3,NTOTAL                                                  MOVE  81
         DO 81 J=1,4                                                    MOVE  82
         P(I,J)=P(I,J)*PRO                                              MOVE  83
 81      CONTINUE                                                       MOVE  84
 91   CONTINUE                                                          MOVE  85
C----                                                                   MOVE  86
C[2]-                                                                   MOVE  87
C----                                                                   MOVE  88
C  APPLY A BOOST TO THE FINAL STATE                                     MOVE  89
      IF(IW.EQ.0.OR.IW.EQ.1)                                            MOVE  90
C                                                                       MOVE  91
     *THEN                                                              MOVE  92
C                                                                       MOVE  93
C  BRING THE FINAL STATE TO ITS C.M.S                                   MOVE  94
      TR2  = 0.D+00                                                     MOVE  95
       DO 121 J=1,3                                                     MOVE  96
       TR(J)=-PTOT(J)/PTOT(4)                                           MOVE  97
       TR2  =TR2+TR(J)**2                                               MOVE  98
 121   CONTINUE                                                         MOVE  99
      TR(4)=1.D+00/DSQRT(1.D+00-TR2)                                    MOVE 100
      DO 151 I=3,NTOTAL                                                 MOVE 101
         DO 131 J=1,4                                                   MOVE 102
         PIN(J)=P(I,J)                                                  MOVE 103
 131     CONTINUE                                                       MOVE 104
      CALL LORENZ(TR,PIN,POUT)                                          MOVE 105
         DO 141 J=1,4                                                   MOVE 106
         P(I,J)=POUT(J)                                                 MOVE 107
 141     CONTINUE                                                       MOVE 108
 151  CONTINUE                                                          MOVE 109
C                                                                       MOVE 110
      END IF                                                            MOVE 111
C                                                                       MOVE 112
      TR(4)= 2.D+00-K(4)                                                MOVE 113
      TR2  = 0.D+00                                                     MOVE 114
      DO 161 J=1,3                                                      MOVE 115
      TR(J)=-K(J)/TR(4)                                                 MOVE 116
      TR2  =TR2+TR(J)**2                                                MOVE 117
 161  CONTINUE                                                          MOVE 118
      TR(4)=1.D+00/DSQRT(1.D+00-TR2)                                    MOVE 119
      DO 191 I=3,NTOTAL                                                 MOVE 120
         DO 171 J=1,4                                                   MOVE 121
         PIN(J)=P(I,J)                                                  MOVE 122
 171     CONTINUE                                                       MOVE 123
      CALL LORENZ(TR,PIN,POUT)                                          MOVE 124
         DO 181 J=1,3                                                   MOVE 125
         P(I,J)=POUT(J)                                                 MOVE 126
 181     CONTINUE                                                       MOVE 127
 191  CONTINUE                                                          MOVE 128
C----                                                                   MOVE 129
C[3]-                                                                   MOVE 130
C----                                                                   MOVE 131
C  MODIFY THE ENERGIES OF THE FINAL STATE PARTICLES                     MOVE 132
      DO 231 I=3,NTOTAL                                                 MOVE 133
      P(I,6)=0.D+00                                                     MOVE 134
         DO 221 J=1,3                                                   MOVE 135
         P(I,6)=P(I,6)+P(I,J)**2                                        MOVE 136
 221     CONTINUE                                                       MOVE 137
      P(I,4)=DSQRT(P(I,6)+P(I,5)**2)                                    MOVE 138
      P(I,6)=DSQRT(P(I,6))                                              MOVE 139
 231  CONTINUE                                                          MOVE 140
C----                                                                   MOVE 141
C[4]-                                                                   MOVE 142
C----                                                                   MOVE 143
C  RESCALE THE PHOTON 4-MOMENTA                                         MOVE 144
      DO 251 J=1,4                                                      MOVE 145
         PTOT(J)=0.D+00                                                 MOVE 146
         DO 241 I=3,NTOTAL                                              MOVE 147
         PTOT(J)=PTOT(J)+P(I,J)                                         MOVE 148
 241     CONTINUE                                                       MOVE 149
 251  CONTINUE                                                          MOVE 150
      WTOT=PTOT(4)**2-PTOT(1)**2-PTOT(2)**2-PTOT(3)**2                  MOVE 151
      EGAM=1.D+00-WTOT*.25D+00                                          MOVE 152
      IF(EGAM.LT.KM) EGAM=KM                                            MOVE 153
         K0=K(4)                                                        MOVE 154
         DO 261 J=1,4                                                   MOVE 155
         K(J)=K(J)*EGAM/K(4)                                            MOVE 156
 261     CONTINUE                                                       MOVE 157
C----                                                                   MOVE 158
C[5]-                                                                   MOVE 159
C----                                                                   MOVE 160
      E2=(2.D+00-K(4))**2                                               MOVE 161
      BETA=(PTOT(4)*K0-EGAM*(2.D+00-EGAM))/(E2+K0**2)                   MOVE 162
      TR(4)=1.D+00/DSQRT(1.D+00-BETA**2)                                MOVE 163
      DO 271 J=1,3                                                      MOVE 164
      TR(J)=K(J)/K(4)*BETA                                              MOVE 165
 271  CONTINUE                                                          MOVE 166
      DO 321 I=3,NTOTAL                                                 MOVE 167
         DO  281 J=1,4                                                  MOVE 168
         PIN(J)=P(I,J)                                                  MOVE 169
 281     CONTINUE                                                       MOVE 170
      CALL LORENZ(TR,PIN,POUT)                                          MOVE 171
         DO  291 J=1,4                                                  MOVE 172
         P(I,J)=POUT(J)                                                 MOVE 173
 291     CONTINUE                                                       MOVE 174
 321  CONTINUE                                                          MOVE 175
      RETURN                                                            MOVE 176
      END                                                               MOVE 177
      SUBROUTINE RADCOR                                                 RADCOR 2
      IMPLICIT REAL*8(A-H,K,O-Z)                                        RADCOR 3
      SAVE                                                              RADCOR 4
C Final state radiation common                                          RADFINA2
      COMMON/RADFIN/P(50,6),K(4),RADLOG(50,4),ZC(50)                    RADFINA3
     *,ALFA,PI,CST,B,WCOR,KMAX,REDUCE                                   RADFINA4
     *,NCHA,NTOTAL,JDEBUG,NTRIAL,KM,IRAD,ICH(50),IW,IFLAG               RADFINA5
      DIMENSION IETA(50)                                                RADCOR 6
      DATA IETA/-1,-1,48*1/                                             RADCOR 7
C...............................................................        RADCOR 8
      IF(NTRIAL.EQ.1.OR.IW.EQ.1..OR.IW.EQ.3)          THEN              RADCOR 9
C----SET THE ZC FACTORS                                                 RADCOR10
        DO 21 I=1,NTOTAL                                                RADCOR11
        ZC(I)=ICH(I)*IETA(I)                                            RADCOR12
 21     CONTINUE                                                        RADCOR13
C----CALCULATION OF BETA(=B)                                            RADCOR14
      CALL GETBET                                                       RADCOR15
C----CHOICE OF IRAD                                                     RADCOR16
      SELECT=RN(2)                                                      RADCOR17
        DO 31 I=1,NTOTAL                                                RADCOR18
      IF(SELECT.LT.RADLOG(I,1).AND.ICH(I).NE.0) THEN                    RADCOR19
                                                IRAD=I                  RADCOR20
                                                GO TO 111               RADCOR21
                                                END IF                  RADCOR22
 31     CONTINUE                                                        RADCOR23
      IRAD=NCHA                                                         RADCOR24
 111                                            CONTINUE                RADCOR25
C----GENERATION OF K0 : B*K0**(1-B)*(1.-KL+0.5*KL**2) [NORMALIZED]      RADCOR26
 112  CONTINUE                                                          RADCOR27
      K0=KM                                                             RADCOR28
      AR=RN(3)                                                          RADCOR29
C.........................                                              RADCOR30
      IF(AR.GT.KM**B) THEN                                              RADCOR31
C.........................                                              RADCOR32
      K0=DEXP(DLOG(AR)/B)                                               RADCOR33
      IF(K0.GT.KMAX)                            GO TO 112               RADCOR34
      KL=K0/P(IRAD,6)                                                   RADCOR35
      IF(KL.GT.1.D+00)                          GO TO 112               RADCOR36
      WK0=1.D+00-KL+0.5D+00*KL**2                                       RADCOR37
      IF(RN(4).GT.WK0)                       GO TO 112                  RADCOR38
C----UPDATE THE ZC FACTORS                                              RADCOR39
        DO 41 I=1,NTOTAL                                                RADCOR40
        IF(ICH(I).NE.0.AND.I.NE.IRAD) THEN                              RADCOR41
         KL=K0/P(IRAD,6)                                                RADCOR42
         IF(KL.GT.1.D+00) WKL=0.D+00                                    RADCOR43
                          WKL=1.D+00-KL+0.5D+00*KL**2                   RADCOR44
         ZC(I)=ZC(I)*DSQRT(WKL/WK0)                                     RADCOR45
                                      END IF                            RADCOR46
 41     CONTINUE                                                        RADCOR47
C.........................                                              RADCOR48
                      END IF                                            RADCOR49
C.........................                                              RADCOR50
C...............................................................        RADCOR51
                                                          END IF        RADCOR52
C...............................................................        RADCOR53
C----GENERATION OF PHI                                                  RADCOR54
      RN1=RN(5)                                                         RADCOR55
      PHI=RN1*2.D+00*PI                                                 RADCOR56
C----GENERATION OF COS(THE)                                             RADCOR57
      RN2=RN(6)                                                         RADCOR58
      V=P(IRAD,6)/P(IRAD,4)                                             RADCOR59
      CTH=(1.D+00-(1.D+00+V)/DEXP(RADLOG(IRAD,3)*RN2))/V                RADCOR60
C----GET K-VECTOR IN LABORATORY                                         RADCOR61
      CALL TURN(PHI,CTH,K0)                                             RADCOR62
C----CORRECTION WEIGHT                                                  RADCOR63
      CALL GETWEI                                                       RADCOR64
      RETURN                                                            RADCOR65
      END                                                               RADCOR66
      SUBROUTINE TECPRO(A,B,C)                                          TECPRO 2
      IMPLICIT REAL*8(A-H,K,O-Z)                                        TECPRO 3
      DIMENSION A(3),B(3),C(3)                                          TECPRO 4
      C(1)=A(2)*B(3)-A(3)*B(2)                                          TECPRO 5
      C(2)=A(3)*B(1)-A(1)*B(3)                                          TECPRO 6
      C(3)=A(1)*B(2)-A(2)*B(1)                                          TECPRO 7
      CNORM=0.D+00                                                      TECPRO 8
         DO 351 J=1,3                                                   TECPRO 9
         CNORM=CNORM+C(J)**2                                            TECPRO10
 351     CONTINUE                                                       TECPRO11
      CNORM=1.D+00/DSQRT(CNORM)                                         TECPRO12
         DO 361 J=1,3                                                   TECPRO13
         C(J)=C(J)*CNORM                                                TECPRO14
 361     CONTINUE                                                       TECPRO15
      RETURN                                                            TECPRO16
      END                                                               TECPRO17
      SUBROUTINE TURN(PHI,CTH,K0)                                       TURN   2
      IMPLICIT REAL*8(A-H,K,O-Z)                                        TURN   3
      SAVE                                                              TURN   4
C Final state radiation common                                          RADFINA2
      COMMON/RADFIN/P(50,6),K(4),RADLOG(50,4),ZC(50)                    RADFINA3
     *,ALFA,PI,CST,B,WCOR,KMAX,REDUCE                                   RADFINA4
     *,NCHA,NTOTAL,JDEBUG,NTRIAL,KM,IRAD,ICH(50),IW,IFLAG               RADFINA5
      DIMENSION X(3),Y(3),Z(3),R(3)                                     TURN   6
      DATA R/1.D0,1.D0,0.D0/                                            TURN   7
C----ROTATION TO RETURN TO THE LABORATORY                               TURN   8
      STH=DSQRT(1.D+00-CTH**2)                                          TURN   9
      CPH=DCOS(PHI)                                                     TURN  10
      SPH=DSIN(PHI)                                                     TURN  11
        DO 21 J=1,3                                                     TURN  12
        Z(J)=P(IRAD,J)                                                  TURN  13
 21     CONTINUE                                                        TURN  14
      CALL TECPRO(Z,R,X)                                                TURN  15
      CALL TECPRO(Z,X,Y)                                                TURN  16
        REN=1.D+00/P(IRAD,6)                                            TURN  17
        DO 31 J=1,3                                                     TURN  18
        K(J)=(X(J)*STH*CPH+Y(J)*STH*SPH+Z(J)*CTH*REN)*K0                TURN  19
 31     CONTINUE                                                        TURN  20
      K(4)=K0                                                           TURN  21
      IF(JDEBUG.EQ.1) THEN                                              TURN  22
      PRINT *,' PHOTON : KX-Y-Z-E ',K                                   TURN  23
      END IF                                                            TURN  24
      RETURN                                                            TURN  25
      END                                                               TURN  26
      SUBROUTINE BOOST(BETAX,BETAY,BETAZ,P)                             BOOST  2
C-------------------------------------------------------------------    BOOST  3
C!  Perform a Lorentz transformation of the quadriimpulsion P           BOOST  4
C   from frame 1 to frame 2                                             BOOST  5
C                                                                       BOOST  6
C   Input:     Passed:    --BETAX,Y,Z    2's velocity / 1               BOOST  7
C                         --P,           quadriimpulsion in 1           BOOST  8
C                                                                       BOOST  9
C   Output:    Passed:    --P,           quadriimpulsion in 2           BOOST 10
C                                                                       BOOST 11
C   P. Janot  --  20 oct 1988                                           BOOST 12
C-------------------------------------------------------------------    BOOST 13
      IMPLICIT REAL*8 (A-Z)                                             BOOST 14
      DIMENSION p(4)                                                    BOOST 15
      BETA2 = BETAX**2 + BETAY**2 + BETAZ**2                            BOOST 16
      IF(BETA2 .EQ. 0.) RETURN                                          BOOST 17
      GAMMA = 1./SQRT(1.-BETA2)                                         BOOST 18
      ONE   = BETAX*P(1) + BETAY*P(2) + BETAZ*P(3)                      BOOST 19
      TWO   = (GAMMA-1.)*ONE/BETA2- GAMMA*P(4)                          BOOST 20
      P(1)  = P(1) + BETAX*TWO                                          BOOST 21
      P(2)  = P(2) + BETAY*TWO                                          BOOST 22
      P(3)  = P(3) + BETAZ*TWO                                          BOOST 23
      P(4)  = GAMMA*(-ONE+P(4))                                         BOOST 24
      RETURN                                                            BOOST 25
      END                                                               BOOST 26
      DOUBLE PRECISION FUNCTION crost(smin,smax,ind2)                   CROST  2
C-----------------------------------------------------------------------CROST  3
C! Integrated value of the cross section over the ffbar invariant mass  CROST  4
C  values, as chosen in FERMISV                                         CROST  5
C                                                                       CROST  6
C  Patrick Janot -- 01 Apr 1994                                         CROST  7
C                                                                       CROST  8
C-----------------------------------------------------------------------CROST  9
      IMPLICIT REAL*8(A-H,O-Z)                                          CROST 10
C Power laws for various distributions in our ps generators             POWERLW2
      REAL*8 ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ,BBCS2,   POWERLW3
     &       BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF             POWERLW4
      COMMON /DPWRS/ ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ, POWERLW5
     &      BBCS2, BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF       POWERLW6
C Some physics constants                                                CONSTPH2
      REAL*8 ALPHA,RMZ,RGZ,SW2,RMZ2,RMZRGZ,RMZGZ2                       CONSTPH3
      COMMON / FISIKX / ALPHA,RMZ,RGZ,SW2,RMZ2,RMZRGZ,RMZGZ2            CONSTPH4
C setup commons                                                         SETUPCO2
      REAL*8 ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                          SETUPCO3
      COMMON /SETUP/ ECM,SECM,EBEAM,ECMISR,SISR,EBEISR                  SETUPCO4
C                                                                       CROST 14
      GOTO (1,2,3,4,5,6,7,8) ind2                                       CROST 15
C                                                                       CROST 16
    1 crost = HILGTN(0d0,ansp,smin,smax)                                CROST 17
      GOTO 999                                                          CROST 18
C                                                                       CROST 19
    2 crost = HILGTN(0d0,bfsk,smin,smax)                                CROST 20
      GOTO 999                                                          CROST 21
C                                                                       CROST 22
    3 crost = HILGTN(0d0,bbsk,smin,smax)                                CROST 23
      GOTO 999                                                          CROST 24
C                                                                       CROST 25
    4 crost = HILGTN(0d0,c1sk,smin,smax)                                CROST 26
      GOTO 999                                                          CROST 27
C                                                                       CROST 28
    5 crost = HILGTN(0d0,c2skp,smin,smax)                               CROST 29
      GOTO 999                                                          CROST 30
C                                                                       CROST 31
    6 crost = HILGTN(0d0,c1sk,smin,smax)                                CROST 32
      GOTO 999                                                          CROST 33
C                                                                       CROST 34
    7 ymin = (smin-rmz2)/rmzrgz                                         CROST 35
      ymax = (smax-rmz2)/rmzrgz                                         CROST 36
      crost = 1./(2d0*ecmisr*rmzrgz)*(ATAN(ymax) - ATAN(ymin))          CROST 37
      GOTO 999                                                          CROST 38
C                                                                       CROST 39
    8 crost = HILGTN(skoff,musk,smin,smax)                              CROST 40
C                                                                       CROST 41
  999 RETURN                                                            CROST 42
      END                                                               CROST 43
      DOUBLE PRECISION FUNCTION crosv(s,ind2)                           CROSV  2
C-----------------------------------------------------------------------CROSV  3
C! The parametrization of the cross-section variation with the ffbar    CROSV  4
C  invariant mass, as chosen in FERMISV                                 CROSV  5
C                                                                       CROSV  6
C  Patrick Janot -- 01 Apr 1994                                         CROSV  7
C                                                                       CROSV  8
C-----------------------------------------------------------------------CROSV  9
      IMPLICIT REAL*8(A-H,O-Z)                                          CROSV 10
C Power laws for various distributions in our ps generators             POWERLW2
      REAL*8 ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ,BBCS2,   POWERLW3
     &       BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF             POWERLW4
      COMMON /DPWRS/ ANSP,ANSQ,ANCOST,BFSK,BFK0,BFCOST,BFCS2,BBSK,BBSQ, POWERLW5
     &      BBCS2, BBCOST,C1SK,C1K0,C2SKP,C2CSK,MUSK,MUCOST,SKOFF       POWERLW6
C Some physics constants                                                CONSTPH2
      REAL*8 ALPHA,RMZ,RGZ,SW2,RMZ2,RMZRGZ,RMZGZ2                       CONSTPH3
      COMMON / FISIKX / ALPHA,RMZ,RGZ,SW2,RMZ2,RMZRGZ,RMZGZ2            CONSTPH4
C                                                                       CROSV 13
      GOTO (1,2,3,4,5,6,7,8) ind2                                       CROSV 14
C                                                                       CROSV 15
    1 crosv = 1./s**ansp                                                CROSV 16
      GOTO 999                                                          CROSV 17
C                                                                       CROSV 18
    2 crosv = 1./s**bfsk                                                CROSV 19
      GOTO 999                                                          CROSV 20
C                                                                       CROSV 21
    3 crosv = 1./s**bbsk                                                CROSV 22
      GOTO 999                                                          CROSV 23
C                                                                       CROSV 24
    4 crosv = 1./s**c1sk                                                CROSV 25
      GOTO 999                                                          CROSV 26
C                                                                       CROSV 27
    5 crosv = 1./s**c2skp                                               CROSV 28
      GOTO 999                                                          CROSV 29
C                                                                       CROSV 30
    6 crosv = 1./s**c1sk                                                CROSV 31
      GOTO 999                                                          CROSV 32
C                                                                       CROSV 33
    7 crosv = 1./((s-rmz2)**2+rmzrgz**2)                                CROSV 34
      GOTO 999                                                          CROSV 35
C                                                                       CROSV 36
    8 crosv = 1./(s+skoff)**musk                                        CROSV 37
C                                                                       CROSV 38
  999 RETURN                                                            CROSV 39
      END                                                               CROSV 40
      DOUBLE PRECISION FUNCTION piform(s)                               PIFORM 2
C-----------------------------------------------------------------------PIFORM 3
C! The pion form factor below 1.5 GeV                                   PIFORM 4
C                                                                       PIFORM 5
C Input...                                                              PIFORM 6
C o s    : the value of the qqbar invariant mass squared                PIFORM 7
C                                                                       PIFORM 8
C  Patrick Janot -- 01 Apr 1994                                         PIFORM 9
C                                                                       PIFORM10
C-----------------------------------------------------------------------PIFORM11
      IMPLICIT REAL*8 (A-H,O-Z)                                         PIFORM12
C The quark and lepton masses                                           QMASSES2
      REAL*8 amu, amd, ams, amc, amb, amt                               QMASSES3
      REAL*8 amel, ammu, amto, amne, amnm, amnt                         QMASSES4
      PARAMETER ( amu = .005D0, amd = .010d0, ams = .150d0 )            QMASSES5
      PARAMETER ( amc = 1.37d0, amb = 4.70d0, amt = 170.d0 )            QMASSES6
      PARAMETER ( amel= .511D-3, ammu= .1057D0, amto = 1.777d0 )        QMASSES7
      PARAMETER ( amne= 1D-4, amnm= 1D-4, amnt = 1D-4 )                 QMASSES8
C The pi value, you shouldn't touch !                                   RESONA 2
      PARAMETER ( pi = 3.1415926353 )                                   RESONA 3
C Alpha QED                                                             RESONA 4
      PARAMETER ( alpha = 1./137.0 )                                    RESONA 5
C The normalized mu+mu- cross section                                   RESONA 6
      PARAMETER ( cmumu = 4.*pi*alpha**2/3. )                           RESONA 7
C The mu,pi, K, D0  masses                                              RESONA 8
      PARAMETER ( xmu  = 0.10565839D0, xmu2  = xmu **2 )                RESONA 9
      PARAMETER ( xmpi = 0.13956755D0, xmpi2 = xmpi**2 )                RESONA10
      PARAMETER ( xmka = 0.493646D0  , xmka2 = xmka**2 )                RESONA11
      PARAMETER ( xmd0 = 1.8695D0    , xmd02 = xmd0**2 )                RESONA12
      PARAMETER ( xmb0 = 5.2776D0    , xmb02 = xmb0**2 )                RESONA13
C The pion form factor                                                  RESONA14
      PARAMETER ( a1 = 0.29020D0, a2 = -2.301D0 )                       RESONA15
      PARAMETER ( a3 = -0.0121D0, a4 =  1.849D0 )                       RESONA16
      PARAMETER ( xm = 1.2D0, xm2 = xm**2, xg = 0.15D0, pow = 0.22D0 )  RESONA17
C The omega resonance                                                   RESONA18
      PARAMETER ( xmom = 0.78195D0 , xmom2 = xmom**2 )                  RESONA19
      PARAMETER ( gmom = 0.00843D0 , gmome = 0.0000006 )                RESONA20
C The omega(1390) resonance                                             RESONA21
      PARAMETER ( x1390 = 1.3910D0  , x13902 = x1390**2 )               RESONA22
      PARAMETER ( g1390 = 0.2240D0  , g1390e = 0.0000002D0 )            RESONA23
C The rho(1450) resonance                                               RESONA24
      PARAMETER ( x1450 = 1.4500D0  , x14502 = x1450**2 )               RESONA25
C     PARAMETER ( g1450 = 0.2370D0  , g1450e = 0.0000025D0 ) ! ???      RESONA26
      PARAMETER ( g1450 = 0.2370D0  , g1450e = 0.0000010D0 )            RESONA27
C The omega(1600) resonance                                             RESONA28
      PARAMETER ( x1600 = 1.5940D0  , x16002 = x1600**2 )               RESONA29
      PARAMETER ( g1600 = 0.1000D0  , g1600e = 0.0000002D0 )            RESONA30
C The rho(1700) resonance                                               RESONA31
      PARAMETER ( x1700 = 1.7120D0  , x17002 = x1700**2 )               RESONA32
C     PARAMETER ( g1700 = 0.2130D0  , g1700e = 0.0000035 ) ! ???        RESONA33
      PARAMETER ( g1700 = 0.2130D0  , g1700e = 0.0000012 )              RESONA34
C The Janot(2600) resonance                                             RESONA35
      PARAMETER ( x2600 = 2.6000D0  , x26002 = x2600**2 )               RESONA36
      PARAMETER ( g2600 = 0.7000D0  , g2600e = 0.0000055 )              RESONA37
C The Phi(1020) resonance                                               RESONA38
      PARAMETER ( x1020 = 1.0194D0  , x10202 = x1020**2 )               RESONA39
      PARAMETER ( g1020 = 0.0044D0  , g1020e = 0.00000137D0 )           RESONA40
C The Phi(1680) resonance                                               RESONA41
      PARAMETER ( x1680 = 1.6800D0  , x16802 = x1680**2 )               RESONA42
      PARAMETER ( g1680 = 0.1500D0  , g1680e = 0.00000045D0 )           RESONA43
C The J/Psi(1s) resonance                                               RESONA44
      PARAMETER ( x3097 = 3.0969D0  , x30972 = x3097**2 )               RESONA45
      PARAMETER ( g3097 = 0.000068D0, g3097e = 0.00000507D0 )           RESONA46
C The J/Psi(2s) resonance                                               RESONA47
      PARAMETER ( x3685 = 3.6860D0  , x36852 = x3685**2 )               RESONA48
      PARAMETER ( g3685 = 0.000243D0, g3685e = 0.00000214D0 )           RESONA49
C The Psi(3770) resonance                                               RESONA50
      PARAMETER ( x3770 = 3.7699D0  , x37702 = x3770**2 )               RESONA51
      PARAMETER ( g3770 = 0.0236D0  , g3770e = 0.00000024D0 )           RESONA52
C The Psi(4040) resonance                                               RESONA53
      PARAMETER ( x4040 = 4.0400D0  , x40402 = x4040**2 )               RESONA54
      PARAMETER ( g4040 = 0.0520D0  , g4040e = 0.00000075D0 )           RESONA55
C The Psi(4160) resonance                                               RESONA56
      PARAMETER ( x4160 = 4.1590D0  , x41602 = x4160**2 )               RESONA57
      PARAMETER ( g4160 = 0.0780D0  , g4160e = 0.00000077D0 )           RESONA58
C The Psi(4415) resonance                                               RESONA59
      PARAMETER ( x4415 = 4.4150D0  , x44152 = x4415**2 )               RESONA60
      PARAMETER ( g4415 = 0.0430D0  , g4415e = 0.00000047D0 )           RESONA61
C The Janot(7000) resonance                                             RESONA62
      PARAMETER ( x7000 = 6.6000D0  , x70002 = x7000**2 )               RESONA63
      PARAMETER ( g7000 = 1.2500D0  , g7000e = 0.00000450D0 )           RESONA64
C The Upsilon(1s)(9460) resonance                                       RESONA65
      PARAMETER ( x9460 = 9.4603D0  , x94602 = x9460**2 )               RESONA66
      PARAMETER ( g9460 = 0.000052D0, g9460e = 0.00000134D0 )           RESONA67
C The Upsilon(2s)(10023) resonance                                      RESONA68
      PARAMETER ( x0023 = 10.023D0  , x00232 = x0023**2 )               RESONA69
      PARAMETER ( g0023 = 0.000043D0, g0023e = 0.00000059D0 )           RESONA70
C The Upsilon(3s)(10355) resonance                                      RESONA71
      PARAMETER ( x0355 = 10.355D0  , x03552 = x0355**2 )               RESONA72
      PARAMETER ( g0355 = 0.000024D0, g0355e = 0.00000044D0 )           RESONA73
C The Upsilon(4s)(10580) resonance                                      RESONA74
      PARAMETER ( x0580 = 10.580D0  , x05802 = x0580**2 )               RESONA75
      PARAMETER ( g0580 = 0.0238D0  , g0580e = 0.00000024D0 )           RESONA76
C The Upsilon(10860) resonance                                          RESONA77
      PARAMETER ( x0860 = 10.865D0  , x08602 = x0860**2 )               RESONA78
      PARAMETER ( g0860 = 0.1100D0  , g0860e = 0.00000031D0 )           RESONA79
C The Upsilon(11020) resonance                                          RESONA80
      PARAMETER ( x1111 = 11.019D0  , x11112 = x1111**2 )               RESONA81
      PARAMETER ( g1111 = 0.0790D0  , g1111e = 0.00000013D0 )           RESONA82
C And the common y affering                                             RESONA83
      COMMON / resonance / rparam(4,10,5), nres(5)                      RESONA84
      COMMON / extrmes   / rmin(10), rmax(10)                           RESONA85
C                                                                       PIFORM15
      COMPLEX*16 gg, formpi                                             PIFORM16
C                                                                       PIFORM17
      q2 = s/4.-xmpi2                                                   PIFORM18
      qq = SQRT(q2)                                                     PIFORM19
      ss = SQRT(s)                                                      PIFORM20
C                                                                       PIFORM21
      IF ( ss.GT. 2.*xmpi .AND. ss .LT. 1.5 ) THEN                      PIFORM22
        ffre = 1./pi*(xmpi2-s/3.)                                       PIFORM23
     .       + 2./pi*qq**3/ss*DLOG((ss+2.*qq)/(2.*xmpi))                PIFORM24
        ffim = -qq**3/ss                                                PIFORM25
C                                                                       PIFORM26
        formpi = (DCMPLX(a1-a2*xmpi2)/DCMPLX(a1+a2*q2+ffre,ffim)        PIFORM27
     .         +  DCMPLX(a3)*CDEXP(DCMPLX(0D0,a4))                      PIFORM28
     .         *  DCMPLX(xmom2)                                         PIFORM29
     .         /  DCMPLX(s-xmom2,xmom*gmom))                            PIFORM30
C                                                                       PIFORM31
        IF ( ss .LT. xmpi+xmom ) THEN                                   PIFORM32
          gg = DCMPLX(xm2/(s-xm2))                                      PIFORM33
        ELSE                                                            PIFORM34
          gg = DCMPLX(xm2)/DCMPLX(s-xm2,xm*xg)                          PIFORM35
        ENDIF                                                           PIFORM36
C                                                                       PIFORM37
        formpi = formpi * gg**pow                                       PIFORM38
        piform = CDABS(formpi)**2                                       PIFORM39
      ELSE                                                              PIFORM40
        piform = 0D0                                                    PIFORM41
      ENDIF                                                             PIFORM42
C                                                                       PIFORM43
      RETURN                                                            PIFORM44
      END                                                               PIFORM45
      SUBROUTINE QCD1(smin,smax,ind1,ind2,qcdf)                         QCD1   2
C-----------------------------------------------------------------------QCD1   3
C!This part initializes the QCD correction to gamma* --> qqbar          QCD1   4
C It calculates some quantities, and performs the numerical             QCD1   5
C integration over the qqbar squared invariant mass spectrum.           QCD1   6
C                                                                       QCD1   7
C Inputs ...                                                            QCD1   8
C  o SMIN  : Minimum value for M_qqbar**2                               QCD1   9
C  o SMAX  : Minimum value for M_qqbar**2                               QCD1  10
C  o IND1  :   = 1 for uubar                                            QCD1  11
C              = 2 for ddbar                                            QCD1  12
C              = 3 for ssbar                                            QCD1  13
C              = 4 for ccbar                                            QCD1  14
C              = 5 for bbbar                                            QCD1  15
C  o IND2  :   = 1 for Annhilation integration type                     QCD1  16
C              = 2 for brem-forw.  integration type                     QCD1  17
C              = 3 for brem-back.  integration type                     QCD1  18
C              = 4 Convs1          integration type                     QCD1  19
C              = 5 Convs2          integration type                     QCD1  20
C              = 6 Convz1          integration type                     QCD1  21
C              = 7 Convz2          integration type                     QCD1  22
C              = 8 Multip          integration type                     QCD1  23
C                                                                       QCD1  24
C Output ...                                                            QCD1  25
C QCDF  : The QCD correction factor                                     QCD1  26
C                                                                       QCD1  27
C Patrick Janot -- 01 Apr 1994                                          QCD1  28
C                                                                       QCD1  29
C-----------------------------------------------------------------------QCD1  30
      IMPLICIT REAL*8(A-H,O-Z)                                          QCD1  31
C The pi value, you shouldn't touch !                                   RESONA 2
      PARAMETER ( pi = 3.1415926353 )                                   RESONA 3
C Alpha QED                                                             RESONA 4
      PARAMETER ( alpha = 1./137.0 )                                    RESONA 5
C The normalized mu+mu- cross section                                   RESONA 6
      PARAMETER ( cmumu = 4.*pi*alpha**2/3. )                           RESONA 7
C The mu,pi, K, D0  masses                                              RESONA 8
      PARAMETER ( xmu  = 0.10565839D0, xmu2  = xmu **2 )                RESONA 9
      PARAMETER ( xmpi = 0.13956755D0, xmpi2 = xmpi**2 )                RESONA10
      PARAMETER ( xmka = 0.493646D0  , xmka2 = xmka**2 )                RESONA11
      PARAMETER ( xmd0 = 1.8695D0    , xmd02 = xmd0**2 )                RESONA12
      PARAMETER ( xmb0 = 5.2776D0    , xmb02 = xmb0**2 )                RESONA13
C The pion form factor                                                  RESONA14
      PARAMETER ( a1 = 0.29020D0, a2 = -2.301D0 )                       RESONA15
      PARAMETER ( a3 = -0.0121D0, a4 =  1.849D0 )                       RESONA16
      PARAMETER ( xm = 1.2D0, xm2 = xm**2, xg = 0.15D0, pow = 0.22D0 )  RESONA17
C The omega resonance                                                   RESONA18
      PARAMETER ( xmom = 0.78195D0 , xmom2 = xmom**2 )                  RESONA19
      PARAMETER ( gmom = 0.00843D0 , gmome = 0.0000006 )                RESONA20
C The omega(1390) resonance                                             RESONA21
      PARAMETER ( x1390 = 1.3910D0  , x13902 = x1390**2 )               RESONA22
      PARAMETER ( g1390 = 0.2240D0  , g1390e = 0.0000002D0 )            RESONA23
C The rho(1450) resonance                                               RESONA24
      PARAMETER ( x1450 = 1.4500D0  , x14502 = x1450**2 )               RESONA25
C     PARAMETER ( g1450 = 0.2370D0  , g1450e = 0.0000025D0 ) ! ???      RESONA26
      PARAMETER ( g1450 = 0.2370D0  , g1450e = 0.0000010D0 )            RESONA27
C The omega(1600) resonance                                             RESONA28
      PARAMETER ( x1600 = 1.5940D0  , x16002 = x1600**2 )               RESONA29
      PARAMETER ( g1600 = 0.1000D0  , g1600e = 0.0000002D0 )            RESONA30
C The rho(1700) resonance                                               RESONA31
      PARAMETER ( x1700 = 1.7120D0  , x17002 = x1700**2 )               RESONA32
C     PARAMETER ( g1700 = 0.2130D0  , g1700e = 0.0000035 ) ! ???        RESONA33
      PARAMETER ( g1700 = 0.2130D0  , g1700e = 0.0000012 )              RESONA34
C The Janot(2600) resonance                                             RESONA35
      PARAMETER ( x2600 = 2.6000D0  , x26002 = x2600**2 )               RESONA36
      PARAMETER ( g2600 = 0.7000D0  , g2600e = 0.0000055 )              RESONA37
C The Phi(1020) resonance                                               RESONA38
      PARAMETER ( x1020 = 1.0194D0  , x10202 = x1020**2 )               RESONA39
      PARAMETER ( g1020 = 0.0044D0  , g1020e = 0.00000137D0 )           RESONA40
C The Phi(1680) resonance                                               RESONA41
      PARAMETER ( x1680 = 1.6800D0  , x16802 = x1680**2 )               RESONA42
      PARAMETER ( g1680 = 0.1500D0  , g1680e = 0.00000045D0 )           RESONA43
C The J/Psi(1s) resonance                                               RESONA44
      PARAMETER ( x3097 = 3.0969D0  , x30972 = x3097**2 )               RESONA45
      PARAMETER ( g3097 = 0.000068D0, g3097e = 0.00000507D0 )           RESONA46
C The J/Psi(2s) resonance                                               RESONA47
      PARAMETER ( x3685 = 3.6860D0  , x36852 = x3685**2 )               RESONA48
      PARAMETER ( g3685 = 0.000243D0, g3685e = 0.00000214D0 )           RESONA49
C The Psi(3770) resonance                                               RESONA50
      PARAMETER ( x3770 = 3.7699D0  , x37702 = x3770**2 )               RESONA51
      PARAMETER ( g3770 = 0.0236D0  , g3770e = 0.00000024D0 )           RESONA52
C The Psi(4040) resonance                                               RESONA53
      PARAMETER ( x4040 = 4.0400D0  , x40402 = x4040**2 )               RESONA54
      PARAMETER ( g4040 = 0.0520D0  , g4040e = 0.00000075D0 )           RESONA55
C The Psi(4160) resonance                                               RESONA56
      PARAMETER ( x4160 = 4.1590D0  , x41602 = x4160**2 )               RESONA57
      PARAMETER ( g4160 = 0.0780D0  , g4160e = 0.00000077D0 )           RESONA58
C The Psi(4415) resonance                                               RESONA59
      PARAMETER ( x4415 = 4.4150D0  , x44152 = x4415**2 )               RESONA60
      PARAMETER ( g4415 = 0.0430D0  , g4415e = 0.00000047D0 )           RESONA61
C The Janot(7000) resonance                                             RESONA62
      PARAMETER ( x7000 = 6.6000D0  , x70002 = x7000**2 )               RESONA63
      PARAMETER ( g7000 = 1.2500D0  , g7000e = 0.00000450D0 )           RESONA64
C The Upsilon(1s)(9460) resonance                                       RESONA65
      PARAMETER ( x9460 = 9.4603D0  , x94602 = x9460**2 )               RESONA66
      PARAMETER ( g9460 = 0.000052D0, g9460e = 0.00000134D0 )           RESONA67
C The Upsilon(2s)(10023) resonance                                      RESONA68
      PARAMETER ( x0023 = 10.023D0  , x00232 = x0023**2 )               RESONA69
      PARAMETER ( g0023 = 0.000043D0, g0023e = 0.00000059D0 )           RESONA70
C The Upsilon(3s)(10355) resonance                                      RESONA71
      PARAMETER ( x0355 = 10.355D0  , x03552 = x0355**2 )               RESONA72
      PARAMETER ( g0355 = 0.000024D0, g0355e = 0.00000044D0 )           RESONA73
C The Upsilon(4s)(10580) resonance                                      RESONA74
      PARAMETER ( x0580 = 10.580D0  , x05802 = x0580**2 )               RESONA75
      PARAMETER ( g0580 = 0.0238D0  , g0580e = 0.00000024D0 )           RESONA76
C The Upsilon(10860) resonance                                          RESONA77
      PARAMETER ( x0860 = 10.865D0  , x08602 = x0860**2 )               RESONA78
      PARAMETER ( g0860 = 0.1100D0  , g0860e = 0.00000031D0 )           RESONA79
C The Upsilon(11020) resonance                                          RESONA80
      PARAMETER ( x1111 = 11.019D0  , x11112 = x1111**2 )               RESONA81
      PARAMETER ( g1111 = 0.0790D0  , g1111e = 0.00000013D0 )           RESONA82
C And the common y affering                                             RESONA83
      COMMON / resonance / rparam(4,10,5), nres(5)                      RESONA84
      COMMON / extrmes   / rmin(10), rmax(10)                           RESONA85
      DIMENSION X(1000,10,5),YY(1000,10,5)                              QCD1  33
      DIMENSION F(1000),A(1000),Y(1000),Z(1000),XNEW(1000)              QCD1  34
      DIMENSION iflag(10),xmin(10,5),xmax(10,5)                         QCD1  35
      DIMENSION binary(10)                                              QCD1  36
      LOGICAL first                                                     QCD1  37
      DATA binary/512,256,128,64,32,16,8,4,2,1/                         QCD1  38
      DATA first /.TRUE./                                               QCD1  39
C                                                                       QCD1  40
      IF ( first ) THEN                                                 QCD1  41
        CALL resinit                                                    QCD1  42
        first =.FALSE.                                                  QCD1  43
      ENDIF                                                             QCD1  44
C                                                                       QCD1  45
C Parameters of numerical integration step                              QCD1  46
C   o XMIN and XMAX : lower and upper integration bounds                QCD1  47
C   o ITER          : number of iterations                              QCD1  48
C   o N             : number of integration steps                       QCD1  49
C                                                                       QCD1  50
      xmin(ind2,ind1) = DMAX1(SQRT(smin),rmin(ind1))                    QCD1  51
      xmax(ind2,ind1) =       SQRT(smax)                                QCD1  52
      smin            = xmin(ind2,ind1)**2                              QCD1  53
      smax            = xmax(ind2,ind1)**2                              QCD1  54
      n = 1000                                                          QCD1  55
      iter = 6                                                          QCD1  56
      IF ( ind1 .GE. 4 ) iter = 14                                      QCD1  57
C                                                                       QCD1  58
C Initialize by choosing equidistant u values ( du = Cx**(beta-1)dx)    QCD1  59
C with an increased sampling around mz.                                 QCD1  60
C                                                                       QCD1  61
      WRITE(6,*) '----------------------------------------'             QCD1  62
      WRITE(6,*) '       Compute QCD corrections          '             QCD1  63
      WRITE(6,*) '       for integration # ',ind2                       QCD1  64
      WRITE(6,*) '----------------------------------------'             QCD1  65
      IT=0                                                              QCD1  66
      M=N-1                                                             QCD1  67
      DU=(xmax(ind2,ind1)-xmin(ind2,ind1))/FLOAT(m)                     QCD1  68
      X(1,IND2,ind1)=xmin(ind2,ind1)                                    QCD1  69
      DO 101 I=2,N                                                      QCD1  70
  101 X(I,IND2,ind1)=X(I-1,IND2,ind1)+DU                                QCD1  71
C                                                                       QCD1  72
      DO 99 I=1,N                                                       QCD1  73
        X(I,ind2,ind1)=X(I,ind2,ind1)**2                                QCD1  74
   99 CONTINUE                                                          QCD1  75
C                                                                       QCD1  76
C Starting point for iterations                                         QCD1  77
C                                                                       QCD1  78
  100 CONTINUE                                                          QCD1  79
      CALL vzero(iflag(1),10)                                           QCD1  80
      DO 98 ires = 1, nres(ind1)                                        QCD1  81
   98 IF ( smin .GT. rparam(4,ires,ind1) ) iflag(ires) = 1              QCD1  82
C                                                                       QCD1  83
      IF ( it .LT. iter/2 ) THEN                                        QCD1  84
        DO 97 I = 1, M                                                  QCD1  85
          DO 96 ires = 1, nres(ind1)                                    QCD1  86
            IF ( X(I,ind2,ind1) .GE. rparam(4,ires,ind1) .AND.          QCD1  87
     .                               iflag(ires) .EQ. 0) THEN           QCD1  88
              X(I,ind2,ind1) = rparam(4,ires,ind1)                      QCD1  89
              iflag(ires) = 1                                           QCD1  90
            ENDIF                                                       QCD1  91
   96     CONTINUE                                                      QCD1  92
C                                                                       QCD1  93
C       IF ( I .GT. 2 ) THEN                                            QCD1  94
C         IF ( X(I  ,ind2,ind1)-X(I-1,ind2,ind1) .GT.                   QCD1  95
C    .    4. *(X(I-1,ind2,ind1)-X(I-2,ind2,ind1)) ) THEN                QCD1  96
C           X(I,ind2,ind1)=(X(I,ind2,ind1)+2.*(X(I-1,ind2,ind1)))/3.    QCD1  97
C         ENDIF                                                         QCD1  98
C       ENDIF                                                           QCD1  99
   97   CONTINUE                                                        QCD1 100
      ENDIF                                                             QCD1 101
C     DO 111 i=1,n                                                      QCD1 102
C 111 WRITE(6,*) i,SQRT(x(i,ind2,ind1)),rratio(x(i,ind2,ind1),ind1)     QCD1 103
C                                                                       QCD1 104
C Calculate function values                                             QCD1 105
C                                                                       QCD1 106
      DO 102 I=1,N                                                      QCD1 107
      ss = x(i,ind2,ind1)                                               QCD1 108
C 102 F(I) =  CROSV(ss,ind2)                                            QCD1 109
  102 F(I) =  RRATIO(ss,ind1)* CROSV(ss,ind2)                           QCD1 110
C 102 F(I) =  RRATIO(ss,ind1)                                           QCD1 111
C                                                                       QCD1 112
C CALCULATE BIN AREAS                                                   QCD1 113
C                                                                       QCD1 114
      DO 103 I=1,M                                                      QCD1 115
  103 A(I)=(X(I+1,IND2,ind1)-X(I,IND2,ind1))*(F(I+1)+F(I))/2.           QCD1 116
C                                                                       QCD1 117
C Calculate cumulative spectrum Y values                                QCD1 118
C                                                                       QCD1 119
      Y(1)=0.D0                                                         QCD1 120
      DO 104 I=2,N                                                      QCD1 121
  104 Y(I)=Y(I-1)+A(I-1)                                                QCD1 122
C                                                                       QCD1 123
C Put equidistant points on Y scale                                     QCD1 124
C                                                                       QCD1 125
      DZ=Y(N)/FLOAT(M)                                                  QCD1 126
      Z(1)=0.D0                                                         QCD1 127
      DO 105 I=2,N                                                      QCD1 128
  105 Z(I)=Z(I-1)+DZ                                                    QCD1 129
C                                                                       QCD1 130
C Determine spacing of Z points in between Y points. From this,         QCD1 131
C determine new X values and finally replace old values                 QCD1 132
C                                                                       QCD1 133
      XNEW(1)=X(1,IND2,ind1)                                            QCD1 134
      XNEW(N)=X(N,IND2,ind1)                                            QCD1 135
      K=1                                                               QCD1 136
      DO 108 I=2,M                                                      QCD1 137
  106 IF( Y(K+1) .GT. Z(I) ) GOTO 107                                   QCD1 138
      K=K+1                                                             QCD1 139
      GOTO 106                                                          QCD1 140
  107 R= ( Z(I) - Y(K) ) / ( Y(K+1) - Y(K) )                            QCD1 141
  108 XNEW(I) = X(K,IND2,ind1)                                          QCD1 142
     .        + ( X(K+1,IND2,ind1)-X(K,IND2,ind1) )*R                   QCD1 143
      DO 109 I=1,N                                                      QCD1 144
  109 X(I,IND2,ind1)=XNEW(I)                                            QCD1 145
C                                                                       QCD1 146
c Check on end of iterations and return                                 QCD1 147
C                                                                       QCD1 148
      IT=IT+1                                                           QCD1 149
      IF ( it .LE. iter/2 ) THEN                                        QCD1 150
        SIG1 = 0.                                                       QCD1 151
        NT = 1                                                          QCD1 152
      ELSE                                                              QCD1 153
        NT = IT-iter/2+1                                                QCD1 154
      ENDIF                                                             QCD1 155
      SIG1 = SIG1+Y(M)                                                  QCD1 156
      PRINT 3,IT,SIG1/FLOAT(NT)                                         QCD1 157
    3 FORMAT(' Iteration # ',i3,'  Integral =',e15.6)                   QCD1 158
      IF(IT.LT.ITER) GOTO 100                                           QCD1 159
C                                                                       QCD1 160
c Present results in form of correction                                 QCD1 161
C                                                                       QCD1 162
C     do 110 i=1,n                                                      QCD1 163
C 110 WRITE(6,*) i,sqrt(x(i,ind2,ind1)),rratio(x(i,ind2,ind1),ind1)     QCD1 164
      CALL ucopy(y(1),yy(1,ind2,ind1),2*n)                              QCD1 165
      SIG0 = CROST(smin,smax,ind2)                                      QCD1 166
      SIG1 = SIG1/FLOAT(NT)                                             QCD1 167
      QCDF = SIG1/SIG0                                                  QCD1 168
      PRINT 4,SIG0,SIG1,QCDF                                            QCD1 169
    4 FORMAT(/' Results of the initialization step :',/,                QCD1 170
     .        ' Noncorrected cross section :',e15.6,/,                  QCD1 171
     .        '    Corrected cross section :',e15.6,/,                  QCD1 172
     .        '    QCD correction          :',f10.3,' %',/,             QCD1 173
     .       ' ',72(1H=))                                               QCD1 174
      RETURN                                                            QCD1 175
      ENTRY QCD2(ind1,ind2,umin,umax,uk)                                QCD1 176
C-----------------------------------------------------------------------QCD1 177
C! This part generates the invariant mass squared of the qqbar system.  QCD1 178
C  The invariant mass spectrum must have been examined by calling QCD1  QCD1 179
C  before the first call to this entry.                                 QCD1 180
C                                                                       QCD1 181
C  Inputs :                                                             QCD1 182
C    o IND1     : the kind of quarks                                    QCD1 183
C    o IND2     : the type of phase space integration                   QCD1 184
C    o UMIN/MAX : the lower and upper bound for the generation          QCD1 185
C                                                                       QCD1 186
C  Output :                                                             QCD1 187
C    o UK       : the generated value of the invariant mass squared     QCD1 188
C                                                                       QCD1 189
C  Patrick Janot -- 01 Apr 1994                                         QCD1 190
C                                                                       QCD1 191
C-----------------------------------------------------------------------QCD1 192
C                                                                       QCD1 193
C Find the bin corresponding to umax (umin is assumned to be the        QCD1 194
C first bin)                                                            QCD1 195
C                                                                       QCD1 196
      l = m                                                             QCD1 197
      DO 200 ibin = 1, 10                                               QCD1 198
        IF     ( umax .GT. x(l,ind2,ind1) ) THEN                        QCD1 199
          IF ( ibin .EQ. 1 ) GOTO 202                                   QCD1 200
          l = l + binary(ibin)                                          QCD1 201
        ELSEIF ( umax .LT. x(l,ind2,ind1) ) THEN                        QCD1 202
          l = l - binary(ibin)                                          QCD1 203
        ELSE                                                            QCD1 204
          GOTO 201                                                      QCD1 205
        ENDIF                                                           QCD1 206
  200 CONTINUE                                                          QCD1 207
  201 CONTINUE                                                          QCD1 208
      IF ( umax .LT. x(l,ind2,ind1) ) l = l-1                           QCD1 209
C                                                                       QCD1 210
C Generate the qqbar invariant mass squared from cumulative spectrum    QCD1 211
C bins                                                                  QCD1 212
C                                                                       QCD1 213
  202 R=L*RN(1.)                                                        QCD1 214
      I=INT(R)                                                          QCD1 215
      S=R-I                                                             QCD1 216
      UK = X(I+1,IND2,ind1)                                             QCD1 217
     .   + S*( X(I+2,IND2,ind1)-X(I+1,IND2,ind1) )                      QCD1 218
      IF ( uk .GT. umax ) GOTO 202                                      QCD1 219
C                                                                       QCD1 220
      RETURN                                                            QCD1 221
      ENTRY QCD3(ind1,ind2,umin,umax,utot)                              QCD1 222
C-----------------------------------------------------------------------QCD1 223
C! This part computes the integral between umax and umin                QCD1 224
C                                                                       QCD1 225
C  Inputs :                                                             QCD1 226
C    o IND1     : the kind of quarks                                    QCD1 227
C    o IND2     : the type of phase space integration                   QCD1 228
C    o UMIN/MAX : the lower and upper bound for the generation          QCD1 229
C                                                                       QCD1 230
C  Output :                                                             QCD1 231
C    o UTOT     : the value of the integral between UMIN and UMAX       QCD1 232
C                                                                       QCD1 233
C  Patrick Janot -- 01 Apr 1994                                         QCD1 234
C                                                                       QCD1 235
C-----------------------------------------------------------------------QCD1 236
C                                                                       QCD1 237
C Find the bin corresponding to umax (umin is assumned to be the        QCD1 238
C first bin)                                                            QCD1 239
C                                                                       QCD1 240
      l = m                                                             QCD1 241
      DO 300 ibin = 1, 10                                               QCD1 242
        IF     ( umax .GT. x(l,ind2,ind1) ) THEN                        QCD1 243
          IF ( ibin .EQ. 1 ) GOTO 302                                   QCD1 244
          l = l + binary(ibin)                                          QCD1 245
        ELSEIF ( umax .LT. x(l,ind2,ind1) ) THEN                        QCD1 246
          l = l - binary(ibin)                                          QCD1 247
        ELSE                                                            QCD1 248
          GOTO 301                                                      QCD1 249
        ENDIF                                                           QCD1 250
  300 CONTINUE                                                          QCD1 251
  301 CONTINUE                                                          QCD1 252
      IF     ( umax .LT. x(l,ind2,ind1) ) l = l-1                       QCD1 253
C                                                                       QCD1 254
  302 utot = yy(l+1,ind2,ind1)                                          QCD1 255
     .     -(yy(l+1,ind2,ind1)-yy(l,ind2,ind1))                         QCD1 256
     .     /( x(l+1,ind2,ind1)- x(l,ind2,ind1))                         QCD1 257
     .     *( x(l+1,ind2,ind1)- umax          )                         QCD1 258
C                                                                       QCD1 259
      RETURN                                                            QCD1 260
      END                                                               QCD1 261
      SUBROUTINE RESINIT                                                RESINIT2
C-----------------------------------------------------------------      RESINIT3
C! Put all the resonance parameters in a single array                   RESINIT4
C                                                                       RESINIT5
C  Patrick Janot -- 02 Apr 1994                                         RESINIT6
C-----------------------------------------------------------------      RESINIT7
      IMPLICIT REAL*8 (A-H,O-Z)                                         RESINIT8
C The pi value, you shouldn't touch !                                   RESONA 2
      PARAMETER ( pi = 3.1415926353 )                                   RESONA 3
C Alpha QED                                                             RESONA 4
      PARAMETER ( alpha = 1./137.0 )                                    RESONA 5
C The normalized mu+mu- cross section                                   RESONA 6
      PARAMETER ( cmumu = 4.*pi*alpha**2/3. )                           RESONA 7
C The mu,pi, K, D0  masses                                              RESONA 8
      PARAMETER ( xmu  = 0.10565839D0, xmu2  = xmu **2 )                RESONA 9
      PARAMETER ( xmpi = 0.13956755D0, xmpi2 = xmpi**2 )                RESONA10
      PARAMETER ( xmka = 0.493646D0  , xmka2 = xmka**2 )                RESONA11
      PARAMETER ( xmd0 = 1.8695D0    , xmd02 = xmd0**2 )                RESONA12
      PARAMETER ( xmb0 = 5.2776D0    , xmb02 = xmb0**2 )                RESONA13
C The pion form factor                                                  RESONA14
      PARAMETER ( a1 = 0.29020D0, a2 = -2.301D0 )                       RESONA15
      PARAMETER ( a3 = -0.0121D0, a4 =  1.849D0 )                       RESONA16
      PARAMETER ( xm = 1.2D0, xm2 = xm**2, xg = 0.15D0, pow = 0.22D0 )  RESONA17
C The omega resonance                                                   RESONA18
      PARAMETER ( xmom = 0.78195D0 , xmom2 = xmom**2 )                  RESONA19
      PARAMETER ( gmom = 0.00843D0 , gmome = 0.0000006 )                RESONA20
C The omega(1390) resonance                                             RESONA21
      PARAMETER ( x1390 = 1.3910D0  , x13902 = x1390**2 )               RESONA22
      PARAMETER ( g1390 = 0.2240D0  , g1390e = 0.0000002D0 )            RESONA23
C The rho(1450) resonance                                               RESONA24
      PARAMETER ( x1450 = 1.4500D0  , x14502 = x1450**2 )               RESONA25
C     PARAMETER ( g1450 = 0.2370D0  , g1450e = 0.0000025D0 ) ! ???      RESONA26
      PARAMETER ( g1450 = 0.2370D0  , g1450e = 0.0000010D0 )            RESONA27
C The omega(1600) resonance                                             RESONA28
      PARAMETER ( x1600 = 1.5940D0  , x16002 = x1600**2 )               RESONA29
      PARAMETER ( g1600 = 0.1000D0  , g1600e = 0.0000002D0 )            RESONA30
C The rho(1700) resonance                                               RESONA31
      PARAMETER ( x1700 = 1.7120D0  , x17002 = x1700**2 )               RESONA32
C     PARAMETER ( g1700 = 0.2130D0  , g1700e = 0.0000035 ) ! ???        RESONA33
      PARAMETER ( g1700 = 0.2130D0  , g1700e = 0.0000012 )              RESONA34
C The Janot(2600) resonance                                             RESONA35
      PARAMETER ( x2600 = 2.6000D0  , x26002 = x2600**2 )               RESONA36
      PARAMETER ( g2600 = 0.7000D0  , g2600e = 0.0000055 )              RESONA37
C The Phi(1020) resonance                                               RESONA38
      PARAMETER ( x1020 = 1.0194D0  , x10202 = x1020**2 )               RESONA39
      PARAMETER ( g1020 = 0.0044D0  , g1020e = 0.00000137D0 )           RESONA40
C The Phi(1680) resonance                                               RESONA41
      PARAMETER ( x1680 = 1.6800D0  , x16802 = x1680**2 )               RESONA42
      PARAMETER ( g1680 = 0.1500D0  , g1680e = 0.00000045D0 )           RESONA43
C The J/Psi(1s) resonance                                               RESONA44
      PARAMETER ( x3097 = 3.0969D0  , x30972 = x3097**2 )               RESONA45
      PARAMETER ( g3097 = 0.000068D0, g3097e = 0.00000507D0 )           RESONA46
C The J/Psi(2s) resonance                                               RESONA47
      PARAMETER ( x3685 = 3.6860D0  , x36852 = x3685**2 )               RESONA48
      PARAMETER ( g3685 = 0.000243D0, g3685e = 0.00000214D0 )           RESONA49
C The Psi(3770) resonance                                               RESONA50
      PARAMETER ( x3770 = 3.7699D0  , x37702 = x3770**2 )               RESONA51
      PARAMETER ( g3770 = 0.0236D0  , g3770e = 0.00000024D0 )           RESONA52
C The Psi(4040) resonance                                               RESONA53
      PARAMETER ( x4040 = 4.0400D0  , x40402 = x4040**2 )               RESONA54
      PARAMETER ( g4040 = 0.0520D0  , g4040e = 0.00000075D0 )           RESONA55
C The Psi(4160) resonance                                               RESONA56
      PARAMETER ( x4160 = 4.1590D0  , x41602 = x4160**2 )               RESONA57
      PARAMETER ( g4160 = 0.0780D0  , g4160e = 0.00000077D0 )           RESONA58
C The Psi(4415) resonance                                               RESONA59
      PARAMETER ( x4415 = 4.4150D0  , x44152 = x4415**2 )               RESONA60
      PARAMETER ( g4415 = 0.0430D0  , g4415e = 0.00000047D0 )           RESONA61
C The Janot(7000) resonance                                             RESONA62
      PARAMETER ( x7000 = 6.6000D0  , x70002 = x7000**2 )               RESONA63
      PARAMETER ( g7000 = 1.2500D0  , g7000e = 0.00000450D0 )           RESONA64
C The Upsilon(1s)(9460) resonance                                       RESONA65
      PARAMETER ( x9460 = 9.4603D0  , x94602 = x9460**2 )               RESONA66
      PARAMETER ( g9460 = 0.000052D0, g9460e = 0.00000134D0 )           RESONA67
C The Upsilon(2s)(10023) resonance                                      RESONA68
      PARAMETER ( x0023 = 10.023D0  , x00232 = x0023**2 )               RESONA69
      PARAMETER ( g0023 = 0.000043D0, g0023e = 0.00000059D0 )           RESONA70
C The Upsilon(3s)(10355) resonance                                      RESONA71
      PARAMETER ( x0355 = 10.355D0  , x03552 = x0355**2 )               RESONA72
      PARAMETER ( g0355 = 0.000024D0, g0355e = 0.00000044D0 )           RESONA73
C The Upsilon(4s)(10580) resonance                                      RESONA74
      PARAMETER ( x0580 = 10.580D0  , x05802 = x0580**2 )               RESONA75
      PARAMETER ( g0580 = 0.0238D0  , g0580e = 0.00000024D0 )           RESONA76
C The Upsilon(10860) resonance                                          RESONA77
      PARAMETER ( x0860 = 10.865D0  , x08602 = x0860**2 )               RESONA78
      PARAMETER ( g0860 = 0.1100D0  , g0860e = 0.00000031D0 )           RESONA79
C The Upsilon(11020) resonance                                          RESONA80
      PARAMETER ( x1111 = 11.019D0  , x11112 = x1111**2 )               RESONA81
      PARAMETER ( g1111 = 0.0790D0  , g1111e = 0.00000013D0 )           RESONA82
C And the common y affering                                             RESONA83
      COMMON / resonance / rparam(4,10,5), nres(5)                      RESONA84
      COMMON / extrmes   / rmin(10), rmax(10)                           RESONA85
C                                                                       RESINI10
      CALL vzero(rparam(1,1,1),400)                                     RESINI11
      CALL vzero(nres(1),5)                                             RESINI12
C                                                                       RESINI13
C The uubar and ddbar resonances                                        RESINI14
C                                                                       RESINI15
C The omega(783)                                                        RESINI16
C                                                                       RESINI17
      rparam(1,1,1) = xmom                                              RESINI18
      rparam(2,1,1) = gmom                                              RESINI19
      rparam(3,1,1) = gmome                                             RESINI20
      rparam(4,1,1) = xmom2                                             RESINI21
C                                                                       RESINI22
C The omega(1390)                                                       RESINI23
C                                                                       RESINI24
      rparam(1,2,1) = x1390                                             RESINI25
      rparam(2,2,1) = g1390                                             RESINI26
      rparam(3,2,1) = g1390e                                            RESINI27
      rparam(4,2,1) = x13902                                            RESINI28
C                                                                       RESINI29
C The rho(1450)                                                         RESINI30
C                                                                       RESINI31
      rparam(1,3,1) = x1450                                             RESINI32
      rparam(2,3,1) = g1450                                             RESINI33
      rparam(3,3,1) = g1450e                                            RESINI34
      rparam(4,3,1) = x14502                                            RESINI35
C                                                                       RESINI36
C The omega(1600)                                                       RESINI37
C                                                                       RESINI38
      rparam(1,4,1) = x1600                                             RESINI39
      rparam(2,4,1) = g1600                                             RESINI40
      rparam(3,4,1) = g1600e                                            RESINI41
      rparam(4,4,1) = x16002                                            RESINI42
C                                                                       RESINI43
C The rho(1700)                                                         RESINI44
C                                                                       RESINI45
      rparam(1,5,1) = x1700                                             RESINI46
      rparam(2,5,1) = g1700                                             RESINI47
      rparam(3,5,1) = g1700e                                            RESINI48
      rparam(4,5,1) = x17002                                            RESINI49
C                                                                       RESINI50
C The Janot(2600)                                                       RESINI51
C                                                                       RESINI52
      rparam(1,6,1) = x2600                                             RESINI53
      rparam(2,6,1) = g2600                                             RESINI54
      rparam(3,6,1) = g2600e                                            RESINI55
      rparam(4,6,1) = x26002                                            RESINI56
C                                                                       RESINI57
      nres(1) = 6                                                       RESINI58
      nres(2) = 6                                                       RESINI59
      DO ires = 1, nres(2)                                              RESINI60
        DO jpar = 1, 4                                                  RESINI61
          rparam(jpar,ires,2) = rparam(jpar,ires,1)                     RESINI62
        ENDDO                                                           RESINI63
      ENDDO                                                             RESINI64
C                                                                       RESINI65
      rmin(1) = 2.*xmpi                                                 RESINI66
      rmin(2) = rmin(1)                                                 RESINI67
C                                                                       RESINI68
C                                                                       RESINI69
C The ssbar resonances                                                  RESINI70
C                                                                       RESINI71
C The Phi(1020)                                                         RESINI72
C                                                                       RESINI73
      rparam(1,1,3) = x1020                                             RESINI74
      rparam(2,1,3) = g1020                                             RESINI75
      rparam(3,1,3) = g1020e                                            RESINI76
      rparam(4,1,3) = x10202                                            RESINI77
C                                                                       RESINI78
C The Phi(1680)                                                         RESINI79
C                                                                       RESINI80
      rparam(1,2,3) = x1680                                             RESINI81
      rparam(2,2,3) = g1680                                             RESINI82
      rparam(3,2,3) = g1680e                                            RESINI83
      rparam(4,2,3) = x16802                                            RESINI84
C                                                                       RESINI85
      nres(3) = 2                                                       RESINI86
C     rmin(3) = 2.*xmka  ! This is too low for JETSET. PJ-26/10/94      PJ941022
      rmin(3) = 1D0                                                     PJ941023
C                                                                       RESINI88
C The ccbar resonances                                                  RESINI89
C                                                                       RESINI90
C The J/Psi(3097)                                                       RESINI91
C                                                                       RESINI92
      rparam(1,1,4) = x3097                                             RESINI93
      rparam(2,1,4) = g3097                                             RESINI94
      rparam(3,1,4) = g3097e                                            RESINI95
      rparam(4,1,4) = x30972                                            RESINI96
C                                                                       RESINI97
C The Psi(3685)                                                         RESINI98
C                                                                       RESINI99
      rparam(1,2,4) = x3685                                             RESIN100
      rparam(2,2,4) = g3685                                             RESIN101
      rparam(3,2,4) = g3685e                                            RESIN102
      rparam(4,2,4) = x36852                                            RESIN103
C                                                                       RESIN104
C The Psi(3770)                                                         RESIN105
C                                                                       RESIN106
      rparam(1,3,4) = x3770                                             RESIN107
      rparam(2,3,4) = g3770                                             RESIN108
      rparam(3,3,4) = g3770e                                            RESIN109
      rparam(4,3,4) = x37702                                            RESIN110
C                                                                       RESIN111
C The Psi(4040)                                                         RESIN112
C                                                                       RESIN113
      rparam(1,4,4) = x4040                                             RESIN114
      rparam(2,4,4) = g4040                                             RESIN115
      rparam(3,4,4) = g4040e                                            RESIN116
      rparam(4,4,4) = x40402                                            RESIN117
C                                                                       RESIN118
C The Psi(4160)                                                         RESIN119
C                                                                       RESIN120
      rparam(1,5,4) = x4160                                             RESIN121
      rparam(2,5,4) = g4160                                             RESIN122
      rparam(3,5,4) = g4160e                                            RESIN123
      rparam(4,5,4) = x41602                                            RESIN124
C                                                                       RESIN125
C The Psi(4415)                                                         RESIN126
C                                                                       RESIN127
      rparam(1,6,4) = x4415                                             RESIN128
      rparam(2,6,4) = g4415                                             RESIN129
      rparam(3,6,4) = g4415e                                            RESIN130
      rparam(4,6,4) = x44152                                            RESIN131
C                                                                       RESIN132
C The Janot(7000)                                                       RESIN133
C                                                                       RESIN134
      rparam(1,7,4) = x7000                                             RESIN135
      rparam(2,7,4) = g7000                                             RESIN136
      rparam(3,7,4) = g7000e                                            RESIN137
      rparam(4,7,4) = x70002                                            RESIN138
C                                                                       RESIN139
      nres(4) = 7                                                       RESIN140
      rmin(4) = x3097-40.*g3097                                         RESIN141
C                                                                       RESIN142
C The bbbar resonances                                                  RESIN143
C                                                                       RESIN144
C The Upsilon(1s)                                                       RESIN145
C                                                                       RESIN146
      rparam(1,1,5) = x9460                                             RESIN147
      rparam(2,1,5) = g9460                                             RESIN148
      rparam(3,1,5) = g9460e                                            RESIN149
      rparam(4,1,5) = x94602                                            RESIN150
C                                                                       RESIN151
C The Upsilon(2s)                                                       RESIN152
C                                                                       RESIN153
      rparam(1,2,5) = x0023                                             RESIN154
      rparam(2,2,5) = g0023                                             RESIN155
      rparam(3,2,5) = g0023e                                            RESIN156
      rparam(4,2,5) = x00232                                            RESIN157
C                                                                       RESIN158
C The Upsilon(3s)                                                       RESIN159
C                                                                       RESIN160
      rparam(1,3,5) = x0355                                             RESIN161
      rparam(2,3,5) = g0355                                             RESIN162
      rparam(3,3,5) = g0355e                                            RESIN163
      rparam(4,3,5) = x03552                                            RESIN164
C                                                                       RESIN165
C The Upsilon(4s)                                                       RESIN166
C                                                                       RESIN167
      rparam(1,4,5) = x0580                                             RESIN168
      rparam(2,4,5) = g0580                                             RESIN169
      rparam(3,4,5) = g0580e                                            RESIN170
      rparam(4,4,5) = x05802                                            RESIN171
C                                                                       RESIN172
C The Upsilon(10860)                                                    RESIN173
C                                                                       RESIN174
      rparam(1,5,5) = x0860                                             RESIN175
      rparam(2,5,5) = g0860                                             RESIN176
      rparam(3,5,5) = g0860e                                            RESIN177
      rparam(4,5,5) = x08602                                            RESIN178
C                                                                       RESIN179
C The Upsilon(11020)                                                    RESIN180
C                                                                       RESIN181
      rparam(1,6,5) = x1111                                             RESIN182
      rparam(2,6,5) = g1111                                             RESIN183
      rparam(3,6,5) = g1111e                                            RESIN184
      rparam(4,6,5) = x11112                                            RESIN185
C                                                                       RESIN186
      nres(5) = 6                                                       RESIN187
      rmin(5) = x9460-40.*g9460                                         RESIN188
C                                                                       RESIN189
      RETURN                                                            RESIN190
      END                                                               RESIN191
      DOUBLE PRECISION FUNCTION rratio(s,ind1)                          RRATIO 2
C-----------------------------------------------------------------------RRATIO 3
C! The R ratio parametrization !                                        RRATIO 4
C                                                                       RRATIO 5
C Input...                                                              RRATIO 6
C o S    : the value of the qqbar invariant mass squared                RRATIO 7
C o IND1 : = 1 for uubar,                                               RRATIO 8
C          = 2 for ddbar,                                               RRATIO 9
C          = 3 for ssbar,                                               RRATIO10
C          = 4 for ccbar,                                               RRATIO11
C          = 5 for bbbar,                                               RRATIO12
C                                                                       RRATIO13
C  Patrick Janot -- 01 Apr 1994                                         RRATIO14
C                                                                       RRATIO15
C-----------------------------------------------------------------------RRATIO16
      IMPLICIT REAL*8 (A-H,O-Z)                                         RRATIO17
C The quark and lepton masses                                           QMASSES2
      REAL*8 amu, amd, ams, amc, amb, amt                               QMASSES3
      REAL*8 amel, ammu, amto, amne, amnm, amnt                         QMASSES4
      PARAMETER ( amu = .005D0, amd = .010d0, ams = .150d0 )            QMASSES5
      PARAMETER ( amc = 1.37d0, amb = 4.70d0, amt = 170.d0 )            QMASSES6
      PARAMETER ( amel= .511D-3, ammu= .1057D0, amto = 1.777d0 )        QMASSES7
      PARAMETER ( amne= 1D-4, amnm= 1D-4, amnt = 1D-4 )                 QMASSES8
C The pi value, you shouldn't touch !                                   RESONA 2
      PARAMETER ( pi = 3.1415926353 )                                   RESONA 3
C Alpha QED                                                             RESONA 4
      PARAMETER ( alpha = 1./137.0 )                                    RESONA 5
C The normalized mu+mu- cross section                                   RESONA 6
      PARAMETER ( cmumu = 4.*pi*alpha**2/3. )                           RESONA 7
C The mu,pi, K, D0  masses                                              RESONA 8
      PARAMETER ( xmu  = 0.10565839D0, xmu2  = xmu **2 )                RESONA 9
      PARAMETER ( xmpi = 0.13956755D0, xmpi2 = xmpi**2 )                RESONA10
      PARAMETER ( xmka = 0.493646D0  , xmka2 = xmka**2 )                RESONA11
      PARAMETER ( xmd0 = 1.8695D0    , xmd02 = xmd0**2 )                RESONA12
      PARAMETER ( xmb0 = 5.2776D0    , xmb02 = xmb0**2 )                RESONA13
C The pion form factor                                                  RESONA14
      PARAMETER ( a1 = 0.29020D0, a2 = -2.301D0 )                       RESONA15
      PARAMETER ( a3 = -0.0121D0, a4 =  1.849D0 )                       RESONA16
      PARAMETER ( xm = 1.2D0, xm2 = xm**2, xg = 0.15D0, pow = 0.22D0 )  RESONA17
C The omega resonance                                                   RESONA18
      PARAMETER ( xmom = 0.78195D0 , xmom2 = xmom**2 )                  RESONA19
      PARAMETER ( gmom = 0.00843D0 , gmome = 0.0000006 )                RESONA20
C The omega(1390) resonance                                             RESONA21
      PARAMETER ( x1390 = 1.3910D0  , x13902 = x1390**2 )               RESONA22
      PARAMETER ( g1390 = 0.2240D0  , g1390e = 0.0000002D0 )            RESONA23
C The rho(1450) resonance                                               RESONA24
      PARAMETER ( x1450 = 1.4500D0  , x14502 = x1450**2 )               RESONA25
C     PARAMETER ( g1450 = 0.2370D0  , g1450e = 0.0000025D0 ) ! ???      RESONA26
      PARAMETER ( g1450 = 0.2370D0  , g1450e = 0.0000010D0 )            RESONA27
C The omega(1600) resonance                                             RESONA28
      PARAMETER ( x1600 = 1.5940D0  , x16002 = x1600**2 )               RESONA29
      PARAMETER ( g1600 = 0.1000D0  , g1600e = 0.0000002D0 )            RESONA30
C The rho(1700) resonance                                               RESONA31
      PARAMETER ( x1700 = 1.7120D0  , x17002 = x1700**2 )               RESONA32
C     PARAMETER ( g1700 = 0.2130D0  , g1700e = 0.0000035 ) ! ???        RESONA33
      PARAMETER ( g1700 = 0.2130D0  , g1700e = 0.0000012 )              RESONA34
C The Janot(2600) resonance                                             RESONA35
      PARAMETER ( x2600 = 2.6000D0  , x26002 = x2600**2 )               RESONA36
      PARAMETER ( g2600 = 0.7000D0  , g2600e = 0.0000055 )              RESONA37
C The Phi(1020) resonance                                               RESONA38
      PARAMETER ( x1020 = 1.0194D0  , x10202 = x1020**2 )               RESONA39
      PARAMETER ( g1020 = 0.0044D0  , g1020e = 0.00000137D0 )           RESONA40
C The Phi(1680) resonance                                               RESONA41
      PARAMETER ( x1680 = 1.6800D0  , x16802 = x1680**2 )               RESONA42
      PARAMETER ( g1680 = 0.1500D0  , g1680e = 0.00000045D0 )           RESONA43
C The J/Psi(1s) resonance                                               RESONA44
      PARAMETER ( x3097 = 3.0969D0  , x30972 = x3097**2 )               RESONA45
      PARAMETER ( g3097 = 0.000068D0, g3097e = 0.00000507D0 )           RESONA46
C The J/Psi(2s) resonance                                               RESONA47
      PARAMETER ( x3685 = 3.6860D0  , x36852 = x3685**2 )               RESONA48
      PARAMETER ( g3685 = 0.000243D0, g3685e = 0.00000214D0 )           RESONA49
C The Psi(3770) resonance                                               RESONA50
      PARAMETER ( x3770 = 3.7699D0  , x37702 = x3770**2 )               RESONA51
      PARAMETER ( g3770 = 0.0236D0  , g3770e = 0.00000024D0 )           RESONA52
C The Psi(4040) resonance                                               RESONA53
      PARAMETER ( x4040 = 4.0400D0  , x40402 = x4040**2 )               RESONA54
      PARAMETER ( g4040 = 0.0520D0  , g4040e = 0.00000075D0 )           RESONA55
C The Psi(4160) resonance                                               RESONA56
      PARAMETER ( x4160 = 4.1590D0  , x41602 = x4160**2 )               RESONA57
      PARAMETER ( g4160 = 0.0780D0  , g4160e = 0.00000077D0 )           RESONA58
C The Psi(4415) resonance                                               RESONA59
      PARAMETER ( x4415 = 4.4150D0  , x44152 = x4415**2 )               RESONA60
      PARAMETER ( g4415 = 0.0430D0  , g4415e = 0.00000047D0 )           RESONA61
C The Janot(7000) resonance                                             RESONA62
      PARAMETER ( x7000 = 6.6000D0  , x70002 = x7000**2 )               RESONA63
      PARAMETER ( g7000 = 1.2500D0  , g7000e = 0.00000450D0 )           RESONA64
C The Upsilon(1s)(9460) resonance                                       RESONA65
      PARAMETER ( x9460 = 9.4603D0  , x94602 = x9460**2 )               RESONA66
      PARAMETER ( g9460 = 0.000052D0, g9460e = 0.00000134D0 )           RESONA67
C The Upsilon(2s)(10023) resonance                                      RESONA68
      PARAMETER ( x0023 = 10.023D0  , x00232 = x0023**2 )               RESONA69
      PARAMETER ( g0023 = 0.000043D0, g0023e = 0.00000059D0 )           RESONA70
C The Upsilon(3s)(10355) resonance                                      RESONA71
      PARAMETER ( x0355 = 10.355D0  , x03552 = x0355**2 )               RESONA72
      PARAMETER ( g0355 = 0.000024D0, g0355e = 0.00000044D0 )           RESONA73
C The Upsilon(4s)(10580) resonance                                      RESONA74
      PARAMETER ( x0580 = 10.580D0  , x05802 = x0580**2 )               RESONA75
      PARAMETER ( g0580 = 0.0238D0  , g0580e = 0.00000024D0 )           RESONA76
C The Upsilon(10860) resonance                                          RESONA77
      PARAMETER ( x0860 = 10.865D0  , x08602 = x0860**2 )               RESONA78
      PARAMETER ( g0860 = 0.1100D0  , g0860e = 0.00000031D0 )           RESONA79
C The Upsilon(11020) resonance                                          RESONA80
      PARAMETER ( x1111 = 11.019D0  , x11112 = x1111**2 )               RESONA81
      PARAMETER ( g1111 = 0.0790D0  , g1111e = 0.00000013D0 )           RESONA82
C And the common y affering                                             RESONA83
      COMMON / resonance / rparam(4,10,5), nres(5)                      RESONA84
      COMMON / extrmes   / rmin(10), rmax(10)                           RESONA85
C                                                                       RRATIO20
      COMPLEX*16 gg, formpi                                             RRATIO21
      DIMENSION res(10)                                                 RRATIO22
C                                                                       RRATIO23
      ss = SQRT(s)                                                      RRATIO24
      rratio = 0.                                                       RRATIO25
      CALL vzero(res(1),20)                                             RRATIO26
C                                                                       RRATIO27
      DO ires = 1, nres(ind1)                                           RRATIO28
        IF ( DABS(ss-rparam(1,ires,ind1)) .LT.                          RRATIO29
     .           40.*rparam(2,ires,ind1)       ) THEN                   RRATIO30
          res(ires) = 3.*pi*rparam(2,ires,ind1)*rparam(3,ires,ind1)     RRATIO31
     .            / ( (ss-rparam(1,ires,ind1))**2                       RRATIO32
     .                   +rparam(2,ires,ind1) **2/4.)                   RRATIO33
        ENDIF                                                           RRATIO34
        IF ( ind1 .LE. 2 .AND. ires .GE. 2 .AND. ss .LE. .922D0 )       RRATIO35
     .  res(ires) = 0D0                                                 RRATIO36
        rratio = rratio + res(ires)/cmumu                               RRATIO37
      ENDDO                                                             RRATIO38
C                                                                       RRATIO39
      GOTO (1,2,3,4,5) ind1                                             RRATIO40
C                                                                       RRATIO41
C The uubar/ddbar contribution                                          RRATIO42
C                                                                       RRATIO43
    1 CONTINUE                                                          RRATIO44
      betau = SQRT(1.-4.*amu**2/s)                                      RRATIO45
      GOTO 6                                                            RRATIO46
C                                                                       RRATIO47
    2 CONTINUE                                                          RRATIO48
      betau= SQRT(1.-4.*amd**2/s)                                       RRATIO49
C                                                                       RRATIO50
    6 CONTINUE                                                          RRATIO51
      IF ( ss .LT. 2*xmpi ) THEN                                        RRATIO52
        rratio = 0.                                                     RRATIO53
        GOTO 999                                                        RRATIO54
      ENDIF                                                             RRATIO55
C                                                                       RRATIO56
C Compute the pion form factor                                          RRATIO57
C                                                                       RRATIO58
      beta = SQRT(1.-4.*xmpi2/s)                                        RRATIO59
      rhores  = 0.                                                      RRATIO60
      IF (ss .LT. 1.5) rhores = piform(s) * beta**3 / 4.                RRATIO61
      IF (ss .LT. .922D0) THEN                                          RRATIO62
        beta = 0D0                                                      RRATIO63
      ELSE                                                              RRATIO64
        beta = SQRT(1D0-.922D0**2/s)                                    RRATIO65
      ENDIF                                                             RRATIO66
C                                                                       RRATIO67
      rratio = (beta + 3./5.*(rratio + rhores))                         RRATIO68
     .       / (betau*(3.-betau**2)/2.)                                 RRATIO69
C                                                                       RRATIO70
C Note : the factor 3/5 = 1/5 / (1/3) for d and 4/5 / (4/3) for u       RRATIO71
C 1/5 and 4/5 are the relative contributions of d and u to R            RRATIO72
C 1/3 and 4/3 are already in FERMISV and deconvoluted here              RRATIO73
C                                                                       RRATIO74
      GOTO 999                                                          RRATIO75
C                                                                       RRATIO76
C The ssbar contribution                                                RRATIO77
C                                                                       RRATIO78
    3 CONTINUE                                                          RRATIO79
      IF ( ss .LT. 2*xmka ) THEN                                        RRATIO80
        rratio = 0.                                                     RRATIO81
        GOTO 999                                                        RRATIO82
      ENDIF                                                             RRATIO83
C                                                                       RRATIO84
      beta = 0D0                                                        RRATIO85
      betas= SQRT(1.-4D0*ams**2/s)                                      RRATIO86
      IF ( ss .GT. 4.*xmka ) beta = SQRT(1.-4.*xmka2/s)                 RRATIO87
C     rratio = (beta**3+3D0*rratio) / (betas*(3D0-betas**2)/2.)         RRATIO88
      rratio = (beta   +3D0*rratio) / (betas*(3D0-betas**2)/2.)         RRATIO89
C                                                                       RRATIO90
C Note : the factor 3 [=1/(1/3)] is used to deconvulte the 1/3          RRATIO91
C charge/color factor already present in FERMSIV                        RRATIO92
C                                                                       RRATIO93
      GOTO 999                                                          RRATIO94
C                                                                       RRATIO95
C The ccbar contribution                                                RRATIO96
C                                                                       RRATIO97
    4 CONTINUE                                                          RRATIO98
      IF ( ss .LE. rmin(4) ) THEN                                       RRATIO99
        rratio = 0.                                                     RRATI100
        GOTO 999                                                        RRATI101
      ENDIF                                                             RRATI102
C                                                                       RRATI103
      betad0 = 0.D0                                                     RRATI104
      betac  = SQRT(1.-4D0*amc**2/s)                                    RRATI105
      IF ( ss .GT. 2D0*xmd0 ) betad0 = SQRT(1.-4D0*xmd0**2/s)           RRATI106
C     rratio = (betad0**3+3D0/4D0*rratio) / (betac*(3D0-betac**2)/2.)   RRATI107
      rratio = (betad0   +3D0/4D0*rratio) / (betac*(3D0-betac**2)/2.)   RRATI108
C                                                                       RRATI109
C Note : the factor 3/4 [=1/(4/3)] is used to deconvolute the 4/3       RRATI110
C charge/color factor already present in FERMSIV                        RRATI111
C                                                                       RRATI112
      GOTO 999                                                          RRATI113
C                                                                       RRATI114
C The bbbar contribution                                                RRATI115
C                                                                       RRATI116
    5 CONTINUE                                                          RRATI117
      IF ( ss .LE. rmin(5) ) THEN                                       RRATI118
        rratio = 0.                                                     RRATI119
        GOTO 999                                                        RRATI120
      ENDIF                                                             RRATI121
C                                                                       RRATI122
      betab0 = 0.D0                                                     RRATI123
      betab  = SQRT(1.-4D0*amb**2/s)                                    RRATI124
C     IF ( ss .GT. 2D0*xmb0 ) betab0 = SQRT(1.-4D0*xmb02/s)             RRATI125
      IF ( ss .GT. x1111 ) THEN                                         RRATI126
        betab0 = 1D0  ! Cope with R_exp                                 RRATI127
      ELSEIF ( ss .GT. x9460 ) THEN                                     RRATI128
        betab0 = 0.1  ! Make QCD1 converge                              RRATI129
      ENDIF                                                             RRATI130
C     rratio = (betab0**3+3D0*rratio) / (betab*(3D0-betab**2)/2.)       RRATI131
      rratio = (betab0   +3D0*rratio) / (betab*(3D0-betab**2)/2.)       RRATI132
C                                                                       RRATI133
C Note : the factor 3 [=1/(1/3)] is used to deconvolute the 1/3         RRATI134
C charge/color factor already present in FERMSIV                        RRATI135
C                                                                       RRATI136
  999 CONTINUE                                                          RRATI137
C                                                                       RRATI138
C Now the 1+alpha_s/pi correction                                       RRATI139
C                                                                       RRATI140
      IF ( ss .GE. 2*xmb0 ) THEN                                        RRATI141
        alqcd = runalf(ss,5D0)                                          RRATI142
      ELSEIF ( ss .GE. 2D0*xmd0 ) THEN                                  RRATI143
        alqcdb0 = runalf(2D0*xmb0,5.D0)                                 BB950912
        alqcd   = (1D0-4D0*xmd02/s)                                     RRATI145
     .          / (1D0-xmd02/xmb02) * alqcdb0                           RRATI146
      ELSE                                                              RRATI147
        alqcd = 0D0                                                     RRATI148
      ENDIF                                                             RRATI149
      rratio = rratio * ( 1.D0 + alqcd )                                RRATI150
C                                                                       RRATI151
      RETURN                                                            RRATI152
      END                                                               RRATI153
      FUNCTION runalf(qm,fn)                                            RUNALF 2
C-------------------------------------------------------------------    RUNALF 3
C! Running strong coupling constant                                     RUNALF 4
C                                                                       RUNALF 5
C  Inputs:       --qm,   Scale mass                                     RUNALF 6
C                --fn,    flavour number                                RUNALF 7
C                                                                       RUNALF 8
C  Ouptuts:      --runalf,  The value of the alphas(qm**2)/pi           RUNALF 9
C                                                                       RUNALF10
C  P. Janot  --  22 nov 1989.                                           RUNALF11
C------------------------------------------------------------------     RUNALF12
      IMPLICIT REAL*8 (A-H,O-Z)                                         RUNALF13
C The quark and lepton masses                                           QMASSES2
      REAL*8 amu, amd, ams, amc, amb, amt                               QMASSES3
      REAL*8 amel, ammu, amto, amne, amnm, amnt                         QMASSES4
      PARAMETER ( amu = .005D0, amd = .010d0, ams = .150d0 )            QMASSES5
      PARAMETER ( amc = 1.37d0, amb = 4.70d0, amt = 170.d0 )            QMASSES6
      PARAMETER ( amel= .511D-3, ammu= .1057D0, amto = 1.777d0 )        QMASSES7
      PARAMETER ( amne= 1D-4, amnm= 1D-4, amnt = 1D-4 )                 QMASSES8
      COMMON / alcoef / beta1,beta2,beta3,gama1,gama2,gama3             RUNALF15
C                                                                       RUNALF16
C Lambda_QCD (reference: nf=5)                                          RUNALF17
C                                                                       RUNALF18
      xlamda5 = .200                                                    RUNALF19
C                                                                       RUNALF20
C Lambda_QCD at nf # 5                                                  RUNALF21
C                                                                       RUNALF22
      xlamda4 = xlamda5 * (amb/xlamda5)**(2./25.)                       RUNALF23
     .        * (DLOG(amb**2/xlamda5**2))**(963./14375.)                RUNALF24
      xlamda3 = xlamda4 * (amc/xlamda4)**(2./27.)                       RUNALF25
     .        * (DLOG(amc**2/xlamda4**2))**(107./2025.)                 RUNALF26
      xlamda6 = xlamda5 * (xlamda5/amt)**(2./21.)                       RUNALF27
     .        * (DLOG(amt**2/xlamda5**2))**(-321./3381.)                RUNALF28
C                                                                       RUNALF29
C Lambda_QCD at fn                                                      RUNALF30
C                                                                       RUNALF31
      nf = fn                                                           RUNALF32
      IF ( nf .LT. 3 ) xlamda = xlamda3                                 RUNALF33
      IF ( nf .EQ. 3 ) xlamda = (fn-3.)*xlamda4+(4.-fn)*xlamda3         RUNALF34
      IF ( nf .EQ. 4 ) xlamda = (fn-4.)*xlamda5+(5.-fn)*xlamda4         RUNALF35
      IF ( nf .EQ. 5 ) xlamda = (fn-5.)*xlamda6+(6.-fn)*xlamda5         RUNALF36
      IF ( nf .GE. 6 ) xlamda =        xlamda6                          RUNALF37
C                                                                       RUNALF38
C  Coefficients                                                         RUNALF39
C                                                                       RUNALF40
      beta1 = -11./2. + fn/3.                                           RUNALF41
      beta2 = -51./4. + 19./12.*fn                                      RUNALF42
      beta3 = (-2857. + 5033.*fn/9. - 325.*fn**2/27.) / 64.             RUNALF43
      gama1 = 2.                                                        RUNALF44
      gama2 = 101./12. -5./18.*fn                                       RUNALF45
      gama3 = (3747. - (160.*1.2020569+2216./9.)*fn -140.*fn**2/27.)    RUNALF46
     .            / 96.                                                 RUNALF47
C                                                                       RUNALF48
C  alpha_s / pi                                                         RUNALF49
C                                                                       RUNALF50
      qm2 = qm                                                          RUNALF51
      IF ( qm2 .LT. 1. ) qm2 = 1.                                       RUNALF52
      as = 1./(-beta1*DLOG(qm2/xlamda))                                 RUNALF53
      aal = DLOG(2.*DLOG(qm2/xlamda))                                   RUNALF54
      runalf = as * (1. - as*beta2/beta1*aal                            RUNALF55
     .       + as**2 * ( (beta2/beta1)**2 * ( aal**2 -aal - 1. )        RUNALF56
     .       + beta3/beta1))                                            RUNALF57
C                                                                       RUNALF58
      RETURN                                                            RUNALF59
      END                                                               RUNALF60
