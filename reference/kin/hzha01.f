      SUBROUTINE askusi(icode)                                          ASKUSI 2
C-----------------------------------------------------------------------ASKUSI 3
C! Initialization routine for HZHA01                                    ASKUSI 4
C                                                                       ASKUSI 5
C-----------------------------------------------------------------------ASKUSI 6
C                                                                       ASKUSI 8
      PARAMETER(lpdec=48)                                               ASKUSI 9
      PARAMETER(igcod=7019)                                             ASKUSI10
      INTEGER nodec(lpdec)                                              ASKUSI11
      INTEGER altabl,namind,alrlep                                      ASKUSI12
      EXTERNAL altabl,namind,alrlep                                     ASKUSI13
C                                                                       ASKUSI14
      DIMENSION indneu(4), indcha(2)                                    ASKUSI15
      DATA indneu/51,52,53,54/                                          ASKUSI16
      DATA indcha/55,56/                                                ASKUSI17
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
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),               CONSTS 2
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw      CONSTS 3
C                                                                       CONSTS 4
      PARAMETER (maxpro= 8)                                             HHPROD 2
      COMMON / cropro / cross(maxpro), sthr(maxpro), reduc(maxpro)      HHPROD 3
      CHARACTER*14 chapro(maxpro)                                       HHPROD 4
      DATA chapro /                                                     HHPROD 5
     .             'e+e- --> h Z',                                      HHPROD 6
     .             'e+e- --> H Z',                                      HHPROD 7
     .             'e+e- --> h A',                                      HHPROD 8
     .             'e+e- --> H A',                                      HHPROD 9
     .             'W+W- --> h  ',                                      HHPROD10
     .             'W+W- --> H  ',                                      HHPROD11
     .             'Z Z  --> h  ',                                      HHPROD12
     .             'Z Z  --> H  '                                       HHPROD13
     .                                /                                 HHPROD14
      COMMON / prosim / iproyn(maxpro),nevpro(maxpro)                   HHPROD15
      CHARACTER*14 channel                                              HHDECK 2
      CHARACTER*21 channeut, chanchar                                   HHDECK 3
      PARAMETER(nchneut=8,nchchar=5)                                    HHDECK 4
      COMMON / hhdeck / branch(nchan,nhig),width(nhig),                 HHDECK 5
     .                  parwth(nhig),xymas(2,nchan,nhig),               HHDECK 6
     .                  xywid(2,nchan,nhig)                             HHDECK 7
      COMMON / chaneu / ichn(2),                                        HHDECK 8
     .                  wneut(4,4,nhig), wchar(2,2,nhig),               HHDECK 9
     .                  widneut(4), brneut(nchneut,4),                  HHDECK10
     .                  widchar(2), brchar(nchchar,2)                   HHDECK11
      COMMON / chanch / channeut(nchneut,4), chanchar(nchchar,2)        HHDECK12
      COMMON / vect4 / pvect4(5,2)                                      HHDECK13
      COMMON / hinput / ichan(nchan,nhig), channel(nchan,nhig)          HHDECK14
      COMMON / hisbr / bchpp,bchgg                                      HHDECK15
      DIMENSION ph(4)                                                   HHDECK16
C                                                                       HHDECK17
      DIMENSION brai(11), kcode(11), xmasmi(11)                         ZZDECK 2
      DATA brai/.0335,.0665,.0335,.0665,.0335,.0665,                    ZZDECK 3
     .          .1540,.1190,.1540,.1190,.1540/                          ZZDECK 4
      DATA kcode/11,12,13,14,15,16,1,2,3,4,5/                           ZZDECK 5
      DATA xmasmi/6*0.,0.3,0.3,1.0,4.0,11.0/                            ZZDECK 6
      COMMON / zzdec / braz(11), kselec(11), fracz                      ZZDECK 7
C                                                                       ZZDECK 8
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,      MISCL  2
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),            MISCL  3
     &                 nevent(11)                                       MISCL  4
      INTEGER loutbe,idb1,idb2,nevent                                   MISCL  5
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad                      MISCL  6
C                                                                       MISCL  7
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
C                                                                       BMACRO30
C                                                                       ASKUSI27
      loutbe = IW(6)                                                    ASKUSI28
C...Set generator code                                                  ASKUSI29
      icode = igcod                                                     ASKUSI30
C                                                                       ASKUSI31
C...Create the KLIN bank and complete the PART bank                     ASKUSI32
C                                                                       ASKUSI33
      CALL kxl74a(ipart,iklin)                                          ASKUSI34
      IF( ipart .LE . 0 .OR. iklin .LE. 0 ) THEN                        ASKUSI35
        WRITE(loutbe,1000) ipart,iklin                                  ASKUSI36
 1000   FORMAT(1X,'+++ASKUSI+++ ERROR FILLING PART OR KLIN---STOP'      ASKUSI37
     $  ,2I5)                                                           ASKUSI38
        STOP                                                            ASKUSI39
      ENDIF                                                             ASKUSI40
C                                                                       ASKUSI41
C Welcome !                                                             ASKUSI42
C                                                                       ASKUSI43
      WRITE(loutbe,1001) icode                                          ASKUSI44
 1001 FORMAT(//15X,'+---------------------------------------------+'/   ASKUSI45
     .      15X,'|                                             |'/      ASKUSI46
     .      15X,'|     W E L C O M E   T O    H Z H A 0 1      |'/      ASKUSI47
     .      15X,'|     **********************************      |'/      ASKUSI48
     .      15X,'|      The (SUSY) Higgs boson generator       |'/      ASKUSI49
     .      15X,'|             in e+e- collisions              |'/      ASKUSI50
     .      15X,'|     **********************************      |'/      ASKUSI51
     .      15X,'|             Code is # ',I6,'                |'/      ASKUSI52
     .      15X,'|                                             |'/      ASKUSI53
     .      15X,'|                                             |'/      ASKUSI54
     .      15X,'|     Last date of change : 23 Sep 1995       |'/      ASKUSI55
     .      15X,'|     Patrick Janot  --  CERN/PPE             |'/      ASKUSI56
     .      15X,'+---------------------------------------------+'//)    ASKUSI57
C                                                                       ASKUSI58
C Read the generator parameters                                         ASKUSI59
C                                                                       ASKUSI60
      ngene = NAMIND('GENE')                                            ASKUSI61
      jgene = IW(ngene)                                                 ASKUSI62
      IF(jgene.NE.0) THEN                                               ASKUSI63
        iklei   = NINT(RW(jgene+1))                                     ASKUSI64
        iproc   = NINT(RW(jgene+2))                                     ASKUSI65
        xrad    =      RW(jgene+3)                                      ASKUSI66
        ecm     =      RW(jgene+4)                                      ASKUSI67
        empir   =      RW(jgene+5)                                      ASKUSI68
        ism     =      RW(jgene+6)                                      ASKUSI69
        icar    =      RW(jgene+7)                                      ASKUSI70
      ELSE                                                              ASKUSI71
        iklei   = 1                                                     ASKUSI72
        iproc   = 1                                                     ASKUSI73
        xrad    = 1.                                                    ASKUSI74
        ecm     = 190.                                                  ASKUSI75
        empir   = 4.0                                                   ASKUSI76
        ism     = 1                                                     ASKUSI77
        icar    = 3                                                     ASKUSI78
      ENDIF                                                             ASKUSI79
C                                                                       ASKUSI80
C Read the Standard Model parameters                                    ASKUSI81
C                                                                       ASKUSI82
      ngsmo = NAMIND('GSMO')                                            ASKUSI83
      jgsmo = IW(ngsmo)                                                 ASKUSI84
      IF(jgsmo.NE.0) THEN                                               ASKUSI85
        amz     =      RW(jgsmo+1)                                      ASKUSI86
        gmz     =      RW(jgsmo+2)                                      ASKUSI87
        g_f     =      Rw(jgsmo+3)                                      ASKUSI88
        amt     =      RW(jgsmo+4)                                      ASKUSI89
        amh     =      RW(jgsmo+5)                                      ASKUSI90
        xlamda5 =      RW(jgsmo+6)                                      ASKUSI91
      ELSE                                                              ASKUSI92
        amz     = 91.189                                                ASKUSI93
        gmz     = 2.497                                                 ASKUSI94
        g_f     = 1.166392E-5                                           ASKUSI95
        amt     = 174.                                                  ASKUSI96
        amh     = 60.                                                   ASKUSI97
        xlamda5 = .208                                                  ASKUSI98
      ENDIF                                                             ASKUSI99
C                                                                       ASKUS100
C Read the MSSM parameters                                              ASKUS101
C                                                                       ASKUS102
      ngsus = NAMIND('GSUS')                                            ASKUS103
      jgsus = IW(ngsus)                                                 ASKUS104
      IF(jgsus.NE.0) THEN                                               ASKUS105
        amarun  =      RW(jgsus+1)                                      ASKUS106
        tb      =      RW(jgsus+2)                                      ASKUS107
        susM    =      RW(jgsus+3)                                      ASKUS108
        susMU   =      RW(jgsus+4)                                      ASKUS109
        susAt   =      RW(jgsus+5)                                      ASKUS110
        susAb   =      RW(jgsus+6)                                      ASKUS111
        susSMQ  =      RW(jgsus+7)                                      ASKUS112
        susSMU  =      RW(jgsus+8)                                      ASKUS113
        susSMD  =      RW(jgsus+9)                                      ASKUS114
        susSML  =      RW(jgsus+10)                                     ASKUS115
        susSME  =      RW(jgsus+11)                                     ASKUS116
      ELSE                                                              ASKUS117
        amarun  = 100.                                                  ASKUS118
        tb      = 10.                                                   ASKUS119
        susM    = 0.                                                    ASKUS120
        susMU   = 0.                                                    ASKUS121
        susAt   = 0.                                                    ASKUS122
        susAb   = 0.                                                    ASKUS123
        susSMQ  = 1000.                                                 ASKUS124
        susSMU  = 1000.                                                 ASKUS125
        susSMD  = 1000.                                                 ASKUS126
        susSML  = 1000.                                                 ASKUS127
        susSME  = 1000.                                                 ASKUS128
      ENDIF                                                             ASKUS129
C                                                                       ASKUS130
      CALL vzero(nevpro(1),maxpro)                                      ASKUS131
      npryn = NAMIND('PRYN')                                            ASKUS132
      jpryn = IW(npryn)                                                 ASKUS133
      IF(jpryn.NE.0) THEN                                               ASKUS134
        DO ipro = 1, maxpro                                             ASKUS135
          iproyn(ipro) = IW(jpryn+ipro)                                 ASKUS136
        ENDDO                                                           ASKUS137
      ELSE                                                              ASKUS138
        CALL vzero(iproyn(1),maxpro)                                    ASKUS139
        iproyn(iproc) = 1                                               ASKUS140
      ENDIF                                                             ASKUS141
C                                                                       ASKUS142
      igzdc = NLINK('GZDC',0)                                           ASKUS143
      DO ifs = 1, 11                                                    ASKUS144
        IF ( igzdc .LE. 0 ) THEN                                        ASKUS145
          kselec(ifs) = 1                                               ASKUS146
        ELSE                                                            ASKUS147
          kselec(ifs) = IW(igzdc+ifs)                                   ASKUS148
        ENDIF                                                           ASKUS149
      ENDDO                                                             ASKUS150
C                                                                       ASKUS151
      braz(1) = brai(1) * kselec(1)                                     ASKUS152
      DO ifs = 2, 11                                                    ASKUS153
        braz(ifs) = brai(ifs)*kselec(ifs)+braz(ifs-1)                   ASKUS154
      ENDDO                                                             ASKUS155
C                                                                       ASKUS156
      fracz = braz(11)                                                  ASKUS157
      DO ifs = 1, 11                                                    ASKUS158
        braz(ifs) = braz(ifs)/braz(11)                                  ASKUS159
      ENDDO                                                             ASKUS160
C                                                                       ASKUS161
      pmas(23,1) = amz                                                  ASKUS162
      pmas(23,2) = gmz                                                  ASKUS163
      pmas( 6,1) = amt                                                  ASKUS164
C                                                                       ASKUS165
      TABL(1)  = iklei                                                  ASKUS166
      TABL(2)  = iproc                                                  ASKUS167
      TABL(3)  = xrad                                                   ASKUS168
      TABL(4)  = ecm                                                    ASKUS169
      TABL(5)  = empir                                                  ASKUS170
      TABL(6)  = ism                                                    ASKUS171
      TABL(7)  = amz                                                    ASKUS172
      TABL(8)  = gmz                                                    ASKUS173
      TABL(9)  = g_f                                                    ASKUS174
      TABL(10) = amt                                                    ASKUS175
      TABL(11) = amh                                                    ASKUS176
      TABL(12) = xlamda5                                                ASKUS177
      TABL(13) = amarun                                                 ASKUS178
      TABL(14) = tb                                                     ASKUS179
      TABL(15) = susM                                                   ASKUS180
      TABL(16) = susMU                                                  ASKUS181
      TABL(17) = susAt                                                  ASKUS182
      TABL(18) = susAb                                                  ASKUS183
      TABL(19) = susSMQ                                                 ASKUS184
      TABL(20) = susSMU                                                 ASKUS185
      TABL(21) = susSMD                                                 ASKUS186
      TABL(22) = susSML                                                 ASKUS187
      TABL(23) = susSME                                                 ASKUS188
C                                                                       ASKUS189
C...Initialize Higgs decay routine                                      ASKUS190
C                                                                       ASKUS191
      CALL hhinit                                                       ASKUS192
      IF ( amh .LT. 0. ) STOP 99                                        ASKUS193
      IF ( ism .EQ. 0 .AND. amst(1) .LE. 0. ) STOP 99                   ASKUS194
      WRITE(6,*) ' Lifetimes (ps) : ',                                  ASKUS195
     .            tauh(1)*1E12,tauh(2)*1E12,tauh(3)*1E12                ASKUS196
C                                                                       ASKUS197
C...Compute Born cross sections                                         ASKUS198
C                                                                       ASKUS199
      CALL vzero(wtot (1),4)                                            ASKUS200
      CALL vzero(wtot2(1),4)                                            ASKUS201
      CALL vzero(ntry (1),4)                                            ASKUS202
      CALL vzero(nacc (1),4)                                            ASKUS203
C                                                                       ASKUS204
      sbeam = ecm**2                                                    ASKUS205
      DO 5 ipro = 1, maxpro                                             ASKUS206
        cross(ipro) = crocom(ipro,sbeam)                                ASKUS207
    5 CONTINUE                                                          ASKUS208
C                                                                       ASKUS209
      CALL vzero(reduc(1),maxpro)                                       ASKUS210
      reduc(1) = fracz * parwth(2) / width(2)                           ASKUS211
      reduc(5) = parwth(2) / width(2)                                   ASKUS212
      reduc(7) = parwth(2) / width(2)                                   ASKUS213
      IF ( ism .EQ. 0 ) THEN                                            ASKUS214
        reduc(2) = fracz * parwth(1) / width(1)                         ASKUS215
        reduc(3) = parwth(2)*parwth(3) / (width(2)*width(3))            ASKUS216
        reduc(4) = parwth(1)*parwth(3) / (width(1)*width(3))            ASKUS217
        reduc(6) = parwth(1) / width(1)                                 ASKUS218
        reduc(8) = parwth(1) / width(1)                                 ASKUS219
      ENDIF                                                             ASKUS220
C                                                                       ASKUS221
      WRITE(loutbe,2000) (chapro(ipro),                                 ASKUS222
     .                    cross (ipro),                                 ASKUS223
     .                    reduc (ipro),ipro=1,maxpro)                   ASKUS224
 2000 FORMAT(1x,'Born level cross section have been computed:'/         ASKUS225
     .     8(1x,'   . ',A14,' : ',E12.4,' fb  * BR = ',F8.6/))          ASKUS226
C...                                                                    ASKUS227
C...End of HZHA stuff                                                   ASKUS228
C...                                                                    ASKUS229
C                                                                       ASKUS230
C...Fill back the Higgs masses and lifetimes into the PART bank         ASKUS231
C                                                                       ASKUS232
      napar = NAMIND('PART')                                            ASKUS233
      jpart = IW(napar)                                                 ASKUS234
      mdcy(lucomp(25),1) = 0                                            ASKUS235
      mdcy(lucomp(35),1) = 0                                            ASKUS236
      mdcy(lucomp(36),1) = 0                                            ASKUS237
      pmas(lucomp(25),4) = tauh(2)/3.33E-12                             ASKUS238
      pmas(lucomp(35),4) = tauh(1)/3.33E-12                             ASKUS239
      pmas(lucomp(36),4) = tauh(3)/3.33E-12                             ASKUS240
C...for h                                                               ASKUS241
      ipart = kgpart(25)                                                ASKUS242
      IF ( ipart .LE. 0 ) THEN                                          ASKUS243
        ipart = KBPART(100,'Higgs h     ',100,pmas(25,1),0.,0.)         ASKUS244
        jpart = IW(napar)                                               ASKUS245
        IW(jpart+lmhlen+(ipart-1)*iw(jpart+1)+10) = ipart               ASKUS246
        index = KBKLIN(ipart,25)                                        ASKUS247
      ELSE                                                              ASKUS248
        rw(jpart+lmhlen+(ipart-1)*iw(jpart+1)+6) = pmas(25,1)           ASKUS249
      ENDIF                                                             ASKUS250
      IF ( tauh(2) .GT. 1.E-15 .AND. ipart .GT. 0 )                     ASKUS251
     .  RW(jpart+lmhlen+(ipart-1)*iw(jpart+1)+8) = tauh(2)              ASKUS252
C...for H                                                               ASKUS253
      ipart = kgpart(35)                                                ASKUS254
      IF ( ipart .LE. 0 ) THEN                                          ASKUS255
        ipart = KBPART(100,'Higgs H     ',100,pmas(35,1),0.,0.)         ASKUS256
        jpart = IW(napar)                                               ASKUS257
        IW(jpart+lmhlen+(ipart-1)*iw(jpart+1)+10) = ipart               ASKUS258
        index = KBKLIN(ipart,35)                                        ASKUS259
      ELSE                                                              ASKUS260
        rw(jpart+lmhlen+(ipart-1)*iw(jpart+1)+6) = pmas(35,1)           ASKUS261
      ENDIF                                                             ASKUS262
      IF ( tauh(1) .GT. 1.E-15 .AND. ipart .GT. 0 )                     ASKUS263
     .  RW(jpart+lmhlen+(ipart-1)*iw(jpart+1)+8) = tauh(1)              ASKUS264
C...for A                                                               ASKUS265
      ipart = kgpart(36)                                                ASKUS266
      IF ( ipart .LE. 0 ) THEN                                          ASKUS267
        ipart = KBPART(100,'Higgs A     ',100,pmas(36,1),0.,0.)         ASKUS268
        jpart = IW(napar)                                               ASKUS269
        IW(jpart+lmhlen+(ipart-1)*iw(jpart+1)+10) = ipart               ASKUS270
        index = KBKLIN(ipart,36)                                        ASKUS271
      ELSE                                                              ASKUS272
        rw(jpart+lmhlen+(ipart-1)*iw(jpart+1)+6) = pmas(36,1)           ASKUS273
      ENDIF                                                             ASKUS274
      IF ( tauh(3) .GT. 1.E-15 .AND. ipart .GT. 0 )                     ASKUS275
     .  RW(jpart+lmhlen+(ipart-1)*iw(jpart+1)+8) = tauh(3)              ASKUS276
C...For Z                                                               ASKUS277
      ipart = kgpart(23)                                                ASKUS278
      rw(jpart+lmhlen+(ipart-1)*iw(jpart+1)+6) = pmas(23,1)             ASKUS279
C...For top                                                             ASKUS280
      ipart = kgpart(6)                                                 ASKUS281
      rw(jpart+lmhlen+(ipart-1)*iw(jpart+1)+6) = pmas(6,1)              ASKUS282
      ianti = ITABL(jpart,ipart,10)                                     ASKUS283
      kapar = krow(jpart,ianti)                                         ASKUS284
      rw(kapar+6)=pmas(6,1)                                             ASKUS285
C                                                                       ASKUS286
C...Add neutralinos to the particle list                                ASKUS287
C                                                                       ASKUS288
      DO ineut = 1, 4                                                   ASKUS289
        jchi = indneu(ineut)                                            ASKUS290
        kchi = LUCOMP(jchi)                                             ASKUS291
C                                                                       ASKUS292
        CHAF(kchi)(1:4)='Chi0'                                          ASKUS293
        WRITE(CHAF(kchi)(4:4),50) ineut                                 ASKUS294
   50   FORMAT(I1)                                                      ASKUS295
        PMAS(kchi,1) = ABS(amneut(ineut))                               ASKUS296
        IF ( ineut .NE. 1 ) MDCY(kchi,1) = 0                            ASKUS297
        KCHG(kchi,1) = 0                                                ASKUS298
        KCHG(kchi,3) = 0                                                ASKUS299
C                                                                       ASKUS300
        ipart = KGPART(jchi)                                            ASKUS301
        IF ( ipart .LE. 0 ) THEN                                        ASKUS302
          ipart = KBPART(100,chaf(kchi)(1:4)//'_0      ',               ASKUS303
     .                   100,pmas(kchi,1),0.,0.)                        ASKUS304
          jpart = IW(napar)                                             ASKUS305
          IW(jpart+lmhlen+(ipart-1)*IW(jpart+1)+10) = ipart             ASKUS306
          index = KBKLIN(ipart,jchi)                                    ASKUS307
        ELSE                                                            ASKUS308
          rw(jpart+lmhlen+(ipart-1)*IW(jpart+1)+6) = pmas(kchi,1)       ASKUS309
        ENDIF                                                           ASKUS310
      ENDDO                                                             ASKUS311
C                                                                       ASKUS312
C...Add Charginos to the particle list                                  ASKUS313
C                                                                       ASKUS314
      DO ichar = 1, 2                                                   ASKUS315
        jchi = indcha(ichar)                                            ASKUS316
        kchi = LUCOMP(jchi)                                             ASKUS317
C                                                                       ASKUS318
        CHAF(kchi)(1:4)='Chi0'                                          ASKUS319
        WRITE(CHAF(kchi)(4:4),51) ichar                                 ASKUS320
   51   FORMAT(I1)                                                      ASKUS321
        PMAS(kchi,1) = ABS(amchar(ichar))                               ASKUS322
        MDCY(kchi,1) = 0                                                ASKUS323
        KCHG(kchi,1) = 3                                                ASKUS324
        KCHG(kchi,3) = 1                                                ASKUS325
C                                                                       ASKUS326
        ipart = KGPART(jchi)                                            ASKUS327
        IF ( ipart .LE. 0 ) THEN                                        ASKUS328
          ipart = KBPART(100,chaf(kchi)(1:4)//'~+      ',               ASKUS329
     .                   100,pmas(kchi,1),1.,0.)                        ASKUS330
          jpart = IW(napar)                                             ASKUS331
          IW(jpart+lmhlen+(ipart-1)*IW(jpart+1)+10) = ipart             ASKUS332
          index = KBKLIN(ipart,jchi)                                    ASKUS333
        ELSE                                                            ASKUS334
          rw(jpart+lmhlen+(ipart-1)*IW(jpart+1)+6) = pmas(kchi,1)       ASKUS335
        ENDIF                                                           ASKUS336
C                                                                       ASKUS337
        ipart = KGPART(-jchi)                                           ASKUS338
        IF ( ipart .LE. 0 ) THEN                                        ASKUS339
          ipart = KBPART(100,chaf(kchi)(1:4)//'~-      ',               ASKUS340
     .                   100,pmas(kchi,1),-1.,0.)                       ASKUS341
          jpart = IW(napar)                                             ASKUS342
          IW(jpart+lmhlen+(ipart-1)*IW(jpart+1)+10) = ipart             ASKUS343
          index = KBKLIN(ipart,-jchi)                                   ASKUS344
        ELSE                                                            ASKUS345
          rw(jpart+lmhlen+(ipart-1)*IW(jpart+1)+6) = pmas(kchi,1)       ASKUS346
        ENDIF                                                           ASKUS347
C                                                                       ASKUS348
      ENDDO                                                             ASKUS349
C                                                                       ASKUS350
C...Get list of lund particle # which should not be decayed             ASKUS351
      mxdec = KNODEC(nodec,lpdec)                                       ASKUS352
C     WRITE(6,*) 'KNODEC = ',mxdec,' LPDEC = ',lpdec                    ASKUS353
      mxdec = MIN(mxdec,lpdec)                                          ASKUS354
C...Inhibit lund decays which should be done in galeph                  ASKUS355
      IF ( mxdec .GT. 0 ) THEN                                          ASKUS356
        DO 10 i=1,mxdec                                                 ASKUS357
          IF ( nodec(i) .GT. 0 ) THEN                                   ASKUS358
            jidb = NLINK('MDC1',nodec(i))                               ASKUS359
            IF ( jidb .eq. 0 ) mdcy(lucomp(nodec(i)),1) = 0             ASKUS360
          ENDIF                                                         ASKUS361
   10   CONTINUE                                                        ASKUS362
      ENDIF                                                             ASKUS363
C...Vertex smearing                                                     ASKUS364
      sdvrt(1) = 0.035                                                  ASKUS365
      sdvrt(2) = 0.0012                                                 ASKUS366
      sdvrt(3) = 1.28                                                   ASKUS367
      jsvrt = NLINK('SVRT',0)                                           ASKUS368
      IF ( jsvrt .NE. 0 ) THEN                                          ASKUS369
        sdvrt(1) = RW(jsvrt+1)                                          ASKUS370
        sdvrt(2) = RW(jsvrt+2)                                          ASKUS371
        sdvrt(3) = RW(jsvrt+3)                                          ASKUS372
      ENDIF                                                             ASKUS373
      tabl(24) = sdvrt(1)                                               ASKUS374
      tabl(25) = sdvrt(2)                                               ASKUS375
      tabl(26) = sdvrt(3)                                               ASKUS376
C                                                                       ASKUS377
C  Fill the KPAR bank with the generator parameters                     ASKUS378
C                                                                       ASKUS379
       jkpar = altabl('KPAR',26,1,tabl,'2I,(F)','C')                    ASKUS380
C  Fill RLEP bank                                                       ASKUS381
       iebeam = NINT(ecm*500)                                           ASKUS382
       jrlep = alrlep(iebeam,'    ',0,0,0)                              ASKUS383
C...Debug flags                                                         ASKUS384
      jdebu = IW(NAMIND('DEBU'))                                        ASKUS385
      IF ( jdebu .GT. 0 ) THEN                                          ASKUS386
        idb1 = IW(jdebu+1)                                              ASKUS387
        idb2 = IW(jdebu+2)                                              ASKUS388
      ENDIF                                                             ASKUS389
C                                                                       ASKUS390
C  Initialize events counters                                           ASKUS391
C                                                                       ASKUS392
       DO 11 i = 1,11                                                   ASKUS393
   11  nevent(i) = 0                                                    ASKUS394
C                                                                       ASKUS395
C  Print PART and KLIN banks                                            ASKUS396
C                                                                       ASKUS397
C     CALL PRPART                                                       ASKUS398
C                                                                       ASKUS399
      CALL PRTABL('KPAR',0)                                             ASKUS400
      CALL PRTABL('RLEP',0)                                             ASKUS401
      CALL PRTABL('KHHG',0)                                             ASKUS402
C                                                                       ASKUS403
      RETURN                                                            ASKUS404
      END                                                               ASKUS405
      SUBROUTINE ASKUSE(IDPR,ISTA,NITR,NIVX,ECMS,WEIT)                  ASKUSE 2
C-----------------------------------------------------------------------ASKUSE 3
C! Event generation                                                     ASKUSE 4
C-----------------------------------------------------------------------ASKUSE 5
      DIMENSION qg1(4),qg2(4),qh(4),qa(4),ph(4),hh(4),qp(4),qm(4)       ASKUSE 6
      DIMENSION qz(4),q1(4),q2(4),q3(4),ch(4),ijoin(2)                  ASKUSE 7
      DIMENSION ptrak(4,2)                                              ASKUSE 8
      REAL*8 qin1(4),qin2(4),qout1(4),qout2(4),qout3(4)                 ASKUSE 9
      REAL DUMMY(4)                                                     ASKUSE10
      DATA dummy/4*0./                                                  ASKUSE11
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
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,      MISCL  2
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),            MISCL  3
     &                 nevent(11)                                       MISCL  4
      INTEGER loutbe,idb1,idb2,nevent                                   MISCL  5
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad                      MISCL  6
C                                                                       MISCL  7
      PARAMETER (maxpro= 8)                                             HHPROD 2
      COMMON / cropro / cross(maxpro), sthr(maxpro), reduc(maxpro)      HHPROD 3
      CHARACTER*14 chapro(maxpro)                                       HHPROD 4
      DATA chapro /                                                     HHPROD 5
     .             'e+e- --> h Z',                                      HHPROD 6
     .             'e+e- --> H Z',                                      HHPROD 7
     .             'e+e- --> h A',                                      HHPROD 8
     .             'e+e- --> H A',                                      HHPROD 9
     .             'W+W- --> h  ',                                      HHPROD10
     .             'W+W- --> H  ',                                      HHPROD11
     .             'Z Z  --> h  ',                                      HHPROD12
     .             'Z Z  --> H  '                                       HHPROD13
     .                                /                                 HHPROD14
      COMMON / prosim / iproyn(maxpro),nevpro(maxpro)                   HHPROD15
      DIMENSION brai(11), kcode(11), xmasmi(11)                         ZZDECK 2
      DATA brai/.0335,.0665,.0335,.0665,.0335,.0665,                    ZZDECK 3
     .          .1540,.1190,.1540,.1190,.1540/                          ZZDECK 4
      DATA kcode/11,12,13,14,15,16,1,2,3,4,5/                           ZZDECK 5
      DATA xmasmi/6*0.,0.3,0.3,1.0,4.0,11.0/                            ZZDECK 6
      COMMON / zzdec / braz(11), kselec(11), fracz                      ZZDECK 7
C                                                                       ZZDECK 8
      EXTERNAL sigma1,sigma2,sigma3,sigma4                              SIGMAS 2
      EXTERNAL sigma5,sigma6,sigma7,sigma8                              SIGMAS 3
      EXTERNAL sigmat                                                   SIGMAS 4
C                                                                       SIGMAS 5
C                                                                       ASKUSE18
C  Initialization ASKUSE's arguments                                    ASKUSE19
C                                                                       ASKUSE20
      ista   = 0                                                        ASKUSE21
      nitr   = 0                                                        ASKUSE22
      nivx   = 0                                                        ASKUSE23
      ecms   = ecm                                                      ASKUSE24
      weit   = 1.                                                       ASKUSE25
      sbeam  = ecm**2                                                   ASKUSE26
      n7lu   = 0                                                        ASKUSE27
      idpr   = 0                                                        ASKUSE28
      ipro   = 0                                                        ASKUSE29
      jchanh = 0                                                        ASKUSE30
      jchana = 0                                                        ASKUSE31
      ifs    = 0                                                        ASKUSE32
C                                                                       ASKUSE33
C  Store beam particles also in bos banks                               ASKUSE34
C                                                                       ASKUSE35
      ipart = kgpart(11)                                                ASKUSE36
      DO 2 itr = 1,2                                                    ASKUSE37
         DO 9 i=1,4                                                     ASKUSE38
 9       ptrak(i,itr) = 0.                                              ASKUSE39
         ipart = kgpart(11)                                             ASKUSE40
         ptrak(3,itr) = 0.5*ecm                                         ASKUSE41
         IF ( itr .EQ. 2 ) THEN                                         ASKUSE42
           ipart = kgpart(-11)                                          ASKUSE43
           ptrak(3,itr) =- 0.5*ecm                                      ASKUSE44
         ENDIF                                                          ASKUSE45
         ist=kbkine(-itr,ptrak(1,itr),ipart,0)                          ASKUSE46
         IF ( ist .LE. 0 ) THEN                                         ASKUSE47
            ista = -2                                                   ASKUSE48
            GO TO 998                                                   ASKUSE49
         ENDIF                                                          ASKUSE50
  2   CONTINUE                                                          ASKUSE51
C                                                                       ASKUSE52
C  Choice of the channel (if ipro = 0)                                  ASKUSE53
C                                                                       ASKUSE54
      sigtot = 0.                                                       ASKUSE55
      DO ipro = 1 , maxpro                                              ASKUSE56
        sigtot = sigtot + iproyn(ipro)*cross(ipro)*reduc(ipro)          ASKUSE57
      ENDDO                                                             ASKUSE58
C                                                                       ASKUSE59
      IF ( sigtot .EQ. 0. ) THEN                                        ASKUSE60
        WRITE(6,*) 'Not a single process has a non-vanishing',          ASKUSE61
     .             ' Cross section. Execution stopped.'                 ASKUSE62
        STOP 99                                                         ASKUSE63
      ENDIF                                                             ASKUSE64
C                                                                       ASKUSE65
      rnch = RNDM(dummy)                                                ASKUSE66
      rint = 0.                                                         ASKUSE67
      DO ipro = 1 , maxpro                                              ASKUSE68
        rint = rint + iproyn(ipro)*cross(ipro)*reduc(ipro) / sigtot     ASKUSE69
        if ( rnch .LT. rint ) GOTO 30                                   ASKUSE70
      ENDDO                                                             ASKUSE71
  30  CONTINUE                                                          ASKUSE72
C                                                                       ASKUSE73
C  Now generate the event                                               ASKUSE74
C                                                                       ASKUSE75
      IF ( cross(ipro)*reduc(ipro) .LE. 0. ) THEN                       ASKUSE76
        WRITE(6,*) 'Warning : Process ',chapro(ipro),                   ASKUSE77
     .  ' has a vanishing cross-section, Execution stopped'             ASKUSE78
        STOP 99                                                         ASKUSE79
      ENDIF                                                             ASKUSE80
C                                                                       ASKUSE81
      nevent(1) = nevent(1) + 1                                         ASKUSE82
C     WRITE(6,*) 'Event # ',nevent(1)                                   ASKUSE83
      nevpro(ipro) = nevpro(ipro) + 1                                   ASKUSE84
    1 CALL hzha(ipro,ecms,qg1,qg2,qh,qa,hh,izpol)                       ASKUSE85
C                                                                       ASKUSE86
C  Radiative photon in the initial state ?                              ASKUSE87
C                                                                       ASKUSE88
      IF ( xrad .GT. 0. ) THEN                                          ASKUSE89
        n7lu = n7lu + 1                                                 ASKUSE90
        kfg = 22                                                        ASKUSE91
        CALL hhlu1(n7lu,kfg,qg1(1),qg1(2),qg1(3),qg1(4),0.)             ASKUSE92
        k7lu(n7lu,1) = 1                                                ASKUSE93
        k7lu(n7lu,3) = 0                                                ASKUSE94
        n7lu = n7lu + 1                                                 ASKUSE95
        kfg = 22                                                        ASKUSE96
        CALL hhlu1(n7lu,kfg,qg2(1),qg2(2),qg2(3),qg2(4),0.)             ASKUSE97
        k7lu(n7lu,1) = 1                                                ASKUSE98
        k7lu(n7lu,3) = 0                                                ASKUSE99
      ENDIF                                                             ASKUS100
C                                                                       ASKUS101
      IF ( ipro .GE. 5 ) GOTO 1000                                      ASKUS102
C                                                                       ASKUS103
C  Virtual exchange boson (gamma or Z0)                                 ASKUS104
C                                                                       ASKUS105
      n7lu = n7lu + 1                                                   ASKUS106
      kfz = 23                                                          ASKUS107
      CALL hhlu1(n7lu,kfz,-qg1(1)-qg2(1),-qg1(2)-qg2(2),                ASKUS108
     .                    -qg1(3)-qg2(3),ecm-qg1(4)-qg2(4),-1.)         ASKUS109
      k7lu(n7lu,1) = 11                                                 ASKUS110
      k7lu(n7lu,3) = 0                                                  ASKUS111
      k7lu(n7lu,4) = n7lu + 1                                           ASKUS112
      k7lu(n7lu,5) = n7lu + 2                                           ASKUS113
      ipo1 = n7lu                                                       ASKUS114
C                                                                       ASKUS115
C  Fill the h (if IPRO = 1 or 3) or the H (if IPRO = 2 or 4)            ASKUS116
C                                                                       ASKUS117
      IF ( ipro .EQ. 1 .OR. ipro .EQ. 3 ) THEN                          ASKUS118
        kfh = 25                                                        ASKUS119
        jhig = 2                                                        ASKUS120
      ELSE                                                              ASKUS121
        kfh = 35                                                        ASKUS122
        jhig = 1                                                        ASKUS123
      ENDIF                                                             ASKUS124
      n7lu = n7lu + 1                                                   ASKUS125
      qhe = SQRT(qh(1)**2+qh(2)**2+qh(3)**2+qh(4)**2)                   ASKUS126
      CALL hhlu1(n7lu,kfh,qh(1),qh(2),qh(3),qhe,qh(4))                  ASKUS127
      k7lu(n7lu,1) = 1                                                  ASKUS128
      k7lu(n7lu,3) = ipo1                                               ASKUS129
      ipoh = n7lu                                                       ASKUS130
C                                                                       ASKUS131
C   Fill the Z0 (if IPRO = 1 or 2) or the A (if IPRO = 3 or 4)          ASKUS132
C                                                                       ASKUS133
      IF ( ipro .LE. 2 ) THEN                                           ASKUS134
        kfa = 23                                                        ASKUS135
      ELSE                                                              ASKUS136
        kfa = 36                                                        ASKUS137
      ENDIF                                                             ASKUS138
      n7lu = n7lu + 1                                                   ASKUS139
      qae = SQRT(qa(1)**2+qa(2)**2+qa(3)**2+qa(4)**2)                   ASKUS140
      CALL hhlu1(n7lu,kfa,qa(1),qa(2),qa(3),qae,qa(4))                  ASKUS141
      k7lu(n7lu,1) = 1                                                  ASKUS142
      k7lu(n7lu,3) = ipo1                                               ASKUS143
      ipoa = n7lu                                                       ASKUS144
C                                                                       ASKUS145
C  Decay the Higgses                                                    ASKUS146
C                                                                       ASKUS147
      CALL hhdecay(qh,jchanh,jhig,ipoh)                                 ASKUS148
      IF ( ipro.EQ.3 .OR. ipro.EQ.4 ) CALL hhdecay(qa,jchana,3,ipoa)    ASKUS149
      GOTO 100                                                          ASKUS150
C                                                                       ASKUS151
C Now the WW/ZZ fusion                                                  ASKUS152
C                                                                       ASKUS153
 1000 CONTINUE                                                          ASKUS154
C                                                                       ASKUS155
C Fill the neutrinos (WW) or the e+e- pair (ZZ)                         ASKUS156
C                                                                       ASKUS157
      IF ( ipro .EQ. 5 .OR. ipro .EQ. 6 ) THEN                          ASKUS158
        kfn = 12                                                        ASKUS159
      ELSEIF ( ipro .EQ. 7 .OR. ipro .EQ. 8 ) THEN                      ASKUS160
        kfn = 11                                                        ASKUS161
      ENDIF                                                             ASKUS162
      q5 = pmas(kfn,1)                                                  ASKUS163
C                                                                       ASKUS164
      n7lu = n7lu + 1                                                   ASKUS165
      CALL hhlu1(n7lu, kfn,qh(1),qh(2),qh(3),qh(4),q5)                  ASKUS166
      k7lu(n7lu,1) = 1                                                  ASKUS167
      k7lu(n7lu,3) = 0                                                  ASKUS168
C                                                                       ASKUS169
      n7lu = n7lu + 1                                                   ASKUS170
      CALL hhlu1(n7lu,-kfn,qa(1),qa(2),qa(3),qa(4),q5)                  ASKUS171
      k7lu(n7lu,1) = 1                                                  ASKUS172
      k7lu(n7lu,3) = 0                                                  ASKUS173
C                                                                       ASKUS174
C Fill the Higgs                                                        ASKUS175
C                                                                       ASKUS176
      IF ( ipro .EQ. 5 .OR. ipro .EQ. 7 ) THEN                          ASKUS177
        kfh = 25                                                        ASKUS178
        jhig = 2                                                        ASKUS179
      ELSEIF ( ipro .EQ. 6 .OR. ipro .EQ. 8 ) THEN                      ASKUS180
        kfh = 35                                                        ASKUS181
        jhig = 1                                                        ASKUS182
      ENDIF                                                             ASKUS183
C                                                                       ASKUS184
      n7lu = n7lu + 1                                                   ASKUS185
      hhe = SQRT(hh(1)**2+hh(2)**2+hh(3)**2+hh(4)**2)                   ASKUS186
      CALL hhlu1(n7lu,kfh,hh(1),hh(2),hh(3),hhe,hh(4))                  ASKUS187
      k7lu(n7lu,1) = 1                                                  ASKUS188
      k7lu(n7lu,3) = 0                                                  ASKUS189
      ipoh = n7lu                                                       ASKUS190
C                                                                       ASKUS191
C  Decay the Higgs                                                      ASKUS192
C                                                                       ASKUS193
      CALL hhdecay(hh,jchanh,jhig,ipoh)                                 ASKUS194
C                                                                       ASKUS195
C Is there any Higgses in the decay products?                           ASKUS196
C                                                                       ASKUS197
  100 nnhg = 0                                                          ASKUS198
      DO i7lu = 1 , n7lu                                                ASKUS199
        IF ( ( k7lu(i7lu,2) .EQ. 25 .OR.                                ASKUS200
     .         k7lu(i7lu,2) .EQ. 35 .OR.                                ASKUS201
     .         k7lu(i7lu,2) .EQ. 36 ) .AND.                             ASKUS202
     .         k7lu(i7lu,1) .EQ. 1 ) THEN                               ASKUS203
          ph(1) = p7lu(i7lu,1)                                          ASKUS204
          ph(2) = p7lu(i7lu,2)                                          ASKUS205
          ph(3) = p7lu(i7lu,3)                                          ASKUS206
          ph(4) = p7lu(i7lu,4)                                          ASKUS207
          iph = i7lu                                                    ASKUS208
          nnhg = nnhg + 1                                               ASKUS209
          IF ( k7lu(i7lu,2) .EQ. 35 ) khig = 1                          ASKUS210
          IF ( k7lu(i7lu,2) .EQ. 25 ) khig = 2                          ASKUS211
          IF ( k7lu(i7lu,2) .EQ. 36 ) khig = 3                          ASKUS212
          CALL hhdecay(ph,jchan,khig,iph)                               ASKUS213
        ENDIF                                                           ASKUS214
      ENDDO                                                             ASKUS215
      IF ( nnhg .GT. 0 ) GOTO 100                                       ASKUS216
C                                                                       ASKUS217
C Any neutralino or chargino in the decay products ?                    ASKUS218
C                                                                       ASKUS219
  200 nnchi = 0                                                         ASKUS220
      DO i7lu = 1 , n7lu                                                ASKUS221
        IF ( ABS(k7lu(i7lu,2)) .GE. 52 .AND.                            ASKUS222
     .       ABS(k7lu(i7lu,2)) .LE. 56 .AND.                            ASKUS223
     .         k7lu(i7lu,1) .EQ. 1 ) THEN                               ASKUS224
          ch(1) = p7lu(i7lu,1)                                          ASKUS225
          ch(2) = p7lu(i7lu,2)                                          ASKUS226
          ch(3) = p7lu(i7lu,3)                                          ASKUS227
          ch(4) = p7lu(i7lu,4)                                          ASKUS228
          ich = i7lu                                                    ASKUS229
          nnchi = nnchi + 1                                             ASKUS230
          IF ( ABS(k7lu(i7lu,2)) .LE. 54 ) THEN                         ASKUS231
            ichi1 = k7lu(i7lu,2)-50                                     ASKUS232
            ichi = 0                                                    ASKUS233
          ELSEIF ( k7lu(i7lu,2) .GE. 55 ) THEN                          ASKUS234
            ichi1 = k7lu(i7lu,2)-54                                     ASKUS235
            ichi = 1                                                    ASKUS236
          ELSE                                                          ASKUS237
            ichi1 = -k7lu(i7lu,2)-54                                    ASKUS238
            ichi = -1                                                   ASKUS239
          ENDIF                                                         ASKUS240
          CALL decchi(ch,ichi1,ichi,ich)                                ASKUS241
        ENDIF                                                           ASKUS242
      ENDDO                                                             ASKUS243
      IF ( nnchi .GT. 0 ) GOTO 200                                      ASKUS244
C                                                                       ASKUS245
C                                                                       ASKUS246
C  Decay the Z (if any!)                                                ASKUS247
C                                                                       ASKUS248
      IF ( ipro .LT. 3 ) THEN                                           ASKUS249
        IF ( iklei .EQ. 0 ) THEN                                        ASKUS250
          CALL zdecay(qa,izpol,q1,q2,ifs)                               ASKUS251
        ELSE                                                            ASKUS252
          ifs = izpol                                                   ASKUS253
          DO i = 1,3                                                    ASKUS254
            q2(i) = hh(i)                                               ASKUS255
            q1(i) = qa(i)-q2(i)                                         ASKUS256
          ENDDO                                                         ASKUS257
        ENDIF                                                           ASKUS258
        kff = kcode(ifs)                                                ASKUS259
        q5 = ulmass(kff)                                                ASKUS260
        q1(4) = SQRT(q1(1)**2+q1(2)**2+q1(3)**2+q5**2)                  ASKUS261
        q2(4) = SQRT(q2(1)**2+q2(2)**2+q2(3)**2+q5**2)                  ASKUS262
C                                                                       ASKUS263
C FSR for charged leptons                                               ASKUS264
C                                                                       ASKUS265
        IF ( ifs .EQ. 1 .OR. ifs .EQ. 3 .OR. ifs .EQ. 5 ) THEN          ASKUS266
C                                                                       ASKUS267
          DO id = 1, 4                                                  ASKUS268
             qin1(id) = DBLE(q1(id))                                    ASKUS269
             qin2(id) = DBLE(q2(id))                                    ASKUS270
          ENDDO                                                         ASKUS271
C                                                                       ASKUS272
          CALL addglu(qin1,qin2,qout1,qout2,qout3,0)                    ASKUS273
C                                                                       ASKUS274
C Put back the leptons                                                  ASKUS275
C                                                                       ASKUS276
          DO ID = 1, 4                                                  ASKUS277
             q1(id) = REAL(qout1(id))                                   ASKUS278
             q2(id) = REAL(qout2(id))                                   ASKUS279
             q3(id) = REAL(qout3(id))                                   ASKUS280
          ENDDO                                                         ASKUS281
C                                                                       ASKUS282
C  Feed LUND with the 4-vectors                                         ASKUS283
C                                                                       ASKUS284
          k7lu(ipoa,4) = n7lu+1                                         ASKUS285
          IF ( q3(4) .GT. 0. ) THEN                                     ASKUS286
            n7lu = n7lu + 1                                             ASKUS287
            kfg = 22                                                    ASKUS288
            CALL hhlu1(n7lu,kfg,q3(1),q3(2),q3(3),q3(4),0.)             ASKUS289
            k7lu(n7lu,3) = ipoa                                         ASKUS290
          ENDIF                                                         ASKUS291
C                                                                       ASKUS292
        ENDIF                                                           ASKUS293
C                                                                       ASKUS294
        n7lu = n7lu + 1                                                 ASKUS295
        ijoin(1) = n7lu                                                 ASKUS296
        CALL hhlu1(n7lu,kff,q1(1),q1(2),q1(3),q1(4),q5)                 ASKUS297
        k7lu(ijoin(1),3) = ipoa                                         ASKUS298
C                                                                       ASKUS299
        n7lu = n7lu + 1                                                 ASKUS300
        ijoin(2) = n7lu                                                 ASKUS301
        CALL hhlu1(n7lu,-kff,q2(1),q2(2),q2(3),q2(4),q5)                ASKUS302
        k7lu(ijoin(2),3) = ipoa                                         ASKUS303
        k7lu(ipoa,5) = n7lu                                             ASKUS304
C                                                                       ASKUS305
C  Prepare and execute parton shower for quarks                         ASKUS306
C                                                                       ASKUS307
        xmm    = qa(4)                                                  ASKUS308
        IF ( kff .LT. 6 ) THEN                                          ASKUS309
          k7lu(ijoin(1),1) = 2                                          ASKUS310
          njoin = 2                                                     ASKUS311
          CALL lujoin(njoin,ijoin)                                      ASKUS312
          CALL lushow(ijoin(1), ijoin(2), xmm)                          ASKUS313
        ENDIF                                                           ASKUS314
C                                                                       ASKUS315
C Don/t decay the Z twice!                                              ASKUS316
C                                                                       ASKUS317
        k7lu(ipoa,1) = 11                                               ASKUS318
C                                                                       ASKUS319
      ENDIF                                                             ASKUS320
C                                                                       ASKUS321
C  Other decays and fragmentation                                       ASKUS322
C                                                                       ASKUS323
      CALL luexec                                                       ASKUS324
C                                                                       ASKUS325
C  Listing of the event                                                 ASKUS326
C                                                                       ASKUS327
      IF ( nevent(1) .GE. idb1 .AND.                                    ASKUS328
     .     nevent(1) .LE. idb2 ) CALL lulist(1)                         ASKUS329
C                                                                       ASKUS330
C  Smear vertex position                                                ASKUS331
C                                                                       ASKUS332
      CALL rannor (rx,ry)                                               ASKUS333
      CALL rannor (rz,dum)                                              ASKUS334
      vrtx(1) = rx*sdvrt(1)                                             ASKUS335
      vrtx(2) = ry*sdvrt(2)                                             ASKUS336
      vrtx(3) = rz*sdvrt(3)                                             ASKUS337
      vrtx(4) = 0.                                                      ASKUS338
C                                                                       ASKUS339
C  Event header                                                         ASKUS340
C                                                                       ASKUS341
      idpr = MAX(ifs,jchanA) + 100*jchanh + 10000*ipro                  ASKUS342
C                                                                       ASKUS343
C      Call the specific routine KXL7AL to fill BOS banks               ASKUS344
C                                                                       ASKUS345
      CALL kxl7al (vrtx,ist,nivx,nitr)                                  ASKUS346
      IF ( ist .NE. 0 ) GOTO 998                                        ASKUS347
C                                                                       ASKUS348
C  Event counters                                                       ASKUS349
C                                                                       ASKUS350
  998 ista = ist                                                        ASKUS351
      IF ( ist .EQ. 0 ) nevent(2) = nevent(2) + 1                       ASKUS352
      IF ( ist .GT. 0) THEN                                             ASKUS353
        nevent(3) = nevent(3) + 1                                       ASKUS354
        nevent(4) = nevent(4) + 1                                       ASKUS355
        WRITE(6,*) 'Evt ',nevent(1),' ist = ',ist                       ASKUS356
        CALL lulist(1)                                                  ASKUS357
      ELSEIF ( ist .LT. 0) THEN                                         ASKUS358
        nevent(3) = nevent(3) + 1                                       ASKUS359
        nevent(4-ist) = nevent(4-ist) + 1                               ASKUS360
      ENDIF                                                             ASKUS361
C                                                                       ASKUS362
      RETURN                                                            ASKUS363
      END                                                               ASKUS364
      SUBROUTINE USCJOB                                                 USCJOB 2
C-----------------------------------------------------------------------USCJOB 3
C! Routine for printout at the end of a run                             USCJOB 4
C                                                                       USCJOB 5
C-----------------------------------------------------------------------USCJOB 6
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      PARAMETER (maxpro= 8)                                             HHPROD 2
      COMMON / cropro / cross(maxpro), sthr(maxpro), reduc(maxpro)      HHPROD 3
      CHARACTER*14 chapro(maxpro)                                       HHPROD 4
      DATA chapro /                                                     HHPROD 5
     .             'e+e- --> h Z',                                      HHPROD 6
     .             'e+e- --> H Z',                                      HHPROD 7
     .             'e+e- --> h A',                                      HHPROD 8
     .             'e+e- --> H A',                                      HHPROD 9
     .             'W+W- --> h  ',                                      HHPROD10
     .             'W+W- --> H  ',                                      HHPROD11
     .             'Z Z  --> h  ',                                      HHPROD12
     .             'Z Z  --> H  '                                       HHPROD13
     .                                /                                 HHPROD14
      COMMON / prosim / iproyn(maxpro),nevpro(maxpro)                   HHPROD15
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,      MISCL  2
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),            MISCL  3
     &                 nevent(11)                                       MISCL  4
      INTEGER loutbe,idb1,idb2,nevent                                   MISCL  5
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad                      MISCL  6
C                                                                       MISCL  7
      REAL DUMMY(4)                                                     USCJOB10
C                                                                       USCJOB11
      CALL hhfini                                                       USCJOB12
C                                                                       USCJOB13
      DO ipr = 1, 4                                                     USCJOB14
        ipro = ipr + 4                                                  USCJOB15
        IF ( ntry(ipr) .GT. 0 ) THEN                                    USCJOB16
          sigto = wtot(ipr)/FLOAT(ntry(ipr))                            USCJOB17
          dsig  = SQRT((wtot2(ipr)/FLOAT(ntry(ipr)) - sigto**2)         USCJOB18
     .          / FLOAT(ntry(ipr)))                                     USCJOB19
        WRITE(6,*) ' '                                                  USCJOB20
        WRITE(6,*) '--------------------------------------------------' USCJOB21
        WRITE(6,*) ' Generated cross section for ',chapro(ipro),' :'    USCJOB22
        WRITE(6,*) '    Sigma = ',sigto,' +/- ',dsig,' fb'              USCJOB23
        WRITE(6,*) '    # of trials          : ',ntry(ipr)              USCJOB24
        WRITE(6,*) '    # of accepted events : ',nacc(ipr)              USCJOB25
        WRITE(6,*) '--------------------------------------------------' USCJOB26
        WRITE(6,*) ' '                                                  USCJOB27
        ENDIF                                                           USCJOB28
      ENDDO                                                             USCJOB29
C                                                                       USCJOB30
      WRITE(6,*) '--------------------------------------------------'   USCJOB31
      WRITE(6,*) 'Number of events generated in each channel : '        USCJOB32
      WRITE(6,*) '--------------------------------------------------'   USCJOB33
      DO ipro = 1, maxpro                                               USCJOB34
        WRITE(6,*) chapro(ipro),' : ',nevpro(ipro),' events.'           USCJOB35
      ENDDO                                                             USCJOB36
      WRITE(6,*) '--------------------------------------------------'   USCJOB37
C                                                                       USCJOB38
C Maximum weight reached                                                USCJOB39
C                                                                       USCJOB40
      WRITE (LOUTBE,100) EMPIRM                                         USCJOB41
  100 FORMAT (//1X,'+++++MAXIMUM WEIGHT REACHED   ++++++',F10.3)        USCJOB42
C                                                                       USCJOB43
C                                                                       USCJOB44
C Print event counters                                                  USCJOB45
C                                                                       USCJOB46
       WRITE(LOUTBE,101)                                                USCJOB47
  101  FORMAT(//20X,'EVENTS STATISTICS',                                USCJOB48
     &         /20X,'*****************')                                USCJOB49
       WRITE(LOUTBE,102) NEVENT(1),NEVENT(2),NEVENT(3)                  USCJOB50
  102  FORMAT(/5X,'# OF GENERATED EVENTS                      = ',I10,  USCJOB51
     &        /5X,'# OF ACCEPTED  EVENTS                      = ',I10,  USCJOB52
     &        /5X,'# OF REJECTED  EVENTS (ISTA # 0 in ASKUSE) = ',I10)  USCJOB53
       WRITE(LOUTBE,103)                                                USCJOB54
  103  FORMAT(//20X,'ERRORS STATISTICS',                                USCJOB55
     &         /20X,'*****************')                                USCJOB56
       WRITE(LOUTBE,104) (NEVENT(I),I=4,11)                             USCJOB57
  104  FORMAT(/10X,'IR= 1 LUND ERROR unknown part   # OF REJECT = ',I10,USCJOB58
     &        /10X,'IR= 2 KINE/VERT banks missing   # OF REJECT = ',I10,USCJOB59
     &        /10X,'IR= 3 no space for VERT/KINE    # OF REJECT = ',I10,USCJOB60
     &        /10X,'IR= 4 LUND ERROR too many tracks# OF REJECT = ',I10,USCJOB61
     &        /10X,'IR= 5 LUND ERROR Beam wrong pos # OF REJECT = ',I10,USCJOB62
     &        /10X,'IR= 6 LUND ERROR Status code >5 # OF REJECT = ',I10,USCJOB63
     &        /10X,'IR= 7 free for user             # OF REJECT = ',I10,USCJOB64
     &        /10X,'IR= 8 free for user             # OF REJECT = ',I10)USCJOB65
C                                                                       USCJOB66
      RETURN                                                            USCJOB67
      END                                                               USCJOB68
      SUBROUTINE ADDGLU(QIN1,QIN2,QOUT1,QOUT2,QOUT3,IOPT)               ADDGLU 2
C-----------------------------------------------------------            ADDGLU 3
C                                                                       ADDGLU 4
C!EVENT GENERATOR AFFIX TO GO FROM TWO TO THREE JETS IN THE             ADDGLU 5
C!DECAY OF A HEAVY COLOUR-SINGLET VECTOR BOSON (Z,W,...)                ADDGLU 6
C THE MOMENTA 'QIN' ARE INPUT, THE MOMENTA 'QOUT' ARE OUTPUT            ADDGLU 7
C                                                                       ADDGLU 8
CRK-----------------                                                    ADDGLU 9
C IOPT=0 : THE CORRECT ALGORITHM                                        ADDGLU10
C IOPT=1 : CHOOSE X'S WITH EQUAL PROBABILITY                            ADDGLU11
C IOPT=2 : CHOOSE THE LARGEST OF THE X'S                                ADDGLU12
C IOPT=3 : CHOOSE ALWAYS X1                                             ADDGLU13
C-----------------------------------------------------------            ADDGLU14
      IMPLICIT REAL*8(A-H,O-Z)                                          ADDGLU15
      REAL*4 RNDM                                                       ADDGLU16
      EXTERNAL RNDM                                                     ADDGLU17
      DIMENSION QIN1(4),QIN2(4),QOUT1(4),QOUT2(4),QOUT3(4)              ADDGLU18
      DIMENSION Q(4),R1(4),R2(4),E1(3),E2(3)                            ADDGLU19
      DATA PI/3.1415926536D0/                                           ADDGLU20
      DATA XLAM/0.2D0/                                                  ADDGLU21
C                                                                       ADDGLU22
C CONSTRUCT THE OVERALL MOMENTUM FROM THE QIN, AND ITS MASS             ADDGLU23
      DO 101 K=1,4                                                      ADDGLU24
  101 Q(K)=QIN1(K)+QIN2(K)                                              ADDGLU25
      W=DSQRT(Q(4)**2-Q(3)**2-Q(2)**2-Q(1)**2)                          ADDGLU26
C                                                                       ADDGLU27
C CONSTRUCT QIN1,2 IN Q'S C.M. FRAME, CALL'EM R1,2                      ADDGLU28
      R1(4)=.5*W                                                        ADDGLU29
      R2(4)=W-R1(4)                                                     ADDGLU30
      RR=(QIN1(4)+R1(4))/(Q(4)+W)                                       ADDGLU31
      DO 102 K=1,3                                                      ADDGLU32
      R1(K)=QIN1(K)-RR*Q(K)                                             ADDGLU33
  102 R2(K)=-R1(K)                                                      ADDGLU34
C                                                                       ADDGLU35
C COMPUTE THE VALUE OF ALFA(S), ASSUMING THREE                          ADDGLU36
C QUARK GENERATRIONS AND LAMBDA(QCD)=0.2 GEV                            ADDGLU37
C     ALFAS=6.*PI/27./DLOG(W/XLAM)                                      ADDGLU38
      ALFAS=1.D0/137.036D0                                              ADDGLU39
C                                                                       ADDGLU40
C COMPUTE GAMMA, ASSUMING THE SMALL-GAMMA APPROXIMATION IS VALID        ADDGLU41
C (CHECKED OUT AS HELPED BY ROEL'S ZX SPECTRUM)                         ADDGLU42
      GG=2.5-PI**2/3.-3.*(ALFAS+PI)/(2.*ALFAS)                          ADDGLU43
      G=DEXP(.75*(-1.-DSQRT(1.-8.*GG/9.)))                              ADDGLU44
C      PRINT *,'GAMMA =',G                                              ADDGLU45
C      IF(1.EQ.1) STOP                                                  ADDGLU46
C                                                                       ADDGLU47
C GENERATE THE VALUES FOR X1 AND X2 BY MAPPING AND W.R.P.               ADDGLU48
  301 X1=1.-(1.-G)/DEXP(RNDM(1)*DLOG((1.-G)/G))                         ADDGLU49
      X2=1.-(1.-G)/DEXP(RNDM(2)*DLOG((1.-G)/G))                         ADDGLU50
      IF((X1+X2).GT.1.D0) GOTO 302                                      ADDGLU51
      X1=1.-X1                                                          ADDGLU52
      X2=1.-X2                                                          ADDGLU53
  302 CONTINUE                                                          ADDGLU54
      WEIGHT=.5*X1*X2*(X1*X1+X2*X2)/(X1*X2+(1.-X1)*(1.-X2))             ADDGLU55
      WMAX=(1.-G)**4/((1.-G)**2+G**2)                                   ADDGLU56
      IF(WMAX.LT.WEIGHT) PRINT 303,G,X1,X2,WMAX,WEIGHT                  ADDGLU57
  303 FORMAT(' WEIGHT ANOMALY:',5D15.6)                                 ADDGLU58
      IF(RNDM(3).GT.(WEIGHT/WMAX)) GOTO 301                             ADDGLU59
C                                                                       ADDGLU60
C CHOOSE BETWEEN THE TWO POSSIBLE ORIENTATIONS                          ADDGLU61
C OPTIONS 1,2 AND 3 ARE WRONG: OPTION 0 GIVES THE CORRECT RESULT        ADDGLU62
CRK-------------                                                        ADDGLU63
      IF(IOPT.NE.0) GOTO 351                                            ADDGLU64
      XX=X1**2/(X1**2+X2**2)                                            ADDGLU65
C      CALL HISTO1(4,10,0.D0,1.D0,XX,1.D0)                              ADDGLU66
      IF(RNDM(4).GT.XX) GOTO 501                                        ADDGLU67
      GOTO 401                                                          ADDGLU68
CRK-------------                                                        ADDGLU69
  351 IF(IOPT.NE.1) GOTO 352                                            ADDGLU70
      IF(RNDM(4).GT.0.5D0) GOTO 501                                     ADDGLU71
      GOTO 401                                                          ADDGLU72
CRK-------------                                                        ADDGLU73
  352 IF(IOPT.NE.2) GOTO 353                                            ADDGLU74
      IF(X2.GT.X1) GOTO 501                                             ADDGLU75
      GOTO 401                                                          ADDGLU76
CRK-------------                                                        ADDGLU77
  353 IF(IOPT.NE.3) GOTO 354                                            ADDGLU78
      GOTO 401                                                          ADDGLU79
CRK-------------                                                        ADDGLU80
  354 PRINT 355,IOPT                                                    ADDGLU81
  355 FORMAT(' WRONG OPTION: IOPT=',I10)                                ADDGLU82
      STOP                                                              ADDGLU83
CRK-------------                                                        ADDGLU84
C                                                                       ADDGLU85
C CASE A: Q1 RETAINS ITS ORIGINAL DIRECTION                             ADDGLU86
C COMPUTE THE ANGULAR PARAMETERS OF THE Q1 DIRECTION...                 ADDGLU87
  401 DO 402 K=1,3                                                      ADDGLU88
  402 E1(K)=R1(K)/R1(4)                                                 ADDGLU89
      C=E1(3)                                                           ADDGLU90
      S=DSQRT(1.-C**2)                                                  ADDGLU91
      CF=E1(2)/S                                                        ADDGLU92
      SF=E1(1)/S                                                        ADDGLU93
C ...THE ANGLES OF Q2 W.R.T Q1...                                       ADDGLU94
      C12=1.-2./X1-2./X2+2./X1/X2                                       ADDGLU95
      S12=DSQRT(1.-C12**2)                                              ADDGLU96
      PSI=2.*PI*RNDM(5)                                                 ADDGLU97
      CP=DCOS(PSI)                                                      ADDGLU98
      SP=DSIN(PSI)                                                      ADDGLU99
C ...AND COMPUTE THE DIRECTION OF Q2                                    ADDGL100
      E2(1)=SF*(C*S12*SP+S*C12)-CF*S12*CP                               ADDGL101
      E2(2)=CF*(C*S12*SP+S*C12)+SF*S12*CP                               ADDGL102
      E2(3)=-S*S12*SP+C*C12                                             ADDGL103
      GOTO 601                                                          ADDGL104
C                                                                       ADDGL105
C CASE B: Q2 RETAINS ITS ORIGINAL DIRECTION                             ADDGL106
C COMPUTE THE ANGULAR PARAMETERS OF THE Q2 DIRECTION...                 ADDGL107
  501 DO 502 K=1,3                                                      ADDGL108
  502 E2(K)=R2(K)/R2(4)                                                 ADDGL109
      C=E2(3)                                                           ADDGL110
      S=DSQRT(1.-C**2)                                                  ADDGL111
      CF=E2(2)/S                                                        ADDGL112
      SF=E2(1)/S                                                        ADDGL113
C ...THE ANGLES OF Q1 W.R.T Q2...                                       ADDGL114
      C12=1.-2./X1-2./X2+2./X1/X2                                       ADDGL115
      S12=DSQRT(1.-C12**2)                                              ADDGL116
      PSI=2.*PI*RNDM(5)                                                 ADDGL117
      CP=DCOS(PSI)                                                      ADDGL118
      SP=DSIN(PSI)                                                      ADDGL119
C ...AND COMPUTE THE DIRECTION OF Q1                                    ADDGL120
      E1(1)=SF*(C*S12*SP+S*C12)-CF*S12*CP                               ADDGL121
      E1(2)=CF*(C*S12*SP+S*C12)+SF*S12*CP                               ADDGL122
      E1(3)=-S*S12*SP+C*C12                                             ADDGL123
C                                                                       ADDGL124
C RETURN FROM BOTH CASES: CONSTRUCT Q1 AND Q2 IN THE C.M. FRAME...      ADDGL125
  601 R1(4)=X1*R1(4)                                                    ADDGL126
      R2(4)=X2*R2(4)                                                    ADDGL127
      DO 602 K=1,3                                                      ADDGL128
      R1(K)=R1(4)*E1(K)                                                 ADDGL129
  602 R2(K)=R2(4)*E2(K)                                                 ADDGL130
C ...BOOST'EM TO THE LAB FRAME...                                       ADDGL131
      QOUT1(4)=(Q(4)*R1(4)+Q(3)*R1(3)+Q(2)*R1(2)+Q(1)*R1(1))/W          ADDGL132
      QOUT2(4)=(Q(4)*R2(4)+Q(3)*R2(3)+Q(2)*R2(2)+Q(1)*R2(1))/W          ADDGL133
      QQ1=(QOUT1(4)+R1(4))/(Q(4)+W)                                     ADDGL134
      QQ2=(QOUT2(4)+R2(4))/(Q(4)+W)                                     ADDGL135
      DO 603 K=1,3                                                      ADDGL136
      QOUT1(K)=R1(K)+Q(K)*QQ1                                           ADDGL137
  603 QOUT2(K)=R2(K)+Q(K)*QQ2                                           ADDGL138
C ...AND CONSTRUCT THE GLUON MOMENTUM BY MOMENTUM CONSERVATION          ADDGL139
      DO 605 K=1,4                                                      ADDGL140
  605 QOUT3(K)=Q(K)-QOUT1(K)-QOUT2(K)                                   ADDGL141
      RETURN                                                            ADDGL142
      END                                                               ADDGL143
      FUNCTION brelec(xx)                                               BRELEC 2
C-------------------------------------------------------------------    BRELEC 3
C! Compute the decay branching ratio of W* --> e nue                    BRELEC 4
C  when produced in the chi' --> chi+ W* decay.                         BRELEC 5
C                                                                       BRELEC 6
C  Input:     -- xx,  the mass difference chi'-chi+                     BRELEC 7
C                                                                       BRELEC 8
C  Output:    -- brelec, the decay branching ratio                      BRELEC 9
C                                                                       BRELEC10
C  V. Bertin, for the CHA001 generator                                  BRELEC11
C                                                                       BRELEC12
C  Modif:  Patrick Janot (31 Aug 1995)                                  BRELEC13
C          Adapt the routine for the HZHAxx generator                   BRELEC14
C-------------------------------------------------------------------    BRELEC15
      DIMENSION seuil(4),brtot(5)                                       BRELEC16
      DATA seuil/.106,.140,1.8,2.0/                                     BRELEC17
      DATA brtot/1.,2.,5.,6.,9./                                        BRELEC18
C                                                                       BRELEC19
      br = brtot(1)                                                     BRELEC20
      DO is = 1,4                                                       BRELEC21
       IF ( xx .GE. seuil(is) ) br = brtot(is+1)                        BRELEC22
      ENDDO                                                             BRELEC23
      brelec = 1. / br                                                  BRELEC24
C                                                                       BRELEC25
      RETURN                                                            BRELEC26
      END                                                               BRELEC27
      FUNCTION brnunu(xh,xl,icp)                                        BRNUNU 2
C-------------------------------------------------------------------    BRNUNU 3
C! Compute the decay branching ratio of Z* --> nu nubar                 BRNUNU 4
C  when produced in the chi' --> chi Z* decay.                          BRNUNU 5
C                                                                       BRNUNU 6
C  Input:     -- xh,  the first neutralino mass                         BRNUNU 7
C             -- xl,  the second neutralino mass                        BRNUNU 8
C             -- icp, the relative mass sign                            BRNUNU 9
C                                                                       BRNUNU10
C  Output:    -- brnunu, the decay branching ratio                      BRNUNU11
C                                                                       BRNUNU12
C  V. Bertin, for the CHA001 generator                                  BRNUNU13
C                                                                       BRNUNU14
C  Modif:  Patrick Janot (31 Aug 1995)                                  BRNUNU15
C          Adapt the routine for the HZHAxx generator                   BRNUNU16
C-------------------------------------------------------------------    BRNUNU17
      REAL*8 xh,xl                                                      BRNUNU18
      DIMENSION dlim(19),bnup(20),bnum(20)                              BRNUNU19
      DATA dlim/.001,.212,.270,.988,1.5,2.,3.,4.,5.,7.,8.,10.,          BRNUNU20
     .          12.,15.,20.,25.,30.,60.,90./                            BRNUNU21
      DATA bnup/1.,.856,.774,.600,.497,.356,.323,.303,.286,.271,.256,   BRNUNU22
     .          .256,.248,.234,.222,.213,.209,.2075,.205,.204/          BRNUNU23
      DATA bnum/1.,.857,.729,.465,.389,.289,.275,.278,.248,.234,.232,   BRNUNU24
     .          .232,.232,.217,.209,.203,.203,.203,.203,.204/           BRNUNU25
C                                                                       BRNUNU26
      dx = xh-xl                                                        BRNUNU27
      IF ( icp .GE. 0 ) brneu = bnup(1)                                 BRNUNU28
      IF ( icp .LT. 0 ) brneu = bnum(1)                                 BRNUNU29
      DO idm = 1,18                                                     BRNUNU30
       IF ( dx .LE. dlim(3) ) THEN                                      BRNUNU31
        IF ( dx .GT. dlim(idm) .AND. dx .LE. dlim(idm+1) ) THEN         BRNUNU32
         IF ( icp .GE. 0 ) brneu = bnup(idm+1)                          BRNUNU33
         IF ( icp .LT. 0 ) brneu = bnum(idm+1)                          BRNUNU34
        ENDIF                                                           BRNUNU35
       ELSE IF ( dx .LT. dlim(19) ) THEN                                BRNUNU36
        IF ( dx .GT. dlim(idm) .AND. dx .LE. dlim(idm+1) ) THEN         BRNUNU37
         IF ( icp .GE. 0 ) THEN                                         BRNUNU38
          brneu = (bnup(idm+1) - bnup(idm)) * (dx - dlim(idm))          BRNUNU39
     .           / (dlim(idm+1) - dlim(idm)) + bnup(idm)                BRNUNU40
         ELSE                                                           BRNUNU41
          brneu = (bnum(idm+1) - bnum(idm)) * (dx - dlim(idm))          BRNUNU42
     .           / (dlim(idm+1) - dlim(idm)) + bnum(idm)                BRNUNU43
         ENDIF                                                          BRNUNU44
        ENDIF                                                           BRNUNU45
       ELSE                                                             BRNUNU46
        IF ( icp .GE. 0 ) brneu = bnup(20)                              BRNUNU47
        IF ( icp .LT. 0 ) brneu = bnum(20)                              BRNUNU48
       ENDIF                                                            BRNUNU49
      ENDDO                                                             BRNUNU50
C                                                                       BRNUNU51
      brnunu = brneu                                                    BRNUNU52
C                                                                       BRNUNU53
      RETURN                                                            BRNUNU54
      END                                                               BRNUNU55
      FUNCTION brwipj(a,b,c)                                            BRWIPJ 2
C ------------------------------------------------------------------    BRWIPJ 3
C! Perfom a 1-dim convolution Sigma(hZ) * Breit Wigner                  BRWIPJ 4
C                                                                       BRWIPJ 5
C  Inputs:        a   is ecm**2                                         BRWIPJ 6
C                 b   is the Higgs mass                                 BRWIPJ 7
C                 c,  is the Higgs width                                BRWIPJ 8
C                                                                       BRWIPJ 9
C  Output:        brwipj, the hZ cross section with width effects       BRWIPJ10
C                                                                       BRWIPJ11
C  Patrick Janot -- 01 Sep 1995                                         BRWIPJ12
C -------------------------------------------------------------------   BRWIPJ13
      IMPLICIT REAL*8(A-H,O-Z)                                          BRWIPJ14
      REAL*4 a,b,c                                                      BRWIPJ15
      COMMON /BHV/ s,am1,w1                                             BRWIPJ16
      EXTERNAL fsub                                                     BRWIPJ17
      DIMENSION x(1)                                                    BRWIPJ18
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)        DATAPI 2
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          DATAPI 3
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         DATAPI 4
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         DATAPI 5
C                                                                       DATAPI 6
      s      = a                                                        BRWIPJ20
      am1    = b                                                        BRWIPJ21
      w1     = c                                                        BRWIPJ22
      xlo    = -DATAN2(am1,w1)                                          BRWIPJ23
      xhi    =  DATAN2(s-am1**2,am1*w1)                                 BRWIPJ24
      brwipj = DGMLT1(fsub,xlo,xhi,1,6,x)                               BRWIPJ25
     .       / (piby2+DATAN2(am1,w1))                                   BRWIPJ26
      RETURN                                                            BRWIPJ27
      END                                                               BRWIPJ28
      FUNCTION brwisi(a,b,c,d,e)                                        BRWISI 2
C -------------------------------------------------------------------   BRWISI 3
C! Perfom a 2-dim convolution Sigma(hA)*BreitWigner(h)*BreitWigner(A)   BRWISI 4
C                                                                       BRWISI 5
C  Inputs          a   is ecm**2                                        BRWISI 6
C                  b,c are h,A on-shell masses                          BRWISI 7
C                  d,e are h,A widths                                   BRWISI 8
C                                                                       BRWISI 9
C  Output:         brwisi, the hA cross section with width effects      BRWISI10
C                                                                       BRWISI11
C  Patrick Janot -- 01 Sep 1995                                         BRWISI12
C -------------------------------------------------------------------   BRWISI13
      IMPLICIT REAL*8(A-H,O-Z)                                          BRWISI14
      REAL*4 a,b,c,d,e                                                  BRWISI15
      COMMON /BWC/ s,am1,am2,w1,w2                                      BRWISI16
      EXTERNAL fsub2                                                    BRWISI17
      DIMENSION x(2)                                                    BRWISI18
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)        DATAPI 2
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          DATAPI 3
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         DATAPI 4
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         DATAPI 5
C                                                                       DATAPI 6
      s      = a                                                        BRWISI20
      am1    = b                                                        BRWISI21
      am2    = c                                                        BRWISI22
      w1     = d                                                        BRWISI23
      w2     = e                                                        BRWISI24
      x2lo   = -DATAN2(am2,w2)                                          BRWISI25
      x2hi   =  DATAN2(s-am2**2,am2*w2)                                 BRWISI26
      brwisi = DGMLT2(fsub2,x2lo,x2hi,1,6,x)                            BRWISI27
     .       / (piby2+DATAN2(am1,w1))                                   BRWISI28
     .       / (piby2+DATAN2(am2,w2))                                   BRWISI29
C                                                                       BRWISI30
      RETURN                                                            BRWISI31
      END                                                               BRWISI32
      FUNCTION brwwzz(a,b,c,ityp)                                       BRWWZZ 2
C ------------------------------------------------------------------    BRWWZZ 3
C! Compute the decay width of a Higgs boson into W+W- and Z Z           BRWWZZ 4
C  with a 2-dim convolution phase space * Breit Wigner                  BRWWZZ 5
C                                                                       BRWWZZ 6
C  Inputs :   a   is mh (Higgs mass)                                    BRWWZZ 7
C             b   is mv (Boson mass)                                    BRWWZZ 8
C             c   is gv (Boson width)                                   BRWWZZ 9
C             ityp = 1 or 2 for two different ways of copmputing        BRWWZZ10
C                                                                       BRWWZZ11
C Patrick Janot. 19 Sep 1995.                                           BRWWZZ12
C -------------------------------------------------------------------   BRWWZZ13
      IMPLICIT REAL*8(A-H,O-Z)                                          BRWWZZ14
      REAL*4 a,b,c                                                      BRWWZZ15
      COMMON /BWC/ s,am1,am2,w1,w2                                      BRWWZZ16
      EXTERNAL fsubwz2                                                  BRWWZZ17
      DIMENSION x(2)                                                    BRWWZZ18
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)        DATAPI 2
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          DATAPI 3
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         DATAPI 4
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         DATAPI 5
C                                                                       DATAPI 6
      s      = a**2                                                     BRWWZZ20
      am1    = b                                                        BRWWZZ21
      am2    = b                                                        BRWWZZ22
      w1     = c                                                        BRWWZZ23
      w2     = c                                                        BRWWZZ24
      x2lo   = -DATAN2(am2,w2)                                          BRWWZZ25
      x2hi   =  DATAN2(s-am2**2,am2*w2)                                 BRWWZZ26
      brwwzz = DGMLT2(fsubwz2,x2lo,x2hi,4,8,x)                          BRWWZZ27
C    .       / (x2lo-x2hi)**2                                           BRWWZZ28
     .       / (piby2+DATAN2(am1,w1))                                   BRWWZZ29
     .       / (piby2+DATAN2(am2,w2))                                   BRWWZZ30
C                                                                       BRWWZZ31
      RETURN                                                            BRWWZZ32
      END                                                               BRWWZZ33
      SUBROUTINE bwgene(xmin,xmax,x,g,w,dj)                             BWGENE 2
C-----------------------------------------------------------------      BWGENE 3
C! Generate w with a Breit-Wigner probability, centered on x            BWGENE 4
C  and of width g. Variable dj is the corresponding Jacobian.           BWGENE 5
C                                                                       BWGENE 6
C  Patrick Janot -- 15 Oct. 1992                                        BWGENE 7
C-----------------------------------------------------------------      BWGENE 8
      IMPLICIT REAL*8(A-H,O-Z)                                          BWGENE 9
      REAL*4 xmin,xmax,x,g,w,dj,RNDM                                    BWGENE10
      EXTERNAL RNDM                                                     BWGENE11
C                                                                       BWGENE12
      argmin = (xmin**2-x**2)/(x*g)                                     BWGENE13
      argmax = (xmax**2-x**2)/(x*g)                                     BWGENE14
      w2 = x**2 + x * g *  DTAN (                                       BWGENE15
     .                   ( DATAN(argmax) - DATAN(argmin) )              BWGENE16
     .                   * RNDM(w2)                                     BWGENE17
     .                   + DATAN(argmin) )                              BWGENE18
C                                                                       BWGENE19
      w  = DSQRT(w2)                                                    BWGENE20
C                                                                       BWGENE21
      dj = DATAN(argmax)-DATAN(argmin)                                  BWGENE22
      dj = dj / (x*g)                                                   BWGENE23
C                                                                       BWGENE24
      RETURN                                                            BWGENE25
      END                                                               BWGENE26
      SUBROUTINE chaneucp                                               CHANEUC2
C------------------------------------------------------------------     CHANEUC3
C!  Compute neutralino and chargino couplings to the Higgses            CHANEUC4
C                                                                       CHANEUC5
C  Input:    /PARAM/ MSSM parameters                                    CHANEUC6
C                                                                       CHANEUC7
C  Output:   /PARAM/ aa1,aa2,aa3, the couplings to the neutral          CHANEUC8
C                                 Higgs bosons                          CHANEUC9
C                                                                       CHANEU10
C  P. Janot -- 4 December 1994                                          CHANEU11
C------------------------------------------------------------------     CHANEU12
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
C                                                                       CHANEU14
C Couplings F_ijh of the neutral Higgs boson "h" to a neutralino        CHANEU15
C pair chi_i,chi_j ("h" = generic name for H,h,A)                       CHANEU16
C                                                                       CHANEU17
      DO 1 i=1,4                                                        CHANEU18
        DO 2 j=1,4                                                      CHANEU19
          aa(1,i,j) = ( qqmat(i,j)*ca-ssmat(i,j)*sa)/2.                 CHANEU20
          aa(2,i,j) = ( qqmat(i,j)*sa+ssmat(i,j)*ca)/2.                 CHANEU21
          aa(3,i,j) = (-qqmat(i,j)*sb+ssmat(i,j)*cb)/2.                 CHANEU22
    2   CONTINUE                                                        CHANEU23
    1 CONTINUE                                                          CHANEU24
C                                                                       CHANEU25
C Couplings F_ijh of the neutral Higgs boson "h" to a chargino          CHANEU26
C pair chi+_i,chi-_j ("h" = generic name for H,h,A)                     CHANEU27
C                                                                       CHANEU28
      DO 3 i=1,2                                                        CHANEU29
        DO 4 j=1,2                                                      CHANEU30
          bb(1,i,j) = ( ca*vmat(i,1)*umat(j,2)                          CHANEU31
     .                 +sa*vmat(i,2)*umat(j,1))/SQRT(2.)                CHANEU32
          bb(2,i,j) = (-sa*vmat(i,1)*umat(j,2)                          CHANEU33
     .                 +ca*vmat(i,2)*umat(j,1))/SQRT(2.)                CHANEU34
          bb(3,i,j) = (-sb*vmat(i,1)*umat(j,2)                          CHANEU35
     .                 -cb*vmat(i,2)*umat(j,1))/SQRT(2.)                CHANEU36
    4   CONTINUE                                                        CHANEU37
    3 CONTINUE                                                          CHANEU38
C                                                                       CHANEU39
      RETURN                                                            CHANEU40
      END                                                               CHANEU41
      SUBROUTINE chargi                                                 CHARGI 2
C------------------------------------------------------------------     CHARGI 3
C!  Compute the chargino masses from the MSSM parameters                CHARGI 4
C                                                                       CHARGI 5
C  Input:    /PARAM/ MSSM parameters                                    CHARGI 6
C                                                                       CHARGI 7
C  Output:   /PARAM/ amchar(2) the chargino masses                      CHARGI 8
C                                                                       CHARGI 9
C                                                                       CHARGI10
C  P. Janot -- 4 December 1994                                          CHARGI11
C------------------------------------------------------------------     CHARGI12
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)        DATAPI 2
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          DATAPI 3
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         DATAPI 4
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         DATAPI 5
C                                                                       DATAPI 6
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),               CONSTS 2
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw      CONSTS 3
C                                                                       CONSTS 4
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,      MISCL  2
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),            MISCL  3
     &                 nevent(11)                                       MISCL  4
      INTEGER loutbe,idb1,idb2,nevent                                   MISCL  5
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad                      MISCL  6
C                                                                       MISCL  7
      DIMENSION u(2,2),v(2,2)                                           CHARGI17
C                                                                       CHARGI18
C The chargino mass matrix                                              CHARGI19
C                                                                       CHARGI20
      a11 = susM2                                                       CHARGI21
      a12 = SQRT(2.)*amw*sb                                             CHARGI22
      a21 = SQRT(2.)*amw*cb                                             CHARGI23
      a22 = susMU                                                       CHARGI24
C                                                                       CHARGI25
C The angle of the two diagonalization matrices                         CHARGI26
C                                                                       CHARGI27
      r1   = 2.*(a22*a12+a11*a21)                                       CHARGI28
      t1   = a22**2-a11**2+a21**2-a12**2                                CHARGI29
      phi1 = ATAN2(r1,t1)/2.                                            CHARGI30
      c1   = COS(phi1)                                                  CHARGI31
      s1   = SIN(phi1)                                                  CHARGI32
C                                                                       CHARGI33
      r2   = 2.*(a22*a21+a11*a12)                                       CHARGI34
      t2   = a22**2-a11**2+a12**2-a21**2                                CHARGI35
      phi2 = ATAN2(r2,t2)/2.                                            CHARGI36
      c2   = COS(phi2)                                                  CHARGI37
      s2   = SIN(phi2)                                                  CHARGI38
C                                                                       CHARGI39
C The eigenvalues (=the chargino masses)                                CHARGI40
C                                                                       CHARGI41
      amchar(1) =  a11*c1*c2+a22*s1*s2-a12*c1*s2-a21*s1*c2              CHARGI42
      amchar(2) =  a11*s1*s2+a22*c1*c2+a12*s1*c2+a21*c1*s2              CHARGI43
C                                                                       CHARGI44
C Check for mass positivity and mass ordering                           CHARGI45
C                                                                       CHARGI46
      eps1 = SIGN(1.,amchar(1))                                         CHARGI47
      eps2 =-SIGN(1.,amchar(2))                                         CHARGI48
C                                                                       CHARGI49
C The diagonalization matrices U and V                                  CHARGI50
C                                                                       CHARGI51
      IF ( ABS(amchar(2)) .LE. ABS(amchar(1)) ) THEN                    CHARGI52
        u(1,1) = -s1                                                    CHARGI53
        u(1,2) = -c1                                                    CHARGI54
        u(2,1) =  c1                                                    CHARGI55
        u(2,2) = -s1                                                    CHARGI56
        v(1,1) =  s2*eps2                                               CHARGI57
        v(1,2) =  c2*eps2                                               CHARGI58
        v(2,1) =  c2*eps1                                               CHARGI59
        v(2,2) = -s2*eps1                                               CHARGI60
      ELSE                                                              CHARGI61
        u(1,1) = -c1                                                    CHARGI62
        u(1,2) =  s1                                                    CHARGI63
        u(2,1) = -s1                                                    CHARGI64
        u(2,2) = -c1                                                    CHARGI65
        v(1,1) = -c2*eps1                                               CHARGI66
        v(1,2) =  s2*eps1                                               CHARGI67
        v(2,1) =  s2*eps2                                               CHARGI68
        v(2,2) =  c2*eps2                                               CHARGI69
      ENDIF                                                             CHARGI70
      CALL ucopy(u(1,1),umat(1,1),2*2)                                  CHARGI71
      CALL ucopy(v(1,1),vmat(1,1),2*2)                                  CHARGI72
C                                                                       CHARGI73
C The mass after ordering                                               CHARGI74
C                                                                       CHARGI75
      amchar(1) = u(1,1)*a11*v(1,1)                                     CHARGI76
     .          + u(1,1)*a12*v(1,2)                                     CHARGI77
     .          + u(1,2)*a21*v(1,1)                                     CHARGI78
     .          + u(1,2)*a22*v(1,2)                                     CHARGI79
      amchar(2) = u(2,1)*a11*v(2,1)                                     CHARGI80
     .          + u(2,1)*a12*v(2,2)                                     CHARGI81
     .          + u(2,2)*a21*v(2,1)                                     CHARGI82
     .          + u(2,2)*a22*v(2,2)                                     CHARGI83
C                                                                       CHARGI84
      RETURN                                                            CHARGI85
      END                                                               CHARGI86
      FUNCTION chicha(ichip,ichar,isens)                                CHICHA 2
C-------------------------------------------------------------------    CHICHA 3
C! Compute the decay width of chi --> chi+ W* or chi+ --> chi W*        CHICHA 4
C  where chi' is any of the four neutralinos.                           CHICHA 5
C                                                                       CHICHA 6
C  Input:     -- ichip, the neutralino index                            CHICHA 7
C             -- ichar, the chargino index                              CHICHA 8
C             -- isens, = 1 for chi  --> chi+ W*                        CHICHA 9
C                       = 2 for chi+ --> chi  W*                        CHICHA10
C                                                                       CHICHA11
C  Output:    -- chicha, the decay width in GeV                         CHICHA12
C                                                                       CHICHA13
C  V. Bertin, for the CHA001 generator                                  CHICHA14
C                                                                       CHICHA15
C  Modif:  Patrick Janot (31 Aug 1995)                                  CHICHA16
C          Adapt the routine for the HZHAxx generator                   CHICHA17
C-------------------------------------------------------------------    CHICHA18
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),               CONSTS 2
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw      CONSTS 3
C                                                                       CONSTS 4
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)        DATAPI 2
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          DATAPI 3
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         DATAPI 4
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         DATAPI 5
C                                                                       DATAPI 6
      REAL*8 a,g,xx,xip,clr2,cltr, DGMLT1, xlo,xhi,x(1)                 CHICHA22
      COMMON / chachi / a,g,xx,xip,clr2,cltr                            CHICHA23
      EXTERNAL dgmlt1, fsubcha                                          CHICHA24
C                                                                       CHICHA25
C     PRINT *,'ichii :',ichip                                           CHICHA26
      chicha = 0.                                                       CHICHA27
      IF ( isens .EQ. 1 ) THEN                                          CHICHA28
        xip = ABS(amneut(ichip))                                        CHICHA29
        dif = (xip - amchar(ichar))                                     CHICHA30
        xx = amchar(ichar) / amneut(ichip)                              CHICHA31
        IF ( dif .LT. .0005 ) GOTO 999                                  CHICHA32
      ELSEIF ( isens .EQ. 2 ) THEN                                      CHICHA33
        xip = ABS(amchar(ichar))                                        CHICHA34
        dif = (xip - ABS(amneut(ichip)))                                CHICHA35
        xx = amneut(ichip) / amchar(ichar)                              CHICHA36
        IF ( dif .LT. .0005 ) GOTO 999                                  CHICHA37
      ELSE                                                              CHICHA38
        GOTO 999                                                        CHICHA39
      ENDIF                                                             CHICHA40
C                                                                       CHICHA41
      copl = fieldn(2,ichip)*vmat(ichar,1)                              CHICHA42
     .     - fieldn(4,ichip)*vmat(ichar,2)/SQRT(2.)                     CHICHA43
      copr = fieldn(2,ichip)*umat(ichar,1)                              CHICHA44
     .     + fieldn(3,ichip)*umat(ichar,2)/SQRT(2.)                     CHICHA45
      clr2 = copl**2 + copr**2                                          CHICHA46
      cltr = copl * copr                                                CHICHA47
C     PRINT *,'copl copr clr2 cltr :',copl,copr,clr2,cltr               CHICHA48
C                                                                       CHICHA49
      a = amw                                                           CHICHA50
      g = gmw                                                           CHICHA51
      xlo    = -DATAN2(a/xip,g/xip)                                     CHICHA52
      xhi    =  DATAN2((1D0-DABS(xx))**2-(a/xip)**2,a*g/xip**2)         CHICHA53
      partot = DGMLT1(fsubcha,xlo,xhi,1,6,x)                            CHICHA54
     .       / ( a * g / xip**2 )                                       CHICHA55
C                                                                       CHICHA56
      brel = brelec(dif)                                                CHICHA57
C                                                                       CHICHA58
      chicha = alpha(0)**2 * xip * partot / brel                        CHICHA59
     .       / (48.*pi*sw2**2)                                          CHICHA60
C     PRINT *,'gamcha partot brel :',gamcha,partot,brel                 CHICHA61
      RETURN                                                            CHICHA62
C                                                                       CHICHA63
 999  CONTINUE                                                          CHICHA64
      RETURN                                                            CHICHA65
      END                                                               CHICHA66
      SUBROUTINE chidec                                                 CHIDEC 2
C------------------------------------------------------------------     CHIDEC 3
C! Compute the decay width and branching ratios of the neutralinos      CHIDEC 4
C  into Z* chi, W*-/+ chi+/- and chi gamma, and of the charginos        CHIDEC 5
C  yet into W* chi only ( Z* chi+ to come)                              CHIDEC 6
C                                                                       CHIDEC 7
C  Patrick Janot -- 31 August 1995                                      CHIDEC 8
C------------------------------------------------------------------     CHIDEC 9
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      CHARACTER*14 channel                                              HHDECK 2
      CHARACTER*21 channeut, chanchar                                   HHDECK 3
      PARAMETER(nchneut=8,nchchar=5)                                    HHDECK 4
      COMMON / hhdeck / branch(nchan,nhig),width(nhig),                 HHDECK 5
     .                  parwth(nhig),xymas(2,nchan,nhig),               HHDECK 6
     .                  xywid(2,nchan,nhig)                             HHDECK 7
      COMMON / chaneu / ichn(2),                                        HHDECK 8
     .                  wneut(4,4,nhig), wchar(2,2,nhig),               HHDECK 9
     .                  widneut(4), brneut(nchneut,4),                  HHDECK10
     .                  widchar(2), brchar(nchchar,2)                   HHDECK11
      COMMON / chanch / channeut(nchneut,4), chanchar(nchchar,2)        HHDECK12
      COMMON / vect4 / pvect4(5,2)                                      HHDECK13
      COMMON / hinput / ichan(nchan,nhig), channel(nchan,nhig)          HHDECK14
      COMMON / hisbr / bchpp,bchgg                                      HHDECK15
      DIMENSION ph(4)                                                   HHDECK16
C                                                                       HHDECK17
C                                                                       CHIDEC12
      CALL vzero(brneut(1,1),4*nchneut)                                 CHIDEC13
      CALL vzero(brchar(1,1),2*nchchar)                                 CHIDEC14
      CALL vzero(widneut(1),4)                                          CHIDEC15
      CALL vzero(widchar(1),2)                                          CHIDEC16
C                                                                       CHIDEC17
      DO ineut = 1, 4                                                   CHIDEC18
C                                                                       CHIDEC19
C chi' --> chi gamma                                                    CHIDEC20
C                                                                       CHIDEC21
        DO jneut = 1, ineut-1                                           CHIDEC22
          brneut(jneut,ineut) = chipho(ineut,jneut)                     CHIDEC23
        ENDDO                                                           CHIDEC24
C                                                                       CHIDEC25
C chi' --> chi Z*                                                       CHIDEC26
C                                                                       CHIDEC27
        DO jneut = 1, ineut-1                                           CHIDEC28
          brneut(3+jneut,ineut) = chizst(ineut,jneut)                   CHIDEC29
        ENDDO                                                           CHIDEC30
C                                                                       CHIDEC31
C chi' --> chi+/- W*-/+                                                 CHIDEC32
C chi+ --> chi'   W*-/+                                                 CHIDEC33
C                                                                       CHIDEC34
        DO jchar = 1, 2                                                 CHIDEC35
          IF ( ABS(amneut(ineut)) .GT. ABS(amchar(jchar)) ) THEN        CHIDEC36
            brneut(6+jchar,ineut) = chicha(ineut,jchar,1)               CHIDEC37
          ELSE                                                          CHIDEC38
            brchar(ineut,jchar) = chicha(ineut,jchar,2)                 CHIDEC39
          ENDIF                                                         CHIDEC40
        ENDDO                                                           CHIDEC41
C                                                                       CHIDEC42
      ENDDO                                                             CHIDEC43
C                                                                       CHIDEC44
C chi+(2) --> chi+(1) Z*-/+  ! Not implemented yet                      CHIDEC45
C                                                                       CHIDEC46
CCCC  brchar(5,2) = chazst(1.)                                          CHIDEC47
C                                                                       CHIDEC48
C Total width for the neutralinos                                       CHIDEC49
C                                                                       CHIDEC50
      DO ineut = 1, 4                                                   CHIDEC51
        widneut(ineut) = 0.                                             CHIDEC52
        DO ic = 1 , nchneut                                             CHIDEC53
          widneut(ineut) = widneut(ineut) + brneut(ic,ineut)            CHIDEC54
        ENDDO                                                           CHIDEC55
C                                                                       CHIDEC56
C And branching ratios                                                  CHIDEC57
C                                                                       CHIDEC58
        IF ( widneut(ineut) .NE. 0. ) THEN                              CHIDEC59
          DO ic = 1 , nchneut                                           CHIDEC60
            brneut(ic,ineut) = brneut(ic,ineut)/widneut(ineut)          CHIDEC61
          ENDDO                                                         CHIDEC62
        ENDIF                                                           CHIDEC63
C                                                                       CHIDEC64
      ENDDO                                                             CHIDEC65
C                                                                       CHIDEC66
C Total width for the charginos                                         CHIDEC67
C                                                                       CHIDEC68
      DO ichar = 1, 2                                                   CHIDEC69
        widchar(ichar) = 0.                                             CHIDEC70
        DO ic = 1 , nchchar                                             CHIDEC71
          widchar(ichar) = widchar(ichar) + brchar(ic,ichar)            CHIDEC72
        ENDDO                                                           CHIDEC73
C                                                                       CHIDEC74
C And branching ratios                                                  CHIDEC75
C                                                                       CHIDEC76
        IF ( widchar(ichar) .NE. 0. ) THEN                              CHIDEC77
          DO ic = 1 , nchchar                                           CHIDEC78
            brchar(ic,ichar) = brchar(ic,ichar)/widchar(ichar)          CHIDEC79
          ENDDO                                                         CHIDEC80
        ENDIF                                                           CHIDEC81
C                                                                       CHIDEC82
      ENDDO                                                             CHIDEC83
C                                                                       CHIDEC84
C Here we go !                                                          CHIDEC85
C                                                                       CHIDEC86
      channeut(7,1) = 'chi10 --> chi1+/- W  '                           CHIDEC87
      channeut(8,1) = 'chi10 --> chi2+/- W  '                           CHIDEC88
      channeut(1,2) = 'chi20 --> chi10 gamma'                           CHIDEC89
      channeut(4,2) = 'chi20 --> chi10   Z  '                           CHIDEC90
      channeut(7,2) = 'chi20 --> chi1+/- W  '                           CHIDEC91
      channeut(8,2) = 'chi20 --> chi2+/- W  '                           CHIDEC92
      channeut(1,3) = 'chi30 --> chi10 gamma'                           CHIDEC93
      channeut(2,3) = 'chi30 --> chi20 gamma'                           CHIDEC94
      channeut(4,3) = 'chi30 --> chi10   Z  '                           CHIDEC95
      channeut(5,3) = 'chi30 --> chi20   Z  '                           CHIDEC96
      channeut(7,3) = 'chi30 --> chi1+/- W  '                           CHIDEC97
      channeut(8,3) = 'chi30 --> chi2+/- W  '                           CHIDEC98
      channeut(1,4) = 'chi40 --> chi10 gamma'                           CHIDEC99
      channeut(2,4) = 'chi40 --> chi20 gamma'                           CHIDE100
      channeut(3,4) = 'chi40 --> chi30 gamma'                           CHIDE101
      channeut(4,4) = 'chi40 --> chi10   Z  '                           CHIDE102
      channeut(5,4) = 'chi40 --> chi20   Z  '                           CHIDE103
      channeut(6,4) = 'chi40 --> chi30   Z  '                           CHIDE104
      channeut(7,4) = 'chi40 --> chi1+/- W  '                           CHIDE105
      channeut(8,4) = 'chi40 --> chi2+/- W  '                           CHIDE106
C                                                                       CHIDE107
      chanchar(1,1) = 'chi1+ --> chi10   W  '                           CHIDE108
      chanchar(2,1) = 'chi1+ --> chi20   W  '                           CHIDE109
      chanchar(3,1) = 'chi1+ --> chi30   W  '                           CHIDE110
      chanchar(4,1) = 'chi1+ --> chi40   W  '                           CHIDE111
      chanchar(1,2) = 'chi2+ --> chi10   W  '                           CHIDE112
      chanchar(2,2) = 'chi2+ --> chi20   W  '                           CHIDE113
      chanchar(3,2) = 'chi2+ --> chi30   W  '                           CHIDE114
      chanchar(4,2) = 'chi2+ --> chi40   W  '                           CHIDE115
      chanchar(5,2) = 'chi2+ --> chi1+   Z  '                           CHIDE116
C                                                                       CHIDE117
      RETURN                                                            CHIDE118
      END                                                               CHIDE119
      SUBROUTINE chideca(p1,ichi1,ichi,ichi2,jchi,ifn)                  CHIDECA2
C------------------------------------------------------------------     CHIDECA3
C!  Derive the quadrimomenta of the neutral/charg-ino decay             CHIDECA4
C   particles                                                           CHIDECA5
C                                                                       CHIDECA6
C   Input:   -- p1(4),  the quadri-momentum  of the initial             CHIDECA7
C                       charg/neutral-ino                               CHIDECA8
C            -- ichi1,  its index                                       CHIDECA9
C            -- ichi,   its electric charge (absolute value)            CHIDEC10
C            -- ichi2,  the index of the final charg/neutral-ino        CHIDEC11
C            -- jchi,   its electric charge                             CHIDEC12
C            -- ifn,    the final state boson index                     CHIDEC13
C                        = 0 for a photon                               CHIDEC14
C                        = 1 for a Z                                    CHIDEC15
C                        = 2 for a W                                    CHIDEC16
C                                                                       CHIDEC17
C   Output:  -- pvect4,  the quadri-momenta of the final state          CHIDEC18
C                        particles, stored in /VECT4/                   CHIDEC19
C                                                                       CHIDEC20
C  P. Janot --  31 Aug 1995                                             CHIDEC21
C------------------------------------------------------------------     CHIDEC22
      DIMENSION p1(4),p2(4),p3(4)                                       CHIDEC23
      REAL*8 betax,betay,betaz                                          CHIDEC24
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      CHARACTER*14 channel                                              HHDECK 2
      CHARACTER*21 channeut, chanchar                                   HHDECK 3
      PARAMETER(nchneut=8,nchchar=5)                                    HHDECK 4
      COMMON / hhdeck / branch(nchan,nhig),width(nhig),                 HHDECK 5
     .                  parwth(nhig),xymas(2,nchan,nhig),               HHDECK 6
     .                  xywid(2,nchan,nhig)                             HHDECK 7
      COMMON / chaneu / ichn(2),                                        HHDECK 8
     .                  wneut(4,4,nhig), wchar(2,2,nhig),               HHDECK 9
     .                  widneut(4), brneut(nchneut,4),                  HHDECK10
     .                  widchar(2), brchar(nchchar,2)                   HHDECK11
      COMMON / chanch / channeut(nchneut,4), chanchar(nchchar,2)        HHDECK12
      COMMON / vect4 / pvect4(5,2)                                      HHDECK13
      COMMON / hinput / ichan(nchan,nhig), channel(nchan,nhig)          HHDECK14
      COMMON / hisbr / bchpp,bchgg                                      HHDECK15
      DIMENSION ph(4)                                                   HHDECK16
C                                                                       HHDECK17
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)        DATAPI 2
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          DATAPI 3
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         DATAPI 4
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         DATAPI 5
C                                                                       DATAPI 6
C                                                                       CHIDEC28
      CALL vzero(pvect4(1,1),10)                                        CHIDEC29
C                                                                       CHIDEC30
C Masses of the two charg/neutral-inos                                  CHIDEC31
C                                                                       CHIDEC32
      IF ( ichi .EQ. 0 ) THEN                                           CHIDEC33
        chi1mas = ABS(amneut(ichi1))                                    CHIDEC34
      ELSE                                                              CHIDEC35
        chi1mas = ABS(amchar(ichi1))                                    CHIDEC36
      ENDIF                                                             CHIDEC37
C                                                                       CHIDEC38
      IF ( jchi .EQ. 0 ) THEN                                           CHIDEC39
        chi2mas = ABS(amneut(ichi2))                                    CHIDEC40
      ELSE                                                              CHIDEC41
        chi2mas = ABS(amchar(ichi2))                                    CHIDEC42
      ENDIF                                                             CHIDEC43
      IF ( idbg .GE. 10 ) THEN                                          CHIDEC44
        WRITE(6,*) ' +++ CHIDECA +++ '                                  CHIDEC45
        WRITE(6,*) 'am1/am2 : ',chi1mas,chi2mas                         CHIDEC46
      ENDIF                                                             CHIDEC47
C                                                                       CHIDEC48
C Generate the mass of the final boson according to a Breit-Wigner      CHIDEC49
C                                                                       CHIDEC50
    1 CONTINUE                                                          CHIDEC51
      IF ( ifn .EQ. 0 ) THEN                                            CHIDEC52
        zstarm = 0.                                                     CHIDEC53
      ELSE                                                              CHIDEC54
        zstarmx = chi1mas-chi2mas                                       CHIDEC55
        zstarmn = 0.                                                    CHIDEC56
        IF ( ifn .EQ. 1 ) THEN                                          CHIDEC57
          amm = amz                                                     CHIDEC58
          gmm = gmz                                                     CHIDEC59
        ELSE                                                            CHIDEC60
          amm = amw                                                     CHIDEC61
          gmm = gmw                                                     CHIDEC62
        ENDIF                                                           CHIDEC63
        CALL bwgene(zstarmn,zstarmx,amm,gmm,zstarm,djdum)               CHIDEC64
        IF ( zstarm .GE. zstarmx .OR. zstarm .LE. zstarmn ) GOTO 1      CHIDEC65
      ENDIF                                                             CHIDEC66
      IF ( idbg .GE. 10 ) THEN                                          CHIDEC67
        WRITE(6,*) ' +++ CHIDECA +++ '                                  CHIDEC68
        WRITE(6,*) 'zstarm : ',zstarm                                   CHIDEC69
      ENDIF                                                             CHIDEC70
C                                                                       CHIDEC71
C Compute the phase space factor                                        CHIDEC72
C                                                                       CHIDEC73
      phspace = SQRT( (chi1mas**2-(chi2mas+zstarm)**2)                  CHIDEC74
     .               *(chi1mas**2-(chi2mas-zstarm)**2) )                CHIDEC75
     .        / (2. * chi1mas )                                         CHIDEC76
      phspmax = (chi1mas**2 - chi2mas**2)                               CHIDEC77
     .        / (2. * chi1mas )                                         CHIDEC78
C                                                                       CHIDEC79
C Select according phase space                                          CHIDEC80
C                                                                       CHIDEC81
      IF ( phspace .LT. phspmax * RNDM(zstarm) ) GOTO 1                 CHIDEC82
C                                                                       CHIDEC83
C The boson quadri-momentum (assuming a uniform decay)                  CHIDEC84
C                                                                       CHIDEC85
      phistr = 2.*pi*RNDM(zstarm)                                       CHIDEC86
      costar = RNDM(zstarm)                                             CHIDEC87
      sintar = SQRT(1.-costar**2)                                       CHIDEC88
      p3(1) = phspace * sintar * COS(phistr)                            CHIDEC89
      p3(2) = phspace * sintar * SIN(phistr)                            CHIDEC90
      p3(3) = phspace * costar                                          CHIDEC91
      p3(4) = SQRT(phspace**2+zstarm**2)                                CHIDEC92
C                                                                       CHIDEC93
C The charg/neutral-ino quadri-momentum                                 CHIDEC94
C                                                                       CHIDEC95
      p2(1) = -p3(1)                                                    CHIDEC96
      p2(2) = -p3(2)                                                    CHIDEC97
      p2(3) = -p3(3)                                                    CHIDEC98
      p2(4) = SQRT(phspace**2+chi2mas**2)                               CHIDEC99
      IF ( idbg .GE. 10 ) THEN                                          CHIDE100
        WRITE(6,*) ' +++ CHIDECA +++ Avant boost '                      CHIDE101
        WRITE(6,*) ' p2    : ',p2                                       CHIDE102
        WRITE(6,*) ' p3    : ',p3                                       CHIDE103
        WRITE(6,*) ' p2+p3 : ',(p2(i)+p3(i),i=1,4)                      CHIDE104
      ENDIF                                                             CHIDE105
C                                                                       CHIDE106
C Boost in the LAB frame                                                CHIDE107
C                                                                       CHIDE108
      betax = -p1(1)/p1(4)                                              CHIDE109
      betay = -p1(2)/p1(4)                                              CHIDE110
      betaz = -p1(3)/p1(4)                                              CHIDE111
      CALL lorenz(betax,betay,betaz,p2)                                 CHIDE112
      CALL lorenz(betax,betay,betaz,p3)                                 CHIDE113
      CALL ucopy(p2(1),pvect4(1,1),4)                                   CHIDE114
      CALL ucopy(p3(1),pvect4(1,2),4)                                   CHIDE115
      pvect4(5,1) = chi2mas                                             CHIDE116
      pvect4(5,2) = zstarm                                              CHIDE117
C                                                                       CHIDE118
      IF ( idbg .GE. 10 ) THEN                                          CHIDE119
        WRITE(6,*) ' +++ CHIDECA +++ Apres boost'                       CHIDE120
        WRITE(6,*) ' p2    : ',p2                                       CHIDE121
        WRITE(6,*) ' p3    : ',p3                                       CHIDE122
        WRITE(6,*) ' p2+p3 : ',(p2(i)+p3(i),i=1,4)                      CHIDE123
        WRITE(6,*) ' p1    : ',p1                                       CHIDE124
      ENDIF                                                             CHIDE125
  999 RETURN                                                            CHIDE126
      END                                                               CHIDE127
      FUNCTION chipho(ichip, ichi)                                      CHIPHO 2
C-------------------------------------------------------------------    CHIPHO 3
C! Compute the decay width of chi' --> chi gamma                        CHIPHO 4
C  where chi' and chi are any two of the four neutralinos.              CHIPHO 5
C                                                                       CHIPHO 6
C  Input:     -- ichip, the first neutralino index                      CHIPHO 7
C             -- ichi,  the second neutralino index                     CHIPHO 8
C                                                                       CHIPHO 9
C  Output:    -- chipho, the decay width in GeV                         CHIPHO10
C                                                                       CHIPHO11
C  V. Bertin, for the CHA001 generator                                  CHIPHO12
C                                                                       CHIPHO13
C  Modif:  Patrick Janot (31 Aug 1995)                                  CHIPHO14
C          Adapt the routine for the HZHAxx generator                   CHIPHO15
C          In particular, the sfermion and Higgs masses have            CHIPHO16
C          been computed with the MSSM relations instead of             CHIPHO17
C          being entered with DATA statements                           CHIPHO18
C-------------------------------------------------------------------    CHIPHO19
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),               CONSTS 2
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw      CONSTS 3
C                                                                       CONSTS 4
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)        DATAPI 2
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          DATAPI 3
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         DATAPI 4
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         DATAPI 5
C                                                                       DATAPI 6
      REAL*8 fi,fj,fk,fi2                                               CHIPHO23
      EXTERNAL fi,fj,fk,fi2                                             CHIPHO24
      DIMENSION amsf(8),amf(8),ch(8),color(8)                           CHIPHO25
      DIMENSION fielnp(4),fielnm(4)                                     CHIPHO26
      DATA color/3.,3.,3.,3.,3.,1.,1.,1./                               CHIPHO27
C                                                                       CHIPHO28
      chigam = 0.                                                       CHIPHO29
c     PRINT *,'ichii ichio :',ichip,ichi                                CHIPHO30
C                                                                       CHIPHO31
C-- Masses & CP & charges                                               CHIPHO32
C                                                                       CHIPHO33
      xxip = amneut(ichip)                                              CHIPHO34
      xip  = ABS(xxip)                                                  CHIPHO35
      xxi  = amneut(ichi)                                               CHIPHO36
      xi   = ABS(xxi)                                                   CHIPHO37
      amhg  = amw                                                       CHIPHO38
      DO i = 1, 6                                                       CHIPHO39
        amsf(i) = susSMQ                                                CHIPHO40
      ENDDO                                                             CHIPHO41
      amsf(7) = amsb(1)                                                 CHIPHO42
      amsf(8) = amsb(2)                                                 CHIPHO43
      amf(1) = amd                                                      CHIPHO44
      amf(2) = amu                                                      CHIPHO45
      amf(3) = ams                                                      CHIPHO46
      amf(4) = amc                                                      CHIPHO47
      amf(5) = amb                                                      CHIPHO48
      amf(6) = ame                                                      CHIPHO49
      amf(7) = ammu                                                     CHIPHO50
      amf(8) = amtau                                                    CHIPHO51
      ch(1)  =-1./3.                                                    CHIPHO52
      ch(3)  =-1./3.                                                    CHIPHO53
      ch(5)  =-1./3.                                                    CHIPHO54
      ch(2)  = 2./3.                                                    CHIPHO55
      ch(4)  = 2./3.                                                    CHIPHO56
      ch(6)  =-1.                                                       CHIPHO57
      ch(7)  =-1.                                                       CHIPHO58
      ch(8)  =-1.                                                       CHIPHO59
C                                                                       CHIPHO60
      IF ( xi .GE. xip ) GOTO 999                                       CHIPHO61
C                                                                       CHIPHO62
C-- Relevant fields combinaisons                                        CHIPHO63
C                                                                       CHIPHO64
      tw = sw/cw                                                        CHIPHO65
      fielnp(ichip) = fieldn(2,ichip) + tw * fieldn(1,ichip)            CHIPHO66
      fielnm(ichip) = fieldn(2,ichip) - tw * fieldn(1,ichip)            CHIPHO67
      fielnp(ichi ) = fieldn(2,ichi)  + tw * fieldn(1,ichi)             CHIPHO68
      fielnm(ichi ) = fieldn(2,ichi)  - tw * fieldn(1,ichi)             CHIPHO69
C                                                                       CHIPHO70
C-- Initialisation                                                      CHIPHO71
C                                                                       CHIPHO72
      copw = 0.                                                         CHIPHO73
      copf = 0.                                                         CHIPHO74
      copg = 0.                                                         CHIPHO75
      coph = 0.                                                         CHIPHO76
      chigam = 0.                                                       CHIPHO77
C                                                                       CHIPHO78
      DO 1 icharg = 1,2                                                 CHIPHO79
C      IF ( xip .GE. amw + amchar(icharg) ) GOTO 901                    CHIPHO80
C--- W-Chargino loop                                                    CHIPHO81
       aw = fieldn(2,ichi)*fieldn(2,ichip)*                             CHIPHO82
     .      (vmat(icharg,1)**2 - umat(icharg,1)**2)                     CHIPHO83
     .    + .5 * (fieldn(4,ichi)*fieldn(4,ichip)*vmat(icharg,2)**2      CHIPHO84
     .         -  fieldn(3,ichi)*fieldn(3,ichip)*umat(icharg,2)**2)     CHIPHO85
     .    - (vmat(icharg,1) * vmat(icharg,2) *                          CHIPHO86
     .       (fieldn(2,ichi) * fieldn(4,ichip) +                        CHIPHO87
     .        fieldn(4,ichi) * fieldn(2,ichip))                         CHIPHO88
     .     + umat(icharg,1) * umat(icharg,2) *                          CHIPHO89
     .       (fieldn(2,ichi) * fieldn(3,ichip) +                        CHIPHO90
     .        fieldn(3,ichi) * fieldn(2,ichip))) / SQRT(2.)             CHIPHO91
       bw = .5 * umat(icharg,2) * vmat(icharg,2) *                      CHIPHO92
     .            (fieldn(3,ichi) * fieldn(4,ichip) -                   CHIPHO93
     .             fieldn(4,ichi) * fieldn(3,ichip))                    CHIPHO94
     .     + (umat(icharg,1) * vmat(icharg,2) *                         CHIPHO95
     .        (fieldn(2,ichi) * fieldn(4,ichip) -                       CHIPHO96
     .         fieldn(4,ichi) * fieldn(2,ichip))                        CHIPHO97
     .      + umat(icharg,2) * vmat(icharg,1) *                         CHIPHO98
     .        (fieldn(2,ichi) * fieldn(3,ichip) -                       CHIPHO99
     .         fieldn(3,ichi) * fieldn(2,ichip))) / SQRT(2.)            CHIPH100
       copw = copw                                                      CHIPH101
     .      + aw * (xxip                                                CHIPH102
     .           * (FI2(amw/amz,amchar(icharg)/amz,xip/amz,xi/amz)      CHIPH103
     .           -  FJ( amw/amz,amchar(icharg)/amz,xip/amz,xi/amz)      CHIPH104
     .           -  FK( amw/amz,amchar(icharg)/amz,xip/amz,xi/amz))     CHIPH105
     .           +  xxi                                                 CHIPH106
     .           * (FJ( amw/amz,amchar(icharg)/amz,xip/amz,xi/amz)      CHIPH107
     .           -  FK( amw/amz,amchar(icharg)/amz,xip/amz,xi/amz)))    CHIPH108
     .           +  2. * bw * amchar(icharg)                            CHIPH109
     .           *  FJ( amw/amz,amchar(icharg)/amz,xip/amz,xi/amz)      CHIPH110
C     PRINT *,'W loop : ich aw bw copw :',icharg,aw,bw,copw             CHIPH111
C                                                                       CHIPH112
C--- H-Chargino loop                                                    CHIPH113
C      IF ( xip .GE. amhp + amchar(icharg) ) GOTO 902                   CHIPH114
       ah = s2b                                                         CHIPH115
     .     * (umat(icharg,1) * vmat(icharg,1)                           CHIPH116
     .      * (fieldn(4,ichi) * fieldn(3,ichip)                         CHIPH117
     .       - fieldn(3,ichi) * fieldn(4,ichip))                        CHIPH118
     .     - (fielnp(ichip)  *                                          CHIPH119
     .        (fieldn(3,ichi) * umat(icharg,1) * vmat(icharg,2)         CHIPH120
     .       + fieldn(4,ichi) * umat(icharg,2) * vmat(icharg,1))        CHIPH121
     .      - fielnp(ichi) *                                            CHIPH122
     .        (fieldn(3,ichip) * umat(icharg,1) * vmat(icharg,2)        CHIPH123
     .       + fieldn(4,ichip) * umat(icharg,2) * vmat(icharg,1)))      CHIPH124
     .      / SQRT(2.))                                                 CHIPH125
       bh = 2.* cb2 * (fieldn(4,ichi) * vmat(icharg,1)                  CHIPH126
     .               + fielnp(ichi) * vmat(icharg,2) / SQRT(2.))        CHIPH127
     .              * (fieldn(4,ichip) * vmat(icharg,1)                 CHIPH128
     .               + fielnp(ichip) * vmat(icharg,2) / SQRT(2.))       CHIPH129
     .    - 2.* sb2 * (fieldn(3,ichi) * umat(icharg,1)                  CHIPH130
     .               - fielnp(ichi) * umat(icharg,2) / SQRT(2.))        CHIPH131
     .              * (fieldn(3,ichip) * umat(icharg,1)                 CHIPH132
     .               - fielnp(ichip) * umat(icharg,2) / SQRT(2.))       CHIPH133
       coph = coph +                                                    CHIPH134
     .        bh * ( xxip                                               CHIPH135
     .           * (FI2(amhp/amz,amchar(icharg)/amz,xip/amz,xi/amz)     CHIPH136
     .           -  FK( amhp/amz,amchar(icharg)/amz,xip/amz,xi/amz))    CHIPH137
     .           -   xxi                                                CHIPH138
     .           *  FK( amhp/amz,amchar(icharg)/amz,xip/amz,xi/amz))    CHIPH139
     .           + amchar(icharg) * ah                                  CHIPH140
     .           *  FI( amhp/amz,amchar(icharg)/amz,xip/amz,xi/amz)     CHIPH141
C     PRINT *,'H loop : ich ah bh coph :',icharg,ah,bh,coph             CHIPH142
C                                                                       CHIPH143
C--- G-Chargino loop                                                    CHIPH144
       ag = -s2b                                                        CHIPH145
     .     * (umat(icharg,1) * vmat(icharg,1)                           CHIPH146
     .      * (fieldn(4,ichi) * fieldn(3,ichip)                         CHIPH147
     .       - fieldn(3,ichi) * fieldn(4,ichip))                        CHIPH148
     .     - (fielnp(ichip)  *                                          CHIPH149
     .        (fieldn(3,ichi) * umat(icharg,1) * vmat(icharg,2)         CHIPH150
     .       + fieldn(4,ichi) * umat(icharg,2) * vmat(icharg,1))        CHIPH151
     .      - fielnp(ichi) *                                            CHIPH152
     .        (fieldn(3,ichip) * umat(icharg,1) * vmat(icharg,2)        CHIPH153
     .       + fieldn(4,ichip) * umat(icharg,2) * vmat(icharg,1)))      CHIPH154
     .      / SQRT(2.))                                                 CHIPH155
       bg = 2.* sb2 * (fieldn(4,ichi) * vmat(icharg,1)                  CHIPH156
     .               + fielnp(ichi) * vmat(icharg,2) / SQRT(2.))        CHIPH157
     .              * (fieldn(4,ichip) * vmat(icharg,1)                 CHIPH158
     .               + fielnp(ichip) * vmat(icharg,2) / SQRT(2.))       CHIPH159
     .    - 2.* cb2 * (fieldn(3,ichi) * umat(icharg,1)                  CHIPH160
     .               - fielnp(ichi) * umat(icharg,2) / SQRT(2.))        CHIPH161
     .              * (fieldn(3,ichip) * umat(icharg,1)                 CHIPH162
     .               - fielnp(ichip) * umat(icharg,2) / SQRT(2.))       CHIPH163
       copg = copg +                                                    CHIPH164
     .        bg * ( xxip                                               CHIPH165
     .           * (FI2(amhg/amz,amchar(icharg)/amz,xip/amz,xi/amz)     CHIPH166
     .           -  FK( amhg/amz,amchar(icharg)/amz,xip/amz,xi/amz))    CHIPH167
     .           -   xxi                                                CHIPH168
     .           *  FK( amhg/amz,amchar(icharg)/amz,xip/amz,xi/amz))    CHIPH169
     .           +  amchar(icharg) * ag                                 CHIPH170
     .           *  FI( amhg/amz,amchar(icharg)/amz,xip/amz,xi/amz)     CHIPH171
C     PRINT *,'G loop : ich ag bg copg :',icharg,ag,bg,copg             CHIPH172
 1    CONTINUE                                                          CHIPH173
C                                                                       CHIPH174
C--- Sfermion-Fermion loop                                              CHIPH175
      DO 2 isfer=1,8                                                    CHIPH176
C      IF ( xip .GE. amsf(isfer) + amf(isfer) ) GOTO 903                CHIPH177
       IF ( ch(isfer) .GE. 0. ) THEN                                    CHIPH178
C--- Up fermions except top                                             CHIPH179
        af = -2. * amf(isfer) / (amw * sb)                              CHIPH180
     .     * (fieldn(4,ichi) * (.5 * fielnm(ichip)                      CHIPH181
     .     +  ch(isfer) * tw * fieldn(1,ichip))                         CHIPH182
     .     - fieldn(4,ichip) * (.5 *fielnm(ichi)                        CHIPH183
     .     + ch(isfer) * tw * fieldn(1,ichi))                           CHIPH184
     .     - ch(isfer) * tw                                             CHIPH185
     .     * (fieldn(4,ichip) * fieldn(1,ichi)                          CHIPH186
     .     -  fieldn(1,ichip) * fieldn(4,ichi)))                        CHIPH187
        bf = fielnm(ichip) * fielnm(ichi)                               CHIPH188
     .     + 2.* ch(isfer) * tw                                         CHIPH189
     .     * (fieldn(1,ichi ) * fielnm(ichip)                           CHIPH190
     .     +  fieldn(1,ichip) * fielnm(ichi))                           CHIPH191
       ELSE                                                             CHIPH192
C--- Down fermions                                                      CHIPH193
        af = -2. * amf(isfer) / (amw * cb)                              CHIPH194
     .     * (fieldn(3,ichi) * (-.5 * fielnm(ichip)                     CHIPH195
     .     + ch(isfer) * tw * fieldn(1,ichip))                          CHIPH196
     .     - fieldn(3,ichip) * (-.5 *fielnm(ichi)                       CHIPH197
     .     + ch(isfer) * tw * fieldn(1,ichi))                           CHIPH198
     .     - ch(isfer) * tw                                             CHIPH199
     .     * (fieldn(3,ichip) * fieldn(1,ichi)                          CHIPH200
     .     -  fieldn(1,ichip) * fieldn(3,ichi)))                        CHIPH201
        bf = fielnm(ichip) * fielnm(ichi)                               CHIPH202
     .     - 2.* ch(isfer) * tw                                         CHIPH203
     .     * (fieldn(1,ichi) * fielnm(ichip)                            CHIPH204
     .     + fieldn(1,ichip) * fielnm(ichi))                            CHIPH205
       ENDIF                                                            CHIPH206
       copf = copf                                                      CHIPH207
     .      + ch(isfer) * color(isfer)                                  CHIPH208
     .      * (bf*(xxip                                                 CHIPH209
     .           *(FI2(amsf(isfer)/amz,amf(isfer)/amz,xip/amz,xi/amz)   CHIPH210
     .           - FK( amsf(isfer)/amz,amf(isfer)/amz,xip/amz,xi/amz))  CHIPH211
     .           - xxi                                                  CHIPH212
     .           * FK( amsf(isfer)/amz,amf(isfer)/amz,xip/amz,xi/amz))  CHIPH213
     .        +af* amf(isfer)                                           CHIPH214
     .           * FI( amsf(isfer)/amz,amf(isfer)/amz,xip/amz,xi/amz))  CHIPH215
C     PRINT *,'F loop : isf af bf copf :',isfer,af,bf,copf              CHIPH216
 2    CONTINUE                                                          CHIPH217
C                                                                       CHIPH218
      ctt = COS(topmix)                                                 CHIPH219
      stt = SIN(topmix)                                                 CHIPH220
C                                                                       CHIPH221
C-- 1st scalar top                                                      CHIPH222
C                                                                       CHIPH223
      at1= (ctt * (fielnm(ichi) + 4./3. * tw * fieldn(1,ichi))          CHIPH224
     .    + stt * fieldn(4,ichi) * amt / (amw * sb))                    CHIPH225
     .   * (ctt * fieldn(4,ichip) * amt / (amw * sb)                    CHIPH226
     .    - 4./3. * stt * fieldn(1,ichip) * tw)                         CHIPH227
     .   - (ctt * fieldn(4,ichi) * amt / (amw * sb)                     CHIPH228
     .    - 4./3. * stt * fieldn(1,ichi) * tw)                          CHIPH229
     .   * (ctt * (fielnm(ichip) + 4./3. * tw * fieldn(1,ichip))        CHIPH230
     .    + stt * fieldn(4,ichip) * amt / (amw * sb))                   CHIPH231
      bt1= (ctt * (fielnm(ichi) + 4./3. * tw * fieldn(1,ichi))          CHIPH232
     .    + stt * fieldn(4,ichi) * amt / (amw * sb))                    CHIPH233
     .   * (ctt * (fielnm(ichip) + 4./3. * tw * fieldn(1,ichip))        CHIPH234
     .    + stt * fieldn(4,ichip) * amt / (amw * sb))                   CHIPH235
     .   - (ctt * fieldn(4,ichi) * amt / (amw * sb)                     CHIPH236
     .    - 4./3. * stt * fieldn(1,ichi) * tw)                          CHIPH237
     .   * (ctt * fieldn(4,ichip) * amt / (amw * sb)                    CHIPH238
     .    - 4./3. * stt * fieldn(1,ichip) * tw)                         CHIPH239
      copf = copf                                                       CHIPH240
     .    + 2./3. * 3. *                                                CHIPH241
     .      (bt1 * (xxip                                                CHIPH242
     .           * (FI2(amst(1)/amz,amt/amz,xip/amz,xi/amz)             CHIPH243
     .           -  FK( amst(1)/amz,amt/amz,xip/amz,xi/amz))            CHIPH244
     .           - xxi                                                  CHIPH245
     .           *  FK( amst(1)/amz,amt/amz,xip/amz,xi/amz))            CHIPH246
     .      +at1 * amt                                                  CHIPH247
     .           *  FI( amst(1)/amz,amt/amz,xip/amz,xi/amz))            CHIPH248
c     PRINT *,'T1 loop : at1 bt1 copf :',at1,bt1,copf                   CHIPH249
C--- 2nd scalar top                                                     CHIPH250
      at2= (stt * (fielnm(ichi) + 4./3. * tw * fieldn(1,ichi))          CHIPH251
     .    - ctt * fieldn(4,ichi) * amt / (amw * sb))                    CHIPH252
     .   * (stt * fieldn(4,ichip) * amt / (amw * sb)                    CHIPH253
     .    + 4./3. * ctt * fieldn(1,ichip) * tw)                         CHIPH254
     .   - (stt * fieldn(4,ichi) * amt / (amw * sb)                     CHIPH255
     .    + 4./3. * ctt * fieldn(1,ichi) * tw)                          CHIPH256
     .   * (stt * (fielnm(ichip) + 4./3. * tw * fieldn(1,ichip))        CHIPH257
     .    - ctt * fieldn(4,ichip) * amt / (amw * sb))                   CHIPH258
      bt2= (stt * (fielnm(ichi) + 4./3. * tw * fieldn(1,ichi))          CHIPH259
     .    - ctt * fieldn(4,ichi) * amt / (amw * sb))                    CHIPH260
     .   * (stt * (fielnm(ichip) + 4./3. * tw * fieldn(1,ichip))        CHIPH261
     .    - ctt * fieldn(4,ichip) * amt / (amw * sb))                   CHIPH262
     .   - (stt * fieldn(4,ichi) * amt / (amw * sb)                     CHIPH263
     .    + 4./3. * ctt * fieldn(1,ichi) * tw)                          CHIPH264
     .   * (stt * fieldn(4,ichip) * amt / (amw * sb)                    CHIPH265
     .    + 4./3. * ctt * fieldn(1,ichip) * tw)                         CHIPH266
      copf = copf                                                       CHIPH267
     .     + 2./3. * 3. *                                               CHIPH268
     .      (bt2 * (xxip                                                CHIPH269
     .           * (FI2(amst(2)/amz,amt/amz,xip/amz,xi/amz)             CHIPH270
     .           -  FK( amst(2)/amz,amt/amz,xip/amz,xi/amz))            CHIPH271
     .           - xxi                                                  CHIPH272
     .           *  FK( amst(2)/amz,amt/amz,xip/amz,xi/amz))            CHIPH273
     .     + at2 * amt                                                  CHIPH274
     .           *  FI( amst(2)/amz,amt/amz,xip/amz,xi/amz))            CHIPH275
C     PRINT *,'T2 loop : at2 bt2 copf :',at2,bt2,copf                   CHIPH276
C                                                                       CHIPH277
C-- Total coupling constant                                             CHIPH278
C                                                                       CHIPH279
      copt = copw - (coph + copg +copf)/ 4.                             CHIPH280
C                                                                       CHIPH281
C-- Width chip --> chi gamma                                            CHIPH282
C                                                                       CHIPH283
      chigam = alpha(0)**3 * (copt/amz)**2                              CHIPH284
     .       * ((xip/amz)**2 - (xi/amz)**2)**3                          CHIPH285
     .       / (xip/amz)**3                                             CHIPH286
     .       / (8. * pi**2 * sw2**2)                                    CHIPH287
     .       * amz                                                      CHIPH288
C     PRINT *,'copt chigam :',copt,chigam                               CHIPH289
C                                                                       CHIPH290
      chipho = chigam                                                   CHIPH291
C                                                                       CHIPH292
C     RETURN                                                            CHIPH293
C                                                                       CHIPH294
C900  CONTINUE                                                          CHIPH295
C     WRITE(6,*) 'Impossible decay ',ichip,' into ',ichi                CHIPH296
C     chipho = 0.                                                       CHIPH297
C     RETURN                                                            CHIPH298
C                                                                       CHIPH299
C901  CONTINUE                                                          CHIPH300
C     WRITE(6,*) 'Posssible direct decay into W-Chargino'               CHIPH301
C     chipho = 0.                                                       CHIPH302
C     RETURN                                                            CHIPH303
C                                                                       CHIPH304
C902  CONTINUE                                                          CHIPH305
C     WRITE(6,*) 'Posssible direct decay into H-Chargino'               CHIPH306
C     chipho = 0.                                                       CHIPH307
C     RETURN                                                            CHIPH308
C                                                                       CHIPH309
C903  CONTINUE                                                          CHIPH310
C     WRITE(6,*) 'Posssible direct decay into Fermion-Sfermion'         CHIPH311
C     chipho = 0.                                                       CHIPH312
C     RETURN                                                            CHIPH313
C                                                                       CHIPH314
  999 RETURN                                                            CHIPH315
      END                                                               CHIPH316
      FUNCTION chizst(ichip,ichi)                                       CHIZST 2
C-------------------------------------------------------------------    CHIZST 3
C! Compute the decay width of chi' --> chi Z*                           CHIZST 4
C  where chi' and chi are any two of the four neutralinos.              CHIZST 5
C                                                                       CHIZST 6
C  Input:     -- ichip, the first neutralino index                      CHIZST 7
C             -- ichi,  the second neutralino index                     CHIZST 8
C                                                                       CHIZST 9
C  Output:    -- chizst, the decay width in GeV                         CHIZST10
C                                                                       CHIZST11
C  V. Bertin, for the CHA001 generator                                  CHIZST12
C                                                                       CHIZST13
C  Modif:  Patrick Janot (31 Aug 1995)                                  CHIZST14
C          Adapt the routine for the HZHAxx generator                   CHIZST15
C-------------------------------------------------------------------    CHIZST16
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),               CONSTS 2
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw      CONSTS 3
C                                                                       CONSTS 4
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)        DATAPI 2
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          DATAPI 3
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         DATAPI 4
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         DATAPI 5
C                                                                       DATAPI 6
      REAL*8 a,g,xx,xi,xip,clr2,cltr, DGMLT1, xlo,xhi,x(1)              CHIZST20
      COMMON / chachi / a,g,xx,xip,clr2,cltr                            CHIZST21
      EXTERNAL dgmlt1, fsubzst                                          CHIZST22
C                                                                       CHIZST23
C     PRINT *,'ichii ichio :',ichip,ichi                                CHIZST24
      chizst = 0.                                                       CHIZST25
      xip = ABS(amneut(ichip))                                          CHIZST26
      xi  = ABS(amneut(ichi))                                           CHIZST27
      xx = amneut(ichi) / amneut(ichip)                                 CHIZST28
      isgn = 1                                                          CHIZST29
      IF ( xx .LT. 0D0 ) isgn = -1                                      CHIZST30
      IF ( xip .LE. xi ) GOTO 999                                       CHIZST31
C                                                                       CHIZST32
      a = amz                                                           CHIZST33
      g = gmz                                                           CHIZST34
      xlo    = -DATAN2(a/xip,g/xip)                                     CHIZST35
      xhi    =  DATAN2((1D0-DABS(xx))**2-(a/xip)**2,a*g/xip**2)         CHIZST36
      partot = DGMLT1(fsubzst,xlo,xhi,1,6,x)                            CHIZST37
     .       / ( a * g / xip**2 )                                       CHIZST38
C                                                                       CHIZST39
      atrix = - fieldn(3,ichip)*fieldn(3,ichi)                          CHIZST40
     +        + fieldn(4,ichip)*fieldn(4,ichi)                          CHIZST41
C                                                                       CHIZST42
      bneutr = brnunu(xip,xi,isgn) / 3.                                 CHIZST43
C                                                                       CHIZST44
      gamchp = alpha(0)**2 * xip * atrix**2 * partot                    CHIZST45
     .       / ( 192. * pi * bneutr )                                   CHIZST46
     .       / ( sw2 * cw2 )**2                                         CHIZST47
C     PRINT *,'partot atrix bneutr :',partot,atrix,bneutr               CHIZST48
C                                                                       CHIZST49
      chizst = gamchp                                                   CHIZST50
C     PRINT *,'gamchp :',gamchp                                         CHIZST51
C                                                                       CHIZST52
 999  CONTINUE                                                          CHIZST53
      RETURN                                                            CHIZST54
      END                                                               CHIZST55
      SUBROUTINE COMBRA                                                 COMBRA 2
C------------------------------------------------------------------     COMBRA 3
C!  Compute branching ratios                                            COMBRA 4
C                                                                       COMBRA 5
C  Input:    /PARAM/                                                    COMBRA 6
C                                                                       COMBRA 7
C  Output:   /HHDECK/                                                   COMBRA 8
C                                                                       COMBRA 9
C   P. Janot -- 24 August 1991                                          COMBRA10
C------------------------------------------------------------------     COMBRA11
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      CHARACTER*14 channel                                              HHDECK 2
      CHARACTER*21 channeut, chanchar                                   HHDECK 3
      PARAMETER(nchneut=8,nchchar=5)                                    HHDECK 4
      COMMON / hhdeck / branch(nchan,nhig),width(nhig),                 HHDECK 5
     .                  parwth(nhig),xymas(2,nchan,nhig),               HHDECK 6
     .                  xywid(2,nchan,nhig)                             HHDECK 7
      COMMON / chaneu / ichn(2),                                        HHDECK 8
     .                  wneut(4,4,nhig), wchar(2,2,nhig),               HHDECK 9
     .                  widneut(4), brneut(nchneut,4),                  HHDECK10
     .                  widchar(2), brchar(nchchar,2)                   HHDECK11
      COMMON / chanch / channeut(nchneut,4), chanchar(nchchar,2)        HHDECK12
      COMMON / vect4 / pvect4(5,2)                                      HHDECK13
      COMMON / hinput / ichan(nchan,nhig), channel(nchan,nhig)          HHDECK14
      COMMON / hisbr / bchpp,bchgg                                      HHDECK15
      DIMENSION ph(4)                                                   HHDECK16
C                                                                       HHDECK17
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
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),               CONSTS 2
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw      CONSTS 3
C                                                                       CONSTS 4
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)        DATAPI 2
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          DATAPI 3
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         DATAPI 4
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         DATAPI 5
C                                                                       DATAPI 6
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,      MISCL  2
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),            MISCL  3
     &                 nevent(11)                                       MISCL  4
      INTEGER loutbe,idb1,idb2,nevent                                   MISCL  5
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad                      MISCL  6
C                                                                       MISCL  7
      COMPLEX f12, f1, f0, gzi1, gzi2, branch1, branch2, branch3        COMBRA18
      COMPLEX gagasm, gagall, ggsm, ggall                               COMBRA19
      REAL*8 phspgz, brwwzz                                             COMBRA20
      EXTERNAL f12, f1, f0, gzi1, gzi2, phspgz, brwwzz                  COMBRA21
      COMMON /gagagg/ gagasm(nhig),gagall(nhig),ggsm(nhig),ggall(nhig)  COMBRA22
      CHARACTER*1 star, higgs(nhig)                                     COMBRA23
      DATA higgs/'H','h','A'/                                           COMBRA24
      DATA hbar/6.583173D-25/                                           COMBRA25
C                                                                       COMBRA26
C                                                                       COMBRA27
      ada(am1,am2,am3) = (am1**2-am2**2-am3**2)**2-4.*am2**2*am3**2     COMBRA28
C                                                                       COMBRA29
      CALL vzero(branch(1,1),nhig*nchan)                                COMBRA30
      CALL vzero(width(1),nhig)                                         COMBRA31
      CALL vzero(parwth(1),nhig)                                        COMBRA32
      CALL vzero(wwmax(1,1),2*nhig)                                     COMBRA33
      CALL vzero(jtyp(1,1),2*nhig)                                      COMBRA34
C                                                                       COMBRA35
      couplh = 1./(32.*pi*amw**2)                                       COMBRA36
      brtau = couplh*amtau**2                                           COMBRA37
      bre   = couplh*ame**2                                             COMBRA38
      brmu  = couplh*ammu**2                                            COMBRA39
      brb   = 3.*couplh*amb**2                                          COMBRA40
      brc   = 3.*couplh*amc**2                                          COMBRA41
      brs   = 3.*couplh*ams**2                                          COMBRA42
      brt   = 3.*couplh*amt**2                                          COMBRA43
      brw   = 1./(64.*pi*amw**2)                                        COMBRA44
      brz   = 1./(128.*pi*amz**2*cw2)                                   COMBRA45
      brga  = 1./(1024.*pi**3*amw**2)                                   COMBRA46
      brgg  = 1./(512.*pi**3*amw**2)                                    COMBRA47
      brgz  = 1./(512.*pi**3*amw**2)                                    COMBRA48
C                                                                       COMBRA49
C Compute partial widths                                                COMBRA50
C                                                                       COMBRA51
      DO 1 jhig = 1, nhig                                               COMBRA52
C                                                                       COMBRA53
        IF ( ism .EQ. 1 .AND. jhig .NE. 2 ) GOTO 1                      COMBRA54
C                                                                       COMBRA55
C Define couplings                                                      COMBRA56
C                                                                       COMBRA57
        IF ( jhig .EQ. 1 ) THEN                                         COMBRA58
          rup  = sa/sb                                                  COMBRA59
          su1  = sa/sb                                                  COMBRA60
          su2  =-ca/sb                                                  COMBRA61
          rdw  = ca/cb                                                  COMBRA62
          sd1  = ca/cb                                                  COMBRA63
          sd2  =-sa/cb                                                  COMBRA64
          rw   = COS(beta-alfa)                                         COMBRA65
          rhp  = rw - c2b*cab/(2.*cw2)                                  COMBRA66
          epsh = 1.                                                     COMBRA67
          rsf  = cab                                                    COMBRA68
          kpw  = 3                                                      COMBRA69
          eta  = 1.                                                     COMBRA70
        ELSEIF ( jhig .EQ. 2 ) THEN                                     COMBRA71
          IF ( ism .EQ. 0 ) THEN                                        COMBRA72
            rup  = ca/sb                                                COMBRA73
            su1  = ca/sb                                                COMBRA74
            su2  = sa/sb                                                COMBRA75
            rdw  =-sa/cb                                                COMBRA76
            sd1  =-sa/cb                                                COMBRA77
            sd2  =-ca/cb                                                COMBRA78
            rw   = SIN(beta-alfa)                                       COMBRA79
            rhp  = rw + c2b*sab/(2.*cw2)                                COMBRA80
            epsh = 1.                                                   COMBRA81
            rsf  = -sab                                                 COMBRA82
            kpw  = 3                                                    COMBRA83
            eta  = 1.                                                   COMBRA84
          ELSE                                                          COMBRA85
            rup  = 1.                                                   COMBRA86
            su1  = 1.                                                   COMBRA87
            su2  = 1.                                                   COMBRA88
            rdw  = 1.                                                   COMBRA89
            sd1  = 1.                                                   COMBRA90
            sd2  = 1.                                                   COMBRA91
            rw   = 1.                                                   COMBRA92
            rhp  = 0.                                                   COMBRA93
            epsh = 0.                                                   COMBRA94
            rsf  = 0.                                                   COMBRA95
            kpw  = 3                                                    COMBRA96
            eta  = 0.                                                   COMBRA97
          ENDIF                                                         COMBRA98
        ELSEIF ( jhig .EQ. 3 ) THEN                                     COMBRA99
          rup  = 1./tb                                                  COMBR100
          su1  = 0.                                                     COMBR101
          su2  = 0.                                                     COMBR102
          rdw  = tb                                                     COMBR103
          sd1  = 0.                                                     COMBR104
          sd2  = 0.                                                     COMBR105
          rw   = 0.                                                     COMBR106
          rhp  = 0.                                                     COMBR107
          epsh = 0.                                                     COMBR108
          rsf  = 0.                                                     COMBR109
          kpw  = 1                                                      COMBR110
          eta  = -1.                                                    COMBR111
        ENDIF                                                           COMBR112
C                                                                       COMBR113
C Running quark masses                                                  COMBR114
C                                                                       COMBR115
        runamu = amu/100.                                               COMBR116
        runamd = amd/100.                                               COMBR117
        runams = runmas(ams,jhig,rads)                                  COMBR118
        runamc = runmas(amc,jhig,radc)                                  COMBR119
        runamb = runmas(amb,jhig,radb)                                  COMBR120
        runamt = runmas(amt,jhig,radt)                                  COMBR121
        rggamu = 1.                                                     COMBR122
        rggamd = 1.                                                     COMBR123
        rggams = 1.                                                     COMBR124
        rggamc = 1.                                                     COMBR125
        rggamb = 1.                                                     COMBR126
        rggamt = 1.                                                     COMBR127
C                                                                       COMBR128
C h,H,A --> gamma gamma                                                 COMBR129
C                                                                       COMBR130
        branch1 =                                                       COMBR131
C Leptons                                                               COMBR132
     .         rdw * f12(jhig,ame)                                      COMBR133
     .       + rdw * f12(jhig,ammu)                                     COMBR134
     .       + rdw * f12(jhig,amtau)                                    COMBR135
C Down type quarks                                                      COMBR136
     .       + rdw * f12(jhig,amd*SQRT(rggamd)) / 3.                    COMBR137
     .       + rdw * f12(jhig,ams*SQRT(rggams)) / 3.                    COMBR138
     .       + rdw * f12(jhig,amb*SQRT(rggamb)) / 3.                    COMBR139
C Up type quarks                                                        COMBR140
     .       + rup * f12(jhig,amu*SQRT(rggamu)) * 4./3.                 COMBR141
     .       + rup * f12(jhig,amc*SQRT(rggamc)) * 4./3.                 COMBR142
     .       + rup * f12(jhig,amt*SQRT(rggamt)) * 4./3.                 COMBR143
C W boson                                                               COMBR144
     .       + rw * f1(jhig,amw)                                        COMBR145
C                                                                       COMBR146
        gagasm(jhig) = branch1                                          COMBR147
C       WRITE(6,*) jhig,branch1                                         COMBR148
        IF ( ism .EQ. 0 ) branch1 = branch1                             COMBR149
C Charged Higgs bosons                                                  COMBR150
     .       + rhp * f0(jhig,amhp) * amw**2/amhp**2                     COMBR151
C       WRITE(6,*) jhig,branch1                                         COMBR152
        IF ( ism .EQ. 0 ) branch1 = branch1                             COMBR153
C S-leptons (Left)                                                      COMBR154
     .     + ( epsh*(ame  **2/amz**2*rdw + (sw2-1./2.)*rsf)             COMBR155
     .       + epsh*(ammu **2/amz**2*rdw + (sw2-1./2.)*rsf)             COMBR156
     .       + epsh*(amtau**2/amz**2*rdw + (sw2-1./2.)*rsf) )           COMBR157
     .     * f0(jhig,susSML) * (amz/susSML)**2                          COMBR158
C S-leptons (Right)                                                     COMBR159
     .     + ( epsh*(ame  **2/amz**2*rdw -  sw2*rsf)                    COMBR160
     .       + epsh*(ammu **2/amz**2*rdw -  sw2*rsf)                    COMBR161
     .       + epsh*(amtau**2/amz**2*rdw -  sw2*rsf) )                  COMBR162
     .     * f0(jhig,susSME) * (amz/susSME)**2                          COMBR163
C S-Down type quarks (Left)                                             COMBR164
     .     + ( epsh*(amd**2/amz**2*rdw + (sw2/3.-1./2.)*rsf)            COMBR165
     .       + epsh*(ams**2/amz**2*rdw + (sw2/3.-1./2.)*rsf) )          COMBR166
     .     * f0(jhig,susSMQ) * (amz/susSMQ)**2 / 3.                     COMBR167
C S-Down type quarks (Right)                                            COMBR168
     .     + ( epsh*(amd**2/amz**2*rdw -  sw2/3.*rsf)                   COMBR169
     .       + epsh*(ams**2/amz**2*rdw -  sw2/3.*rsf) )                 COMBR170
C    .     * f0(jhig,susSMD) * (amz/susSMD)**2 / 3.                     COMBR171
     .     * f0(jhig,susSMQ) * (amz/susSMQ)**2 / 3.                     COMBR172
C S-Up type quarks (Left)                                               COMBR173
     .     + ( epsh*(amu**2/amz**2*rup - (sw2*2./3.-1./2.)*rsf)         COMBR174
     .       + epsh*(amc**2/amz**2*rup - (sw2*2./3.-1./2.)*rsf) )       COMBR175
     .     * f0(jhig,susSMQ) * (amz/susSMQ)**2 * 4./3.                  COMBR176
C S-Up type quarks (Right)                                              COMBR177
C       WRITE(6,*) jhig,branch1                                         COMBR178
        IF ( ism .EQ. 0 ) branch1 = branch1                             COMBR179
     .     + ( epsh*(amu**2/amz**2*rup +  sw2*2./3.*rsf)                COMBR180
     .       + epsh*(amc**2/amz**2*rup +  sw2*2./3.*rsf) )              COMBR181
C    .     * f0(jhig,susSMU) * (amz/susSMU)**2 * 4./3.                  COMBR182
     .     * f0(jhig,susSMQ) * (amz/susSMQ)**2 * 4./3.                  COMBR183
C       WRITE(6,*) jhig,branch1                                         COMBR184
        IF ( ism .EQ. 0 ) branch1 = branch1                             COMBR185
C S-bottom quarks                                                       COMBR186
     .     + epsh                                                       COMBR187
     .     * ( (amb**2/amz**2*rdw+(sw2/3.-1./2.)*rsf)                   COMBR188
     .       * (SIN(botmix))**2                                         COMBR189
     .       + (amb**2/amz**2*rdw- sw2/3.       *rsf)                   COMBR190
     .       * (COS(botmix))**2                                         COMBR191
     .       - (amb/amz*(sd1*susAb/amz+sd2*susMu/amz))                  COMBR192
     .       * SIN(botmix)*COS(botmix) )                                COMBR193
     .     * f0(jhig,amsb(2)) * (amz/amsb(2))**2 / 3.                   COMBR194
C                                                                       COMBR195
     .     + epsh                                                       COMBR196
     .     * ( (amb**2/amz**2*rdw+(sw2/3.-1./2.)*rsf)                   COMBR197
     .       * (COS(botmix))**2                                         COMBR198
     .       + (amb**2/amz**2*rdw- sw2/3.       *rsf)                   COMBR199
     .       * (SIN(botmix))**2                                         COMBR200
     .       + (amb/amz*(sd1*susAb/amz+sd2*susMu/amz))                  COMBR201
     .       * SIN(botmix)*COS(botmix) )                                COMBR202
     .     * f0(jhig,amsb(1)) * (amz/amsb(1))**2 / 3.                   COMBR203
C       WRITE(6,*) jhig,branch1                                         COMBR204
        IF ( ism .EQ. 0 ) branch1 = branch1                             COMBR205
C S-top quarks                                                          COMBR206
     .     + epsh                                                       COMBR207
     .     * ( (amt**2/amz**2*rup-(sw2*2./3.-1./2.)*rsf)                COMBR208
     .       * (SIN(topmix))**2                                         COMBR209
     .       + (amt**2/amz**2*rup+ sw2*2./3.       *rsf)                COMBR210
     .       * (COS(topmix))**2                                         COMBR211
     .       - (amt/amz*(su1*susAt/amz+su2*susMu/amz))                  COMBR212
     .       * SIN(topmix)*COS(topmix) )                                COMBR213
     .     * f0(jhig,amst(2)) * (amz/amst(2))**2 * 4./3.                COMBR214
C                                                                       COMBR215
     .     + epsh                                                       COMBR216
     .     * ( (amt**2/amz**2*rup-(sw2*2./3.-1./2.)*rsf)                COMBR217
     .       * (COS(topmix))**2                                         COMBR218
     .       + (amt**2/amz**2*rup+ sw2*2./3.       *rsf)                COMBR219
     .       * (SIN(topmix))**2                                         COMBR220
     .       + (amt/amz*(su1*susAt/amz+su2*susMu/amz))                  COMBR221
     .       * SIN(topmix)*COS(topmix) )                                COMBR222
     .     * f0(jhig,amst(1)) * (amz/amst(1))**2 * 4./3.                COMBR223
C       WRITE(6,*) jhig,branch1                                         COMBR224
        IF ( ism .EQ. 0 ) branch1 = branch1                             COMBR225
C Charginos                                                             COMBR226
     .       + 2.*bb(jhig,1,1) * f12(jhig,amchar(1))* amw/amchar(1)     COMBR227
     .       + 2.*bb(jhig,2,2) * f12(jhig,amchar(2))* amw/amchar(2)     COMBR228
C That's it                                                             COMBR229
C       WRITE(6,*) jhig,branch1                                         COMBR230
        gagall(jhig) = branch1                                          COMBR231
        branch(1,jhig) = brga * alpha(jhig)**2 * gweak2(jhig)           COMBR232
     .                        * amhig(jhig)**3                          COMBR233
     .                        * CABS(branch1)**2                        COMBR234
C                                                                       COMBR235
C h,H,A --> gluon gluon                                                 COMBR236
C                                                                       COMBR237
        xmh = amhig(jhig)                                               COMBR238
        IF ( xmh .GT. 1. ) THEN                                         COMBR239
          fnh = 3.                                                      COMBR240
          IF ( amc .LE. xmh/2. ) fnh = fnh + 1.                         COMBR241
          IF ( amb .LE. xmh/2. ) fnh = fnh + 1.                         COMBR242
          IF ( amt .LE. xmh/2. ) fnh = fnh + 1.                         COMBR243
          alphah = alphas(jhig)                                         COMBR244
          gw2    = gweak2(jhig)                                         COMBR245
C                                                                       COMBR246
          branch2 =                                                     COMBR247
C Down type quarks                                                      COMBR248
     .         rdw * f12(jhig,amd*SQRT(rggamd))                         COMBR249
     .       + rdw * f12(jhig,ams*SQRT(rggams))                         COMBR250
     .       + rdw * f12(jhig,amb*SQRT(rggamb))                         COMBR251
C Up type quarks                                                        COMBR252
     .       + rup * f12(jhig,amu*SQRT(rggamu))                         COMBR253
     .       + rup * f12(jhig,amc*SQRT(rggamc))                         COMBR254
     .       + rup * f12(jhig,amt*SQRT(rggamt))                         COMBR255
C                                                                       COMBR256
C         WRITE(6,*) jhig,branch2                                       COMBR257
          ggsm (jhig) = branch2                                         COMBR258
          IF ( ism .EQ. 0 ) branch2 = branch2                           COMBR259
C S-Down type quarks (Left)                                             COMBR260
     .     + ( epsh*(amd**2/amz**2*rdw + (sw2/3.-1./2.)*rsf)            COMBR261
     .       + epsh*(ams**2/amz**2*rdw + (sw2/3.-1./2.)*rsf) )          COMBR262
     .     * f0(jhig,susSMQ) * (amz/susSMQ)**2                          COMBR263
C S-Down type quarks (Right)                                            COMBR264
     .     + ( epsh*(amd**2/amz**2*rdw -  sw2/3.*rsf)                   COMBR265
     .       + epsh*(ams**2/amz**2*rdw -  sw2/3.*rsf) )                 COMBR266
C    .     * f0(jhig,susSMD) * (amz/susSMD)**2                          COMBR267
     .     * f0(jhig,susSMQ) * (amz/susSMQ)**2                          COMBR268
C         WRITE(6,*) jhig,branch2                                       COMBR269
          IF ( ism .EQ. 0 ) branch2 = branch2                           COMBR270
C S-Up type quarks (Left)                                               COMBR271
     .     + ( epsh*(amu**2/amz**2*rup - (sw2*2./3.-1./2.)*rsf)         COMBR272
     .       + epsh*(amc**2/amz**2*rup - (sw2*2./3.-1./2.)*rsf) )       COMBR273
     .     * f0(jhig,susSMQ) * (amz/susSMQ)**2                          COMBR274
C S-Up type quarks (Right)                                              COMBR275
     .     + ( epsh*(amu**2/amz**2*rup +  sw2*2./3.*rsf)                COMBR276
     .       + epsh*(amc**2/amz**2*rup +  sw2*2./3.*rsf) )              COMBR277
C    .     * f0(jhig,susSMU) * (amz/susSMU)**2                          COMBR278
     .     * f0(jhig,susSMU) * (amz/susSMQ)**2                          COMBR279
C S-bottom quarks                                                       COMBR280
C         WRITE(6,*) jhig,branch2                                       COMBR281
          IF ( ism .EQ. 0 ) branch2 = branch2                           COMBR282
     .     + epsh                                                       COMBR283
     .     * ( (amb**2/amz**2*rdw+(sw2/3.-1./2.)*rsf)                   COMBR284
     .       * (SIN(botmix))**2                                         COMBR285
     .       + (amb**2/amz**2*rdw- sw2/3.       *rsf)                   COMBR286
     .       * (COS(botmix))**2                                         COMBR287
     .       - (amb/amz*(sd1*susAb/amz+sd2*susMu/amz))                  COMBR288
     .       * SIN(botmix)*COS(botmix) )                                COMBR289
     .     * f0(jhig,amsb(2)) * (amz/amsb(2))**2                        COMBR290
C                                                                       COMBR291
     .     + epsh                                                       COMBR292
     .     * ( (amb**2/amz**2*rdw+(sw2/3.-1./2.)*rsf)                   COMBR293
     .       * (COS(botmix))**2                                         COMBR294
     .       + (amb**2/amz**2*rdw- sw2/3.       *rsf)                   COMBR295
     .       * (SIN(botmix))**2                                         COMBR296
     .       + (amb/amz*(sd1*susAb/amz+sd2*susMu/amz))                  COMBR297
     .       * SIN(botmix)*COS(botmix) )                                COMBR298
     .     * f0(jhig,amsb(1)) * (amz/amsb(1))**2                        COMBR299
C S-top quarks                                                          COMBR300
C         WRITE(6,*) jhig,branch2                                       COMBR301
          IF ( ism .EQ. 0 ) branch2 = branch2                           COMBR302
     .     + epsh                                                       COMBR303
     .     * ( (amt**2/amz**2*rup-(sw2*2./3.-1./2.)*rsf)                COMBR304
     .       * (SIN(topmix))**2                                         COMBR305
     .       + (amt**2/amz**2*rup+ sw2*2./3.       *rsf)                COMBR306
     .       * (COS(topmix))**2                                         COMBR307
     .       - (amt/amz*(su1*susAt/amz+su2*susMu/amz))                  COMBR308
     .       * SIN(topmix)*COS(topmix) )                                COMBR309
     .     * f0(jhig,amst(2)) * (amz/amst(2))**2                        COMBR310
C                                                                       COMBR311
     .     + epsh                                                       COMBR312
     .     * ( (amt**2/amz**2*rup-(sw2*2./3.-1./2.)*rsf)                COMBR313
     .       * (COS(topmix))**2                                         COMBR314
     .       + (amt**2/amz**2*rup+ sw2*2./3.       *rsf)                COMBR315
     .       * (SIN(topmix))**2                                         COMBR316
     .       + (amt/amz*(su1*susAt/amz+su2*susMu/amz))                  COMBR317
     .       * SIN(topmix)*COS(topmix) )                                COMBR318
     .     * f0(jhig,amst(1)) * (amz/amst(1))**2                        COMBR319
C That's it                                                             COMBR320
C                                                                       COMBR321
          ggall(jhig) = branch2                                         COMBR322
C         WRITE(6,*) jhig,branch2                                       COMBR323
C         WRITE(6,*) '-------------------'                              COMBR324
C         WRITE(6,*) jhig,gagasm(jhig),' (gaga sm )'                    COMBR325
C         WRITE(6,*) jhig,gagall(jhig),' (gaga all)'                    COMBR326
C         WRITE(6,*) jhig,ggsm  (jhig) ,'(glgl sm )'                    COMBR327
C         WRITE(6,*) jhig,ggall (jhig),' (glgl all)'                    COMBR328
C         WRITE(6,*) '-------------------'                              COMBR329
          branch(2,jhig) = brgg * alphah**2 * xmh**3 * gw2              COMBR330
     .                   * CABS(branch2)**2                             COMBR331
C But account also for large QCD corrections (Kniehl et al)             COMBR332
     .                   * ( 1D0 + alphah/pi*(95./4.-7.*fnh/6.) )       COMBR333
        ELSE                                                            COMBR334
          branch(2,jhig) = 0.                                           COMBR335
        ENDIF                                                           COMBR336
C                                                                       COMBR337
C h,H,A --> gamma Z0                                                    COMBR338
C                                                                       COMBR339
        xmh = amhig(jhig)                                               COMBR340
        IF ( xmh .GT. 20. ) THEN                                        COMBR341
          tw = sw/cw                                                    COMBR342
          tauw = 4.*amw**2/amhig(jhig)**2                               COMBR343
          branch3 =                                                     COMBR344
C Leptons                                                               COMBR345
     .   2.* (-1./2. + 2.*sw2) / (sw*cw)                                COMBR346
     .     * (  rdw * (epsh*gzi1(jhig,ame  )-gzi2(jhig,ame  ))          COMBR347
     .        + rdw * (epsh*gzi1(jhig,ammu )-gzi2(jhig,ammu ))          COMBR348
     .        + rdw * (epsh*gzi1(jhig,amtau)-gzi2(jhig,amtau)) )        COMBR349
C Down type quarks                                                      COMBR350
     . + 2.* (-1./2. + 2.*sw2/3.) / (sw*cw)                             COMBR351
     .     * ( rdw * (epsh*gzi1(jhig,amd)-gzi2(jhig,amd))               COMBR352
     .       + rdw * (epsh*gzi1(jhig,ams)-gzi2(jhig,ams))               COMBR353
     .       + rdw * (epsh*gzi1(jhig,amb)-gzi2(jhig,amb)) )             COMBR354
C Up type quarks                                                        COMBR355
     . - 4.* (+1./2. - 4.*sw2/3.) / (sw*cw)                             COMBR356
     .     * ( rup * (epsh*gzi1(jhig,amu)-gzi2(jhig,amu))               COMBR357
     .       + rup * (epsh*gzi1(jhig,amc)-gzi2(jhig,amc))               COMBR358
     .       + rup * (epsh*gzi1(jhig,amt)-gzi2(jhig,amt)) )             COMBR359
C W boson                                                               COMBR360
     . - 1./tw * rw                                                     COMBR361
     .    * ( 4.*(3.-tw**2)                  * gzi2(jhig,amw)           COMBR362
     .    + ((1.+2./tauw)*tw**2-(5.+2./tauw))* gzi1(jhig,amw))          COMBR363
C                                                                       COMBR364
C         WRITE(6,*) jhig,branch3                                       COMBR365
          IF ( ism .EQ. 0 ) branch3 = branch3                           COMBR366
C Charged Higgs bosons                                                  COMBR367
     . + (1.-2.*sw2) / (sw*cw)                                          COMBR368
     .       * rhp * gzi1(jhig,amhp) * amw**2/amhp**2                   COMBR369
C         WRITE(6,*) jhig,branch3                                       COMBR370
          IF ( ism .EQ. 0 ) branch3 = branch3                           COMBR371
C S-leptons (Left)                                                      COMBR372
     . + 2.* (-1./2. + sw2) / (sw*cw)                                   COMBR373
     .     * ( epsh*(ame  **2/amz**2*rdw + (sw2-1./2.)*rsf)             COMBR374
     .       + epsh*(ammu **2/amz**2*rdw + (sw2-1./2.)*rsf)             COMBR375
     .       + epsh*(amtau**2/amz**2*rdw + (sw2-1./2.)*rsf) )           COMBR376
     .     * gzi1(jhig,susSML) * (amz/susSML)**2                        COMBR377
C S-leptons (Right)                                                     COMBR378
     . + 2.*           sw2  / (sw*cw)                                   COMBR379
     .     * ( epsh*(ame  **2/amz**2*rdw -  sw2*rsf)                    COMBR380
     .       + epsh*(ammu **2/amz**2*rdw -  sw2*rsf)                    COMBR381
     .       + epsh*(amtau**2/amz**2*rdw -  sw2*rsf) )                  COMBR382
     .     * gzi1(jhig,susSME) * (amz/susSME)**2                        COMBR383
C S-Down type quarks (Left)                                             COMBR384
     . + 2.* (-1./2. + sw2/3.) / (sw*cw)                                COMBR385
     .     * ( epsh*(amd**2/amz**2*rdw + (sw2/3.-1./2.)*rsf)            COMBR386
     .       + epsh*(ams**2/amz**2*rdw + (sw2/3.-1./2.)*rsf) )          COMBR387
     .     * gzi1(jhig,susSMQ) * (amz/susSMQ)**2                        COMBR388
C S-Down type quarks (Right)                                            COMBR389
     . + 2.*           sw2/3.  / (sw*cw)                                COMBR390
     .     * ( epsh*(amd**2/amz**2*rdw -  sw2/3.*rsf)                   COMBR391
     .       + epsh*(ams**2/amz**2*rdw -  sw2/3.*rsf) )                 COMBR392
C    .     * gzi1(jhig,susSMD) * (amz/susSMD)**2                        COMBR393
     .     * gzi1(jhig,susSMQ) * (amz/susSMQ)**2                        COMBR394
C S-Up type quarks (Left)                                               COMBR395
     . - 4.* (1./2. - 2.*sw2/3.) / (sw*cw)                              COMBR396
     .     * ( epsh*(amu**2/amz**2*rup - (sw2*2./3.-1./2.)*rsf)         COMBR397
     .       + epsh*(amc**2/amz**2*rup - (sw2*2./3.-1./2.)*rsf) )       COMBR398
     .     * gzi1(jhig,susSMQ) * (amz/susSMQ)**2                        COMBR399
C S-Up type quarks (Right)                                              COMBR400
C         WRITE(6,*) jhig,branch3                                       COMBR401
          IF ( ism .EQ. 0 ) branch3 = branch3                           COMBR402
     . - 4.* (      - 2.*sw2/3.) / (sw*cw)                              COMBR403
     .     + ( epsh*(amu**2/amz**2*rup +  sw2*2./3.*rsf)                COMBR404
     .       + epsh*(amc**2/amz**2*rup +  sw2*2./3.*rsf) )              COMBR405
C    .     * gzi1(jhig,susSMU) * (amz/susSMU)**2                        COMBR406
     .     * gzi1(jhig,susSMQ) * (amz/susSMQ)**2                        COMBR407
C         WRITE(6,*) jhig,branch1                                       COMBR408
          IF ( ism .EQ. 0 ) branch3 = branch3                           COMBR409
C S-bottom quarks                                                       COMBR410
     .     + epsh                                                       COMBR411
     .     * ( 2.* (-1./2. +    sw2/3.) / (sw*cw)                       COMBR412
     .       + (amb**2/amz**2*rdw+(sw2/3.-1./2.)*rsf)                   COMBR413
     .       * (SIN(botmix))**2                                         COMBR414
     .     +   2.*              sw2/3.  / (sw*cw)                       COMBR415
     .       * (amb**2/amz**2*rdw- sw2/3.       *rsf)                   COMBR416
     .       * (COS(botmix))**2                                         COMBR417
     .     -   1.* (-1./2. + 2.*sw2/3.) / (sw*cw)                       COMBR418
     .       * (amb/amz*(sd1*susAb/amz+sd2*susMu/amz))                  COMBR419
     .       * SIN(botmix)*COS(botmix) )                                COMBR420
     .     * gzi1(jhig,amsb(2)) * (amz/amsb(2))**2                      COMBR421
C                                                                       COMBR422
     .     + epsh                                                       COMBR423
     .     * ( 2.* (-1./2. +    sw2/3.) / (sw*cw)                       COMBR424
     .       * (amb**2/amz**2*rdw+(sw2/3.-1./2.)*rsf)                   COMBR425
     .       * (COS(botmix))**2                                         COMBR426
     .     +   2.*              sw2/3.  / (sw*cw)                       COMBR427
     .       * (amb**2/amz**2*rdw- sw2/3.       *rsf)                   COMBR428
     .       * (SIN(botmix))**2                                         COMBR429
     .     +   1.* (-1./2. + 2.*sw2/3.) / (sw*cw)                       COMBR430
     .       * (amb/amz*(sd1*susAb/amz+sd2*susMu/amz))                  COMBR431
     .       * SIN(botmix)*COS(botmix) )                                COMBR432
     .     * gzi1(jhig,amsb(1)) * (amz/amsb(1))**2                      COMBR433
C         WRITE(6,*) jhig,branch1                                       COMBR434
          IF ( ism .EQ. 0 ) branch3 = branch3                           COMBR435
C S-top quarks                                                          COMBR436
     .     + epsh                                                       COMBR437
     .     * ( - 4.* (1./2. - 2.*sw2/3.) / (sw*cw)                      COMBR438
     .       * (amt**2/amz**2*rup-(sw2*2./3.-1./2.)*rsf)                COMBR439
     .       * (SIN(topmix))**2                                         COMBR440
     .     -     4.* (      - 2.*sw2/3.) / (sw*cw)                      COMBR441
     .       * (amt**2/amz**2*rup+ sw2*2./3.       *rsf)                COMBR442
     .       * (COS(topmix))**2                                         COMBR443
     .     +     2.* (1./2. - 4.*sw2/3.) / (sw*cw)                      COMBR444
     .       * (amt/amz*(su1*susAt/amz+su2*susMu/amz))                  COMBR445
     .       * SIN(topmix)*COS(topmix) )                                COMBR446
     .     * gzi1(jhig,amst(2)) * (amz/amst(2))**2                      COMBR447
C                                                                       COMBR448
     .     + epsh                                                       COMBR449
     .     * ( - 4.* (1./2. - 2.*sw2/3.) / (sw*cw)                      COMBR450
     .       * (amt**2/amz**2*rup-(sw2*2./3.-1./2.)*rsf)                COMBR451
     .       * (COS(topmix))**2                                         COMBR452
     .     -     4.* (      - 2.*sw2/3.) / (sw*cw)                      COMBR453
     .       * (amt**2/amz**2*rup+ sw2*2./3.       *rsf)                COMBR454
     .       * (SIN(topmix))**2                                         COMBR455
     .     -     2.* (1./2. - 4.*sw2/3.) / (sw*cw)                      COMBR456
     .       * (amt/amz*(su1*susAt/amz+su2*susMu/amz))                  COMBR457
     .       * SIN(topmix)*COS(topmix) )                                COMBR458
     .     * gzi1(jhig,amst(1)) * (amz/amst(1))**2                      COMBR459
C         WRITE(6,*) jhig,branch3                                       COMBR460
          IF ( ism .EQ. 0 ) branch3 = branch3                           COMBR461
C Charginos                                                             COMBR462
     .     + 2.*bb(jhig,1,1) * 2. / (cw*sw)                             COMBR463
     .       * ( - vmat(1,1)**2 - vmat(1,2)**2/2.                       COMBR464
     .           - umat(1,1)**2 - umat(1,2)**2/2.                       COMBR465
     .           + 2.*sw2 )                                             COMBR466
     .       * (epsh*gzi1(jhig,amchar(1))-gzi2(jhig,amchar(1)))         COMBR467
     .       *  amw/amchar(1)                                           COMBR468
     .     + 2.*bb(jhig,2,2) * 2. / (cw*sw)                             COMBR469
     .       * ( - vmat(2,1)**2 - vmat(2,2)**2/2.                       COMBR470
     .           - umat(2,1)**2 - umat(2,2)**2/2.                       COMBR471
     .           + 2.*sw2 )                                             COMBR472
     .       * (epsh*gzi1(jhig,amchar(2))-gzi2(jhig,amchar(2)))         COMBR473
     .       *  amw/amchar(2)                                           COMBR474
C That's it                                                             COMBR475
C         WRITE(6,*) jhig,branch3                                       COMBR476
          branch(11,jhig) = brgz * alpha(jhig)**2 * gweak2(jhig)        COMBR477
     .                           * amhig(jhig)**3                       COMBR478
     .                           * CABS(branch3)**2                     COMBR479
     .                           * phspgz(amhig(jhig)**2,amz,gmz)       COMBR480
        ELSE                                                            COMBR481
          branch(11,jhig) = 0.                                          COMBR482
        ENDIF                                                           COMBR483
C                                                                       COMBR484
C h,H,A --> e+e-                                                        COMBR485
C                                                                       COMBR486
        IF ( amhig(jhig) .GT. 2.*ame) branch(12,jhig) =                 COMBR487
     .            gweak2(jhig)*bre*rdw**2*amhig(jhig)*                  COMBR488
     .            SQRT(1.-4*ame**2/amhig(jhig)**2)**kpw                 COMBR489
     .            * weakcor(jhig,ame,1.,rup/rdw,rw/rdw)                 COMBR490
C                                                                       COMBR491
C h,H,A --> mu+mu-                                                      COMBR492
C                                                                       COMBR493
        IF ( amhig(jhig) .GT. 2.*ammu) branch(13,jhig) =                COMBR494
     .            gweak2(jhig)*brmu*rdw**2*amhig(jhig)*                 COMBR495
     .            SQRT(1.-4*ammu**2/amhig(jhig)**2)**kpw                COMBR496
     .            * weakcor(jhig,ammu,1.,rup/rdw,rw/rdw)                COMBR497
C                                                                       COMBR498
C h,H,A --> tau+tau-                                                    COMBR499
C                                                                       COMBR500
        IF ( amhig(jhig) .GT. 2.*amtau) branch(3,jhig) =                COMBR501
     .            gweak2(jhig)*brtau*rdw**2*amhig(jhig)*                COMBR502
     .            SQRT(1.-4*amtau**2/amhig(jhig)**2)**kpw               COMBR503
     .            * weakcor(jhig,amtau,1.,rup/rdw,rw/rdw)               COMBR504
C                                                                       COMBR505
C h,H,A --> s sbar                                                      COMBR506
C                                                                       COMBR507
        IF ( amhig(jhig) .GT. AMAX1(1.,2.*ams) )                        COMBR508
     .    branch(14,jhig) = runams * rads *                             COMBR509
     .            gweak2(jhig)*brb*rdw**2*amhig(jhig)*                  COMBR510
     .            SQRT(1.-4*ams**2*runams/amhig(jhig)**2)**kpw          COMBR511
     .            * weakcor(jhig,ams,1./3.,rup/rdw,rw/rdw)              COMBR512
C                                                                       COMBR513
C h,H,A --> b bbar                                                      COMBR514
C                                                                       COMBR515
        IF ( amhig(jhig) .GT. 2.*amb)                                   COMBR516
     .    branch(5,jhig) = runamb * radc *                              COMBR517
     .            gweak2(jhig)*brb*rdw**2*amhig(jhig)*                  COMBR518
     .            SQRT(1.-4*amb**2*runamb/amhig(jhig)**2)**kpw          COMBR519
     .            * weakcor(jhig,amb,1./3.,rup/rup,rw/rup)              COMBR520
C                                                                       COMBR521
C h,H,A --> c cbar                                                      COMBR522
C                                                                       COMBR523
        IF ( amhig(jhig) .GT. AMAX1(3.7,2.*amc) )                       COMBR524
     .    branch(4,jhig) = runamc * radb *                              COMBR525
     .            gweak2(jhig)*brc*rup**2*amhig(jhig)*                  COMBR526
     .            SQRT(1.-4*amc**2*runamc/amhig(jhig)**2)**kpw          COMBR527
     .            * weakcor(jhig,amc,2./3.,rup/rdw,rw/rdw)              COMBR528
C                                                                       COMBR529
C  h,H,A --> t tbar                                                     COMBR530
C                                                                       COMBR531
        IF ( amhig(jhig) .GT. 2.*amt)                                   COMBR532
     .    branch(6,jhig) = runamt * radt *                              COMBR533
     .            gweak2(jhig)*brt*rup**2*amhig(jhig)*                  COMBR534
     .            SQRT(1.-4*amt**2*runamt/amhig(jhig)**2)**kpw          COMBR535
C                                                                       COMBR536
C  h,H,A --> W W, Z Z                                                   COMBR537
C                                                                       COMBR538
        IF ( jhig .NE. 3 .AND. amhig(jhig) .GT. 20. ) THEN              COMBR539
          const = 3. * SQRT(2.) * G_F / (4.*pi*amhig(jhig)**3)          COMBR540
          compww = const*brwwzz(amhig(jhig),amw,gmw)                    COMBR541
          compzz = const*brwwzz(amhig(jhig),amz,gmz)/2.                 COMBR542
          branch(7,jhig) = rw**2 * compww                               COMBR543
          branch(8,jhig) = rw**2 * compzz                               COMBR544
        ENDIF                                                           COMBR545
C                                                                       COMBR546
        IF ( ism .EQ. 1 ) THEN                                          COMBR547
          CALL vzero(amneut(1),4)                                       COMBR548
          CALL vzero(amchar(1),2)                                       COMBR549
          CALL vzero(wneut(1,1,1),4*4*nhig)                             COMBR550
          CALL vzero(wchar(1,1,1),2*2*nhig)                             COMBR551
          wneut(1,1,jhig) = 1.                                          COMBR552
          branch(15,jhig) = branch(12,jhig)/1E6                         COMBR553
          branch(16,jhig) = 0.                                          COMBR554
        ENDIF                                                           COMBR555
C                                                                       COMBR556
C Now, only the MSSM is concerned.                                      COMBR557
C                                                                       COMBR558
        IF ( ism .EQ. 0 ) THEN                                          COMBR559
C                                                                       COMBR560
C h,H,A --> chi chi                                                     COMBR561
C                                                                       COMBR562
          DO i = 1, 4                                                   COMBR563
            DO j = i, 4                                                 COMBR564
              IF ( i .EQ. j ) THEN                                      COMBR565
                dij = 1.                                                COMBR566
              ELSE                                                      COMBR567
                dij = 0.                                                COMBR568
              ENDIF                                                     COMBR569
              IF ( amhig(jhig) .GT.                                     COMBR570
     .              ABS(amneut(i))+ABS(amneut(j)) ) THEN                COMBR571
                wneut(i,j,jhig) =                                       COMBR572
     .              gweak2(jhig)/(8.*pi*amhig(jhig)**3*(1.+dij))        COMBR573
     .            * aa(jhig,i,j)**2                                     COMBR574
     .            * (amhig(jhig)**2-(amneut(i)+eta*amneut(j))**2)       COMBR575
     .            * SQRT((amneut(i)**2+amneut(j)**2-amhig(jhig)**2)**2  COMBR576
     .                -4.*amneut(i)**2*amneut(j)**2)                    COMBR577
              ELSE                                                      COMBR578
                wneut(i,j,jhig) = 0.                                    COMBR579
              ENDIF                                                     COMBR580
              branch(15,jhig) = branch(15,jhig) + wneut(i,j,jhig)       COMBR581
            ENDDO                                                       COMBR582
          ENDDO                                                         COMBR583
C                                                                       COMBR584
          IF ( branch(15,jhig) .GT. 0. ) THEN                           COMBR585
            DO i = 1, 4                                                 COMBR586
              DO j = 1, 4                                               COMBR587
                wneut(i,j,jhig) = wneut(i,j,jhig) / branch(15,jhig)     COMBR588
              ENDDO                                                     COMBR589
            ENDDO                                                       COMBR590
          ENDIF                                                         COMBR591
C                                                                       COMBR592
C h,H,A --> chi+chi-                                                    COMBR593
C                                                                       COMBR594
          DO i = 1, 2                                                   COMBR595
            DO j = 1, 2                                                 COMBR596
              IF ( amhig(jhig) .GT.                                     COMBR597
     .              ABS(amchar(i))+ABS(amchar(j)) ) THEN                COMBR598
                wchar(i,j,jhig) =                                       COMBR599
     .              gweak2(jhig)/(16.*pi*amhig(jhig)**3)                COMBR600
     .            * ( (bb(jhig,i,j)**2+bb(jhig,j,i)**2)                 COMBR601
     .               *(amhig(jhig)**2-amchar(i)**2-amchar(j)**2)        COMBR602
     .               -4.*bb(jhig,i,j)*bb(jhig,j,i)                      COMBR603
     .               *eta*amchar(i)*amchar(j) )                         COMBR604
     .            * SQRT((amchar(i)**2+amchar(j)**2-amhig(jhig)**2)**2  COMBR605
     .                -4.*amchar(i)**2*amchar(j)**2)                    COMBR606
              ELSE                                                      COMBR607
                wchar(i,j,jhig) = 0.                                    COMBR608
              ENDIF                                                     COMBR609
              branch(16,jhig) = branch(16,jhig) + wchar(i,j,jhig)       COMBR610
            ENDDO                                                       COMBR611
          ENDDO                                                         COMBR612
C                                                                       COMBR613
          IF ( branch(16,jhig) .GT. 0. ) THEN                           COMBR614
            DO i = 1, 2                                                 COMBR615
              DO j = 1, 2                                               COMBR616
                wchar(i,j,jhig) = wchar(i,j,jhig) / branch(16,jhig)     COMBR617
              ENDDO                                                     COMBR618
            ENDDO                                                       COMBR619
          ENDIF                                                         COMBR620
C                                                                       COMBR621
        ENDIF                                                           COMBR622
C                                                                       COMBR623
    1 CONTINUE                                                          COMBR624
C                                                                       COMBR625
      IF ( ism .EQ. 1 ) GOTO 2                                          COMBR626
C                                                                       COMBR627
C  H --> A A                                                            COMBR628
C                                                                       COMBR629
      IF ( gmh .GT. 2.*ama ) branch(9,1) =                              COMBR630
     .     gweak2(1)*amz**2*SQRT(1.-4.*ama**2/gmh**2)                   COMBR631
     .              * (c2b*cab)**2                                      COMBR632
     .              / (128.*pi*gmh*cw2)                                 COMBR633
C                                                                       COMBR634
C  h --> A A                                                            COMBR635
C                                                                       COMBR636
      IF ( amh .GT. 2.*ama ) branch(9,2) =                              COMBR637
     .     gweak2(2)*amz**2*SQRT(1.-4.*ama**2/amh**2)                   COMBR638
C    .              * (c2b*sab)**2                                      COMBR639
C Add radiative corrections (Zwirner et al, CERN-TH 6151/91)            COMBR640
     .              * (c2b*sab                                          COMBR641
     .              + 3.*gweak2(2)*cw2*ca*cb**2*amt**4                  COMBR642
     .              / (8.*pi**2*sb**3*amw**4)                           COMBR643
     .              * ALOG(1.+amsq**2/amt**2) ) **2                     COMBR644
C                                                                       COMBR645
     .              / (128.*pi*amh*cw2)                                 COMBR646
C                                                                       COMBR647
C  A --> Z h                                                            COMBR648
C                                                                       COMBR649
      x = ada(ama,amz,amh)                                              COMBR650
      IF ( ama .GT. amh+amz .AND. x .GT. 0. )                           COMBR651
     .  branch(9,3) = gweak2(3)*SQRT(x)*cab2                            COMBR652
     .              / (64.*pi*ama**3*cw2)                               COMBR653
     .              * (amz**2-2.*(ama**2+amh**2)                        COMBR654
     .              + (ama**2-amh**2)**2/amz**2)                        COMBR655
C                                                                       COMBR656
C  H --> h h                                                            COMBR657
C                                                                       COMBR658
      IF ( gmh .GT. 2.*amh ) branch(10,1) =                             COMBR659
     .     gweak2(1)*amz**2*SQRT(1.-4.*amh**2/gmh**2)                   COMBR660
     .              * (c2a*cab-2.*s2a*sab)**2                           COMBR661
     .              / (128.*pi*gmh*cw2)                                 COMBR662
C                                                                       COMBR663
    2 CONTINUE                                                          COMBR664
      CALL vzero(tauh(1),nhig)                                          COMBR665
      DO 3 jhig = 1, nhig                                               COMBR666
        DO jchan = 1, nchan                                             COMBR667
          width(jhig) = width(jhig) + branch(jchan,jhig)                COMBR668
          IF ( ichan(jchan,jhig) .EQ. 1 )                               COMBR669
     .    parwth(jhig) = parwth(jhig) + branch(jchan,jhig)              COMBR670
        ENDDO                                                           COMBR671
C                                                                       COMBR672
        IF ( width(jhig) .EQ. 0. ) THEN                                 COMBR673
          IF ( idbg .GE. 0 ) WRITE(6,2001) higgs(jhig),amhig(jhig)      COMBR674
          GOTO 3                                                        COMBR675
        ELSE                                                            COMBR676
          tauh(jhig) = hbar/width(jhig)                                 COMBR677
          IF ( idbg .GE. 0 ) WRITE(6,1001)                              COMBR678
     .    higgs(jhig),amhig(jhig),width(jhig),parwth(jhig)              COMBR679
        ENDIF                                                           COMBR680
C                                                                       COMBR681
        btot = 0.                                                       COMBR682
        DO jchan = 1, nchan                                             COMBR683
          star = ' '                                                    COMBR684
          if (ichan(jchan,jhig) .eq. 1) star = '*'                      COMBR685
          IF ( width(jhig) .GT. 0. )                                    COMBR686
     .    branch(jchan,jhig) = branch(jchan,jhig)/width(jhig)*100.      COMBR687
          btot = btot + branch(jchan,jhig)                              COMBR688
          IF ( idbg .GE. 0 .AND. branch(jchan,jhig) .GT. 0.)            COMBR689
     .    WRITE(6,1002) channel(jchan,jhig),branch(jchan,jhig),star     COMBR690
          IF ( ichan(jchan,jhig) .EQ. 1 .AND.                           COMBR691
     .         branch(jchan,jhig) .GT. 0. ) THEN                        COMBR692
            branch(jchan,jhig) = branch(jchan,jhig)*width(jhig)/        COMBR693
     .                         parwth(jhig)/100.                        COMBR694
          ELSE                                                          COMBR695
            branch(jchan,jhig) = 0.                                     COMBR696
          ENDIF                                                         COMBR697
        ENDDO                                                           COMBR698
C                                                                       COMBR699
        IF ( branch(15,jhig) .GT. 0. ) THEN                             COMBR700
          IF ( idbg .GE. 0 ) WRITE(6,1005)                              COMBR701
          DO ineut = 1, 4                                               COMBR702
            DO jneut = ineut,4                                          COMBR703
              IF ( ineut .EQ. jneut ) THEN                              COMBR704
                br = wneut(ineut,jneut,jhig)                            COMBR705
              ELSE                                                      COMBR706
                br = wneut(ineut,jneut,jhig)+wneut(jneut,ineut,jhig)    COMBR707
              ENDIF                                                     COMBR708
              br = br*branch(15,jhig)*parwth(jhig)/width(jhig)*100.     COMBR709
              IF ( br .GT. 0. .AND. idbg .GE. 0 ) THEN                  COMBR710
                IF ( jhig.EQ. 1 ) WRITE(6,1006) ineut,jneut,br          COMBR711
                IF ( jhig.EQ. 2 ) WRITE(6,1016) ineut,jneut,br          COMBR712
                IF ( jhig.EQ. 3 ) WRITE(6,1026) ineut,jneut,br          COMBR713
              ENDIF                                                     COMBR714
            ENDDO                                                       COMBR715
          ENDDO                                                         COMBR716
        ENDIF                                                           COMBR717
C                                                                       COMBR718
        IF ( branch(16,jhig) .GT. 0. ) THEN                             COMBR719
          IF ( idbg .GE. 0 ) WRITE(6,1007)                              COMBR720
          DO ichar = 1, 2                                               COMBR721
            DO jchar = ichar,2                                          COMBR722
              IF ( ichar .EQ. jchar ) THEN                              COMBR723
                br = wchar(ichar,jchar,jhig)                            COMBR724
              ELSE                                                      COMBR725
                br = wchar(ichar,jchar,jhig)+wchar(jchar,ichar,jhig)    COMBR726
              ENDIF                                                     COMBR727
              br = br*branch(16,jhig)*parwth(jhig)/width(jhig)*100.     COMBR728
              IF ( br .GT. 0. .AND. idbg .GE. 0 ) THEN                  COMBR729
                IF ( jhig .EQ. 1 ) WRITE(6,1008) ichar,jchar,br         COMBR730
                IF ( jhig .EQ. 2 ) WRITE(6,1018) ichar,jchar,br         COMBR731
                IF ( jhig .EQ. 3 ) WRITE(6,1028) ichar,jchar,br         COMBR732
              ENDIF                                                     COMBR733
            ENDDO                                                       COMBR734
          ENDDO                                                         COMBR735
        ENDIF                                                           COMBR736
    3 CONTINUE                                                          COMBR737
      IF ( idbg .GE. 0 ) WRITE(6,1004)                                  COMBR738
C                                                                       COMBR739
C Fill internal width table                                             COMBR740
C                                                                       COMBR741
      CALL vzero(xywid(1,1,1),2*nchan*nhig)                             COMBR742
      DO jhig = 1 , nhig                                                COMBR743
        xywid(1,1,jhig) = 0.                                            COMBR744
        xywid(1,2,jhig) = 0.                                            COMBR745
        xywid(1,3,jhig) = 0.                                            COMBR746
        xywid(1,4,jhig) = 0.                                            COMBR747
        xywid(1,5,jhig) = 0.                                            COMBR748
        xywid(1,6,jhig) = 0.                                            COMBR749
        xywid(1,7,jhig) = pmas(24,2)                                    COMBR750
        xywid(1,8,jhig) = pmas(23,2)                                    COMBR751
        xywid(1,9,jhig) = width(3)                                      COMBR752
        xywid(1,10,jhig) = width(2)                                     COMBR753
        xywid(1,11,jhig) = 0.                                           COMBR754
        xywid(1,12,jhig) = 0.                                           COMBR755
        xywid(1,13,jhig) = 0.                                           COMBR756
        xywid(1,14,jhig) = 0.                                           COMBR757
        xywid(1,15,jhig) = 0.                                           COMBR758
        DO jchan = 1 , nchan                                            COMBR759
          xywid(2,jchan,jhig) = xywid(1,jchan,jhig)                     COMBR760
        ENDDO                                                           COMBR761
        xywid(2,11,jhig) = pmas(23,2)                                   COMBR762
      ENDDO                                                             COMBR763
C                                                                       COMBR764
      xywid(1,9,3) = width(2)                                           COMBR765
      xywid(2,9,3) = pmas(23,2)                                         COMBR766
C                                                                       COMBR767
      pmas(25,2) = width(2)                                             COMBR768
      pmas(35,2) = width(1)                                             COMBR769
      pmas(36,2) = width(3)                                             COMBR770
C                                                                       COMBR771
      pmas(25,3) = AMIN1(pmas(25,1),10.*width(2))                       COMBR772
      pmas(35,3) = AMIN1(pmas(35,1),10.*width(1))                       COMBR773
      pmas(36,3) = AMIN1(pmas(36,1),10.*width(3))                       COMBR774
C                                                                       COMBR775
  999 RETURN                                                            COMBR776
C-----------------------------------------------------------------------COMBR777
 1000 FORMAT(/' With Lambda_QCD(5) = ',F8.5,' GeV,'/                    COMBR778
     .        '      alpha_s(mZ)   = ',F8.5,' ... '/)                   COMBR779
 1001 FORMAT(/50('-')//                                                 COMBR780
     .       1x,'The following branching ratios have been computed :'/  COMBR781
     .       1X,'   ',A1,' mass                 : ',F8.3,' GeV/c**2'/   COMBR782
     .       1X,'    Total decay width     : ',F8.3,' GeV'/             COMBR783
     .       1X,'    Width to be generated : ',F8.3,' GeV'//)           COMBR784
 2001 FORMAT(/50('-')/                                                  COMBR785
     .       1X,'   ',A1,' (mass : ',F8.3,' GeV/c**2)'/                 COMBR786
     .       1x,'    won''t be decayed by HHDECAY !'//)                 COMBR787
 1002 FORMAT(1X,'Channel ',A14,' --- BR = ',F10.5,' % (',A1,')')        COMBR788
 1003 FORMAT(1x,'The branching fraction into the lightest ',            COMBR789
     .          'neutralino pair is ',F10.5,'%'/)                       COMBR790
 1004 FORMAT(/1X,'   (*) = channel requested'/)                         COMBR791
 1005 FORMAT(/8x,' Detail of the neutralino BRs : ')                    COMBR792
 1006 FORMAT(10x,' o H --> chi',I1,'0 chi',I1,'0 ',20('.'),1x,F9.5,'%') COMBR793
 1016 FORMAT(10x,' o h --> chi',I1,'0 chi',I1,'0 ',20('.'),1x,F9.5,'%') COMBR794
 1026 FORMAT(10x,' o A --> chi',I1,'0 chi',I1,'0 ',20('.'),1x,F9.5,'%') COMBR795
 1007 FORMAT(/8x,' Detail of the chargino BRs : ')                      COMBR796
 1008 FORMAT(10x,' o H --> chi',I1,'+ chi',I1,'- ',20('.'),1x,F9.5,'%') COMBR797
 1018 FORMAT(10x,' o h --> chi',I1,'+ chi',I1,'- ',20('.'),1x,F9.5,'%') COMBR798
 1028 FORMAT(10x,' o A --> chi',I1,'+ chi',I1,'- ',20('.'),1x,F9.5,'%') COMBR799
      END                                                               COMBR800
      FUNCTION CROCOM(ipro, sbeam)                                      CROCOM 2
C---------------------------------------------------------------------- CROCOM 3
C! Cross sections for e+e- --> hZ, HZ, hA, HA, hnn, Hnn, hee, Hee       CROCOM 4
C                                                                       CROCOM 5
C  Input :        ipro,   process Id                                    CROCOM 6
C                 sbeam,  is ecm**2                                     CROCOM 7
C                                                                       CROCOM 8
C  Output:        crocom, the cross-section value                       CROCOM 9
C                                                                       CROCOM10
C  Patrick Janot -- 31 oct 1992                                         CROCOM11
C---------------------------------------------------------------------- CROCOM12
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
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),               CONSTS 2
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw      CONSTS 3
C                                                                       CONSTS 4
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)        DATAPI 2
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          DATAPI 3
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         DATAPI 4
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         DATAPI 5
C                                                                       DATAPI 6
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,      MISCL  2
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),            MISCL  3
     &                 nevent(11)                                       MISCL  4
      INTEGER loutbe,idb1,idb2,nevent                                   MISCL  5
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad                      MISCL  6
C                                                                       MISCL  7
      CHARACTER*14 channel                                              HHDECK 2
      CHARACTER*21 channeut, chanchar                                   HHDECK 3
      PARAMETER(nchneut=8,nchchar=5)                                    HHDECK 4
      COMMON / hhdeck / branch(nchan,nhig),width(nhig),                 HHDECK 5
     .                  parwth(nhig),xymas(2,nchan,nhig),               HHDECK 6
     .                  xywid(2,nchan,nhig)                             HHDECK 7
      COMMON / chaneu / ichn(2),                                        HHDECK 8
     .                  wneut(4,4,nhig), wchar(2,2,nhig),               HHDECK 9
     .                  widneut(4), brneut(nchneut,4),                  HHDECK10
     .                  widchar(2), brchar(nchchar,2)                   HHDECK11
      COMMON / chanch / channeut(nchneut,4), chanchar(nchchar,2)        HHDECK12
      COMMON / vect4 / pvect4(5,2)                                      HHDECK13
      COMMON / hinput / ichan(nchan,nhig), channel(nchan,nhig)          HHDECK14
      COMMON / hisbr / bchpp,bchgg                                      HHDECK15
      DIMENSION ph(4)                                                   HHDECK16
C                                                                       HHDECK17
      REAL*4 kappa                                                      CROCOM19
      REAL*8 brwipj, brwisi                                             CROCOM20
      EXTERNAL brwipj, brwisi                                           CROCOM21
      LOGICAL first                                                     CROCOM22
      DATA first /.TRUE./                                               CROCOM23
      ada(am1,am2,am3) = (am1**2-am2**2-am3**2)**2-4.*am2**2*am3**2     CROCOM24
C                                                                       CROCOM25
C  Calcul des sections efficaces de production (hZ,HZ,hA,HA,WWh,WWH)    CROCOM26
C                                                                       CROCOM27
      ebeam = SQRT(sbeam)/2.                                            CROCOM28
C                                                                       CROCOM29
      brwig = (sbeam-amz**2)**2 + amz**2*gmz**2                         CROCOM30
      nevt  = 250                                                       CROCOM31
      crocom = 0.                                                       CROCOM32
C                                                                       CROCOM33
      IF ( ipro .EQ. 1 ) THEN                                           CROCOM34
C                                                                       CROCOM35
        IF ( iklei.EQ.0 .AND. amh+amz.GT.2.*ebeam-10.*gmz ) THEN        CROCOM36
          DO 1 ievt = 1, nevt                                           CROCOM37
            bmin = 20.                                                  CROCOM38
            bmax = 2.*ebeam                                             CROCOM39
            CALL bwgene(bmin,bmax,amz,gmz,bmz,djdum)                    CROCOM40
            almbda = ada(2.*ebeam,amh,bmz)                              CROCOM41
            IF ( amh+bmz .GT. 2.*ebeam .OR. almbda .LE. 0. ) GOTO 1     CROCOM42
            weight = sqrt(almbda)*(almbda+12.*sbeam*amz**2)             CROCOM43
            sbj =                                                       CROCOM44
     .      pi * alpha2 * weight * (1.+(1.-4.*sw2)**2)                  CROCOM45
     .      / (192.*sbeam**2*sw2**2*cw2**2*brwig)                       CROCOM46
            crocom = crocom + sab2*sbj                                  CROCOM47
    1     CONTINUE                                                      CROCOM48
          crocom = crocom/FLOAT(nevt)                                   CROCOM49
        ELSEIF ( iklei .EQ. 0 ) THEN                                    CROCOM50
          almbda = ada(2.*ebeam,amh,amz)                                CROCOM51
          weight = sqrt(almbda)*(almbda+12.*sbeam*amz**2)               CROCOM52
          sbj =                                                         CROCOM53
     .    pi * alpha2 * weight * (1.+(1.-4.*sw2)**2)                    CROCOM54
     .    / (192.*sbeam**2*sw2**2*cw2**2*brwig)                         CROCOM55
          crocom = sab2*sbj                                             CROCOM56
        ELSE                                                            CROCOM57
          sbj = brwipj(sbeam,amh,width(2))                              CROCOM58
          crocom = sab2 * alpha2 * sbj                                  CROCOM59
        ENDIF                                                           CROCOM60
C                                                                       CROCOM61
      ELSEIF ( ipro .EQ. 2 .AND. ism .EQ. 0 ) THEN                      CROCOM62
C                                                                       CROCOM63
        IF ( iklei.EQ.0 .AND. gmh+amz .GT. 2.*ebeam-10.*gmz ) THEN      CROCOM64
          DO 2 ievt = 1, nevt                                           CROCOM65
            bmin = 20.                                                  CROCOM66
            bmax = 2.*ebeam                                             CROCOM67
            CALL bwgene(bmin,bmax,amz,gmz,bmz,djdum)                    CROCOM68
            almbda = ada(2.*ebeam,gmh,bmz)                              CROCOM69
            IF ( gmh+bmz .GT. 2.*ebeam .OR. almbda .LE. 0. ) GOTO 2     CROCOM70
            weight = sqrt(almbda)*(almbda+12.*sbeam*amz**2)             CROCOM71
            sbj =                                                       CROCOM72
     .      pi * alpha2 * weight * (1.+(1.-4.*sw2)**2)                  CROCOM73
     .      / (192.*sbeam**2*sw2**2*cw2**2*brwig)                       CROCOM74
            crocom = crocom + cab2*sbj                                  CROCOM75
    2     CONTINUE                                                      CROCOM76
          crocom = crocom/FLOAT(nevt)                                   CROCOM77
        ELSEIF ( iklei .EQ. 0 ) THEN                                    CROCOM78
          almbda = ada(2.*ebeam,gmh,amz)                                CROCOM79
          weight = sqrt(almbda)*(almbda+12.*sbeam*amz**2)               CROCOM80
          sbj =                                                         CROCOM81
     .    pi * alpha2 * weight * (1.+(1.-4.*sw2)**2)                    CROCOM82
     .    / (192.*sbeam**2*sw2**2*cw2**2*brwig)                         CROCOM83
          crocom = cab2*sbj                                             CROCOM84
        ELSE                                                            CROCOM85
          sbj = brwipj(sbeam,gmh,width(1))                              CROCOM86
          crocom = cab2 * alpha2 * sbj                                  CROCOM87
        ENDIF                                                           CROCOM88
C                                                                       CROCOM89
      ELSEIF ( ipro .EQ. 3 .AND. ism .EQ. 0 ) THEN                      CROCOM90
C                                                                       CROCOM91
        kappa = brwisi(sbeam,amh,ama,width(2),width(3)) / 8.            CROCOM92
        shA = pi*alpha2*kappa * (8.*sw2**2-4.*sw2+1.) * sbeam           CROCOM93
     .      / (12.*cw2**2*sw2**2*brwig)                                 CROCOM94
        crocom = cab2*shA                                               CROCOM95
C                                                                       CROCOM96
      ELSEIF ( ipro .EQ. 4 .AND. ism .EQ. 0 ) THEN                      CROCOM97
C                                                                       CROCOM98
        kappa = brwisi(sbeam,gmh,ama,width(1),width(3)) / 8.            CROCOM99
        sHA = pi*alpha2*kappa * (8.*sw2**2-4.*sw2+1.) * sbeam           CROCO100
     .      / (12.*cw2**2*sw2**2*brwig)                                 CROCO101
        crocom = sab2*sHA                                               CROCO102
C                                                                       CROCO103
      ELSEIF ( ipro .EQ. 5 .OR. ipro .EQ. 7 ) THEN                      CROCO104
C                                                                       CROCO105
        crocom = sab2 * sigmawwh(sbeam,ipro)                            CROCO106
C                                                                       CROCO107
      ELSEIF ( (ipro.EQ.6.OR.ipro.EQ.8) .AND. ism .EQ. 0 ) THEN         CROCO108
C                                                                       CROCO109
        crocom = cab2 * sigmawwh(sbeam,ipro)                            CROCO110
C                                                                       CROCO111
      ELSE                                                              CROCO112
      ENDIF                                                             CROCO113
C                                                                       CROCO114
  999 RETURN                                                            CROCO115
      END                                                               CROCO116
      SUBROUTINE decchi(ch,ichi1,ichi,ich)                              DECCHI 2
C----------------------------------------------------------------       DECCHI 3
C! Decay a chargino or a neutralino into two bodies                     DECCHI 4
C  ( chi' --> chi gamma, chi Z(*), chi+/- W(*)-/+)                      DECCHI 5
C  ( chi+ --> chi W+(*))                                                DECCHI 6
C  Cascades are allowed by consecutive calls to DECCHI                  DECCHI 7
C                                                                       DECCHI 8
C Input:   o ch(1-4) = the quadri-momentum of the chi                   DECCHI 9
C          o ichi1   = neutralino/chargino index                        DECCHI10
C          o ichi    = its charge                                       DECCHI11
C          o ich     = the position in the LUJET common block           DECCHI12
C                                                                       DECCHI13
C Output:  o p2(1-4) = the LSP quadri-momentum                          DECCHI14
C          o p3(1-4) = the virtual boson quadri-momentum                DECCHI15
C                                                                       DECCHI16
C Patrick Janot -- 31 Aug 1994                                          DECCHI17
C----------------------------------------------------------------       DECCHI18
      DIMENSION ch(4)                                                   DECCHI19
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      CHARACTER*14 channel                                              HHDECK 2
      CHARACTER*21 channeut, chanchar                                   HHDECK 3
      PARAMETER(nchneut=8,nchchar=5)                                    HHDECK 4
      COMMON / hhdeck / branch(nchan,nhig),width(nhig),                 HHDECK 5
     .                  parwth(nhig),xymas(2,nchan,nhig),               HHDECK 6
     .                  xywid(2,nchan,nhig)                             HHDECK 7
      COMMON / chaneu / ichn(2),                                        HHDECK 8
     .                  wneut(4,4,nhig), wchar(2,2,nhig),               HHDECK 9
     .                  widneut(4), brneut(nchneut,4),                  HHDECK10
     .                  widchar(2), brchar(nchchar,2)                   HHDECK11
      COMMON / chanch / channeut(nchneut,4), chanchar(nchchar,2)        HHDECK12
      COMMON / vect4 / pvect4(5,2)                                      HHDECK13
      COMMON / hinput / ichan(nchan,nhig), channel(nchan,nhig)          HHDECK14
      COMMON / hisbr / bchpp,bchgg                                      HHDECK15
      DIMENSION ph(4)                                                   HHDECK16
C                                                                       HHDECK17
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)        DATAPI 2
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          DATAPI 3
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         DATAPI 4
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         DATAPI 5
C                                                                       DATAPI 6
C                                                                       DECCHI23
      IF ( ichi.EQ.0 .AND. (ichi1.LE.0.OR.ichi.GT.4) ) THEN             DECCHI24
        WRITE(6,*) '+++ DECCHI +++ Wrong Neutralino #',ichi1            DECCHI25
        STOP 99                                                         DECCHI26
      ENDIF                                                             DECCHI27
C                                                                       DECCHI28
      IF ( ichi.EQ.1 .AND. (ichi1.LE.0.OR.ichi.GT.2) ) THEN             DECCHI29
        WRITE(6,*) '+++ DECCHI +++ Wrong Chargino #',ichi1              DECCHI30
        STOP 99                                                         DECCHI31
      ENDIF                                                             DECCHI32
C                                                                       DECCHI33
      IF ( ichi .EQ. 0 .AND. widneut(ichi1) .EQ. 0.) THEN               DECCHI34
        WRITE(6,*) '+++ DECCHI +++ No accesible channels for ',         DECCHI35
     .             'neutralino # ',ichi1                                DECCHI36
        STOP 99                                                         DECCHI37
      ENDIF                                                             DECCHI38
C                                                                       DECCHI39
      IF ( ichi .EQ. 1 .AND. widchar(ichi1) .EQ. 0.) THEN               DECCHI40
        WRITE(6,*) '+++ DECCHI +++ No accesible channels for ',         DECCHI41
     .             'chargino # ',ichi1                                  DECCHI42
        STOP 99                                                         DECCHI43
      ENDIF                                                             DECCHI44
C                                                                       DECCHI45
      IF ( ichi .EQ. 0 ) THEN                                           DECCHI46
        nchn = nchneut                                                  DECCHI47
      ELSE                                                              DECCHI48
        nchn = nchchar                                                  DECCHI49
      ENDIF                                                             DECCHI50
C                                                                       DECCHI51
C  Choice of the decay channel.                                         DECCHI52
C                                                                       DECCHI53
      rnch = RNDM(dummy)                                                DECCHI54
      rint = 0.D0                                                       DECCHI55
      DO jc = 1 , nchn                                                  DECCHI56
        IF ( ichi .EQ. 0 ) THEN                                         DECCHI57
          brch = brneut(jc,ichi1)                                       DECCHI58
        ELSE                                                            DECCHI59
          brch = brchar(jc,ichi1)                                       DECCHI60
        ENDIF                                                           DECCHI61
        rint = rint + brch                                              DECCHI62
        if ( rnch .lt. rint ) GOTO 30                                   DECCHI63
      ENDDO                                                             DECCHI64
  30  CONTINUE                                                          DECCHI65
C                                                                       DECCHI66
C Store the indices of the final state:                                 DECCHI67
C       o ichi2 = the neutral/charg-ino index                           DECCHI68
C       o jchi  = 0 for a neutralino, 1 for a chargino                  DECCHI69
C       o ifn   = 0 for a photon, 1 for a Z and 2 for a W               DECCHI70
C                                                                       DECCHI71
      IF ( ichi .EQ. 0 ) THEN                                           DECCHI72
        ichi2 = jc-(jc-1)/3*3                                           DECCHI73
        jchi  = (jc-1)/6                                                DECCHI74
        ifn   = (jc-1)/3                                                DECCHI75
        IF ( RNDM(jchi) .LT. 0.5 ) jchi = -jchi                         DECCHI76
      ELSE                                                              DECCHI77
        ichi2 = jc-(jc-1)/4*4                                           DECCHI78
        jchi  = (jc-1)/4 * ichi                                         DECCHI79
        ifn   = 2-jc/4                                                  DECCHI80
      ENDIF                                                             DECCHI81
C                                                                       DECCHI82
      IF ( idbg .GE. 10 ) THEN                                          DECCHI83
        WRITE(6,*) ' +++ DECCHI +++ '                                   DECCHI84
        WRITE(6,*) 'ichi1,ichi : ',ichi1,ichi                           DECCHI85
        WRITE(6,*) 'ichi2,jchi : ',ichi2,jchi                           DECCHI86
        WRITE(6,*) 'ifn       : ',ifn                                   DECCHI87
      ENDIF                                                             DECCHI88
C                                                                       DECCHI89
C Build the relevant quadri-impulsions                                  DECCHI90
C                                                                       DECCHI91
      CALL chideca(ch,ichi1,ichi,ichi2,jchi,ifn)                        DECCHI92
C                                                                       DECCHI93
C Fill the common LUJET accordingly                                     DECCHI94
C                                                                       DECCHI95
      CALL filujt(ich,ichi2,jchi,ifn)                                   DECCHI96
C                                                                       DECCHI97
  999 RETURN                                                            DECCHI98
      END                                                               DECCHI99
      SUBROUTINE dilokp(xreal,ximag,f,ff)                               DILOKP 2
C------------------------------------------------------------------     DILOKP 3
C! Function used in the determination of chi' --> chi gamma             DILOKP 4
C                                                                       DILOKP 5
C V. Bertin for CHA001                                                  DILOKP 6
C------------------------------------------------------------------     DILOKP 7
      REAL*8 xreal,ximag,f,b2,ff,pi                                     DILOKP 8
      COMPLEX*16 f1,f2,zwei                                             DILOKP 9
C                                                                       DILOKP10
      pi = 3.1415926535897932                                           DILOKP11
C--berechnet (f(1/z)+f(1/zquer))*.5 --->F...(z=(xreal,ximag))           DILOKP12
      b2 = xreal*xreal+ximag*ximag                                      DILOKP13
      xreal = xreal/b2                                                  DILOKP14
      ximag = -ximag/b2                                                 DILOKP15
C--z=1/z                                                                DILOKP16
      b2 = 1./b2                                                        DILOKP17
Cvb   PRINT *,'real imag b2 :',xreal,ximag,b2                           DILOKP18
      CALL fvonz(xreal,ximag,f1)                                        DILOKP19
C--f1=f(1/z)                                                            DILOKP20
      ximag = -ximag                                                    DILOKP21
      CALL fvonz(xreal,ximag,f2)                                        DILOKP22
C--f2=f(1/zquer)                                                        DILOKP23
C     WRITE(6,*)'Imaginaerteil von (f1+f2) muesste 0 sein'              DILOKP24
      f = DREAL((f1+f2)/2.d0)                                           DILOKP25
      ff = f                                                            DILOKP26
      zwei = DCMPLX(2.d0,0.d0)                                          DILOKP27
C     WRITE(6,*)'Im((f1+f2)/2)',dimag((f1+f2)/zwei)                     DILOKP28
      RETURN                                                            DILOKP29
      END                                                               DILOKP30
      SUBROUTINE dsigklei(ipro,s,qh,qq,qp,qm,ifs)                       DSIGKLE2
C---------------------------------------------------------------        DSIGKLE3
C! Routine for generating final states in the reaction                  DSIGKLE4
C                                                                       DSIGKLE5
C       e+ e-  ---> mu+ mu- H                                           DSIGKLE6
C---------------------------------------------------------------        DSIGKLE7
      DIMENSION qh(4),qp(4),qm(4),qq(4),rp(4)                           DSIGKLE8
      REAL*8 a,b,x,xp,x1,x2,a1,a2,xap,fap,xmh,eb                        DSIGKLE9
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
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
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),               CONSTS 2
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw      CONSTS 3
C                                                                       CONSTS 4
      DIMENSION brai(11), kcode(11), xmasmi(11)                         ZZDECK 2
      DATA brai/.0335,.0665,.0335,.0665,.0335,.0665,                    ZZDECK 3
     .          .1540,.1190,.1540,.1190,.1540/                          ZZDECK 4
      DATA kcode/11,12,13,14,15,16,1,2,3,4,5/                           ZZDECK 5
      DATA xmasmi/6*0.,0.3,0.3,1.0,4.0,11.0/                            ZZDECK 6
      COMMON / zzdec / braz(11), kselec(11), fracz                      ZZDECK 7
C                                                                       ZZDECK 8
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,      MISCL  2
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),            MISCL  3
     &                 nevent(11)                                       MISCL  4
      INTEGER loutbe,idb1,idb2,nevent                                   MISCL  5
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad                      MISCL  6
C                                                                       MISCL  7
      LOGICAL first                                                     DSIGKL15
      DATA first /.TRUE./                                               DSIGKL16
      DATA empir0/0./                                                   DSIGKL17
C                                                                       DSIGKL18
C Derived constants                                                     DSIGKL19
C                                                                       DSIGKL20
      IF ( first ) THEN                                                 DSIGKL21
        g     = 1./SQRT(sw2*(1.-sw2))                                   DSIGKL22
        tpi   = 8.*ATAN(1.)                                             DSIGKL23
        cv2   = (g/4.*(4.*sw2-1.))**2                                   DSIGKL24
        ca2   = (-g/4.)**2                                              DSIGKL25
        c1    = (cv2+ca2)**2 + 4.*cv2*ca2                               DSIGKL26
        c2    = (cv2+ca2)**2 - 4.*cv2*ca2                               DSIGKL27
        cx    = AMAX1(c1,c2)                                            DSIGKL28
        first = .FALSE.                                                 DSIGKL29
      ENDIF                                                             DSIGKL30
C                                                                       DSIGKL31
    1 ee = SQRT(s)                                                      DSIGKL32
      IF ( ipro .EQ. 1 ) THEN                                           DSIGKL33
        CALL bwgene(0.,ee,pmas(25,1),pmas(25,2),ymh,djdum)              DSIGKL34
        xmh = ymh                                                       DSIGKL35
      ELSE                                                              DSIGKL36
        CALL bwgene(0.,ee,pmas(35,1),pmas(35,2),ymh,djdum)              DSIGKL37
        xmh = ymh                                                       DSIGKL38
      ENDIF                                                             DSIGKL39
      eb   = SQRT(s)/2.                                                 DSIGKL40
      a    = 4.*xmh**2/s                                                DSIGKL41
      b    = amz*gmz/s                                                  DSIGKL42
      xp   = 1. + (xmh**2-amz**2)/s                                     DSIGKL43
      x1   = SQRT(a)                                                    DSIGKL44
      x2   = 1.+a/4.                                                    DSIGKL45
      a1   = ATAN((x1-xp)/b)                                            DSIGKL46
      a2   = ATAN((x2-xp)/b)                                            DSIGKL47
      xap  = DMIN1( DMAX1(x1+b,xp) , x2 )                               DSIGKL48
      fap  = ((12.+2.*a)-12.*xap+xap**2)*SQRT(xap**2-a)                 DSIGKL49
C                                                                       DSIGKL50
C Generation of the Z decay                                             DSIGKL51
C                                                                       DSIGKL52
      choix = RNDM(choix)                                               DSIGKL53
      DO ifs = 1,11                                                     DSIGKL54
       IF ( choix .LT. braz(ifs) ) GOTO 2                               DSIGKL55
      ENDDO                                                             DSIGKL56
C                                                                       DSIGKL57
C Check if the centre-of-mass energy after ISR is large enough          DSIGKL58
C                                                                       DSIGKL59
    2 CONTINUE                                                          DSIGKL60
      kff    = kcode(ifs)                                               DSIGKL61
      xmdec  = ULMASS(kff)                                              DSIGKL62
      IF ( s .LT. (xmh+2.*xmdec)**2 ) GOTO 1                            DSIGKL63
C                                                                       DSIGKL64
C Generation of x value                                                 DSIGKL65
C                                                                       DSIGKL66
  100 r  = RNDM(dum1)                                                   DSIGKL67
      x  = xp + b * TAN ( r*a1 + (1.-r)*a2 )                            DSIGKL68
      w3 = ((12.+2.*a)-12.*x+x*x) * SQRT(x*x-a) / fap                   DSIGKL69
      empir0= AMAX1(empir0,w3)                                          DSIGKL70
      IF ( w3 .LE. empir ) GOTO 102                                     DSIGKL71
      WRITE(6,101) w3,empir                                             DSIGKL72
  101 FORMAT('DSIGKLEI: Weight W3 =',f6.3,' is larger than',f6.3,';',/, DSIGKL73
     .       '         You have to increase the value of ''EMPIR'' .')  DSIGKL74
      STOP                                                              DSIGKL75
  102 IF ( w3 .LT. (RNDM(dum2)*empir) ) GOTO 100                        DSIGKL76
C                                                                       DSIGKL77
C Generation of higgs solid angle                                       DSIGKL78
C                                                                       DSIGKL79
  200 ch = -1. + 2.*RNDM(dum3)                                          DSIGKL80
      w4 = 1.-ch*ch*(x*x-a)/(8.*(1.-x)+x*x+a)                           DSIGKL81
      IF ( w4 .LT. RNDM(dum4) ) GOTO 200                                DSIGKL82
      fh = tpi*RNDM(dum5)                                               DSIGKL83
C                                                                       DSIGKL84
C Construct Higgs momentum                                              DSIGKL85
C                                                                       DSIGKL86
      IF ( eb*x .LT. xmh ) THEN                                         DSIGKL87
        qh(4) = xmh                                                     DSIGKL88
        qvec = 0.                                                       DSIGKL89
      ELSE                                                              DSIGKL90
        qh(4) = eb*x                                                    DSIGKL91
        qvec  = SQRT ((eb*x)**2-xmh**2)                                 DSIGKL92
      ENDIF                                                             DSIGKL93
      sh    = SQRT (1.-ch**2)                                           DSIGKL94
      qh(1) = qvec * sh * SIN(fh)                                       DSIGKL95
      qh(2) = qvec * sh * COS(fh)                                       DSIGKL96
      qh(3) = qvec * ch                                                 DSIGKL97
C                                                                       DSIGKL98
C Construct sum of muon momenta                                         DSIGKL99
C                                                                       DSIGK100
      DO 300 i=1,4                                                      DSIGK101
  300 qq(i) = -qh(i)                                                    DSIGK102
      qq(4) = 2.*eb + qq(4)                                             DSIGK103
      xmt2 = qq(4)**2-qq(3)**2-qq(2)**2-qq(1)**2                        DSIGK104
      IF ( xmt2 .LE. 0. ) THEN                                          DSIGK105
        xmt2 = 1.1E-6                                                   DSIGK106
        qq(4) = SQRT(qq(3)**2+qq(2)**2+qq(1)**2+xmt2)                   DSIGK107
      ENDIF                                                             DSIGK108
C                                                                       DSIGK109
C Generate mu+ momentum (PEP) and energy (EP) in the mu+mu- CM frame    DSIGK110
C                                                                       DSIGK111
      ep = SQRT (s*(1.-x)+xmh**2)/2.                                    DSIGK112
      pep2= ep*ep-xmdec*xmdec                                           DSIGK113
      pep = -1.                                                         DSIGK114
      IF ( pep2 .GT. 0. ) pep = SQRT(pep2)                              DSIGK115
C                                                                       DSIGK116
C Try again if this configuration kinematically not o.k.                DSIGK117
C                                                                       DSIGK118
      IF ( pep .LE. 0. ) GOTO 100                                       DSIGK119
C                                                                       DSIGK120
C Generate the muon directions in the mu+mu- CM frame                   DSIGK121
C                                                                       DSIGK122
  400 cm    = -1. + 2.*RNDM(dum6)                                       DSIGK123
      sm    = SQRT (1.-cm*cm)                                           DSIGK124
      fm    = tpi*RNDM(dum7)                                            DSIGK125
      rp(1) = pep * sm * SIN(fm)                                        DSIGK126
      rp(2) = pep * sm * COS(fm)                                        DSIGK127
      rp(3) = pep * cm                                                  DSIGK128
      rp(4) = ep                                                        DSIGK129
C                                                                       DSIGK130
C Boost mu+ momenta to lab frame and construct mu- momentum             DSIGK131
C                                                                       DSIGK132
      qp(4)=(qq(4)*rp(4)+qq(1)*rp(1)+qq(2)*rp(2)+qq(3)*rp(3))/(2.*ep)   DSIGK133
      qr   =(rp(4)+qp(4))/(2.*ep+qq(4))                                 DSIGK134
      DO 500 i=1,3                                                      DSIGK135
      qp(i)=rp(i)+qq(i)*qr                                              DSIGK136
  500 qm(i)=qq(i)-qp(i)                                                 DSIGK137
      qm(4)=qq(4)-qp(4)                                                 DSIGK138
C                                                                       DSIGK139
C Calculate weight of muon angular variables                            DSIGK140
C                                                                       DSIGK141
      ppdqp = eb * ( qp(4) - qp(3) )                                    DSIGK142
      pmdqp = eb * ( qp(4) + qp(3) )                                    DSIGK143
      ppdqm = eb * ( qm(4) - qm(3) )                                    DSIGK144
      pmdqm = eb * ( qm(4) + qm(3) )                                    DSIGK145
      w5 = ( c1*ppdqm*pmdqp + c2*ppdqp*pmdqm )                          DSIGK146
     .   / ( cx*(ppdqp+ppdqm)*(pmdqp+pmdqm))                            DSIGK147
      IF ( w5 .LT. RNDM(dum8) ) GOTO 400                                DSIGK148
C                                                                       DSIGK149
C Store the maximum weight                                              DSIGK150
C                                                                       DSIGK151
      empirm = empir0                                                   DSIGK152
C                                                                       DSIGK153
C Et voila.                                                             DSIGK154
C                                                                       DSIGK155
      qh(4) = xmh                                                       DSIGK156
      qq(4) = SQRT(xmt2)                                                DSIGK157
C                                                                       DSIGK158
      RETURN                                                            DSIGK159
      END                                                               DSIGK160
      SUBROUTINE dsigma(ipro,ss,qh,qa,hh,izpol)                         DSIGMA 2
C-----------------------------------------------------------------------DSIGMA 3
C!  Angular distributions for Higgs production                          DSIGMA 4
C                                                                       DSIGMA 5
C   Patrick Janot  --  27 Aug 1991                                      DSIGMA 6
C-----------------------------------------------------------------------DSIGMA 7
      DIMENSION qh(4), qa(4), hh(4), gg(4)                              DSIGMA 8
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
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
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)        DATAPI 2
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          DATAPI 3
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         DATAPI 4
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         DATAPI 5
C                                                                       DATAPI 6
      EXTERNAL sigma1,sigma2,sigma3,sigma4                              SIGMAS 2
      EXTERNAL sigma5,sigma6,sigma7,sigma8                              SIGMAS 3
      EXTERNAL sigmat                                                   SIGMAS 4
C                                                                       SIGMAS 5
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,      MISCL  2
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),            MISCL  3
     &                 nevent(11)                                       MISCL  4
      INTEGER loutbe,idb1,idb2,nevent                                   MISCL  5
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad                      MISCL  6
C                                                                       MISCL  7
      ada(am1,am2,am3) = (am1**2-am2**2-am3**2)**2-4.*am2**2*am3**2     DSIGMA14
C                                                                       DSIGMA15
      ee = SQRT(ss)                                                     DSIGMA16
      izpol = 0                                                         DSIGMA17
C                                                                       DSIGMA18
      IF ( ipro .GE. 5 ) GOTO 1000                                      DSIGMA19
C                                                                       DSIGMA20
C HZ & HA                                                               DSIGMA21
C                                                                       DSIGMA22
C Take into account the widths of the Z and of the Higgses              DSIGMA23
C                                                                       DSIGMA24
      IF ( ipro .EQ. 1 .OR. ipro .EQ. 2 ) THEN                          DSIGMA25
C                                                                       DSIGMA26
        IF ( iklei .EQ. 1 ) THEN                                        DSIGMA27
C                                                                       DSIGMA28
          CALL dsigklei(ipro,ss,qh,qa,hh,gg,izpol)                      DSIGMA29
          GOTO 999                                                      DSIGMA30
C                                                                       DSIGMA31
        ELSE                                                            DSIGMA32
C                                                                       DSIGMA33
          plmbda = ada(ee,0.,0.)                                        DSIGMA34
          wmax   = SQRT(plmbda)*(plmbda+12.*ss*amz**2)                  DSIGMA35
  101     CONTINUE                                                      DSIGMA36
          IF ( ipro .EQ. 1 ) THEN                                       DSIGMA37
            CALL bwgene(0.,ee    ,pmas(25,1),pmas(25,2),am1,djdum)      DSIGMA38
            CALL bwgene(0.,ee-am1,amz       ,gmz       ,am2,djdum)      DSIGMA39
          ELSE                                                          DSIGMA40
            CALL bwgene(0.,ee    ,pmas(35,1),pmas(35,2),am1,djdum)      DSIGMA41
            CALL bwgene(0.,ee-am1,amz       ,gmz       ,am2,djdum)      DSIGMA42
          ENDIF                                                         DSIGMA43
          almbda = ada(ee,am1,am2)                                      DSIGMA44
          IF ( am2 + am1 .GT. ee .OR. almbda .LT. 0. ) GOTO 101         DSIGMA45
          weight = SQRT(almbda)*(almbda+12.*ss*amz**2)                  DSIGMA46
          IF ( weight/wmax .LT. RNDM(weight) ) GOTO 101                 DSIGMA47
C                                                                       DSIGMA48
        ENDIF                                                           DSIGMA49
C                                                                       DSIGMA50
      ELSEIF ( ipro .EQ. 3 .OR. ipro .EQ. 4 ) THEN                      DSIGMA51
C                                                                       DSIGMA52
        wmax   = SQRT(ada(ee,0.,0.))**3                                 DSIGMA53
  102   CONTINUE                                                        DSIGMA54
        IF ( ipro .EQ. 3 ) THEN                                         DSIGMA55
          CALL bwgene(0.,ee    ,pmas(25,1),pmas(25,2),am1,djdum)        DSIGMA56
          CALL bwgene(0.,ee-am1,pmas(36,1),pmas(36,2),am2,djdum)        DSIGMA57
        ELSE                                                            DSIGMA58
          CALL bwgene(0.,ee    ,pmas(35,1),pmas(35,2),am1,djdum)        DSIGMA59
          CALL bwgene(0.,ee-am1,pmas(36,1),pmas(36,2),am2,djdum)        DSIGMA60
        ENDIF                                                           DSIGMA61
        almbda = ada(ee,am1,am2)                                        DSIGMA62
        IF ( am2 + am1 .GT. ee .OR. almbda .LT. 0. ) GOTO 102           DSIGMA63
        weight = SQRT(almbda)**3                                        DSIGMA64
        IF ( weight/wmax .LT. RNDM(weight) ) GOTO 102                   DSIGMA65
C                                                                       DSIGMA66
      ENDIF                                                             DSIGMA67
C                                                                       DSIGMA68
      qh(4) = (ss+am1**2-am2**2) / (2.*ee)                              DSIGMA69
      qa(4) = (ss-am1**2+am2**2) / (2.*ee)                              DSIGMA70
      pmom2 = (ss-(am1+am2)**2)  * (ss-(am1-am2)**2)                    DSIGMA71
      pmom  = SQRT(pmom2)/(2.*ee)                                       DSIGMA72
C                                                                       DSIGMA73
 100  c = 2.*rndm(c) - 1.                                               DSIGMA74
      IF ( ipro .GE. 3 ) THEN                                           DSIGMA75
        weight = 1.-c**2                                                DSIGMA76
        wmax = 1.                                                       DSIGMA77
        IF ( weight/wmax .LT. RNDM(dummy) ) GOTO 100                    DSIGMA78
      ELSE                                                              DSIGMA79
        weight = 1.+c**2 + qa(4)**2/am2**2 * (1.-c**2)                  DSIGMA80
        wmax   = 1. + (qa(4)/am2)**2                                    DSIGMA81
        IF ( weight/wmax .LT. RNDM(dummy) ) GOTO 100                    DSIGMA82
        IF ( (1+c**2)/weight .GT. RNDM(dummy) ) THEN                    DSIGMA83
          izpol = 1                                                     DSIGMA84
        ELSE                                                            DSIGMA85
          izpol = 0                                                     DSIGMA86
        ENDIF                                                           DSIGMA87
      ENDIF                                                             DSIGMA88
      p = 2.*pi*rndm(p)                                                 DSIGMA89
      s = SQRT(1.-c**2)                                                 DSIGMA90
C                                                                       DSIGMA91
      qh(4) = am1                                                       DSIGMA92
      qh(3) = pmom * c                                                  DSIGMA93
      qh(2) = pmom * s * SIN(p)                                         DSIGMA94
      qh(1) = pmom * s * COS(p)                                         DSIGMA95
      qa(4) = am2                                                       DSIGMA96
      qa(3) =-qh(3)                                                     DSIGMA97
      qa(2) =-qh(2)                                                     DSIGMA98
      qa(1) =-qh(1)                                                     DSIGMA99
      GOTO 999                                                          DSIGM100
C                                                                       DSIGM101
C WW fusion now.                                                        DSIGM102
C                                                                       DSIGM103
 1000 CONTINUE                                                          DSIGM104
C                                                                       DSIGM105
      CALL dsigwwh(ipro,ee,qh,qa,hh)                                    DSIGM106
C                                                                       DSIGM107
  999 RETURN                                                            DSIGM108
      END                                                               DSIGM109
      SUBROUTINE dsigwwh(ipro,ee,p1,p2,hh)                              DSIGWWH2
C -------------------------------------------------------------------   DSIGWWH3
C! Generate unweighted events in the WW/ZZ fusion into h channels       DSIGWWH4
C                                                                       DSIGWWH5
C Input:    ee,   the effective centre-of-mass energy                   DSIGWWH6
C           ipro, = 5 for WW --> h                                      DSIGWWH7
C                 = 6 for WW --> H                                      DSIGWWH8
C                 = 7 for ZZ --> h                                      DSIGWWH9
C                 = 8 for ZZ --> H                                      DSIGWW10
C                                                                       DSIGWW11
C Output    p1,   the neutrino/e- momentum                              DSIGWW12
C           p2,   the anti-neutrino/e+ momentum                         DSIGWW13
C           hh,   the Higgs momentum                                    DSIGWW14
C                                                                       DSIGWW15
C Patrick Janot -- 3 Sep 1995                                           DSIGWW16
C -------------------------------------------------------------------   DSIGWW17
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
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
      PARAMETER (maxpro= 8)                                             HHPROD 2
      COMMON / cropro / cross(maxpro), sthr(maxpro), reduc(maxpro)      HHPROD 3
      CHARACTER*14 chapro(maxpro)                                       HHPROD 4
      DATA chapro /                                                     HHPROD 5
     .             'e+e- --> h Z',                                      HHPROD 6
     .             'e+e- --> H Z',                                      HHPROD 7
     .             'e+e- --> h A',                                      HHPROD 8
     .             'e+e- --> H A',                                      HHPROD 9
     .             'W+W- --> h  ',                                      HHPROD10
     .             'W+W- --> H  ',                                      HHPROD11
     .             'Z Z  --> h  ',                                      HHPROD12
     .             'Z Z  --> H  '                                       HHPROD13
     .                                /                                 HHPROD14
      COMMON / prosim / iproyn(maxpro),nevpro(maxpro)                   HHPROD15
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)        DATAPI 2
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          DATAPI 3
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         DATAPI 4
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         DATAPI 5
C                                                                       DATAPI 6
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),               CONSTS 2
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw      CONSTS 3
C                                                                       CONSTS 4
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,      MISCL  2
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),            MISCL  3
     &                 nevent(11)                                       MISCL  4
      INTEGER loutbe,idb1,idb2,nevent                                   MISCL  5
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad                      MISCL  6
C                                                                       MISCL  7
      DIMENSION p1(4), p2(4), pm(4), pp(4), hh(4), ptot(4)              DSIGWW24
      REAL*8 qbt,y0,tau,betah,x1,x2,xtau,px,pmax,eb,dv,dv1,dv2          DSIGWW25
      REAL*8 densf,densit,c1,c2,s1,s2,fi1,fi2,c1df,c2df                 DSIGWW26
      REAL*8 x,ratio1,ratio2,ratio3,born1,born2,bwh                     DSIGWW27
      evolwz(x) = -(1.+x)*DLOG(x)-2.*(1.-x)                             DSIGWW28
C                                                                       DSIGWW29
      ss   = ee**2                                                      DSIGWW30
      eb   = ee/2.                                                      DSIGWW31
C                                                                       DSIGWW32
C The incoming momenta                                                  DSIGWW33
C                                                                       DSIGWW34
      pm(1) = 0.                                                        DSIGWW35
      pm(2) = 0.                                                        DSIGWW36
      pm(3) = eb                                                        DSIGWW37
      pm(4) = eb                                                        DSIGWW38
      pp(1) = 0.                                                        DSIGWW39
      pp(2) = 0.                                                        DSIGWW40
      pp(3) =-eb                                                        DSIGWW41
      pp(4) = eb                                                        DSIGWW42
C                                                                       DSIGWW43
C Compute relevant couplings                                            DSIGWW44
C                                                                       DSIGWW45
      e = SQRT(4.*pi*alpha(0))                                          DSIGWW46
C                                                                       DSIGWW47
C Case 1: WW scattering                                                 DSIGWW48
C                                                                       DSIGWW49
      IF ( ipro .EQ. 5 .OR. ipro .EQ. 6 ) THEN                          DSIGWW50
C                                                                       DSIGWW51
        rmv  = amw                                                      DSIGWW52
        v1   = e/sw/SQRT(8.)                                            DSIGWW53
        v2   = v1                                                       DSIGWW54
        a1   = v1                                                       DSIGWW55
        a2   = v1                                                       DSIGWW56
        gvvh = e/sw*rmv                                                 DSIGWW57
C                                                                       DSIGWW58
C Case 2: ZZ scattering                                                 DSIGWW59
C                                                                       DSIGWW60
      ELSEIF ( ipro .EQ. 7 .OR. ipro .EQ. 8 ) THEN                      DSIGWW61
C                                                                       DSIGWW62
        rmv  = amz                                                      DSIGWW63
        a1   =-e/4./sw/cw                                               DSIGWW64
        v1   = a1*(1.-4.*sw2)                                           DSIGWW65
        a2   = a1                                                       DSIGWW66
        v2   = v1                                                       DSIGWW67
        gvvh = e*rmv/sw/cw                                              DSIGWW68
C                                                                       DSIGWW69
      ENDIF                                                             DSIGWW70
C                                                                       DSIGWW71
C This goes into the matrix element squared                             DSIGWW72
C                                                                       DSIGWW73
      con1 = gvvh**2*((v1-a1)**2*(v2-a2)**2+(v1+a1)**2*(v2+a2)**2)      DSIGWW74
      con2 = gvvh**2*((v1-a1)**2*(v2+a2)**2+(v1+a1)**2*(v2-a2)**2)      DSIGWW75
C                                                                       DSIGWW76
C Cross section comes out in pb                                         DSIGWW77
C                                                                       DSIGWW78
      picob = alpha2 / alpha(0)**2                                      DSIGWW79
     .               / (2.*ss*twopi**5)                                 DSIGWW80
C                                                                       DSIGWW81
C Generate the mass of the Higgs                                        DSIGWW82
C                                                                       DSIGWW83
      IF     ( ipro .EQ. 5 .OR. ipro .EQ. 7 ) THEN                      DSIGWW84
C                                                                       DSIGWW85
        rmh  = pmas(25,1)                                               DSIGWW86
        rgh  = pmas(25,2)                                               DSIGWW87
        cou  = sab2                                                     DSIGWW88
C                                                                       DSIGWW89
      ELSEIF ( ipro .EQ. 6 .OR. ipro .EQ. 8 ) THEN                      DSIGWW90
C                                                                       DSIGWW91
        rmh  = pmas(35,1)                                               DSIGWW92
        rgh  = pmas(35,2)                                               DSIGWW93
        cou  = cab2                                                     DSIGWW94
C                                                                       DSIGWW95
      ENDIF                                                             DSIGWW96
C                                                                       DSIGWW97
      rmh2 = rmh**2                                                     DSIGWW98
      rmgh = rmh*rgh                                                    DSIGWW99
      CALL bwgene(0.,ee,rmh,rgh,rm1,djdum)                              DSIGW100
      rm2 = rm1**2                                                      DSIGW101
C                                                                       DSIGW102
C Approximate the maximal weight for a better efficiency                DSIGW103
C                                                                       DSIGW104
      ratio1 = rmh2/ecm**2                                              DSIGW105
      ratio2 = rm2/ss                                                   DSIGW106
      ratio3 = ss/rmh**2                                                DSIGW107
      born2  = rmh/rgh                                                  DSIGW108
      born1  = -born2 * (1.-ratio3)                                     DSIGW109
C                                                                       DSIGW110
      wmax = 4. * cross(ipro)                                           DSIGW111
C                                                                       DSIGW112
      IF ( 1.-ratio2 .GT. 1D-2 ) THEN                                   DSIGW113
        evol = evolwz(ratio2)                                           DSIGW114
      ELSE                                                              DSIGW115
        evol = (1.-ratio2)**2/6. + (1.-ratio2)**3/6.                    DSIGW116
      ENDIF                                                             DSIGW117
C                                                                       DSIGW118
      IF ( ratio3 .GT. 1D-1 ) THEN                                      DSIGW119
        evol = evol                                                     DSIGW120
     .       * ( DATAN(born1)+DATAN(born2) )                            DSIGW121
     .       / pi                                                       DSIGW122
      ELSE                                                              DSIGW123
        evol = evol                                                     DSIGW124
     .       * ( (rmh/rgh)    / (1.+(rmh/rgh)**2)    * ratio3           DSIGW125
     .       +   (rmh/rgh)**3 / (1.+(rmh/rgh)**2)**2 * ratio3**2 )      DSIGW126
     .       / pi                                                       DSIGW127
      ENDIF                                                             DSIGW128
      evol = evol / evolwz(ratio1)                                      DSIGW129
C                                                                       DSIGW130
      IF ( evol .LT. 1E-2 ) evol = 0.                                   DSIGW131
C    .  evol = cou * sigmawwh(ss,ipro) / cross(ipro)                    DSIGW132
                                                                        DSIGW133
C                                                                       DSIGW134
      wmax = wmax * evol                                                DSIGW135
C                                                                       DSIGW136
C Compute the delta-V angular distribution cutoff                       DSIGW137
C                                                                       DSIGW138
      rmv2  = rmv**2                                                    DSIGW139
      betah = 1.-rm2/ss                                                 DSIGW140
      dv    = 2.*rmv2/(ss*betah)                                        DSIGW141
      dv1   = 1.+dv                                                     DSIGW142
      dv2   = dv*(2.+dv)                                                DSIGW143
C                                                                       DSIGW144
C Constant part of the local density                                    DSIGW145
C                                                                       DSIGW146
      densf = (dv2/4./pi)**2 * 64./ss                                   DSIGW147
C                                                                       DSIGW148
C Generation of fermion scattering angles                               DSIGW149
C                                                                       DSIGW150
    2 c1  = dv1-dv2/(2.*RNDM(1)+dv)                                     DSIGW151
      c2  = dv1-dv2/(2.*RNDM(3)+dv)                                     DSIGW152
      fi1 = twopi*RNDM(2)                                               DSIGW153
      fi2 = twopi*RNDM(4)                                               DSIGW154
C                                                                       DSIGW155
C Construct fermion directions                                          DSIGW156
C                                                                       DSIGW157
      s1    = SQRT(1.-c1*c1)                                            DSIGW158
      s2    = SQRT(1.-c2*c2)                                            DSIGW159
      p1(1) = s1*SIN(fi1)                                               DSIGW160
      p1(2) = s1*COS(fi1)                                               DSIGW161
      p1(3) = c1                                                        DSIGW162
      p1(4) = 1.                                                        DSIGW163
      p2(1) =-s2*SIN(fi2)                                               DSIGW164
      p2(2) =-s2*COS(fi2)                                               DSIGW165
      p2(3) =-c2                                                        DSIGW166
      p2(4) = 1.                                                        DSIGW167
C                                                                       DSIGW168
C Compute acollinearity parameter                                       DSIGW169
C                                                                       DSIGW170
      tau = prosca(p1,p2)/2.                                            DSIGW171
C                                                                       DSIGW172
C Generate x1 value such that x1 and x2 distributions are the same      DSIGW173
C                                                                       DSIGW174
      y0   = 1.-betah*tau                                               DSIGW175
      pmax = betah*betah/(4.*y0)                                        DSIGW176
    1 CONTINUE                                                          DSIGW177
      x1   = betah*RNDM(5)                                              DSIGW178
      xtau = 1./(1.-tau*x1)                                             DSIGW179
      x2   = (betah-x1)*xtau                                            DSIGW180
      px   = x1*x2*xtau                                                 DSIGW181
      IF ( px .LT. RNDM(6)*pmax ) GOTO 1                                DSIGW182
C                                                                       DSIGW183
C Construct fermion momenta                                             DSIGW184
C                                                                       DSIGW185
      DO k=1,4                                                          DSIGW186
        p1(k) = p1(k)*eb*x1                                             DSIGW187
        p2(k) = p2(k)*eb*x2                                             DSIGW188
      ENDDO                                                             DSIGW189
C                                                                       DSIGW190
C The higgs momentum follows from momentum conservation                 DSIGW191
C                                                                       DSIGW192
      DO k=1,4                                                          DSIGW193
        hh(k) = pm(k)+pp(k)-p1(k)-p2(k)                                 DSIGW194
      ENDDO                                                             DSIGW195
C                                                                       DSIGW196
C Check on algorithm by computing the Higgs mass                        DSIGW197
C                                                                       DSIGW198
      chk = (hh(4)**2-hh(3)**2-hh(2)**2-hh(1)**2)/rm2 - 1.              DSIGW199
      IF ( ABS(chk) .GT. 1e-3 ) WRITE(*,*) ' +++ mh chk:',chk           DSIGW200
      hh(4) = rm1                                                       DSIGW201
C                                                                       DSIGW202
C Compute the local density                                             DSIGW203
C                                                                       DSIGW204
      IF( betah*tau .GT. 1D-3 ) THEN                                    DSIGW205
        qbt  = (-(1.+y0)*DLOG(y0)+2.*y0-2.)/(tau*tau*tau)               DSIGW206
      ELSE                                                              DSIGW207
        qbt  = betah**3*(10.+10.*betah*tau+9.*betah**2*tau**2)/60.      DSIGW208
      ENDIF                                                             DSIGW209
      c1df   = -c1+dv1                                                  DSIGW210
      c2df   = -c2+dv1                                                  DSIGW211
      densit = densf/(qbt*c1df*c1df*c2df*c2df)                          DSIGW212
C                                                                       DSIGW213
C Compute exact matrix element squared for                              DSIGW214
C   o e+ e- ---> nu_e-bar nu_e W+ W- --> nu_e-bar nu-e h                DSIGW215
C   o e+ e- ---> e+ e-         Z  Z  --> e+ e-         h                DSIGW216
C                                                                       DSIGW217
      vv1   = -2.*prosca(pm,p1)-rmv2                                    DSIGW218
      vv2   = -2.*prosca(pp,p2)-rmv2                                    DSIGW219
      u1    = -2.*prosca(pm,p2)                                         DSIGW220
      u2    = -2.*prosca(pp,p1)                                         DSIGW221
      s1    =  2.*prosca(p1,p2)                                         DSIGW222
      tfi2  = ( con1*u1*u2 + con2*ss*s1 )                               DSIGW223
     .      / ( vv1*vv1 * vv2*vv2)                                      DSIGW224
C                                                                       DSIGW225
C Compute the weight                                                    DSIGW226
C                                                                       DSIGW227
      weight = cou * picob * tfi2  / densit                             DSIGW228
     .       * (1.+deltar)**3 * (1.-5./3.*deltar)                       DSIGW229
C                                                                       DSIGW230
C Apply the rejection algorithm                                         DSIGW231
C                                                                       DSIGW232
      IF ( weight .GT. 1.05*wmax ) THEN                                 DSIGW233
C       WRITE(6,*) 'Warning at ECM = ',ee,' GeV'                        DSIGW234
C       WRITE(6,*) 'Weight (',weight,') > Wmax (',wmax,')'              DSIGW235
        wmax = 3.6*weight                                               DSIGW236
        GOTO 2                                                          DSIGW237
      ELSEIF ( weight .GT. wmax ) THEN                                  DSIGW238
        wmax = weight                                                   DSIGW239
      ENDIF                                                             DSIGW240
      ipr = ipro-4                                                      DSIGW241
      wtot (ipr) = wtot (ipr) + weight                                  DSIGW242
      wtot2(ipr) = wtot2(ipr) + weight**2                               DSIGW243
      ntry (ipr) = ntry (ipr) + 1                                       DSIGW244
      IF ( weight/wmax .LT. RNDM(weight) ) GOTO 2                       DSIGW245
C                                                                       DSIGW246
      nacc(ipr) = nacc(ipr) + 1                                         DSIGW247
C                                                                       DSIGW248
      RETURN                                                            DSIGW249
      END                                                               DSIGW250
      FUNCTION FFFF(A,Z,Y)                                              FFFF   2
C--------------------------------------------------------------------   FFFF   3
C! Analytical expression for the e+e- --> hZ cross section              FFFF   4
C                                                                       FFFF   5
C  From Ronald Kleiss.                                                  FFFF   6
C--------------------------------------------------------------------   FFFF   7
      IMPLICIT COMPLEX*16(A-H,O-Z)                                      FFFF   8
      REAL*8 A,A2,Y                                                     FFFF   9
C                                                                       FFFF  10
      z2 = z*z                                                          FFFF  11
      z3 = z*z2                                                         FFFF  12
      z4 = z*z3                                                         FFFF  13
      a2 = a*a                                                          FFFF  14
C                                                                       FFFF  15
      c0 = ( z4 - 2.*a*z2 + a2 ) /z4 *                                  FFFF  16
     .     ( z4 - 24.*z3 + (48.+10.*a)*z2 - 24.*a*z + a2)               FFFF  17
      c1 = a2*a2/(3.*z)                                                 FFFF  18
      c2 = - a*a2*( 24.*z - a )/(2.*z2)                                 FFFF  19
      c3 = a2*( 8.*z2*(a+6.) - 24.*a*z + a2 ) /z3                       FFFF  20
      c4 = -a2*( 24.*z3 + 8.*z2*(a+6.) - 24.*a*z + a2 )/z4              FFFF  21
      c5 = z3 - 24.*z2 + 8.*z*(a+6.) + 24.*a                            FFFF  22
      c6 = z2/2. - 12.*z + 4.*(a+6.)                                    FFFF  23
      c7 = z/3. - 8.                                                    FFFF  24
      c8 = 1./4.                                                        FFFF  25
C                                                                       FFFF  26
      FFFF = c0*CDLOG(y-z) + c4*DLOG(y) +                               FFFF  27
     . (c1+y*(c2+y*(c3+y*y*(c5+y*(c6+y*(c7+y*c8))))))/(y*y*y)           FFFF  28
C                                                                       FFFF  29
      RETURN                                                            FFFF  30
      END                                                               FFFF  31
      FUNCTION FI( ambos , amfer , chii , chio )                        FI     2
C------------------------------------------------------------------     FI     3
C! Function used in the determination of chi' --> chi gamma             FI     4
C                                                                       FI     5
C V. Bertin for CHA001                                                  FI     6
C------------------------------------------------------------------     FI     7
      IMPLICIT REAL*8(A-H,O-Z)                                          FI     8
      REAL*4 ambos,amfer,chii,chio                                      FI     9
C                                                                       FI    10
C     PRINT *,'FI mbos mfer chii chio :',ambos,amfer,chii,chio          FI    11
      result = 0.D0                                                     FI    12
C                                                                       FI    13
C     IF ( chii .GE. ambos + amfer ) THEN                               FI    14
C      PRINT *,'ambos amfer chii :',ambos,amfer,chii                    FI    15
C      GOTO 999                                                         FI    16
C     ENDIF                                                             FI    17
      IF ( chio .LT. 1.E-9 ) chio = 1.E-9                               FI    18
      IF ( chii .LT. 1.E-9 ) chii = 1.E-9                               FI    19
C                                                                       FI    20
      hi = (ambos**2 - amfer**2 - chii**2) / (2.* chii**2)              FI    21
      wi = hi**2 - (amfer / chii)**2                                    FI    22
c     PRINT *,'hi wi :',hi,wi                                           FI    23
      IF ( wi .LT. 0.D0 )  THEN                                         FI    24
       ximag = DSQRT(-wi)                                               FI    25
       xreal = -hi                                                      FI    26
       CALL dilokp(xreal,ximag,f1,f2)                                   FI    27
      ELSE                                                              FI    28
       xrpi = -hi + DSQRT(wi)                                           FI    29
       IF ( ABS(xrpi) .LE. 1.D-15) xrpi = 1.D-15                        FI    30
       xrpi = 1. / xrpi                                                 FI    31
       xrmi = -hi - DSQRT(wi)                                           FI    32
       IF ( ABS(xrmi) .LE. 1.D-15) xrmi = 1.D-15                        FI    33
       xrmi = 1. / xrmi                                                 FI    34
       f1 = ddilog(xrpi)                                                FI    35
       f2 = ddilog(xrmi)                                                FI    36
      ENDIF                                                             FI    37
c     PRINT *,'f1   f2 :',f1,f2                                         FI    38
C                                                                       FI    39
      ho = (ambos**2 - amfer**2 - chio**2) / (2.* chio**2)              FI    40
      wo = ho**2 - (amfer / chio)**2                                    FI    41
c     PRINT *,'ho wo :',ho,wo                                           FI    42
      IF ( wo .LT. 0.D0 ) THEN                                          FI    43
       ximag = DSQRT(-wo)                                               FI    44
       xreal = -ho                                                      FI    45
       CALL dilokp(xreal,ximag,f3,f4)                                   FI    46
      ELSE                                                              FI    47
       xrpi = -ho + DSQRT(wo)                                           FI    48
       IF ( ABS(xrpi) .LE. 1.D-15) xrpi = 1.D-15                        FI    49
       xrpi = 1. / xrpi                                                 FI    50
       xrmi = -ho - DSQRT(wo)                                           FI    51
       IF ( ABS(xrmi) .LE. 1.D-15) xrmi = 1.D-15                        FI    52
       xrmi = 1. / xrmi                                                 FI    53
       f3 = ddilog(xrpi)                                                FI    54
       f4 = ddilog(xrmi)                                                FI    55
      ENDIF                                                             FI    56
c     PRINT *,'f3   f4 :',f3,f4                                         FI    57
C                                                                       FI    58
      result = (-f1 -f2 + f3 + f4) / (chii**2 - chio**2)                FI    59
c     PRINT *,'result :',result                                         FI    60
C                                                                       FI    61
 999  CONTINUE                                                          FI    62
      fi = result                                                       FI    63
      RETURN                                                            FI    64
      END                                                               FI    65
      SUBROUTINE filujt(ich,ichi2,jchi,ifn)                             FILUJT 2
C------------------------------------------------------------------     FILUJT 3
C! Fill LUJETS common block after a neutral/charg-ino decay.            FILUJT 4
C                                                                       FILUJT 5
C  Input:    ich,   the position of the neutral/charg-ino in LUJET      FILUJT 6
C            ichi2, the index of the final charg/neutral-ino            FILUJT 7
C            jchi,  its charge (absolute value)                         FILUJT 8
C            ifn,   the index of the final boson                        FILUJT 9
C                     = 0 for a photon                                  FILUJT10
C                     = 1 for a Z                                       FILUJT11
C                     = 2 for a W                                       FILUJT12
C                                                                       FILUJT13
C  Patrick Janot -- 31 Aug 1995                                         FILUJT14
C------------------------------------------------------------------     FILUJT15
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
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      CHARACTER*14 channel                                              HHDECK 2
      CHARACTER*21 channeut, chanchar                                   HHDECK 3
      PARAMETER(nchneut=8,nchchar=5)                                    HHDECK 4
      COMMON / hhdeck / branch(nchan,nhig),width(nhig),                 HHDECK 5
     .                  parwth(nhig),xymas(2,nchan,nhig),               HHDECK 6
     .                  xywid(2,nchan,nhig)                             HHDECK 7
      COMMON / chaneu / ichn(2),                                        HHDECK 8
     .                  wneut(4,4,nhig), wchar(2,2,nhig),               HHDECK 9
     .                  widneut(4), brneut(nchneut,4),                  HHDECK10
     .                  widchar(2), brchar(nchchar,2)                   HHDECK11
      COMMON / chanch / channeut(nchneut,4), chanchar(nchchar,2)        HHDECK12
      COMMON / vect4 / pvect4(5,2)                                      HHDECK13
      COMMON / hinput / ichan(nchan,nhig), channel(nchan,nhig)          HHDECK14
      COMMON / hisbr / bchpp,bchgg                                      HHDECK15
      DIMENSION ph(4)                                                   HHDECK16
C                                                                       HHDECK17
      DIMENSION ijoin(2)                                                FILUJT19
      DIMENSION p1(4),p2(4),p3(4)                                       FILUJT20
C                                                                       FILUJT21
      ichi = k7lu(ich,2)/ABS(k7lu(ich,2))                               FILUJT22
C                                                                       FILUJT23
      IF ( jchi .EQ. 0 ) THEN                                           FILUJT24
        kf1 = 50+ichi2                                                  FILUJT25
      ELSE                                                              FILUJT26
        kf1 = jchi * (54+ichi2)                                         FILUJT27
      ENDIF                                                             FILUJT28
C                                                                       FILUJT29
      kf2 = 22 + ifn                                                    FILUJT30
      IF ( kf2 .EQ. 24 ) THEN                                           FILUJT31
        IF ( ichi .EQ. 0 ) THEN                                         FILUJT32
          kf2 = -jchi * kf2                                             FILUJT33
        ELSE                                                            FILUJT34
          kf2 =  ichi * kf2                                             FILUJT35
        ENDIF                                                           FILUJT36
      ENDIF                                                             FILUJT37
C                                                                       FILUJT38
      n7lu = n7lu + 1                                                   FILUJT39
      ipochi = n7lu                                                     FILUJT40
      CALL hhlu1(ipochi,kf1,pvect4(1,1),pvect4(2,1),                    FILUJT41
     .                      pvect4(3,1),pvect4(4,1),pvect4(5,1))        FILUJT42
      k7lu(ich,4) = ipochi                                              FILUJT43
      k7lu(ipochi,3) = ich                                              FILUJT44
C                                                                       FILUJT45
      n7lu = n7lu + 1                                                   FILUJT46
      ipobos = n7lu                                                     FILUJT47
      CALL hhlu1(ipobos,kf2,pvect4(1,2),pvect4(2,2),                    FILUJT48
     .                      pvect4(3,2),pvect4(4,2),pvect4(5,2))        FILUJT49
      k7lu(ich,5) = ipobos                                              FILUJT50
      k7lu(ipobos,3) = ich                                              FILUJT51
C                                                                       FILUJT52
C Special path for W and Z                                              FILUJT53
C                                                                       FILUJT54
      IF ( ifn .GT. 0 ) CALL wzdecy(ipobos)                             FILUJT55
      k7lu(ich,1) = 11                                                  FILUJT56
C                                                                       FILUJT57
      IF ( idbg .GE. 5 ) CALL lulist(1)                                 FILUJT58
  999 RETURN                                                            FILUJT59
      END                                                               FILUJT60
      FUNCTION FINT(a,b,c)                                              FINT   2
C--------------------------------------------------------------------   FINT   3
C! Quick integration for the Higgs pole masses computation              FINT   4
C                                                                       FINT   5
C  Improves on the previous method of Carena/Wagner by a                FINT   6
C  factor > 100.                                                        FINT   7
C                                                                       FINT   8
C  Patrick Janot -- 21 Sep 1995                                         FINT   9
C--------------------------------------------------------------------   FINT  10
      IMPLICIT REAL*8(A-H,O-Z)                                          FINT  11
      COMMON /CEM/ y1,y2,p2                                             FINT  12
      EXTERNAL fintsub                                                  FINT  13
      DIMENSION x(1)                                                    FINT  14
      p2 = a                                                            FINT  15
      y1 = b                                                            FINT  16
      y2 = c                                                            FINT  17
      xlo = 0D0                                                         FINT  18
      xhi = 1D0                                                         FINT  19
      fint  = DGMLT1(fintsub,xlo,xhi,1,6,x)                             FINT  20
      RETURN                                                            FINT  21
      END                                                               FINT  22
      SUBROUTINE fintsub(m,u1,f1,x)                                     FINT  23
      IMPLICIT REAL*8(A-H,O-Z)                                          FINT  24
      COMMON /CEM/ y1,y2,p2                                             FINT  25
      DIMENSION u1(*),f1(*),x(1)                                        FINT  26
      DO l = 1,m                                                        FINT  27
        x(1) = u1(l)                                                    FINT  28
        f1(l) = LOG(ABS(x(1)*y1+(1-x(1))*y2-x(1)*(1-x(1))*p2)           FINT  29
     .                /(x(1)*(y1-y2)+y2))                               FINT  30
      ENDDO                                                             FINT  31
      RETURN                                                            FINT  32
      END                                                               FINT  33
      FUNCTION FINTAN(p2,y1,y2)                                         FINTAN 2
C--------------------------------------------------------------------   FINTAN 3
C! Analytical expression for the integration done in FINT               FINTAN 4
C                                                                       FINTAN 5
C  Not used in the code                                                 FINTAN 6
C                                                                       FINTAN 7
C  From M. Carena and C. Wagner, 21 Sep 1995                            FINTAN 8
C--------------------------------------------------------------------   FINTAN 9
      IMPLICIT REAL*8(A-H,M,O-Z)                                        FINTAN10
      delta=(y1-y2)/p2                                                  FINTAN11
      erre=(abs((1.+delta)**2-4.*y1/p2))**.5                            FINTAN12
      fintan=-1.+.5*((y1+y2)/(y1-y2)-delta)*log(y2/y1)                  FINTAN13
     *    +.5*erre*log(abs((delta**2-(1.+erre)**2)) /                   FINTAN14
     *      abs((delta**2-(1.-erre)**2)) )                              FINTAN15
      RETURN                                                            FINTAN16
      END                                                               FINTAN17
      FUNCTION FI2( ambos , amfer , chii , chio )                       FI2    2
C------------------------------------------------------------------     FI2    3
C! Function used in the determination of chi' --> chi gamma             FI2    4
C                                                                       FI2    5
C V. Bertin for CHA001                                                  FI2    6
C------------------------------------------------------------------     FI2    7
      IMPLICIT REAL*8(A-H,O-Z)                                          FI2    8
      REAL*4 ambos,amfer,chii,chio                                      FI2    9
      COMPLEX*16 lambi,lambo,him,hom,hip,hop,ri2                        FI2   10
C                                                                       FI2   11
      IF ( chio .LT. 1.E-9 ) chio = 1.E-9                               FI2   12
      IF ( chii .LT. 1.E-9 ) chii = 1.E-9                               FI2   13
      IF ( amfer.LT. 1.E-9 ) amfer= 1.E-9                               FI2   14
C                                                                       FI2   15
c     PRINT *,'FI2 mbos mfer chii chio :',ambos,amfer,chii,chio         FI2   16
      parai = amfer**2 + ambos**2 - chii**2                             FI2   17
      parao = amfer**2 + ambos**2 - chio**2                             FI2   18
      rambi = parai**2 - (2.*ambos*amfer)**2                            FI2   19
      rambo = parao**2 - (2.*ambos*amfer)**2                            FI2   20
      zero = 0.d0                                                       FI2   21
c     PRINT *,'rambi rambo zero:',rambi,rambo,zero                      FI2   22
      lambi = DCMPLX(rambi,zero)                                        FI2   23
      lambo = DCMPLX(rambo,zero)                                        FI2   24
      lambi = CDSQRT(lambi)                                             FI2   25
      lambo = CDSQRT(lambo)                                             FI2   26
c     PRINT *,'lambi lambo :',lambi,lambo                               FI2   27
c     PRINT *,'parai parao :',parai,parao                               FI2   28
      hip = parai + lambi                                               FI2   29
      him = parai - lambi                                               FI2   30
      hop = parao + lambo                                               FI2   31
      hom = parao - lambo                                               FI2   32
      IF (CDABS(him) .LT. 1.d-12) him = DCMPLX(1.d-12,0.d0)             FI2   33
      IF (CDABS(hom) .LT. 1.d-12) hom = DCMPLX(1.d-12,0.d0)             FI2   34
c     PRINT *,'hip him hop hom :',hip,him,hop,hom                       FI2   35
C                                                                       FI2   36
c     ri2 = (CDLOG(him / hip) * lambi / (2.* chii**2)                   FI2   37
c    .     - CDLOG(hom / hop) * lambo / (2.* chio**2))                  FI2   38
c    .     / (chii**2 - chio**2)                                        FI2   39
c    .    + (ambos**2 - amfer**2) / (2. * chii**2 * chio**2)            FI2   40
c    .     * LOG(amfer**2 / ambos**2)                                   FI2   41
      faci = .5d0 / chii**2 / (chii**2 - chio**2)                       FI2   42
      faco = .5d0 / chio**2 / (chii**2 - chio**2)                       FI2   43
      ri2 = CDLOG(him / hip) * lambi * faci                             FI2   44
     .    - CDLOG(hom / hop) * lambo * faco                             FI2   45
      fi2 = DREAL(ri2)                                                  FI2   46
     .    + (ambos**2 - amfer**2) / (2.d0 * chii**2 * chio**2)          FI2   47
     .     * LOG(amfer**2 / ambos**2)                                   FI2   48
c     PRINT *,'ri2 fi2:',ri2,fi2                                        FI2   49
      RETURN                                                            FI2   50
      END                                                               FI2   51
      FUNCTION FJ( ambos , amfer , chii , chio )                        FJ     2
C------------------------------------------------------------------     FJ     3
C! Function used in the determination of chi' --> chi gamma             FJ     4
C                                                                       FJ     5
C V. Bertin for CHA001                                                  FJ     6
C------------------------------------------------------------------     FJ     7
      IMPLICIT REAL*8(A-H,O-Z)                                          FJ     8
      REAL*4 ambos,amfer,chii,chio                                      FJ     9
C                                                                       FJ    10
c     PRINT *,'FJ mbos mfer chii chio :',ambos,amfer,chii,chio          FJ    11
      fj = fi(amfer,ambos,chii,chio)                                    FJ    12
      RETURN                                                            FJ    13
      END                                                               FJ    14
      FUNCTION FK( ambos , amfer , chii , chio )                        FK     2
C------------------------------------------------------------------     FK     3
C! Function used in the determination of chi' --> chi gamma             FK     4
C                                                                       FK     5
C V. Bertin for CHA001                                                  FK     6
C------------------------------------------------------------------     FK     7
      IMPLICIT REAL*8(A-H,O-Z)                                          FK     8
      REAL*4 ambos,amfer,chii,chio                                      FK     9
C                                                                       FK    10
c     PRINT *,'FK mbos mfer chii chio :',ambos,amfer,chii,chio          FK    11
      fk = (1.                                                          FK    12
     .    + amfer**2 * FI(ambos,amfer,chii,chio)                        FK    13
     .    + ambos**2 * FJ(ambos,amfer,chii,chio)                        FK    14
     .    - chii**2 * FI2(ambos,amfer,chii,chio))                       FK    15
     .   / (chio**2 - chii**2)                                          FK    16
      RETURN                                                            FK    17
      END                                                               FK    18
      SUBROUTINE fsubcha(m,u1,f1,x)                                     FSUBCHA2
C--------------------------------------------------------------------   FSUBCHA3
C! A routine for the chi+ decays into W* chi                            FSUBCHA4
C                                                                       FSUBCHA5
C  Patrick Janot -- 23 Sep 1995                                         FSUBCHA6
C--------------------------------------------------------------------   FSUBCHA7
      IMPLICIT REAL*8(A-H,O-Z)                                          FSUBCHA8
      COMMON / chachi / a,g,xx,xip,clr2,cltr                            FSUBCHA9
      DIMENSION u1(*),f1(*),x(1)                                        FSUBCH10
      DO l = 1,m                                                        FSUBCH11
        x(1) = u1(l)                                                    FSUBCH12
        u = (a/xip)**2+a/xip*g/xip*TAN(x(1))                            FSUBCH13
        part1 = -clr2 * u**2                                            FSUBCH14
     .        + .5 *((1. + xx**2)* clr2 - 12.* cltr * xx)* u            FSUBCH15
     .        + .5 *(1.-xx**2)**2 * clr2                                FSUBCH16
        part2 = u**2 - 2.*(1.+xx**2)*u + (1.-xx**2)**2                  FSUBCH17
        f1(l) = part1 * DSQRT(part2)                                    FSUBCH18
      ENDDO                                                             FSUBCH19
      RETURN                                                            FSUBCH20
      END                                                               FSUBCH21
      SUBROUTINE fsubgz(m,u1,f1,x)                                      FSUBGZ 2
C--------------------------------------------------------------------   FSUBGZ 3
C! A routine for the h --> Z gamma decay including Z width              FSUBGZ 4
C                                                                       FSUBGZ 5
C  Patrick Janot -- 23 Sep 1995                                         FSUBGZ 6
C--------------------------------------------------------------------   FSUBGZ 7
      IMPLICIT REAL*8(A-H,O-Z)                                          FSUBGZ 8
      COMMON /BHV/ s,am1,w1                                             FSUBGZ 9
      DIMENSION u1(*),f1(*),x(1)                                        FSUBGZ10
      DO l = 1,m                                                        FSUBGZ11
        x(1) = u1(l)                                                    FSUBGZ12
        u = am1**2+am1*w1*TAN(x(1))                                     FSUBGZ13
        f1(l) = (1.-u/s)**3                                             FSUBGZ14
      ENDDO                                                             FSUBGZ15
      RETURN                                                            FSUBGZ16
      END                                                               FSUBGZ17
      SUBROUTINE fsubwz2(m,u2,f2,x)                                     FSUBWZ 2
C--------------------------------------------------------------------   FSUBWZ 3
C! Two routines for the h-->WW/ZZ decays including W/Z widths           FSUBWZ 4
C                                                                       FSUBWZ 5
C  Patrick Janot -- 15 Sep 1995                                         FSUBWZ 6
C--------------------------------------------------------------------   FSUBWZ 7
      IMPLICIT REAL*8(A-H,O-Z)                                          FSUBWZ 8
      COMMON /BWC/ s,am1,am2,w1,w2                                      FSUBWZ 9
      EXTERNAL fsubwz1                                                  FSUBWZ10
      DIMENSION u2(*),f2(*),x(2)                                        FSUBWZ11
      DO l = 1,m                                                        FSUBWZ12
        x(2)  = u2(l)                                                   FSUBWZ13
        am2W2 = am2**2+am2*w2*DTAN(x(2))                                FSUBWZ14
        am2w  = DSQRT(DMAX1(0D0,am2W2))                                 FSUBWZ15
        x1lo  = -DATAN2(am1,w1)                                         FSUBWZ16
        x1hi  =  DATAN2((SQRT(s)-am2W)**2-am1**2,am1*w1)                FSUBWZ17
        IF ( x1lo .LT. x1hi ) THEN                                      FSUBWZ18
          f2(l) = DGMLT1(fsubwz1,x1lo,x1hi,4,8,x)                       FSUBWZ19
        ELSE                                                            FSUBWZ20
          f2(l) = 0.                                                    FSUBWZ21
        ENDIF                                                           FSUBWZ22
      ENDDO                                                             FSUBWZ23
      RETURN                                                            FSUBWZ24
      END                                                               FSUBWZ25
      SUBROUTINE fsubwz1(m,u1,f1,x)                                     FSUBWZ26
      IMPLICIT REAL*8(A-H,O-Z)                                          FSUBWZ27
      COMMON /BWC/ s,am1,am2,w1,w2                                      FSUBWZ28
      DIMENSION u1(*),f1(*),x(2)                                        FSUBWZ29
      DO l = 1,m                                                        FSUBWZ30
        x(1) = u1(l)                                                    FSUBWZ31
        u = (am1**2+am1*w1*DTAN(x(1)))                                  FSUBWZ32
        v = (am2**2+am2*w2*DTAN(x(2)))                                  FSUBWZ33
        ss = DMAX1((s-u-v)**2-4.*u*v, 0D0)                              FSUBWZ34
        f1(l) =  DSQRT(ss) * (u*v+ss/12.)                               FSUBWZ35
      ENDDO                                                             FSUBWZ36
      RETURN                                                            FSUBWZ37
      END                                                               FSUBWZ38
      SUBROUTINE fsubzst(m,u1,f1,x)                                     FSUBZST2
C--------------------------------------------------------------------   FSUBZST3
C! A routine for the chi' decays into Z* chi                            FSUBZST4
C                                                                       FSUBZST5
C  Patrick Janot -- 23 Sep 1995                                         FSUBZST6
C--------------------------------------------------------------------   FSUBZST7
      IMPLICIT REAL*8(A-H,O-Z)                                          FSUBZST8
      COMMON / chachi / a,g,xx,xip,clr2,cltr                            FSUBZST9
      DIMENSION u1(*),f1(*),x(1)                                        FSUBZS10
      DO l = 1,m                                                        FSUBZS11
        x(1) = u1(l)                                                    FSUBZS12
        u = (a/xip)**2+a/xip*g/xip*TAN(x(1))                            FSUBZS13
        part1 = -1.*u**2                                                FSUBZS14
     .        + .5*(1.+6.*xx+xx**2)*u                                   FSUBZS15
     .        + .5*(1.-xx**2)**2                                        FSUBZS16
        part2 = u**2                                                    FSUBZS17
     .        - 2.*(1.+xx**2)*u                                         FSUBZS18
     .        + (1.-xx**2)**2                                           FSUBZS19
        f1(l) = part1 * DSQRT(part2)                                    FSUBZS20
      ENDDO                                                             FSUBZS21
      RETURN                                                            FSUBZS22
      END                                                               FSUBZS23
      SUBROUTINE fsub2(m,u2,f2,x)                                       FSUB0122
C--------------------------------------------------------------------   FSUB0123
C! Three routines for the hZ/hA cross sections including h/A widths     FSUB0124
C                                                                       FSUB0125
C  Patrick Janot -- 01 Sep 1995                                         FSUB0126
C--------------------------------------------------------------------   FSUB0127
      IMPLICIT REAL*8(A-H,O-Z)                                          FSUB0128
      COMMON /BWC/ s,am1,am2,w1,w2                                      FSUB0129
      EXTERNAL fsub1                                                    FSUB0110
      DIMENSION u2(*),f2(*),x(2)                                        FSUB0111
      DO l = 1,m                                                        FSUB0112
        x(2)  = u2(l)                                                   FSUB0113
        am2W2 = am2**2+am2*w2*tan(x(2))                                 FSUB0114
        am2w  = DSQRT(DMAX1(0D0,am2W2))                                 FSUB0115
        x1lo  = -DATAN2(am1,w1)                                         FSUB0116
        x1hi  =  DATAN2((DSQRT(s)-am2W)**2-am1**2,am1*w1)               FSUB0117
        IF ( x1lo .LT. x1hi ) THEN                                      FSUB0118
          f2(l) = DGMLT1(fsub1,x1lo,x1hi,1,6,x)                         FSUB0119
        ELSE                                                            FSUB0120
          f2(l) = 0.                                                    FSUB0121
        ENDIF                                                           FSUB0122
      ENDDO                                                             FSUB0123
      RETURN                                                            FSUB0124
      END                                                               FSUB0125
      SUBROUTINE fsub1(m,u1,f1,x)                                       FSUB0126
      IMPLICIT REAL*8(A-H,O-Z)                                          FSUB0127
      COMMON /BWC/ s,am1,am2,w1,w2                                      FSUB0128
      DIMENSION u1(*),f1(*),x(2)                                        FSUB0129
      DO l = 1,m                                                        FSUB0130
        x(1) = u1(l)                                                    FSUB0131
        u = (am1**2+am1*w1*DTAN(x(1)))/s                                FSUB0132
        v = (am2**2+am2*w2*DTAN(x(2)))/s                                FSUB0133
        ss = DMAX1(1.+u*u+v*v-2.*(u*v+u+v),0D0)                         FSUB0134
        f1(l) = ss*DSQRT(ss)                                            FSUB0135
      ENDDO                                                             FSUB0136
      RETURN                                                            FSUB0137
      END                                                               FSUB0138
      SUBROUTINE fsub(m,u1,f1,x)                                        FSUB0139
      IMPLICIT REAL*8(A-H,O-Z)                                          FSUB0140
      REAL*4 sigklei,u,ss                                               FSUB0141
      COMMON /BHV/ s,am1,w1                                             FSUB0142
      DIMENSION u1(*),f1(*),x(1)                                        FSUB0143
      ss = s                                                            FSUB0144
      DO l = 1,m                                                        FSUB0145
        x(1) = u1(l)                                                    FSUB0146
        u = DSQRT(am1**2+am1*w1*DTAN(x(1)))                             FSUB0147
        f1(l) = sigklei(u,ss)                                           FSUB0148
      ENDDO                                                             FSUB0149
      RETURN                                                            FSUB0150
      END                                                               FSUB0151
      FUNCTION ftau(tau)                                                FTAU   2
C------------------------------------------------------------------     FTAU   3
C! Famous function appearing in H --> gamma gamma partial widths        FTAU   4
C                                                                       FTAU   5
C  Patrick Janot -- 03 oct 1991                                         FTAU   6
C------------------------------------------------------------------     FTAU   7
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)        DATAPI 2
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          DATAPI 3
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         DATAPI 4
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         DATAPI 5
C                                                                       DATAPI 6
      COMPLEX ftau                                                      FTAU   9
C                                                                       FTAU  10
      IF ( tau .GE. 1 ) THEN                                            FTAU  11
        ftaur = (ASIN(SQRT(1./tau)))**2                                 FTAU  12
        ftaui = 0.                                                      FTAU  13
      ELSEIF ( tau .GE. 1.E-5 ) THEN                                    FTAU  14
        b0 = SQRT(1.-tau)                                               FTAU  15
        ftaur = (-(ALOG((1.-b0)/(1+b0)))**2 +pi2)/4.                    FTAU  16
        ftaui = pi/2.*ALOG((1.+b0)/(1.-b0))                             FTAU  17
      ELSE                                                              FTAU  18
        ftaur = (-ALOG(tau/4.)+pi2)/4.                                  FTAU  19
        ftaui = pi/2.*ALOG(4./tau)                                      FTAU  20
      ENDIF                                                             FTAU  21
C                                                                       FTAU  22
      ftau = CMPLX(ftaur,ftaui)                                         FTAU  23
C                                                                       FTAU  24
      RETURN                                                            FTAU  25
      END                                                               FTAU  26
      SUBROUTINE fvonz(xreal,ximag,f1)                                  FVONZ  2
C------------------------------------------------------------------     FVONZ  3
C! Function used in the determination of chi' --> chi gamma             FVONZ  4
C                                                                       FVONZ  5
C V. Bertin for CHA001                                                  FVONZ  6
C------------------------------------------------------------------     FVONZ  7
      REAL*8 xreal,ximag,f,pi                                           FVONZ  8
      COMPLEX*16 f1,z,g,a,f3,f4                                         FVONZ  9
C                                                                       FVONZ 10
      pi = 3.1415926535897932                                           FVONZ 11
CVb   PRINT *,'FVONZ : xreal ximag :',xreal,ximag                       FVONZ 12
C--Mitchell 7.1 und 7.2                                                 FVONZ 13
      f4 = 1.                                                           FVONZ 14
      f3 = 0.                                                           FVONZ 15
      z = DCMPLX(xreal,ximag)                                           FVONZ 16
Cvb   PRINT *,'z cdbas(z) :',z,CDABS(z)                                 FVONZ 17
      IF ((CDABS(z)) .GT. 1.d0) THEN                                    FVONZ 18
C--Michell 4.1                                                          FVONZ 19
       f3 = -pi*pi/6.d0-.5*CDLOG(-z)*CDLOG(-z)                          FVONZ 20
       f4 = -1.                                                         FVONZ 21
       z = 1./z                                                         FVONZ 22
      ENDIF                                                             FVONZ 23
Cvb   PRINT *,'f3 f4 z :',f3,f4,z                                       FVONZ 24
      n = 1                                                             FVONZ 25
      g = 0.                                                            FVONZ 26
      a = .25*z                                                         FVONZ 27
10    g = g+a                                                           FVONZ 28
      a = a*z*n*n/DBLE(n+2)/DBLE(n+2)                                   FVONZ 29
      IF((CDABS(a)) .LT. .0000001) GOTO 20                              FVONZ 30
      n = n+1                                                           FVONZ 31
      GOTO 10                                                           FVONZ 32
20    f1 = (z*(3.+g)+2.*(1.-z)*CDLOG(1.-z))/(1.+z)                      FVONZ 33
      f1 = f1*f4+f3                                                     FVONZ 34
Cvb   PRINT *,'f1 :',f1                                                 FVONZ 35
      RETURN                                                            FVONZ 36
      END                                                               FVONZ 37
      FUNCTION f0(jhig,hmass)                                           F0     2
C--------------------------------------------------------------------   F0     3
C! Utility routine to compute gamma gamma and gluon gluon widths        F0     4
C                                                                       F0     5
C Input:    jhig,   Higgs type                                          F0     6
C           hmass,  spin 0 boson mass                                   F0     7
C                                                                       F0     8
C Output:   f0,    the value of the function                            F0     9
C                                                                       F0    10
C Patrick Janot -- 03 oct 1991                                          F0    11
C--------------------------------------------------------------------   F0    12
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      COMPLEX ftau, f0                                                  F0    14
      EXTERNAL ftau                                                     F0    15
C                                                                       F0    16
      tau = 4.*hmass**2/amhig(jhig)**2                                  F0    17
      f0 = tau*(1.-tau*ftau(tau))                                       F0    18
C                                                                       F0    19
      RETURN                                                            F0    20
      END                                                               F0    21
      FUNCTION f1(jhig,bmass)                                           F1     2
C--------------------------------------------------------------------   F1     3
C! Utility routine to compute gamma gamma and gluon gluon widths        F1     4
C                                                                       F1     5
C Input:    jhig,   Higgs type                                          F1     6
C           bmass,  spin 1 boson mass                                   F1     7
C                                                                       F1     8
C Output:   f1,    the value of the function                            F1     9
C                                                                       F1    10
C Patrick Janot -- 03 oct 1991                                          F1    11
C--------------------------------------------------------------------   F1    12
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      COMPLEX ftau, f1                                                  F1    14
      EXTERNAL ftau                                                     F1    15
C                                                                       F1    16
      tau = 4.*bmass**2/amhig(jhig)**2                                  F1    17
      f1 = 2. + 3.*tau + 3.*tau*(2.-tau)*ftau(tau)                      F1    18
C                                                                       F1    19
      RETURN                                                            F1    20
      END                                                               F1    21
      FUNCTION f12(jhig,fmass)                                          F12    2
C--------------------------------------------------------------------   F12    3
C! Utility routine to compute gamma gamma and gluon gluon widths        F12    4
C                                                                       F12    5
C Input:    jhig,   Higgs type                                          F12    6
C           fmass,  fermion mass                                        F12    7
C                                                                       F12    8
C Output:   f12,    the value of the function                           F12    9
C                                                                       F12   10
C Patrick Janot -- 03 oct 1991                                          F12   11
C--------------------------------------------------------------------   F12   12
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)        DATAPI 2
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          DATAPI 3
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         DATAPI 4
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         DATAPI 5
C                                                                       DATAPI 6
      COMPLEX ftau, f12                                                 F12   15
      EXTERNAL ftau                                                     F12   16
C                                                                       F12   17
      IF ( jhig .LE. 2 ) epsh = 1.                                      F12   18
      IF ( jhig .EQ. 3 ) epsh = 0.                                      F12   19
      tau = 4.*fmass**2/amhig(jhig)**2                                  F12   20
      f12 = -2.*tau * (epsh + (1.-tau*epsh)*ftau(tau))                  F12   21
C                                                                       F12   22
      RETURN                                                            F12   23
      END                                                               F12   24
      FUNCTION gcaren(X,Y)                                              GCAREN 2
C--------------------------------------------------------------------   GCAREN 3
C! gcaren(x,y) = 2. - (x+y)/(x-y)*LOG(x/y)                              GCAREN 4
C                                                                       GCAREN 5
C  20 Sep 1995                                                          GCAREN 6
C--------------------------------------------------------------------   GCAREN 7
      IMPLICIT REAL*8(A-H,L,M,O-Z)                                      GCAREN 8
      gcaren = 2. - (x+y)/(x-y)*LOG(x/y)                                GCAREN 9
      RETURN                                                            GCAREN10
      END                                                               GCAREN11
      FUNCTION gf1(x,y)                                                 GF1    2
C--------------------------------------------------------------------   GF1    3
C! gf1(x,y) = log(x/y)/(x-y)                                            GF1    4
C                                                                       GF1    5
C  3 Dec 1994                                                           GF1    6
C--------------------------------------------------------------------   GF1    7
      u = x/y                                                           GF1    8
      e = u-1.                                                          GF1    9
      IF ( ABS(e) .GT. 1.E-6 ) THEN                                     GF1   10
        gf1 = log(u)/(u-1) /y                                           GF1   11
      ELSE                                                              GF1   12
        gf1 = (1.-e/2.+e**2/3.) /y                                      GF1   13
      ENDIF                                                             GF1   14
      END                                                               GF1   15
      FUNCTION gf2(x,y)                                                 GF2    2
C--------------------------------------------------------------------   GF2    3
C! gf2 = (2-(x+y)/(x-y)*log(x/y))/(x-y)**2                              GF2    4
C                                                                       GF2    5
C  3 Dec 1994                                                           GF2    6
C--------------------------------------------------------------------   GF2    7
      u = x/y                                                           GF2    8
      e = u-1.                                                          GF2    9
      IF ( ABS(e) .GT. 1.E-4 ) THEN                                     GF2   10
        gf2 = (2.-(u+1)/(u-1)*log(u))/(u-1)**2 /y**2                    GF2   11
      ELSE                                                              GF2   12
        gf2 = -(1.-e)/6. /y**2                                          GF2   13
      ENDIF                                                             GF2   14
      END                                                               GF2   15
      FUNCTION gf3(x,y,z)                                               GF3    2
C--------------------------------------------------------------------   GF3    3
C! gf3(x,y) = [x.gf1(x,z) - y.gf1(y,z)]/(x-y)                           GF3    4
C                                                                       GF3    5
C  3 Dec 1994                                                           GF3    6
C--------------------------------------------------------------------   GF3    7
      u = x/y                                                           GF3    8
      e = u-1.                                                          GF3    9
      IF ( ABS(e) .GT. 1.E-6 ) THEN                                     GF3   10
       gf3 = (u*gf1(x,z)-gf1(y,z))/(u-1.)                               GF3   11
      ELSE                                                              GF3   12
        v = x/z                                                         GF3   13
        f = v-1.                                                        GF3   14
        IF ( ABS(f) .GT. 1.E-6 ) THEN                                   GF3   15
          gf3 = (1.-z*gf1(x,z))/(x-z)                                   GF3   16
        ELSE                                                            GF3   17
          gf3 = (1./2.-v/3.)/z                                          GF3   18
        ENDIF                                                           GF3   19
      ENDIF                                                             GF3   20
      END                                                               GF3   21
      SUBROUTINE GFUNCAR(ma,tanb,mq,mur,md,mtop,At,Ab,mu,vh,            GFUNCAR2
     *                stop1,stop2)                                      GFUNCAR3
C--------------------------------------------------------------------   GFUNCAR4
C! Compute D-terms contributing to the Higgs masses                     GFUNCAR5
C                                                                       GFUNCAR6
C  From M. Carena and C. Wagner, 21 Sep 1995                            GFUNCAR7
C--------------------------------------------------------------------   GFUNCAR8
      IMPLICIT REAL*8 (A-H,L,M,O-Z)                                     GFUNCAR9
      REAL*8 cma,ctb,cmq,cmur,cmdr,cmtop,cau,cad,cmu,cmchi              CCAREN 2
      REAL*8 cmh,cmhp,chm,chmp,camp,cmhch,csa,cca                       CCAREN 3
      REAL*8 cstop1,cstop2,csbot1,csbot2,ctanbA                         CCAREN 4
      REAL*8 rmtop,rmbot,mz,alpha_1,alpha_2,alpha_3,v,ppi,sint,stw      CCAREN 5
      COMMON / mcarena / rmtop,rmbot,mz,alpha_1,alpha_2,alpha_3,        CCAREN 6
     .                   v,ppi,sint,stw                                 CCAREN 7
C                                                                       CCAREN 8
      DIMENSION DIAH(2),VH(2,2),VH1(2,2),VH2(2,2),                      GFUNCA11
     *          vh3t(2,2),vh3b(2,2),                                    GFUNCA12
     *          hmix(2,2),al(2,2),m2(2,2)                               GFUNCA13
                                                                        GFUNCA14
      if(dabs(mu).lt.0.000001) mu = 0.000001                            GFUNCA15
      mq2 = mq**2                                                       GFUNCA16
      mur2 = mur**2                                                     GFUNCA17
      md2 = md**2                                                       GFUNCA18
      tanbA = tanb                                                      GFUNCA19
      sinbA = tanbA/(tanbA**2+1.)**.5                                   GFUNCA20
      cosbA = sinbA/tanbA                                               GFUNCA21
                                                                        GFUNCA22
      sinb = tanb/(tanb**2+1.)**.5                                      GFUNCA23
      cosb = sinb/tanb                                                  GFUNCA24
Cpaj  pi = 3.14159                                                      GFUNCA25
Cpaj  g2 = (0.0336*4.*pi)**.5                                           GFUNCA26
Cpaj  g12 = (0.0101*4.*pi)                                              GFUNCA27
Cpaj  g1 = g12**.5                                                      GFUNCA28
Cpaj  mz = 91.18                                                        GFUNCA29
Cpaj  v = 174.1                                                         GFUNCA30
Cpaj  mw = (g2**2*v**2/2.)**.5                                          GFUNCA31
Cpaj  alpha3 = 0.12/(1.+23/12./pi*0.12*log(mtop**2/mz**2))              GFUNCA32
Cpaj  mb = 3.                                                           GFUNCA33
      pi = ppi                 ! paj                                    GFUNCA34
      mb = rmbot               ! paj                                    GFUNCA35
      alpha1 = alpha_1         ! paj                                    GFUNCA36
      alpha2 = alpha_2         ! paj                                    GFUNCA37
      alpha3 = alpha_3         ! paj                                    GFUNCA38
      g1 = (alpha1*4.*pi)**.5  ! paj                                    GFUNCA39
      g2 = (alpha2*4.*pi)**.5  ! paj                                    GFUNCA40
      g3 = (alpha3*4.*pi)**.5  ! paj                                    GFUNCA41
      g12 = g1**2                                                       GFUNCA42
      g32 = g3**2                                                       GFUNCA43
      if(mq.gt.mur) mst = mq                                            GFUNCA44
      if(mur.gt.mq.or.mur.eq.mq) mst = mur                              GFUNCA45
                                                                        GFUNCA46
      msusyt = (mst**2  + mtop**2)**.5                                  GFUNCA47
                                                                        GFUNCA48
      if(mq.gt.md) msb = mq                                             GFUNCA49
      if(md.gt.mq.or.md.eq.mq) msb = md                                 GFUNCA50
                                                                        GFUNCA51
      msusyb = (msb**2 + mb**2)**.5                                     GFUNCA52
                                                                        GFUNCA53
      tt = log(msusyt**2/mtop**2)                                       GFUNCA54
      tb = log(msusyb**2/mtop**2)                                       GFUNCA55
                                                                        GFUNCA56
Cpaj  rmtop = mtop/(1.+4.*alpha3/3./pi)                                 GFUNCA57
Cpaj  ht = rmtop/(174.*sinb)                                            GFUNCA58
Cpaj  htst = rmtop/174.1                                                GFUNCA59
Cpaj  hb = mb/174./cosb                                                 GFUNCA60
Cpaj  g32 = alpha3*4.*pi                                                GFUNCA61
      ht = rmtop/( v  *sinb)   ! paj                                    GFUNCA62
      htst = rmtop/ v          ! paj                                    GFUNCA63
      hb = mb/ v  /cosb        ! paj                                    GFUNCA64
      bt2 = -(8.*g32 - 9.*ht**2/2. - hb**2/2.)/(4.*pi)**2               GFUNCA65
      bb2 = -(8.*g32 - 9.*hb**2/2. - ht**2/2.)/(4.*pi)**2               GFUNCA66
      al2 = 3./8./pi**2*ht**2                                           GFUNCA67
      bt2st = -(8.*g32 - 9.*htst**2/2.)/(4.*pi)**2                      GFUNCA68
      alst = 3./8./pi**2*htst**2                                        GFUNCA69
      al1 = 3./8./pi**2*hb**2                                           GFUNCA70
                                                                        GFUNCA71
      al(1,1) = al1                                                     GFUNCA72
      al(1,2) = (al2+al1)/2.                                            GFUNCA73
      al(2,1) = (al2+al1)/2.                                            GFUNCA74
      al(2,2) = al2                                                     GFUNCA75
                                                                        GFUNCA76
      mtop4 = rmtop**4.*(1.+2.*bt2*tt- al2*tt)                          GFUNCA77
      mtop2 = mtop4**.5                                                 GFUNCA78
      mbot4 = mb**4.*(1.+2.*bb2*tb - al1*tb)                            GFUNCA79
      mbot2 = mbot4**.5                                                 GFUNCA80
                                                                        GFUNCA81
Cpaj  vi = 174.1*(1. + 3./32./pi**2*htst**2*                            GFUNCA82
      vi =   v  *(1. + 3./32./pi**2*htst**2*                            GFUNCA83
     *log(mtop**2/ma**2))                                               GFUNCA84
      h1i = vi* cosbA                                                   GFUNCA85
      h2i = vi*sinbA                                                    GFUNCA86
      h1t = h1i*(1.+3./8./pi**2*hb**2*log(ma**2/msusyt**2))**.25        GFUNCA87
      h2t = h2i*(1.+3./8./pi**2*ht**2*log(ma**2/msusyt**2))**.25        GFUNCA88
      h1b = h1i*(1.+3./8./pi**2*hb**2*log(ma**2/msusyb**2))**.25        GFUNCA89
      h2b = h2i*(1.+3./8./pi**2*ht**2*log(ma**2/msusyb**2))**.25        GFUNCA90
                                                                        GFUNCA91
      tanbst = h2t/h1t                                                  GFUNCA92
      sinbt = tanbst/(1.+tanbst**2)**.5                                 GFUNCA93
      cosbt = sinbt/tanbst                                              GFUNCA94
                                                                        GFUNCA95
      tanbsb = h2b/h1b                                                  GFUNCA96
      sinbb = tanbsb/(1.+tanbsb**2)**.5                                 GFUNCA97
      cosbb = sinbb/tanbsb                                              GFUNCA98
                                                                        GFUNCA99
      stop12 = (mq2 + mur2)*.5 + mtop2                                  GFUNC100
     *  +1./8.*(g2**2+g1**2)*(h1t**2-h2t**2)                            GFUNC101
     *   +(((g2**2-5.*g1**2/3.)/4.*(h1t**2-h2t**2) +                    GFUNC102
     *   mq2 - mur2)**2*0.25 + mtop2*(At-mu/tanbst)**2)**.5             GFUNC103
      stop22 = (mq2 + mur2)*.5 + mtop2                                  GFUNC104
     *  +1./8.*(g2**2+g1**2)*(h1t**2-h2t**2)                            GFUNC105
     *   - (((g2**2-5.*g1**2/3.)/4.*(h1t**2-h2t**2) +                   GFUNC106
     *  mq2 - mur2)**2*0.25                                             GFUNC107
     *  + mtop2*(At-mu/tanbst)**2)**.5                                  GFUNC108
      if(stop22.lt.0.) goto 4237                                        GFUNC109
      sbot12 = (mq2 + md2)*.5                                           GFUNC110
     *   - 1./8.*(g2**2+g1**2)*(h1b**2-h2b**2)                          GFUNC111
     *  + (((g1**2/3.-g2**2)/4.*(h1b**2-h2b**2) +                       GFUNC112
     *  mq2 - md2)**2*0.25 + mbot2*(Ab-mu*tanbsb)**2)**.5               GFUNC113
      sbot22 = (mq2 + md2)*.5                                           GFUNC114
     *   - 1./8.*(g2**2+g1**2)*(h1b**2-h2b**2)                          GFUNC115
     *   - (((g1**2/3.-g2**2)/4.*(h1b**2-h2b**2) +                      GFUNC116
     *   mq2 - md2)**2*0.25 + mbot2*(Ab-mu*tanbsb)**2)**.5              GFUNC117
      if(sbot22.lt.0.) goto 4237                                        GFUNC118
                                                                        GFUNC119
      stop1 = stop12**.5                                                GFUNC120
      stop2 = stop22**.5                                                GFUNC121
      sbot1 = sbot12**.5                                                GFUNC122
      sbot2 = sbot22**.5                                                GFUNC123
C                                                                       GFUNC124
      vh1(1,1) = 1./tanbst                                              GFUNC125
      vh1(2,1) = -1.                                                    GFUNC126
      vh1(1,2) = -1.                                                    GFUNC127
      vh1(2,2) = tanbst                                                 GFUNC128
      vh2(1,1) = tanbst                                                 GFUNC129
      vh2(1,2) = -1.                                                    GFUNC130
      vh2(2,1) = -1.                                                    GFUNC131
      vh2(2,2) = 1./tanbst                                              GFUNC132
cccccccccccccccccccccccccccccccc                                        GFUNC133
ccc   D-terms                                                           GFUNC134
cccccccccccccccccccccccccccccccc                                        GFUNC135
Cpaj  stw=.2315                                                         GFUNC136
                                                                        GFUNC137
      f1t=(mq2-mur2)/(stop12-stop22)*(.5-4./3.*stw)*                    GFUNC138
     *         log(stop1/stop2)                                         GFUNC139
     *        +(.5-2./3.*stw)*log(stop1*stop2/(mq2+mtop2))              GFUNC140
     *        + 2./3.*stw*log(stop1*stop2/(mur2+mtop2))                 GFUNC141
                                                                        GFUNC142
      f1b=(mq2-md2)/(sbot12-sbot22)*(-.5+2./3.*stw)*                    GFUNC143
     *        log(sbot1/sbot2)                                          GFUNC144
     *        +(-.5+1./3.*stw)*log(sbot1*sbot2/(mq2+mbot2))             GFUNC145
     *        - 1./3.*stw*log(sbot1*sbot2/(md2+mbot2))                  GFUNC146
                                                                        GFUNC147
      f2t=mtop2**.5*(at-mu/tanbst)/(stop12-stop22)*                     GFUNC148
     *         (-.5*log(stop12/stop22)                                  GFUNC149
     *        +(4./3.*stw-.5)*(mq2-mur2)/(stop12-stop22)*               GFUNC150
     *         gcaren(stop12,stop22))                                   GFUNC151
                                                                        GFUNC152
      f2b=mbot2**.5*(ab-mu*tanbsb)/(sbot12-sbot22)*                     GFUNC153
     *         (.5*log(sbot12/sbot22)                                   GFUNC154
     *        +(-2./3.*stw+.5)*(mq2-md2)/(sbot12-sbot22)*               GFUNC155
     *        gcaren(sbot12,sbot22))                                    GFUNC156
                                                                        GFUNC157
      vh3b(1,1) = mbot4/(cosbb**2)*(log(sbot1**2*sbot2**2/              GFUNC158
     *  (mq2+mbot2)/(md2+mbot2))                                        GFUNC159
     *  + 2.*(Ab*(Ab-mu*tanbsb)/(sbot1**2-sbot2**2))*                   GFUNC160
     *  log(sbot1**2/sbot2**2)) +                                       GFUNC161
     *  Mbot4/(cosbb**2)*(Ab*(Ab-mu*tanbsb)/                            GFUNC162
     *  (sbot1**2-sbot2**2))**2*gcaren(sbot12,sbot22)                   GFUNC163
                                                                        GFUNC164
      vh3t(1,1) =                                                       GFUNC165
     *  mtop4/(sinbt**2)*(mu*(-At+mu/tanbst)/(stop1**2                  GFUNC166
     * -stop2**2))**2*gcaren(stop12,stop22)                             GFUNC167
                                                                        GFUNC168
      vh3b(1,1)=vh3b(1,1)+                                              GFUNC169
     *    mz**2*(2*mbot2*f1b-mbot2**.5*ab*f2b)                          GFUNC170
                                                                        GFUNC171
      vh3t(1,1) = vh3t(1,1) +                                           GFUNC172
     *  mz**2*(mtop2**.5*mu/tanbst*f2t)                                 GFUNC173
                                                                        GFUNC174
      vh3t(2,2) = mtop4/(sinbt**2)*(log(stop1**2*stop2**2/              GFUNC175
     *  (mq2+mtop2)/(mur2+mtop2))                                       GFUNC176
     *  + 2.*(At*(At-mu/tanbst)/(stop1**2-stop2**2))*                   GFUNC177
     *  log(stop1**2/stop2**2)) +                                       GFUNC178
     *  mtop4/(sinbt**2)*(At*(At-mu/tanbst)/                            GFUNC179
     *  (stop1**2-stop2**2))**2*gcaren(stop12,stop22)                   GFUNC180
                                                                        GFUNC181
      vh3b(2,2) =                                                       GFUNC182
     *  Mbot4/(cosbb**2)*(mu*(-Ab+mu*tanbsb)/(sbot1**2                  GFUNC183
     * -sbot2**2))**2*gcaren(sbot12,sbot22)                             GFUNC184
                                                                        GFUNC185
      vh3t(2,2)=vh3t(2,2)+                                              GFUNC186
     *    mz**2*(-2*mtop2*f1t+mtop2**.5*at*f2t)                         GFUNC187
                                                                        GFUNC188
      vh3b(2,2) = vh3b(2,2) -mz**2*mbot2**.5*mu*tanbsb*f2b              GFUNC189
                                                                        GFUNC190
      vh3t(1,2) = -                                                     GFUNC191
     *   mtop4/(sinbt**2)*mu*(At-mu/tanbst)/                            GFUNC192
     * (stop1**2-stop2**2)*(log(stop1**2/stop2**2) + At*                GFUNC193
     * (At - mu/tanbst)/(stop1**2-stop2**2)*gcaren(stop12,stop22))      GFUNC194
                                                                        GFUNC195
      vh3b(1,2) =                                                       GFUNC196
     * - mbot4/(cosbb**2)*mu*(At-mu*tanbsb)/                            GFUNC197
     * (sbot1**2-sbot2**2)*(log(sbot1**2/sbot2**2) + Ab*                GFUNC198
     * (Ab - mu*tanbsb)/(sbot1**2-sbot2**2)*gcaren(sbot12,sbot22))      GFUNC199
                                                                        GFUNC200
      vh3t(1,2)=vh3t(1,2) +                                             GFUNC201
     *      mz**2*(mtop2/tanbst*f1t-mtop2**.5*(at/tanbst+mu)/2.*f2t)    GFUNC202
                                                                        GFUNC203
      vh3b(1,2)=vh3b(1,2)                                               GFUNC204
     *  +mz**2*(-mbot2*tanbsb*f1b+mbot2**.5*(ab*tanbsb+mu)/2.*f2b)      GFUNC205
                                                                        GFUNC206
      vh3t(2,1) = vh3t(1,2)                                             GFUNC207
      vh3b(2,1) = vh3b(1,2)                                             GFUNC208
                                                                        GFUNC209
      tq = log((mq2 + mtop2)/mtop2)                                     GFUNC210
      tu = log((mur2+mtop2)/mtop2)                                      GFUNC211
      tqd = log((mq2 + mb**2)/mb**2)                                    GFUNC212
      td = log((md2+mb**2)/mb**2)                                       GFUNC213
                                                                        GFUNC214
                                                                        GFUNC215
      DO 8910 I = 1,2                                                   GFUNC216
      DO 8911 J = 1,2                                                   GFUNC217
        vh(i,j) =                                                       GFUNC218
     *  6./(8.*pi**2*(h1t**2+h2t**2))                                   GFUNC219
     *  *vh3t(i,j)*0.5*(1.-al(i,j)*tt/2.) +                             GFUNC220
     *  6./(8.*pi**2*(h1b**2+h2b**2))                                   GFUNC221
     *  *vh3b(i,j)*0.5*(1.-al(i,j)*tb/2.)                               GFUNC222
 8911 CONTINUE                                                          GFUNC223
 8910 CONTINUE                                                          GFUNC224
                                                                        GFUNC225
      GOTO 4236                                                         GFUNC226
 4237 DO 6868 I =1,2                                                    GFUNC227
      DO 6867 J = 1,2                                                   GFUNC228
        vh(i,j) = -1.d+15                                               GFUNC229
 6867 CONTINUE                                                          GFUNC230
 6868 CONTINUE                                                          GFUNC231
C                                                                       GFUNC232
 4236 RETURN                                                            GFUNC233
      END                                                               GFUNC234
      SUBROUTINE gsub3(m,u3,f3,x)                                       GSUB0122
C--------------------------------------------------------------------   GSUB0123
C! Three routines for the WW/ZZ-->h cross section including h width     GSUB0124
C                                                                       GSUB0125
C  Patrick Janot -- 01 Sep 1995                                         GSUB0126
C--------------------------------------------------------------------   GSUB0127
      IMPLICIT REAL*8(A-H,K,O-Z)                                        GSUB0128
      COMMON / sigww / kappah, kappav, v, a, rmh, rgh, ss               GSUB0129
      EXTERNAL gsub2                                                    GSUB0110
      DIMENSION u3(*),f3(*),x(3)                                        GSUB0111
C                                                                       GSUB0112
      DO l = 1,m                                                        GSUB0113
        x(3)  = u3(l)                                                   GSUB0114
        u = DSQRT(rmh**2+rmh*rgh*DTAN(x(3)))                            GSUB0115
        kappah = u**2/ss                                                GSUB0116
        x2lo  = kappah                                                  GSUB0117
        x2hi  = 1.                                                      GSUB0118
        f3(l) = DGMLT2(gsub2,x2lo,x2hi,1,6,x)                           GSUB0119
      ENDDO                                                             GSUB0120
C                                                                       GSUB0121
      RETURN                                                            GSUB0122
      END                                                               GSUB0123
      SUBROUTINE gsub2(m,u2,f2,x)                                       GSUB0124
      IMPLICIT REAL*8(A-H,K,O-Z)                                        GSUB0125
      EXTERNAL gsub1                                                    GSUB0126
      DIMENSION  u2(*),f2(*),x(3)                                       GSUB0127
C                                                                       GSUB0128
      DO l = 1,m                                                        GSUB0129
        x(2)  = u2(l)                                                   GSUB0130
        x1lo  = 1.                                                      GSUB0131
        x1hi  = 1./x(2)**2                                              GSUB0132
        f2(l) = DGMLT1(gsub1,x1lo,x1hi,1,6,x)                           GSUB0133
      ENDDO                                                             GSUB0134
C                                                                       GSUB0135
      RETURN                                                            GSUB0136
      END                                                               GSUB0137
      SUBROUTINE gsub1(m,u1,f1,x)                                       GSUB0138
      IMPLICIT REAL*8(A-H,K,O-Z)                                        GSUB0139
      COMMON / sigww / kappah, kappav, v, a, rmh, rgh, ss               GSUB0140
      DIMENSION u1(*),f1(*),x(3)                                        GSUB0141
C                                                                       GSUB0142
      DO l = 1,m                                                        GSUB0143
        x(1) = u1(l)                                                    GSUB0144
        y = 1./DSQRT(x(1))                                              GSUB0145
        z = y * (x(2)-kappah) / (kappav * x(2))                         GSUB0146
        f = ( 2.*x(2)/y**3 - (1.+2.*x(2))/y**2                          GSUB0147
     .     + (2.+x(2))/(2.*y) - 1./2. )                                 GSUB0148
     .    * ( z/(1.+z) - DLOG(1.+z) )                                   GSUB0149
     .    + x(2)*z**2*(1.-y)/(y**3*(1.+z))                              GSUB0150
        g = (-x(2)/y**2 + (2.+x(2))/(2.*y) - 1./2.)                     GSUB0151
     .    * ( z/(1.+z) - DLOG(1.+z) )                                   GSUB0152
        f1(l) = ( (v**2+a**2)**2 * f + 4.*v**2*a**2 * g )               GSUB0153
     .        / ( 1. + (y-x(2))/kappav )**2                             GSUB0154
     .        * y**3 / 2.                                               GSUB0155
      ENDDO                                                             GSUB0156
C                                                                       GSUB0157
      RETURN                                                            GSUB0158
      END                                                               GSUB0159
      FUNCTION gtau(tau)                                                GTAU   2
C------------------------------------------------------------------     GTAU   3
C! Famous function appearing in H --> Z gamma partial width             GTAU   4
C                                                                       GTAU   5
C  Patrick Janot -- 23 Sep 1995                                         GTAU   6
C------------------------------------------------------------------     GTAU   7
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)        DATAPI 2
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          DATAPI 3
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         DATAPI 4
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         DATAPI 5
C                                                                       DATAPI 6
      COMPLEX gtau                                                      GTAU   9
C                                                                       GTAU  10
      IF ( tau .GE. 1 ) THEN                                            GTAU  11
        gtaur = SQRT(tau-1.) * ASIN(SQRT(1./tau))                       GTAU  12
        gtaui = 0.                                                      GTAU  13
      ELSEIF ( tau .GE. 1E-5 ) THEN                                     GTAU  14
        b0 = SQRT(1.-tau)                                               GTAU  15
        gtaur = b0 * ALOG((1.+b0)/(1-b0)) / 2.                          GTAU  16
        gtaui = -pi                                                     GTAU  17
      ELSE                                                              GTAU  18
        gtaur = -ALOG(tau/4.) / 2.                                      GTAU  19
        gtaui = -pi                                                     GTAU  20
      ENDIF                                                             GTAU  21
C                                                                       GTAU  22
      gtau = CMPLX(gtaur,gtaui)                                         GTAU  23
C                                                                       GTAU  24
      RETURN                                                            GTAU  25
      END                                                               GTAU  26
      FUNCTION gzi1(jhig,am)                                            GZI1   2
C--------------------------------------------------------------------   GZI1   3
C! Utility routine to compute the Z gamma width                         GZI1   4
C                                                                       GZI1   5
C Input:    jhig,   Higgs type                                          GZI1   6
C           am  ,   the mass of the the loop particle                   GZI1   7
C                                                                       GZI1   8
C Output:   gzi1,   the value of the function                           GZI1   9
C                                                                       GZI1  10
C Patrick Janot -- 23 sep 1995                                          GZI1  11
C--------------------------------------------------------------------   GZI1  12
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      COMPLEX ftau, gtau, gzi1                                          GZI1  14
      EXTERNAL ftau, gtau                                               GZI1  15
C                                                                       GZI1  16
      a = 4.*am**2/amhig(jhig)**2                                       GZI1  17
      b = 4.*am**2/amz**2                                               GZI1  18
C                                                                       GZI1  19
      gzi1 = a*b       / 2. / (a-b)                                     GZI1  20
     .     + a**2*b**2 / 2. / (a-b)**2 * (ftau(a)-ftau(b))              GZI1  21
     .     + a**2*b         / (a-b)**2 * (gtau(a)-gtau(b))              GZI1  22
C                                                                       GZI1  23
      RETURN                                                            GZI1  24
      END                                                               GZI1  25
      FUNCTION gzi2(jhig,am)                                            GZI2   2
C--------------------------------------------------------------------   GZI2   3
C! Utility routine to compute the Z gamma width                         GZI2   4
C                                                                       GZI2   5
C Input:    jhig,   Higgs type                                          GZI2   6
C           am  ,   the mass of the the loop particle                   GZI2   7
C                                                                       GZI2   8
C Output:   gzi2,   the value of the function                           GZI2   9
C                                                                       GZI2  10
C Patrick Janot -- 23 sep 1995                                          GZI2  11
C--------------------------------------------------------------------   GZI2  12
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      COMPLEX ftau, gtau, gzi2                                          GZI2  14
      EXTERNAL ftau, gtau                                               GZI2  15
C                                                                       GZI2  16
      a = 4.*am**2/amhig(jhig)**2                                       GZI2  17
      b = 4.*am**2/amz**2                                               GZI2  18
C                                                                       GZI2  19
      gzi2 = -a*b       / 2. / (a-b)    * (ftau(a)-ftau(b))             GZI2  20
C                                                                       GZI2  21
      RETURN                                                            GZI2  22
      END                                                               GZI2  23
      SUBROUTINE hhdecay(ph,jchan,jhig,iph)                             HHDECAY2
C--------------------------------------------------------------------   HHDECAY3
C! Main routine to decay Higgses of the MSSM.                           HHDECAY4
C  The Higgs is decayed in its rest frame. Then, a boost is performed   HHDECAY5
C  for going back to the lab.                                           HHDECAY6
C                                                                       HHDECAY7
C  Input:     Passed:   --PH(4),   Higgs quadriimpulsion (in the lab)   HHDECAY8
C                                  with the format :                    HHDECAY9
C                                  PH(1,2,3)    momentum (in GeV)       HHDECA10
C                             -->  PH(4)        Mass        "           HHDECA11
C                       --JHIG,    higgs type ( 1:H, 2:h, 3:A )         HHDECA12
C                       --IPH,     (optional) Row number in             HHDECA13
C                                  the LUJETS common block              HHDECA14
C  Output:    Passed:   --JCHAN,   channel selected                     HHDECA15
C                                                                       HHDECA16
C   P. Janot  --  26 Aug 1991                                           HHDECA17
C-----------------------------------------------------------------------HHDECA18
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      CHARACTER*14 channel                                              HHDECK 2
      CHARACTER*21 channeut, chanchar                                   HHDECK 3
      PARAMETER(nchneut=8,nchchar=5)                                    HHDECK 4
      COMMON / hhdeck / branch(nchan,nhig),width(nhig),                 HHDECK 5
     .                  parwth(nhig),xymas(2,nchan,nhig),               HHDECK 6
     .                  xywid(2,nchan,nhig)                             HHDECK 7
      COMMON / chaneu / ichn(2),                                        HHDECK 8
     .                  wneut(4,4,nhig), wchar(2,2,nhig),               HHDECK 9
     .                  widneut(4), brneut(nchneut,4),                  HHDECK10
     .                  widchar(2), brchar(nchchar,2)                   HHDECK11
      COMMON / chanch / channeut(nchneut,4), chanchar(nchchar,2)        HHDECK12
      COMMON / vect4 / pvect4(5,2)                                      HHDECK13
      COMMON / hinput / ichan(nchan,nhig), channel(nchan,nhig)          HHDECK14
      COMMON / hisbr / bchpp,bchgg                                      HHDECK15
      DIMENSION ph(4)                                                   HHDECK16
C                                                                       HHDECK17
      COMMON / h0evt / kevt(nchan,nhig)                                 COUNTS 2
C                                                                       COUNTS 3
      DIMENSION brloc(nchan,nhig)                                       HHDECA22
      DIMENSION wnloc(4,4,nhig),wcloc(2,2,nhig)                         HHDECA23
C                                                                       HHDECA24
      IF ( jhig .LE. 0 .OR. jhig .GT. nhig ) THEN                       HHDECA25
        WRITE(6,*) '+++ HHDECAY +++ Wrong Higgs number'                 HHDECA26
        STOP 99                                                         HHDECA27
      ENDIF                                                             HHDECA28
C                                                                       HHDECA29
      IF ( parwth(jhig) .EQ. 0.) THEN                                   HHDECA30
        WRITE(6,1000) jhig                                              HHDECA31
        STOP 99                                                         HHDECA32
      ENDIF                                                             HHDECA33
C                                                                       HHDECA34
C  Simply get rid of the decay channels suddenly closed due             HHDECA35
C  to too low a value of the generated Higgs mass (due to the           HHDECA36
C  decay width)                                                         HHDECA37
C                                                                       HHDECA38
      CALL ucopy(branch(1,1),brloc(1,1),nchan*nhig)                     HHDECA39
      CALL ucopy(wneut(1,1,1),wnloc(1,1,1),4*4*nhig)                    HHDECA40
      CALL ucopy(wchar(1,1,1),wcloc(1,1,1),2*2*nhig)                    HHDECA41
C                                                                       HHDECA42
      IF ( ph(4) .LT. 1.       ) branch( 2,jhig) = 0.                   HHDECA43
      IF ( ph(4) .LT. 2.*amtau ) branch( 3,jhig) = 0.                   HHDECA44
      IF ( ph(4) .LT. 2.*amc   ) branch( 4,jhig) = 0.                   HHDECA45
      IF ( ph(4) .LT. 2.*amb   ) branch( 5,jhig) = 0.                   HHDECA46
      IF ( ph(4) .LT. 2.*amt   ) branch( 6,jhig) = 0.                   HHDECA47
      IF ( ph(4) .LT. 20.      ) branch( 7,jhig) = 0.                   HHDECA48
      IF ( ph(4) .LT. 20.      ) branch( 8,jhig) = 0.                   HHDECA49
      IF ( ph(4) .LT. 2.*ama  .AND. jhig .LE. 2 )                       HHDECA50
     .                           branch( 9,jhig) = 0.                   HHDECA51
      IF ( ph(4) .LT. amh+amz  ) branch( 9,3   ) = 0.                   HHDECA52
      IF ( ph(4) .LT. 2.*amh   ) branch(10,jhig) = 0.                   HHDECA53
      IF ( ph(4) .LT. 20.      ) branch(11,jhig) = 0.                   HHDECA54
      IF ( ph(4) .LT. 2*ame    ) branch(12,jhig) = 0.                   HHDECA55
      IF ( ph(4) .LT. 2*ammu   ) branch(13,jhig) = 0.                   HHDECA56
      IF ( ph(4) .LT. 2*ams    ) branch(14,jhig) = 0.                   HHDECA57
C                                                                       HHDECA58
      IF ( branch(15,jhig) .GT. 0. ) THEN                               HHDECA59
        frn = 0.                                                        HHDECA60
        DO i = 1, 4                                                     HHDECA61
          DO j = 1, 4                                                   HHDECA62
            IF ( ph(4).LT.amneut(i)+amneut(j) ) THEN                    HHDECA63
              wneut(i,j,jhig) = 0.                                      HHDECA64
            ELSE                                                        HHDECA65
              frn = frn + wneut(i,j,jhig)                               HHDECA66
            ENDIF                                                       HHDECA67
          ENDDO                                                         HHDECA68
        ENDDO                                                           HHDECA69
        branch(15,jhig) = branch(15,jhig) * frn                         HHDECA70
      ENDIF                                                             HHDECA71
C                                                                       HHDECA72
      IF ( branch(16,jhig) .GT. 0. ) THEN                               HHDECA73
        frc = 0.                                                        HHDECA74
        DO i = 1, 2                                                     HHDECA75
          DO j = 1, 2                                                   HHDECA76
            IF ( ph(4).LT.amchar(i)+amchar(j) ) THEN                    HHDECA77
              wchar(i,j,jhig) = 0.                                      HHDECA78
            ELSE                                                        HHDECA79
              frc = frc + wchar(i,j,jhig)                               HHDECA80
            ENDIF                                                       HHDECA81
          ENDDO                                                         HHDECA82
        ENDDO                                                           HHDECA83
        branch(16,jhig) = branch(16,jhig) * frc                         HHDECA84
      ENDIF                                                             HHDECA85
C                                                                       HHDECA86
      frch = 0.                                                         HHDECA87
      DO ich = 1, nchan                                                 HHDECA88
        frch = frch + branch(ich,jhig)                                  HHDECA89
      ENDDO                                                             HHDECA90
C                                                                       HHDECA91
C  Choice of the channel.                                               HHDECA92
C                                                                       HHDECA93
      IF ( frch .EQ. 0. ) THEN                                          HHDECA94
        jchan = 1                                                       HHDECA95
         GOTO 30                                                        HHDECA96
      ENDIF                                                             HHDECA97
C                                                                       HHDECA98
      rnch = RNDM(dummy)                                                HHDECA99
      rint = 0.D0                                                       HHDEC100
      DO jchan = 1 , nchan                                              HHDEC101
        rint = rint + branch(jchan,jhig)/frch                           HHDEC102
        if ( rnch .lt. rint ) GOTO 30                                   HHDEC103
      ENDDO                                                             HHDEC104
  30  CONTINUE                                                          HHDEC105
      IF (idbg .GE. 10) WRITE(6,1001) rnch,rint,jchan,jhig              HHDEC106
C                                                                       HHDEC107
      kevt(jchan,jhig) = kevt(jchan,jhig) + 1                           HHDEC108
C                                                                       HHDEC109
C  If a SUSY decay (chi chi or chi+ chi-) has been chosen, select       HHDEC110
C  which one precisely                                                  HHDEC111
C                                                                       HHDEC112
      rnch = RNDM(dummy)                                                HHDEC113
      rint = 0.D0                                                       HHDEC114
C                                                                       HHDEC115
      IF ( jchan .EQ. 15 ) THEN                                         HHDEC116
C                                                                       HHDEC117
        DO i = 1, 4                                                     HHDEC118
          DO j = 1, 4                                                   HHDEC119
            rint = rint + wneut(i,j,jhig)/frn                           HHDEC120
            IF ( rnch .LT. rint ) THEN                                  HHDEC121
              xymas(1,15,jhig) = ABS(amneut(i))                         HHDEC122
              xymas(2,15,jhig) = ABS(amneut(j))                         HHDEC123
              xywid(1,15,jhig) = 0.                                     HHDEC124
              xywid(2,15,jhig) = 0.                                     HHDEC125
              ichi = 0                                                  HHDEC126
              ichn(1) = i                                               HHDEC127
              ichn(2) = j                                               HHDEC128
              IF ( idbg .GE. 10 ) THEN                                  HHDEC129
                WRITE(6,*) 'Neutralino decay : ',i,j                    HHDEC130
                WRITE(6,*) 'Masses : ',ABS(amneut(i)),ABS(amneut(j))    HHDEC131
              ENDIF                                                     HHDEC132
              GOTO 40                                                   HHDEC133
            ENDIF                                                       HHDEC134
          ENDDO                                                         HHDEC135
        ENDDO                                                           HHDEC136
C                                                                       HHDEC137
      ELSEIF ( jchan .EQ. 16 ) THEN                                     HHDEC138
C                                                                       HHDEC139
        DO i = 1, 2                                                     HHDEC140
          DO j = 1, 2                                                   HHDEC141
            rint = rint + wchar(i,j,jhig)/frc                           HHDEC142
            IF ( rnch .LT. rint ) THEN                                  HHDEC143
              xymas(1,16,jhig) = ABS(amchar(i))                         HHDEC144
              xymas(2,16,jhig) = ABS(amchar(j))                         HHDEC145
              xywid(1,16,jhig) = 0.                                     HHDEC146
              xywid(2,16,jhig) = 0.                                     HHDEC147
              ichi = 1                                                  HHDEC148
              ichn(1) = i                                               HHDEC149
              ichn(2) = j                                               HHDEC150
              IF ( idbg .GE. 10 ) THEN                                  HHDEC151
                WRITE(6,*) 'Chargino decay : ',i,j                      HHDEC152
                WRITE(6,*) 'Masses : ',ABS(amchar(i)),ABS(amchar(j))    HHDEC153
              ENDIF                                                     HHDEC154
              GOTO 40                                                   HHDEC155
            ENDIF                                                       HHDEC156
          ENDDO                                                         HHDEC157
        ENDDO                                                           HHDEC158
C                                                                       HHDEC159
      ENDIF                                                             HHDEC160
C                                                                       HHDEC161
      CALL ucopy(brloc(1,1),branch(1,1),nchan*nhig)                     HHDEC162
      CALL ucopy(wnloc(1,1,1),wneut(1,1,1),4*4*nhig)                    HHDEC163
      CALL ucopy(wcloc(1,1,1),wchar(1,1,1),2*2*nhig)                    HHDEC164
C                                                                       HHDEC165
C  Build quadriimpulsions of the 2 decay particles                      HHDEC166
C                                                                       HHDEC167
   40 CALL pick4(ph,jchan,jhig)                                         HHDEC168
C                                                                       HHDEC169
C  Fill LUJETS common block. Lorentz-boost back to the lab.             HHDEC170
C                                                                       HHDEC171
      CALL hhlujt(jchan,jhig,ph,iph)                                    HHDEC172
C                                                                       HHDEC173
  999 RETURN                                                            HHDEC174
C------------------------------------------------------------------     HHDEC175
1000  FORMAT(1X,' +++ HHDECAY +++   No accessible decay channel',       HHDEC176
     .          ' for Higgs # ',I1)                                     HHDEC177
1001  FORMAT(1X,' +++ HHDECAY +++ RNDM : ',F8.6,' < ? ',F8.6,           HHDEC178
     .       ' Channel ',I3,', Type ',I3,' Higgs # ',I1)                HHDEC179
      END                                                               HHDEC180
      SUBROUTINE HHFINI                                                 HHFINI 2
C---------------------------------------------------------------------- HHFINI 3
C! Termination for neutral Higgs decays routines (from SM or MSSM)      HHFINI 4
C                                                                       HHFINI 5
C   P. Janot -- 24 August 1991                                          HHFINI 6
C---------------------------------------------------------------------- HHFINI 7
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      CHARACTER*14 channel                                              HHDECK 2
      CHARACTER*21 channeut, chanchar                                   HHDECK 3
      PARAMETER(nchneut=8,nchchar=5)                                    HHDECK 4
      COMMON / hhdeck / branch(nchan,nhig),width(nhig),                 HHDECK 5
     .                  parwth(nhig),xymas(2,nchan,nhig),               HHDECK 6
     .                  xywid(2,nchan,nhig)                             HHDECK 7
      COMMON / chaneu / ichn(2),                                        HHDECK 8
     .                  wneut(4,4,nhig), wchar(2,2,nhig),               HHDECK 9
     .                  widneut(4), brneut(nchneut,4),                  HHDECK10
     .                  widchar(2), brchar(nchchar,2)                   HHDECK11
      COMMON / chanch / channeut(nchneut,4), chanchar(nchchar,2)        HHDECK12
      COMMON / vect4 / pvect4(5,2)                                      HHDECK13
      COMMON / hinput / ichan(nchan,nhig), channel(nchan,nhig)          HHDECK14
      COMMON / hisbr / bchpp,bchgg                                      HHDECK15
      DIMENSION ph(4)                                                   HHDECK16
C                                                                       HHDECK17
      COMMON / h0evt / kevt(nchan,nhig)                                 COUNTS 2
C                                                                       COUNTS 3
      WRITE(6,1000)                                                     HHFINI11
      WRITE(6,1001) (jchan, jchan=1,nchan)                              HHFINI12
      WRITE(6,1002) (jhig,(kevt(jchan,jhig),jchan=1,nchan),jhig=1,nhig) HHFINI13
C---------------------------------------------------------------------- HHFINI14
 1000 FORMAT(1x,' Total number of events in various decay channels :'/) HHFINI15
 1001 FORMAT(4x,16(4x,I2))                                              HHFINI16
 1002 FORMAT(3(1x,I2,1x,16(I6)/))                                       HHFINI17
      RETURN                                                            HHFINI18
      END                                                               HHFINI19
      SUBROUTINE HHINIT                                                 HHINIT 2
C------------------------------------------------------------------     HHINIT 3
C!  Initialisation for neutral Higgs decays routines(from SM or MSSM)   HHINIT 4
C           . h0 lightest scalar                                        HHINIT 5
C           . H0 heaviest scalar    mh < 2m_mu or mh > ~ GeV/c2         HHINIT 6
C           . A  pseudoscalar                                           HHINIT 7
C                                                                       HHINIT 8
C  Input:     none                                                      HHINIT 9
C                                                                       HHINIT10
C                                                                       HHINIT11
C  Output:   /HHDECK/   --BRANCH(ichan,ihig), branching ratios          HHINIT12
C                       --WIDTH(ihig), total widths                     HHINIT13
C                                                                       HHINIT14
C   P. Janot -- 24 August 1991                                          HHINIT15
C------------------------------------------------------------------     HHINIT16
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      COMMON / h0evt / kevt(nchan,nhig)                                 COUNTS 2
C                                                                       COUNTS 3
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,      MISCL  2
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),            MISCL  3
     &                 nevent(11)                                       MISCL  4
      INTEGER loutbe,idb1,idb2,nevent                                   MISCL  5
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad                      MISCL  6
C                                                                       MISCL  7
C                                                                       HHINIT20
C  Set event numbers to zero                                            HHINIT21
C                                                                       HHINIT22
      CALL vzero(kevt(1,1),nchan*nhig)                                  HHINIT23
C                                                                       HHINIT24
C  Set defaults conditions                                              HHINIT25
C                                                                       HHINIT26
      CALL setdef                                                       HHINIT27
C                                                                       HHINIT28
C  Set user's conditions if requested                                   HHINIT29
C                                                                       HHINIT30
      CALL usrdef                                                       HHINIT31
C                                                                       HHINIT32
C  Determine parameters, couplings...                                   HHINIT33
C                                                                       HHINIT34
      CALL parcou                                                       HHINIT35
      IF ( amh .LT. 0. ) RETURN                                         HHINIT36
      IF ( ism .EQ. 0 .AND. amst(1) .LT. 0. ) RETURN                    HHINIT37
C                                                                       HHINIT38
C  Compute decay widths and branching ratios                            HHINIT39
C                                                                       HHINIT40
      CALL combra                                                       HHINIT41
C                                                                       HHINIT42
  999 RETURN                                                            HHINIT43
      END                                                               HHINIT44
      SUBROUTINE hhlujt(jchan,jhig,ph,iph)                              HHLUJT 2
C------------------------------------------------------------------     HHLUJT 3
C! Fill LUJETS common block after a Higgs boson decay.                  HHLUJT 4
C                                                                       HHLUJT 5
C  Patrick Janot -- 26 Aug 1991                                         HHLUJT 6
C------------------------------------------------------------------     HHLUJT 7
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
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      CHARACTER*14 channel                                              HHDECK 2
      CHARACTER*21 channeut, chanchar                                   HHDECK 3
      PARAMETER(nchneut=8,nchchar=5)                                    HHDECK 4
      COMMON / hhdeck / branch(nchan,nhig),width(nhig),                 HHDECK 5
     .                  parwth(nhig),xymas(2,nchan,nhig),               HHDECK 6
     .                  xywid(2,nchan,nhig)                             HHDECK 7
      COMMON / chaneu / ichn(2),                                        HHDECK 8
     .                  wneut(4,4,nhig), wchar(2,2,nhig),               HHDECK 9
     .                  widneut(4), brneut(nchneut,4),                  HHDECK10
     .                  widchar(2), brchar(nchchar,2)                   HHDECK11
      COMMON / chanch / channeut(nchneut,4), chanchar(nchchar,2)        HHDECK12
      COMMON / vect4 / pvect4(5,2)                                      HHDECK13
      COMMON / hinput / ichan(nchan,nhig), channel(nchan,nhig)          HHDECK14
      COMMON / hisbr / bchpp,bchgg                                      HHDECK15
      DIMENSION ph(4)                                                   HHDECK16
C                                                                       HHDECK17
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,      MISCL  2
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),            MISCL  3
     &                 nevent(11)                                       MISCL  4
      INTEGER loutbe,idb1,idb2,nevent                                   MISCL  5
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad                      MISCL  6
C                                                                       MISCL  7
      DIMENSION ijoin(2), jjoin(2)                                      HHLUJT12
      DIMENSION p1(4),p2(4),p3(4)                                       HHLUJT13
C                                                                       HHLUJT14
      IF ( jchan .EQ. 1 ) THEN                                          HHLUJT15
        kf1 = 22                                                        HHLUJT16
        kf2 = 22                                                        HHLUJT17
      ELSEIF ( jchan .EQ. 2 ) THEN                                      HHLUJT18
        kf1 = 21                                                        HHLUJT19
        kf2 = 21                                                        HHLUJT20
      ELSEIF ( jchan .EQ. 3 ) THEN                                      HHLUJT21
        kf1 = 15                                                        HHLUJT22
        kf2 =-15                                                        HHLUJT23
      ELSEIF ( jchan .EQ. 4 ) THEN                                      HHLUJT24
        kf1 = 4                                                         HHLUJT25
        kf2 =-4                                                         HHLUJT26
      ELSEIF ( jchan .EQ. 5 ) THEN                                      HHLUJT27
        kf1 = 5                                                         HHLUJT28
        kf2 =-5                                                         HHLUJT29
      ELSEIF ( jchan .EQ. 6 ) THEN                                      HHLUJT30
        kf1 = 6                                                         HHLUJT31
        kf2 =-6                                                         HHLUJT32
      ELSEIF ( jchan .EQ. 7 ) THEN                                      HHLUJT33
        kf1 = 24                                                        HHLUJT34
        kf2 =-24                                                        HHLUJT35
      ELSEIF ( jchan .EQ. 8 ) THEN                                      HHLUJT36
        kf1 = 23                                                        HHLUJT37
        kf2 = 23                                                        HHLUJT38
      ELSEIF ( jchan .EQ. 9 ) THEN                                      HHLUJT39
        IF ( jhig .NE. 3 ) THEN                                         HHLUJT40
          kf1 = 36                                                      HHLUJT41
          kf2 = 36                                                      HHLUJT42
        ELSE                                                            HHLUJT43
          kf1 = 25                                                      HHLUJT44
          kf2 = 23                                                      HHLUJT45
        ENDIF                                                           HHLUJT46
      ELSEIF ( jchan .EQ. 10 ) THEN                                     HHLUJT47
        kf1 = 25                                                        HHLUJT48
        kf2 = 25                                                        HHLUJT49
      ELSEIF ( jchan .EQ. 11 ) THEN                                     HHLUJT50
        kf1 = 22                                                        HHLUJT51
        kf2 = 23                                                        HHLUJT52
      ELSEIF ( jchan .EQ. 12 ) THEN                                     HHLUJT53
        kf1 = 11                                                        HHLUJT54
        kf2 =-11                                                        HHLUJT55
      ELSEIF ( jchan .EQ. 13 ) THEN                                     HHLUJT56
        kf1 = 13                                                        HHLUJT57
        kf2 =-13                                                        HHLUJT58
      ELSEIF ( jchan .EQ. 14 ) THEN                                     HHLUJT59
        kf1 = 3                                                         HHLUJT60
        kf2 =-3                                                         HHLUJT61
      ELSEIF ( jchan .EQ. 15 ) THEN                                     HHLUJT62
        IF ( ism .EQ. 1 ) THEN                                          HHLUJT63
          kf1 = 51                                                      HHLUJT64
          kf2 = 51                                                      HHLUJT65
        ELSE                                                            HHLUJT66
          kf1 = 50+ichn(1)                                              HHLUJT67
          kf2 = 50+ichn(2)                                              HHLUJT68
        ENDIF                                                           HHLUJT69
      ELSEIF ( jchan .EQ. 16 ) THEN                                     HHLUJT70
        kf1 = 54+ichn(1)                                                HHLUJT71
        kf2 =-54-ichn(2)                                                HHLUJT72
      ENDIF                                                             HHLUJT73
C                                                                       HHLUJT74
      n7lu = n7lu + 1                                                   HHLUJT75
      ijoin(1) = n7lu                                                   HHLUJT76
      CALL hhlu1(ijoin(1),kf1,pvect4(1,1),pvect4(2,1),                  HHLUJT77
     .                    pvect4(3,1),pvect4(4,1),pvect4(5,1))          HHLUJT78
      k7lu(iph,4) = n7lu                                                HHLUJT79
      k7lu(ijoin(1),3) = iph                                            HHLUJT80
C                                                                       HHLUJT81
      n7lu = n7lu + 1                                                   HHLUJT82
      ijoin(2) = n7lu                                                   HHLUJT83
      CALL hhlu1(ijoin(2),kf2,pvect4(1,2),pvect4(2,2),                  HHLUJT84
     .                    pvect4(3,2),pvect4(4,2),pvect4(5,2))          HHLUJT85
      k7lu(iph,5) = n7lu                                                HHLUJT86
      k7lu(ijoin(2),3) = iph                                            HHLUJT87
C                                                                       HHLUJT88
C Parton shower preparation in case of a quark system                   HHLUJT89
C                                                                       HHLUJT90
      IF ( (jchan.GE.4 .AND. jchan.LE.6) .OR. jchan.EQ.2 ) THEN         HHLUJT91
        k7lu(ijoin(1),1) = 2                                            HHLUJT92
        njoin = 2                                                       HHLUJT93
        CALL lujoin(njoin,ijoin)                                        HHLUJT94
        xmm = p7lu(iph,5)                                               HHLUJT95
        CALL lushow(ijoin(1), ijoin(2), xmm)                            HHLUJT96
      ENDIF                                                             HHLUJT97
C                                                                       HHLUJT98
C Decay and parton shower if WW or ZZ                                   HHLUJT99
C                                                                       HHLUJ100
      IF ( jchan .EQ. 7 .OR. jchan .EQ. 8 ) THEN                        HHLUJ101
        CALL wzdecy(ijoin(1))                                           HHLUJ102
        CALL wzdecy(ijoin(2))                                           HHLUJ103
      ENDIF                                                             HHLUJ104
C                                                                       HHLUJ105
      k7lu(iph,1) = 11                                                  HHLUJ106
C                                                                       HHLUJ107
      IF ( idbg .GE. 5 ) CALL lulist(1)                                 HHLUJ108
  999 RETURN                                                            HHLUJ109
      END                                                               HHLUJ110
      SUBROUTINE hhlu1(ipa,kf,px,py,pz,pe,pm)                           HHLU1  2
C------------------------------------------------------------------     HHLU1  3
C! Add one entry to the LUND event record                               HHLU1  4
C                                                                       HHLU1  5
C  Patrick Janot -- 26 Aug 1991                                         HHLU1  6
C------------------------------------------------------------------     HHLU1  7
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
C                                                                       HHLU1  9
      qm = pm                                                           HHLU1 10
      IF ( qm .LT. 0. ) qm = SQRT(pe**2-px**2-py**2-pz**2)              HHLU1 11
      DO 100 J=1,5                                                      HHLU1 12
      k7lu(ipa,j)=0                                                     HHLU1 13
      p7lu(ipa,j)=0.                                                    HHLU1 14
  100 v7lu(ipa,j)=0.                                                    HHLU1 15
C...Store parton/particle in K and P vectors.                           HHLU1 16
      k7lu(ipa,1)=1                                                     HHLU1 17
      k7lu(ipa,2)=kf                                                    HHLU1 18
      p7lu(ipa,5)=qm                                                    HHLU1 19
      p7lu(ipa,4)=pe                                                    HHLU1 20
      p7lu(ipa,1)=px                                                    HHLU1 21
      p7lu(ipa,2)=py                                                    HHLU1 22
      p7lu(ipa,3)=pz                                                    HHLU1 23
C                                                                       HHLU1 24
  999 RETURN                                                            HHLU1 25
      END                                                               HHLU1 26
      SUBROUTINE hzha(ipro,ecms,qg1,qg2,qh,qa,hh,izpol)                 HZHA   2
C-----------------------------------------------------------------------HZHA   3
C! Event generation for the various Higgs production processes          HZHA   4
C                                                                       HZHA   5
C  Input :       ipro,   the process Id                                 HZHA   6
C                ecms,   the centre-of-mass energy                      HZHA   7
C                                                                       HZHA   8
C  Output:       qg1[4]                                                 HZHA   9
C                qg2[4], the initial state photons                      HZHA  10
C                qh[4]                                                  HZHA  11
C                qa[4]                                                  HZHA  12
C                hh[4],  the final state particles (Higgs,Z,fermions)   HZHA  13
C                izpol,  the Z polarization, when IKLEI=0, for hZ.      HZHA  14
C                                                                       HZHA  15
C   Patrick Janot -- 27 Aug 1991                                        HZHA  16
C---------------------------------------------------------------------- HZHA  17
      DIMENSION qg1(4),qg2(4),qh(4),qa(4),rh(4),ra(4),rg2(4)            HZHA  18
      DIMENSION gh(4),hh(4)                                             HZHA  19
      LOGICAL first                                                     HZHA  20
      DATA first/.TRUE./                                                HZHA  21
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
      PARAMETER (maxpro= 8)                                             HHPROD 2
      COMMON / cropro / cross(maxpro), sthr(maxpro), reduc(maxpro)      HHPROD 3
      CHARACTER*14 chapro(maxpro)                                       HHPROD 4
      DATA chapro /                                                     HHPROD 5
     .             'e+e- --> h Z',                                      HHPROD 6
     .             'e+e- --> H Z',                                      HHPROD 7
     .             'e+e- --> h A',                                      HHPROD 8
     .             'e+e- --> H A',                                      HHPROD 9
     .             'W+W- --> h  ',                                      HHPROD10
     .             'W+W- --> H  ',                                      HHPROD11
     .             'Z Z  --> h  ',                                      HHPROD12
     .             'Z Z  --> H  '                                       HHPROD13
     .                                /                                 HHPROD14
      COMMON / prosim / iproyn(maxpro),nevpro(maxpro)                   HHPROD15
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,      MISCL  2
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),            MISCL  3
     &                 nevent(11)                                       MISCL  4
      INTEGER loutbe,idb1,idb2,nevent                                   MISCL  5
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad                      MISCL  6
C                                                                       MISCL  7
      EXTERNAL sigma1,sigma2,sigma3,sigma4                              SIGMAS 2
      EXTERNAL sigma5,sigma6,sigma7,sigma8                              SIGMAS 3
      EXTERNAL sigmat                                                   SIGMAS 4
C                                                                       SIGMAS 5
C                                                                       HZHA  26
      s = ecms**2                                                       HZHA  27
      e = ecms/2.                                                       HZHA  28
      CALL vzero(qg1(1),4)                                              HZHA  29
      CALL vzero(qg2(1),4)                                              HZHA  30
      CALL vzero(rg2(1),4)                                              HZHA  31
      CALL vzero(qh(1),4)                                               HZHA  32
      CALL vzero(ra(1),4)                                               HZHA  33
      CALL vzero(rh(1),4)                                               HZHA  34
      CALL vzero(qa(1),4)                                               HZHA  35
      CALL vzero(hh(1),4)                                               HZHA  36
      CALL vzero(gh(1),4)                                               HZHA  37
C                                                                       HZHA  38
C  First compute the total cross section with Brems- or beams-strahlung HZHA  39
C  (if requested)                                                       HZHA  40
C                                                                       HZHA  41
      IF ( first ) THEN                                                 HZHA  42
C                                                                       HZHA  43
        CALL vzero(sthr(1),maxpro)                                      HZHA  44
C                                                                       HZHA  45
C Bremsstrahlung- Beamstrahlung                                         HZHA  46
C                                                                       HZHA  47
        IF ( xrad .GT. 0. ) THEN                                        HZHA  48
C                                                                       HZHA  49
          IF ( iklei .EQ. 0 ) THEN                                      HZHA  50
            sthr(1) = (pmas(23,1)+pmas(25,1)-10.*pmas(23,2))**2         HZHA  51
            sthr(2) = (pmas(23,1)+pmas(35,1)-10.*pmas(23,2))**2         HZHA  52
          ELSE                                                          HZHA  53
            sthr(1) = 0.1                                               HZHA  54
            sthr(2) = 0.1                                               HZHA  55
          ENDIF                                                         HZHA  56
          sthr(3) = 0.1                                                 HZHA  57
          sthr(4) = 0.1                                                 HZHA  58
          sthr(5) = 0.1                                                 HZHA  59
          sthr(6) = 0.1                                                 HZHA  60
          sthr(7) = 0.1                                                 HZHA  61
          sthr(8) = 0.1                                                 HZHA  62
C                                                                       HZHA  63
          IF (cross(1)*iproyn(1)*reduc(1) .GT. 0.)                      HZHA  64
     .      CALL remt1(e,sigma1,sthr(1),1,cross(1),xrad)                HZHA  65
          IF (cross(2)*iproyn(2)*reduc(2) .GT. 0.)                      HZHA  66
     .      CALL remt1(e,sigma2,sthr(2),2,cross(2),xrad)                HZHA  67
          IF (cross(3)*iproyn(3)*reduc(3) .GT. 0.)                      HZHA  68
     .      CALL remt1(e,sigma3,sthr(3),3,cross(3),xrad)                HZHA  69
          IF (cross(4)*iproyn(4)*reduc(4) .GT. 0.)                      HZHA  70
     .      CALL remt1(e,sigma4,sthr(4),4,cross(4),xrad)                HZHA  71
          IF (cross(5)*iproyn(5)*reduc(5) .GT. 0.)                      HZHA  72
     .      CALL remt1(e,sigma5,sthr(5),5,cross(5),xrad)                HZHA  73
          IF (cross(6)*iproyn(6)*reduc(6) .GT. 0.)                      HZHA  74
     .      CALL remt1(e,sigma6,sthr(6),6,cross(6),xrad)                HZHA  75
          IF (cross(7)*iproyn(7)*reduc(7) .GT. 0.)                      HZHA  76
     .      CALL remt1(e,sigma7,sthr(7),7,cross(7),xrad)                HZHA  77
          IF (cross(8)*iproyn(8)*reduc(8) .GT. 0.)                      HZHA  78
     .      CALL remt1(e,sigma8,sthr(8),8,cross(8),xrad)                HZHA  79
C                                                                       HZHA  80
          WRITE(6,1000) (chapro(i),cross(i),iproyn(i),i=1,maxpro)       HZHA  81
 1000     FORMAT(                                                       HZHA  82
     .     //20x,'------------------------------------------------'/    HZHA  83
     .       20x,'Final cross sections :'//                             HZHA  84
     .     8(20x,'     o ',A14,'  : ',E10.4,' fb  (',I1,')'/)/          HZHA  85
     .       20x,' (0) = Channel not requested, only Born level.'/      HZHA  86
     .       20x,' (1) = Channel requested, ISR included.'/             HZHA  87
     .       20x,'------------------------------------------------'//)  HZHA  88
        ENDIF                                                           HZHA  89
        first = .FALSE.                                                 HZHA  90
      ENDIF                                                             HZHA  91
C                                                                       HZHA  92
C  Event generation                                                     HZHA  93
C                                                                       HZHA  94
      IF ( xrad .GT. 0. ) THEN                                          HZHA  95
    1   CALL remt2(qg1,rg2,ipro)                                        HZHA  96
        s1 = s * (1.-qg1(4)/e)                                          HZHA  97
        e1 = SQRT(s1)/2.                                                HZHA  98
        s2 = s1 * (1.-rg2(4)/e1)                                        HZHA  99
        IF ( s2 .LE. sthr(ipro) ) THEN                                  HZHA 100
          WRITE(6,*) ' *** Warning *** Not enough energy for production'HZHA 101
     .               ,s2,' < ',sthr(ipro),' !'                          HZHA 102
          GOTO 1                                                        HZHA 103
        ENDIF                                                           HZHA 104
        CALL dsigma(ipro,s2,qh,qa,hh,izpol)                             HZHA 105
C                                                                       HZHA 106
        IF ( ipro .LT. 5 ) THEN                                         HZHA 107
          qhm = qh(4)                                                   HZHA 108
          qam = qa(4)                                                   HZHA 109
          qh(4) = SQRT(qh(1)**2+qh(2)**2+qh(3)**2+qh(4)**2)             HZHA 110
          qa(4) = SQRT(qa(1)**2+qa(2)**2+qa(3)**2+qa(4)**2)             HZHA 111
        ELSE                                                            HZHA 112
          hhm = hh(4)                                                   HZHA 113
          hh(4) = SQRT(hh(1)**2+hh(2)**2+hh(3)**2+hh(4)**2)             HZHA 114
        ENDIF                                                           HZHA 115
C                                                                       HZHA 116
        CALL remt3(qh, rh, 2)                                           HZHA 117
        CALL remt3(qa, ra, 2)                                           HZHA 118
        CALL remt3(rh, qh, 1)                                           HZHA 119
        CALL remt3(ra, qa, 1)                                           HZHA 120
        IF ( ipro.GE.5 .OR. (ipro.LE.2.AND.iklei.EQ.1) ) THEN           HZHA 121
          CALL remt3(hh, gh, 2)                                         HZHA 122
          CALL remt3(gh, hh, 1)                                         HZHA 123
        ENDIF                                                           HZHA 124
        CALL remt3(rg2,qg2,1)                                           HZHA 125
C                                                                       HZHA 126
        IF ( ipro .LT. 5 ) THEN                                         HZHA 127
          qh(4) = qhm                                                   HZHA 128
          qa(4) = qam                                                   HZHA 129
        ELSE                                                            HZHA 130
          hh(4) = hhm                                                   HZHA 131
        ENDIF                                                           HZHA 132
C                                                                       HZHA 133
      ELSE                                                              HZHA 134
        call dsigma(ipro,s,qh,qa,hh,izpol)                              HZHA 135
      ENDIF                                                             HZHA 136
C                                                                       HZHA 137
C  End of event generation                                              HZHA 138
C                                                                       HZHA 139
      RETURN                                                            HZHA 140
      END                                                               HZHA 141
      SUBROUTINE LORENZ(BETAX,BETAY,BETAZ,P)                            LORENZ 2
C-------------------------------------------------------------------    LORENZ 3
C!  Perform a Lorentz transformation of the quadriimpulsion P           LORENZ 4
C   from frame 1 to frame 2                                             LORENZ 5
C                                                                       LORENZ 6
C   Input:     Passed:    --BETAX,Y,Z    2's velocity / 1               LORENZ 7
C                         --P,           quadriimpulsion in 1           LORENZ 8
C                                                                       LORENZ 9
C   Output:    Passed:    --P,           quadriimpulsion in 2           LORENZ10
C                                                                       LORENZ11
C   P. Janot  --  20 oct 1988                                           LORENZ12
C-------------------------------------------------------------------    LORENZ13
      IMPLICIT REAL*8 (A-Z)                                             LORENZ14
      REAL*4 P(4)                                                       LORENZ15
      BETA2 = BETAX**2 + BETAY**2 + BETAZ**2                            LORENZ16
      IF(BETA2 .EQ. 0.) RETURN                                          LORENZ17
      GAMMA = 1./SQRT(1.-BETA2)                                         LORENZ18
      ONE   = BETAX*P(1) + BETAY*P(2) + BETAZ*P(3)                      LORENZ19
      TWO   = (GAMMA-1.)*ONE/BETA2- GAMMA*P(4)                          LORENZ20
      P(1)  = P(1) + BETAX*TWO                                          LORENZ21
      P(2)  = P(2) + BETAY*TWO                                          LORENZ22
      P(3)  = P(3) + BETAZ*TWO                                          LORENZ23
      P(4)  = GAMMA*(-ONE+P(4))                                         LORENZ24
      RETURN                                                            LORENZ25
      END                                                               LORENZ26
      SUBROUTINE neutra                                                 NEUTRA 2
C------------------------------------------------------------------     NEUTRA 3
C!  Compute neutralino masses from the MSSM parameters                  NEUTRA 4
C                                                                       NEUTRA 5
C  Input:    /PARAM/ MSSM parameters                                    NEUTRA 6
C                                                                       NEUTRA 7
C  Output:   /PARAM/ amneut(4),   the neutralino masses.                NEUTRA 8
C                                                                       NEUTRA 9
C  P. Janot -- 4 December 1994                                          NEUTRA10
C------------------------------------------------------------------     NEUTRA11
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)        DATAPI 2
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          DATAPI 3
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         DATAPI 4
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         DATAPI 5
C                                                                       DATAPI 6
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),               CONSTS 2
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw      CONSTS 3
C                                                                       CONSTS 4
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,      MISCL  2
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),            MISCL  3
     &                 nevent(11)                                       MISCL  4
      INTEGER loutbe,idb1,idb2,nevent                                   MISCL  5
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad                      MISCL  6
C                                                                       MISCL  7
      DIMENSION y(4,4), wr(4), ir(4), zr(4,4), rr(4,4),                 NEUTRA16
     .          work(4)                                                 NEUTRA17
C                                                                       NEUTRA18
      susM1 = susM * 5./(8.*(1.-sw2))                                   NEUTRA19
      susM2 = susM * 3./(8.*sw2)                                        NEUTRA20
C                                                                       NEUTRA21
c The neutralino mass matrix Y(4,4)                                     NEUTRA22
C                                                                       NEUTRA23
      y(1,1) =  susM1                                                   NEUTRA24
      y(1,2) =  0.                                                      NEUTRA25
      y(1,3) = -amz*sw*cb                                               NEUTRA26
      y(1,4) =  amz*sw*sb                                               NEUTRA27
      y(2,1) =  y(1,2)                                                  NEUTRA28
      y(2,2) =  susM2                                                   NEUTRA29
      y(2,3) =  amz*cw*cb                                               NEUTRA30
      y(2,4) = -amz*cw*sb                                               NEUTRA31
      y(3,1) =  y(1,3)                                                  NEUTRA32
      y(3,2) =  y(2,3)                                                  NEUTRA33
      y(3,3) =  0.                                                      NEUTRA34
      y(3,4) = -susMU                                                   NEUTRA35
      y(4,1) =  y(1,4)                                                  NEUTRA36
      y(4,2) =  y(2,4)                                                  NEUTRA37
      y(4,3) =  y(3,4)                                                  NEUTRA38
      y(4,4) =  0.                                                      NEUTRA39
C                                                                       NEUTRA40
C  Diagonalization of the neutralino mass matrix;                       NEUTRA41
C  ZR corresponds to the matrix N transpose; the eigenvalues            NEUTRA42
C  are contained in WR, and might be <0 or >0 depending of              NEUTRA43
C  the CP quantum number of the neutralino                              NEUTRA44
C                                                                       NEUTRA45
      CALL eisrs1 (4,4,y,wr,zr,ierr,work)                               NEUTRA46
      CALL ucopy(zr(1,1),rr(1,1),16)                                    NEUTRA47
      DO 20 i = 1, 4                                                    NEUTRA48
        work(i) = ABS(wr(i))                                            NEUTRA49
   20 CONTINUE                                                          NEUTRA50
C                                                                       NEUTRA51
C Sort the neutralinos in increasing mass order                         NEUTRA52
C                                                                       NEUTRA53
      CALL sortzv(work,ir,4,1,0,0)                                      NEUTRA54
      DO 10 i = 1, 4                                                    NEUTRA55
        k         = ir(i)                                               NEUTRA56
        amneut(i) = wr(k)                                               NEUTRA57
        DO 11 j = 1, 4                                                  NEUTRA58
          zr(j,i) = rr(j,k)                                             NEUTRA59
   11   CONTINUE                                                        NEUTRA60
   10 CONTINUE                                                          NEUTRA61
      CALL ucopy(zr(1,1),fieldn(1,1),4*4)                               NEUTRA62
C                                                                       NEUTRA63
C Matrices Q''(ij) and S''(ij)                                          NEUTRA64
C                                                                       NEUTRA65
      DO 1 i=1,4                                                        NEUTRA66
        DO 2 j=1,4                                                      NEUTRA67
          tw=sw/cw                                                      NEUTRA68
          qqmat(i,j) = ( zr(3,i)*(zr(2,j)-tw*zr(1,j))                   NEUTRA69
     .               +   zr(3,j)*(zr(2,i)-tw*zr(1,i)) )                 NEUTRA70
          ssmat(i,j) = ( zr(4,i)*(zr(2,j)-tw*zr(1,j))                   NEUTRA71
     .               +   zr(4,j)*(zr(2,i)-tw*zr(1,i)) )                 NEUTRA72
    2   CONTINUE                                                        NEUTRA73
    1 CONTINUE                                                          NEUTRA74
C                                                                       NEUTRA75
      RETURN                                                            NEUTRA76
      END                                                               NEUTRA77
      SUBROUTINE PARCOU                                                 PARCOU 2
C------------------------------------------------------------------     PARCOU 3
C!  Compute parameters, couplings                                       PARCOU 4
C                                                                       PARCOU 5
C  Input:    /PARAM/ SM and MSSM parameters                             PARCOU 6
C                                                                       PARCOU 7
C  Output:   /PARAM/ Relevant couplings, masses, angles...              PARCOU 8
C                                                                       PARCOU 9
C   P. Janot -- 24 August 1991                                          PARCOU10
C------------------------------------------------------------------     PARCOU11
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      CHARACTER*14 channel                                              HHDECK 2
      CHARACTER*21 channeut, chanchar                                   HHDECK 3
      PARAMETER(nchneut=8,nchchar=5)                                    HHDECK 4
      COMMON / hhdeck / branch(nchan,nhig),width(nhig),                 HHDECK 5
     .                  parwth(nhig),xymas(2,nchan,nhig),               HHDECK 6
     .                  xywid(2,nchan,nhig)                             HHDECK 7
      COMMON / chaneu / ichn(2),                                        HHDECK 8
     .                  wneut(4,4,nhig), wchar(2,2,nhig),               HHDECK 9
     .                  widneut(4), brneut(nchneut,4),                  HHDECK10
     .                  widchar(2), brchar(nchchar,2)                   HHDECK11
      COMMON / chanch / channeut(nchneut,4), chanchar(nchchar,2)        HHDECK12
      COMMON / vect4 / pvect4(5,2)                                      HHDECK13
      COMMON / hinput / ichan(nchan,nhig), channel(nchan,nhig)          HHDECK14
      COMMON / hisbr / bchpp,bchgg                                      HHDECK15
      DIMENSION ph(4)                                                   HHDECK16
C                                                                       HHDECK17
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
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)        DATAPI 2
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          DATAPI 3
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         DATAPI 4
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         DATAPI 5
C                                                                       DATAPI 6
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),               CONSTS 2
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw      CONSTS 3
C                                                                       CONSTS 4
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,      MISCL  2
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),            MISCL  3
     &                 nevent(11)                                       MISCL  4
      INTEGER loutbe,idb1,idb2,nevent                                   MISCL  5
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad                      MISCL  6
C                                                                       MISCL  7
C                                                                       PARCOU18
C Quark and lepton masses                                               PARCOU19
C                                                                       PARCOU20
      amb   = pmas(5,1)                                                 PARCOU21
      amc   = pmas(4,1)                                                 PARCOU22
      amtau = pmas(15,1)                                                PARCOU23
      ame   = pmas(11,1)                                                PARCOU24
      ammu  = pmas(13,1)                                                PARCOU25
      amu   = .005                                                      PARCOU26
      amd   = .010                                                      PARCOU27
      ams   = .150                                                      PARCOU28
C                                                                       PARCOU29
C Z mass and width, W width.                                            PARCOU30
C                                                                       PARCOU31
      amz    = pmas(23,1)                                               PARCOU32
      gmz    = pmas(23,2)                                               PARCOU33
      gmw    = pmas(24,2)                                               PARCOU34
C                                                                       PARCOU35
C QED coupling constant at the Z                                        PARCOU36
C                                                                       PARCOU37
      alpha(0)  = ulalem(amz**2)                                        PARCOU38
      alpha2    = 0.38937966E12 * alpha(0)**2                           PARCOU39
C                                                                       PARCOU40
C QCD coupling constant at the Z                                        PARCOU41
C                                                                       PARCOU42
      fnz       = 5.                                                    PARCOU43
      alphas(0) = pi*runalf(amz,fnz)                                    PARCOU44
C                                                                       PARCOU45
C Sin**2 theta_E effective fine tuned from G_F, alpha_QED, mZ           PARCOU46
C                                                                       PARCOU47
      sw2    = 0.5*(1.-SQRT(1.-4.*pi*alpha(0)/(SQRT(2.)*G_F*amz**2)))   PARCOU48
      sw     = SQRT(sw2)                                                PARCOU49
      cw2    = 1.-sw2                                                   PARCOU50
      cw     = SQRT(cw2)                                                PARCOU51
C                                                                       PARCOU52
C Miscallaneous                                                         PARCOU53
C                                                                       PARCOU54
      gweak2(0) = 4. * pi * alpha(0) / sw2                              PARCOU55
      deltar    = 3.*G_F*amt**2/(8*pi**2*SQRT(2.))                      PARCOU56
      paru(102) = sw2                                                   PARCOU57
C                                                                       PARCOU58
C W mass from mZ, weinberg angle and delta(rho)                         PARCOU59
C                                                                       PARCOU60
      amw    = amz*cw/SQRT(1.-deltar)                                   PARCOU61
C                                                                       PARCOU62
C Running Delta rho to compute cross sections                           PARCOU63
C                                                                       PARCOU64
      alpha_3 = runalf(amt,5.) * pi                                     PARCOU65
      factork = 16.11                                                   PARCOU66
     .       - 1.04*(4.-amu/amt-amd/amt-ams/amt-amc/amt-amb/amt)        PARCOU67
      amtcor = (1. + 4./3. *alpha_3/pi + factork*(alpha_3/pi)**2 )      PARCOU68
      deltar = deltar / amtcor**2                                       PARCOU69
C                                                                       PARCOU70
C Relevant print out                                                    PARCOU71
C                                                                       PARCOU72
      IF ( idbg .GE. 0 ) THEN                                           PARCOU73
        WRITE(6,1003) amz,gmz,amt,g_f,xlamda5                           PARCOU74
        WRITE(6,1004) 1./alpha(0),sw2,deltar,amw,alphas(0)              PARCOU75
      ENDIF                                                             PARCOU76
C                                                                       PARCOU77
      IF ( ism .EQ. 0 ) THEN                                            PARCOU78
C                                                                       PARCOU79
C Now ... The MSSM !                                                    PARCOU80
C                                                                       PARCOU81
        beta = ATAN(tb)                                                 PARCOU82
        cb = COS(beta)                                                  PARCOU83
        sb = SIN(beta)                                                  PARCOU84
        amsq = susSMQ                                                   PARCOU85
C                                                                       PARCOU86
C Compute the S-top and S-bottom masses. Other squark splitting is      PARCOU87
C assumed to be negligible.                                             PARCOU88
C                                                                       PARCOU89
        CALL squarks                                                    PARCOU90
        IF ( amst(1) .LE. 0. ) RETURN                                   PARCOU91
C                                                                       PARCOU92
C Compute the neutralino masses                                         PARCOU93
C                                                                       PARCOU94
        CALL neutra                                                     PARCOU95
C                                                                       PARCOU96
C Compute the chargino masses                                           PARCOU97
C                                                                       PARCOU98
        CALL chargi                                                     PARCOU99
C                                                                       PARCO100
C Compute the neutral and charged Higgs masses, together with the       PARCO101
C mixing angle alpha in the CP-even sector                              PARCO102
C                                                                       PARCO103
        CALL shiggs                                                     PARCO104
        IF ( amh .LT. 0. ) RETURN                                       PARCO105
C                                                                       PARCO106
C Compute the chargino and neutralino couplings to the Higgs bosons     PARCO107
C                                                                       PARCO108
        CALL chaneucp                                                   PARCO109
C                                                                       PARCO110
C Compute the decay branching ratios of the neutralinos in chi Z*       PARCO111
C in chi gamma and in chi+/- W*+/-                                      PARCO112
C                                                                       PARCO113
        CALL chidec                                                     PARCO114
C                                                                       PARCO115
      ELSE                                                              PARCO116
C                                                                       PARCO117
C The Standard Model !                                                  PARCO118
C                                                                       PARCO119
        ama  = 0.                                                       PARCO120
        gmh  = 0.                                                       PARCO121
        beta =  pi/4.                                                   PARCO122
        alfa = -pi/4.                                                   PARCO123
        tb   = 1.                                                       PARCO124
        ta   =-1.                                                       PARCO125
        c2b  = 0.                                                       PARCO126
        s2b  = 1.                                                       PARCO127
        sab2 = 1.                                                       PARCO128
        cab2 = 0.                                                       PARCO129
        cb = COS(beta)                                                  PARCO130
        sb = SIN(beta)                                                  PARCO131
        ca = COS(alfa)                                                  PARCO132
        sa = SIN(alfa)                                                  PARCO133
        cab = 1.                                                        PARCO134
        sab = 0.                                                        PARCO135
        c2a = 0.                                                        PARCO136
C                                                                       PARCO137
      ENDIF                                                             PARCO138
C                                                                       PARCO139
      amhig(1) = gmh                                                    PARCO140
      amhig(2) = amh                                                    PARCO141
      amhig(3) = ama                                                    PARCO142
      pmas(24,1) = amw                                                  PARCO143
      pmas(25,1) = amh                                                  PARCO144
      pmas(35,1) = gmh                                                  PARCO145
      pmas(36,1) = ama                                                  PARCO146
      chaf(25) = 'h       '                                             PARCO147
      chaf(35) = 'H       '                                             PARCO148
      chaf(36) = 'A       '                                             PARCO149
C     mdme(337,1) = 0                                                   PARCO150
      mdme(174,1) = 0                                                   PARCO151
      mdme(153,1) = 0                                                   PARCO152
C                                                                       PARCO153
C The QED/QCD running coupling constants at mh, mH, mA                  PARCO154
C                                                                       PARCO155
        DO jhig = 1, nhig                                               PARCO156
          fnh = 3.                                                      PARCO157
          xmh          = amhig(jhig)                                    PARCO158
          IF ( xmh .LE. 1. ) xmh = 1.                                   PARCO159
          IF ( amc .LE. xmh/2. ) fnh = fnh + 1.                         PARCO160
          IF ( amb .LE. xmh/2. ) fnh = fnh + 1.                         PARCO161
          IF ( amt .LE. xmh/2. ) fnh = fnh + 1.                         PARCO162
C                                                                       PARCO163
          alphas(jhig) = pi*runalf(xmh,fnh)                             PARCO164
          alpha (jhig) = ulalem(xmh**2)                                 PARCO165
          gweak2(jhig) = 4. * pi * alpha(jhig) / sw2                    PARCO166
C                                                                       PARCO167
        ENDDO                                                           PARCO168
C                                                                       PARCO169
C Print out...                                                          PARCO170
C                                                                       PARCO171
      IF ( idbg .GE. 0 ) THEN                                           PARCO172
        IF ( ism .EQ. 0 ) THEN                                          PARCO173
          WRITE(6,1000) amarun,tb,susM,susMU,susAt,susAB,susSMQ,        PARCO174
     .                              susSMU,susSMD,susSML,susSME         PARCO175
          WRITE(6,1001) amh,gmh,ama,amhp                                PARCO176
          WRITE(6,2001) amst(1),amst(2),topmix,                         PARCO177
     .                  amsb(1),amsb(2),botmix                          PARCO178
          WRITE(6,2003) amneut                                          PARCO179
          WRITE(6,2004) amchar                                          PARCO180
C                                                                       PARCO181
          IF ( amchar(1) .LT. ABS(amneut(1)) ) WRITE(6,2008)            PARCO182
C                                                                       PARCO183
          DO ineut = 1, 4                                               PARCO184
            WRITE(6,2005) ineut,amneut(ineut)                           PARCO185
            DO ic = 1, nchneut                                          PARCO186
              IF ( brneut(ic,ineut) .GT. 0. )                           PARCO187
     .        WRITE(6,2007) channeut(ic,ineut),100.*brneut(ic,ineut)    PARCO188
            ENDDO                                                       PARCO189
          ENDDO                                                         PARCO190
C                                                                       PARCO191
          DO ichar = 1, 2                                               PARCO192
            WRITE(6,2006) ichar,amchar(ichar)                           PARCO193
            DO ic = 1, nchchar                                          PARCO194
              IF ( brchar(ic,ichar) .GT. 0. )                           PARCO195
     .        WRITE(6,2007) chanchar(ic,ichar),100.*brchar(ic,ichar)    PARCO196
            ENDDO                                                       PARCO197
          ENDDO                                                         PARCO198
C                                                                       PARCO199
          sasb = sa/sb                                                  PARCO200
          cacb = ca/cb                                                  PARCO201
          casb = ca/sb                                                  PARCO202
          sacb = sa/cb                                                  PARCO203
          WRITE(6,2002) sin(beta-alfa),cos(beta-alfa),                  PARCO204
     .                  sasb,cacb,casb,sacb,1./tb,tb                    PARCO205
        ELSE                                                            PARCO206
        ENDIF                                                           PARCO207
      ENDIF                                                             PARCO208
C                                                                       PARCO209
C Fill internal mass table                                              PARCO210
C                                                                       PARCO211
      CALL vzero(xymas(1,1,1),2*nchan*nhig)                             PARCO212
      DO jhig = 1 , nhig                                                PARCO213
        xymas(1,1,jhig) = 0.                                            PARCO214
        xymas(1,2,jhig) = 0.                                            PARCO215
        xymas(1,3,jhig) = amtau                                         PARCO216
        xymas(1,4,jhig) = amc                                           PARCO217
        xymas(1,5,jhig) = amb                                           PARCO218
        xymas(1,6,jhig) = amt                                           PARCO219
        xymas(1,7,jhig) = amw                                           PARCO220
        xymas(1,8,jhig) = amz                                           PARCO221
        xymas(1,9,jhig) = ama                                           PARCO222
        xymas(1,10,jhig) = amh                                          PARCO223
        xymas(1,11,jhig) = 0.                                           PARCO224
        xymas(1,12,jhig) = ame                                          PARCO225
        xymas(1,13,jhig) = ammu                                         PARCO226
        xymas(1,14,jhig) = ams                                          PARCO227
        xymas(1,15,jhig) = 0.                                           PARCO228
        xymas(1,16,jhig) = 0.                                           PARCO229
        DO jchan = 1 , nchan                                            PARCO230
          xymas(2,jchan,jhig) = xymas(1,jchan,jhig)                     PARCO231
        ENDDO                                                           PARCO232
        xymas(2,11,jhig) = amz                                          PARCO233
      ENDDO                                                             PARCO234
      xymas(1,9,3) = amh                                                PARCO235
      xymas(2,9,3) = amz                                                PARCO236
C------------------------------------------------------------------     PARCO237
 1000 FORMAT(1x,50('-')//                                               PARCO238
     .       1x,'With the following input parameters of the MSSM :'/    PARCO239
     .       1x,'   . A mass[Run] : ',F8.3,' GeV/c**2'/                 PARCO240
     .       1x,'   . Tan beta    : ',F8.3/                             PARCO241
     .       1x,'   . M           : ',F8.3,' GeV/c**2'/                 PARCO242
     .       1x,'   . mu          : ',F8.3,' GeV/c**2'/                 PARCO243
     .       1x,'   . At          : ',F8.3/                             PARCO244
     .       1x,'   . Ab          : ',F8.3/                             PARCO245
     .       1x,'   . mQ          : ',F8.3,' GeV/c**2'/                 PARCO246
     .       1x,'   . mU          : ',F8.3,' GeV/c**2'/                 PARCO247
     .       1x,'   . mD          : ',F8.3,' GeV/c**2'/                 PARCO248
     .       1x,'   . mL          : ',F8.3,' GeV/c**2'/                 PARCO249
     .       1x,'   . mE          : ',F8.3,' GeV/c**2'/)                PARCO250
 1001 FORMAT(1x,'we found the following Higgs masses :'/                PARCO251
     .       1x,'   . h mass      : ',F8.3,' GeV/c**2'/                 PARCO252
     .       1x,'   . H mass      : ',F8.3,' GeV/c**2'/                 PARCO253
     .       1x,'   . A mass[Pole]: ',F8.3,' GeV/c**2'/                 PARCO254
     .       1x,'   . H+/- mass   : ',F8.3,' GeV/c**2'/)                PARCO255
 1002 FORMAT(1x,'and sin**2(beta-alfa) = ',F8.6//                       PARCO256
     .       1x,50('-')//)                                              PARCO257
 1003 FORMAT(1x,50('-')//                                               PARCO258
     .       1x,'With the following input parameters :'/                PARCO259
     .       1x,'   . Z mass      : ',F10.4,' GeV/c**2'/                PARCO260
     .       1x,'   . Z width     : ',F10.4,' GeV'/                     PARCO261
     .       1x,'   . top mass    : ',F10.4,' GeV/c**2'/                PARCO262
     .       1x,'   . G_F         : ',E10.4,' GeV**-2'/                 PARCO263
     .       1x,'   . Lamba_QCD(5): ',F10.4,' GeV'/)                    PARCO264
 1004 FORMAT(1x,'we found the following values :'/                      PARCO265
     .       1x,'   . alpha(mZ)   : 1/',F8.3/                           PARCO266
     .       1x,'   . sin**2(Th_W): ',F10.4/                            PARCO267
     .       1x,'   . delta(rho)  : ',E10.4/                            PARCO268
     .       1x,'   . W mass      : ',F10.4,' GeV/c**2'/                PARCO269
     .       1x,'   . alphas(mZ)  : ',F10.4,/)                          PARCO270
 2001 FORMAT(1x,' and ... the following squark masses :'/               PARCO271
     .       1x,'   . t~1 mass    : ',F8.3,' GeV/c**2'/                 PARCO272
     .       1x,'   . t~2 mass    : ',F8.3,' GeV/c**2'/                 PARCO273
     .       1x,'   . Mixing      : ',F8.4,' rd.'/                      PARCO274
     .       1x,'   . b~1 mass    : ',F8.3,' GeV/c**2'/                 PARCO275
     .       1x,'   . b~2 mass    : ',F8.3,' GeV/c**2'/                 PARCO276
     .       1x,'   . Mixing      : ',F8.4,' rd.'/)                     PARCO277
 2003 FORMAT(1x,' and ... the following neutralino masses :'/           PARCO278
     .       1x,'   . chi(1) mass : ',F8.3,' GeV/c**2'/                 PARCO279
     .       1x,'   . chi(2) mass : ',F8.3,' GeV/c**2'/                 PARCO280
     .       1x,'   . chi(3) mass : ',F8.3,' GeV/c**2'/                 PARCO281
     .       1x,'   . chi(4) mass : ',F8.3,' GeV/c**2'/)                PARCO282
 2004 FORMAT(1x,' and ... the following chargino masses :'/             PARCO283
     .       1x,'   . chi1+- mass : ',F8.3,' GeV/c**2'/                 PARCO284
     .       1x,'   . chi2+- mass : ',F8.3,' GeV/c**2'/)                PARCO285
 2005 FORMAT(/1x,'Branching ratios for the neutralino ',                PARCO286
     .           I1, ' (mass ',F8.3, ' GeV/c**2)')                      PARCO287
 2006 FORMAT(/1x,'Branching ratios for the chargino ',                  PARCO288
     .           I1, ' (mass ',F8.3, ' GeV/c**2)')                      PARCO289
 2007 FORMAT(10x,'o ',A21,1x,20('.'),1x,F9.5,'%')                       PARCO290
 2008 FORMAT(//'                  +++ WARNING +++'/                     PARCO291
     .       '      The ligthest neutralino is NOT the LSP ! '/         PARCO292
     .       '      The program  will stop  when  trying to '/          PARCO293
     .       '           decay the lightest chargino'/                  PARCO294
     .       '                  +++ WARNING +++'//)                     PARCO295
 2002 FORMAT(/1x,' and ... the following couplings :'/                  PARCO296
     .       1x,'   . hWW or hZZ  : ',F8.3,/                            PARCO297
     .       1x,'   . HWW or HZZ  : ',F8.3,/                            PARCO298
     .       1x,'   . Htt         : ',F8.3,/                            PARCO299
     .       1x,'   . Hbb         : ',F8.3,/                            PARCO300
     .       1x,'   . htt         : ',F8.3,/                            PARCO301
     .       1x,'   . hbb         : ',F8.3,/                            PARCO302
     .       1x,'   . Att         : ',F8.3,/                            PARCO303
     .       1x,'   . Abb         : ',F8.3,/)                           PARCO304
  999 RETURN                                                            PARCO305
      END                                                               PARCO306
      FUNCTION phspgz(a,b,c)                                            PHSPGZ 2
C ------------------------------------------------------------------    PHSPGZ 3
C! Phase space factor for  h --> gamma Z                                PHSPGZ 4
C                                                                       PHSPGZ 5
C  Inputs:        a   is the Higgs mass squared                         PHSPGZ 6
C                 b   is the Z mass                                     PHSPGZ 7
C                 c,  is the Z width                                    PHSPGZ 8
C                                                                       PHSPGZ 9
C  Output:        phspgz, the hZ cross section with width effects       PHSPGZ10
C                                                                       PHSPGZ11
C  Patrick Janot -- 01 Sep 1995                                         PHSPGZ12
C -------------------------------------------------------------------   PHSPGZ13
      IMPLICIT REAL*8(A-H,O-Z)                                          PHSPGZ14
      REAL*4 a,b,c                                                      PHSPGZ15
      COMMON /BHV/ s,am1,w1                                             PHSPGZ16
      EXTERNAL fsubgz                                                   PHSPGZ17
      DIMENSION x(1)                                                    PHSPGZ18
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)        DATAPI 2
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          DATAPI 3
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         DATAPI 4
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         DATAPI 5
C                                                                       DATAPI 6
      s      = a                                                        PHSPGZ20
      am1    = b                                                        PHSPGZ21
      w1     = c                                                        PHSPGZ22
      xlo    = -DATAN2(am1,w1)                                          PHSPGZ23
      xhi    =  DATAN2(s-am1**2,am1*w1)                                 PHSPGZ24
      phspgz = DGMLT1(fsubgz,xlo,xhi,1,6,x)                             PHSPGZ25
     .       / (piby2+DATAN2(am1,w1))                                   PHSPGZ26
      RETURN                                                            PHSPGZ27
      END                                                               PHSPGZ28
      SUBROUTINE pick4(ph,jchan,jhig)                                   PICK4  2
C------------------------------------------------------------------     PICK4  3
C!  Derive the quadrimomenta of the Higgs decay particles               PICK4  4
C                                                                       PICK4  5
C  P. Janot --  26 Aug 1991                                             PICK4  6
C------------------------------------------------------------------     PICK4  7
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      CHARACTER*14 channel                                              HHDECK 2
      CHARACTER*21 channeut, chanchar                                   HHDECK 3
      PARAMETER(nchneut=8,nchchar=5)                                    HHDECK 4
      COMMON / hhdeck / branch(nchan,nhig),width(nhig),                 HHDECK 5
     .                  parwth(nhig),xymas(2,nchan,nhig),               HHDECK 6
     .                  xywid(2,nchan,nhig)                             HHDECK 7
      COMMON / chaneu / ichn(2),                                        HHDECK 8
     .                  wneut(4,4,nhig), wchar(2,2,nhig),               HHDECK 9
     .                  widneut(4), brneut(nchneut,4),                  HHDECK10
     .                  widchar(2), brchar(nchchar,2)                   HHDECK11
      COMMON / chanch / channeut(nchneut,4), chanchar(nchchar,2)        HHDECK12
      COMMON / vect4 / pvect4(5,2)                                      HHDECK13
      COMMON / hinput / ichan(nchan,nhig), channel(nchan,nhig)          HHDECK14
      COMMON / hisbr / bchpp,bchgg                                      HHDECK15
      DIMENSION ph(4)                                                   HHDECK16
C                                                                       HHDECK17
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)        DATAPI 2
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          DATAPI 3
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         DATAPI 4
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         DATAPI 5
C                                                                       DATAPI 6
      DIMENSION ptot(4),pp(4),qq(4),cormas(2)                           PICK4 11
      REAL*8 betax, betay, betaz, p4                                    PICK4 12
      CALL vzero(pvect4(1,1),10)                                        PICK4 13
C                                                                       PICK4 14
      amloc = ph(4)                                                     PICK4 15
C                                                                       PICK4 16
C Generate mass according to the width                                  PICK4 17
C                                                                       PICK4 18
      DO idc = 1 , 2                                                    PICK4 19
        IF ( idbg .GE. 10 ) WRITE(6,*) idc                              PICK4 20
     .      ,xymas(idc,jchan,jhig)                                      PICK4 21
     .      ,xywid(idc,jchan,jhig)                                      PICK4 22
        IF ( xywid(idc,jchan,jhig) .GT. 0.05 ) THEN                     PICK4 23
          x = xymas(idc,jchan,jhig)                                     PICK4 24
          g = xywid(idc,jchan,jhig)                                     PICK4 25
          IF ( idc .EQ. 1 ) THEN                                        PICK4 26
            xmin = 2.*ame                                               PICK4 27
            xmax = amloc-xmin                                           PICK4 28
          ELSE                                                          PICK4 29
            xmin = 2.*ame                                               PICK4 30
            xmax = amloc-cormas(1)                                      PICK4 31
          ENDIF                                                         PICK4 32
          CALL bwgene(xmin,xmax,x,g,cormas(idc),djdummy)                PICK4 33
        ELSE                                                            PICK4 34
          cormas(idc) = xymas(idc,jchan,jhig)                           PICK4 35
        ENDIF                                                           PICK4 36
      ENDDO                                                             PICK4 37
C                                                                       PICK4 38
C  Special treatment for WW, ZZ, Z gamma                                PICK4 39
C                                                                       PICK4 40
      IF ( jchan .EQ. 7 .OR. jchan .EQ. 8 ) THEN                        PICK4 41
        xma2 = xmax**2                                                  PICK4 42
        xh2  = amloc**2                                                 PICK4 43
        xm12 = cormas(1)**2                                             PICK4 44
    1   xm22 = cormas(2)**2                                             PICK4 45
        weight = SQRT((xh2-xm12-xm22)**2-4.*xm12*xm22)                  PICK4 46
     .         * (xm12*xm22 + ((xh2-xm12-xm22)**2-4.*xm12*xm22)/12.)    PICK4 47
        wmax   = (xh2-xm12) * (xm12*xma2 + (xh2-xm12)**2/12.)           PICK4 48
        IF ( weight/wmax .LT. RNDM(weight) ) THEN                       PICK4 49
          CALL bwgene(xmin,xmax,x,g,cormas(2),djdummy)                  PICK4 50
          GOTO 1                                                        PICK4 51
        ENDIF                                                           PICK4 52
C                                                                       PICK4 53
        IF ( RNDM(xm1) .GT. 0.5 ) THEN                                  PICK4 54
          xm1 = cormas(1)                                               PICK4 55
          cormas(1) = cormas(2)                                         PICK4 56
          cormas(2) = xm1                                               PICK4 57
        ENDIF                                                           PICK4 58
C                                                                       PICK4 59
      ENDIF                                                             PICK4 60
C                                                                       PICK4 61
C  Special treatment for Z gamma                                        PICK4 62
C                                                                       PICK4 63
      IF ( jchan .EQ. 11 ) THEN                                         PICK4 64
    2   weight = (1.-cormas(2)**2/amloc**2)**3                          PICK4 65
        IF ( weight .LT. RNDM(weight) ) THEN                            PICK4 66
          CALL bwgene(xmin,xmax,x,g,cormas(2),djdummy)                  PICK4 67
          GOTO 2                                                        PICK4 68
        ENDIF                                                           PICK4 69
      ENDIF                                                             PICK4 70
C                                                                       PICK4 71
C Compute quadri-momenta                                                PICK4 72
C                                                                       PICK4 73
      pvect4(4,1) = (amloc**2+cormas(1)**2-cormas(2)**2)                PICK4 74
     .            / (amloc*2.)                                          PICK4 75
      pvect4(4,2) = (amloc**2+cormas(2)**2-cormas(1)**2)                PICK4 76
     .            / (amloc*2.)                                          PICK4 77
      pmom2       = (amloc**2-(cormas(1)+cormas(2))**2)                 PICK4 78
     .            * (amloc**2-(cormas(1)-cormas(2))**2)                 PICK4 79
      pmom = SQRT(pmom2)/(2.*amloc)                                     PICK4 80
      c = 2.*rndm(c) - 1.                                               PICK4 81
      p = 2.*pi*rndm(p)                                                 PICK4 82
      s = SQRT(1.-c**2)                                                 PICK4 83
      pvect4(3,1) = pmom * c                                            PICK4 84
      pvect4(2,1) = pmom * s * SIN(p)                                   PICK4 85
      pvect4(1,1) = pmom * s * COS(p)                                   PICK4 86
      pvect4(3,2) =-pvect4(3,1)                                         PICK4 87
      pvect4(2,2) =-pvect4(2,1)                                         PICK4 88
      pvect4(1,2) =-pvect4(1,1)                                         PICK4 89
C                                                                       PICK4 90
C  Boost back to the lab                                                PICK4 91
C                                                                       PICK4 92
      CALL ucopy(pvect4(1,1),pp(1),4)                                   PICK4 93
      CALL ucopy(pvect4(1,2),qq(1),4)                                   PICK4 94
      betax = -ph(1)                                                    PICK4 95
      betay = -ph(2)                                                    PICK4 96
      betaz = -ph(3)                                                    PICK4 97
      p4 = SQRT(amloc**2+betax**2+betay**2+betaz**2)                    PICK4 98
      betax = betax/p4                                                  PICK4 99
      betay = betay/p4                                                  PICK4100
      betaz = betaz/p4                                                  PICK4101
      CALL lorenz(betax,betay,betaz,pp)                                 PICK4102
      CALL lorenz(betax,betay,betaz,qq)                                 PICK4103
      CALL ucopy(pp(1),pvect4(1,1),4)                                   PICK4104
      CALL ucopy(qq(1),pvect4(1,2),4)                                   PICK4105
      pvect4(5,1) = cormas(1)                                           PICK4106
      pvect4(5,2) = cormas(2)                                           PICK4107
C                                                                       PICK4108
      amas1 = (pvect4(4,1)**2                                           PICK4109
     .      -  pvect4(1,1)**2                                           PICK4110
     .      -  pvect4(2,1)**2                                           PICK4111
     .      -  pvect4(3,1)**2)                                          PICK4112
      amas2 = (pvect4(4,2)**2                                           PICK4113
     .      -  pvect4(1,2)**2                                           PICK4114
     .      -  pvect4(2,2)**2                                           PICK4115
     .      -  pvect4(3,2)**2)                                          PICK4116
      pxtot = pvect4(1,1) + pvect4(1,2)                                 PICK4117
      pytot = pvect4(2,1) + pvect4(2,2)                                 PICK4118
      pztot = pvect4(3,1) + pvect4(3,2)                                 PICK4119
      entot = pvect4(4,1) + pvect4(4,2)                                 PICK4120
      amtot = entot**2-pxtot**2-pytot**2-pztot**2                       PICK4121
C                                                                       PICK4122
      IF ( idbg .GE. 10 ) THEN                                          PICK4123
        WRITE(6,1000) (pvect4(i,1),i=1,4),SQRT(amas1)                   PICK4124
        WRITE(6,1000) (pvect4(i,2),i=1,4),SQRT(amas2)                   PICK4125
        WRITE(6,1000) pxtot,pytot,pztot,entot,SQRT(amtot)               PICK4126
      ENDIF                                                             PICK4127
C                                                                       PICK4128
  999 RETURN                                                            PICK4129
C------------------------------------------------------------------     PICK4130
1000  FORMAT(1X,5(2X,F9.3))                                             PICK4131
      END                                                               PICK4132
      SUBROUTINE pole(ihiggs,mchi,ma,tanb,mq,mur,mdr,mtop,at,ab,mu,     POLE   2
     *                mh,mhp,hm,hmp,amp,mhch,sa,ca,                     POLE   3
     *                stop1,stop2,sbot1,sbot2,tanbA)                    POLE   4
C------------------------------------------------------------------     POLE   5
C! Computes the Higgs pole masses and mixing angles.                    POLE   6
C                                                                       POLE   7
C    Inputs: ihiggs(explained below),mchi,ma,tanb,mq,mur,mdr,mtop,      POLE   8
C    at,ab,mu                                                           POLE   9
C                                                                       POLE  10
C    where mchi is the largest chargino mass, ma is the running         POLE  11
C    CP-odd Higgs mass, tanb is the value of the ratio of vacuum        POLE  12
C    expectaion values at the scale mtop,mq is the third generation     POLE  13
C    left handed squark mass parameter, mur is the third generation     POLE  14
C    right handed stop mass parameter, mdr is the third generation      POLE  15
C    right handed sbottom mass parameter, mtop is the pole top quark    POLE  16
C    mass; at,ab are the soft supersymmetry breaking trilinear          POLE  17
C    couplings of the stop and sbottoms, respectively, and mu is the    POLE  18
C    supersymmetric mass parameter.                                     POLE  19
C                                                                       POLE  20
C                                                                       POLE  21
C    Output: mh and mhp which are the lightest CP-even Higgs running    POLE  22
C    and pole masses, respectively; hm and hmp are the heaviest CP-even POLE  23
C    Higgs running and pole masses, repectively; sa and ca are the      POLE  24
C    sin(alpha) and cos(alpha) where alpha is the Higgs mixing angle.   POLE  25
C    amp is the CP-odd Higgs pole mass. stop1,stop2,sbot1 and sbot2     POLE  26
C    are the stop and sbottom mass eigenvalues. Finally, tanbA is       POLE  27
C    the value of tanb at the CP-odd Higgs mass scale.                  POLE  28
C                                                                       POLE  29
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCcC        POLE  30
ccccccccccccccccccccccccccccccccccccccccccc                             POLE  31
ccccc  The parameter ihiggs=0,1,2,3 corresponds to the                  POLE  32
ccccc  number of Higgses whose pole mass is computed                    POLE  33
ccccc   by the subroutine vac(...). If ihiggs=0 only running            POLE  34
ccccc   masses are given, what makes the running of the program         POLE  35
ccccc   much faster and it is quite generally a good approximation      POLE  36
ccccc   (for a theoretical discussion see Ref. below).                  POLE  37
ccccc    If ihiggs=1, only the pole                                     POLE  38
ccccc   mass for h is computed. If ihiggs=2, then h and H, and          POLE  39
ccccc   if ihiggs=3, then h,H,A polarizations are computed              POLE  40
ccccccccccccccccccccccccccccccccccccccccccccccc                         POLE  41
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc         POLE  42
c                                                                       POLE  43
c       Program based on the work by M. Carena, M. Quiros               POLE  44
c       and C.E.M. Wagner, "Effective potential methods and             POLE  45
c       the Higgs mass spectrum in the MSSM", CERN-TH/95-157.           POLE  46
c                                                                       POLE  47
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc          POLE  48
C                                                                       POLE  49
      IMPLICIT REAL*8(A-H,M,O-Z)                                        POLE  50
      CALL vac(ihiggs,mchi,ma,tanb,mq,mur,mdr,mtop,at,ab,mu,            POLE  51
     *        mh,mhp,hm,hmp,amp,mhch,stop1,stop2,sbot1,sbot2,           POLE  52
     *        sa,ca,stop1w,stop2w,tanbA)                                POLE  53
      sinb = tanb/(tanb**2+1.)**.5                                      POLE  54
      cosb = 1./(tanb**2+1.)**.5                                        POLE  55
      sinbma = sinb*ca - cosb*sa                                        POLE  56
      RETURN                                                            POLE  57
      END                                                               POLE  58
      FUNCTION prosca(p1,p2)                                            PROSCA 2
C-------------------------------------------------------------------    PROSCA 3
C! Compute the scalar product p1.p2                                     PROSCA 4
C-------------------------------------------------------------------    PROSCA 5
      DIMENSION p1(4), p2(4)                                            PROSCA 6
      prosca = p1(4)*p2(4) - p1(3)*p2(3) - p1(2)*p2(2) - p1(1)*p2(1)    PROSCA 7
      RETURN                                                            PROSCA 8
      END                                                               PROSCA 9
      SUBROUTINE radcor(xmq,jhig,fnq,r1,r2)                             RADCOR 2
C-------------------------------------------------------------------    RADCOR 3
C! Compute the 1st order rad. cor. to H --> qqbar(g) width              RADCOR 4
C                                                                       RADCOR 5
C  Inputs:       --xmq,   the running quark mass                        RADCOR 6
C                --jhig,  the Higgs type                                RADCOR 7
C                --fnq,   the quark flavour index                       RADCOR 8
C                                                                       RADCOR 9
C  Ouptuts:      --r1,    the alpha_s    correction                     RADCOR10
C                --r2,    the alpha_s**2 correction                     RADCOR11
C                                                                       RADCOR12
C  P. Janot  --  22 nov 1989.                                           RADCOR13
C------------------------------------------------------------------     RADCOR14
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      REAL*8 betaq, abeta                                               RADCOR16
      xmh = amhig(jhig)                                                 RADCOR17
      IF ( xmh .LT. 2.*xmq ) THEN                                       RADCOR18
C       betaq = DSQRT(1D0-4D0*xmq**2/xmh**2)                            RADCOR19
C       abeta = (1.+betaq**2)                                           RADCOR20
C    .        * (4.*DDILOG( (1D0-betaq)/(1D0+betaq))                    RADCOR21
C    .        +  2.*DDILOG(-(1D0-betaq)/(1D0+betaq))                    RADCOR22
C    .        -  3.*DLOG(2D0/(1D0+betaq))*DLOG((1D0+betaq)/(1D0-betaq)) RADCOR23
C    .        -  2.*DLOG(betaq)*DLOG((1D0+betaq)/(1D0-betaq)))          RADCOR24
C    .        -  3.*betaq*DLOG(4D0/(1D0-betaq**2))                      RADCOR25
C    .        -  4.*betaq*DLOG(betaq)                                   RADCOR26
C       IF ( jhig .LE. 2 ) THEN                                         RADCOR27
C         r1 = abeta/betaq                                              RADCOR28
C    .       + (3.+34.*betaq**2-13.*betaq**4)                           RADCOR29
C    .       / (16.*betaq**3)                                           RADCOR30
C    .       * DLOG((1D0+betaq)/(1D0-betaq))                            RADCOR31
C    .       + 3. * (-1.+7.*betaq**2) / (8.*betaq**2)                   RADCOR32
C       ELSEIF (jhig .EQ. 3 ) THEN                                      RADCOR33
C         r1 = abeta/betaq                                              RADCOR34
C    .       + (19.+2.*betaq**2+3.*betaq**4)                            RADCOR35
C    .       / (16.*betaq)                                              RADCOR36
C    .       * DLOG((1D0+betaq)/(1D0-betaq))                            RADCOR37
C    .       + 3. * (7.-betaq**2) / 8.                                  RADCOR38
C       ELSE                                                            RADCOR39
C         r1 = 3.*ALOG(xmq/xmh)                                         RADCOR40
C       ENDIF                                                           RADCOR41
C       r1 = r1 - 3.*ALOG(xmq/xmh)                                      RADCOR42
C       r2 = 0.                                                         RADCOR43
        r1 = 0.                                                         RADCOR44
        r2 = 0.                                                         RADCOR45
      ELSE                                                              RADCOR46
        r1 = 17./3.-40.*(xmq/xmh)**2                                    RADCOR47
        r2 = 35.9399-1.3586*fnq                                         RADCOR48
      ENDIF                                                             RADCOR49
C                                                                       RADCOR50
  999 RETURN                                                            RADCOR51
      END                                                               RADCOR52
      SUBROUTINE REMT1(EBEAM,CROSS,STHR,INDEX,sig1,xrad)                REMT1  2
C-----------------------------------------------------------------------REMT1  3
C! The famous Kleiss initial state radiator, modified for two photons.  REMT1  4
C                                                                       REMT1  5
C It calculates some quantities, and performs the                       REMT1  6
C numerical integration over the photon spectrum.                       REMT1  7
C                                                                       REMT1  8
C EBEAM=Beam energy (in gev)                                            REMT1  9
C CROSS=Nonradiative cross section, to be defined                       REMT1 10
C       with one variable: cross(s),                                    REMT1 11
C       where s is the invariant mass of the e+e- pair.                 REMT1 12
C STHR =The kinematical threshold, i.e. the lowest allowed              REMT1 13
C       value of s for which the nonradiative process can               REMT1 14
C       take place ( in gev**2 )                                        REMT1 15
C XRAD =The fudge factor that increases the beta to simulate            REMT1 16
C       the beamstrahlung                                               REMT1 17
C                                                                       REMT1 18
c 851015 A. Schwarz. have to call REMT1 always when mass                REMT1 19
c        of Z0-decay products changes, since threshold changes          REMT1 20
c        as well; dimension of X(1000) changed to X(1000,10)            REMT1 21
C 921109 P. Janot. Change the effective distribution in 1/x             REMT1 22
C        integrated between a fictive xmin and 1. to the expo-          REMT1 23
C        nentiated distribution in 1/x**(1-beta) integrated             REMT1 24
C        between 0. and 1. as it should be.                             REMT1 25
C 921110 P. Janot. Generation of 2 ISR photons.                         REMT1 26
C 921111 P. Janot. Implementation of effective beamstrahlung option     REMT1 27
C-----------------------------------------------------------------------REMT1 28
      IMPLICIT REAL*8(A-H,O-Z)                                          REMT1 29
      COMMON / flags  / idbg                                            REMT1 30
      DIMENSION X(1000,10),F(1000),A(1000),Y(1000),Z(1000),XNEW(1000)   REMT1 31
      DIMENSION xn(10),wmax(10)                                         REMT1 32
      REAL*4 QK(4),QK1(4),QK2(4),QIN(4),QOUT(4),QKA(4),QKB(4)           REMT1 33
      REAL*4 ebeam, cross, sthr, sig1, xrad, sig0, delt                 REMT1 34
      REAL*4 rndm                                                       REMT1 35
      EXTERNAL cross                                                    REMT1 36
      DATA INIT/0/                                                      REMT1 37
C                                                                       REMT1 38
      SAVE                                                              REMT1 39
C                                                                       REMT1 40
C DEFINITION OF BREMSSTRAHLUNG SPECTRUM                                 REMT1 41
      DATA ame/.511D-3/                                                 REMT1 42
C                                                                       REMT1 43
C Definition of Bremsstrahlung spectrum                                 REMT1 44
C                                                                       REMT1 45
      xkmax(sb)  = 1.-sthr/sb                                           REMT1 46
      xps(sb)    = 2.*ame**2/sb                                         REMT1 47
      xpt(sb)    = (2.+xps(sb))/xps(sb)                                 REMT1 48
      beta(sb)   = 2.*alf/pi*(1.-2.*xps(sb))*(xpl-1.)*xrad              REMT1 49
      spectr(xk) = bet * dj1                                            REMT1 50
     .                 * (1.+3.*bet/4.+alf/pi*(pi**2/3.-1./2.)          REMT1 51
     .                 - xk**(1.-bet) + xk**(2.-bet)/2.                 REMT1 52
     .                 - bet**2/24.*(2.*pi**2-37./4.+xpl/3.)            REMT1 53
     .                 + bet/8.*strange(xk,bet) )                       REMT1 54
     .                 * cross(SNGL(s*(1.-xk)))                         REMT1 55
C        Store EBEAM into local variable EBEA for later use             REMT1 56
      EBEA = EBEAM                                                      REMT1 57
C                                                                       REMT1 58
C Initialize a few quantities and constants                             REMT1 59
C                                                                       REMT1 60
      s   = 4.*ebeam**2                                                 REMT1 61
      sb  = s                                                           REMT1 62
      pi  = 4.*DATAN(1.d0)                                              REMT1 63
      tpi = 2.*pi                                                       REMT1 64
C     alf = ulalem(SNGL(s))                                             REMT1 65
      alf = 1./137.035989                                               REMT1 66
      xpl  = DLOG(xpt(s))                                               REMT1 67
      x1   = 0.                                                         REMT1 68
      xn(index) = xkmax(s)                                              REMT1 69
      wmax(index) = 0.                                                  REMT1 70
      bet  = beta(s)                                                    REMT1 71
      dj1 = xn(index)**bet/bet                                          REMT1 72
      sig1 = 0.                                                         REMT1 73
C                                                                       REMT1 74
C Parameters of numerical integration step                              REMT1 75
      N    = 500                                                        REMT1 76
      ITER = 6                                                          REMT1 77
C                                                                       REMT1 78
C Initialize by choosing equidistant u values ( du = Cx**(beta-1)dx)    REMT1 79
C with an increased sampling around mz.                                 REMT1 80
C                                                                       REMT1 81
      IF ( idbg .GE. 0 ) THEN                                           REMT1 82
        WRITE(6,*) ' '                                                  REMT1 83
        WRITE(6,*) '----------------------------------------'           REMT1 84
        WRITE(6,*) '    Compute radiative corrections      '            REMT1 85
        WRITE(6,*) '        for process # ',index                       REMT1 86
        WRITE(6,*) '----------------------------------------'           REMT1 87
      ENDIF                                                             REMT1 88
      IT=0                                                              REMT1 89
      M=N-1                                                             REMT1 90
      DU=1./FLOAT(M)                                                    REMT1 91
      X(1,INDEX)=0.                                                     REMT1 92
      DO 101 I=2,N                                                      REMT1 93
  101 X(I,INDEX)=X(I-1,INDEX)+DU                                        REMT1 94
C                                                                       REMT1 95
C Starting point for iterations                                         REMT1 96
C                                                                       REMT1 97
  100 CONTINUE                                                          REMT1 98
C                                                                       REMT1 99
C Calculate function values                                             REMT1100
C                                                                       REMT1101
      DO 102 I=1,N                                                      REMT1102
  102 F(I)=SPECTR(xn(index)*X(I,INDEX)**(1./bet))                       REMT1103
C                                                                       REMT1104
C CALCULATE BIN AREAS                                                   REMT1105
      DO 103 I=1,M                                                      REMT1106
  103 A(I)=(X(I+1,INDEX)-X(I,INDEX))*(F(I+1)+F(I))/2.                   REMT1107
C                                                                       REMT1108
C CALCULATE CUMULATIVE SPECTRUM Y VALUES                                REMT1109
      Y(1)=0.D0                                                         REMT1110
      DO 104 I=2,N                                                      REMT1111
  104 Y(I)=Y(I-1)+A(I-1)                                                REMT1112
C                                                                       REMT1113
C PUT EQUIDISTANT POINTS ON Y SCALE                                     REMT1114
      DZ=Y(N)/FLOAT(M)                                                  REMT1115
      Z(1)=0.D0                                                         REMT1116
      DO 105 I=2,N                                                      REMT1117
  105 Z(I)=Z(I-1)+DZ                                                    REMT1118
C                                                                       REMT1119
C DETERMINE SPACING OF Z POINTS IN BETWEEN Y POINTS                     REMT1120
C FROM THIS, DETERMINE NEW X VALUES AND FINALLY REPLACE OLD VALUES      REMT1121
      XNEW(1)=X(1,INDEX)                                                REMT1122
      XNEW(N)=X(N,INDEX)                                                REMT1123
      K=1                                                               REMT1124
      DO 108 I=2,M                                                      REMT1125
  106 IF( Y(K+1) .GT. Z(I) ) GOTO 107                                   REMT1126
      K=K+1                                                             REMT1127
      GOTO 106                                                          REMT1128
  107 R= ( Z(I) - Y(K) ) / ( Y(K+1) - Y(K) )                            REMT1129
  108 XNEW(I) = X(K,INDEX) + ( X(K+1,INDEX)-X(K,INDEX) )*R              REMT1130
      DO 109 I=1,N                                                      REMT1131
  109 X(I,INDEX)=XNEW(I)                                                REMT1132
C                                                                       REMT1133
C CHECK ON END OF ITERATIONS AND RETURN                                 REMT1134
      IT=IT+1                                                           REMT1135
      IF ( it .LE. 3 ) THEN                                             REMT1136
        SIG1 = 0.                                                       REMT1137
        NT = 1                                                          REMT1138
      ELSE                                                              REMT1139
        NT = IT-2                                                       REMT1140
      ENDIF                                                             REMT1141
      SIG1 = SIG1+Y(M)                                                  REMT1142
C     PRINT 3,IT,SIG1/FLOAT(NT)                                         REMT1143
    3 FORMAT(' Iteration # ',i3,'  Integral =',e15.6)                   REMT1144
      IF(IT.LT.ITER) GOTO 100                                           REMT1145
C                                                                       REMT1146
C PRESENT RESULTS IN FORM OF CORRECTION                                 REMT1147
      SIG0 = CROSS(SNGL(S))                                             REMT1148
      SIG1 = SIG1/FLOAT(NT)                                             REMT1149
      DELT = (SIG1/SIG0-1.)*100.                                        REMT1150
C     IF(INIT.GT.1) RETURN                                              REMT1151
C     INIT = 2                                                          REMT1152
      IF ( idbg .GE. 0 ) PRINT 4,SIG0,SIG1,DELT                         REMT1153
    4 FORMAT(/' Results of the initialization step :',/,                REMT1154
     .        ' Nonradiative cross section :',e15.6,/,                  REMT1155
     .        '    Radiative cross section :',e15.6,/,                  REMT1156
     .        '    Radiative correction    :',f10.3,' %',/)             REMT1157
C     WRITE(6,500) (SQRT(s*(1.-xn*x(i,index)**(1./bet))),i=1,500)       REMT1158
C 500 FORMAT(10(F10.5,1X))                                              REMT1159
      RETURN                                                            REMT1160
      ENTRY REMT2(QK1,QK2,IDEC)                                         REMT1161
C-----------------------------------------------------------------------REMT1162
C THIS PART GENERATES A BREMSSTRAHLUNG PHOTON                           REMT1163
C AND CALCULATES WHICH BEAM AXIS TO CHOOSE FOR                          REMT1164
C THE GENERATION OF THE 'NONRADIATIVE' CROSS SECTION.                   REMT1165
C THE PHOTON ENERGY SPECTRUM MUST HAVE BEEN EXAMINED                    REMT1166
C BY CALLING ENTRY 'REMT1' BEFORE THE FIRST CALL TO                     REMT1167
C THIS ENTRY.                                                           REMT1168
C-----------------------------------------------------------------------REMT1169
C                                                                       REMT1170
C INITIALIZE FLAG FOR REMT3                                             REMT1171
C     INDX = MINDEX(IDEC)                                               REMT1172
      INDX = IDEC                                                       REMT1173
      IR=0                                                              REMT1174
C                                                                       REMT1175
C GENERATE total PHOTON ENERGY FROM CUMULATIVE SPECTRUM BINS            REMT1176
  200 R=M*RNDM(1.)                                                      REMT1177
      I=INT(R)                                                          REMT1178
      S=R-I                                                             REMT1179
      UK = X(I+1,INDX) + S*( X(I+2,INDX)-X(I+1,INDX) )                  REMT1180
      XK = xn(INDX)*uk**(1./bet)                                        REMT1181
      IF ( xk .LE. 1D-17 ) xk = 1D-17                                   REMT1182
C                                                                       REMT1183
C Generate the energy of the photons                                    REMT1184
C                                                                       REMT1185
      IF ( xk .LE. 1D-6 ) THEN                                          REMT1186
        yx = xk**2/4.                                                   REMT1187
      ELSE                                                              REMT1188
        yx = (1.-SQRT(1.-xk))**2                                        REMT1189
      ENDIF                                                             REMT1190
      yk = (RNDM(0.)*yx**bet)**(1./bet)                                 REMT1191
      IF ( yk/xk .GT. 1D-6 .OR. yk/xk**2 .GT. 1D-6 ) THEN               REMT1192
        xka  = .5*( (xk+yk)-SQRT((xk+yk)**2-4.*yk) )                    REMT1193
      ELSE                                                              REMT1194
        xka = yk/xk                                                     REMT1195
      ENDIF                                                             REMT1196
C 1D-15 rather than 1D-20 for these poor UNIX computers                 REMT1197
      IF ( xka .LE. 1D-15 ) xka = 1D-15                                 REMT1198
      xkb  = xk+yk-yk/xk                                                REMT1199
      IF ( xkb .LE. 1D-15 ) xkb = 1D-15                                 REMT1200
C                                                                       REMT1201
      IF ( RNDM(0.2) .GT. 0.5 ) THEN                                    REMT1202
        xk1 = xka                                                       REMT1203
        xk2 = xkb                                                       REMT1204
      ELSE                                                              REMT1205
        xk2 = xka                                                       REMT1206
        xk1 = xkb                                                       REMT1207
      ENDIF                                                             REMT1208
C                                                                       REMT1209
C GENERATE AZIMUTHAL SCATTERING ANGLE OF THE two PHOTONs                REMT1210
      FG1 = TPI*RNDM(1.0)                                               REMT1211
      FG2 = TPI*RNDM(1.5)                                               REMT1212
C                                                                       REMT1213
C GENERATE COSINE OF POLAR SCATTERING ANGLE OF THE two PHOTONs          REMT1214
      sb1 = sb                                                          REMT1215
  201 V1 = XPS(sb1) * ( XPT(sb1)**RNDM(2.0) - 1. )                      REMT1216
      W1 = XPS(sb1) + V1*(1.-.5*V1)                                     REMT1217
      W1 = RNDM(3.0)/(1.-(XK1*XK1*W1+2.*XPS(Sb1)*(1.-XK1)/W1)           REMT1218
     .              /(1.+(1.-XK1)**2))                                  REMT1219
      IF(W1.GT.1.) GOTO 201                                             REMT1220
      W1 = -1. + 2.*W1                                                  REMT1221
      CG1=SIGN(1.-V1,W1)                                                REMT1222
      sb2 = sb1*(1.-xk1)                                                REMT1223
  202 V2 = XPS(sb2) * ( XPT(sb2)**RNDM(2.5) - 1. )                      REMT1224
      W2 = XPS(sb2) + V2*(1.-.5*V2)                                     REMT1225
      W2 = RNDM(3.5)/(1.-(XK2*XK2*W2+2.*XPS(Sb2)*(1.-XK2)/W2)           REMT1226
     .              /(1.+(1.-XK2)**2))                                  REMT1227
      IF(W2.GT.1.) GOTO 202                                             REMT1228
      W2 = -1. + 2.*W2                                                  REMT1229
      CG2=SIGN(1.-V2,W2)                                                REMT1230
C                                                                       REMT1231
C CHOOSE WHICH OF THE TWO Z AXES SHOULD BE CONSIDERED                   REMT1232
                                                                        REMT1233
      CH1=-1.                                                           REMT1234
      CH2=-1.                                                           REMT1235
      IF(ABS(W1).LT.(1./(1.+(1.-2./(1.+XK1*CG1/(2.-XK1)))**2)))         REMT1236
     .CH1=+1.                                                           REMT1237
      IF(ABS(W2).LT.(1./(1.+(1.-2./(1.+XK2*CG2/(2.-XK2)))**2)))         REMT1238
     .CH2=+1.                                                           REMT1239
C                                                                       REMT1240
C CONSTRUCT PHOTON FOUR-MOMENTA                                         REMT1241
      SG1=SQRT(V1*(2.-V1))                                              REMT1242
      QK1(4)=XK1*SQRT(sb1)/2.                                           REMT1243
      QK1(1)=QK1(4)*SG1*COS(FG1)                                        REMT1244
      QK1(2)=QK1(4)*SG1*SIN(FG1)                                        REMT1245
      QK1(3)=QK1(4)*CG1                                                 REMT1246
C                                                                       REMT1247
      SG2=SQRT(V2*(2.-V2))                                              REMT1248
      Qk2(4)=XK2*SQRT(sb2)/2.                                           REMT1249
      Qk2(1)=Qk2(4)*SG2*COS(FG2)                                        REMT1250
      Qk2(2)=Qk2(4)*SG2*SIN(FG2)                                        REMT1251
      Qk2(3)=Qk2(4)*CG2                                                 REMT1252
C - Correction for UNIX !!!                                             REMT1253
      CALL ucopy(qk1(1),qka(1),4)                                       REMT1254
      CALL ucopy(qk2(1),qkb(1),4)                                       REMT1255
C                                                                       REMT1256
      RETURN                                                            REMT1257
C                                                                       REMT1258
      ENTRY REMT3(QIN,QOUT,IPHOT)                                       REMT1259
C-----------------------------------------------------------------------REMT1260
C THIS PART PERFORMS THE ROTATIONS AND BOOSTS OF THE I.S.R.             REMT1261
C FORMALISM AFTER THE USER'S BLACK BOX HAS RUN AN EVENT.                REMT1262
C THE INPUT VECTOR (FROM USERS BLACK BOX) IS QIN;                       REMT1263
C THE RESULTING VECTOR IN THE LAB FRAME IS QOUT.                        REMT1264
C-----------------------------------------------------------------------REMT1265
C                                                                       REMT1266
C INITIALIZATION PART: ONCE FOR EVERY GENERATED PHOTON MOMENTUM         REMT1267
C     IF(IR.NE.0) GOTO 301                                              REMT1268
C     IR=1                                                              REMT1269
      IF ( iphot .EQ. 1 ) THEN                                          REMT1270
        CALL ucopy(qka(1),qk(1),4)                                      REMT1271
        ebea = SQRT(sb1)/2.                                             REMT1272
        ch = ch1                                                        REMT1273
      ELSE                                                              REMT1274
        CALL ucopy(qkb(1),qk(1),4)                                      REMT1275
        ebea = SQRT(sb2)/2.                                             REMT1276
        ch = ch2                                                        REMT1277
      ENDIF                                                             REMT1278
C                                                                       REMT1279
C CALCULATE ROTATTION PARAMETERS FOR BEAM DIRECTION IN C.M.S.           REMT1280
      YK=QK(4)**2-QK(1)**2-QK(2)**2-QK(3)**2                            REMT1281
      XKP = SQRT( QK(1)**2 + QK(2)**2 )                                 REMT1282
      XKM = 2.* SQRT( EBEA*(EBEA-QK(4)) + YK/4. )                       REMT1283
      XKD = 2.*EBEA - QK(4) + XKM                                       REMT1284
      XKA = ( CH + QK(3)/XKD )/XKM                                      REMT1285
      XKB = SQRT( (1.+XKA*QK(3))**2 + (XKA*XKP)**2 )                    REMT1286
      S1  = XKA*XKP/XKB                                                 REMT1287
      C1  = (1.+XKA*QK(3))/XKB                                          REMT1288
      S2  = QK(1)/XKP                                                   REMT1289
      C2  = QK(2)/XKP                                                   REMT1290
      Y1=C1**2+S1**2-1.                                                 REMT1291
      Y2=C2**2+S2**2-1.                                                 REMT1292
C                                                                       REMT1293
C ROTATE INPUT VECTOR QIN(I) TO CORRESPOND WITH CHOZEN Z-AXIS           REMT1294
  301 QQ =  C1*QIN(2) + S1*QIN(3)                                       REMT1295
      QZ = -S1*QIN(2) + C1*QIN(3)                                       REMT1296
      QX =  C2*QIN(1) + S2*QQ                                           REMT1297
      QY = -S2*QIN(1) + C2*QQ                                           REMT1298
C                                                                       REMT1299
C BOOST ROTATED VECTOR TO LAB FRAME VECTOR QOUT                         REMT1300
      QOUT(4)=((XKD-XKM)*QIN(4)-QK(1)*QX-QK(2)*QY-QK(3)*QZ)/XKM         REMT1301
      QQ     =(QIN(4)+QOUT(4))/XKD                                      REMT1302
      QOUT(1)= QX - QK(1)*QQ                                            REMT1303
      QOUT(2)= QY - QK(2)*QQ                                            REMT1304
      QOUT(3)= QZ - QK(3)*QQ                                            REMT1305
C                                                                       REMT1306
      RETURN                                                            REMT1307
      END                                                               REMT1308
      SUBROUTINE rghm(mchi,ma,tanb,mq,mur,md,mtop,au,ad,mu,             RGHM   2
     *    mhp,hmp,mhch,sa,ca,tanbA)                                     RGHM   3
C---------------------------------------------------------------------  RGHM   4
C!  Computes the Higgs running masses and mixing angles                 RGHM   5
C                                                                       RGHM   6
C   From M. Carena and C. Wagner, 21 Sep 1995                           RGHM   7
C---------------------------------------------------------------------  RGHM   8
      IMPLICIT REAL*8(A-H,L,M,O-Z)                                      RGHM   9
      REAL*8 cma,ctb,cmq,cmur,cmdr,cmtop,cau,cad,cmu,cmchi              CCAREN 2
      REAL*8 cmh,cmhp,chm,chmp,camp,cmhch,csa,cca                       CCAREN 3
      REAL*8 cstop1,cstop2,csbot1,csbot2,ctanbA                         CCAREN 4
      REAL*8 rmtop,rmbot,mz,alpha_1,alpha_2,alpha_3,v,ppi,sint,stw      CCAREN 5
      COMMON / mcarena / rmtop,rmbot,mz,alpha_1,alpha_2,alpha_3,        CCAREN 6
     .                   v,ppi,sint,stw                                 CCAREN 7
C                                                                       CCAREN 8
      DIMENSION VH(2,2),M2(2,2),M2P(2,2)                                RGHM  11
C                                                                       RGHM  12
Cpaj  mz = 91.18                                                        RGHM  13
Cpaj  alpha1 = 0.0101                                                   RGHM  14
Cpaj  alpha2 = 0.0337                                                   RGHM  15
Cpaj  alpha3Z = 0.12                                                    RGHM  16
Cpaj  v = 174.1                                                         RGHM  17
Cpaj  pi = 3.14159                                                      RGHM  18
      alpha1 = alpha_1     ! paj                                        RGHM  19
      alpha2 = alpha_2     ! paj                                        RGHM  20
      alpha3 = alpha_3     ! paj                                        RGHM  21
      pi = ppi             ! paj                                        RGHM  22
      tanbA = tanb                                                      RGHM  23
      tanbt = tanb                                                      RGHM  24
C                                                                       RGHM  25
C     mbottom(mtop) = 3. GeV                                            RGHM  26
Cpaj  mb = 3.                                                           RGHM  27
      mb = rmbot           ! paj                                        RGHM  28
Cpaj  alpha3 = alpha3Z/(1. +(11. - 10./3.)/4./pi*alpha3Z*               RGHM  29
Cpaj *log(mtop**2/mz**2))                                               RGHM  30
C                                                                       RGHM  31
C     rmtop= running top quark mass                                     RGHM  32
Cpaj  rmtop = mtop/(1.+4.*alpha3/3./pi)                                 RGHM  33
      tq = log((mq**2+mtop**2)/mtop**2)                                 RGHM  34
      tu = log((mur**2 + mtop**2)/mtop**2)                              RGHM  35
      td = log((md**2 + mtop**2)/mtop**2)                               RGHM  36
      sinb = tanb/((1. + tanb**2)**.5)                                  RGHM  37
      cosb = sinb/tanb                                                  RGHM  38
      if(ma.gt.mtop)                                                    RGHM  39
     *tanbA = tanb*(1.-3./32./pi**2*                                    RGHM  40
     *(rmtop**2/v**2/sinb**2-mb**2/v**2/cosb**2)*                       RGHM  41
     *log(ma**2/mtop**2))                                               RGHM  42
      if(ma.lt.mtop.or.ma.eq.mtop) tanbt = tanbA                        RGHM  43
      sinb = tanbt/((1. + tanbt**2)**.5)                                RGHM  44
      cosb = 1./((1. + tanbt**2)**.5)                                   RGHM  45
      cos2b = (tanbt**2 - 1.)/(tanbt**2 + 1.)                           RGHM  46
      g1 = (alpha1*4.*pi)**.5                                           RGHM  47
      g2 = (alpha2*4.*pi)**.5                                           RGHM  48
      g3 = (alpha3*4.*pi)**.5                                           RGHM  49
      hu = rmtop/v/sinb                                                 RGHM  50
      hd =  mb/v/cosb                                                   RGHM  51
C                                                                       RGHM  52
      CALL Gfuncar(ma,tanbA,mq,mur,md,mtop,Au,Ad,mu,vh,stop1,stop2)     RGHM  53
C                                                                       RGHM  54
      if(mq.gt.mur) tp = tq - tu                                        RGHM  55
      if(mq.lt.mur.or.mq.eq.mur) tp = tu - tq                           RGHM  56
      if(mq.gt.mur) tdp = tu                                            RGHM  57
      if(mq.lt.mur.or.mq.eq.mur) tdp = tq                               RGHM  58
      if(mq.gt.md) tpd = tq - td                                        RGHM  59
      if(mq.lt.md.or.mq.eq.md) tpd = td - tq                            RGHM  60
      if(mq.gt.md) tdpd = td                                            RGHM  61
      if(mq.lt.md.or.mq.eq.md) tdpd = tq                                RGHM  62
                                                                        RGHM  63
      if(mq.gt.md) dlambda1 = 6./96./pi**2*g1**2*hd**2*tpd              RGHM  64
      if(mq.lt.md.or.mq.eq.md) dlambda1 = 3./32./pi**2*                 RGHM  65
     * hd**2*(g1**2/3.+g2**2)*tpd                                       RGHM  66
                                                                        RGHM  67
      if(mq.gt.mur) dlambda2 =12./96./pi**2*g1**2*hu**2*tp              RGHM  68
      if(mq.lt.mur.or.mq.eq.mur) dlambda2 = 3./32./pi**2*               RGHM  69
     * hu**2*(-g1**2/3.+g2**2)*tp                                       RGHM  70
                                                                        RGHM  71
      dlambda3 = 0.                                                     RGHM  72
      dlambda4 = 0.                                                     RGHM  73
                                                                        RGHM  74
      if(mq.gt.md) dlambda3 = -1./32./pi**2*g1**2*hd**2*tpd             RGHM  75
      if(mq.lt.md.or.mq.eq.md) dlambda3 = 3./64./pi**2*hd**2*           RGHM  76
     *(g2**2-g1**2/3.)*tpd                                              RGHM  77
                                                                        RGHM  78
      if(mq.gt.mur) dlambda3 = dlambda3 -                               RGHM  79
     *1./16./pi**2*g1**2*hu**2*tp                                       RGHM  80
      if(mq.lt.mur.or.mq.eq.mur) dlambda3 = dlambda3 +                  RGHM  81
     * 3./64./pi**2*hu**2*(g2**2+g1**2/3.)*tp                           RGHM  82
                                                                        RGHM  83
      if(mq.lt.mur) dlambda4 = -3./32./pi**2*g2**2*hu**2*tp             RGHM  84
      if(mq.lt.md) dlambda4 = dlambda4 - 3./32./pi**2*g2**2*            RGHM  85
     *hd**2*tpd                                                         RGHM  86
C                                                                       RGHM  87
      lambda1 = ((g1**2 + g2**2)/4.)*                                   RGHM  88
     * (1.-3.*hd**2*(tpd + tdpd)/8./pi**2)                              RGHM  89
     *+(3.*hd**4./16./pi**2) *tpd*(1.                                   RGHM  90
     *+ (3.*hd**2/2. + hu**2/2.                                         RGHM  91
     *- 8.*g3**2) * (tpd + 2.*tdpd)/16./pi**2)                          RGHM  92
     *+(3.*hd**4./8./pi**2) *tdpd*(1.  + (3.*hd**2/2. + hu**2/2.        RGHM  93
     *- 8.*g3**2) * tdpd/16./pi**2) + dlambda1                          RGHM  94
      lambda2 = ((g1**2 + g2**2)/4.)*(1.-3.*hu**2*                      RGHM  95
     *(tp + tdp)/8./pi**2)                                              RGHM  96
     *+(3.*hu**4./16./pi**2) *tp*(1.                                    RGHM  97
     *+ (3.*hu**2/2. + hd**2/2.                                         RGHM  98
     *- 8.*g3**2) * (tp + 2.*tdp)/16./pi**2)                            RGHM  99
     *+(3.*hu**4./8./pi**2) *tdp*(1. + (3.*hu**2/2. + hd**2/2.          RGHM 100
     *- 8.*g3**2) * tdp/16./pi**2) + dlambda2                           RGHM 101
      lambda3 = ((g2**2 - g1**2)/4.)*(1.-3.*                            RGHM 102
     *(hu**2)*(tp + tdp)/16./pi**2 -3.*                                 RGHM 103
     *(hd**2)*(tpd + tdpd)/16./pi**2) +dlambda3                         RGHM 104
      lambda4 = (- g2**2/2.)*(1.                                        RGHM 105
     *-3.*(hu**2)*(tp + tdp)/16./pi**2                                  RGHM 106
     *-3.*(hd**2)*(tpd + tdpd)/16./pi**2) +dlambda4                     RGHM 107
      lambda5 = 0.                                                      RGHM 108
      lambda6 = 0.                                                      RGHM 109
      lambda7 = 0.                                                      RGHM 110
                                                                        RGHM 111
      m2(1,1) = 2.*v**2*(lambda1*cosb**2+2.*lambda6*                    RGHM 112
     *cosb*sinb + lambda5*sinb**2) + ma**2*sinb**2                      RGHM 113
                                                                        RGHM 114
      m2(2,2) = 2.*v**2*(lambda5*cosb**2+2.*lambda7*                    RGHM 115
     *cosb*sinb + lambda2*sinb**2) + ma**2*cosb**2                      RGHM 116
      m2(1,2) = 2.*v**2*(lambda6*cosb**2+(lambda3+lambda4)*             RGHM 117
     *cosb*sinb + lambda7*sinb**2) - ma**2*sinb*cosb                    RGHM 118
                                                                        RGHM 119
      m2(2,1) = m2(1,2)                                                 RGHM 120
ccccccccccccccccccccccccccccccccccccccccccccccccc                       RGHM 121
ccc  this is the contribution from light charginos/neutralinos          RGHM 122
ccccccccccccccccccccccccccccccccccccccccccccccccc                       RGHM 123
                                                                        RGHM 124
        mssusy=(.5*(mq**2+mur**2)+mtop**2)**.5                          RGHM 125
                                                                        RGHM 126
        if(mchi.gt.mssusy)goto 3790                                     RGHM 127
        if(mchi.lt.mtop) mchi=mtop                                      RGHM 128
                                                                        RGHM 129
        tchar=log(mssusy**2/mchi**2)                                    RGHM 130
                                                                        RGHM 131
        deltal12=(9./64./pi**2*g2**4+5./192./pi**2*g1**4)*tchar         RGHM 132
        deltal3p4=(3./64./pi**2*g2**4+7./192./pi**2*g1**4               RGHM 133
     *       +4./32/pi**2*g1**2*g2**2)*tchar                            RGHM 134
                                                                        RGHM 135
        deltam112=2.*deltal12*v**2*cosb**2                              RGHM 136
        deltam222=2.*deltal12*v**2*sinb**2                              RGHM 137
        deltam122=2.*deltal3p4*v**2*sinb*cosb                           RGHM 138
                                                                        RGHM 139
        m2(1,1)=m2(1,1)+deltam112                                       RGHM 140
        m2(2,2)=m2(2,2)+deltam222                                       RGHM 141
        m2(1,2)=m2(1,2)+deltam122                                       RGHM 142
        m2(2,1)=m2(2,1)+deltam122                                       RGHM 143
                                                                        RGHM 144
 3790   continue                                                        RGHM 145
                                                                        RGHM 146
ccccccccccccccccccccccccccccccccccccccccccc                             RGHM 147
ccc  end of charginos/neutralinos                                       RGHM 148
ccccccccccccccccccccccccccccccccccccccccccc                             RGHM 149
                                                                        RGHM 150
      do 9800 i = 1,2                                                   RGHM 151
      do 9801 j = 1,2                                                   RGHM 152
      m2p(i,j) = m2(i,j) + vh(i,j)                                      RGHM 153
 9801 continue                                                          RGHM 154
 9800 continue                                                          RGHM 155
                                                                        RGHM 156
      Trm2p = m2p(1,1) + m2p(2,2)                                       RGHM 157
      detm2p = m2p(1,1)*m2p(2,2) - m2p(1,2)*m2p(2,1)                    RGHM 158
                                                                        RGHM 159
      mh2p = (Trm2p - (Trm2p**2 - 4.* detm2p)**.5)/2.                   RGHM 160
      HM2p = (Trm2p + (Trm2p**2 - 4.* detm2p)**.5)/2.                   RGHM 161
      HMp = Hm2p**.5                                                    RGHM 162
      IF ( mh2p .LT. 0. ) THEN                                          RGHM 163
        mh   = -1.                                                      RGHM 164
        HM   = -1.                                                      RGHM 165
        mhch = -1.                                                      RGHM 166
        RETURN                                                          RGHM 167
      ENDIF                                                             RGHM 168
      mhp = mh2p**.5                                                    RGHM 169
      sin2alpha = 2.*m2p(1,2)/(Trm2p**2-4.*detm2p)**.5                  RGHM 170
      cos2alpha = (m2p(1,1)-m2p(2,2))/(Trm2p**2-4.*detm2p)**.5          RGHM 171
      if(cos2alpha.gt.0.) alpha = asin(sin2alpha)/2.                    RGHM 172
      if(cos2alpha.lt.0.) alpha = -pi/2.-asin(sin2alpha)/2.             RGHM 173
      sa = sin(alpha)                                                   RGHM 174
      ca = cos(alpha)                                                   RGHM 175
      sqbma = (sinb*ca - cosb*sa)**2                                    RGHM 176
C                                                                       RGHM 177
C Bretelle for the charged Higgses                                      RGHM 178
C                                                                       RGHM 179
      MS = ((mq**2 + mur**2)/2. + mtop**2)**.5                          RGHM 180
      t = log(MS**2/mtop**2)                                            RGHM 181
      aud = (-6.*mu**2/MS**2 - ( mu**2- Ad*Au)**2/MS**4                 RGHM 182
     *+ 3.*(Au + Ad)**2/MS**2)/6.                                       RGHM 183
      carlos4 = (- g2**2/2.)*(1.-3.*(hu**2 + hd**2)*t/16./pi**2)        RGHM 184
     *-(6.*hu**2*hd**2/16./pi**2) * (t + aud/2. + (hu**2 + hd**2        RGHM 185
     *- 8.*g3**2) * (aud*t + t**2)/16./pi**2)                           RGHM 186
     *+(3.*hu**4/96./pi**2) * (3.*mu**2/MS**2 - mu**2*Au**2/            RGHM 187
     *MS**4)*                                                           RGHM 188
     *(1+ (6.*hu**2 -2.* hd**2/2.                                       RGHM 189
     *-  16.*g3**2) *t/16./pi**2)                                       RGHM 190
     *+(3.*hd**4/96./pi**2) * (3.*mu**2/MS**2 - mu**2*Ad**2/            RGHM 191
     *MS**4)*                                                           RGHM 192
     *(1+ (6.*hd**2 -2.* hu**2/2.                                       RGHM 193
     *-  16.*g3**2) *t/16./pi**2)                                       RGHM 194
      carlos5 = -(3.*hu**4* mu**2*Au**2/96./pi**2/MS**4) *              RGHM 195
     * (1- (2.*hd**2 -6.* hu**2 + 16.*g3**2) *t/16./pi**2)              RGHM 196
     *-(3.*hd**4* mu**2*Ad**2/96./pi**2/MS**4) *                        RGHM 197
     * (1- (2.*hu**2 -6.* hd**2 + 16.*g3**2) *t/16./pi**2)              RGHM 198
      mhch2 = mA**2 + (carlos5 - carlos4)* v**2                         RGHM 199
      IF ( mhch2 .GE. 0. ) THEN                                         RGHM 200
        mhch = SQRT(mhch2)                                              RGHM 201
      ELSE                                                              RGHM 202
        mhch = -SQRT(-mhch2)                                            RGHM 203
      ENDIF                                                             RGHM 204
C                                                                       RGHM 205
 2242 RETURN                                                            RGHM 206
      END                                                               RGHM 207
      FUNCTION runalf(xmu,fn)                                           RUNALF 2
C-------------------------------------------------------------------    RUNALF 3
C! Running strong coupling constant                                     RUNALF 4
C                                                                       RUNALF 5
C  Inputs:       --xmu,   Scale mass                                    RUNALF 6
C                --fn,    flavour number                                RUNALF 7
C                                                                       RUNALF 8
C  Ouptuts:      --runalf,  The value of the alphas(xmu)/pi             RUNALF 9
C                                                                       RUNALF10
C  P. Janot  --  22 nov 1989.                                           RUNALF11
C------------------------------------------------------------------     RUNALF12
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      COMMON / alcoef / beta1,beta2,beta3,gama1,gama2,gama3             RUNALF14
C                                                                       RUNALF15
C Lambda_QCD at nf # 5 (from lambda_QCD(5), given in data cards)        RUNALF16
C                                                                       RUNALF17
      xlamda4 = xlamda5 * (amb/xlamda5)**(2./25.)                       RUNALF18
     .        * (ALOG(amb**2/xlamda5**2))**(963./14375.)                RUNALF19
      xlamda3 = xlamda4 * (amc/xlamda4)**(2./27.)                       RUNALF20
     .        * (ALOG(amc**2/xlamda4**2))**(107./2025.)                 RUNALF21
      xlamda6 = xlamda5 * (xlamda5/amt)**(2./21.)                       RUNALF22
     .        * (ALOG(amt**2/xlamda5**2))**(-321./3381.)                RUNALF23
C                                                                       RUNALF24
C Lambda_QCD at fn                                                      RUNALF25
C                                                                       RUNALF26
      nf = fn                                                           RUNALF27
      IF ( nf .LT. 3 ) xlamda = xlamda3                                 RUNALF28
      IF ( nf .EQ. 3 ) xlamda = (fn-3.)*xlamda4+(4.-fn)*xlamda3         RUNALF29
      IF ( nf .EQ. 4 ) xlamda = (fn-4.)*xlamda5+(5.-fn)*xlamda4         RUNALF30
      IF ( nf .EQ. 5 ) xlamda = (fn-5.)*xlamda6+(6.-fn)*xlamda5         RUNALF31
      IF ( nf .GE. 6 ) xlamda =        xlamda6                          RUNALF32
C                                                                       RUNALF33
C  Coefficients                                                         RUNALF34
C                                                                       RUNALF35
      beta1 = -11./2. + fn/3.                                           RUNALF36
      beta2 = -51./4. + 19./12.*fn                                      RUNALF37
      beta3 = (-2857. + 5033.*fn/9. - 325.*fn**2/27.) / 64.             RUNALF38
      gama1 = 2.                                                        RUNALF39
      gama2 = 101./12. -5./18.*fn                                       RUNALF40
      gama3 = (3747. - (160.*1.2020569+2216./9.)*fn -140.*fn**2/27.)    RUNALF41
     .            / 96.                                                 RUNALF42
C                                                                       RUNALF43
C  alpha_s / pi                                                         RUNALF44
C                                                                       RUNALF45
      xmu2 = xmu                                                        RUNALF46
      IF ( xmu2 .LT. .5 ) xmu2 = .5                                     RUNALF47
      as = 1./(-beta1*ALOG(xmu2/xlamda))                                RUNALF48
      aal = ALOG(2.*ALOG(xmu2/xlamda))                                  RUNALF49
      runalf = as * (1. - as*beta2/beta1*aal                            RUNALF50
     .       + as**2 * ( (beta2/beta1)**2 * ( aal**2 -aal - 1. )        RUNALF51
     .       + beta3/beta1))                                            RUNALF52
C                                                                       RUNALF53
      RETURN                                                            RUNALF54
      END                                                               RUNALF55
      FUNCTION RUNMAS(xmq,jhig,facrad)                                  RUNMAS 2
C-------------------------------------------------------------------    RUNMAS 3
C! Compute the running mass term in H --> qqbar(g) width                RUNMAS 4
C                                                                       RUNMAS 5
C  Inputs:       --xmq,   quark mass                                    RUNMAS 6
C                --jhig,  Higgs type                                    RUNMAS 7
C                                                                       RUNMAS 8
C  Ouptuts:      --runmas,  The value of the running mass               RUNMAS 9
C                --facrad,  The finite QCD corrections                  RUNMAS10
C                                                                       RUNMAS11
C  P. Janot  --  22 nov 1989.                                           RUNMAS12
C------------------------------------------------------------------     RUNMAS13
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),               CONSTS 2
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw      CONSTS 3
C                                                                       CONSTS 4
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)        DATAPI 2
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          DATAPI 3
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         DATAPI 4
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         DATAPI 5
C                                                                       DATAPI 6
      COMMON / alcoef / beta1,beta2,beta3,gama1,gama2,gama3             RUNMAS17
C                                                                       RUNMAS18
      xmh    = amhig (jhig)                                             RUNMAS19
C                                                                       RUNMAS20
C  Flavour number (assume mH >> mu, md, ms)                             RUNMAS21
C                                                                       RUNMAS22
      fnh = 3.                                                          RUNMAS23
      IF ( amc .LE. xmh/2. ) fnh = fnh + 1.                             RUNMAS24
      IF ( amb .LE. xmh/2. ) fnh = fnh + 1.                             RUNMAS25
      IF ( amt .LE. xmh/2. ) fnh = fnh + 1.                             RUNMAS26
C                                                                       RUNMAS27
      alphah = runalf(xmh,fnh)                                          RUNMAS28
C                                                                       RUNMAS29
C Compute mq(mH)                                                        RUNMAS30
C                                                                       RUNMAS31
      cmulth =  beta2/beta1*(gama1/beta1-gama2/beta2)                   RUNMAS32
      cmulth2 = .5*( (beta2/beta1)**2*(gama1/beta1-gama2/beta2)**2      RUNMAS33
     .              -(beta2/beta1)**2*(gama1/beta1-gama2/beta2)         RUNMAS34
     .              +(beta3/beta1)   *(gama1/beta1-gama3/beta3) )       RUNMAS35
      xmhp = (-beta1*alphah)**(-gama1/beta1)                            RUNMAS36
     .     * (1. + cmulth*alphah + cmulth2*alphah**2 )                  RUNMAS37
C                                                                       RUNMAS38
C Compute mq(Mq)                                                        RUNMAS39
C                                                                       RUNMAS40
      IF     ( xmq .EQ. ams ) THEN                                      RUNMAS41
        fnq = 3.                                                        RUNMAS42
        factork = 16.11                                                 RUNMAS43
     .          - 1.04*(2.-amu/ams-amd/ams)                             RUNMAS44
      ELSEIF ( xmq .EQ. amc ) THEN                                      RUNMAS45
        fnq = 4.                                                        RUNMAS46
        factork = 16.11                                                 RUNMAS47
     .          - 1.04*(3.-amu/amc-amd/amc-ams/amc)                     RUNMAS48
      ELSEIF ( xmq .EQ. amb ) THEN                                      RUNMAS49
        fnq = 5.                                                        RUNMAS50
        factork = 16.11                                                 RUNMAS51
     .          - 1.04*(4.-amu/amb-amd/amb-ams/amb-amc/amb)             RUNMAS52
      ELSEIF ( xmq .EQ. amt ) THEN                                      RUNMAS53
        fnq = 6.                                                        RUNMAS54
        factork = 16.11                                                 RUNMAS55
     .          - 1.04*(4.-amu/amt-amd/amt-ams/amt-amc/amt-amb/amt)     RUNMAS56
      ELSE                                                              RUNMAS57
      ENDIF                                                             RUNMAS58
      alphaq = runalf(xmq,fnq)                                          RUNMAS59
C     alphaq = runalf(2.*xmq,fnq)                                       RUNMAS60
      cmultq =  beta2/beta1*(gama1/beta1-gama2/beta2)                   RUNMAS61
      cmultq2 = .5*( (beta2/beta1)**2*(gama1/beta1-gama2/beta2)**2      RUNMAS62
     .              -(beta2/beta1)**2*(gama1/beta1-gama2/beta2)         RUNMAS63
     .              +(beta3/beta1)   *(gama1/beta1-gama3/beta3) )       RUNMAS64
      xmqp = (-beta1*alphaq)**(-gama1/beta1)                            RUNMAS65
     .     * (1. + cmultq*alphaq + cmultq2*alphaq**2 )                  RUNMAS66
     .     * (1. + 4./3. *alphaq + factork*alphaq**2 )                  RUNMAS67
C                                                                       RUNMAS68
      runmas = (xmhp/xmqp)**2                                           RUNMAS69
C                                                                       RUNMAS70
C Finite radiative corrections due to gluon radiations                  RUNMAS71
C                                                                       RUNMAS72
      runrun = runmas                                                   RUNMAS73
      CALL radcor(xmq*SQRT(runrun),jhig,fnq,r1,r2)                      RUNMAS74
      facrad = 1. + r1 * alphah + r2 * alphah**2                        RUNMAS75
C     facqed = 1. + r1 * alpha(jhig) + r2 * alpha(jhig)**2              RUNMAS76
C                                                                       RUNMAS77
C  Here we go                                                           RUNMAS78
C                                                                       RUNMAS79
 999  RETURN                                                            RUNMAS80
C-------------------------------------------------------------------    RUNMAS81
      END                                                               RUNMAS82
      SUBROUTINE SETDEF                                                 SETDEF 2
C-------------------------------------------------------------          SETDEF 3
C!  Set default conditions to HHDECAY                                   SETDEF 4
C                                                                       SETDEF 5
C   Input:   / LUNCOM /  --PMAS, mass array from LUND                   SETDEF 6
C                                                                       SETDEF 7
C   Output:  / H0DECK /  --ichan = 1 for requested channels             SETDEF 8
C                        --channel = channel names                      SETDEF 9
C                        --idbg        debug level                      SETDEF10
C                                                                       SETDEF11
C   P. Janot  --  26 Aug 1991                                           SETDEF12
C-------------------------------------------------------------          SETDEF13
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      CHARACTER*14 channel                                              HHDECK 2
      CHARACTER*21 channeut, chanchar                                   HHDECK 3
      PARAMETER(nchneut=8,nchchar=5)                                    HHDECK 4
      COMMON / hhdeck / branch(nchan,nhig),width(nhig),                 HHDECK 5
     .                  parwth(nhig),xymas(2,nchan,nhig),               HHDECK 6
     .                  xywid(2,nchan,nhig)                             HHDECK 7
      COMMON / chaneu / ichn(2),                                        HHDECK 8
     .                  wneut(4,4,nhig), wchar(2,2,nhig),               HHDECK 9
     .                  widneut(4), brneut(nchneut,4),                  HHDECK10
     .                  widchar(2), brchar(nchchar,2)                   HHDECK11
      COMMON / chanch / channeut(nchneut,4), chanchar(nchchar,2)        HHDECK12
      COMMON / vect4 / pvect4(5,2)                                      HHDECK13
      COMMON / hinput / ichan(nchan,nhig), channel(nchan,nhig)          HHDECK14
      COMMON / hisbr / bchpp,bchgg                                      HHDECK15
      DIMENSION ph(4)                                                   HHDECK16
C                                                                       HHDECK17
C                                                                       SETDEF16
      idbg = 0                                                          SETDEF17
C                                                                       SETDEF18
      DO jhig  = 1, nhig                                                SETDEF19
        DO jchan = 1, nchan                                             SETDEF20
          ichan(jchan,jhig) = 1                                         SETDEF21
        ENDDO                                                           SETDEF22
      ENDDO                                                             SETDEF23
C                                                                       SETDEF24
      channel( 1,1) = 'H --> gam gam '                                  SETDEF25
      channel( 2,1) = 'H --> glu glu '                                  SETDEF26
      channel( 3,1) = 'H --> tau+tau-'                                  SETDEF27
      channel( 4,1) = 'H --> c c bar '                                  SETDEF28
      channel( 5,1) = 'H --> b b bar '                                  SETDEF29
      channel( 6,1) = 'H --> t t bar '                                  SETDEF30
      channel( 7,1) = 'H --> W+ W-   '                                  SETDEF31
      channel( 8,1) = 'H --> Z0 Z0   '                                  SETDEF32
      channel( 9,1) = 'H --> A A     '                                  SETDEF33
      channel(10,1) = 'H --> h h     '                                  SETDEF34
      channel(11,1) = 'H --> Z0 gam  '                                  SETDEF35
      channel(12,1) = 'H --> e+ e-   '                                  SETDEF36
      channel(13,1) = 'H --> mu+ mu- '                                  SETDEF37
      channel(14,1) = 'H --> s sbar  '                                  SETDEF38
      channel(15,1) = 'H --> chi chi '                                  SETDEF39
      channel(16,1) = 'H --> chi+chi-'                                  SETDEF40
C                                                                       SETDEF41
      channel( 1,2) = 'h --> gam gam '                                  SETDEF42
      channel( 2,2) = 'h --> glu glu '                                  SETDEF43
      channel( 3,2) = 'h --> tau+tau-'                                  SETDEF44
      channel( 4,2) = 'h --> c c bar '                                  SETDEF45
      channel( 5,2) = 'h --> b b bar '                                  SETDEF46
      channel( 6,2) = 'h --> t t bar '                                  SETDEF47
      channel( 7,2) = 'h --> W+ W-   '                                  SETDEF48
      channel( 8,2) = 'h --> Z0 Z0   '                                  SETDEF49
      channel( 9,2) = 'h --> A A     '                                  SETDEF50
      channel(10,2) = '              '                                  SETDEF51
      channel(11,2) = 'h --> Z0 gam  '                                  SETDEF52
      channel(12,2) = 'h --> e+ e-   '                                  SETDEF53
      channel(13,2) = 'h --> mu+ mu- '                                  SETDEF54
      channel(14,2) = 'h --> s sbar  '                                  SETDEF55
      channel(15,2) = 'h --> chi chi '                                  SETDEF56
      channel(16,2) = 'h --> chi+chi-'                                  SETDEF57
C                                                                       SETDEF58
      channel( 1,3) = 'A --> gam gam '                                  SETDEF59
      channel( 2,3) = 'A --> glu glu '                                  SETDEF60
      channel( 3,3) = 'A --> tau+tau-'                                  SETDEF61
      channel( 4,3) = 'A --> c c bar '                                  SETDEF62
      channel( 5,3) = 'A --> b b bar '                                  SETDEF63
      channel( 6,3) = 'A --> t t bar '                                  SETDEF64
      channel( 7,3) = 'A --> W+ W-   '                                  SETDEF65
      channel( 8,3) = 'A --> Z0 Z0   '                                  SETDEF66
      channel( 9,3) = 'A --> Z0 h    '                                  SETDEF67
      channel(10,3) = '              '                                  SETDEF68
      channel(11,3) = 'A --> Z0 gam  '                                  SETDEF69
      channel(12,3) = 'A --> e+ e-   '                                  SETDEF70
      channel(13,3) = 'A --> mu+ mu- '                                  SETDEF71
      channel(14,3) = 'A --> s sbar  '                                  SETDEF72
      channel(15,3) = 'A --> chi chi '                                  SETDEF73
      channel(16,3) = 'A --> chi+chi-'                                  SETDEF74
C                                                                       SETDEF75
  999 RETURN                                                            SETDEF76
C-----------------------------------------------------------------------SETDEF77
      END                                                               SETDEF78
      SUBROUTINE shiggs                                                 SHIGGS 2
C------------------------------------------------------------------     SHIGGS 3
C!  Compute SUSY Higgs masses from the MSSM parameters                  SHIGGS 4
C                                                                       SHIGGS 5
C  Input:    /PARAM/ MSSM parameters                                    SHIGGS 6
C                                                                       SHIGGS 7
C  Output:   /PARAM/ amh, gmh, amhp, the squared Higgs masses           SHIGGS 8
C                    alfa, the mixing angle in the CP-even sector       SHIGGS 9
C                                                                       SHIGGS10
C  P. Janot -- 3 December 1994                                          SHIGGS11
C------------------------------------------------------------------     SHIGGS12
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)        DATAPI 2
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          DATAPI 3
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         DATAPI 4
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         DATAPI 5
C                                                                       DATAPI 6
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),               CONSTS 2
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw      CONSTS 3
C                                                                       CONSTS 4
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,      MISCL  2
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),            MISCL  3
     &                 nevent(11)                                       MISCL  4
      INTEGER loutbe,idb1,idb2,nevent                                   MISCL  5
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad                      MISCL  6
C                                                                       MISCL  7
      REAL*8 cma,ctb,cmq,cmur,cmdr,cmtop,cau,cad,cmu,cmchi              CCAREN 2
      REAL*8 cmh,cmhp,chm,chmp,camp,cmhch,csa,cca                       CCAREN 3
      REAL*8 cstop1,cstop2,csbot1,csbot2,ctanbA                         CCAREN 4
      REAL*8 rmtop,rmbot,mz,alpha_1,alpha_2,alpha_3,v,ppi,sint,stw      CCAREN 5
      COMMON / mcarena / rmtop,rmbot,mz,alpha_1,alpha_2,alpha_3,        CCAREN 6
     .                   v,ppi,sint,stw                                 CCAREN 7
C                                                                       CCAREN 8
C                                                                       SHIGGS18
C Compute running top/bottom masses and coupling constants              SHIGGS19
C at the top mass scale                                                 SHIGGS20
C                                                                       SHIGGS21
      alpha_1 = ulalem(amt**2) / cw2                                    SHIGGS22
      alpha_2 = alpha_1 / sw2 * cw2                                     SHIGGS23
      alpha_3 = runalf(amt,5.) * pi                                     SHIGGS24
      factork = 16.11                                                   SHIGGS25
     .        - 1.04*(4.-amu/amt-amd/amt-ams/amt-amc/amt-amb/amt)       SHIGGS26
      runamt = amt                                                      SHIGGS27
     .       / (1. + 4./3. *alpha_3/pi + factork*(alpha_3/pi)**2 )      SHIGGS28
      amhig(1) = amt                                                    SHIGGS29
      runamb = amb * SQRT(runmas(amb,1,radb))                           SHIGGS30
      amhig(1) = 0.                                                     SHIGGS31
C                                                                       SHIGGS32
      IF ( icar .LE. 0 ) THEN                                           SHIGGS33
C                                                                       SHIGGS34
C This is the EPA approximation                                         SHIGGS35
C                                                                       SHIGGS36
        ama   = amarun                                                  SHIGGS37
C                                                                       SHIGGS38
        amst1 = amst(1)**2                                              SHIGGS39
        amst2 = amst(2)**2                                              SHIGGS40
        amsb1 = amsb(1)**2                                              SHIGGS41
        amsb2 = amsb(2)**2                                              SHIGGS42
C                                                                       SHIGGS43
        crat    = susAt - susMU/tb                                      SHIGGS44
        crab    = susAb - susMU*tb                                      SHIGGS45
C                                                                       SHIGGS46
C The Delta_ij correction terms entering the mass matrix                SHIGGS47
C                                                                       SHIGGS48
        delta11 = runamb**4/cb2                                         SHIGGS49
     .          * (ALOG(amsb1*amsb2/runamb**4)                          SHIGGS50
     .          + 2.*susAb*crab   * gf1(amsb1,amsb2)                    SHIGGS51
     .          + (susAb*crab)**2 * gf2(amsb1,amsb2) )                  SHIGGS52
     .          + runamt**4/sb2                                         SHIGGS53
     .          * (susMU*crat)**2 * gf2(amst1,amst2)                    SHIGGS54
C                                                                       SHIGGS55
        delta22 = runamt**4/sb2                                         SHIGGS56
     .          * (ALOG(amst1*amst2/runamt**4)                          SHIGGS57
     .          + 2.*susAt*crat   * gf1(amst1,amst2)                    SHIGGS58
     .          + (susAt*crat)**2 * gf2(amst1,amst2) )                  SHIGGS59
     .          + runamb**4/cb2                                         SHIGGS60
     .          * (susMU*crab)**2 * gf2(amsb1,amsb2)                    SHIGGS61
C                                                                       SHIGGS62
        delta12 =-runamt**4/sb2                                         SHIGGS63
     .          * susMU*crat *    ( gf1(amst1,amst2)                    SHIGGS64
     .          + susAt*crat *      gf2(amst1,amst2) )                  SHIGGS65
     .          - runamb**4/cb2                                         SHIGGS66
     .          * susMU*crab *    (  gf1(amsb1,amsb2)                   SHIGGS67
     .          + susAb*crab *       gf2(amsb1,amsb2) )                 SHIGGS68
C                                                                       SHIGGS69
C The scale of the correction                                           SHIGGS70
C                                                                       SHIGGS71
        corr    = 3.*G_F/(SQRT(2.)*pi**2)                               SHIGGS72
C                                                                       SHIGGS73
        delta11 = corr*delta11/2.                                       SHIGGS74
        delta22 = corr*delta22/2.                                       SHIGGS75
        delta12 = corr*delta12/2.                                       SHIGGS76
C                                                                       SHIGGS77
C The eigen values of the CP-even Higgs mass matrix                     SHIGGS78
C                                                                       SHIGGS79
        a11   =  amz**2*cb2 + ama**2*sb2  + delta11                     SHIGGS80
        a22   =  amz**2*sb2 + ama**2*cb2  + delta22                     SHIGGS81
        a12   = -(amz**2 + ama**2)*s2b/2. + delta12                     SHIGGS82
        delta = SQRT( (a11-a22)**2 + 4.*a12**2 )                        SHIGGS83
        amh   = (a11 + a22 - delta)/2.                                  SHIGGS84
        gmh   = (a11 + a22 + delta)/2.                                  SHIGGS85
C                                                                       SHIGGS86
C Check the MSSM consistency                                            SHIGGS87
C                                                                       SHIGGS88
        IF ( amh .LE. 0. .OR. gmh .LE. 0. ) THEN                        SHIGGS89
          IF ( idbg .GE. 0 ) THEN                                       SHIGGS90
            WRITE(6,1000) ama,tb,susM,susMU,susAt,susAB,susSMQ,susSMU,  SHIGGS91
     .                           susSMD,susSML,susSME                   SHIGGS92
            WRITE(6,2000) amh,gmh                                       SHIGGS93
          ENDIF                                                         SHIGGS94
          RETURN                                                        SHIGGS95
C         STOP 99                                                       SHIGGS96
        ENDIF                                                           SHIGGS97
C                                                                       SHIGGS98
        IF ( amh .GE. 0 ) THEN                                          SHIGGS99
          amh   = SQRT(amh)                                             SHIGG100
        ELSE                                                            SHIGG101
          amh   = -SQRT(-amh)                                           SHIGG102
        ENDIF                                                           SHIGG103
        gmh   = SQRT(gmh)                                               SHIGG104
C                                                                       SHIGG105
C The CP-even mixing angle alpha (no ambiguities with ATAN2)            SHIGG106
C                                                                       SHIGG107
        costa = (a11-a22) / delta                                       SHIGG108
        sinta = 2.*a12    / delta                                       SHIGG109
        alfa = ATAN2(sinta,costa)/2.                                    SHIGG110
C                                                                       SHIGG111
C Radiative correction to the charged Higgs mass (coded only in         SHIGG112
C the case of a large stop mixing)                                      SHIGG113
C                                                                       SHIGG114
        deltahp = -corr * runamt**4*susMU**2/4.                         SHIGG115
     .          * gf3(amst1,amst2,susSMQ**2)                            SHIGG116
        amhp = SQRT(amw**2+ama**2+deltahp)                              SHIGG117
C                                                                       SHIGG118
      ELSE                                                              SHIGG119
C                                                                       SHIGG120
C Here is the two-loops improved renormalization group calculation.     SHIGG121
C                                                                       SHIGG122
        cma   = amarun                                                  SHIGG123
        ctb   = tb                                                      SHIGG124
        cmq   = susSMQ                                                  SHIGG125
        cmur  = susSMU                                                  SHIGG126
        cmdr  = susSMD                                                  SHIGG127
        cmtop = amt                                                     SHIGG128
        cAu   = susAt                                                   SHIGG129
        cAd   = susAb                                                   SHIGG130
        cmu   = susMU                                                   SHIGG131
        cmchi = amchar(2)                                               SHIGG132
C                                                                       SHIGG133
        rmtop = runamt                                                  SHIGG134
        rmbot = runamb                                                  SHIGG135
C                                                                       SHIGG136
        ppi = pi                                                        SHIGG137
        mz  = amz                                                       SHIGG138
        v   = 1. / SQRT(g_f * SQRT(2.)) / SQRT(2.)                      SHIGG139
        sint = sw2                                                      SHIGG140
        stw  = sw2                                                      SHIGG141
C                                                                       SHIGG142
        CALL pole(3,cmchi,cma,ctb,cmq,cmur,cmdr,cmtop,cAu,cAd,cmu,      SHIGG143
     *              cmh,cmhp,chm,chmp,camp,cmhch,csa,cca,               SHIGG144
     *              cstop1,cstop2,csbot1,csbot2,ctanbA)                 SHIGG145
C                                                                       SHIGG146
        amh  = cmhp                                                     SHIGG147
        gmh  = cHMp                                                     SHIGG148
        ama  = camp                                                     SHIGG149
C                                                                       SHIGG150
        amhp = cmhch                                                    SHIGG151
        costa = cca                                                     SHIGG152
        sinta = csa                                                     SHIGG153
        alfa = ATAN2(sinta,costa)                                       SHIGG154
C                                                                       SHIGG155
      ENDIF                                                             SHIGG156
C                                                                       SHIGG157
C Various couplings and angles.                                         SHIGG158
C                                                                       SHIGG159
      ta   = TAN(alfa)                                                  SHIGG160
      sab2 = SIN(beta-alfa)**2                                          SHIGG161
      cab2 = COS(beta-alfa)**2                                          SHIGG162
      ca = COS(alfa)                                                    SHIGG163
      sa = SIN(alfa)                                                    SHIGG164
      c2a = COS(2.*alfa)                                                SHIGG165
      cab = COS(alfa+beta)                                              SHIGG166
      sab = SIN(alfa+beta)                                              SHIGG167
C                                                                       SHIGG168
      RETURN                                                            SHIGG169
C------------------------------------------------------------------     SHIGG170
 1000 FORMAT(1x,50('-')//                                               SHIGG171
     .       1x,'With the following input parameters of the MSSM :'/    SHIGG172
     .       1x,'   . A mass      : ',F8.3,' GeV/c**2'/                 SHIGG173
     .       1x,'   . Tan beta    : ',F8.3/                             SHIGG174
     .       1x,'   . M           : ',F8.3,' GeV/c**2'/                 SHIGG175
     .       1x,'   . mu          : ',F8.3,' GeV/c**2'/                 SHIGG176
     .       1x,'   . At          : ',F8.3/                             SHIGG177
     .       1x,'   . Ab          : ',F8.3/                             SHIGG178
     .       1x,'   . mQ          : ',F8.3,' GeV/c**2'/                 SHIGG179
     .       1x,'   . mU          : ',F8.3,' GeV/c**2'/                 SHIGG180
     .       1x,'   . mD          : ',F8.3,' GeV/c**2'/                 SHIGG181
     .       1x,'   . mL          : ',F8.3,' GeV/c**2'/                 SHIGG182
     .       1x,'   . mE          : ',F8.3,' GeV/c**2'/)                SHIGG183
 2000 FORMAT(' A non physical set of Higgs masses has',                 SHIGG184
     .       ' been obtained :'/                                        SHIGG185
     .       1x,'   . mh**2       : ',E10.4,' (GeV/c**2)**2'/           SHIGG186
     .       1x,'   . mH**2       : ',E10.4,' (GeV/c**2)**2'/           SHIGG187
     .       1x,' +++++++  STOP execution here +++++++'//)              SHIGG188
      END                                                               SHIGG189
      FUNCTION sigklei(xmh,s)                                           SIGKLEI2
C--------------------------------------------------------------------   SIGKLEI3
C! Total cross section for the process e+e- ---> H Z*                   SIGKLEI4
C                                                                       SIGKLEI5
C Subroutine from R. Kleiss, slightly modified to run with              SIGKLEI6
C my COMMONs                                                            SIGKLEI7
C                                                                       SIGKLEI8
C Input : s = beam-beam invariant mass square (GeV**2);                 SIGKLEI9
C         xmh = Higgs mass                                              SIGKLE10
C                                                                       SIGKLE11
C Modifications :                                                       SIGKLE12
C   Patrick Janot                                                       SIGKLE13
C       -- 28 Jan 1992 : Implement improved Born approximation          SIGKLE14
C       -- 12 Dec 1993 : Implement eeZ Vertex correction                SIGKLE15
C--------------------------------------------------------------------   SIGKLE16
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),               CONSTS 2
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw      CONSTS 3
C                                                                       CONSTS 4
      DIMENSION brai(11), kcode(11), xmasmi(11)                         ZZDECK 2
      DATA brai/.0335,.0665,.0335,.0665,.0335,.0665,                    ZZDECK 3
     .          .1540,.1190,.1540,.1190,.1540/                          ZZDECK 4
      DATA kcode/11,12,13,14,15,16,1,2,3,4,5/                           ZZDECK 5
      DATA xmasmi/6*0.,0.3,0.3,1.0,4.0,11.0/                            ZZDECK 6
      COMMON / zzdec / braz(11), kselec(11), fracz                      ZZDECK 7
C                                                                       ZZDECK 8
C                                                                       SIGKLE20
      COMPLEX*16 xq,z1,z2,ffff                                          SIGKLE21
      REAL*8 a,x1                                                       SIGKLE22
C                                                                       SIGKLE23
C Derived constants                                                     SIGKLE24
C                                                                       SIGKLE25
      g    = 1./SQRT(sw2*(1.-sw2))                                      SIGKLE26
      ca   = -g/4.                                                      SIGKLE27
      cv   = g*(4.*sw2-1.)/4.                                           SIGKLE28
      a    = 4.*xmh**2/s                                                SIGKLE29
      b    = amz*gmz/s                                                  SIGKLE30
      xp   = 1.+(xmh**2-amz**2)/s                                       SIGKLE31
      x1   = SQRT(a)                                                    SIGKLE32
      x2   = 1.+ a/4.                                                   SIGKLE33
C The eeZ and mumuZ couplings                                           SIGKLE34
      con1 = alpha(0) * (g*(cv**2+ca**2))**2 / (36.*s)                  SIGKLE35
C The "first" Z Breit-Wigner                                            SIGKLE36
      con2 = (amz**2/s)/( (1.-amz**2/s)**2 + b**2 )                     SIGKLE37
C The full width correction                                             SIGKLE38
      con3 = alpha(0)*amz*(cv**2+ca**2)/(3.*gmz)                        SIGKLE39
C The Improved Born approximation                                       SIGKLE40
      con4 = (1.+deltar)**2                                             SIGKLE41
     .     * (1.-8.*deltar/3.)                                          SIGKLE42
C And the weak correction associated to the e+e- --> Z vertex           SIGKLE43
     .     * (1.-8.*deltar*(1.-sw2)*cv*ca/(cv**2+ca**2))                SIGKLE44
C The overall multiplicative constant                                   SIGKLE45
      cons = con1 * con2 * con4 / con3                                  SIGKLE46
C                                                                       SIGKLE47
C The integral factor using an external function                        SIGKLE48
C                                                                       SIGKLE49
      xq      = CMPLX(xp,b)                                             SIGKLE50
      z1      = xq + CDSQRT( xq*xq - a )                                SIGKLE51
      z2      = 2.*xq - z1                                              SIGKLE52
C                                                                       SIGKLE53
C The integration                                                       SIGKLE54
C                                                                       SIGKLE55
      IF ( CDABS(z1) .NE. 0. ) THEN                                     SIGKLE56
        sigklei = 1./(8.*b) * IMAG( 1./(z1-z2)                          SIGKLE57
     .          * ( FFFF(a,z1,2D0)                                      SIGKLE58
     .            - FFFF(a,z1,x1)                                       SIGKLE59
     .            - FFFF(a,z2,2D0)                                      SIGKLE60
     .            + FFFF(a,z2,x1) ))                                    SIGKLE61
      ELSE                                                              SIGKLE62
        sigklei = 0.                                                    SIGKLE63
      ENDIF                                                             SIGKLE64
C                                                                       SIGKLE65
C     sigklei = sigklei * cons / brai(3)                                SIGKLE66
      sigklei = sigklei * cons                                          SIGKLE67
C                                                                       SIGKLE68
      RETURN                                                            SIGKLE69
      END                                                               SIGKLE70
      FUNCTION sigmawwh(s,ipro)                                         SIGMAWW2
C -------------------------------------------------------------------   SIGMAWW3
C! Compute the IBA cross section of the WW/ZZ fusion into h             SIGMAWW4
C                                                                       SIGMAWW5
C Input:    s,    the effective centre-of-mass energy squared           SIGMAWW6
C           ipro, = 5 for WW --> h                                      SIGMAWW7
C                 = 6 for WW --> H                                      SIGMAWW8
C                 = 7 for ZZ --> h                                      SIGMAWW9
C                 = 8 for ZZ --> H                                      SIGMAW10
C                                                                       SIGMAW11
C Patrick Janot -- 2 Sep 1995                                           SIGMAW12
C -------------------------------------------------------------------   SIGMAW13
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
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
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),               CONSTS 2
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw      CONSTS 3
C                                                                       CONSTS 4
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)        DATAPI 2
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          DATAPI 3
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         DATAPI 4
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         DATAPI 5
C                                                                       DATAPI 6
      EXTERNAL gsub3, dgmlt3                                            SIGMAW18
      REAL*8 x3lo,x3hi,x(3),DGMLT3                                      SIGMAW19
C                                                                       SIGMAW20
      REAL*8 kappah, kappav, v, a, rmh, rgh, ss                         SIGMAW21
      COMMON / sigww / kappah, kappav, v, a, rmh, rgh, ss               SIGMAW22
C                                                                       SIGMAW23
C Case 1: WW scattering                                                 SIGMAW24
C                                                                       SIGMAW25
      IF ( ipro .EQ. 5 .OR. ipro .EQ. 6 ) THEN                          SIGMAW26
C                                                                       SIGMAW27
        rmv  = amw                                                      SIGMAW28
        v    = SQRT(2.)                                                 SIGMAW29
        a    = SQRT(2.)                                                 SIGMAW30
        gvvh = gweak2(0) * rmv**2                                       SIGMAW31
C                                                                       SIGMAW32
C Case 2: ZZ scattering                                                 SIGMAW33
C                                                                       SIGMAW34
      ELSEIF ( ipro .EQ. 7 .OR. ipro .EQ. 8 ) THEN                      SIGMAW35
C                                                                       SIGMAW36
        rmv  = amz                                                      SIGMAW37
        v    = -1.+4.*sw2                                               SIGMAW38
        a   = -1.                                                       SIGMAW39
        gvvh = gweak2(0) * rmv**2 / cw**2                               SIGMAW40
C                                                                       SIGMAW41
      ENDIF                                                             SIGMAW42
C                                                                       SIGMAW43
C The Higgs mass                                                        SIGMAW44
C                                                                       SIGMAW45
      IF     ( ipro .EQ. 5 .OR. ipro .EQ. 7 ) THEN                      SIGMAW46
C                                                                       SIGMAW47
        rmh  = pmas(25,1)                                               SIGMAW48
        rgh  = pmas(25,2)                                               SIGMAW49
C                                                                       SIGMAW50
      ELSEIF ( ipro .EQ. 6 .OR. ipro .EQ. 8 ) THEN                      SIGMAW51
C                                                                       SIGMAW52
        rmh  = pmas(35,1)                                               SIGMAW53
        rgh  = pmas(35,2)                                               SIGMAW54
C                                                                       SIGMAW55
      ENDIF                                                             SIGMAW56
C                                                                       SIGMAW57
      con1     = G_F**3*rmv**4 / (64.*SQRT(2.)*pi**3)                   SIGMAW58
     .         * alpha2 / alpha(0)**2                                   SIGMAW59
C                                                                       SIGMAW60
      con2     = alpha2 * gvvh                                          SIGMAW61
     .         / (1024. * pi * sw2**2 * amw**4 )                        SIGMAW62
      con3     = (1.+deltar)**3 * (1.-5./3.*deltar)                     SIGMAW63
C                                                                       SIGMAW64
      kappav   = rmv**2/s                                               SIGMAW65
C                                                                       SIGMAW66
      ss     = s                                                        SIGMAW67
      x3lo    = -DATAN2(rmh,rgh)                                        SIGMAW68
      x3hi    =  DATAN2(ss-rmh**2,rmh*rgh)                              SIGMAW69
      sigmawwh = con2 * con3 * DGMLT3(gsub3,x3lo,x3hi,1,6,x)            SIGMAW70
     .                       / (piby2+DATAN2(rmh,rgh))                  SIGMAW71
C                                                                       SIGMAW72
  999 RETURN                                                            SIGMAW73
      END                                                               SIGMAW74
      FUNCTION sigma1(s)                                                SIGSIG 2
C--------------------------------------------------------------------   SIGSIG 3
C! Eight functions sigma1(s) --> sigma8(s) for the cross sections       SIGSIG 4
C  of the processes 1 to 8.                                             SIGSIG 5
C                                                                       SIGSIG 6
C--------------------------------------------------------------------   SIGSIG 7
      sigma1 = crocom(1,s)                                              SIGSIG 8
      RETURN                                                            SIGSIG 9
      END                                                               SIGSIG10
C                                                                       SIGSIG11
      FUNCTION sigma2(s)                                                SIGSIG12
      sigma2 = crocom(2,s)                                              SIGSIG13
      RETURN                                                            SIGSIG14
      END                                                               SIGSIG15
C                                                                       SIGSIG16
      FUNCTION sigma3(s)                                                SIGSIG17
      sigma3 = crocom(3,s)                                              SIGSIG18
      RETURN                                                            SIGSIG19
      END                                                               SIGSIG20
C                                                                       SIGSIG21
      FUNCTION sigma4(s)                                                SIGSIG22
      sigma4 = crocom(4,s)                                              SIGSIG23
      RETURN                                                            SIGSIG24
      END                                                               SIGSIG25
C                                                                       SIGSIG26
      FUNCTION sigma5(s)                                                SIGSIG27
      sigma5 = crocom(5,s)                                              SIGSIG28
      RETURN                                                            SIGSIG29
      END                                                               SIGSIG30
C                                                                       SIGSIG31
      FUNCTION sigma6(s)                                                SIGSIG32
      sigma6 = crocom(6,s)                                              SIGSIG33
      RETURN                                                            SIGSIG34
      END                                                               SIGSIG35
C                                                                       SIGSIG36
      FUNCTION sigma7(s)                                                SIGSIG37
      sigma7 = crocom(7,s)                                              SIGSIG38
      RETURN                                                            SIGSIG39
      END                                                               SIGSIG40
C                                                                       SIGSIG41
      FUNCTION sigma8(s)                                                SIGSIG42
      sigma8 = crocom(8,s)                                              SIGSIG43
      RETURN                                                            SIGSIG44
      END                                                               SIGSIG45
C                                                                       SIGSIG46
      FUNCTION sigmat(s)                                                SIGSIG47
      sigmat = sigma1(s)+sigma2(s)+sigma3(s)+sigma4(s)+sigma5(s)        SIGSIG48
      RETURN                                                            SIGSIG49
      END                                                               SIGSIG50
      SUBROUTINE squarks                                                SQUARKS2
C------------------------------------------------------------------     SQUARKS3
C!  Compute squark masses from the MSSM parameters                      SQUARKS4
C                                                                       SQUARKS5
C  Input:    /PARAM/ MSSM parameters                                    SQUARKS6
C                                                                       SQUARKS7
C  Output:   /PARAM/ amst(2), amsb(2), the stop and sbottom masses      SQUARKS8
C                                                                       SQUARKS9
C   P. Janot -- 4 December 1994                                         SQUARK10
C------------------------------------------------------------------     SQUARK11
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)        DATAPI 2
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          DATAPI 3
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         DATAPI 4
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         DATAPI 5
C                                                                       DATAPI 6
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),               CONSTS 2
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw      CONSTS 3
C                                                                       CONSTS 4
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,      MISCL  2
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),            MISCL  3
     &                 nevent(11)                                       MISCL  4
      INTEGER loutbe,idb1,idb2,nevent                                   MISCL  5
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad                      MISCL  6
C                                                                       MISCL  7
C                                                                       SQUARK16
C Compute running top/bottom masses and coupling constants              SQUARK17
C at the top mass scale                                                 SQUARK18
C                                                                       SQUARK19
      alpha_3 = runalf(amt,5.) * pi                                     SQUARK20
      factork = 16.11                                                   SQUARK21
     .        - 1.04*(4.-amu/amt-amd/amt-ams/amt-amc/amt-amb/amt)       SQUARK22
      runamt = amt                                                      SQUARK23
     .       / (1. + 4./3. *alpha_3/pi + factork*(alpha_3/pi)**2 )      SQUARK24
      amhig(1) = amt                                                    SQUARK25
      runamb = amb * SQRT(runmas(amb,1,radb))                           SQUARK26
      amhig(1) = 0.                                                     SQUARK27
C                                                                       SQUARK28
C                                                                       SQUARK29
      c2b = COS(2.*beta)                                                SQUARK30
      s2b = SIN(2.*beta)                                                SQUARK31
      cb2 = COS(beta)**2                                                SQUARK32
      sb2 = SIN(beta)**2                                                SQUARK33
C                                                                       SQUARK34
C The Stop mass matrix                                                  SQUARK35
C                                                                       SQUARK36
      t11 = runamt**2 + susSMQ**2 + (4.*amw**2-1.*amz**2)*c2b/6.        SQUARK37
      t22 = runamt**2 + susSMU**2 - (4.*amw**2-4.*amz**2)*c2b/6.        SQUARK38
C     t12 = runamt * (susAt + susMU/tb)    ! Sign mistake (Marcela)     SQUARK39
      t12 = runamt * (susAt - susMU/tb)                                 SQUARK40
C                                                                       SQUARK41
C The Stop masses                                                       SQUARK42
C                                                                       SQUARK43
      delta   = SQRT( (t11-t22)**2 + 4.*t12**2 )                        SQUARK44
      amst(1) = (t11 + t22 - delta)/2.                                  SQUARK45
      amst(2) = (t11 + t22 + delta)/2.                                  SQUARK46
C                                                                       SQUARK47
C The mixing angle in the Stop sector                                   SQUARK48
C                                                                       SQUARK49
      cosmix = -(t11-t22)                                               SQUARK50
      sinmix = -2.*t12                                                  SQUARK51
      topmix = 0.                                                       SQUARK52
      IF ( cosmix*sinmix .NE. 0. ) topmix = ATAN2(sinmix,cosmix)/2.     SQUARK53
C                                                                       SQUARK54
C The Sbottom mass matrix                                               SQUARK55
C                                                                       SQUARK56
      b11 = runamb**2 + susSMQ**2 - (2.*amw**2+1.*amz**2)*c2b/6.        SQUARK57
      b22 = runamb**2 + susSMD**2 + (2.*amw**2-2.*amz**2)*c2b/6.        SQUARK58
C     b12 = runamb * (susAb + susMU*tb)  ! Sign mistake (Marcela)       SQUARK59
      b12 = runamb * (susAb - susMU*tb)                                 SQUARK60
C                                                                       SQUARK61
C The Sbottom masses                                                    SQUARK62
C                                                                       SQUARK63
      delta   = SQRT( (b11-b22)**2 + 4.*b12**2 )                        SQUARK64
      amsb(1) = (b11 + b22 - delta)/2.                                  SQUARK65
      amsb(2) = (b11 + b22 + delta)/2.                                  SQUARK66
C                                                                       SQUARK67
C The mixing angle in the Sbottom sector                                SQUARK68
C                                                                       SQUARK69
      cosmix = -(b11-b22)                                               SQUARK70
      sinmix = -2.*b12                                                  SQUARK71
      botmix = 0.                                                       SQUARK72
      IF ( cosmix*sinmix .NE. 0. ) botmix = ATAN2(sinmix,cosmix)/2.     SQUARK73
C                                                                       SQUARK74
C Check the MSSM consistency                                            SQUARK75
C                                                                       SQUARK76
      IF ( amst(1)*amst(2) .LE. 0. .OR.                                 SQUARK77
     .     amsb(1)*amsb(2) .LE. 0. ) THEN                               SQUARK78
        IF ( idbg .GE. 0 ) THEN                                         SQUARK79
          WRITE(6,1000) ama,tb,susM,susMU,susAt,susAB,susSMQ,susSMU,    SQUARK80
     .                         susSMD,susSML,susSME                     SQUARK81
          WRITE(6,2000) amst(1),amst(2),amsb(1),amsb(2)                 SQUARK82
        ENDIF                                                           SQUARK83
        RETURN                                                          SQUARK84
      ENDIF                                                             SQUARK85
C                                                                       SQUARK86
      amst(1) = SQRT(amst(1))                                           SQUARK87
      amst(2) = SQRT(amst(2))                                           SQUARK88
      amsb(1) = SQRT(amsb(1))                                           SQUARK89
      amsb(2) = SQRT(amsb(2))                                           SQUARK90
C                                                                       SQUARK91
      RETURN                                                            SQUARK92
C------------------------------------------------------------------     SQUARK93
 1000 FORMAT(1x,50('-')//                                               SQUARK94
     .       1x,'With the following input parameters of the MSSM :'/    SQUARK95
     .       1x,'   . A mass      : ',F8.3,' GeV/c**2'/                 SQUARK96
     .       1x,'   . Tan beta    : ',F8.3/                             SQUARK97
     .       1x,'   . M           : ',F8.3,' GeV/c**2'/                 SQUARK98
     .       1x,'   . mu          : ',F8.3,' GeV/c**2'/                 SQUARK99
     .       1x,'   . At          : ',F8.3/                             SQUAR100
     .       1x,'   . Ab          : ',F8.3/                             SQUAR101
     .       1x,'   . mQ          : ',F8.3,' GeV/c**2'/                 SQUAR102
     .       1x,'   . mU          : ',F8.3,' GeV/c**2'/                 SQUAR103
     .       1x,'   . mD          : ',F8.3,' GeV/c**2'/                 SQUAR104
     .       1x,'   . mL          : ',F8.3,' GeV/c**2'/                 SQUAR105
     .       1x,'   . mE          : ',F8.3,' GeV/c**2'/)                SQUAR106
 2000 FORMAT(' A non physical set of stop and sbottom masses has',      SQUAR107
     .       ' been obtained :'/                                        SQUAR108
     .       1x,'   . mStop1^2    : ',E10.4,' (GeV/c**2)**2'/           SQUAR109
     .       1x,'   . mStop2^2    : ',E10.4,' (GeV/c**2)**2'/           SQUAR110
     .       1x,'   . mSbottom1^2 : ',E10.4,' (GeV/c**2)**2'/           SQUAR111
     .       1x,'   . mSbottom2^2 : ',E10.4,' (GeV/c**2)**2'/           SQUAR112
     .       1x,' +++++++  STOP execution here +++++++'//)              SQUAR113
      END                                                               SQUAR114
      FUNCTION strange(x,bet)                                           STRANGE2
C-------------------------------------------------------------          STRANGE3
C! Function used for the ISR computation to alpha_QED**2                STRANGE4
C                                                                       STRANGE5
C-------------------------------------------------------------          STRANGE6
      IMPLICIT REAL*8(A-H,O-Z)                                          STRANGE7
      IF ( x .GT. 0D0 ) THEN                                            STRANGE8
        strange = x**(1.-bet)                                           STRANGE9
     .          * (4.*(2.-x)*DLOG(1./x)                                 STRANG10
     .          - (1.+3.*(1.-x)**2)/x*DLOG(1.-x)                        STRANG11
     .          -  6.+x )                                               STRANG12
      ELSEIF ( x .EQ. 0D0 ) THEN                                        STRANG13
        strange = 0.                                                    STRANG14
      ENDIF                                                             STRANG15
      RETURN                                                            STRANG16
      END                                                               STRANG17
      SUBROUTINE USRDEF                                                 USRDEF 2
C-------------------------------------------------------------          USRDEF 3
C!  The user puts here his running conditions for HZHA                  USRDEF 4
C                                                                       USRDEF 5
C   Input:  Data cards file or user program                             USRDEF 6
C                                                                       USRDEF 7
C  P. Janot  --  24 aug 1991                                            USRDEF 8
C-------------------------------------------------------------          USRDEF 9
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      CHARACTER*14 channel                                              HHDECK 2
      CHARACTER*21 channeut, chanchar                                   HHDECK 3
      PARAMETER(nchneut=8,nchchar=5)                                    HHDECK 4
      COMMON / hhdeck / branch(nchan,nhig),width(nhig),                 HHDECK 5
     .                  parwth(nhig),xymas(2,nchan,nhig),               HHDECK 6
     .                  xywid(2,nchan,nhig)                             HHDECK 7
      COMMON / chaneu / ichn(2),                                        HHDECK 8
     .                  wneut(4,4,nhig), wchar(2,2,nhig),               HHDECK 9
     .                  widneut(4), brneut(nchneut,4),                  HHDECK10
     .                  widchar(2), brchar(nchchar,2)                   HHDECK11
      COMMON / chanch / channeut(nchneut,4), chanchar(nchchar,2)        HHDECK12
      COMMON / vect4 / pvect4(5,2)                                      HHDECK13
      COMMON / hinput / ichan(nchan,nhig), channel(nchan,nhig)          HHDECK14
      COMMON / hisbr / bchpp,bchgg                                      HHDECK15
      DIMENSION ph(4)                                                   HHDECK16
C                                                                       HHDECK17
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      COMMON / BCS / IW(1000)                                           BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(1000)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      INTEGER ITAB(nchan*nhig)                                          USRDEF13
      REAL*4 TAB(nchan*nhig)                                            USRDEF14
      EQUIVALENCE (TAB(1),ITAB(1))                                      USRDEF15
C                                                                       USRDEF16
C  General steering conditions and cutoffs.                             USRDEF17
C  Look for bank 'GHHG'                                                 USRDEF18
C                                                                       USRDEF19
      KNGHHG = NAMIND('GHHG')                                           USRDEF20
      IF (KNGHHG .LE. 0) GOTO 100                                       USRDEF21
      INGHHG = IW(KNGHHG)                                               USRDEF22
      IF (INGHHG .GT. 0) THEN                                           USRDEF23
        LNGHHG = IW(INGHHG)                                             USRDEF24
C                                                                       USRDEF25
C  Fill run conditions. GHHG format is :                                USRDEF26
C   Type    Inte  Inte                                                  USRDEF27
C          ICHAN  IDBG                                                  USRDEF28
C          (0=all)                                                      USRDEF29
C          (n=channel n only)                                           USRDEF30
C                                                                       USRDEF31
        IF (LNGHHG .GT. 0) THEN                                         USRDEF32
C  --  Channel number (if only one is selected)                         USRDEF33
          KW    = 1                                                     USRDEF34
          KCHAN = IW(INGHHG+KW)                                         USRDEF35
          IF(KCHAN .GT. nCHAN .OR. KCHAN .LT. 0) THEN                   USRDEF36
            WRITE(6,1000) KW,KCHAN,nCHAN                                USRDEF37
            KCHAN = 0                                                   USRDEF38
          ENDIF                                                         USRDEF39
          IF (KCHAN .NE. 0) THEN                                        USRDEF40
            CALL VZERO(ICHAN(1,1),nCHAN*nHIG)                           USRDEF41
            ICHAN(KCHAN,1) = 1                                          USRDEF42
            ICHAN(KCHAN,2) = 1                                          USRDEF43
            ICHAN(KCHAN,3) = 1                                          USRDEF44
          ENDIF                                                         USRDEF45
C  --  Debug level                                                      USRDEF46
          IDBG   = IW(INGHHG+2)                                         USRDEF47
        ENDIF                                                           USRDEF48
      ENDIF                                                             USRDEF49
C                                                                       USRDEF50
C  Define a set of decay channels FOR H                                 USRDEF51
C  Look for bank 'GCHn' (nth word = 0 to  disable  channel n,           USRDEF52
C                                 = 1 to  enable   channel n)           USRDEF53
C                                                                       USRDEF54
  100 KNGCHA = NAMIND('GCH1')                                           USRDEF55
      IF (KNGCHA .LE. 0) GOTO 110                                       USRDEF56
      INGCHA = IW(KNGCHA)                                               USRDEF57
      IF (INGCHA .GT. 0) THEN                                           USRDEF58
        LNGCHA = IW(INGCHA)                                             USRDEF59
C                                                                       USRDEF60
C  Fill ICHAN array                                                     USRDEF61
C                                                                       USRDEF62
        IF(LNGCHA .EQ. NCHAN) THEN                                      USRDEF63
          DO 1 KCHAN=1,NCHAN                                            USRDEF64
            ICHAN(KCHAN,1) = IW(INGCHA+KCHAN)                           USRDEF65
1         CONTINUE                                                      USRDEF66
        ELSEIF(LNGCHA .NE. 0) THEN                                      USRDEF67
          WRITE(6,1003) LNGCHA,NCHAN                                    USRDEF68
        ENDIF                                                           USRDEF69
      ENDIF                                                             USRDEF70
C                                                                       USRDEF71
C  Same for h                                                           USRDEF72
C                                                                       USRDEF73
  110 KNGCHA = NAMIND('GCH2')                                           USRDEF74
      IF (KNGCHA .LE. 0) GOTO 120                                       USRDEF75
      INGCHA = IW(KNGCHA)                                               USRDEF76
      IF (INGCHA .GT. 0) THEN                                           USRDEF77
        LNGCHA = IW(INGCHA)                                             USRDEF78
C                                                                       USRDEF79
C  Fill ICHAN array                                                     USRDEF80
C                                                                       USRDEF81
        IF(LNGCHA .EQ. NCHAN) THEN                                      USRDEF82
          DO 2 KCHAN=1,NCHAN                                            USRDEF83
            ICHAN(KCHAN,2) = IW(INGCHA+KCHAN)                           USRDEF84
2         CONTINUE                                                      USRDEF85
        ELSEIF(LNGCHA .NE. 0) THEN                                      USRDEF86
          WRITE(6,1003) LNGCHA,NCHAN                                    USRDEF87
        ENDIF                                                           USRDEF88
      ENDIF                                                             USRDEF89
C                                                                       USRDEF90
C  Same for A                                                           USRDEF91
C                                                                       USRDEF92
  120 KNGCHA = NAMIND('GCH3')                                           USRDEF93
      IF (KNGCHA .LE. 0) GOTO 130                                       USRDEF94
      INGCHA = IW(KNGCHA)                                               USRDEF95
      IF (INGCHA .GT. 0) THEN                                           USRDEF96
        LNGCHA = IW(INGCHA)                                             USRDEF97
C                                                                       USRDEF98
C  Fill ICHAN array                                                     USRDEF99
C                                                                       USRDE100
        IF(LNGCHA .EQ. NCHAN) THEN                                      USRDE101
          DO 3 KCHAN=1,NCHAN                                            USRDE102
            ICHAN(KCHAN,3) = IW(INGCHA+KCHAN)                           USRDE103
3         CONTINUE                                                      USRDE104
        ELSEIF(LNGCHA .NE. 0) THEN                                      USRDE105
          WRITE(6,1003) LNGCHA,NCHAN                                    USRDE106
        ENDIF                                                           USRDE107
      ENDIF                                                             USRDE108
C                                                                       USRDE109
C  Print out of final values chosen for running H0DECAY                 USRDE110
C                                                                       USRDE111
  130 CONTINUE                                                          USRDE112
      WRITE(6,1004) IDBG                                                USRDE113
      IF ( IDBG .LT. 2 )  GOTO 11                                       USRDE114
      WRITE(6,1005)                                                     USRDE115
      DO 10 KHIG =1,nHIG                                                USRDE116
      DO 10 KCHAN=1,NCHAN                                               USRDE117
        IF(ICHAN(KCHAN,KHIG) .EQ. 1)                                    USRDE118
     .  WRITE(6,1006) KCHAN,CHANNEL(KCHAN,KHIG)                         USRDE119
   10 CONTINUE                                                          USRDE120
   11 CONTINUE                                                          USRDE121
C                                                                       USRDE122
C  Create and fill bank KHIG with those values.                         USRDE123
C                                                                       USRDE124
      DO 20 KCHAN=1,NCHAN                                               USRDE125
      DO 20 KHIG =1,NHIG                                                USRDE126
        ITAB(KCHAN + NCHAN*(KHIG-1)) = ICHAN(KCHAN,KHIG)                USRDE127
   20 CONTINUE                                                          USRDE128
      CALL ALTABL('KHHG',nchan*nhig,1,TAB,'(I)','C')                    USRDE129
      RETURN                                                            USRDE130
C-------------------------------------------------------------------    USRDE131
  999 FORMAT(1X,'    +++ USRDEF +++ Warning +++'/                       USRDE132
     .       1X,'Not enough space for creating KHIG bank')              USRDE133
 1000 FORMAT(/1X,'+++ USRDEF Warning +++ '/                             USRDE134
     .       1X,'Word #  1 in data card GHHG (',I6,                     USRDE135
     .          ') should be between 0 and ',I2)                        USRDE136
 1003 FORMAT(1X,'+++ USRDEF Warning +++ '/                              USRDE137
     .       1X,' Length of data card GCHA (',I6,                       USRDE138
     .          ') should be 0 or ',I2)                                 USRDE139
 1004 FORMAT(1X,'Initial conditions for running H0DECAY are :'/         USRDE140
     .       5X,'. Debug level             : ',I2//)                    USRDE141
1005  FORMAT(1X,'Requested decay channels are :'/)                      USRDE142
1006  FORMAT(5X,I2,'. ',A12)                                            USRDE143
1009  FORMAT(1X,'+++ USRDEF Warning +++ '/                              USRDE144
     .       1X,'Word #',I2,' in data card GHGG (',D14.6,               USRDE145
     .          ') should be between .6 and 2.5')                       USRDE146
      END                                                               USRDE147
      SUBROUTINE vac(ihiggs,mchi,ma,tanb,mq,mur,mdr,                    VAC    2
     *  mtop,at,ab,mu,mh,mhp,hm,hmp,amp,mhch,stop1,stop2,               VAC    3
     *  sbot1,sbot2,sa,ca,stop1w,stop2w,tanbA)                          VAC    4
C--------------------------------------------------------------------   VAC    5
C! Computes vacuum polarization effects to Higgs masses                 VAC    6
C                                                                       VAC    7
C  From M. Carena and C. Wagner.                                        VAC    8
C                                                                       VAC    9
C  Modifications :                                                      VAC   10
C      1. Patrick Janot -- 20 Sep 1995                                  VAC   11
C         Improves the iteration procedure to fasten the routine        VAC   12
C         (--> factor 100 improvement roughly)                          VAC   13
C--------------------------------------------------------------------   VAC   14
      IMPLICIT REAL*8(A-H,M,O-Z)                                        VAC   15
      REAL*8 cma,ctb,cmq,cmur,cmdr,cmtop,cau,cad,cmu,cmchi              CCAREN 2
      REAL*8 cmh,cmhp,chm,chmp,camp,cmhch,csa,cca                       CCAREN 3
      REAL*8 cstop1,cstop2,csbot1,csbot2,ctanbA                         CCAREN 4
      REAL*8 rmtop,rmbot,mz,alpha_1,alpha_2,alpha_3,v,ppi,sint,stw      CCAREN 5
      COMMON / mcarena / rmtop,rmbot,mz,alpha_1,alpha_2,alpha_3,        CCAREN 6
     .                   v,ppi,sint,stw                                 CCAREN 7
C                                                                       CCAREN 8
      DIMENSION delta(2,2),coupt(2,2),T(2,2),sstop2(2),                 VAC   17
     *ssbot2(2),B(2,2),coupb(2,2),                                      VAC   18
     *hcoupt(2,2),hcoupb(2,2),                                          VAC   19
     *acoupt(2,2),acoupb(2,2)                                           VAC   20
      DIMENSION polar(3),pr(3)                                          VAC   21
C                                                                       VAC   22
      delta(1,1) = 1.                                                   VAC   23
      delta(2,2) = 1.                                                   VAC   24
      delta(1,2) = 0.                                                   VAC   25
      delta(2,1) = 0.                                                   VAC   26
C                                                                       VAC   27
Cpaj  v = 174.1                                                         VAC   28
Cpaj  mz=91.18                                                          VAC   29
Cpaj  pi=3.14159                                                        VAC   30
Cpaj  alpha3z=.12                                                       VAC   31
Cpaj  alpha3=1./(1./alpha3z+23./6./pi*log(mtop/mz))                     VAC   32
Cpaj  rmtop = mtop/(1.+4*alpha3/3./pi)                                  VAC   33
Cpaj  rmbot = 3.                                                        VAC   34
      pi = ppi           ! paj                                          VAC   35
C                                                                       VAC   36
      ht = rmtop /v                                                     VAC   37
      CALL rghm(mchi,ma,tanb,mq,mur,mdr,mtop,at,ab,                     VAC   38
     *   mu,mh,hm,mhch,sa,ca,tanbA)                                     VAC   39
      sinb = tanb/(tanb**2+1.)**.5                                      VAC   40
      cosb = 1./(tanb**2+1.)**.5                                        VAC   41
      cos2b = sinb**2 - cosb**2                                         VAC   42
      sinbpa = sinb*ca + cosb*sa                                        VAC   43
      cosbpa = cosb*ca - sinb*sa                                        VAC   44
      mq2 = mq**2                                                       VAC   45
      mur2 = mur**2                                                     VAC   46
      mdr2 = mdr**2                                                     VAC   47
      mst11 = Rmtop**2 + mq2  - 0.35*MZ**2*cos2b                        VAC   48
      mst22 = Rmtop**2 + mur2 - 0.15*MZ**2*cos2b                        VAC   49
      if(mst11.lt.0.) goto 3333                                         VAC   50
      if(mst22.lt.0.) goto 3333                                         VAC   51
      msb11 = Rmbot**2 + mq2  + 0.42*MZ**2*cos2b                        VAC   52
      msb22 = Rmbot**2 + mdr2 + 0.08*MZ**2*cos2b                        VAC   53
      if(msb11.lt.0.) goto 3333                                         VAC   54
      if(msb22.lt.0.) goto 3333                                         VAC   55
      wmst11 = Rmtop**2 + mq2                                           VAC   56
      wmst22 = Rmtop**2 + mur2                                          VAC   57
      mst12 = Rmtop*(At - mu/tanb)                                      VAC   58
      msb12 = Rmbot*(Ab - mu*tanb)                                      VAC   59
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    VAC   60
C                                                                       VAC   61
C                                                                       VAC   62
C            Stop Eigenvalues calculation                               VAC   63
C                                                                       VAC   64
C                                                                       VAC   65
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     VAC   66
      Stop12 = 0.5*(mst11+mst22) +                                      VAC   67
     * 0.5*((mst11+mst22)**2 -                                          VAC   68
     * 4.*(mst11*mst22 - mst12**2))**.5                                 VAC   69
      Stop22 = 0.5*(mst11+mst22) -                                      VAC   70
     * 0.5*((mst11+mst22)**2 - 4.*(mst11*mst22 - mst12**2))**.5         VAC   71
      if(Stop22.lt.0.) goto 3333                                        VAC   72
      sstop2(1) = stop12                                                VAC   73
      sstop2(2) = stop22                                                VAC   74
      stop1 = Stop12**.5                                                VAC   75
      stop2 = Stop22**.5                                                VAC   76
      stop1w = stop1                                                    VAC   77
      stop2w = stop2                                                    VAC   78
C                                                                       VAC   79
      IF ( mst12 .EQ. 0. ) THEN                                         VAC   80
        xst11 = 1.                                                      VAC   81
        xst12 = 0.                                                      VAC   82
        xst21 = 0.                                                      VAC   83
        xst22 = 1.                                                      VAC   84
      ELSE                                                              VAC   85
        xst11 = mst12/(mst12**2+(mst11-stop12)**2)**.5                  VAC   86
        xst12 = - (mst11-stop12)/(mst12**2+(mst11-stop12)**2)**.5       VAC   87
        xst21 = mst12/(mst12**2+(mst11-stop22)**2)**.5                  VAC   88
        xst22 = - (mst11-stop22)/(mst12**2+(mst11-stop22)**2)**.5       VAC   89
      ENDIF                                                             VAC   90
C                                                                       VAC   91
      T(1,1) = xst11                                                    VAC   92
      T(2,2) = xst22                                                    VAC   93
      T(1,2) = xst12                                                    VAC   94
      T(2,1) = xst21                                                    VAC   95
C                                                                       VAC   96
      Sbot12 = 0.5*(msb11+msb22) +                                      VAC   97
     * 0.5*((msb11+msb22)**2 -                                          VAC   98
     * 4.*(msb11*msb22 - msb12**2))**.5                                 VAC   99
      Sbot22 = 0.5*(msb11+msb22) -                                      VAC  100
     * 0.5*((msb11+msb22)**2 - 4.*(msb11*msb22 - msb12**2))**.5         VAC  101
      if(Sbot22.lt.0.) goto 3333                                        VAC  102
      sbot1 = Sbot12**.5                                                VAC  103
      sbot2 = Sbot22**.5                                                VAC  104
      ssbot2(1) = sbot12                                                VAC  105
      ssbot2(2) = sbot22                                                VAC  106
C                                                                       VAC  107
      IF ( msb12 .EQ. 0. ) THEN                                         VAC  108
        xsb11 = 1.                                                      VAC  109
        xsb12 = 0.                                                      VAC  110
        xsb21 = 0.                                                      VAC  111
        xsb22 = 1.                                                      VAC  112
      ELSE                                                              VAC  113
        xsb11 = msb12/(msb12**2+(msb11-sbot12)**2)**.5                  VAC  114
        xsb12 = - (msb11-sbot12)/(msb12**2+(msb11-sbot12)**2)**.5       VAC  115
        xsb21 = msb12/(msb12**2+(msb11-sbot22)**2)**.5                  VAC  116
        xsb22 = - (msb11-sbot22)/(msb12**2+(msb11-sbot22)**2)**.5       VAC  117
      ENDIF                                                             VAC  118
C                                                                       VAC  119
      B(1,1) = xsb11                                                    VAC  120
      B(2,2) = xsb22                                                    VAC  121
      B(1,2) = xsb12                                                    VAC  122
      B(2,1) = xsb21                                                    VAC  123
C                                                                       VAC  124
Cpaj  sint = 0.2320                                                     VAC  125
      sqr = 2.**.5                                                      VAC  126
Cpaj  vp = 174.1*sqr                                                    VAC  127
      vp = v*sqr                                                        VAC  128
C                                                                       VAC  129
cccccccccccccccccccccccccccccccccccc                                    VAC  130
ccc    starting of light higgs                                          VAC  131
cccccccccccccccccccccccccccccccccccc                                    VAC  132
cccccccccccccccccccccccccccccccccccccccc                                VAC  133
      if(ihiggs.eq.0)goto 3524                                          VAC  134
ccccccccccccccccccccccccccccccccccccccccc                               VAC  135
      DO 4646 i = 1,2                                                   VAC  136
      DO 4576 j = 1,2                                                   VAC  137
        coupt(i,j) =                                                    VAC  138
Cpaj * sint*mz**2*2.*sqr/174./3.*sinbpa*(delta(i,j) +                   VAC  139
     * sint*mz**2*2.*sqr/ v  /3.*sinbpa*(delta(i,j) +                   VAC  140
     * (3. - 8.*sint)/4./sint*T(1,i)*T(1,j))                            VAC  141
Cpaj * -rmtop**2/174.1**2*vp/sinb*ca*delta(i,j)                         VAC  142
     * -rmtop**2/  v  **2*vp/sinb*ca*delta(i,j)                         VAC  143
     * -rmtop/vp/sinb*(At*ca + mu*sa)*(T(1,i)*T(2,j) +                  VAC  144
     *T(1,j)*T(2,i))                                                    VAC  145
 4576 CONTINUE                                                          VAC  146
 4646 CONTINUE                                                          VAC  147
C                                                                       VAC  148
      DO 1646 i = 1,2                                                   VAC  149
      DO 1576 j = 1,2                                                   VAC  150
        coupb(i,j) =                                                    VAC  151
Cpaj * -sint*mz**2*2.*sqr/174./6.*sinbpa*(delta(i,j) +                  VAC  152
     * -sint*mz**2*2.*sqr/ v  /6.*sinbpa*(delta(i,j) +                  VAC  153
     * (3. - 4.*sint)/2./sint*B(1,i)*B(1,j))                            VAC  154
Cpaj * +rmbot**2/174.1**2*vp/cosb*sa*delta(i,j)                         VAC  155
     * +rmbot**2/  v  **2*vp/cosb*sa*delta(i,j)                         VAC  156
     * +rmbot/vp/cosb*(Ab*sa + mu*ca)*(B(1,i)*B(2,j) +                  VAC  157
     * B(1,j)*B(2,i))                                                   VAC  158
 1576 CONTINUE                                                          VAC  159
 1646 CONTINUE                                                          VAC  160
C                                                                       VAC  161
      prun = mh                                                         VAC  162
      eps = 1D-4*prun                                                   VAC  163
      iter = 0                                                          VAC  164
7007  iter = iter + 1                                                   VAC  165
      DO 7980 i3 = 1,3                                                  VAC  166
        pr(i3)=prun+(i3-2)*eps/2                                        VAC  167
        p2=pr(i3)**2                                                    VAC  168
        polt = 0.                                                       VAC  169
        DO 7979 i = 1,2                                                 VAC  170
        DO 7978 j = 1,2                                                 VAC  171
         polt = polt + coupt(i,j)**2*3.*                                VAC  172
     *   fint(p2,sstop2(i),sstop2(j))/16./pi**2                         VAC  173
 7978   CONTINUE                                                        VAC  174
 7979   CONTINUE                                                        VAC  175
        polb = 0.                                                       VAC  176
        DO 9979 i = 1,2                                                 VAC  177
        DO 9978 j = 1,2                                                 VAC  178
          polb = polb + coupb(i,j)**2*3.*                               VAC  179
     *    fint(p2,ssbot2(i),ssbot2(j))/16./pi**2                        VAC  180
 9978   CONTINUE                                                        VAC  181
 9979   CONTINUE                                                        VAC  182
        rmtop2 = rmtop**2                                               VAC  183
        mtop2=mtop**2                                                   VAC  184
C                                                                       VAC  185
        poltt =                                                         VAC  186
Cpaj * 3.*rmtop**2/8./pi**2/174.1**2*                                   VAC  187
     * 3.*rmtop**2/8./pi**2/  v  **2*                                   VAC  188
     * ca**2/sinb**2 *                                                  VAC  189
     *   (-2.*mtop**2+.5*p2)*                                           VAC  190
     *  fint(p2,mtop2,mtop2)                                            VAC  191
C                                                                       VAC  192
        pol = polt + polb + poltt                                       VAC  193
        polar(i3) = p2 - mh**2 - pol                                    VAC  194
 7980 CONTINUE                                                          VAC  195
      deriv = (polar(3)-polar(1))/eps                                   VAC  196
      drun = - polar(2)/deriv                                           VAC  197
      prun = prun + drun                                                VAC  198
      p2 = prun**2                                                      VAC  199
      IF ( ABS(drun) .LT. 1D-4 ) GOTO 7777                              VAC  200
      GOTO 7007                                                         VAC  201
 7777 CONTINUE                                                          VAC  202
C                                                                       VAC  203
       mhp = p2**.5                                                     VAC  204
C                                                                       VAC  205
cccccccccccccccccccccccccccccccccccccccc                                VAC  206
ccc   end of light higgs                                                VAC  207
cccccccccccccccccccccccccccccccccccccccc                                VAC  208
 3340 IF(ihiggs.EQ.1)GOTO 3524                                          VAC  209
ccccccccccccccccccccccccccccccccccccccccc                               VAC  210
ccc starting of heavy higgs                                             VAC  211
cccccccccccccccccccccccccccccccccccccccccc                              VAC  212
C                                                                       VAC  213
      DO 1446 I = 1,2                                                   VAC  214
      DO 1476 J = 1,2                                                   VAC  215
        hcoupt(i,j) =                                                   VAC  216
Cpaj * -sint*mz**2*2.*sqr/174./3.*cosbpa*(delta(i,j) +                  VAC  217
     * -sint*mz**2*2.*sqr/ v  /3.*cosbpa*(delta(i,j) +                  VAC  218
     * (3. - 8.*sint)/4./sint*T(1,i)*T(1,j))                            VAC  219
Cpaj * -rmtop**2/174.1**2*vp/sinb*sa*delta(i,j)                         VAC  220
     * -rmtop**2/  v  **2*vp/sinb*sa*delta(i,j)                         VAC  221
     * -rmtop/vp/sinb*(At*sa - mu*ca)*(T(1,i)*T(2,j) +                  VAC  222
     *T(1,j)*T(2,i))                                                    VAC  223
 1476 CONTINUE                                                          VAC  224
 1446 CONTINUE                                                          VAC  225
C                                                                       VAC  226
      DO 1146 I = 1,2                                                   VAC  227
      DO 1176 J = 1,2                                                   VAC  228
        hcoupb(i,j) =                                                   VAC  229
Cpaj * sint*mz**2*2.*sqr/174./6.*cosbpa*(delta(i,j) +                   VAC  230
     * sint*mz**2*2.*sqr/ v  /6.*cosbpa*(delta(i,j) +                   VAC  231
     * (3. - 4.*sint)/2./sint*B(1,i)*B(1,j))                            VAC  232
Cpaj * -rmbot**2/174.1**2*vp/cosb*ca*delta(i,j)                         VAC  233
     * -rmbot**2/  v  **2*vp/cosb*ca*delta(i,j)                         VAC  234
     * -rmbot/vp/cosb*(Ab*ca - mu*sa)*(B(1,i)*B(2,j) +                  VAC  235
     * B(1,j)*B(2,i))                                                   VAC  236
        hcoupb(i,j)=0.                                                  VAC  237
 1176 CONTINUE                                                          VAC  238
 1146 CONTINUE                                                          VAC  239
C                                                                       VAC  240
      prun = hm                                                         VAC  241
      eps = 1D-4*prun                                                   VAC  242
      iter = 0                                                          VAC  243
 1001 iter = iter + 1                                                   VAC  244
      DO 1780 i3 = 1,3                                                  VAC  245
        pr(i3)=prun+(i3-2)*eps/2                                        VAC  246
        hp2=pr(i3)**2                                                   VAC  247
C                                                                       VAC  248
        hpolt = 0.                                                      VAC  249
        do 1779 i = 1,2                                                 VAC  250
        do 1778 j = 1,2                                                 VAC  251
        hpolt = hpolt + hcoupt(i,j)**2*3.*                              VAC  252
     *  fint(hp2,sstop2(i),sstop2(j))/16./pi**2                         VAC  253
 1778 CONTINUE                                                          VAC  254
 1779 CONTINUE                                                          VAC  255
C                                                                       VAC  256
      hpolb = 0.                                                        VAC  257
      DO 1979 I = 1,2                                                   VAC  258
      DO 1978 J = 1,2                                                   VAC  259
        hpolb = hpolb + hcoupb(i,j)**2*3.*                              VAC  260
     *  fint(hp2,ssbot2(i),ssbot2(j))/16./pi**2                         VAC  261
 1978 CONTINUE                                                          VAC  262
 1979 CONTINUE                                                          VAC  263
C                                                                       VAC  264
      rmtop2 = rmtop**2                                                 VAC  265
      mtop2  = mtop**2                                                  VAC  266
C                                                                       VAC  267
      hpoltt =                                                          VAC  268
Cpaj * 3.*rmtop**2/8./pi**2/174.1**2*                                   VAC  269
     * 3.*rmtop**2/8./pi**2/  v  **2*                                   VAC  270
     *  sa**2/sinb**2 *                                                 VAC  271
     *   (-2.*mtop**2+.5*hp2)*                                          VAC  272
     *  fint(hp2,mtop2,mtop2)                                           VAC  273
C                                                                       VAC  274
      hpol = hpolt + hpolb + hpoltt                                     VAC  275
      polar(i3) =hp2-hm**2-hpol                                         VAC  276
 1780 CONTINUE                                                          VAC  277
      deriv = (polar(3)-polar(1))/eps                                   VAC  278
      drun = - polar(2)/deriv                                           VAC  279
      prun = prun + drun                                                VAC  280
      hp2 = prun**2                                                     VAC  281
      IF ( ABS(drun) .LT. 1D-4 ) GOTO 1111                              VAC  282
      GOTO 1001                                                         VAC  283
 1111 CONTINUE                                                          VAC  284
C                                                                       VAC  285
      hmp = hp2**.5                                                     VAC  286
ccccccccccccccccccccccccccccccccccccccccccc                             VAC  287
ccc  end of heavy higgs                                                 VAC  288
cccccccccccccccccccccccccccccccccccccccccccc                            VAC  289
      if(ihiggs.eq.2)goto 3524                                          VAC  290
cccccccccccccccccccccccccccccccccccccccccccc                            VAC  291
ccc  beginning of pseudoscalar higgs                                    VAC  292
cccccccccccccccccccccccccccccccccccccccccccc                            VAC  293
C                                                                       VAC  294
      DO 3446 i = 1,2                                                   VAC  295
      DO 3476 j = 1,2                                                   VAC  296
        acoupt(i,j) =                                                   VAC  297
     * -rmtop/vp/sinb*(At*cosb + mu*sinb)*                              VAC  298
     *  (T(1,i)*T(2,j) -T(1,j)*T(2,i))                                  VAC  299
 3476 CONTINUE                                                          VAC  300
 3446 CONTINUE                                                          VAC  301
C                                                                       VAC  302
      DO 3146 I = 1,2                                                   VAC  303
      DO 3176 J = 1,2                                                   VAC  304
        acoupb(i,j) =                                                   VAC  305
     * rmbot/vp/cosb*(Ab*sinb + mu*cosb)*                               VAC  306
     *  (B(1,i)*B(2,j) -B(1,j)*B(2,i))                                  VAC  307
 3176 CONTINUE                                                          VAC  308
 3146 CONTINUE                                                          VAC  309
C                                                                       VAC  310
      prun = ma                                                         VAC  311
      eps = 1D-4*prun                                                   VAC  312
      iter = 0                                                          VAC  313
 6006 iter = iter + 1                                                   VAC  314
      DO 3780 i3 = 1,3                                                  VAC  315
        pr(i3)=prun+(i3-2)*eps/2                                        VAC  316
        ap2=pr(i3)**2                                                   VAC  317
        apolt = 0.                                                      VAC  318
        DO 3779 I = 1,2                                                 VAC  319
        DO 3778 J = 1,2                                                 VAC  320
          apolt = apolt + acoupt(i,j)**2*3.*                            VAC  321
     *    fint(ap2,sstop2(i),sstop2(j))/16./pi**2                       VAC  322
 3778   CONTINUE                                                        VAC  323
 3779   CONTINUE                                                        VAC  324
        apolb = 0.                                                      VAC  325
        DO 3979 I = 1,2                                                 VAC  326
        DO 3978 J = 1,2                                                 VAC  327
          apolb = apolb + acoupb(i,j)**2*3.*                            VAC  328
     *    fint(ap2,ssbot2(i),ssbot2(j))/16./pi**2                       VAC  329
 3978   CONTINUE                                                        VAC  330
 3979   CONTINUE                                                        VAC  331
        rmtop2 = rmtop**2                                               VAC  332
        mtop2=mtop**2                                                   VAC  333
        apoltt =                                                        VAC  334
Cpaj *  3.*rmtop**2/8./pi**2/174.1**2*                                  VAC  335
     *  3.*rmtop**2/8./pi**2/  v  **2*                                  VAC  336
     *  cosb**2/sinb**2 *                                               VAC  337
     *   (-.5*ap2)*                                                     VAC  338
     *  fint(ap2,mtop2,mtop2)                                           VAC  339
        apol = apolt + apolb + apoltt                                   VAC  340
        polar(i3) = ap2 - ma**2 -apol                                   VAC  341
 3780 CONTINUE                                                          VAC  342
      deriv = (polar(3)-polar(1))/eps                                   VAC  343
      drun = - polar(2)/deriv                                           VAC  344
      prun = prun + drun                                                VAC  345
      ap2 = prun**2                                                     VAC  346
      IF ( ABS(drun) .LT. 1D-4 ) GOTO 6666                              VAC  347
      GOTO 6006                                                         VAC  348
 6666 CONTINUE                                                          VAC  349
C                                                                       VAC  350
      amp = ap2**.5                                                     VAC  351
C                                                                       VAC  352
ccccccccccccccccccccccccccccccccccccccccccc                             VAC  353
ccc end of pseudoscalar higgs                                           VAC  354
cccccccccccccccccccccccccccccccccccccccccccc                            VAC  355
        if(ihiggs.eq.3)goto 3524                                        VAC  356
cccccccccccccccccccccccccccccccccccccccccccc                            VAC  357
C                                                                       VAC  358
3524  RETURN                                                            VAC  359
3333  RETURN                                                            VAC  360
      END                                                               VAC  361
      FUNCTION weakcor(jhig,fmas,fchrg,rt,rw)                           WEAKCOR2
C-------------------------------------------------------------------    WEAKCOR3
C! Compute the one loop electroweak correction to h --> ffbar           WEAKCOR4
C  (for f = e,mu,tau,u,d,s,c,b, but not t)                              WEAKCOR5
C                                                                       WEAKCOR6
C Patrick Janot -- 28 Sept 1995                                         WEAKCOR7
C-------------------------------------------------------------------    WEAKCOR8
      PARAMETER ( nchan=16, nhig=3 )                                    PARAM  2
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,      PARAM  3
     .                  amtau, amb, amc, amt, ame, ammu, amu,           PARAM  4
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),          PARAM  5
     .                  amsq, amneut(4),amchar(2), amarun               PARAM  6
      COMMON / lifeti / tauh(nhig)                                      PARAM  7
      COMMON / conqcd / xlamda5                                         PARAM  8
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2             PARAM  9
      COMMON / mixing / alfa, beta, topmix, botmix,                     PARAM 10
     .                  aa(nhig,4,4),bb(nhig,2,2),                      PARAM 11
     .                  fieldn(4,4), umat(2,2), vmat(2,2),              PARAM 12
     .                  ssmat(4,4),qqmat(4,4)                           PARAM 13
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,             PARAM 14
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab          PARAM 15
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,      PARAM 16
     .                  susSMD, susSML, susSME, susM1, susM2            PARAM 17
      COMMON / flags  / idbg                                            PARAM 18
      DIMENSION suspar(11)                                              PARAM 19
      EQUIVALENCE(susM,suspar(1))                                       PARAM 20
C                                                                       PARAM 21
      PARAMETER(nstep=20)                                               PARAM 22
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)               PARAM 23
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)                PARAM 24
C                                                                       PARAM 25
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)        DATAPI 2
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          DATAPI 3
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         DATAPI 4
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         DATAPI 5
C                                                                       DATAPI 6
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),               CONSTS 2
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw      CONSTS 3
C                                                                       CONSTS 4
      IF ( fmas .EQ. amb ) THEN                                         WEAKCO12
        cf = 1.                                                         WEAKCO13
      ELSE                                                              WEAKCO14
        cf = 7.-4.*alphas(jhig)                                         WEAKCO15
      ENDIF                                                             WEAKCO16
C                                                                       WEAKCO17
      qed     =      alpha(jhig)/pi * 3./2. * fchrg**2                  WEAKCO18
     .             * (3./2.-ALOG(amhig(jhig)**2/fmas**2))               WEAKCO19
C                                                                       WEAKCO20
      weak    =      G_F / (8.*pi**2*SQRT(2.))                          WEAKCO21
     .             * ( Cf*amt**2*rt**2                                  WEAKCO22
     .               + amw**2*rw**2*(3.*ALOG(cw2)/sw2-5.)               WEAKCO23
     .               + amz**2*rw**2*(.5-3.*(1.-4.*sw2*fchrg)**2) )      WEAKCO24
C                                                                       WEAKCO25
      weakcor = 1. + qed + weak                                         WEAKCO26
C                                                                       WEAKCO27
      RETURN                                                            WEAKCO28
      END                                                               WEAKCO29
      SUBROUTINE wzdecy(iwz)                                            WZDECY 2
C-------------------------------------------------------------------    WZDECY 3
C! Decay a virtual W or Z, and prepare the parton shower                WZDECY 4
C  in case of a hadronic decay                                          WZDECY 5
C                                                                       WZDECY 6
C  Input :    iwz, the position of the virtual boson in /LUJET/         WZDECY 7
C                                                                       WZDECY 8
C  Patrick Janot -- 01 Sep 1995                                         WZDECY 9
C-------------------------------------------------------------------    WZDECY10
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
      DIMENSION ijoin(2)                                                WZDECY12
C                                                                       WZDECY13
    1 CALL ludecy(iwz)                                                  WZDECY14
C                                                                       WZDECY15
C In case of a hadronic decay                                           WZDECY16
C                                                                       WZDECY17
      IF ( IABS(k7lu(n7lu,2)) .LE. 8 ) THEN                             WZDECY18
C                                                                       WZDECY19
C Reject too low masses to avoid infinite loops in LUEXEC               WZDECY20
C                                                                       WZDECY21
        xmm = p7lu(iwz,5)                                               WZDECY22
        IF ( xmm .LE. 0.300 ) THEN                                      WZDECY23
          k7lu(n7lu,1) = 1                                              WZDECY24
          n7lu = n7lu - 2                                               WZDECY25
          GOTO 1                                                        WZDECY26
        ENDIF                                                           WZDECY27
C                                                                       WZDECY28
C From Torbjorn : Reset mstj(92) to avoid a duplication of the          WZDECY29
C parton shower and a subsequent crash in LUEXEC                        WZDECY30
C                                                                       WZDECY31
        mstj(92) = 0                                                    WZDECY32
C                                                                       WZDECY33
C Prepare the parton shower                                             WZDECY34
C                                                                       WZDECY35
        ijoin(1) = k7lu(iwz,4)                                          WZDECY36
        ijoin(2) = k7lu(iwz,5)                                          WZDECY37
        njoin = 2                                                       WZDECY38
        CALL lujoin(njoin,ijoin)                                        WZDECY39
        CALL lushow(ijoin(1), ijoin(2), xmm)                            WZDECY40
C                                                                       WZDECY41
      ENDIF                                                             WZDECY42
C                                                                       WZDECY43
      RETURN                                                            WZDECY44
      END                                                               WZDECY45
      FUNCTION zcaren(x)                                                ZCAREN 2
C-----------------------------------------------------------------------ZCAREN 3
C! Function used for the Higgs masses computation                       ZCAREN 4
C                                                                       ZCAREN 5
C  From M. Carena and C. Wagner                                         ZCAREN 6
C-----------------------------------------------------------------------ZCAREN 7
      IMPLICIT REAL*8(A-H,M,O-Z)                                        ZCAREN 8
      SA = 1. - 4.*X                                                    ZCAREN 9
      if(Sa.lt.0.) sa1 = abs(sa)                                        ZCAREN10
      if(SA.lt.0.) Zcaren = 2.*SA1**.5*atan(1./SA1**.5)                 ZCAREN11
      if(SA.gt.0.) Zcaren = SA**.5*log((1.+SA**.5)/(1.-SA**.5))         ZCAREN12
      RETURN                                                            ZCAREN13
      END                                                               ZCAREN14
      SUBROUTINE zdecay(qz,izpol,q1,q2,ifs)                             ZDECAY 2
C-----------------------------------------------------------------------ZDECAY 3
C! Decay a Z boson with a polarization IZPOL                            ZDECAY 4
C                                                                       ZDECAY 5
C  Not used if IKLEI = 1                                                ZDECAY 6
C                                                                       ZDECAY 7
C  Patrick Janot -- 25 Nov 1991                                         ZDECAY 8
C-----------------------------------------------------------------------ZDECAY 9
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)        DATAPI 2
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          DATAPI 3
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         DATAPI 4
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         DATAPI 5
C                                                                       DATAPI 6
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      COMMON / BCS / IW(1000)                                           BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(1000)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      DIMENSION brai(11), kcode(11), xmasmi(11)                         ZZDECK 2
      DATA brai/.0335,.0665,.0335,.0665,.0335,.0665,                    ZZDECK 3
     .          .1540,.1190,.1540,.1190,.1540/                          ZZDECK 4
      DATA kcode/11,12,13,14,15,16,1,2,3,4,5/                           ZZDECK 5
      DATA xmasmi/6*0.,0.3,0.3,1.0,4.0,11.0/                            ZZDECK 6
      COMMON / zzdec / braz(11), kselec(11), fracz                      ZZDECK 7
C                                                                       ZZDECK 8
      DIMENSION qz(4),q1(4),q2(4),p1(4),p2(4)                           ZDECAY13
      REAL*8 betax, betay, betaz, a(3,3)                                ZDECAY14
C                                                                       ZDECAY15
      CALL vzero(q1(1),4)                                               ZDECAY16
      CALL vzero(q2(1),4)                                               ZDECAY17
      CALL vzero(p1(1),4)                                               ZDECAY18
      CALL vzero(p2(1),4)                                               ZDECAY19
C                                                                       ZDECAY20
C Choice of the decay channel                                           ZDECAY21
C                                                                       ZDECAY22
      choix = RNDM(choix)                                               ZDECAY23
      DO ifs = 1,11                                                     ZDECAY24
       IF ( choix .LT. braz(ifs) ) GOTO 1                               ZDECAY25
      ENDDO                                                             ZDECAY26
C                                                                       ZDECAY27
C Choice of the polar decay angle in the Z frame                        ZDECAY28
C                                                                       ZDECAY29
    1 costet = 2.*rndm(costet) - 1.                                     ZDECAY30
      IF ( izpol .EQ. 1 ) THEN                                          ZDECAY31
        weight = (1.+costet**2)/2.                                      ZDECAY32
      ELSE                                                              ZDECAY33
        weight = (1.-costet**2)/2.                                      ZDECAY34
      ENDIF                                                             ZDECAY35
      IF ( weight .LT. RNDM(weight) ) GOTO 1                            ZDECAY36
      sintet = SQRT(1.-costet**2)                                       ZDECAY37
C                                                                       ZDECAY38
C Choice of the azimuthal decay angle in the Z frame                    ZDECAY39
C                                                                       ZDECAY40
      phi = 2.*pi*RNDM(phi)                                             ZDECAY41
      sinphi = SIN(phi)                                                 ZDECAY42
      cosphi = COS(phi)                                                 ZDECAY43
C                                                                       ZDECAY44
C 4-vectors of the decay products in the Z frame                        ZDECAY45
C                                                                       ZDECAY46
      kff = kcode(ifs)                                                  ZDECAY47
      pz    = SQRT(qz(1)**2+qz(2)**2+qz(3)**2)                          ZDECAY48
      ez    = SQRT(qz(4)**2+pz**2)                                      ZDECAY49
      pt = SQRT(qz(1)**2+qz(2)**2)                                      ZDECAY50
      ene = qz(4)/2.                                                    ZDECAY51
      pm = ulmass(kff)                                                  ZDECAY52
      pmm = SQRT(ene**2-pm**2)                                          ZDECAY53
      q1(4) = ene                                                       ZDECAY54
      q1(3) = pmm*costet                                                ZDECAY55
      q1(2) = pmm*sintet*sinphi                                         ZDECAY56
      q1(1) = pmm*sintet*cosphi                                         ZDECAY57
      q2(4) = q1(4)                                                     ZDECAY58
      q2(3) =-q1(3)                                                     ZDECAY59
      q2(2) =-q1(2)                                                     ZDECAY60
      q2(1) =-q1(1)                                                     ZDECAY61
C                                                                       ZDECAY62
C Boost in the lab frame                                                ZDECAY63
C                                                                       ZDECAY64
      betax = 0.                                                        ZDECAY65
      betay = 0.                                                        ZDECAY66
      betaz =-pz/ez                                                     ZDECAY67
      CALL lorenz(betax,betay,betaz,q1)                                 ZDECAY68
      CALL lorenz(betax,betay,betaz,q2)                                 ZDECAY69
C                                                                       ZDECAY70
C Rotation to the standard reference frame                              ZDECAY71
C                                                                       ZDECAY72
      a(1,3) = qz(1)/pz                                                 ZDECAY73
      a(2,3) = qz(2)/pz                                                 ZDECAY74
      a(3,3) = qz(3)/pz                                                 ZDECAY75
      a(1,2) = qz(2)/pt                                                 ZDECAY76
      a(2,2) =-qz(1)/pt                                                 ZDECAY77
      a(3,2) = 0.                                                       ZDECAY78
      a(1,1) = a(2,2)*a(3,3)-a(3,2)*a(2,3)                              ZDECAY79
      a(2,1) = a(3,2)*a(1,3)-a(1,2)*a(3,3)                              ZDECAY80
      a(3,1) = a(1,2)*a(2,3)-a(2,2)*a(1,3)                              ZDECAY81
      p1(1) = a(1,1)*q1(1)+a(1,2)*q1(2)+a(1,3)*q1(3)                    ZDECAY82
      p1(2) = a(2,1)*q1(1)+a(2,2)*q1(2)+a(2,3)*q1(3)                    ZDECAY83
      p1(3) = a(3,1)*q1(1)+a(3,2)*q1(2)+a(3,3)*q1(3)                    ZDECAY84
      p2(1) = a(1,1)*q2(1)+a(1,2)*q2(2)+a(1,3)*q2(3)                    ZDECAY85
      p2(2) = a(2,1)*q2(1)+a(2,2)*q2(2)+a(2,3)*q2(3)                    ZDECAY86
      p2(3) = a(3,1)*q2(1)+a(3,2)*q2(2)+a(3,3)*q2(3)                    ZDECAY87
      CALL ucopy(p1(1),q1(1),3)                                         ZDECAY88
      CALL ucopy(p2(1),q2(1),3)                                         ZDECAY89
      q1(4) = SQRT(q1(1)**2+q1(2)**2+q1(3)**2+pm**2)                    ZDECAY90
      q2(4) = SQRT(q2(1)**2+q2(2)**2+q2(3)**2+pm**2)                    ZDECAY91
C                                                                       ZDECAY92
      RETURN                                                            ZDECAY93
      END                                                               ZDECAY94
