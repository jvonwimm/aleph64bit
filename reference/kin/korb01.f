      SUBROUTINE ASKUSI(IGCOD)                                          ASKUSI 2
C--------------------------------------------------------------------   ASKUSI 3
C Initialization                 B. Bloch-Devaux June 1992.             ASKUSI 4
C--------------------------------------------------------------------   ASKUSI 5
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      COMMON / IDPART/ IA1                                              IDPART 2
      COMMON / INOUT / INUT,IOUT                                        INOUT  2
      COMMON / KGCOMM / ISTA,IDPR,ECMS,WEIT,VRTEX(4),TABL(40),NEVENT(8) KGCOMM 2
      PARAMETER (L1MST=40, L1PAR=80)                                    LUNDCOM2
      PARAMETER (L2KTYP=120, L2PMAS=120, L2PWID=60, L2KFR=80, L2CFR=40) LUNDCOM3
      PARAMETER (L3DPAR=20, L3IDB=120, L3CBR=400, L3KDP=1600)           LUNDCOM4
      PARAMETER (L4CHAG=50, L4CHAF=100)                                 LUNDCOM5
      PARAMETER (LEMSTE=40, LEPARE=80)                                  LUNDCOM6
      PARAMETER (LJNPAR=2000)                                           LUNDCOM7
      COMMON /LUDAT1/   MSTLU1(L1MST),PARLU1(L1PAR)                     LUNDCOM8
      COMMON /LUDAT2/   KTYPL2(L2KTYP),PMASL2(L2PMAS),PWIDL2(L2PWID)    LUNDCOM9
     &                , KFRLU2(L2KFR),CFRLU2(L2CFR)                     LUNDCO10
      COMMON /LUDAT3/   DPARL3(L3DPAR),IDBLU3(L3IDB),CBRLU3(L3CBR)      LUNDCO11
     &                , KDPLU3(L3KDP)                                   LUNDCO12
      COMMON /LUDAT4/   CHAGL4(L4CHAG),CHAFL4(L4CHAF)                   LUNDCO13
      CHARACTER*4 CHAGL4,CHAFL4                                         LUNDCO14
      COMMON /LUDATE/   MSTELE(LEMSTE),PARELE(LEPARE)                   LUNDCO15
      COMMON /LUJETS/   NPARLU,KODELU(LJNPAR,2),PARTLU(LJNPAR,5)        LUNDCO16
C                                                                       LUNDCO17
      DIMENSION E1(3),E2(3),SDVRT(3)                                    ASKUSI11
      PARAMETER (LPDEC = 48)                                            ASKUSI12
C                                                                       ASKUSI13
C Generator code (see KINLIB DOC)                                       ASKUSI14
C                                                                       ASKUSI15
      PARAMETER ( IGCO = 4006 )                                         ASKUSI16
C                                                                       ASKUSI17
      INTEGER NODEC(LPDEC)                                              ASKUSI18
      INTEGER ALTABL,ALRLEP                                             ASKUSI19
      EXTERNAL ALTABL ,ALRLEP                                           ASKUSI20
      CHARACTER TNAM*12                                                 ASKUSI21
C                                                                       ASKUSI22
C   Return generator code                                               ASKUSI23
C                                                                       ASKUSI24
      IGCOD= IGCO                                                       ASKUSI25
      INUT = IW(5)                                                      ASKUSI26
      IOUT = IW(6)                                                      ASKUSI27
      WRITE(IOUT,101) IGCOD                                             ASKUSI28
 101  FORMAT(/,10X,'KORB01 - CODE NUMBER =',I4,                         ASKUSI29
     &       /,10X,'**************************',                        ASKUSI30
     &   /,10x,'Last mod = August 19  , 1992  ')                        BBL001 1
C                                                                       ASKUSI32
C Input parameters for the generator (see subroutine koralb for commentsASKUSI33
C                                                                       ASKUSI34
      AMZ    = 91.17                                                    ASKUSI35
      AMTOP  = 100.                                                     ASKUSI36
      AMH    = 100.                                                     ASKUSI37
      AMNUTA = 0.010                                                    ASKUSI38
      AMNEUT = 0.010                                                    ASKUSI39
      SINW2  = 0.232                                                    ASKUSI40
      GAMM   = 2.5                                                      ASKUSI41
      KEYGSW = 0                                                        ASKUSI42
      KEYRAD = 0                                                        ASKUSI43
      ITRANS = 1                                                        ASKUSI44
      ITFIN  = 1                                                        ASKUSI45
C     NNEUT  = 3                                                        ASKUSI46
      XK0    = 0.01                                                     ASKUSI47
      VVMIN  = 0.00001                                                  ASKUSI48
      VVMAX  = 1.                                                       ASKUSI49
      ENE    = 45.6                                                     ASKUSI50
C                                                                       ASKUSI51
C Lund identifier for electron = 7                                      ASKUSI52
C                                                                       ASKUSI53
      KFB =  7                                                          ASKUSI54
      DO 10 I = 1,3                                                     ASKUSI55
       E1(I) = 0.                                                       ASKUSI56
   10  E2(I) = 0.                                                       ASKUSI57
      JAK1   =  0                                                       ASKUSI58
      JAK2   =  0                                                       ASKUSI59
      ISPIN  =  1                                                       ASKUSI60
      ITDKRC =  0                                                       ASKUSI61
      XK0DEC =  .001                                                    ASKUSI62
      GV     =  1.                                                      ASKUSI63
      GA     = -1.                                                      ASKUSI64
C                                                                       ASKUSI65
C  The default values can be changed by the DATA CARD GKOB              ASKUSI66
C                                                                       ASKUSI67
      JGENE = NLINK('GKOB',0)                                           ASKUSI68
      IF(JGENE.NE.0) THEN                                               ASKUSI69
       AMZ    = RW(JGENE+1)                                             ASKUSI70
       AMTOP  = RW(JGENE+2)                                             ASKUSI71
       AMH    = RW(JGENE+3)                                             ASKUSI72
       AMNUTA = RW(JGENE+4)                                             ASKUSI73
       AMNEUT = RW(JGENE+5)                                             ASKUSI74
       SINW2  = RW(JGENE+6)                                             ASKUSI75
       GAMM   = RW(JGENE+7)                                             ASKUSI76
       KEYGSW = IW(JGENE+8)                                             ASKUSI77
       KEYRAD = IW(JGENE+9)                                             ASKUSI78
       ITRANS = IW(JGENE+10)                                            ASKUSI79
       ITFIN  = IW(JGENE+11)                                            ASKUSI80
C      NNEUT  = IW(JGENE+12)                                            ASKUSI81
       XK0    = RW(JGENE+13)                                            ASKUSI82
       VVMIN  = RW(JGENE+14)                                            ASKUSI83
       VVMAX  = RW(JGENE+15)                                            ASKUSI84
      ENDIF                                                             ASKUSI85
C                                                                       ASKUSI86
C  by the DATA CARD GBEA                                                ASKUSI87
C                                                                       ASKUSI88
      JGBEA = NLINK('GBEA',0)                                           ASKUSI89
      IF(JGBEA.NE.0) THEN                                               ASKUSI90
       ENE   = RW(JGBEA+1)                                              ASKUSI91
       KFB   = IW(JGBEA+2)                                              ASKUSI92
       E1(1) = RW(JGBEA+3)                                              ASKUSI93
       E1(2) = RW(JGBEA+4)                                              ASKUSI94
       E1(3) = RW(JGBEA+5)                                              ASKUSI95
       E2(1) = RW(JGBEA+6)                                              ASKUSI96
       E2(2) = RW(JGBEA+7)                                              ASKUSI97
       E2(3) = RW(JGBEA+8)                                              ASKUSI98
      ENDIF                                                             ASKUSI99
C                                                                       ASKUS100
C  by the DATA CARD GTAU                                                ASKUS101
C                                                                       ASKUS102
      JGTAU = NLINK('GTAU',0)                                           ASKUS103
      IF(JGTAU.NE.0) THEN                                               ASKUS104
       JAK1   = IW(JGTAU+1)                                             ASKUS105
       JAK2   = IW(JGTAU+2)                                             ASKUS106
       ISPIN  = IW(JGTAU+3)                                             ASKUS107
       ITDKRC = IW(JGTAU+4)                                             ASKUS108
       XK0DEC = RW(JGTAU+5)                                             ASKUS109
       GV     = RW(JGTAU+6)                                             ASKUS110
       GA     = RW(JGTAU+7)                                             ASKUS111
      ENDIF                                                             ASKUS112
C                                                                       ASKUS113
C  All the parameters are stored in TABL(I)                             ASKUS114
C                                                                       ASKUS115
      TABL(1)  = AMZ                                                    ASKUS116
C     TABL(2)  = AMTOP                                                  ASKUS117
      TABL(3)  = GAMM                                                   ASKUS118
      TABL(4)  = AMNUTA                                                 ASKUS119
      TABL(5)  = AMNEUT                                                 ASKUS120
      TABL(6)  = SINW2                                                  ASKUS121
C     TABL(7)  = GAMM                                                   ASKUS122
      TABL(8)  = KEYGSW                                                 ASKUS123
      TABL(9)  = KEYRAD                                                 ASKUS124
      TABL(10) = ITRANS                                                 ASKUS125
      TABL(11) = ITFIN                                                  ASKUS126
C     TABL(12) = NNEUT                                                  ASKUS127
      TABL(13) = XK0                                                    ASKUS128
      TABL(14) = VVMIN                                                  ASKUS129
      TABL(15) = VVMAX                                                  ASKUS130
      TABL(16) = ENE                                                    ASKUS131
      TABL(17) = KFB                                                    ASKUS132
      TABL(18) = E1(1)                                                  ASKUS133
      TABL(19) = E1(2)                                                  ASKUS134
      TABL(20) = E1(3)                                                  ASKUS135
      TABL(21) = E2(1)                                                  ASKUS136
      TABL(22) = E2(2)                                                  ASKUS137
      TABL(23) = E2(3)                                                  ASKUS138
      TABL(24) = JAK1                                                   ASKUS139
      TABL(25) = JAK2                                                   ASKUS140
      TABL(26) = ISPIN                                                  ASKUS141
      TABL(27) = ITDKRC                                                 ASKUS142
      TABL(28) = XK0DEC                                                 ASKUS143
      TABL(29) = GV                                                     ASKUS144
      TABL(30) = GA                                                     ASKUS145
C                                                                       ASKUS146
C  Main vertex initialization                                           ASKUS147
C                                                                       ASKUS148
      SDVRT(1) = 0.0185                                                 ASKUS149
      SDVRT(2) = 0.0008                                                 ASKUS150
      SDVRT(3) = 1.02                                                   ASKUS151
      JSVRT = NLINK('SVRT',0)                                           ASKUS152
      IF(JSVRT.NE.0) THEN                                               ASKUS153
       SDVRT(1) = RW(JSVRT+1)                                           ASKUS154
       SDVRT(2) = RW(JSVRT+2)                                           ASKUS155
       SDVRT(3) = RW(JSVRT+3)                                           ASKUS156
      ENDIF                                                             ASKUS157
      TABL(31) = SDVRT(1)                                               ASKUS158
      TABL(32) = SDVRT(2)                                               ASKUS159
      TABL(33) = SDVRT(3)                                               ASKUS160
C                                                                       ASKUS161
C  Fill the KPAR bank with the generator parameters                     ASKUS162
C                                                                       ASKUS163
      NCOL = 33                                                         ASKUS164
      NROW = 1                                                          ASKUS165
      JKPAR = ALTABL('KPAR',NCOL,NROW,TABL,'2I,(F)','C')                ASKUS166
C  Fill RLEP bank                                                       ASKUS167
       IEBEAM = NINT(ENE *1000  )                                       ASKUS168
       JRLEP = ALRLEP(IEBEAM,'    ',0,0,0)                              ASKUS169
C                                                                       ASKUS170
C Initialization event counters                                         ASKUS171
C                                                                       ASKUS172
      DO 20 I = 1,8                                                     ASKUS173
       NEVENT(I) = 0                                                    ASKUS174
   20 CONTINUE                                                          ASKUS175
C                                                                       ASKUS176
C   If tau generation then define A1 and anti-A1 for Lund ....          ASKUS177
C   first we look at a free place to book it in the range 91-100        ASKUS178
C                                                                       ASKUS179
      IA1 = 0                                                           ASKUS180
      IF(ITFIN.EQ.1) THEN                                               ASKUS181
       DO 30 IP = 91,100                                                ASKUS182
        TNAM = ' '                                                      ASKUS183
        CALL LUNAME(IP,TNAM)                                            ASKUS184
        IF(TNAM.NE.' ') GO TO 30                                        ASKUS185
        IA1 = IP                                                        ASKUS186
        GO TO 40                                                        ASKUS187
   30  CONTINUE                                                         ASKUS188
   40  CONTINUE                                                         ASKUS189
       IF(IA1.EQ.0) THEN                                                ASKUS190
        WRITE(IOUT,                                                     ASKUS191
     &'(10X,''ERROR...NO FREE PLACE TO BOOK A NEW PARTICLE, STOP!'')')  ASKUS192
        STOP                                                            ASKUS193
       ENDIF                                                            ASKUS194
C                                                                       ASKUS195
C   then, we book the mass, charge and name in the following way :      ASKUS196
C   mass and charge in /LUDAT2/, name in /LUDAT4/                       ASKUS197
C                                                                       ASKUS198
       PMASL2(IA1)   = 1.251                                            ASKUS199
       KTYPL2(IA1)   =  3                                               ASKUS200
       CHAFL4(IA1)   = 'A1  '                                           ASKUS201
      ENDIF                                                             ASKUS202
CBBL modify Lund masses according to input masses                       ASKUS203
      PMASL2(106) = AMTOP                                               ASKUS204
      PMASL2(4)   = AMH                                                 ASKUS205
      PMASL2(2)   = AMZ                                                 ASKUS206
      PMASL2(12)  = AMNUTA                                              ASKUS207
      PMASL2(107) = 150.                                                ASKUS208
      PMASL2(108) = 300.                                                ASKUS209
CBBL                                                                    ASKUS210
C init pi0 decays by jetset                                             ASKUS211
      IFT = 0                                                           ASKUS212
      CALL LUTAUD(IFT)                                                  ASKUS213
      IF ( IFT.NE.0 ) THEN                                              ASKUS214
         WRITE(IW(6),'(1X,'' ERROR IN LUTAUD RETURN '',I3)') IFT        ASKUS215
         GO TO 60                                                       ASKUS216
      ENDIF                                                             ASKUS217
      CALL KXLUPA (IPART,IKLIN)                                         ASKUS218
      IF (IPART.LE.0 .OR. IKLIN.LE.0) THEN                              ASKUS219
       WRITE (IOUT,'(1X,''ASKUSI :error in PART or KLIN bank - STOP - ''ASKUS220
     &                 ,2I3)') IPART,IKLIN                              ASKUS221
       STOP                                                             ASKUS222
      ENDIF                                                             ASKUS223
CBBL Z0 mass must be updated directly in PART bank (Z0 IS #2 FOR LUND)  ASKUS224
      NAPAR = NAMIND('PART')                                            ASKUS225
      JPART = IW(NAPAR)                                                 ASKUS226
      IZ0 = KGPART(2)                                                   ASKUS227
      IF (JPART.NE.0 .AND. IZ0.GT.0)                                    ASKUS228
     $    RW(JPART+LMHLEN+(IZ0-1)*IW(JPART+LMHCOL)+6) = AMZ             ASKUS229
CBBL                                                                    ASKUS230
C                                                                       ASKUS231
C   Inhibit decays                                                      ASKUS232
C                                                                       ASKUS233
      MXDEC=KNODEC(NODEC,LPDEC)                                         ASKUS234
      MXDEC=MIN(MXDEC,LPDEC)                                            ASKUS235
      IF (MXDEC.GT.0) THEN                                              ASKUS236
         DO 50 I=1,MXDEC                                                ASKUS237
            IF (NODEC(I).GT.0) IDBLU3(NODEC(I))=0                       ASKUS238
   50    CONTINUE                                                       ASKUS239
      ENDIF                                                             ASKUS240
C                                                                       ASKUS241
C   Call generator for initialization                                   ASKUS242
C                                                                       ASKUS243
      LENTRY = 1                                                        ASKUS244
      CALL KORB01(LENTRY)                                               ASKUS245
C                                                                       ASKUS246
C  Print PART and KPAR banks                                            ASKUS247
C                                                                       ASKUS248
      CALL PRPART                                                       ASKUS249
      CALL PRTABL('RLEP',0)                                             ASKUS250
      CALL PRTABL('KPAR',0)                                             ASKUS251
C                                                                       ASKUS252
 60   RETURN                                                            ASKUS253
      END                                                               ASKUS254
      SUBROUTINE ASKUSE (IDP,IST,NTRK,NVRT,ECM,WEI)                     ASKUSE 2
C --------------------------------------------------------------------  ASKUSE 3
C Generation                     G. Bonneaud August, October 1988.      ASKUSE 4
C                                G. Bonneaud February 1989.             ASKUSE 5
C                                "     "     June     1989.             ASKUSE 6
C --------------------------------------------------------------------  ASKUSE 7
C--------------------------------------------------------------------   ASKUSE 8
C     input     : none                                                  ASKUSE 9
C                                                                       ASKUSE10
C     output    : 6 arguments                                           ASKUSE11
C          IDP    : process identification                              ASKUSE12
C          IST    : status flag ( 0 means ok)                           ASKUSE13
C          NTRK   : number of tracks generated and kept                 ASKUSE14
C          NVRT   : number of vertices generated                        ASKUSE15
C          ECM    : center of mass energy for the event                 ASKUSE16
C          WEI    : event weight always equal to 1                      ASKUSE17
C--------------------------------------------------------------------   ASKUSE18
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      COMMON / KGCOMM / ISTA,IDPR,ECMS,WEIT,VRTEX(4),TABL(40),NEVENT(8) KGCOMM 2
      PARAMETER (L1MST=40, L1PAR=80)                                    LUNDCOM2
      PARAMETER (L2KTYP=120, L2PMAS=120, L2PWID=60, L2KFR=80, L2CFR=40) LUNDCOM3
      PARAMETER (L3DPAR=20, L3IDB=120, L3CBR=400, L3KDP=1600)           LUNDCOM4
      PARAMETER (L4CHAG=50, L4CHAF=100)                                 LUNDCOM5
      PARAMETER (LEMSTE=40, LEPARE=80)                                  LUNDCOM6
      PARAMETER (LJNPAR=2000)                                           LUNDCOM7
      COMMON /LUDAT1/   MSTLU1(L1MST),PARLU1(L1PAR)                     LUNDCOM8
      COMMON /LUDAT2/   KTYPL2(L2KTYP),PMASL2(L2PMAS),PWIDL2(L2PWID)    LUNDCOM9
     &                , KFRLU2(L2KFR),CFRLU2(L2CFR)                     LUNDCO10
      COMMON /LUDAT3/   DPARL3(L3DPAR),IDBLU3(L3IDB),CBRLU3(L3CBR)      LUNDCO11
     &                , KDPLU3(L3KDP)                                   LUNDCO12
      COMMON /LUDAT4/   CHAGL4(L4CHAG),CHAFL4(L4CHAF)                   LUNDCO13
      CHARACTER*4 CHAGL4,CHAFL4                                         LUNDCO14
      COMMON /LUDATE/   MSTELE(LEMSTE),PARELE(LEPARE)                   LUNDCO15
      COMMON /LUJETS/   NPARLU,KODELU(LJNPAR,2),PARTLU(LJNPAR,5)        LUNDCO16
C                                                                       LUNDCO17
      COMMON / TAUHEL / HELT1,HELT2                                     TAUHEL 2
      REAL*4 HELT1,HELT2                                                TAUHEL 3
      DIMENSION E1(3),E2(3)                                             ASKUSE23
      PARAMETER (LWP = 4)                                               ASKUSE24
C                                                                       ASKUSE25
      IST  = 0                                                          ASKUSE26
      IDP  = 0                                                          ASKUSE27
      ECM  = 0.                                                         ASKUSE28
      WEI  = 0.                                                         ASKUSE29
C                                                                       ASKUSE30
C  Generate primary vertex                                              ASKUSE31
C                                                                       ASKUSE32
      CALL RANNOR (RN1,RN2)                                             ASKUSE33
      CALL RANNOR (RN3,DUM)                                             ASKUSE34
      VRTEX(1) = RN1*TABL(31)                                           ASKUSE35
      VRTEX(2) = RN2*TABL(32)                                           ASKUSE36
      VRTEX(3) = RN3*TABL(33)                                           ASKUSE37
      VRTEX(4) = 0.                                                     ASKUSE38
C                                                                       ASKUSE39
C  Event generation                                                     ASKUSE40
C                                                                       ASKUSE41
      LENTRY = 2                                                        ASKUSE42
      NEVENT(1) = NEVENT(1) + 1                                         ASKUSE43
      CALL KORB01(LENTRY)                                               ASKUSE44
      IDP  = IDPR                                                       ASKUSE45
      ECM  = ECMS                                                       ASKUSE46
      WEI  = WEIT                                                       ASKUSE47
      IST  = ISTA                                                       ASKUSE48
      IF(IST.NE.0) THEN                                                 ASKUSE49
       NEVENT(4) = NEVENT(4) + 1                                        ASKUSE50
       GO TO 20                                                         ASKUSE51
      ENDIF                                                             ASKUSE52
C   Decay pi0's                                                         ASKUSE53
      CALL LUEXEC                                                       ASKUSE54
C  Book all banks                                                       ASKUSE55
C                                                                       ASKUSE56
      CALL KXLUAL(VRTEX,ISTA,NVRT,NTRK)                                 ASKUSE57
      IST = ISTA                                                        ASKUSE58
      IF(IST.NE.0) THEN                                                 ASKUSE59
       NEVENT(5) = NEVENT(5) + 1                                        ASKUSE60
       GO TO 20                                                         ASKUSE61
      ENDIF                                                             ASKUSE62
C                                                                       ASKUSE63
C  Now book the polarization bank 'KPOL' if necessary                   ASKUSE64
C                                                                       ASKUSE65
      E1(3) = TABL(20)                                                  ASKUSE66
      E2(3) = TABL(23)                                                  ASKUSE67
      ISPIN = TABL(26)                                                  ASKUSE68
      IF(E1(3).NE.0..OR.E2(3).NE.0..OR.ISPIN.EQ.1) THEN                 ASKUSE69
       NPART = 4                                                        ASKUSE70
       LE = LMHLEN + NPART*LWP                                          ASKUSE71
       CALL AUBOS('KPOL',0,LE,JKPOL,IGARB)                              ASKUSE72
       CALL BLIST(IW,'E+','KPOL')                                       ASKUSE73
       CALL BKFMT('KPOL','2I,(I,3F)')                                   ASKUSE74
       IF(JKPOL.GT.0) THEN                                              ASKUSE75
        IW(JKPOL+LMHCOL) = LWP                                          ASKUSE76
        IW(JKPOL+LMHROW) = NPART                                        ASKUSE77
        IW(JKPOL+LMHLEN+1) = -1                                         ASKUSE78
        RW(JKPOL+LMHLEN+2) = TABL(18)                                   ASKUSE79
        RW(JKPOL+LMHLEN+3) = TABL(19)                                   ASKUSE80
        RW(JKPOL+LMHLEN+4) = TABL(20)                                   ASKUSE81
        IW(JKPOL+LMHLEN+LWP+1) = -2                                     ASKUSE82
        RW(JKPOL+LMHLEN+LWP+2) = TABL(21)                               ASKUSE83
        RW(JKPOL+LMHLEN+LWP+3) = TABL(22)                               ASKUSE84
        RW(JKPOL+LMHLEN+LWP+4) = -TABL(23)                              ASKUSE85
        IW(JKPOL+LMHLEN+2*LWP+1) = 1                                    ASKUSE86
        RW(JKPOL+LMHLEN+2*LWP+2) = 0.                                   ASKUSE87
        RW(JKPOL+LMHLEN+2*LWP+3) = 0.                                   ASKUSE88
        RW(JKPOL+LMHLEN+2*LWP+4) = HELT1                                ASKUSE89
        IW(JKPOL+LMHLEN+3*LWP+1) = 2                                    ASKUSE90
        RW(JKPOL+LMHLEN+3*LWP+2) = 0.                                   ASKUSE91
        RW(JKPOL+LMHLEN+3*LWP+3) = 0.                                   ASKUSE92
        RW(JKPOL+LMHLEN+3*LWP+4) = HELT2                                ASKUSE93
       ELSE                                                             ASKUSE94
        IST = 1                                                         ASKUSE95
        NEVENT(6) = NEVENT(6) + 1                                       ASKUSE96
       ENDIF                                                            ASKUSE97
      ENDIF                                                             ASKUSE98
C                                                                       ASKUSE99
C  Event counters                                                       ASKUS100
C                                                                       ASKUS101
      IF(IST.EQ.0) THEN                                                 ASKUS102
       NEVENT(2) = NEVENT(2) + 1                                        ASKUS103
       DO 10 IP = 3,NPARLU                                              ASKUS104
        IF(KODELU(IP,1).EQ.0.AND.KODELU(IP,2).EQ.1) THEN                ASKUS105
         NEVENT(8) = NEVENT(8) + 1                                      ASKUS106
         GO TO 30                                                       ASKUS107
        ENDIF                                                           ASKUS108
   10  CONTINUE                                                         ASKUS109
       NEVENT(7) = NEVENT(7) + 1                                        ASKUS110
      ENDIF                                                             ASKUS111
   20 IF(IST.NE.0) NEVENT(3) = NEVENT(3) + 1                            ASKUS112
C                                                                       ASKUS113
   30 RETURN                                                            ASKUS114
      END                                                               ASKUS115
      SUBROUTINE USCJOB                                                 USCJOB 2
C --------------------------------------------------------------------  USCJOB 3
C End of generation              G. Bonneaud August, October 1988.      USCJOB 4
C --------------------------------------------------------------------  USCJOB 5
      COMMON / INOUT / INUT,IOUT                                        INOUT  2
      COMMON / KGCOMM / ISTA,IDPR,ECMS,WEIT,VRTEX(4),TABL(40),NEVENT(8) KGCOMM 2
C                                                                       USCJOB 8
C End of generation                                                     USCJOB 9
C                                                                       USCJOB10
      LENTRY = 3                                                        USCJOB11
      CALL KORB01(LENTRY)                                               USCJOB12
C                                                                       USCJOB13
C Print event counters                                                  USCJOB14
C                                                                       USCJOB15
       WRITE(IOUT,101)                                                  USCJOB16
  101  FORMAT(//20X,'EVENTS STATISTICS',                                USCJOB17
     &         /20X,'*****************')                                USCJOB18
       WRITE(IOUT,102) NEVENT(1),NEVENT(2),NEVENT(3),                   USCJOB19
     &                 NEVENT(7),NEVENT(8)                              USCJOB20
  102  FORMAT(/5X,'# OF GENERATED EVENTS                      = ',I10,  USCJOB21
     &        /5X,'# OF ACCEPTED  EVENTS                      = ',I10,  USCJOB22
     &        /5X,'# OF REJECTED  EVENTS (ISTA # 0 in ASKUSE) = ',I10,  USCJOB23
     &        /5X,'# OF EVENTS WITHOUT PHOTON                 = ',I10,  USCJOB24
     &        /5X,'# OF EVENTS WITH PHOTON                    = ',I10)  USCJOB25
       WRITE(IOUT,103)                                                  USCJOB26
  103  FORMAT(//20X,'ERRORS STATISTICS',                                USCJOB27
     &         /20X,'*****************')                                USCJOB28
       WRITE(IOUT,104) NEVENT(4),NEVENT(5),NEVENT(6)                    USCJOB29
  104  FORMAT(/10X,'ISTA # 0 FROM KORALB        # OF REJECT = ',I10,    USCJOB30
     &        /10X,'ISTA # 0 FROM KXLUAL        # OF REJECT = ',I10,    USCJOB31
     &        /10X,'ISTA # 0 FROM JKPOL         # OF REJECT = ',I10)    USCJOB32
C                                                                       USCJOB33
      RETURN                                                            USCJOB34
      END                                                               USCJOB35
      SUBROUTINE KORB01(LENTRY)                                         KORB01 2
C --------------------------------------------------------------------  KORB01 3
C Koralb                         B.Bloch-Devaux June  1992.             KORB01 4
C --------------------------------------------------------------------  KORB01 5
C                                                                       KORB01 6
C     MONTE CARLO EVENT GENERATOR FOR THE PROCESSES                     KORB01 7
C                                                                       KORB01 8
C         E+(PB1) E-(PB2)   ---->  TAU+(QP) TAU-(QM)                    KORB01 9
C                                                                       KORB0110
C     AND                                                               KORB0111
C                                                                       KORB0112
C         E+(PB1) E-(PB2)   ---->  TAU+(QP) TAU-(QM) PHOTON(PH)         KORB0113
C                                                                       KORB0114
C                                                                       KORB0115
C  THE INPUT QUANTITIES ARE                                             KORB0116
C ENE    ENERGY OF A BEAM (GEV)                                         KORB0117
C AMZ    Z0    MASS (GEV)                                               KORB0118
C AMTOP  TOP   MASS (GEV)                                               KORB0119
C AMH    HIGGS MASS (GEV)                                               KORB0120
C AMNUTA NEUTRINO TAU MASS (GEV)                                        KORB0121
C E1     =  SPIN POLARIZATION VECTOR FOR THE FIRST BEAM.                KORB0122
C E2     =  SPIN POLARIZATION VECTOR FOR THE SECOND BEAM,               KORB0123
C           BOTH IN THE CORRESPONDING BEAM PARTICLE REST FRAME          KORB0124
C           AND IN BOTH CASES THIRD AXIS DIRECTED ALONG FIRST BEAM,     KORB0125
C           I.E. EE1(3) AND -EE2(3) ARE HELICITIES.                     KORB0126
C ISPIN  =  0,1  SPIN EFFECTS IN DECAY SWITCHED OFF,ON.                 KORB0127
C INRAN  =  INITIALISATION CONSTANT FOR RAND. NUM. GEN. RNF100, POSITIVEKORB0128
C KEYGSW,   IMPLEMENTATION LEVEL OF GLASHOW-SALAM-WEINBERG MODEL:       KORB0129
C        =  0,  N0 Z0, ONLY PHOTON EXCHANGE, NO Z0, NO VAC. POL.,       KORB0130
C        =  1,  PHOTON AND Z0, NO VACUUM POLARISATIONS,                 KORB0131
C        =  2,  PHOTON AND Z0, GSW VACUUM POLARISATIONS INCLUDED,       KORB0132
C        =  3,  ALL GSW CORRECTIONS INCLUDED                            KORB0133
C KEYRAD =  0,  NO QED BREMSSTRAHLUNG,                                  KORB0134
C        =  1,  WITH QED BREMSSTRAHLUNG.                                KORB0135
C JAK1,JAK2, DECAY TYPE FOR TAU+ AND TAU-.                              KORB0136
C            DECAY MODES INCLUDED ARE:                                  KORB0137
C            JAK  =  1  ELECTRON DECAY                                  KORB0138
C                 =  2  MU  DECAY,                                      KORB0139
C                 =  3  PI DECAY ,                                      KORB0140
C                 =  4  RHO DECAY,                                      KORB0141
C                 =  5  A1  DECAY,                                      KORB0142
C                 =  0  INCLUSIVE:  JAK=1,2,3,4,5                       KORB0143
C                 = -1  NO DECAY.                                       KORB0144
C ITFIN  =  1  TAU PAIR PRODUCTION,                                     KORB0145
C        =  2  MUON PAIR PRODUCTION.                                    KORB0146
C KFB = 7,-7 FLAVOUR CODE OF FIRST BEAM, KFB=7  FOR ELECTRON.           KORB0147
C ITDKRC=0 DECAY OF TAU USING TAUOLA,                                   KORB0148
C       >0 RESERVED FOR FUTURE DEVELOPEMENT.                            KORB0149
C GV AND GA ARE COUPLING CONSTANTS OF W-BOSON TO TAU LEPTON,            KORB0150
C       GV=1,GA=-1 REPRESENT THE STANDARD V-A COUPLING.                 KORB0151
C                                                                       KORB0152
C --------------------------------------------------------------------  KORB0153
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      COMMON / INOUT / INUT,IOUT                                        INOUT  2
      COMMON / JAKI   /  JAK1,JAK2,JAKP,JAKM,KTOM                       JAKI   2
      COMMON / KGCOMM / ISTA,IDPR,ECMS,WEIT,VRTEX(4),TABL(40),NEVENT(8) KGCOMM 2
C                                                                       KORB0158
      DIMENSION PB1(4),PB2(4),XPAR(40)                                  KORB0159
      DIMENSION E1(3),E2(3)                                             KORB0160
      INTEGER NPAR(40)                                                  KORB0161
      INTEGER ALTABL                                                    KORB0162
      EXTERNAL ALTABL                                                   KORB0163
C                                                                       KORB0164
C                                                                       KORB0165
C  INITIALIZATION            *********************                      KORB0166
C                                                                       KORB0167
      IF(LENTRY.EQ.1) THEN                                              KORB0168
       NADEB = NAMIND('DEBU')                                           KORB0169
       JDEBU = IW(NADEB)                                                KORB0170
       IF(JDEBU.NE.0) THEN                                              KORB0171
        IDB1 = IW(JDEBU+1)                                              KORB0172
        IDB2 = IW(JDEBU+2)                                              KORB0173
       ENDIF                                                            KORB0174
C                                                                       KORB0175
C Initialization of XPAR and NPAR tables : generator's parameters       KORB0176
C                                                                       KORB0177
       NPAR(1)  = TABL(26)                                              KORB0178
       NPAR(2)  = 0.                                                    KORB0179
       NPAR(3)  = TABL(8)                                               KORB0180
       NPAR(4)  = TABL(9)                                               KORB0181
       NPAR(5)  = TABL(24)                                              KORB0182
       NPAR(6)  = TABL(25)                                              KORB0183
       NPAR(7)  = TABL(11)                                              KORB0184
       NPAR(8)  = TABL(27)                                              KORB0185
       NPAR(9)  = TABL(10)                                              KORB0186
C      NPAR(11) = TABL(12)                                              KORB0187
       XPAR(1)  = TABL(1)                                               KORB0188
       XPAR(2)  = TABL(3)                                               KORB0189
C      XPAR(3)  = TABL(2)                                               KORB0190
       XPAR(4)  = TABL(29)                                              KORB0191
       XPAR(5)  = TABL(30)                                              KORB0192
       XPAR(6)  = TABL(6)                                               KORB0193
C      XPAR(7)  = TABL(7)                                               KORB0194
       XPAR(8)  = TABL(4)                                               KORB0195
       XPAR(9)  = TABL(5)                                               KORB0196
       XPAR(11) = TABL(13)                                              KORB0197
       XPAR(12) = TABL(14)                                              KORB0198
       XPAR(13) = TABL(15)                                              KORB0199
       XPAR(14) = TABL(5)                                               KORB0100
       DO 1 I = 1,3                                                     KORB0101
        E1(I) = TABL(17+I)                                              KORB0102
    1   E2(I) = TABL(20+I)                                              KORB0103
       PB1(1)  = 0.                                                     KORB0104
       PB1(2)  = 0.                                                     KORB0105
       PB1(3)  = TABL(16)                                               KORB0106
       PB1(4)  = TABL(16)                                               KORB0107
       DO 2 I=1,3                                                       KORB0108
    2  PB2(I) = -PB1(I)                                                 KORB0109
       PB2(4) =  PB1(4)                                                 KORB0110
       KFB    =  TABL(17)                                               KORB0111
C                                                                       KORB0112
C KORALB initialization step                                            KORB0113
C                                                                       KORB0114
       NMODE  = -1                                                      KORB0115
       CALL KORALB(NMODE,KFB,PB1,E1,-KFB,PB2,E2,XPAR,NPAR)              KORB0116
C                                                                       KORB0117
C Booking histos                                                        KORB0118
C                                                                       KORB0119
       ITFIN = NPAR(7)                                                  KORB0120
       CALL BUKERD(NMODE,ITFIN)                                         KORB0121
C                                                                       KORB0122
       RETURN                                                           KORB0123
      ENDIF                                                             KORB0124
C                                                                       KORB0125
C  EVENT GENERATION          *********************                      KORB0126
C                                                                       KORB0127
      IF(LENTRY.EQ.2) THEN                                              KORB0128
C                                                                       KORB0129
C Event generation                                                      KORB0130
C                                                                       KORB0131
       NMODE = 0                                                        KORB0132
       CALL KORALB(NMODE,KFB,PB1,E1,-KFB,PB2,E2,XPAR,NPAR)              KORB0133
C                                                                       KORB0134
       ECMS = 2.*TABL(16)                                               KORB0135
       WEIT = 1.                                                        KORB0136
       ISTA = 0                                                         KORB0137
C                                                                       KORB0138
C Update process code                                                   KORB0139
C                                                                       KORB0140
       IF(ITFIN.EQ.1) THEN                                              KORB0141
        ID1 = JAKP                                                      KORB0142
        ID2 = JAKM                                                      KORB0143
cam warning !!! KKEVID to be decoded differently in ALPHA !!!           KORB0144
        IDPR = 100*ID1+ID2                                              KORB0145
        XPR = FLOAT(IDPR)                                               KORB0146
        CALL HFILL(10000,FLOAT(ID1)+.1,FLOAT(ID2)+.1,1.)                KORB0147
       ELSE                                                             KORB0148
        IDPR= ITFIN                                                     KORB0149
       ENDIF                                                            KORB0150
C                                                                       KORB0151
C Print first events depending of DEBUG option                          KORB0152
C                                                                       KORB0153
       IF(NEVENT(1).GE.IDB1.AND.NEVENT(1).LE.IDB2) THEN                 KORB0154
C       CALL DUMPL8                                                     KORB0155
        CALL LULIST(11)                                                 KORB0156
       ENDIF                                                            KORB0157
C                                                                       KORB0158
C Fill histos                                                           KORB0159
C                                                                       KORB0160
       CALL BUKERD(NMODE,ITFIN)                                         KORB0161
C                                                                       KORB0162
       RETURN                                                           KORB0163
      ENDIF                                                             KORB0164
C                                                                       KORB0165
C  END OF GENERATION         *********************                      KORB0166
C                                                                       KORB0167
      IF(LENTRY.EQ.3) THEN                                              KORB0168
C                                                                       KORB0169
C Generator end                                                         KORB0170
C                                                                       KORB0171
       NMODE = 1                                                        KORB0172
       NPAR1 = 0                                                        KORB0173
       NPAR2 = 0                                                        KORB0174
       CALL KORALB(NMODE,NPAR1,PB1,E1,NPAR2,PB2,E2,XPAR,NPAR)           KORB0175
C                                                                       KORB0176
C Print histos                                                          KORB0177
C                                                                       KORB0178
       CALL BUKERD(NMODE,ITFIN)                                         KORB0179
      ENDIF                                                             KORB0180
C                                                                       KORB0181
      RETURN                                                            KORB0182
      END                                                               KORB0183
      SUBROUTINE BUKERD(IMOD,ITFIN)                                     BUKERD 2
C --------------------------------------------------------------------  BUKERD 3
C Book histos                    B.Bloch-Devaux  June 1992              BUKERD 4
C --------------------------------------------------------------------  BUKERD 5
      COMMON / JAKI   /  JAK1,JAK2,JAKP,JAKM,KTOM                       JAKI   2
      PARAMETER (L1MST=40, L1PAR=80)                                    LUNDCOM2
      PARAMETER (L2KTYP=120, L2PMAS=120, L2PWID=60, L2KFR=80, L2CFR=40) LUNDCOM3
      PARAMETER (L3DPAR=20, L3IDB=120, L3CBR=400, L3KDP=1600)           LUNDCOM4
      PARAMETER (L4CHAG=50, L4CHAF=100)                                 LUNDCOM5
      PARAMETER (LEMSTE=40, LEPARE=80)                                  LUNDCOM6
      PARAMETER (LJNPAR=2000)                                           LUNDCOM7
      COMMON /LUDAT1/   MSTLU1(L1MST),PARLU1(L1PAR)                     LUNDCOM8
      COMMON /LUDAT2/   KTYPL2(L2KTYP),PMASL2(L2PMAS),PWIDL2(L2PWID)    LUNDCOM9
     &                , KFRLU2(L2KFR),CFRLU2(L2CFR)                     LUNDCO10
      COMMON /LUDAT3/   DPARL3(L3DPAR),IDBLU3(L3IDB),CBRLU3(L3CBR)      LUNDCO11
     &                , KDPLU3(L3KDP)                                   LUNDCO12
      COMMON /LUDAT4/   CHAGL4(L4CHAG),CHAFL4(L4CHAF)                   LUNDCO13
      CHARACTER*4 CHAGL4,CHAFL4                                         LUNDCO14
      COMMON /LUDATE/   MSTELE(LEMSTE),PARELE(LEPARE)                   LUNDCO15
      COMMON /LUJETS/   NPARLU,KODELU(LJNPAR,2),PARTLU(LJNPAR,5)        LUNDCO16
C                                                                       LUNDCO17
      COMMON / UTIL4 / AQP(4),AQM(4),APH(4)                             UTIL4  2
      REAL*4           AQP   ,AQM   ,APH                                UTIL4  3
      COMMON / IDFC  / IDFF                                             IDFC   2
C                                                                       BUKERD10
C     ====================                                              BUKERD11
C                                                                       BUKERD12
      IF(IMOD.EQ.-1) THEN                                               BUKERD13
C                                                                       BUKERD14
      IF(ITFIN.EQ.2) THEN                                               BUKERD15
       CALL HTITLE('MU+ MU- GAMMA(s) FINAL STATE')                      BUKERD16
       CALL HBOOK1(10001,'MU+ energy distribution $',50,0.,50.,0.)      BUKERD17
       CALL HIDOPT(10001,'LOGY')                                        BUKERD18
       CALL HBOOK1(10002,'MU+ polar angle distribution$',41,-1.,1.05,0.)BUKERD19
       CALL HIDOPT(10002,'LOGY')                                        BUKERD20
       CALL HBOOK1(10003,'MU+ azimuthal angle distribution $',          BUKERD21
     &                                                   40,0.,360.,0.) BUKERD22
       CALL HBOOK1(10004,'MU- energy distribution $',50,0.,50.,0.)      BUKERD23
       CALL HIDOPT(10004,'LOGY')                                        BUKERD24
       CALL HBOOK1(10005,'MU- polar angle distribution$',41,-1.,1.05,0.)BUKERD25
       CALL HIDOPT(10005,'LOGY')                                        BUKERD26
       CALL HBOOK1(10006,'MU- azimuthal angle distribution $',          BUKERD27
     &                                                   40,0.,360.,0.) BUKERD28
       CALL HBOOK1(10007,'MU+/MU- accolinearity distribution (degr.)$', BUKERD29
     &                                                   40,0.,180.,0.) BUKERD30
       CALL HIDOPT(10007,'LOGY')                                        BUKERD31
       CALL HBOOK1(10008,'MU+/MU- accoplanarity distribution (degr.)$', BUKERD32
     &                                                   40,0.,180.,0.) BUKERD33
       CALL HIDOPT(10008,'LOGY')                                        BUKERD34
       CALL HBOOK1(10009,'Photon multiplicity $',40,0.,40.,0.)          BUKERD35
       CALL HBOOK1(10010,'Photon energy distribution $',50,0.,50.,0.)   BUKERD36
       CALL HIDOPT(10010,'LOGY')                                        BUKERD37
       CALL HBOOK1(10011,'Photon angular spectrum $', 50,-1.,1.,0.)     BUKERD38
       CALL HIDOPT(10011,'LOGY')                                        BUKERD39
C                                                                       BUKERD40
      ELSE IF(ITFIN.EQ.1) THEN                                          BUKERD41
       CALL HTITLE('TAU+ TAU- KORLAB   FINAL STATE')                    BUKERD42
       CALL HTABLE(10000,'TAU DECAY MODES$',11,0.,11.,11,.0,11.,0.)     BUKERD43
       CALL HBOOK1(10001,'PHOTON ENERGY SPECTRUM $', 50,0.,1.,0.)       BUKERD44
       CALL HIDOPT(10001,'LOGY')                                        BUKERD45
       CALL HBOOK1(10002,'PHOTON ANGULAR SPECTRUM $', 50,-1.,1.,0.)     BUKERD46
       CALL HIDOPT(10002,'LOGY')                                        BUKERD47
       CALL HBOOK1(10003,'POSITRON ENERGY SPECTRUM $', 50,0.,1.,0.)     BUKERD48
       CALL HBOOK1(10004,'POSITRON ANGULAR SPECTRUM $', 50,-1.,1.,0.)   BUKERD49
       CALL HBOOK1(10005,'MU+ ENERGY SPECTRUM $', 50,0.,1.,0.)          BUKERD50
       CALL HBOOK1(10006,'MU+ ANGULAR SPECTRUM $', 50,-1.,1.,0.)        BUKERD51
       CALL HBOOK1(10007,'PI+ ENERGY SPECTRUM DIRECT FROM TAU$', 50,    BUKERD52
     &   0.,1.,0.)                                                      BUKERD53
       CALL HBOOK1(10008,'PI+ ANGULAR SPECTRUM DIRECT FROM TAU$', 50,   BUKERD54
     &   -1.,1.,0.)                                                     BUKERD55
       CALL HBOOK1(10009,'PI+ ENERGY SPECTRUM FROM RHO$', 50,           BUKERD56
     &   0.,1.,0.)                                                      BUKERD57
       CALL HBOOK1(10010,'PI+ ANGULAR SPECTRUM FROM RHO $', 50,         BUKERD58
     &   -1.,1.,0.)                                                     BUKERD59
       CALL HBOOK1(10011,'PI+ ENERGY SPECTRUM FROM A1$', 50,            BUKERD60
     &   0.,1.,0.)                                                      BUKERD61
       CALL HBOOK1(10012,'PI+ ANGULAR SPECTRUM FROM A1 $', 50,          BUKERD62
     & -1.,1.,0.)                                                       BUKERD63
       CALL HBOOK1(10013,'PI+ ENERGY  SPECTR. FROM K*+$',50,            BUKERD64
     &   .0,1.,.0)                                                      BUKERD65
       CALL HBOOK1(10014,'PI+ ANGULAR SPECTR. FROM K*+$',50,            BUKERD66
     &   -1.,1.,.0)                                                     BUKERD67
       CALL HBOOK1(10015,'PI+- ENERGY  SPECTR. FROM MULTIPI+$',50,      BUKERD68
     &   .0,1.,.0)                                                      BUKERD69
       CALL HBOOK1(10016,'PI+- ANGULAR SPECTR. FROM MULTIPI+$',50,      BUKERD70
     &   -1.,1.,.0)                                                     BUKERD71
       CALL HBOOK1(10017,'K+ ENERGY  SPECTR. DIRECT FROM TAU+',50,      BUKERD72
     &   .0,1.,.0)                                                      BUKERD73
       CALL HBOOK1(10018,'K+ ANGULAR SPECTR. DIRECT FROM TAU+$',50,     BUKERD74
     &   -1.,1.,.0)                                                     BUKERD75
       CALL HBOOK1(10019,'K+ ENERGY  SPECTR. FROM K*+$',50,             BUKERD76
     &   .0,1.,.0)                                                      BUKERD77
       CALL HBOOK1(10020,'K+ ANGULAR SPECTR. FROM K*+$',50,             BUKERD78
     &   -1.,1.,.0)                                                     BUKERD79
       CALL HBOOK1(20003,'E-  ENERGY SPECTR. FROM TAU-$',50,            BUKERD80
     &   .0,1.,.0)                                                      BUKERD81
       CALL HBOOK1(20004,'E- ANGULAR SPECTR. FROM TAU-$',50,            BUKERD82
     &   -1.,1.,.0)                                                     BUKERD83
       CALL HBOOK1(20005,'MU- ENERGY SPECTR. FROM TAU-$',50,            BUKERD84
     &   .0,1.,.0)                                                      BUKERD85
       CALL HBOOK1(20006,'MU- ANGULAR SPECTR. FROM TAU-$',50,           BUKERD86
     &   -1.,1.,.0)                                                     BUKERD87
       CALL HBOOK1(20007,'PI- ENERGY SPECTR. FROM TAU-$',50,            BUKERD88
     &   .0,1.,.0)                                                      BUKERD89
       CALL HBOOK1(20008,'PI- ANGULAR SPECTR. FROM TAU-$',50,           BUKERD90
     &   -1.,1.,.0)                                                     BUKERD91
       CALL HBOOK1(20009,'PI- ENERGY SPECTR. FROM RHO-$',50,            BUKERD92
     &   .0,1.,.0)                                                      BUKERD93
       CALL HBOOK1(20010,'PI- ANGULAR SPECTR. FROM RHO-$',50,           BUKERD94
     &   -1.,1.,.0)                                                     BUKERD95
       CALL HBOOK1(20011,'PI+- ENERGY SPECTR. FROM A1-$',50,            BUKERD96
     &   .0,1.,.0)                                                      BUKERD97
       CALL HBOOK1(20012,'PI+- ANGULAR SPECTR. FROM A1-$',50,           BUKERD98
     &   -1.,1.,.0)                                                     BUKERD99
       CALL HBOOK1(20013,'PI- ENERGY SPECTR. FROM K*-$',50,             BUKER100
     &   .0,1.,.0)                                                      BUKER101
       CALL HBOOK1(20014,'PI- ANGULAR SPECTR. FROM K*-$',50,            BUKER102
     &   -1.,1.,.0)                                                     BUKER103
       CALL HBOOK1(20015,'PI+- ENERGY  SPECTR. FROM MULTIPI-$',50,      BUKER104
     &   .0,1.,.0)                                                      BUKER105
       CALL HBOOK1(20016,'PI+- ANGULAR SPECTR. FROM MULTIPI-$',50,      BUKER106
     &   -1.,1.,.0)                                                     BUKER107
       CALL HBOOK1(20017,'K- ENERGY  SPECTR. DIRECT FROM TAU-',50,      BUKER108
     &   .0,1.,.0)                                                      BUKER109
       CALL HBOOK1(20018,'K- ANGULAR SPECTR. DIRECT FROM TAU-$',50,     BUKER110
     &   -1.,1.,.0)                                                     BUKER111
       CALL HBOOK1(20019,'K- ENERGY  SPECTR. FROM K*-$',50,             BUKER112
     &   .0,1.,.0)                                                      BUKER113
       CALL HBOOK1(20020,'K- ANGULAR SPECTR. FROM K*-$',50,             BUKER114
     &   -1.,1.,.0)                                                     BUKER115
      ELSE IF(ITFIN.EQ.3) THEN                                          BUKER116
       CALL HTITLE('NU NUBAR GAMMA(s) FINAL STATE')                     BUKER117
       CALL HBOOK1(10009,'Photon multiplicity $',40,0.,40.,0.)          BUKER118
       CALL HBOOK1(10010,'Photon energy distribution $',50,0.,50.,0.)   BUKER119
       CALL HIDOPT(10010,'LOGY')                                        BUKER120
       CALL HBOOK1(10011,'PHOTON ANGULAR SPECTRUM $', 50,-1.,1.,0.)     BUKER121
       CALL HIDOPT(10011,'LOGY')                                        BUKER122
      ELSE IF(ITFIN.GT.3) THEN                                          BUKER123
       CALL HTITLE('Q QBAR  GAMMA(s) FINAL STATE')                      BUKER124
       CALL HBOOK1(10001,'QBAR energy distribution $',50,0.,50.,0.)     BUKER125
       CALL HIDOPT(10001,'LOGY')                                        BUKER126
       CALL HBOOK1(10002,'QBAR polar angle distribution',41,-1.,1.05,0.)BUKER127
       CALL HIDOPT(10002,'LOGY')                                        BUKER128
       CALL HBOOK1(10003,'QBAR azimuthal angle distribution ',          BUKER129
     &                                                   40,0.,360.,0.) BUKER130
       CALL HBOOK1(10004,'Q   energy distribution $',50,0.,50.,0.)      BUKER131
       CALL HIDOPT(10004,'LOGY')                                        BUKER132
       CALL HBOOK1(10005,'Q   polar angle distribution$',41,-1.,1.05,0.)BUKER133
       CALL HIDOPT(10005,'LOGY')                                        BUKER134
       CALL HBOOK1(10006,'Q   azimuthal angle distribution $',          BUKER135
     &                                                   40,0.,360.,0.) BUKER136
       CALL HBOOK1(10007,'QBAR/Q  accolinearity distribution (degr.)$', BUKER137
     &                                                   40,0.,180.,0.) BUKER138
       CALL HIDOPT(10007,'LOGY')                                        BUKER139
       CALL HBOOK1(10008,'QBAR/Q  accoplanarity distribution (degr.)$', BUKER140
     &                                                   40,0.,180.,0.) BUKER141
       CALL HIDOPT(10008,'LOGY')                                        BUKER142
       CALL HBOOK1(10009,'Photon multiplicity $',40,0.,40.,0.)          BUKER143
       CALL HBOOK1(10010,'Photon energy distribution $',50,0.,50.,0.)   BUKER144
       CALL HIDOPT(10010,'LOGY')                                        BUKER145
       CALL HBOOK1(10011,'Photon angular spectrum $', 50,-1.,1.,0.)     BUKER146
       CALL HIDOPT(10011,'LOGY')                                        BUKER147
      ENDIF                                                             BUKER148
C                                                                       BUKER149
C     =======================                                           BUKER150
C                                                                       BUKER151
      ELSE IF(IMOD.EQ.0) THEN                                           BUKER152
C                                                                       BUKER153
      IF(ITFIN.GE.2) THEN                                               BUKER154
       IF (ITFIN.NE.3) THEN                                             BUKER155
       CALL HFILL(10001,PARTLU(3,4),0.,1.)                              BUKER156
       THEMUP = PARTLU(3,3)/PARTLU(3,4)                                 BUKER157
       IF(THEMUP.GE. 1.) THEMUP =  0.999999                             BUKER158
       IF(THEMUP.LE.-1.) THEMUP = -0.999999                             BUKER159
       CALL HFILL(10002,THEMUP,0.,1.)                                   BUKER160
       PHIMUP = PARTLU(3,2)/SQRT(PARTLU(3,1)**2+PARTLU(3,2)**2)         BUKER161
       PHIMUP = 180.*ACOS(PHIMUP)/3.14159                               BUKER162
       IF(PARTLU(3,1).LT.0.) PHIMUP = 360. - PHIMUP                     BUKER163
       CALL HFILL(10003,PHIMUP,0.,1.)                                   BUKER164
       CALL HFILL(10004,PARTLU(4,4),0.,1.)                              BUKER165
       THEMUM = PARTLU(4,3)/PARTLU(4,4)                                 BUKER166
       IF(THEMUM.GE. 1.) THEMUM =  0.999999                             BUKER167
       IF(THEMUM.LE.-1.) THEMUM = -0.999999                             BUKER168
       CALL HFILL(10005,THEMUM,0.,1.)                                   BUKER169
       PHIMUM = PARTLU(4,2)/SQRT(PARTLU(4,1)**2+PARTLU(4,2)**2)         BUKER170
       PHIMUM = ACOS(PHIMUM)                                            BUKER171
       PHIMUM = 180.*PHIMUM/3.14159                                     BUKER172
       IF(PARTLU(4,1).LT.0.) PHIMUM = 360. - PHIMUM                     BUKER173
       CALL HFILL(10006,PHIMUM,0.,1.)                                   BUKER174
       ACCOLI = PARTLU(3,1)*PARTLU(4,1)+PARTLU(3,2)*PARTLU(4,2)+        BUKER175
     &          PARTLU(3,3)*PARTLU(4,3)                                 BUKER176
       ACCOLI = ACCOLI/                                                 BUKER177
     &           (SQRT(PARTLU(3,1)**2+PARTLU(3,2)**2+PARTLU(3,3)**2)*   BUKER178
     &            SQRT(PARTLU(4,1)**2+PARTLU(4,2)**2+PARTLU(4,3)**2))   BUKER179
       IF(ACCOLI.GE. 1.) ACCOLI =  0.999999                             BUKER180
       IF(ACCOLI.LE.-1.) ACCOLI = -0.999999                             BUKER181
       ACCOLI = 180.*(1.-ACOS(ACCOLI)/3.14159)                          BUKER182
       CALL HFILL(10007,ACCOLI,0.,1.)                                   BUKER183
       ACCOPL = PARTLU(3,1)*PARTLU(4,1)+PARTLU(3,2)*PARTLU(4,2)         BUKER184
       ACCOPL = ACCOPL/(SQRT(PARTLU(3,1)**2+PARTLU(3,2)**2)*            BUKER185
     &                  SQRT(PARTLU(4,1)**2+PARTLU(4,2)**2))            BUKER186
       IF(ACCOPL.GE. 1.) ACCOPL =  0.999999                             BUKER187
       IF(ACCOPL.LE.-1.) ACCOPL = -0.999999                             BUKER188
       ACCOPL = 180.*(1.-ACOS(ACCOPL)/3.14159)                          BUKER189
       CALL HFILL(10008,ACCOPL,0.,1.)                                   BUKER190
       ENDIF                                                            BUKER191
       XPHOTON = NPARLU - 4                                             BUKER192
       IF(XPHOTON.NE.0.) THEN                                           BUKER193
        CALL HFILL(10009,XPHOTON,0.,1.)                                 BUKER194
        DO 10 I = 5,NPARLU                                              BUKER195
         THEPHO = PARTLU(I,3)/PARTLU(I,4)                               BUKER196
         IF(THEPHO.GE. 1.) THEPHO =  0.999999                           BUKER197
         IF(THEPHO.LE.-1.) THEPHO = -0.999999                           BUKER198
         CALL HFILL(10011,THEPHO,0.,1.)                                 BUKER199
   10    CALL HFILL(10010,PARTLU(I,4),0.,1.)                            BUKER200
       ENDIF                                                            BUKER201
C                                                                       BUKER202
      ELSE IF(ITFIN.EQ.1) THEN                                          BUKER203
       ENE = PARTLU(1,4)                                                BUKER204
C PHOTON ENERGY AND ANGULAR SPECTRUM                                    BUKER205
       IF(APH(4).GT.0.0001) THEN                                        BUKER206
        CALL HFILL(10001,APH(4)/ENE,0.,1.)                              BUKER207
        THEPHO = APH(3)/APH(4)                                          BUKER208
        IF(THEPHO.GE. 1.) THEPHO =  0.999999                            BUKER209
        IF(THEPHO.LE.-1.) THEPHO = -0.999999                            BUKER210
        CALL HFILL(10002,THEPHO,0.,1.)                                  BUKER211
       ENDIF                                                            BUKER212
       DO 50 IP = 5,NPARLU                                              BUKER213
CAM  SKIP RADIATIVE GAMMA OR UNSTABLE PARTICLE                          BUKER214
        IF(KODELU(IP,1).EQ.0.OR.KODELU(IP,1).GE.10000) GO TO 50         BUKER215
C                                                                       BUKER216
        IPORIG=KODELU(IP,1)                                             BUKER217
        KKFORI=KODELU(IPORIG,2)                                         BUKER218
        KFORIG=ABS(KKFORI)                                              BUKER219
        IF(KKFORI.NE.11.AND.KKFORI.NE.-11) THEN                         BUKER220
         IPORIG=MOD(KODELU(IPORIG,1),10000)                             BUKER221
         KKFORI=KODELU(IPORIG,2)                                        BUKER222
        ENDIF                                                           BUKER223
            IF    (KKFORI.EQ.+IDFF) THEN                                BUKER224
              JAK=JAKP                                                  BUKER225
            ELSEIF(KKFORI.EQ.-IDFF) THEN                                BUKER226
              JAK=JAKM                                                  BUKER227
        ELSE                                                            BUKER228
         PRINT *,' ILLEGAL KFORIG IN BUKERD',                           BUKER229
     &    IP,KKFORI,KODELU(IP,1),KODELU(IP,2),KODELU(KODELU(IP,1),1),   BUKER230
     &    KODELU(KODELU(IP,1),2),IPORIG                                 BUKER231
         STOP                                                           BUKER232
        ENDIF                                                           BUKER233
            IH0 = 15000+SIGN(5000,KKFORI)                               BUKER234
C                                                                       BUKER235
        XMOD = SQRT(PARTLU(IP,1)*PARTLU(IP,1)+                          BUKER236
     &           PARTLU(IP,2)*PARTLU(IP,2)+PARTLU(IP,3)*PARTLU(IP,3))   BUKER237
C POSITRON ENERGY AND ANGULAR SPECTRUM                                  BUKER238
        IF(KODELU(IP,2).EQ.7.OR.KODELU(IP,2).EQ.-7) THEN                BUKER239
C ELECTRON ENERGY AND ANGULAR SPECTRUM FROM TAU                         BUKER240
         CALL HFILL(IH0+3, PARTLU(IP,4)/ENE ,0.,1.)                     BUKER241
         CALL HFILL(IH0+4, PARTLU(IP,3)/XMOD,0.,1.)                     BUKER242
        ELSEIF(KODELU(IP,2).EQ.9.OR.KODELU(IP,2).EQ.-9) THEN            BUKER243
C MU ENERGY AND ANGULAR SPECTRUM FROM TAU                               BUKER244
         CALL HFILL(IH0+5, PARTLU(IP,4)/ENE ,0.,1.)                     BUKER245
         CALL HFILL(IH0+6, PARTLU(IP,3)/XMOD,0.,1.)                     BUKER246
        ELSEIF(KODELU(IP,2).EQ.17.OR.KODELU(IP,2).EQ.-17) THEN          BUKER247
         ISGN=SIGN(1,KODELU(IP,2))                                      BUKER248
         IF( KFORIG.EQ.11.AND.JAK.EQ.3) THEN                            BUKER249
C PI ENERGY AND ANGULAR SPECTRUM DIRECT FROM TAU                        BUKER250
          CALL HFILL(IH0+7, PARTLU(IP,4)/ENE ,0.,1.)                    BUKER251
          CALL HFILL(IH0+8,ISGN*PARTLU(IP,3)/XMOD,0.,1.)                BUKER252
         ELSE IF(KFORIG.EQ.27) THEN                                     BUKER253
C PI ENERGY AND ANGULAR SPECTRUM FROM RHO                               BUKER254
          CALL HFILL(IH0+9,  PARTLU(IP,4)/ENE ,0.,1.)                   BUKER255
          CALL HFILL(IH0+10,ISGN*PARTLU(IP,3)/XMOD,0.,1.)               BUKER256
         ELSE IF(KFORIG.EQ.94) THEN                                     BUKER257
C PI ENERGY AND ANGULAR SPECTRUM FROM A1                                BUKER258
          CALL HFILL(IH0+11, PARTLU(IP,4)/ENE ,0.,1.)                   BUKER259
          CALL HFILL(IH0+12, PARTLU(IP,3)/XMOD,0.,1.)                   BUKER260
         ELSE IF(KFORIG.EQ.28) THEN                                     BUKER261
C PI ENERGY AND ANGULAR SPECTRUM FROM K*                                BUKER262
          CALL HFILL(IH0+13, PARTLU(IP,4)/ENE ,0.,1.)                   BUKER263
          CALL HFILL(IH0+14,ISGN*PARTLU(IP,3)/XMOD,0.,1.)               BUKER264
         ELSE                                                           BUKER265
C PI+- ENERGY AND ANGULAR SPECTRUM FROM MULTIPION DECAYS                BUKER266
          CALL HFILL(IH0+15, PARTLU(IP,4)/ENE ,0.,1.)                   BUKER267
          CALL HFILL(IH0+16, PARTLU(IP,3)/XMOD,0.,1.)                   BUKER268
         ENDIF                                                          BUKER269
        ELSEIF(KODELU(IP,2).EQ.18.OR.KODELU(IP,2).EQ.-18) THEN          BUKER270
         ISGN=SIGN(1,KODELU(IP,2))                                      BUKER271
         IF( KFORIG.EQ.11) THEN                                         BUKER272
C K ENERGY AND ANGULAR SPECTRUM DIRECT FROM TAU                         BUKER273
          CALL HFILL(IH0+17, PARTLU(IP,4)/ENE ,0.,1.)                   BUKER274
          CALL HFILL(IH0+18,ISGN*PARTLU(IP,3)/XMOD,0.,1.)               BUKER275
         ELSE                                                           BUKER276
C K ENERGY AND ANGULAR SPECTRUM FROM K*                                 BUKER277
          CALL HFILL(IH0+19,  PARTLU(IP,4)/ENE ,0.,1.)                  BUKER278
          CALL HFILL(IH0+20,ISGN*PARTLU(IP,3)/XMOD,0.,1.)               BUKER279
         ENDIF                                                          BUKER280
        END IF                                                          BUKER281
 50    CONTINUE                                                         BUKER282
      ENDIF                                                             BUKER283
C                                                                       BUKER284
C     ========================                                          BUKER285
C                                                                       BUKER286
      ELSE IF(IMOD.EQ. 1) THEN                                          BUKER287
C                                                                       BUKER288
       CALL HMINIM(0,0.)                                                BUKER289
       CALL HIDOPT(0,'1EVL')                                            BUKER290
       CALL HIDOPT(0,'INTE')                                            BUKER291
       CALL HINDEX                                                      BUKER292
      ENDIF                                                             BUKER293
C                                                                       BUKER294
      RETURN                                                            BUKER295
      END                                                               BUKER296
