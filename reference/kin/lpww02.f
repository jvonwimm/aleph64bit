      SUBROUTINE ASKUSI(IGCOD)                                          ASKUSI 2
C====================================================================== ASKUSI 3
C                                                                       ASKUSI 4
C User initialization routine for LEPWW inside KINGAL.                  ASKUSI 5
C                                                                       ASKUSI 6
C Set generator code                                                    ASKUSI 7
C Read GENE card                                                        ASKUSI 8
C---------------------------------------------------------------------- ASKUSI 9
      IMPLICIT NONE                                                     ASKUSI10
C........................................                               LEPWWA 2
C LEPWW.INC                                                             LEPWWA 3
C                                                                       LEPWWA 4
      INTEGER LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK             LEPWWA 5
      INTEGER IFL1(2),IFL2(2),IFK(10),BK(10)                            LEPWWA 6
      LOGICAL NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS                        LEPWWA 7
      REAL    JSCOD0,JSCOD1                                             LEPWWA 8
      REAL    SDVRT,VERTEX,XSECT                                        LEPWWA 9
      DOUBLE PRECISION ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW        LEPWWA10
      DOUBLE PRECISION SIGWW,WWWSUM,WWWERR,WWWMAX                       LEPWWA11
      DOUBLE PRECISION SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,WMAX,WLARGE           LEPWWA12
      DOUBLE PRECISION TOTSUM,TOTERR                                    LEPWWA13
      DOUBLE PRECISION XISR1,XISR2                                      LEPWWA14
      DOUBLE PRECISION CFACT,COULF                                      LEPWWA15
      DOUBLE PRECISION FLMAS1,FLMAS2,FLMASS                             LEPWWA16
      DOUBLE PRECISION V,A,B,XM2,XMG                                    LEPWWA17
      DOUBLE PRECISION XXS                                              LEPWWA18
      DOUBLE PRECISION P1,P2,Q1,Q2,Q3,Q4,REALP,BOS1,BOS2                LEPWWA19
      DOUBLE PRECISION WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM        LEPWWA20
      DOUBLE PRECISION W0,W1,W2                                         LEPWWA21
      DOUBLE PRECISION ALF(2),PIJ                                       LEPWWA22
      INTEGER BRSTW(9),BRSTZ(12),NWW,NZZ                                LEPWWA23
      INTEGER IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG   LEPWWA24
      INTEGER UWFLAG                                                    LEPWWA25
      DOUBLE PRECISION WWUSER,ZWUSER                                    LEPWWA26
C                                                                       LEPWWA27
      COMMON / LEPWWI / LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK,  LEPWWA28
     * IFL1,IFL2,                                                       LEPWWA29
     * IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG,UWFLAG,  LEPWWA30
     * NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS                               LEPWWA31
      COMMON / LEPWWF / JSCOD0,JSCOD1,SDVRT(3),VERTEX(4),XSECT          LEPWWA32
      COMMON / LEPWIN / WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM,      LEPWWA33
     * W0,W1,W2                                                         LEPWWA34
      COMMON / WZBRS  / BRSTW,BRSTZ,NWW,NZZ                             LEPWWA35
C                                                                       LEPWWA36
      COMMON / KOPLIN / ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW,      LEPWWA37
     * SIGWW,WWWSUM,WWWERR,WWWMAX,SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,           LEPWWA38
     * TOTSUM,TOTERR,CFACT,FLMASS(12),                                  LEPWWA39
     * FLMAS1(2),FLMAS2(2),WMAX,WLARGE,COULF,                           LEPWWA40
     * ALF,PIJ,                                                         LEPWWA41
     * XISR1,XISR2,                                                     LEPWWA42
     * WWUSER,ZWUSER                                                    LEPWWA43
      COMMON / KOPLUT / V(4,5),A(4,5),B(5,5,5),XM2(5),XMG(5)            LEPWWA44
      COMMON / FLAKIN / IFK,BK                                          LEPWWA45
      COMMON / PARTIS / XXS(12)                                         LEPWWA46
      COMMON / MOMENZ / P1(4),P2(4),Q1(4),Q2(4),Q3(4),Q4(4),REALP(5,4), LEPWWA47
     * BOS1(5),BOS2(5)                                                  LEPWWA48
C........................................                               LEPWWA49
      INTEGER JN7LU,K7LU,LJNPAR                                         LUNDCOM2
      REAL    P7LU,V7LU                                                 LUNDCOM3
      PARAMETER( LJNPAR= 4000)                                          LUNDCOM4
      COMMON /LUJETS/ JN7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),              LUNDCOM5
     *                V7LU(LJNPAR,5)                                    LUNDCOM6
C                                                                       LUNDCOM7
      INTEGER MSTJ,MSTU                                                 LUNDCOM8
      REAL    PARJ,PARU                                                 LUNDCOM9
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)           LUNDCO10
C                                                                       LUNDCO11
      INTEGER KCHG                                                      LUNDCO12
      REAL    PMAS,PARF,VCKM                                            LUNDCO13
      COMMON /LUDAT2/ KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)      LUNDCO14
C                                                                       LUNDCO15
      INTEGER MDCY,MDME,KFDP,L2PAR,l2PARF                               LUNDCO16
      PARAMETER (L2PAR=500, L2PARF=2000 )                               LUNDCO17
      REAL BRAT                                                         LUNDCO18
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),        LUNDCO19
     &                KFDP(L2PARF,5)                                    LUNDCO20
      COMMON /LUDAT4/ CHAF(L2PAR)                                       LUNDCO21
      CHARACTER*8 CHAF                                                  LUNDCO22
      INTEGER ISTA,IDPR,NEVENT                                          KGCOMM 2
      REAL    ECMS,WEIT,VRTEX,TABL                                      KGCOMM 3
      COMMON / KGCOMM / ISTA,IDPR,ECMS,WEIT,VRTEX(4),TABL(40),NEVENT(8) KGCOMM 4
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
C                                                                       ASKUSI15
      INTEGER IGCOD,IGCODE,IVERS                                        ASKUSI16
      CHARACTER*11 CHRELS                                               ASKUSI17
      PARAMETER(IGCODE= 5032 )                                          ASKUSI18
      PARAMETER(IVERS = 200)                                            ASKUSI19
      PARAMETER(CHRELS=' 9 May,1995')                                   ASKUSI20
C                                                                       ASKUSI21
      INTEGER NLINK,NAMIND                                              ASKUSI22
      INTEGER I,JGENE,JSVRT,NCOL,NROW,JKPAR,ALTABL,JKLIN,LPDEC          ASKUSI23
      INTEGER IEBEAM,JRLEP,ALRLEP,NAPAR,JPART,KKPART,JPARMA,JPARMW      ASKUSI24
      PARAMETER( JPARMA =  6, JPARMW= 9)                                ASKUSI25
      PARAMETER( LPDEC= 48 )                                            ASKUSI26
      INTEGER NODEC(LPDEC),MXDEC,KNODEC,JIDB                            ASKUSI27
      INTEGER KKEP,KKEN,KKZ,KKW,KKT,KKB                                 ASKUSI28
      PARAMETER( KKEN= 11, KKEP= -11)                                   ASKUSI29
      PARAMETER( KKZ= 23, KKW= 24, KKT= 6, KKB= 5)                      ASKUSI30
      INTEGER LUPAR,KGPART,LUCOMP,IJSCOD,IJS,IKLCOD                     ASKUSI31
      REAL    EBEAM,MASSUM,ULMASS                                       ASKUSI32
      DOUBLE PRECISION ALFAI,SCM                                        ASKUSI33
      EXTERNAL NLINK,NAMIND,ALTABL,ALRLEP,KGPART,LUCOMP                 ASKUSI34
      EXTERNAL ULMASS,IJSCOD,IKLCOD,KNODEC                              ASKUSI35
C                                                                       ASKUSI36
C Set generator code.                                                   ASKUSI37
C                                                                       ASKUSI38
      IGCOD = IGCODE                                                    ASKUSI39
      WRITE(6,10) IGCOD,IVERS,CHRELS                                    ASKUSI40
C                                                                       ASKUSI41
C Set default values                                                    ASKUSI42
C                                                                       ASKUSI43
      CALL LEPWWD                                                       ASKUSI44
C                                                                       ASKUSI45
C The default values can be changed by the DATA CARD GENE               ASKUSI46
C                                                                       ASKUSI47
      JGENE = NLINK('GENE',0)                                           ASKUSI48
      IF (JGENE.NE.0) THEN                                              ASKUSI49
        XMZ    = DBLE( RW(JGENE+1) )                                    ASKUSI50
        XMW    = DBLE( RW(JGENE+2) )                                    ASKUSI51
        XMT    = DBLE( RW(JGENE+3) )                                    ASKUSI52
        ALFAI  = DBLE( RW(JGENE+4) )                                    ASKUSI53
        IF (ALFAI.GT.0.D0) THEN                                         ASKUSI54
          ALFA = 1.D0/ALFAI                                             ASKUSI55
        ENDIF                                                           ASKUSI56
        ALFAS  = DBLE( RW(JGENE+5) )                                    ASKUSI57
        GF     = DBLE( RW(JGENE+6) )                                    ASKUSI58
        SIN2W  = DBLE( RW(JGENE+7) )                                    ASKUSI59
        ECM    = DBLE( RW(JGENE+8) )                                    ASKUSI60
        IRFLAG =       IW(JGENE+9)                                      ASKUSI61
        CSFLAG =       IW(JGENE+10)                                     ASKUSI62
        BWFLAG =       IW(JGENE+11)                                     ASKUSI63
        ASFLAG =       IW(JGENE+12)                                     ASKUSI64
        HIFLAG =       IW(JGENE+13)                                     ASKUSI65
        FRFLAG =       IW(JGENE+14)                                     ASKUSI66
        IZFLAG =       IW(JGENE+15)                                     ASKUSI67
        ILFLAG =       IW(JGENE+16)                                     ASKUSI68
        UWFLAG =       IW(JGENE+17)                                     ASKUSI69
        WWUSER =       RW(JGENE+18)                                     ASKUSI70
        ZWUSER =       RW(JGENE+19)                                     ASKUSI71
      ELSE                                                              ASKUSI72
        WRITE(6,18)                                                     ASKUSI73
        call exit                                                       ASKUSI74
      ENDIF                                                             ASKUSI75
C                                                                       ASKUSI76
C beam spot                                                             ASKUSI77
C                                                                       ASKUSI78
      JSVRT = NLINK('SVRT',0)                                           ASKUSI79
      IF(JSVRT.NE.0) THEN                                               ASKUSI80
        SDVRT(1) = RW(JSVRT+1)                                          ASKUSI81
        SDVRT(2) = RW(JSVRT+2)                                          ASKUSI82
        SDVRT(3) = RW(JSVRT+3)                                          ASKUSI83
      ENDIF                                                             ASKUSI84
C                                                                       ASKUSI85
C initialize the generator                                              ASKUSI86
C                                                                       ASKUSI87
      CALL LPWWIN                                                       ASKUSI88
C                                                                       ASKUSI89
C  All the parameters are stored in TABL(I)                             ASKUSI90
C                                                                       ASKUSI91
      TABL( 1) = SNGL( XMZ )                                            ASKUSI92
      TABL( 2) = SNGL( XMW )                                            ASKUSI93
      TABL( 3) = SNGL( XMT )                                            ASKUSI94
      TABL( 4) = SNGL( ALFA )                                           ASKUSI95
      TABL( 5) = SNGL( ALFAS )                                          ASKUSI96
      TABL( 6) = SNGL( GF )                                             ASKUSI97
      TABL( 7) = SNGL( ECM )                                            ASKUSI98
      TABL( 8) = SNGL( XMG(2)/XM2(2) )                                  ASKUSI99
      TABL( 9) = SNGL( XMG(3)/XM2(3) )                                  ASKUS100
      TABL(10) = SNGL( SIN2W )                                          ASKUS101
      TABL(11) = SNGL( WWWMAX )                                         ASKUS102
      TABL(12) = SDVRT(1)                                               ASKUS103
      TABL(13) = SDVRT(2)                                               ASKUS104
      TABL(14) = SDVRT(3)                                               ASKUS105
      TABL(15) = FLOAT(IRFLAG)                                          ASKUS106
      TABL(16) = FLOAT(CSFLAG)                                          ASKUS107
      TABL(17) = FLOAT(BWFLAG)                                          ASKUS108
      TABL(18) = FLOAT(ASFLAG)                                          ASKUS109
      TABL(19) = FLOAT(FRFLAG)                                          ASKUS110
      TABL(20) = FLOAT(IZFLAG)                                          ASKUS111
      TABL(21) = FLOAT(ILFLAG)                                          ASKUS112
      TABL(22) = FLOAT(UWFLAG)                                          ASKUS113
      TABL(23) = SNGL( WWUSER )                                         ASKUS114
      TABL(24) = SNGL( ZWUSER )                                         ASKUS115
C                                                                       ASKUS116
C  Fill the KPAR bank with the generator parameters                     ASKUS117
C                                                                       ASKUS118
      NCOL = 24                                                         ASKUS119
      NROW = 1                                                          ASKUS120
      JKPAR = ALTABL('KPAR',NCOL,NROW,TABL,'(F)','C')                   ASKUS121
C                                                                       ASKUS122
C  Fill RLEP bank                                                       ASKUS123
C                                                                       ASKUS124
      EBEAM = 0.5*SNGL(ECM)                                             ASKUS125
      IEBEAM = NINT(EBEAM*1000)                                         ASKUS126
      JRLEP = ALRLEP(IEBEAM,'    ',0,0,0)                               ASKUS127
C                                                                       ASKUS128
C create PART bank, using JETSET7.4 scheme.                             ASKUS129
C                                                                       ASKUS130
      CALL KXL74A(JPART,JKLIN)                                          ASKUS131
      IF ((JPART.LE.0).OR.(JKLIN.LE.0)) THEN                            ASKUS132
        WRITE(LUOUTP,910) JPART,JKLIN                                   ASKUS133
        call exit                                                       ASKUS134
      ENDIF                                                             ASKUS135
C                                                                       ASKUS136
C Update masses in the PART bank for Z, W, and top.                     ASKUS137
C                                                                       ASKUS138
      NAPAR = NAMIND('PART')                                            ASKUS139
      JPART = IW(NAPAR)                                                 ASKUS140
      IF (JPART.GT.0) THEN                                              ASKUS141
C                                                                       ASKUS142
        KKPART = KGPART(KKZ)                                            ASKUS143
        IF (KKPART.GT.0) THEN                                           ASKUS144
          RW(JPART+LMHLEN+(KKPART-1)*IW(JPART+1)+JPARMA) = SNGL(XMZ)    ASKUS145
          RW(JPART+LMHLEN+(KKPART-1)*IW(JPART+1)+JPARMW) = SNGL(XMG(2)) ASKUS146
        ENDIF                                                           ASKUS147
C                                                                       ASKUS148
        KKPART = KGPART(KKW)                                            ASKUS149
        IF (KKPART.GT.0) THEN                                           ASKUS150
          RW(JPART+LMHLEN+(KKPART-1)*IW(JPART+1)+JPARMA) = SNGL(XMW)    ASKUS151
          RW(JPART+LMHLEN+(KKPART-1)*IW(JPART+1)+JPARMW) = SNGL(XMG(3)) ASKUS152
        ENDIF                                                           ASKUS153
C                                                                       ASKUS154
        KKPART = KGPART(-KKW)                                           ASKUS155
        IF (KKPART.GT.0) THEN                                           ASKUS156
          RW(JPART+LMHLEN+(KKPART-1)*IW(JPART+1)+JPARMA) = SNGL(XMW)    ASKUS157
          RW(JPART+LMHLEN+(KKPART-1)*IW(JPART+1)+JPARMW) = SNGL(XMG(4)) ASKUS158
        ENDIF                                                           ASKUS159
C                                                                       ASKUS160
        KKPART = KGPART(KKT)                                            ASKUS161
        IF (KKPART.GT.0)                                                ASKUS162
     *  RW(JPART+LMHLEN+(KKPART-1)*IW(JPART+1)+JPARMA) = SNGL(XMT)      ASKUS163
C                                                                       ASKUS164
        KKPART = KGPART(-KKT)                                           ASKUS165
        IF (KKPART.GT.0)                                                ASKUS166
     *  RW(JPART+LMHLEN+(KKPART-1)*IW(JPART+1)+JPARMA) = SNGL(XMT)      ASKUS167
      ELSE                                                              ASKUS168
        WRITE(LUOUTP,920) JPART                                         ASKUS169
      ENDIF                                                             ASKUS170
C                                                                       ASKUS171
C   Inhibit decays                                                      ASKUS172
C                                                                       ASKUS173
      MXDEC=KNODEC(NODEC,LPDEC)                                         ASKUS174
      MXDEC=MIN(MXDEC,LPDEC)                                            ASKUS175
      IF (MXDEC.GT.0) THEN                                              ASKUS176
         DO 20 I=1,MXDEC                                                ASKUS177
          IF (NODEC(I).GT.0) THEN                                       ASKUS178
            JIDB = NLINK('MDC1',NODEC(I))                               ASKUS179
            IF ( JIDB .EQ. 0 ) MDCY(LUCOMP(NODEC(I)),1) = 0             ASKUS180
          ENDIF                                                         ASKUS181
   20    CONTINUE                                                       ASKUS182
      ENDIF                                                             ASKUS183
C                                                                       ASKUS184
      RETURN                                                            ASKUS185
   10 FORMAT(///,                                                       ASKUS186
     * 4X,'***************************************************',/,      ASKUS187
     * 4X,'*                                                 *',/,      ASKUS188
     * 4X,'*                LPWW02 GENERATOR                 *',/,      ASKUS189
     * 4X,'*                                                 *',/,      ASKUS190
     * 4X,'*    KINGAL CODE = ',I5,'                          *',/,     ASKUS191
     * 4X,'*    VERSION     = ',I5,'                          *',/,     ASKUS192
     * 4X,'*    RELEASED    = ',A11,20X,'*',/,                          ASKUS193
     * 4X,'*                                                 *',/,      ASKUS194
     * 4X,'***************************************************',/)      ASKUS195
   18 FORMAT(////,' ************* ERROR ************ ',/,/,             ASKUS196
     * 8X,'YOU MUST PROVIDE A "GENE" CARD.',///)                        ASKUS197
  910 FORMAT(//,' =ASKUSI= Error creating PART bank !!! ',/,            ASKUS198
     * 10X,'IPART=',I4,4X,'JKLIN=',I4,/)                                ASKUS199
  920 FORMAT(//,' =ASKUSI= PART bank is missing !!! ',/)                ASKUS200
      END                                                               ASKUS201
      SUBROUTINE ASKUSE(IDPR0,ISTA0,NTRK,NVRT,ECMS0,WEIT0)              ASKUSE 2
C====================================================================== ASKUSE 3
C                                                                       ASKUSE 4
C Generate one LEPWW event, inside KINGAL.                              ASKUSE 5
C                                                                       ASKUSE 6
C---------------------------------------------------------------------- ASKUSE 7
      IMPLICIT NONE                                                     ASKUSE 8
C........................................                               LEPWWA 2
C LEPWW.INC                                                             LEPWWA 3
C                                                                       LEPWWA 4
      INTEGER LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK             LEPWWA 5
      INTEGER IFL1(2),IFL2(2),IFK(10),BK(10)                            LEPWWA 6
      LOGICAL NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS                        LEPWWA 7
      REAL    JSCOD0,JSCOD1                                             LEPWWA 8
      REAL    SDVRT,VERTEX,XSECT                                        LEPWWA 9
      DOUBLE PRECISION ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW        LEPWWA10
      DOUBLE PRECISION SIGWW,WWWSUM,WWWERR,WWWMAX                       LEPWWA11
      DOUBLE PRECISION SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,WMAX,WLARGE           LEPWWA12
      DOUBLE PRECISION TOTSUM,TOTERR                                    LEPWWA13
      DOUBLE PRECISION XISR1,XISR2                                      LEPWWA14
      DOUBLE PRECISION CFACT,COULF                                      LEPWWA15
      DOUBLE PRECISION FLMAS1,FLMAS2,FLMASS                             LEPWWA16
      DOUBLE PRECISION V,A,B,XM2,XMG                                    LEPWWA17
      DOUBLE PRECISION XXS                                              LEPWWA18
      DOUBLE PRECISION P1,P2,Q1,Q2,Q3,Q4,REALP,BOS1,BOS2                LEPWWA19
      DOUBLE PRECISION WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM        LEPWWA20
      DOUBLE PRECISION W0,W1,W2                                         LEPWWA21
      DOUBLE PRECISION ALF(2),PIJ                                       LEPWWA22
      INTEGER BRSTW(9),BRSTZ(12),NWW,NZZ                                LEPWWA23
      INTEGER IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG   LEPWWA24
      INTEGER UWFLAG                                                    LEPWWA25
      DOUBLE PRECISION WWUSER,ZWUSER                                    LEPWWA26
C                                                                       LEPWWA27
      COMMON / LEPWWI / LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK,  LEPWWA28
     * IFL1,IFL2,                                                       LEPWWA29
     * IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG,UWFLAG,  LEPWWA30
     * NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS                               LEPWWA31
      COMMON / LEPWWF / JSCOD0,JSCOD1,SDVRT(3),VERTEX(4),XSECT          LEPWWA32
      COMMON / LEPWIN / WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM,      LEPWWA33
     * W0,W1,W2                                                         LEPWWA34
      COMMON / WZBRS  / BRSTW,BRSTZ,NWW,NZZ                             LEPWWA35
C                                                                       LEPWWA36
      COMMON / KOPLIN / ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW,      LEPWWA37
     * SIGWW,WWWSUM,WWWERR,WWWMAX,SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,           LEPWWA38
     * TOTSUM,TOTERR,CFACT,FLMASS(12),                                  LEPWWA39
     * FLMAS1(2),FLMAS2(2),WMAX,WLARGE,COULF,                           LEPWWA40
     * ALF,PIJ,                                                         LEPWWA41
     * XISR1,XISR2,                                                     LEPWWA42
     * WWUSER,ZWUSER                                                    LEPWWA43
      COMMON / KOPLUT / V(4,5),A(4,5),B(5,5,5),XM2(5),XMG(5)            LEPWWA44
      COMMON / FLAKIN / IFK,BK                                          LEPWWA45
      COMMON / PARTIS / XXS(12)                                         LEPWWA46
      COMMON / MOMENZ / P1(4),P2(4),Q1(4),Q2(4),Q3(4),Q4(4),REALP(5,4), LEPWWA47
     * BOS1(5),BOS2(5)                                                  LEPWWA48
C........................................                               LEPWWA49
      INTEGER JN7LU,K7LU,LJNPAR                                         LUNDCOM2
      REAL    P7LU,V7LU                                                 LUNDCOM3
      PARAMETER( LJNPAR= 4000)                                          LUNDCOM4
      COMMON /LUJETS/ JN7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),              LUNDCOM5
     *                V7LU(LJNPAR,5)                                    LUNDCOM6
C                                                                       LUNDCOM7
      INTEGER MSTJ,MSTU                                                 LUNDCOM8
      REAL    PARJ,PARU                                                 LUNDCOM9
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)           LUNDCO10
C                                                                       LUNDCO11
      INTEGER KCHG                                                      LUNDCO12
      REAL    PMAS,PARF,VCKM                                            LUNDCO13
      COMMON /LUDAT2/ KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)      LUNDCO14
C                                                                       LUNDCO15
      INTEGER MDCY,MDME,KFDP,L2PAR,l2PARF                               LUNDCO16
      PARAMETER (L2PAR=500, L2PARF=2000 )                               LUNDCO17
      REAL BRAT                                                         LUNDCO18
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),        LUNDCO19
     &                KFDP(L2PARF,5)                                    LUNDCO20
      COMMON /LUDAT4/ CHAF(L2PAR)                                       LUNDCO21
      CHARACTER*8 CHAF                                                  LUNDCO22
      INTEGER ISTA,IDPR,NEVENT                                          KGCOMM 2
      REAL    ECMS,WEIT,VRTEX,TABL                                      KGCOMM 3
      COMMON / KGCOMM / ISTA,IDPR,ECMS,WEIT,VRTEX(4),TABL(40),NEVENT(8) KGCOMM 4
C                                                                       ASKUSE12
      INTEGER IDPR0,ISTA0,NTRK,NVRT                                     ASKUSE13
      REAL    ECMS0,WEIT0,VCOOR(4)                                      ASKUSE14
      INTEGER KKPART,KGPART                                             ASKUSE15
      INTEGER KKEP,KKEN,KKPHOT,NTR,NVX,JKINE,KBKINE                     ASKUSE16
      PARAMETER( KKEN= 11, KKEP= -11, KKPHOT= 22)                       ASKUSE17
      REAL    PXYZM(4),EPHO,EPHMIN,EMASS                                ASKUSE18
      PARAMETER(EPHMIN=0.001)                                           ASKUSE19
      PARAMETER( EMASS = 0.51099906E-3 )                                ASKUSE20
      EXTERNAL KGPART,KBKINE                                            ASKUSE21
C                                                                       ASKUSE22
      IDPR0 = 0                                                         ASKUSE23
      ISTA0 = 0                                                         ASKUSE24
      NTRK  = 0                                                         ASKUSE25
      NVRT  = 0                                                         ASKUSE26
C                                                                       ASKUSE27
C======================== GENERATE AN EVENT ============================ASKUSE28
C                                                                       ASKUSE29
      CALL LEPWWG                                                       ASKUSE30
C                                                                       ASKUSE31
C======================= INTERFACE TO ALEPH ============================ASKUSE32
C                                                                       ASKUSE33
C Fill beam particle KINE banks, with negative vertex number!           ASKUSE34
C The beams are listed according to their energy after ISR.             ASKUSE35
C Include ISR photons if their energy is at least 1 MeV.                ASKUSE36
C                                                                       ASKUSE37
      PXYZM(1) = 0.0                                                    ASKUSE38
      PXYZM(2) = 0.0                                                    ASKUSE39
      PXYZM(3) = SNGL(0.5D0*XISR1*ECM)                                  ASKUSE40
      PXYZM(4) = EMASS                                                  ASKUSE41
      KKPART = KGPART(KKEN)                                             ASKUSE42
      NTR = -1                                                          ASKUSE43
      NVX = 0                                                           ASKUSE44
      JKINE = KBKINE(NTR,PXYZM,KKPART,NVX)                              ASKUSE45
      IF (JKINE.LE.0) THEN                                              ASKUSE46
        WRITE(LUOUTP,9210) JKINE                                        ASKUSE47
        call exit                                                       ASKUSE48
      ENDIF                                                             ASKUSE49
      PXYZM(3) = -SNGL(0.5D0*XISR2*ECM)                                 ASKUSE50
      PXYZM(4) = EMASS                                                  ASKUSE51
      KKPART = KGPART(KKEP)                                             ASKUSE52
      NTR = NTR-1                                                       ASKUSE53
      NVX = 0                                                           ASKUSE54
      JKINE = KBKINE(NTR,PXYZM,KKPART,NVX)                              ASKUSE55
      IF (JKINE.LE.0) THEN                                              ASKUSE56
        WRITE(LUOUTP,9210) JKINE                                        ASKUSE57
        call exit                                                       ASKUSE58
      ENDIF                                                             ASKUSE59
      EPHO = SNGL(0.5D0*ECM*(1.0D0-XISR1))                              ASKUSE60
      IF (EPHO.GE.EPHMIN) THEN                                          ASKUSE61
        PXYZM(3) = EPHO                                                 ASKUSE62
        PXYZM(4) = 0.0                                                  ASKUSE63
        KKPART = KGPART(KKPHOT)                                         ASKUSE64
        NTR = NTR-1                                                     ASKUSE65
        NVX = 0                                                         ASKUSE66
        JKINE = KBKINE(NTR,PXYZM,KKPART,NVX)                            ASKUSE67
      ENDIF                                                             ASKUSE68
      EPHO = SNGL(0.5D0*ECM*(1.0D0-XISR2))                              ASKUSE69
      IF (EPHO.GE.EPHMIN) THEN                                          ASKUSE70
        PXYZM(3) = -EPHO                                                ASKUSE71
        PXYZM(4) = 0.0                                                  ASKUSE72
        KKPART = KGPART(KKPHOT)                                         ASKUSE73
        NTR = NTR-1                                                     ASKUSE74
        NVX = 0                                                         ASKUSE75
        JKINE = KBKINE(NTR,PXYZM,KKPART,NVX)                            ASKUSE76
      ENDIF                                                             ASKUSE77
C                                                                       ASKUSE78
C  Book all banks                                                       ASKUSE79
C                                                                       ASKUSE80
      CALL UCOPY(VERTEX,VCOOR,4)                                        ASKUSE81
      CALL UCOPY(VERTEX,VRTEX,4)                                        ASKUSE82
      CALL KXL7AL(VCOOR,ISTA,NVRT,NTRK)                                 ASKUSE83
C                                                                       ASKUSE84
C process code:                                                         ASKUSE85
C   IFL1(1) : Lund code for up-type fermion from W+  (or Z)             ASKUSE86
C   IFL2(1) : Lund code for up-type fermion from W-  (or Z)             ASKUSE87
C   IPEAK : how phase space was selected                                ASKUSE88
C             0 = pure WW state, so peaking in WW                       ASKUSE89
C             1 = WW/ZZ state, with peaking in WW                       ASKUSE90
C             2 = WW/ZZ state, with peaking in ZZ                       ASKUSE91
C             3 = pure ZZ state, so peaking in ZZ                       ASKUSE92
C                                                                       ASKUSE93
      IDPR = IFL1(1) + 100*IFL2(1) + 10000*IPEAK                        ASKUSE94
C                                                                       ASKUSE95
C return information                                                    ASKUSE96
C                                                                       ASKUSE97
      WEIT = 1.0                                                        ASKUSE98
      ECMS = SNGL(ECM)                                                  ASKUSE99
      ISTA0 = ISTA                                                      ASKUS100
      WEIT0 = WEIT                                                      ASKUS101
      ECMS0 = ECMS                                                      ASKUS102
      IDPR0 = IDPR                                                      ASKUS103
C                                                                       ASKUS104
      RETURN                                                            ASKUS105
 9001 FORMAT(/,' =ASKUSE= No PART bank !!!! STOP.',/)                   ASKUS106
 9210 FORMAT(/,' =ASKUSE= KINE bank for beam electrons not created:',   ASKUS107
     * 3X,'JKINE=',I4,/)                                                ASKUS108
      END                                                               ASKUS109
      SUBROUTINE USCJOB                                                 USCJOB 2
C====================================================================== USCJOB 3
C finish job                                                            USCJOB 4
C---------------------------------------------------------------------- USCJOB 5
      IMPLICIT NONE                                                     USCJOB 6
      CALL LPWWF                                                        USCJOB 7
      END                                                               USCJOB 8
      SUBROUTINE USKRIN(ECMIN)                                          USKRIN 2
C====================================================================== USKRIN 3
C Reinitialize LEPWW generator.                                         USKRIN 4
C---------------------------------------------------------------------- USKRIN 5
      IMPLICIT NONE                                                     USKRIN 6
      REAL ECMIN                                                        USKRIN 7
      RETURN                                                            USKRIN 8
      END                                                               USKRIN 9
      FUNCTION XKSECT(ECMIN)                                            XKSECT 2
C====================================================================== XKSECT 3
C Calculate cross section at new CM energy.                             XKSECT 4
C---------------------------------------------------------------------- XKSECT 5
      IMPLICIT NONE                                                     XKSECT 6
      REAL ECMIN,XKSECT                                                 XKSECT 7
      XKSECT=0.                                                         XKSECT 8
      RETURN                                                            XKSECT 9
      END                                                               XKSECT10
