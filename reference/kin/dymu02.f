      SUBROUTINE ASKUSI(IGCOD)                                          ASKUSI 2
C--------------------------------------------------------------------   ASKUSI 3
C      B.Bloch-Devaux  June 1989  IMPLEMENTATION OF DYMU3               ASKUSI 4
C              DYINIT initialises DYMU3                                 ASKUSI 5
C                                                                       ASKUSI 6
C     structure : subroutine                                            ASKUSI 7
C                                                                       ASKUSI 8
C     input     : none                                                  ASKUSI 9
C                                                                       ASKUSI10
C     output    : generator code as define in the KINGAL library        ASKUSI11
C--------------------------------------------------------------------   ASKUSI12
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      PARAMETER (L1MST=40, L1PAR=80)                                    LUNDCOM2
      PARAMETER (L2KTYP=120, L2PMAS=120, L2PWID=60, L2KFR=80, L2CFR=40) LUNDCOM3
      PARAMETER (L3DPAR=20, L3IDB=120, L3CBR=400, L3KDP=1600)           LUNDCOM4
      PARAMETER (LEMSTE=40, LEPARE=80)                                  LUNDCOM5
      PARAMETER (LJNPAR=2000)                                           LUNDCOM6
      COMMON /LUDAT1/   MSTLU1(L1MST),PARLU1(L1PAR)                     LUNDCOM7
      COMMON /LUDAT2/   KTYPL2(L2KTYP),PMASL2(L2PMAS),PWIDL2(L2PWID)    LUNDCOM8
     &                , KFRLU2(L2KFR),CFRLU2(L2CFR)                     LUNDCOM9
      COMMON /LUDAT3/   DPARL3(L3DPAR),IDBLU3(L3IDB),CBRLU3(L3CBR)      LUNDCO10
     &                , KDPLU3(L3KDP)                                   LUNDCO11
      COMMON /LUDATE/   MSTELE(LEMSTE),PARELE(LEPARE)                   LUNDCO12
      COMMON /LUJETS/   NPARLU,KODELU(LJNPAR,2),PARTLU(LJNPAR,5)        LUNDCO13
C                                                                       LUNDCO14
      COMMON/ZDUMP / IDEBU , NEVT                                       LUSTAT 2
      COMMON/LUSTAT/ ICOULU(10),SVRT(3)                                 LUSTAT 3
      COMMON/RESULT/ SIGBOR,SIGTOT,ERRSIG,ASYTOT,ERRASY                 LUSTAT 4
      PARAMETER ( IGCO = 3004)                                          ASKUSI16
C                                                                       ASKUSI17
C     DYMU3 INPUT VARIABLES                                             ASKUSI18
C                                                                       ASKUSI19
      COMMON /CONST/ ALFA,PI,ALFA1                                      DYCONS 2
      COMMON /RUNPAR/ SOLD,ID2,ID3,FINEXP,POIDS,INTERF,XK0              DYCONS 3
      COMMON /WEAK/ AEL,AMU,AMZ,GAMM,SW2,CA2,CV2,CA2CV2,COL,T3,QI       DYCONS 4
      COMMON /BEAM/ S0,EBEAM                                            DYCONS 5
      COMMON /TAU / TAU,CPTAU,HEL,PITAU(4)                              DYCONS 6
      COMMON/WEAKQ/WEAKC(11,6),XSECT(6),XTOT                            DYCONS 7
      DIMENSION TABL(25),ISEED(3)                                       ASKUSI21
C                                                                       ASKUSI22
      PARAMETER (LPDEC=48)                                              ASKUSI23
      INTEGER NODEC(LPDEC)                                              ASKUSI24
      INTEGER ALTABL,ALRLEP                                             ASKUSI25
      EXTERNAL ALTABL,ALRLEP                                            ASKUSI26
      DO 5 K=1,10                                                       ASKUSI27
 5    ICOULU(K)=0                                                       ASKUSI28
      XTOT = 0.                                                         ASKUSI29
C                                                                       ASKUSI30
C   RETURN THE GENERATOR CODE                                           ASKUSI31
C                                                                       ASKUSI32
      IGCOD=IGCO                                                        ASKUSI33
C                                                                       ASKUSI34
C      DYMU3 PARAMETERS     DEFAULT VALUES                              ASKUSI35
C                         BEAM ENERGY                                   ASKUSI36
      EBEAM    =46.1                                                    ASKUSI37
C                         MZ,GAMZ,SIN2 (EFFECTIVE)                      ASKUSI38
      AMZ      =92.                                                     ASKUSI39
      GAMM = 2.56                                                       ASKUSI40
      SW2 = .2293                                                       ASKUSI41
      SOLD = -1.                                                        ASKUSI42
      ID2 = 0                                                           ASKUSI43
      ID3 = 0                                                           ASKUSI44
      IEXPO = 1                                                         ASKUSI45
      FINEXP =1.                                                        ASKUSI46
      POIDS = 1.                                                        ASKUSI47
      INTERF = 0                                                        ASKUSI48
      IDEBU = 0                                                         ASKUSI49
      NEVT = 0                                                          ASKUSI50
      TAU = 0.                                                          ASKUSI51
C                         K0 MINIMUM HARD PHOTON ENERGY/EBEAM           ASKUSI52
      XK0 = 0.003                                                       ASKUSI53
C                         WEAK ISOSPIN AND CHARGE OF OUTGOING FERMION   ASKUSI54
      T3  =-0.5                                                         ASKUSI55
      QI =  -1.                                                         ASKUSI56
      COL = 1.                                                          ASKUSI57
      ITYPE = 2                                                         ASKUSI58
      NQUA = 0                                                          ASKUSI59
C   DEFAULT SETUP IS MUON PAIR PRODUCTION                               ASKUSI60
C                                                                       ASKUSI61
      NABRIN=NAMIND('GDYM')                                             ASKUSI62
      ID=IW(NABRIN)                                                     ASKUSI63
      IF (ID.NE.0) THEN                                                 ASKUSI64
          ITYPE = NINT(RW(ID+1))                                        ASKUSI65
          EBEAM     = RW(ID+2)                                          ASKUSI66
          AMZ       = RW(ID+3)                                          ASKUSI67
          GAMM      = RW(ID+4)                                          ASKUSI68
          SW2       = RW(ID+5)                                          ASKUSI69
          IDEBU     = IW(ID+6)                                          ASKUSI70
          ID2       = RW(ID+7)                                          ASKUSI71
          TAU       = RW(ID+8)                                          ASKUSI72
          FINEXP    = RW(ID+9)                                          ASKUSI73
          POIDS     = RW(ID+10)                                         ASKUSI74
          XK0       = RW(ID+11)                                         ASKUSI75
          QCDFAC    = RW(ID+12)                                         ASKUSI76
          NQUA      = IW(ID+13)                                         ASKUSI77
          IFIRST    = IW(ID+14)                                         ASKUSI78
          NEVMA     = IW(ID+15)                                         ASKUSI79
      ENDIF                                                             ASKUSI80
C                                                                       ASKUSI81
C    update lund parameters from Data cards                             ASKUSI82
C                                                                       ASKUSI83
      CALL KXLUCO(LUPAR)                                                ASKUSI84
C                                                                       ASKUSI85
C    SEt up default masses for LUND : Z0                                ASKUSI86
C                                                                       ASKUSI87
      PMASL2(2) = AMZ                                                   ASKUSI88
      JSVER = IW(NAMIND('SVRT'))                                        ASKUSI89
      IF (JSVER.GT.0) THEN                                              ASKUSI90
         DO 3 K = 1,3                                                   ASKUSI91
 3       SVRT(K)      = RW(JSVER+K)                                     ASKUSI92
      ELSE                                                              ASKUSI93
         SVRT(1)    = 0.035                                             ASKUSI94
         SVRT(2)    = 0.0012                                            ASKUSI95
         SVRT(3)    = 1.28                                              ASKUSI96
      ENDIF                                                             ASKUSI97
C                                                                       ASKUSI98
      AEL = ULMASS(0,7)                                                 ASKUSI99
C         Leptons                                                       ASKUS100
          IF (ITYPE.LE.3) THEN                                          ASKUS101
              T3         = -0.5                                         ASKUS102
              QI         = -1.                                          ASKUS103
              COL        = 1.                                           ASKUS104
              AMU        = ULMASS(0,5+2*ITYPE)                          ASKUS105
          ELSEIF ( ITYPE.GT.10) THEN                                    ASKUS106
C    Quarks                                                             ASKUS107
              ITY=ITYPE-10+500                                          ASKUS108
              QI         = LUCHGE(ITY)/3.                               ASKUS109
              T3         = SIGN(0.5,QI)                                 ASKUS110
              COL        = 3.*QCDFAC                                    ASKUS111
              AMU        = PMASL2(ITY-400)                              ASKUS112
          ELSEIF ( ITYPE.EQ.10)  THEN                                   ASKUS113
C    Save initial seeds to start the real events                        ASKUS114
           CALL RDMOUT(ISEED)                                           ASKUS115
           I1 = ISEED(1)                                                ASKUS116
           I2 = ISEED(2)                                                ASKUS117
           I3 = ISEED(3)                                                ASKUS118
C    Quarks mixture                                                     ASKUS119
           DO 15  II=1,6                                                ASKUS120
 15         XSECT(II)=0.                                                ASKUS121
C   No DEBUG needed here!                                               ASKUS122
             IDEBU = 0                                                  ASKUS123
             DO 6 II = 1,NQUA                                           ASKUS124
                KTY = IFIRST+II-1                                       ASKUS125
                NEVT = 0                                                ASKUS126
                ITY=KTY-10+500                                          ASKUS127
                QI     = LUCHGE(ITY)/3.                                 ASKUS128
                T3     = SIGN(0.5,QI)                                   ASKUS129
                COL    = 3.*QCDFAC                                      ASKUS130
                AMU    = PMASL2(ITY-400)                                ASKUS131
                CALL DYINIT(KTY)                                        ASKUS132
C    COPY coupling constants for this type in an array                  ASKUS133
                CALL UCOPY(AEL,WEAKC(1,KTY-10),11)                      ASKUS134
                DO 66 KEV=1,NEVMA                                       ASKUS135
                   CALL DYMUS(WEI)                                      ASKUS136
  66            CONTINUE                                                ASKUS137
                CALL FINISH(KTY,NEVMA)                                  ASKUS138
C   Store cross-sections for each process                               ASKUS139
                XSECT(KTY-10) = SIGTOT                                  ASKUS140
  6          CONTINUE                                                   ASKUS141
C   Store total cross-section                                           ASKUS142
             XTOT = XSECT(1)                                            ASKUS143
             DO 75 II = 2,NQUA                                          ASKUS144
  75         XTOT = XTOT+XSECT(II)                                      ASKUS145
             KAUX = IFIRST-10                                           ASKUS146
             DO 7 II=2,NQUA                                             ASKUS147
  7          XSECT(KAUX+II-1) = XSECT(KAUX+II-1-1)+XSECT(KAUX+II-1)     ASKUS148
C     Normalize                                                         ASKUS149
             DO 8 II = 1,NQUA                                           ASKUS150
  8          XSECT(KAUX+II-1) = XSECT(KAUX+II-1)/XSECT(KAUX+NQUA-1)     ASKUS151
             WRITE(IW(6),100)   XSECT                                   ASKUS152
 100         FORMAT('       INTEGRATED CROSS SECTIONS FOR QUARKS ',     ASKUS153
     $              /,9X,6F10.4)                                        ASKUS154
C    Restore initial seed                                               ASKUS155
             CALL RMARIN(I1,I2,I3)                                      ASKUS156
          ENDIF                                                         ASKUS157
      IF (ITYPE.GT.2) THEN                                              ASKUS158
         CALL LUTAUD(IFL)                                               ASKUS159
         IF (IFL.NE.0) CALL EXIT                                        ASKUS160
      ENDIF                                                             ASKUS161
      CALL KXLUPA (IPART,IKLIN)                                         ASKUS162
      IF (IPART.LE.0 .OR. IKLIN.LE.0) THEN                              ASKUS163
         WRITE (IW(6),'(1X,''error in PART or KLIN bank - STOP - ''     ASKUS164
     +                 ,2I3)') IPART,IKLIN                              ASKUS165
         GOTO 20                                                        ASKUS166
      ENDIF                                                             ASKUS167
C                                                                       ASKUS168
C   Inhibit decays                                                      ASKUS169
C                                                                       ASKUS170
      MXDEC=KNODEC(NODEC,LPDEC)                                         ASKUS171
      MXDEC=MIN(MXDEC,LPDEC)                                            ASKUS172
      IF (MXDEC.GT.0) THEN                                              ASKUS173
         DO 10 I=1,MXDEC                                                ASKUS174
            IF (NODEC(I).GT.0) IDBLU3(NODEC(I))=0                       ASKUS175
   10    CONTINUE                                                       ASKUS176
      ENDIF                                                             ASKUS177
C   Restore DEBUG if any                                                ASKUS178
        IDEBU     = IW(ID+6)                                            ASKUS179
C                                                                       ASKUS180
C       INITIALIZE DYMU3                                                ASKUS181
C                                                                       ASKUS182
        CALL DYINIT(ITYPE)                                              ASKUS183
C                                                                       ASKUS184
C   dump the generator parameters for this run in a bank                ASKUS185
C assume all parameters are real and stored as a single row             ASKUS186
      TABL(1) = FLOAT(ITYPE)                                            ASKUS187
      TABL(2) = EBEAM                                                   ASKUS188
      TABL(3) = AMZ                                                     ASKUS189
      TABL(4) = GAMM                                                    ASKUS190
      TABL(5) = SW2                                                     ASKUS191
      TABL(6) = FLOAT(ID2)                                              ASKUS192
      TABL(7) = FLOAT(IEXPO)                                            ASKUS193
      TABL(8) = FINEXP                                                  ASKUS194
      TABL(9) = POIDS                                                   ASKUS195
      TABL(10) = XK0                                                    ASKUS196
      TABL(11) = QCDFAC                                                 ASKUS197
      TABL(12) = FLOAT(NQUA)                                            ASKUS198
      TABL(13) = FLOAT(IFIRST)                                          ASKUS199
      TABL(14) = FLOAT(NEVMA)                                           ASKUS200
      TABL(15) = TAU                                                    ASKUS201
      TABL(16) = SVRT(1)                                                ASKUS202
      TABL(17) = SVRT(2)                                                ASKUS203
      TABL(18) = SVRT(3)                                                ASKUS204
      NWB = 18                                                          ASKUS205
      IND = ALTABL('KPAR',NWB,1,TABL,'2I,(F)','C')                      ASKUS206
C  Fill RLEP bank                                                       ASKUS207
      IEBEAM = NINT(EBEAM *1000  )                                      ASKUS208
      JRLEP = ALRLEP(IEBEAM,'    ',0,0,0)                               ASKUS209
C                                                                       ASKUS210
C  Print PART and KLIN banks                                            ASKUS211
C                                                                       ASKUS212
      IF (IDEBU.EQ.2) THEN                                              ASKUS213
         CALL LULIST(3)                                                 ASKUS214
         CALL PRPART                                                    ASKUS215
      ENDIF                                                             ASKUS216
C                                                                       ASKUS217
      CALL PRTABL('KPAR',0)                                             ASKUS218
      CALL PRTABL('RLEP',0)                                             ASKUS219
   20 RETURN                                                            ASKUS220
      END                                                               ASKUS221
      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)                 ASKUSE 2
C--------------------------------------------------------------------   ASKUSE 3
C      B. BLOCH-DEVAUX JUNE 1989 IMPLEMENTATION OF DYMU3                ASKUSE 4
C          DYMUS GENERATES ONE EVENT                                    ASKUSE 5
C     structure : subroutine                                            ASKUSE 6
C                                                                       ASKUSE 7
C     input     : none                                                  ASKUSE 8
C                                                                       ASKUSE 9
C     output    : 6 arguments                                           ASKUSE10
C          IDPR   : process identification if meanigful                 ASKUSE11
C          ISTA   : status flag ( 0 means ok), use it to reject         ASKUSE12
C                   unwanted events                                     ASKUSE13
C          NTRK   : number of tracks generated and kept                 ASKUSE14
C                  (i.e. # KINE banks  written)                         ASKUSE15
C          NVRT   : number of vertices generated                        ASKUSE16
C                   (i.e. # VERT banks written)                         ASKUSE17
C          ECMS   : center of mass energy for the event (may be         ASKUSE18
C                   different from nominal cms energy)                  ASKUSE19
C          WEIT   : event weight ( not 1 if a weighting method is used) ASKUSE20
C--------------------------------------------------------------------   ASKUSE21
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      PARAMETER (L1MST=40, L1PAR=80)                                    LUNDCOM2
      PARAMETER (L2KTYP=120, L2PMAS=120, L2PWID=60, L2KFR=80, L2CFR=40) LUNDCOM3
      PARAMETER (L3DPAR=20, L3IDB=120, L3CBR=400, L3KDP=1600)           LUNDCOM4
      PARAMETER (LEMSTE=40, LEPARE=80)                                  LUNDCOM5
      PARAMETER (LJNPAR=2000)                                           LUNDCOM6
      COMMON /LUDAT1/   MSTLU1(L1MST),PARLU1(L1PAR)                     LUNDCOM7
      COMMON /LUDAT2/   KTYPL2(L2KTYP),PMASL2(L2PMAS),PWIDL2(L2PWID)    LUNDCOM8
     &                , KFRLU2(L2KFR),CFRLU2(L2CFR)                     LUNDCOM9
      COMMON /LUDAT3/   DPARL3(L3DPAR),IDBLU3(L3IDB),CBRLU3(L3CBR)      LUNDCO10
     &                , KDPLU3(L3KDP)                                   LUNDCO11
      COMMON /LUDATE/   MSTELE(LEMSTE),PARELE(LEPARE)                   LUNDCO12
      COMMON /LUJETS/   NPARLU,KODELU(LJNPAR,2),PARTLU(LJNPAR,5)        LUNDCO13
C                                                                       LUNDCO14
      COMMON/ZDUMP / IDEBU , NEVT                                       LUSTAT 2
      COMMON/LUSTAT/ ICOULU(10),SVRT(3)                                 LUSTAT 3
      COMMON/RESULT/ SIGBOR,SIGTOT,ERRSIG,ASYTOT,ERRASY                 LUSTAT 4
      DIMENSION SVERT(3),VERT(4),TABL(3)                                ASKUSE25
      NEVT = NEVT+1                                                     ASKUSE26
      IF ( NEVT.GT.5)IDEBU = 0                                          ASKUSE27
C                                                                       ASKUSE28
C  Generate vertex postion                                              ASKUSE29
C                                                                       ASKUSE30
      CALL RANNOR(RX,RY)                                                ASKUSE31
      CALL RANNOR(RZ,DUM)                                               ASKUSE32
      VERT(1)=RX*SVRT(1)                                                ASKUSE33
      VERT(2)=RY*SVRT(2)                                                ASKUSE34
      VERT(3)=RZ*SVRT(3)                                                ASKUSE35
      VERT(4)=0.                                                        ASKUSE36
C                                                                       ASKUSE37
C                                                                       ASKUSE38
      CALL DYGENE(IDPR,ISTAT,ECMS,WEIT)                                 ASKUSE39
      CALL KXLUAL(VERT,IST,NVRT,NTRK)                                   ASKUSE40
      IF (IST.EQ.0 .AND. ISTAT.EQ.0) THEN                               ASKUSE41
         ICOULU(10) = ICOULU(10)+1                                      ASKUSE42
      ELSEIF (IST.GT.0) THEN                                            ASKUSE43
         ICOULU(1) = ICOULU(1) +1                                       ASKUSE44
         ICOULU(9) = ICOULU(9) +1                                       ASKUSE45
      ELSEIF ( IST.LT.0) THEN                                           ASKUSE46
         ICOULU(-IST) = ICOULU(-IST) +1                                 ASKUSE47
         ICOULU(9) = ICOULU(9) +1                                       ASKUSE48
      ELSEIF ( ISTAT.GT.0) THEN                                         ASKUSE49
         ICOULU(6) = ICOULU(6) +1                                       ASKUSE50
         ICOULU(9) = ICOULU(9) +1                                       ASKUSE51
      ENDIF                                                             ASKUSE52
C                                                                       ASKUSE53
C  You can use the status word to decide not to keep the event          ASKUSE54
C  as you may generate only part of the particles spectra               ASKUSE55
C                                                                       ASKUSE56
      ISTA = IST+ISTAT*1000                                             ASKUSE57
      RETURN                                                            ASKUSE58
      END                                                               ASKUSE59
         SUBROUTINE DYMULU (ITYP2,WEI)                                  DYMULU 2
C-----------------------------------------------------------------------DYMULU 3
C   B.Bloch-Devaux june 1989                                            DYMULU 4
C                                                                       DYMULU 5
C                LUND SHOWER DYMUS   INTERFACE                          DYMULU 6
C                                                                       DYMULU 7
C        SET UP THE LUND COMMON                                         DYMULU 8
C          1 E-         }                                               DYMULU 9
C          2 E+         }                                               DYMULU10
C          3 INITIAL STATE PHOTON FROM positron if any  (GAP)           DYMULU11
C          4 INITIAL STATE PHOTON FROM electron if any  (GAM)           DYMULU12
C          5 Z ( ARBITRARY REALLY IS THE updated  INITIAL system )      DYMULU13
C          6 GAMMA ( FINAL STATE if any)        ( GAF)                  DYMULU14
C          7 ANTIFERMION   }                    ( PFP)                  DYMULU15
C          8  FERMION       }   OF FLAVOR ITYP2  ( PFM)                 DYMULU16
C      ( IN CASE OF QUARKS A SHOWER LINE INBETWEEN)                     DYMULU17
C     ITYP:      1=E, 2=MU, 3=TAU, 11=U, 12=D, 13=S, 14=C, 15=B, 16=T   DYMULU18
C     WEI :      Event weight                                           DYMULU19
C                                                                       DYMULU20
C-----------------------------------------------------------------------DYMULU21
      COMMON / VECLAB / PFP(4),PFM(4),GAP(4),GAM(4),GAF(4)              VECLAB 2
      COMMON /CONST/ ALFA,PI,ALFA1                                      DYCONS 2
      COMMON /RUNPAR/ SOLD,ID2,ID3,FINEXP,POIDS,INTERF,XK0              DYCONS 3
      COMMON /WEAK/ AEL,AMU,AMZ,GAMM,SW2,CA2,CV2,CA2CV2,COL,T3,QI       DYCONS 4
      COMMON /BEAM/ S0,EBEAM                                            DYCONS 5
      COMMON /TAU / TAU,CPTAU,HEL,PITAU(4)                              DYCONS 6
      COMMON/WEAKQ/WEAKC(11,6),XSECT(6),XTOT                            DYCONS 7
         DIMENSION Z(4)                                                 DYMULU24
      PARAMETER (L1MST=40, L1PAR=80)                                    LUNDCOM2
      PARAMETER (L2KTYP=120, L2PMAS=120, L2PWID=60, L2KFR=80, L2CFR=40) LUNDCOM3
      PARAMETER (L3DPAR=20, L3IDB=120, L3CBR=400, L3KDP=1600)           LUNDCOM4
      PARAMETER (LEMSTE=40, LEPARE=80)                                  LUNDCOM5
      PARAMETER (LJNPAR=2000)                                           LUNDCOM6
      COMMON /LUDAT1/   MSTLU1(L1MST),PARLU1(L1PAR)                     LUNDCOM7
      COMMON /LUDAT2/   KTYPL2(L2KTYP),PMASL2(L2PMAS),PWIDL2(L2PWID)    LUNDCOM8
     &                , KFRLU2(L2KFR),CFRLU2(L2CFR)                     LUNDCOM9
      COMMON /LUDAT3/   DPARL3(L3DPAR),IDBLU3(L3IDB),CBRLU3(L3CBR)      LUNDCO10
     &                , KDPLU3(L3KDP)                                   LUNDCO11
      COMMON /LUDATE/   MSTELE(LEMSTE),PARELE(LEPARE)                   LUNDCO12
      COMMON /LUJETS/   NPARLU,KODELU(LJNPAR,2),PARTLU(LJNPAR,5)        LUNDCO13
C                                                                       LUNDCO14
         DATA NEV/0/,ITY/7/,ITYG /1/                                    DYMULU26
C                                                                       DYMULU27
C     CLEAN-UP                                                          DYMULU28
C                                                                       DYMULU29
         DO 5 I=1,20                                                    DYMULU30
         DO 5 J=1,2                                                     DYMULU31
         KODELU(I,J)=0                                                  DYMULU32
    5 CONTINUE                                                          DYMULU33
         DO 6 I=1,20                                                    DYMULU34
         DO 6 J=1,5                                                     DYMULU35
         PARTLU(I,J)=0.                                                 DYMULU36
    6 CONTINUE                                                          DYMULU37
C                                                                       DYMULU38
C  First electron beams                                                 DYMULU39
C                                                                       DYMULU40
        KODELU(1,1)=40000                                               DYMULU41
        KODELU(1,2)=ITY                                                 DYMULU42
        KODELU(2,1)=40000                                               DYMULU43
        KODELU(2,2)=-ITY                                                DYMULU44
        PARTLU(1,5)=ULMASS(0,ITY)                                       DYMULU45
        PARTLU(2,5)=ULMASS(0,ITY)                                       DYMULU46
        PARTLU(1,3) = -EBEAM                                            DYMULU47
        PARTLU(2,3) =  EBEAM                                            DYMULU48
        PARTLU(1,4) =  EBEAM                                            DYMULU49
        PARTLU(2,4) =  EBEAM                                            DYMULU50
        NPARLU = 2                                                      DYMULU51
C                                                                       DYMULU52
C        THEN  Z AND PHOTONS                                            DYMULU53
C                                                                       DYMULU54
      IF (GAP(4).GT.1.E-06) THEN                                        DYMULU55
         NPARLU = NPARLU+1                                              DYMULU56
        KODELU(NPARLU,1)=2                                              DYMULU57
        KODELU(NPARLU,2)=ITYG                                           DYMULU58
        PARTLU(NPARLU,5)=ULMASS(0,ITYG)                                 DYMULU59
      DO 10 I=1,4                                                       DYMULU60
        PARTLU(NPARLU,I)=GAP(I)                                         DYMULU61
   10 CONTINUE                                                          DYMULU62
      ENDIF                                                             DYMULU63
      IF (GAM(4).GT.1.E-06) THEN                                        DYMULU64
         NPARLU = NPARLU+1                                              DYMULU65
        KODELU(NPARLU,1)=1                                              DYMULU66
        KODELU(NPARLU,2)=ITYG                                           DYMULU67
        PARTLU(NPARLU,5)=ULMASS(0,ITYG)                                 DYMULU68
      DO 20 I=1,4                                                       DYMULU69
        PARTLU(NPARLU,I)=GAM(I)                                         DYMULU70
   20 CONTINUE                                                          DYMULU71
      ENDIF                                                             DYMULU72
         NPARLU = NPARLU+1                                              DYMULU73
        KODELU(NPARLU,1)=20000                                          DYMULU74
        KODELU(NPARLU,2)=2                                              DYMULU75
      DO 30 I=1,4                                                       DYMULU76
        PARTLU(NPARLU,I)=-(GAP(I)+GAM(I))                               DYMULU77
   30 CONTINUE                                                          DYMULU78
        PARTLU(NPARLU,4)=PARTLU(NPARLU,4)+2.*EBEAM                      DYMULU79
        Z2=ABS(PARTLU(NPARLU,4)**2-                                     DYMULU80
     +     PARTLU(NPARLU,1)**2-PARTLU(NPARLU,2)**2-PARTLU(NPARLU,3)**2) DYMULU81
        PARTLU(NPARLU,5)=SQRT(Z2)                                       DYMULU82
        IZLU = NPARLU                                                   DYMULU83
C                                                                       DYMULU84
C  IS THERE A FINAL STATE PHOTON?                                       DYMULU85
C                                                                       DYMULU86
      IF (GAF(4).GT.1.E-06) THEN                                        DYMULU87
         NPARLU = NPARLU+1                                              DYMULU88
        KODELU(NPARLU,1)=0                                              DYMULU89
        KODELU(NPARLU,2)=ITYG                                           DYMULU90
        PARTLU(NPARLU,5)=ULMASS(0,ITYG)                                 DYMULU91
      DO 15 I=1,4                                                       DYMULU92
        PARTLU(NPARLU,I)=GAF(I)                                         DYMULU93
   15 CONTINUE                                                          DYMULU94
      ENDIF                                                             DYMULU95
      IF(ITYP2.LT.10) THEN                                              DYMULU96
C                                                                       DYMULU97
C        THESE ARE LEPTONS  NO SHOWER  LINES NEEDED                     DYMULU98
C          THIS REPRODUCES THE LUND6.3 LEPTON FLAVOR CODE               DYMULU99
C                                                                       DYMUL100
         KMASS=5+2*ITYP2                                                DYMUL101
         KODELU(NPARLU+1,1)= IZLU                                       DYMUL102
         KODELU(NPARLU+2,1)= IZLU                                       DYMUL103
         KODELU(NPARLU+1,2)=-KMASS                                      DYMUL104
         KODELU(NPARLU+2,2)=KMASS                                       DYMUL105
         PARTLU(NPARLU+1,5)=ULMASS(0,KMASS)                             DYMUL106
         PARTLU(NPARLU+2,5)=ULMASS(0,KMASS)                             DYMUL107
         DO 40 I=1,4                                                    DYMUL108
           PARTLU(NPARLU+1,I)=PFP(I)                                    DYMUL109
           PARTLU(NPARLU+2,I)=PFM(I)                                    DYMUL110
   40    CONTINUE                                                       DYMUL111
         NPARLU=NPARLU+2                                                DYMUL112
         CALL LUEXEC                                                    DYMUL113
      ELSE                                                              DYMUL114
C                                                                       DYMUL115
C     QUARKS                                                            DYMUL116
C                                                                       DYMUL117
                                                                        DYMUL118
         KMASS=500+ITYP2-10                                             DYMUL119
         KODELU(NPARLU+1,1)=10000+IZLU                                  DYMUL120
         KODELU(NPARLU+3,1)= IZLU                                       DYMUL121
         KODELU(NPARLU+1,2)=-KMASS                                      DYMUL122
         KODELU(NPARLU+3,2)=KMASS                                       DYMUL123
         PARTLU(NPARLU+1,5)=ULMASS(0,KMASS)                             DYMUL124
         PARTLU(NPARLU+3,5)=ULMASS(0,KMASS)                             DYMUL125
         DO 140 I=1,4                                                   DYMUL126
           PARTLU(NPARLU+1,I)=PFP(I)                                    DYMUL127
           PARTLU(NPARLU+3,I)=PFM(I)                                    DYMUL128
  140    CONTINUE                                                       DYMUL129
C                                                                       DYMUL130
C   FILL THE COLOUR LINES                                               DYMUL131
C                                                                       DYMUL132
         KODELU(NPARLU+2,1) = 70000 + NPARLU+1                          DYMUL133
         KODELU(NPARLU+4,1) = 70000 + NPARLU+3                          DYMUL134
         KODELU(NPARLU+2,2) =  1000 + NPARLU+1                          DYMUL135
         KODELU(NPARLU+4,2) =  1000 + NPARLU+3                          DYMUL136
         PARTLU(NPARLU+2,1) =  NPARLU + 3                               DYMUL137
         PARTLU(NPARLU+2,2) =  NPARLU + 3                               DYMUL138
         PARTLU(NPARLU+4,1) =  NPARLU + 1                               DYMUL139
         PARTLU(NPARLU+4,2) =  NPARLU + 1                               DYMUL140
         N = NPARLU                                                     DYMUL141
         NPARLU = NPARLU+4                                              DYMUL142
C                                                                       DYMUL143
C         CALCULATE THE QQBAR MASS                                      DYMUL144
C                                                                       DYMUL145
         DO 150 I=1,4                                                   DYMUL146
           Z(I)=PFP(I)+PFM(I)                                           DYMUL147
  150    CONTINUE                                                       DYMUL148
         QMAX= SQRT( Z(4)**2- Z(3)**2-Z(2)**2-Z(1)**2 )                 DYMUL149
C                                                                       DYMUL150
C         GENERATE PARTON SHOWER                                        DYMUL151
C                                                                       DYMUL152
         IF( NEV.LT.10)CALL LULIST(11)                                  DYMUL153
         CALL LUSHOW(N+1,N+3,QMAX)                                      DYMUL154
         CALL LUEXEC                                                    DYMUL155
C                                                                       DYMUL156
C  LINKS THE QUARK BACK TO THE Z                                        DYMUL157
C                                                                       DYMUL158
         DO 160 IP=N+1,NPARLU                                           DYMUL159
         IF (ABS(KODELU(IP,2)).LT.500) GO TO 197                        DYMUL160
         KODELU(IP,1) = 10000*(KODELU(IP,1)/10000)+IZLU                 DYMUL161
  160    CONTINUE                                                       DYMUL162
  197    CONTINUE                                                       DYMUL163
C                                                                       DYMUL164
      ENDIF                                                             DYMUL165
C                                                                       DYMUL166
C   Now check if the initial electron was along positive Z axis.        DYMUL167
C   If not reverse Px and Pz components for all particles               DYMUL168
C                                                                       DYMUL169
       IF ( PARTLU(1,3).LT.0.) THEN                                     DYMUL170
          DO 201 IP=1,NPARLU                                            DYMUL171
          DO 202 JJ=1,3,2                                               DYMUL172
  202       PARTLU(IP,JJ) = -PARTLU(IP,JJ)                              DYMUL173
  201     CONTINUE                                                      DYMUL174
       ENDIF                                                            DYMUL175
      NEV=NEV+1                                                         DYMUL176
      IF(NEV.LE.10)CALL LULIST(11)                                      DYMUL177
      IF ( GAP(4).GT.1.E-06) THEN                                       DYMUL178
C    There is a photon                                                  DYMUL179
      EE= GAP(3)/SQRT(GAP(1)**2+GAP(2)**2+GAP(3)**2)                    DYMUL180
      IF (EE.GE.1.) EE=0.999                                            DYMUL181
      IF (EE.LE.-1.) EE=-0.999                                          DYMUL182
      CALL HFILL(10003,GAP(4),EE ,WEI)                                  DYMUL183
      ENDIF                                                             DYMUL184
      IF ( GAM(4).GT.1.E-06) THEN                                       DYMUL185
      EE= GAM(3)/SQRT(GAM(1)**2+GAM(2)**2+GAM(3)**2)                    DYMUL186
      IF (EE.GE.1.) EE=0.999                                            DYMUL187
      IF (EE.LE.-1.) EE=-0.999                                          DYMUL188
      CALL HFILL(10003,GAM(4),EE ,WEI)                                  DYMUL189
      ENDIF                                                             DYMUL190
      EE = PFP(4)                                                       DYMUL191
      CALL HFILL(10002,EE,DUM,WEI)                                      DYMUL192
      EE = PFM(4)                                                       DYMUL193
      CALL HFILL(10001,EE,DUM,WEI)                                      DYMUL194
      EE= PFP(3)/SQRT(PFP(1)**2+PFP(2)**2+PFP(3)**2)                    DYMUL195
      IF (EE.GE.1.) EE=0.999                                            DYMUL196
      IF (EE.LE.-1.) EE=-0.999                                          DYMUL197
      CALL HFILL(10005,EE,DUM,WEI)                                      DYMUL198
      EE= PFM(3)/SQRT(PFM(1)**2+PFM(2)**2+PFM(3)**2)                    DYMUL199
      IF (EE.GE.1.) EE=0.999                                            DYMUL200
      IF (EE.LE.-1.) EE=-0.999                                          DYMUL201
      CALL HFILL(10004,EE,DUM,WEI)                                      DYMUL202
      EE=FLOAT(NPARLU)-3.                                               DYMUL203
      CALL HFILL(10006,EE,DUM,WEI)                                      DYMUL204
      CALL HFILL(10007,WEI,DUM,1.)                                      DYMUL205
      IF ( GAF(4).GT.1.E-06) THEN                                       DYMUL206
      EE= GAF(3)/SQRT(GAF(1)**2+GAF(2)**2+GAF(3)**2)                    DYMUL207
      IF (EE.GE.1.) EE=0.999                                            DYMUL208
      IF (EE.LE.-1.) EE=-0.999                                          DYMUL209
      CALL HFILL(10008,GAF(4),EE ,WEI)                                  DYMUL210
      ENDIF                                                             DYMUL211
      EE = FLOAT(ITYP2)                                                 DYMUL212
      CALL HFILL(10009,EE,DUM,WEI)                                      DYMUL213
      RETURN                                                            DYMUL214
      END                                                               DYMUL215
          SUBROUTINE DYINIT(NTYP)                                       DYINIT 2
C-----------------------------------------------------------------------DYINIT 3
C    B.Bloch -Devaux June 1989                                          DYINIT 4
C         ORIGINAL VERSION OF DYMU3 AS PROVIDED BY J.E.Campagne         DYINIT 5
C                       June 1989                                       DYINIT 6
C  This is a subset of original subroutine INIRUN to compute secondary  DYINIT 7
C  quantities according to requested final state.                       DYINIT 8
C-----------------------------------------------------------------------DYINIT 9
      COMMON /CONST/ ALFA,PI,ALFA1                                      DYCONS 2
      COMMON /RUNPAR/ SOLD,ID2,ID3,FINEXP,POIDS,INTERF,XK0              DYCONS 3
      COMMON /WEAK/ AEL,AMU,AMZ,GAMM,SW2,CA2,CV2,CA2CV2,COL,T3,QI       DYCONS 4
      COMMON /BEAM/ S0,EBEAM                                            DYCONS 5
      COMMON /TAU / TAU,CPTAU,HEL,PITAU(4)                              DYCONS 6
      COMMON/WEAKQ/WEAKC(11,6),XSECT(6),XTOT                            DYCONS 7
      COMMON/ZDUMP / IDEBU , NEVT                                       LUSTAT 2
      COMMON/LUSTAT/ ICOULU(10),SVRT(3)                                 LUSTAT 3
      COMMON/RESULT/ SIGBOR,SIGTOT,ERRSIG,ASYTOT,ERRASY                 LUSTAT 4
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      COMMON / VECLAB / PFP(4),PFM(4),GAP(4),GAM(4),GAF(4)              VECLAB 2
      COMMON /COUNTS/ SIG,SIG2,SECFWD,SECBKW,SCFWD2,SCBKW2              DYINIT14
      COMMON /EVTS/ NEVT1,NEVT2,NFWD,NBKW                               DYINIT15
      REAL*8 SIG,SIG2,SECFWD,SECBKW,SCFWD2,SCBKW2                       DYINIT16
C                                                                       DYINIT17
      DIMENSION ISEED(3)                                                DYINIT18
      LOGICAL FIRST                                                     DYINIT19
      DATA FIRST/.TRUE./                                                DYINIT20
      IOUT = IW(6)                                                      DYINIT21
      IF(FIRST)THEN                                                     DYINIT22
        FIRST = .FALSE.                                                 DYINIT23
        WRITE(IOUT,*)'*****************************************'        DYINIT24
        WRITE(IOUT,*)'*         WELCOME    TO    DYMU3        *'        DYINIT25
        WRITE(IOUT,*)'*                                       *'        DYINIT26
        WRITE(IOUT,*)'*         AUTHORS: J.E.CAMPAGNE         *'        DYINIT27
        WRITE(IOUT,*)'*                  R.ZITOUN             *'        DYINIT28
        WRITE(IOUT,*)'*                                       *'        DYINIT29
        WRITE(IOUT,*)'*         31 JULY 89 RELEASE            *'        DYINIT30
        WRITE(IOUT,*)'*                                       *'        DYINIT31
        WRITE(IOUT,*)'*****************************************'        DYINIT32
        WRITE(IOUT,*)' '                                                DYINIT33
*----                                                                   DYINIT34
      CALL HBOOK1(10001,'OUTGOING FERMION ENERGY$',50,0.,50.,0.)        DYINIT35
      CALL HBOOK1(10002,'OUTGOING ANTIFERMION ENERGY$',50,0.,50.,0.)    DYINIT36
      CALL HBOOK2(10003,'INITIAL PHOTON ENERGY VS COST',50,0.,15.,      DYINIT37
     $   50,-1.,1.,0.)                                                  DYINIT38
      CALL HBOOK1(10004,'POLAR ANGLE FERMION$',50,-1.,1.,0.)            DYINIT39
      CALL HBOOK1(10005,'POLAR ANGLE ANTIFERMION$',50,-1.,1.,0.)        DYINIT40
      CALL HBOOK1(10006,'FINAL MULTIPLICITY GENERATED$',60,2.,122.,0.)  DYINIT41
      CALL HBOOK1(10007,'WEIGHT DISTRIBUTION  ',50,0.,2.,0.)            DYINIT42
      CALL HBOOK2(10008,'FINAL   PHOTON ENERGY VS COST',50,0.,20.,      DYINIT43
     $   50,-1.,1.,0.)                                                  DYINIT44
      CALL HBPRO(0,0.)                                                  DYINIT45
      CALL HBOOK1(10009,'EVENT TYPE DISTRIBUTION  ',20,1.,21.,0.)       DYINIT46
C                                                                       DYINIT47
      ENDIF                                                             DYINIT48
C                                                                       DYINIT49
      ITYP = NTYP                                                       DYINIT50
      NTYPO = NTYP                                                      DYINIT51
C                                                                       DYINIT52
       WRITE(IOUT,1533) ITYP                                            DYINIT53
 1533 FORMAT( 30X,'**************************************************', DYINIT54
     &/,30X,'* D Y M U 3     A D A P T E D   T O   K I N G A L  : '/    DYINIT55
     X ,30X,'*   GENERATING FERMION TYPE:',I3,/,                        DYINIT56
     &30X,'*****************************************************')      DYINIT57
*---- BEAM ENERGY                                                       DYINIT58
*                                                                       DYINIT59
*                                                                       DYINIT60
      ECMS = 2.*EBEAM                                                   DYINIT61
      S0 = 4.*EBEAM**2                                                  DYINIT62
*                                                                       DYINIT63
*----  COUPLING CONSTANTS                                               DYINIT64
*                                                                       DYINIT65
      CV    = (-1.+4.*SW2)/4./SQRT(SW2*(1.-SW2))                        DYINIT66
      CA    = -1./4./SQRT(SW2*(1.-SW2))                                 DYINIT67
      CVPRI = (-2*T3/QI+4.*SW2)/4./SQRT(SW2*(1.-SW2))                   DYINIT68
      CAPRI = -T3/QI/2./SQRT(SW2*(1.-SW2))                              DYINIT69
      CV2 = CVPRI*CV                                                    DYINIT70
      CA2 = CAPRI*CA                                                    DYINIT71
      CA2CV2 = ( CV**2+CA**2 )*( CVPRI**2+CAPRI**2 )                    DYINIT72
      CALL DYMUSI                                                       DYINIT73
*                                                                       DYINIT74
*---- CONST1                                                            DYINIT75
*                                                                       DYINIT76
      ALFA  = 1./137.036                                                DYINIT77
      PI    = 3.14159265                                                DYINIT78
      ALFA1 = ALFA/PI                                                   DYINIT79
*                                                                       DYINIT80
*----                                                                   DYINIT81
*                                                                       DYINIT82
      WRITE(IOUT,*)'*************************************************'  DYINIT83
      WRITE(IOUT,*)'*     RUN PARAMETERS FOR RUN',ITYP                  DYINIT84
      WRITE(IOUT,*)'******                                 **********'  DYINIT85
      WRITE(IOUT,1000) AMZ,GAMM,SW2                                     DYINIT86
      WRITE(IOUT,1003) ECMS,EBEAM                                       DYINIT87
 1000   FORMAT('     Z MASS =',F8.3,' GEV ,      Z WIDTH =',F6.3,       DYINIT88
     &         ' GEV ,  SIN2 TETA =',F7.4)                              DYINIT89
 1003   FORMAT(' CMS ENERGY =',F8.3,' GEV ,  BEAM ENERGY =',F8.3)       DYINIT90
*                                                                       DYINIT91
      IF(POIDS.EQ.1)THEN                                                DYINIT92
        WRITE(IOUT,*)'UNWEIGHTED EVENTS'                                DYINIT93
      ELSE                                                              DYINIT94
        WRITE(IOUT,*)'WEIGHTED EVENTS'                                  DYINIT95
      ENDIF                                                             DYINIT96
      WRITE(IOUT,*)'INITIAL STATE EXPONENTIATION'                       DYINIT97
      IF(FINEXP.EQ.1)THEN                                               DYINIT98
        WRITE(IOUT,*)'FINAL STATE EXPONENTIATION'                       DYINIT99
      ELSE IF(FINEXP.EQ.0) THEN                                         DYINI100
        WRITE(IOUT,*)'NO FINAL STATE EXPONENTIATION.'                   DYINI101
      ELSE IF(FINEXP.EQ.-1) THEN                                        DYINI102
        WRITE(IOUT,*)'NO FINAL STATE PHOTON'                            DYINI103
      ENDIF                                                             DYINI104
      IF(ID2.EQ.1)THEN                                                  DYINI105
        WRITE(IOUT,*)'DII IN D(X)'                                      DYINI106
      ELSE                                                              DYINI107
        WRITE(IOUT,*)'DII NOT IN D(X)'                                  DYINI108
      ENDIF                                                             DYINI109
      IF(ID3.EQ.1)THEN                                                  DYINI110
        WRITE(IOUT,*)'DIII IN D(X)'                                     DYINI111
      ELSE                                                              DYINI112
        WRITE(IOUT,*)'DIII NOT IN D(X)'                                 DYINI113
      ENDIF                                                             DYINI114
      IF(INTERF.EQ.0)THEN                                               DYINI115
        WRITE(IOUT,*)'NO INTERFERENCE'                                  DYINI116
      ELSE                                                              DYINI117
        WRITE(IOUT,*)'INTERFERENCE WITH K0 =',XK0                       DYINI118
      ENDIF                                                             DYINI119
*                                                                       DYINI120
      CALL RDMOUT(ISEED)                                                DYINI121
      WRITE(IOUT,*)'INITIAL SEEDS ARE',ISEED                            DYINI122
      WRITE(IOUT,*)'*************************************************'  DYINI123
*                                                                       DYINI124
*---- SET TO ZERO                                                       DYINI125
*                                                                       DYINI126
      SIG    = 0.                                                       DYINI127
      SIG2   = 0.                                                       DYINI128
      SECFWD = 0.                                                       DYINI129
      SECBKW = 0.                                                       DYINI130
      SCFWD2 = 0.                                                       DYINI131
      SCBKW2 = 0.                                                       DYINI132
      NEVT1  = 0                                                        DYINI133
      NEVT2  = 0                                                        DYINI134
      NFWD   = 0                                                        DYINI135
      NBKW   = 0                                                        DYINI136
C                                                                       DYINI137
      RETURN                                                            DYINI138
C ----------------------------------------------------------------------DYINI139
C                                                                       DYINI140
      ENTRY DYGENE(IDPR,ISTAT,ECMM,WEIT)                                DYINI141
C                                                                       DYINI142
      IF (NTYPO.EQ.10 ) THEN                                            DYINI143
C   Decide which flavor to generate                                     DYINI144
        XX = RNDM(DUM)                                                  DYINI145
        DO 30 I=1,6                                                     DYINI146
          IF (XX.LT.XSECT(I)) GO TO 40                                  DYINI147
  30    CONTINUE                                                        DYINI148
  40    ITYP = I                                                        DYINI149
C   Copy corresponding coupling constants                               DYINI150
        CALL UCOPY(WEAKC(1,I),AEL,11)                                   DYINI151
        CALL DYMUSI                                                     DYINI152
        ITYP= ITYP+NTYPO                                                DYINI153
      ENDIF                                                             DYINI154
C                                                                       DYINI155
      CALL DYMUS (WE)                                                   DYINI156
      IDPR=ITYP*1000                                                    DYINI157
      ECMM=ECMS                                                         DYINI158
      WEIT=WE                                                           DYINI159
      ISTAT=0                                                           DYINI160
C                                                                       DYINI161
C                                LUND INTERFACE                         DYINI162
      CALL DYMULU(ITYP,WEIT)                                            DYINI163
C                                                                       DYINI164
  100 CONTINUE                                                          DYINI165
      RETURN                                                            DYINI166
C                                                                       DYINI167
C-----------------------------------------------------------------------DYINI168
      ENTRY DYSTAT                                                      DYINI169
  191 CONTINUE                                                          DYINI170
C                                                                       DYINI171
      CALL FINISH(NTYPO,NEVT)                                           DYINI172
C                                                                       DYINI173
      RETURN                                                            DYINI174
      END                                                               DYINI175
      SUBROUTINE USCJOB                                                 USCJOB 2
      COMMON/ZDUMP / IDEBU , NEVT                                       LUSTAT 2
      COMMON/LUSTAT/ ICOULU(10),SVRT(3)                                 LUSTAT 3
      COMMON/RESULT/ SIGBOR,SIGTOT,ERRSIG,ASYTOT,ERRASY                 LUSTAT 4
      COMMON /CONST/ ALFA,PI,ALFA1                                      DYCONS 2
      COMMON /RUNPAR/ SOLD,ID2,ID3,FINEXP,POIDS,INTERF,XK0              DYCONS 3
      COMMON /WEAK/ AEL,AMU,AMZ,GAMM,SW2,CA2,CV2,CA2CV2,COL,T3,QI       DYCONS 4
      COMMON /BEAM/ S0,EBEAM                                            DYCONS 5
      COMMON /TAU / TAU,CPTAU,HEL,PITAU(4)                              DYCONS 6
      COMMON/WEAKQ/WEAKC(11,6),XSECT(6),XTOT                            DYCONS 7
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
C.......................................................................USCJOB 6
       IUT=IW(6)                                                        USCJOB 7
       WRITE(IUT,101)                                                   USCJOB 8
  101  FORMAT(//20X,'EVENTS STATISTICS',                                USCJOB 9
     &         /20X,'*****************')                                USCJOB10
       WRITE(IUT,102) ICOULU(9)+ICOULU(10),ICOULU(10),ICOULU(9)         USCJOB11
  102  FORMAT(/5X,'# OF GENERATED EVENTS                = ',I10,        USCJOB12
     &        /5X,'# OF ACCEPTED  EVENTS                = ',I10,        USCJOB13
     &        /5X,'# OF REJECTED  EVENTS                = ',I10)        USCJOB14
       WRITE(IUT,103)                                                   USCJOB15
  103  FORMAT(//20X,'REJECT STATISTICS',                                USCJOB16
     &         /20X,'*****************')                                USCJOB17
       WRITE(IUT,104) (ICOULU(I),I=1,8)                                 USCJOB18
  104  FORMAT(/10X,'IR= 1 LUND ERROR unknown part    # OF REJECT =',I10,USCJOB19
     &        /10X,'IR= 2 BOS  ERROR KINE/VERT       # OF REJECT =',I10,USCJOB20
     &        /10X,'IR= 3 LUND ERROR too many tracks # OF REJECT =',I10,USCJOB21
     &        /10X,'IR= 4 LUND ERROR Beam wrong pos  # OF REJECT =',I10,USCJOB22
     &        /10X,'IR= 5 LUND ERROR status code >5  # OF REJECT =',I10,USCJOB23
     &        /10X,'IR= 6 USER reject in DYGENE      # OF REJECT =',I10,USCJOB24
     &        /10X,'IR= 7 free for user              # OF REJECT =',I10,USCJOB25
     &        /10X,'IR= 8 free for user              # OF REJECT =',I10)USCJOB26
      CALL DYSTAT                                                       USCJOB27
      IF (XTOT.NE.0.) WRITE (IUT,105) XTOT                              USCJOB28
  105 FORMAT (/20X,'***************************************',           USCJOB29
     &      /,/20X,' TOTAL CROSS SECTION FOR MIXED FLAVOR: ',           USCJOB30
     &      /,/30X,  F12.4,/20X,'*************************************')USCJOB31
      RETURN                                                            USCJOB32
      END                                                               USCJOB33
