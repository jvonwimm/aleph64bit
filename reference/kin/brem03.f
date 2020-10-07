cFrom BLOCH@alws.cern.ch Fri Feb 13 15:12:29 2004
cDate: Fri, 13 Feb 2004 15:11:08 +0100
cFrom: BLOCH@alws.cern.ch
cTo: BLOCH@alws.cern.ch

      SUBROUTINE ASKUSI(IGCOD)                                          ASKUSI 2
C--------------------------------------------------------------------   ASKUSI 3
C      ALAIN BLONDEL DEC 1 1987 IMPLEMENTATION OF BREM5                 ASKUSI 4
C              THE MAIN PROGRAM OF BREM5  IS CUT                        ASKUSI 5
C            INTO 4 ENTRIES                                             ASKUSI 6
C              BRINIT                                                   ASKUSI 7
C              BRFLOP ( SWITCHES POLARIZATION AROUND AFTER N/2 EVTS)    ASKUSI 8
C              BRGENE ( GENERATES ONE EVENT)                            ASKUSI 9
C              BRSTAT ( CALCULATES  CROSS SECTIONS AND ASYMMETRIES)     ASKUSI10
C                                                                       ASKUSI11
C     structure : subroutine                                            ASKUSI12
C                                                                       ASKUSI13
C     input     : none                                                  ASKUSI14
C                                                                       ASKUSI15
C     output    : generator code as define in the KINGAL library        ASKUSI16
C--------------------------------------------------------------------   ASKUSI17
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
      COMMON/LUSTAT/ ICOULU(10)                                         ASKUSI20
      PARAMETER ( IGCO = 5006)                                          ASKUSI21
C                                                                       ASKUSI22
C     BREM5 INPUT VARIABLES                                             ASKUSI23
C                                                                       ASKUSI24
      REAL*8 RDATIN                                                     ASKUSI25
      COMMON/INDAT/RDATIN(45)                                           ASKUSI26
      DIMENSION TABL(45)                                                ASKUSI27
C                                                                       ASKUSI28
      PARAMETER (LPDEC=48)                                              ASKUSI29
      INTEGER NODEC(LPDEC)                                              ASKUSI30
      INTEGER ALTABL,ALRLEP                                             BBL01  1
      EXTERNAL ALTABL ,ALRLEP                                           BBL01  2
      DO 5 K=1,10                                                       ASKUSI32
 5    ICOULU(K)=0                                                       ASKUSI33
C                                                                       ASKUSI34
C   RETURN THE GENERATOR CODE                                           ASKUSI35
C                                                                       ASKUSI36
      IGCOD=IGCO                                                        ASKUSI37
      WRITE (IW(6),1000) IGCOD                                          BBL01  3
 1000 FORMAT(30X,78('*'),/,40X,'WELCOME TO THE BREM5 MC BREM03'         BBL01  4
     $    ,/,70X,'GENERATOR CODE IS :',I10,/,40X,                       BBL01  5
     $ 'LAST MODIFIED November  13,1990',                               BBL01  6
     $  /,30X,78('*'))                                                  BBL01  7
C                                                                       ASKUSI38
C      BREM5 PARAMETERS     DEFAULT VALUES                              ASKUSI39
C                         N GENERATED EVENTS---IRRELEVANT               ASKUSI40
      RDATIN(1)=1000.D0                                                 ASKUSI41
C                         BEAM ENERGY                                   ASKUSI42
      RDATIN(2)=46.1D0                                                  ASKUSI43
C                         MZ                                            ASKUSI44
      RDATIN(3)=92.D0                                                   ASKUSI45
      RDATIN(4)=10.D0                                                   ASKUSI46
      RDATIN(5)=170.D0                                                  ASKUSI47
      RDATIN(6)=1.D0                                                    ASKUSI48
      RDATIN(7)=.2D0                                                    ASKUSI49
      RDATIN(8)=.2D0                                                    ASKUSI50
      RDATIN(9)=180.D0                                                  ASKUSI51
C                         K0 MINIMUM HARD PHOTON ENERGY/EBEAM           ASKUSI52
      RDATIN(10)=.01D0                                                  ASKUSI53
C                         WEAK ISOSPIN AND CHARGE OF INCOMING FERMION   ASKUSI54
      RDATIN(11)=-0.5D0                                                 ASKUSI55
      RDATIN(12)=-1.D0                                                  ASKUSI56
C                           "    "      "     "      OUTGOING   "       ASKUSI57
      RDATIN(13)=-0.5D0                                                 ASKUSI58
      RDATIN(14)=-1.D0                                                  ASKUSI59
C                             MH                                        ASKUSI60
      RDATIN(15)=100.D0                                                 ASKUSI61
C                             MT                                        ASKUSI62
      RDATIN(16)=60.D0                                                  ASKUSI63
C    BEAM POLARIZATION ( WILL BE REVERSED IN MIDDLE OF RUN IF NON ZERO) ASKUSI64
      RDATIN(17)=0.D0                                                   ASKUSI65
      RDATIN(18)=1.D0                                                   ASKUSI66
      RDATIN(19)=.1056D0                                                ASKUSI67
      RDATIN(20)=2.D0                                                   ASKUSI68
      RDATIN(21)=19.D0                                                  ASKUSI69
      RDATIN(22)=1.D0                                                   ASKUSI70
      RDATIN(23)=219.D0                                                 ASKUSI71
      RDATIN(24)=1.D0                                                   ASKUSI72
      ITYPE = 2                                                         ASKUSI73
      IPPAR = 0                                                         BBL01  8
C   DEFAULT SETUP IS MUON PAIR PRODUCTION                               ASKUSI74
C                                                                       ASKUSI75
      NABRIN=NAMIND('GBRE')                                             ASKUSI76
      ID=IW(NABRIN)                                                     ASKUSI77
      IF (ID.NE.0) THEN                                                 ASKUSI78
          RDATIN(2) = RW(ID+2)                                          ASKUSI79
          RDATIN(3) = RW(ID+3)                                          ASKUSI80
          RDATIN(4) = RW(ID+9)                                          ASKUSI81
          RDATIN(5) = RW(ID+10)                                         ASKUSI82
          RDATIN(6) = RW(ID+7)                                          ASKUSI83
          RDATIN(7) = RW(ID+11)                                         ASKUSI84
          RDATIN(8) = RW(ID+12)                                         ASKUSI85
          RDATIN(9) = RW(ID+13)                                         ASKUSI86
          RDATIN(10) = RW(ID+8)                                         ASKUSI87
          RDATIN(15) = RW(ID+4)                                         ASKUSI88
          RDATIN(16) = RW(ID+5)                                         ASKUSI89
          RDATIN(17) = RW(ID+14)                                        ASKUSI90
          RDATIN(24) = RW(ID+6)                                         ASKUSI91
          RDATIN(23) = RW(ID+15)                                        ASKUSI92
          ITYPE = NINT(RW(ID+1))                                        ASKUSI93
          IPPAR = NINT(RW(ID+16))                                       BBL01  9
      ENDIF                                                             ASKUSI94
C                                                                       ASKUSI95
C    update lund parameters from Data cards                             ASKUSI96
C                                                                       ASKUSI97
      CALL KXLUCO(LUPAR)                                                ASKUSI98
C                                                                       ASKUSI99
C    SEt up default masses for LUND : Z0 , Higgs  , Top                 ASKUS100
C                                                                       ASKUS101
      PMASL2(2) = RDATIN(3)                                             ASKUS102
      PMASL2(4) = RDATIN(15)                                            ASKUS103
      PMASL2(106) = RDATIN(16)                                          ASKUS104
      JSVER = IW(NAMIND('SVRT'))                                        ASKUS105
      IF (JSVER.GT.0) THEN                                              ASKUS106
         DO 3 K = 1,3                                                   ASKUS107
 3       RDATIN(26+K) = RW(JSVER+K)                                     ASKUS108
      ELSE                                                              ASKUS109
         RDATIN(27) = 0.035                                             ASKUS110
         RDATIN(28) = 0.0012                                            ASKUS111
         RDATIN(29) = 1.28                                              ASKUS112
      ENDIF                                                             ASKUS113
C                                                                       ASKUS114
C         Leptons                                                       ASKUS115
          IF (ITYPE.LE.3) THEN                                          ASKUS116
              RDATIN(13) = -0.5D0                                       ASKUS117
              RDATIN(14) = -1.D0                                        ASKUS118
              RDATIN(18) = 1.D0                                         ASKUS119
              RDATIN(19) = ULMASS(0,5+2*ITYPE)                          ASKUS120
              IF (ITYPE.EQ.3) THEN                                      ASKUS121
                 CALL LUTAUD(IFL)                                       ASKUS122
                 IF (IFL.NE.0) CALL EXIT                                ASKUS123
              ENDIF                                                     ASKUS124
          ELSE                                                          ASKUS125
C    Quarks                                                             ASKUS126
              ITY=ITYPE-10+500                                          ASKUS127
              RDATIN(14) = LUCHGE(ITY)/3.                               ASKUS128
              RDATIN(13) = SIGN(0.5D0,RDATIN(14))                       ASKUS129
              RDATIN(18) = 3.D0                                         ASKUS130
              RDATIN(19) = PMASL2(ITY-400)                              ASKUS131
          ENDIF                                                         ASKUS132
      CALL KXLUPA (IPART,IKLIN)                                         ASKUS133
      IF (IPART.LE.0 .OR. IKLIN.LE.0) THEN                              ASKUS134
         WRITE (IW(6),'(1X,''error in PART or KLIN bank - STOP - ''     ASKUS135
     +                 ,2I3)') IPART,IKLIN                              ASKUS136
         GOTO 20                                                        ASKUS137
      ENDIF                                                             ASKUS138
C                                                                       ASKUS139
C   Inhibit decays                                                      ASKUS140
C                                                                       ASKUS141
      MXDEC=KNODEC(NODEC,LPDEC)                                         ASKUS142
      MXDEC=MIN(MXDEC,LPDEC)                                            ASKUS143
      IF (MXDEC.GT.0) THEN                                              ASKUS144
         DO 10 I=1,MXDEC                                                ASKUS145
            IF (NODEC(I).GT.0) IDBLU3(NODEC(I))=0                       ASKUS146
   10    CONTINUE                                                       ASKUS147
      ENDIF                                                             ASKUS148
      NWB=29                                                            ASKUS149
C                                                                       ASKUS150
C       INITIALIZE BREM5                                                ASKUS151
C                                                                       ASKUS152
        CALL BRINIT(ITYPE)                                              ASKUS153
C                                                                       ASKUS154
C   dump the generator parameters for this run in a bank                ASKUS155
C assume all parameters are real and stored as a single row             ASKUS156
      DO 11 I=1,NWB                                                     ASKUS157
 11   TABL(I) = RDATIN(I)                                               ASKUS158
      IND = ALTABL('KPAR',NWB,1,TABL,'2I,(F)','C')                      ASKUS159
C                                                                       ASKUS160
C                                                                       ASKUS161
C  Print PART and KLIN banks                                            ASKUS162
C                                                                       ASKUS163
      IF (IPPAR.GT.0) CALL PRPART                                       BBL01 10
C                                                                       ASKUS165
      CALL PRTABL('KPAR',0)                                             ASKUS166
C  Fill RLEP bank                                                       BBL01 11
      IEBEAM = NINT(RDATIN(2)*1000.)                                    BBL01 12
      JRLEP = ALRLEP(IEBEAM,'    ',0,0,0)                               BBL01 13
      CALL PRTABL('RLEP',0)                                             BBL01 14
   20 RETURN                                                            ASKUS167
      END                                                               ASKUS168
      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)                 ASKUSE 2
C--------------------------------------------------------------------   ASKUSE 3
C      ALAIN  BLONDEL DEC 1 1987 IMPLEMENTATION OF BREM5                ASKUSE 4
                                                                        ASKUSE 5
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
      COMMON/LUSTAT/ ICOULU(10)                                         ASKUSE24
      DIMENSION SVERT(3),VERT(4),TABL(3)                                ASKUSE25
C                                                                       ASKUSE26
C     BREM5 INPUT VARIABLES                                             ASKUSE27
C                                                                       ASKUSE28
      REAL*8 RDATIN                                                     ASKUSE29
      COMMON/INDAT/RDATIN(45)                                           ASKUSE30
      DATA NIT/0/                                                       ASKUSE31
      DATA SVERT / 0.035 , 0.0012 , 1.28 /                              ASKUSE32
      DATA IEV1, IEV2 / 1 , 1000 /                                      ASKUSE33
C                                                                       ASKUSE34
C THE FIRST THING TO DO IS TO FLIP THE POLARIZATION AROUND IF NEEDED    ASKUSE35
C                                                                       ASKUSE36
      IF( NIT.NE.0) GO TO 2                                             ASKUSE37
        JTRIG=NLINK('TRIG',0)                                           ASKUSE38
        IF ( JTRIG.GT.0) THEN                                           ASKUSE39
           IEV1=IW(JTRIG+1)                                             ASKUSE40
           IEV2=IW(JTRIG+2)                                             ASKUSE41
        ENDIF                                                           ASKUSE42
        IEFLOP =(IEV2-IEV1+1)/2 +1                                      ASKUSE43
C  if you need the standard interaction point                           ASKUSE44
C  you may get the sigmas of the gaussion smearing                      ASKUSE45
C  from a data card if you like it                                      ASKUSE46
C                                                                       ASKUSE47
C  SVERT   SIGMAX  SIGMAY  SIGMAZ                                       ASKUSE48
C                                                                       ASKUSE49
        NASVER=NAMIND('SVRT')                                           ASKUSE50
        JSVER=IW(NASVER)                                                ASKUSE51
        IF (JSVER.NE.0) THEN                                            ASKUSE52
           SVERT(1)=RW(JSVER+1)                                         ASKUSE53
           SVERT(2)=RW(JSVER+2)                                         ASKUSE54
           SVERT(3)=RW(JSVER+3)                                         ASKUSE55
        ENDIF                                                           ASKUSE56
  2   NIT=NIT+1                                                         ASKUSE57
      IF(NIT.EQ.IEFLOP) CALL BRFLOP                                     ASKUSE58
C                                                                       ASKUSE59
C  Generate vertex postion                                              ASKUSE60
C                                                                       ASKUSE61
      CALL RANNOR(RX,RY)                                                ASKUSE62
      CALL RANNOR(RZ,DUM)                                               ASKUSE63
      VERT(1)=RX*SVERT(1)                                               ASKUSE64
      VERT(2)=RY*SVERT(2)                                               ASKUSE65
      VERT(3)=RZ*SVERT(3)                                               ASKUSE66
      VERT(4)=0.                                                        ASKUSE67
C                                                                       ASKUSE68
C                                                                       ASKUSE69
      CALL BRGENE(IDPR,ISTAT,ECMS,WEIT)                                 ASKUSE70
      CALL KXLUAL(VERT,IST,NVRT,NTRK)                                   ASKUSE71
      IF (IST.EQ.0 .AND. ISTAT.EQ.0) THEN                               ASKUSE72
         ICOULU(10) = ICOULU(10)+1                                      ASKUSE73
      ELSEIF (IST.GT.0) THEN                                            ASKUSE74
         ICOULU(1) = ICOULU(1) +1                                       ASKUSE75
         ICOULU(9) = ICOULU(9) +1                                       ASKUSE76
      ELSEIF ( IST.LT.0) THEN                                           ASKUSE77
         ICOULU(-IST) = ICOULU(-IST) +1                                 ASKUSE78
         ICOULU(9) = ICOULU(9) +1                                       ASKUSE79
      ELSEIF ( ISTAT.GT.0) THEN                                         ASKUSE80
         ICOULU(6) = ICOULU(6) +1                                       ASKUSE81
         ICOULU(9) = ICOULU(9) +1                                       ASKUSE82
      ENDIF                                                             ASKUSE83
C                                                                       ASKUSE84
C  You can use the status word to decide not to keep the event          ASKUSE85
C  as you may generate only part of the particles spectra               ASKUSE86
C                                                                       ASKUSE87
      ISTA = IST+ISTAT*1000                                             ASKUSE88
      RETURN                                                            ASKUSE89
      END                                                               ASKUSE90
         SUBROUTINE BREMLU (BPP,BPM,BQP,BQM,BQK,ITYP1,ITYP2,WEI)        BREMLU 2
C-----------------------------------------------------------------------BREMLU 3
C   ALAIN BLONDEL DEC 1 1987          BDL@CERNVM                        BREMLU 4
C   B.Bloch-Devaux November 1990                                        BBL01 15
C                                                                       BREMLU 5
C                LUND SHOWER BREMMUS INTERFACE                          BREMLU 6
C                                                                       BREMLU 7
C        SET UP THE LUND COMMON                                         BREMLU 8
C             1 E+         }                       ( BPP)               BREMLU 9
C             2 E-         } OF FLAVOR ITYP1       ( BPM)               BREMLU10
C             3 Z ( ARBITRARY REALLY IS THE E+E- SYSTEM)                BREMLU11
C    IF ANY, (4)GAMMA   ( INITIAL OR FINAL STATE)  ( BQK)               BREMLU12
C        4 or 5 ANTIFERMION   }                    ( BQP)               BBL01 16
C        5 or 6 FERMION       }   OF FLAVOR ITYP2  ( BQM)               BBL01 17
C          THEN following particles are those after parton shower evol. BBL01 18
C          the original fermions are artificially kept in the two above BBL01 19
C          lines.                                                       BBL01 20
C      ( IN CASE OF QUARKS A SHOWER LINE INBETWEEN)                     BREMLU15
C     ITYP:      1=E, 2=MU, 3=TAU, 11=U, 12=D, 13=S, 14=C, 15=B, 16=T   BREMLU16
C     WEI :      Event weight                                           BREMLU17
C                                                                       BREMLU18
C-----------------------------------------------------------------------BREMLU19
         REAL*8 BPP(4),BPM(4),BQP(4),BQM(4),BQK(4)                      BREMLU20
         DIMENSION Z(4)                                                 BREMLU21
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
         DATA NEV/0/                                                    BREMLU23
C                                                                       BREMLU24
C     CLEAN-UP                                                          BREMLU25
C                                                                       BREMLU26
         DO 5 I=1,20                                                    BREMLU27
         DO 5 J=1,2                                                     BREMLU28
         KODELU(I,J)=0                                                  BREMLU29
    5 CONTINUE                                                          BREMLU30
         DO 6 I=1,20                                                    BREMLU31
         DO 6 J=1,5                                                     BREMLU32
         PARTLU(I,J)=0.                                                 BREMLU33
    6 CONTINUE                                                          BREMLU34
C                                                                       BREMLU35
C        FIRST E+ E-  Z AND PHOTON                                      BREMLU36
C                                                                       BREMLU37
        ITY = 7                                                         BREMLU38
        KODELU(1,1)=40000                                               BREMLU39
        KODELU(1,2)=-ITY                                                BREMLU40
        KODELU(2,1)=40000                                               BREMLU41
        KODELU(2,2)=ITY                                                 BREMLU42
        KODELU(3,1)=20000                                               BREMLU43
        KODELU(3,2)=2                                                   BREMLU44
        PARTLU(1,5)=ULMASS(0,ITY)                                       BREMLU45
        PARTLU(2,5)=ULMASS(0,ITY)                                       BREMLU46
      DO 10 I=1,4                                                       BREMLU47
        PARTLU(1,I)=BPP(I)                                              BREMLU48
        PARTLU(2,I)=BPM(I)                                              BREMLU49
        PARTLU(3,I)=BPP(I)+BPM(I)                                       BREMLU50
   10 CONTINUE                                                          BREMLU51
        Z2=ABS(PARTLU(3,4)**2-                                          BREMLU52
     +        PARTLU(3,1)**2-PARTLU(3,2)**2-PARTLU(3,3)**2)             BREMLU53
        PARTLU(3,5)=SQRT(Z2)                                            BREMLU54
        NPARLU=3                                                        BREMLU55
        IZLU = NPARLU                                                   BREMLU56
      IF(BQK(4).GT.1.D-04 ) THEN                                        BREMLU57
C                                                                       BREMLU58
C  THERE IS A PHOTON, TAKES CARE OF IT                                  BREMLU59
C                                                                       BREMLU60
         KODELU(4,1)=0                                                  BREMLU61
         KODELU(4,2)=1                                                  BREMLU62
      DO 15 I=1,4                                                       BREMLU63
         PARTLU(4,I)=BQK(I)                                             BREMLU64
   15 CONTINUE                                                          BREMLU65
         PARTLU(4,5)=0.                                                 BREMLU66
         NPARLU=NPARLU+1                                                BREMLU67
      ENDIF                                                             BREMLU68
      IF(ITYP2.LT.10) THEN                                              BREMLU69
C                                                                       BREMLU70
C        THESE ARE LEPTONS  NO SHOWER  LINES NEEDED                     BREMLU71
C          THIS REPRODUCES THE LUND6.3 LEPTON FLAVOR CODE               BREMLU72
C                                                                       BREMLU73
         KMASS=5+2*ITYP2                                                BREMLU74
         KODELU(NPARLU+1,1)= IZLU                                       BREMLU75
         KODELU(NPARLU+2,1)= IZLU                                       BREMLU76
         KODELU(NPARLU+1,2)=-KMASS                                      BREMLU77
         KODELU(NPARLU+2,2)=KMASS                                       BREMLU78
         PARTLU(NPARLU+1,5)=ULMASS(0,KMASS)                             BREMLU79
         PARTLU(NPARLU+2,5)=ULMASS(0,KMASS)                             BREMLU80
         DO 40 I=1,4                                                    BREMLU81
           PARTLU(NPARLU+1,I)=BQP(I)                                    BREMLU82
           PARTLU(NPARLU+2,I)=BQM(I)                                    BREMLU83
   40    CONTINUE                                                       BREMLU84
         NPARLU=NPARLU+2                                                BREMLU85
         CALL LUEXEC                                                    BREMLU86
      ELSE                                                              BREMLU87
C                                                                       BREMLU88
C     QUARKS                                                            BREMLU89
C                                                                       BREMLU90
                                                                        BREMLU91
         KMASS=500+ITYP2-10                                             BREMLU92
         IQU1 = NPARLU+1                                                BBL01 21
         IQU2 = NPARLU+2                                                BBL01 22
         KODELU(NPARLU+1,1)=30000+IZLU                                  BBL01 23
         KODELU(NPARLU+2,1)= 20000+IZLU                                 BBL01 24
         KODELU(NPARLU+1,2)=-KMASS                                      BBL01 25
         KODELU(NPARLU+2,2)=KMASS                                       BBL01 26
         PARTLU(NPARLU+1,5)=ULMASS(0,KMASS)                             BBL01 27
         PARTLU(NPARLU+2,5)=ULMASS(0,KMASS)                             BBL01 28
         DO 141 I=1,4                                                   BBL01 29
           PARTLU(NPARLU+1,I)=BQP(I)                                    BBL01 30
           PARTLU(NPARLU+2,I)=BQM(I)                                    BBL01 31
  141    CONTINUE                                                       BBL01 32
         NPARLU=NPARLU+2                                                BBL01 33
C  now the quarks for shower lines                                      BBL01 34
         KODELU(NPARLU+1,1)=10000+IQU1                                  BBL01 35
         KODELU(NPARLU+3,1)= IQU2                                       BBL01 36
         KODELU(NPARLU+1,2)=-KMASS                                      BREMLU95
         KODELU(NPARLU+3,2)=KMASS                                       BREMLU96
         PARTLU(NPARLU+1,5)=ULMASS(0,KMASS)                             BREMLU97
         PARTLU(NPARLU+3,5)=ULMASS(0,KMASS)                             BREMLU98
         DO 140 I=1,4                                                   BREMLU99
           PARTLU(NPARLU+1,I)=BQP(I)                                    BREML100
           PARTLU(NPARLU+3,I)=BQM(I)                                    BREML101
  140    CONTINUE                                                       BREML102
C                                                                       BREML103
C   FILL THE COLOUR LINES                                               BREML104
C                                                                       BREML105
         KODELU(NPARLU+2,1) = 70000 + NPARLU+1                          BREML106
         KODELU(NPARLU+4,1) = 70000 + NPARLU+3                          BREML107
         KODELU(NPARLU+2,2) =  1000 + NPARLU+1                          BREML108
         KODELU(NPARLU+4,2) =  1000 + NPARLU+3                          BREML109
         PARTLU(NPARLU+2,1) =  NPARLU + 3                               BREML110
         PARTLU(NPARLU+2,2) =  NPARLU + 3                               BREML111
         PARTLU(NPARLU+4,1) =  NPARLU + 1                               BREML112
         PARTLU(NPARLU+4,2) =  NPARLU + 1                               BREML113
C                                                                       BREML114
C        I NEED TO CALCULATE THE QQBAR MASS                             BREML115
C                                                                       BREML116
         N=NPARLU                                                       BREML117
         NPARLU=NPARLU+4                                                BREML118
         DO 150 I=1,4                                                   BREML119
           Z(I)=BQP(I)+BQM(I)                                           BREML120
  150    CONTINUE                                                       BREML121
         QMAX= SQRT( Z(4)**2- Z(3)**2-Z(2)**2-Z(1)**2 )                 BREML122
C                                                                       BREML123
C         GENERATE PARTON SHOWER                                        BREML124
C                                                                       BREML125
         IF( NEV.LE.10)CALL LULIST(11)                                  BBL01 37
         CALL LUSHOW(N+1,N+3,QMAX)                                      BREML127
         CALL LUEXEC                                                    BREML128
C                                                                       BREML129
C  LINKS THE QUARK BACK TO THE Z                                        BREML130
C                                                                       BREML131
C        KODELU(N+1,1)=30000                                            BREML132
         DO 160 IP=N+1,NPARLU                                           BREML133
         IF (ABS(KODELU(IP,2)).LT.500) GO TO 197                        BREML134
            IZL = IZLU                                                  BBL01 38
            IF(KODELU(IP,2).LT.500) IZL = IQU1                          BBL01 39
            IF (KODELU(IP,2).GT.500) IZL =IQU2                          BBL01 40
            KODELU(IP,1) = 10000*(KODELU(IP,1)/10000)+IZL               BBL01 41
  160    CONTINUE                                                       BREML136
  197    CONTINUE                                                       BREML137
C                                                                       BREML138
      ENDIF                                                             BREML139
C                                                                       BREML140
C   Now check if the initial electron was along positive Z axis.        BREML141
C   If not reverse Px and Pz components for all particles               BREML142
C                                                                       BREML143
      IF ( PARTLU(2,3).LT.0.) THEN                                      BREML144
         DO 201 IP=1,NPARLU                                             BREML145
         DO 202 JJ=1,3,2                                                BREML146
 202       PARTLU(IP,JJ) = -PARTLU(IP,JJ)                               BREML147
 201     CONTINUE                                                       BREML148
      ENDIF                                                             BREML149
      NEV=NEV+1                                                         BREML150
      IF(NEV.LE.10)CALL LULIST(11)                                      BBL01 42
      IF ( PARTLU(4,5).EQ.0.) THEN                                      BREML152
C    There is a photon                                                  BREML153
      CALL HFILL(10003,PARTLU(4,4),DUM,WEI)                             BREML154
      ENDIF                                                             BREML155
      EE = BQP(4)                                                       BREML156
      CALL HFILL(10002,EE,DUM,WEI)                                      BREML157
      EE = BQM(4)                                                       BREML158
      CALL HFILL(10001,EE,DUM,WEI)                                      BREML159
      EE= BQP(3)/SQRT(BQP(1)**2+BQP(2)**2+BQP(3)**2)                    BREML160
      CALL HFILL(10005,EE,DUM,WEI)                                      BREML161
      EE= BQM(3)/SQRT(BQM(1)**2+BQM(2)**2+BQM(3)**2)                    BREML162
      CALL HFILL(10004,EE,DUM,WEI)                                      BREML163
      EE=NPARLU-3                                                       BREML164
      CALL HFILL(10006,EE,DUM,WEI)                                      BREML165
      CALL HFILL(10007,WEI,DUM,1.)                                      BREML166
      RETURN                                                            BREML167
      END                                                               BREML168
          SUBROUTINE BRINIT(NTYP)                                       BRINIT 2
C-----------------------------------------------------------------------BRINIT 3
C    ALAIN BLONDEL DEC 1 1987                                           BRINIT 4
C         ORIGINAL VERSION OF BREM5 AS PROVIDED BY ROBIN STUART         BRINIT 5
C                       MARCH 1987                                      BRINIT 6
C        HERE IS THE DRIVING PROGRAM ONLY, SOURCE IS IN BREM5 FORTRAN   BRINIT 7
C        THE LUND INTERFACE CAN BE COMMON TO ALL THIS TYPE OF GENERATORSBRINIT 8
C          IT IS THE LUND SHOWER TYPE EXCLUSIVELY                       BRINIT 9
C-----------------------------------------------------------------------BRINIT10
C                                                                       BRINIT11
C FORTOPT VS FIXED NOOPT NOPRINT                                        BRINIT12
C NEW IMPROVED VERSION OF BREMMUS, KLEISS/STUART/LYNN                   BRINIT13
C                                                                       BRINIT14
C IN THIS PROGRAM POLARIZATION XPOL IS FOR INITIAL STATE ELECTRON       BRINIT15
C ONLY. UNPOLARIZED CROSS SECTION IS LEFT+RIGHT, POLARIZATION           BRINIT16
C CROSS SECTION IS LEFT-RIGHT. TOTAL CROSS SECTION IS                   BRINIT17
C   (LEFT+RIGHT)-XPOL*(LEFT-RIGHT)                                      BRINIT18
C NOTE MODIFICATION OF EFFECTIVE POLARIZATION FOR HARD BREM.            BRINIT19
C                                                                       BRINIT20
C THIS MAIN ROUTINE DOES THE SETUP, EVENT GENERATION LOOP, AND          BRINIT21
C OUTPUTS THE FINAL RESULTS                                             BRINIT22
C                                                                       BRINIT23
C                                                                       BRINIT24
C                                                                       BRINIT25
C                                                                       BRINIT26
      IMPLICIT REAL*8(A-H,O-Z)                                          BRINIT27
      COMMON/INDAT/RDATIN(45)                                           BRINIT28
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
C                                                                       BRINIT30
C RDATIN CONTAINS THE INPUT PARAMETERS SET-UP IN BREM2 INPUT            BRINIT31
C                                                                       BRINIT32
C RDATIN(4)        MIN MU POLAR ANGLE                                   BRINIT33
C RDATIN(5)        MAX MU POLAR ANGLE                                   BRINIT34
C RDATIN(6)        MAX HARD PHOTON ENERGY/BEAM ENERGY                   BRINIT35
C RDATIN(7)        MIN MUON ENERGY (often E/2)                          BRINIT36
C RDATIN(8)        MIN ANIT-MUON ENERGY                                 BRINIT37
C RDATIN(9)        ACOLINEARITY CUT ANGLE                               BRINIT38
C RDATIN(17)       POLARIZATION INITIAL STATE ELECTRON                  BRINIT39
C RDATIN(18)       NUMBER OF FINAL STATE COLORS                         BRINIT40
C RDATIN(19)       FINAL FERMION MASS                                   BRINIT41
C RDATIN(20)       NO. OF TREE DIAGRAMS                                 BRINIT42
C RDATIN(21)       NO. OF 1 LOOP DIAGRAMS                               BRINIT43
C RDATIN(22)       SWITCH TO TURN HISTOGRAMS ON & OFF                   BRINIT44
C RDATIN(23)       RANDOM NUMBER GENERATOR SEED                         BRINIT45
C RDATIN(24)       TEST PARAMETER 1= FINAL STATE RAD,0= NONE            BRINIT46
C      THE FOLLOWING  PARAMETERS ARE NOT USED NORMALLY                  BRINIT47
C RDATIN(25)       NUMBER OF ECM POINTS                                 BRINIT48
C RDATIN(26)       STEP SIZE IN EBEAM  FOR ECM SCAN                     BRINIT49
C                                                                       BRINIT50
C                                                                       BRINIT51
C  COMMON CONSTS CONTAINS FREQUENTLY USED CONSTANTS......               BRINIT52
C                                                                       BRINIT53
      COMMON/CONSTS/PI,ALFA1,CV1,CV2,CA1,CA2,DEGRAD,RADDEG              BRINIT54
C                                                                       BRINIT55
      DIMENSION PP(4),PM(4),QP(4),QM(4),QK(4)                           BRINIT56
      DIMENSION RESULT(2),ANOC(2),FINAL(2),ACUT(2)                      BRINIT57
      DIMENSION EVNOCF(2),EVNOCT(2),EVCUTF(2),EVCUTT(2)                 BRINIT58
      LOGICAL HISTPR                                                    BRINIT59
      REAL*4 ECMS,WEIT                                                  BRINIT60
C                                                                       BRINIT61
C                                                                       BRINIT62
C RDATIN CONTAINS THE INPUT PARAMETERS SET-UP BY DEFAULT AND            BRINIT63
C EVENTUALLY MODIFIED BY USER'S INPUT CARDS                             BRINIT64
C                                                                       BRINIT65
C                                                                       BRINIT66
       PI=3.1415926535897932D0                                          BRINIT67
       ALFA1=7.297351D-3                                                BRINIT68
       RADDEG=0.017453293D0                                             BRINIT69
       DEGRAD=57.29577951D0                                             BRINIT70
      ITYPE = NTYP                                                      BRINIT71
      IOUT = IW(6)                                                      BRINIT72
C                                                                       BRINIT73
      ISEED = RDATIN(23)                                                BRINIT74
C                                                                       BRINIT75
C                    USE BETTER RANDOM NUMBER GENERATOR                 BRINIT76
C                    INITIALISE ONCE ONLY SO GET DIFFERENT              BRINIT77
C                    RANDOM NUMBERS FOR SECOND GO                       BRINIT78
C                                                                       BRINIT79
      EB=RDATIN(2)                                                      BRINIT80
      XMZ=RDATIN(3)                                                     BRINIT81
      THMIN=RDATIN(4)                                                   BRINIT82
      THMAX=RDATIN(5)                                                   BRINIT83
      XKMAX=RDATIN(6)                                                   BRINIT84
      EMMIN=RDATIN(7)                                                   BRINIT85
      EMPLS=RDATIN(8)                                                   BRINIT86
      ACOL=RDATIN(9)                                                    BRINIT87
       WRITE(IOUT,1533) ITYPE                                           BRINIT88
 1533 FORMAT( 30X,'**************************************************', BRINIT89
     &/,30X,'* B R E M 5     A D A P T E D   T O   K I N G A L  : '/    BRINIT90
     X ,30X,'*   GENERATING FERMION TYPE:',I3,/,                        BRINIT91
     &30X,'*****************************************************')      BRINIT92
      POL=RDATIN(17)                                                    BRINIT93
      IFLOP=0                                                           BRINIT94
      IFPOL=1                                                           BRINIT95
      IF(POL.LE.0.01) IFPOL=0                                           BRINIT96
      IF(POL.LE.0.01) POL=0.D0                                          BRINIT97
C                                                                       BRINIT98
C                                                                       BRINIT99
      IPOL=1                                                            BRINI100
C                                                                       BRINI101
      IF( IPOL . EQ . 1 ) CALL RDMIN(ISEED)                             BRINI102
C                                                                       BRINI103
      CALL HBOOK1(10001,'OUTGOING FERMION ENERGY$',50,0.,50.,0.)        BRINI104
      CALL HBOOK1(10002,'OUTGOING ANTIFERMION ENERGY$',50,0.,50.,0.)    BRINI105
      CALL HBOOK1(10003,'PHOTON ENERGY IF ANY$',50,0.,20.,0.)           BRINI106
      CALL HBOOK1(10004,'POLAR ANGLE FERMION$',50,-1.,1.,0.)            BRINI107
      CALL HBOOK1(10005,'POLAR ANGLE ANTIFERMION$',50,-1.,1.,0.)        BRINI108
      CALL HBOOK1(10006,'FINAL MULTIPLICITY GENERATED$',50,5.,105.,0.)  BRINI109
      CALL HBOOK1(10007,'WEIGHT DISTRIBUTION  ',50,0.,2.,0.)            BRINI110
C                                                                       BRINI111
C                                                                       BRINI112
      GO TO 80                                                          BRINI113
C ----------------------------------------------------------------------BRINI114
C     SPECIAL ENTRY TO SWITCH POLARIZATION AROUND                       BRINI115
C                                                                       BRINI116
      ENTRY BRFLOP                                                      BRINI117
      IF(IFPOL.EQ.0) RETURN                                             BRINI118
      IFLOP=1                                                           BRINI119
      GOTO 191                                                          BRINI120
   70 IFLOP=0                                                           BRINI121
      IPOL=2                                                            BRINI122
      POL=DABS(POL)                                                     BRINI123
      IF(IPOL .EQ. 2.AND.POL.NE.0.0D0)POL= -POL                         BRINI124
      IF(IPOL .EQ. 2.AND.POL.NE.0.0D0)WRITE (IOUT, 1010) POL            BRINI125
 1010 FORMAT(1X,50(1H-),/' CHANGING R-POLARIZATION OF THE E- BEAM TO',  BRINI126
     .F10.4,/1X,50(1H-))                                                BRINI127
   80 CONTINUE                                                          BRINI128
      IF(RDATIN(22) .NE. 0.0D0)HISTPR=.TRUE.                            BRINI129
      CALL HISTCL                                                       BRINI130
C     CALL RNCLR                                                        BRINI131
      XNOCT=0.D0                                                        BRINI132
      XNOCF=0.D0                                                        BRINI133
      XCUTT=0.D0                                                        BRINI134
      XCUTF=0.D0                                                        BRINI135
C                                                                       BRINI136
C THE BOUNDARY BETWEEN SOFT AND HARD  BREMSSTRAHLUNG                    BRINI137
C                                                                       BRINI138
      XK0=RDATIN(10)                                                    BRINI139
C                                                                       BRINI140
C INITIALIZE THE SUBPROGRAMS                                            BRINI141
C                                                                       BRINI142
C                                                                       BRINI143
      CALL SETMUS(EB,XMZ,THMIN,THMAX,XKMAX,POL,IPOL)                    BRINI144
C                                                                       BRINI145
C                                                                       BRINI146
      RETURN                                                            BRINI147
C                                                                       BRINI148
      ENTRY BRGENE(IDPR,ISTAT,ECMS,WEIT)                                BRINI149
C                                                                       BRINI150
C PP,PM are initial electron 4-vectors                                  BRINI151
C QP,QM,QK are final muon and photon LAB-frame 4-vectors                BRINI152
C                                                                       BRINI153
      CALL GENMUS(PP,PM,QP,QM,QK,WE,ICON)                               BRINI154
      IDPR=ITYPE*1000+ICON                                              BRINI155
      ECMS=EB*2.                                                        BRINI156
      WEIT=WE                                                           BRINI157
      ISTAT=0                                                           BRINI158
C                                                                       BRINI159
C                                LUND INTERFACE                         BRINI160
      ITYP1 = 1                                                         BRINI161
      CALL BREMLU(PP,PM,QP,QM,QK,ITYP1,ITYPE,WEIT)                      BRINI162
C                                                                       BRINI163
      PMOMP = SQRT (QP(1)**2+QP(2)**2+QP(3)**2)                         BRINI164
      PMOMM = SQRT (QM(1)**2+QM(2)**2+QM(3)**2)                         BRINI165
      CP=QP(3)/PMOMP                                                    BRINI166
      CM=-QM(3)/PMOMM                                                   BRINI167
      IJ=7                                                              BRINI168
      IK=20                                                             BRINI169
      XD=-1.D0                                                          BRINI170
      XDD=1.D0                                                          BRINI171
      CALL HISTO1(IJ,IK,XD,XDD,CP,WE)                                   BRINI172
      CALL HISTO1(IJ,IK,XD,XDD,CM,WE)                                   BRINI173
      XNOCT=XNOCT+WE                                                    BRINI174
      IF(CP.GT.0.D0) XNOCF=XNOCF+WE                                     BRINI175
      IF(QP(4).LT.EMMIN.OR.QM(4).LT.EMPLS) GO TO 100                    BRINI176
      Z=(QP(1)*QM(1)+QP(2)*QM(2)+QP(3)*QM(3))/(PMOMP*PMOMM)             BRINI177
C                                                                       BRINI178
C                                                                       BRINI179
      IF (Z.GT.1.D0) Z=1.D0                                             BRINI180
      IF (Z.LT.-1.D0) Z=-1.D0                                           BRINI181
      Z=DEGRAD*DACOS(-Z)                                                BRINI182
      IM=8                                                              BRINI183
      IN=9                                                              BRINI184
      YD=0.D0                                                           BRINI185
      YDD=40.D0                                                         BRINI186
      CALL HISTO1(IM,IK,YD,YDD,Z,WE)                                    BRINI187
      IF(Z.GT.ACOL) GOTO 100                                            BRINI188
      CALL HISTO1(IN,IK,XD,XDD,CP,WE)                                   BRINI189
      CALL HISTO1(IN,IK,XD,XDD,CM,WE)                                   BRINI190
      XCUTT=XCUTT+WE                                                    BRINI191
      IF(CP.GT.0.D0) XCUTF=XCUTF+WE                                     BRINI192
  100 CONTINUE                                                          BRINI193
      RETURN                                                            BRINI194
C                                                                       BRINI195
C     DO 200 I=1,6                                                      BRINI196
C 200 CALL HISTO2(I,0)                                                  BRINI197
C                                                                       BRINI198
C-----------------------------------------------------------------------BRINI199
      ENTRY BRSTAT                                                      BRINI200
  191 CONTINUE                                                          BRINI201
C                                                                       BRINI202
      CALL ENDMUS(RESULT(IPOL))                                         BRINI203
      IF(.NOT. HISTPR)GO TO 190                                         BRINI204
C                                                                       BRINI205
C     CALL HISTO2(10,0)                                                 BRINI206
      CALL HISTO2(8,1)                                                  BRINI207
      CALL HISTO2(7,0)                                                  BRINI208
      CALL HISTO2(9,0)                                                  BRINI209
 190  CONTINUE                                                          BRINI210
C                                                                       BRINI211
      ANOC(IPOL)=100.*(2.*XNOCF/XNOCT-1.)                               BRINI212
      ACUT(IPOL)=100.*(2.*XCUTF/XCUTT-1.)                               BRINI213
      FINAL(IPOL)=XCUTT/XNOCT*RESULT(IPOL)                              BRINI214
      EVNOCF(IPOL)=XNOCF                                                BRINI215
      EVNOCT(IPOL)=XNOCT                                                BRINI216
      EVCUTF(IPOL)=XCUTF                                                BRINI217
      EVCUTT(IPOL)=XCUTT                                                BRINI218
C                                                                       BRINI219
      WRITE (IOUT, 300) ANOC(IPOL),ACUT(IPOL),FINAL(IPOL)               BRINI220
  300 FORMAT(1X,50(1H-),/,'THE INTEGRATED FORWARD-BACKWARD ASYMMETRIES  BRINI221
     $ ARE:',/,                                                         BRINI222
     .  10X,   ' WITHOUT ADDITIONAL CUTS =',F10.4,' %',/,               BRINI223
     .  10X,   '    WITH ADDITIONAL CUTS =',F10.4,' %',/,               BRINI224
     .  10X,   ' MOREOVER, DUE TO YOUR CUTS THE CROSS SECTION',/,       BRINI225
     .  10X,   ' IS REDUCED TO ',D15.6,' PICOBARNS')                    BRINI226
      IF(IFLOP.EQ.1) GO TO 70                                           BRINI227
 1000 CONTINUE                                                          BRINI228
       IF (IPOL.EQ. 1.OR.POL.EQ.0.D0) GO TO 900                         BRINI229
      ALRNOC = (RESULT(1)-RESULT(2))/(RESULT(1)+RESULT(2))/DABS(POL)    BRINI230
      ALRCUT = (FINAL(1)-FINAL(2))/(FINAL(1)+FINAL(2))/DABS(POL)        BRINI231
      WRITE (IOUT, 1020) ALRNOC,ALRCUT                                  BRINI232
 1020 FORMAT(1X,50(1H-),/,' THE LONGITUDINAL POLARIZATION ASYMMETRY IS:'BRINI233
     . ,10X     ,/,' WITHOUT ADDITIONAL CUTS =',F10.4                   BRINI234
     . ,10X     ,/,'    WITH ADDITIONAL CUTS =',F10.4                   BRINI235
     . ,10X     ,/1X,50(1H-))                                           BRINI236
C                                                                       BRINI237
C     THE POLARIZED FORWARD-BACWARD ASYMMETRY                           BRINI238
C                                                                       BRINI239
      APNOC=.5*(ANOC(2)-ANOC(1))/.75/DABS(POL)                          BRINI240
      APCUT=.5*(ACUT(2)-ACUT(1))/.75/DABS(POL)                          BRINI241
      WRITE (IOUT, 1021) APNOC,APCUT                                    BRINI242
 1021 FORMAT(1X,50(1H-),/,' THE  POLARIZED  ASYMMETRY IS:'              BRINI243
     .  ,10X    ,/,' WITHOUT ADDITIONAL CUTS =',F10.4                   BRINI244
     .  ,10X    ,/,'    WITH ADDITIONAL CUTS =',F10.4                   BRINI245
     .  ,10X    ,/1X,50(1H-))                                           BRINI246
C                                                                       BRINI247
      APONOC=((EVNOCF(1)-EVNOCF(2))*2-(EVNOCT(1)-EVNOCT(2)))            BRINI248
     .          /(EVNOCT(1)+EVNOCT(2))/.75/DABS(POL)                    BRINI249
      APOCUT=((EVCUTF(1)-EVCUTF(2))*2-(EVCUTT(1)-EVCUTT(2)))            BRINI250
     .          /(EVCUTT(1)+EVCUTT(2))/.75/DABS(POL)                    BRINI251
      WRITE (IOUT, 1022) APONOC,APOCUT                                  BRINI252
 1022 FORMAT(1X,50(1H-),/,' THE FANCY POLARIZED  ASYMMETRY IS:'         BRINI253
     . ,10X     ,/,' WITHOUT ADDITIONAL CUTS =',F10.4                   BRINI254
     . ,10X     ,/,'    WITH ADDITIONAL CUTS =',F10.4                   BRINI255
     . ,10X     ,/1X,50(1H-))                                           BRINI256
      ANPNOC=((EVNOCF(1)+EVNOCF(2))*2-(EVNOCT(1)+EVNOCT(2)))            BRINI257
     .          /(EVNOCT(1)+EVNOCT(2))                                  BRINI258
      ANPCUT=((EVCUTF(1)+EVCUTF(2))*2-(EVCUTT(1)+EVCUTT(2)))            BRINI259
     .          /(EVCUTT(1)+EVCUTT(2))                                  BRINI260
      WRITE (IOUT, 1023) ANPNOC,ANPCUT                                  BRINI261
 1023 FORMAT(1X,50(1H-),/,' THE UNPOLARIZED  ASYMMETRY IS:'             BRINI262
     . ,10X     ,/,' WITHOUT ADDITIONAL CUTS =',F10.4                   BRINI263
     . ,10X     ,/,'    WITH ADDITIONAL CUTS =',F10.4                   BRINI264
     . ,10X     ,/1X,50(1H-))                                           BRINI265
  900 CONTINUE                                                          BRINI266
      RETURN                                                            BRINI267
      END                                                               BRINI268
      SUBROUTINE USCJOB                                                 USCJOB 2
      COMMON/LUSTAT/ ICOULU(10)                                         USCJOB 3
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
C.......................................................................USCJOB 5
       IUT=IW(6)                                                        USCJOB 6
       WRITE(IUT,101)                                                   USCJOB 7
  101  FORMAT(//20X,'EVENTS STATISTICS',                                USCJOB 8
     &         /20X,'*****************')                                USCJOB 9
       WRITE(IUT,102) ICOULU(9)+ICOULU(10),ICOULU(10),ICOULU(9)         USCJOB10
  102  FORMAT(/5X,'# OF GENERATED EVENTS                = ',I10,        USCJOB11
     &        /5X,'# OF ACCEPTED  EVENTS                = ',I10,        USCJOB12
     &        /5X,'# OF REJECTED  EVENTS                = ',I10)        USCJOB13
       WRITE(IUT,103)                                                   USCJOB14
  103  FORMAT(//20X,'REJECT STATISTICS',                                USCJOB15
     &         /20X,'*****************')                                USCJOB16
       WRITE(IUT,104) (ICOULU(I),I=1,8)                                 USCJOB17
  104  FORMAT(/10X,'IR= 1 LUND ERROR unknown part    # OF REJECT =',I10,USCJOB18
     &        /10X,'IR= 2 BOS  ERROR KINE/VERT       # OF REJECT =',I10,USCJOB19
     &        /10X,'IR= 3 LUND ERROR too many tracks # OF REJECT =',I10,USCJOB20
     &        /10X,'IR= 4 LUND ERROR Beam wrong pos  # OF REJECT =',I10,USCJOB21
     &        /10X,'IR= 5 LUND ERROR status code >5  # OF REJECT =',I10,USCJOB22
     &        /10X,'IR= 6 USER reject in BRGENE      # OF REJECT =',I10,USCJOB23
     &        /10X,'IR= 7 free for user              # OF REJECT =',I10,USCJOB24
     &        /10X,'IR= 8 free for user              # OF REJECT =',I10)USCJOB25
      CALL BRSTAT                                                       USCJOB26
      RETURN                                                            USCJOB27
      END                                                               USCJOB28
      FUNCTION WKCORS(CSCAT)                                            WKCORS 2
C                                                                       WKCORS 3
C   A.BLONDEL  DEC 3 1987                                               WKCORS 4
C         MODIFICATION OF THE ORIGINAL VERSION OF BREM5 ACCORDING TO    WKCORS 5
C            P. RANKIN'S RECIEPE                                        WKCORS 6
C***************************                                            WKCORS 7
C  THIS FUNCTION RETURNS THE BORN + ONE-LOOP                            WKCORS 8
C  CROSS-SECTION IN GEV (NO PHOTONIC GRAPHS)                            WKCORS 9
      IMPLICIT LOGICAL (A-H,O-Z)                                        WKCORS10
      REAL*8 CSCAT,WKCORS                                               WKCORS11
C     POLX CONTAINS POLARISATION INFO FOR USE BY WKCORS                 WKCORS12
      COMMON/POLX/XPOL                                                  WKCORS13
      REAL*8 XPOL                                                       WKCORS14
      COMMON/GAUSPM/S,POLN,T3E,QE,T3M,QM,MSOUT,NCOL,NDIAG0,NDIAGA       WKCORS15
      REAL*8 S,POLN,T3E,QE,T3M,QM,MSOUT                                 WKCORS16
      REAL*8 XSECL,XSECR                                                WKCORS17
      POLN=XPOL                                                         WKCORS18
      CALL MATELM(S,CSCAT,T3E,QE,T3M,QM)                                WKCORS19
C=======================================                                WKCORS20
      CALL HELAMP(S,CSCAT)                                              WKCORS21
C=========================                                              WKCORS22
      CALL XSGEV(S,CSCAT,MSOUT,NDIAG0,NDIAGA,XSECL,XSECR)               WKCORS23
C========================================================               WKCORS24
C   A P P L Y   H E R E   C O R R E C T I O N                           WKCORS25
C    S U G G E S T E D    B Y     P. R A N K I N                        WKCORS26
C  I.E.:                                                                WKCORS27
      WKCORS= 0.5D0*((1.D0-POLN)*XSECL+(1.D0+POLN)*XSECR)               WKCORS28
C                                                                       WKCORS29
C INSTEAD OF:                                                           WKCORS30
C                                                                       WKCORS31
C     WKCORS=DFLOAT(NCOL)                                               WKCORS32
C    C      *0.5D0*((1.D0-POLN)*XSECL+(1.D0+POLN)*XSECR)                WKCORS33
C                                                                       WKCORS34
      RETURN                                                            WKCORS35
      END                                                               WKCORS36
