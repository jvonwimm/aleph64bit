      SUBROUTINE ARIAIN                                                 ARIAIN 2
C ------------------------------------------------------------------    ARIAIN 3
C - B. Bloch  - APRIL 1991                                              ARIAIN 4
C! Initialization routine of ARIADNE 3.3  generator                     ARIAIN 5
C ------------------------------------------------------------------    ARIAIN 6
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
C                                                                       LUN7CO12
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      COMMON /AROPTN/ IAR(10),KAR(10),VAR(10)                           ARIACOM2
      COMMON /TEST/ ISPLIC                                              ARIACOM3
      PARAMETER ( ICOZ = 23 , ICOH = 25 , ICOT = 6 )                    BBL002 1
      DIMENSION KFL(8)                                                  ARIAIN11
      DATA NIT/0/                                                       ARIAIN12
C  Set up some default values for masses and initial conditions         ARIAIN13
      CALL ARINIT                                                       ARIAIN14
C                                                                       ARIAIN15
      PMAS(LUCOMP(ICOH),1)= 100.                                        ARIAIN16
      PMAS(LUCOMP(ICOT),1)= 100.                                        ARIAIN17
      PMAS(LUCOMP(ICOZ),1)= 91.2                                        ARIAIN18
C   HIGGS Mass , TOP Mass and Z0 mass defined, can be overwritten by    ARIAIN19
C   a PMA1 card                                                         ARIAIN20
C select no fragmentation in AREEVT and LUEEVT                          ARIAIN21
      IAR(8)=0                                                          ARIAIN22
      MSTJ(105)=0                                                       ARIAIN23
C do not select final state photon emission in jetset                   BBL002 3
      MSTJ(41)=0                                                        BBL002 4
C select final state photon emission in ariadne                         ARIAIN26
C constant alpha_em                                                     ARIAIN27
      KAR(9)=1                                                          ARIAIN28
C or running alpha_em                                                   ARIAIN29
C      KAR(9)=2                                                         ARIAIN30
C select cutoff photon emission when secondary q-qb are produced        ARIAIN31
C      ISPLIC=1                                                         ARIAIN32
C or not                                                                ARIAIN33
      ISPLIC=0                                                          ARIAIN34
C select no setting of parameters in ariadne calls                      ARIAIN35
      IAR(6)=0                                                          ARIAIN36
C set parameters in jetset according to ALEPH fit                       ARIAIN37
      PARJ(81)=0.318                                                    ARIAIN38
      PARJ(82)=1.43                                                     ARIAIN39
      PARJ(21)=0.36                                                     ARIAIN40
      PARJ(41)=0.50                                                     ARIAIN41
      PARJ(42)=0.92                                                     ARIAIN42
C set parameters in ariadne according to Aleph fit                      ARIAIN43
      VAR(1)=0.318                                                      ARIAIN44
      VAR(3)=1.00                                                       ARIAIN45
C set cutoff in pt for photon emission to same as for gluon             ARIAIN46
      VAR(7)=VAR(3)                                                     ARIAIN47
C set constant alpha_em in ariadne                                      ARIAIN48
      VAR(8)=0.007297353                                                ARIAIN49
C                                                                       ARIAIN50
      CALL KXARCO(LAPAR)                                                ARIAIN51
      CALL HBOOK1(10022,' # OF GENERATED PHOTONS(ISR+FSR) PER EVENT',   ARIAIN52
     $    30,0.,30.,0.)                                                 ARIAIN53
      RETURN                                                            ARIAIN54
      ENTRY ARIAEV                                                      ARIAIN55
      MSTJ101=MSTJ(101)                                                 BBL002 5
      MSTJ(101)=5                                                       BBL002 6
      MSTJ41=MSTJ(41)                                                   BBL002 7
      MSTJ(41)=0                                                        BBL002 8
      MSTJ105=MSTJ(105)                                                 BBL002 9
      MSTJ(105)=0                                                       BBL00210
C----------------------------------------                               ARIAIN62
      CALL LUEDIT(12)                                                   ARIAIN63
      DO 10 I = 1,N7LU                                                  ARIAIN64
       IF (K7LU(I,1).EQ.2) GO TO 11                                     ARIAIN65
 10   CONTINUE                                                          ARIAIN66
 11   IAR(1) = I                                                        ARIAIN67
      IAR(2) = 0                                                        ARIAIN68
      IF (K7LU(N7LU,1).EQ.1) IAR(2) = N7LU                              ARIAIN69
C----------------------------------------                               ARIAIN70
      CALL ARIADNE                                                      ARIAIN71
      MSTJ(101)=MSTJ101                                                 BBL00211
      MSTJ(41)=MSTJ41                                                   BBL00212
      MSTJ(105)=MSTJ105                                                 BBL00213
      NPH = 0                                                           ARIAIN76
      NIT = NIT +1                                                      ARIAIN77
C     ECMS = ECM                                                        ARIAIN78
C count number of photons emitted                                       ARIAIN79
      DO 200 I=1,N7LU                                                   ARIAIN80
        IF(K7LU(I,2).EQ.22) NPH=NPH+1                                   ARIAIN81
200   CONTINUE                                                          ARIAIN82
      CALL HFILL(10022,FLOAT(NPH),0.,1.)                                ARIAIN83
C do fragmentation                                                      ARIAIN84
      CALL LUEXEC                                                       ARIAIN85
      IF (NIT.LE.5) CALL LULIST(1)                                      ARIAIN86
      RETURN                                                            ARIAIN87
      END                                                               ARIAIN88
      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)                 ASKUSE 2
C--------------------------------------------------------------------   ASKUSE 3
C      B. Bloch -Devaux November 1990 IMPLEMENTATION OF HVFL02          ASKUSE 4
C!   Steering of HVFL various steps                                     ASKUSE 5
C     structure : subroutine                                            ASKUSE 6
C                                                                       ASKUSE 7
C     input     : none                                                  ASKUSE 8
C                                                                       ASKUSE 9
C     output    : 6 arguments                                           ASKUSE10
C          IDPR   : process identification if meaningful                ASKUSE11
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
C                                                                       LUN7CO12
      COMMON / MASTER / IGENE,IWEIT,IMIX,IVBU,IFL,ECM,IEV1,IEV2,IEFLOP, MASTER 2
     $          ISEM,IDEC,WTMAX,SVRT(3),ICOULU(10),IPHO,ILAM,IP17,IDDC  BBL00914
      COMMON/HVFPRNT/IPPRI                                              BBL008 6
      PARAMETER (LBPOS=100,NBU=521,NBD=511,NBS=531,NBC=541)             BCODES 2
      PARAMETER (NLB=5122,NXB= 5132)                                    BBL00511
C     LBPOS  = internal JETSET 7.3 code for B meson                     BCODES 3
      PARAMETER (NXB0=5232,NOB= 5332)                                   BBL007 9
C     NBU    = internal JETSET 7.3 code for Bu meson                    BCODES 4
C     NBD    = internal JETSET 7.3 code for Bd meson                    BCODES 5
C     NBS    = internal JETSET 7.3 code for Bs meson                    BCODES 6
C     NBC    = internal JETSET 7.3 code for Bc meson                    BCODES 7
C     NLB    = internal JETSET 7.3 code for /\B BARYON                  BBL00512
C     NXB    = internal JETSET 7.3 code for XIB BARYON                  BBL00513
C     NXB0   = internal JETSET 7.3 code for XIB0 BARYON                 BBL00710
C     NOB    = internal JETSET 7.3 code for OMEGAB BARYON               BBL00711
      PARAMETER (NDU=421,NDD=411,NDS=431)                               DCODES 2
      PARAMETER (NLC=4122,NXC= 4132)                                    DCODES 3
C     NDU    = internal JETSET 7.3 code for Du meson                    DCODES 4
C     NDD    = internal JETSET 7.3 code for Dd meson                    DCODES 5
C     NDS    = internal JETSET 7.3 code for Ds meson                    DCODES 6
C     NLC    = internal JETSET 7.3 code for /\C BARYON                  DCODES 7
C     NXC    = internal JETSET 7.3 code for XIC BARYON                  DCODES 8
      INTEGER ALTABL,NAMIND                                             ASKUSE26
      EXTERNAL ALTABL,NAMIND                                            ASKUSE27
      COMMON/ZDUMP/IDEBU,NEVT                                           ASKUSE28
      DIMENSION VERT(4),TABL(3)                                         ASKUSE29
      DIMENSION ZB(LJNPAR)                                              ASKUSE30
      DATA NIT/0/,WM /0./                                               ASKUSE31
C  Reset ZB storage  and entries in /LUJETS/                            BBL00113
      CALL VZERO(ZB,LJNPAR)                                             BBL00114
      N7LU = 0                                                          BBL00115
C Reset fragmentation storage in common                                 ASKUSE34
      MSTU(90) = 0                                                      ASKUSE35
      NIT=NIT+1                                                         ASKUSE36
C  DISABLE B mesons decay                                               ASKUSE37
      DO 10 KF = NBD,NBC,10                                             ASKUSE38
         KC = LUCOMP(KF)                                                ASKUSE39
         IF (KC.GT.0) MDCY(KC,1) = 0                                    ASKUSE40
  10  CONTINUE                                                          ASKUSE41
C  DISABLE D mesons decay                                               BBL00518
      DO 12 KF = NDD,NDS,10                                             BBL00519
      DO 12 KL = 1,5,2                                                  BBL00520
         KC = LUCOMP(KF+KL-1)                                           BBL00521
         IF (KC.GT.0) MDCY(KC,1) = 0                                    BBL00522
  12  CONTINUE                                                          BBL00523
      KCI = LUCOMP(413)                                                 BBL00524
C  Disable B baryons decay and D baryons decays                         BBL00525
      DO 11 KE = 4000,5000,1000                                         BBL00526
      DO 11 KF = 100,300,100                                            BBL00527
      DO 11 KL = 10,30,10                                               ASKUSE44
      DO 11 KM = 2,4,2                                                  ASKUSE45
         KC = LUCOMP(KE+KF+KL+KM)                                       BBL00528
         IF (KC.GT.0) MDCY(KC,1) = 0                                    ASKUSE47
  11  CONTINUE                                                          ASKUSE48
      IF (IGENE.EQ.-1) THEN                                             ASKUSE49
        CALL SIGENE(IDPR,ISTAT,ECMS)                                    ASKUSE50
        WEIT = 1.                                                       ASKUSE51
      ELSEIF ( IGENE.EQ.1.OR. IGENE.EQ.11) THEN                         BBL00116
        ECMS = ECM                                                      ASKUSE53
        WEIT = 1.                                                       ASKUSE54
        ISTAT = 0                                                       ASKUSE55
        CALL LUGENE(IFL,ECM,IDPR)                                       ASKUSE56
        IDPR = 1000*IDPR                                                ASKUSE57
      ELSE IF (IGENE.EQ.3 .OR. IGENE.EQ.13) THEN                        BBL00117
        IF ( NIT.GT.5 ) IDEBU =0                                        BBL00118
        CALL DYMUEV(IDPR,ISTAT,ECMS,WEIT)                               BBL00119
        IDPR = IDPR-10000                                               BBL00120
      ELSE IF (IGENE.EQ.2.OR. IGENE.EQ.12) THEN                         BBL00121
        IF(NIT.EQ.IEFLOP) CALL BRFLOP                                   BBL00122
  15    CALL BREMEV(IDPR,ISTAT,ECMS,WEIT)                               BBL00123
        IDPR = IDPR-10000                                               BBL00124
        WM = MAX (WM,WEIT)                                              BBL00125
        IF (WM.GT.1..AND.NIT.LE.20) WRITE (IW(6),'(1X,''Max weight reachBBL00126
     &ed'',F10.4)')   WM                                                BBL00127
        IF (IWEIT.EQ.0) THEN                                            BBL00128
           IF(WEIT.GT.WTMAX.AND.NIT.LE.20) WRITE(IW(6),555) WEIT,WTMAX  BBL00129
 555    FORMAT (1X,'+++++WARNING!!!!!!! THE ACTUAL WEIGHT IS ',F10.6,'  BBL00130
     $ THE MAXIMUM WEIGHT WAS SET TO ',F10.6)                           BBL00131
           IF (WEIT/WTMAX.LT.RNDM(DUM)) THEN                            BBL00132
             ICOULU(9)= ICOULU(9)+1                                     BBL00133
             ICOULU(6)= ICOULU(6)+1                                     BBL00134
             N7LU = 0                                                   BBL00135
             IF (NIT.LT.5) WRITE(IW(6),'(1X,''++EVENT TRIED'',I5,''.REJEBBL00136
     $CTED FOR WEIGHT (WTMAX)'',2F10.4)') NIT,WEIT,WTMAX                BBL00137
             GO TO 15                                                   BBL00138
           ENDIF                                                        BBL00139
           WEIT = 1.                                                    BBL00140
        ENDIF                                                           BBL00141
      ENDIF                                                             BBL00142
      IF (IGENE.GT.10) CALL ARIAEV                                      BBL00143
      CALL FILBOK(WEIT)                                                 BBL00144
C                                                                       ASKUSE83
C  Generate vertex postion                                              ASKUSE84
C                                                                       ASKUSE85
      CALL RANNOR(RX,RY)                                                ASKUSE86
      CALL RANNOR(RZ,DUM)                                               ASKUSE87
      VERT(1)=RX*SVRT(1)                                                ASKUSE88
      VERT(2)=RY*SVRT(2)                                                ASKUSE89
      VERT(3)=RZ*SVRT(3)                                                ASKUSE90
      VERT(4)=0.                                                        ASKUSE91
C                                                                       ASKUSE92
      IFMI = 0                                                          ASKUSE93
      IFVB = 0                                                          ASKUSE94
      IF (IMIX.GT.0) CALL LUNMIX(IFMI)                                  ASKUSE95
C    REACTIVATE B meson decays                                          ASKUSE96
      DO 20 KF = NBD,NBC,10                                             ASKUSE97
         KC = LUCOMP(KF)                                                ASKUSE98
         IF (KC.GT.0) MDCY(KC,1) = 1                                    ASKUSE99
  20  CONTINUE                                                          ASKUS100
C  REACTIVATE B baryons decay                                           ASKUS101
C  except if special decay required                                     BBL00719
      DO 21 KF = 5100,5300,100                                          ASKUS102
      DO 21 KL = 10,30,10                                               ASKUS103
      DO 21 KM = 2,4,2                                                  ASKUS104
         KC = LUCOMP(KF+KL+KM)                                          ASKUS105
         KBAR = KF+KL+KM                                                BBL00720
         IF ( ILAM.GT.0 .AND.(KBAR.EQ.NLB .OR. KBAR.EQ.NXB .OR. KBAR.EQ.BBL00721
     $        NXB0 .OR. KBAR.EQ.NOB ) ) GO TO 21                        BBL00722
         IF (KC.GT.0) MDCY(KC,1) = 1                                    ASKUS106
  21  CONTINUE                                                          ASKUS107
      IF (IDEC.GT.0) CALL LUNDEC(IFDC)                                  ASKUS108
      IF (IVBU.GT.0) CALL LUNVBU(IFVB)                                  ASKUS109
      IDPR = IDPR+10*IFMI+100*IFVB                                      ASKUS110
      EE = FLOAT(IFVB)                                                  ASKUS111
      FF = FLOAT(IFMI)                                                  ASKUS112
      CALL HFILL(10030,EE,FF,1.)                                        ASKUS113
C    LETS LUND FINISH                                                   ASKUS114
      IF (N7LU.GT.0) CALL LUZETA(ZB)                                    ASKUS115
      CALL LUEXEC                                                       ASKUS116
C     look for special b baryons decays if requested                    BBL00723
      IF(ILAM.GT.0) CALL LULAMB                                         BBL00724
C     look for semileptonic B decays if requested  ( and internal brem) BBL00529
      IF(ISEM.GT.0) CALL LUNSEM                                         BBL00530
C     apply internal Brem to semi-lep B decays if not yet done          BBL00531
      IF(IPHO.GT.0 .AND. ISEM.EQ.0 ) CALL LUNPHO(5)                     BBL00532
      IF(IPHO.GT.0) CALL LUNPHO(15)                                     BBL00533
C    REACTIVATE D meson decays                                          BBL00534
      DO 22 KF = NDD,NDS,10                                             BBL00535
      DO 22 KL = 1,5,2                                                  BBL00536
         KC = LUCOMP(KF+KL-1)                                           BBL00537
         IF (KC.GT.0) MDCY(KC,1) = 1                                    BBL00538
  22  CONTINUE                                                          BBL00539
C  REACTIVATE D baryons decay                                           BBL00540
      DO 23 KF = 4100,4300,100                                          BBL00541
      DO 23 KL = 10,30,10                                               BBL00542
      DO 23 KM = 2,4,2                                                  BBL00543
         KC = LUCOMP(KF+KL+KM)                                          BBL00544
         IF (KC.GT.0) MDCY(KC,1) = 1                                    BBL00545
  23  CONTINUE                                                          BBL00546
C  Select special chain if needed                                       BBL00916
      IF (IDDC.GT.0) CALL LUNDDC(IFDD)                                  BBL00917
      CALL LUEXEC                                                       BBL00548
      IF(IPHO.GT.0) CALL LUNPHO(4)                                      BBL00549
      IF(IPHO.GT.0) CALL LUNPHO(14)                                     BBL00550
      CALL LUEXEC                                                       BBL00551
      IF ( IPPRI/10 .GT.0) THEN                                         BBL008 7
         CALL LUTABU(11)                                                BBL008 8
         CALL LUTABU(21)                                                BBL008 9
         IF (IGENE.EQ.-1) CALL LUTABU(51)                               BBL00810
      ENDIF                                                             BBL00811
C   That's it !......                                                   BBL00552
      CALL KXL7MI(VERT,IST,NVRT,NTRK)                                   ASKUS120
      IF (IST.NE.0) THEN                                                ASKUS121
         WRITE(IW(6),'('' ++++WARNING PROBLEME AT EVENT '',I10,'' IER ''ASKUS122
     $   ,I8)') NIT , IST                                               ASKUS123
         CALL LULIST(1)                                                 ASKUS124
      ENDIF                                                             ASKUS125
C   Set the ZB value according to KINE numbering, i.e. remove beam part.ASKUS126
C   and transmit z of mother to subsequent heavy baryons and mesons     ASKUS127
      IBEA = 0                                                          ASKUS128
      DO 27 ITR=1,N7LU                                                  ASKUS129
      KS = K7LU(ITR,1)                                                  ASKUS130
      KM = K7LU(ITR,3)                                                  ASKUS131
C  Give same z to all daughters  of a mother                            ASKUS132
      IF (KM.GT.IBEA .AND. ZB(KM-IBEA).GT.0. ) ZB(ITR) = ZB(KM-IBEA)    ASKUS133
      IF ( KS.EQ.21 .AND. ABS(K7LU(ITR,2)).EQ.11 ) THEN                 BBL00410
       IBEA = IBEA +1                                                   ASKUS135
      ELSE                                                              ASKUS136
       ZB(ITR-IBEA) = ZB (ITR)                                          ASKUS137
      ENDIF                                                             ASKUS138
 27   CONTINUE                                                          ASKUS139
C                                                                       ASKUS140
C   Book & fill the bank KZFR with info on fragmentation                ASKUS141
C                                                                       ASKUS142
      NP = N7LU-IBEA                                                    ASKUS143
      JKZFR = ALTABL('KZFR',1,NP,ZB,'2I,(F)','E')                       ASKUS144
      IF(JKZFR.LE.0) ISTAT = -1                                         ASKUS145
      IF (MSTU(24).GT.0) THEN                                           ASKUS146
        IST   = -8                                                      ASKUS147
        CALL LULIST(1)                                                  ASKUS148
      ENDIF                                                             ASKUS149
      IF ( IDDC.GT.0) THEN                                              BBL00918
         IF ( IFDD .EQ.0) ISTAT = -2                                    BBL00919
      ENDIF                                                             BBL00920
      IF (IST.EQ.0 .AND. ISTAT.EQ.0) THEN                               ASKUS150
         ICOULU(10) = ICOULU(10)+1                                      ASKUS151
      ELSEIF (IST.GT.0) THEN                                            ASKUS152
         ICOULU(1) = ICOULU(1) +1                                       ASKUS153
         ICOULU(9) = ICOULU(9) +1                                       ASKUS154
      ELSEIF ( IST.LT.0) THEN                                           ASKUS155
         ICOULU(-IST) = ICOULU(-IST) +1                                 ASKUS156
         ICOULU(9) = ICOULU(9) +1                                       ASKUS157
      ELSEIF ( ISTAT.GT.0) THEN                                         ASKUS158
         ICOULU(6) = ICOULU(6) +1                                       ASKUS159
         ICOULU(9) = ICOULU(9) +1                                       ASKUS160
      ELSEIF ( ISTAT.EQ.-1) THEN                                        ASKUS161
         ICOULU(7) = ICOULU(7) +1                                       ASKUS162
         ICOULU(9) = ICOULU(9) +1                                       ASKUS163
      ELSEIF ( ISTAT.EQ.-2) THEN                                        BBL00921
         ICOULU(8) = ICOULU(8) +1                                       BBL00922
         ICOULU(9) = ICOULU(9) +1                                       BBL00923
      ENDIF                                                             ASKUS164
C                                                                       ASKUS165
      ISTA = IST+ISTAT*1000                                             ASKUS166
      EE = KLU(0,2)                                                     BBL00145
      CALL HFILL(10007,EE,DUM,WEIT)                                     BBL00146
      RETURN                                                            ASKUS167
      END                                                               ASKUS168
      SUBROUTINE ASKUSI(IGCOD)                                          ASKUSI 2
C--------------------------------------------------------------------   ASKUSI 3
C      B.Bloch-Devaux November 90 IMPLEMENTATION OF HVFL02              ASKUSI 4
C                                                                       ASKUSI 5
C!   Steering of HVFL initialisation steps                              ASKUSI 6
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
C                                                                       LUN7CO12
      PARAMETER (LNBRCP=4)                                              USERCP 2
      COMMON/USERCP/NPARCP,JCODCP(LNBRCP),RHOCPM                        USERCP 3
      COMPLEX RHOCPM                                                    USERCP 4
      COMPLEX PSQD,PSQS                                                 MIXING 2
      COMMON/ MIXING /XD,YD,CHID,RD,XS,YS,CHIS,RS,PSQD,PSQS             MIXING 3
      COMMON/ PROBMI /CDCPBA,CDCPB,CSCPBA,CSCPB                         MIXING 4
      COMMON / MASTER / IGENE,IWEIT,IMIX,IVBU,IFL,ECM,IEV1,IEV2,IEFLOP, MASTER 2
     $          ISEM,IDEC,WTMAX,SVRT(3),ICOULU(10),IPHO,ILAM,IP17,IDDC  BBL00914
      COMMON/HVFPRNT/IPPRI                                              BBL00812
      DIMENSION TABL(20)                                                BBL00553
      PARAMETER ( IGCO = 5023)                                          BBL00813
C                                                                       ASKUSI20
      PARAMETER (LPDEC=48)                                              ASKUSI21
      INTEGER NODEC(LPDEC)                                              ASKUSI22
      INTEGER ALTABL,ALRLEP                                             ASKUSI23
      EXTERNAL ALTABL ,ALRLEP                                           ASKUSI24
      CHARACTER*10 NAME(20),SINGN                                       ASKUSI25
      CHARACTER*30 DATE                                                 ASKUSI26
      DATA FVERS/                                                       ASKUSI27
     $1.06                                                              BBL00915
     $/                                                                 ASKUSI29
      DATA DATE/                                                        ASKUSI30
     $'March    31, 1994'                                               BBL00913
     $/                                                                 ASKUSI32
      DATA NAME/'LUND 7.3  ','BREM5     ','DYMU3     ',                 BBL00147
     $          7*' ',                                                  BBL00148
     $           'NO WEIGHT ','WEIGHTED  ','JETSET7.3',                 BBL00149
     $           'ARIADNE3.3',6*' '/                                    BBL00150
      DATA SINGN/'SINGLE GEN'/                                          ASKUSI36
C                                                                       ASKUSI37
C   RETURN THE GENERATOR CODE                                           ASKUSI38
C                                                                       ASKUSI39
      IGCOD=IGCO                                                        ASKUSI40
C RESET ERROR COUNTERS                                                  ASKUSI41
      DO 5 K=1,10                                                       ASKUSI42
 5    ICOULU(K)=0                                                       ASKUSI43
      JHVF=IW(NAMIND('GHVF'))                                           ASKUSI44
      WTMAX = 1.E-10                                                    ASKUSI45
      IF(JHVF.GT.0)THEN                                                 ASKUSI46
         IGENE=IW(JHVF+1)                                               ASKUSI47
         IWEIT=IW(JHVF+2)                                               ASKUSI48
         WTMAX=RW(JHVF+3)                                               ASKUSI49
         IPPART=IW(JHVF+4)                                              ASKUSI50
       ELSE                                                             ASKUSI51
C default is JETSET 7.3 without weights                                 ASKUSI52
         IGENE=1                                                        ASKUSI53
         IWEIT=0                                                        ASKUSI54
         WTMAX=1.02                                                     ASKUSI55
         IPPART=0                                                       ASKUSI56
       ENDIF                                                            ASKUSI57
       IPPRI = IPPART                                                   BBL00814
C                                                                       ASKUSI58
C   Issue the relevant parameters                                       ASKUSI59
C                                                                       ASKUSI60
      IUT = IW(6)                                                       ASKUSI61
      WRITE (IUT,1000) IGCOD                                            ASKUSI62
      WRITE(IUT,999) FVERS,DATE                                         ASKUSI63
      IF (IGENE.GT.0) THEN                                              ASKUSI64
        IF ( IGENE.LT.10 ) THEN                                         BBL00151
           WRITE (IUT,1001) IGENE,NAME(IGENE)                           BBL00152
           WRITE (IUT,1003) NAME(13)                                    BBL00153
        ELSE IF (IGENE.GT.10) THEN                                      BBL00154
           WRITE (IUT,1001) IGENE-10,NAME(IGENE-10)                     BBL00155
           WRITE (IUT,1003) NAME(14)                                    BBL00156
        ENDIF                                                           BBL00157
      ELSE                                                              ASKUSI66
        WRITE (IUT,1001) IGENE,SINGN                                    ASKUSI67
      ENDIF                                                             ASKUSI68
      WRITE (IUT,1002) IWEIT,NAME(IWEIT+11),WTMAX                       ASKUSI69
 999  FORMAT(30X,'*  ','VERSION ',F6.2,'-LAST MODIFIED ON   ',A30)      ASKUSI70
 1000 FORMAT(30X,78('*'),/,40X,'WELCOME TO THE HEAVY FLAVOR MC HVFL04'  BBL00815
     $    ,/,70X,'GENERATOR CODE IS :',I10)                             ASKUSI72
 1001 FORMAT (30X,'*  YOU CHOOSE GENERATOR #',I5,1X,A10)                ASKUSI73
 1003 FORMAT (30X,'*  YOU CHOOSE AS FINAL STATE RADIATION:   ',A10)     BBL00158
 1002 FORMAT (30X,'*  WEIGHT FLAG IS',I5,1X,A10,' MAX WEIGHT =',F10.5,/,ASKUSI74
     $30X,'*    IRRELEVANT FOR LUND',/,30X,78('*'))                     ASKUSI75
       JTRIG=NLINK('TRIG',0)                                            ASKUSI76
       IF ( JTRIG.GT.0) THEN                                            ASKUSI77
          IEV1=IW(JTRIG+1)                                              ASKUSI78
          IEV2=IW(JTRIG+2)                                              ASKUSI79
       ELSE                                                             ASKUSI80
          IEV1 = 1                                                      ASKUSI81
          IEV2 = 1000                                                   ASKUSI82
       ENDIF                                                            ASKUSI83
       IEFLOP =(IEV2-IEV1+1)/2 +1                                       ASKUSI84
C  if you need the standard interaction point                           ASKUSI85
C  you may get the sigmas of the gaussion smearing                      ASKUSI86
C  from a data card if you like it                                      ASKUSI87
C  SVRT   SIGMAX  SIGMAY  SIGMAZ                                        ASKUSI88
C                                                                       ASKUSI89
        NASVRT=NAMIND('SVRT')                                           ASKUSI90
        JSVRT=IW(NASVRT)                                                ASKUSI91
        IF (JSVRT.NE.0) THEN                                            ASKUSI92
           SVRT(1)=RW(JSVRT+1)                                          ASKUSI93
           SVRT(2)=RW(JSVRT+2)                                          ASKUSI94
           SVRT(3)=RW(JSVRT+3)                                          ASKUSI95
        ELSE                                                            ASKUSI96
           SVRT(1)=0.0110                                               BBL00924
           SVRT(2)=0.0005                                               BBL00925
           SVRT(3)=0.7000                                               BBL00926
        ENDIF                                                           ASKUS100
C                                                                       ASKUS101
C     Necessary to keep info on fragmentation                           ASKUS102
C                                                                       ASKUS103
      MSTU(17) = 1                                                      ASKUS104
C                                                                       ASKUS105
C  Set up some default values for masses and initial conditions         ASKUS106
C   Higgs, top, Z0 masses ...4th generation masses                      ASKUS107
      PMAS(LUCOMP(25),1)= 100.                                          ASKUS108
      PMAS(LUCOMP( 6),1)= 100.                                          ASKUS109
      PMAS(LUCOMP(23),1)= 91.2                                          ASKUS110
      PMAS(LUCOMP( 7),1)= 150.                                          ASKUS111
      PMAS(LUCOMP( 8),1)= 200.                                          ASKUS112
C   store beam electrons  and Z0                                        BBL00159
      MSTJ(115) = 3                                                     BBL00160
C   initial state radiation                                             ASKUS115
      MSTJ(107)   = 1                                                   ASKUS116
C   Final   state radiation                                             ASKUS117
      MSTJ( 41)   = 2                                                   ASKUS118
C  use non discrete masses for resonnances                              ASKUS119
      MSTJ( 24) =  2                                                    ASKUS120
C   SLAC fragm. functions for b,c  Symetric LUND for u,d,s              ASKUS121
      MSTJ( 11)   = 3                                                   ASKUS122
C   mod to lund fragm. functions params                                 ASKUS123
      PARJ ( 21)  =0.358                                                ASKUS124
      PARJ  (41)  =0.500                                                ASKUS125
      PARJ  (42)  =0.840                                                ASKUS126
      PARJ  (81)  =0.310                                                ASKUS127
      PARJ  (82)  =1.500                                                ASKUS128
C  mod Peterson's fragm. functions params                               ASKUS129
      PARJ  (54)  = -0.200                                              ASKUS130
      PARJ  (55)  = -0.006                                              ASKUS131
      PARU (102)  =  0.232                                              ASKUS132
      PARJ (123)  =  91.17                                              ASKUS133
      PARU (124)  =  2.5                                                ASKUS134
C    book some general histos                                           BBL00161
       CALL INIBOK                                                      BBL00162
C    Init generator                                                     ASKUS135
       IF     (IGENE.EQ.1 .OR. IGENE.EQ.11) THEN                        BBL00163
          CALL INILUN(IFLV,ECMS)                                        ASKUS137
       ELSEIF (IGENE.EQ.2 .OR. IGENE.EQ.12) THEN                        BBL00164
          CALL INIBRE(IFLV,ECMS)                                        ASKUS139
       ELSEIF (IGENE.EQ.-1) THEN                                        ASKUS140
          CALL INISIN(IFLV,ECMS)                                        ASKUS141
       ELSEIF (IGENE.EQ.3 .OR. IGENE.EQ.13) THEN                        BBL00165
          CALL INIDY3(IFLV,ECMS)                                        ASKUS143
       ENDIF                                                            ASKUS144
       ECM =ECMS                                                        ASKUS145
       IFL =IFLV                                                        ASKUS146
       IF ( ECM .LT.0.) CALL LUKFDI(KFL1,KFL2,KFL3,KF)                  BBL00816
C EXTEND PART BANK                                                      ASKUS147
       CALL KXL7PA(IPART,IKLIN)                                         ASKUS148
       IF (IPART.LE.0 .OR. IKLIN.LE.0) THEN                             ASKUS149
         WRITE (IW(6),'(1X,''error in PART or KLIN bank -RETURN- ''     ASKUS150
     +                 ,2I3)') IPART,IKLIN                              ASKUS151
         GOTO 20                                                        ASKUS152
       ENDIF                                                            ASKUS153
       IF (IGENE.GT.10) CALL INIARI(IFLV,ECMS)                          BBL00214
       CALL INIVBU(IFVBU)                                               ASKUS154
       CALL INIMIX(IFMIX)                                               ASKUS155
       CALL INISEM(IFSEM)                                               ASKUS156
       CALL INIDEC(IFDEC)                                               ASKUS157
       CALL INIDDC(IFDDC)                                               BBL00927
       CALL INIPHO(IFPHO)                                               BBL00554
       CALL INILAM(IFLAM)                                               BBL00725
       CALL INIP17(IFP17)                                               BBL00726
       IMIX = IFMIX                                                     ASKUS158
       ISEM=IFSEM                                                       ASKUS159
       IPHO=IFPHO                                                       BBL00555
       IVBU = IFVBU                                                     ASKUS160
       ILAM = IFLAM                                                     BBL00727
       IP17 = IFP17                                                     BBL00728
       IDEC = IFDEC                                                     ASKUS161
       IDDC = IFDDC                                                     BBL00928
C                                                                       ASKUS162
C   Inhibit decays                                                      ASKUS163
C                                                                       ASKUS164
      MXDEC=KNODEC(NODEC,LPDEC)                                         ASKUS165
      MXDEC=MIN(MXDEC,LPDEC)                                            ASKUS166
      IF (MXDEC.GT.0) THEN                                              ASKUS167
         DO 10 I=1,MXDEC                                                ASKUS168
            IF (NODEC(I).GT.0) THEN                                     ASKUS169
               JIDB = NLINK('MDC1',NODEC(I))                            ASKUS170
               IF (JIDB .EQ. 0) MDCY(LUCOMP(NODEC(I)),1) = 0            ASKUS171
            ENDIF                                                       ASKUS172
   10    CONTINUE                                                       ASKUS173
      ENDIF                                                             ASKUS174
C                                                                       ASKUS175
C  Print PART and KLIN banks                                            ASKUS176
C                                                                       ASKUS177
      IF ( MOD(IPPART,10).GT.0) THEN                                    BBL00929
         CALL LULIST(12)                                                ASKUS179
         CALL PRPART                                                    ASKUS180
      ELSE                                                              BBL00818
         CALL LUTABU(10)                                                BBL00819
         CALL LUTABU(20)                                                BBL00820
         IF (IGENE.EQ.-1) CALL LUTABU(50)                               BBL00821
      ENDIF                                                             ASKUS181
C   BUILD KHVF BANK                                                     ASKUS182
      TABL(1) = IGENE                                                   ASKUS183
      TABL(2) = IFL                                                     ASKUS184
      TABL(3) = IWEIT                                                   ASKUS185
      TABL(4) = IMIX                                                    ASKUS186
      TABL(5) = IVBU                                                    ASKUS187
      TABL(6) = ECMS                                                    ASKUS188
      TABL(7) = SVRT(1)                                                 ASKUS189
      TABL(8) = SVRT(2)                                                 ASKUS190
      TABL(9) = SVRT(3)                                                 ASKUS191
      TABL(10) = ISEM                                                   ASKUS192
      TABL(11) = FVERS*100.                                             ASKUS193
      TABL(12) = IPHO                                                   BBL00556
      TABL(13) = ILAM                                                   BBL00729
      TABL(14) = IP17                                                   BBL00730
      NWB = 14                                                          BBL00731
      IND = ALTABL('KHVF',NWB,1,TABL,'2I,(F)','C')                      ASKUS195
      CALL PRTABL('KHVF',0)                                             ASKUS196
C  Fill RLEP bank                                                       ASKUS197
      IEBEAM = NINT(ECMS* 500  )                                        ASKUS198
      JRLEP = ALRLEP(IEBEAM,'    ',0,0,0)                               ASKUS199
      CALL PRTABL('RLEP',0)                                             ASKUS200
   20 RETURN                                                            ASKUS201
      END                                                               ASKUS202
      SUBROUTINE BREMIN(NTYP)                                           BREMIN 2
C-----------------------------------------------------------------------BREMIN 3
C    B.Bloch April 1991                                                 BREMIN 4
C         ORIGINAL VERSION OF BREM5 AS PROVIDED BY ROBIN STUART         BREMIN 5
C                       MARCH 1987                                      BREMIN 6
C        HERE IS THE DRIVING PROGRAM ONLY, SOURCE IS IN BREM5 FORTRAN   BREMIN 7
C        THE LUND INTERFACE CAN BE COMMON TO ALL THIS TYPE OF GENERATORSBREMIN 8
C          IT IS THE LUND SHOWER TYPE EXCLUSIVELY                       BREMIN 9
C       The original version has been modified to branch to JETSET 7.3  BREMIN10
C-----------------------------------------------------------------------BREMIN11
C NEW IMPROVED VERSION OF BREMMUS, KLEISS/STUART/LYNN                   BREMIN12
C                                                                       BREMIN13
C IN THIS PROGRAM POLARIZATION XPOL IS FOR INITIAL STATE ELECTRON  ONLY.BREMIN14
C UNPOLARIZED CROSS SECTION IS LEFT+RIGHT, POLARIZATION CROSS SECTION ISBREMIN15
C LEFT-RIGHT. TOTAL CROSS SECTION IS  (LEFT+RIGHT)-XPOL*(LEFT-RIGHT)    BREMIN16
C NOTE MODIFICATION OF EFFECTIVE POLARIZATION FOR HARD BREM.            BREMIN17
C                                                                       BREMIN18
C THIS MAIN ROUTINE DOES THE SETUP, EVENT GENERATION LOOP, AND          BREMIN19
C OUTPUTS THE FINAL RESULTS                                             BREMIN20
C                                                                       BREMIN21
      IMPLICIT REAL*8(A-H,O-Z)                                          BREMIN22
      COMMON/INDAT/RDATIN(45)                                           BREMCOM2
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
C                                                                       BREMIN25
C RDATIN CONTAINS THE INPUT PARAMETERS SET-UP IN BREM2 INPUT            BREMIN26
C                                                                       BREMIN27
C RDATIN(4)        MIN MU POLAR ANGLE                                   BREMIN28
C RDATIN(5)        MAX MU POLAR ANGLE                                   BREMIN29
C RDATIN(6)        MAX HARD PHOTON ENERGY/BEAM ENERGY                   BREMIN30
C RDATIN(7)        MIN MUON ENERGY (often E/2)                          BREMIN31
C RDATIN(8)        MIN ANIT-MUON ENERGY                                 BREMIN32
C RDATIN(9)        ACOLINEARITY CUT ANGLE                               BREMIN33
C RDATIN(17)       POLARIZATION INITIAL STATE ELECTRON                  BREMIN34
C RDATIN(18)       NUMBER OF FINAL STATE COLORS                         BREMIN35
C RDATIN(19)       FINAL FERMION MASS                                   BREMIN36
C RDATIN(20)       NO. OF TREE DIAGRAMS                                 BREMIN37
C RDATIN(21)       NO. OF 1 LOOP DIAGRAMS                               BREMIN38
C RDATIN(22)       SWITCH TO TURN HISTOGRAMS ON & OFF                   BREMIN39
C RDATIN(23)       RANDOM NUMBER GENERATOR SEED                         BREMIN40
C RDATIN(24)       TEST PARAMETER 1= FINAL STATE RAD,0= NONE            BREMIN41
C      THE FOLLOWING  PARAMETERS ARE NOT USED NORMALLY                  BREMIN42
C RDATIN(25)       NUMBER OF ECM POINTS                                 BREMIN43
C RDATIN(26)       STEP SIZE IN EBEAM  FOR ECM SCAN                     BREMIN44
C                                                                       BREMIN45
C                                                                       BREMIN46
C  COMMON CONSTS CONTAINS FREQUENTLY USED CONSTANTS......               BREMIN47
C                                                                       BREMIN48
      COMMON/CONSTS/PI,ALFA1,CV1,CV2,CA1,CA2,DEGRAD,RADDEG              BREMIN49
C                                                                       BREMIN50
      DIMENSION PP(4),PM(4),QP(4),QM(4),QK(4)                           BREMIN51
      DIMENSION RESULT(2),ANOC(2),FINAL(2),ACUT(2)                      BREMIN52
      DIMENSION EVNOCF(2),EVNOCT(2),EVCUTF(2),EVCUTT(2)                 BREMIN53
      LOGICAL HISTPR                                                    BREMIN54
      REAL*4 ECMS,WEIT,PAUX(4),DUM,EE,PAUX2(4)                          BREMIN55
C                                                                       BREMIN56
C                                                                       BREMIN57
C RDATIN CONTAINS THE INPUT PARAMETERS SET-UP BY DEFAULT AND            BREMIN58
C EVENTUALLY MODIFIED BY USER'S INPUT CARDS                             BREMIN59
C                                                                       BREMIN60
C                                                                       BREMIN61
       PI=3.1415926535897932D0                                          BREMIN62
       ALFA1=7.297351D-3                                                BREMIN63
       RADDEG=0.017453293D0                                             BREMIN64
       DEGRAD=57.29577951D0                                             BREMIN65
       ITYPE = NTYP                                                     BREMIN66
       IOUT = IW(6)                                                     BREMIN67
C                                                                       BREMIN68
       ISEED = RDATIN(23)                                               BREMIN69
C                                                                       BREMIN70
       EB=RDATIN(2)                                                     BREMIN71
       XMZ=RDATIN(3)                                                    BREMIN72
       THMIN=RDATIN(4)                                                  BREMIN73
       THMAX=RDATIN(5)                                                  BREMIN74
       XKMAX=RDATIN(6)                                                  BREMIN75
       EMMIN=RDATIN(7)                                                  BREMIN76
       EMPLS=RDATIN(8)                                                  BREMIN77
       ACOL=RDATIN(9)                                                   BREMIN78
       WRITE(IOUT,1533) ITYPE                                           BREMIN79
 1533 FORMAT( 30X,'**************************************************', BREMIN80
     &/,30X,'* B R E M 5     A D A P T E D   T O   K I N G A L  : '/    BREMIN81
     X ,30X,'*   GENERATING FERMION TYPE:',I3,/,                        BREMIN82
     &30X,'*****************************************************')      BREMIN83
       POL=RDATIN(17)                                                   BREMIN84
       IFLOP=0                                                          BREMIN85
       IFPOL=1                                                          BREMIN86
       IF(POL.LE.0.01) IFPOL=0                                          BREMIN87
       IF(POL.LE.0.01) POL=0.D0                                         BREMIN88
C                                                                       BREMIN89
       IPOL=1                                                           BREMIN90
      IF( IPOL . EQ . 1 ) CALL RDMIN(ISEED)                             BREMIN91
C                                                                       BREMIN92
      CALL HBOOK1(10047,'WEIGHT DISTRIBUTION  ',50,0.,2.,0.)            BREMIN93
C                                                                       BREMIN94
C                                                                       BREMIN95
      GO TO 80                                                          BREMIN96
C ----------------------------------------------------------------------BREMIN97
C     SPECIAL ENTRY TO SWITCH POLARIZATION AROUND                       BREMIN98
C                                                                       BREMIN99
      ENTRY BRFLOP                                                      BREMI100
      IF(IFPOL.EQ.0) RETURN                                             BREMI101
      IFLOP=1                                                           BREMI102
      GOTO 191                                                          BREMI103
   70 IFLOP=0                                                           BREMI104
      IPOL=2                                                            BREMI105
      POL=DABS(POL)                                                     BREMI106
      IF(IPOL .EQ. 2.AND.POL.NE.0.0D0)POL= -POL                         BREMI107
      IF(IPOL .EQ. 2.AND.POL.NE.0.0D0)WRITE (IOUT, 1010) POL            BREMI108
 1010 FORMAT(1X,50(1H-),/' CHANGING R-POLARIZATION OF THE E- BEAM TO',  BREMI109
     .F10.4,/1X,50(1H-))                                                BREMI110
   80 CONTINUE                                                          BREMI111
      IF(RDATIN(22) .NE. 0.0D0)HISTPR=.TRUE.                            BREMI112
      CALL HISTCL                                                       BREMI113
C     CALL RNCLR                                                        BREMI114
      XNOCT=0.D0                                                        BREMI115
      XNOCF=0.D0                                                        BREMI116
      XCUTT=0.D0                                                        BREMI117
      XCUTF=0.D0                                                        BREMI118
C                                                                       BREMI119
C THE BOUNDARY BETWEEN SOFT AND HARD  BREMSSTRAHLUNG                    BREMI120
C                                                                       BREMI121
      XK0=RDATIN(10)                                                    BREMI122
C                                                                       BREMI123
C INITIALIZE THE SUBPROGRAMS                                            BREMI124
C                                                                       BREMI125
      CALL SETMUS(EB,XMZ,THMIN,THMAX,XKMAX,POL,IPOL)                    BREMI126
C                                                                       BREMI127
      RETURN                                                            BREMI128
C                                                                       BREMI129
      ENTRY BREMEV(IDPR,ISTAT,ECMS,WEIT)                                BREMI130
C                                                                       BREMI131
C PP,PM are initial electron 4-vectors                                  BREMI132
C QP,QM,QK are final muon and photon LAB-frame 4-vectors                BREMI133
C                                                                       BREMI134
      CALL GENMUS(PP,PM,QP,QM,QK,WE,ICON)                               BREMI135
      IDPR=ITYPE*1000+ICON                                              BREMI136
      ECMS=EB*2.                                                        BREMI137
      WEIT=WE                                                           BREMI138
      ISTAT=0                                                           BREMI139
C Fill beam electrons ,Z0,photons                                       BREMI140
      DO 15 I = 1,4                                                     BREMI141
 15   PAUX(I) = PP(I)                                                   BREMI142
      CALL KXL7FL(21,-11,0,0,0,PAUX,NLA)                                BREMI143
      DO 16 I = 1,4                                                     BREMI144
 16   PAUX(I) = PM(I)                                                   BREMI145
      CALL KXL7FL(21, 11,0,0,0,PAUX,NLA)                                BREMI146
      DO 17 I = 1,4                                                     BREMI147
 17   PAUX(I) = PP(I)+PM(I)                                             BREMI148
      CALL KXL7FL(21, 23,1,0,0,PAUX,NLA)                                BREMI149
      IZLU = NLA                                                        BREMI150
      IF(QK(4).GT.1.D-04 ) THEN                                         BREMI151
C  there is a photon, takes care of it                                  BREMI152
        DO 18 I = 1,4                                                   BREMI153
 18     PAUX(I) = QK(I)                                                 BREMI154
        CALL KXL7FL( 1, 22,IZLU,0,0,PAUX,NLA)                           BREMI155
      ENDIF                                                             BREMI156
C                       jetset 7.3 parton shower INTERFACE              BREMI157
      DO 19 I = 1,4                                                     BREMI158
        PAUX2(I) = QM(I)                                                BREMI159
 19     PAUX(I) = QP(I)                                                 BREMI160
      CALL QQTOPS(PAUX,PAUX2,ITYPE,IZLU)                                BREMI161
C                                                                       BREMI162
      EE=FLOAT(ITYPE)                                                   BREMI163
      CALL HFILL(10006,EE,DUM,WEIT)                                     BREMI164
      CALL HFILL(10047,WEIT,DUM,1.)                                     BREMI165
      PMOMP = SQRT (QP(1)**2+QP(2)**2+QP(3)**2)                         BREMI166
      PMOMM = SQRT (QM(1)**2+QM(2)**2+QM(3)**2)                         BREMI167
      CP=QP(3)/PMOMP                                                    BREMI168
      CM=-QM(3)/PMOMM                                                   BREMI169
      IJ=7                                                              BREMI170
      IK=20                                                             BREMI171
      XD=-1.D0                                                          BREMI172
      XDD=1.D0                                                          BREMI173
      CALL HISTO1(IJ,IK,XD,XDD,CP,WE)                                   BREMI174
      CALL HISTO1(IJ,IK,XD,XDD,CM,WE)                                   BREMI175
      XNOCT=XNOCT+WE                                                    BREMI176
      IF(CP.GT.0.D0) XNOCF=XNOCF+WE                                     BREMI177
      IF(QP(4).LT.EMMIN.OR.QM(4).LT.EMPLS) GO TO 100                    BREMI178
      Z=(QP(1)*QM(1)+QP(2)*QM(2)+QP(3)*QM(3))/(PMOMP*PMOMM)             BREMI179
C                                                                       BREMI180
C                                                                       BREMI181
      IF (Z.GT.1.D0) Z=1.D0                                             BREMI182
      IF (Z.LT.-1.D0) Z=-1.D0                                           BREMI183
      Z=DEGRAD*DACOS(-Z)                                                BREMI184
      IM=8                                                              BREMI185
      IN=9                                                              BREMI186
      YD=0.D0                                                           BREMI187
      YDD=40.D0                                                         BREMI188
      CALL HISTO1(IM,IK,YD,YDD,Z,WE)                                    BREMI189
      IF(Z.GT.ACOL) GOTO 100                                            BREMI190
      CALL HISTO1(IN,IK,XD,XDD,CP,WE)                                   BREMI191
      CALL HISTO1(IN,IK,XD,XDD,CM,WE)                                   BREMI192
      XCUTT=XCUTT+WE                                                    BREMI193
      IF(CP.GT.0.D0) XCUTF=XCUTF+WE                                     BREMI194
  100 CONTINUE                                                          BREMI195
      RETURN                                                            BREMI196
C-----------------------------------------------------------------------BREMI197
      ENTRY BREMND                                                      BREMI198
  191 CONTINUE                                                          BREMI199
C                                                                       BREMI200
      CALL ENDMUS(RESULT(IPOL))                                         BREMI201
      IF(HISTPR) THEN                                                   BREMI202
C                                                                       BREMI203
        CALL HISTO2(8,1)                                                BREMI204
        CALL HISTO2(7,0)                                                BREMI205
        CALL HISTO2(9,0)                                                BREMI206
      ENDIF                                                             BREMI207
C                                                                       BREMI208
      ANOC(IPOL)=100.*(2.*XNOCF/XNOCT-1.)                               BREMI209
      ACUT(IPOL)=100.*(2.*XCUTF/XCUTT-1.)                               BREMI210
      FINAL(IPOL)=XCUTT/XNOCT*RESULT(IPOL)                              BREMI211
      EVNOCF(IPOL)=XNOCF                                                BREMI212
      EVNOCT(IPOL)=XNOCT                                                BREMI213
      EVCUTF(IPOL)=XCUTF                                                BREMI214
      EVCUTT(IPOL)=XCUTT                                                BREMI215
C                                                                       BREMI216
      WRITE (IOUT, 300) ANOC(IPOL),ACUT(IPOL),FINAL(IPOL)               BREMI217
  300 FORMAT(1X,50(1H-),/,'THE INTEGRATED FORWARD-BACKWARD ASYMMETRIES  BREMI218
     $ ARE:',/,                                                         BREMI219
     .  10X,   ' WITHOUT ADDITIONAL CUTS =',F10.4,' %',/,               BREMI220
     .  10X,   '    WITH ADDITIONAL CUTS =',F10.4,' %',/,               BREMI221
     .  10X,   ' MOREOVER, DUE TO YOUR CUTS THE CROSS SECTION',/,       BREMI222
     .  10X,   ' IS REDUCED TO ',D15.6,' PICOBARNS')                    BREMI223
      IF(IFLOP.EQ.1) GO TO 70                                           BREMI224
 1000 CONTINUE                                                          BREMI225
       IF (IPOL.EQ. 1.OR.POL.EQ.0.D0) GO TO 900                         BREMI226
      ALRNOC = (RESULT(1)-RESULT(2))/(RESULT(1)+RESULT(2))/DABS(POL)    BREMI227
      ALRCUT = (FINAL(1)-FINAL(2))/(FINAL(1)+FINAL(2))/DABS(POL)        BREMI228
      WRITE (IOUT, 1020) ALRNOC,ALRCUT                                  BREMI229
 1020 FORMAT(1X,50(1H-),/,' THE LONGITUDINAL POLARIZATION ASYMMETRY IS:'BREMI230
     . ,10X     ,/,' WITHOUT ADDITIONAL CUTS =',F10.4                   BREMI231
     . ,10X     ,/,'    WITH ADDITIONAL CUTS =',F10.4                   BREMI232
     . ,10X     ,/1X,50(1H-))                                           BREMI233
C                                                                       BREMI234
C     THE POLARIZED FORWARD-BACWARD ASYMMETRY                           BREMI235
C                                                                       BREMI236
      APNOC=.5*(ANOC(2)-ANOC(1))/.75/DABS(POL)                          BREMI237
      APCUT=.5*(ACUT(2)-ACUT(1))/.75/DABS(POL)                          BREMI238
      WRITE (IOUT, 1021) APNOC,APCUT                                    BREMI239
 1021 FORMAT(1X,50(1H-),/,' THE  POLARIZED  ASYMMETRY IS:'              BREMI240
     .  ,10X    ,/,' WITHOUT ADDITIONAL CUTS =',F10.4                   BREMI241
     .  ,10X    ,/,'    WITH ADDITIONAL CUTS =',F10.4                   BREMI242
     .  ,10X    ,/1X,50(1H-))                                           BREMI243
C                                                                       BREMI244
      APONOC=((EVNOCF(1)-EVNOCF(2))*2-(EVNOCT(1)-EVNOCT(2)))            BREMI245
     .          /(EVNOCT(1)+EVNOCT(2))/.75/DABS(POL)                    BREMI246
      APOCUT=((EVCUTF(1)-EVCUTF(2))*2-(EVCUTT(1)-EVCUTT(2)))            BREMI247
     .          /(EVCUTT(1)+EVCUTT(2))/.75/DABS(POL)                    BREMI248
      WRITE (IOUT, 1022) APONOC,APOCUT                                  BREMI249
 1022 FORMAT(1X,50(1H-),/,' THE FANCY POLARIZED  ASYMMETRY IS:'         BREMI250
     . ,10X     ,/,' WITHOUT ADDITIONAL CUTS =',F10.4                   BREMI251
     . ,10X     ,/,'    WITH ADDITIONAL CUTS =',F10.4                   BREMI252
     . ,10X     ,/1X,50(1H-))                                           BREMI253
      ANPNOC=((EVNOCF(1)+EVNOCF(2))*2-(EVNOCT(1)+EVNOCT(2)))            BREMI254
     .          /(EVNOCT(1)+EVNOCT(2))                                  BREMI255
      ANPCUT=((EVCUTF(1)+EVCUTF(2))*2-(EVCUTT(1)+EVCUTT(2)))            BREMI256
     .          /(EVCUTT(1)+EVCUTT(2))                                  BREMI257
      WRITE (IOUT, 1023) ANPNOC,ANPCUT                                  BREMI258
 1023 FORMAT(1X,50(1H-),/,' THE UNPOLARIZED  ASYMMETRY IS:'             BREMI259
     . ,10X     ,/,' WITHOUT ADDITIONAL CUTS =',F10.4                   BREMI260
     . ,10X     ,/,'    WITH ADDITIONAL CUTS =',F10.4                   BREMI261
     . ,10X     ,/1X,50(1H-))                                           BREMI262
  900 CONTINUE                                                          BREMI263
      RETURN                                                            BREMI264
      END                                                               BREMI265
      REAL FUNCTION CPDECL(P,AM,T0,IOSCI)                               CPDECL 2
C---------------------------------------------------------------------- CPDECL 3
C! Generates the decay length DCL in case of B decay into a             CPDECL 4
C! CP eigenstate final state                                            CPDECL 5
C  AUTHORS: A. Falvard -B.Bloch-Devaux        881024                    CPDECL 6
C                                                                       CPDECL 7
C     INPUT :    P = momentum in gev                                    CPDECL 8
C               AM = mass in gev/c**2                                   CPDECL 9
C               T0 = Proper life time                                   CPDECL10
C            IOSCI = status versus oscillaton                           CPDECL11
C     OUTPUT:                                                           CPDECL12
C            CPDECL = decay length                                      CPDECL13
C---------------------------------------------------------------------- CPDECL14
      PARAMETER (LNBRCP=4)                                              USERCP 2
      COMMON/USERCP/NPARCP,JCODCP(LNBRCP),RHOCPM                        USERCP 3
      COMPLEX RHOCPM                                                    USERCP 4
      COMPLEX PSQD,PSQS                                                 MIXING 2
      COMMON/ MIXING /XD,YD,CHID,RD,XS,YS,CHIS,RS,PSQD,PSQS             MIXING 3
      COMMON/ PROBMI /CDCPBA,CDCPB,CSCPBA,CSCPB                         MIXING 4
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12         ALCONS 2
      REAL RADEG, DEGRA                                                 ALCONS 3
      REAL CLGHT                                                        ALCONS 4
      INTEGER NBITW, NBYTW, LCHAR                                       ALCONS 5
      PARAMETER (PI=3.141592653589)                                     ALCONS 6
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)                          ALCONS 7
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          ALCONS 8
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         ALCONS 9
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         ALCONS10
      PARAMETER (CLGHT = 29.9792458)                                    ALCONS11
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)              ALCONS12
C                                                                       ALCONS13
      PARAMETER (CLITS = CLGHT * 1.E+09)                                CPDECL18
      COMPLEX R                                                         CPDECL19
      EXTERNAL RNDM                                                     CPDECL20
 1     Z=RNDM(DUM)                                                      CPDECL21
      IF(Z.EQ.0.)Z=1.                                                   CPDECL22
      AL=ALOG(Z)                                                        CPDECL23
      DCL=-T0*AL                                                        CPDECL24
      X=XD                                                              CPDECL25
      Y=YD                                                              CPDECL26
      R=RHOCPM/PSQD                                                     CPDECL27
      IF(IOSCI.EQ.3.OR.IOSCI.EQ.4)THEN                                  CPDECL28
      X=XS                                                              CPDECL29
      Y=YS                                                              CPDECL30
      R=RHOCPM/PSQS                                                     CPDECL31
      ENDIF                                                             CPDECL32
      AS=AIMAG(R)                                                       CPDECL33
      ICP = 1 -2*MOD(IOSCI,2)                                           CPDECL34
      FCON=1.+ICP*AS*SIN(-X*AL)                                         CPDECL35
      F1MAX=2.                                                          CPDECL36
      IF(F1MAX*RNDM(DUM).GT.FCON)GOTO 1                                 CPDECL37
      CPDECL=P*DCL/AM*CLITS                                             CPDECL38
      RETURN                                                            CPDECL39
      END                                                               CPDECL40
      SUBROUTINE DECSTA                                                 DECSTA 2
C-------------------------------------------------------------          DECSTA 3
C! Termination routine for selected B mesons decays                     DECSTA 4
C! and requested decay chains                                           BBL00930
C  B. Bloch -Devaux March 94                                            BBL00931
C--------------------------------------------------------------         DECSTA 5
      COMMON / MASTER / IGENE,IWEIT,IMIX,IVBU,IFL,ECM,IEV1,IEV2,IEFLOP, MASTER 2
     $          ISEM,IDEC,WTMAX,SVRT(3),ICOULU(10),IPHO,ILAM,IP17,IDDC  BBL00914
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
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
C                                                                       LUN7CO12
      COMMON /CHAINS/ KFDAU,KFMOTH,KFCOMP(2)                            CHAINS 2
C   KFDAU  = internal JETSET 7.3 code for daughter decayed particle     CHAINS 3
C   KFMOTH = internal JETSET 7.3 code for mother of decayed particle    CHAINS 4
C   KFCOMP(2)internal JETSET 7.3 code for companions of decayed particleCHAINS 5
      PARAMETER (LBPOS=100,NBU=521,NBD=511,NBS=531,NBC=541)             BCODES 2
      PARAMETER (NLB=5122,NXB= 5132)                                    BBL00511
C     LBPOS  = internal JETSET 7.3 code for B meson                     BCODES 3
      PARAMETER (NXB0=5232,NOB= 5332)                                   BBL007 9
C     NBU    = internal JETSET 7.3 code for Bu meson                    BCODES 4
C     NBD    = internal JETSET 7.3 code for Bd meson                    BCODES 5
C     NBS    = internal JETSET 7.3 code for Bs meson                    BCODES 6
C     NBC    = internal JETSET 7.3 code for Bc meson                    BCODES 7
C     NLB    = internal JETSET 7.3 code for /\B BARYON                  BBL00512
C     NXB    = internal JETSET 7.3 code for XIB BARYON                  BBL00513
C     NXB0   = internal JETSET 7.3 code for XIB0 BARYON                 BBL00710
C     NOB    = internal JETSET 7.3 code for OMEGAB BARYON               BBL00711
      IUT=IW(6)                                                         DECSTA10
      IF ( IDEC.LE.0) GO TO 500                                         BBL00933
      WRITE(IUT,101)                                                    DECSTA11
      KC = LUCOMP(LBPOS)                                                DECSTA12
      NDEC=MDCY(KC,3)                                                   DECSTA13
      IDC=MDCY(KC,2)                                                    DECSTA14
      NOPE=0                                                            DECSTA15
      BRSU=0.                                                           DECSTA16
      DO 120 IDL=IDC,IDC+NDEC-1                                         DECSTA17
      IF(MDME(IDL,1).NE.1) GOTO 120                                     DECSTA18
      IF(MDME(IDL,2).GT.100) GOTO 120                                   DECSTA19
      NOPE=NOPE+1                                                       DECSTA20
      BRSU=BRSU+BRAT(IDL)                                               DECSTA21
  120 CONTINUE                                                          DECSTA22
      NOP1=NOPE                                                         DECSTA23
      BRS1=BRSU                                                         DECSTA24
      NOP2=NOPE                                                         DECSTA25
      BRS2=BRSU                                                         DECSTA26
      IF ( IDEC.EQ.1 .OR. IDEC.EQ.3) THEN                               DECSTA27
         NOP1=0                                                         DECSTA28
         BRS1=0.                                                        DECSTA29
         JDEC=IW(NAMIND('GDC1'))                                        DECSTA30
         IF(JDEC.GT.0)THEN                                              DECSTA31
           DO 121 IDL=1,NDEC                                            DECSTA32
             IF(IW(JDEC+IDL).NE.1) GOTO 121                             DECSTA33
             NOP1=NOP1+1                                                DECSTA34
             BRS1=BRS1+BRAT(IDC+IDL-1)                                  DECSTA35
  121      CONTINUE                                                     DECSTA36
         ENDIF                                                          DECSTA37
      ENDIF                                                             DECSTA38
      IF ( IDEC.GE.2) THEN                                              DECSTA39
         NOP2=0                                                         DECSTA40
         BRS2=0.                                                        DECSTA41
         JDEC=IW(NAMIND('GDC2'))                                        DECSTA42
         IF(JDEC.GT.0)THEN                                              DECSTA43
           DO 122 IDL=1,NDEC                                            DECSTA44
             IF(IW(JDEC+IDL).NE.1) GOTO 122                             DECSTA45
             NOP2=NOP2+1                                                DECSTA46
             BRS2=BRS2+BRAT(IDC+IDL-1)                                  DECSTA47
  122      CONTINUE                                                     DECSTA48
         ENDIF                                                          DECSTA49
      ENDIF                                                             DECSTA50
      WRITE(IUT,102) NOPE,BRSU,NOP1,BRS1,NOP2,BRS2                      DECSTA51
      WRITE(IUT,105) BRS1/BRSU,BRS2/BRSU                                DECSTA52
  500 CONTINUE                                                          BBL00934
      IF ( IDDC.LE.0) GO TO 600                                         BBL00935
      WRITE(IUT,103)  KFDAU,KFMOTH,KFCOMP                               BBL00936
      KC = LUCOMP(KFDAU)                                                BBL00937
      NDEC=MDCY(KC,3)                                                   BBL00938
      IDC=MDCY(KC,2)                                                    BBL00939
      NOPE=0                                                            BBL00940
      BRSU=0.                                                           BBL00941
      DO 130 IDL=IDC,IDC+NDEC-1                                         BBL00942
         IF(MDME(IDL,1).NE.1) GOTO 130                                  BBL00943
         IF(MDME(IDL,2).GT.100) GOTO 130                                BBL00944
         NOPE=NOPE+1                                                    BBL00945
         BRSU=BRSU+BRAT(IDL)                                            BBL00946
  130 CONTINUE                                                          BBL00947
      NOP1=NOPE                                                         BBL00948
      BRS1=BRSU                                                         BBL00949
      IF ( IDDC.EQ.1 ) THEN                                             BBL00950
         NOP1=0                                                         BBL00951
         BRS1=0.                                                        BBL00952
         JDEC=IW(NAMIND('GDDC'))                                        BBL00953
         IF(JDEC.GT.0)THEN                                              BBL00954
           DO 131 IDL=1,NDEC                                            BBL00955
             IF(IW(JDEC+IDL+3).NE.1) GOTO 131                           BBL00956
             NOP1=NOP1+1                                                BBL00957
             BRS1=BRS1+BRAT(IDC+IDL-1)                                  BBL00958
  131      CONTINUE                                                     BBL00959
         ENDIF                                                          BBL00960
      ENDIF                                                             BBL00961
      WRITE(IUT,104) NOPE,BRSU,NOP1,BRS1                                BBL00962
      WRITE(IUT,105) BRS1/BRSU                                          BBL00963
  600 CONTINUE                                                          BBL00964
  101 FORMAT(//20X,'B decay  STATISTICS',/,20X,'*******************')   DECSTA53
  102 FORMAT(//20X,'B decay  total decay modes :          ',I10,F12.5,  DECSTA54
     $      //,20X,'First  B decay selected decay modes : ',I10,F12.5,  DECSTA55
     $      //,20X,'Second B decay selected decay modes : ',I10,F12.5)  DECSTA56
  103 FORMAT(//20X,'Special decay  chain STATISTICS',/,20X,             BBL00965
     $             '*******************************',/,20X,             BBL00966
     $ ' for particle code ',I5,' coming from mother code ',I5,         BBL00967
     $ ' and with at least one of the companions ',2I5)                 BBL00968
  104 FORMAT(//20X,'Special decay  chain total modes :    ',I10,F12.5,  BBL00969
     $      //,20X,'First    decay selected decay modes : ',I10,F12.5)  BBL00970
  105 FORMAT(//20X,'Cross-section must be multiplied by : ',2F12.5)     DECSTA57
      RETURN                                                            DECSTA58
      END                                                               DECSTA59
      REAL FUNCTION DSMALL (AJ,AM,AN,BETA)                              DSMALL 2
C--------------------------------------------------------------------   DSMALL 3
C      B.Bloch-Devaux  November 1989  IMPLEMENTATION OF DDJMNB          DSMALL 4
C! LUNSEM  implementation of DDJMNB                                     DSMALL 5
C                                                                       DSMALL 6
C     structure : function                                              DSMALL 7
C                                                                       DSMALL 8
C     input     : same arguments as DDJMNB but REAL*4                   DSMALL 9
C                                                                       DSMALL10
C     output    : return value of DDJMNB but REAL*4                     DSMALL11
C--------------------------------------------------------------------   DSMALL12
      REAL*8 DDJMNB,DAM,DAN,DAJ,DBETA                                   DSMALL13
      EXTERNAL DDJMNB                                                   DSMALL14
      DAM = DBLE(AM)                                                    DSMALL15
      DAN = DBLE(AN)                                                    DSMALL16
      DAJ = DBLE(AJ)                                                    DSMALL17
      DBETA= DBLE(BETA)                                                 DSMALL18
      DSMALL = SNGL(DDJMNB(DAJ,DAM,DAN,DBETA))                          DSMALL19
      RETURN                                                            DSMALL20
      END                                                               DSMALL21
      SUBROUTINE DYMUIN(NTYP)                                           DYMUIN 2
C-----------------------------------------------------------------------DYMUIN 3
C    B.Bloch -Devaux APRIL 1991                                         DYMUIN 4
C         ORIGINAL VERSION OF DYMU3 AS PROVIDED BY J.E.Campagne         DYMUIN 5
C                       June 1989                                       DYMUIN 6
C  This is a subset of original subroutine INIRUN to compute secondary  DYMUIN 7
C  quantities according to requested final state.                       DYMUIN 8
C-----------------------------------------------------------------------DYMUIN 9
      COMMON /CONST/ ALFA,PI,ALFA1                                      DYMUCOM2
      COMMON /RUNPAR/ SOLD,ID2,ID3,FINEXP,POIDS,INTERF,XK0              DYMUCOM3
      COMMON /WEAK/ AEL,AMU,AMZ,GAMM,SW2,CA2,CV2,CA2CV2,COL,T3,QI       DYMUCOM4
      COMMON /BEAM/ S0,EBEAM                                            DYMUCOM5
      COMMON /TAU / TAUV,CPTAU,HEL,PITAU(4)                             DYMUCOM6
      COMMON/WEAKQ/WEAKC(11,6),XSECT(6),XTOT                            DYMUCOM7
      COMMON/ZDUMP / IDEBU , NEVT                                       DYMUCOM8
      COMMON/RESULT/ SIGBOR,SIGTOT,ERRSIG,ASYTOT,ERRASY                 DYMUCOM9
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      COMMON / VECLAB / PFP(4),PFM(4),GAP(4),GAM(4),GAF(4)              DYMUIN12
      COMMON /COUNTS/ SIG,SIG2,SECFWD,SECBKW,SCFWD2,SCBKW2              DYMUIN13
      COMMON /EVTS/ NEVT1,NEVT2,NFWD,NBKW                               DYMUIN14
      REAL*8 SIG,SIG2,SECFWD,SECBKW,SCFWD2,SCBKW2                       DYMUIN15
C                                                                       DYMUIN16
      DIMENSION PBEA(4)                                                 DYMUIN17
      DIMENSION ISEED(3)                                                DYMUIN18
      LOGICAL FIRST                                                     DYMUIN19
      DATA FIRST/.TRUE./                                                DYMUIN20
      IOUT = IW(6)                                                      DYMUIN21
      IF(FIRST)THEN                                                     DYMUIN22
        FIRST = .FALSE.                                                 DYMUIN23
        WRITE(IOUT,*)'*****************************************'        DYMUIN24
        WRITE(IOUT,*)'*         WELCOME    TO    DYMU3        *'        DYMUIN25
        WRITE(IOUT,*)'*                                       *'        DYMUIN26
        WRITE(IOUT,*)'*         AUTHORS: J.E.CAMPAGNE         *'        DYMUIN27
        WRITE(IOUT,*)'*                  R.ZITOUN             *'        DYMUIN28
        WRITE(IOUT,*)'*                                       *'        DYMUIN29
        WRITE(IOUT,*)'*         19 nov  89 RELEASE            *'        DYMUIN30
        WRITE(IOUT,*)'*                                       *'        DYMUIN31
        WRITE(IOUT,*)'*****************************************'        DYMUIN32
        WRITE(IOUT,*)' '                                                DYMUIN33
*----                                                                   DYMUIN34
       CALL HBOOK1(10047,'WEIGHT DISTRIBUTION  ',50,0.,2.,0.)           DYMUIN35
C                                                                       DYMUIN36
      ENDIF                                                             DYMUIN37
C                                                                       DYMUIN38
      ITYP = NTYP                                                       DYMUIN39
      NTYPO = NTYP                                                      DYMUIN40
C                                                                       DYMUIN41
       WRITE(IOUT,1533) ITYP                                            DYMUIN42
 1533 FORMAT( 30X,'**************************************************', DYMUIN43
     &/,30X,'* D Y M U 3     A D A P T E D   T O   K I N G A L  : '/    DYMUIN44
     X ,30X,'*   GENERATING FERMION TYPE:',I3,/,                        DYMUIN45
     &30X,'*****************************************************')      DYMUIN46
*---- BEAM ENERGY                                                       DYMUIN47
*                                                                       DYMUIN48
*                                                                       DYMUIN49
      ECMS = 2.*EBEAM                                                   DYMUIN50
      S0 = 4.*EBEAM**2                                                  DYMUIN51
*                                                                       DYMUIN52
*----  COUPLING CONSTANTS                                               DYMUIN53
*                                                                       DYMUIN54
      CV    = (-1.+4.*SW2)/4./SQRT(SW2*(1.-SW2))                        DYMUIN55
      CA    = -1./4./SQRT(SW2*(1.-SW2))                                 DYMUIN56
      CVPRI = (-2*T3/QI+4.*SW2)/4./SQRT(SW2*(1.-SW2))                   DYMUIN57
      CAPRI = -T3/QI/2./SQRT(SW2*(1.-SW2))                              DYMUIN58
      CV2 = CVPRI*CV                                                    DYMUIN59
      CA2 = CAPRI*CA                                                    DYMUIN60
      CA2CV2 = ( CV**2+CA**2 )*( CVPRI**2+CAPRI**2 )                    DYMUIN61
      CALL DYMUSI                                                       DYMUIN62
*                                                                       DYMUIN63
*---- CONST1                                                            DYMUIN64
*                                                                       DYMUIN65
      ALFA  = 1./137.036                                                DYMUIN66
      PI    = 3.14159265                                                DYMUIN67
      ALFA1 = ALFA/PI                                                   DYMUIN68
*                                                                       DYMUIN69
*----                                                                   DYMUIN70
*                                                                       DYMUIN71
      WRITE(IOUT,*)'*************************************************'  DYMUIN72
      WRITE(IOUT,*)'*     RUN PARAMETERS FOR RUN',ITYP                  DYMUIN73
      WRITE(IOUT,*)'******                                 **********'  DYMUIN74
      WRITE(IOUT,1000) AMZ,GAMM,SW2                                     DYMUIN75
      WRITE(IOUT,1003) ECMS,EBEAM                                       DYMUIN76
 1000   FORMAT('     Z MASS =',F8.3,' GEV ,      Z WIDTH =',F6.3,       DYMUIN77
     &         ' GEV ,  SIN2 TETA =',F7.4)                              DYMUIN78
 1003   FORMAT(' CMS ENERGY =',F8.3,' GEV ,  BEAM ENERGY =',F8.3)       DYMUIN79
*                                                                       DYMUIN80
      IF(POIDS.EQ.1)THEN                                                DYMUIN81
        WRITE(IOUT,*)'UNWEIGHTED EVENTS'                                DYMUIN82
      ELSE                                                              DYMUIN83
        WRITE(IOUT,*)'WEIGHTED EVENTS'                                  DYMUIN84
      ENDIF                                                             DYMUIN85
      WRITE(IOUT,*)'INITIAL STATE EXPONENTIATION'                       DYMUIN86
      IF(FINEXP.EQ.1)THEN                                               DYMUIN87
        WRITE(IOUT,*)'FINAL STATE EXPONENTIATION'                       DYMUIN88
      ELSE IF(FINEXP.EQ.0) THEN                                         DYMUIN89
        WRITE(IOUT,*)'NO FINAL STATE EXPONENTIATION.'                   DYMUIN90
      ELSE IF(FINEXP.EQ.-1) THEN                                        DYMUIN91
        WRITE(IOUT,*)'NO FINAL STATE PHOTON'                            DYMUIN92
      ENDIF                                                             DYMUIN93
      IF(ID2.EQ.1)THEN                                                  DYMUIN94
        WRITE(IOUT,*)'DII IN D(X)'                                      DYMUIN95
      ELSE                                                              DYMUIN96
        WRITE(IOUT,*)'DII NOT IN D(X)'                                  DYMUIN97
      ENDIF                                                             DYMUIN98
      IF(ID3.EQ.1)THEN                                                  DYMUIN99
        WRITE(IOUT,*)'DIII IN D(X)'                                     DYMUI100
      ELSE                                                              DYMUI101
        WRITE(IOUT,*)'DIII NOT IN D(X)'                                 DYMUI102
      ENDIF                                                             DYMUI103
      IF(INTERF.EQ.0)THEN                                               DYMUI104
        WRITE(IOUT,*)'NO INTERFERENCE'                                  DYMUI105
      ELSE                                                              DYMUI106
        WRITE(IOUT,*)'INTERFERENCE WITH K0 =',XK0                       DYMUI107
      ENDIF                                                             DYMUI108
*                                                                       DYMUI109
      CALL RDMOUT(ISEED)                                                DYMUI110
      WRITE(IOUT,*)'INITIAL SEEDS ARE',ISEED                            DYMUI111
      WRITE(IOUT,*)'*************************************************'  DYMUI112
*                                                                       DYMUI113
*---- SET TO ZERO                                                       DYMUI114
*                                                                       DYMUI115
      SIG    = 0.                                                       DYMUI116
      SIG2   = 0.                                                       DYMUI117
      SECFWD = 0.                                                       DYMUI118
      SECBKW = 0.                                                       DYMUI119
      SCFWD2 = 0.                                                       DYMUI120
      SCBKW2 = 0.                                                       DYMUI121
      NEVT1  = 0                                                        DYMUI122
      NEVT2  = 0                                                        DYMUI123
      NFWD   = 0                                                        DYMUI124
      NBKW   = 0                                                        DYMUI125
C                                                                       DYMUI126
      RETURN                                                            DYMUI127
C ----------------------------------------------------------------------DYMUI128
C                                                                       DYMUI129
      ENTRY DYMUEV(IDPR,ISTAT,ECMM,WEIT)                                DYMUI130
C                                                                       DYMUI131
      IF (NTYPO.EQ.10 ) THEN                                            DYMUI132
C   Decide which flavor to generate                                     DYMUI133
        XX = RNDM(DUM)                                                  DYMUI134
        DO 30 I=1,6                                                     DYMUI135
          IF (XX.LT.XSECT(I)) GO TO 40                                  DYMUI136
  30    CONTINUE                                                        DYMUI137
  40    ITYP = I                                                        DYMUI138
C   Copy corresponding coupling constants                               DYMUI139
        CALL UCOPY(WEAKC(1,I),AEL,11)                                   DYMUI140
        CALL DYMUSI                                                     DYMUI141
        ITYP= ITYP+NTYPO                                                DYMUI142
      ENDIF                                                             DYMUI143
C                                                                       DYMUI144
      CALL DYMUS (WE)                                                   DYMUI145
      IDPR=ITYP*1000                                                    DYMUI146
      ECMM=ECMS                                                         DYMUI147
      WEIT=WE                                                           DYMUI148
      ISTAT=0                                                           DYMUI149
C                                LUND INTERFACE                         DYMUI150
C Fill beam electrons ,Z0,photons                                       DYMUI151
      PBEA(1)=0.                                                        DYMUI152
      PBEA(2) = 0.                                                      DYMUI153
      PBEA(3) = EBEAM                                                   DYMUI154
      PBEA(4) = EBEAM                                                   DYMUI155
      CALL KXL7FL(21,-11,0,0,0,PBEA,NLA)                                DYMUI156
      PBEA(3) = -EBEAM                                                  DYMUI157
      CALL KXL7FL(21, 11,0,0,0,PBEA,NLA)                                DYMUI158
      IF (GAP(4).GT.1.E-06) THEN                                        DYMUI159
         CALL KXL7FL( 1, 22,1,0,0,GAP,NLA)                              DYMUI160
      ENDIF                                                             DYMUI161
      IF (GAM(4).GT.1.E-06) THEN                                        DYMUI162
         CALL KXL7FL( 1, 22,2,0,0,GAM,NLA)                              DYMUI163
      ENDIF                                                             DYMUI164
      DO 35 I=1,4                                                       DYMUI165
        PBEA(I)=-(GAP(I)+GAM(I))                                        DYMUI166
   35 CONTINUE                                                          DYMUI167
      PBEA(4)= PBEA(4)+2.*EBEAM                                         BBL003 8
      CALL KXL7FL(21, 23,1,0,0,PBEA,NLA)                                DYMUI168
      IZLU = NLA                                                        DYMUI169
      IF (GAF(4).GT.1.E-06) THEN                                        DYMUI170
         CALL KXL7FL( 1, 22,IZLU,0,0,GAF,NLA)                           DYMUI171
      ENDIF                                                             DYMUI172
      CALL QQTOPS(PFP,PFM,ITYP,IZLU)                                    DYMUI173
      EE=FLOAT(ITYP)                                                    DYMUI174
      CALL HFILL(10006,EE,DUM,WEIT)                                     DYMUI175
      CALL HFILL(10047,WEIT,DUM,1.)                                     DYMUI176
C                                                                       DYMUI177
  100 CONTINUE                                                          DYMUI178
      RETURN                                                            DYMUI179
C                                                                       DYMUI180
C-----------------------------------------------------------------------DYMUI181
      ENTRY DYMUND                                                      DYMUI182
C                                                                       DYMUI183
      CALL FINISH(NTYPO,NEVT)                                           DYMUI184
C                                                                       DYMUI185
      RETURN                                                            DYMUI186
      END                                                               DYMUI187
      SUBROUTINE FILBOK(WEI)                                            FILBOK 2
C--------------------------------------------------------------------   FILBOK 3
C      B. BLOCH-DEVAUX April  1991                                      FILBOK 4
C                                                                       FILBOK 5
C!   Fill some standard histos from JETSET 7.3 common                   FILBOK 6
C    WEI is the weight to be used in filling                            FILBOK 7
C--------------------------------------------------------------------   FILBOK 8
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
C                                                                       LUN7CO12
      DO 30 ITR = 1 , N7LU                                              FILBOK10
        IMOTH = K7LU(ITR,3)                                             FILBOK11
        IMTYP = IABS(K7LU(IMOTH,2))                                     FILBOK12
        ILUN = K7LU(ITR,2)                                              FILBOK13
C   Final state radiation photon comes from a quark                     FILBOK14
        IF (ILUN.EQ.22 .AND. IMTYP.GE.1 .AND. IMTYP.LE.6)               FILBOK15
     &       CALL HFILL(10008,P7LU(ITR,4),DUM,WEI)                      FILBOK16
C  Main vertex particles                                                FILBOK17
        IF (IMOTH.LE.1.AND. (ILUN.EQ.22))THEN                           FILBOK18
C    There is a photon                                                  FILBOK19
           CALL HFILL(10003,P7LU(ITR,4),DUM,WEI)                        FILBOK20
        ELSEIF (ILUN.GT.0 .AND. ILUN.LE.6.AND.IMTYP.EQ.23) THEN         FILBOK21
C   fermion                                                             FILBOK22
           CALL HFILL(10001,P7LU(ITR,4),DUM,WEI)                        FILBOK23
           EE=P7LU(ITR,3)/SQRT(P7LU(ITR,1)**2+P7LU(ITR,2)**2+           FILBOK24
     $       P7LU(ITR,3)**2)                                            FILBOK25
           CALL HFILL(10004,EE,DUM,WEI)                                 FILBOK26
        ELSEIF (ILUN.LT.0 .AND. ILUN.GE.-6.AND.IMTYP.EQ.23)  THEN       FILBOK27
C   anti-fermion                                                        FILBOK28
           CALL HFILL(10002,P7LU(ITR,4),DUM,WEI)                        FILBOK29
           EE= P7LU(ITR,3)/SQRT(P7LU(ITR,1)**2+P7LU(ITR,2)**2+          FILBOK30
     &     P7LU(ITR,3)**2)                                              FILBOK31
           CALL HFILL(10005,EE,DUM,WEI)                                 FILBOK32
        ENDIF                                                           FILBOK33
  30  CONTINUE                                                          FILBOK34
      RETURN                                                            FILBOK35
      END                                                               FILBOK36
      SUBROUTINE HEPFIL(IM,JF,JL)                                       HEPFIL 2
C...Purpose: to convert JETSET event record contents to or from         HEPFIL 3
C...the standard event record commonblock.                              HEPFIL 4
      PARAMETER (NMXHEP=2000)                                           HEPEVT 2
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),           HEPEVT 3
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)   HEPEVT 4
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)                     HEPFIL 6
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)             HEPFIL 7
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)        HEPFIL 8
      SAVE /HEPEVT/                                                     HEPFIL 9
      SAVE /LUJETS/,/LUDAT1/,/LUDAT2/                                   HEPFIL10
C...Conversion from JETSET to standard, the easy part.                  HEPFIL11
        NEVHEP=0                                                        HEPFIL12
        NHEP=0                                                          HEPFIL13
        IF(N.GT.NMXHEP) CALL LUERRM(8,                                  HEPFIL14
     &  '(LUHEPC:) no more space in /HEPEVT/')                          HEPFIL15
        DO 140 I=IM,JL                                                  HEPFIL16
        IF((I.GT.IM).AND.(I.LT.JF)) GO TO 140                           HEPFIL17
        NHEP=NHEP+1                                                     HEPFIL18
        ISTHEP(NHEP)=0                                                  HEPFIL19
        IF(K(I,1).GE.1.AND.K(I,1).LE.10) ISTHEP(NHEP)=1                 HEPFIL20
        IF(K(I,1).GE.11.AND.K(I,1).LE.20) ISTHEP(NHEP)=2                HEPFIL21
        IF(K(I,1).GE.21.AND.K(I,1).LE.30) ISTHEP(NHEP)=3                HEPFIL22
        IF(K(I,1).GE.31.AND.K(I,1).LE.100) ISTHEP(NHEP)=K(I,1)          HEPFIL23
        IDHEP(NHEP)=K(I,2)                                              HEPFIL24
        JMOHEP(1,NHEP)=K(I,3)                                           HEPFIL25
        IF ( I.EQ.IM) THEN                                              HEPFIL26
           JMOHEP(1,NHEP)=0                                             HEPFIL27
           JMOTH = NHEP                                                 HEPFIL28
        ELSE IF ( I.GE.JF .AND. I.LE.JL) THEN                           HEPFIL29
           JMOHEP(1,NHEP)= JMOTH                                        HEPFIL30
           ISTHEP(NHEP)= 1                                              HEPFIL31
        ENDIF                                                           HEPFIL32
        JMOHEP(2,NHEP)=0                                                HEPFIL33
        IF(K(I,1).NE.3.AND.K(I,1).NE.13.AND.K(I,1).NE.14                HEPFIL34
     $  .AND.I.EQ.IM )THEN                                              HEPFIL35
          JDAHEP(1,NHEP)=K(I,4)-JF+NHEP+1                               HEPFIL36
          JDAHEP(2,NHEP)=K(I,5)-JF+NHEP+1                               HEPFIL37
        ELSE                                                            HEPFIL38
          JDAHEP(1,NHEP)=0                                              HEPFIL39
          JDAHEP(2,NHEP)=0                                              HEPFIL40
        ENDIF                                                           HEPFIL41
        DO 100 J=1,5                                                    HEPFIL42
  100   PHEP(J,NHEP)=P(I,J)                                             HEPFIL43
        DO 110 J=1,4                                                    HEPFIL44
  110   VHEP(J,NHEP)=V(I,J)                                             HEPFIL45
  140   CONTINUE                                                        HEPFIL46
      RETURN                                                            HEPFIL47
      END                                                               HEPFIL48
      SUBROUTINE HEPLIS(IFLAG)                                          HEPLIS 2
C...Purpose: print the standard event record commonblock.               HEPLIS 3
      PARAMETER (NMXHEP=2000)                                           HEPEVT 2
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),           HEPEVT 3
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)   HEPEVT 4
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      WRITE(IW(6),10)                                                   HEPLIS 6
      DO 140 I=1,NHEP                                                   HEPLIS 7
        WRITE(IW(6),100)I,ISTHEP(I),IDHEP(I),(JMOHEP(JB,I),JB=1,2),     HEPLIS 8
     $  (JDAHEP(JB,I),JB=1,2),(PHEP(JB,I),JB=1,5)                       HEPLIS 9
  140 CONTINUE                                                          HEPLIS10
 10   FORMAT (10X,' NHEP ISTHEP IDHEP JMOHEP  JDAHEP  PHEP  ')          HEPLIS11
 100  FORMAT (7I6,5F10.4)                                               HEPLIS12
      RETURN                                                            HEPLIS13
      END                                                               HEPLIS14
      FUNCTION HIMAX (IMODEL,J,M1,M2)                                   HIMAX  2
C---------------------------------------------------------------------  HIMAX  3
C     A. FALVARD   -   130189                                           HIMAX  4
C                                                                       HIMAX  5
C! Computes maximal value of Q2 distribution for semileptonic decay     HIMAX  6
C             B OR D ----> EL + NEUT + M2                               HIMAX  7
C     THE EXPRESSIONS AND FORTRAN CODE FOR MODELS 1,2,3,4 WERE          HIMAX  8
C     GIVEN BY G. SCHULER.                                              HIMAX  9
C---------------------------------------------------------------------  HIMAX 10
      REAL M1,M2                                                        HIMAX 11
      PARAMETER (NCUT=100)                                              HIMAX 12
      PARAMETER (NMODEL=5,NDECAY=11)                                    AFL001 1
      COMMON/SEMLEP/IMOSEM,WTM(NMODEL,NDECAY),IHEL,KHEL,PRM1,PR0,PRP1,  SEMLEP 3
     $          BBDAT(NDECAY),BXDAT(NDECAY)  ,                          SEMLEP 4
     $          NO1(NDECAY),NO2(NDECAY),OVER(NMODEL,NDECAY)             SEMLEP 5
     $          , IMODSS,R1,R2,R3                                       AFL001 2
      REAL MFF,MFF2,MFF3,MFF4                                           SEMLEP 6
      COMMON/MFFX/MFF(NMODEL,NDECAY),MFF2(NMODEL,NDECAY),               SEMLEP 7
     $            MFF3(NMODEL,NDECAY),MFF4(NMODEL,NDECAY)               SEMLEP 8
      REAL MB,MD,MQ,MBOT,MX,MUP,MUM,MS,MSQ,MRSQ,KSQ                     SEMLEP 9
      COMMON /SCORA/MB,MD,MQ,MBOT,MX,MUP,MUM,MS,MSQ,MRSQ,KSQ            SEMLEP10
     *,BM,BMSQ,XMTSQ,BB,BX,XMT,BBX,BR,BRX                               SEMLEP11
      COMMON /SCOROU/AL,CP,Q,QV                                         SEMLEP12
      HIMAX=0.                                                          HIMAX 14
      Q2MAX=(M1-M2)**2                                                  HIMAX 15
      DO 1 K=1,NCUT                                                     HIMAX 16
      Q2=Q2MAX*K/NCUT                                                   HIMAX 17
      T=Q2                                                              HIMAX 18
      IF(IMODEL.LE.4)THEN                                               HIMAX 19
         VM=MFF(IMODEL,J)**2/(MFF(IMODEL,J)**2-Q2)                      HIMAX 20
         VM2=MFF2(IMODEL,J)**2/(MFF2(IMODEL,J)**2-Q2)                   HIMAX 21
         VM3=MFF3(IMODEL,J)**2/(MFF3(IMODEL,J)**2-Q2)                   HIMAX 22
         VM4=MFF4(IMODEL,J)**2/(MFF4(IMODEL,J)**2-Q2)                   HIMAX 23
         OVER1=OVER(IMODEL,J)                                           HIMAX 24
      ENDIF                                                             HIMAX 25
      PI=4.*ATAN(1.)                                                    HIMAX 26
      QPLUS=(M1+M2)**2-Q2                                               HIMAX 27
      QMINS=(M1-M2)**2-Q2                                               HIMAX 28
      IF(QMINS.LT.0.)GOTO 1                                             HIMAX 29
      PC=SQRT(QPLUS*QMINS)/(2.*M1)                                      HIMAX 30
      IF(IMODEL.EQ.5)THEN                                               HIMAX 31
        Y=Q2/M1**2                                                      HIMAX 32
        PX=PC                                                           HIMAX 33
        TM=(BM-XMT)**2                                                  HIMAX 34
        T=TM-Y*BMSQ                                                     HIMAX 35
        FEX=EXP(-MSQ*T/(4.*KSQ*BBX))                                    HIMAX 36
      FEX1=FEX*((BB*BX/BBX)**1.5)*SQRT(MX/MBOT)                         AFL001 3
      GV1=0.5*(1./MQ-MD*BR/(2.*MUM*MX))                                 AFL001 4
      F1=2.*MBOT                                                        AFL001 5
      GV=GV1*FEX1                                                       AFL001 6
      F=F1*FEX1                                                         AFL001 7
      AP=-0.5*FEX1/MX*(1.+MD/MB*((BB**2-BX**2)/(BB**2+BX**2))           AFL001 8
     *-MD**2/(4.*MUM*MBOT)*BX**4/BBX**2)                                AFL001 9
      AMSQ=MSQ/(4.*KSQ*BBX)                                             AFL00110
      FEX5=FEX*((BB*BX/BBX)**2.5)*SQRT(MX/MBOT)                         AFL00111
      QV=0.5*FEX5*MD/(MX*BB)                                            AFL00112
      AL=-FEX5*MBOT*BB*((1./MUM)+0.5*(MD/(MBOT*KSQ*                     AFL00113
     &(BB**2)))*T*((1./MQ)-MD*BR/(2.*MUM*MX)))                          AFL00114
      CP=FEX5*(MD*MB/(4.*BB*MBOT))*((1./MUM)-(MD*                       AFL00115
     &MQ/(2.*MX*(MUM**2)))*BR)                                          AFL00116
      R=FEX5*MBOT*BB/(SQRT(2.)*MUP)                                     AFL00117
      SP=FEX5*MD/(SQRT(2.)*BB*MBOT)*(1.+MB/(2.*MUM)-MB*MD*MQ            AFL00118
     &/(4.*MUP*MUM*MX)*BR)                                              AFL00119
      V=FEX5*MBOT*BB/(4.*SQRT(2.)*MB*MQ*MX)                             AFL00120
      FEX2=FEX1                                                         AFL00121
      UP=FEX5*MD*MQ*MB/(SQRT(6.)*BB*MX*MUM)                             AFL00122
        Q=FEX2*(1.+(MB/(2.*MUM))-MB*MQ*MD*BR/(4.*MUP*MUM*MX))           HIMAX 44
      ENDIF                                                             HIMAX 45
      XH1=0.                                                            HIMAX 46
      XH2=0.                                                            HIMAX 47
      XH3=0.                                                            HIMAX 48
      IF(MOD(J,2).EQ.0)GOTO 201                                         HIMAX 49
C.. +* DECAY                                                            HIMAX 50
      IF (IMODEL.EQ.1) THEN                                             HIMAX 51
C..SEMILEPTONIC B-->D,D* DECAY.                                         HIMAX 52
      IF(J.LT.7)THEN                                                    HIMAX 53
          XH1 = SQRT(T) * VM * OVER1  *                                 HIMAX 54
     &    ( M1 + M2 - IHEL * 2.*M1 * PC * VM /(M1+M2)  )                HIMAX 55
        ELSE                                                            HIMAX 56
          XH1 = SQRT(T) * VM * OVER1 *                                  HIMAX 57
     &    ( M1 + M2 - IHEL * 2.*M1 * PC * VM3 /(M1+M2)  )               HIMAX 58
        ENDIF                                                           HIMAX 59
      ELSEIF (IMODEL.EQ.2) THEN                                         HIMAX 60
        XH1 = SQRT(T) *( 10.9 - M1*PC*0.521) *0.6269*                   HIMAX 61
C    &  EXP( -0.0296*( Q2MAX - T) )                                     HIMAX 62
     &  EXP( -0.0145*( Q2MAX - T) )                                     HIMAX 63
      ELSEIF (IMODEL.EQ.3) THEN                                         HIMAX 64
        XH1 = SQRT(2.*T)*SQRT( M1**2+M2**2-T )                          HIMAX 65
      ELSEIF (IMODEL.EQ.4) THEN                                         HIMAX 66
        XH1 = SQRT(T) * OVER1 *                                         HIMAX 67
     &  ( (M1 + M2)*VM2 - IHEL * 2.*M1 * PC * VM /(M1+M2)  )            HIMAX 68
        ELSEIF (IMODEL.EQ.5) THEN                                       HIMAX 69
        IF(J.LT.9)THEN                                                  AFL00123
           XH1 = SQRT(Q2)*(F-IHEL*M1*PC*2.*GV)                          AFL00124
        ELSEIF(J.EQ.9)THEN                                              AFL00125
           XH1 = SQRT(Q2)*(AL-IHEL*M1*PC*2.*QV)                         AFL00126
        ELSEIF(J.EQ.11)THEN                                             AFL00127
           XH1 = SQRT(Q2)*(R-IHEL*M1*PC*2.*V)                           AFL00128
        ENDIF                                                           AFL00129
      ENDIF                                                             HIMAX 71
C.. -* DECAY                                                            HIMAX 72
      IF (IMODEL.EQ.1) THEN                                             HIMAX 73
        IF(J.LT.7) THEN                                                 HIMAX 74
C..SEMILEPTONIC B-->D,D* DECAY.                                         HIMAX 75
          XH2 = SQRT(T) * VM * OVER1 *                                  HIMAX 76
     &    ( M1 + M2 + IHEL * 2.*M1 * PC * VM /(M1+M2)  )                HIMAX 77
        ELSE                                                            HIMAX 78
          XH2 = SQRT(T) * VM * OVER1 *                                  HIMAX 79
     &    ( M1 + M2 + IHEL * 2.*M1 * PC * VM3 /(M1+M2)  )               HIMAX 80
        ENDIF                                                           HIMAX 81
      ELSEIF (IMODEL.EQ.2) THEN                                         HIMAX 82
        XH2 = SQRT(T) *( 10.9 + M1*PC*0.521) *0.6269*                   HIMAX 83
C    &  EXP( -0.0296*( Q2MAX - T) )                                     HIMAX 84
     &  EXP( -0.0145*( Q2MAX - T) )                                     HIMAX 85
      ELSEIF (IMODEL.EQ.3) THEN                                         HIMAX 86
        XH2 = SQRT(2.*T)*SQRT( M1**2+M2**2-T )                          HIMAX 87
      ELSEIF (IMODEL.EQ.4) THEN                                         HIMAX 88
        XH2 = SQRT(T) * OVER1 *                                         HIMAX 89
     &  ( (M1 + M2)*VM2 + IHEL * 2.*M1 * PC * VM /(M1+M2)  )            HIMAX 90
      ELSEIF (IMODEL.EQ.5) THEN                                         HIMAX 91
        IF(J.LT.9)THEN                                                  AFL00130
           XH2 = SQRT(Q2)*(F+IHEL*M1*PC*2.*GV)                          AFL00131
        ELSEIF(J.EQ.9)THEN                                              AFL00132
           XH2 = SQRT(Q2)*(AL+IHEL*M1*PC*2.*QV)                         AFL00133
        ELSEIF(J.EQ.11)THEN                                             AFL00134
           XH2 = SQRT(Q2)*(R+IHEL*M1*PC*2.*V)                           AFL00135
        ENDIF                                                           AFL00136
      ENDIF                                                             HIMAX 93
C.. 0* DECAY                                                            HIMAX 94
      IF (IMODEL.EQ.1) THEN                                             HIMAX 95
        IF(J.LT.7) THEN                                                 HIMAX 96
C..SEMILEPTONIC B-->D,D* DECAY.                                         HIMAX 97
          XH3 = VM * OVER1 / M2 *                                       HIMAX 98
     &    ( (M1**2 - M2**2 -T) * (M1 + M2) /2. - 2. * M1**2 * PC**2     HIMAX 99
     &     * VM /(M1+M2))                                               HIMAX100
        ELSE                                                            HIMAX101
          XH3 = VM * OVER1 / M2 *                                       HIMAX102
     &    ( (M1**2 - M2**2 -T) * (M1 + M2) /2. - 2. * M1**2 * PC**2     HIMAX103
     &     * VM3 /(M1+M2))                                              HIMAX104
        ENDIF                                                           HIMAX105
      ELSEIF (IMODEL.EQ.2) THEN                                         HIMAX106
        XH3 = 1./(2.*M2) * ( M1**2 - M2**2 - T) *10.90 *0.6269*         HIMAX107
C    &   EXP( -0.0296*( Q2MAX - T) )                                    HIMAX108
     &   EXP( -0.0145*( Q2MAX - T) )                                    HIMAX109
      ELSEIF (IMODEL.EQ.3) THEN                                         HIMAX110
        XH3 = ( M1**2-M2**2-T) * SQRT(M1**2+M2**2-T) / (SQRT(2.)*M2)    HIMAX111
      ELSEIF (IMODEL.EQ.4) THEN                                         HIMAX112
        XH3 = OVER1 / M2 *                                              HIMAX113
     &  ( (M1**2 - M2**2 -T) * (M1 + M2)*VM2 / 2. - 2. * M1**2 * PC**2  HIMAX114
     &   * VM4 /(M1+M2))                                                HIMAX115
      ELSEIF (IMODEL.EQ.5) THEN                                         HIMAX116
        IF(J.LT.9)THEN                                                  AFL00137
           XH3 = 1./(2.*M2)*((M1**2-M2**2-Q2)*F                         AFL00138
     &    +4.*M1**2*PC**2*AP)                                           AFL00139
        ELSEIF(J.EQ.9)THEN                                              AFL00140
           XH3 = 1./(2.*M2)*((M1**2-M2**2-Q2)*AL                        AFL00141
     &     +4.*M1**2*PC**2*CP)                                          AFL00142
        ELSEIF(J.EQ.11)THEN                                             AFL00143
           XH3 = 1./(2.*M2)*((M1**2-M2**2-Q2)*R                         AFL00144
     &     +4.*M1**2*PC**2*SP)                                          AFL00145
        ENDIF                                                           AFL00146
      ENDIF                                                             HIMAX119
      GOTO 200                                                          HIMAX120
 201  CONTINUE                                                          HIMAX121
C.. 0 DECAY                                                             HIMAX122
      IF (IMODEL.EQ.1) THEN                                             HIMAX123
        XH3 = 2. * M1 * PC * VM * OVER1                                 HIMAX124
      ELSEIF (IMODEL.EQ.2) THEN                                         HIMAX125
C       XH3 = 2.*M1*PC*1.8075*0.6269*EXP(-0.0296*(Q2MAX-T))             HIMAX126
        XH3 = 2.*M1*PC*1.8075*0.6269*EXP(-0.0145*(Q2MAX-T))             HIMAX127
      ELSEIF (IMODEL.EQ.3) THEN                                         HIMAX128
        XH3 = SQRT(2.)*PC*SQRT( M1**2+M2**2-T )                         HIMAX129
      ELSEIF (IMODEL.EQ.4) THEN                                         HIMAX130
        XH3 = 2. * M1 * PC * VM * OVER1                                 HIMAX131
      ELSEIF (IMODEL.EQ.5) THEN                                         HIMAX132
        IF(J.LT.10)XH3 = 2.*M1*PC*2.*Q                                  AFL00147
        IF(J.EQ.10)XH3 = 2.*M1*PC*2.*UP                                 AFL00148
      ENDIF                                                             HIMAX134
 200  H1MAX=(XH1**2+XH2**2+XH3**2)*PC                                   HIMAX135
      IF(H1MAX.GT.HIMAX)HIMAX=H1MAX                                     HIMAX136
 1    CONTINUE                                                          HIMAX137
      RETURN                                                            HIMAX138
      END                                                               HIMAX139
      SUBROUTINE INIARI(IFLV,ECMS)                                      INIARI 2
C--------------------------------------------------------------------   INIARI 3
C    B.Bloch-Devaux  April 1991 Implementation of ARIADNE inside HVFL02 INIARI 4
C!  initialisation routine for ARIADNE generator                        INIARI 5
C                                                                       INIARI 6
C     structure : subroutine                                            INIARI 7
C                                                                       INIARI 8
C     input     : IFLV : flavor code to be generated                    INIARI 9
C                 ECMS : center of mass energy                          INIARI10
C                                                                       INIARI11
C     output    : none                                                  INIARI12
C--------------------------------------------------------------------   INIARI13
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
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
C                                                                       LUN7CO12
      COMMON /AROPTN/ IAR(10),KAR(10),VAR(10)                           ARIACOM2
      COMMON /TEST/ ISPLIC                                              ARIACOM3
C    init gene                                                          INIARI17
      CALL ARIAIN                                                       INIARI18
C  Suppress call to LUEXEC in QQTOPS and LUEEVT                         INIARI19
      MSTJ(105) =0                                                      INIARI20
      MSTJ(101) =5                                                      INIARI21
      MSTJ(41) =0                                                       INIARI22
C   Print out ARIADNE parameters values                                 INIARI23
      CALL ARVALU                                                       INIARI24
      IFL  = IFLV                                                       INIARI25
      ECM  = ECMS                                                       INIARI26
      RETURN                                                            INIARI27
      END                                                               INIARI28
      SUBROUTINE INIBOK                                                 INIBOK 2
C------------------------------------------------------------------     INIBOK 3
C! Book some general standard histos                                    INIBOK 4
C    B.Bloch-Devaux April 1991                                          INIBOK 5
C------------------------------------------------------------------     INIBOK 6
      CALL HBOOK1(10001,'Outgoing FERMION Energy$',100,0.,50.,0.)       INIBOK 7
      CALL HBOOK1(10002,'OUTgoing ANTIFERMION Energy$',100,0.,50.,0.)   INIBOK 8
      CALL HBOOK1(10004,'POLAR ANGLE FERMION$',50,-1.,1.,0.)            INIBOK 9
      CALL HBOOK1(10005,'POLAR ANGLE ANTIFERMION$',50,-1.,1.,0.)        INIBOK10
      CALL HBOOK1(10003,'ISR PHOTON Energy IF ANY$',50,0.,25.,0.)       INIBOK11
      CALL HBOOK1(10008,'FSR PHOTON Energy IF ANY$',60,0.,30.,0.)       INIBOK12
      CALL HBOOK1(10006,'event type produced',60,0.,60.,0.)             INIBOK13
      CALL HBOOK1(10007,'Final Multiplicity',50,0.,100.,0.)             INIBOK14
      RETURN                                                            INIBOK15
      END                                                               INIBOK16
      SUBROUTINE INIBRE(IFLV,ECMS)                                      INIBRE 2
C--------------------------------------------------------------------   INIBRE 3
C      B. BLOCH-DEVAUX April  1991 Implementation of BREM5 inside HVFL02BBL00167
C                                                                       INIBRE 5
C!   Initialisation routine of BREM5  generator                         INIBRE 6
C     structure : subroutine                                            INIBRE 8
C                                                                       INIBRE 9
C     input     : none                                                  INIBRE10
C                                                                       INIBRE11
C     output    : IFL : flavor code to be generated                     INIBRE12
C                 ECMS: center of mass energy                           INIBRE13
C--------------------------------------------------------------------   INIBRE14
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
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
C                                                                       LUN7CO12
      PARAMETER ( ICOZ = 23 , ICOH = 25 , ICOT = 6 )                    BBL002 1
C                                                                       BBL00169
C     BREM5 INPUT VARIABLES                                             BBL00170
C                                                                       BBL00171
      REAL*8 RDATIN                                                     BBL00172
      COMMON/INDAT/RDATIN(45)                                           BREMCOM2
      DIMENSION TABL(45)                                                BBL00174
      INTEGER ALTABL                                                    BBL00175
C                                                                       BBL00176
C      BREM5 PARAMETERS     DEFAULT VALUES                              BBL00177
C                         N GENERATED EVENTS---IRRELEVANT               BBL00178
      RDATIN(1)=1000.D0                                                 BBL00179
C                         BEAM ENERGY                                   BBL00180
      RDATIN(2)=46.1D0                                                  BBL00181
C                         MZ                                            BBL00182
      RDATIN(3)=92.D0                                                   BBL00183
      RDATIN(4)=10.D0                                                   BBL00184
      RDATIN(5)=170.D0                                                  BBL00185
      RDATIN(6)=1.D0                                                    BBL00186
      RDATIN(7)=.2D0                                                    BBL00187
      RDATIN(8)=.2D0                                                    BBL00188
      RDATIN(9)=180.D0                                                  BBL00189
C                         K0 MINIMUM HARD PHOTON ENERGY/EBEAM           BBL00190
      RDATIN(10)=.01D0                                                  BBL00191
C                         WEAK ISOSPIN AND CHARGE OF INCOMING FERMION   BBL00192
      RDATIN(11)=-0.5D0                                                 BBL00193
      RDATIN(12)=-1.D0                                                  BBL00194
C                           "    "      "     "      OUTGOING   "       BBL00195
      RDATIN(13)=-0.5D0                                                 BBL00196
      RDATIN(14)=-1.D0                                                  BBL00197
C                             MH                                        BBL00198
      RDATIN(15)=100.D0                                                 BBL00199
C                             MT                                        BBL00100
      RDATIN(16)=60.D0                                                  BBL00101
C    BEAM POLARIZATION ( WILL BE REVERSED IN MIDDLE OF RUN IF NON ZERO) BBL00102
      RDATIN(17)=0.D0                                                   BBL00103
      RDATIN(18)=1.D0                                                   BBL00104
      RDATIN(19)=.1056D0                                                BBL00105
      RDATIN(20)=2.D0                                                   BBL00106
      RDATIN(21)=19.D0                                                  BBL00107
      RDATIN(22)=1.D0                                                   BBL00108
      RDATIN(23)=219.D0                                                 BBL00109
      RDATIN(24)=0.D0                                                   BBL00110
      ITYPE = 11                                                        BBL00111
C   DEFAULT SETUP IS D DBAR PAIR PRODUCTION                             BBL00112
C                                                                       BBL00113
      NABRIN=NAMIND('GBRE')                                             BBL00114
      ID=IW(NABRIN)                                                     BBL00115
      IF (ID.NE.0) THEN                                                 BBL00116
          RDATIN(2) = RW(ID+2)                                          BBL00117
          RDATIN(3) = RW(ID+3)                                          BBL00118
          RDATIN(4) = RW(ID+9)                                          BBL00119
          RDATIN(5) = RW(ID+10)                                         BBL00120
          RDATIN(6) = RW(ID+7)                                          BBL00121
          RDATIN(7) = RW(ID+11)                                         BBL00122
          RDATIN(8) = RW(ID+12)                                         BBL00123
          RDATIN(9) = RW(ID+13)                                         BBL00124
          RDATIN(10) = RW(ID+8)                                         BBL00125
          RDATIN(15) = RW(ID+4)                                         BBL00126
          RDATIN(16) = RW(ID+5)                                         BBL00127
          RDATIN(17) = RW(ID+14)                                        BBL00128
          RDATIN(24) = RW(ID+6)                                         BBL00129
          RDATIN(23) = RW(ID+15)                                        BBL00130
          ITYPE = NINT(RW(ID+1))                                        BBL00131
      ENDIF                                                             BBL00132
C                                                                       BBL00133
C    SEt up default masses for LUND : Z0 , Higgs  , Top                 BBL00134
C                                                                       BBL00135
      PMAS(LUCOMP(ICOZ),1) = RDATIN(3)                                  BBL00136
      PMAS(LUCOMP(ICOH),1) = RDATIN(15)                                 BBL00137
      PMAS(LUCOMP(ICOT),1) = RDATIN(16)                                 BBL00138
C                                                                       BBL00139
C         Leptons                                                       BBL00140
          IF (ITYPE.LE.3) THEN                                          BBL00141
             WRITE(IW(6),'(1X,''++++++YOU REQUESTED TO GENERATE LEPTONS.BBL00142
     & ..STOP  !! PLEASE USE ANOTHER GENERATOR THAN HVFL02!  ++++++'')')BBL00143
             CALL EXIT                                                  BBL00144
          ELSE                                                          BBL00145
C    Quarks                                                             BBL00146
              ITY=ITYPE-10                                              BBL00147
              RDATIN(14) = LUCHGE(ITY)/3.                               BBL00148
              RDATIN(13) = SIGN(0.5D0,RDATIN(14))                       BBL00149
              RDATIN(18) = 3.D0                                         BBL00150
              RDATIN(19) = PMAS(LUCOMP(ITY),1)                          BBL00151
          ENDIF                                                         BBL00152
      NWB=24                                                            BBL00153
      IFLV=ITYPE-10                                                     BBL00154
      ECMS = 2.*RDATIN(2)                                               BBL00155
      IF (RDATIN(24).NE.0.) THEN                                        BBL00156
        WRITE(IW(6),'(1X,''++++++YOU SHOULD NOT REQUEST FINAL STATE RADIBBL00157
     &ATION BY BREM5'',/,'', IT WILL BE GENERATED WITHIN PARTON SHOWER  BBL00158
     &BY JETSET 7.3 OR QCD CASCADE DIPOLE BY ARIADNE 3.3'')')           BBL00159
        RDATIN(24) = 0.D0                                               BBL00160
      ENDIF                                                             BBL00161
C                                                                       BBL00162
C       INITIALIZE BREM5                                                BBL00163
C                                                                       BBL00164
        CALL BREMIN(ITYPE)                                              BBL00165
C                                                                       BBL00166
C   dump the generator parameters for this run in a bank                BBL00167
C assume all parameters are real and stored as a single row             BBL00168
      DO 11 I=1,NWB                                                     BBL00169
 11   TABL(I) = RDATIN(I)                                               BBL00170
      IND = ALTABL('KBRE',NWB,1,TABL,'2I,(F)','C')                      BBL00171
C                                                                       BBL00172
      CALL PRTABL('KBRE',0)                                             BBL00173
      RETURN                                                            INIBRE17
      END                                                               INIBRE18
      SUBROUTINE INIDDC(IDEC)                                           INIDDC 2
C ------------------------------------------------------------------    INIDDC 3
C - B. Bloch  - March  1994                                             INIDDC 4
C! Initialization routine of selected decay modes for a given chain     INIDDC 5
C  IDEC = 0  NO selected modes required                                 INIDDC 6
C  IDEC = 1  selected modes required for first chain                    INIDDC 7
C ------------------------------------------------------------------    INIDDC 8
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
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
C                                                                       LUN7CO12
      COMMON /CHAINS/ KFDAU,KFMOTH,KFCOMP(2)                            CHAINS 2
C   KFDAU  = internal JETSET 7.3 code for daughter decayed particle     CHAINS 3
C   KFMOTH = internal JETSET 7.3 code for mother of decayed particle    CHAINS 4
C   KFCOMP(2)internal JETSET 7.3 code for companions of decayed particleCHAINS 5
      DIMENSION ITABL(200)                                              INIDDC12
      INTEGER ALTABL                                                    INIDDC13
C                                                                       INIDDC14
      IDEC = 0                                                          INIDDC15
      IUT = IW(6)                                                       INIDDC16
C GET selected decay modes for the KF decay  scheme GDDC CARD           INIDDC17
      JDEC=IW(NAMIND('GDDC'))                                           INIDDC18
      IF(JDEC.GT.0)THEN                                                 INIDDC19
        KFDAU = IW(JDEC+1)                                              INIDDC20
        KFMOTH = IW(JDEC+2)                                             INIDDC21
        KFCOMP(1) = IW(JDEC+3)                                          INIDDC22
        KFCOMP(2) = IW(JDEC+4)                                          INIDDC23
        KC = LUCOMP(KFDAU)                                              INIDDC24
        IF ( KC.GT.0) THEN                                              INIDDC25
C   Check length consistency                                            INIDDC26
           NDEC  = MDCY(KC,3)                                           INIDDC27
           IF(IW(JDEC)-4.NE.NDEC) GO TO 99                              INIDDC28
           IDEC = IDEC +1                                               INIDDC29
           JDEC1 = JDEC                                                 INIDDC30
           CALL UCOPY ( IW(JDEC+1),ITABL(1),NDEC+4)                     INIDDC31
        ELSE                                                            INIDDC32
           GO TO 98                                                     INIDDC33
        ENDIF                                                           INIDDC34
      ELSE                                                              INIDDC35
         CALL UFILL (ITABL,1,NDEC+4,1)                                  INIDDC36
      ENDIF                                                             INIDDC37
C                                                                       INIDDC38
C   Issue the relevant parameters                                       INIDDC39
C                                                                       INIDDC40
      IF (IDEC.GT.0) THEN                                               INIDDC41
        CALL HBOOK1(10040,'NUMBER OF DECAY CHAIN PER EVENT ',5,0.,5.,0.)INIDDC42
        WRITE (IUT,1000) KFDAU,KFMOTH,KFCOMP                            INIDDC43
        IF (IDEC.GT.0) WRITE(IUT,1005) (IW(JDEC1+K),K=5,NDEC+4)         INIDDC44
 1000 FORMAT(15X,98('*'),/,/,40X,' HVFL04 ADAPTED  ',                   INIDDC45
     $'To handle SELECTED decay chain    ',/,                           INIDDC46
     $'Particle code from decay of mother with companions ',/,4I10)     INIDDC47
 1005 FORMAT (10X,'* First chain selected decays : ',/,2X,50I2,4X,'*')  INIDDC48
C   dump the selected decay modes for this run in a bank  KDDC          INIDDC49
C  all parameters are integer and stored as 1 row                       INIDDC50
        IND = ALTABL('KDDC',NDEC+4,1,ITABL,'2I,(I)','C')                INIDDC51
C                                                                       INIDDC52
        CALL PRTABL('KDDC',0)                                           INIDDC53
      ENDIF                                                             INIDDC54
      RETURN                                                            INIDDC55
 99   WRITE(IUT,'('' ===INIDDC  : INCONSISTENT DECAY MODES NUMBERS :'', INIDDC56
     $ 2I10)') IW(JDEC),NDEC                                            INIDDC57
      RETURN                                                            INIDDC58
 98   WRITE(IUT,'('' ===INIDDC  : UNKNOWN PARTICLE CODE :'',I10)') KFDAUINIDDC59
      END                                                               INIDDC60
      SUBROUTINE INIDEC(IDEC)                                           INIDEC 2
C ------------------------------------------------------------------    INIDEC 3
C - B. Bloch  - February 1991                                           INIDEC 4
C! Initialization routine of selected decay modes for B's               INIDEC 5
C  IDEC = 0  NO selected modes required                                 INIDEC 6
C  IDEC = 1  selected modes required for first B meson                  INIDEC 7
C  IDEC = 2  selected modes required for second B meson                 INIDEC 8
C  IDEC = 3  selected modes required for both B mesons                  INIDEC 9
C ------------------------------------------------------------------    INIDEC10
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
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
C                                                                       LUN7CO12
      PARAMETER (LBPOS=100,NBU=521,NBD=511,NBS=531,NBC=541)             BCODES 2
      PARAMETER (NLB=5122,NXB= 5132)                                    BBL00511
C     LBPOS  = internal JETSET 7.3 code for B meson                     BCODES 3
      PARAMETER (NXB0=5232,NOB= 5332)                                   BBL007 9
C     NBU    = internal JETSET 7.3 code for Bu meson                    BCODES 4
C     NBD    = internal JETSET 7.3 code for Bd meson                    BCODES 5
C     NBS    = internal JETSET 7.3 code for Bs meson                    BCODES 6
C     NBC    = internal JETSET 7.3 code for Bc meson                    BCODES 7
C     NLB    = internal JETSET 7.3 code for /\B BARYON                  BBL00512
C     NXB    = internal JETSET 7.3 code for XIB BARYON                  BBL00513
C     NXB0   = internal JETSET 7.3 code for XIB0 BARYON                 BBL00710
C     NOB    = internal JETSET 7.3 code for OMEGAB BARYON               BBL00711
      DIMENSION ITABL(200)                                              INIDEC14
      INTEGER ALTABL                                                    INIDEC15
C                                                                       INIDEC16
      IDEC = 0                                                          INIDEC17
      IUT = IW(6)                                                       INIDEC18
C GET selected decay modes for each of the B decays  GDC1,GDC2 CARDS    INIDEC19
      KC = LUCOMP(LBPOS)                                                INIDEC20
      IF (KC.GT.0) THEN                                                 INIDEC21
         NDEC=MDCY(KC,3)                                                INIDEC22
         JDEC=IW(NAMIND('GDC1'))                                        INIDEC23
         IF(JDEC.GT.0)THEN                                              INIDEC24
C   Check length consistency                                            INIDEC25
            IF(IW(JDEC).NE.NDEC) GO TO 99                               INIDEC26
            IDEC = IDEC +1                                              INIDEC27
            JDEC1 = JDEC                                                INIDEC28
            CALL UCOPY ( IW(JDEC+1),ITABL(1),NDEC)                      INIDEC29
         ELSE                                                           INIDEC30
            CALL UFILL (ITABL,1,NDEC,1)                                 INIDEC31
         ENDIF                                                          INIDEC32
         JDEC=IW(NAMIND('GDC2'))                                        INIDEC33
         IF(JDEC.GT.0)THEN                                              INIDEC34
            IF(IW(JDEC).NE.NDEC) GO TO 99                               INIDEC35
            IDEC = IDEC +2                                              INIDEC36
            JDEC2 = JDEC                                                INIDEC37
            CALL UCOPY ( IW(JDEC+1),ITABL(NDEC+1),NDEC)                 INIDEC38
         ELSE                                                           INIDEC39
            CALL UFILL (ITABL,NDEC+1,2*NDEC,1)                          INIDEC40
         ENDIF                                                          INIDEC41
      ENDIF                                                             INIDEC42
C                                                                       INIDEC43
C   Issue the relevant parameters                                       INIDEC44
C                                                                       INIDEC45
      IF (IDEC.GT.0) THEN                                               INIDEC46
        WRITE (IUT,1000)                                                INIDEC47
      IF (IDEC.EQ.1.OR.IDEC.EQ.3) WRITE(IUT,1005) (IW(JDEC1+K),K=1,NDEC)INIDEC48
      IF (IDEC.GE.2) WRITE (IUT,1006) (IW(JDEC2+K),K=1,NDEC)            INIDEC49
 1000 FORMAT(15X,98('*'),/,/,40X,' HVFL02 ADAPTED  ',                   INIDEC50
     $'TO HANDLE SELECTED B MESON DECAYS ',/)                           INIDEC51
 1005 FORMAT (10X,'* First  B meson selected decays : ',40I2,4X,'*')    BBL00558
 1006 FORMAT (10X,'* Second B meson selected decays : ',40I2,4X,'*')    BBL00559
C                                                                       INIDEC54
C   dump the selected decay modes for this run in a bank  KDEC          INIDEC55
C  all parameters are integer and stored as 2 rows ( one for each B)    INIDEC56
      IND = ALTABL('KDEC',NDEC,2,ITABL,'2I,(I)','C')                    INIDEC57
C                                                                       INIDEC58
      CALL PRTABL('KDEC',0)                                             INIDEC59
      CALL HBOOK1(10009,' # B MESONS produced per event',5,0.,5.,0.)    INIDEC60
      CALL HTABLE(10019,' # BD VS BU  produced per event',5,0.,5.,5,0., BBL00174
     $5., 0.)                                                           BBL00175
      CALL HBPRO(10019,0.)                                              BBL00176
      CALL HBOOK1(10039,' # BS MESONS produced per event',5,0.,5.,0.)   BBL00177
      CALL HBOOK1(10049,' # BC MESONS produced per event',5,0.,5.,0.)   BBL00178
      CALL HTABLE(10029,' # BD FIRST VS #BU FIRST per event',5,0.,5.,   BBL00179
     $ 5,0.,5., 0.)                                                     BBL00180
      ENDIF                                                             INIDEC61
      RETURN                                                            INIDEC62
 99   WRITE(IUT,'('' ===INIDEC  : INCONSISTENT DECAY MODES NUMBERS :'', INIDEC63
     $ 2I10)') IW(JDEC),NDEC                                            INIDEC64
      END                                                               INIDEC65
      SUBROUTINE INIDY3(IFLV,ECMS)                                      INIDY3 2
C--------------------------------------------------------------------   INIDY3 3
C    B.Bloch-Devaux  April 1991 Implementation of DYMU3 inside HVFL02   BBL00181
C!  initialisation routine for DYMU3 generator                          BBL00182
C                                                                       INIDY3 7
C     structure : subroutine                                            INIDY3 8
C                                                                       INIDY3 9
C     input     : none                                                  INIDY310
C                                                                       INIDY311
C     output    : IFLV : flavor code to be generated                    INIDY312
C                 ECMS : center of mass energy                          INIDY313
C--------------------------------------------------------------------   INIDY314
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
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
C                                                                       LUN7CO12
      PARAMETER ( ICOZ = 23 , ICOH = 25 , ICOT = 6 )                    BBL002 1
      COMMON /CONST/ ALFA,PI,ALFA1                                      DYMUCOM2
      COMMON /RUNPAR/ SOLD,ID2,ID3,FINEXP,POIDS,INTERF,XK0              DYMUCOM3
      COMMON /WEAK/ AEL,AMU,AMZ,GAMM,SW2,CA2,CV2,CA2CV2,COL,T3,QI       DYMUCOM4
      COMMON /BEAM/ S0,EBEAM                                            DYMUCOM5
      COMMON /TAU / TAUV,CPTAU,HEL,PITAU(4)                             DYMUCOM6
      COMMON/WEAKQ/WEAKC(11,6),XSECT(6),XTOT                            DYMUCOM7
      COMMON/ZDUMP / IDEBU , NEVT                                       DYMUCOM8
      COMMON/RESULT/ SIGBOR,SIGTOT,ERRSIG,ASYTOT,ERRASY                 DYMUCOM9
      DIMENSION TABL(25),ISEED(3)                                       BBL00185
      INTEGER ALTABL                                                    BBL00186
      XTOT = 0.                                                         BBL00187
C                                                                       BBL00188
C      DYMU3 PARAMETERS     DEFAULT VALUES                              BBL00189
C                         BEAM ENERGY                                   BBL00190
      EBEAM    =46.1                                                    BBL00191
C                         MZ,GAMZ,SIN2 (EFFECTIVE)                      BBL00192
      AMZ      =91.2                                                    BBL00193
      GAMM = 2.56                                                       BBL00194
      SW2 = .2293                                                       BBL00195
      SOLD = -1.                                                        BBL00196
      ID2 = 0                                                           BBL00197
      ID3 = 0                                                           BBL00198
      IEXPO = 1                                                         BBL00199
      FINEXP =-1.                                                       BBL00200
      POIDS = 1.                                                        BBL00201
      INTERF = 0                                                        BBL00202
      IDEBU = 0                                                         BBL00203
      NEVT = 0                                                          BBL00204
      TAUV = 0.                                                         BBL00205
C                         K0 MINIMUM HARD PHOTON ENERGY/EBEAM           BBL00206
      XK0 = 0.003                                                       BBL00207
C                         WEAK ISOSPIN AND CHARGE OF OUTGOING FERMION   BBL00208
      QCDFAC = 1.04                                                     BBL00209
      T3  =-0.5                                                         BBL00210
      QI =  -1./3.                                                      BBL00211
      COL = 3.*QCDFAC                                                   BBL00212
      ITYPE = 15                                                        BBL00213
      NQUA = 0                                                          BBL00214
C   DEFAULT SETUP IS BB BAR PAIR PRODUCTION                             BBL00215
C                                                                       BBL00216
      NAGDYM=NAMIND('GDYM')                                             BBL00217
      ID=IW(NAGDYM)                                                     BBL00218
      IF (ID.NE.0) THEN                                                 BBL00219
          ITYPE = NINT(RW(ID+1))                                        BBL00220
          EBEAM     = RW(ID+2)                                          BBL00221
          AMZ       = RW(ID+3)                                          BBL00222
          GAMM      = RW(ID+4)                                          BBL00223
          SW2       = RW(ID+5)                                          BBL00224
          IDEBU     = IW(ID+6)                                          BBL00225
          ID2       = RW(ID+7)                                          BBL00226
          TAUV       = RW(ID+8)                                         BBL00227
          FINEXP    = RW(ID+9)                                          BBL00228
          POIDS     = RW(ID+10)                                         BBL00229
          XK0       = RW(ID+11)                                         BBL00230
          QCDFAC    = RW(ID+12)                                         BBL00231
          NQUA      = IW(ID+13)                                         BBL00232
          IFIRST    = IW(ID+14)                                         BBL00233
          NEVMA     = IW(ID+15)                                         BBL00234
      ENDIF                                                             BBL00235
      IF (FINEXP.NE.0.) THEN                                            BBL00236
        WRITE(IW(6),'(1X,''++++++YOU SHOULD NOT REQUEST FINAL STATE RADIBBL00237
     &ATION BY DYMU3'',/,'', IT WILL BE GENERATED WITHIN PARTON SHOWER  BBL00238
     &BY JETSET 7.3 OR QCD CASCADE DIPOLE BY ARIADNE 3.3'')')           BBL00239
        FINEXP = -1.                                                    BBL00240
      ENDIF                                                             BBL00241
C                                                                       BBL00242
C    SEt up default masses for LUND : Z0                                BBL00243
C                                                                       BBL00244
      PMAS(LUCOMP(ICOZ),1) = AMZ                                        BBL00245
      AEL = ULMASS(11)                                                  BBL00246
C         Leptons                                                       BBL00247
          IF (ITYPE.LE.3) THEN                                          BBL00248
             WRITE(IW(6),'(1X,''++++++YOU REQUESTED TO GENERATE LEPTONS.BBL00249
     & ..STOP  !! PLEASE USE ANOTHER GENERATOR THAN HVFL02!  ++++++'')')BBL00250
             CALL EXIT                                                  BBL00251
          ELSEIF ( ITYPE.GT.10) THEN                                    BBL00252
C    Quarks                                                             BBL00253
              ITY=ITYPE-10                                              BBL00254
              QI         = LUCHGE(ITY)/3.                               BBL00255
              T3         = SIGN(0.5,QI)                                 BBL00256
              COL        = 3.*QCDFAC                                    BBL00257
              AMU        = PMAS(LUCOMP(ITY),1)                          BBL00258
          ELSEIF ( ITYPE.EQ.10)  THEN                                   BBL00259
C    Save initial seeds to start the real events                        BBL00260
           CALL RDMOUT(ISEED)                                           BBL00261
           I1 = ISEED(1)                                                BBL00262
           I2 = ISEED(2)                                                BBL00263
           I3 = ISEED(3)                                                BBL00264
C    Quarks mixture                                                     BBL00265
           DO 15  II=1,6                                                BBL00266
 15         XSECT(II)=0.                                                BBL00267
C   No DEBUG needed here!                                               BBL00268
             IDEBU = 0                                                  BBL00269
             DO 6 II = 1,NQUA                                           BBL00270
                KTY = IFIRST+II-1                                       BBL00271
                NEVT = 0                                                BBL00272
                ITY=KTY-10                                              BBL00273
                QI     = LUCHGE(ITY)/3.                                 BBL00274
                T3     = SIGN(0.5,QI)                                   BBL00275
                COL    = 3.*QCDFAC                                      BBL00276
                AMU    = PMAS(LUCOMP(ITY),1)                            BBL00277
                CALL DYMUIN(KTY)                                        BBL00278
C    COPY coupling constants for this type in an array                  BBL00279
                CALL UCOPY(AEL,WEAKC(1,KTY-10),11)                      BBL00280
                DO 66 KEV=1,NEVMA                                       BBL00281
                   CALL DYMUS(WEI)                                      BBL00282
  66            CONTINUE                                                BBL00283
                CALL FINISH(KTY,NEVMA)                                  BBL00284
C   Store cross-sections for each process                               BBL00285
                XSECT(KTY-10) = SIGTOT                                  BBL00286
  6          CONTINUE                                                   BBL00287
C   Store total cross-section                                           BBL00288
             XTOT = XSECT(1)                                            BBL00289
             DO 75 II = 2,NQUA                                          BBL00290
  75         XTOT = XTOT+XSECT(II)                                      BBL00291
             KAUX = IFIRST-10                                           BBL00292
             DO 7 II=2,NQUA                                             BBL00293
  7          XSECT(KAUX+II-1) = XSECT(KAUX+II-1-1)+XSECT(KAUX+II-1)     BBL00294
C     Normalize                                                         BBL00295
             DO 8 II = 1,NQUA                                           BBL00296
  8          XSECT(KAUX+II-1) = XSECT(KAUX+II-1)/XSECT(KAUX+NQUA-1)     BBL00297
             WRITE(IW(6),100)   XSECT                                   BBL00298
 100         FORMAT('       INTEGRATED CROSS SECTIONS FOR QUARKS ',     BBL00299
     $              /,9X,6F10.4)                                        BBL00300
C    Restore initial seed                                               BBL00301
             CALL RMARIN(I1,I2,I3)                                      BBL00302
          ENDIF                                                         BBL00303
C   Restore DEBUG if any                                                BBL00304
        IDEBU     = IW(ID+6)                                            BBL00305
      IFLV = ITYPE-10                                                   BBL00306
      ECMS = 2.*EBEAM                                                   BBL00307
C                                                                       BBL00308
C       INITIALIZE DYMU3                                                BBL00309
C                                                                       BBL00310
        CALL DYMUIN(ITYPE)                                              BBL00311
C                                                                       BBL00312
C   dump the generator parameters for this run in a bank                BBL00313
C assume all parameters are real and stored as a single row             BBL00314
      TABL(1) = FLOAT(ITYPE)                                            BBL00315
      TABL(2) = EBEAM                                                   BBL00316
      TABL(3) = AMZ                                                     BBL00317
      TABL(4) = GAMM                                                    BBL00318
      TABL(5) = SW2                                                     BBL00319
      TABL(6) = FLOAT(ID2)                                              BBL00320
      TABL(7) = FLOAT(IEXPO)                                            BBL00321
      TABL(8) = FINEXP                                                  BBL00322
      TABL(9) = POIDS                                                   BBL00323
      TABL(10) = XK0                                                    BBL00324
      TABL(11) = QCDFAC                                                 BBL00325
      TABL(12) = FLOAT(NQUA)                                            BBL00326
      TABL(13) = FLOAT(IFIRST)                                          BBL00327
      TABL(14) = FLOAT(NEVMA)                                           BBL00328
      TABL(15) = TAUV                                                   BBL00329
      NWB = 15                                                          BBL00330
      IND = ALTABL('KDYM',NWB,1,TABL,'2I,(F)','C')                      BBL00331
C                                                                       BBL00332
      CALL PRTABL('KDYM',0)                                             BBL00333
      RETURN                                                            INIDY317
      END                                                               INIDY318
      SUBROUTINE INILAM(ILAM)                                           INILAM 2
C ------------------------------------------------------------------    INILAM 3
C - B. Bloch  - NOVEMBER 1993                                           INILAM 4
C! initialization routine of special decay dynamics for b baryons       INILAM 5
C ------------------------------------------------------------------    INILAM 6
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      COMMON/POLAMB/ISPINL,IFORML,W0LAMB                                POLAMB 2
C ISPINL : Switch on/off spin effects in semileptonic B_Baryons decays  POLAMB 3
C IFORML : Switch on/off form factor  in semileptonic B_Baryons decays  POLAMB 4
C W0LAMB : form factor value if used                                    POLAMB 5
      DIMENSION TABL(10)                                                INILAM 9
      INTEGER ALTABL                                                    INILAM10
C                                                                       INILAM11
C GET B BARYON DECAY PARAMETERS                                         INILAM12
          ILAM = 1                                                      INILAM13
          JLAM=IW(NAMIND('GLAM'))                                       INILAM14
          IF(JLAM.GT.0)THEN                                             INILAM15
            ISPINL = IW(JLAM+1)                                         INILAM16
            IFORML = IW(JLAM+2)                                         INILAM17
            W0LAMB = RW(JLAM+3)                                         INILAM18
            IF ( ISPINL+IFORML.EQ.0) ILAM = 0                           INILAM19
          ELSE                                                          INILAM20
            ISPINL = 0                                                  INILAM21
            IFORML = 1                                                  INILAM22
            W0LAMB = 0.89                                               INILAM23
          ENDIF                                                         INILAM24
C                                                                       INILAM25
C   Issue the relevant parameters                                       INILAM26
C                                                                       INILAM27
      IUT = IW(6)                                                       INILAM28
      WRITE (IUT,1000)                                                  INILAM29
      WRITE (IUT,1001) ISPINL,IFORML,W0LAMB                             INILAM30
 1000 FORMAT(30X,78('*'),/,/,40X,' HERE IS LUND7.3 ADAPTED TO HANDLE',  INILAM31
     $ 'BBaryons decays')                                               INILAM32
 1001 FORMAT (30X,'*  SPIN effects on/off , Form factor on/off , value 'INILAM33
     $ ,/,30X,10X,I10,10X,I10,F10.4,10X,'*')                            INILAM34
C                                                                       INILAM35
      CALL HBOOK1(10031,'Electron spectrum b baryon decay',50,0.,50.,0.)INILAM36
      CALL HBOOK1(10032,'Muon     spectrum b baryon decay',50,0.,50.,0.)INILAM37
      CALL HBOOK1(10033,'Tau      spectrum b baryon decay',50,0.,50.,0.)INILAM38
C                                                                       INILAM39
C   dump the generator parameters for this run in a bank                INILAM40
C        all parameters are real and stored as a single row             INILAM41
      TABL(1) = FLOAT(ISPINL)                                           INILAM42
      TABL(2) = FLOAT(IFORML)                                           INILAM43
      TABL(3) = W0LAMB                                                  INILAM44
      NWB = 3                                                           INILAM45
      IND = ALTABL('KLAM',NWB,1,TABL,'2I,(F)','C')                      INILAM46
C                                                                       INILAM47
      CALL PRTABL('KLAM',0)                                             INILAM48
      RETURN                                                            INILAM49
      END                                                               INILAM50
      SUBROUTINE INILUN(IFL,ECMS)                                       INILUN 2
C ------------------------------------------------------------------    INILUN 3
C - B. Bloch  - November 1990                                           INILUN 4
C! Initialization routine of LUND 7.3 generator                         INILUN 5
C ------------------------------------------------------------------    INILUN 6
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
C -----------------------------------------------------------------     INILUN 8
C - get the LUND flavour IFL if given on data card                      INILUN 9
         NLUND = NAMIND ('GLUN')                                        INILUN10
         JLUND = IW(NLUND)                                              INILUN11
         IF (JLUND .NE. 0) THEN                                         INILUN12
            IFL = IW(JLUND+1)                                           INILUN13
            ECMS =  RW(JLUND+2)                                         INILUN14
         ELSE                                                           INILUN15
            IFL = 0                                                     INILUN16
            ECMS = 92.2                                                 INILUN17
         ENDIF                                                          INILUN18
      RETURN                                                            INILUN27
      END                                                               INILUN28
      SUBROUTINE INIMIX(IMIX)                                           INIMIX 2
C ------------------------------------------------------------------    INIMIX 3
C - B. Bloch  - DECEMBER 1988                                           INIMIX 4
C! Initialization routine of MIXING and CPV constants                   INIMIX 5
C ------------------------------------------------------------------    INIMIX 6
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      COMPLEX PSQD,PSQS                                                 MIXING 2
      COMMON/ MIXING /XD,YD,CHID,RD,XS,YS,CHIS,RS,PSQD,PSQS             MIXING 3
      COMMON/ PROBMI /CDCPBA,CDCPB,CSCPBA,CSCPB                         MIXING 4
      PARAMETER (LNBRCP=4)                                              USERCP 2
      COMMON/USERCP/NPARCP,JCODCP(LNBRCP),RHOCPM                        USERCP 3
      COMPLEX RHOCPM                                                    USERCP 4
      DIMENSION TABL(20)                                                INIMIX10
      INTEGER ALTABL                                                    INIMIX11
C                                                                       INIMIX12
C GET MIXING PARAMETERS IF GIVEN IN STEERING DATA CARDS   881024        INIMIX13
C LOOK AT A.FALVARD AND B.BLOCH-DEVAUX- ALEPH NOTE # ?                  INIMIX14
C FOR DEFINITIONS                                                       INIMIX15
          IMIX = 0                                                      INIMIX16
          JMIX=IW(NAMIND('GMIX'))                                       INIMIX17
          IF(JMIX.GT.0)THEN                                             INIMIX18
            XD=RW(JMIX+1)                                               INIMIX19
            YD=RW(JMIX+2)                                               INIMIX20
            XS=RW(JMIX+3)                                               INIMIX21
            YS=RW(JMIX+4)                                               INIMIX22
            IMIX = 1                                                    INIMIX23
          ELSE                                                          INIMIX24
            XD=0.                                                       INIMIX25
            YD=0.                                                       INIMIX26
            XS=0.                                                       INIMIX27
            YS=0.                                                       INIMIX28
          ENDIF                                                         INIMIX29
C GET CP VIOLATION PARAMETERS IN DB=2 TRANSITIONS                       INIMIX30
          JCPV=IW(NAMIND('GCPV'))                                       INIMIX31
          IF(JCPV.GT.0)THEN                                             INIMIX32
            PMOD=RW(JCPV+1)                                             INIMIX33
            PHAD=RW(JCPV+2)                                             INIMIX34
            PMOS=RW(JCPV+3)                                             INIMIX35
            PHAS=RW(JCPV+4)                                             INIMIX36
            IMIX = 1                                                    INIMIX37
          ELSE                                                          INIMIX38
            PMOD = 1.                                                   INIMIX39
            PMOS = 1.                                                   INIMIX40
            PHAD = 0.                                                   INIMIX41
            PHAS = 0.                                                   INIMIX42
          ENDIF                                                         INIMIX43
            PSQD=CMPLX(PMOD*COS(PHAD),PMOD*SIN(PHAD))                   INIMIX44
            PSQS=CMPLX(PMOS*COS(PHAS),PMOS*SIN(PHAS))                   INIMIX45
C GET CP VIOLATION IN A SPECIFIC B DECAY WITH A CP EIGENSTATE           INIMIX46
          JSTA=IW(NAMIND('GSTA'))                                       INIMIX47
          NPARCP=0                                                      INIMIX48
          IF(JSTA.NE.0)THEN                                             INIMIX49
            NPARCP=IW(JSTA+1)                                           INIMIX50
            IF (NPARCP.GT.4) THEN                                       INIMIX51
             WRITE (IW(6),'(1X,''error in GSTA more than 4 part STOP- ''INIMIX52
     +                 ,I3)') NPARCP                                    INIMIX53
             CALL EXIT                                                  INIMIX54
            ENDIF                                                       INIMIX55
            IF(NPARCP.GT.0)THEN                                         INIMIX56
              DO 1 IS=1,NPARCP                                          INIMIX57
              JCODCP(IS)=IW(JSTA+1+IS)                                  INIMIX58
 1            CONTINUE                                                  INIMIX59
              ROMOD=RW(JSTA+1+NPARCP+1)                                 INIMIX60
              ROPHA=RW(JSTA+1+NPARCP+2)                                 INIMIX61
              RHOCPM=CMPLX(ROMOD*COS(ROPHA),ROMOD*SIN(ROPHA))           INIMIX62
              IMIX =1                                                   INIMIX63
            ENDIF                                                       INIMIX64
          ENDIF                                                         INIMIX65
C                                                                       INIMIX66
C  MIXING PARAMETERS WITHOUT CP VIOLATION                               INIMIX67
C                                                                       INIMIX68
            RD=(XD**2+YD**2)/(2.+XD**2-YD**2)                           INIMIX69
            RS=(XS**2+YS**2)/(2.+XS**2-YS**2)                           INIMIX70
            CHID=RD/(1.+RD)                                             INIMIX71
            CHIS=RS/(1.+RS)                                             INIMIX72
C                                                                       INIMIX73
C ONE COMPUTES THE MIXING PROBABILITIES OF B AND BBAR                   INIMIX74
C TAKING INTO ACCOUNT CP VIOLATION.                                     INIMIX75
C                                                                       INIMIX76
            PSQDM=CABS(PSQD)                                            INIMIX77
            PSQSM=CABS(PSQS)                                            INIMIX78
            QSPDM=1./PSQDM                                              INIMIX79
            QSPSM=1./PSQSM                                              INIMIX80
            CDCPB=(QSPDM**2*(XD**2+YD**2))/(2.+XD**2-YD**2+             INIMIX81
     *      QSPDM**2*(XD**2+YD**2))                                     INIMIX82
            CSCPB=(QSPSM**2*(XS**2+YS**2))/(2.+XS**2-YS**2+             INIMIX83
     *      QSPSM**2*(XS**2+YS**2))                                     INIMIX84
            CDCPBA=(PSQDM**2*(XD**2+YD**2))/(2.+XD**2-YD**2+            INIMIX85
     *      PSQDM**2*(XD**2+YD**2))                                     INIMIX86
            CSCPBA=(PSQSM**2*(XS**2+YS**2))/(2.+XS**2-YS**2+            INIMIX87
     *      PSQSM**2*(XS**2+YS**2))                                     INIMIX88
C                                                                       INIMIX89
C   Issue the relevant parameters                                       INIMIX90
C                                                                       INIMIX91
      IUT = IW(6)                                                       INIMIX92
      WRITE (IUT,1000)                                                  INIMIX93
      WRITE (IUT,1001) XD,YD,XS,YS                                      INIMIX94
      WRITE (IUT,1002) PMOD ,PHAD,PMOS ,PHAS                            INIMIX95
      WRITE (IUT,1003) CDCPB,CDCPBA,CSCPB,CSCPBA                        INIMIX96
      WRITE (IUT,1004) NPARCP                                           INIMIX97
      IF (NPARCP.GT.0) WRITE (IUT,1005) (JCODCP(K),K=1,NPARCP)          INIMIX98
      IF (NPARCP.GT.0) WRITE (IUT,1006) ROMOD,ROPHA                     INIMIX99
      WRITE (IUT,1007)                                                  INIMI100
 1000 FORMAT(30X,78('*'),/,/,40X,'WELCOME TO MODIFIED LUND7.3 ,ADAPTED',INIMI101
     $' TO HANDLE BBbar MIXING   ',/,                                   INIMI102
     $ 30X,'*',25X,'               Bd                      Bs',10X,'*') INIMI103
 1001 FORMAT (30X,'*  xq  ,yq  ',17X, 4F12.4,'*')                       INIMI104
 1002 FORMAT (30X,'*  CPV p/q , phase',11X,4F12.4, '*')                 INIMI105
 1003 FORMAT (30X,'* Resulting prob. Pq , Pqbar ',4F12.4,'*')           INIMI106
 1004 FORMAT (30X,'* Final state requested with ',I10,'  particles ',   INIMI107
     $  26X,'*')                                                        INIMI108
 1005 FORMAT (30X,'* LUND codes to be selected : ',4I10,4X,'*')         INIMI109
 1006 FORMAT (30X,'* Module and phase          : ',2F12.4,23X,'*')      INIMI110
 1007 FORMAT (30X,78('*') )                                             INIMI111
C                                                                       INIMI112
      CALL HBOOK1(10010,'DECAY PATH (tau unit) Bd->Bd$',50,0.,10.,0.)   INIMI113
      CALL HBOOK1(10011,'DECAY PATH (tau unit) Bd->BdBAR$',50,0.,10.,0.)INIMI114
      CALL HBOOK1(10012,'DECAY PATH (tau unit) Bs->Bs$',50,0.,10.,0.)   INIMI115
      CALL HBOOK1(10013,'DECAY PATH (tau unit) Bs->BsBAR$',50,0.,10.,0.)INIMI116
      CALL HBOOK1(10014,'TRUE Decaylength (CM) Bd->Bd$',50,0.,1.5,0.)   INIMI117
      CALL HBOOK1(10015,'TRUE Decaylength (CM) Bd->BdBAR$',50,0.,1.5,0.)INIMI118
      CALL HBOOK1(10016,'TRUE Decaylength (CM) Bs->Bs$',50,0.,1.5,0.)   INIMI119
      CALL HBOOK1(10017,'TRUE Decaylength (CM) Bs->BsBar$',50,0.,1.5,0.)INIMI120
C                                                                       INIMI121
C   dump the generator parameters for this run in a bank                INIMI122
C assume all parameters are real and stored as a single row             INIMI123
      TABL(1) = XD                                                      INIMI124
      TABL(2) = YD                                                      INIMI125
      TABL(3) = XS                                                      INIMI126
      TABL(4) = YS                                                      INIMI127
      TABL(5) = PMOD                                                    INIMI128
      TABL(6) = PHAD                                                    INIMI129
      TABL(7) = PMOS                                                    INIMI130
      TABL(8) = PHAS                                                    INIMI131
      TABL(9) = CDCPB                                                   INIMI132
      TABL(10)= CDCPBA                                                  INIMI133
      TABL(11)= CSCPB                                                   INIMI134
      TABL(12)= CSCPBA                                                  INIMI135
      TABL(13)= FLOAT(NPARCP)                                           INIMI136
      IF(NPARCP.GT.0) THEN                                              INIMI137
       DO 30 II=1,NPARCP                                                INIMI138
 30    TABL(13+II)= FLOAT(JCODCP(II))                                   INIMI139
       TABL(13+NPARCP+1)= ROMOD                                         INIMI140
       TABL(13+NPARCP+2)= ROPHA                                         INIMI141
       NWB = 15+NPARCP                                                  INIMI142
      ELSE                                                              INIMI143
        NWB = 13                                                        INIMI144
      ENDIF                                                             INIMI145
      IND = ALTABL('KMIX',NWB,1,TABL,'2I,(F)','C')                      INIMI146
C                                                                       INIMI147
      CALL PRTABL('KMIX',0)                                             INIMI148
      RETURN                                                            INIMI149
      END                                                               INIMI150
      SUBROUTINE INIPHO(IPHO)                                           INIPHO 2
C ------------------------------------------------------------------    INIPHO 3
C - B. Bloch  - JUNE 1992                                               INIPHO 4
C! Initialization routine for internal brem in B and D meson semi-lep   INIPHO 5
C  IPHO = 0  no internal brem                                           INIPHO 6
C       =x1  internal brems on D meson semi lep decay only              INIPHO 7
C       =x2  internal brems on B meson semi lep decay only              INIPHO 8
C       =x3  internal brems on both Meson types                         INIPHO 9
C       =1x  internal brems on D baryon semi lep decay only             INIPHO10
C       =2x  internal brems on B baryon semi lep decay only             INIPHO11
C       =3x  internal brems on both Baryon types                        INIPHO12
C ------------------------------------------------------------------    INIPHO13
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      IPHO =0                                                           INIPHO15
      JPHO=IW(NAMIND('GPHO'))                                           INIPHO16
      IF(JPHO.GT.0)THEN                                                 INIPHO17
         IPHO = IW(JPHO+1)                                              INIPHO18
         IMES = MOD ( IPHO,10)                                          INIPHO19
         IBAR = IPHO/10                                                 INIPHO20
         IF ( IPHO.GT.0)  CALL PHOINI                                   INIPHO21
         IF ( IPHO.GT.0 .AND. IMES.GT.0 ) THEN                          INIPHO22
            CALL HBOOK1(10050,'PHOTON Energy from Dmeso SEMI LEP DECAY',INIPHO23
     $      50 ,0.,15.,0.)                                              INIPHO24
            CALL HBOOK1(10052,'PHOTON Energy from Bmeso SEMI LEP DECAY',INIPHO25
     $      50 ,0.,20.,0.)                                              INIPHO26
         ENDIF                                                          INIPHO27
         IF ( IPHO.GT.0 .AND. IBAR.GT.0 ) THEN                          INIPHO28
            CALL HBOOK1(10051,'PHOTON Energy from Dbary SEMI LEP DECAY',INIPHO29
     $      50 ,0.,15.,0.)                                              INIPHO30
            CALL HBOOK1(10053,'PHOTON Energy from Bbary SEMI LEP DECAY',INIPHO31
     $      50 ,0.,20.,0.)                                              INIPHO32
         ENDIF                                                          INIPHO33
      ENDIF                                                             INIPHO34
      RETURN                                                            INIPHO35
      END                                                               INIPHO36
      SUBROUTINE INIP17(IP17)                                           INIP17 2
C ------------------------------------------------------------------    INIP17 3
C - B. Bloch  - NOVEMBER 1993                                           INIP17 4
C! initialization routine for flavour dependent PARJ(17)                INIP17 5
C ------------------------------------------------------------------    INIP17 6
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
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
C                                                                       LUN7CO12
      COMMON/PARJ17/P17(5)                                              PARJ17 2
C      fraction of T/V production for flavor 1...5  from GP17 card      PARJ17 3
      DIMENSION TABL(10)                                                INIP1710
      INTEGER ALTABL                                                    INIP1711
C                                                                       INIP1712
C GET flavour dependant fraction of tensor mesons among vector mesons   INIP1713
          IP17 = 0                                                      INIP1714
          JP17=IW(NAMIND('GP17'))                                       INIP1715
          IF(JP17.GT.0)THEN                                             INIP1716
            P17(2) = RW(JP17+1)                                         INIP1717
            P17(3) = RW(JP17+2)                                         INIP1718
            P17(4) = RW(JP17+3)                                         INIP1719
            P17(5) = RW(JP17+4)                                         INIP1720
            P17(1) = P17(2)                                             INIP1721
            IP17 = 1                                                    INIP1722
          ELSE                                                          INIP1723
            P17(1) = PARJ(17)                                           INIP1724
            P17(2) = PARJ(17)                                           INIP1725
            P17(3) = PARJ(17)                                           INIP1726
            P17(4) = PARJ(17)                                           INIP1727
            P17(5) = PARJ(17)                                           INIP1728
          ENDIF                                                         INIP1729
C                                                                       INIP1730
C   Issue the relevant parameters                                       INIP1731
C                                                                       INIP1732
      IUT = IW(6)                                                       INIP1733
      WRITE (IUT,1000)                                                  INIP1734
      WRITE (IUT,1001) (P17(I),I=2,5)                                   INIP1735
 1000 FORMAT(30X,78('*'),/,/,40X,' You will use the following fraction 'INIP1736
     $,'Tensor/vector  ')                                               INIP1737
 1001 FORMAT (30X,'*       ud         s         c         b    ',/,     INIP1738
     $        30X,4F10.4,'*')                                           INIP1739
C                                                                       INIP1740
C   dump the generator parameters for this run in a bank                INIP1741
C        all parameters are real and stored as a single row             INIP1742
      TABL(1) = P17(1)                                                  INIP1743
      TABL(2) = P17(2)                                                  INIP1744
      TABL(3) = P17(3)                                                  INIP1745
      TABL(4) = P17(4)                                                  INIP1746
      TABL(5) = P17(5)                                                  INIP1747
      NWB = 5                                                           INIP1748
      IND = ALTABL('KP17',NWB,1,TABL,'2I,(F)','C')                      INIP1749
C                                                                       INIP1750
      CALL PRTABL('KP17',0)                                             INIP1751
      RETURN                                                            INIP1752
      END                                                               INIP1753
      SUBROUTINE INISEM(ISEM)                                           INISEM 2
C---------------------------------------------------------------------- INISEM 3
C     A. Falvard , B.Bloch-  October 1990                               INISEM 4
C                                                                       INISEM 5
C! Initialisation for all B  SemiLeptonic decays                        INISEM 6
C  Creates KSEM bank with relevant input data                           INISEM 7
C  ISEM : MODEL used for semileptonic decays                            INISEM 8
C         0= no model required,>0 model # required                      INISEM 9
C---------------------------------------------------------------------- INISEM10
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
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
C                                                                       LUN7CO12
      PARAMETER (NMODEL=5,NDECAY=11)                                    AFL001 1
      COMMON/SEMLEP/IMOSEM,WTM(NMODEL,NDECAY),IHEL,KHEL,PRM1,PR0,PRP1,  SEMLEP 3
     $          BBDAT(NDECAY),BXDAT(NDECAY)  ,                          SEMLEP 4
     $          NO1(NDECAY),NO2(NDECAY),OVER(NMODEL,NDECAY)             SEMLEP 5
     $          , IMODSS,R1,R2,R3                                       AFL001 2
      REAL MFF,MFF2,MFF3,MFF4                                           SEMLEP 6
      COMMON/MFFX/MFF(NMODEL,NDECAY),MFF2(NMODEL,NDECAY),               SEMLEP 7
     $            MFF3(NMODEL,NDECAY),MFF4(NMODEL,NDECAY)               SEMLEP 8
      REAL MB,MD,MQ,MBOT,MX,MUP,MUM,MS,MSQ,MRSQ,KSQ                     SEMLEP 9
      COMMON /SCORA/MB,MD,MQ,MBOT,MX,MUP,MUM,MS,MSQ,MRSQ,KSQ            SEMLEP10
     *,BM,BMSQ,XMTSQ,BB,BX,XMT,BBX,BR,BRX                               SEMLEP11
      COMMON /SCOROU/AL,CP,Q,QV                                         SEMLEP12
      DIMENSION TABL(5)                                                 INISEM14
      INTEGER ALTABL                                                    INISEM15
      DIMENSION BDAT(NDECAY),XDAT(NDECAY)                               INISEM16
      DIMENSION NNO1(NDECAY),NNO2(NDECAY),OOVER(NMODEL,NDECAY)          INISEM17
      DIMENSION XMFF(NMODEL,NDECAY),XMFF2(NMODEL,NDECAY),               INISEM18
     $         XMFF3(NMODEL,NDECAY),XMFF4(NMODEL,NDECAY)                INISEM19
      PARAMETER (LBPOS=100,NBU=521,NBD=511,NBS=531,NBC=541)             BCODES 2
      PARAMETER (NLB=5122,NXB= 5132)                                    BBL00511
C     LBPOS  = internal JETSET 7.3 code for B meson                     BCODES 3
      PARAMETER (NXB0=5232,NOB= 5332)                                   BBL007 9
C     NBU    = internal JETSET 7.3 code for Bu meson                    BCODES 4
C     NBD    = internal JETSET 7.3 code for Bd meson                    BCODES 5
C     NBS    = internal JETSET 7.3 code for Bs meson                    BCODES 6
C     NBC    = internal JETSET 7.3 code for Bc meson                    BCODES 7
C     NLB    = internal JETSET 7.3 code for /\B BARYON                  BBL00512
C     NXB    = internal JETSET 7.3 code for XIB BARYON                  BBL00513
C     NXB0   = internal JETSET 7.3 code for XIB0 BARYON                 BBL00710
C     NOB    = internal JETSET 7.3 code for OMEGAB BARYON               BBL00711
      DATA NNO1/6*NBU,2*421,3*NBU/                                      AFL00149
      DATA NNO2/423,421,213,211,323,321,323,321,3*425/                  AFL00150
      DATA BDAT/6*0.41,2*0.39,3*0.41/                                   AFL00151
      DATA XDAT/2*0.39,2*0.31,4*0.34,3*0.39/                            AFL00152
C                                                                       AFL00153
C     IMODEL=1  KORNER-SCHULER         IDECAY=1    B--> D*              AFL00154
C            2  GRINSTEIN ET AL.              2    B--> D               AFL00155
C            3  PIETSCHMANN ET AL.            3    B--> RHO             AFL00156
C            4  BAUER, STECH, WIRBEL          4    B--> PI              AFL00157
C            5  ISGUR, SCUORA, GRINSTEIN      5    B--> K*              AFL00158
C                                             6    B--> K               AFL00159
C                                             7    D--> K*              AFL00160
C                                             8    D--> K               AFL00161
C                                             9    B-->D** (PSEU. V 1)  AFL00162
C                                            10    B-->D** (SCALAIRE)   AFL00163
C                                            11    B-->D** (PSEU. V 2)  AFL00164
C                                                                       AFL00165
C OVER                                                                  AFL00166
      DATA (OOVER(1,I),I=1,NDECAY)/2*0.7,4*0.33,5*0.82/                 AFL00167
      DATA (OOVER(2,I),I=1,NDECAY)/2*0.7,4*0.33,5*0.82/                 AFL00168
      DATA (OOVER(3,I),I=1,NDECAY)/2*0.7,4*0.33,5*0.82/                 AFL00169
      DATA (OOVER(4,I),I=1,NDECAY)/2*0.7,4*0.33,5*0.82/                 AFL00170
      DATA (OOVER(5,I),I=1,NDECAY)/11*0./                               AFL00171
C MFF                                                                   AFL00172
      DATA (XMFF(1,I),I=1,NDECAY)/2*6.34,4*5.33,5*2.11/                 AFL00173
      DATA (XMFF(2,I),I=1,NDECAY)/2*6.34,4*5.33,5*2.11/                 AFL00174
      DATA (XMFF(3,I),I=1,NDECAY)/2*6.34,4*5.33,5*2.11/                 AFL00175
      DATA (XMFF(4,I),I=1,NDECAY)/2*6.34,4*5.33,5*2.11/                 AFL00176
      DATA (XMFF(5,I),I=1,NDECAY)/11*0./                                AFL00177
C MFF2                                                                  AFL00178
      DATA (XMFF2(1,I),I=1,NDECAY)/2*6.8,4*5.8,5*2.6/                   AFL00179
      DATA (XMFF2(2,I),I=1,NDECAY)/2*6.8,4*5.8,5*2.6/                   AFL00180
      DATA (XMFF2(3,I),I=1,NDECAY)/2*6.8,4*5.8,5*2.6/                   AFL00181
      DATA (XMFF2(4,I),I=1,NDECAY)/2*6.8,4*5.8,5*2.6/                   AFL00182
      DATA (XMFF2(5,I),I=1,NDECAY)/11*0./                               AFL00183
C MFF3                                                                  AFL00184
      DATA (XMFF3(1,I),I=1,NDECAY)/2*6.34,4*5.33,5*2.71/                AFL00185
      DATA (XMFF3(2,I),I=1,NDECAY)/2*6.34,4*5.33,5*2.71/                AFL00186
      DATA (XMFF3(3,I),I=1,NDECAY)/2*6.34,4*5.33,5*2.71/                AFL00187
      DATA (XMFF3(4,I),I=1,NDECAY)/2*6.34,4*5.33,5*2.71/                AFL00188
      DATA (XMFF3(5,I),I=1,NDECAY)/11*0./                               AFL00189
C MFF4                                                                  AFL00190
      DATA (XMFF4(1,I),I=1,NDECAY)/2*6.73,4*5.5,5*2.53/                 AFL00191
      DATA (XMFF4(2,I),I=1,NDECAY)/2*6.73,4*5.5,5*2.53/                 AFL00192
      DATA (XMFF4(3,I),I=1,NDECAY)/2*6.73,4*5.5,5*2.53/                 AFL00193
      DATA (XMFF4(4,I),I=1,NDECAY)/2*6.73,4*5.5,5*2.53/                 AFL00194
      DATA (XMFF4(5,I),I=1,NDECAY)/11*0./                               AFL00195
C-------------------------------------------------------------          INISEM65
C   Init common variables                                               INISEM66
C                                                                       INISEM67
      DO 10 II = 1,NDECAY                                               INISEM68
         NO1(II) = NNO1(II)                                             INISEM69
         NO2(II) = NNO2(II)                                             INISEM70
         BBDAT(II) = BDAT(II)                                           INISEM71
         BXDAT(II) = XDAT(II)                                           INISEM72
         DO 20 IJ = 1,NMODEL                                            INISEM73
           OVER(IJ,II) = OOVER(IJ,II)                                   INISEM74
           MFF (IJ,II) = XMFF (IJ,II)                                   INISEM75
           MFF2(IJ,II) = XMFF2(IJ,II)                                   INISEM76
           MFF3(IJ,II) = XMFF3(IJ,II)                                   INISEM77
           MFF4(IJ,II) = XMFF4(IJ,II)                                   INISEM78
  20     CONTINUE                                                       INISEM79
  10  CONTINUE                                                          INISEM80
C---------------------------------------------------------              INISEM81
      ISEM=0                                                            INISEM82
      KHEL=0                                                            INISEM83
      IHEL=0                                                            INISEM84
      CALL VZERO(WTM,NMODEL*NDECAY)                                     INISEM85
      JSEM=IW(NAMIND('GSEM'))                                           INISEM86
      IF(JSEM.NE.0)THEN                                                 INISEM87
         IMOSEM=IW(JSEM+1)                                              INISEM88
         IF (IMOSEM.LE.0) GO TO 999                                     INISEM89
         JDSS=IW(NAMIND('GDSS'))                                        AFL00196
         IMODSS=0                                                       AFL00197
         IF(JDSS.NE.0)THEN                                              AFL00198
           IMODSS=IW(JDSS+1)                                            AFL00199
           IF(IMODSS.EQ.4)THEN                                          AFL00100
             NDADSS=IW(JDSS)                                            AFL00101
             IF(NDADSS.NE.4)THEN                                        AFL00102
              WRITE(IW(6),'(''ERROR D** - YOU  HAVE A WRONG # OF '',    AFL00103
     *   ''  PARAMETERS IN GDSS'',I3)') NDADSS                          AFL00104
              CALL EXIT                                                 AFL00105
             ENDIF                                                      AFL00106
             R1=RW(JDSS+2)                                              AFL00107
             R2=RW(JDSS+3)                                              AFL00108
             R3=RW(JDSS+4)                                              AFL00109
           ENDIF                                                        AFL00110
         ENDIF                                                          AFL00111
         MSTSO=MSTJ(24)                                                 INISEM90
         MSTJ(24)=0                                                     INISEM91
         IHEL=IW(JSEM+2)                                                INISEM92
         ISEM=IMOSEM                                                    INISEM93
         CALL HBOOK1(10020,'LEPTON ENERGY SPECTRUM IN CMS',50,0.,5.,0.) INISEM94
         CALL HBOOK1(10021,'LEPTON ENERGY BOOSTED IN LAB',50,0.,35.,0.) INISEM95
         DO 2 J=1,NDECAY                                                INISEM96
         AM1=ULMASS(NO1(J))                                             INISEM97
         AM2=ULMASS(NO2(J))                                             INISEM98
         IF(J.GE.9.AND.IMODSS.EQ.0)GOTO 2                               AFL00112
         IMOSIM=IMOSEM                                                  AFL00113
         IF(J.GE.9)IMOSEM=5                                             AFL00114
         IF(IMOSEM.EQ.5)THEN                                            INISEM99
           CALL LUIFLV (NO1(J),IFLA,IFLB,IFLC,KSP)                      INISE100
           CALL LUIFLV (NO2(J),IFLA1,IFLB1,IFLC1,KSP1)                  INISE101
           MB=ULMASS(IFLA)                                              INISE102
           MD=ULMASS(IFLB)                                              INISE103
           MQ=ULMASS(IFLA1)                                             INISE104
           IF(IFLA.EQ.4)MB=1.82                                         AFL00115
           IF(IFLA.EQ.5)MB=5.12                                         AFL00116
           IF(IFLB.EQ.1.OR.IFLB.EQ.2)MD=0.33                            AFL00117
           IF(IFLB.EQ.3)MD=0.55                                         AFL00118
           IF(IFLB.EQ.4)MD=1.82                                         AFL00119
           IF(IFLA1.EQ.3)MQ=0.55                                        AFL00120
           IF(IFLA1.EQ.4)MQ=1.82                                        AFL00121
           BM=AM1                                                       INISE105
           BMSQ=BM**2                                                   INISE106
           XMT=AM2                                                      INISE107
           XMTSQ=XMT**2                                                 INISE108
           BB=BBDAT(J)                                                  INISE109
           BX=BXDAT(J)                                                  INISE110
           MBOT=MB+MD                                                   INISE111
           MX=MD+MQ                                                     INISE112
           MUP=(MB*MQ)/(MB+MQ)                                          INISE113
           MUM=(MB*MQ)/(MB-MQ)                                          INISE114
           MS=MBOT-MX                                                   INISE115
           MSQ=(MD**2)/(MX*MBOT)                                        INISE116
           MRSQ=BMSQ/XMTSQ                                              INISE117
           BBX=(BB**2+BX**2)/2.                                         INISE118
           BR=BB**2/BBX                                                 INISE119
           BRX=BX**2/BBX                                                INISE120
           KSQ=0.7**2                                                   INISE121
         ENDIF                                                          INISE122
C                                                                       INISE123
C  CHECK OF F , ETC....                                                 INISE124
C                                                                       INISE125
C  WHAT DO WE WANT FOR RHO AND K*?                                      INISE126
C                                                                       INISE127
         WTM(IMOSEM,J)=HIMAX(IMOSEM,J,AM1,AM2)*1.3                      INISE128
         IF(J.GE.9.AND.IMODSS.NE.0)IMOSEM=IMOSIM                        AFL00122
 2       CONTINUE                                                       INISE129
         MSTJ(24)=MSTSO                                                 INISE130
         NDASEM=IW(JSEM)                                                INISE131
         IF(NDASEM.GT.2.AND.NDASEM.LT.5)THEN                            INISE132
           WRITE(IW(6),100) NDASEM                                      INISE133
 100       FORMAT(1X,'GSEM DATA CARD : WARNING - YOU SHOULD PROVIDE     INISE134
     *   2 OR 5 DATA : NOT',I4)                                         INISE135
         ELSEIF (NDASEM.EQ.5) THEN                                      INISE136
           PRM11=RW(JSEM+3)                                             INISE137
           PR01=RW(JSEM+4)                                              INISE138
           PRP11=RW(JSEM+5)                                             INISE139
           PRM1=PRM11                                                   INISE140
           PR0=PRM1+PR01                                                INISE141
           PRP1=PR0+PRP11                                               INISE142
           IF(PRP1.GT.1.)WRITE(IW(6),'(1X,''***WARNING**** PROBABILITIESINISE143
     $ IN GSEM CARD DO NOT ADD TO 1. ****CHANGE IT!'' )')               INISE144
           KHEL=1                                                       INISE145
         ENDIF                                                          INISE146
C                                                                       INISE147
C  Create KSEM bank                                                     INISE148
C                                                                       INISE149
          TABL(1) = FLOAT(ISEM)                                         INISE150
          TABL(2) = FLOAT(IHEL)                                         INISE151
          IF (NDASEM.EQ.5) THEN                                         INISE152
            TABL(3) = PRM11                                             INISE153
            TABL(4) = PR01                                              INISE154
            TABL(5) = PRP11                                             INISE155
          ENDIF                                                         INISE156
          NWB = NDASEM                                                  INISE157
          IND = ALTABL('KSEM',NWB,1,TABL,'2I,(F)','C')                  INISE158
          CALL PRTABL('KSEM',0)                                         INISE159
          IF ( IMODSS.NE.0) THEN                                        BBL00411
             TABL(1) = FLOAT(IMODSS)                                    BBL00412
             NWB = 1                                                    BBL00413
             IF ( IMODSS.EQ.4) THEN                                     BBL00414
                NWB = 4                                                 BBL00415
                TABL(2) = R1                                            BBL00416
                TABL(3) = R2                                            BBL00417
                TABL(4) = R3                                            BBL00418
             ENDIF                                                      BBL00419
             IND = ALTABL('KDSS',NWB,1,TABL,'2I,(F)','C')               BBL00420
             CALL PRTABL('KDSS',0)                                      BBL00421
          ENDIF                                                         BBL00422
      ENDIF                                                             INISE160
999   RETURN                                                            INISE161
      END                                                               INISE162
      SUBROUTINE INISIN(IFLV,ECMS)                                      INISIN 2
C ------------------------------------------------------------------    INISIN 3
C - B. Bloch  - October 1990                                            INISIN 4
C! Init single particle generation  jetset 7.3                          INISIN 5
C                                                                       INISIN 6
C ------------------------------------------------------------------    INISIN 7
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      COMMON/SINGEN/ITYPSI,PMINSI,PMAXSI,COMISI,COMASI,PHMISI,PHMASI    INISIN 9
      JSIN=IW(NAMIND('GSIN'))                                           INISIN10
      IF(JSIN.NE.0)THEN                                                 INISIN11
         ITYPSI=IW(JSIN+1)                                              INISIN12
         PMINSI=RW(JSIN+2)                                              INISIN13
         PMAXSI=RW(JSIN+3)                                              INISIN14
         COMISI=RW(JSIN+4)                                              INISIN15
         COMASI=RW(JSIN+5)                                              INISIN16
         PHMISI=RW(JSIN+6)                                              INISIN17
         PHMASI=RW(JSIN+7)                                              INISIN18
      ELSE                                                              INISIN19
         WRITE(IW(6),100)                                               INISIN20
         CALL EXIT                                                      INISIN21
      ENDIF                                                             INISIN22
 100  FORMAT (1X,'YOU DID NOT GIVE ANY INPUT CARD GSIN !!!!! STOP-')    INISIN23
      IFLV = ITYPSI                                                     INISIN24
      ECMS = 2.*ULMASS(ITYPSI)                                          INISIN25
      RETURN                                                            INISIN26
      END                                                               INISIN27
      SUBROUTINE INIVBU(IVBU)                                           INIVBU 2
C ------------------------------------------------------------------    INIVBU 3
C - B. Bloch  - OCTOBER  1990 - JETSET 7.3 version                      INIVBU 4
C                                                                       INIVBU 5
C                                                            ENTRY pointINIVBU 6
C! Initialization routine of Vbu transitions                 INIVBU     INIVBU 7
C  Implement b-->u transition according to requested model   VBCVBU     INIVBU 8
C  Restore   b-->c transition as defined primarily           VBUVBC     INIVBU 9
C                                                                       INIVBU10
C ------------------------------------------------------------------    INIVBU11
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
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
C                                                                       LUN7CO12
      PARAMETER (LBPOS=100,NBU=521,NBD=511,NBS=531,NBC=541)             BCODES 2
      PARAMETER (NLB=5122,NXB= 5132)                                    BBL00511
C     LBPOS  = internal JETSET 7.3 code for B meson                     BCODES 3
      PARAMETER (NXB0=5232,NOB= 5332)                                   BBL007 9
C     NBU    = internal JETSET 7.3 code for Bu meson                    BCODES 4
C     NBD    = internal JETSET 7.3 code for Bd meson                    BCODES 5
C     NBS    = internal JETSET 7.3 code for Bs meson                    BCODES 6
C     NBC    = internal JETSET 7.3 code for Bc meson                    BCODES 7
C     NLB    = internal JETSET 7.3 code for /\B BARYON                  BBL00512
C     NXB    = internal JETSET 7.3 code for XIB BARYON                  BBL00513
C     NXB0   = internal JETSET 7.3 code for XIB0 BARYON                 BBL00710
C     NOB    = internal JETSET 7.3 code for OMEGAB BARYON               BBL00711
      COMMON/VBUCOM/ IMATBU,PRBUBC,PARDBU,ITYPBU                        VBUCOM 3
      PARAMETER (NLEPTO=3,LMATHA=42)                                    INIVBU15
      PARAMETER(LTHRES=32,LBMAX=5,LBROW=4,NTYPBU=4)                     INIVBU16
      DIMENSION CBRBU(LBMAX,NTYPBU)                                     INIVBU17
C     NLEPTO  = number of semi-leptonic modes                           INIVBU18
C     LMATHA  = matrix element type for Hadronic modes                  INIVBU19
C     LTHRESH = index of PARJ location corresponding to threshold       INIVBU20
C     LBPOS   = B meson flavor location                                 INIVBU21
C     LBMAX   = # of transitions for b quark                            INIVBU22
C     LBROW   = max number of particles in a final state                INIVBU23
C     NTYPBU  = # of Vbu transition types implemented                   INIVBU24
      DIMENSION KDPSTO(LBMAX,LBROW),CBRSTO(LBMAX),MDMSTO(LBMAX,2)       INIVBU25
      DIMENSION ICHBU1(LBMAX,LBROW),IMATB1(LBMAX)                       INIVBU26
      DIMENSION ICHABU(LBMAX,LBROW),LBMOD(NTYPBU),TABL(50)              INIVBU27
      INTEGER ALTABL                                                    INIVBU28
      PARAMETER (NNUEL=12,NNUMU=14,NNUTO=16,NEL=11,NMU=13,NTO=15)       LCODES 2
C     NNUEL  = internal JETSET 7.3 code for neutrino electron           LCODES 3
C     NNUMU  = internal JETSET 7.3 code for neutrino mu                 LCODES 4
C     NNUTO  = internal JETSET 7.3 code for neutrino tau                LCODES 5
C     NEL    = internal JETSET 7.3 code for electron                    LCODES 6
C     NMU    = internal JETSET 7.3 code for muon                        LCODES 7
C     NTO    = internal JETSET 7.3 code for tau                         LCODES 8
      DATA ICHABU/-12,-14,-16,-2,-4,11,13,15,1,3,5*2,5*81/              INIVBU30
      DATA ICHBU1/-12,4*0,11,4*0,2,4*0,81,4*0/ IMATB1/42,4*0/           INIVBU31
      DATA LBMOD/5,5,5,0/                                               INIVBU32
C ----BR's are now non cumulative!!                                     INIVBU33
      DATA (CBRBU(IU,1),IU=1,5)/0.143,0.143,0.071,0.429,0.214/          INIVBU34
      DATA (CBRBU(IU,2),IU=1,5)/0.134,0.134,0.052,0.49,0.19/            INIVBU35
      DATA (CBRBU(IU,3),IU=1,5)/0.117,0.117,0.043,0.465,0.258/          INIVBU36
      DATA (CBRBU(IU,4),IU=1,5)/5*0./                                   INIVBU37
C                                                                       INIVBU38
C  Get B-->U transitions parameters                                     INIVBU39
C   IMATBU: LUND Matrix Element (42 or 44)                              INIVBU40
C       42 the u and spectator collapse --> meson                       INIVBU41
C       44 other mesonic systems can be produced if the available       INIVBU42
C          invariant mass is large enough                               INIVBU43
C                                                                       INIVBU44
C   PARDBU: PARJ(32) can be modified to produce more realistic features INIVBU45
C           when IMATBU=44                                              INIVBU46
C                                                                       INIVBU47
C   ITYPBU: = 1 Free Quark Model                                        INIVBU48
C             2  NLL QCD CORRECTIONS WITH CONSTITUENT MASS FOR QUARKS   INIVBU49
C             3     "     "     "     "   CURRENT       "       "       INIVBU50
C             4  user defined transition                                INIVBU51
C            other models may be provided later                         INIVBU52
      IVBU =0                                                           INIVBU53
      JVBU=IW(NAMIND('GVBU'))                                           INIVBU54
      IF(JVBU.NE.0)THEN                                                 INIVBU55
         IMATBU=IW(JVBU+1)                                              INIVBU56
         PRBUBC=RW(JVBU+2)                                              INIVBU57
         PARDBU=RW(JVBU+3)                                              INIVBU58
         ITYPBU=IW(JVBU+4)                                              INIVBU59
         IF (PRBUBC.GT.0.) IVBU = ITYPBU                                INIVBU60
      ELSE                                                              INIVBU61
         IMATBU=42                                                      INIVBU62
         PRBUBC=0.                                                      INIVBU63
         PARDBU=1.0                                                     INIVBU64
         ITYPBU=1                                                       INIVBU65
      ENDIF                                                             INIVBU66
      CALL HTABLE(10030,'b->u transition codes/event VS mixing code',   INIVBU67
     $   6,0., 6., 8,0., 8.,0.)                                         INIVBU68
      CALL HBPRO(0,0.)                                                  INIVBU69
      IF ( IVBU.GT.0) THEN                                              INIVBU70
C                                                                       INIVBU71
C   This modifies the LUND decay table of the B mesons.                 INIVBU72
C   Free Quark Model expectation is introduced as a first approximation INIVBU73
C                                                                       INIVBU74
        CALL VZERO(CBRSTO,LBMAX)                                        INIVBU75
        CALL VZERO(KDPSTO,LBMAX*LBROW)                                  INIVBU76
        CALL VZERO(MDMSTO,LBMAX*2)                                      INIVBU77
C                                                                       INIVBU78
C  The original B Decays (Decay Mode and Branching Ratios) are stored   INIVBU79
C                                                                       INIVBU80
        IF(ITYPBU.LT.1.OR.ITYPBU.GT.NTYPBU)THEN                         INIVBU81
          WRITE(IW(6),102)                                              INIVBU82
 102      FORMAT(1X,'THE TYPE FOR B-->U TRANSITION IS UNCORRECT')       INIVBU83
          CALL EXIT                                                     INIVBU84
        ENDIF                                                           INIVBU85
        JBUS = 0                                                        INIVBU86
        IF(ITYPBU.EQ.4)THEN                                             INIVBU87
C                                                                       INIVBU88
C  B-->U  TRANSITION INTRODUCED BY USERS                                INIVBU89
C  (NOT MORE THAN 4 DECAY CHAINS)                                       INIVBU90
C                                                                       INIVBU91
          JBUS=IW(NAMIND('GBUS'))                                       INIVBU92
          IF(JBUS.NE.0)THEN                                             INIVBU93
            NCOM=IW(JBUS)/6                                             INIVBU94
            IF(NCOM.EQ.0)GOTO 30                                        INIVBU95
            DO 17 IBU=1,NCOM                                            INIVBU96
            CBRBU(IBU,ITYPBU)=RW(JBUS+(IBU-1)*6+6)                      INIVBU97
            DO 18 JBU=1,LBROW                                           INIVBU98
            IF(IBU.EQ.1)THEN                                            INIVBU99
              IF(IW(JBUS+(IBU-1)*6+1+JBU).NE.ICHBU1(IBU,JBU) .AND.(     INIVB100
     $            IW(JBUS+(IBU-1)*6+1).NE.IMATB1(IBU)))THEN             INIVB101
              WRITE(IW(6),103) IMATB1,(ICHBU1(1,JJBU),JJBU=1,LBROW)     INIVB102
 103          FORMAT(1X,'INIVBU : WARNING -  YOUR FIRST DECAY CHANNEL   INIVB103
     *        FOR B-->U TRANSITION IS UNCORRECT: IT SHOULD BE:',5I10)   INIVB104
              GOTO 17                                                   INIVB105
              ENDIF                                                     INIVB106
            ENDIF                                                       INIVB107
            ICHBU1(IBU,JBU)=IW(JBUS+(IBU-1)*6+1+JBU)                    INIVB108
 18         CONTINUE                                                    INIVB109
            IMATB1(IBU)=IW(JBUS+(IBU-1)*6+1)                            INIVB110
 17         CONTINUE                                                    INIVB111
            LBMOD(ITYPBU)=LBMOD(ITYPBU)+NCOM                            INIVB112
 30         CONTINUE                                                    INIVB113
          ENDIF                                                         INIVB114
        ENDIF                                                           INIVB115
        LBMIN=LBMOD(ITYPBU)                                             INIVB116
        KC = LUCOMP(LBPOS)                                              INIVB117
        IENTRY = MDCY(KC,2)                                             INIVB118
        NMBR = MIN(MDCY(KC,3),LBMIN)                                    INIVB119
        IF ( NMBR.GT.0) THEN                                            INIVB120
          DO 11 NBR = 1,NMBR                                            INIVB121
            IF(NBR.GT.LBMIN)GOTO 12                                     INIVB122
            CBRSTO(NBR)=BRAT(IENTRY)                                    INIVB123
            MDMSTO(NBR,2)=MDME(IENTRY,2)                                INIVB124
            MDMSTO(NBR,1)=MDME(IENTRY,1)                                INIVB125
            DO 13 I=1,LBROW                                             INIVB126
            KDPSTO(NBR,I)=KFDP(IENTRY,I)                                INIVB127
 13         CONTINUE                                                    INIVB128
            IENTRY=IENTRY+1                                             INIVB129
 11       CONTINUE                                                      INIVB130
        ENDIF                                                           INIVB131
 12     CONTINUE                                                        INIVB132
        NBR=NMBR                                                        INIVB133
        PARSTO=PARJ(LTHRES)                                             INIVB134
        IF(NBR.LT.LBMIN)THEN                                            INIVB135
         WRITE(IW(6),100)                                               INIVB136
 100     FORMAT(1X,'YOU DO NOT HAVE ENOUGH SPACE TO IMPLEMENT VBU IN /LUINIVB137
     $DAT3/  ')                                                         INIVB138
C                                                                       INIVB139
C  PREVOIR DE CREER (LBMIN-NBR)LIGNES                                   INIVB140
C                                                                       INIVB141
         CALL EXIT                                                      INIVB142
        ENDIF                                                           INIVB143
C   Fill bank KVBU                                                      INIVB144
        TABL(1) = FLOAT(IVBU)                                           INIVB145
        TABL(2) = FLOAT(ITYPBU)                                         INIVB146
        TABL(3) = PRBUBC                                                INIVB147
        TABL(4) = FLOAT(IMATBU)                                         INIVB148
        TABL(5) = PARDBU                                                INIVB149
        NWB = 5                                                         INIVB150
        IF (JBUS.NE.0) THEN                                             INIVB151
          DO 56 ICO = 1,NCOM                                            INIVB152
          DO 55 II= 1,5                                                 INIVB153
 55       TABL(5+II+6*(ICO-1)) = FLOAT(IW(JBUS+II+6*(ICO-1)))           INIVB154
          TABL(5+6+6*(ICO-1)) = RW(JBUS+6+6*(ICO-1))                    INIVB155
 56       CONTINUE                                                      INIVB156
          NWB = NWB +IW(JBUS)                                           INIVB157
        ENDIF                                                           INIVB158
        IND = ALTABL('KVBU',NWB,1,TABL,'2I,(F)','C')                    INIVB159
        CALL PRTABL('KVBU',0)                                           INIVB160
      ENDIF                                                             INIVB161
      RETURN                                                            INIVB162
C                                                                       INIVB163
      ENTRY VBCVBU                                                      INIVB164
C  IMPLEMENT B-->U TRANSITIONS                                          INIVB165
      PARJ(LTHRES)=PARDBU                                               INIVB166
      IENTRY=MDCY(LUCOMP(LBPOS),2)                                      INIVB167
C                                                                       INIVB168
C  Free Quark Model expectation for Branching ratios                    INIVB169
C                                                                       INIVB170
      IF(ITYPBU.LE.3)THEN                                               INIVB171
C                                                                       INIVB172
C  New Decay Modes are built                                            INIVB173
C                                                                       INIVB174
        DO 4 I=1,LBMIN                                                  INIVB175
        IMATT=IMATBU                                                    INIVB176
        IF(I.GT.NLEPTO)IMATT=LMATHA                                     INIVB177
        MDME(IENTRY,2) =IMATT                                           INIVB178
        MDME(IENTRY,1) =1                                               INIVB179
        BRAT(IENTRY)=CBRBU(I,ITYPBU)                                    INIVB180
        DO 5 J=1,LBROW                                                  INIVB181
         KFDP(IENTRY,J)=ICHABU(I,J)                                     INIVB182
  5     CONTINUE                                                        INIVB183
        IENTRY=IENTRY+1                                                 INIVB184
  4     CONTINUE                                                        INIVB185
        ELSEIF (ITYPBU.EQ.4)THEN                                        INIVB186
          DO 19 IEN=1,NCOM                                              INIVB187
            BRAT(IENTRY-1+IEN)=CBRBU(IEN,ITYPBU)                        INIVB188
            MDME(IENTRY-1+IEN,2) = IMATB1(IEN)                          INIVB189
            MDME(IENTRY-1+IEN,1) =1                                     INIVB190
            DO 20 JEN=1,LBROW                                           INIVB191
              KFDP(IENTRY-1+IEN,JEN)=ICHBU1(IEN,JEN)                    INIVB192
 20         CONTINUE                                                    INIVB193
 19       CONTINUE                                                      INIVB194
      ELSE                                                              INIVB195
        WRITE(IW(6),101)                                                INIVB196
 101   FORMAT(1X,'YOU DID NOT IMPLEMENT CORRECTLY B-->U TRANSITION ')   INIVB197
      ENDIF                                                             INIVB198
      NTOT = MDCY(KC,3)-LBMIN                                           INIVB199
      IF ( NTOT .GT. 0) THEN                                            INIVB200
        DO 3 IJ = 1,NTOT                                                INIVB201
          MDME(MDCY(KC,2)+LBMIN+IJ-1,1) =0                              INIVB202
  3     CONTINUE                                                        INIVB203
      ENDIF                                                             INIVB204
      RETURN                                                            INIVB205
      ENTRY VBUVBC                                                      INIVB206
C---------------------------------------------------------------------  INIVB207
C   Routine to restore b->c transition in B decays                      INIVB208
C---------------------------------------------------------------------  INIVB209
      IENTRY = MDCY(LUCOMP(LBPOS),2)                                    INIVB210
      DO 1 I=1,NBR                                                      INIVB211
      BRAT(IENTRY)=CBRSTO(I)                                            INIVB212
      MDME(IENTRY,1)=MDMSTO(I,1)                                        INIVB213
      MDME(IENTRY,2)=MDMSTO(I,2)                                        INIVB214
        DO 2 J=1,LBROW                                                  INIVB215
        KFDP(IENTRY,J)=KDPSTO(I,J)                                      INIVB216
 2      CONTINUE                                                        INIVB217
      IENTRY=IENTRY+1                                                   INIVB218
 1    CONTINUE                                                          INIVB219
      PARJ(LTHRES)=PARSTO                                               INIVB220
      NTOT = MDCY(KC,3)-LBMIN                                           INIVB221
      IF ( NTOT .GT. 0) THEN                                            INIVB222
        DO 23 IJ = 1,NTOT                                               INIVB223
          MDME(MDCY(KC,2)+LBMIN+IJ-1,1) =1                              INIVB224
 23     CONTINUE                                                        INIVB225
      ENDIF                                                             INIVB226
      RETURN                                                            INIVB227
      END                                                               INIVB228
      INTEGER FUNCTION KBCP(NT)                                         KBCP   2
C---------------------------------------------------------------------  KBCP   3
C!  Analyse event history to determine if event corresponds to B decay  KBCP   4
C into the user specified final state  (as defined in the GSTA card)    KBCP   5
C AUTHOR: B. Bloch-Devaux           881110                              KBCP   6
C                                                                       KBCP   7
C   Input : NT track number                                             KBCP   8
C                                                                       KBCP   9
C   Output:  KBCP = status versus Final state of decaying track NT      KBCP  10
C           0 means not a B meson or B meson not decaying into          KBCP  11
C                     specified final state (GSTA card)                 KBCP  12
C           +1 means B meson (d or s ) decaying into specified channel  KBCP  13
C           -1 means Bbar meson (d or s) decaying into specified channelKBCP  14
C                                                                       KBCP  15
C---------------------------------------------------------------------  KBCP  16
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      PARAMETER (LNBRCP=4)                                              USERCP 2
      COMMON/USERCP/NPARCP,JCODCP(LNBRCP),RHOCPM                        USERCP 3
      COMPLEX RHOCPM                                                    USERCP 4
      DIMENSION NIDEN(LNBRCP)                                           KBCP  19
      PARAMETER (ICOPSI=443,ICOK0S=310)                                 CODPAR72
      PARAMETER (LBPOS=100,NBU=521,NBD=511,NBS=531,NBC=541)             BCODES 2
      PARAMETER (NLB=5122,NXB= 5132)                                    BBL00511
C     LBPOS  = internal JETSET 7.3 code for B meson                     BCODES 3
      PARAMETER (NXB0=5232,NOB= 5332)                                   BBL007 9
C     NBU    = internal JETSET 7.3 code for Bu meson                    BCODES 4
C     NBD    = internal JETSET 7.3 code for Bd meson                    BCODES 5
C     NBS    = internal JETSET 7.3 code for Bs meson                    BCODES 6
C     NBC    = internal JETSET 7.3 code for Bc meson                    BCODES 7
C     NLB    = internal JETSET 7.3 code for /\B BARYON                  BBL00512
C     NXB    = internal JETSET 7.3 code for XIB BARYON                  BBL00513
C     NXB0   = internal JETSET 7.3 code for XIB0 BARYON                 BBL00710
C     NOB    = internal JETSET 7.3 code for OMEGAB BARYON               BBL00711
C    LUND 7.3 codes for BD, BDbar, BS ,BSbar , Psi and K0s              CODPAR74
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
C - index of the next vertex/track to be stored in KINE/VERT            KMACRO 2
C   bank known by its index JVK                                         KMACRO 3
      KNEXVK(JVK) = JVK + IW(JVK+1)+IW(JVK+2)+IW(JVK+3)                 KMACRO 4
C - # of vertices/tracks which could be stored in KINE/VERT             KMACRO 5
C   bank known by its index JVK                                         KMACRO 6
      LFRVK(JVK)  = IW(JVK) - (IW(JVK+1)+IW(JVK+2)+IW(JVK+3))           KMACRO 7
C - index of the 1st parameter of KINE/VERT bank known by its           KMACRO 8
C   index JVK                                                           KMACRO 9
      KPARVK(JVK) = JVK + IW(JVK+1)                                     KMACRO10
C - index of 1st vertex/track # contained into the list of              KMACRO11
C   bank KINE/VERT known by its index JVK                               KMACRO12
      KLISVK(JVK) = JVK + IW(JVK+1) + IW(JVK+2)                         KMACRO13
C - charge of ALEPH particle# JPA                                       KMACRO14
      CHARGE(JPA) = RTABL(IW(NAPAR),JPA,7)                              KMACRO15
C - mass of ALEPH particle# JPA                                         KMACRO16
      PARMAS(JPA) = RTABL(IW(NAPAR),JPA,6)                              KMACRO17
C - time of life of ALEPH particle# JPA                                 KMACRO18
      TIMLIF(JPA) = RTABL(IW(NAPAR),JPA,8)                              KMACRO19
C - # of vertices on a track known by its BOS index JVK /               KMACRO20
C   # of outgoing tracks of a vertex known by its BOS index JVK         KMACRO21
      NOFVK(JVK)  = IW(JVK+3)                                           KMACRO22
C - Particle type of a track known by its BOS index JVK                 KMACRO23
      KINTYP(JVK) = IW(KPARVK(JVK)+5)                                   KMACRO24
C - incoming track # of a vertex known by its BOS index JVK             KMACRO25
      INPTRK(JVK) = IW(KPARVK(JVK)+5)                                   KMACRO26
C - origin vertex # of a track known by its BOS index JVK               KMACRO27
      INPVRT(JVK) = IW(KLISVK(JVK)+1)                                   KMACRO28
C - momentum of a track known by its BOS index JVK                      KMACRO29
      PMODVK(JVK) = SQRT (RW(KPARVK(JVK)+1)**2 + RW(KPARVK(JVK)+2)**2   KMACRO30
     &                   +RW(KPARVK(JVK)+3)**2)                         KMACRO31
C - energy of a track known by its BOS index JVK                        KMACRO32
      ENERVK(JVK) = RW(KPARVK(JVK)+4)                                   KMACRO33
C - time of flight of the icoming particle to the vertex known by its   KMACRO34
C   BOS index JVK                                                       KMACRO35
      TOFLIT(JVK) = RW(KPARVK(JVK)+4)                                   KMACRO36
C - radius of the vertex known by its BOS index                         KMACRO37
      RADVK(JVK)  = SQRT (RW(KPARVK(JVK)+1)**2 + RW(KPARVK(JVK)+2)**2)  KMACRO38
C - mother track # of a track known by its BOS index                    KMACRO39
      MOTHVK(JVK) = INPTRK (NLINK('VERT',INPVRT(JVK)))                  KMACRO40
      KBCP=0                                                            KBCP  23
      IF( NPARCP.LE.0 ) GOTO 999                                        KBCP  24
      JKHIS = IW (NAMIND('KHIS'))                                       KBCP  25
      JKLIN = IW (NAMIND('KLIN'))                                       KBCP  26
C  IF KLIN OR KHIS BANK NOT HERE NO ANALYSIS CAN BE DONE                KBCP  27
      IF (JKHIS*JKLIN.NE.0) THEN                                        KBCP  28
      NPAR = IW(JKHIS+LMHROW)                                           KBCP  29
      IF (NPAR.GT.0) THEN                                               KBCP  30
      JKINM = NLINK('KINE',NT)                                          KBCP  31
      IF (JKINM.NE.0) THEN                                              KBCP  32
      DO 1 K = 1,LNBRCP                                                 KBCP  33
 1    NIDEN(K)=0                                                        KBCP  34
      ICODM = KINTYP(JKINM)                                             KBCP  35
      LCODM = ITABL(JKLIN,ICODM,1)                                      KBCP  36
      KCODM = ABS (LCODM)                                               KBCP  37
C  ICODM IS ALEPH CODE, LCODM THE GENERATOR(LUND) CORRESPONDING CODE    KBCP  38
      IF (KCODM.EQ.NBD .OR. KCODM.EQ.NBS) THEN                          KBCP  39
C  WE HAVE A Bd OR A Bs   ...look for daughters                         KBCP  40
      DO 10 I = NT+1,NPAR                                               KBCP  41
        IMOTH = MOD(ITABL(JKHIS,I,1),10000)                             KBCP  42
        IF (IMOTH.NE.NT) GO TO 10                                       KBCP  43
C  Mother is the right one                                              KBCP  44
C  Look if daughter's type is among the requested ones                  KBCP  45
C  Daughter's KINE has NR =I                                            KBCP  46
      JKINF = NLINK('KINE',I)                                           KBCP  47
      IF (JKINF.NE.0 ) THEN                                             KBCP  48
        ICODF = KINTYP(JKINF)                                           KBCP  49
        LCODF = ITABL(JKLIN,ICODF,1)                                    KBCP  50
        DO 20 K = 1,NPARCP                                              KBCP  51
          IF (LCODF.EQ.JCODCP(K)) THEN                                  KBCP  52
             NIDEN(K) = NIDEN(K) +1                                     KBCP  53
          ENDIF                                                         KBCP  54
  20    CONTINUE                                                        KBCP  55
      ENDIF                                                             KBCP  56
  10  CONTINUE                                                          KBCP  57
      ENDIF                                                             KBCP  58
C  Look if we get all particles required as final state                 KBCP  59
      KBCP = 1                                                          KBCP  60
      DO 30 K = 1,NPARCP                                                KBCP  61
      IF (NIDEN(K).NE.1) KBCP=0                                         KBCP  62
 30   CONTINUE                                                          KBCP  63
      KBCP = KBCP * SIGN(1 , LCODM)                                     KBCP  64
      ENDIF                                                             KBCP  65
      ENDIF                                                             KBCP  66
      ENDIF                                                             KBCP  67
 999  CONTINUE                                                          KBCP  68
      RETURN                                                            KBCP  69
      END                                                               KBCP  70
      INTEGER FUNCTION KBOSCI(NT)                                       KBOSCI 2
C---------------------------------------------------------------------- KBOSCI 3
C! Gives status of track # NT versus B osccillation from KINE and KHIS  KBOSCI 4
C! banks analysis                                                       KBOSCI 5
C  AUTHOR:  A.FALVARD                 881024                            KBOSCI 6
C           B.Bloch-Devaux            900926                            KBOSCI 7
C                                                                       KBOSCI 8
C     =1       BD-->BD        ( +C.C.)                                  KBOSCI 9
C     =2       BD-->BDBAR     ( +C.C.)                                  KBOSCI10
C     =3       BS-->BS        ( +C.C.)                                  KBOSCI11
C     =4       BS-->BSBAR     ( +C.C.)                                  KBOSCI12
C---------------------------------------------------------------------  KBOSCI13
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      PARAMETER (ICOPSI=443,ICOK0S=310)                                 CODPAR72
      PARAMETER (LBPOS=100,NBU=521,NBD=511,NBS=531,NBC=541)             BCODES 2
      PARAMETER (NLB=5122,NXB= 5132)                                    BBL00511
C     LBPOS  = internal JETSET 7.3 code for B meson                     BCODES 3
      PARAMETER (NXB0=5232,NOB= 5332)                                   BBL007 9
C     NBU    = internal JETSET 7.3 code for Bu meson                    BCODES 4
C     NBD    = internal JETSET 7.3 code for Bd meson                    BCODES 5
C     NBS    = internal JETSET 7.3 code for Bs meson                    BCODES 6
C     NBC    = internal JETSET 7.3 code for Bc meson                    BCODES 7
C     NLB    = internal JETSET 7.3 code for /\B BARYON                  BBL00512
C     NXB    = internal JETSET 7.3 code for XIB BARYON                  BBL00513
C     NXB0   = internal JETSET 7.3 code for XIB0 BARYON                 BBL00710
C     NOB    = internal JETSET 7.3 code for OMEGAB BARYON               BBL00711
C    LUND 7.3 codes for BD, BDbar, BS ,BSbar , Psi and K0s              CODPAR74
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
C - index of the next vertex/track to be stored in KINE/VERT            KMACRO 2
C   bank known by its index JVK                                         KMACRO 3
      KNEXVK(JVK) = JVK + IW(JVK+1)+IW(JVK+2)+IW(JVK+3)                 KMACRO 4
C - # of vertices/tracks which could be stored in KINE/VERT             KMACRO 5
C   bank known by its index JVK                                         KMACRO 6
      LFRVK(JVK)  = IW(JVK) - (IW(JVK+1)+IW(JVK+2)+IW(JVK+3))           KMACRO 7
C - index of the 1st parameter of KINE/VERT bank known by its           KMACRO 8
C   index JVK                                                           KMACRO 9
      KPARVK(JVK) = JVK + IW(JVK+1)                                     KMACRO10
C - index of 1st vertex/track # contained into the list of              KMACRO11
C   bank KINE/VERT known by its index JVK                               KMACRO12
      KLISVK(JVK) = JVK + IW(JVK+1) + IW(JVK+2)                         KMACRO13
C - charge of ALEPH particle# JPA                                       KMACRO14
      CHARGE(JPA) = RTABL(IW(NAPAR),JPA,7)                              KMACRO15
C - mass of ALEPH particle# JPA                                         KMACRO16
      PARMAS(JPA) = RTABL(IW(NAPAR),JPA,6)                              KMACRO17
C - time of life of ALEPH particle# JPA                                 KMACRO18
      TIMLIF(JPA) = RTABL(IW(NAPAR),JPA,8)                              KMACRO19
C - # of vertices on a track known by its BOS index JVK /               KMACRO20
C   # of outgoing tracks of a vertex known by its BOS index JVK         KMACRO21
      NOFVK(JVK)  = IW(JVK+3)                                           KMACRO22
C - Particle type of a track known by its BOS index JVK                 KMACRO23
      KINTYP(JVK) = IW(KPARVK(JVK)+5)                                   KMACRO24
C - incoming track # of a vertex known by its BOS index JVK             KMACRO25
      INPTRK(JVK) = IW(KPARVK(JVK)+5)                                   KMACRO26
C - origin vertex # of a track known by its BOS index JVK               KMACRO27
      INPVRT(JVK) = IW(KLISVK(JVK)+1)                                   KMACRO28
C - momentum of a track known by its BOS index JVK                      KMACRO29
      PMODVK(JVK) = SQRT (RW(KPARVK(JVK)+1)**2 + RW(KPARVK(JVK)+2)**2   KMACRO30
     &                   +RW(KPARVK(JVK)+3)**2)                         KMACRO31
C - energy of a track known by its BOS index JVK                        KMACRO32
      ENERVK(JVK) = RW(KPARVK(JVK)+4)                                   KMACRO33
C - time of flight of the icoming particle to the vertex known by its   KMACRO34
C   BOS index JVK                                                       KMACRO35
      TOFLIT(JVK) = RW(KPARVK(JVK)+4)                                   KMACRO36
C - radius of the vertex known by its BOS index                         KMACRO37
      RADVK(JVK)  = SQRT (RW(KPARVK(JVK)+1)**2 + RW(KPARVK(JVK)+2)**2)  KMACRO38
C - mother track # of a track known by its BOS index                    KMACRO39
      MOTHVK(JVK) = INPTRK (NLINK('VERT',INPVRT(JVK)))                  KMACRO40
      KBOSCI=0                                                          KBOSCI18
      IF(NT.EQ.1)GOTO 999                                               KBOSCI19
      JKINE=NLINK('KINE',NT)                                            KBOSCI20
      JNAME=KINTYP(JKINE)                                               KBOSCI21
      JKLIN = NLINK('KLIN',0)                                           KBOSCI22
      IF (JKLIN.EQ.0) GO TO 999                                         KBOSCI23
      LNAME = ITABL(JKLIN,JNAME,1)                                      KBOSCI24
      KNAME = ABS ( LNAME)                                              KBOSCI25
      IF(KNAME.NE.NBD.AND.KNAME.NE.NBS)GOTO 999                         KBOSCI26
      IKHIS=NLINK('KHIS',0)                                             KBOSCI27
      IF(IKHIS.EQ.0)GOTO 999                                            KBOSCI28
      IMOTH=MOD(IW(IKHIS+LMHLEN+NT),10000)                              KBOSCI29
      JKINE=NLINK('KINE',IMOTH)                                         KBOSCI30
      JNAME1=KINTYP(JKINE)                                              KBOSCI31
      LNAME1 = ITABL(JKLIN,JNAME1,1)                                    KBOSCI32
      KNAME1 = ABS ( LNAME1)                                            KBOSCI33
      IF(KNAME1.NE.NBD.AND.KNAME1.NE.NBS)GOTO 999                       KBOSCI34
      IF (LNAME . EQ. LNAME1 ) THEN                                     KBOSCI35
         KBOSCI = 1                                                     KBOSCI36
      ELSEIF (LNAME.EQ.-LNAME1) THEN                                    KBOSCI37
         KBOSCI = 2                                                     KBOSCI38
      ENDIF                                                             KBOSCI39
      IF (KNAME1.EQ.NBS) KBOSCI=KBOSCI+2                                KBOSCI40
C                                                                       KBOSCI41
C  KBOSCI=1      BD-->BD      (+ C.C.)                                  KBOSCI42
C  KBOSCI=2      BD-->BDB     (+ C.C.)                                  KBOSCI43
C  KBOSCI=3      BS-->BS      (+ C.C.)                                  KBOSCI44
C  KBOSCI=4      BS-->BSB     (+ C.C.)                                  KBOSCI45
C                                                                       KBOSCI46
 999  CONTINUE                                                          KBOSCI47
      RETURN                                                            KBOSCI48
      END                                                               KBOSCI49
      INTEGER FUNCTION KB7MIX(ITR)                                      KB7MIX 2
C---------------------------------------------------------------------  KB7MIX 3
C! Check if track is result of B mixing  LUND 7.3 version               KB7MIX 4
C  AUTHOR: B. BLOCH-DEVAUX AND A. FALVARD          881024 900926        KB7MIX 5
C  works from quantities in the lund coomon                             KB7MIX 6
C  =0:  THE ITR TRACK IS NOT THE RESULT OF B-BBAR OSCILLATION           KB7MIX 7
C  =1:  "     "    "  IS THE RESULT OF B-BBAR OSCILLATION               KB7MIX 8
C---------------------------------------------------------------------  KB7MIX 9
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
C                                                                       LUN7CO12
      PARAMETER (ICOPSI=443,ICOK0S=310)                                 CODPAR72
      PARAMETER (LBPOS=100,NBU=521,NBD=511,NBS=531,NBC=541)             BCODES 2
      PARAMETER (NLB=5122,NXB= 5132)                                    BBL00511
C     LBPOS  = internal JETSET 7.3 code for B meson                     BCODES 3
      PARAMETER (NXB0=5232,NOB= 5332)                                   BBL007 9
C     NBU    = internal JETSET 7.3 code for Bu meson                    BCODES 4
C     NBD    = internal JETSET 7.3 code for Bd meson                    BCODES 5
C     NBS    = internal JETSET 7.3 code for Bs meson                    BCODES 6
C     NBC    = internal JETSET 7.3 code for Bc meson                    BCODES 7
C     NLB    = internal JETSET 7.3 code for /\B BARYON                  BBL00512
C     NXB    = internal JETSET 7.3 code for XIB BARYON                  BBL00513
C     NXB0   = internal JETSET 7.3 code for XIB0 BARYON                 BBL00710
C     NOB    = internal JETSET 7.3 code for OMEGAB BARYON               BBL00711
C    LUND 7.3 codes for BD, BDbar, BS ,BSbar , Psi and K0s              CODPAR74
      ILUN=ABS(K7LU(ITR,2))                                             KB7MIX12
      KB7MIX=0                                                          KB7MIX13
      IF(ITR.EQ.N7LU)RETURN                                             KB7MIX14
      IF(ILUN.EQ.NBD.OR.ILUN.EQ.NBS)THEN                                KB7MIX15
C  WE HAVE A BD MESON OR A BS MESON ,  LOOK IF DAUGHTER IS ONE ALSO     KB7MIX16
        IDAUG = K7LU(ITR,4)                                             KB7MIX17
        ILAST = K7LU(ITR,5)                                             KB7MIX18
        IF (IDAUG.GT.0 .AND.ILAST.EQ.IDAUG) THEN                        KB7MIX19
C  ARE MOTHER AND DAUGHTER B MESONS?                                    KB7MIX20
          ILUM=ABS(K7LU(IDAUG,2))                                       KB7MIX21
          IF(ILUN.EQ.ILUM)THEN                                          KB7MIX22
            KB7MIX=1                                                    KB7MIX23
C  MIXING OCCURED IN THE PROCESS                                        KB7MIX24
          ENDIF                                                         KB7MIX25
C  ONE DAUGHTER FOUND BUT NOT A B MESON:  NO MIXING AND RETURN          KB7MIX26
        ENDIF                                                           KB7MIX27
      ENDIF                                                             KB7MIX28
      RETURN                                                            KB7MIX29
      END                                                               KB7MIX30
      SUBROUTINE KFMIBK (VMAIN,RPARA,IPARA,MTRAK,ISTAT)                 KFMIBK 2
C -----------------------------------------------------------           KFMIBK 3
C   A. FALVARD and B. Bloch to include mixing and CP violation Oct.88   KFMIBK 4
C (from KFEVBK  from J.Boucrot - B.Bloch - F.Ranjard - 870515  )        KFMIBK 5
C! Fill event banks KINE VERT KHIS                                      KFMIBK 6
C  first KINE and VERT banks are booked and filled with parameters      KFMIBK 7
C        sent as arguments (all vertices at the same position).         KFMIBK 8
C  then  depending on the decay length of secondary particles , the     KFMIBK 9
C        secondary vertices are displaced from the main vertex . The    KFMIBK10
C        propagation follows a straight line for neutral generating a   KFMIBK11
C        secondary vertec, and a simple helix for charged particles.    KFMIBK12
C        In case of charge particles generating a secondary vertex,     KFMIBK13
C        swim Px and Py of all secondaries up to decay vertex. Then     KFMIBK14
C        store the time of flight.                                      KFMIBK15
C        The magnetic field is assumed to be 15.0 Kgauss.               KFMIBK16
C                                                                       KFMIBK17
C - structure: SUBROUTINE subprogram                                    KFMIBK18
C              USER ENTRY NAME: KFMIBK                                  KFMIBK19
C              External References: KBVERT/KBKINE/KGPART/KGDECL(ALEPHLIBKFMIBK20
C                                   KBOSCI/MIDECL/CPUSER(THIS LIB)      KFMIBK21
C              Comdecks referenced: BCS, ALCONS, KIPARA, BMACRO, KMACRO KFMIBK22
C                                                                       KFMIBK23
C - USAGE   : CALL KFMIBK (VMAIN,RPARA,IPARA,MTRAK,ISTAT)               KFMIBK24
C - Input   : VMAIN          = vx,vy,vz of the main vertex              KFMIBK25
C             RPARA (1-4,k)  = px,py,pz,(E) of track(k)                 KFMIBK26
C                              if RPARA(4,k)=0. then the Energy is      KFMIBK27
C                              computed by the package itself           KFMIBK28
C             IPARA (1,k)    = vertex# of the origin of the track(k)    KFMIBK29
C                   (2,k)    = vertex# of the decay of the track(k)     KFMIBK30
C                                0 if there is no decay                 KFMIBK31
C                   (3,k)    = ALEPH particle#                          KFMIBK32
C             MTRAK          = # of tracks                              KFMIBK33
C             ISTAT          = return code  ( 0 means OK)               KFMIBK34
C                              -1 means too many particles              KFMIBK35
C                              -2 means wrong KINE/VERT booking         KFMIBK36
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12         ALCONS 2
      REAL RADEG, DEGRA                                                 ALCONS 3
      REAL CLGHT                                                        ALCONS 4
      INTEGER NBITW, NBYTW, LCHAR                                       ALCONS 5
      PARAMETER (PI=3.141592653589)                                     ALCONS 6
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)                          ALCONS 7
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          ALCONS 8
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         ALCONS 9
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         ALCONS10
      PARAMETER (CLGHT = 29.9792458)                                    ALCONS11
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)              ALCONS12
C                                                                       ALCONS13
      PARAMETER (LHKIN=3, LPKIN=5, LKVX=2, LHVER=3, LPVER=5, LVKI=50)   KIPARA 2
      PARAMETER (LGDCA=32)                                              KIPARA 3
      PARAMETER (LRPART=200, LCKLIN=1)                                  KIPARA 4
      PARAMETER (LRECL=16380, LRUN=1, LEXP=1001, LRTYP=1000)            KIPARA 5
      CHARACTER*60 LTITL                                                KIPARA 6
      PARAMETER (LUCOD=0, LNOTRK=100, LTITL='KINGAL run')               KIPARA 7
      PARAMETER (LUTRK=300)                                             KIPARA 8
      PARAMETER (BFIEL=15., CFIEL=BFIEL*3.E-4)                          KIPARA 9
      PARAMETER (CLITS = CLGHT * 1.E+09)                                KFMIBK40
      INTEGER IPARA(3,*)                                                KFMIBK41
      REAL RPARA(4,*),VMAIN(3)                                          KFMIBK42
      REAL KGDECL,MIDECL,CPDECL                                         KFMIBK43
      EXTERNAL KBOSCI                                                   KFMIBK44
      LOGICAL FDECAY,FNEUTR                                             KFMIBK45
      DATA NAPAR /0/                                                    KFMIBK46
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
C - index of the next vertex/track to be stored in KINE/VERT            KMACRO 2
C   bank known by its index JVK                                         KMACRO 3
      KNEXVK(JVK) = JVK + IW(JVK+1)+IW(JVK+2)+IW(JVK+3)                 KMACRO 4
C - # of vertices/tracks which could be stored in KINE/VERT             KMACRO 5
C   bank known by its index JVK                                         KMACRO 6
      LFRVK(JVK)  = IW(JVK) - (IW(JVK+1)+IW(JVK+2)+IW(JVK+3))           KMACRO 7
C - index of the 1st parameter of KINE/VERT bank known by its           KMACRO 8
C   index JVK                                                           KMACRO 9
      KPARVK(JVK) = JVK + IW(JVK+1)                                     KMACRO10
C - index of 1st vertex/track # contained into the list of              KMACRO11
C   bank KINE/VERT known by its index JVK                               KMACRO12
      KLISVK(JVK) = JVK + IW(JVK+1) + IW(JVK+2)                         KMACRO13
C - charge of ALEPH particle# JPA                                       KMACRO14
      CHARGE(JPA) = RTABL(IW(NAPAR),JPA,7)                              KMACRO15
C - mass of ALEPH particle# JPA                                         KMACRO16
      PARMAS(JPA) = RTABL(IW(NAPAR),JPA,6)                              KMACRO17
C - time of life of ALEPH particle# JPA                                 KMACRO18
      TIMLIF(JPA) = RTABL(IW(NAPAR),JPA,8)                              KMACRO19
C - # of vertices on a track known by its BOS index JVK /               KMACRO20
C   # of outgoing tracks of a vertex known by its BOS index JVK         KMACRO21
      NOFVK(JVK)  = IW(JVK+3)                                           KMACRO22
C - Particle type of a track known by its BOS index JVK                 KMACRO23
      KINTYP(JVK) = IW(KPARVK(JVK)+5)                                   KMACRO24
C - incoming track # of a vertex known by its BOS index JVK             KMACRO25
      INPTRK(JVK) = IW(KPARVK(JVK)+5)                                   KMACRO26
C - origin vertex # of a track known by its BOS index JVK               KMACRO27
      INPVRT(JVK) = IW(KLISVK(JVK)+1)                                   KMACRO28
C - momentum of a track known by its BOS index JVK                      KMACRO29
      PMODVK(JVK) = SQRT (RW(KPARVK(JVK)+1)**2 + RW(KPARVK(JVK)+2)**2   KMACRO30
     &                   +RW(KPARVK(JVK)+3)**2)                         KMACRO31
C - energy of a track known by its BOS index JVK                        KMACRO32
      ENERVK(JVK) = RW(KPARVK(JVK)+4)                                   KMACRO33
C - time of flight of the icoming particle to the vertex known by its   KMACRO34
C   BOS index JVK                                                       KMACRO35
      TOFLIT(JVK) = RW(KPARVK(JVK)+4)                                   KMACRO36
C - radius of the vertex known by its BOS index                         KMACRO37
      RADVK(JVK)  = SQRT (RW(KPARVK(JVK)+1)**2 + RW(KPARVK(JVK)+2)**2)  KMACRO38
C - mother track # of a track known by its BOS index                    KMACRO39
      MOTHVK(JVK) = INPTRK (NLINK('VERT',INPVRT(JVK)))                  KMACRO40
      FDECAY(JTR) = IPARA(2,JTR).GT.1 .AND. IPARA(2,JTR).NE.IPARA(1,JTR)KFMIBK49
      FNEUTR(JPA) = ABS (CHARGE(JPA)) .LT. .1                           KFMIBK50
C -------------------------------------------------------               KFMIBK51
      ISTAT = 0                                                         KFMIBK52
C                                                                       KFMIBK53
C - Get 'PART' name-index at the 1st entry                              KFMIBK54
      IF (NAPAR .EQ. 0) NAPAR = NAMIND ('PART')                         KFMIBK55
C                                                                       KFMIBK56
C - Create main vertex bank                                             KFMIBK57
      IVMAI = 1                                                         KFMIBK58
      JVERT = KBVERT (IVMAI,VMAIN,0)                                    KFMIBK59
C                                                                       KFMIBK60
C - Fill VERT and KINE banks                                            KFMIBK61
      DO 1 NT = 1,MTRAK                                                 KFMIBK62
         JKINE = KBKINE (NT,RPARA(1,NT),IPARA(3,NT),IPARA(1,NT))        KFMIBK63
         IF (JKINE.LE.0) GOTO 998                                       KFMIBK64
         IF (IPARA(2,NT).GT.0) THEN                                     KFMIBK65
            JVERT = KBVERT (IPARA(2,NT),VMAIN,NT)                       KFMIBK66
            IF (JVERT.LE.0) GOTO 998                                    KFMIBK67
         ENDIF                                                          KFMIBK68
 1    CONTINUE                                                          KFMIBK69
C                                                                       KFMIBK70
C - Propagate secondary vertices if any                                 KFMIBK71
C                                                                       KFMIBK72
      DO 100 NT = 1,MTRAK                                               KFMIBK73
         IPART = IPARA(3,NT)                                            KFMIBK74
         PMOD = SQRT (RPARA(1,NT)**2+RPARA(2,NT)**2+RPARA(3,NT)**2)     KFMIBK75
         TLIF = TIMLIF (IPART)                                          KFMIBK76
         ZMAS = PARMAS (IPART)                                          KFMIBK77
         IF (FDECAY(NT)) THEN                                           KFMIBK78
           JKINE=NLINK('KINE',NT)                                       KFMIBK79
           IKHIS=NLINK('KHIS',0)                                        KFMIBK80
           IOSCI=KBOSCI(NT)                                             KFMIBK81
           IF(IOSCI.EQ.0)THEN                                           KFMIBK82
             DCLEN=KGDECL(PMOD,ZMAS,TLIF)                               KFMIBK83
           ELSE                                                         KFMIBK84
C                                                                       KFMIBK85
C Take into account  B MIXING - 881024                                  KFMIBK86
C                                                                       KFMIBK87
             ICP = KBCP(NT)                                             KFMIBK88
C LOOKS FOR SPECIFIC DECAYS FOR CP VIOLATION                            KFMIBK89
C                                                                       KFMIBK90
             IF(ICP.NE.0)THEN                                           KFMIBK91
                DCLEN=CPDECL(PMOD,ZMAS,TLIF,IOSCI)                      KFMIBK92
             ELSE                                                       KFMIBK93
                DCLEN=MIDECL(PMOD,ZMAS,TLIF,IOSCI)                      KFMIBK94
             ENDIF                                                      KFMIBK95
             CALL HFILL(10013+IOSCI,DCLEN,0.,1.)                        KFMIBK96
           ENDIF                                                        KFMIBK97
C---------------------------------------------------------------------- KFMIBK98
            IF (DCLEN .LE. 0.) GOTO 100                                 KFMIBK99
C           get the origin vertex                                       KFMIB100
            IVOR = IPARA(1,NT)                                          KFMIB101
            JVOR = NLINK ('VERT',IVOR)                                  KFMIB102
            KVO  = KPARVK (JVOR)                                        KFMIB103
C           get the decay vertex                                        KFMIB104
            IVOUT = IPARA(2,NT)                                         KFMIB105
            JVERT = NLINK ('VERT',IVOUT)                                KFMIB106
            KVX   = KPARVK (JVERT)                                      KFMIB107
            KVTR  = KLISVK (JVERT)                                      KFMIB108
C                                                                       KFMIB109
C           straight line for neutral generating a secondary vx         KFMIB110
            IF (FNEUTR(IPART)) THEN                                     KFMIB111
               DO 102 IX = 1,3                                          KFMIB112
                  RW(KVX+IX) = RW(KVO+IX) + RPARA(IX,NT)*DCLEN/PMOD     KFMIB113
 102           CONTINUE                                                 KFMIB114
            ELSE                                                        KFMIB115
C                                                                       KFMIB116
C           propagation according to asimple helix for charged          KFMIB117
               PT = SQRT (RPARA(1,NT)**2+RPARA(2,NT)**2)                KFMIB118
               RAD = PT / (CFIEL*CHARGE(IPART))                         KFMIB119
               DPSI = DCLEN / RAD                                       KFMIB120
               DXDS = RPARA(1,NT) / PMOD                                KFMIB121
               DYDS = RPARA(2,NT) / PMOD                                KFMIB122
               DZDS = RPARA(3,NT) / PMOD                                KFMIB123
               CPSI = COS (DPSI)                                        KFMIB124
               SPSI = SIN (DPSI)                                        KFMIB125
               DX = RAD * (DXDS*SPSI - DYDS*CPSI + DYDS)                KFMIB126
               DY = RAD * (DYDS*SPSI + DXDS*CPSI - DXDS)                KFMIB127
               DZ = DCLEN * DZDS                                        KFMIB128
               RW(KVX+1)  = RW(KVO+1) + DX                              KFMIB129
               RW(KVX+2)  = RW(KVO+2) + DY                              KFMIB130
               RW(KVX+3)  = RW(KVO+3) + DZ                              KFMIB131
C           swim Px and Py of all secondaries up to decay vertex        KFMIB132
               MTVX = IW(JVERT+3)                                       KFMIB133
               IF (MTVX .GT. 0) THEN                                    KFMIB134
                  DO 103 N=1,MTVX                                       KFMIB135
                     NS = IW (KVTR+N)                                   KFMIB136
                     JKINE = NLINK ('KINE',NS)                          KFMIB137
                     IF (JKINE.EQ.0) GOTO 998                           KFMIB138
                     KTR = KPARVK (JKINE)                               KFMIB139
                     RW(KTR+1) = RPARA(1,NS)*CPSI - RPARA(2,NS)*SPSI    KFMIB140
                     RW(KTR+2) = RPARA(1,NS)*SPSI + RPARA(2,NS)*CPSI    KFMIB141
 103              CONTINUE                                              KFMIB142
               ENDIF                                                    KFMIB143
            ENDIF                                                       KFMIB144
C           Store the time of flight                                    KFMIB145
            RW(KVX+4)  = RW(KVO+4) + DCLEN/CLITS                        KFMIB146
         ENDIF                                                          KFMIB147
C                                                                       KFMIB148
 100   CONTINUE                                                         KFMIB149
C                                                                       KFMIB150
       GOTO 999                                                         KFMIB151
C                                                                       KFMIB152
C - Error                                                               KFMIB153
C      unsuccessfull booking of VERT or KINE                            KFMIB154
 998   ISTAT = -2                                                       KFMIB155
C                                                                       KFMIB156
C - End                                                                 KFMIB157
 999   CONTINUE                                                         KFMIB158
       END                                                              KFMIB159
      SUBROUTINE KXL7FL(KS,KF,KM,KFD,KLD,P,NLA)                         KXL7FL 2
C--------------------------------------------------------------------   KXL7FL 3
C      B. BLOCH-DEVAUX April  1991                                      KXL7FL 4
C                                                                       KXL7FL 5
C!   Add an entry in LUJETS common JETSET 7.3 version                   KXL7FL 6
C     structure : subroutine                                            KXL7FL 7
C                                                                       KXL7FL 8
C     input     : KS  : STATUS CODE                                     KXL7FL 9
C                 KF  : PARTICLE CODE                                   KXL7FL10
C                 KM  : MOTHER NUMBER                                   KXL7FL11
C                 KFD : FIRST DAUGHTER ( 0 IF NO)                       KXL7FL12
C                 KFL : LAST  DAUGHTER ( 0 IF NO)                       KXL7FL13
C    output     : NLA : entry filled                                    KXL7FL14
C--------------------------------------------------------------------   KXL7FL15
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
C                                                                       LUN7CO12
      DIMENSION P(4)                                                    KXL7FL17
      N7LU = N7LU+1                                                     KXL7FL18
      NLA = N7LU                                                        KXL7FL19
      K7LU(N7LU,1) = KS                                                 KXL7FL20
      K7LU(N7LU,2) = KF                                                 KXL7FL21
      K7LU(N7LU,3) = KM                                                 KXL7FL22
      K7LU(N7LU,4) = KFD                                                KXL7FL23
      K7LU(N7LU,5) = KLD                                                KXL7FL24
      DO 10 I=1,4                                                       KXL7FL25
        P7LU(N7LU,I) = P(I)                                             KXL7FL26
 10   CONTINUE                                                          KXL7FL27
      IF (KF.NE.23) THEN                                                KXL7FL28
         P7LU(N7LU,5)=ULMASS(KF)                                        KXL7FL29
      ELSE                                                              KXL7FL30
         P7LU(N7LU,5)=SQRT(ABS(P7LU(N7LU,4)**2-P7LU(N7LU,3)**2-         KXL7FL31
     $  P7LU(N7LU,2)**2-P7LU(N7LU,1)**2 ))                              KXL7FL32
      ENDIF                                                             KXL7FL33
      RETURN                                                            KXL7FL34
      END                                                               KXL7FL35
      SUBROUTINE KXL7MI (VMAIN,ISTAT,MVX,MTRK)                          KXL7MI 2
C ---------------------------------------------------------             KXL7MI 3
C   B. Bloch -Devaux September 1990 from KXLUMI                         KXL7MI 4
C! Build the event interface LUND7.3-Aleph  including mixing effects    KXL7MI 5
C - Fill    : PTRAK(ix,n)  = px,py,pz,E( or Mass from Alephlib 9.0) of  KXL7MI 6
C                            track(n)                                   KXL7MI 7
C                            if E or M=0.it will be filled by the systemKXL7MI 8
C             IPVNU(1,n)   = origin vertex # of track(n)                KXL7MI 9
C                  (2,n)   = decay vertex # of track(n)                 KXL7MI10
C                             0 if no decay                             KXL7MI11
C                  (3,n)   = ALEPH particle #                           KXL7MI12
C             IPCOD(n)     = LUND history code of track(n)              KXL7MI13
C - Book    : KHIS bank filled with IPCOD(n)                            KXL7MI14
C - CALL    : KFMIBK (VMAIN,PTRAK,IPVNU,MTRK,JSTAT)                     KXL7MI15
C             to book propagate the decay and fill VERT and KINE        KXL7MI16
C                                                                       KXL7MI17
C - structure: SUBROUTINE subprogram                                    KXL7MI18
C              User Entry Name: KXLUMI                                  KXL7MI19
C              External Regerences: NAMIND(BOS77)                       KXL7MI20
C                                   ALTABL/ALVERS/KFMIBK(ALEPHLIB)      KXL7MI21
C                                   KB7MIX (THIS LIB)                   KXL7MI22
C              Comdecks referenced: BCS, LUNDCOM, ALCONS, KIPARA        KXL7MI23
C                                                                       KXL7MI24
C - usage   : CALL KXL7MI (VMAIN,ISTAT,MVX,MTRK)                        KXL7MI25
C - Input   : VMAIN = vx,vy,vz,tof of the primary vertex                KXL7MI26
C - Output  : ISTAT = status word ( = 0 means OK)                       KXL7MI27
C                     - 1 means VERT or KINE bank missing               KXL7MI28
C                     - 2 means not enough space for VERT or KINE       KXL7MI29
C                     - 3 means too many tracks                         KXL7MI30
C                     - 4 electrons beams not stored as lines 1 and 2   KXL7MI31
C                     - 5 means Lund status code larger than 4 found    KXL7MI32
C                     > 0 means unknown LUND particle# ISTAT            KXL7MI33
C             MVX   = # of vertices                                     KXL7MI34
C             MTRK  = # of tracks to be propagated ( no beam electrons )KXL7MI35
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
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
C                                                                       LUN7CO12
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12         ALCONS 2
      REAL RADEG, DEGRA                                                 ALCONS 3
      REAL CLGHT                                                        ALCONS 4
      INTEGER NBITW, NBYTW, LCHAR                                       ALCONS 5
      PARAMETER (PI=3.141592653589)                                     ALCONS 6
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)                          ALCONS 7
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          ALCONS 8
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         ALCONS 9
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         ALCONS10
      PARAMETER (CLGHT = 29.9792458)                                    ALCONS11
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)              ALCONS12
C                                                                       ALCONS13
      PARAMETER (LHKIN=3, LPKIN=5, LKVX=2, LHVER=3, LPVER=5, LVKI=50)   KIPARA 2
      PARAMETER (LGDCA=32)                                              KIPARA 3
      PARAMETER (LRPART=200, LCKLIN=1)                                  KIPARA 4
      PARAMETER (LRECL=16380, LRUN=1, LEXP=1001, LRTYP=1000)            KIPARA 5
      CHARACTER*60 LTITL                                                KIPARA 6
      PARAMETER (LUCOD=0, LNOTRK=100, LTITL='KINGAL run')               KIPARA 7
      PARAMETER (LUTRK=300)                                             KIPARA 8
      PARAMETER (BFIEL=15., CFIEL=BFIEL*3.E-4)                          KIPARA 9
      PARAMETER ( TLIMI=1.E-15)                                         KXL7MI40
      REAL PTRAK(4,LUTRK),VMAIN(4)                                      KXL7MI41
      INTEGER IPVNU(3,LUTRK),IPCOD(LUTRK)                               KXL7MI42
      INTEGER ALTABL                                                    KXL7MI43
      DATA NAPAR/0/                                                     KXL7MI44
      DATA IFIR/0/                                                      KXL7MI45
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
C - index of the next vertex/track to be stored in KINE/VERT            KMACRO 2
C   bank known by its index JVK                                         KMACRO 3
      KNEXVK(JVK) = JVK + IW(JVK+1)+IW(JVK+2)+IW(JVK+3)                 KMACRO 4
C - # of vertices/tracks which could be stored in KINE/VERT             KMACRO 5
C   bank known by its index JVK                                         KMACRO 6
      LFRVK(JVK)  = IW(JVK) - (IW(JVK+1)+IW(JVK+2)+IW(JVK+3))           KMACRO 7
C - index of the 1st parameter of KINE/VERT bank known by its           KMACRO 8
C   index JVK                                                           KMACRO 9
      KPARVK(JVK) = JVK + IW(JVK+1)                                     KMACRO10
C - index of 1st vertex/track # contained into the list of              KMACRO11
C   bank KINE/VERT known by its index JVK                               KMACRO12
      KLISVK(JVK) = JVK + IW(JVK+1) + IW(JVK+2)                         KMACRO13
C - charge of ALEPH particle# JPA                                       KMACRO14
      CHARGE(JPA) = RTABL(IW(NAPAR),JPA,7)                              KMACRO15
C - mass of ALEPH particle# JPA                                         KMACRO16
      PARMAS(JPA) = RTABL(IW(NAPAR),JPA,6)                              KMACRO17
C - time of life of ALEPH particle# JPA                                 KMACRO18
      TIMLIF(JPA) = RTABL(IW(NAPAR),JPA,8)                              KMACRO19
C - # of vertices on a track known by its BOS index JVK /               KMACRO20
C   # of outgoing tracks of a vertex known by its BOS index JVK         KMACRO21
      NOFVK(JVK)  = IW(JVK+3)                                           KMACRO22
C - Particle type of a track known by its BOS index JVK                 KMACRO23
      KINTYP(JVK) = IW(KPARVK(JVK)+5)                                   KMACRO24
C - incoming track # of a vertex known by its BOS index JVK             KMACRO25
      INPTRK(JVK) = IW(KPARVK(JVK)+5)                                   KMACRO26
C - origin vertex # of a track known by its BOS index JVK               KMACRO27
      INPVRT(JVK) = IW(KLISVK(JVK)+1)                                   KMACRO28
C - momentum of a track known by its BOS index JVK                      KMACRO29
      PMODVK(JVK) = SQRT (RW(KPARVK(JVK)+1)**2 + RW(KPARVK(JVK)+2)**2   KMACRO30
     &                   +RW(KPARVK(JVK)+3)**2)                         KMACRO31
C - energy of a track known by its BOS index JVK                        KMACRO32
      ENERVK(JVK) = RW(KPARVK(JVK)+4)                                   KMACRO33
C - time of flight of the icoming particle to the vertex known by its   KMACRO34
C   BOS index JVK                                                       KMACRO35
      TOFLIT(JVK) = RW(KPARVK(JVK)+4)                                   KMACRO36
C - radius of the vertex known by its BOS index                         KMACRO37
      RADVK(JVK)  = SQRT (RW(KPARVK(JVK)+1)**2 + RW(KPARVK(JVK)+2)**2)  KMACRO38
C - mother track # of a track known by its BOS index                    KMACRO39
      MOTHVK(JVK) = INPTRK (NLINK('VERT',INPVRT(JVK)))                  KMACRO40
C ------------------------------------------------------                KXL7MI48
      IF (NAPAR .EQ. 0) NAPAR = NAMIND ('PART')                         KXL7MI49
      IF (IFIR.EQ.0) THEN                                               KXL7MI50
         IFIR = IFIR+1                                                  KXL7MI51
         CALL ALVERS(ALEFV)                                             KXL7MI52
      ENDIF                                                             KXL7MI53
C                                                                       KXL7MI54
C - Check particle buffer length                                        KXL7MI55
      IF (N7LU .GT. LUTRK) THEN                                         KXL7MI56
         WRITE (IW(6),'(/1X,''+++KXL7MI+++ not enough space to save''   KXL7MI57
     &         ,'' the event :# of tracks = '',I2,2X,''allowed = '',I2  KXL7MI58
     &         /13X,''==>increase LUTRK in *CD KIPARA'')') N7LU,LUTRK   KXL7MI59
         ISTAT = - 3                                                    KXL7MI60
         GOTO 999                                                       KXL7MI61
      ENDIF                                                             KXL7MI62
C                                                                       KXL7MI63
C - Build array containing vertex # and particle # of each track        KXL7MI64
C                                                                       KXL7MI65
      IBEA=0                                                            KXL7MI66
      NVER = 1                                                          KXL7MI67
      DO 10 ITR=1,N7LU                                                  KXL7MI68
C Look for "mother" particle                                            KXL7MI69
         ILUN  = K7LU(ITR,2)                                            KXL7MI70
         IPART = KGPART(ILUN)                                           KXL7MI71
         IF (IPART .LE. 0) GOTO 998                                     KXL7MI72
         KS = K7LU(ITR,1)                                               KXL7MI73
         IF ( KS.EQ.21 .AND. ILUN.EQ.23 ) KS = 11                       KXL7MI74
         IMOTH = K7LU(ITR,3)                                            KXL7MI75
C                                                                       KXL7MI76
C Store now momentum components and codes of the track :                KXL7MI77
          DO 9 I=1,3                                                    KXL7MI78
 9        PTRAK(I,ITR-IBEA) = P7LU(ITR,I)                               KXL7MI79
          IF (ALEFV.LT.9.0) THEN                                        KXL7MI80
             PTRAK(4,ITR-IBEA) = P7LU(ITR,4)                            KXL7MI81
          ELSE                                                          KXL7MI82
             PTRAK(4,ITR-IBEA) = P7LU(ITR,5)                            KXL7MI83
          ENDIF                                                         KXL7MI84
          IPVNU(3,ITR-IBEA)=IPART                                       KXL7MI85
          IPCOD(ITR-IBEA)=KS*10000+K7LU(ITR,3)                          KXL7MI86
             IF (KS.LE.5) THEN                                          KXL7MI87
C            Particle not decayed in LUND                               KXL7MI88
C            if stable particle created in initial state ,IMOTH=0       KXL7MI89
                 IF (IMOTH-IBEA.LE.0 ) THEN                             KXL7MI90
                   IPVNU(1,ITR-IBEA)=1                                  KXL7MI91
                ELSE                                                    KXL7MI92
                   IPVNU(1,ITR-IBEA)=IPVNU(2,IMOTH-IBEA)                KXL7MI93
                ENDIF                                                   KXL7MI94
                IPVNU(2,ITR-IBEA)=0                                     KXL7MI95
             ELSE IF ( KS.GE.11 .AND. KS.LE.15 ) THEN                   KXL7MI96
C            Particle has decayed in LUND                               KXL7MI97
                 IF (IMOTH-IBEA.LE.0 ) THEN                             KXL7MI98
C               Primary parton                                          KXL7MI99
                   IPVNU(1,ITR-IBEA)=1                                  KXL7M100
                ELSE                                                    KXL7M101
                   IPVNU(1,ITR-IBEA)=IPVNU(2,IMOTH-IBEA)                KXL7M102
                ENDIF                                                   KXL7M103
C               Decay inside LUND and finite lifetime :                 KXL7M104
C               this track will be propagated in KFMIBK until its decay KXL7M105
                TLIF = TIMLIF (IPART)                                   KXL7M106
C                                                                       KXL7M107
C In case of mixing  , do not propagate the original B                  KXL7M108
C                                                                       KXL7M109
                IF(KB7MIX(ITR).EQ.1)TLIF=0.                             KXL7M110
                IF (TLIF.GT.TLIMI .AND. MDCY(LUCOMP(ILUN),1).GT.0) THEN KXL7M111
                   NVER=NVER+1                                          KXL7M112
                   IPVNU(2,ITR-IBEA)=NVER                               KXL7M113
                ELSE                                                    KXL7M114
C               Decay is immediate ( will not be propagated)            KXL7M115
                   IPVNU(2,ITR-IBEA)=IPVNU(1,ITR-IBEA)                  KXL7M116
                ENDIF                                                   KXL7M117
             ELSE IF (KS.EQ.21) THEN                                    KXL7M118
C            electron beams were stored as well                         KXL7M119
C            check that they appear only on lines 1 or 2                KXL7M120
                ILUN=-4                                                 KXL7M121
                IF (ITR.GT.2) GO TO 998                                 KXL7M122
                IST=KBKINE(-ITR,PTRAK(1,ITR-IBEA),IPART,0)              KXL7M123
                IF (IST.LE.0) THEN                                      KXL7M124
                   ILUN=-2                                              KXL7M125
                   GO TO 998                                            KXL7M126
                ENDIF                                                   KXL7M127
                IBEA=IBEA+1                                             KXL7M128
             ELSE IF (KS.GE.30) THEN                                    KXL7M129
                ILUN=-5                                                 KXL7M130
                GO TO 998                                               KXL7M131
             ENDIF                                                      KXL7M132
C                                                                       KXL7M133
C         Update history code                                           KXL7M134
          IF (IMOTH.GT.IBEA) THEN                                       KXL7M135
             IPCOD(ITR-IBEA)=IPCOD(ITR-IBEA)-IBEA                       KXL7M136
           ELSE                                                         KXL7M137
             IPCOD(ITR-IBEA)=IPCOD(ITR-IBEA)-IMOTH                      KXL7M138
           ENDIF                                                        KXL7M139
 10    CONTINUE                                                         KXL7M140
C                                                                       KXL7M141
       NPARL = N7LU-IBEA                                                KXL7M142
C - Fill history bank KHIS                                              KXL7M143
       JKHIS = ALTABL ('KHIS',1,NPARL,IPCOD,'I','E')                    KXL7M144
C - Propagate decays and fill KINE and VERT banks                       KXL7M145
       CALL KFMIBK(VMAIN,PTRAK,IPVNU,NPARL,IFAIL)                       KXL7M146
C                                                                       KXL7M147
       MVX = NVER                                                       KXL7M148
       MTRK = NPARL                                                     KXL7M149
       ISTAT = IFAIL                                                    KXL7M150
       GOTO 999                                                         KXL7M151
C                                                                       KXL7M152
C - Error                                                               KXL7M153
C      unknown LUND particle                                            KXL7M154
 998   ISTAT = ILUN                                                     KXL7M155
C                                                                       KXL7M156
 999   RETURN                                                           KXL7M157
       END                                                              KXL7M158
      SUBROUTINE LUGENE(IFL,ECM,IDPR)                                   LUGENE 2
C------------------------------------------------------------------     LUGENE 3
C! Generate LUND event, fill some histos   JETSET 7.3 version           LUGENE 4
C    IFL : FLAVOR CODE TO BE GENERATED                                  LUGENE 5
C    ECM : ENERGY C.M. AVAILABLE                                        LUGENE 6
C    IDPR: RETURNED PROCESS CODE (1-5)                                  LUGENE 7
C------------------------------------------------------------------     LUGENE 8
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
C                                                                       LUN7CO12
      DIMENSION KFL(8)                                                  LUGENE10
      DATA NIT/0/                                                       LUGENE11
      NIT = NIT +1                                                      LUGENE12
      CALL LUEEVT(IFL,ECM)                                              LUGENE13
      IF (NIT.LE.5) CALL LULIST(1)                                      LUGENE14
C                                                                       LUGENE44
C   Update IDPR                                                         LUGENE45
C                                                                       LUGENE46
        IDPR = 0                                                        LUGENE47
C Look for flavor generated                                             LUGENE48
        DO 5 I=1,8                                                      LUGENE49
 5      KFL(I)=0                                                        LUGENE50
        NFL=0                                                           LUGENE51
        DO 40 I=1,N7LU                                                  LUGENE52
           ITYP=ABS(KLU(I,9))                                           LUGENE53
           IF (ITYP.GT.8 .OR. ITYP.EQ.0) GO TO 40                       LUGENE54
           IF ( NFL.GT.0) THEN                                          LUGENE55
              DO 41 J=1,NFL                                             LUGENE56
              IF (ITYP.EQ.KFL(J)) GO TO 40                              LUGENE57
  41          CONTINUE                                                  LUGENE58
           ENDIF                                                        LUGENE59
           NFL=NFL+1                                                    LUGENE60
           KFL(NFL)=ITYP                                                LUGENE61
           IDPR=10*IDPR+ITYP                                            LUGENE62
  40    CONTINUE                                                        LUGENE63
        EE=FLOAT(IDPR)                                                  BBL00334
        CALL HFILL(10006,EE,DUM,1.)                                     BBL00335
        WEI = 1.                                                        BBL00336
        RETURN                                                          LUGENE64
        END                                                             LUGENE65
      SUBROUTINE LUKFDI(KFL1,KFL2,KFL3,KF)                              LUKFDI 2
C...Purpose: to generate a new flavour pair and combine off a hadron.   LUKFDI 3
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)             LUKFDI 4
      COMMON/PARJ17/P17(5)                                              PARJ17 2
C      fraction of T/V production for flavor 1...5  from GP17 card      PARJ17 3
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)        LUKFDI 6
      SAVE /LUDAT1/,/LUDAT2/                                            LUKFDI 7
      SAVE IFIR                                                         BBL00822
      DATA   IFIR/0/                                                    BBL00823
C...Default flavour values. Input consistency checks.                   LUKFDI 8
      IF ( IFIR.LE.0) THEN                                              BBL00824
         IFIR = 1                                                       BBL00825
         PRINT * , ' +++ LUKFDI : MODIFIED VERSION USED  +++'           BBL00826
      ENDIF                                                             BBL00827
      KF1A=IABS(KFL1)                                                   LUKFDI 9
      KF2A=IABS(KFL2)                                                   LUKFDI10
      KFL3=0                                                            LUKFDI11
      KF=0                                                              LUKFDI12
      IF(KF1A.EQ.0) RETURN                                              LUKFDI13
      IF(KF2A.NE.0) THEN                                                LUKFDI14
        IF(KF1A.LE.10.AND.KF2A.LE.10.AND.KFL1*KFL2.GT.0) RETURN         LUKFDI15
        IF(KF1A.GT.10.AND.KF2A.GT.10) RETURN                            LUKFDI16
        IF((KF1A.GT.10.OR.KF2A.GT.10).AND.KFL1*KFL2.LT.0) RETURN        LUKFDI17
      ENDIF                                                             LUKFDI18
C...Check if tabulated flavour probabilities are to be used.            LUKFDI19
      IF(MSTJ(15).EQ.1) THEN                                            LUKFDI20
        KTAB1=-1                                                        LUKFDI21
        IF(KF1A.GE.1.AND.KF1A.LE.6) KTAB1=KF1A                          LUKFDI22
        KFL1A=MOD(KF1A/1000,10)                                         LUKFDI23
        KFL1B=MOD(KF1A/100,10)                                          LUKFDI24
        KFL1S=MOD(KF1A,10)                                              LUKFDI25
        IF(KFL1A.GE.1.AND.KFL1A.LE.4.AND.KFL1B.GE.1.AND.KFL1B.LE.4)     LUKFDI26
     &  KTAB1=6+KFL1A*(KFL1A-2)+2*KFL1B+(KFL1S-1)/2                     LUKFDI27
        IF(KFL1A.GE.1.AND.KFL1A.LE.4.AND.KFL1A.EQ.KFL1B) KTAB1=KTAB1-1  LUKFDI28
        IF(KF1A.GE.1.AND.KF1A.LE.6) KFL1A=KF1A                          LUKFDI29
        KTAB2=0                                                         LUKFDI30
        IF(KF2A.NE.0) THEN                                              LUKFDI31
          KTAB2=-1                                                      LUKFDI32
          IF(KF2A.GE.1.AND.KF2A.LE.6) KTAB2=KF2A                        LUKFDI33
          KFL2A=MOD(KF2A/1000,10)                                       LUKFDI34
          KFL2B=MOD(KF2A/100,10)                                        LUKFDI35
          KFL2S=MOD(KF2A,10)                                            LUKFDI36
          IF(KFL2A.GE.1.AND.KFL2A.LE.4.AND.KFL2B.GE.1.AND.KFL2B.LE.4)   LUKFDI37
     &    KTAB2=6+KFL2A*(KFL2A-2)+2*KFL2B+(KFL2S-1)/2                   LUKFDI38
          IF(KFL2A.GE.1.AND.KFL2A.LE.4.AND.KFL2A.EQ.KFL2B) KTAB2=KTAB2-1LUKFDI39
        ENDIF                                                           LUKFDI40
        IF(KTAB1.GE.0.AND.KTAB2.GE.0) GOTO 140                          LUKFDI41
      ENDIF                                                             LUKFDI42
C...Parameters and breaking diquark parameter combinations.             LUKFDI43
  100 PAR2=PARJ(2)                                                      LUKFDI44
      PAR3=PARJ(3)                                                      LUKFDI45
      PAR4=3.*PARJ(4)                                                   LUKFDI46
      IF(MSTJ(12).GE.2) THEN                                            LUKFDI47
        PAR3M=SQRT(PARJ(3))                                             LUKFDI48
        PAR4M=1./(3.*SQRT(PARJ(4)))                                     LUKFDI49
        PARDM=PARJ(7)/(PARJ(7)+PAR3M*PARJ(6))                           LUKFDI50
        PARS0=PARJ(5)*(2.+(1.+PAR2*PAR3M*PARJ(7))*(1.+PAR4M))           LUKFDI51
        PARS1=PARJ(7)*PARS0/(2.*PAR3M)+PARJ(5)*(PARJ(6)*(1.+PAR4M)+     LUKFDI52
     &  PAR2*PAR3M*PARJ(6)*PARJ(7))                                     LUKFDI53
        PARS2=PARJ(5)*2.*PARJ(6)*PARJ(7)*(PAR2*PARJ(7)+(1.+PAR4M)/PAR3M)LUKFDI54
        PARSM=MAX(PARS0,PARS1,PARS2)                                    LUKFDI55
        PAR4=PAR4*(1.+PARSM)/(1.+PARSM/(3.*PAR4M))                      LUKFDI56
      ENDIF                                                             LUKFDI57
C...Choice of whether to generate meson or baryon.                      LUKFDI58
  105 MBARY=0                                                           LUKFDI59
      KFDA=0                                                            LUKFDI60
      IF(KF1A.LE.10) THEN                                               LUKFDI61
        IF(KF2A.EQ.0.AND.MSTJ(12).GE.1.AND.(1.+PARJ(1))*RLU(0).GT.1.)   LUKFDI62
     &  MBARY=1                                                         LUKFDI63
        IF(KF2A.GT.10) MBARY=2                                          LUKFDI64
        IF(KF2A.GT.10.AND.KF2A.LE.10000) KFDA=KF2A                      LUKFDI65
      ELSE                                                              LUKFDI66
        MBARY=2                                                         LUKFDI67
        IF(KF1A.LE.10000) KFDA=KF1A                                     LUKFDI68
      ENDIF                                                             LUKFDI69
C...Possibility of process diquark -> meson + new diquark.              LUKFDI70
      IF(KFDA.NE.0.AND.MSTJ(12).GE.2) THEN                              LUKFDI71
        KFLDA=MOD(KFDA/1000,10)                                         LUKFDI72
        KFLDB=MOD(KFDA/100,10)                                          LUKFDI73
        KFLDS=MOD(KFDA,10)                                              LUKFDI74
        WTDQ=PARS0                                                      LUKFDI75
        IF(MAX(KFLDA,KFLDB).EQ.3) WTDQ=PARS1                            LUKFDI76
        IF(MIN(KFLDA,KFLDB).EQ.3) WTDQ=PARS2                            LUKFDI77
        IF(KFLDS.EQ.1) WTDQ=WTDQ/(3.*PAR4M)                             LUKFDI78
        IF((1.+WTDQ)*RLU(0).GT.1.) MBARY=-1                             LUKFDI79
        IF(MBARY.EQ.-1.AND.KF2A.NE.0) RETURN                            LUKFDI80
      ENDIF                                                             LUKFDI81
C...Flavour for meson, possibly with new flavour.                       LUKFDI82
      IF(MBARY.LE.0) THEN                                               LUKFDI83
        KFS=ISIGN(1,KFL1)                                               LUKFDI84
        IF(MBARY.EQ.0) THEN                                             LUKFDI85
          IF(KF2A.EQ.0) KFL3=ISIGN(1+INT((2.+PAR2)*RLU(0)),-KFL1)       LUKFDI86
          KFLA=MAX(KF1A,KF2A+IABS(KFL3))                                LUKFDI87
          KFLB=MIN(KF1A,KF2A+IABS(KFL3))                                LUKFDI88
          IF(KFLA.NE.KF1A) KFS=-KFS                                     LUKFDI89
C...Splitting of diquark into meson plus new diquark.                   LUKFDI90
        ELSE                                                            LUKFDI91
          KFL1A=MOD(KF1A/1000,10)                                       LUKFDI92
          KFL1B=MOD(KF1A/100,10)                                        LUKFDI93
  110     KFL1D=KFL1A+INT(RLU(0)+0.5)*(KFL1B-KFL1A)                     LUKFDI94
          KFL1E=KFL1A+KFL1B-KFL1D                                       LUKFDI95
          IF((KFL1D.EQ.3.AND.RLU(0).GT.PARDM).OR.(KFL1E.EQ.3.AND.       LUKFDI96
     &    RLU(0).LT.PARDM)) THEN                                        LUKFDI97
            KFL1D=KFL1A+KFL1B-KFL1D                                     LUKFDI98
            KFL1E=KFL1A+KFL1B-KFL1E                                     LUKFDI99
          ENDIF                                                         LUKFD100
          KFL3A=1+INT((2.+PAR2*PAR3M*PARJ(7))*RLU(0))                   LUKFD101
          IF((KFL1E.NE.KFL3A.AND.RLU(0).GT.(1.+PAR4M)/MAX(2.,1.+PAR4M)).LUKFD102
     &    OR.(KFL1E.EQ.KFL3A.AND.RLU(0).GT.2./MAX(2.,1.+PAR4M)))        LUKFD103
     &    GOTO 110                                                      LUKFD104
          KFLDS=3                                                       LUKFD105
          IF(KFL1E.NE.KFL3A) KFLDS=2*INT(RLU(0)+1./(1.+PAR4M))+1        LUKFD106
          KFL3=ISIGN(10000+1000*MAX(KFL1E,KFL3A)+100*MIN(KFL1E,KFL3A)+  LUKFD107
     &    KFLDS,-KFL1)                                                  LUKFD108
          KFLA=MAX(KFL1D,KFL3A)                                         LUKFD109
          KFLB=MIN(KFL1D,KFL3A)                                         LUKFD110
          IF(KFLA.NE.KFL1D) KFS=-KFS                                    LUKFD111
        ENDIF                                                           LUKFD112
C...Form meson, with spin and flavour mixing for diagonal states.       LUKFD113
CC HVFL ------------------------------                                  LUKFD114
        IF ( KFLA.LE.5) THEN                                            LUKFD115
           PARJ17 = PARJ(17)                                            LUKFD116
           PARJ(17) = P17(KFLA)                                         LUKFD117
        ENDIF                                                           LUKFD118
CC HVFL ------------------------------                                  LUKFD119
        IF(KFLA.LE.2) KMUL=INT(PARJ(11)+RLU(0))                         LUKFD120
        IF(KFLA.EQ.3) KMUL=INT(PARJ(12)+RLU(0))                         LUKFD121
        IF(KFLA.GE.4) KMUL=INT(PARJ(13)+RLU(0))                         LUKFD122
        IF(KMUL.EQ.0.AND.PARJ(14).GT.0.) THEN                           LUKFD123
          IF(RLU(0).LT.PARJ(14)) KMUL=2                                 LUKFD124
        ELSEIF(KMUL.EQ.1.AND.PARJ(15)+PARJ(16)+PARJ(17).GT.0.) THEN     LUKFD125
          RMUL=RLU(0)                                                   LUKFD126
          IF(RMUL.LT.PARJ(15)) KMUL=3                                   LUKFD127
          IF(KMUL.EQ.1.AND.RMUL.LT.PARJ(15)+PARJ(16)) KMUL=4            LUKFD128
          IF(KMUL.EQ.1.AND.RMUL.LT.PARJ(15)+PARJ(16)+PARJ(17)) KMUL=5   LUKFD129
        ENDIF                                                           LUKFD130
CC HVFL ------------------------------                                  LUKFD131
        IF ( KFLA.LE.5) THEN                                            LUKFD132
           PARJ(17) = PARJ17                                            LUKFD133
        ENDIF                                                           LUKFD134
CC HVFL ------------------------------                                  LUKFD135
        KFLS=3                                                          LUKFD136
        IF(KMUL.EQ.0.OR.KMUL.EQ.3) KFLS=1                               LUKFD137
        IF(KMUL.EQ.5) KFLS=5                                            LUKFD138
        IF(KFLA.NE.KFLB) THEN                                           LUKFD139
          KF=(100*KFLA+10*KFLB+KFLS)*KFS*(-1)**KFLA                     LUKFD140
        ELSE                                                            LUKFD141
          RMIX=RLU(0)                                                   LUKFD142
          IMIX=2*KFLA+10*KMUL                                           LUKFD143
          IF(KFLA.LE.3) KF=110*(1+INT(RMIX+PARF(IMIX-1))+               LUKFD144
     &    INT(RMIX+PARF(IMIX)))+KFLS                                    LUKFD145
          IF(KFLA.GE.4) KF=110*KFLA+KFLS                                LUKFD146
        ENDIF                                                           LUKFD147
        IF(KMUL.EQ.2.OR.KMUL.EQ.3) KF=KF+ISIGN(10000,KF)                LUKFD148
        IF(KMUL.EQ.4) KF=KF+ISIGN(20000,KF)                             LUKFD149
C...Optional extra suppression of eta and eta'.                         LUKFD150
        IF(KF.EQ.221) THEN                                              LUKFD151
          IF(RLU(0).GT.PARJ(25)) GOTO 105                               LUKFD152
        ELSEIF(KF.EQ.331) THEN                                          LUKFD153
          IF(RLU(0).GT.PARJ(26)) GOTO 105                               LUKFD154
        ENDIF                                                           LUKFD155
C...Generate diquark flavour.                                           LUKFD156
      ELSE                                                              LUKFD157
  120   IF(KF1A.LE.10.AND.KF2A.EQ.0) THEN                               LUKFD158
          KFLA=KF1A                                                     LUKFD159
  130     KFLB=1+INT((2.+PAR2*PAR3)*RLU(0))                             LUKFD160
          KFLC=1+INT((2.+PAR2*PAR3)*RLU(0))                             LUKFD161
          KFLDS=1                                                       LUKFD162
          IF(KFLB.GE.KFLC) KFLDS=3                                      LUKFD163
          IF(KFLDS.EQ.1.AND.PAR4*RLU(0).GT.1.) GOTO 130                 LUKFD164
          IF(KFLDS.EQ.3.AND.PAR4.LT.RLU(0)) GOTO 130                    LUKFD165
          KFL3=ISIGN(1000*MAX(KFLB,KFLC)+100*MIN(KFLB,KFLC)+KFLDS,KFL1) LUKFD166
C...Take diquark flavour from input.                                    LUKFD167
        ELSEIF(KF1A.LE.10) THEN                                         LUKFD168
          KFLA=KF1A                                                     LUKFD169
          KFLB=MOD(KF2A/1000,10)                                        LUKFD170
          KFLC=MOD(KF2A/100,10)                                         LUKFD171
          KFLDS=MOD(KF2A,10)                                            LUKFD172
C...Generate (or take from input) quark to go with diquark.             LUKFD173
        ELSE                                                            LUKFD174
          IF(KF2A.EQ.0) KFL3=ISIGN(1+INT((2.+PAR2)*RLU(0)),KFL1)        LUKFD175
          KFLA=KF2A+IABS(KFL3)                                          LUKFD176
          KFLB=MOD(KF1A/1000,10)                                        LUKFD177
          KFLC=MOD(KF1A/100,10)                                         LUKFD178
          KFLDS=MOD(KF1A,10)                                            LUKFD179
        ENDIF                                                           LUKFD180
C...SU(6) factors for formation of baryon. Try again if fails.          LUKFD181
        KBARY=KFLDS                                                     LUKFD182
        IF(KFLDS.EQ.3.AND.KFLB.NE.KFLC) KBARY=5                         LUKFD183
        IF(KFLA.NE.KFLB.AND.KFLA.NE.KFLC) KBARY=KBARY+1                 LUKFD184
        WT=PARF(60+KBARY)+PARJ(18)*PARF(70+KBARY)                       LUKFD185
        IF(MBARY.EQ.1.AND.MSTJ(12).GE.2) THEN                           LUKFD186
          WTDQ=PARS0                                                    LUKFD187
          IF(MAX(KFLB,KFLC).EQ.3) WTDQ=PARS1                            LUKFD188
          IF(MIN(KFLB,KFLC).EQ.3) WTDQ=PARS2                            LUKFD189
          IF(KFLDS.EQ.1) WTDQ=WTDQ/(3.*PAR4M)                           LUKFD190
          IF(KFLDS.EQ.1) WT=WT*(1.+WTDQ)/(1.+PARSM/(3.*PAR4M))          LUKFD191
          IF(KFLDS.EQ.3) WT=WT*(1.+WTDQ)/(1.+PARSM)                     LUKFD192
        ENDIF                                                           LUKFD193
        IF(KF2A.EQ.0.AND.WT.LT.RLU(0)) GOTO 120                         LUKFD194
C...Form baryon. Distinguish Lambda- and Sigmalike baryons.             LUKFD195
        KFLD=MAX(KFLA,KFLB,KFLC)                                        LUKFD196
        KFLF=MIN(KFLA,KFLB,KFLC)                                        LUKFD197
        KFLE=KFLA+KFLB+KFLC-KFLD-KFLF                                   LUKFD198
        KFLS=2                                                          LUKFD199
        IF((PARF(60+KBARY)+PARJ(18)*PARF(70+KBARY))*RLU(0).GT.          LUKFD200
     &  PARF(60+KBARY)) KFLS=4                                          LUKFD201
        KFLL=0                                                          LUKFD202
        IF(KFLS.EQ.2.AND.KFLD.GT.KFLE.AND.KFLE.GT.KFLF) THEN            LUKFD203
          IF(KFLDS.EQ.1.AND.KFLA.EQ.KFLD) KFLL=1                        LUKFD204
          IF(KFLDS.EQ.1.AND.KFLA.NE.KFLD) KFLL=INT(0.25+RLU(0))         LUKFD205
          IF(KFLDS.EQ.3.AND.KFLA.NE.KFLD) KFLL=INT(0.75+RLU(0))         LUKFD206
        ENDIF                                                           LUKFD207
        IF(KFLL.EQ.0) KF=ISIGN(1000*KFLD+100*KFLE+10*KFLF+KFLS,KFL1)    LUKFD208
        IF(KFLL.EQ.1) KF=ISIGN(1000*KFLD+100*KFLF+10*KFLE+KFLS,KFL1)    LUKFD209
      ENDIF                                                             LUKFD210
      RETURN                                                            LUKFD211
C...Use tabulated probabilities to select new flavour and hadron.       LUKFD212
  140 IF(KTAB2.EQ.0.AND.MSTJ(12).LE.0) THEN                             LUKFD213
        KT3L=1                                                          LUKFD214
        KT3U=6                                                          LUKFD215
      ELSEIF(KTAB2.EQ.0.AND.KTAB1.GE.7.AND.MSTJ(12).LE.1) THEN          LUKFD216
        KT3L=1                                                          LUKFD217
        KT3U=6                                                          LUKFD218
      ELSEIF(KTAB2.EQ.0) THEN                                           LUKFD219
        KT3L=1                                                          LUKFD220
        KT3U=22                                                         LUKFD221
      ELSE                                                              LUKFD222
        KT3L=KTAB2                                                      LUKFD223
        KT3U=KTAB2                                                      LUKFD224
      ENDIF                                                             LUKFD225
      RFL=0.                                                            LUKFD226
      DO 150 KTS=0,2                                                    LUKFD227
      DO 150 KT3=KT3L,KT3U                                              LUKFD228
      RFL=RFL+PARF(120+80*KTAB1+25*KTS+KT3)                             LUKFD229
  150 CONTINUE                                                          LUKFD230
      RFL=RLU(0)*RFL                                                    LUKFD231
      DO 160 KTS=0,2                                                    LUKFD232
      KTABS=KTS                                                         LUKFD233
      DO 160 KT3=KT3L,KT3U                                              LUKFD234
      KTAB3=KT3                                                         LUKFD235
      RFL=RFL-PARF(120+80*KTAB1+25*KTS+KT3)                             LUKFD236
  160 IF(RFL.LE.0.) GOTO 170                                            LUKFD237
  170 CONTINUE                                                          LUKFD238
C...Reconstruct flavour of produced quark/diquark.                      LUKFD239
      IF(KTAB3.LE.6) THEN                                               LUKFD240
        KFL3A=KTAB3                                                     LUKFD241
        KFL3B=0                                                         LUKFD242
        KFL3=ISIGN(KFL3A,KFL1*(2*KTAB1-13))                             LUKFD243
      ELSE                                                              LUKFD244
        KFL3A=1                                                         LUKFD245
        IF(KTAB3.GE.8) KFL3A=2                                          LUKFD246
        IF(KTAB3.GE.11) KFL3A=3                                         LUKFD247
        IF(KTAB3.GE.16) KFL3A=4                                         LUKFD248
        KFL3B=(KTAB3-6-KFL3A*(KFL3A-2))/2                               LUKFD249
        KFL3=1000*KFL3A+100*KFL3B+1                                     LUKFD250
        IF(KFL3A.EQ.KFL3B.OR.KTAB3.NE.6+KFL3A*(KFL3A-2)+2*KFL3B) KFL3=  LUKFD251
     &  KFL3+2                                                          LUKFD252
        KFL3=ISIGN(KFL3,KFL1*(13-2*KTAB1))                              LUKFD253
      ENDIF                                                             LUKFD254
C...Reconstruct meson code.                                             LUKFD255
      IF(KFL3A.EQ.KFL1A.AND.KFL3B.EQ.KFL1B.AND.(KFL3A.LE.3.OR.          LUKFD256
     &KFL3B.NE.0)) THEN                                                 LUKFD257
        RFL=RLU(0)*(PARF(143+80*KTAB1+25*KTABS)+PARF(144+80*KTAB1+      LUKFD258
     &  25*KTABS)+PARF(145+80*KTAB1+25*KTABS))                          LUKFD259
        KF=110+2*KTABS+1                                                LUKFD260
        IF(RFL.GT.PARF(143+80*KTAB1+25*KTABS)) KF=220+2*KTABS+1         LUKFD261
        IF(RFL.GT.PARF(143+80*KTAB1+25*KTABS)+PARF(144+80*KTAB1+        LUKFD262
     &  25*KTABS)) KF=330+2*KTABS+1                                     LUKFD263
      ELSEIF(KTAB1.LE.6.AND.KTAB3.LE.6) THEN                            LUKFD264
        KFLA=MAX(KTAB1,KTAB3)                                           LUKFD265
        KFLB=MIN(KTAB1,KTAB3)                                           LUKFD266
        KFS=ISIGN(1,KFL1)                                               LUKFD267
        IF(KFLA.NE.KF1A) KFS=-KFS                                       LUKFD268
        KF=(100*KFLA+10*KFLB+2*KTABS+1)*KFS*(-1)**KFLA                  LUKFD269
      ELSEIF(KTAB1.GE.7.AND.KTAB3.GE.7) THEN                            LUKFD270
        KFS=ISIGN(1,KFL1)                                               LUKFD271
        IF(KFL1A.EQ.KFL3A) THEN                                         LUKFD272
          KFLA=MAX(KFL1B,KFL3B)                                         LUKFD273
          KFLB=MIN(KFL1B,KFL3B)                                         LUKFD274
          IF(KFLA.NE.KFL1B) KFS=-KFS                                    LUKFD275
        ELSEIF(KFL1A.EQ.KFL3B) THEN                                     LUKFD276
          KFLA=KFL3A                                                    LUKFD277
          KFLB=KFL1B                                                    LUKFD278
          KFS=-KFS                                                      LUKFD279
        ELSEIF(KFL1B.EQ.KFL3A) THEN                                     LUKFD280
          KFLA=KFL1A                                                    LUKFD281
          KFLB=KFL3B                                                    LUKFD282
        ELSEIF(KFL1B.EQ.KFL3B) THEN                                     LUKFD283
          KFLA=MAX(KFL1A,KFL3A)                                         LUKFD284
          KFLB=MIN(KFL1A,KFL3A)                                         LUKFD285
          IF(KFLA.NE.KFL1A) KFS=-KFS                                    LUKFD286
        ELSE                                                            LUKFD287
          CALL LUERRM(2,'(LUKFDI:) no matching flavours for qq -> qq')  LUKFD288
          GOTO 100                                                      LUKFD289
        ENDIF                                                           LUKFD290
        KF=(100*KFLA+10*KFLB+2*KTABS+1)*KFS*(-1)**KFLA                  LUKFD291
C...Reconstruct baryon code.                                            LUKFD292
      ELSE                                                              LUKFD293
        IF(KTAB1.GE.7) THEN                                             LUKFD294
          KFLA=KFL3A                                                    LUKFD295
          KFLB=KFL1A                                                    LUKFD296
          KFLC=KFL1B                                                    LUKFD297
        ELSE                                                            LUKFD298
          KFLA=KFL1A                                                    LUKFD299
          KFLB=KFL3A                                                    LUKFD300
          KFLC=KFL3B                                                    LUKFD301
        ENDIF                                                           LUKFD302
        KFLD=MAX(KFLA,KFLB,KFLC)                                        LUKFD303
        KFLF=MIN(KFLA,KFLB,KFLC)                                        LUKFD304
        KFLE=KFLA+KFLB+KFLC-KFLD-KFLF                                   LUKFD305
        IF(KTABS.EQ.0) KF=ISIGN(1000*KFLD+100*KFLF+10*KFLE+2,KFL1)      LUKFD306
        IF(KTABS.GE.1) KF=ISIGN(1000*KFLD+100*KFLE+10*KFLF+2*KTABS,KFL1)LUKFD307
      ENDIF                                                             LUKFD308
C...Check that constructed flavour code is an allowed one.              LUKFD309
      IF(KFL2.NE.0) KFL3=0                                              LUKFD310
      KC=LUCOMP(KF)                                                     LUKFD311
      IF(KC.EQ.0) THEN                                                  LUKFD312
        CALL LUERRM(2,'(LUKFDI:) user-defined flavour probabilities '// LUKFD313
     &  'failed')                                                       LUKFD314
        GOTO 100                                                        LUKFD315
      ENDIF                                                             LUKFD316
      RETURN                                                            LUKFD317
      END                                                               LUKFD318
      SUBROUTINE LULAMB                                                 LULAMB 2
C---------------------------------------------------------------------- LULAMB 3
C     B.Bloch   : November 1993 inspired by code from Sjostrand         LULAMB 4
C! Apply sipin effects and/or form factor effect to semi leptonic       LULAMB 5
C! decays of B Baryons into Lambdac lepton neutrino                     LULAMB 6
C---------------------------------------------------------------------- LULAMB 7
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
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
C                                                                       LUN7CO12
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12         ALCONS 2
      REAL RADEG, DEGRA                                                 ALCONS 3
      REAL CLGHT                                                        ALCONS 4
      INTEGER NBITW, NBYTW, LCHAR                                       ALCONS 5
      PARAMETER (PI=3.141592653589)                                     ALCONS 6
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)                          ALCONS 7
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          ALCONS 8
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         ALCONS 9
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         ALCONS10
      PARAMETER (CLGHT = 29.9792458)                                    ALCONS11
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)              ALCONS12
C                                                                       ALCONS13
      PARAMETER (LBPOS=100,NBU=521,NBD=511,NBS=531,NBC=541)             BCODES 2
      PARAMETER (NLB=5122,NXB= 5132)                                    BBL00511
C     LBPOS  = internal JETSET 7.3 code for B meson                     BCODES 3
      PARAMETER (NXB0=5232,NOB= 5332)                                   BBL007 9
C     NBU    = internal JETSET 7.3 code for Bu meson                    BCODES 4
C     NBD    = internal JETSET 7.3 code for Bd meson                    BCODES 5
C     NBS    = internal JETSET 7.3 code for Bs meson                    BCODES 6
C     NBC    = internal JETSET 7.3 code for Bc meson                    BCODES 7
C     NLB    = internal JETSET 7.3 code for /\B BARYON                  BBL00512
C     NXB    = internal JETSET 7.3 code for XIB BARYON                  BBL00513
C     NXB0   = internal JETSET 7.3 code for XIB0 BARYON                 BBL00710
C     NOB    = internal JETSET 7.3 code for OMEGAB BARYON               BBL00711
      PARAMETER (NDU=421,NDD=411,NDS=431)                               DCODES 2
      PARAMETER (NLC=4122,NXC= 4132)                                    DCODES 3
C     NDU    = internal JETSET 7.3 code for Du meson                    DCODES 4
C     NDD    = internal JETSET 7.3 code for Dd meson                    DCODES 5
C     NDS    = internal JETSET 7.3 code for Ds meson                    DCODES 6
C     NLC    = internal JETSET 7.3 code for /\C BARYON                  DCODES 7
C     NXC    = internal JETSET 7.3 code for XIC BARYON                  DCODES 8
      PARAMETER (NNUEL=12,NNUMU=14,NNUTO=16,NEL=11,NMU=13,NTO=15)       LCODES 2
C     NNUEL  = internal JETSET 7.3 code for neutrino electron           LCODES 3
C     NNUMU  = internal JETSET 7.3 code for neutrino mu                 LCODES 4
C     NNUTO  = internal JETSET 7.3 code for neutrino tau                LCODES 5
C     NEL    = internal JETSET 7.3 code for electron                    LCODES 6
C     NMU    = internal JETSET 7.3 code for muon                        LCODES 7
C     NTO    = internal JETSET 7.3 code for tau                         LCODES 8
      COMMON / MASTER / IGENE,IWEIT,IMIX,IVBU,IFL,ECM,IEV1,IEV2,IEFLOP, MASTER 2
     $          ISEM,IDEC,WTMAX,SVRT(3),ICOULU(10),IPHO,ILAM,IP17,IDDC  BBL00914
      COMMON/POLAMB/ISPINL,IFORML,W0LAMB                                POLAMB 2
C ISPINL : Switch on/off spin effects in semileptonic B_Baryons decays  POLAMB 3
C IFORML : Switch on/off form factor  in semileptonic B_Baryons decays  POLAMB 4
C W0LAMB : form factor value if used                                    POLAMB 5
      PARAMETER (NBRMX=20)                                              LULAMB16
      DIMENSION MDMSTO(NBRMX)                                           LULAMB17
      SAVE MDMSTO                                                       LULAMB18
C...Loop over event to find all Lambda_b.                               LULAMB19
      NLAMB=0                                                           LULAMB20
      IDCK =0                                                           LULAMB21
      DO 130 I=1,N7LU                                                   LULAMB22
         KF=ABS(K7LU(I,2))                                              LULAMB23
C  We have b baryon /\b,Xb or Omegab                                    LULAMB24
         IF ((KF.EQ.NLB).OR.(KF.EQ.NXB).OR.(KF.EQ.NXB0).OR.(KF.EQ.NOB)) LULAMB25
     $    THEN                                                          LULAMB26
C... Allow the decay                                                    LULAMB27
         KC = LUCOMP(KF)                                                LULAMB28
         MDCY(KC,1) = 1                                                 LULAMB29
C...Store orginal possible decay schemes                                LULAMB30
         NMBR = MDCY(85,3)                                              LULAMB31
         IF (NMBR.GT.NBRMX)THEN                                         LULAMB32
            WRITE(6,'('' YOU SHOULD ENLARGE NBRMX IN LULAMB TO '',I10)')LULAMB33
     $      NMBR                                                        LULAMB34
            CALL EXIT                                                   LULAMB35
         ENDIF                                                          LULAMB36
         DO 22 II= 1,NMBR                                               LULAMB37
           MDMSTO(II) = MDME(MDCY(85,2)+II-1,1)                         LULAMB38
   22    CONTINUE                                                       LULAMB39
         XLB=2.*P7LU(I,4)/ECM                                           LULAMB40
C...Rotate Lambda_b to sit along z axis and then boost back to rest.    LULAMB41
         PHILB=ULANGL(P7LU(I,1),P7LU(I,2))                              LULAMB42
         CALL LUDBRB(I,I,0.,-PHILB,0D0,0D0,0D0)                         LULAMB43
         THELB=ULANGL(P7LU(I,3),P7LU(I,1))                              LULAMB44
         CALL LUDBRB(I,I,-THELB,0.,0D0,0D0,0D0)                         LULAMB45
         BEZLB=P7LU(I,3)/P7LU(I,4)                                      LULAMB46
         CALL LUDBRB(I,I,0.,0.,0D0,0D0,-DBLE(BEZLB))                    LULAMB47
C...Select z component of b spin in its rest frame.                     LULAMB48
       IF(ISPINL.EQ.1) THEN                                             LULAMB49
C...Construct propagators.                                              LULAMB50
          SFF=1./(16.*PARU(102)*(1.-PARU(102)))                         LULAMB51
          SFW=ECM**4/((ECM**2-PARJ(123)**2)**2+(PARJ(123)*PARJ(124))**2)LULAMB52
          SFI=SFW*(1.-(PARJ(123)/ECM)**2)                               LULAMB53
          QE=-1.                                                        LULAMB54
          AE=-1.                                                        LULAMB55
          VE=-1.+4.*PARU(102)                                           LULAMB56
          QF=-1./3.                                                     LULAMB57
          AF=-1.                                                        LULAMB58
          VF=-1.+(4./3.)*PARU(102)                                      LULAMB59
          F0=QE**2*QF**2+2.*QE*VE*QF*VF*SFI*SFF+                        LULAMB60
     &   (VE**2+AE**2)*(VF**2+AF**2)*SFW*SFF**2                         LULAMB61
          F1=2.*QE*AE*QF*AF*SFI*SFF+4.*VE*AE*VF*AF*SFW*SFF**2           LULAMB62
          F2=2.*QE*VE*QF*AF*SFI*SFF+2.*(VE**2+AE**2)*VF*AF*SFW*SFF**2   LULAMB63
          F3=2.*QE*AE*QF*VF*SFI*SFF+2.*VE*AE*(VF**2+AF**2)*SFW*SFF**2   LULAMB64
C...Pick s_z = +-1 according to relative probabilities.                 LULAMB65
          CTHE=COS(THELB)                                               LULAMB66
          FUNPOL=(1.+CTHE**2)*F0+2.*CTHE*F1                             LULAMB67
          FPOL=(1.+CTHE**2)*F2+2.*CTHE*F3                               LULAMB68
          SPINZB=1.                                                     LULAMB69
          IF(FUNPOL-FPOL.GT.2.*FUNPOL*RLU(0)) SPINZB=-1.                LULAMB70
       ENDIF                                                            LULAMB71
C...Do decay and set initial weight of decay.                           LULAMB72
       NSAV=N7LU                                                        LULAMB73
       ISDEC = 0                                                        LULAMB74
  110  CALL LUDECY(I)                                                   LULAMB75
C      KS=K7LU(I,1)                                                     LULAMB76
       IF ( ISDEC.EQ.0) THEN                                            LULAMB77
C  Decay is a SEMI lep decay ?                                          LULAMB78
         JF=K7LU(I,4)                                                   LULAMB79
         JL=K7LU(I,5)                                                   LULAMB80
C  Semileptonic e,mu,tau  only with neutrino , lepton AND c baryon      LULAMB81
         NLEPN = 0                                                      LULAMB82
         NLEPC = 0                                                      LULAMB83
         DO 2 JJ = JF,JL                                                LULAMB84
           KFJJ = ABS(K7LU(JJ,2))                                       LULAMB85
           IF( KFJJ.EQ.NNUEL .OR. KFJJ.EQ.NNUMU .OR. KFJJ.EQ.NNUTO)     LULAMB86
     $     NLEPN = NLEPN+KFJJ-10                                        LULAMB87
           IF( KFJJ.EQ.NEL .OR. KFJJ.EQ.NMU .OR. KFJJ.EQ.NTO)           LULAMB88
     $     NLEPC = NLEPC+KFJJ-10                                        LULAMB89
  2      CONTINUE                                                       LULAMB90
         NLAMB=NLAMB+1                                                  LULAMB91
         IF (NLEPN*NLEPC.EQ.0) GO TO 140                                LULAMB92
         IDCK = NLEPN/2                                                 LULAMB93
         ILAMB=I                                                        LULAMB94
         IDLAMB=NSAV+1                                                  LULAMB95
         ISGNLB=ISIGN(1,K7LU(I,2))                                      LULAMB96
       ENDIF                                                            LULAMB97
       WTLB=1.                                                          LULAMB98
C...Construct spin-dependent weight.                                    LULAMB99
       IF(ISPINL.EQ.1) THEN                                             LULAM100
          WTSPIN=0.5*(1.-SPINZB*P7LU(NSAV+1,3)/P7LU(NSAV+1,4))          LULAM101
          WTLB=WTLB*WTSPIN                                              LULAM102
       ENDIF                                                            LULAM103
C...Construct form factor weight.                                       LULAM104
       IF(IFORML.EQ.1) THEN                                             LULAM105
         W=P7LU(NSAV+3,4)/P7LU(NSAV+3,5)                                LULAM106
         WTFORM=W0LAMB**2/(W0LAMB**2-2.+2.*W)                           LULAM107
         WTLB=WTLB*WTFORM**2                                            LULAM108
       ENDIF                                                            LULAM109
C...Reject kinematics according to total weight, go back for new.       LULAM110
       IF(WTLB.LT.RLU(0)) THEN                                          LULAM111
          K7LU(I,1)=1                                                   LULAM112
          N7LU=NSAV                                                     LULAM113
          ISDEC = ISDEC + 1                                             LULAM114
C... Set up decay mode as the only one                                  LULAM115
          DO 23 II= 1,NMBR                                              LULAM116
              MDME(MDCY(85,2)+II-1,1)= 0                                LULAM117
   23     CONTINUE                                                      LULAM118
          MDME(MDCY(85,2)+IDCK-1,1)= 1                                  LULAM119
          GOTO 110                                                      LULAM120
       ENDIF                                                            LULAM121
C...Boost and rotate back Lambda_b and its products.                    LULAM122
 140   CALL LUDBRB(I,I,0.,0.,0D0,0D0,DBLE(BEZLB))                       LULAM123
       CALL LUDBRB(NSAV+1,N7LU,0.,0.,0D0,0D0,DBLE(BEZLB))               LULAM124
       CALL LUDBRB(I,I,THELB,PHILB,0D0,0D0,0D0)                         LULAM125
       CALL LUDBRB(NSAV+1,N7LU,THELB,PHILB,0D0,0D0,0D0)                 LULAM126
C... Restore full list of decays                                        LULAM127
       DO 24 II= 1,NMBR                                                 LULAM128
           MDME(MDCY(85,2)+II-1,1)=MDMSTO(II)                           LULAM129
   24  CONTINUE                                                         LULAM130
C...histogram E lepton if semi lep                                      LULAM131
      IF ( IDCK.GT.0) THEN                                              LULAM132
        ELEPT=P7LU(IDLAMB+1,4)                                          LULAM133
        CALL HF1(10030+IDCK,ELEPT,1.)                                   LULAM134
      ENDIF                                                             LULAM135
C...End loop over Lambda_b;                                             LULAM136
      ENDIF                                                             LULAM137
  130 CONTINUE                                                          LULAM138
      IF(NLAMB.GE.1) CALL LUEXEC                                        LULAM139
 111  CONTINUE                                                          LULAM140
 999  RETURN                                                            LULAM141
      END                                                               LULAM142
      SUBROUTINE LUNDDC(IDDC)                                           LUNDDC 2
C------------------------------------------------------------------     LUNDDC 3
C! Performs Particle decay with selected decay modes if it is the       LUNDDC 4
C! right decay chain                                                    LUNDDC 5
C  B.Bloch-Devaux March  1994                                           LUNDDC 6
C    IDDC : SELECTED DECAYFLAG 0 NO SELECTED DECAY DONE                 LUNDDC 7
C                              1 SELECTED DECAY FOR FIRST chain         LUNDDC 8
C------------------------------------------------------------------     LUNDDC 9
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
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
C                                                                       LUN7CO12
      COMMON /CHAINS/ KFDAU,KFMOTH,KFCOMP(2)                            CHAINS 2
C   KFDAU  = internal JETSET 7.3 code for daughter decayed particle     CHAINS 3
C   KFMOTH = internal JETSET 7.3 code for mother of decayed particle    CHAINS 4
C   KFCOMP(2)internal JETSET 7.3 code for companions of decayed particleCHAINS 5
      IDDC = 0                                                          LUNDDC13
      KC = LUCOMP(KFDAU)                                                LUNDDC14
      NDEC=MDCY(KC,3)                                                   LUNDDC15
      IDC=MDCY(KC,2)                                                    LUNDDC16
      DO 100 I=1,N7LU                                                   LUNDDC17
         KFB = ABS(K7LU(I,2))                                           LUNDDC18
C Look for daughter KFDAU                                               LUNDDC19
         IF (KFB.EQ.KFDAU) THEN                                         LUNDDC20
C Look for mother   KFMOTH                                              LUNDDC21
            KFM = K7LU(I,3)                                             LUNDDC22
            IF ( ABS(K7LU(KFM,2)).EQ.KFMOTH) THEN                       LUNDDC23
C Look for daughter companions KFCOMP                                   LUNDDC24
              IOK = 1                                                   LUNDDC25
              DO 101 J = 1,2                                            LUNDDC26
                 KFC = KFCOMP(J)                                        LUNDDC27
                 IF ( KFC.GT.0) THEN                                    LUNDDC28
                    DO 102 KK = K7LU(KFM,4),K7LU(KFM,5)                 LUNDDC29
                       IF ( ABS(K7LU(KK,2)).EQ.KFC ) GO TO 103          LUNDDC30
 102                CONTINUE                                            LUNDDC31
                 ENDIF                                                  LUNDDC32
 101          CONTINUE                                                  LUNDDC33
              IF ( KFCOMP(1)+KFCOMP(2).EQ.0) GO TO 103                  LUNDDC34
              IOK = 0                                                   LUNDDC35
 103          CONTINUE                                                  LUNDDC36
C  set up the requested decay modes for the daughter and perform decay  LUNDDC37
              IF ( IOK.GT.0) THEN                                       LUNDDC38
                 JDEC=IW(NAMIND('GDDC'))                                LUNDDC39
                 IF(JDEC.GT.0) THEN                                     LUNDDC40
                    CALL UCOPY (IW(JDEC+5),MDME(IDC,1),NDEC)            LUNDDC41
                    IDDC = IDDC + 1                                     LUNDDC42
                 ENDIF                                                  LUNDDC43
                 CALL LUDECY(I)                                         LUNDDC44
                 CALL UFILL (MDME(IDC,1),1,NDEC,1)                      LUNDDC45
                 GO TO 500                                              LUNDDC46
                 ENDIF                                                  LUNDDC47
            ENDIF                                                       LUNDDC48
         ENDIF                                                          LUNDDC49
C                                                                       LUNDDC50
 100  CONTINUE                                                          LUNDDC51
 500  CALL HFILL(10040,FLOAT(IDDC),DUM,1.)                              LUNDDC52
      RETURN                                                            LUNDDC53
      END                                                               LUNDDC54
      SUBROUTINE LUJFIL(IM,ID)                                          LUJFIL 2
C...Purpose: to convert to JETSET event record contents from            LUJFIL 3
C...the standard event record commonblock.                              LUJFIL 4
      PARAMETER (NMXHEP=2000)                                           HEPEVT 2
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),           HEPEVT 3
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)   HEPEVT 4
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)                     LUJFIL 6
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)             LUJFIL 7
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)        LUJFIL 8
      SAVE /HEPEVT/                                                     LUJFIL 9
      SAVE /LUJETS/,/LUDAT1/,/LUDAT2/                                   LUJFIL10
C...Conversion from standard to JETSET, the easy part.                  LUJFIL11
        IF(NHEP.GT.MSTU(4)) CALL LUERRM(8,                              LUJFIL12
     &  '(LUHEPC:) no more space in /LUJETS/')                          LUJFIL13
        NN=NHEP                                                         LUJFIL14
        DO 180 I=1,NN                                                   LUJFIL15
        IF ( I.EQ.1 ) THEN                                              LUJFIL16
          II = IM+I-1                                                   LUJFIL17
        ELSE                                                            LUJFIL18
          II = ID+I-2                                                   LUJFIL19
        ENDIF                                                           LUJFIL20
        K(II,1)=0                                                       LUJFIL21
        IF(ISTHEP(I).EQ.1) K(II,1)=1                                    LUJFIL22
        IF(ISTHEP(I).EQ.2) K(II,1)=11                                   LUJFIL23
        IF(ISTHEP(I).EQ.3) K(II,1)=21                                   LUJFIL24
        IF ( I.GT.1 ) THEN                                              LUJFIL25
           K(II,2)=IDHEP(I)                                             LUJFIL26
           K(II,3)= IM                                                  LUJFIL27
           DO 160 J=1,5                                                 LUJFIL28
  160      P(II,J)=PHEP(J,I)                                            LUJFIL29
        ENDIF                                                           LUJFIL30
        IF ( JDAHEP(1,I).GT.0) THEN                                     LUJFIL31
           K(II,4)=JDAHEP(1,I)+ID-2                                     LUJFIL32
           K(II,5)=JDAHEP(2,I)+ID-2                                     LUJFIL33
        ELSE                                                            LUJFIL34
           K(II,4)=0                                                    LUJFIL35
           K(II,5)=0                                                    LUJFIL36
        ENDIF                                                           LUJFIL37
  180   CONTINUE                                                        LUJFIL38
      N = II                                                            LUJFIL39
      RETURN                                                            LUJFIL40
      END                                                               LUJFIL41
      SUBROUTINE LUNDEC(IFDC)                                           LUNDEC 2
C------------------------------------------------------------------     LUNDEC 3
C! Performs B meson decay one at a time with selected decay modes       LUNDEC 4
C  B.Bloch-Devaux February 1991                                         LUNDEC 5
C    IFDC : SELECTED DECAYFLAG 0 NO SELECTED DECAY DONE                 LUNDEC 6
C                              1 SELECTED DECAY FOR FIRST B MESON       LUNDEC 7
C                              2 SELECTED DECAY FOR SECOND B MESON      LUNDEC 8
C                              3 SELECTED DECAY FOR BOTH B MESONS       LUNDEC 9
C------------------------------------------------------------------     LUNDEC10
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
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
C                                                                       LUN7CO12
       LOGICAL BMES,BMESS,BMEST                                         BBL00732
      PARAMETER (LBPOS=100,NBU=521,NBD=511,NBS=531,NBC=541)             BCODES 2
      PARAMETER (NLB=5122,NXB= 5132)                                    BBL00511
C     LBPOS  = internal JETSET 7.3 code for B meson                     BCODES 3
      PARAMETER (NXB0=5232,NOB= 5332)                                   BBL007 9
C     NBU    = internal JETSET 7.3 code for Bu meson                    BCODES 4
C     NBD    = internal JETSET 7.3 code for Bd meson                    BCODES 5
C     NBS    = internal JETSET 7.3 code for Bs meson                    BCODES 6
C     NBC    = internal JETSET 7.3 code for Bc meson                    BCODES 7
C     NLB    = internal JETSET 7.3 code for /\B BARYON                  BBL00512
C     NXB    = internal JETSET 7.3 code for XIB BARYON                  BBL00513
C     NXB0   = internal JETSET 7.3 code for XIB0 BARYON                 BBL00710
C     NOB    = internal JETSET 7.3 code for OMEGAB BARYON               BBL00711
C    LUND 7.3 codes for BD, BS ,BU                                      LUNDEC15
      PARAMETER ( LSTR = 92)                                            BBL00338
      IFDC = 0                                                          LUNDEC16
      KC = LUCOMP(LBPOS)                                                LUNDEC17
      NDEC=MDCY(KC,3)                                                   LUNDEC18
      IDC=MDCY(KC,2)                                                    LUNDEC19
      IMES =0                                                           LUNDEC20
      IBD =0                                                            BBL00339
      IBU =0                                                            BBL00340
      IBS =0                                                            BBL00341
      IBC =0                                                            BBL00342
      IBU1 =0                                                           BBL00343
      IBD1 =0                                                           BBL00344
      DO 100 I=1,N7LU                                                   BBL00345
         KFB = ABS(K7LU(I,2))                                           BBL00346
C Look for string and its daughters                                     BBL00347
         IF (KFB.EQ.LSTR) THEN                                          BBL00348
            DO 101 J= K7LU(I,4),K7LU(I,5)                               BBL00349
              KFA = ABS(K7LU(J,2))                                      BBL00350
              BMES =(KFA.EQ.NBD).OR.(KFA.EQ.NBS).OR.( KFA.EQ.NBU).OR.   BBL00351
     $        (KFA.EQ.NBC)                                              BBL00352
              BMESS=(KFA.EQ.NBD+2).OR.(KFA.EQ.NBS+2).OR.                BBL00353
     $        ( KFA.EQ.NBU+2).OR.(KFA.EQ.NBC+2).OR.BMES                 BBL00354
              BMEST=(KFA.EQ.NBD+4).OR.(KFA.EQ.NBU+4).OR.(KFA.EQ.NBS+4)  BBL00733
     $        .OR.(KFA.EQ.NBC+4).OR.BMESS                               BBL00734
C  Look for undecayed B mesons                                          BBL00355
              IF (BMEST) THEN                                           BBL00735
C                                                                       BBL00357
C  SET UP THE REQUESTED DECAY MODES FOR THE SUCCESSIVE B MESONS         BBL00358
C  AND PERFORM DECAY OF B IF UNDECAYED ( OR ITS DAUGHTER IF DECAYED :   BBL00359
C  THAT IS WHEN IT IS A B0 WHICH IS COPIED FOR MIXING AND/OR A B*)      BBL00360
C                                                                       BBL00361
                 JP = J                                                 BBL00362
                 IF(K7LU(J,1).GT.10) THEN                               BBL00363
                    IMOTH = J                                           BBL00364
    5               JP = K7LU(IMOTH,4)                                  BBL00365
                    IF ( K7LU(JP,1).GT.10) THEN                         BBL00366
                      IMOTH = JP                                        BBL00367
                      GO TO 5                                           BBL00368
                    ENDIF                                               BBL00369
                 ENDIF                                                  BBL00370
                 KFF = ABS(K7LU(JP,2))                                  BBL00371
                 IMES = IMES +1                                         BBL00372
                 IF (KFF.EQ.NBD) IBD = IBD +1                           BBL00373
                 IF (KFF.EQ.NBU) IBU = IBU +1                           BBL00374
                 IF (KFF.EQ.NBS) IBS = IBS +1                           BBL00375
                 IF (KFF.EQ.NBC) IBC = IBC +1                           BBL00376
                 IF(MOD(IMES,2).EQ.1) THEN                              BBL00377
                   JDEC=IW(NAMIND('GDC1'))                              BBL00378
                   IF(JDEC.GT.0) THEN                                   BBL00379
                      CALL UCOPY (IW(JDEC+1),MDME(IDC,1),NDEC)          BBL00380
                      IFDC = IFDC + 1                                   BBL00381
                   ENDIF                                                BBL00382
                   CALL LUDECY(JP)                                      BBL00383
                   CALL UFILL (MDME(IDC,1),1,NDEC,1)                    BBL00384
                   IF ( KFF.EQ.NBD) IBD1 = IBD1+1                       BBL00385
                   IF ( KFF.EQ.NBU) IBU1 = IBU1+1                       BBL00386
                 ELSE                                                   BBL00387
                   JDEC=IW(NAMIND('GDC2'))                              BBL00388
                   IF(JDEC.GT.0) THEN                                   BBL00389
                      CALL UCOPY (IW(JDEC+1),MDME(IDC,1),NDEC)          BBL00390
                      IFDC = IFDC + 2                                   BBL00391
                   ENDIF                                                BBL00392
                   CALL LUDECY(JP)                                      BBL00393
                   CALL UFILL (MDME(IDC,1),1,NDEC,1)                    BBL00394
                 ENDIF                                                  BBL00395
              ENDIF                                                     BBL00396
C                                                                       BBL00397
 101        CONTINUE                                                    BBL00398
         ENDIF                                                          BBL00399
C                                                                       LUNDEC50
 100  CONTINUE                                                          LUNDEC51
      X = FLOAT(IMES)                                                   LUNDEC52
      IF (IMES.GT.2) CALL LULIST(1)                                     LUNDEC53
      CALL HFILL(10009,X,DUM,1.)                                        LUNDEC54
      X = FLOAT(IBD)                                                    BBL00400
      Y = FLOAT(IBU)                                                    BBL00401
      CALL HFILL(10019,X,Y,1.)                                          BBL00402
      X = FLOAT(IBS)                                                    BBL00403
      CALL HFILL(10039,X,DUM,1.)                                        BBL00404
      X = FLOAT(IBC)                                                    BBL00405
      CALL HFILL(10049,X,DUM,1.)                                        BBL00406
      X = FLOAT(IBD1)                                                   BBL00407
      Y = FLOAT(IBU1)                                                   BBL00408
      CALL HFILL(10029,X,Y,1.)                                          BBL00409
      RETURN                                                            LUNDEC55
      END                                                               LUNDEC56
      SUBROUTINE LUNMIX(IFMI)                                           LUNMIX 2
C------------------------------------------------------------------     LUNMIX 3
C!   Performs B Bbar mixing                                             LUNMIX 4
C  Include  INTEGRAL B MIXING (24/10/88)                                LUNMIX 5
C    IFMI : MIXING FLAG  0 NO MIXING DONE , 1 MIXING ON INITIAL B       LUNMIX 6
C                        2 MIXING ON INITIAL BBAR, 3 MIXING ON BOTH     LUNMIX 7
C------------------------------------------------------------------     LUNMIX 8
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
C                                                                       LUN7CO12
      COMPLEX PSQD,PSQS                                                 MIXING 2
      COMMON/ MIXING /XD,YD,CHID,RD,XS,YS,CHIS,RS,PSQD,PSQS             MIXING 3
      COMMON/ PROBMI /CDCPBA,CDCPB,CSCPBA,CSCPB                         MIXING 4
      PARAMETER (LNBRCP=4)                                              USERCP 2
      COMMON/USERCP/NPARCP,JCODCP(LNBRCP),RHOCPM                        USERCP 3
      COMPLEX RHOCPM                                                    USERCP 4
      PARAMETER (ICOPSI=443,ICOK0S=310)                                 CODPAR72
      PARAMETER (LBPOS=100,NBU=521,NBD=511,NBS=531,NBC=541)             BCODES 2
      PARAMETER (NLB=5122,NXB= 5132)                                    BBL00511
C     LBPOS  = internal JETSET 7.3 code for B meson                     BCODES 3
      PARAMETER (NXB0=5232,NOB= 5332)                                   BBL007 9
C     NBU    = internal JETSET 7.3 code for Bu meson                    BCODES 4
C     NBD    = internal JETSET 7.3 code for Bd meson                    BCODES 5
C     NBS    = internal JETSET 7.3 code for Bs meson                    BCODES 6
C     NBC    = internal JETSET 7.3 code for Bc meson                    BCODES 7
C     NLB    = internal JETSET 7.3 code for /\B BARYON                  BBL00512
C     NXB    = internal JETSET 7.3 code for XIB BARYON                  BBL00513
C     NXB0   = internal JETSET 7.3 code for XIB0 BARYON                 BBL00710
C     NOB    = internal JETSET 7.3 code for OMEGAB BARYON               BBL00711
C    LUND 7.3 codes for BD, BDbar, BS ,BSbar , Psi and K0s              CODPAR74
      IFMI = 0                                                          LUNMIX13
      NPAR=N7LU                                                         LUNMIX14
      DO 100 I=1,NPAR                                                   LUNMIX15
      IF(ABS(K7LU(I,2)).EQ.NBD.OR.ABS(K7LU(I,2)).EQ.NBS)THEN            LUNMIX16
C                                                                       LUNMIX17
C  ADD A NEW B MESON RESULTING FROM MIXING WITH SAME                    LUNMIX18
C  KINEMATICAL PARAMETERS THAN THE ORIGINAL B AND APPROPRIATE LUND CODE LUNMIX19
C                                                                       LUNMIX20
         K7LU(I,1)=K7LU(I,1)+10                                         LUNMIX21
         K7LU(I,4)=N7LU+1                                               LUNMIX22
         K7LU(I,5)=N7LU+1                                               LUNMIX23
         N7LU=N7LU+1                                                    LUNMIX24
         K7LU(N7LU,1)=1                                                 LUNMIX25
         K7LU(N7LU,3)=I                                                 LUNMIX26
         K7LU(N7LU,4)=0                                                 LUNMIX27
         K7LU(N7LU,5)=0                                                 LUNMIX28
         DO 101 J=1,5                                                   LUNMIX29
          P7LU(N7LU,J)=P7LU(I,J)                                        LUNMIX30
          V7LU(N7LU,J)=V7LU(I,J)                                        LUNMIX31
 101     CONTINUE                                                       LUNMIX32
C                                                                       LUNMIX33
C  ISIGN=1 : NO B OSCILLATION   ;   ISIGN=-1  : B OSCILLATION           LUNMIX34
C                                                                       LUNMIX35
         ISIGN=1                                                        LUNMIX36
         IF(K7LU(I,2).EQ.NBD.AND.RNDM(DUM).LT.CDCPB)THEN                LUNMIX37
           ISIGN=-1                                                     LUNMIX38
           IFMI = IFMI +1                                               LUNMIX39
         ELSEIF (K7LU(I,2).EQ.-NBD.AND.RNDM(DUM).LT.CDCPBA)THEN         LUNMIX40
           ISIGN =-1                                                    LUNMIX41
           IFMI = IFMI +2                                               LUNMIX42
         ELSEIF(K7LU(I,2).EQ.NBS.AND.RNDM(DUM).LT.CSCPB)THEN            LUNMIX43
           ISIGN=-1                                                     LUNMIX44
           IFMI = IFMI +1                                               LUNMIX45
         ELSEIF(K7LU(I,2).EQ.-NBS.AND.RNDM(DUM).LT.CSCPBA)THEN          LUNMIX46
           ISIGN=-1                                                     LUNMIX47
           IFMI = IFMI +2                                               LUNMIX48
         ENDIF                                                          LUNMIX49
         K7LU(N7LU,2)=ISIGN*K7LU(I,2)                                   LUNMIX50
       ENDIF                                                            LUNMIX51
 100  CONTINUE                                                          LUNMIX52
      RETURN                                                            LUNMIX53
      END                                                               LUNMIX54
      SUBROUTINE LUNPHO(JFLA)                                           LUNPHO 2
C---------------------------------------------------------------------- LUNPHO 3
C     B.Bloch & D.Colling : June   1992 for use of PHOTOS to generate   LUNPHO 4
C                         internal brem in B and D meson semi-lep decaysLUNPHO 5
C! Steering for radiative Semileptonic decays of Heavy Mesons (B and D )LUNPHO 6
C    JFLA = heavy flavor to be considered   4 = D MESONS  5= B MESONS   LUNPHO 7
C                                          14 = D Baryon 15= B Baryon   LUNPHO 8
C    JFLA <0 only particle #-JFLA must be considered ( B meson)         LUNPHO 9
C---------------------------------------------------------------------- LUNPHO10
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
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
C                                                                       LUN7CO12
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12         ALCONS 2
      REAL RADEG, DEGRA                                                 ALCONS 3
      REAL CLGHT                                                        ALCONS 4
      INTEGER NBITW, NBYTW, LCHAR                                       ALCONS 5
      PARAMETER (PI=3.141592653589)                                     ALCONS 6
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)                          ALCONS 7
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          ALCONS 8
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         ALCONS 9
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         ALCONS10
      PARAMETER (CLGHT = 29.9792458)                                    ALCONS11
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)              ALCONS12
C                                                                       ALCONS13
      PARAMETER (LBPOS=100,NBU=521,NBD=511,NBS=531,NBC=541)             BCODES 2
      PARAMETER (NLB=5122,NXB= 5132)                                    BBL00511
C     LBPOS  = internal JETSET 7.3 code for B meson                     BCODES 3
      PARAMETER (NXB0=5232,NOB= 5332)                                   BBL007 9
C     NBU    = internal JETSET 7.3 code for Bu meson                    BCODES 4
C     NBD    = internal JETSET 7.3 code for Bd meson                    BCODES 5
C     NBS    = internal JETSET 7.3 code for Bs meson                    BCODES 6
C     NBC    = internal JETSET 7.3 code for Bc meson                    BCODES 7
C     NLB    = internal JETSET 7.3 code for /\B BARYON                  BBL00512
C     NXB    = internal JETSET 7.3 code for XIB BARYON                  BBL00513
C     NXB0   = internal JETSET 7.3 code for XIB0 BARYON                 BBL00710
C     NOB    = internal JETSET 7.3 code for OMEGAB BARYON               BBL00711
      PARAMETER (NDU=421,NDD=411,NDS=431)                               DCODES 2
      PARAMETER (NLC=4122,NXC= 4132)                                    DCODES 3
C     NDU    = internal JETSET 7.3 code for Du meson                    DCODES 4
C     NDD    = internal JETSET 7.3 code for Dd meson                    DCODES 5
C     NDS    = internal JETSET 7.3 code for Ds meson                    DCODES 6
C     NLC    = internal JETSET 7.3 code for /\C BARYON                  DCODES 7
C     NXC    = internal JETSET 7.3 code for XIC BARYON                  DCODES 8
      PARAMETER (NNUEL=12,NNUMU=14,NNUTO=16,NEL=11,NMU=13,NTO=15)       LCODES 2
C     NNUEL  = internal JETSET 7.3 code for neutrino electron           LCODES 3
C     NNUMU  = internal JETSET 7.3 code for neutrino mu                 LCODES 4
C     NNUTO  = internal JETSET 7.3 code for neutrino tau                LCODES 5
C     NEL    = internal JETSET 7.3 code for electron                    LCODES 6
C     NMU    = internal JETSET 7.3 code for muon                        LCODES 7
C     NTO    = internal JETSET 7.3 code for tau                         LCODES 8
      COMMON / MASTER / IGENE,IWEIT,IMIX,IVBU,IFL,ECM,IEV1,IEV2,IEFLOP, MASTER 2
     $          ISEM,IDEC,WTMAX,SVRT(3),ICOULU(10),IPHO,ILAM,IP17,IDDC  BBL00914
      DATA  IFIR / 0/                                                   LUNPHO18
      IF ( IFIR.EQ.0) THEN                                              LUNPHO19
          IMES = MOD(IPHO,10)                                           LUNPHO20
          IBAR = IPHO/10                                                LUNPHO21
      ENDIF                                                             LUNPHO22
      IF ( JFLA.GT.0) THEN                                              LUNPHO23
         IFLA = JFLA                                                    LUNPHO24
         I0 = 0                                                         LUNPHO25
      ELSE IF ( JFLA.LT.0) THEN                                         LUNPHO26
         IFLA = 5                                                       LUNPHO27
         I0 = -JFLA-1                                                   LUNPHO28
      ELSE                                                              LUNPHO29
         GO TO 999                                                      LUNPHO30
      ENDIF                                                             LUNPHO31
      IF ( IFLA.EQ.4) THEN                                              LUNPHO32
         NFIR = NDD                                                     LUNPHO33
         NSEC = NFIR                                                    LUNPHO34
         INC = 10                                                       LUNPHO35
         IF ( IAND(IMES,1).EQ.0) GO TO 999                              LUNPHO36
      ELSE IF ( IFLA.EQ.5) THEN                                         LUNPHO37
         NFIR = NBD                                                     LUNPHO38
         NSEC = NFIR                                                    LUNPHO39
         INC = 10                                                       LUNPHO40
         IF ( IAND(IMES,2).EQ.0) GO TO 999                              LUNPHO41
      ELSE IF ( IFLA.EQ.14) THEN                                        LUNPHO42
         NFIR = NLC                                                     LUNPHO43
         NSEC = NXC                                                     LUNPHO44
         INC = 100                                                      LUNPHO45
         IF ( IAND(IBAR,1).EQ.0) GO TO 999                              LUNPHO46
      ELSE IF ( IFLA.EQ.15) THEN                                        LUNPHO47
         NFIR = NLB                                                     LUNPHO48
         NSEC = NXB                                                     LUNPHO49
         INC = 100                                                      LUNPHO50
         IF ( IAND(IBAR,2).EQ.0) GO TO 999                              LUNPHO51
      ELSE                                                              LUNPHO52
         GO TO 999                                                      LUNPHO53
      ENDIF                                                             LUNPHO54
      I = I0                                                            LUNPHO55
1     I = I+1                                                           LUNPHO56
         IF (JFLA.LT.0 .AND. I.GT.-JFLA) GO TO 111                      LUNPHO57
         IF ( I.GT.N7LU ) GO TO 111                                     LUNPHO58
         KF=ABS(K7LU(I,2))                                              LUNPHO59
C  We have a Bd,Bu or Bs , D0,D+- or Ds , /\b type./\c type...          LUNPHO60
         IF ((KF.EQ.NFIR).OR.(KF.EQ.NSEC).OR.(KF.EQ.NSEC+INC).OR.       LUNPHO61
     $    (KF.EQ.NSEC+2*INC)) THEN                                      LUNPHO62
            KS=K7LU(I,1)                                                LUNPHO63
C  It is a decayed particle                                             LUNPHO64
            IF(KS.LT.10) GO TO 1                                        LUNPHO65
C  Decay is a SEMI lep decay                                            LUNPHO66
            JF=K7LU(I,4)                                                LUNPHO67
            JL=K7LU(I,5)                                                LUNPHO68
C  Semileptonic e,mu only with neutrino  and lepton                     LUNPHO69
            NLEPN = 0                                                   LUNPHO70
            NLEPC = 0                                                   LUNPHO71
            DO 2 JJ = JF,JL                                             LUNPHO72
              KFJJ = ABS(K7LU(JJ,2))                                    LUNPHO73
              IF( KFJJ.EQ.NNUEL .OR. KFJJ.EQ.NNUMU) NLEPN = 1           LUNPHO74
              IF( KFJJ.EQ.NEL .OR. KFJJ.EQ.NMU) NLEPC =1                LUNPHO75
  2        CONTINUE                                                     LUNPHO76
           IF (NLEPN*NLEPC.NE.1) GO TO 1                                LUNPHO77
C  Transfer the decay to HEPEVT common                                  LUNPHO78
           CALL HEPFIL(I,JF,JL)                                         LUNPHO79
C        CALL HEPLIS(1)                                                 LUNPHO80
C  CALL PHOTOS for mother particle                                      LUNPHO81
         CALL PHOCAL(IMOD)                                              LUNPHO82
C        CALL HEPLIS(1)                                                 LUNPHO83
C   IF no extra photon added nothing to do                              LUNPHO84
         IF ( IMOD.EQ.0) GO TO 1                                        LUNPHO85
C                                                                       LUNPHO86
C  KILL THE DECAY PRODUCTS OF B/D                                       LUNPHO87
C                                                                       LUNPHO88
         NPARI=N7LU                                                     LUNPHO89
         IGARB0 = I                                                     LUNPHO90
         K7LU(IGARB0,1) = KS -10                                        LUNPHO91
         K7LU(IGARB0,4)=0                                               LUNPHO92
         K7LU(IGARB0,5)=0                                               LUNPHO93
C  Identify in the remaining list the successive daughters              LUNPHO94
         DO 6 IGARB= I+1,NPARI                                          LUNPHO95
           IGARB1=IGARB                                                 LUNPHO96
 7         IGARB1=K7LU(IGARB1,3)                                        LUNPHO97
           IF(IGARB1.EQ.IGARB0.OR.IGARB1.EQ.1000)THEN                   LUNPHO98
              K7LU(IGARB,3)=1000                                        LUNPHO99
              K7LU(IGARB,4)=0                                           LUNPH100
              K7LU(IGARB,5)=0                                           LUNPH101
              GOTO 6                                                    LUNPH102
           ENDIF                                                        LUNPH103
           IF(IGARB1.LT.IGARB0)THEN                                     LUNPH104
              GOTO 6                                                    LUNPH105
           ELSE                                                         LUNPH106
              GOTO 7                                                    LUNPH107
           ENDIF                                                        LUNPH108
 6       CONTINUE                                                       LUNPH109
C   Compress the remaining entries                                      LUNPH110
         N7LU=JF-1                                                      LUNPH111
         IMOT0 = -1                                                     LUNPH112
         DO 8 IGARB= JF ,NPARI                                          LUNPH113
            IF(K7LU(IGARB,3).EQ.1000)GOTO 8                             LUNPH114
            N7LU=N7LU+1                                                 LUNPH115
            K7LU(N7LU,1)=K7LU(IGARB,1)                                  LUNPH116
            K7LU(N7LU,2)=K7LU(IGARB,2)                                  LUNPH117
            K7LU(N7LU,3)=K7LU(IGARB,3)                                  LUNPH118
            K7LU(N7LU,4)=K7LU(IGARB,4)                                  LUNPH119
            K7LU(N7LU,5)=K7LU(IGARB,5)                                  LUNPH120
            DO 9 IGARB2=1,5                                             LUNPH121
              P7LU(N7LU,IGARB2)=P7LU(IGARB,IGARB2)                      LUNPH122
 9          CONTINUE                                                    LUNPH123
C   Update current daughter number for the mother                       LUNPH124
            IMOTH = K7LU(N7LU,3)                                        LUNPH125
            IF (IMOTH.EQ.IMOT0) GOTO 10                                 LUNPH126
              IMOT0 = IMOTH                                             LUNPH127
              K7LU(IMOTH,4) = K7LU(IMOTH,4)+N7LU-IGARB                  LUNPH128
              K7LU(IMOTH,5) = K7LU(IMOTH,5)+N7LU-IGARB                  LUNPH129
 10         CONTINUE                                                    LUNPH130
C   Update current mother number for the daughters                      LUNPH131
            IF (K7LU(IGARB,5).LE.0) GOTO 8                              LUNPH132
            DO 11 IGARB2= K7LU(IGARB,4),K7LU(IGARB,5)                   LUNPH133
               K7LU(IGARB2,3)=N7LU                                      LUNPH134
 11         CONTINUE                                                    LUNPH135
 8       CONTINUE                                                       LUNPH136
C  WE HAVE NOW A LIST WITH B/D MOTHER IN I AND DAUGHTERS CAN BE FILLED  LUNPH137
C FROM POSITION N7LU+1                                                  LUNPH138
         CALL LUJFIL(I,N7LU+1)                                          LUNPH139
         IF (IFLA.EQ.4 ) IDD = 10050                                    LUNPH140
         IF (IFLA.EQ.14) IDD = 10051                                    LUNPH141
         IF (IFLA.EQ.5 ) IDD = 10052                                    LUNPH142
         IF (IFLA.EQ.15) IDD = 10053                                    LUNPH143
         CALL HFILL(IDD,P7LU(N7LU,4),DUM,1.)                            LUNPH144
        ENDIF                                                           LUNPH145
      GO TO 1                                                           LUNPH146
 111  CONTINUE                                                          LUNPH147
 999  RETURN                                                            LUNPH148
      END                                                               LUNPH149
      SUBROUTINE LUNSEM                                                 LUNSEM 2
C---------------------------------------------------------------------- LUNSEM 3
C     A. FALVARD   -   130389   in HVFL01                               LUNSEM 4
C                  -   Modified :  november 1990 for HVFL02             LUNSEM 5
C     B.Bloch      -   Modified : January  1991 for JETSET73            LUNSEM 6
C! Steering for Semileptonic decays of Heavy Mesons. (B ONLY for now)   LUNSEM 7
C---------------------------------------------------------------------- LUNSEM 8
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
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
C                                                                       LUN7CO12
      PARAMETER (NMODEL=5,NDECAY=11)                                    AFL001 1
      COMMON/SEMLEP/IMOSEM,WTM(NMODEL,NDECAY),IHEL,KHEL,PRM1,PR0,PRP1,  SEMLEP 3
     $          BBDAT(NDECAY),BXDAT(NDECAY)  ,                          SEMLEP 4
     $          NO1(NDECAY),NO2(NDECAY),OVER(NMODEL,NDECAY)             SEMLEP 5
     $          , IMODSS,R1,R2,R3                                       AFL001 2
      REAL MFF,MFF2,MFF3,MFF4                                           SEMLEP 6
      COMMON/MFFX/MFF(NMODEL,NDECAY),MFF2(NMODEL,NDECAY),               SEMLEP 7
     $            MFF3(NMODEL,NDECAY),MFF4(NMODEL,NDECAY)               SEMLEP 8
      REAL MB,MD,MQ,MBOT,MX,MUP,MUM,MS,MSQ,MRSQ,KSQ                     SEMLEP 9
      COMMON /SCORA/MB,MD,MQ,MBOT,MX,MUP,MUM,MS,MSQ,MRSQ,KSQ            SEMLEP10
     *,BM,BMSQ,XMTSQ,BB,BX,XMT,BBX,BR,BRX                               SEMLEP11
      COMMON /SCOROU/AL,CP,Q,QV                                         SEMLEP12
      COMMON / MASTER / IGENE,IWEIT,IMIX,IVBU,IFL,ECM,IEV1,IEV2,IEFLOP, MASTER 2
     $          ISEM,IDEC,WTMAX,SVRT(3),ICOULU(10),IPHO,ILAM,IP17,IDDC  BBL00914
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12         ALCONS 2
      REAL RADEG, DEGRA                                                 ALCONS 3
      REAL CLGHT                                                        ALCONS 4
      INTEGER NBITW, NBYTW, LCHAR                                       ALCONS 5
      PARAMETER (PI=3.141592653589)                                     ALCONS 6
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)                          ALCONS 7
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          ALCONS 8
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         ALCONS 9
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         ALCONS10
      PARAMETER (CLGHT = 29.9792458)                                    ALCONS11
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)              ALCONS12
C                                                                       ALCONS13
      PARAMETER (LBPOS=100,NBU=521,NBD=511,NBS=531,NBC=541)             BCODES 2
      PARAMETER (NLB=5122,NXB= 5132)                                    BBL00511
C     LBPOS  = internal JETSET 7.3 code for B meson                     BCODES 3
      PARAMETER (NXB0=5232,NOB= 5332)                                   BBL007 9
C     NBU    = internal JETSET 7.3 code for Bu meson                    BCODES 4
C     NBD    = internal JETSET 7.3 code for Bd meson                    BCODES 5
C     NBS    = internal JETSET 7.3 code for Bs meson                    BCODES 6
C     NBC    = internal JETSET 7.3 code for Bc meson                    BCODES 7
C     NLB    = internal JETSET 7.3 code for /\B BARYON                  BBL00512
C     NXB    = internal JETSET 7.3 code for XIB BARYON                  BBL00513
C     NXB0   = internal JETSET 7.3 code for XIB0 BARYON                 BBL00710
C     NOB    = internal JETSET 7.3 code for OMEGAB BARYON               BBL00711
      PARAMETER (NNUEL=12,NNUMU=14,NNUTO=16,NEL=11,NMU=13,NTO=15)       LCODES 2
C     NNUEL  = internal JETSET 7.3 code for neutrino electron           LCODES 3
C     NNUMU  = internal JETSET 7.3 code for neutrino mu                 LCODES 4
C     NNUTO  = internal JETSET 7.3 code for neutrino tau                LCODES 5
C     NEL    = internal JETSET 7.3 code for electron                    LCODES 6
C     NMU    = internal JETSET 7.3 code for muon                        LCODES 7
C     NTO    = internal JETSET 7.3 code for tau                         LCODES 8
      DO 1 I=1,N7LU                                                     LUNSEM15
         KF=ABS(K7LU(I,2))                                              LUNSEM16
C  We have a Bd,Bu or Bs                                                LUNSEM17
         IF(KF.NE.NBD .AND. KF.NE.NBU .AND. KF.NE.NBS) GO TO 1          LUNSEM18
         KS=K7LU(I,1)                                                   LUNSEM19
C  It is a decayed particle                                             LUNSEM20
         IF(KS.LT.10) GO TO 1                                           LUNSEM21
C  Decay is a 3 body decay                                              LUNSEM22
         JF=K7LU(I,4)                                                   LUNSEM23
         JL=K7LU(I,5)                                                   LUNSEM24
C define helicity as unknown                                            BBL00561
         IWHELI = 99                                                    BBL00562
         NDEC = JL-JF+1                                                 BBL00563
C  Semileptonic e,mu only with neutrino first, then lepton              LUNSEM26
         NLEPN = 0                                                      LUNSEM27
         NLEPC = 0                                                      LUNSEM28
         KFNU = ABS(K7LU(JF,2))                                         LUNSEM29
         IF( KFNU.EQ.NNUEL .OR. KFNU.EQ.NNUMU) NLEPN = 1                LUNSEM30
         KFLE = ABS(K7LU(JF+1,2))                                       LUNSEM31
         IF( KFLE.EQ.NEL .OR. KFLE.EQ.NMU) NLEPC =1                     LUNSEM32
         IF (NLEPN*NLEPC.NE.1) GO TO 1                                  LUNSEM33
         IF ( NDEC.NE.3 ) GO TO 90                                      BBL00564
C                                                                       LUNSEM34
C  Q2 GENERATION                                                        LUNSEM35
C                                                                       LUNSEM36
         KD1=ABS(K7LU(JF  ,2))                                          LUNSEM37
         KD2=ABS(K7LU(JF+1,2))                                          LUNSEM38
         KD3=ABS(K7LU(JF+2,2))                                          LUNSEM39
         IDECAY=0                                                       LUNSEM40
         KHIGH = KD3/100                                                LUNSEM41
         KLOW  = MOD(KD3,10)                                            LUNSEM42
C Identify which decay from third particle                              LUNSEM43
C   KHIGH = 4 ---> D* (KLOW = 3) OR  D (KLOW = 1)    IDECAY = 1 , 2     LUNSEM44
C   KHIGH = 3 ---> K* (KLOW = 3) OR  K (KLOW = 1)    IDECAY = 5 , 6     LUNSEM45
C   KHIGH = 1,2--> RHO(KLOW = 3) OR  PI(KLOW = 1)    IDECAY = 3 , 4     LUNSEM46
         IF (KHIGH.LE.0) GO TO 90                                       BBL00565
         IF(KLOW.GT.3.AND.IMODSS.EQ.0)GOTO 90                           BBL00566
         IF(KLOW.GT.5)GOTO 90                                           BBL00567
         IF (KHIGH.LE.2) IDECAY = 3                                     LUNSEM49
         IF (KHIGH.EQ.3) IDECAY = 5                                     LUNSEM50
         IF (KHIGH.EQ.4) IDECAY = 1                                     LUNSEM51
         IF (KLOW.LE.1) IDECAY =IDECAY+1                                LUNSEM52
         IF(KLOW.EQ.5)THEN                                              AFL00125
           IMODS1=IMODSS                                                AFL00126
           IF(IMODSS.EQ.4)THEN                                          AFL00127
             XRD=RNDM()                                                 AFL00128
             IF(XRD.LT.R1)IMODSS=1                                      AFL00129
             IF(XRD.GE.R1.AND.XRD.LT.(R1+R2))IMODSS=2                   AFL00130
             IF(XRD.GE.(R1+R2).AND.XRD.LE.(R1+R2+R3))IMODSS=3           AFL00131
           ENDIF                                                        AFL00132
           IF(IMODSS.EQ.1)IDECAY=9                                      AFL00133
           IF(IMODSS.EQ.2)IDECAY=11                                     AFL00134
           IF(IMODSS.EQ.3)IDECAY=10                                     AFL00135
         ENDIF                                                          AFL00136
         IMOSIM=IMOSEM                                                  AFL00137
         IF(IDECAY.GE.9)IMOSEM=5                                        AFL00138
         IF(IDECAY.EQ.0)GOTO 90                                         BBL00568
         AM1=P7LU(I,5)                                                  LUNSEM54
         AM2=P7LU(JL,5)                                                 LUNSEM55
         IF(IMOSEM.EQ.5)THEN                                            LUNSEM56
           KD3=ABS(K7LU(JF+2,2))                                        LUNSEM57
           CALL LUIFLV (KF,IFLA,IFLB,IFLC,KSP)                          LUNSEM58
           CALL LUIFLV (KD3,IFLA1,IFLB1,IFLC1,KSP1)                     LUNSEM59
           IFLA=ABS(IFLA)                                               LUNSEM60
           IFLB=ABS(IFLB)                                               LUNSEM61
           IFLA1=ABS(IFLA1)                                             LUNSEM62
           MB=ULMASS(IFLA)                                              LUNSEM63
           MD=ULMASS(IFLB)                                              LUNSEM64
           MQ=ULMASS(IFLA1)                                             LUNSEM65
           IF(IFLA.EQ.4)MB=1.82                                         AFL00139
           IF(IFLA.EQ.5)MB=5.12                                         AFL00140
           IF(IFLB.EQ.1.OR.IFLB.EQ.2)MD=0.33                            AFL00141
           IF(IFLB.EQ.3)MD=0.55                                         AFL00142
           IF(IFLB.EQ.4)MD=1.82                                         AFL00143
           IF(IFLA1.EQ.3)MQ=0.55                                        AFL00144
           IF(IFLA1.EQ.4)MQ=1.82                                        AFL00145
           BM=AM1                                                       LUNSEM66
           BMSQ=BM**2                                                   LUNSEM67
           XMT=AM2                                                      LUNSEM68
           XMTSQ=XMT**2                                                 LUNSEM69
           BB=BBDAT(IDECAY)                                             LUNSEM70
           BX=BXDAT(IDECAY)                                             LUNSEM71
           MBOT=MB+MD                                                   LUNSEM72
           MX=MD+MQ                                                     LUNSEM73
           MUP=(MB*MQ)/(MB+MQ)                                          LUNSEM74
           MUM=(MB*MQ)/(MB-MQ)                                          LUNSEM75
           MS=MBOT-MX                                                   LUNSEM76
           MSQ=(MD**2)/(MX*MBOT)                                        LUNSEM77
           MRSQ=BMSQ/XMTSQ                                              LUNSEM78
           BBX=(BB**2+BX**2)/2.                                         LUNSEM79
           BR=BB**2/BBX                                                 LUNSEM80
           BRX=BX**2/BBX                                                LUNSEM81
           KSQ=0.7**2                                                   LUNSEM82
         ENDIF                                                          LUNSEM83
         Q2MIN=P7LU(JF+1,5)**2                                          LUNSEM84
         Q2MAX=(AM1-AM2)**2                                             LUNSEM85
         IF(KHEL.EQ.0.OR.IDECAY.NE.1)THEN                               LUNSEM86
 3         Q2=Q2MIN+RNDM(DUM)*(Q2MAX-Q2MIN)                             LUNSEM87
           QPLUS=(AM1+AM2)**2-Q2                                        LUNSEM88
           QMINS=(AM1-AM2)**2-Q2                                        LUNSEM89
           PC=SQRT(QPLUS*QMINS)/(2.*AM1)                                LUNSEM90
           IF (MOD(IDECAY,2).EQ.0)THEN                                  LUNSEM91
              XF=XDZERO(IMOSEM,IDECAY,AM1,AM2,Q2)**2*PC                 LUNSEM92
           ELSE                                                         LUNSEM93
              XF=(XHPLUS(IMOSEM,IDECAY,AM1,AM2,Q2)**2+XHMOINS(IMOSEM,   LUNSEM94
     *        IDECAY,AM1,AM2,Q2)**2+XHZERO(IMOSEM,IDECAY,AM1,AM2,Q2)**2)LUNSEM95
     *        *PC                                                       LUNSEM96
           ENDIF                                                        LUNSEM97
           XQ2RAN=RNDM(DUM)                                             LUNSEM98
           IF(XQ2RAN.GT.XF/WTM(IMOSEM,IDECAY))GOTO 3                    LUNSEM99
C                                                                       LUNSE100
C  W HELICITY GENERATION                                                LUNSE101
C                                                                       LUNSE102
           IF(MOD(IDECAY,2).EQ.0)THEN                                   LUNSE103
             IWHELI=0                                                   LUNSE104
           ELSE                                                         LUNSE105
             XHRAN=RNDM()                                               LUNSE106
             XHP=XHPLUS(IMOSEM,IDECAY,AM1,AM2,Q2)**2*PC/XF              LUNSE107
             XHM=XHMOINS(IMOSEM,IDECAY,AM1,AM2,Q2)**2*PC/XF             LUNSE108
             XHZ=XHZERO(IMOSEM,IDECAY,AM1,AM2,Q2)**2*PC/XF              LUNSE109
             IWHELI=1                                                   LUNSE110
             IF(XHRAN.GT.XHP)IWHELI=-1                                  LUNSE111
             IF(XHRAN.GT.(XHP+XHM))IWHELI=0                             LUNSE112
           ENDIF                                                        LUNSE113
         ELSE                                                           LUNSE114
           XHRAN=RNDM(DUM)                                              LUNSE115
           IF(XHRAN.LE.PRM1)IWHELI=-1                                   LUNSE116
           IF(XHRAN.GT.PRM1.AND.XHRAN.LE.PR0)IWHELI=0                   LUNSE117
           IF(XHRAN.GT.PR0)IWHELI=1                                     LUNSE118
 100       Q2=Q2MIN+RNDM(DUM)*(Q2MAX-Q2MIN)                             LUNSE119
           QPLUS=(AM1+AM2)**2-Q2                                        LUNSE120
           QMINS=(AM1-AM2)**2-Q2                                        LUNSE121
           PC=SQRT(QPLUS*QMINS)/(2.*AM1)                                LUNSE122
           IF(IWHELI.EQ.-1)XF=XHMOINS(IMOSEM,IDECAY,AM1,AM2,Q2)**2*PC   LUNSE123
           IF(IWHELI.EQ.0)XF=XHZERO(IMOSEM,IDECAY,AM1,AM2,Q2)**2*PC     LUNSE124
           IF(IWHELI.EQ.1)XF=XHPLUS(IMOSEM,IDECAY,AM1,AM2,Q2)**2*PC     LUNSE125
           XQ2RAN=RNDM(DUM)                                             LUNSE126
           IF(XQ2RAN.GT.XF/WTM(IMOSEM,IDECAY))GOTO 100                  LUNSE127
         ENDIF                                                          LUNSE128
         IF(IDECAY.GE.9)THEN                                            AFL00146
           IMOSEM=IMOSIM                                                AFL00147
           IMODSS=IMODS1                                                AFL00148
         ENDIF                                                          AFL00149
C                                                                       LUNSE129
C  BOOST BACK B DECAY PRODUCES AT REST                                  LUNSE130
C                                                                       LUNSE131
         BXB=-P7LU(I,1)/P7LU(I,4)                                       LUNSE132
         BYB=-P7LU(I,2)/P7LU(I,4)                                       LUNSE133
         BZB=-P7LU(I,3)/P7LU(I,4)                                       LUNSE134
         MSTU(1)=JF                                                     LUNSE135
         MSTU(2)=JL                                                     LUNSE136
         PX3=P7LU(JL,1)                                                 LUNSE137
         PY3=P7LU(JL,2)                                                 LUNSE138
         PZ3=P7LU(JL,3)                                                 LUNSE139
         E3=P7LU(JL,4)                                                  LUNSE140
         CALL LUROBO(0.,0.,BXB,BYB,BZB)                                 LUNSE141
         COTH3=-1.+2*RNDM(DUM)                                          LUNSE142
         THE3=ACOS(COTH3)                                               LUNSE143
         THEW=PI-THE3                                                   LUNSE144
         PHI3=2.*PI*RNDM(DUM)                                           LUNSE145
         PHIW=PI+PHI3                                                   LUNSE146
         P7LU(JL,1)=PC*SIN(THE3)*COS(PHI3)                              LUNSE147
         P7LU(JL,2)=PC*SIN(THE3)*SIN(PHI3)                              LUNSE148
         P7LU(JL,3)=PC*COS(THE3)                                        LUNSE149
         P7LU(JL,4)=SQRT(PC**2+P7LU(JL,5)**2)                           LUNSE150
         PXW=-P7LU(JL,1)                                                LUNSE151
         PYW=-P7LU(JL,2)                                                LUNSE152
         PZW=-P7LU(JL,3)                                                LUNSE153
         EW=SQRT(Q2+PC**2)                                              LUNSE154
C                                                                       LUNSE155
C  GENERATION OF LEPTONIC W DECAY                                       LUNSE156
C                                                                       LUNSE157
         DMAX=1.                                                        LUNSE158
         IF(IWHELI.EQ.0)DMAX=0.5                                        LUNSE159
 4       THELEP=ACOS(-1.+2.*RNDM())*180./PI                             LUNSE160
         RTHLEP=RNDM()                                                  LUNSE161
         IF(RTHLEP.GT.DSMALL(1.,FLOAT(IWHELI),-1.,THELEP)**2/DMAX)GOTO 4LUNSE162
         THELEP=THELEP*PI/180.                                          LUNSE163
         PHILEP=2.*PI*RNDM()                                            LUNSE164
         PLEP=(Q2-Q2MIN)/(2.*SQRT(Q2))                                  LUNSE165
         P7LU(JF+1,1)=PLEP*SIN(THELEP)*COS(PHILEP)                      LUNSE166
         P7LU(JF+1,2)=PLEP*SIN(THELEP)*SIN(PHILEP)                      LUNSE167
         P7LU(JF+1,3)=PLEP*COS(THELEP)                                  LUNSE168
         P7LU(JF+1,4)=SQRT(P7LU(JF+1,1)**2+P7LU(JF+1,2)**2              LUNSE169
     *   +P7LU(JF+1,3)**2+P7LU(JF+1,5)**2)                              LUNSE170
         P7LU(JF,1)=-P7LU(JF+1,1)                                       LUNSE171
         P7LU(JF,2)=-P7LU(JF+1,2)                                       LUNSE172
         P7LU(JF,3)=-P7LU(JF+1,3)                                       LUNSE173
         P7LU(JF,4)=SQRT(P7LU(JF,1)**2+P7LU(JF,2)**2                    LUNSE174
     *   +P7LU(JF,3)**2+P7LU(JF,5)**2)                                  LUNSE175
         MSTU(2)=JF+1                                                   LUNSE176
         CALL LUROBO(0.,0.,0.,0.,PC/EW)                                 LUNSE177
         CALL LUROBO(THEW,PHIW,0.,0.,0.)                                LUNSE178
         CALL HFILL(10020,P7LU(JF+1,4),0.,1.)                           LUNSE179
         MSTU(2)=JL                                                     LUNSE180
         CALL LUROBO(0.,0.,-BXB,-BYB,-BZB)                              LUNSE181
C LEPTON ENERGY AFTER BOOST.                                            LUNSE182
         CALL HFILL(10021,P7LU(JF+1,4),0.,1.)                           LUNSE183
         MSTU(1)=1                                                      LUNSE184
         MSTU(2)=N7LU                                                   LUNSE185
C                                                                       LUNSE186
C  KILL THE DECAY PRODUCTS OF B DAUGHTERS                               LUNSE187
C                                                                       LUNSE188
         NPARI=N7LU                                                     LUNSE189
         N7LU=JL                                                        LUNSE190
         DO 5 IGARB0=JF,JL                                              LUNSE191
           KST = K7LU(IGARB0,1)                                         LUNSE192
           IF ( KST.GT.10 .AND. KST.LT.20 ) THEN                        LUNSE193
              K7LU(IGARB0,1) = KST-10                                   LUNSE194
              K7LU(IGARB0,4)=0                                          LUNSE195
              K7LU(IGARB0,5)=0                                          LUNSE196
           ELSE                                                         LUNSE197
              GO TO 5                                                   LUNSE198
           ENDIF                                                        LUNSE199
C  Identify in the remaining list the successive daughters              LUNSE200
         DO 6 IGARB=JL+1,NPARI                                          LUNSE201
           IGARB1=IGARB                                                 LUNSE202
 7         IGARB1=K7LU(IGARB1,3)                                        LUNSE203
           IF(IGARB1.EQ.IGARB0.OR.IGARB1.EQ.1000)THEN                   LUNSE204
              K7LU(IGARB,3)=1000                                        LUNSE205
              K7LU(IGARB,4)=0                                           LUNSE206
              K7LU(IGARB,5)=0                                           LUNSE207
              GOTO 6                                                    LUNSE208
           ENDIF                                                        LUNSE209
           IF(IGARB1.LT.IGARB0)THEN                                     LUNSE210
              GOTO 6                                                    LUNSE211
           ELSE                                                         LUNSE212
              GOTO 7                                                    LUNSE213
           ENDIF                                                        LUNSE214
 6       CONTINUE                                                       LUNSE215
 5       CONTINUE                                                       LUNSE216
C   Compress the remaining entries                                      LUNSE217
         IMOT0 = -1                                                     LUNSE218
         DO 8 IGARB=JL+1,NPARI                                          LUNSE219
            IF(K7LU(IGARB,3).EQ.1000)GOTO 8                             LUNSE220
            N7LU=N7LU+1                                                 LUNSE221
            K7LU(N7LU,1)=K7LU(IGARB,1)                                  LUNSE222
            K7LU(N7LU,2)=K7LU(IGARB,2)                                  LUNSE223
            K7LU(N7LU,3)=K7LU(IGARB,3)                                  LUNSE224
            K7LU(N7LU,4)=K7LU(IGARB,4)                                  LUNSE225
            K7LU(N7LU,5)=K7LU(IGARB,5)                                  LUNSE226
            DO 9 IGARB2=1,5                                             LUNSE227
              P7LU(N7LU,IGARB2)=P7LU(IGARB,IGARB2)                      LUNSE228
 9          CONTINUE                                                    LUNSE229
C   Update current daughter number for the mother                       LUNSE230
            IMOTH = K7LU(N7LU,3)                                        LUNSE231
            IF (IMOTH.EQ.IMOT0) GOTO 10                                 LUNSE232
              IMOT0 = IMOTH                                             LUNSE233
              K7LU(IMOTH,4) = K7LU(IMOTH,4)+N7LU-IGARB                  LUNSE234
              K7LU(IMOTH,5) = K7LU(IMOTH,5)+N7LU-IGARB                  LUNSE235
 10         CONTINUE                                                    LUNSE236
C   Update current mother number for the daughters                      LUNSE237
            IF (K7LU(IGARB,5).LE.0) GOTO 8                              LUNSE238
            DO 11 IGARB2= K7LU(IGARB,4),K7LU(IGARB,5)                   LUNSE239
               K7LU(IGARB2,3)=N7LU                                      LUNSE240
 11         CONTINUE                                                    LUNSE241
 8       CONTINUE                                                       LUNSE242
         MSTU(2)=N7LU                                                   LUNSE243
C   Apply intern Brem if needed                                         BBL00569
 90      CONTINUE                                                       BBL00570
         IF(IPHO.GT.0) CALL LUNPHO(-I)                                  BBL00571
         IF ( IWHELI.EQ.99) GO TO 1                                     BBL00572
         LF=K7LU(I,4)                                                   BBL00573
         LL=K7LU(I,5)                                                   BBL00574
         DO 14 JL = LF,LL                                               BBL00575
         IFLAV3=ABS(K7LU(JL,2))                                         BBL00576
         IF ( IFLAV3.LE.100) GO TO 14                                   BBL00577
         CALL LUIFLV(IFLAV3,IFLA3,IFLB3,IFLC3,KSP3)                     LUNSE245
C                                                                       LUNSE246
C  PRODUCE ANGULAR DISTRIBUTION FOR VECTOR --> PSEUDO. + PSEUDO.        LUNSE247
C  DECAY D* , K* AND RHO  , THEN MODIFY ANGULAR DISTRIBUTION            BBL00578
         IF(IABS(KSP3).EQ.1)THEN                                        LUNSE249
            N7LU1=N7LU                                                  LUNSE250
            CALL LUDECY(JL)                                             LUNSE251
            DNPAR=N7LU-N7LU1                                            LUNSE252
            IF(DNPAR.NE.2)GOTO 12                                       LUNSE253
            KFA1=ABS(K7LU(N7LU1+1,2))                                   LUNSE254
            KFA2=ABS(K7LU(N7LU1+2,2))                                   LUNSE255
            CALL LUIFLV(KFA1,IFLA1,IFLB1,IFLC1,KSP1)                    LUNSE256
            CALL LUIFLV(KFA2,IFLA2,IFLB2,IFLC2,KSP2)                    LUNSE257
            IF(KSP1.NE.0.OR.KSP2.NE.0)GOTO 12                           LUNSE258
            BXB=-P7LU(JL,1)/P7LU(JL,4)                                  LUNSE259
            BYB=-P7LU(JL,2)/P7LU(JL,4)                                  LUNSE260
            BZB=-P7LU(JL,3)/P7LU(JL,4)                                  LUNSE261
            MSTU(1)=N7LU1+1                                             LUNSE262
            MSTU(2)=N7LU                                                LUNSE263
            CALL LUROBO(0.,0.,BXB,BYB,BZB)                              LUNSE264
            PHID=2*PI*RNDM(DUM)                                         LUNSE265
 13         COTHED=-1.+2.*RNDM(DUM)                                     LUNSE266
            THED=ACOS(COTHED)*180./PI                                   LUNSE267
            IF(RNDM(DUM).GT.DSMALL(1.,FLOAT(IWHELI),0.,THED)**2)GOTO 13 LUNSE268
            THED=THED*PI/180.                                           LUNSE269
            PD=SQRT(P7LU(N7LU1+1,1)**2+P7LU(N7LU1+1,2)**2               LUNSE270
     *      +P7LU(N7LU1+1,3)**2)                                        LUNSE271
            P7LU(N7LU1+1,1)=PD*SIN(THED)*COS(PHID)                      LUNSE272
            P7LU(N7LU1+1,2)=PD*SIN(THED)*SIN(PHID)                      LUNSE273
            P7LU(N7LU1+1,3)=PD*COS(THED)                                LUNSE274
            P7LU(N7LU,1)=-P7LU(N7LU1+1,1)                               LUNSE275
            P7LU(N7LU,2)=-P7LU(N7LU1+1,2)                               LUNSE276
            P7LU(N7LU,3)=-P7LU(N7LU1+1,3)                               LUNSE277
            BB=SQRT(BXB**2+BYB**2+BZB**2)                               LUNSE278
            CALL LUROBO(0.,0.,0.,0.,BB)                                 LUNSE279
            PHIDST=ACOS(-BXB/SQRT(BXB**2+BYB**2))                       LUNSE280
            IF(-BYB.LT.0.)PHIDST=2*PI-PHIDST                            LUNSE281
            THEDST=ACOS(-BZB/BB)                                        LUNSE282
            CALL LUROBO(THEDST,PHIDST,0.,0.,0.)                         LUNSE283
 12         CONTINUE                                                    LUNSE284
            MSTU(1)=1                                                   BBL00579
            MSTU(2)=N7LU                                                LUNSE285
         ENDIF                                                          LUNSE286
 14      CONTINUE                                                       BBL00580
 1    CONTINUE                                                          LUNSE288
      RETURN                                                            LUNSE289
      END                                                               LUNSE290
      SUBROUTINE LUNVBU(IFVB)                                           LUNVBU 2
C------------------------------------------------------------------     LUNVBU 3
C! Include  VBU transitions if any                                      LUNVBU 4
C  B.Bloch November 90                                                  LUNVBU 5
C   IFVB : Vbu transition flag  0= no Vbu occured , 1= Vbu on initial B LUNVBU 6
C                               2= Vbu on initial Bbar, 3= Vbu on both  LUNVBU 7
C------------------------------------------------------------------     LUNVBU 8
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
C                                                                       LUN7CO12
      PARAMETER (LBPOS=100,NBU=521,NBD=511,NBS=531,NBC=541)             BCODES 2
      PARAMETER (NLB=5122,NXB= 5132)                                    BBL00511
C     LBPOS  = internal JETSET 7.3 code for B meson                     BCODES 3
      PARAMETER (NXB0=5232,NOB= 5332)                                   BBL007 9
C     NBU    = internal JETSET 7.3 code for Bu meson                    BCODES 4
C     NBD    = internal JETSET 7.3 code for Bd meson                    BCODES 5
C     NBS    = internal JETSET 7.3 code for Bs meson                    BCODES 6
C     NBC    = internal JETSET 7.3 code for Bc meson                    BCODES 7
C     NLB    = internal JETSET 7.3 code for /\B BARYON                  BBL00512
C     NXB    = internal JETSET 7.3 code for XIB BARYON                  BBL00513
C     NXB0   = internal JETSET 7.3 code for XIB0 BARYON                 BBL00710
C     NOB    = internal JETSET 7.3 code for OMEGAB BARYON               BBL00711
      COMMON/VBUCOM/ IMATBU,PRBUBC,PARDBU,ITYPBU                        VBUCOM 3
C Introduce Vbu transitions if needed for non yet decayed B meson       BBL00736
      IFVB = 0                                                          LUNVBU12
      DO 102 I=1,N7LU                                                   LUNVBU13
       IF(K7LU(I,1).LT.10)THEN                                          LUNVBU14
       KF=ABS(K7LU(I,2))                                                BBL00737
       IF ( KF.EQ.NBU .OR. KF.EQ.NBD .OR. KF.EQ.NBS .OR. KF.EQ.NBC) THENBBL00738
        IF(RNDM(DUM).LT.PRBUBC)THEN                                     LUNVBU16
          CALL VBCVBU                                                   LUNVBU17
          CALL LUDECY(I)                                                LUNVBU18
          CALL VBUVBC                                                   LUNVBU19
          IF (K7LU(I,2).GT.0) THEN                                      LUNVBU20
              IFVB = IFVB +1                                            LUNVBU21
          ELSEIF (K7LU(I,2).LT.0) THEN                                  LUNVBU22
              IFVB = IFVB +2                                            LUNVBU23
          ENDIF                                                         LUNVBU24
        ENDIF                                                           LUNVBU25
       ENDIF                                                            LUNVBU26
       ENDIF                                                            LUNVBU27
 102  CONTINUE                                                          LUNVBU28
      RETURN                                                            LUNVBU29
      END                                                               LUNVBU30
      SUBROUTINE LUZETA(ZB)                                             LUZETA 2
C------------------------------------------------------------------     LUZETA 3
C! Transfer the content of fragmention information  to ZB array         LUZETA 4
C    B.Bloch-Devaux January   1991                                      LUZETA 5
C---------------------------------------------------------------------- LUZETA 6
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
C                                                                       LUN7CO12
      DIMENSION ZB(LJNPAR)                                              LUZETA 8
      IF (MSTU(90).GT.0) THEN                                           LUZETA 9
         DO 10 I = 1,MSTU(90)                                           LUZETA10
         J = MSTU(91+I-1)                                               LUZETA11
 10      ZB(J) = PARU(91+I-1)                                           LUZETA12
      ENDIF                                                             LUZETA13
      RETURN                                                            LUZETA14
      END                                                               LUZETA15
      REAL FUNCTION MIDECL(P,AM,T0,IOSCI)                               MIDECL 2
C---------------------------------------------------------------------  MIDECL 3
C  GENERATES THE CORRECT DECAY LENGTH FOR MIXING      881024            MIDECL 4
C  AUTHOR: A. FALVARD                                                   MIDECL 5
C             INPUTS:                                                   MIDECL 6
C                    -P            :MOMENTUM                            MIDECL 7
C                    -AM           :MASS                                MIDECL 8
C                    -T0           :INTRINSEQ LIFE TIME                 MIDECL 9
C                    -IOSCI        :STATUS OF THE PARTICLE VERSUS MIXINGMIDECL10
C                                   LOOK AT KBOSCI FOR DEFINITION       MIDECL11
C---------------------------------------------------------------------- MIDECL12
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12         ALCONS 2
      REAL RADEG, DEGRA                                                 ALCONS 3
      REAL CLGHT                                                        ALCONS 4
      INTEGER NBITW, NBYTW, LCHAR                                       ALCONS 5
      PARAMETER (PI=3.141592653589)                                     ALCONS 6
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)                          ALCONS 7
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)          ALCONS 8
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)                         ALCONS 9
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)                         ALCONS10
      PARAMETER (CLGHT = 29.9792458)                                    ALCONS11
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)              ALCONS12
C                                                                       ALCONS13
      COMPLEX PSQD,PSQS                                                 MIXING 2
      COMMON/ MIXING /XD,YD,CHID,RD,XS,YS,CHIS,RS,PSQD,PSQS             MIXING 3
      COMMON/ PROBMI /CDCPBA,CDCPB,CSCPBA,CSCPB                         MIXING 4
      PARAMETER (CLITS = CLGHT * 1.E+09)                                MIDECL15
      EXTERNAL RNDM                                                     MIDECL16
      X=XD                                                              MIDECL17
      Y=YD                                                              MIDECL18
      IF(IOSCI.EQ.3.OR.IOSCI.EQ.4)THEN                                  MIDECL19
      X=XS                                                              MIDECL20
      Y=YS                                                              MIDECL21
      ENDIF                                                             MIDECL22
 1    Z=RNDM(DUM)                                                       MIDECL23
      IF(Z.EQ.0.)Z=1.                                                   MIDECL24
      AL=ALOG(Z)                                                        MIDECL25
      MIDECL=-T0*AL                                                     MIDECL26
C      SIGN=1.                                                          MIDECL27
C      IF(IOSCI.EQ.2.OR.IOSCI.EQ.4)SIGN=-1.                             MIDECL28
      II= MOD(IOSCI,2)                                                  MIDECL29
      FCON=COSH(-Y*AL)+(2*II-1)*COS(-X*AL)                              MIDECL30
      F1MAX=1.+COSH(14.*Y)                                              MIDECL31
      IF(F1MAX*RNDM(DUM).GT.FCON)GOTO 1                                 MIDECL32
      CALL HFILL(10009+IOSCI,-AL,0.,1.)                                 MIDECL33
      MIDECL=P*MIDECL/AM*CLITS                                          MIDECL34
      RETURN                                                            MIDECL35
      END                                                               MIDECL36
      SUBROUTINE PHOCAL(IMOD)                                           PHOCAL 2
C...Purpose: call photos and check if any extra photon added            PHOCAL 3
      PARAMETER (NMXHEP=2000)                                           HEPEVT 2
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),           HEPEVT 3
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)   HEPEVT 4
      IMOD = 0                                                          PHOCAL 5
      NHEPI = NHEP                                                      PHOCAL 6
      CALL PHOTOS(1)                                                    PHOCAL 7
      NHEPF = NHEP                                                      PHOCAL 8
      IMOD=NHEPF-NHEPI                                                  PHOCAL 9
      RETURN                                                            PHOCAL10
      END                                                               PHOCAL11
      SUBROUTINE QQTOPS (BQP,BQM,ITYP2,IZLU)                            QQTOPS 2
C-----------------------------------------------------------------------QQTOPS 3
C   B.Bloch-Devaux April 1991                                           QQTOPS 4
C                                                                       QQTOPS 5
C        jetset 7.3 parton shower INTERFACE                             QQTOPS 6
C                                                                       QQTOPS 7
C        SET UP THE LUND COMMON   WITH NEXT POSITIONS                   QQTOPS 8
C        N+1    ANTIFERMION   }                    ( BQP)               QQTOPS 9
C        N+2    FERMION       }   OF FLAVOR ITYP2  ( BQM)               QQTOPS10
C       ITYP2   1=E, 2=MU, 3=TAU, 11=d, 12=u, 13=s, 14=c, 15=b, 16=t    QQTOPS11
C        IZLU   is the mother of the fermion pair                       QQTOPS12
C          THEN following particles are those after parton shower evol. QQTOPS13
C          the original fermions are artificially kept in the two above QQTOPS14
C          lines.                                                       QQTOPS15
C                                                                       QQTOPS16
C-----------------------------------------------------------------------QQTOPS17
      DIMENSION BQP(4),BQM(4),Z(4),IJOIN(2)                             QQTOPS18
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
C                                                                       LUN7CO12
      DATA NEV/0/                                                       QQTOPS20
C                                                                       QQTOPS21
       IF(ITYP2.LT.10) THEN                                             QQTOPS22
C                                                                       QQTOPS23
C        THESE ARE LEPTONS  NO SHOWER  LINES NEEDED                     QQTOPS24
C          THIS REPRODUCES THE LUND7.3 LEPTON FLAVOR CODE               QQTOPS25
C                                                                       QQTOPS26
         KMASS=9+2*ITYP2                                                QQTOPS27
         K7LU(N7LU+1,1)= 1                                              QQTOPS28
         K7LU(N7LU+2,1)= 1                                              QQTOPS29
         K7LU(N7LU+1,3)= IZLU                                           QQTOPS30
         K7LU(N7LU+2,3)= IZLU                                           QQTOPS31
         K7LU(N7LU+1,2)= -KMASS                                         QQTOPS32
         K7LU(N7LU+2,2)= KMASS                                          QQTOPS33
         P7LU(N7LU+1,5)= ULMASS(KMASS)                                  QQTOPS34
         P7LU(N7LU+2,5)= ULMASS(KMASS)                                  QQTOPS35
         DO 40 I=1,4                                                    QQTOPS36
           P7LU(N7LU+1,I)=BQP(I)                                        QQTOPS37
           P7LU(N7LU+2,I)=BQM(I)                                        QQTOPS38
   40    CONTINUE                                                       QQTOPS39
         N7LU=N7LU+2                                                    QQTOPS40
         CALL LUEXEC                                                    QQTOPS41
      ELSE                                                              QQTOPS42
C                                                                       QQTOPS43
C     QUARKS                                                            QQTOPS44
C                                                                       QQTOPS45
                                                                        QQTOPS46
         KMASS=ITYP2 -10                                                QQTOPS47
C  now the quarks for shower lines                                      QQTOPS48
         K7LU(N7LU+1,1)= 3                                              QQTOPS49
         K7LU(N7LU+1,2)=-KMASS                                          QQTOPS50
         K7LU(N7LU+1,3)= IZLU                                           QQTOPS51
         K7LU(N7LU+2,1)= 3                                              QQTOPS52
         K7LU(N7LU+2,2)= KMASS                                          QQTOPS53
         K7LU(N7LU+2,3)= IZLU                                           QQTOPS54
         P7LU(N7LU+1,5)=ULMASS(KMASS)                                   QQTOPS55
         P7LU(N7LU+2,5)=ULMASS(KMASS)                                   QQTOPS56
         DO 140 I=1,4                                                   QQTOPS57
           P7LU(N7LU+1,I)=BQP(I)                                        QQTOPS58
           P7LU(N7LU+2,I)=BQM(I)                                        QQTOPS59
  140    CONTINUE                                                       QQTOPS60
C                                                                       QQTOPS61
C   Connect the partons in  a string configuration                      QQTOPS62
C                                                                       QQTOPS63
         NJOIN = 2                                                      QQTOPS64
         IJOIN(1) = N7LU+1                                              QQTOPS65
         IJOIN(2) = N7LU+2                                              QQTOPS66
         N7LU=N7LU+2                                                    QQTOPS67
         CALL LUJOIN( NJOIN,IJOIN)                                      QQTOPS68
C                                                                       QQTOPS69
C    CALCULATE THE QQBAR MASS                                           QQTOPS70
C                                                                       QQTOPS71
         N=N7LU                                                         QQTOPS72
         DO 150 I=1,4                                                   QQTOPS73
           Z(I)=BQP(I)+BQM(I)                                           QQTOPS74
  150    CONTINUE                                                       QQTOPS75
         QMAX= SQRT( Z(4)**2- Z(3)**2-Z(2)**2-Z(1)**2 )                 QQTOPS76
C                                                                       QQTOPS77
C         GENERATE PARTON SHOWER                                        QQTOPS78
C                                                                       QQTOPS79
         IF( NEV.LE. 5)CALL LULIST(1)                                   QQTOPS80
         CALL LUSHOW(N,N-1,QMAX)                                        QQTOPS81
         IF (MSTJ(105).GT.0) CALL LUEXEC                                QQTOPS82
C                                                                       QQTOPS83
C                                                                       QQTOPS84
      ENDIF                                                             QQTOPS85
C                                                                       QQTOPS86
C   Now check if the initial electron was along positive Z axis.        QQTOPS87
C   If not reverse Px and Pz components for all particles               QQTOPS88
C                                                                       QQTOPS89
      IF ( P7LU(1,3)*K7LU(1,2).LT.0..AND. ABS(K7LU(1,2)).EQ.11) THEN    QQTOPS90
         DO 201 IP=1,N7LU                                               QQTOPS91
         DO 202 JJ=1,3,2                                                QQTOPS92
 202       P7LU(IP,JJ) = -P7LU(IP,JJ)                                   QQTOPS93
 201     CONTINUE                                                       QQTOPS94
      ENDIF                                                             QQTOPS95
      NEV=NEV+1                                                         QQTOPS96
      IF(NEV.LE. 5)CALL LULIST(1)                                       QQTOPS97
      RETURN                                                            QQTOPS98
      END                                                               QQTOPS99
      SUBROUTINE SIGENE(IDPR,ISTAT,ECMS)                                SIGENE 2
C-------------------------------------------------------------          SIGENE 3
C! Generate single particle and fill lund common with it                SIGENE 4
C  B. Bloch November 90 for JETSET 7.3 version                          SIGENE 5
C--------------------------------------------------------------         SIGENE 6
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
C                                                                       LUN7CO12
      COMMON/SINGEN/ITYPSI,PMINSI,PMAXSI,COMISI,COMASI,PHMISI,PHMASI    SIGENE 8
      DATA NIT /0/                                                      SIGENE 9
      NIT = NIT + 1                                                     SIGENE10
      PMOD = PMINSI + (PMAXSI-PMINSI)*RNDM(DUM)                         SIGENE11
      COST = COMISI + (COMASI-COMISI)*RNDM(DUM)                         SIGENE12
      PHI  = PHMISI + (PHMASI-PHMISI)*RNDM(DUM)                         SIGENE13
      SINT = SQRT(1.-COST*COST)                                         SIGENE14
      XMAS = ULMASS(ITYPSI)                                             SIGENE15
      N7LU = 1                                                          SIGENE16
      K7LU(1,1) = 1                                                     SIGENE17
      K7LU(1,2) = ITYPSI                                                SIGENE18
      K7LU(1,3) = 0                                                     SIGENE19
      K7LU(1,4) = 0                                                     SIGENE20
      K7LU(1,5) = 0                                                     SIGENE21
      P7LU(1,1) = PMOD*COS(PHI)*SINT                                    SIGENE22
      P7LU(1,2) = PMOD*SIN(PHI)*SINT                                    SIGENE23
      P7LU(1,3) = PMOD*COST                                             SIGENE24
      P7LU(1,4) = SQRT(PMOD*PMOD+XMAS*XMAS)                             SIGENE25
      P7LU(1,5) = XMAS                                                  SIGENE26
      IDPR = ITYPSI*1000                                                SIGENE27
      ISTAT = 0                                                         SIGENE28
      ECMS = PMOD                                                       SIGENE29
      IF( NIT.LE.5 ) CALL LULIST(1)                                     SIGENE30
      RETURN                                                            SIGENE31
      END                                                               SIGENE32
      SUBROUTINE USCJOB                                                 USCJOB 2
C-------------------------------------------------------------          USCJOB 3
C! Termination routine JETSET 7.3 version                               USCJOB 4
C--------------------------------------------------------------         USCJOB 5
      COMMON / MASTER / IGENE,IWEIT,IMIX,IVBU,IFL,ECM,IEV1,IEV2,IEFLOP, MASTER 2
     $          ISEM,IDEC,WTMAX,SVRT(3),ICOULU(10),IPHO,ILAM,IP17,IDDC  BBL00914
      COMMON/HVFPRNT/IPPRI                                              BBL00828
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
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
C                                                                       LUN7CO12
      COMMON /CONST/ ALFA,PI,ALFA1                                      DYMUCOM2
      COMMON /RUNPAR/ SOLD,ID2,ID3,FINEXP,POIDS,INTERF,XK0              DYMUCOM3
      COMMON /WEAK/ AEL,AMU,AMZ,GAMM,SW2,CA2,CV2,CA2CV2,COL,T3,QI       DYMUCOM4
      COMMON /BEAM/ S0,EBEAM                                            DYMUCOM5
      COMMON /TAU / TAUV,CPTAU,HEL,PITAU(4)                             DYMUCOM6
      COMMON/WEAKQ/WEAKC(11,6),XSECT(6),XTOT                            DYMUCOM7
      COMMON/ZDUMP / IDEBU , NEVT                                       DYMUCOM8
      COMMON/RESULT/ SIGBOR,SIGTOT,ERRSIG,ASYTOT,ERRASY                 DYMUCOM9
C.......................................................................USCJOB10
       IUT=IW(6)                                                        USCJOB11
       WRITE(IUT,101)                                                   USCJOB12
  101  FORMAT(//20X,'EVENTS STATISTICS',                                USCJOB13
     &         /20X,'*****************')                                USCJOB14
       WRITE(IUT,102) ICOULU(9)+ICOULU(10),ICOULU(10),ICOULU(9)         USCJOB15
  102  FORMAT(/5X,'# OF GENERATED EVENTS                = ',I10,        USCJOB16
     &        /5X,'# OF ACCEPTED  EVENTS                = ',I10,        USCJOB17
     &        /5X,'# OF REJECTED  EVENTS                = ',I10)        USCJOB18
       WRITE(IUT,103)                                                   USCJOB19
  103  FORMAT(//20X,'REJECT STATISTICS',                                USCJOB20
     &         /20X,'*****************')                                USCJOB21
       WRITE(IUT,104) (ICOULU(I),I=1,8)                                 USCJOB22
  104  FORMAT(/10X,'IR= 1 LUND ERROR unknown part    # OF REJECT =',I10,USCJOB23
     &        /10X,'IR= 2 BOS  ERROR KINE/VERT       # OF REJECT =',I10,USCJOB24
     &        /10X,'IR= 3 LUND ERROR too many tracks # OF REJECT =',I10,USCJOB25
     &        /10X,'IR= 4 LUND ERROR Beam wrong pos  # OF REJECT =',I10,USCJOB26
     &        /10X,'IR= 5 LUND ERROR status code >5  # OF REJECT =',I10,USCJOB27
     &        /10X,'IR= 6 USER reject in BRGENE      # OF REJECT =',I10,USCJOB28
     &        /10X,'IR= 7 BOS ERROR KZFR bank        # OF REJECT =',I10,USCJOB29
     &        /10X,'IR= 8 User selected decay chain  # OF REJECT =',I10)BBL00971
      IF (IGENE.EQ.2 .OR. IGENE.EQ.12) THEN                             BBL00411
        CALL BREMND                                                     BBL00412
      ELSEIF (IGENE.EQ.3 .OR. IGENE.EQ.13) THEN                         BBL00413
        CALL DYMUND                                                     BBL00414
        IF ( XTOT.NE.0.) WRITE(IUT,106) XTOT                            USCJOB35
  106   FORMAT (/10X,' CORRESPONDING TOTAL CROSS SECTION (NANOBARN):',  USCJOB36
     $  F12.4)                                                          USCJOB37
      ELSEIF (IGENE.EQ.1.OR.IGENE.EQ.11) THEN                           BBL00415
        WRITE (IUT,105) (PARJ(II),II=141,148)                           USCJOB39
 105    FORMAT(//,20X,'FINAL RESULTS FROM LUND GENERATION',             USCJOB40
     $          /,20X,'**********************************',             USCJOB41
     $          /,10X,'R value as given in massless QED   :',F10.5,     USCJOB42
     $          /,10X,'R value including weak effects     :',F10.5,     USCJOB43
     $          /,10X,'R value including QCD corrections  :',F10.5,     USCJOB44
     $          /,10X,'R value including I.S.R. effects   :',F10.5,     USCJOB45
     $          /,10X,'Absolute cross sections in nb      :',           USCJOB46
     $          /,10X,'As given  in massless QED   :',F10.5,            USCJOB47
     $          /,10X,'Including  weak effects     :',F10.5,            USCJOB48
     $          /,10X,'Including  QCD corrections  :',F10.5,            USCJOB49
     $          /,10X,'Including  I.S.R. effects   :',F10.5)            USCJOB50
      ENDIF                                                             USCJOB51
      IF ( IDEC+IDDC.GT.0) CALL DECSTA                                  BBL00972
      IF ( IPPRI/10 .GT.0) THEN                                         BBL00829
         CALL LUTABU(12)                                                BBL00830
         CALL LUTABU(22)                                                BBL00831
         IF (IGENE.EQ.-1) CALL LUTABU(52)                               BBL00832
      ENDIF                                                             BBL00833
      RETURN                                                            USCJOB53
      END                                                               USCJOB54
      FUNCTION XDZERO (IMODEL,J,M1,M2,Q2)                               XDZERO 2
C---------------------------------------------------------------------- XDZERO 3
C     A. FALVARD   -   130389                                           XDZERO 4
C                                                                       XDZERO 5
C! H0 HELICITY AMPLITUDE FOR B OR D --> EL + NEUT + PSEUDOSCALAR        XDZERO 6
C---------------------------------------------------------------------  XDZERO 7
      REAL M1,M2                                                        XDZERO 8
      PARAMETER (NMODEL=5,NDECAY=11)                                    AFL001 1
      COMMON/SEMLEP/IMOSEM,WTM(NMODEL,NDECAY),IHEL,KHEL,PRM1,PR0,PRP1,  SEMLEP 3
     $          BBDAT(NDECAY),BXDAT(NDECAY)  ,                          SEMLEP 4
     $          NO1(NDECAY),NO2(NDECAY),OVER(NMODEL,NDECAY)             SEMLEP 5
     $          , IMODSS,R1,R2,R3                                       AFL001 2
      REAL MFF,MFF2,MFF3,MFF4                                           SEMLEP 6
      COMMON/MFFX/MFF(NMODEL,NDECAY),MFF2(NMODEL,NDECAY),               SEMLEP 7
     $            MFF3(NMODEL,NDECAY),MFF4(NMODEL,NDECAY)               SEMLEP 8
      REAL MB,MD,MQ,MBOT,MX,MUP,MUM,MS,MSQ,MRSQ,KSQ                     SEMLEP 9
      COMMON /SCORA/MB,MD,MQ,MBOT,MX,MUP,MUM,MS,MSQ,MRSQ,KSQ            SEMLEP10
     *,BM,BMSQ,XMTSQ,BB,BX,XMT,BBX,BR,BRX                               SEMLEP11
      COMMON /SCOROU/AL,CP,Q,QV                                         SEMLEP12
      XDZERO=0.                                                         XDZERO10
      T=Q2                                                              XDZERO11
      Q2MAX = (M1-M2)**2                                                XDZERO12
      IF(IMODEL.LE.4)THEN                                               XDZERO13
         VM=MFF(IMODEL,J)**2/(MFF(IMODEL,J)**2-Q2)                      XDZERO14
         VM2=MFF2(IMODEL,J)**2/(MFF2(IMODEL,J)**2-Q2)                   XDZERO15
         VM3=MFF3(IMODEL,J)**2/(MFF3(IMODEL,J)**2-Q2)                   XDZERO16
         VM4=MFF4(IMODEL,J)**2/(MFF4(IMODEL,J)**2-Q2)                   XDZERO17
         OVER1=OVER(IMODEL,J)                                           XDZERO18
      ENDIF                                                             XDZERO19
      PI=4.*ATAN(1.)                                                    XDZERO20
      QPLUS=(M1+M2)**2-Q2                                               XDZERO21
      QMINS=(M1-M2)**2-Q2                                               XDZERO22
      IF(QMINS.LT.0.)GOTO 1                                             XDZERO23
      PC=SQRT(QPLUS*QMINS)/(2.*M1)                                      XDZERO24
      IF(IMODEL.EQ.5)THEN                                               XDZERO25
        Y=Q2/M1**2                                                      XDZERO26
        PX=PC                                                           XDZERO27
        TM=(BM-XMT)**2                                                  XDZERO28
        T=TM-Y*BMSQ                                                     XDZERO29
        FEX=EXP(-MSQ*T/(4.*KSQ*BBX))                                    XDZERO30
        FEX1=FEX*((BB*BX/BBX)**2.5)*SQRT(MX/MBOT)                       XDZERO31
        QV=0.5*FEX1*MD/(MX*BB)                                          XDZERO32
        AL=-FEX1*MBOT*BB*((1./MUM)+0.5*(MD/(MBOT*KSQ*                   XDZERO33
     &  (BB**2)))*T*((1./MQ)-MD*BR/(2.*MUM*MX)))                        XDZERO34
        CP=FEX1*(MD*MB/(4.*BB*MBOT))*((1./MUM)-(MD*                     XDZERO35
     &  MQ/(2.*MX*(MUM**2)))*BR)                                        XDZERO36
        FEX2=FEX*((BB*BX/BBX)**1.5)                                     XDZERO37
        Q=FEX2*(1.+(MB/(2.*MUM))-MB*MQ*MD*BR/(4.*MUP*MUM*MX))           XDZERO38
        FEX5=FEX*((BB*BX/BBX)**2.5)*SQRT(MX/MBOT)                       AFL00150
        UP=FEX5*MD*MQ*MB/(SQRT(6.)*BB*MX*MUM)                           AFL00151
      ENDIF                                                             XDZERO39
C.. 0 DECAY                                                             XDZERO40
      IF (IMODEL.EQ.1) THEN                                             XDZERO41
        XDZERO = 2. * M1 * PC * VM * OVER1                              XDZERO42
      ELSEIF (IMODEL.EQ.2) THEN                                         XDZERO43
C       XDZERO = 2.*M1*PC*1.8075*0.6269*EXP(-0.0296*(Q2MAX-T))          XDZERO44
        XDZERO = 2.*M1*PC*1.8075*0.6269*EXP(-0.0145*(Q2MAX-T))          XDZERO45
      ELSEIF (IMODEL.EQ.3) THEN                                         XDZERO46
        XDZERO = SQRT(2.)*PC*SQRT( M1**2+M2**2-T )                      XDZERO47
      ELSEIF (IMODEL.EQ.4) THEN                                         XDZERO48
        XDZERO = 2. * M1 * PC * VM * OVER1                              XDZERO49
      ELSEIF (IMODEL.EQ.5) THEN                                         XDZERO50
        IF(J.LT.10)XDZERO = PC*2.*M1*2.*Q                               AFL00152
        IF(J.EQ.10)XDZERO = PC*2.*M1*2.*UP                              AFL00153
      ENDIF                                                             XDZERO52
 200  CONTINUE                                                          XDZERO53
 1    CONTINUE                                                          XDZERO54
      RETURN                                                            XDZERO55
      END                                                               XDZERO56
      FUNCTION XHMOINS (IMODEL,J,M1,M2,Q2)                              XHMOINS2
C-----------------------------------------------------------------------XHMOINS3
C     A. FALVARD   -   130389                                           XHMOINS4
C                                                                       XHMOINS5
C! H- HELICITY AMPLITUDE FOR B OR D --> EL + NEUT + VECTOR MESON        XHMOINS6
C-----------------------------------------------------------------------XHMOINS7
      REAL M1,M2                                                        XHMOINS8
      PARAMETER (NMODEL=5,NDECAY=11)                                    AFL001 1
      COMMON/SEMLEP/IMOSEM,WTM(NMODEL,NDECAY),IHEL,KHEL,PRM1,PR0,PRP1,  SEMLEP 3
     $          BBDAT(NDECAY),BXDAT(NDECAY)  ,                          SEMLEP 4
     $          NO1(NDECAY),NO2(NDECAY),OVER(NMODEL,NDECAY)             SEMLEP 5
     $          , IMODSS,R1,R2,R3                                       AFL001 2
      REAL MFF,MFF2,MFF3,MFF4                                           SEMLEP 6
      COMMON/MFFX/MFF(NMODEL,NDECAY),MFF2(NMODEL,NDECAY),               SEMLEP 7
     $            MFF3(NMODEL,NDECAY),MFF4(NMODEL,NDECAY)               SEMLEP 8
      REAL MB,MD,MQ,MBOT,MX,MUP,MUM,MS,MSQ,MRSQ,KSQ                     SEMLEP 9
      COMMON /SCORA/MB,MD,MQ,MBOT,MX,MUP,MUM,MS,MSQ,MRSQ,KSQ            SEMLEP10
     *,BM,BMSQ,XMTSQ,BB,BX,XMT,BBX,BR,BRX                               SEMLEP11
      COMMON /SCOROU/AL,CP,Q,QV                                         SEMLEP12
      XHMOINS=0.                                                        XHMOIN10
      T=Q2                                                              XHMOIN11
      Q2MAX = (M1-M2)**2                                                XHMOIN12
      IF(IMODEL.LE.4)THEN                                               XHMOIN13
         VM=MFF(IMODEL,J)**2/(MFF(IMODEL,J)**2-Q2)                      XHMOIN14
         VM2=MFF2(IMODEL,J)**2/(MFF2(IMODEL,J)**2-Q2)                   XHMOIN15
         VM3=MFF3(IMODEL,J)**2/(MFF3(IMODEL,J)**2-Q2)                   XHMOIN16
         VM4=MFF4(IMODEL,J)**2/(MFF4(IMODEL,J)**2-Q2)                   XHMOIN17
         OVER1=OVER(IMODEL,J)                                           XHMOIN18
      ENDIF                                                             XHMOIN19
      PI=4.*ATAN(1.)                                                    XHMOIN20
      QPLUS=(M1+M2)**2-Q2                                               XHMOIN21
      QMINS=(M1-M2)**2-Q2                                               XHMOIN22
      IF(QMINS.LT.0.)GOTO 1                                             XHMOIN23
      PC=SQRT(QPLUS*QMINS)/(2.*M1)                                      XHMOIN24
      IF(IMODEL.EQ.5)THEN                                               XHMOIN25
        Y=Q2/M1**2                                                      XHMOIN26
        PX=PC                                                           XHMOIN27
        TM=(BM-XMT)**2                                                  XHMOIN28
        T=TM-Y*BMSQ                                                     XHMOIN29
        FEX=EXP(-MSQ*T/(4.*KSQ*BBX))                                    XHMOIN30
      FEX1=FEX*((BB*BX/BBX)**1.5)*SQRT(MX/MBOT)                         AFL00154
      GV1=0.5*(1./MQ-MD*BR/(2.*MUM*MX))                                 AFL00155
      F1=2.*MBOT                                                        AFL00156
      GV=GV1*FEX1                                                       AFL00157
      F=F1*FEX1                                                         AFL00158
      AP=-0.5*FEX1/MX*(1.+MD/MB*((BB**2-BX**2)/(BB**2+BX**2))           AFL00159
     *-MD**2/(4.*MUM*MBOT)*BX**4/BBX**2)                                AFL00160
      AMSQ=MSQ/(4.*KSQ*BBX)                                             AFL00161
      FEX5=FEX*((BB*BX/BBX)**2.5)*SQRT(MX/MBOT)                         AFL00162
      QV=0.5*FEX5*MD/(MX*BB)                                            AFL00163
      AL=-FEX5*MBOT*BB*((1./MUM)+0.5*(MD/(MBOT*KSQ*                     AFL00164
     &(BB**2)))*T*((1./MQ)-MD*BR/(2.*MUM*MX)))                          AFL00165
      CP=FEX5*(MD*MB/(4.*BB*MBOT))*((1./MUM)-(MD*                       AFL00166
     &MQ/(2.*MX*(MUM**2)))*BR)                                          AFL00167
      R=FEX5*MBOT*BB/(SQRT(2.)*MUP)                                     AFL00168
      SP=FEX5*MD/(SQRT(2.)*BB*MBOT)*(1.+MB/(2.*MUM)-MB*MD*MQ            AFL00169
     &/(4.*MUP*MUM*MX)*BR)                                              AFL00170
      V=FEX5*MBOT*BB/(4.*SQRT(2.)*MB*MQ*MX)                             AFL00171
      FEX2=FEX1                                                         AFL00172
      UP=FEX5*MD*MQ*MB/(SQRT(6.)*BB*MX*MUM)                             AFL00173
        Q=FEX2*(1.+(MB/(2.*MUM))-MB*MQ*MD*BR/(4.*MUP*MUM*MX))           XHMOIN38
      ENDIF                                                             XHMOIN39
C.. -* DECAY                                                            XHMOIN40
                                                                        XHMOIN41
      IF (IMODEL.EQ.1) THEN                                             XHMOIN42
        IF(J.LT.7) THEN                                                 XHMOIN43
C..SEMILEPTONIC B-->D,D* DECAY.                                         XHMOIN44
          XHMOINS = SQRT(T) * VM * OVER1 *                              XHMOIN45
     &    ( M1 + M2 + IHEL * 2.*M1 * PC * VM /(M1+M2)  )                XHMOIN46
        ELSE                                                            XHMOIN47
          XHMOINS = SQRT(T) * VM * OVER1 *                              XHMOIN48
     &    ( M1 + M2 + IHEL * 2.*M1 * PC * VM3 /(M1+M2)  )               XHMOIN49
        ENDIF                                                           XHMOIN50
      ELSEIF (IMODEL.EQ.2) THEN                                         XHMOIN51
        XHMOINS = SQRT(T) *( 10.9 + M1*PC*0.521) *0.6269*               XHMOIN52
C    &  EXP( -0.0296*( Q2MAX - T) )                                     XHMOIN53
     &  EXP( -0.0145*( Q2MAX - T) )                                     XHMOIN54
      ELSEIF (IMODEL.EQ.3) THEN                                         XHMOIN55
        XHMOINS = SQRT(2.*T)*SQRT( M1**2+M2**2-T )                      XHMOIN56
      ELSEIF (IMODEL.EQ.4) THEN                                         XHMOIN57
        XHMOINS = SQRT(T) * OVER1 *                                     XHMOIN58
     &  ( (M1 + M2)*VM2 + IHEL * 2.*M1 * PC * VM /(M1+M2)  )            XHMOIN59
      ELSEIF (IMODEL.EQ.5)THEN                                          XHMOIN60
      IF(J.LT.9)THEN                                                    AFL00174
         XHMOINS=SQRT(Q2)*(F+IHEL*M1*PC*2.*GV)                          AFL00175
      ELSEIF(J.EQ.9)THEN                                                AFL00176
         XHMOINS = SQRT(Q2)*(AL+IHEL*M1*PC*2.*QV)                       AFL00177
      ELSEIF(J.EQ.11)THEN                                               AFL00178
         XHMOINS = SQRT(Q2)*(R+IHEL*M1*PC*2.*V)                         AFL00179
      ENDIF                                                             AFL00180
      ENDIF                                                             XHMOIN62
 200  CONTINUE                                                          XHMOIN63
 1    CONTINUE                                                          XHMOIN64
      RETURN                                                            XHMOIN65
      END                                                               XHMOIN66
      FUNCTION XHPLUS (IMODEL,J,M1,M2,Q2)                               XHPLUS 2
C---------------------------------------------------------------------- XHPLUS 3
C     A. FALVARD   -   130389                                           XHPLUS 4
C                                                                       XHPLUS 5
C!  H+ HELICITY AMPLITUDE FOR B OR D --> EL + NEUT +VECTOR MESON        XHPLUS 6
C---------------------------------------------------------------------- XHPLUS 7
      REAL M1,M2                                                        XHPLUS 8
      PARAMETER (NMODEL=5,NDECAY=11)                                    AFL001 1
      COMMON/SEMLEP/IMOSEM,WTM(NMODEL,NDECAY),IHEL,KHEL,PRM1,PR0,PRP1,  SEMLEP 3
     $          BBDAT(NDECAY),BXDAT(NDECAY)  ,                          SEMLEP 4
     $          NO1(NDECAY),NO2(NDECAY),OVER(NMODEL,NDECAY)             SEMLEP 5
     $          , IMODSS,R1,R2,R3                                       AFL001 2
      REAL MFF,MFF2,MFF3,MFF4                                           SEMLEP 6
      COMMON/MFFX/MFF(NMODEL,NDECAY),MFF2(NMODEL,NDECAY),               SEMLEP 7
     $            MFF3(NMODEL,NDECAY),MFF4(NMODEL,NDECAY)               SEMLEP 8
      REAL MB,MD,MQ,MBOT,MX,MUP,MUM,MS,MSQ,MRSQ,KSQ                     SEMLEP 9
      COMMON /SCORA/MB,MD,MQ,MBOT,MX,MUP,MUM,MS,MSQ,MRSQ,KSQ            SEMLEP10
     *,BM,BMSQ,XMTSQ,BB,BX,XMT,BBX,BR,BRX                               SEMLEP11
      COMMON /SCOROU/AL,CP,Q,QV                                         SEMLEP12
      XHPLUS=0.                                                         XHPLUS10
      T=Q2                                                              XHPLUS11
      Q2MAX = (M1-M2)**2                                                XHPLUS12
      IF(IMODEL.LE.4)THEN                                               XHPLUS13
         VM=MFF(IMODEL,J)**2/(MFF(IMODEL,J)**2-Q2)                      XHPLUS14
         VM2=MFF2(IMODEL,J)**2/(MFF2(IMODEL,J)**2-Q2)                   XHPLUS15
         VM3=MFF3(IMODEL,J)**2/(MFF3(IMODEL,J)**2-Q2)                   XHPLUS16
         VM4=MFF4(IMODEL,J)**2/(MFF4(IMODEL,J)**2-Q2)                   XHPLUS17
         OVER1=OVER(IMODEL,J)                                           XHPLUS18
      ENDIF                                                             XHPLUS19
      PI=4.*ATAN(1.)                                                    XHPLUS20
      QPLUS=(M1+M2)**2-Q2                                               XHPLUS21
      QMINS=(M1-M2)**2-Q2                                               XHPLUS22
      IF(QMINS.LT.0.)GOTO 1                                             XHPLUS23
      PC=SQRT(QPLUS*QMINS)/(2.*M1)                                      XHPLUS24
      IF(IMODEL.EQ.5)THEN                                               XHPLUS25
        Y=Q2/M1**2                                                      XHPLUS26
        PX=PC                                                           XHPLUS27
        TM=(BM-XMT)**2                                                  XHPLUS28
        T=TM-Y*BMSQ                                                     XHPLUS29
        FEX=EXP(-MSQ*T/(4.*KSQ*BBX))                                    XHPLUS30
      FEX1=FEX*((BB*BX/BBX)**1.5)*SQRT(MX/MBOT)                         AFL00181
      GV1=0.5*(1./MQ-MD*BR/(2.*MUM*MX))                                 AFL00182
      F1=2.*MBOT                                                        AFL00183
      GV=GV1*FEX1                                                       AFL00184
      F=F1*FEX1                                                         AFL00185
      AP=-0.5*FEX1/MX*(1.+MD/MB*((BB**2-BX**2)/(BB**2+BX**2))           AFL00186
     *-MD**2/(4.*MUM*MBOT)*BX**4/BBX**2)                                AFL00187
      FEX5=FEX*((BB*BX/BBX)**2.5)*SQRT(MX/MBOT)                         AFL00188
      QV=0.5*FEX5*MD/(MX*BB)                                            AFL00189
      AL=-FEX5*MBOT*BB*((1./MUM)+0.5*(MD/(MBOT*KSQ*                     AFL00190
     &(BB**2)))*T*((1./MQ)-MD*BR/(2.*MUM*MX)))                          AFL00191
      CP=FEX5*(MD*MB/(4.*BB*MBOT))*((1./MUM)-(MD*                       AFL00192
     &MQ/(2.*MX*(MUM**2)))*BR)                                          AFL00193
      R=FEX5*MBOT*BB/(SQRT(2.)*MUP)                                     AFL00194
      SP=FEX5*MD/(SQRT(2.)*BB*MBOT)*(1.+MB/(2.*MUM)-MB*MD*MQ            AFL00195
     &/(4.*MUP*MUM*MX)*BR)                                              AFL00196
      V=FEX5*MBOT*BB/(4.*SQRT(2.)*MB*MQ*MX)                             AFL00197
      FEX2=FEX1                                                         AFL00198
      UP=FEX5*MD*MQ*MB/(SQRT(6.)*BB*MX*MUM)                             AFL00199
        Q=FEX2*(1.+(MB/(2.*MUM))-MB*MQ*MD*BR/(4.*MUP*MUM*MX))           XHPLUS38
      ENDIF                                                             XHPLUS39
C.. +* DECAY                                                            XHPLUS40
C..SEMILEPTONIC B-->D,D* DECAY.                                         XHPLUS41
      IF(IMODEL.EQ.1) THEN                                              XHPLUS42
      IF(J.LT.7)THEN                                                    XHPLUS43
          XHPLUS = SQRT(T) * VM * OVER1  *                              XHPLUS44
     &    ( M1 + M2 - IHEL * 2.*M1 * PC * VM /(M1+M2)  )                XHPLUS45
        ELSE                                                            XHPLUS46
          XHPLUS = SQRT(T) * VM * OVER1 *                               XHPLUS47
     &    ( M1 + M2 - IHEL * 2.*M1 * PC * VM3 /(M1+M2)  )               XHPLUS48
        ENDIF                                                           XHPLUS49
      ELSEIF (IMODEL.EQ.2) THEN                                         XHPLUS50
        XHPLUS = SQRT(T) *( 10.9 - M1*PC*0.521) *0.6269*                XHPLUS51
C    &  EXP( -0.0296*( Q2MAX - T) )                                     XHPLUS52
     &  EXP( -0.0145*( Q2MAX - T) )                                     XHPLUS53
      ELSEIF (IMODEL.EQ.3) THEN                                         XHPLUS54
        XHPLUS = SQRT(2.*T)*SQRT( M1**2+M2**2-T )                       XHPLUS55
      ELSEIF (IMODEL.EQ.4) THEN                                         XHPLUS56
        XHPLUS = SQRT(T) * OVER1 *                                      XHPLUS57
     &  ( (M1 + M2)*VM2 - IHEL * 2.*M1 * PC * VM /(M1+M2)  )            XHPLUS58
      ELSEIF (IMODEL.EQ.5)THEN                                          XHPLUS59
      IF(J.LT.9)THEN                                                    AFL00200
         XHPLUS=SQRT(Q2)*(F-IHEL*M1*PC*2.*GV)                           AFL00201
      ELSEIF(J.EQ.9)THEN                                                AFL00202
         XHPLUS=SQRT(Q2)*(AL-IHEL*M1*PC*2.*QV)                          AFL00203
      ELSEIF(J.EQ.11)THEN                                               AFL00204
         XHPLUS=SQRT(Q2)*(R-IHEL*M1*PC*2*V)                             AFL00205
      ENDIF                                                             AFL00206
      ENDIF                                                             XHPLUS61
 200  CONTINUE                                                          XHPLUS62
 1    CONTINUE                                                          XHPLUS63
      RETURN                                                            XHPLUS64
      END                                                               XHPLUS65
      FUNCTION XHZERO (IMODEL,J,M1,M2,Q2)                               XHZERO 2
C---------------------------------------------------------------------  XHZERO 3
C     A. FALVARD - 130389                                               XHZERO 4
C                                                                       XHZERO 5
C! H0 HELICITY AMPLITUDE FOR B OR D --> EL + NEUT + VECTOR MESON        XHZERO 6
C---------------------------------------------------------------------  XHZERO 7
      REAL M1,M2                                                        XHZERO 8
      PARAMETER (NCUT=100)                                              XHZERO 9
      PARAMETER (NMODEL=5,NDECAY=11)                                    AFL001 1
      COMMON/SEMLEP/IMOSEM,WTM(NMODEL,NDECAY),IHEL,KHEL,PRM1,PR0,PRP1,  SEMLEP 3
     $          BBDAT(NDECAY),BXDAT(NDECAY)  ,                          SEMLEP 4
     $          NO1(NDECAY),NO2(NDECAY),OVER(NMODEL,NDECAY)             SEMLEP 5
     $          , IMODSS,R1,R2,R3                                       AFL001 2
      REAL MFF,MFF2,MFF3,MFF4                                           SEMLEP 6
      COMMON/MFFX/MFF(NMODEL,NDECAY),MFF2(NMODEL,NDECAY),               SEMLEP 7
     $            MFF3(NMODEL,NDECAY),MFF4(NMODEL,NDECAY)               SEMLEP 8
      REAL MB,MD,MQ,MBOT,MX,MUP,MUM,MS,MSQ,MRSQ,KSQ                     SEMLEP 9
      COMMON /SCORA/MB,MD,MQ,MBOT,MX,MUP,MUM,MS,MSQ,MRSQ,KSQ            SEMLEP10
     *,BM,BMSQ,XMTSQ,BB,BX,XMT,BBX,BR,BRX                               SEMLEP11
      COMMON /SCOROU/AL,CP,Q,QV                                         SEMLEP12
      XHZERO=0.                                                         XHZERO11
      Q2MAX = (M1-M2)**2                                                XHZERO12
      T=Q2                                                              XHZERO13
      IF(IMODEL.LE.4)THEN                                               XHZERO14
         VM=MFF(IMODEL,J)**2/(MFF(IMODEL,J)**2-Q2)                      XHZERO15
         VM2=MFF2(IMODEL,J)**2/(MFF2(IMODEL,J)**2-Q2)                   XHZERO16
         VM3=MFF3(IMODEL,J)**2/(MFF3(IMODEL,J)**2-Q2)                   XHZERO17
         VM4=MFF4(IMODEL,J)**2/(MFF4(IMODEL,J)**2-Q2)                   XHZERO18
         OVER1=OVER(IMODEL,J)                                           XHZERO19
      ENDIF                                                             XHZERO20
      PI=4.*ATAN(1.)                                                    XHZERO21
      QPLUS=(M1+M2)**2-Q2                                               XHZERO22
      QMINS=(M1-M2)**2-Q2                                               XHZERO23
      IF(QMINS.LT.0.)GOTO 1                                             XHZERO24
      PC=SQRT(QPLUS*QMINS)/(2.*M1)                                      XHZERO25
      IF(IMODEL.EQ.5)THEN                                               XHZERO26
        Y=Q2/M1**2                                                      XHZERO27
        PX=PC                                                           XHZERO28
        TM=(BM-XMT)**2                                                  XHZERO29
        T=TM-Y*BMSQ                                                     XHZERO30
        FEX=EXP(-MSQ*T/(4.*KSQ*BBX))                                    XHZERO31
      FEX1=FEX*((BB*BX/BBX)**1.5)*SQRT(MX/MBOT)                         AFL00207
      GV1=0.5*(1./MQ-MD*BR/(2.*MUM*MX))                                 AFL00208
      F1=2.*MBOT                                                        AFL00209
      GV=GV1*FEX1                                                       AFL00210
      F=F1*FEX1                                                         AFL00211
      AP=-0.5*FEX1/MX*(1.+MD/MB*((BB**2-BX**2)/(BB**2+BX**2))           AFL00212
     *-MD**2/(4.*MUM*MBOT)*BX**4/BBX**2)                                AFL00213
      FEX5=FEX*((BB*BX/BBX)**2.5)*SQRT(MX/MBOT)                         AFL00214
      QV=0.5*FEX5*MD/(MX*BB)                                            AFL00215
      AL=-FEX5*MBOT*BB*((1./MUM)+0.5*(MD/(MBOT*KSQ*                     AFL00216
     &(BB**2)))*T*((1./MQ)-MD*BR/(2.*MUM*MX)))                          AFL00217
      CP=FEX5*(MD*MB/(4.*BB*MBOT))*((1./MUM)-(MD*                       AFL00218
     &MQ/(2.*MX*(MUM**2)))*BR)                                          AFL00219
      R=FEX5*MBOT*BB/(SQRT(2.)*MUP)                                     AFL00220
      SP=FEX5*MD/(SQRT(2.)*BB*MBOT)*(1.+MB/(2.*MUM)-MB*MD*MQ            AFL00221
     &/(4.*MUP*MUM*MX)*BR)                                              AFL00222
      V=FEX5*MBOT*BB/(4.*SQRT(2.)*MB*MQ*MX)                             AFL00223
      FEX2=FEX1                                                         AFL00224
      UP=FEX5*MD*MQ*MB/(SQRT(6.)*BB*MX*MUM)                             AFL00225
        Q=FEX2*(1.+(MB/(2.*MUM))-MB*MQ*MD*BR/(4.*MUP*MUM*MX))           XHZERO39
      ENDIF                                                             XHZERO40
C.. 0* DECAY                                                            XHZERO41
      IF (IMODEL.EQ.1) THEN                                             XHZERO42
        IF(J.LT.7) THEN                                                 XHZERO43
C..SEMILEPTONIC B-->D,D* DECAY.                                         XHZERO44
          XHZERO = VM * OVER1 / M2 *                                    XHZERO45
     &    ( (M1**2 - M2**2 -T) * (M1 + M2) /2. - 2. * M1**2 * PC**2     XHZERO46
     &     * VM /(M1+M2))                                               XHZERO47
        ELSE                                                            XHZERO48
          XHZERO = VM * OVER1 / M2 *                                    XHZERO49
     &    ( (M1**2 - M2**2 -T) * (M1 + M2) /2. - 2. * M1**2 * PC**2     XHZERO50
     &     * VM3 /(M1+M2))                                              XHZERO51
        ENDIF                                                           XHZERO52
      ELSEIF (IMODEL.EQ.2) THEN                                         XHZERO53
        XHZERO = 1./(2.*M2) * ( M1**2 - M2**2 - T) *10.90 *0.6269*      XHZERO54
C    &   EXP( -0.0296*( Q2MAX - T) )                                    XHZERO55
     &   EXP( -0.0145*( Q2MAX - T) )                                    XHZERO56
      ELSEIF (IMODEL.EQ.3) THEN                                         XHZERO57
        XHZERO = ( M1**2-M2**2-T) * SQRT(M1**2+M2**2-T) / (SQRT(2.)*M2) XHZERO58
      ELSEIF (IMODEL.EQ.4) THEN                                         XHZERO59
        XHZERO = OVER1 / M2 *                                           XHZERO60
     &  ( (M1**2 - M2**2 -T) * (M1 + M2)*VM2 / 2. - 2. * M1**2 * PC**2  XHZERO61
     &   * VM4 /(M1+M2))                                                XHZERO62
      ELSEIF(IMODEL.EQ.5)THEN                                           XHZERO63
      IF(J.LT.9)THEN                                                    AFL00226
          XHZERO=1./(2.*M2)*((M1**2-M2**2-Q2)*F                         AFL00227
     *  +4.*M1**2*PC**2*AP)                                             AFL00228
      ELSEIF(J.EQ.9)THEN                                                AFL00229
          XHZERO=1./(2.*M2)*((M1**2-M2**2-Q2)*AL                        AFL00230
     *    +4.*M1**2*PC**2*CP)                                           AFL00231
      ELSEIF(J.EQ.11)THEN                                               AFL00232
          XHZERO=1./(2.*M2)*((M1**2-M2**2-Q2)*R                         AFL00233
     *    +4.*M1**2*PC**2*SP)                                           AFL00234
      ENDIF                                                             AFL00235
      ENDIF                                                             XHZERO66
 200  CONTINUE                                                          XHZERO67
 1    CONTINUE                                                          XHZERO68
      RETURN                                                            XHZERO69
      END                                                               XHZERO70
