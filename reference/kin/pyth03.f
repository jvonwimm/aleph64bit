      SUBROUTINE PYCARD                                                 PYCARD 2
C -----------------------------------------------------------------     PYCARD 3
C                                                                       PYCARD 4
C  M FRANK 17/4/91                                                      PYCARD 5
C                                                                       PYCARD 6
C! SET PYTHIA PARAMETERS BY DATA CARDS                                  PYCARD 7
CKEY KINE KINGAL PYTHIA5.5 DECAY  /  USER INTERNAL                      PYCARD 8
C  EVERY PYTHIA PARAMETER IS A BOS DATA CARD KEYWORD,THE INDEX OF THE   PYCARD 9
C  PARAMETER IS THE BANK NUMBER.                                        PYCARD10
C                                                                       PYCARD11
C  THE LIST OF KEYWORDS WITH THEIR FORMAT IS GIVEN BELOW:               PYCARD12
C                                                                       PYCARD13
C 'MSEL'   ,'MSUB'(I),'KFI1'(1,I),'KFI2'(2,I),                          PYCARD14
C 'CKIN'(F),'MSTP'(I),'PARP'(I),'MSTI'(I),'PARI'(F)                     PYCARD15
C 'MINT'(I),'VINT'(F)                                                   PYCARD16
C                                                                       PYCARD17
C FOLLOWING ARRAYS IN THE PYTHIA COMMON BLOCK                           PYCARD18
C   MUST NOT CHANGED BY THE USER:                                       PYCARD19
C                                                                       PYCARD20
C 'ISET'(I),'KFPR'(I),'COEF'(F)                                         PYCARD21
C 'XSFX'(F),'ISIG'(I),'SIGH'(F),'KFPR'(I),'COEF'(F)                     PYCARD22
C 'NGEN'(F,F),'XSEC'(F,F)                                               PYCARD23
C                                                                       PYCARD24
C    KEY  I  /  IVAL     ====>  KEY(I)=IVAL                             PYCARD25
C    RKEY I  /  VALUE    ====>  RKEY(I)=VALUE                           PYCARD26
C                                                                       PYCARD27
C - STRUCTURE: SUBROUTINE SUBPROGRAM                                    PYCARD28
C              USER ENTRY NAME: PYCARD                                  PYCARD29
C              External References: NAMIND/BKFMT/BLIST(BOS77)           PYCARD30
C              Comdecks referenced: BCS,PYTCOM                          PYCARD31
C                                                                       PYCARD32
C - USAGE    : CALL PYCARD                                              PYCARD33
C                                                                       PYCARD34
C   -----------------------------------------------------------------   PYCARD35
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      COMMON/PYSUBS/MSEL,MSUB(200),KFIN(2,-40:40),CKIN(200)             PYTCOM 2
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)             PYTCOM 3
      COMMON/PYINT1/MINT(400),VINT(400)                                 PYTCOM 4
      COMMON/PYINT2/ISET(200),KFPR(200,2),COEF(200,20),ICOL(40,4,2)     PYTCOM 5
      COMMON/PYINT3/XSFX(2,-40:40),ISIG(1000,3),SIGH(1000)              PYTCOM 6
      COMMON/PYINT4/WIDP(21:40,0:40),WIDE(21:40,0:40),WIDS(21:40,3)     PYTCOM 7
      COMMON/PYINT5/NGEN(0:200,3),XSEC(0:200,3)                         PYTCOM 8
      COMMON/PYINT6/PROC(0:200)                                         PYTCOM 9
      CHARACTER PROC*28                                                 PYTCOM10
C                                                                       PYTCOM11
C                                                                       PYCARD38
      PARAMETER (LKEYS=11)                                              PYCARD39
      CHARACTER*4 KEY(LKEYS),CHAINT                                     PYCARD40
      CHARACTER*1 FMT(LKEYS)                                            PYCARD41
      DATA KEY / 'MSEL','MSUB','KFI1','KFI2',                           PYCARD42
     &           'CKIN','MSTP','PARP','MSTI',                           PYCARD43
     &           'PARI','MINT','VINT'/                                  PYCARD44
      DATA FMT /'I','I','I','I',                                        PYCARD45
     &          'F','I','F','I',                                        PYCARD46
     &          'F','I','F'/                                            PYCARD47
      DATA NAPAR/0/                                                     PYCARD48
*                                                                       PYCARD49
      WRITE(6,1)                                                        PYCARD50
 1    FORMAT(//                                                         PYCARD51
     .20X,' YOU ARE RUNNING PYTHIA INSIDE THE                      '//  PYCARD52
     .20X,'     K I N G A L  - PACKAGE                             '//  PYCARD53
     .20X,'      INTERFACE WRITTEN BY M. FRANK                     '/   PYCARD54
     .20X,'      FOR COMPLAINS  SEND MAIL TO :                     '/   PYCARD55
     .20X,'      FRANKM@CERNVM  OR  ALWS::FRANKM  or ALWS::BLOCH   '//  PYCARD56
     .20X,'                                                        '/   PYCARD57
     .15X,'--------------------------------------------------------'/// PYCARD58
     .20X,' YOU SET OF THE PYTHIA PARAMETERS                       '/   PYCARD59
     .20X,' IN THE FREE FORMAT BOS CARD FILE:                      '//) PYCARD60
 2    FORMAT(20X,' KEY : ',A4,'  =  ',I8)                               PYCARD61
 3    FORMAT(20X,' KEY : ',A4,'  =  ',F8.4)                             PYCARD62
 4    FORMAT(20X,' KEY : ',A4,'(',I3,')  =  ',I8)                       PYCARD63
 5    FORMAT(20X,' KEY : ',A4,'(',I3,')  =  ',F8.4)                     PYCARD64
*                                                                       PYCARD65
      DO 50 I=1,LKEYS                                                   PYCARD66
         NAMI=NAMIND(KEY(I))                                            PYCARD67
         IF (IW(NAMI).EQ.0) GOTO 50                                     PYCARD68
         KIND=NAMI+1                                                    PYCARD69
   15    KIND=IW(KIND-1)                                                PYCARD70
         IF (KIND.EQ.0) GOTO 49                                         PYCARD71
         J = IW(KIND-2)                                                 PYCARD72
         GOTO (21,22,23,24,25,26,27,28,29,30,31)I                       PYCARD73
   21    MSEL = IW(KIND+1)                                              PYCARD74
         WRITE(6,2)KEY(I),MSEL                                          PYCARD75
       GOTO 115                                                         PYCARD76
   22    MSUB(J) = IW(KIND+1)                                           PYCARD77
         WRITE(6,4)KEY(I),J,MSUB(J)                                     PYCARD78
       GOTO 15                                                          PYCARD79
   23    KFIN(1,J) = IW(KIND+1)                                         PYCARD80
         WRITE(6,4)KEY(I),J,KFIN(1,J)                                   PYCARD81
       GOTO 15                                                          PYCARD82
   24    KFIN(2,J) = IW(KIND+1)                                         PYCARD83
         WRITE(6,4)KEY(I),J,KFIN(2,J)                                   PYCARD84
       GOTO 15                                                          PYCARD85
   25    CKIN(J) = RW(KIND+1)                                           PYCARD86
         WRITE(6,5)KEY(I),J,CKIN(J)                                     PYCARD87
       GOTO 15                                                          PYCARD88
   26    MSTP(J) = IW(KIND+1)                                           PYCARD89
         WRITE(6,4)KEY(I),J,MSTP(J)                                     PYCARD90
       GOTO 15                                                          PYCARD91
   27    PARP(J) = RW(KIND+1)                                           PYCARD92
         WRITE(6,5)KEY(I),J,PARP(J)                                     PYCARD93
       GOTO  15                                                         PYCARD94
   28    MSTI(J) = IW(KIND+1)                                           PYCARD95
         WRITE(6,4)KEY(I),J,MSTI(J)                                     PYCARD96
       GOTO  15                                                         PYCARD97
   29    PARI(J) = RW(KIND+1)                                           PYCARD98
         WRITE(6,5)KEY(I),J,PARI(J)                                     PYCARD99
       GOTO  15                                                         PYCAR100
   30    MINT(J) = IW(KIND+1)                                           PYCAR101
         WRITE(6,4)KEY(I),J,MINT(J)                                     PYCAR102
       GOTO 15                                                          PYCAR103
   31    VINT(J) = RW(KIND+1)                                           PYCAR104
         WRITE(6,5)KEY(I),J,VINT(J)                                     PYCAR105
       GOTO  15                                                         PYCAR106
   49    CONTINUE                                                       PYCAR107
         CALL BKFMT (KEY(I),FMT(I))                                     PYCAR108
         CALL BLIST (IW,'C+',KEY(I))                                    PYCAR109
       GOTO 50                                                          PYCAR110
  115 CONTINUE                                                          PYCAR111
      GOTO 15                                                           PYCAR112
   50 CONTINUE                                                          PYCAR113
      WRITE(6,6)                                                        PYCAR114
 6    FORMAT(/,/,15X,                                                   PYCAR115
     .'--------------------------------------------------------'//)     PYCAR116
      RETURN                                                            PYCAR117
      END                                                               PYCAR118
      SUBROUTINE PYTCLR                                                 PYTCLR 2
C ------------------------------------------------------------          PYTCLR 3
C -  B. Bloch  - JUNE 1994                                              PYTCLR 4
C                                                                       PYTCLR 5
C  store the history of w decays in a usable format for the interface   PYTCLR 6
C                                                                       PYTCLR 7
C                                                                       PYTCLR 8
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
      COMMON/PYSUBS/MSEL,MSUB(200),KFIN(2,-40:40),CKIN(200)             PYTCOM 2
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)             PYTCOM 3
      COMMON/PYINT1/MINT(400),VINT(400)                                 PYTCOM 4
      COMMON/PYINT2/ISET(200),KFPR(200,2),COEF(200,20),ICOL(40,4,2)     PYTCOM 5
      COMMON/PYINT3/XSFX(2,-40:40),ISIG(1000,3),SIGH(1000)              PYTCOM 6
      COMMON/PYINT4/WIDP(21:40,0:40),WIDE(21:40,0:40),WIDS(21:40,3)     PYTCOM 7
      COMMON/PYINT5/NGEN(0:200,3),XSEC(0:200,3)                         PYTCOM 8
      COMMON/PYINT6/PROC(0:200)                                         PYTCOM 9
      CHARACTER PROC*28                                                 PYTCOM10
C                                                                       PYTCOM11
      INEW = 0                                                          PYTCLR11
      DO 100 ILU = 5,N7LU                                               PYTCLR12
         KF = ABS(K7LU(ILU,2))                                          PYTCLR13
         KS = K7LU(ILU,1)                                               PYTCLR14
         KM = K7LU(ILU,3)                                               PYTCLR15
         K1 = K7LU(ILU,4)                                               PYTCLR16
         K2 = K7LU(ILU,5)                                               PYTCLR17
C incoming beams                                                        PYTCLR18
         IF ( KS.EQ.21 .AND. KF.EQ.11 .AND. KM.LT.7) THEN               PYTCLR19
           INEW = INEW+1                                                PYTCLR20
           K7LU(INEW,1) = K7LU(ILU,1)                                   PYTCLR21
           K7LU(INEW,2) = K7LU(ILU,2)                                   PYTCLR22
           K7LU(INEW,3) = 0                                             PYTCLR23
           K7LU(INEW,4) = 0                                             PYTCLR24
           K7LU(INEW,5) = 0                                             PYTCLR25
C W pair ( single copy of it ) second copy has ks=11 , or Z0 or H0 or GaPYTCLR26
         ELSE IF ( KF.GE.22 .AND. KF.LE.25) THEN                        PYTCLR27
           IF ( KS .EQ.11  ) GO TO 100                                  PYTCLR28
           IF ( KF.EQ.22 .AND.(KM.EQ.7 .OR. KM.EQ.8)) GO TO 100         PYTCLR29
           INEW = INEW+1                                                PYTCLR30
           K7LU(INEW,1) = 11                                            PYTCLR31
           K7LU(INEW,2) = K7LU(ILU,2)                                   PYTCLR32
           IF ( KF.EQ.22 ) K7LU(INEW,1) = 1                             PYTCLR33
           K7LU(INEW,3) = 0                                             PYTCLR34
           K7LU(INEW,4) = 0                                             PYTCLR35
           K7LU(INEW,5) = 0                                             PYTCLR36
C W , Z , H   daughters = leptons or quarks , they are # 7 and 8        PYTCLR37
         ELSE IF ( KM.EQ.7 .OR. KM.EQ.8  ) THEN                         PYTCLR38
           INEW = INEW+1                                                PYTCLR39
           K7LU(INEW,1) = 14                                            PYTCLR40
           IF ( KF.GT.10 ) K7LU(INEW,1) = 1                             PYTCLR41
           K7LU(INEW,2) = K7LU(ILU,2)                                   PYTCLR42
           K7LU(INEW,3) = K7LU(ILU,3)-4                                 PYTCLR43
           MOTH =  k7lu(inew,3)                                         PYTCLR44
           IF(K7LU(MOTH,4).LE.0 .AND. k7lu(MOTH,5).LE.0)                PYTCLR45
     $         k7lu(MOTH,4) = INEW                                      PYTCLR46
           IF(K7LU(MOTH,4).GT.0 .AND. k7lu(MOTH,5).LE.0)                PYTCLR47
     $    k7lu(MOTH,5) = INEW                                           PYTCLR48
C gammas                                                                PYTCLR49
         ELSE IF ( KF.EQ.22  ) THEN                                     PYTCLR50
            IF ( P7LU(ILU,4).LT.1.E-06 ) GO TO 100                      PYTCLR51
            INEW = INEW +1                                              PYTCLR52
            K7LU(INEW,1) = K7LU(ILU,1)                                  PYTCLR53
            K7LU(INEW,2) = K7LU(ILU,2)                                  PYTCLR54
            IF ( KM.LE.2 ) K7LU(INEW,3) = K7LU(ILU,3)                   PYTCLR55
            IF ( KM.GT.2 ) K7LU(INEW,3) = 0                             PYTCLR56
            K7LU(INEW,4) = 0                                            PYTCLR57
            K7LU(INEW,5) = 0                                            PYTCLR58
C extra copy of lepton daughters                                        PYTCLR59
         ELSE IF ( KF.GE.11.AND.KF.LE.16 ) THEN                         PYTCLR60
           GO TO 100                                                    PYTCLR61
C quarks arranged in a string with gluons                               PYTCLR62
         ELSE IF ( KF.LT.10 .OR. KF.EQ.21) THEN                         PYTCLR63
            INEW = INEW +1                                              PYTCLR64
            K7LU(INEW,1) = K7LU(ILU,1)                                  PYTCLR65
            K7LU(INEW,2) = K7LU(ILU,2)                                  PYTCLR66
            K7LU(INEW,3) = K7LU(ILU,3)-4                                PYTCLR67
            K7LU(INEW,4) = K7LU(ILU,4)                                  PYTCLR68
            K7LU(INEW,5) = K7LU(ILU,5)                                  PYTCLR69
         ENDIF                                                          PYTCLR70
         DO 10 I=1,5                                                    PYTCLR71
           P7LU(INEW,I) = P7LU(ILU,I)                                   PYTCLR72
   10    CONTINUE                                                       PYTCLR73
 100  CONTINUE                                                          PYTCLR74
      N7LU = INEW                                                       PYTCLR75
      MSTU(70)=1                                                        PYTCLR76
      RETURN                                                            PYTCLR77
      END                                                               PYTCLR78
      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)                 ASKUSE 2
C ------------------------------------------------------------          ASKUSE 3
C -  B. Bloch  - September 1990 -June 1994 -November 94                 ASKUSE 4
C! GET AN EVENT FROM PYTHIA 5.5                                         ASKUSE 5
C! clean it to keep frmion decay products from Z0, W+- and H0           ASKUSE 6
C! then transfer the information into kine and vert banks.              ASKUSE 7
C                                                                       ASKUSE 8
C  PYTHIA 5.6 NEEDS THE FULL STUFF OF JETSET 7.3                        ASKUSE 9
C                                                                       ASKUSE10
C     structure : subroutine                                            ASKUSE11
C     output arguments :                                                ASKUSE12
C          IDPR   : process identification,each digit corresponds to    ASKUSE13
C          the flavor of the evnt ( several flavors /event is possible) ASKUSE14
C          ISTA   : status flag ( 0 means ok), use it to reject         ASKUSE15
C                   unwanted events                                     ASKUSE16
C          NTRK   : number of tracks generated and kept                 ASKUSE17
C                  (i.e. # KINE banks  written)                         ASKUSE18
C          NVRT   : number of vertices generated                        ASKUSE19
C                   (i.e. # VERT banks written)                         ASKUSE20
C          ECMS   : center of mass energy for the event (may be         ASKUSE21
C                   different from nominal cms energy)                  ASKUSE22
C          WEIT   : event weight ( not 1 if a weighting method is used) ASKUSE23
C -----------------------------------------------------------------     ASKUSE24
      REAL VRTX(4),PTRAK(4,2)                                           ASKUSE25
      INTEGER KFL(8)                                                    ASKUSE26
C                                                                       ASKUSE27
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
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
      COMMON/PYSUBS/MSEL,MSUB(200),KFIN(2,-40:40),CKIN(200)             PYTCOM 2
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)             PYTCOM 3
      COMMON/PYINT1/MINT(400),VINT(400)                                 PYTCOM 4
      COMMON/PYINT2/ISET(200),KFPR(200,2),COEF(200,20),ICOL(40,4,2)     PYTCOM 5
      COMMON/PYINT3/XSFX(2,-40:40),ISIG(1000,3),SIGH(1000)              PYTCOM 6
      COMMON/PYINT4/WIDP(21:40,0:40),WIDE(21:40,0:40),WIDS(21:40,3)     PYTCOM 7
      COMMON/PYINT5/NGEN(0:200,3),XSEC(0:200,3)                         PYTCOM 8
      COMMON/PYINT6/PROC(0:200)                                         PYTCOM 9
      CHARACTER PROC*28                                                 PYTCOM10
C                                                                       PYTCOM11
      COMMON / GLUPAR / SVERT(3),IFL,ECM                                GLUCOM 2
C     IFL      : LUND flavour , set to 0 by default, can be changed     GLUCOM 3
C     ECM      : nominal cms energy                                     GLUCOM 4
C     SVERT    : vertex smearing, set to 0. by default, can be changed  GLUCOM 5
      COMMON / GLUSTA / ICOULU(10)                                      GLUCOM 6
      DATA IFI / 0/                                                     ASKUSE32
C ------------------------------------------------------------------    ASKUSE33
C                                                                       ASKUSE34
      ISTA = 0                                                          ASKUSE35
      IFI  =  IFI + 1                                                   ASKUSE36
      NTRK = 0                                                          ASKUSE37
      NVRT = 0                                                          ASKUSE38
C - SET THE CMS ENERGY FOR THIS EVENT ECMS = ECM                        ASKUSE39
      ECMS = ECM                                                        ASKUSE40
      WEIT = 1.                                                         ASKUSE41
C                                                                       ASKUSE42
C - GET AN EVENT FROM PYTHIA                                            ASKUSE43
C                                                                       ASKUSE44
      IF ((MINT(1).GE.19 .AND. MINT(1).LE.27).or.(mint(1).eq.141)) THEN ASKUSE45
C     this is a W pair production or combination of Z0, W and/or H0     ASKUSE46
          MSTP111 = MSTP(111)                                           ASKUSE47
          MSTP(111)=0                                                   ASKUSE48
          MSTP(125)=1                                                   ASKUSE49
          CALL PYEVNT                                                   ASKUSE50
          CALL PYTCLR                                                   ASKUSE51
          CALL LUEXEC                                                   ASKUSE52
          MSTP(111)=MSTP111                                             ASKUSE53
      ELSE                                                              ASKUSE54
          MSTP(125)=0                                                   ASKUSE55
          CALL PYEVNT                                                   ASKUSE56
C                                                                       ASKUSE57
C - STORE BEAM PARTICLES ALSO IN BOS BANKS                              ASKUSE58
C                                                                       ASKUSE59
         IPART = KGPART(11)                                             ASKUSE60
          DO 2 ITR = 1,2                                                ASKUSE61
             DO 9 I=1,4                                                 ASKUSE62
 9              PTRAK(I,ITR) = 0.                                       ASKUSE63
             IPART = KGPART(11)                                         ASKUSE64
             PTRAK(3,ITR) = 0.5*ECM                                     ASKUSE65
             IF ( ITR.EQ.2) THEN                                        ASKUSE66
                IPART = KGPART(-11)                                     ASKUSE67
                PTRAK(3,ITR) =- 0.5*ECM                                 ASKUSE68
             ENDIF                                                      ASKUSE69
             IST=KBKINE(-ITR,PTRAK(1,ITR),IPART,0)                      ASKUSE70
             IF (IST.LE.0) THEN                                         ASKUSE71
                   ISTA=-2                                              ASKUSE72
                   GO TO 998                                            ASKUSE73
                ENDIF                                                   ASKUSE74
  2        CONTINUE                                                     ASKUSE75
      ENDIF                                                             ASKUSE76
C                                                                       ASKUSE77
C   FILL BOS BANKS                                                      ASKUSE78
C                                                                       ASKUSE79
C - GET THE PRIMARY VERTEX                                              ASKUSE80
      CALL RANNOR (RX,RY)                                               ASKUSE81
      CALL RANNOR (RZ,DUM)                                              ASKUSE82
      VRTX(1) = RX*SVERT(1)                                             ASKUSE83
      VRTX(2) = RY*SVERT(2)                                             ASKUSE84
      VRTX(3) = RZ*SVERT(3)                                             ASKUSE85
      VRTX(4) = 0.                                                      ASKUSE86
C                                                                       ASKUSE87
C     DEBUGGING FIRST FIVE EVENTS:                                      ASKUSE88
C                                                                       ASKUSE89
      IF (IFI.LE.5) CALL LULIST(1)                                      ASKUSE90
C                                                                       ASKUSE91
C      Call the specific routine KXL7AL to fill BOS banks               ASKUSE92
C      the secondary vertices are propagated                            ASKUSE93
      CALL KXL7AL (VRTX,ISTA,NVRT,NTRK)                                 ASKUSE94
C                                                                       ASKUSE95
C   Update IDPR                                                         ASKUSE96
C                                                                       ASKUSE97
        IDPR = 0                                                        ASKUSE98
C Look for flavor generated                                             ASKUSE99
        DO 11 I=1,8                                                     ASKUS100
 11      KFL(I)=0                                                       ASKUS101
        NFL=0                                                           ASKUS102
        DO 20 I=1,N7LU                                                  ASKUS103
           ITYP=ABS(KLU(I,9))                                           ASKUS104
           IF (ITYP.GT.8 .OR. ITYP.EQ.0) GO TO 20                       ASKUS105
           IF ( NFL.GT.0) THEN                                          ASKUS106
              DO 19 J=1,NFL                                             ASKUS107
              IF (ITYP.EQ.KFL(J)) GO TO 20                              ASKUS108
  19          CONTINUE                                                  ASKUS109
           ENDIF                                                        ASKUS110
           NFL=NFL+1                                                    ASKUS111
           KFL(NFL)=ITYP                                                ASKUS112
           IDPR=10*IDPR+ITYP                                            ASKUS113
  20    CONTINUE                                                        ASKUS114
C                                                                       ASKUS115
C  Add the original subprocess requested                                ASKUS116
C                                                                       ASKUS117
      IDPR = IDPR+10000*IFL                                             ASKUS118
      IF (MSTU(24).NE.0) THEN                                           ASKUS119
        WRITE(6,'(''  ---ERROR LUEXEC AT EVENT #  '',I10)') IFI         ASKUS120
        CALL LULIST(1)                                                  ASKUS121
        ISTA = -8                                                       ASKUS122
      ENDIF                                                             ASKUS123
 998  IF (ISTA.EQ.0 ) THEN                                              ASKUS124
         ICOULU(10) = ICOULU(10)+1                                      ASKUS125
      ELSEIF (ISTA.GT.0) THEN                                           ASKUS126
         ICOULU(1) = ICOULU(1) +1                                       ASKUS127
         ICOULU(9) = ICOULU(9) +1                                       ASKUS128
         CALL LULIST(1)                                                 ASKUS129
      ELSEIF ( ISTA.LT.0) THEN                                          ASKUS130
         ICOULU(-ISTA) = ICOULU(-ISTA) +1                               ASKUS131
         ICOULU(9) = ICOULU(9) +1                                       ASKUS132
      ENDIF                                                             ASKUS133
C                                                                       ASKUS134
C  -  FILLING HISTOGRAMS                                                ASKUS135
C                                                                       ASKUS136
      CALL HFILL(10001,FLOAT(N7LU),0.,1.)                               ASKUS137
      CALL HFILL(10002,P7LU(1,4),0.,1.)                                 ASKUS138
      CALL HFILL(10003,P7LU(2,4),0.,1.)                                 ASKUS139
      ET = SQRT(P7LU(1,1)*P7LU(1,1)+P7LU(1,2)*P7LU(1,2)                 ASKUS140
     .         +P7LU(1,3)*P7LU(1,3))                                    ASKUS141
      IF ( ET.GT.0.0001 )   CALL HFILL(10004,P7LU(1,3)/ET,0.,1.)        ASKUS142
      ET = SQRT(P7LU(2,1)*P7LU(2,1)+P7LU(2,2)*P7LU(2,2)                 ASKUS143
     .         +P7LU(2,3)*P7LU(2,3))                                    ASKUS144
      IF ( ET.GT.0.0001 )   CALL HFILL(10005,P7LU(2,3)/ET,0.,1.)        ASKUS145
      DO 10 I=1,N7LU                                                    ASKUS146
        IF(K7LU(I,2).LT.40)CALL HFILL(10006,FLOAT(K7LU(I,2)),0.,1.)     ASKUS147
 10   CONTINUE                                                          ASKUS148
C                                                                       ASKUS149
C -  ANALYSE TREE INFORMATION:                                          ASKUS150
C                                                                       ASKUS151
      CALL LUEDIT(2)                                                    ASKUS152
      NT=0                                                              ASKUS153
      NTC=0                                                             ASKUS154
      NNT=0                                                             ASKUS155
      ET=0.                                                             ASKUS156
      ECT=0.                                                            ASKUS157
      ENT=0.                                                            ASKUS158
      DO 30 I=1,N7LU                                                    ASKUS159
       NT=NT+1                                                          ASKUS160
       ET=ET+P7LU(I,4)                                                  ASKUS161
       IF(ABS(PLU(I,6)).GT.0.1)THEN                                     ASKUS162
        NTC=NTC+1                                                       ASKUS163
        ECT=ECT+P7LU(I,4)                                               ASKUS164
       ELSE                                                             ASKUS165
        NNT=NNT+1                                                       ASKUS166
        ENT=ENT+P7LU(I,4)                                               ASKUS167
        IF((ABS(P7LU(I,1))+ABS(P7LU(I,2))).LT.0.00001)THEN              ASKUS168
          CALL HFILL(10016,P7LU(I,4),0.,1.)                             ASKUS169
          ENT=ENT-P7LU(I,4)                                             ASKUS170
        ENDIF                                                           ASKUS171
       ENDIF                                                            ASKUS172
 30   CONTINUE                                                          ASKUS173
      CALL HFILL(10010,FLOAT(NT),0.,1.)                                 ASKUS174
      CALL HFILL(10011,ET,0.,1.)                                        ASKUS175
      CALL HFILL(10012,FLOAT(NTC),0.,1.)                                ASKUS176
      CALL HFILL(10013,ECT,0.,1.)                                       ASKUS177
      CALL HFILL(10014,FLOAT(NNT),0.,1.)                                ASKUS178
      CALL HFILL(10015,ENT,0.,1.)                                       ASKUS179
C                                                                       ASKUS180
C - THRUST:                                                             ASKUS181
C                                                                       ASKUS182
      CALL LUTHRU(THR,OBL)                                              ASKUS183
      CALL HFILL(10017,THR,0.,1.)                                       ASKUS184
C                                                                       ASKUS185
      RETURN                                                            ASKUS186
      END                                                               ASKUS187
      SUBROUTINE ASKUSI(IGCOD)                                          ASKUSI 2
C ------------------------------------------------------------------    ASKUSI 3
C - B. Bloch  - October   1991                                          ASKUSI 4
C! Initialization routine of PHYTIA 5.7 generator                       ASKUSI 5
C ------------------------------------------------------------------    ASKUSI 6
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
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      COMMON / GLUPAR / SVERT(3),IFL,ECM                                GLUCOM 2
C     IFL      : LUND flavour , set to 0 by default, can be changed     GLUCOM 3
C     ECM      : nominal cms energy                                     GLUCOM 4
C     SVERT    : vertex smearing, set to 0. by default, can be changed  GLUCOM 5
      COMMON / GLUSTA / ICOULU(10)                                      GLUCOM 6
      COMMON/PYSUBS/MSEL,MSUB(200),KFIN(2,-40:40),CKIN(200)             PYTCOM 2
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)             PYTCOM 3
      COMMON/PYINT1/MINT(400),VINT(400)                                 PYTCOM 4
      COMMON/PYINT2/ISET(200),KFPR(200,2),COEF(200,20),ICOL(40,4,2)     PYTCOM 5
      COMMON/PYINT3/XSFX(2,-40:40),ISIG(1000,3),SIGH(1000)              PYTCOM 6
      COMMON/PYINT4/WIDP(21:40,0:40),WIDE(21:40,0:40),WIDS(21:40,3)     PYTCOM 7
      COMMON/PYINT5/NGEN(0:200,3),XSEC(0:200,3)                         PYTCOM 8
      COMMON/PYINT6/PROC(0:200)                                         PYTCOM 9
      CHARACTER PROC*28                                                 PYTCOM10
C                                                                       PYTCOM11
      PARAMETER (LPDEC=48)                                              ASKUSI11
      INTEGER NODEC(LPDEC)                                              ASKUSI12
      INTEGER ALTABL,ALRLEP                                             ASKUSI13
      EXTERNAL ALTABL,ALRLEP                                            ASKUSI14
      DIMENSION TABL(25)                                                ASKUSI15
C  maximum code value for IFL parameter                                 ASKUSI16
      PARAMETER ( IFLMX = 13 )                                          ASKUSI17
C    IGCOD  for PYTHIA 5.7                                              ASKUSI18
      PARAMETER ( IGCO  =  5031)                                        ASKUSI19
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
C -----------------------------------------------------------------     ASKUSI21
C                                                                       ASKUSI22
      IUT = IW(6)                                                       ASKUSI23
C                                                                       ASKUSI24
C   Return generator code IGCOD                                         ASKUSI25
C                                                                       ASKUSI26
      IGCOD = IGCO                                                      ASKUSI27
      WRITE(IUT,101) IGCOD                                              ASKUSI28
 101  FORMAT(/,10X,                                                     ASKUSI29
     &       'PYTH03    CODE NUMBER =',I4,' LAST MODIFICATION ',        ASKUSI30
     $ 'May     3 , 1995'                                               DATE   2
     & ,/,10X,'***********************************************',//)     ASKUSI32
         NLUND = NAMIND ('GPYT')                                        ASKUSI33
         JLUND = IW(NLUND)                                              ASKUSI34
         IF (JLUND .NE. 0) THEN                                         ASKUSI35
            IFL  = IW(JLUND+1)                                          ASKUSI36
            IF(IFL.GE.0 .AND. IFL.LE.IFLMX) MSEL=0                      ASKUSI37
*   6 FLAVOUR HADRON PRODUCTION                                         ASKUSI38
            IF(IFL.EQ.0)THEN                                            ASKUSI39
             MSUB(1)=1                                                  ASKUSI40
             DO 9 IL=MDCY(23,2),MDCY(23,2)+MDCY(23,3)-1                 ASKUSI41
              IKFDP=KFDP(IL,1)                                          ASKUSI42
C        EXCLUDE ALL EXCEPT OF U,D,C,S,B,T                              ASKUSI43
              IF(IKFDP.LT.1 .OR. IKFDP.GT.6)MDME(IL,1)=MIN(0,MDME(IL,1))ASKUSI44
 9           CONTINUE                                                   ASKUSI45
C   5 FLAVOUR HADRON PRODUCTION                                         ASKUSI46
            ELSEIF(IFL.EQ.1)THEN                                        ASKUSI47
             DO 10 IL=MDCY(23,2),MDCY(23,2)+MDCY(23,3)-1                ASKUSI48
              IKFDP=KFDP(IL,1)                                          ASKUSI49
C        EXCLUDE ALL EXCEPT OF U,D,C,S,B                                ASKUSI50
              IF(IKFDP.LT.1 .OR. IKFDP.GT.5)MDME(IL,1)=MIN(0,MDME(IL,1))ASKUSI51
 10          CONTINUE                                                   ASKUSI52
             MSUB(1)=1                                                  ASKUSI53
C   ONLY T TBAR PRODUCTION                                              ASKUSI54
            ELSEIF(IFL.EQ.2)THEN                                        ASKUSI55
             DO 20 IL=MDCY(23,2),MDCY(23,2)+MDCY(23,3)-1                ASKUSI56
               IF(KFDP(IL,1).NE.6)MDME(IL,1)=MIN(0,MDME(IL,1))          ASKUSI57
 20          CONTINUE                                                   ASKUSI58
             MSUB(1)=1                                                  ASKUSI59
C   W+ W-   PRODUCTION                                                  ASKUSI60
            ELSEIF(IFL.EQ.3)THEN                                        ASKUSI61
             MSUB(25)=1                                                 ASKUSI62
C   Z0 Z0 PRODUCTION                                                    ASKUSI63
            ELSEIF(IFL.EQ.4)THEN                                        ASKUSI64
             MSUB(22)=1                                                 ASKUSI65
C   Z0 GAMMA PRODUCTION                                                 ASKUSI66
            ELSEIF(IFL.EQ.5)THEN                                        ASKUSI67
             MSUB(19)=1                                                 ASKUSI68
C   GAMMA GAMMA PRODUCTION                                              ASKUSI69
            ELSEIF(IFL.EQ.6)THEN                                        ASKUSI70
             MSUB(18)=1                                                 ASKUSI71
C   GAMMA H0    PRODUCTION                                              ASKUSI72
            ELSEIF(IFL.EQ.7)THEN                                        ASKUSI73
             MSUB(21)=1                                                 ASKUSI74
C   H0    H0    PRODUCTION                                              ASKUSI75
            ELSEIF(IFL.EQ.8)THEN                                        ASKUSI76
             MSUB(27)=1                                                 ASKUSI77
C   Z0    H0    PRODUCTION                                              ASKUSI78
            ELSEIF(IFL.EQ.9)THEN                                        ASKUSI79
             MSUB(24)=1                                                 ASKUSI80
C   NEUTRINO     PRODUCTION                                             ASKUSI81
            ELSEIF(IFL.EQ.10)THEN                                       ASKUSI82
             MSUB(1)=1                                                  ASKUSI83
             DO 30 IL=MDCY(23,2),MDCY(23,2)+MDCY(23,3)-1                ASKUSI84
               IKFDP=KFDP(IL,1)                                         ASKUSI85
C              EXCLUDE quark and lepton decays                          ASKUSI86
               IF(.NOT. (IKFDP.EQ.12.OR.IKFDP.EQ.14.OR.IKFDP.EQ.16))    ASKUSI87
     $              MDME(IL,1)=MIN(0,MDME(IL,1))                        ASKUSI88
 30          CONTINUE                                                   ASKUSI89
C   LEPTON     PRODUCTION                                               ASKUSI90
            ELSEIF(IFL.EQ.11)THEN                                       ASKUSI91
             MSUB(1)=1                                                  ASKUSI92
             DO 31 IL=MDCY(23,2),MDCY(23,2)+MDCY(23,3)-1                ASKUSI93
               IKFDP=KFDP(IL,1)                                         ASKUSI94
C              EXCLUDE quark and neutrino decays                        ASKUSI95
               IF(.NOT.(IKFDP.EQ.11.OR.IKFDP.EQ.13.OR.IKFDP.EQ.15))     ASKUSI96
     $              MDME(IL,1)=MIN(0,MDME(IL,1))                        ASKUSI97
 31          CONTINUE                                                   ASKUSI98
C   Photoproduction with qcd jets only                                  ASKUSI99
            ELSEIF(IFL.EQ.12)THEN                                       ASKUS100
              MSUB(11) =1                                               ASKUS101
              MSUB(12) =1                                               ASKUS102
              MSUB(13) =1                                               ASKUS103
              MSUB(28) =1                                               ASKUS104
              MSUB(33) =1                                               ASKUS105
              MSUB(53) =1                                               ASKUS106
              MSUB(54) =1                                               ASKUS107
              MSUB(68) =1                                               ASKUS108
C   Photoproduction with high pt photons only                           ASKUS109
            ELSEIF(IFL.EQ.13)THEN                                       ASKUS110
              MSUB(14) =1                                               ASKUS111
              MSUB(18) =1                                               ASKUS112
              MSUB(29) =1                                               ASKUS113
              MSUB(34) =1                                               ASKUS114
              MSUB(114) =1                                              ASKUS115
              MSUB(115) =1                                              ASKUS116
            ENDIF                                                       ASKUS117
C*******                                                                ASKUS118
            ECM  = RW(JLUND+2)                                          ASKUS119
            IPRT = IW(JLUND+3)                                          ASKUS120
         ELSE                                                           ASKUS121
            IFL = 0                                                     ASKUS122
            ECM = 91.2                                                  ASKUS123
            IPRT = 0                                                    ASKUS124
         ENDIF                                                          ASKUS125
C - make use of a smearing of the vertex  if it is given                ASKUS126
         NSVER = NAMIND ('SVRT')                                        ASKUS127
         JSVER = IW(NSVER)                                              ASKUS128
         IF (JSVER .NE. 0) THEN                                         ASKUS129
            SVERT(1) = RW(JSVER+1)                                      ASKUS130
            SVERT(2) = RW(JSVER+2)                                      ASKUS131
            SVERT(3) = RW(JSVER+3)                                      ASKUS132
         ELSE                                                           ASKUS133
            SVERT(1) = 0.0180                                           ASKUS134
            SVERT(2) = 0.0010                                           ASKUS135
            SVERT(3) = 1.00                                             ASKUS136
         ENDIF                                                          ASKUS137
C                                                                       ASKUS138
C   Issue the relevant parameters                                       ASKUS139
C                                                                       ASKUS140
      WRITE (IUT,1000)                                                  ASKUS141
      WRITE (IUT,1007)                                                  ASKUS142
 1000 FORMAT(1X,78('*'),/,/,10X,'WELCOME TO PYTHIA 5.7 USING JETSET 74'/ASKUS143
     $                      10X,'                                   '/) ASKUS144
 1007 FORMAT (1X,78('*') )                                              ASKUS145
C                                                                       ASKUS146
      DO 5  K=1,10                                                      ASKUS147
 5    ICOULU(K)=0                                                       ASKUS148
C                                                                       ASKUS149
C  Set up some default values for masses and initial conditions         ASKUS150
C                                                                       ASKUS151
      PMAS(LUCOMP(25),1)= 100.                                          ASKUS152
      PMAS(LUCOMP( 6),1)= 192.                                          ASKUS153
      PMAS(LUCOMP(23),1)= 91.2                                          ASKUS154
C   HIGGS Mass , TOP Mass and Z0 mass defined, can be overwritten by    ASKUS155
C   a PMA1 card                                                         ASKUS156
C                                                                       ASKUS157
C -- complete PART bank with LUND  particles                            ASKUS158
C    use the library routine KXL74A                                     ASKUS159
      CALL KXL74A (IPART,IKLIN)                                         ASKUS160
      IF (IPART.LE.0 .OR. IKLIN.LE.0) THEN                              ASKUS161
         WRITE (IW(6),'(1X,''error in PART or KLIN bank - STOP - ''     ASKUS162
     +                 ,2I3)') IPART,IKLIN                              ASKUS163
         CALL EXIT                                                      ASKUS164
      ENDIF                                                             ASKUS165
C   Make sure that masses in PART bank are consistent                   ASKUS166
      NAPAR = NAMIND('PART')                                            ASKUS167
C This is the aleph number of the Z0(lund code=23),top (6) and Higgs(25)ASKUS168
C function KGPART returns the ALEPH code corresponding to the LUND code ASKUS169
C required.                                                             ASKUS170
      JPART = IW(NAPAR)                                                 ASKUS171
      IZPART = KGPART(23)                                               ASKUS172
      IF (IZPART.GT.0)  THEN                                            ASKUS173
        ZMAS = PMAS(LUCOMP(23),1)                                       ASKUS174
        KPART = KROW(JPART,IZPART)                                      ASKUS175
        RW(KPART+6)=ZMAS                                                ASKUS176
        IANTI = ITABL(JPART,IZPART,10)                                  ASKUS177
        IF (IANTI.NE.IZPART) THEN                                       ASKUS178
          KAPAR = KROW(JPART,IANTI)                                     ASKUS179
          RW(KAPAR+6)=ZMAS                                              ASKUS180
        ENDIF                                                           ASKUS181
      ENDIF                                                             ASKUS182
      ITPART = KGPART(6)                                                ASKUS183
      IF (ITPART.GT.0)  THEN                                            ASKUS184
        ZMAS = PMAS(LUCOMP( 6),1)                                       ASKUS185
        KPART = KROW(JPART,ITPART)                                      ASKUS186
        RW(KPART+6)=ZMAS                                                ASKUS187
        IANTI = ITABL(JPART,ITPART,10)                                  ASKUS188
        IF (IANTI.NE.ITPART) THEN                                       ASKUS189
          KAPAR = KROW(JPART,IANTI)                                     ASKUS190
          RW(KAPAR+6)=ZMAS                                              ASKUS191
        ENDIF                                                           ASKUS192
      ENDIF                                                             ASKUS193
      IHPART = KGPART(25)                                               ASKUS194
      IF (IHPART.GT.0)  THEN                                            ASKUS195
        ZMAS = PMAS(LUCOMP(25),1)                                       ASKUS196
        KPART = KROW(JPART,IHPART)                                      ASKUS197
        RW(KPART+6)=ZMAS                                                ASKUS198
        IANTI = ITABL(JPART,IHPART,10)                                  ASKUS199
        IF (IANTI.NE.IHPART) THEN                                       ASKUS200
          KAPAR = KROW(JPART,IANTI)                                     ASKUS201
          RW(KAPAR+6)=ZMAS                                              ASKUS202
        ENDIF                                                           ASKUS203
      ENDIF                                                             ASKUS204
C                                                                       ASKUS205
C - Print PART and KLIN banks                                           ASKUS206
      IF (IPRT.GT.0) CALL PRPART                                        ASKUS207
       IF ( IPRT.GT.1) CALL LULIST(12)                                  ASKUS208
C                                                                       ASKUS209
C -- get list of  particle# which should not be decayed                 ASKUS210
C    in LUND  because they are decayed in GALEPH.                       ASKUS211
C    the routines uses the KLIN bank and fills the user array           ASKUS212
C    NODEC in the range [1-LPDEC]                                       ASKUS213
      MXDEC = KNODEC (NODEC,LPDEC)                                      ASKUS214
      MXDEC = MIN (MXDEC,LPDEC)                                         ASKUS215
C                                                                       ASKUS216
C -- inhibit decays in LUND                                             ASKUS217
C    If the user has set some decay channels by data cards they will    ASKUS218
C    will not be overwritten                                            ASKUS219
      IF (MXDEC .GT. 0) THEN                                            ASKUS220
         DO 40 I=1,MXDEC                                                ASKUS221
            IF (NODEC(I).GT.0) THEN                                     ASKUS222
               JIDB = NLINK('MDC1',NODEC(I))                            ASKUS223
               IF (JIDB .EQ. 0) MDCY(LUCOMP(NODEC(I)),1) = 0            ASKUS224
            ENDIF                                                       ASKUS225
   40    CONTINUE                                                       ASKUS226
      ENDIF                                                             ASKUS227
C                                                                       ASKUS228
C   dump the generator parameters for this run in a bank                ASKUS229
C assume all parameters are real and stored as a single row             ASKUS230
      TABL(1) = FLOAT(IFL)                                              ASKUS231
      TABL(2) = ECM                                                     ASKUS232
      DO 11 I=1,3                                                       ASKUS233
 11   TABL(2+I) = SVERT(I)                                              ASKUS234
      NWB =  5                                                          ASKUS235
      IND = ALTABL('KPAR',NWB,1,TABL,'2I,(F)','C')                      ASKUS236
C                                                                       ASKUS237
C  Fill RLEP bank                                                       ASKUS238
      IEBEAM = NINT(ECM* 500. )                                         ASKUS239
      JRLEP = ALRLEP(IEBEAM,'    ',0,0,0)                               ASKUS240
      CALL PRTABL('RLEP',0)                                             ASKUS241
      CALL PRTABL('KPAR',0)                                             ASKUS242
C                                                                       ASKUS243
C     get parameters of the PYTHIA package from cards file              ASKUS244
C                                                                       ASKUS245
      CALL PYCARD                                                       ASKUS246
C   Force non storage of documentation as this could give troubles to   ASKUS247
C   the interface : one needs MSTP(125)=1 for history of W decays       ASKUS248
C   so set up the value dynamically.....                                ASKUS249
C      MSTP(125)=0                                                      ASKUS250
C                                                                       ASKUS251
C    set up PYTHIA job with phyinit for e+e- anihilation in aleph       ASKUS252
C                                                                       ASKUS253
      WRITE(6,999)                                                      ASKUS254
 999  FORMAT(//                                                         ASKUS255
     .20X,' P Y T H I A  WILL BE INITIALIZED FOR THE'//                 ASKUS256
     .20X,'       A L E P H - BEAM DIRECTIONS   '//                     ASKUS257
     .20X,'        E-  IN   +Z     DIRECTION'/                          ASKUS258
     .20X,'        E+  IN   -Z     DIRECTION'//)                        ASKUS259
C                                                                       ASKUS260
      CALL PYINIT('CMS','E-','E+',ECM)                                  ASKUS261
C                                                                       ASKUS262
C  -  BOOKING HISTOGRAMS                                                ASKUS263
C                                                                       ASKUS264
       CALL HBOOK1(10001,'TOTAL MULTIPLICITY GENERATED',50,0.,250.,0.)  ASKUS265
       CALL HBOOK1(10002,'ENERGY OF OUTGOING PARTICLE',50,0.,ECM,0.)    ASKUS266
       CALL HIDOPT(10002,'LOGY')                                        ASKUS267
       CALL HBOOK1(10003,'ENERGY OF OUTGOING ANTI-PARTICLE',50,0.,ECM,0.ASKUS268
     $ )                                                                ASKUS269
       CALL HIDOPT(10003,'LOGY')                                        ASKUS270
       CALL HBOOK1(10004,'COS(THETA) OF OUTGOING PARTICLE',50,-1.,1.,0.)ASKUS271
       CALL HBOOK1(10005,'COS(THETA) OF ANTI-PARTICLE',50,-1.,1.,0.)    ASKUS272
       CALL HBOOK1(10006,'PRIMARY PARTICLES PRODUCED (FERMIONS + BOSONS'ASKUS273
     $            ,40,0.,40.,0.)                                        ASKUS274
       CALL HBOOK1(10010,'STABLE PART.: MULTIPLICITY',50,0.,150.,0.)    ASKUS275
       CALL HBOOK1(10011,'STABLE PART.: SUM OF ENERGY',50,0.,ECM,0.)    ASKUS276
       CALL HIDOPT(10011,'LOGY')                                        ASKUS277
       CALL HBOOK1(10012,'STABLE PART.: CHAR. MULTIPL.',50,0.,100.,0.)  ASKUS278
       CALL HBOOK1(10013,'STABLE PART.: CHAR. ENERGY',50,0.,ECM,0.)     ASKUS279
       CALL HBOOK1(10014,'STABLE PART.: NEUTRAL MULTIPL.',50,0.,100.,0.)ASKUS280
       CALL HBOOK1(10015,'STABLE PART.: NEUTRAL ENERGY',50,0.,ECM,0.)   ASKUS281
       CALL HBOOK1(10016,'STABLE PART.: NEUTR.ENERGY OF LOW PT TRACKS', ASKUS282
     .             50,0.,ECM,0.)                                        ASKUS283
       CALL HIDOPT(10016,'LOGY')                                        ASKUS284
       CALL HBOOK1(10017,'STABLE PART.: THRUST',50,0.,1.,0.)            ASKUS285
      RETURN                                                            ASKUS286
      END                                                               ASKUS287
      SUBROUTINE USCJOB                                                 USCJOB 2
C-------------------------------------------------------------------    USCJOB 3
C! End of job routine    Lund 7.3                                       USCJOB 4
C                                                                       USCJOB 5
C   To be filled by user to print any relevant info                     USCJOB 6
C                                                                       USCJOB 7
C------------------------------------------------------------------     USCJOB 8
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
      COMMON / GLUPAR / SVERT(3),IFL,ECM                                GLUCOM 2
C     IFL      : LUND flavour , set to 0 by default, can be changed     GLUCOM 3
C     ECM      : nominal cms energy                                     GLUCOM 4
C     SVERT    : vertex smearing, set to 0. by default, can be changed  GLUCOM 5
      COMMON / GLUSTA / ICOULU(10)                                      GLUCOM 6
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
C.......................................................................USCJOB12
C                                                                       USCJOB13
C   PRINTOUT OF STSTISTICS DONE BY PYTHIA                               USCJOB14
C                                                                       USCJOB15
       CALL PYSTAT(1)                                                   USCJOB16
C                                                                       USCJOB17
C   PRINTOUT OF DECAY CHANNELS,PARTIAL WIDTHS.... USED BY PYTHIA        USCJOB18
C                                                                       USCJOB19
C      CALL PYSTAT(2)                                                   USCJOB20
C                                                                       USCJOB21
       IUT=IW(6)                                                        USCJOB22
       WRITE(IUT,101)                                                   USCJOB23
  101  FORMAT(//20X,'EVENTS STATISTICS',                                USCJOB24
     &         /20X,'*****************')                                USCJOB25
       WRITE(IUT,102) ICOULU(9)+ICOULU(10),ICOULU(10),ICOULU(9)         USCJOB26
  102  FORMAT(/5X,'# OF GENERATED EVENTS                = ',I10,        USCJOB27
     &        /5X,'# OF ACCEPTED  EVENTS                = ',I10,        USCJOB28
     &        /5X,'# OF REJECTED  EVENTS                = ',I10)        USCJOB29
       WRITE(IUT,103)                                                   USCJOB30
  103  FORMAT(//20X,'REJECT STATISTICS',                                USCJOB31
     &         /20X,'*****************')                                USCJOB32
       WRITE(IUT,104) (ICOULU(I),I=1,8)                                 USCJOB33
  104  FORMAT(/10X,'IR= 1 LUND ERROR unknown part    # OF REJECT =',I10,USCJOB34
     &        /10X,'IR= 2 BOS  ERROR KINE/VERT       # OF REJECT =',I10,USCJOB35
     &        /10X,'IR= 3 LUND ERROR too many tracks # OF REJECT =',I10,USCJOB36
     &        /10X,'IR= 4 LUND ERROR Beam wrong pos  # OF REJECT =',I10,USCJOB37
     &        /10X,'IR= 5 LUND ERROR status code >5  # OF REJECT =',I10,USCJOB38
     &        /10X,'IR= 6 free for user              # OF REJECT =',I10,USCJOB39
     &        /10X,'IR= 7 free for user              # OF REJECT =',I10,USCJOB40
     &        /10X,'IR= 8 LUND error in LUEXEC       # OF REJECT =',I10)USCJOB41
      RETURN                                                            USCJOB42
      END                                                               USCJOB43
