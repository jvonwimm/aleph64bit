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
     .20X,'        FRANKM@CERNVM  OR  ALWS::FRANKM                 '//  PYCARD56
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
C W pair ( single copy of it ) second copy has ks=11 , or Z0 or H0 or GaBBL003 2
         ELSE IF ( KF.GE.22 .AND. KF.LE.25) THEN                        BBL003 3
           IF ( KS .EQ.11  ) GO TO 100                                  BBL003 4
           IF ( KF.EQ.22 .AND.(KM.EQ.7 .OR. KM.EQ.8)) GO TO 100         BBL003 5
           INEW = INEW+1                                                PYTCLR29
           K7LU(INEW,1) = 11                                            PYTCLR30
           K7LU(INEW,2) = K7LU(ILU,2)                                   PYTCLR31
           IF ( KF.EQ.22 ) K7LU(INEW,1) = 1                             BBL003 6
           K7LU(INEW,3) = 0                                             PYTCLR32
           K7LU(INEW,4) = 0                                             PYTCLR33
           K7LU(INEW,5) = 0                                             PYTCLR34
C W , Z , H   daughters = leptons or quarks , they are # 7 and 8        BBL003 7
         ELSE IF ( KM.EQ.7 .OR. KM.EQ.8  ) THEN                         PYTCLR36
           INEW = INEW+1                                                PYTCLR37
           K7LU(INEW,1) = 14                                            PYTCLR38
           IF ( KF.GT.10 ) K7LU(INEW,1) = 1                             PYTCLR39
           K7LU(INEW,2) = K7LU(ILU,2)                                   PYTCLR40
           K7LU(INEW,3) = K7LU(ILU,3)-4                                 PYTCLR41
           MOTH =  k7lu(inew,3)                                         PYTCLR42
           IF(K7LU(MOTH,4).LE.0 .AND. k7lu(MOTH,5).LE.0)                PYTCLR43
     $         k7lu(MOTH,4) = INEW                                      PYTCLR44
           IF(K7LU(MOTH,4).GT.0 .AND. k7lu(MOTH,5).LE.0)                PYTCLR45
     $    k7lu(MOTH,5) = INEW                                           PYTCLR46
C gammas                                                                PYTCLR47
         ELSE IF ( KF.EQ.22  ) THEN                                     PYTCLR48
            IF ( P7LU(ILU,4).LT.1.E-06 ) GO TO 100                      PYTCLR49
            INEW = INEW +1                                              PYTCLR50
            K7LU(INEW,1) = K7LU(ILU,1)                                  PYTCLR51
            K7LU(INEW,2) = K7LU(ILU,2)                                  PYTCLR52
            IF ( KM.LE.2 ) K7LU(INEW,3) = K7LU(ILU,3)                   PYTCLR53
            IF ( KM.GT.2 ) K7LU(INEW,3) = 0                             PYTCLR54
            K7LU(INEW,4) = 0                                            PYTCLR55
            K7LU(INEW,5) = 0                                            PYTCLR56
C extra copy of lepton daughters                                        PYTCLR57
         ELSE IF ( KF.GE.11.AND.KF.LE.16 ) THEN                         PYTCLR58
           GO TO 100                                                    PYTCLR59
C quarks arranged in a string with gluons                               PYTCLR60
         ELSE IF ( KF.LT.10 .OR. KF.EQ.21) THEN                         PYTCLR61
            INEW = INEW +1                                              PYTCLR62
            K7LU(INEW,1) = K7LU(ILU,1)                                  PYTCLR63
            K7LU(INEW,2) = K7LU(ILU,2)                                  PYTCLR64
            K7LU(INEW,3) = K7LU(ILU,3)-4                                PYTCLR65
            K7LU(INEW,4) = K7LU(ILU,4)                                  PYTCLR66
            K7LU(INEW,5) = K7LU(ILU,5)                                  PYTCLR67
         ENDIF                                                          PYTCLR68
         DO 10 I=1,5                                                    PYTCLR69
           P7LU(INEW,I) = P7LU(ILU,I)                                   PYTCLR70
   10    CONTINUE                                                       PYTCLR71
 100  CONTINUE                                                          PYTCLR72
      N7LU = INEW                                                       PYTCLR73
      MSTU(70)=1                                                        PYTCLR74
      RETURN                                                            PYTCLR75
      END                                                               PYTCLR76
      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)                 ASKUSE 2
C ------------------------------------------------------------          ASKUSE 3
C -  B. Bloch  - September 1990 -June 1994 -November 94                 BBL003 8
C! GET AN EVENT FROM PYTHIA 5.5                                         BBL003 9
C! clean it to keep frmion decay products from Z0, W+- and H0           BBL00310
C! then transfer the information into kine and vert banks.              BBL00311
C                                                                       ASKUSE 7
C  PYTHIA 5.6 NEEDS THE FULL STUFF OF JETSET 7.3                        ASKUSE 8
C                                                                       ASKUSE 9
C     structure : subroutine                                            ASKUSE10
C     output arguments :                                                ASKUSE11
C          IDPR   : process identification,each digit corresponds to    ASKUSE12
C          the flavor of the evnt ( several flavors /event is possible) ASKUSE13
C          ISTA   : status flag ( 0 means ok), use it to reject         ASKUSE14
C                   unwanted events                                     ASKUSE15
C          NTRK   : number of tracks generated and kept                 ASKUSE16
C                  (i.e. # KINE banks  written)                         ASKUSE17
C          NVRT   : number of vertices generated                        ASKUSE18
C                   (i.e. # VERT banks written)                         ASKUSE19
C          ECMS   : center of mass energy for the event (may be         ASKUSE20
C                   different from nominal cms energy)                  ASKUSE21
C          WEIT   : event weight ( not 1 if a weighting method is used) ASKUSE22
C -----------------------------------------------------------------     ASKUSE23
      REAL VRTX(4),PTRAK(4,2)                                           ASKUSE24
      INTEGER KFL(8)                                                    ASKUSE25
C                                                                       ASKUSE26
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
      DATA IFI / 0/                                                     ASKUSE31
C ------------------------------------------------------------------    ASKUSE32
C                                                                       ASKUSE33
      ISTA = 0                                                          ASKUSE34
      IFI  =  IFI + 1                                                   ASKUSE35
      NTRK = 0                                                          ASKUSE36
      NVRT = 0                                                          ASKUSE37
C - SET THE CMS ENERGY FOR THIS EVENT ECMS = ECM                        ASKUSE38
      ECMS = ECM                                                        ASKUSE39
      WEIT = 1.                                                         ASKUSE40
C                                                                       ASKUSE41
C - GET AN EVENT FROM PYTHIA                                            ASKUSE42
C                                                                       ASKUSE43
      IF (MINT(1).GE.19 .AND. MINT(1).LE.27) THEN                       BBL00312
C     this is a W pair production or combination of Z0, W and/or H0     BBL00313
          MSTP111 = MSTP(111)                                           BBL002 6
          MSTP(111)=0                                                   BBL002 7
          MSTP(125)=1                                                   BBL002 8
          CALL PYEVNT                                                   BBL002 9
          CALL PYTCLR                                                   BBL00210
          CALL LUEXEC                                                   BBL00211
          MSTP(111)=MSTP111                                             BBL00212
      ELSE                                                              BBL00213
          MSTP(125)=0                                                   BBL00214
          CALL PYEVNT                                                   BBL00215
C                                                                       ASKUSE45
C - STORE BEAM PARTICLES ALSO IN BOS BANKS                              ASKUSE46
C                                                                       ASKUSE47
         IPART = KGPART(11)                                             ASKUSE48
          DO 2 ITR = 1,2                                                ASKUSE49
             DO 9 I=1,4                                                 ASKUSE50
 9              PTRAK(I,ITR) = 0.                                       ASKUSE51
             IPART = KGPART(11)                                         ASKUSE52
             PTRAK(3,ITR) = 0.5*ECM                                     ASKUSE53
             IF ( ITR.EQ.2) THEN                                        ASKUSE54
                IPART = KGPART(-11)                                     ASKUSE55
                PTRAK(3,ITR) =- 0.5*ECM                                 ASKUSE56
             ENDIF                                                      ASKUSE57
             IST=KBKINE(-ITR,PTRAK(1,ITR),IPART,0)                      ASKUSE58
             IF (IST.LE.0) THEN                                         ASKUSE59
                   ISTA=-2                                              ASKUSE60
                   GO TO 998                                            ASKUSE61
                ENDIF                                                   ASKUSE62
  2        CONTINUE                                                     ASKUSE63
      ENDIF                                                             BBL00216
C                                                                       ASKUSE64
C   FILL BOS BANKS                                                      ASKUSE65
C                                                                       ASKUSE66
C - GET THE PRIMARY VERTEX                                              ASKUSE67
      CALL RANNOR (RX,RY)                                               ASKUSE68
      CALL RANNOR (RZ,DUM)                                              ASKUSE69
      VRTX(1) = RX*SVERT(1)                                             ASKUSE70
      VRTX(2) = RY*SVERT(2)                                             ASKUSE71
      VRTX(3) = RZ*SVERT(3)                                             ASKUSE72
      VRTX(4) = 0.                                                      ASKUSE73
C                                                                       ASKUSE74
C     DEBUGGING FIRST FIVE EVENTS:                                      ASKUSE75
C                                                                       ASKUSE76
      IF (IFI.LE.5) CALL LULIST(1)                                      ASKUSE77
C                                                                       ASKUSE78
C      Call the specific routine KXL7AL to fill BOS banks               ASKUSE79
C      the secondary vertices are propagated                            ASKUSE80
      CALL KXL7AL (VRTX,ISTA,NVRT,NTRK)                                 ASKUSE81
C                                                                       ASKUSE82
C   Update IDPR                                                         ASKUSE83
C                                                                       ASKUSE84
        IDPR = 0                                                        ASKUSE85
C Look for flavor generated                                             ASKUSE86
        DO 11 I=1,8                                                     ASKUSE87
 11      KFL(I)=0                                                       ASKUSE88
        NFL=0                                                           ASKUSE89
        DO 20 I=1,N7LU                                                  ASKUSE90
           ITYP=ABS(KLU(I,9))                                           ASKUSE91
           IF (ITYP.GT.8 .OR. ITYP.EQ.0) GO TO 20                       ASKUSE92
           IF ( NFL.GT.0) THEN                                          ASKUSE93
              DO 19 J=1,NFL                                             ASKUSE94
              IF (ITYP.EQ.KFL(J)) GO TO 20                              ASKUSE95
  19          CONTINUE                                                  ASKUSE96
           ENDIF                                                        ASKUSE97
           NFL=NFL+1                                                    ASKUSE98
           KFL(NFL)=ITYP                                                ASKUSE99
           IDPR=10*IDPR+ITYP                                            ASKUS100
  20    CONTINUE                                                        ASKUS101
C                                                                       ASKUS102
C  Add the original subprocess requested                                ASKUS103
C                                                                       ASKUS104
      IDPR = IDPR+10000*IFL                                             ASKUS105
      IF (MSTU(24).NE.0) THEN                                           ASKUS106
        WRITE(6,'(''  ---ERROR LUEXEC AT EVENT #  '',I10)') IFI         ASKUS107
        CALL LULIST(1)                                                  ASKUS108
        ISTA = -8                                                       ASKUS109
      ENDIF                                                             ASKUS110
 998  IF (ISTA.EQ.0 ) THEN                                              ASKUS111
         ICOULU(10) = ICOULU(10)+1                                      ASKUS112
      ELSEIF (ISTA.GT.0) THEN                                           ASKUS113
         ICOULU(1) = ICOULU(1) +1                                       ASKUS114
         ICOULU(9) = ICOULU(9) +1                                       ASKUS115
         CALL LULIST(1)                                                 BBL00217
      ELSEIF ( ISTA.LT.0) THEN                                          ASKUS116
         ICOULU(-ISTA) = ICOULU(-ISTA) +1                               ASKUS117
         ICOULU(9) = ICOULU(9) +1                                       ASKUS118
      ENDIF                                                             ASKUS119
C                                                                       ASKUS120
C  -  FILLING HISTOGRAMS                                                ASKUS121
C                                                                       ASKUS122
      CALL HFILL(10001,FLOAT(N7LU),0.,1.)                               ASKUS123
      CALL HFILL(10002,P7LU(1,4),0.,1.)                                 ASKUS124
      CALL HFILL(10003,P7LU(2,4),0.,1.)                                 ASKUS125
      ET = SQRT(P7LU(1,1)*P7LU(1,1)+P7LU(1,2)*P7LU(1,2)                 ASKUS126
     .         +P7LU(1,3)*P7LU(1,3))                                    ASKUS127
      IF ( ET.GT.0.0001 )   CALL HFILL(10004,P7LU(1,3)/ET,0.,1.)        ASKUS128
      ET = SQRT(P7LU(2,1)*P7LU(2,1)+P7LU(2,2)*P7LU(2,2)                 ASKUS129
     .         +P7LU(2,3)*P7LU(2,3))                                    ASKUS130
      IF ( ET.GT.0.0001 )   CALL HFILL(10005,P7LU(2,3)/ET,0.,1.)        ASKUS131
      DO 10 I=1,N7LU                                                    ASKUS132
        IF(K7LU(I,2).LT.40)CALL HFILL(10006,FLOAT(K7LU(I,2)),0.,1.)     ASKUS133
 10   CONTINUE                                                          ASKUS134
C                                                                       ASKUS135
C -  ANALYSE TREE INFORMATION:                                          ASKUS136
C                                                                       ASKUS137
      CALL LUEDIT(2)                                                    ASKUS138
      NT=0                                                              ASKUS139
      NTC=0                                                             ASKUS140
      NNT=0                                                             ASKUS141
      ET=0.                                                             ASKUS142
      ECT=0.                                                            ASKUS143
      ENT=0.                                                            ASKUS144
      DO 30 I=1,N7LU                                                    ASKUS145
       NT=NT+1                                                          ASKUS146
       ET=ET+P7LU(I,4)                                                  ASKUS147
       IF(ABS(PLU(I,6)).GT.0.1)THEN                                     ASKUS148
        NTC=NTC+1                                                       ASKUS149
        ECT=ECT+P7LU(I,4)                                               ASKUS150
       ELSE                                                             ASKUS151
        NNT=NNT+1                                                       ASKUS152
        ENT=ENT+P7LU(I,4)                                               ASKUS153
        IF((ABS(P7LU(I,1))+ABS(P7LU(I,2))).LT.0.00001)THEN              ASKUS154
          CALL HFILL(10016,P7LU(I,4),0.,1.)                             ASKUS155
          ENT=ENT-P7LU(I,4)                                             ASKUS156
        ENDIF                                                           ASKUS157
       ENDIF                                                            ASKUS158
 30   CONTINUE                                                          ASKUS159
      CALL HFILL(10010,FLOAT(NT),0.,1.)                                 ASKUS160
      CALL HFILL(10011,ET,0.,1.)                                        ASKUS161
      CALL HFILL(10012,FLOAT(NTC),0.,1.)                                ASKUS162
      CALL HFILL(10013,ECT,0.,1.)                                       ASKUS163
      CALL HFILL(10014,FLOAT(NNT),0.,1.)                                ASKUS164
      CALL HFILL(10015,ENT,0.,1.)                                       ASKUS165
C                                                                       ASKUS166
C - THRUST:                                                             ASKUS167
C                                                                       ASKUS168
      CALL LUTHRU(THR,OBL)                                              ASKUS169
      CALL HFILL(10017,THR,0.,1.)                                       ASKUS170
C                                                                       ASKUS171
      RETURN                                                            ASKUS172
      END                                                               ASKUS173
      SUBROUTINE ASKUSI(IGCOD)                                          ASKUSI 2
C ------------------------------------------------------------------    ASKUSI 3
C - B. Bloch  - October   1991                                          ASKUSI 4
C! Initialization routine of PHYTIA 5.6 generator                       ASKUSI 5
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
C    IGCOD  for PYTHIA 5.6                                              ASKUSI18
      PARAMETER ( IGCO  =  5016)                                        ASKUSI19
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
     &       'PYTH02    CODE NUMBER =',I4,' LAST MODIFICATION ',        BBL001 2
     $ 'November  28 , 1994'                                            BBL003 1
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
 1000 FORMAT(1X,78('*'),/,/,10X,'WELCOME TO PYTHIA 5.6 USING JETSET 73'/ASKUS143
     $                      10X,'                                   '/) ASKUS144
 1007 FORMAT (1X,78('*') )                                              ASKUS145
C                                                                       ASKUS146
      DO 5  K=1,10                                                      ASKUS147
 5    ICOULU(K)=0                                                       ASKUS148
C                                                                       ASKUS149
C  Set up some default values for masses and initial conditions         ASKUS150
C                                                                       ASKUS151
      PMAS(LUCOMP(25),1)= 100.                                          ASKUS152
      PMAS(LUCOMP( 6),1)= 100.                                          ASKUS153
      PMAS(LUCOMP(23),1)= 91.2                                          ASKUS154
C   HIGGS Mass , TOP Mass and Z0 mass defined, can be overwritten by    ASKUS155
C   a PMA1 card                                                         ASKUS156
C                                                                       ASKUS157
C -- complete PART bank with LUND  particles                            ASKUS158
C    use the library routine KXL7PA                                     ASKUS159
      CALL KXL7PA (IPART,IKLIN)                                         ASKUS160
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
C   the interface : one needs MSTP(125)=1 for history of W decays       BBL00218
C   so set up the value dynamically.....                                BBL00219
C      MSTP(125)=0                                                      BBL00220
C                                                                       ASKUS250
C    set up PYTHIA job with phyinit for e+e- anihilation in aleph       ASKUS251
C                                                                       ASKUS252
      WRITE(6,999)                                                      ASKUS253
 999  FORMAT(//                                                         ASKUS254
     .20X,' P Y T H I A  WILL BE INITIALIZED FOR THE'//                 ASKUS255
     .20X,'       A L E P H - BEAM DIRECTIONS   '//                     ASKUS256
     .20X,'        E-  IN   +Z     DIRECTION'/                          ASKUS257
     .20X,'        E+  IN   -Z     DIRECTION'//)                        ASKUS258
C                                                                       ASKUS259
      CALL PYINIT('CMS','E-','E+',ECM)                                  ASKUS260
C                                                                       ASKUS261
C  -  BOOKING HISTOGRAMS                                                ASKUS262
C                                                                       ASKUS263
       CALL HBOOK1(10001,'TOTAL MULTIPLICITY GENERATED',50,0.,250.,0.)  ASKUS264
       CALL HBOOK1(10002,'ENERGY OF OUTGOING PARTICLE',50,0.,ECM,0.)    ASKUS265
       CALL HIDOPT(10002,'LOGY')                                        ASKUS266
       CALL HBOOK1(10003,'ENERGY OF OUTGOING ANTI-PARTICLE',50,0.,ECM,0.ASKUS267
     $ )                                                                ASKUS268
       CALL HIDOPT(10003,'LOGY')                                        ASKUS269
       CALL HBOOK1(10004,'COS(THETA) OF OUTGOING PARTICLE',50,-1.,1.,0.)ASKUS270
       CALL HBOOK1(10005,'COS(THETA) OF ANTI-PARTICLE',50,-1.,1.,0.)    ASKUS271
       CALL HBOOK1(10006,'PRIMARY PARTICLES PRODUCED (FERMIONS + BOSONS'ASKUS272
     $            ,40,0.,40.,0.)                                        ASKUS273
       CALL HBOOK1(10010,'STABLE PART.: MULTIPLICITY',50,0.,150.,0.)    ASKUS274
       CALL HBOOK1(10011,'STABLE PART.: SUM OF ENERGY',50,0.,ECM,0.)    ASKUS275
       CALL HIDOPT(10011,'LOGY')                                        ASKUS276
       CALL HBOOK1(10012,'STABLE PART.: CHAR. MULTIPL.',50,0.,100.,0.)  ASKUS277
       CALL HBOOK1(10013,'STABLE PART.: CHAR. ENERGY',50,0.,ECM,0.)     ASKUS278
       CALL HBOOK1(10014,'STABLE PART.: NEUTRAL MULTIPL.',50,0.,100.,0.)BBL00221
       CALL HBOOK1(10015,'STABLE PART.: NEUTRAL ENERGY',50,0.,ECM,0.)   ASKUS280
       CALL HBOOK1(10016,'STABLE PART.: NEUTR.ENERGY OF LOW PT TRACKS', ASKUS281
     .             50,0.,ECM,0.)                                        ASKUS282
       CALL HIDOPT(10016,'LOGY')                                        ASKUS283
       CALL HBOOK1(10017,'STABLE PART.: THRUST',50,0.,1.,0.)            ASKUS284
      RETURN                                                            ASKUS285
      END                                                               ASKUS286
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
