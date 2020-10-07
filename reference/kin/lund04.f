      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)                 ASKUSE 2
C ------------------------------------------------------------          ASKUSE 3
C -  B. Bloch  - March 1994                                             ASKUSE 4
C! get an event from LUND 7.4                                           ASKUSE 5
C  then transfer the information into KINE and VERT banks.              ASKUSE 6
C                                                                       ASKUSE 7
C                                                                       ASKUSE 8
C     structure : subroutine                                            ASKUSE 9
C     output arguments :                                                ASKUSE10
C          IDPR   : process identification,each digit corresponds to    ASKUSE11
C          the flavor of the evnt ( several flavors /event is possible) ASKUSE12
C          ISTA   : status flag ( 0 means ok), use it to reject         ASKUSE13
C                   unwanted events                                     ASKUSE14
C          NTRK   : number of tracks generated and kept                 ASKUSE15
C                  (i.e. # KINE banks  written)                         ASKUSE16
C          NVRT   : number of vertices generated                        ASKUSE17
C                   (i.e. # VERT banks written)                         ASKUSE18
C          ECMS   : center of mass energy for the event (may be         ASKUSE19
C                   different from nominal cms energy)                  ASKUSE20
C          WEIT   : event weight ( not 1 if a weighting method is used) ASKUSE21
C -----------------------------------------------------------------     ASKUSE22
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
      COMMON / GLUPAR / IFL,IPRI,SVERT(3),ECM                           GLUCOM 2
C     IFL      : LUND flavour , set to 0 by default, can be changed     GLUCOM 3
C     IPRI     : PRINT level flag                                       GLUCOM 4
C     ECM      : nominal cms energy                                     GLUCOM 5
C     SVERT    : vertex smearing, set to 0. by default, can be changed  GLUCOM 6
      COMMON / GLUSTA / ICOULU(10)                                      GLUCOM 7
      REAL VRTX(4) ,ZB(LJNPAR)                                          ASKUSE26
      INTEGER KFL(8)                                                    ASKUSE27
      INTEGER ALTABL                                                    ASKUSE28
C                                                                       ASKUSE29
      DATA IFI / 0/                                                     ASKUSE30
C ------------------------------------------------------------------    ASKUSE31
C                                                                       ASKUSE32
      ISTA = 0                                                          ASKUSE33
C  Reset ZB storage  and entries in /LUJETS/                            ASKUSE34
      CALL VZERO(ZB,LJNPAR)                                             ASKUSE35
      N7LU = 0                                                          ASKUSE36
C Reset fragmentation storage in common                                 ASKUSE37
      MSTU(90) = 0                                                      ASKUSE38
C                                                                       ASKUSE39
C   get an event from the generator                                     ASKUSE40
C                                                                       ASKUSE41
      IFI  =  IFI + 1                                                   ASKUSE42
C - set the process identification IDPR = IFL                           ASKUSE43
      IDPR = IFL                                                        ASKUSE44
C - set the cms energy for this event ECMS = ECM                        ASKUSE45
      ECMS = ECM                                                        ASKUSE46
      WEIT = 1.                                                         ASKUSE47
C - get an event from LUND generator                                    ASKUSE48
      IF (IFL.EQ.-1) THEN                                               ASKUSE49
        CALL SIGENE(IDPR,ISTAT,ECMS)                                    ASKUSE50
        WEIT = 1.                                                       ASKUSE51
      ELSEIF ( IFL.GE.0) THEN                                           ASKUSE52
        ECMS = ECM                                                      ASKUSE53
        WEIT = 1.                                                       ASKUSE54
        CALL LUEEVT (IFL,ECMS)                                          ASKUSE55
      ENDIF                                                             ASKUSE56
C PRODUCE THE FINAL LUND EVENT                                          ASKUSE57
      CALL LUEXEC                                                       ASKUSE58
      IF ((N7LU.GT.0).AND.(MSTU(90).GT.0) ) THEN                        ASKUSE59
         DO 10 I= 1, MSTU(90)                                           ASKUSE60
            J = MSTU(91+I-1)                                            ASKUSE61
            ZB(J)=PARU(91+I-1)                                          ASKUSE62
10       CONTINUE                                                       ASKUSE63
      ENDIF                                                             ASKUSE64
      IF ( IPRI/10 .GT.0) THEN                                          ASKUSE65
         CALL LUTABU(11)                                                ASKUSE66
         CALL LUTABU(21)                                                ASKUSE67
         IF (IFL.EQ.-1) CALL LUTABU(51)                                 ASKUSE68
      ENDIF                                                             ASKUSE69
C                                                                       ASKUSE70
      IF (IFI.LE.5) CALL LULIST(1)                                      ASKUSE71
C                                                                       ASKUSE72
C   fill BOS banks                                                      ASKUSE73
C                                                                       ASKUSE74
C - get the primary vertex                                              ASKUSE75
      CALL RANNOR (RX,RY)                                               ASKUSE76
      CALL RANNOR (RZ,DUM)                                              ASKUSE77
      VRTX(1) = RX*SVERT(1)                                             ASKUSE78
      VRTX(2) = RY*SVERT(2)                                             ASKUSE79
      VRTX(3) = RZ*SVERT(3)                                             ASKUSE80
      VRTX(4) = 0.                                                      ASKUSE81
C                                                                       ASKUSE82
      DO 30 ITR = 1 , N7LU                                              ASKUSE83
        IMOTH = K7LU(ITR,3)                                             ASKUSE84
        IMTYP = IABS(K7LU(IMOTH,2))                                     ASKUSE85
        ILUN = K7LU(ITR,2)                                              ASKUSE86
C   Final state radiation photon comes from a quark                     ASKUSE87
        IF (ILUN.EQ.22 .AND. IMTYP.GE.1 .AND. IMTYP.LE.6)               ASKUSE88
     &       CALL HFILL(10023,P7LU(ITR,4),DUM,1.)                       ASKUSE89
C  Main vertex particles                                                ASKUSE90
        IF (IMOTH.LE.1.AND. (ILUN.EQ.22))THEN                           ASKUSE91
C    There is a photon                                                  ASKUSE92
           CALL HFILL(10003,P7LU(ITR,4),DUM,1.)                         ASKUSE93
        ELSEIF (ILUN.GT.0 .AND. ILUN.LE.6.AND.IMTYP.EQ.23) THEN         ASKUSE94
C   fermion                                                             ASKUSE95
           CALL HFILL(10001,P7LU(ITR,4),DUM,1.)                         ASKUSE96
           EE=P7LU(ITR,3)/SQRT(P7LU(ITR,1)**2+P7LU(ITR,2)**2+           ASKUSE97
     $       P7LU(ITR,3)**2)                                            ASKUSE98
           CALL HFILL(10004,EE,DUM,1.)                                  ASKUSE99
        ELSEIF (ILUN.LT.0 .AND. ILUN.GE.-6.AND.IMTYP.EQ.23)  THEN       ASKUS100
C   anti-fermion                                                        ASKUS101
           CALL HFILL(10002,P7LU(ITR,4),DUM,1.)                         ASKUS102
           EE= P7LU(ITR,3)/SQRT(P7LU(ITR,1)**2+P7LU(ITR,2)**2+          ASKUS103
     &     P7LU(ITR,3)**2)                                              ASKUS104
           CALL HFILL(10005,EE,DUM,1.)                                  ASKUS105
      ENDIF                                                             ASKUS106
  30  CONTINUE                                                          ASKUS107
      EE=N7LU                                                           ASKUS108
      CALL HFILL(10006,EE,DUM,1.)                                       ASKUS109
C                                                                       ASKUS110
C   Call the specific routine KXL7AL to fill BOS banks                  ASKUS111
      CALL KXL7AL (VRTX,ISTA,NVRT,NTRK)                                 ASKUS112
C                                                                       ASKUS113
C   Update IDPR                                                         ASKUS114
C                                                                       ASKUS115
        IDPR = 0                                                        ASKUS116
C Look for flavor generated                                             ASKUS117
        DO 5 I=1,8                                                      ASKUS118
 5      KFL(I)=0                                                        ASKUS119
        NFL=0                                                           ASKUS120
        DO 40 I=1,N7LU                                                  ASKUS121
           ITYP=ABS(KLU(I,9))                                           ASKUS122
           IF (ITYP.GT.8 .OR. ITYP.EQ.0) GO TO 40                       ASKUS123
           IF ( NFL.GT.0) THEN                                          ASKUS124
              DO 41 J=1,NFL                                             ASKUS125
              IF (ITYP.EQ.KFL(J)) GO TO 40                              ASKUS126
  41          CONTINUE                                                  ASKUS127
           ENDIF                                                        ASKUS128
           NFL=NFL+1                                                    ASKUS129
           KFL(NFL)=ITYP                                                ASKUS130
           IDPR=10*IDPR+ITYP                                            ASKUS131
  40    CONTINUE                                                        ASKUS132
C   Set the ZB value according to KINE numbering, i.e. remove beam part.ASKUS133
C   and transmit z of mother to subsequent heavy baryons and mesons     ASKUS134
      IBEA = 0                                                          ASKUS135
      DO 27 ITR=1,N7LU                                                  ASKUS136
      KS = K7LU(ITR,1)                                                  ASKUS137
      KM = K7LU(ITR,3)                                                  ASKUS138
C  Give same z to all daughters  of a mother                            ASKUS139
      IF (KM.GT.IBEA .AND. ZB(KM-IBEA).GT.0. ) ZB(ITR) = ZB(KM-IBEA)    ASKUS140
      IF ( KS.EQ.21 .AND. ABS(K7LU(ITR,2)).EQ.11 ) THEN                 ASKUS141
       IBEA = IBEA +1                                                   ASKUS142
      ELSE                                                              ASKUS143
       ZB(ITR-IBEA) = ZB (ITR)                                          ASKUS144
      ENDIF                                                             ASKUS145
 27   CONTINUE                                                          ASKUS146
C                                                                       ASKUS147
C   Book & fill the bank KZFR with info on fragmentation                ASKUS148
C                                                                       ASKUS149
      NP = N7LU-IBEA                                                    ASKUS150
      JKZFR = ALTABL('KZFR',1,NP,ZB,'2I,(F)','E')                       ASKUS151
      IF(JKZFR.LE.0) ISTAT = -1                                         ASKUS152
C                                                                       ASKUS153
      IF (MSTU(24).NE.0) THEN                                           ASKUS154
        WRITE(6,'(''  ---ERROR LUEXEC AT EVENT #  '',I10)') IFI         ASKUS155
        CALL LULIST(1)                                                  ASKUS156
        ISTA = -8                                                       ASKUS157
      ENDIF                                                             ASKUS158
      IF (ISTA.EQ.0 ) THEN                                              ASKUS159
         ICOULU(10) = ICOULU(10)+1                                      ASKUS160
      ELSEIF (ISTA.GT.0) THEN                                           ASKUS161
         ICOULU(1) = ICOULU(1) +1                                       ASKUS162
         ICOULU(9) = ICOULU(9) +1                                       ASKUS163
      ELSEIF ( ISTA.LT.0) THEN                                          ASKUS164
         ICOULU(-ISTA) = ICOULU(-ISTA) +1                               ASKUS165
         ICOULU(9) = ICOULU(9) +1                                       ASKUS166
      ENDIF                                                             ASKUS167
      END                                                               ASKUS168
      SUBROUTINE ASKUSI(IGCOD)                                          ASKUSI 2
C ------------------------------------------------------------------    ASKUSI 3
C - B. Bloch  - March 1994                                              ASKUSI 4
C! Initialization routine of LUND 7.3  generator                        ASKUSI 5
C                                                                       ASKUSI 6
C ------------------------------------------------------------------    ASKUSI 7
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
      COMMON / GLUPAR / IFL,IPRI,SVERT(3),ECM                           GLUCOM 2
C     IFL      : LUND flavour , set to 0 by default, can be changed     GLUCOM 3
C     IPRI     : PRINT level flag                                       GLUCOM 4
C     ECM      : nominal cms energy                                     GLUCOM 5
C     SVERT    : vertex smearing, set to 0. by default, can be changed  GLUCOM 6
      COMMON / GLUSTA / ICOULU(10)                                      GLUCOM 7
      COMMON/SINGEN/ITYPSI,PMINSI,PMAXSI,COMISI,COMASI,PHMISI,PHMASI    SINGEN 2
      PARAMETER (LPDEC=48)                                              ASKUSI12
      INTEGER NODEC(LPDEC)                                              ASKUSI13
      INTEGER ALTABL,ALRLEP                                             ASKUSI14
      EXTERNAL ALTABL,ALRLEP                                            ASKUSI15
      DIMENSION TABL(25)                                                ASKUSI16
      CHARACTER*30 DATE                                                 ASKUSI17
      DATA FVERS/                                                       ASKUSI18
     $1.01                                                              VERSION2
     $/                                                                 ASKUSI20
      DATA DATE/                                                        ASKUSI21
     $ 'October 9   , 1995'                                             BBL001 1
     $/                                                                 ASKUSI23
C  IGCOD  for LUND 7.4                                                  ASKUSI24
      PARAMETER ( IGCO  =  5026)                                        ASKUSI25
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
C -----------------------------------------------------------------     ASKUSI27
      IUT = IW(6)                                                       ASKUSI28
C                                                                       ASKUSI29
C   Return generator code IGCOD                                         ASKUSI30
C                                                                       ASKUSI31
      IGCOD = IGCO                                                      ASKUSI32
C                                                                       ASKUSI33
      DO 5  K=1,10                                                      ASKUSI34
 5    ICOULU(K)=0                                                       ASKUSI35
C - get the LUND flavour IFL if given on data card                      ASKUSI36
      NLUND = NAMIND ('GLUN')                                           ASKUSI37
      JLUND = IW(NLUND)                                                 ASKUSI38
      IF (JLUND .NE. 0) THEN                                            ASKUSI39
         IFL = IW(JLUND+1)                                              ASKUSI40
         ECM  =  RW(JLUND+2)                                            ASKUSI41
         IPRI = IW(JLUND+3)                                             ASKUSI42
      ELSE                                                              ASKUSI43
         IFL = 0                                                        ASKUSI44
         ECM = 91.2                                                     ASKUSI45
         IPRI = 0                                                       ASKUSI46
      ENDIF                                                             ASKUSI47
C  Single particle option                                               ASKUSI48
      IF (IFL.EQ.-1)  then                                              ASKUSI49
         JSIN = IW(NAMIND('GSIN'))                                      ASKUSI50
         IF (JSIN.NE.0) THEN                                            ASKUSI51
            ITYPSI = IW(JSIN+1)                                         ASKUSI52
            PMINSI = RW(JSIN+2)                                         ASKUSI53
            PMAXSI = RW(JSIN+3)                                         ASKUSI54
            COMISI = RW(JSIN+4)                                         ASKUSI55
            COMASI = RW(JSIN+5)                                         ASKUSI56
            PHMISI = RW(JSIN+6)                                         ASKUSI57
            PHMASI = RW(JSIN+7)                                         ASKUSI58
            IFLV = ITYPSI                                               ASKUSI59
            ECM  = ULMASS(ITYPSI)                                       ASKUSI60
         ELSE                                                           ASKUSI61
            WRITE(IUT,100)                                              ASKUSI62
            CALL EXIT                                                   ASKUSI63
         ENDIF                                                          ASKUSI64
      ENDIF                                                             ASKUSI65
C                                                                       ASKUSI68
C - make use of a smearing of the vertex                                ASKUSI69
C   if it is given                                                      ASKUSI70
      NSVER = NAMIND ('SVRT')                                           ASKUSI71
      JSVER = IW(NSVER)                                                 ASKUSI72
      IF (JSVER .NE. 0) THEN                                            ASKUSI73
         SVERT(1) = RW(JSVER+1)                                         ASKUSI74
         SVERT(2) = RW(JSVER+2)                                         ASKUSI75
         SVERT(3) = RW(JSVER+3)                                         ASKUSI76
      ELSE                                                              ASKUSI77
         SVERT(1) = 0.018                                               ASKUSI78
         SVERT(2) = 0.001                                               ASKUSI79
         SVERT(3) = 0.7                                                 ASKUSI80
      ENDIF                                                             ASKUSI81
C   keep fragmentation info                                             ASKUSI82
      MSTU(17) = 1                                                      ASKUSI83
C   store beam electrons  and Z0                                        ASKUSI84
      MSTJ(115) = 3                                                     ASKUSI85
C   initial state radiation                                             ASKUSI86
      MSTJ(107)   = 1                                                   ASKUSI87
C   Final   state radiation                                             ASKUSI88
      MSTJ( 41)   = 2                                                   ASKUSI89
C  use non discrete masses for resonnances                              ASKUSI90
      MSTJ( 24) =  2                                                    ASKUSI91
C   SLAC fragm. functions for b,c  Symetric LUND for u,d,s              ASKUSI92
      MSTJ( 11)   = 3                                                   ASKUSI93
C   mod to lund fragm. functions params                                 ASKUSI94
      PARJ ( 21)  =0.358                                                ASKUSI95
      PARJ  (41)  =0.500                                                ASKUSI96
      PARJ  (42)  =0.840                                                ASKUSI97
      PARJ  (81)  =0.310                                                ASKUSI98
      PARJ  (82)  =1.500                                                ASKUSI99
C  mod Peterson's fragm. functions params                               ASKUS100
      PARJ  (54)  = -0.200                                              ASKUS101
      PARJ  (55)  = -0.006                                              ASKUS102
      PARU (102)  =  0.232                                              ASKUS103
      PARJ (123)  =  91.17                                              ASKUS104
      PARU (124)  =  2.5                                                ASKUS105
C                                                                       ASKUS106
      WRITE(IUT,1007)                                                   ASKUS107
      WRITE(IUT, 99) IGCOD                                              ASKUS108
      WRITE(IUT,101) FVERS,DATE                                         ASKUS109
C                                                                       ASKUS110
C   Issue the relevant parameters                                       ASKUS111
C                                                                       ASKUS112
      WRITE (IUT,1000)                                                  ASKUS113
      WRITE (IUT,1007)                                                  ASKUS114
      IF (IFL.GT.0) THEN                                                ASKUS115
        WRITE (IUT,1001) IFL                                            ASKUS116
      ELSE                                                              ASKUS117
        WRITE (IUT,1002) IFLV                                           ASKUS118
      ENDIF                                                             ASKUS119
      WRITE (IUT,1007)                                                  ASKUS120
C                                                                       ASKUS121
C  Set up some default values for masses and initial conditions         ASKUS122
C                                                                       ASKUS123
      PMAS(LUCOMP(25),1)= 100.                                          ASKUS124
      PMAS(LUCOMP( 6),1)= 174.                                          ASKUS125
      PMAS(LUCOMP(23),1)= 91.2                                          ASKUS126
C   HIGGS Mass , TOP Mass and Z0 mass defined, can be overwritten by    ASKUS127
C   a PMA1 card                                                         ASKUS128
C                                                                       ASKUS129
C -- complete PART bank with LUND  particles                            ASKUS130
C    use the library routine KXL74A                                     ASKUS131
      CALL KXL74A (IPART,IKLIN)                                         ASKUS132
      IF (IPART.LE.0 .OR. IKLIN.LE.0) THEN                              ASKUS133
         WRITE (IW(6),'(1X,''error in PART or KLIN bank - STOP - ''     ASKUS134
     +                 ,2I3)') IPART,IKLIN                              ASKUS135
         CALL EXIT                                                      ASKUS136
      ENDIF                                                             ASKUS137
C                                                                       ASKUS138
C - Print PART and KLIN banks                                           ASKUS139
      IF ( MOD(IPRI,10).GT.0) THEN                                      ASKUS140
         CALL LULIST(12)                                                ASKUS141
         CALL PRPART                                                    ASKUS142
      ENDIF                                                             ASKUS143
      IF ( IPRI/10.GT.0) THEN                                           ASKUS144
         CALL LUTABU(10)                                                ASKUS145
         CALL LUTABU(20)                                                ASKUS146
         IF (IFL.LT.0) CALL LUTABU(50)                                  ASKUS147
      ENDIF                                                             ASKUS148
C                                                                       ASKUS149
C -- get list of  particle# which should not be decayed                 ASKUS150
C    in LUND  because they are decayed in GALEPH.                       ASKUS151
C    the routines uses the KLIN bank and fills the user array           ASKUS152
C    NODEC in the range [1-LPDEC]                                       ASKUS153
      MXDEC = KNODEC (NODEC,LPDEC)                                      ASKUS154
      MXDEC = MIN (MXDEC,LPDEC)                                         ASKUS155
C                                                                       ASKUS156
C -- inhibit decays in LUND                                             ASKUS157
C    If the user has set some decay channels by data cards they will    ASKUS158
C    will not be overwritten                                            ASKUS159
      IF (MXDEC .GT. 0) THEN                                            ASKUS160
         DO 10 I=1,MXDEC                                                ASKUS161
            IF (NODEC(I).GT.0) THEN                                     ASKUS162
               JIDB = NLINK('MDC1',NODEC(I))                            ASKUS163
               IF (JIDB .EQ. 0) MDCY(LUCOMP(NODEC(I)),1) = 0            ASKUS164
            ENDIF                                                       ASKUS165
   10    CONTINUE                                                       ASKUS166
      ENDIF                                                             ASKUS167
C                                                                       ASKUS168
      CALL HBOOK1(10001,'OUTGOING FERMION ENERGY$',50,0.,50.,0.)        ASKUS169
      CALL HBOOK1(10002,'OUTGOING ANTIFERMION ENERGY$',50,0.,50.,0.)    ASKUS170
      CALL HBOOK1(10003,'ISR PHOTON ENERGY IF ANY$',50,0.,20.,0.)       ASKUS171
      CALL HBOOK1(10004,'POLAR ANGLE FERMION$',50,-1.,1.,0.)            ASKUS172
      CALL HBOOK1(10005,'POLAR ANGLE ANTIFERMION$',50,-1.,1.,0.)        ASKUS173
      CALL HBOOK1(10006,'FINAL MULTIPLICITY GENERATED$',50,20.,120.,0.) ASKUS174
      CALL HBOOK1(10023,'FSR PHOTON ENERGIES IF ANY      ',50,0.,20.,0.)ASKUS175
C                                                                       ASKUS176
C   dump the generator parameters for this run in a bank                ASKUS177
C assume all parameters are real and stored as a single row             ASKUS178
      TABL(1) = FLOAT(IFL)                                              ASKUS179
      TABL(2) = ECM                                                     ASKUS180
      DO 11 I=1,3                                                       ASKUS181
 11   TABL(2+I) = SVERT(I)                                              ASKUS182
      NWB = 5                                                           ASKUS183
      IND = ALTABL('KPAR',NWB,1,TABL,'2I,(F)','C')                      ASKUS184
C                                                                       ASKUS185
C  Fill RLEP bank                                                       ASKUS186
      IEBEAM = NINT(ECM* 500  )                                         ASKUS187
      JRLEP = ALRLEP(IEBEAM,'    ',0,0,0)                               ASKUS188
      CALL PRTABL('RLEP',0)                                             ASKUS189
      IF ( IPRI.GT.0 ) CALL PRTABL('KPAR',0)                            ASKUS190
  99  FORMAT(/,10X,'*  LUND04 - CODE NUMBER = ',I10,/)                  ASKUS191
 100  FORMAT(/,10X,'!! PROGRAM EXITED  : NO GSIN CARD WAS GIVEN !!')    ASKUS192
 101  FORMAT(10X,'*  ','Version ',F6.2,' -Last modified on   ',A30)     ASKUS193
 1000 FORMAT(/,10X,'*  WELCOME TO JETSET 7.4 as  LUND04')               ASKUS194
 1001 FORMAT(10X,'*',16X,'flavor choosen  : ',I2,34X,'*')               ASKUS195
 1002 FORMAT(10X,'*',16X,'Single particle code  ',I8,24X,'*')           ASKUS196
 1007 FORMAT(10X,72('*') )                                              ASKUS197
      RETURN                                                            ASKUS198
      END                                                               ASKUS199
       SUBROUTINE KXL74A (IPART,JKLIN)                                  KXL74A 2
C --------------------------------------------------                    KXL74A 3
C - B.Bloch - March 94 adapted to JETSET 7.4 from KXL7PA (JETSET 7.3 )  KXL74A 4
C! fill 'PART' bank with LUND7 particles                                KXL74A 5
CKEY KINE KINGAL LUND7 PART  /  USER                                    KXL74A 6
C  Create the KLUN bank with version and date of JETSET library         KXL74A 7
C  Get  the NOtracking marker word NOTRK from KRUN bank                 KXL74A 8
C  Fill KLIN bank with LUND particle# which correspond                  KXL74A 9
C       to GEANT particles ( or ALEPH particles)                        KXL74A10
C  Fill Antilambda C when necessary                                     KXL74A11
C  Get  LUND particles and transfer them to PART bank                   KXL74A12
C       if they can be produced at LEP energies,                        KXL74A13
C       with a GEANT# and a tracking type set to NOTRK                  KXL74A14
C       because they are not used by GEANT.                             KXL74A15
C  Reduce PART and KLIN banks to their normal size                      KXL74A16
C  Make sure Jetset uses ALEPH values for masses and life times         KXL74A17
C                                                                       KXL74A18
C - structure: SUBROUTINE subprogram                                    KXL74A19
C              User Entry Name: KXL74A                                  KXL74A20
C              External References: NAMIND(BOS77)                       KXL74A21
C                                   KGPART/KBKLIN/KBPART/KXL7TO/KMPART  KXL74A22
C                                   ADBVER/ALKLUN/PRTABL/KXL7CO/AUBPRS  KXL74A23
C                                   (ALEPHLIB)                          KXL74A24
C                                   LUCHGE/ULMASS/LUNAME/LUCOMP(JETSET) KXL74A25
C                                   IUCOMP(CERNLIB)                     KXL74A26
C              Comdecks referenced: BCS, BMACRO,KMACRO                  KXL74A27
C                                                                       KXL74A28
C - Usage    : CALL KXL74A (IPART,JKLIN)                                KXL74A29
C - Output   : IPART   = KBPART return flag                             KXL74A30
C                        .gt. 0 means OK                                KXL74A31
C              JKLIN   = KBKLIN return flag                             KXL74A32
C                        .gt. 0 means OK                                KXL74A33
C*IF .NOT.DOC   ------------------------------------------------------- KXL74A34
      SAVE                                                              KXL74A35
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
      PARAMETER(JPARGN=1,JPARNA=2,JPARCO=5,JPARMA=6,JPARCH=7,JPARLT=8,  KXL74A38
     +          JPARMW=9,JPARAN=10,LPARTA=10)                           KXL74A39
C     ILUGE (LLUGE) are the LUND numbers corresponding to the first     KXL74A40
C                   part of PART bank ( used by GEANT)                  KXL74A41
C     ILUAL (LLUAL) are the LUND numbers corresponding to the rest of   KXL74A42
C                   the PART bank                                       KXL74A43
      PARAMETER ( ELEP = 170.)                                          KXL74A44
      PARAMETER ( IDBNW = 114)                                          KXL74A45
      PARAMETER ( DMAS =0.  , IANTI = 0)                                KXL74A46
      PARAMETER ( LLUGE=52 ,   LLUAL =315)                              KXL74A47
CTM  Particle listing limits                                            KXL74A48
      PARAMETER ( NOPA1=83, NOPA2=89, NOPA3=94, NOPA4=101, LASTP=20885) KXL74A49
      PARAMETER ( NOPA5=700, NOPA6 = 1000, NOPA7= 5400 , NOPA8=10000)   KXL74A50
      INTEGER ILUGE(LLUGE),ILUAL(LLUAL)                                 KXL74A51
      EXTERNAL NAMIND,ALKLUN                                            KXL74A52
      INTEGER  ALKLUN                                                   KXL74A53
      CHARACTER TNAM*16                                                 KXL74A54
      DATA ILUGE /22,-11,11,0,-13,13,111,211,-211,130,321,-321,2112,    KXL74A55
     +           2212,-2212,310,221,3122,3222,3212,3112,                KXL74A56
     +           3322,3312,3334,-2112,-3122,-3222,-3212,-3112,          KXL74A57
     +           -3322,-3312,-3334,-15,15,411,-411,421,-421,            KXL74A58
     +           431,-431,4122,24,-24,23,8*0/                           KXL74A59
      DATA ILAM /4122/                                                  KXL74A60
      DATA ILUAL /-4122,25,551,311,-311,12,-12,14,-14,16,-16,20213,     KXL74A61
     &-20213,20113,10221,10111,331,10441,20443,445,443,30443,441,213,   KXL74A62
     &-213,323,-323,313,-313,423,-423,413,-413,433,-433,113,223,333,    KXL74A63
     &10551,20553,555,553,30553,661,10661,20663,665,21,                 KXL74A64
     &2,1,3,4,5,6,-2,-1,-3,-4,-5,-6,663,                                KXL74A65
     &30663,-521,521,511,-511,531,-531,-541,541,-523,523,513,-513,      KXL74A66
     &533,-533,-543,543,621,-621,611,-611,631,-631,641,-641,            KXL74A67
     &651,-651,623,-623,613,-613,                                       KXL74A68
     &633,-633,643,-643,653,-653,2224,-2224,2214,-2214,2114,-2114,1114, KXL74A69
     &-1114,3224,-3224,3214,-3214,3114,-3114,3324,-3324,3314,-3314,4222,KXL74A70
     &-4222,4212,-4212,4112,-4112,4322,-4322,4312,-4312,4332,-4332,4232,KXL74A71
     &-4232,4132,-4132,4224,-4224,4214,-4214,4114,-4114,4324,-4324,4314,KXL74A72
     &-4314,4334,-4334,4422,-4422,4412,-4412,4432,-4432,4424,-4424,4414,KXL74A73
     &-4414,4434,-4434,4444,-4444,5222,-5222,5212,-5212,5112,-5112,5322,KXL74A74
     &-5322,5312,-5312,5332,-5332,5242,-5242,5142,-5142,5342,-5342,5442,KXL74A75
     &-5442,5522,-5522,5512,-5512,5532,-5532,5542,-5542,6222,-6222,6212,KXL74A76
     &-6212,6112,-6112,6232,-6232,6132,-6132,6332,-6332,6242,-6242,6142,KXL74A77
     &-6142,6342,-6342,6442,-6442,5122,-5122,5232,-5232,5132,-5132,5422,KXL74A78
     &-5422,5412,-5412,5432,-5432,6122,-6122,6322,-6322,6312,-6312,6422,KXL74A79
     &-6422,6412,-6412,6432,-6432,5224,-5224,5214,-5214,5114,-5114,5324,KXL74A80
     &-5324,5314,-5314,5334,-5334,5424,-5424,5414,-5414,5434,-5434,5444,KXL74A81
     &-5444,6224,-6224,6214,-6214,6114,-6114,6324,-6324,6314,-6314,6334,KXL74A82
     &-6334,6424,-6424,6414,-6414,6434,-6434,6444,-6444,6524,-6524,6514,KXL74A83
     &-6514,6534,-6534,6544,-6544,6554,-6554,6252,-6252,6152,-6152,6352,KXL74A84
     &-6352,6452,-6452,6552,-6552,6522,-6522,6512,-6512,6532,-6532,6542,KXL74A85
     &-6542,5524,-5524,5514,-5514,5534,-5534,5544,-5544,5554,-5554,-11, KXL74A86
     &11,5*0/                                                           KXL74A87
C                                                                       KXL74A88
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
C                                                                       KMACRO41
C                                                                       KMACRO42
C ------------------------------------------------------                KXL74A91
C - Get NAPAR name-index of PART bank                                   KXL74A92
      NAPAR = NAMIND ('PART')                                           KXL74A93
C - Get number of columns in PART bank                                  KXL74A94
      IDPAR = IW(NAPAR)                                                 KXL74A95
      LCOPA = LCOLS(IDPAR)                                              KXL74A96
C - Get Data Base version number                                        KXL74A97
      CALL ADBVER(IVERS,IDATE)                                          KXL74A98
C                                                                       KXL74A99
C - NOtrack marker word stored in KRUN bank                             KXL74100
      KNOTRK = ITABL (IW(NAMIND('KRUN')),1,2)                           KXL74101
C                                                                       KXL74102
C - Get JETSET version # and date of last modification                  KXL74103
C   create bank KLUN with this info                                     KXL74104
C                                                                       KXL74105
      ILUVER = MSTU(181)*1000+ MSTU(182)                                KXL74106
      ILULMD = MOD(MSTU(183),100)*10000 + MSTU(184)*100 + MSTU(185)     KXL74107
      IKLUN =  ALKLUN(ILUVER,ILULMD)                                    KXL74108
      IF (IKLUN.LE.0) CALL EXIT                                         KXL74109
      CALL PRTABL ('KLUN',0)                                            KXL74110
C                                                                       KXL74111
C - Fill KLIN with LUGEN particle# for the GEANT particles              KXL74112
C   which are the 1st LLUGE particles of PART                           KXL74113
C                                                                       KXL74114
C                                                                       KXL74115
      DO 1 IPART=1,LLUGE                                                KXL74116
         JKLIN = KBKLIN (IPART,ILUGE(IPART))                            KXL74117
         IF (JKLIN .LE. 0) GOTO 998                                     KXL74118
 1    CONTINUE                                                          KXL74119
CTM  Avoid mass-smearing when using ULMASS for the database             KXL74120
      MSTJ24 = MSTJ(24)                                                 KXL74121
      MSTJ(24) = 0                                                      KXL74122
C - if new PART bank format and content, extend the KLIN bank           KXL74123
      IF (LCOPA.EQ.LPARTA .AND. IVERS.GE.IDBNW) THEN                    KXL74124
         DO 2 IPART = LLUGE+1,LLUAL+LLUGE                               KXL74125
            JKLIN = KBKLIN (IPART,ILUAL(IPART-LLUGE))                   KXL74126
            IF (JKLIN .LE. 0) GOTO 998                                  KXL74127
  2      CONTINUE                                                       KXL74128
C If old content complete with antiLambdac and update new format        KXL74129
C if needed                                                             KXL74130
      ELSEIF (IVERS.LT.IDBNW) THEN                                      KXL74131
C                                                                       KXL74132
C - Fill Antilambda C                                                   KXL74133
C                                                                       KXL74134
         IALAM = KGPART (ILAM)                                          KXL74135
         NAPAR = NAMIND ('PART')                                        KXL74136
         TLIF  = TIMLIF (IALAM)                                         KXL74137
         CHAR  = LUCHGE (ILAM) / 3.                                     KXL74138
CTM New form of ULMASS                                                  KXL74139
         ZMAS  = ULMASS (ILAM)                                          KXL74140
         TNAM  = ' '                                                    KXL74141
         CALL LUNAME (-ILAM,TNAM)                                       KXL74142
         IPART = KBPART (KNOTRK,TNAM,KNOTRK,ZMAS,-CHAR,TLIF)            KXL74143
         IF (IPART .LE. 0) GOTO 998                                     KXL74144
         JKLIN = KBKLIN (IPART,-ILAM)                                   KXL74145
         IF (JKLIN .LE. 0) GOTO 998                                     KXL74146
         IF (LCOPA.EQ.LPARTA) THEN                                      KXL74147
            MPART = KMPART (IPART,DMAS,IALAM)                           KXL74148
            IF (MPART.LE.0) GO TO 998                                   KXL74149
            MPART = KMPART (IALAM,DMAS,IPART)                           KXL74150
            IF (MPART.LE.0) GO TO 998                                   KXL74151
         ENDIF                                                          KXL74152
      ENDIF                                                             KXL74153
C -- Set  parameters by data cards  ,use the library routine KXL7CO     KXL74154
C    this has to be done after the bank KLIN creation                   KXL74155
      CALL KXL7CO (LUPAR)                                               KXL74156
C                                                                       KXL74157
C - Get LUGEN particles and transfer them to PART                       KXL74158
C   if their mass is in ELEP energy range                               KXL74159
C   these particles are not tracked so their GEANT#                     KXL74160
C   and tracking type are set to KNOTRK                                 KXL74161
C                                                                       KXL74162
CTM Modify listing of particles                                         KXL74163
      DO 1000 MYPP=1, LASTP                                             KXL74164
         MYP = MYPP                                                     KXL74165
         IF (MYP.EQ.81.OR.MYP.EQ.82) GOTO 1000                          KXL74166
         IF (MYP.EQ.210.OR.MYP.EQ.2110.OR.MYP.EQ.2210) GOTO 1000        KXL74167
         IF (MYP.GT.NOPA1 .AND. MYP.LT.NOPA2) GOTO 1000                 KXL74168
         IF (MYP.GT.NOPA3 .AND. MYP.LT.NOPA4) GOTO 1000                 KXL74169
         IF (MYP.GT.NOPA5 .AND. MYP.LT.NOPA6) GOTO 1000                 KXL74170
         IF (MYP.GT.NOPA7 .AND. MYP.LT.NOPA8) GOTO 1000                 KXL74171
CTM  Is the particle in the Aleph data-base ?                           KXL74172
         IALP = IUCOMP(MYP,ILUGE,LLUGE)                                 KXL74173
         IF (IALP.GT.0) GO TO 1000                                      KXL74174
         IF ((LCOPA.EQ.LPARTA).AND.(IVERS.GE.IDBNW) .AND.               KXL74175
     +                   ( IUCOMP(MYPP,ILUAL,LLUAL).GT.0)) GO TO 1000   KXL74176
         TNAM = ' '                                                     KXL74177
         CALL LUNAME (MYP,TNAM)                                         KXL74178
         IF (TNAM .EQ. ' ') GOTO 1000                                   KXL74179
         CHAR = LUCHGE (MYP) /3.                                        KXL74180
         CALL KXL7TO(MYP,TLIF)                                          KXL74181
         ZMAS = ULMASS (MYP)                                            KXL74182
C        TLIF = 3.33E-12*PMAS (LUCOMP(MYP),4)                           KXL74183
C                                                                       KXL74184
         IF (ZMAS.GT.ELEP ) GO TO 1000                                  KXL74185
C          store the new particle# IPART                                KXL74186
         IPART = KBPART (KNOTRK,TNAM,KNOTRK,ZMAS,CHAR,TLIF)             KXL74187
         IF (IPART.LE.0) GOTO 998                                       KXL74188
C          store the user generator particle# of the new particle       KXL74189
         JKLIN = KBKLIN (IPART,MYPP)                                    KXL74190
         IF (JKLIN.LE.0) GOTO 998                                       KXL74191
C                                                                       KXL74192
C          do the same for the antiparticle except if identical         KXL74193
         KC = LUCOMP(MYP)                                               KXL74194
         IF ( KC.LE.0 ) GO TO 998                                       KXL74195
         JANTI = KCHG(KC,3)                                             KXL74196
         WIDM  = PMAS(KC,2)                                             KXL74197
         IF (JANTI.EQ.0) THEN                                           KXL74198
           IF (LCOPA.EQ.LPARTA ) THEN                                   KXL74199
              MPART = KMPART (IPART,WIDM,IPART)                         KXL74200
              IF (MPART.LE.0) GO TO 998                                 KXL74201
           ENDIF                                                        KXL74202
         ELSE                                                           KXL74203
           CALL LUNAME (-MYP,TNAM)                                      KXL74204
           IPART = KBPART (KNOTRK,TNAM,KNOTRK,ZMAS,-CHAR,TLIF)          KXL74205
           IF (IPART.LE.0) GOTO 998                                     KXL74206
           IF (LCOPA.EQ.LPARTA ) THEN                                   KXL74207
              MPART = KMPART (IPART,WIDM,IPART-1)                       KXL74208
              IF (MPART.LE.0) GO TO 998                                 KXL74209
              MPART = KMPART (IPART-1,WIDM,IPART)                       KXL74210
              IF (MPART.LE.0) GO TO 998                                 KXL74211
           ENDIF                                                        KXL74212
           JKLIN = KBKLIN (IPART,-MYPP)                                 KXL74213
           IF (JKLIN.LE.0) GOTO 998                                     KXL74214
         ENDIF                                                          KXL74215
C                                                                       KXL74216
 1000 CONTINUE                                                          KXL74217
      CALL AUBPRS ('PARTKLIN')                                          KXL74218
C                                                                       KXL74219
CTM  Make sure that the standard particle data is used by Jetset        KXL74220
      IDPAR = IW(NAPAR)                                                 KXL74221
      IDKLI = IW(NAMIND('KLIN'))                                        KXL74222
      DO 9999 I = 1, LLUAL+LLUGE                                        KXL74223
         ZMAS = RTABL(IDPAR,I,JPARMA)                                   KXL74224
         TLIF = RTABL(IDPAR,I,JPARLT)                                   KXL74225
         WMAS = RTABL(IDPAR,I,JPARMW)                                   KXL74226
         KFCOD = IABS(ITABL(IDKLI,I,1))                                 KXL74227
CTM  Doesn't work for quarks, because 0 not acceptable to Jetset 7.3    KXL74228
         IF (KFCOD.LE.6) GOTO 9999                                      KXL74229
         KCODE = LUCOMP(KFCOD)                                          KXL74230
         IF ( KCODE.LE.0) GO TO 9999                                    KXL74231
         PMAS(KCODE,1) = ZMAS                                           KXL74232
         PMAS(KCODE,2) = WMAS                                           KXL74233
         IF (TLIF.NE.1.E15 )PMAS(KCODE,4) = TLIF/3.33E-12               KXL74234
 9999 CONTINUE                                                          KXL74235
C                                                                       KXL74236
C                                                                       KXL74237
      GOTO 999                                                          KXL74238
C - not enough space                                                    KXL74239
 998  CONTINUE                                                          KXL74240
C - End                                                                 KXL74241
 999  CONTINUE                                                          KXL74242
CTM  Restore the original mass-smearing status-word.                    KXL74243
      MSTJ(24) = MSTJ24                                                 KXL74244
C                                                                       KXL74245
      END                                                               KXL74246
      SUBROUTINE SIGENE(IDPR,ISTAT,ECMS)                                SIGENE 2
C-------------------------------------------------------------          SIGENE 3
C! Generate single particle and fill lund common with it                SIGENE 4
C  B. Bloch November 90 for JETSET 7.3 version                          SIGENE 5
C--------------------------------------------------------------         SIGENE 6
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
      COMMON/SINGEN/ITYPSI,PMINSI,PMAXSI,COMISI,COMASI,PHMISI,PHMASI    SINGEN 2
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
      RETURN                                                            SIGENE30
      END                                                               SIGENE31
      SUBROUTINE USCJOB                                                 USCJOB 2
C-------------------------------------------------------------------    USCJOB 3
C! End of job routine    Lund 7.4                                       USCJOB 4
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
      COMMON / GLUPAR / IFL,IPRI,SVERT(3),ECM                           GLUCOM 2
C     IFL      : LUND flavour , set to 0 by default, can be changed     GLUCOM 3
C     IPRI     : PRINT level flag                                       GLUCOM 4
C     ECM      : nominal cms energy                                     GLUCOM 5
C     SVERT    : vertex smearing, set to 0. by default, can be changed  GLUCOM 6
      COMMON / GLUSTA / ICOULU(10)                                      GLUCOM 7
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
C.......................................................................USCJOB12
       IUT=IW(6)                                                        USCJOB13
       WRITE(IUT,101)                                                   USCJOB14
  101  FORMAT(//20X,'EVENTS STATISTICS',                                USCJOB15
     &         /20X,'*****************')                                USCJOB16
       WRITE(IUT,102) ICOULU(9)+ICOULU(10),ICOULU(10),ICOULU(9)         USCJOB17
  102  FORMAT(/5X,'# OF GENERATED EVENTS                = ',I10,        USCJOB18
     &        /5X,'# OF ACCEPTED  EVENTS                = ',I10,        USCJOB19
     &        /5X,'# OF REJECTED  EVENTS                = ',I10)        USCJOB20
       WRITE(IUT,103)                                                   USCJOB21
  103  FORMAT(//20X,'REJECT STATISTICS',                                USCJOB22
     &         /20X,'*****************')                                USCJOB23
       WRITE(IUT,104) (ICOULU(I),I=1,8)                                 USCJOB24
  104  FORMAT(/10X,'IR= 1 LUND ERROR unknown part    # OF REJECT =',I10,USCJOB25
     &        /10X,'IR= 2 BOS  ERROR KINE/VERT       # OF REJECT =',I10,USCJOB26
     &        /10X,'IR= 3 LUND ERROR too many tracks # OF REJECT =',I10,USCJOB27
     &        /10X,'IR= 4 LUND ERROR Beam wrong pos  # OF REJECT =',I10,USCJOB28
     &        /10X,'IR= 5 LUND ERROR status code >5  # OF REJECT =',I10,USCJOB29
     &        /10X,'IR= 6 free for user              # OF REJECT =',I10,USCJOB30
     &        /10X,'IR= 7 BOS ERROR IN KZFR BANK     # OF REJECT =',I10,USCJOB31
     &        /10X,'IR= 8 LUND error in LUEXEC       # OF REJECT =',I10)USCJOB32
        WRITE (IUT,105) (PARJ(II),II=141,148)                           USCJOB33
 105    FORMAT(//,20X,'FINAL RESULTS FROM LUND GENERATION',             USCJOB34
     $          /,20X,'**********************************',             USCJOB35
     $          /,10X,'R value as given in massless QED   :',F10.5,     USCJOB36
     $          /,10X,'R value including weak effects     :',F10.5,     USCJOB37
     $          /,10X,'R value including QCD corrections  :',F10.5,     USCJOB38
     $          /,10X,'R value including I.S.R. effects   :',F10.5,     USCJOB39
     $          /,10X,'Absolute cross sections in nb      :',           USCJOB40
     $          /,10X,'As given  in massless QED   :',F10.5,            USCJOB41
     $          /,10X,'Including  weak effects     :',F10.5,            USCJOB42
     $          /,10X,'Including  QCD corrections  :',F10.5,            USCJOB43
     $          /,10X,'Including  I.S.R. effects   :',F10.5)            USCJOB44
      IF ( IPRI/10 .GT.0) THEN                                          USCJOB45
         CALL LUTABU(12)                                                USCJOB46
         CALL LUTABU(22)                                                USCJOB47
         IF (IFL.EQ.-1) CALL LUTABU(52)                                 USCJOB48
      ENDIF                                                             USCJOB49
      RETURN                                                            USCJOB50
      END                                                               USCJOB51
      SUBROUTINE USKRIN(EI)                                             USKRIN 2
C                                                                       USKRIN 3
C! Reinitialise the generator with energy  EI and reinit some quantitiesUSKRIN 4
C! LUND04 version  B.Bloch march  1994                                  USKRIN 5
C                                                                       USKRIN 6
      INTEGER LMHLEN, LMHCOL, LMHROW  ,LBCS                             BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)               BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(LBCS )                                          BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(LBCS)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      COMMON / GLUPAR / IFL,IPRI,SVERT(3),ECM                           GLUCOM 2
C     IFL      : LUND flavour , set to 0 by default, can be changed     GLUCOM 3
C     IPRI     : PRINT level flag                                       GLUCOM 4
C     ECM      : nominal cms energy                                     GLUCOM 5
C     SVERT    : vertex smearing, set to 0. by default, can be changed  GLUCOM 6
      COMMON / GLUSTA / ICOULU(10)                                      GLUCOM 7
      DIMENSION TABL(25)                                                USKRIN 9
      INTEGER ALTABL                                                    USKRIN10
      EXTERNAL ALTABL                                                   USKRIN11
      ECM = EI                                                          USKRIN12
C Get KPAR bank, modify                                                 USKRIN13
      IKPAR= NLINK('KPAR',0)                                            USKRIN14
      IF (IKPAR.GT.0) THEN                                              USKRIN15
         NCO = IW(IKPAR+LMHCOL)                                         USKRIN16
         CALL UCOPY(RW(IKPAR+LMHLEN+1),TABL(1),NCO)                     USKRIN17
         CALL BDROP(IW ,'KPAR')                                         USKRIN18
         TABL(2) = EI                                                   USKRIN19
         IND = ALTABL('KPAR',NCO,1,TABL,'2I,(F)','C')                   USKRIN20
         CALL PRTABL('KPAR',0)                                          USKRIN21
      ENDIF                                                             USKRIN22
      CALL VZERO(ICOULU,10)                                             USKRIN23
      RETURN                                                            USKRIN24
      END                                                               USKRIN25
      FUNCTION XKSECT(ECMI)                                             XKSECT 2
C                                                                       XKSECT 3
C! Returns the cross section value in nb for energy ECM                 XKSECT 4
C! LUND04 version  B.Bloch march  1994                                  XKSECT 5
C                                                                       XKSECT 6
      COMMON / GLUPAR / IFL,IPRI,SVERT(3),ECM                           GLUCOM 2
C     IFL      : LUND flavour , set to 0 by default, can be changed     GLUCOM 3
C     IPRI     : PRINT level flag                                       GLUCOM 4
C     ECM      : nominal cms energy                                     GLUCOM 5
C     SVERT    : vertex smearing, set to 0. by default, can be changed  GLUCOM 6
      COMMON / GLUSTA / ICOULU(10)                                      GLUCOM 7
      CALL LUXTOT(IFL,ECMI,XTOT)                                        XKSECT 8
      XKSECT = XTOT                                                     XKSECT 9
      RETURN                                                            XKSECT10
      END                                                               XKSECT11
