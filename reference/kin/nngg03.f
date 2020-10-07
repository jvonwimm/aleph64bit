      SUBROUTINE ASKUSI(IGCOD)                                          ASKUSI 2
C--------------------------------------------------------------------   ASKUSI 3
C                                                                       ASKUSI 4
C                P. Gay                                                 ASKUSI 5
C                G. Bonneaud  May 19, 1988                              ASKUSI 6
C                P. Gay          Nov. 1988 (modified for GENTOT)        ASKUSI 7
C                G. Bonneaud  March   1989                              ASKUSI 8
C                P. Gay       May     1989                              ASKUSI 9
C                G. Bonneaud  May     1989                              ASKUSI10
C                B.Bloch      November  1995                            BBL001 1
C                                                                       ASKUSI11
C                       Initialisation                                  ASKUSI12
C                                                                       ASKUSI13
C--------------------------------------------------------------------   ASKUSI14
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      PARAMETER (LBCS=50000,LCHAR=4)                                    BCS    5
      COMMON/BCS/ IW(LBCS)                                              BCS    6
      INTEGER IW                                                        BCS    7
      REAL RW(LBCS)                                                     BCS    8
      EQUIVALENCE (RW(1),IW(1))                                         BCS    9
      COMMON / COUNTR / NEVENT(3)                                       COUNTR 2
      COMMON / INPOUT / LWRITE                                          INPOUT 2
C                                                                       ASKUSI18
      PARAMETER ( IGCO = 1008)                                          ASKUSI19
C                                                                       ASKUSI20
      DIMENSION SVRT(3),GPAR(23)                                        ASKUSI21
      INTEGER ALTABL, alrlep                                            BBL001 2
C                                                                       ASKUSI23
C     common(s) where the parameters are passed to the generator        ASKUSI24
C                                                                       ASKUSI25
      REAL*4   CMEIN,EDCIN,DPPIN,DPMIN                                  ASKUSI26
      REAL*4   PGMZIN,PGMTIN,PGMHIN,PGHEIN,AQCDIN                       ASKUSI27
      REAL*4   XDHIN,EDLIN,ACDIN                                        ASKUSI28
      REAL*4   XMHIN,ACMIN,WCUTIN,FIIN                                  ASKUSI29
      INTEGER  NTIN,NEVIN,NPIN,NFIN,NHWEIN,NHEVIN                       ASKUSI30
      DIMENSION FIIN(2)                                                 ASKUSI31
      COMMON/DATARE/ CMEIN,EDCIN,DPPIN,DPMIN,                           ASKUSI32
     .                 PGMZIN,PGMTIN,PGMHIN,PGHEIN,AQCDIN,              ASKUSI33
     .                 XDHIN,EDLIN,ACDIN,                               ASKUSI34
     .                 XMHIN,ACMIN,WCUTIN,FIIN                          ASKUSI35
      COMMON/DATAIN/ NTIN,NEVIN,NPIN,NFIN,NHWEIN,NHEVIN                 ASKUSI36
C                                                                       ASKUSI37
      COMMON/VERSION/ TYPVER                                            ASKUSI38
      CHARACTER*7 TYPVER                                                ASKUSI39
      DIMENSION GAM1(4),GAM2(4)                                         ASKUSI40
C                                                                       ASKUSI41
C   Return the generator code as defined in the KINGAL library          ASKUSI42
C                                                                       ASKUSI43
      IGCOD = IGCO                                                      ASKUSI44
C                                                                       ASKUSI45
C   Get Generator parameters ( from a data card if you want             ASKUSI46
C    or by default values if you prefer)                                ASKUSI47
C                                                                       ASKUSI48
      LWRITE = IW(6)                                                    ASKUSI49
      WRITE(LWRITE,101) IGCOD                                           ASKUSI50
 101  FORMAT(/,10X,                                                     ASKUSI51
     &       'NNGG03 - CODE NUMBER =',I4,' Last mod. November ,1995',   BBL001 3
     & /,10X,'**************************************************',//)   ASKUSI53
C                                                                       ASKUSI54
C   Define some key word                                                ASKUSI55
C                                                                       ASKUSI56
      TYPVER='NUCOUNT'                                                  ASKUSI57
C---------------------------------------------------------------------  ASKUSI58
C         CMEIN   c.m.s. energy                                         ASKUSI59
C         NFIN    Number of neutrino families                           ASKUSI60
C         ECDIN   Soft/hard photon energy limit (GeV)                   ASKUSI61
C   Degree of longitudinal polarisation of the initial state            ASKUSI62
C         DPPIN = 1. If (e+) is left handed                             ASKUSI63
C              (Spin in opposite direction to real motion-trimomentum)  ASKUSI64
C         DPMIN = 1. If (e-) is right handed                            ASKUSI65
C              (Spin in same direction to real motion-trimomemtum)      ASKUSI66
C WCUTIN   maximal weight value (should be greater than 1.4)            ASKUSI67
C FIIN(1) and FIIN(2) to choose  the rate of each process               ASKUSI68
C FIIN(1) = 1.  & FIIN(2) = 0.   (nu nu_bar gamma)  only                ASKUSI69
C FIIN(1) = 0.  & FIIN(2) = 1.   (nu nu_bar gamma gamma) only           ASKUSI70
C FIIN(1) = 1.  & FIIN(2) = 1.   both processes                         ASKUSI71
C                                 with respect to each total X-section  ASKUSI72
C NTIN     maximal number of trials                                     ASKUSI73
C NPIN     number of points to initialize the generator ( > 100)        ASKUSI74
C  NHWEIN   = BOOK WEIGHT HISTOGRAMS ? (=0 NO)                          ASKUSI75
C  NHEVIN   = BOOK EVENT HISTOGRAMS ? (=0 NO)                           ASKUSI76
C---------------------------------------------------------------------  ASKUSI77
C defaults values                                                       ASKUSI78
      CMEIN = 92.2                                                      ASKUSI79
      NFIN  = 3                                                         ASKUSI80
      EDCIN = 0.02                                                      ASKUSI81
      DPPIN =  0.0                                                      ASKUSI82
      DPMIN =  0.0                                                      ASKUSI83
      WCUTIN = 1.4                                                      ASKUSI84
      FIIN(1)= 1.                                                       ASKUSI85
      FIIN(2)= 1.                                                       ASKUSI86
      NTIN   = 10000000                                                 ASKUSI87
      NPIN   = 100                                                      ASKUSI88
      NHWEIN = 0                                                        ASKUSI89
      NHEVIN = 1                                                        ASKUSI90
      JGENE=NLINK('GENE',0)                                             ASKUSI91
      IF (JGENE.NE.0) THEN                                              ASKUSI92
       CMEIN = RW(JGENE+1)                                              ASKUSI93
       NFIN  = IW(JGENE+2)                                              ASKUSI94
       EDCIN = RW(JGENE+3)                                              ASKUSI95
       DPPIN = RW(JGENE+4)                                              ASKUSI96
       DPMIN = RW(JGENE+5)                                              ASKUSI97
       WCUTIN = RW(JGENE+6)                                             ASKUSI98
       FIIN(1)= RW(JGENE+7)                                             ASKUSI99
       FIIN(2)= RW(JGENE+8)                                             ASKUS100
       NTIN   = IW(JGENE+9)                                             ASKUS101
       NPIN   = IW(JGENE+10)                                            ASKUS102
       NHWEIN = IW(JGENE+11)                                            ASKUS103
       NHEVIN  = IW(JGENE+12)                                           ASKUS104
      ENDIF                                                             ASKUS105
      GPAR(1) = CMEIN                                                   ASKUS106
      GPAR(2) = NFIN                                                    ASKUS107
      GPAR(3) = EDCIN                                                   ASKUS108
      GPAR(4) = DPPIN                                                   ASKUS109
      GPAR(5) = DPMIN                                                   ASKUS110
      GPAR(6) = WCUTIN                                                  ASKUS111
      GPAR(7) = FIIN(1)                                                 ASKUS112
      GPAR(8) = FIIN(2)                                                 ASKUS113
      GPAR(9) = NTIN                                                    ASKUS114
      GPAR(10) = NPIN                                                   ASKUS115
C---------------------------------------------------------------------  ASKUS116
C PGMZIN     Z mass   (Gev)                                             ASKUS117
C PGMTIN     Top mass (Gev)                                             ASKUS118
C PGMHIN     Higgs mass (Gev)                                           ASKUS119
C PGHEIN     Heavy lepton mass (Gev)                                    ASKUS120
C AQCDIN     Alpha_strong                                               ASKUS121
C---------------------------------------------------------------------  ASKUS122
C defaults values                                                       ASKUS123
      PGMZIN = 92.                                                      ASKUS124
      PGMTIN = 60.0                                                     ASKUS125
      PGMHIN = 100.0                                                    ASKUS126
      PGHEIN = 100.0                                                    ASKUS127
      AQCDIN = 0.12                                                     ASKUS128
      JGSMP=NLINK('GSMP',0)                                             ASKUS129
      IF (JGSMP.NE.0) THEN                                              ASKUS130
       PGMZIN = RW(JGSMP+1)                                             ASKUS131
       PGMTIN = RW(JGSMP+2)                                             ASKUS132
       PGMHIN = RW(JGSMP+3)                                             ASKUS133
       PGHEIN = RW(JGSMP+4)                                             ASKUS134
       AQCDIN = RW(JGSMP+5)                                             ASKUS135
      ENDIF                                                             ASKUS136
      GPAR(11)  = PGMZIN                                                ASKUS137
      GPAR(12)  = PGMTIN                                                ASKUS138
      GPAR(13)  = PGMHIN                                                ASKUS139
      GPAR(14)  = PGHEIN                                                ASKUS140
      GPAR(15)  = AQCDIN                                                ASKUS141
C---------------------------------------------------------------------  ASKUS142
C EDLIN  minimum photon energy (signal) (GeV)                           ASKUS143
C ACDIN  minimum detection angle for the signal photon (Degrees)        ASKUS144
C ACMIN  minimum detection angle for the photon which is not the        ASKUS145
C        signal one (Degrees)                                           ASKUS146
C XDHIN  maximum photon energy (signal) WARNING in Beam unit            ASKUS147
C XMHIN  maximum photon energy which is not the signal one              ASKUS148
C                                       WARNING in Beam unit            ASKUS149
C---------------------------------------------------------------------  ASKUS150
C defaults values                                                       ASKUS151
      EDLIN  = 0.5                                                      ASKUS152
      ACDIN  = 15.                                                      ASKUS153
      ACMIN  = 2.                                                       ASKUS154
      XDHIN  = 1.                                                       ASKUS155
      XMHIN  = 1.                                                       ASKUS156
      JGACP=NLINK('GACP',0)                                             ASKUS157
      IF (JGACP.NE.0) THEN                                              ASKUS158
       EDLIN  = RW(JGACP+1)                                             ASKUS159
       ACDIN  = RW(JGACP+2)                                             ASKUS160
       ACMIN  = RW(JGACP+3)                                             ASKUS161
       XDHIN  = RW(JGACP+4)                                             ASKUS162
       XMHIN  = RW(JGACP+5)                                             ASKUS163
      ENDIF                                                             ASKUS164
      GPAR(16) = EDLIN                                                  ASKUS165
      GPAR(17) = ACDIN                                                  ASKUS166
      GPAR(18) = ACMIN                                                  ASKUS167
      GPAR(19) = XDHIN                                                  ASKUS168
      GPAR(20) = XMHIN                                                  ASKUS169
C                                                                       ASKUS170
C  Main vertex smearing                                                 ASKUS171
      SVRT(1) = 0.035                                                   ASKUS172
      SVRT(2) = 0.0012                                                  ASKUS173
      SVRT(3) = 1.28                                                    ASKUS174
      JSVRT=NLINK('SVRT',0)                                             ASKUS175
      IF (JSVRT.NE.0) THEN                                              ASKUS176
       SVRT(1)=RW(JSVRT+1)                                              ASKUS177
       SVRT(2)=RW(JSVRT+2)                                              ASKUS178
       SVRT(3)=RW(JSVRT+3)                                              ASKUS179
      ENDIF                                                             ASKUS180
      GPAR(21) = SVRT(1)                                                ASKUS181
      GPAR(22) = SVRT(2)                                                ASKUS182
      GPAR(23) = SVRT(3)                                                ASKUS183
C                                                                       ASKUS184
C  Fill the KPAR bank                                                   ASKUS185
      NCOL = 23                                                         ASKUS186
      NROW = 1                                                          ASKUS187
      JKPAR = ALTABL('KPAR',NCOL,NROW,GPAR,'2I,(F)','C')                ASKUS188
C                                                                       ASKUS189
      iebeam = nint(500*cmein)                                          BBL001 4
      jrlep = alrlep(iebeam,'   ',0,0,0)                                BBL001 5
C  Initialize counters                                                  ASKUS190
      DO 10 I = 1,3                                                     ASKUS191
   10  NEVENT(I) = 0                                                    ASKUS192
C                                                                       ASKUS193
C  Then init the generator with needed parameters                       ASKUS194
C                                                                       ASKUS195
      JTRIG = NLINK('TRIG',0)                                           ASKUS196
      IF(JTRIG.NE.0) THEN                                               ASKUS197
       NEVIN = IW(JTRIG+2)                                              ASKUS198
      ELSE                                                              ASKUS199
       NEVIN = 1000                                                     ASKUS200
      ENDIF                                                             ASKUS201
C                                                                       ASKUS202
C                                                                       ASKUS203
      CALL GENTOT('INI',NGAM,GAM1,GAM2)                                 ASKUS204
C                                                                       ASKUS205
C  Print PART and KLIN banks                                            ASKUS206
C                                                                       ASKUS207
      CALL PRPART                                                       ASKUS208
C                                                                       ASKUS209
      CALL PRTABL('KPAR',0)                                             ASKUS210
C                                                                       ASKUS211
      call prtabl('RLEP',0)                                             BBL001 6
      RETURN                                                            ASKUS212
      END                                                               ASKUS213
      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)                 ASKUSE 2
C--------------------------------------------------------------------   ASKUSE 3
C                                                                       ASKUSE 4
C                 P. Gay                                                ASKUSE 5
C                 G. Bonneaud  May 18, 1988                             ASKUSE 6
C                 P. Gay       Nov.    1988                             ASKUSE 7
C                 G. Bonneaud  March   1989                             ASKUSE 8
C                                                                       ASKUSE 9
C - call your generator routine to generate one event                   ASKUSE10
C - fill KINE , VERT , KHIS banks                                       ASKUSE11
C                                                                       ASKUSE12
C     output    : 6 arguments                                           ASKUSE13
C          IDPR   : process identification                              ASKUSE14
C          ISTA   : status flag ( 0 means ok)                           ASKUSE15
C          NTRK   : number of tracks generated and kept                 ASKUSE16
C                  (i.e. # KINE banks  written)                         ASKUSE17
C          NVRT   : number of vertices generated                        ASKUSE18
C                   (i.e. # VERT banks written)                         ASKUSE19
C          ECMS   : center of mass energy for the event (may be         ASKUSE20
C                   different from nominal cms energy)                  ASKUSE21
C          WEIT   : event weight ( not 1 if a weighting method is used) ASKUSE22
C--------------------------------------------------------------------   ASKUSE23
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      PARAMETER (LBCS=50000,LCHAR=4)                                    BCS    5
      COMMON/BCS/ IW(LBCS)                                              BCS    6
      INTEGER IW                                                        BCS    7
      REAL RW(LBCS)                                                     BCS    8
      EQUIVALENCE (RW(1),IW(1))                                         BCS    9
      COMMON / COUNTR / NEVENT(3)                                       COUNTR 2
      COMMON / INPOUT / LWRITE                                          INPOUT 2
C                                                                       ASKUSE27
      DIMENSION SVRT(3),VERT(4),TABL(4)                                 ASKUSE28
      DIMENSION R2(4),T2(4)                                             ASKUSE29
      INTEGER ALTABL                                                    ASKUSE30
      INTEGER IGG                                                       ASKUSE31
      DATA IFIRST / 0 /                                                 ASKUSE32
C                                                                       ASKUSE33
C  Generate vertex position                                             ASKUSE34
C                                                                       ASKUSE35
      CALL RANNOR(RX,RY)                                                ASKUSE36
      CALL RANNOR(RZ,DUM)                                               ASKUSE37
      IF(IFIRST.EQ.0) THEN                                              ASKUSE38
       IFIRST = 1                                                       ASKUSE39
       JKPAR = NLINK('KPAR',0)                                          ASKUSE40
       CMEIN = RW(JKPAR+1+LMHLEN)                                       ASKUSE41
       SVRT(1)=RW(JKPAR+21+LMHLEN)                                      ASKUSE42
       SVRT(2)=RW(JKPAR+22+LMHLEN)                                      ASKUSE43
       SVRT(3)=RW(JKPAR+23+LMHLEN)                                      ASKUSE44
      ENDIF                                                             ASKUSE45
      VERT(1)=RX*SVRT(1)                                                ASKUSE46
      VERT(2)=RY*SVRT(2)                                                ASKUSE47
      VERT(3)=RZ*SVRT(3)                                                ASKUSE48
      VERT(4)=0.                                                        ASKUSE49
C                                                                       ASKUSE50
C   Book VERT bank #1                                                   ASKUSE51
C                                                                       ASKUSE52
      IND=KBVERT(1,VERT,0)                                              ASKUSE53
      IF (IND.LE.0) GO TO 20                                            ASKUSE54
C                                                                       ASKUSE55
C   Event generation (this generator returns either 1 or 2 photons)     ASKUSE56
C                                                                       ASKUSE57
      NEVENT(1) = NEVENT(1) + 1                                         ASKUSE58
      CALL GENTOT('STO',IGG,T2,R2)                                      ASKUSE59
C                                                                       ASKUSE60
      IDPR = IGG                                                        ASKUSE61
      NTRK = IGG                                                        ASKUSE62
      NVRT = 1                                                          ASKUSE63
      ISTA = 0                                                          ASKUSE64
      ECMS = CMEIN                                                      ASKUSE65
      WEIT = 1.                                                         ASKUSE66
C                                                                       ASKUSE67
C   Book KINE banks for beam electrons                                  ASKUSE68
C                                                                       ASKUSE69
      EBEAM = CMEIN/2.                                                  ASKUSE70
      TABL(1) = 0.                                                      ASKUSE71
      TABL(2) = 0.                                                      ASKUSE72
      TABL(3) = -EBEAM                                                  ASKUSE73
      TABL(4) = 0.                                                      ASKUSE74
      IND=KBKINE(-1,TABL,2,0)                                           ASKUSE75
      TABL(3) =  EBEAM                                                  ASKUSE76
      JND=KBKINE(-2,TABL,3,0)                                           ASKUSE77
      IF (IND*JND.EQ.0) GO TO 20                                        ASKUSE78
C                                                                       ASKUSE79
C   Book KINE banks for outgoing particles                              ASKUSE80
C                                                                       ASKUSE81
      TABL(1) = T2(1)                                                   ASKUSE82
      TABL(2) = T2(2)                                                   ASKUSE83
      TABL(3) = T2(3)                                                   ASKUSE84
      TABL(4) = 0.                                                      ASKUSE85
      IND = KBKINE(1,TABL,1,1)                                          ASKUSE86
      ECMS = ECMS - T2(4)                                               ASKUSE87
      IF (IND.EQ.0) GO TO 20                                            ASKUSE88
      IF (IGG.EQ.2) THEN                                                ASKUSE89
       ECMS = ECMS - R2(4)                                              ASKUSE90
       TABL(1) = R2(1)                                                  ASKUSE91
       TABL(2) = R2(2)                                                  ASKUSE92
       TABL(3) = R2(3)                                                  ASKUSE93
       TABL(4) = 0.                                                     ASKUSE94
       JND = KBKINE(2,TABL,1,1)                                         ASKUSE95
       IF (JND.EQ.0) GO TO 20                                           ASKUSE96
      ENDIF                                                             ASKUSE97
C                                                                       ASKUSE98
C    Fill the history bank KHIS                                         ASKUSE99
C                                                                       ASKUS100
      DO 10 I=1,NTRK                                                    ASKUS101
   10 TABL(I)=0.                                                        ASKUS102
      IND=ALTABL('KHIS',1,NTRK,TABL,'I','E')                            ASKUS103
      IF (IND.LE.0) GO TO 20                                            ASKUS104
      NEVENT(2) = NEVENT(2) + 1                                         ASKUS105
      RETURN                                                            ASKUS106
   20 ISTA=1                                                            ASKUS107
      NEVENT(3) = NEVENT(3) + 1                                         ASKUS108
      RETURN                                                            ASKUS109
      END                                                               ASKUS110
      SUBROUTINE USCJOB                                                 USCJOB 2
C-------------------------------------------------------------------    USCJOB 3
C                                                                       USCJOB 4
C                P. Gay and G. Bonneaud May, 1988.                      USCJOB 5
C                                                                       USCJOB 6
C                       End of job routine                              USCJOB 7
C                                                                       USCJOB 8
C------------------------------------------------------------------     USCJOB 9
      COMMON / COUNTR / NEVENT(3)                                       COUNTR 2
      COMMON / INPOUT / LWRITE                                          INPOUT 2
C                                                                       USCJOB12
        DIMENSION GAM1(4),GAM2(4)                                       USCJOB13
C                                                                       USCJOB14
C  Final printout                                                       USCJOB15
C                                                                       USCJOB16
      CALL GENTOT('END',NGAM,GAM1,GAM2)                                 USCJOB17
C                                                                       USCJOB18
       WRITE(LWRITE,101)                                                USCJOB19
  101  FORMAT(//20X,'EVENTS AND ERRORS STATISTICS',                     USCJOB20
     &         /20X,'****************************')                     USCJOB21
       WRITE(LWRITE,102)NEVENT(1),NEVENT(2),NEVENT(3)                   USCJOB22
  102  FORMAT(/5X,'# OF GENERATED EVENTS                       = ',I10, USCJOB23
     &        /5X,'# OF ACCEPTED  EVENTS                       = ',I10, USCJOB24
     &        /5X,'# OF REJECTED  EVENTS (ISTA # 0 IN ASKUSE)  = ',I10) USCJOB25
      RETURN                                                            USCJOB26
      END                                                               USCJOB27
