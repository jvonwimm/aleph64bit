C*HE 03/25/91 16:05:17 C
C*DK ASKUSE
      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)
C --------------------------------------------------------------------
C
C --------------------------------------------------------------------
C*CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C*CC BCS
      COMMON /KGCOMM/ IST,IDP,ILEARN,IPROC,ITOT,IREJEC,NEVENT(10)
      COMMON /KGCOMR/ SDVRT(3),ECM,WEI
      DIMENSION VRTEX(4)
      DATA IFI/0/
      IF (ILEARN.EQ.1) THEN
        ISTA = 10
      ELSE
C  Main vertex generation
C  Generate primary vertex
       CALL RANNOR (RX,RY)
       CALL RANNOR (RZ,DUM)
       VRTEX(1) = RX*SDVRT(1)
       VRTEX(2) = RY*SDVRT(2)
       VRTEX(3) = RZ*SDVRT(3)
       VRTEX(4) = 0.
        CALL ELW2EV
        IF (IFI.LT.5) CALL LULIST(11)
        IFI = IFI+1
        IDPR = IDP
        NVRT = 1
        ISTA = IST
        ECMS = ECM
        WEIT = WEI
       CALL KXLUAL(VRTEX,ISTA,NVRT,NTRK)
       IF(ISTA.EQ.0) THEN
         NEVENT(9) = NEVENT(9)+1
       ELSE IF(ISTA.GT.0) THEN
          NEVENT(6) = NEVENT(6)+1
          NEVENT(7) = NEVENT(7)+1
       ELSE IF(ISTA.LT.0) THEN
          ICO= MAX(6,ABS(ISTA))
          NEVENT(ICO) = NEVENT(ICO)+1
          NEVENT(7) = NEVENT(7)+1
       ENDIF
      ENDIF
      RETURN
      END
C*DK ASKUSI
      SUBROUTINE ASKUSI(IGCOD)
C --------------------------------------------------------------------
C
C --------------------------------------------------------------------
C*CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C*CC BCS
C*CA LUNDCOM
      PARAMETER (L1MST=40, L1PAR=80)
      PARAMETER (L2KTYP=120, L2PMAS=120, L2PWID=60, L2KFR=80, L2CFR=40)
      PARAMETER (L3DPAR=20, L3IDB=120, L3CBR=400, L3KDP=1600)
      PARAMETER (LEMSTE=40, LEPARE=80)
      PARAMETER (LJNPAR=2000)
      COMMON /LUDAT1/   MSTLU1(L1MST),PARLU1(L1PAR)
      COMMON /LUDAT2/   KTYPL2(L2KTYP),PMASL2(L2PMAS),PWIDL2(L2PWID)
     &                , KFRLU2(L2KFR),CFRLU2(L2CFR)
      COMMON /LUDAT3/   DPARL3(L3DPAR),IDBLU3(L3IDB),CBRLU3(L3CBR)
     &                , KDPLU3(L3KDP)
      COMMON /LUDATE/   MSTELE(LEMSTE),PARELE(LEPARE)
      COMMON /LUJETS/   NPARLU,KODELU(LJNPAR,2),PARTLU(LJNPAR,5)
C
C*IF DOC
C*CC LUNDCOM
      LOGICAL MYFINS
      COMMON /KGCOMM/ ISTA,IDPR,ILEARN,IPROC,ITOT,IREJEC,NEVENT(10)
      COMMON /KGCOMR/ SDVRT(3),ECMS,WEIT
      PARAMETER ( IGCO = 6006)
      PARAMETER (LPDEC=48)
      INTEGER NODEC(LPDEC)
C
C   Return the generator code as defined in the KINGAL library
C
      IGCOD = IGCO
      IOUT=IW(6)
C
      WRITE (IOUT,'(30X,''W E L C O M E   T O   E L W 2 T A G   '',/,
     $                30X,''**********************************'',/,
     $                30X,'' Generator code  is  # '',I10       ,/,
     $           30x,'' last modification March     24,1991'',/)')
     $   IGCOD
C  Initialize events counters
       DO 19 I = 1,10
   19  NEVENT(I) = 0
C
      CALL KXLUCO(LUPAR)
      CALL ELW201
      IF ( ILEARN.EQ.0) THEN
         CALL LUTAUD(IFL)
         IF (IFL.NE.0) CALL EXIT
         CALL KLINBK(IFL)
         IF (IFL.LE.0) CALL EXIT
C
C  Print PART and KLIN banks
C
         CALL PRPART
C
         CALL PRTABL('KPAR',0)
         CALL PRTABL('RLEP',0)
      MXDEC = KNODEC(NODEC,LPDEC)
      MXDEC = MIN(MXDEC,LPDEC)
      IF (MXDEC.GT.0) THEN
         DO 12 I=1,MXDEC
            IF ( NODEC(I).GT.0) THEN
              JIDB = NLINK('IDB',NODEC(I))
              IF (JIDB.EQ.0) IDBLU3(NODEC(I)) = 0
            ENDIF
   12    CONTINUE
      ENDIF
C
C Booking of some standard histogrammes
       CALL HBOOK1(10001,'Energy distribution final f1+',30,0.,60.,0.)
       CALL HBOOK1(10002,'Energy distribution final f1-',30,0.,60.,0.)
       CALL HBOOK1(10003,'Energy distribution final f2+',30,0.,60.,0.)
       CALL HBOOK1(10004,'Energy distribution final f2-',30,0.,60.,0.)
       CALL HBOOK1(10005,'costeta distribution final f1+',40,-1.,1.,0.)
       CALL HBOOK1(10006,'costeta distribution final f1-',40,-1.,1.,0.)
       CALL HBOOK1(10007,'costeta distribution final f2+$',40,-1.,1.,0.)
       CALL HBOOK1(10008,'costeta distribution final f2-$',40,-1.,1.,0.)
       CALL HBOOK1(10009,'INVARIANT MASS FIRST PAIR  ',50,.5,100.5,0.)
       CALL HBOOK1(10010,'INVARIANT MASS SECOND PAIR  ',50,.1 ,25.1,0.)
      ELSE
  11    CONTINUE
        DO 10 I=1,ITOT
           CALL TIMAL(TIM)
           IF (TIM.LT.20.) GO TO 55
           CALL ELW2EV
  10    CONTINUE
  55    CONTINUE
         IF (.NOT.MYFINS(IPROC,ITOT,IREJEC))  THEN
           CALL ELW2IN
           CALL TIMAL(TIM)
           IF (TIM.LT.20.) GO TO 56
           GO TO 11
         ENDIF
  56     CONTINUE
         WRITE(IW(6),'('' *******   TIME LIMIT REACHED ******'')')
      ENDIF
      RETURN
      END
C*DK ELW201
      SUBROUTINE ELW201
C***************************************************************
C
C***************************************************************
C  THIS IS THE MAIN PROGRAM, CONSISTING OF:
C 1) INITIALIZATION OF THE GENERATOR;
C 2) GENERATION OF AN EVENT SAMPLE,
C    AND SUBSEQUENT ANALYSIS OF THE EVENTS;
C 3) EVALUATION OF THE TOTAL GENERATED CROSS SECTION
C
C SELECTION ROUTINE FOR VARIOUS 'SUB'PROCESSES OF E E ---> E E E E
C IPROC =1: MU+(Q3)  L+(Q5) MU-(Q4)  L-(Q6)
C        2: MU+(Q3) MU+(Q5) MU-(Q4) MU-(Q6)
C        3:  E+(Q3) MU+(Q5)  E-(Q4) MU-(Q6)
C        4:  E+(Q3)  L+(Q5)  E-(Q4)  L-(Q6)
C        5:  E+(Q3)  E+(Q5)  E-(Q4)  E-(Q6)
CJ.H.    6:  TAU+(Q3)TAU+(Q5)TAU-(Q4)TAU-(Q6)
C
      IMPLICIT DOUBLEPRECISION(A-H,O-Z)
C*CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C*CC BCS
      COMMON /REDUCE/ ISEL(6,3),ILZ(6,3,64)
      COMMON /ANGLE / THMUMI,THMUMA
      COMMON /BOUND / W2MIN,W2MAX
      COMMON /CENINI/ THETA0,C0
      COMMON /CHARGE/ QCHARG,QCHRG2,QCHRG3,QCHRG4
      COMMON /CONST / ALFA,BARN,PI,TWOPI
      COMMON /EDGE  / WDMIN,WDMAX,SDMIN,SDMAX
      COMMON /FACTOR/ FACE,FACL,FACM,PROC
      COMMON /FTSTAT/ SUMFT,SUMFT2,FTMAX,IEEN
      COMMON /GENC  / XLC1(4),XLC2(4),SA3(4),EA3(3)
      COMMON /GEND  / XLD1(4),SAPD(4),SA4(2),EA4
      COMMON /INIT  / PB,ET,EP(3),ECH(3)
      COMMON /INPUT / EB,THMIN,THMAX,ESWE,ESFT,WAP(4),WBP(4),VAP(4)
      COMMON /LOGCM / OUTFL(4)
      COMMON /MASSES/ XM,XMU,XML,XM2,XMU2,XML2
      COMMON /MOMENZ/ Q1(5),Q2(5),Q3(5),Q4(5),Q5(5),Q6(5)
      COMMON /PROPAR/ ID
      COMMON /SELECT/ IC,ICH
      COMMON /VECTOR/ P1(4),P2(4),QM(4),QP(4),PM(4),PP(4)
      COMMON /WESTAT/ SWE(4),SWEK(4),MWE(4),SUM,SUMK,MAXWE,IWE(4),IGEN
      COMMON /WECOUN/ IFAIL(4),IACC(4),INUL(4),ICHG(4),
     .                INEG,IONE,IZERO
      COMMON /WEIGHC/ WEIGHT(4),WEEV,IEVACC
      COMMON /MYCOMN/ PERROR,FTADD(4),ITER
      COMMON /CENDEC/ ICD(16),WCD(16),WCDK(16),ECD(16),XSEC(16),
     &   ESEC(16)
      COMMON /TRANS / IY
      COMMON /COUNTB/ IREGB1,IREGB2,IPROB1,IPROB2
      DIMENSION IDUMP4(4)
      DOUBLEPRECISION MWE,MAXWE
      REAL ZMAS4,ZWID4,SINW24,EB4,ESWE4,ESFT4,THMIN4,THMAX4,THMUI4,
     &   THMUA4,THET04,WMIN4,WMAX4,WMINE4,WMAXE4,PMIN4,PMAX4,
     &   WAP4(4),WBP4(4),PERR4
      REAL PMOM
      DIMENSION  PMOM(5,6)
      INTEGER ITYP(6)  ,NLINK
      LOGICAL OUTFL,MYFINS
C--- ADDED for interfacing
      COMMON /KGCOMM/ IST,IDPR,ILEARN,IPROC,ITOT,IREJEC,NEVENT(10)
      COMMON /KGCOMR/ SDVRT(3),ECMS,WEIT
      REAL SDVRT,ECMS,WEIT,TABL
      REAL RNF100,THET,A,B
      DIMENSION ITAB(4),TABL(34)
      INTEGER ALTABL,ALRLEP
      EXTERNAL ALTABL,ALRLEP,RNF100 ,NLINK
      PARAMETER ( ICODE=7,ICODP=-7,ICOMU=9,ICOMP=-9,ICOTO=11,ICOTP=-11)
C----end adding
      DATA IDIA /0/
C
C ALEPH particle codes.
C
      DATA IBEAME /362/, IBEAMP /361/, ICELEC /3/, ICPOSI /2/,
     &     ICMUPL /5/, ICMUMI /6/, ICTAUP/33/, ICTAUM /34/
C-------------------------------------------------------------
C  INITIALIZATION            *********************
C
      IUT= IW(6)
      JTRIG = NLINK('TRIG',0)
      IF (JTRIG.NE.0) THEN
        ITOT = IW(JTRIG+2)
        IREQAC = ITOT
      ELSE
        ITOT   =  1000
        IREQAC = ITOT
      ENDIF
C
C THE SETUP PHASE: ASK FOR THE INPUT PARAMETERS
C
C-------------------------------------------------------------
C.....EBEAM,PROCESS CLASS
      EB4=46.1
      IPROC= 1
C.....IREJEC,ESTIM. MAX. WEIGHTS,INFORMATION LEVEL
C     NUMBER & CENTRAL DETECTOR THETA ANGLE
      IREJEC = 2
      ESWE4= 1.1
      ESFT4= 1.3
      INFO = 1
      IDUMP = 1
      ILEARN = 1
C.....CUTS ON INVARIANT MASS SQUARED BOTH PAIRS
      WMIN4= 0.
      WMAX4= 0.
      WMINE4 = 0.
      WMAXE4 = 0.
C.....CUTS IN ENERGY AND COS(THETA) FOR OUTPUT PARTICLE 1
      PMIN4 = 0.
      PMINE4 = 0.
      PERR4  = 0.
      THMIN4 = 0.
      THMAX4 = 0.
      THMUI4 = 0.
      THMUA4 = 0.
      THET04 = 0.
C.....RELATIVE IMPORTANCE OF SUBGENERATORS A,B,C AND D........
      WAP(1) = 1.
      WAP(2) = 1.
      WAP(3) = 1.
      WAP(4) = 1.
C.....IMPORTANCE FACTORS
      WBP(1) = 1.
      WBP(2) = 1.
      WBP(3) = 2.31
      WBP(4) = 4.39
C-------------------------------------------------------------
C  The default values can be changed by the DATA CARD GENE
       JGENE = NLINK('GENE',0)
       IF(JGENE.NE.0) THEN
        EB4= RW(JGENE+1)
        IPROC= IW(JGENE+2)
        IREJEC = IW(JGENE+3)
        IDUMP  = IW(JGENE+4)
        INFO   = IW(JGENE+5)
        ILEARN = IW(JGENE+6)
        ESWE4= RW(JGENE+7)
        ESFT4= RW(JGENE+8)
        THET04= RW(JGENE+9)
        ZMAS4= RW(JGENE+10)
        ZWID4= RW(JGENE+11)
        SINW24= RW(JGENE+12)
       ENDIF
       TABL(1) = EB4
       TABL(2) = IPROC
       TABL(3) = IREJEC
       TABL(4) = ESWE4
       TABL(5) = ESFT4
       TABL(6) = THET04
C
C  Main vertex generation
       SDVRT(1) = 0.035
       SDVRT(2) = 0.0012
       SDVRT(3) = 1.28
       JSVRT = NLINK('SVRT',0)
       IF(JSVRT.NE.0) THEN
        SDVRT(1) = RW(JSVRT+1)
        SDVRT(2) = RW(JSVRT+2)
        SDVRT(3) = RW(JSVRT+3)
       ENDIF
       TABL(7) = SDVRT(1)
       TABL(8) = SDVRT(2)
       TABL(9) = SDVRT(3)
C  The default values can be changed by the DATA CARD GCUT
       JGCUT = NLINK('GCUT',0)
       IF(JGCUT.NE.0) THEN
        WMIN4= RW(JGCUT+1)
        WMAX4= RW(JGCUT+2)
        WMINE4 = RW(JGCUT+3)
        WMAXE4 = RW(JGCUT+4)
        PMIN4 = RW(JGCUT+5)
        PMINE4 = RW(JGCUT+6)
        THMIN4 = RW(JGCUT+7)
        THMAX4 = RW(JGCUT+8)
        THMUI4 = RW(JGCUT+9)
        THMUA4 = RW(JGCUT+10)
        PERR4  = RW(JGCUT+11)
       ENDIF
C  The default values can be changed by the DATA CARD GIMP
       JGIMP = NLINK('GIMP',0)
       IF(JGIMP.NE.0) THEN
        DO 18 II = 1,4
 18    WAP4(II) = RW(JGIMP+II)
        DO 12 II = 1,4
 12     WBP4(II)= RW(JGIMP+4+II)
        ENDIF
      DO 21 II = 1,4
        TABL(23+II) = WAP4(II)
 21     TABL(23+II+4) = WBP4(II)
C
       TABL(10) = WMIN4
       TABL(11) = WMAX4
       TABL(12) = WMINE4
       TABL(13) = WMAXE4
       TABL(14) = PMIN4
       TABL(15) = PMINE4
       TABL(16) = THMIN4
       TABL(17) = THMAX4
       TABL(18) = THMUI4
       TABL(19) = THMUA4
       TABL(20) = PERR4
       TABL(21) = ZMAS4
       TABL(22) = ZWID4
       TABL(23) = SINW24
C  Fill the KPAR bank with the generator parameters
       NCOL = 31
       NROW = 1
       JKPAR = ALTABL('KPAR',NCOL,NROW,TABL,'2I,(F)','C')
C  Fill RLEP bank
      IEBEAM = NINT(EB4*1000. )
      JRLEP = ALRLEP(IEBEAM,'    ',0,0,0)
C
      PRINT 151,IPROC,ITOT,IREQAC,IREJEC,IDUMP,INFO,ILEARN
      PRINT 152,ZMAS4,ZWID4,SINW24
      PRINT 153,EB4,ESWE4,ESFT4,THMIN4,THMAX4,THMUI4,THMUA4,THET04
      PRINT 154,WMIN4,WMAX4,WMINE4,WMAXE4,PMIN4,PMINE4,PERR4
      PRINT 155,(WAP4(J),J=1,4),(WBP4(J),J=1,4)
  151 FORMAT(/' I have read in these parameters:'/' IPROC ',I2/
     &   ' ITOT ',I10/' IREQAC ',I10/' IREJEC ',I2/' IDUMP ',I2/
     &   ' INFO ',I2/' ILEARN ',I2/)
  152 FORMAT(/' ZMAS4 ',G12.6/' ZWID4 ',G12.6/' SINW24 ',G12.6)
  153 FORMAT(/' EB4 ',G12.6/' ESWE4,ESFT4 ',2G12.6/
     &   ' THMIN4,THMAX4 ',2G12.6/' THMUI4,THMUA4 ',2G12.6/
     &   ' THET04 ',G12.6)
  154 FORMAT(/' WMIN4,WMAX4 ',2G12.6/' WMINE4,WMAXE4 ',2G12.6/
     &   ' PMIN4,PMINE4 ',2G12.6/' PERR4 ',G12.6)
  155 FORMAT(/' WAP4 ',4(G12.6,1X)/' WBP4 ',4(G12.6,1X)/)
C
C
C Get values from data cards and set other default values
      IF (.TRUE.) THEN
C
      PRINT 1000,IPROC
 1000 FORMAT('0PROCESS NUMBER',I5,' HAS BEEN SELECTED'//
     .       ' DOUBLE TAGGING :'/
     .       '  THMIN < THQM < THMAX'/
     .       '  THMIN < THQP < THMAX'//)
C
C.....BEAM ENERGY (IN GEV)....................................
         EB     = DBLE(EB4)
C
C.....MASS OF BEAM PARTICLES (ELECTRON MASS)..................
         XM     = 0.511D-3/EB
C
C.....MASS OF PRODUCED PARTICLES..............................
         XMU    = 0.1057D0/EB
C
C.....MASS OF SCATTERED OR PRODUCED PARICLES..................
         XML    = 1.784D0/EB
C
C.....ESTIMATED MAXIMUM WEIGHT................................
         ESWE   =  DBLE(ESWE4)
         ESFT   =  DBLE(ESFT4)
C
C.....MINIMUM AND MAXIMUM ANGLE SCATTERED POSITRON
C
C.....MINIMUM AND MAXIMUM ANGLE SCATTERED ELECTRON
         THMIN  = DBLE(THMIN4)
         THMAX  = DBLE(THMAX4)
C
C.....CUTS ON MUON ANGLES
         THMUMI =   DBLE(THMUI4)
         THMUMA =   DBLE(THMUA4)
C
C.....CENTRAL DETECTOR DEFINITION (USED IN CENDTC)
         THETA0 =  DBLE(THET04)
C
C.....RELATIVE IMPORTANCE OF SUBGENERATORS A,B,C AND D........
         DO 201 IDL = 1, 4
            WAP(IDL) = DBLE(WAP4(IDL))
            WBP(IDL) = DBLE(WBP4(IDL))
  201    CONTINUE
         IF (PERR4.LT.0.) THEN
            PERROR = 10.D0
         ELSE
            PERROR = DBLE(PERR4)
         ENDIF
C
C
         IF (ILEARN.EQ.1) THEN
C Set some default starting values if ILEARN = 1
            ESFT = 2.D0
            ESWE = 5.D0
            IF (IREJEC.EQ.1) THEN
               PRINT'(
     &'' IREJEC = 1 option w/ ILEARN = 1 not yet implemented.'')'
               CALL EXIT
C               ITOT = 100
            ELSE
               ITOT = 1000
            ENDIF
            DO 2112 IDL = 1, 4
               WAP(IDL) = 1.D0
               WBP(IDL) = 1.D0
 2112       CONTINUE
         ENDIF
      ENDIF
      IY = 0
      ITER = 0
      GO TO 2109
      ENTRY ELW2IN
C
C J.Hilgart  Start the generator over at this point if ILEARN = 1
C
 2109 CONTINUE
C J.Hilgart
C Set things to zer0
      IGEN = 0
      DO 141 IO = 1, 3
         DO 141 IN = 1, 6
  141       ISEL(IN,IO) = 0
      DO 142 IO = 1, 64
         DO 142 IN = 1, 3
            DO 142 IN1 = 1, 6
  142          ILZ(IN1,IN,IO) = 0
      DO 143 IDL = 1, 4
         FTADD(IDL) = 0.D0
         SWE(IDL) = 0.D0
         SWEK(IDL) = 0.D0
         MWE(IDL) = 0.D0
         IWE(IDL) = 0
         IFAIL(IDL) = 0
         IACC(IDL) = 0
         INUL(IDL) = 0
  143 CONTINUE
      SUM = 0.D0
      SUMK = 0.D0
      MAXWE = 0.D0
      SUMFT = 0.D0
      SUMFT2 = 0.D0
      FTMAX = 0.D0
      IEEN = 0
      INEG = 0
      IONE = 0
      IZERO = 0
      IREGA1 = 0
      IREGA2 = 0
      IREGB1 = 0
      IREGB2 = 0
      IPROB1 = 0
      IPROB2 = 0
      IPROD1 = 0
      IPROD2 = 0
C
      DO 145 IDL = 1, 16
         ICD(IDL) = 0
         WCD(IDL) = 0.D0
         WCDK(IDL) = 0.D0
         ECD(IDL) = 0.D0
         XSEC(IDL) = 0.D0
         ESEC(IDL) = 0.D0
  145 CONTINUE
      FACE   = 1.D+03
      FACL   = 1.D+03
      FACM   = 1.D+03
      PROC   = 1.D+06
      ITER = ITER + 1
      IF (ILEARN.EQ.1)  THEN
         IF (ITER.EQ.1) PRINT 2113
         PRINT 2111,ITER,ITOT,(WAP(J),J=1,4),(WBP(K),K=1,4)
      ENDIF
 2113 FORMAT(/' WE ARE GOING TO FIND SOME OPTIMAL VALUES USING ',
     &   ' SEVERAL ITERATIONS.')
 2111 FORMAT(/' ',6X,'STARTING ITERATION ',I3/
     &   ' ITOT events is',I10/
     &       ' ',6X,'ARRAY WAP : ',4(D18.6,2X)/
     &       ' ',6X,'ARRAY WBP : ',4(D18.6,2X))
      PRINT 2110,ESFT,ESWE
 2110 FORMAT(/' ',6X,'Maximum weights '/
     &   ' ',6X,'ESFT = ',D19.6/
     &   ' ',6X,'ESWE = ',D19.6/)
C
      GOTO (1001,1002,1003,1004,1005,1006), IPROC
 1001 CONTINUE
C E E --> MU MU L L
C THE MU MU PAIR MAY BE A QUARK ANTI QUARK PAIR WITH CHARGE QCHARG
C QM = FOUR MOMENTUM L-   MASS = XML
C QP = FOUR MOMENTUM L+   MASS = XML
C PM = FOUR MOMENTUM MU-  MASS = XMU
C PP = FOUR MOMENTUM MU+  MASS = XMU
C ONLY MCC AND MCD CONTRIBUTE
      WAP(1) = 0.D0
      WAP(2) = 0.D0
      VAP(1) = 0.D0
      VAP(2) = 0.D0
      VAP(3) = 1.D0
      VAP(4) = 1.D0
C...W2MIN W2MAX FREE TO CHOOSE
C...W2MIN = MINIMUM (PM+PP)**2
C LEPTONS      : QCHARG = 1    , ID = 1
C U,C,T QUARKS : QCHARG = -2/3 , ID = 2
C D,S,B QUARKS : QCHARG = 1/3  , ID = 3
C QCHARG = CHARGE CORRESPONDING TO THE LINES WITH 4MOMENTUM PM AND PP
C CONSEQUENTLY IN EE --> L L QUARK ANTI-QUARK CASE THE QUARK 4MOMENTA
C ARE PM AND PP, WHEREAS THE QM AND QP ARE THE L L 4MOMENTA. NOW THE
C XMU IS THE QUARK MASS AND XML IS THE LEPTON MASS.
      ID     = 1
      QCHARG = 1.D0
      ICPAR1 = ICTAUM
      ICPAR2 = ICTAUP
      ICPAR3 = ICMUMI
      ICPAR4 = ICMUPL
C      IF (ITER.EQ.1) PRINT 1011,XM,XMU,XML,QCHARG,W2MIN,W2MAX,
C     &   WAP,WBP,VAP
      IF (ITER.EQ.1) PRINT 1011
 1011 FORMAT('0',130(1H*)//
     .       ' MONTE CARLO SIMULATION OF THE PROCESS : ',
     .       'E+ E- ---> MU+ MU- L+ L-'//' ',130(1H*)//)
      GOTO 1007
 1002 CONTINUE
C E E --> MU MU MU MU
C QM = FOUR MOMENTUM MU-  MASS = XMU
C QP = FOUR MOMENTUM MU+  MASS = XMU
C PM = FOUR MOMENTUM MU-  MASS = XMU
C PP = FOUR MOMENTUM MU+  MASS = XMU
C ONLY MCC AND MCD CONTRIBUTE
      WAP(1) = 0.D0
      WAP(2) = 0.D0
      VAP(1) = 0.D0
      VAP(2) = 0.D0
      VAP(3) = 0.5D0
      VAP(4) = 0.5D0
      XML    = XMU
C QCHARG MAY NOT BE CHANGED
      ID     = 1
      QCHARG = 1.D0
      ICPAR1 = ICMUMI
      ICPAR2 = ICMUPL
      ICPAR3 = ICMUMI
      ICPAR4 = ICMUPL
      IF (ITER.EQ.1) PRINT 1021
 1021 FORMAT('0',130(1H*)//
     .       ' MONTE CARLO SIMULATION OF THE PROCESS : ',
     .       'E+ E- ---> MU+ MU- MU+ MU-'//' ',130(1H*)//)
      GOTO 1007
 1003 CONTINUE
C E E --> E E MU MU
C QM = FOUR MOMENTUM E-   MASS = XM
C QP = FOUR MOMENTUM E+   MASS = XM
C PM = FOUR MOMENTUM MU-  MASS = XMU
C PP = FOUR MOMENTUM MU+  MASS = XMU
C MCA MCB MCC AND MCD CONTRIBUTE
      VAP(1) = 1.D0
      VAP(2) = 1.D0
      VAP(3) = 1.D0
      VAP(4) = 1.D0
      XML    = XM
C...W2MIN W2MAX FREE TO CHOOSE
C W2MIN = MINIMUM (PM+PP)**2
C QCHARG MAY NOT BE CHANGED
      ID     = 1
      QCHARG = 1.D0
      ICPAR1 = ICELEC
      ICPAR2 = ICPOSI
      ICPAR3 = ICMUMI
      ICPAR4 = ICMUPL
      IF (ITER.EQ.1) PRINT 1031
 1031 FORMAT('0',130(1H*)//
     .       ' MONTE CARLO SIMULATION OF THE PROCESS : ',
     .       'E+ E- ---> E+ E- MU+ MU-'//' ',130(1H*)//)
      GOTO 1007
 1004 CONTINUE
C J.Hilgart Changed from eemumu to ee tau tau
C E E --> E E L L
C THE L L PAIR MAY BE A QUARK ANTI QUARK PAIR WITH CHARGE QCHARG
C QM = FOUR MOMENTUM E-  MASS = XM
C QP = FOUR MOMENTUM E+  MASS = XM
C PM = FOUR MOMENTUM L-  MASS = XML
C PP = FOUR MOMENTUM L+  MASS = XML
C MCA MCB MCC AND MCD CONTRIBUTE
      VAP(1) = 1.D0
      VAP(2) = 1.D0
      VAP(3) = 1.D0
      VAP(4) = 1.D0
      IF (ITER.EQ.1) THEN
         XMU    = XML
         XML    = XM
      ENDIF
C...W2MIN W2MAX FREE TO CHOOSE
C W2MIN = MINIMUM (PM+PP)**2
C LEPTONS      : QCHARG = 1
C U,C,T QUARKS : QCHARG = -2/3
C D,S,B QUARKS : QCHARG = 1/3
C QCHARG = CHARGE CORRESPONDING TO THE LINES WITH 4MOMENTUM PM AND PP
      ID     = 1
      QCHARG = 1.D0
      ICPAR1 = ICELEC
      ICPAR2 = ICPOSI
      ICPAR3 = ICTAUM
      ICPAR4 = ICTAUP
      IF (ITER.EQ.1) PRINT 1041
 1041 FORMAT('0',130(1H*)//
     .       ' MONTE CARLO SIMULATION OF THE PROCESS : ',
     .       'E+ E- ---> E+ E- L+ L-'//' ',130(1H*)//)
      GOTO 1007
 1005 VAP(1) = 1.D0
      VAP(2) = 1.D0
      VAP(3) = 0.5D0
      VAP(4) = 0.5D0
      XML    = XM
      XMU    = XM
C QCHARG MAY NOT BE CHANGED
      ID     = 1
      QCHARG = 1.D0
      ICPAR1 = ICELEC
      ICPAR2 = ICPOSI
      ICPAR3 = ICELEC
      ICPAR4 = ICPOSI
C      IF (ITER.EQ.1) PRINT 1051,XM,XMU,XML,QCHARG,W2MIN,W2MAX,
C     &   WAP,WBP,VAP
      IF (ITER.EQ.1) PRINT 1051
 1051 FORMAT('0',130(1H*)//
     .       ' MONTE CARLO SIMULATION OF THE PROCESS : ',
     .       'E+ E- ---> E+ E- E+ E-'//' ',130(1H*)//)
      GOTO 1007
 1006 CONTINUE
C J.H. Modelled after mu mu mu mu
C E E --> TAU TAU TAU TAU
C QM = FOUR MOMENTUM TAU-  MASS = XML
C QP = FOUR MOMENTUM TAU+  MASS = XML
C PM = FOUR MOMENTUM TAU-  MASS = XML
C PP = FOUR MOMENTUM TAU+  MASS = XML
C ONLY MCC AND MCD CONTRIBUTE
      WAP(1) = 0.D0
      WAP(2) = 0.D0
      VAP(1) = 0.D0
      VAP(2) = 0.D0
      VAP(3) = 0.5D0
      VAP(4) = 0.5D0
      XMU    = XML
C QCHARG MAY NOT BE CHANGED
      ID     = 1
      QCHARG = 1.D0
      ICPAR1 = ICTAUM
      ICPAR2 = ICTAUP
      ICPAR3 = ICTAUM
      ICPAR4 = ICTAUP
      IF (ITER.EQ.1) PRINT 1061
 1061 FORMAT('0',130(1H*)//
     .       ' MONTE CARLO SIMULATION OF THE PROCESS : ',
     .       'E+ E- ---> TAU+ TAU- TAU+ TAU-'//' ',130(1H*)//)
      GOTO 1007
 1007 CONTINUE
C
C J.Hilgart
      IF (WMIN4.LE.0.) THEN
         W2MIN  = 4.D0*XMU*XMU
      ELSE
         W2MIN = (DBLE(WMIN4)/EB)**2
      ENDIF
      IF (WMAX4.LE.0.) THEN
         W2MAX  = 4.D0*(1.D0-XML)*(1.D0-XML)
      ELSE
         W2MAX = (DBLE(WMAX4)/EB)**2
      ENDIF
      IF (WMINE4.LE.0.) THEN
         W2MINE = 4.D0*XML*XML
      ELSE
         W2MINE = (DBLE(WMINE4)/EB)**2
      ENDIF
      IF (WMAXE4.LE.0.) THEN
         W2MAXE = 4.D0*(1.D0-XMU)*(1.D0-XMU)
      ELSE
         W2MAXE = (DBLE(WMAXE4)/EB)**2
      ENDIF
C
C Momentum cuts
C
      PMIN = DBLE(PMIN4)/EB
      PMINE = DBLE(PMINE4)/EB
      IF (ITER.EQ.1) PRINT 1012,XM,XMU,XML,QCHARG,W2MIN,W2MAX,
     &   W2MINE,W2MAXE,PMIN,PMINE,WAP,WBP,VAP
 1012 FORMAT(' ',6X,'XM     = ',D19.6/
     .       ' ',6X,'XMU    = ',D19.6/
     .       ' ',6X,'XML    = ',D19.6/
     .       ' ',6X,'QCHARG = ',D19.6/
     .       ' ',6X,'W2MIN  = ',D19.6/
     .       ' ',6X,'W2MAX  = ',D19.6/
     .       ' ',6X,'W2MINE  = ',D19.6/
     .       ' ',6X,'W2MAXE  = ',D19.6/
     .       ' ',6X,'PMIN  = ',D19.6/
     .       ' ',6X,'PMINE  = ',D19.6/
     .       ' ',6X,'ARRAY WAP : ',4(D19.6,2X)/
     .       ' ',6X,'ARRAY WBP : ',4(D19.6,2X)/
     .       ' ',6X,'ARRAY VAP : ',4(D19.6,2X))

C end J.Hilgart
      XM2    = XM*XM
      XMU2   = XMU*XMU
      VAP(2) = 0.D0
      VAP(3) = 0.5D0
      VAP(4) = 0.5D0
      XMU    = XML
C QCHARG MAY NOT BE CHANGED
      ID     = 1
      QCHARG = 1.D0
      ICPAR1 = ICTAUM
      ICPAR2 = ICTAUP
      ICPAR3 = ICTAUM
      ICPAR4 = ICTAUP
      IF (ITER.EQ.1) PRINT 1061
 1061 FORMAT('0',130(1H*)//
     .       ' MONTE CARLO SIMULATION OF THE PROCESS : ',
     .       'E+ E- ---> TAU+ TAU- TAU+ TAU-'//' ',130(1H*)//)
      GOTO 1007
 1007 CONTINUE
C
C J.Hilgart
      IF (WMIN4.LE.0.) THEN
         W2MIN  = 4.D0*XMU*XMU
      ELSE
         W2MIN = (DBLE(WMIN4)/EB)**2
      ENDIF
      IF (WMAX4.LE.0.) THEN
         W2MAX  = 4.D0*(1.D0-XML)*(1.D0-XML)
      ELSE
         W2MAX = (DBLE(WMAX4)/EB)**2
      ENDIF
      IF (WMINE4.LE.0.) THEN
         W2MINE = 4.D0*XML*XML
      ELSE
         W2MINE = (DBLE(WMINE4)/EB)**2
      ENDIF
      IF (WMAXE4.LE.0.) THEN
         W2MAXE = 4.D0*(1.D0-XMU)*(1.D0-XMU)
      ELSE
         W2MAXE = (DBLE(WMAXE4)/EB)**2
      ENDIF
C
C Momentum cuts
C
      PMIN = DBLE(PMIN4)/EB
      PMINE = DBLE(PMINE4)/EB
      IF (ITER.EQ.1) PRINT 1012,XM,XMU,XML,QCHARG,W2MIN,W2MAX,
     &   W2MINE,W2MAXE,PMIN,PMINE,WAP,WBP,VAP
 1012 FORMAT(' ',6X,'XM     = ',D19.6/
     .       ' ',6X,'XMU    = ',D19.6/
     .       ' ',6X,'XML    = ',D19.6/
     .       ' ',6X,'QCHARG = ',D19.6/
     .       ' ',6X,'W2MIN  = ',D19.6/
     .       ' ',6X,'W2MAX  = ',D19.6/
     .       ' ',6X,'W2MINE  = ',D19.6/
     .       ' ',6X,'W2MAXE  = ',D19.6/
     .       ' ',6X,'PMIN  = ',D19.6/
     .       ' ',6X,'PMINE  = ',D19.6/
     .       ' ',6X,'ARRAY WAP : ',4(D19.6,2X)/
     .       ' ',6X,'ARRAY WBP : ',4(D19.6,2X)/
     .       ' ',6X,'ARRAY VAP : ',4(D19.6,2X))

C end J.Hilgart
      XM2    = XM*XM
      XMU2   = XMU*XMU
      XML2   = XML*XML
      QCHRG2 = QCHARG*QCHARG
      QCHRG3 = QCHARG*QCHRG2
      QCHRG4 = QCHRG2*QCHRG2
C
      CALL START(ZMAS4,ZWID4,SINW24,IPROC,ITOT)
      IF (IREJEC.EQ.2) X = 1.D0
      RETURN
C
C
C  EVENT GENERATION          *********************
C
      ENTRY ELW2EV
C
C  EVENT STATUS (0 = O.K.)
       ISTA = 0
C
C  Initialize the track number
C
   10 CONTINUE
      NEVENT(1) = NEVENT(1) + 1
      IS     = 1
      ETA0   = DBLE(RNF100(IS))
      IF (ETA0.LT.EP(2)) IS = -1
      IF (ETA0.LT.EP(2+IS)) GOTO 1
      IC     = 3 + IS
      GOTO 2
    1 IC     = 2 + IS
    2 CONTINUE
C
      IDUMP4(IC) = IDUMP
C
      GOTO (3,4,5,6),IC
    3 CALL MCA(IPROC,IDUMP4(1))
      GOTO 7
    4 CALL MCB(IPROC,IDUMP4(2))
      GOTO 7
    5 CONTINUE
      ETA = DBLE(RNF100(12))
      IF (ETA.LT.EA3(1)) IDEC=1
      IF (ETA.LT.EA3(2).AND.ETA.GT.EA3(1)) IDEC=2
      IF (ETA.LT.EA3(3).AND.ETA.GT.EA3(2)) IDEC=3
      IF (ETA.GT.EA3(3)) IDEC=4
      IF (IDEC.NE.1.AND.IDEC.NE.2.AND.
     .    IDEC.NE.3.AND.IDEC.NE.4) PRINT 666
  666 FORMAT(' $$$$ERROR$$$$ IN MAIN IDEC .NE. 1,2,3,4')
      CALL MCC(IPROC,IDEC,IDUMP4(3))
      GOTO 7
    6 CONTINUE
      ETA = DBLE(RNF100(12))
      IDEC=1
      IF (ETA.GT.EA4) IDEC=2
      CALL MCD(IPROC,IDEC,IDUMP4(4))
    7 CONTINUE
C
      IF (OUTFL(IC)) GOTO 800
      PMV       = DSQRT(PM(1)*PM(1)+PM(2)*PM(2)+PM(3)*PM(3))
      PPV       = DSQRT(PP(1)*PP(1)+PP(2)*PP(2)+PP(3)*PP(3))
      QMV       = DSQRT(QM(1)*QM(1)+QM(2)*QM(2)+QM(3)*QM(3))
      QPV       = DSQRT(QP(1)*QP(1)+QP(2)*QP(2)+QP(3)*QP(3))
C J.Hilgart
      IF (PMV.LT.PMIN .OR. PPV.LT.PMIN .OR. QMV.LT.PMINE .OR.
     &   QPV.LT.PMINE)  GOTO 800
      CSPM      =  PM(3)/PMV
      CSPP      =  PP(3)/PPV
      CSQM      =  QM(3)/QMV
      CSQP      = -QP(3)/QPV
      THQM      = ACOS(CSQM)/PI*180.D0
      THQP      = ACOS(CSQP)/PI*180.D0
      THPM      = ACOS(CSPM)/PI*180.D0
      THPP      = ACOS(CSPP)/PI*180.D0
      IF (THPM.LT.THMUMI.OR.THPM.GT.THMUMA.OR.
     .    THPP.LT.THMUMI.OR.THPP.GT.THMUMA) GOTO 800
      IF (THQM.LT.THMIN .OR.THQM.GT.THMAX .OR.
     .    THQP.LT.THMIN .OR.THQP.GT.THMAX ) GOTO 800
      W2MU      = 2.D0*DOT(PM,PP)+2.D0*XMU2
      W2EL     = 2.D0*DOT(QM,QP)+ 2.D0*XML2
      W2MU2     = 2.D0*DOT(QM,PP)+ XMU2 + XML2
      W2MU3     = 2.D0*DOT(QP,PM) + XMU2 + XML2
      IF (W2MU.LT.W2MIN.OR.W2MU.GT.W2MAX) GOTO 800
      IF (W2EL.LT.W2MINE.OR.W2EL.GT.W2MAXE)  GOTO 800
C      IF (IPROC.NE.2.AND.IPROC.NE.5) GOTO 801
      IF (IPROC.NE.2.AND.IPROC.NE.5.AND.IPROC.NE.6) GOTO 801
      IF (W2EL.LT.W2MIN.OR.W2EL.GT.W2MAX) GOTO 800
      IF (W2MU2.LT.W2MIN.OR.W2MU2.GT.W2MAX) GOTO 800
      IF (W2MU3.LT.W2MIN.OR.W2MU3.GT.W2MAX) GOTO 800
      GOTO 801
  800 CONTINUE
      WEIGHT(IC) = 0.D0
      WEEV       = 0.D0
      FT         = 0.D0
      INUL(IC)   = INUL(IC) + 1
      IZERO      = IZERO    + 1
  801 CONTINUE
C
      IEVACC    = 0
      IWE(IC)   = IWE(IC)  + 1
      IGEN      = IGEN     + 1
      IF (WEIGHT(IC).LE.0.D0.OR.WEIGHT(IC).GT.1.D-30) GOTO 17
      WEIGHT(IC) = 0.D0
      WEEV       = 0.D0
      FT         = 0.D0
   17 CONTINUE
      SWE(IC)   = SWE(IC)  + WEIGHT(IC)
      SUM       = SUM      + WEEV
      SWEK(IC)  = SWEK(IC) + WEIGHT(IC)*WEIGHT(IC)
      SUMK      = SUMK     + WEEV*WEEV
      IF (MWE(IC).LT.WEIGHT(IC)) MWE(IC) = WEIGHT(IC)
      IF (MAXWE.LT.WEEV) MAXWE = WEEV
C
C-----PRODUCTION OF EVENTS WITH WEIGHT 1-------------------------
       IDPR = IC
       WEIT = WEEV
       ECMS = 2.*EB4
      GOTO (25,26) ,IREJEC
   26 CONTINUE
      ETA1   = DBLE(RNF100(11))
      IF (ETA1*ESWE.GT.WEIGHT(IC)) GOTO 8
      GOTO 27
   25 X      = WEEV
   27 CONTINUE
      IEVACC    = 1
      IACC(IC)  = IACC(IC) + 1
      IONE      = IONE     + 1
    8 CONTINUE
C      CALL HISTO1(1,8HWEIGHT   ,20,0.D0,2.D0,WEEV,1.D0)
      CALL HISTO1(1,8HWEIGHT   ,20,0.D0,ESWE,WEEV,1.D0)
      IF (IEVACC.EQ.0) GOTO 10
      IF (X.EQ.0.D0) GOTO 28
      IDIA  = IDIA + 1
      Q1(5) = -XM
      Q2(5) =  XM
      Q3(5) = -XML
      Q4(5) =  XML
      Q5(5) = -XMU
      Q6(5) =  XMU
      DO 13 IV=1,4
      Q1(IV) = P2(IV)
      Q2(IV) = P1(IV)
      Q3(IV) = QP(IV)
      Q4(IV) = QM(IV)
      Q5(IV) = PP(IV)
      Q6(IV) = PM(IV)
   13 CONTINUE
      INFO  = -1
      IF (IDIA.EQ.1) INFO = 1
C
      FT    = DIAM(IPROC,INFO)
      FT    = X*FT
      FTADD(IC) = FTADD(IC) + FT
   28 CONTINUE
      CALL CENDTC(FT)
C      CALL HISTO1(2,8HFT      ,20,0.D0,2.D0,FT,1.D0)
      CALL HISTO1(2,8HFT      ,20,0.D0,ESFT,FT,1.D0)
      SUMFT = SUMFT  + FT
      SUMFT2= SUMFT2 + FT*FT
      IF (FTMAX.LT.FT) FTMAX = FT
C
C REJECTION ALGORITHM WHICH PRODUCES UNWEIGHTED EVENTS
      ETA2  = DBLE(RNF100(2))
      IF (ETA2*ESFT.GT.FT) GOTO 10
      NEVENT(10) = NEVENT(10) + 1
C
C We have an unweighted event. Write it out if requested.
      IEEN  = IEEN + 1
      WEIT = 1.
      IF (ILEARN.EQ.1) GO TO 20
C
C The beam particles
C
      IP = 1
      DO 531 IDL = 1, 4
  531    PMOM(IDL,IP) = REAL(Q1(IDL)*EB)
      PMOM(5,IP) = REAL(ABS(Q1(5))*EB)
      ITYP(IP) = IBEAMP
      IP = 2
      DO 532 IDL = 1, 4
  532    PMOM(IDL,IP) = REAL(Q2(IDL)*EB)
      PMOM(5,IP) = REAL(ABS(Q2(5))*EB)
      ITYP(IP) = IBEAME
C
C Final fermions
C
      IP = 3
      DO 533 IDL = 1, 4
  533    PMOM(IDL,IP) = REAL(Q3(IDL)*EB)
      PMOM(5,IP) = REAL(ABS(Q3(5))*EB)
      ITYP(IP) = ICPAR1
      IP = 4
      DO 534 IDL = 1, 4
  534    PMOM(IDL,IP) = REAL(Q4(IDL)*EB)
      PMOM(5,IP) = REAL(ABS(Q4(5))*EB)
      ITYP(IP) = ICPAR2
      IP = 5
      DO 535 IDL = 1, 4
  535    PMOM(IDL,IP) = REAL(Q5(IDL)*EB)
      PMOM(5,IP) = REAL(ABS(Q5(5))*EB)
      ITYP(IP) = ICPAR3
      IP = 6
      DO 536 IDL = 1, 4
  536    PMOM(IDL,IP) = REAL(Q6(IDL)*EB)
      PMOM(5,IP) = REAL(ABS(Q6(5))*EB)
      ITYP(IP) = ICPAR4
C
       IF( ILEARN.EQ.0) CALL LUFILL(PMOM,ITYP)
        CALL HFILL(10001,PMOM(4,3),0.,WEIT)
        CALL HFILL(10002,PMOM(4,4),0.,WEIT)
        CALL HFILL(10003,PMOM(4,5),0.,WEIT)
        CALL HFILL(10004,PMOM(4,6),0.,WEIT)
        THET = PMOM(3,3)/SQRT(PMOM(1,3)**2+PMOM(2,3)**2+PMOM(3,3)**2)
        CALL HFILL(10005,THET,0.,WEIT)
        THET = PMOM(3,4)/SQRT(PMOM(1,4)**2+PMOM(2,4)**2+PMOM(3,4)**2)
        CALL HFILL(10006,THET,0.,WEIT)
        THET = PMOM(3,5)/SQRT(PMOM(1,5)**2+PMOM(2,5)**2+PMOM(3,5)**2)
        CALL HFILL(10007,THET,0.,WEIT)
        THET = PMOM(3,6)/SQRT(PMOM(1,6)**2+PMOM(2,6)**2+PMOM(3,6)**2)
        CALL HFILL(10008,THET,0.,WEIT)
        XM12 = (PMOM(4,3)+PMOM(4,4))**2-(PMOM(3,3)+PMOM(3,4))**2-
     $         (PMOM(2,3)+PMOM(2,4))**2-(PMOM(1,3)+PMOM(1,4))**2
        IF (XM12.GT.0.) XM12=SQRT(XM12)
        CALL HFILL(10009,XM12,0.,WEIT)
        XM12 = (PMOM(4,5)+PMOM(4,6))**2-(PMOM(3,5)+PMOM(3,6))**2-
     $         (PMOM(2,5)+PMOM(2,6))**2-(PMOM(1,5)+PMOM(1,6))**2
        IF (XM12.GT.0.) XM12=SQRT(XM12)
        CALL HFILL(10010,XM12,0.,WEIT)
C      ENDIF
   20 CONTINUE
      RETURN
C
C  END OF GENERATION         *********************
C
      ENTRY ELW2ND
C
C-----RESULTS----------------------------------------------------
      IF (IREJEC.EQ.1) THEN
        WRITE(IUT,120)
  120  FORMAT('0',6X,'REQUESTED NUMBER OF EVENTS WITH WEIGHT 1 REACHED')
      ENDIF
C-----RESULTS----------------------------------------------------
C J.Hilgart Go back to almost the top if ILEARN = 1 and we have not fini
      CALL FINISH(IPROC,ITOT,IREJEC)
C
      RETURN
      END
C*DK USCJOB
      SUBROUTINE USCJOB
C --------------------------------------------------------------------
C
C --------------------------------------------------------------------
      COMMON /KGCOMM/ IST,IDP,ILEARN,IPROC,ITOT,IREJEC,NEVENT(10)
      COMMON /KGCOMR/ SDVRT(3),ECM,WEI
      CALL ELW2ND
      IUT= 6
      WRITE(IUT,101)
  101  FORMAT(//,20X,'EVENTS STATISTICS',
     &         /,20X,'*****************')
       WRITE(IUT,102)NEVENT(1),NEVENT( 9),NEVENT(10)
  102  FORMAT(/,5X,'# OF GENERATED EVENTS                = ',I10,
     &        /,5X,'# OF ACCEPTED  EVENTS                = ',I10,
     &        /,5X,'# OF ACCEPTED  EVENTS (BY GENERATOR) = ',I10)
      WRITE(IUT,103)
  103  FORMAT(//,20X,'ERRORS STATISTICS',
     &         /20X,'*****************')
      WRITE(IUT,104)NEVENT(2),NEVENT(3),NEVENT(4),NEVENT(5),NEVENT(6)
  104  FORMAT(/,10X,'ISTA = 2 BOS ERROR VERT/KINE# OF REJECT = ',I10,
     &        /,10X,'ISTA = 3 Too many tracks    # OF REJECT = ',I10,
     &        /,10X,'ISTA = 4 Beam in wrong pos. # OF REJECT = ',I10,
     &        /,10X,'ISTA = 5 Wrong status code  # OF REJECT = ',I10,
     &        /,10X,'ISTA = 6 unknown part code  # OF REJECT = ',I10)
      RETURN
      END
C*DK LUFILL
      SUBROUTINE LUFILL(PMOM,ITYP)
C --------------------------------------------------------------------
C
C --------------------------------------------------------------------
C*CA LUNDCOM
      PARAMETER (L1MST=40, L1PAR=80)
      PARAMETER (L2KTYP=120, L2PMAS=120, L2PWID=60, L2KFR=80, L2CFR=40)
      PARAMETER (L3DPAR=20, L3IDB=120, L3CBR=400, L3KDP=1600)
      PARAMETER (LEMSTE=40, LEPARE=80)
      PARAMETER (LJNPAR=2000)
      COMMON /LUDAT1/   MSTLU1(L1MST),PARLU1(L1PAR)
      COMMON /LUDAT2/   KTYPL2(L2KTYP),PMASL2(L2PMAS),PWIDL2(L2PWID)
     &                , KFRLU2(L2KFR),CFRLU2(L2CFR)
      COMMON /LUDAT3/   DPARL3(L3DPAR),IDBLU3(L3IDB),CBRLU3(L3CBR)
     &                , KDPLU3(L3KDP)
      COMMON /LUDATE/   MSTELE(LEMSTE),PARELE(LEPARE)
      COMMON /LUJETS/   NPARLU,KODELU(LJNPAR,2),PARTLU(LJNPAR,5)
C
C*CC LUNDCOM
      DIMENSION PMOM(5,6),ITYP(6),JTYP(6)
      PARAMETER ( ICODE=7,ICODP=-7,ICOMU=9,ICOMP=-9,ICOTO=11,ICOTP=-11)
      DATA IBEAME /362/, IBEAMP /361/, ICELEC /3/, ICPOSI /2/,
     &     ICMUPL /5/, ICMUMI /6/, ICTAUP/33/, ICTAUM /34/
      DO 10 IT=1,6
      IF (ITYP(IT).EQ.IBEAME) JTYP(IT)=ICODE
      IF (ITYP(IT).EQ.IBEAMP) JTYP(IT)=ICODP
      IF (ITYP(IT).EQ.ICELEC) JTYP(IT)=ICODE
      IF (ITYP(IT).EQ.ICPOSI) JTYP(IT)=ICODP
      IF (ITYP(IT).EQ.ICMUMI) JTYP(IT)=ICOMU
      IF (ITYP(IT).EQ.ICMUPL) JTYP(IT)=ICOMP
      IF (ITYP(IT).EQ.ICTAUP) JTYP(IT)=ICOTP
      IF (ITYP(IT).EQ.ICTAUM) JTYP(IT)=ICOTO
      DO 11  J=1,5
  11    PARTLU(IT,J)= PMOM(J,IT)
      KODELU(IT,1)= 0
      IF (IT.LE.2) KODELU(IT,1)=40000
      KODELU(IT,2)= JTYP(IT)
  10  CONTINUE
      NPARLU = 6
      CALL LUEXEC
      RETURN
      END
C*DK KLINBK
       SUBROUTINE KLINBK (JKLIN)
C --------------------------------------------------
C - B.Bloch - 910310
C! fill 'KLIN' bank FOR fisrt 90  LUND particles
CKEY KINE KINGAL LUND KLIN  /  USER
C  Get  the NOtracking marker word NOTRK from KRUN bank
C  Fill KLIN bank with LUND particle# which correspond
C       to GEANT particles ( or ALEPH particles)
C  Reduce PART and KLIN banks to their normal size
C
C - structure: SUBROUTINE subprogram
C              User Entry Name: KLINBK
C              External References: NAMIND(BOS77)
C                                   KBKLIN/AUBPRS
C                                   (ALEPHLIB)
C              Comdecks referenced: BCS,PARTJJ
C
C - Usage    : CALL KLINBK (JKLIN)
C - Output   : JKLIN  gt. 0 means OK
C*CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C*CC BCS
C     ILUGE (LLUGE) are the LUND numbers corresponding to the first
C                   part of PART bank ( used by GEANT)
C     ILUAL (LLUAL) are the LUND numbers corresponding to the rest of
C                   the PART bank
      PARAMETER ( LLUGE=52 ,   LLUAL =315)
      INTEGER ILUGE(LLUGE),ILUAL(LLUAL)
      EXTERNAL NAMIND
      CHARACTER TNAM*12
      DATA ILUGE /1,-7,7,0,-9,9,23,17,-17,38,
     &           18,-18,42,41,-41,37,24,57,43,44,
     &           45,46,47,70,-42,-57,-43,-44,-45,-46,
     &          -47,-70,-11,11,21,-21,20,-20,22,-22,
     &           58,3,-3,2,0,0,0,0,0,0,0,0/
      DATA ILUAL/-58, 4,83,19,-19, 8,-8,10,-10, 12,-12, 94,-94, 95, 0,
     $ 0,25, 3*0,36,96,26, 27,-27, 28,-28, 29,-29,30,-30,31,-31,32,-32,
     $ 33,34,35, 3*0 , 87,  0, 84, 3*0,500,501,502,503,504,505,506,-501,
     $ -502,-503,-504,-505,-506,0,0,101,-101,102,-102,103,-103,104,-104,
     $123,-123,124,-124,125,-125,126,-126,105,-105,106,-106,107,-107,108
     $,-108,109,-109,127,-127,128,-128,129,-129,130,-130,131,-131,61,-61
     $,62,-62,63,-63,64,-64,65,-65,66,-66,67,-67,68,-68,69,-69,48,-48,49
     $,-49,50,-50,51,-51,52,-52,53,-53,59,-59,60,-60,71,-71,72,-72,73,
     $-73,74,-74,75,-75,76,-76,54,-54,55,-55,56,-56,77,-77,78,-78,79,-79
     $,80,-80,145,-145,146,-146,147,-147,148,-148,149,-149,150,-150,151,
     $-151,152,-152,153,-153,154,-154,155,-155,156,-156,157,-157,158,
     $-158,159,-159,160,-160,161,-161,162,-162,163,-163,164,-164,165,
     $-165,166,-166,167,-167,168,-168,241,-241,242,-242,243,-243,244,
     $-244,245,-245,246,-246,247,-247,248,-248,249,-249,250,-250,251,
     $-251,252,-252,293,-293,294,-294,295,-295,296,-296,297,-297,298,
     $-298,299,-299,300,-300,301,-301,302,-302,308,-308,309,-309,310,
     $-310,311,-311,312,-312,313,-313,314,-314,315,-315,316,-316,317,
     $-317,318,-318,319,-319,320,-320,321,-321,322,-322,169,-169,170,
     $-170,171,-171,172,-172,173,-173,253,-253,254,-254,255,-255,256,
     $-256,303,-303,304,-304,305,-305,306,-306,307,-307,-7,7,5*0/
C
      ITABL(ID,NR,L) = IW(ID+LMHLEN+(NR-1)*IW(ID+1)+L)
C ------------------------------------------------------
C - Get NAPAR name-index of PART bank
      NAPAR = NAMIND ('PART')
C - Get number of columns in PART bank
      IDPAR = IW(NAPAR)
C
C - NOtrack marker word stored in KRUN bank
      KNOTRK = ITABL (IW(NAMIND('KRUN')),1,2)
C
C - Fill KLIN with LUGEN particle# for the GEANT particles
C   which are the 1st LLUGE particles of PART
C
      JKLIN = 0
      DO 1 IPART=1,LLUGE
         JKLIN = KBKLIN (IPART,ILUGE(IPART))
         IF (JKLIN .LE. 0) GOTO 998
 1    CONTINUE
C -  extend the KLIN bank
        DO 2 IPART = LLUGE+1,LLUAL+LLUGE
         JKLIN = KBKLIN (IPART,ILUAL(IPART-LLUGE))
         IF (JKLIN .LE. 0) GOTO 998
  2     CONTINUE
C
      CALL AUBPRS ('PARTKLIN')
C
      GOTO 999
C - not enough space
 998  CONTINUE
C - End
 999  CONTINUE
C
      END
