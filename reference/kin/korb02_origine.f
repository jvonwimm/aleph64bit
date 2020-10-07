C----------------------------------------------------------------      |
C   This program was received from F.FIDECARO in June 1992             |
C   and adapted to JETSET 7.3 format in december 1996                  |
C Changes were made by B.Bloch to allow KINGAL interface running       |
C  1) update values of JAKP and JAKM in routines DEKAY1 and DEKAY2     |
C  2) modify WRITE statement format to have corect comment on WIDTH    |
C     of Z0                                                            |
C  3) add protection in routine DPHNPI when using PAWT statement func  |
C  4) modify Random number used so that the program uses ALPEH's RANMAR|
C     implementation                                                   |
C  5) Modify masses values in INIMAS and rates in INITDK to reflects   |
C     the current situation as in KORALZ ( all rates were equal in the |
C     version received from the author                                 |
C  6) changes in LEFILL to account for JETSET7.3 conventions           | 
C-----------------------------------------------------------------------
c---
c only one correction: piret redefined as double precision, stj 22.apr.9
c routine SPIN disabled, renamed as SPIN1
c routine AMPLIT STARB  and routine KORAL degraded. routine ZNEW added.
c also EVENTB
c---
      SUBROUTINE KORALB(MMODE,KFB1,PPB1,EE1,KFB2,PPB2,EE2,XPAR,NPAR)
C     **************************************************************
C=======================================================================
C=======================================================================
C============================KORAL-B====================================
C=======================================================================
C=======================================================================
C
C     ****************************************************************
C     *          *************************************               *
C     *          ***           KORAL-B             ***               *
C     *          ***         VERSION 2.1           ***               *
C     *          ***         AUGUST 1990           ***               *
C     *          *************************************               *
C     *                                                              *
C     *                      AUTHORS                                 *
C     *           STANISLAW JADACH AND  ZBIGNIEW WAS                 *
C     *         JAGELLONIAN UNIVERSITY, KRAKOW, POLAND               *
C     *                                                              *
C     *     THE MONTE CARLO PROGRAM SIMULATING   THE   PROCESS       *
C     *         E+  E-   INTO   TAU+  TAU-  ( PHOTON )               *
C     *     IN QED TO ORDER  ALPHA**3  INCLUDING ALL EFFECTS         *
C     *     DUE TO SPIN AND FINITE MASS OF TAU. Z0 CONTRIBUTION      *
C     *     INCLUDED   IN   THE   LOW   ENERGY  APPROXIMATION.       *
C     ****************************************************************
C
C
C    IMPORTANT NOTE:
C    THIS PROGRAM WILL BE SLOWLY BUT GRADUALLY IMPROVED.
C    BEFORE USING THIS PROGRAM YOU MAY CONTACT Z. WAS
C    THROUGH EARNET/BITNET, WASM AT CERNVM,
C    AND, PERHAPS, YOU WILL GET  A BETTER VERSION.
C    ANY REMARK ON THE PROGRAM,
C    ON ANY NOTICED ERROR OR ANY OTHER PROBLEM IS WELCOMED.
C=================
C PARAMETERS:   =
C=================
C MODE=-1 INITIALIZATION OR REINITIALIZATION MODE,
C         PRIOR TO GENERATION, OBLIGATORY.
C     = 0 GENERATION MODE, M.C. EVENT IS GENERATED.
C     = 1 POSTGENERATION MODE, PRONTOUTS, OPTIONAL.
C
C IF MODE=-1 THEN ALL PARAMETERS ARE INPUT DATA:
C    =======
C KFB1 = 7,-7 FLAVOUR CODE OF FIRST BEAM, KF1=7  FOR ELECTRON.
C KFB2 = 7,-7 FLAVOUR CODE OF FIRST BEAM, KF2=-7 FOR POSITRON.
C PB1 = FOUR MOMENTUM OF THE FIRST BEAM.
C PB2 = FOUR MOMENTUM OF THE SECOND BEAM.
C EE1 = SPIN POLARIZATION VECTOR FOR THE FIRST BEAM.
C EE2 = SPIN POLARIZATION VECTOR FOR THE SECOND BEAM,
C       BOTH IN THE CORRESPONDING BEAM PARTICLE REST FRAME
C       AND IN BOTH CASES THIRD AXIS DIRECTED ALONG FIRST BEAM,
C       I.E. EE1(3) AND -EE2(3) ARE HELICITIES.
C       POLARIZATION MAY BE LONGITUDINAL TRANSVERSE AND ANY OTHER.
C
C OTHER INPUT PARAMETERS ARE HIDEN IN XPAR AND NPAR.
C NPAR(1)=ISPIN
C NPAR(2)=INRAN
C NPAR(3)=KEYGSW
C NPAR(4)=KEYRAD
C NPAR(5)=JAK1
C NPAR(6)=JAK2
C NPAR(7)=TYPE OF THE FINAL STATE FERMION  =(1,2,501,506) DEFINES
C         TAU,MU AND QUARKS.
C NPAR(8)=BREMSSTRAHLUNG IN DECAYS ON/OFF (1/0)
C XPAR( 1)=AMZ     =MASS OF Z0 BOSON
C XPAR( 4)=GV
C XPAR( 5)=AV
C XPAR( 6)=SINW2, ONLY FOR KEYGSW=1,
C XPAR( 8)=AMNUTA, MASS OF TAU NEUTRINO IN DECAY.
C XPAR(11)=XK0, THE INFRARED CUTOFF.
C XPAR(14)=AMFIN, FINAL STATE MASS (ONLY FOR ITFIN=501-506 NOT DUMMY)
C -------
C ISPIN =0,1  SPIN EFFECTS IN DECAY SWITCHED OFF,ON.
C INRAN = INITIALISATION CONSTANT FOR RAND. NUM. GEN. RNF100, POSITIVE.
C KEYGSW, IMPLEMENTATION LEVEL OF GLASHOW-SALAM-WEINBERG MODEL:
C = 0, N0 Z0, ONLY PHOTON EXCHANGE,
C = 1, PHOTON - Z0 INTERFERENCE
C KEYRAD=0, NO QED BREMSSTRAHLUNG,
C       =1, WITH QED BREMSSTRAHLUNG.
C JAK1,JAK2, DECAY TYPE FOR TAU+ AND TAU-.
C DECAY MODES INCLUDED ARE:
C JAK=1 ELECTRON DECAY
C JAK=2 MU  DECAY,
C JAK=3 PI DECAY,
C JAK=4 RHO DECAY,
C JAK=5 A1  DECAY
C JAK=6 K DECAY,
C JAK=7 K* DECAY,
C JAK=8 NPI DECAY
C JAK=0 INCLUSIVE:  JAK=1,2,3,4,5,6,7,8
C JAK=-1 NO DECAY.
C GV AND GA ARE COUPLING CONSTANTS OF W-BOSON OF TO TAU LEPTON,
C GV=1,GA=-1 REPRESENT THE STANDARD V-A COUPLING.
C XK0  IS THE INFRARED CUTOFF IN THE PROGRAM, IT IS A DUMMY
C PARAMETER AND NONE OF RESULTS SHOULD DEPEND ON IT,
C ITS VALUE MAY BE CHOOSEN IN THE RANGE 0.001 TO 0.05 ROUGHLY.
C
C ELSE IF MODE=0 THEN ALL PARAMETERS ARE IGNORED
C         ======
C ELSE IF MODE=1 THEN
C         ======
C NPAR(20)= NEVTOT, NO. OF GENERATED EVENTS,
C XPAR(20)= CSTCM,  INTEGRATED TOTAL CROSS SECTION IN CM**2 UNITS,
C ARE OUTPUT INFORMATION PROVIDED FOR THE USER.
C ENDIF
      IMPLICIT LOGICAL(A-H,O-Z)
      REAL*4 PPB1(4),EE1(3),PPB2(4),EE2(3),XPAR(40)
      INTEGER*4 NPAR(40)
C COMMONS COMMUNICATING WITH PRODUCTION PART
      COMMON / CONTRL/ SWT(6),ISPIN
      REAL*8           SWT
      REAL*8           CSTCMT,ERREL
C THESE COMMUNICATE WITH DECAY PACKAGE TAUOLA
      COMMON / BEAMS / XPB1(4),XPB2(4),KF1,KF2
      REAL*4           XPB1,   XPB2
      COMMON / IDFC  / IDFF
      COMMON / DECPAR / GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      REAL*4            GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      COMMON / JAKI   /  JAK1,JAK2,JAKP,JAKM,KTOM
      COMMON / INOUT / NINP,NOUT
      COMMON /KEYTR/ ITRANS
      common / znowy / amz,gamz
      REAL*8  PB1(4), E1(3), PB2(4), E2(3)
      REAL*8 SINW2,ENE,CSTCM,SUMPT,EM1,EM2,XK0
      REAL*8 AMZ,gamz,DUMMY,AMFIN
      REAL*8 HDUM(4)
      DATA IWARM/0/
      MODE =MMODE
      IF(MODE.EQ.-1) THEN
C     ===================
C     ===================
        IEVEN=0
        IF(IWARM.NE.0) GOTO 910
        NINP=5
        NOUT=6
C LUND IDFIER FOR A1 IS MISSING!  CONVENTION AT HOC !
        IWARM=IWARM+1
        KF1=KFB1
        KF2=KFB2
        ISPIN1=    NPAR(1)
        INRAN=    NPAR(2)
        KEYGSW=   NPAR(3)
        KEYRAD=   NPAR(4)
        JAK1  =   NPAR(5)
        JAK2  =   NPAR(6)
        ITFIN =   NPAR(7)
        ITDKRC=   NPAR(8)
        ITRANS=   NPAR(9)
        AMZ   = DBLE(XPAR(1))
        gamz  = dble(xpar(2))
        SINW2 = DBLE(XPAR(6))
C W-BOSON COUPLINGS TO FINAL STATE FERMION (IN DECAY) FROM INPUT
        GV=XPAR( 4)
        GA=XPAR( 5)
C TAU-NEUTRINO MASS
        AMNUTA= XPAR(8)
C INFRARED CUT-OFF
        XK0= DBLE(XPAR(11))
        IF(XK0.LT.1D-3.OR.XK0.GT.1D-1) GOTO 908
C FINAL STATE QUAR MASS
        IF(ITFIN.GT.100)  AMFIN=XPAR(14)
        DO 15 I=1,4
        XPB1(I)=PPB1(I)
        XPB2(I)=PPB2(I)
        PB1(I)=PPB1(I)
   15   PB2(I)=PPB2(I)
        ENE=PB1(4)
        DO 26 I=1,3
        E1(I)=EE1(I)
   26   E2(I)=EE2(I)
C PRINT INPUT PARAMETERS
        WRITE(NOUT,7000) KF1,KF2,PB1(3),PB1(4),PB2(3),PB2(4)
     &  ,E1(1),E1(2),E1(3),E2(1),E2(2),E2(3)
        WRITE(NOUT,7001)
     $  ITFIN,ISPIN1,INRAN,KEYGSW,KEYRAD,JAK1,JAK2,ITDKRC,ITRANS
        WRITE(NOUT,7002) AMZ,gamz,SINW2,GV,GA,AMNUTA,XK0
C CHECKS
        IF(IABS(KF1).NE.7.OR.IABS(KF2).NE.7) GOTO 900
        IF(KF1*KF2.GT.0) GOTO 900
        IF(DABS(SINW2).GT.1D0) GOTO 905
        IF(KEYRAD.LT.0.OR.KEYRAD.GT.0) GOTO 907
        SUMPT=0.
        DO 20 I=1,2
        SUMPT=SUMPT+PB1(I)
  20    SUMPT=SUMPT+PB2(I)
        IF(SUMPT.GT.0.0001) GOTO 902
        IF (DABS(PB1(3)).NE.DABS(PB1(4))
     &  .OR.DABS(PB2(3)).NE.DABS(PB2(4))
     &  .OR.PB1(4).NE.PB2(4).OR.PB1(3).NE.(-PB2(3)) ) GOTO 903
        EM1=DSQRT(E1(1)**2+E1(2)**2+E1(3)**2)
        EM2=DSQRT(E2(1)**2+E2(2)**2+E2(3)**2)
        IF(EM1.GT.1D0.OR.EM2.GT.1D0) GOTO 904
C INITIALISATION OF TAU DECAY PACKAGE TAUOLA
C ******************************************
        CALL INIMAS
        CALL INITDK
C DETERMINE WHETHER BEAM ALONG Z-AXIS IS AN ELECTR. OR POSITRON
C FOR IDE<0 PB1 (ALONG Z-AXIS) REPRESENTS POSITRON
        IDE= 2*KFB1/IABS(KFB1)
C
C       DETERMINE WHETHER QP REPRESENT PARTICLE OR  ANTIPART.
C                                      (IDF<0)  OR   (IDF>0
C
C       FINAL FERMION  TYPE ETC.
        IF(ITFIN .EQ. 1) THEN
C TAU CASE
C TAU MASS
        AMFIN=AMTAU
        AMTAU=AMFIN
          IDF= 2*KFB1/IABS(KFB1)
          IDFF = 11*IDF/IABS(IDF)
        ELSEIF(ITFIN .EQ. 2) THEN
C MUON CASE
        AMFIN=AMMU
          IDF= 2*KFB1/IABS(KFB1)
          IDFF =  9*IDF/IABS(IDF)
C DECAY SUPRESSED
          JAK1=-1
          JAK2=-1
        ELSE
C QUARK CASE, MASS FROM THE INPUT (AMFIN WAS OVERWRITTEN IN
C                                  INITIALIZATION OF TAUOLA)
          AMFIN=XPAR(14)
          JAK1=-1
          JAK2=-1
          IAA  = ABS(ITFIN)-500
          IF (IAA .EQ. 1 .OR. IAA .EQ. 4 .OR. IAA .EQ. 6) THEN
            IDF= 3*KFB1/IABS(KFB1)
          ELSE
            IDF= 4*KFB1/IABS(KFB1)
          END IF
          IDFF = SIGN(ITFIN,IDF)
        ENDIF
 
        WRITE(NOUT,7003) AMFIN
C SWITCHING OFF Z0
        IF(KEYGSW.EQ.0) SINW2=-.5
C SWITCHING OFF BREMSSTRAHLUNG
        IF(KEYRAD.EQ.0) ENE=-ABS(ENE)
C PRODUCTION PART INITIALIZATION
        CALL STARB(ENE,AMFIN,IDE,IDF,AMZ,SINW2,INRAN,XK0)
C       -------------------------------------------------
C ISPIN REDEFINED
        ISPIN=   ISPIN1
        IF(ISPIN.LT.0.OR.ISPIN.GT.1) GOTO 906
        CALL DEKAY(-1,HDUM)
      ELSEIF(MODE.EQ.0) THEN
C     ======================
C     ======================
        IF(IWARM.EQ.0) GOTO 911
        IEVEN=IEVEN+1
        CALL KORAL(E1,E2)
C       -----------------
        CALL KINCOP
C FILLING LUND RECORD WITH BEAMS FERMIONS AND PHOTON
C TAU CASE
        CALL TLUREB
C       -----------
C TAU DECAYS, REST OF ADMINISTRATION
        KTO=11
        CALL DEKAY(KTO,HDUM)
C       --------------------
        KTO=12
        CALL DEKAY(KTO,HDUM)
C       --------------------
      ELSEIF(MODE.EQ.1) THEN
C     ======================
C     ======================
        CALL FINISB(CSTCMT,ERREL)
C       ------------------
C CALCULATE PARTIAL DECAY WIDTHS
        CALL DEKAY(100,HDUM)
C       --------------------
        NPAR(10)=IEVEN
        XPAR(10)=CSTCMT
        WRITE(NOUT,7010) IEVEN,CSTCMT,ERREL
      ELSE
C     ====
        GOTO 901
      ENDIF
C     =====
      RETURN
  900 PRINT 9900
 9900 FORMAT(' KORALB: NONSENSE VALUE OF BEAM IDENTIFIER')
      STOP
  901 PRINT 9901
 9901 FORMAT(' KORALB: NONSENCE VALUE OF MODE ')
      STOP
  902 PRINT 9902
 9902 FORMAT(' KORALB: NO TRANSV. MOM. ALLOWED FOR BEAMS ')
      STOP
  903 PRINT 9903
 9903 FORMAT(' KORALB: SOME WRONG BEAM MOM. COMPONENT    ')
      STOP
  904 PRINT 9904
 9904 FORMAT(' KORALB: BAD POLARISATION VECTORS ')
      STOP
  905 PRINT 9905
 9905 FORMAT(' KORALB: BAD SINW2 ')
      STOP
  906 PRINT 9906
 9906 FORMAT(' KORALB: BAD ISPIN ')
      STOP
  907 PRINT 9907
 9907 FORMAT(' KORALB: BAD KEYRAD')
      STOP
  908 PRINT 9908 ,XK0
 9908 FORMAT(' KORALB: BAD XK0   ',E12.4)
      STOP
  910 PRINT 9910
 9910 FORMAT(' KORALB: REINITIALISATION NOT ALLOWED')
      STOP
  911 PRINT 9911
 9911 FORMAT(' KORALB: LACK OF INITIALISATION')
      STOP
 7000 FORMAT(//1H1,15(5H*****)
     $ /,' *',     25X,'=KORALB VERSION with Z.  INPUT PARAM ==',9X,1H*,
     $ /,' *',     25X,'==May 1991= KEYRAD=0 OBLIGATORY !! ====',9X,1H*,
     $ /,' *',I20  ,5X,'KF1    =  FIRST BEAM IDENTIFIER        ',9X,1H*,
     $ /,' *',I20  ,5X,'KF2    =  SECOND BEAM IDENTIFIER       ',9X,1H*,
     $ /,' *',     25X,'==== FOUR MOMENTA OF THE BEAMS ========',9X,1H*,
     $ /,' *',F20.9,5X,'PB1(3) =  FIRST BEAM, 3-RD COMPONENT   ',9X,1H*,
     $ /,' *',F20.9,5X,'PB1(4) =              0-TH COMPONENT   ',9X,1H*,
     $ /,' *',F20.9,5X,'PB2(3) = SECOND BEAM, 3-RD COMPONENT   ',9X,1H*,
     $ /,' *',F20.9,5X,'PB2(4) =              0-TH COMPONENT   ',9X,1H*,
     $ /,' *',     25X,'==== SPIN VECTORS OF THE BEAMS ========',9X,1H*,
     $ /,' *',F20.9,5X,'E1(1)  =  FIRST BEAM, 1-ST COMPONENT   ',9X,1H*,
     $ /,' *',F20.9,5X,'E1(2)  =              2-ND COMPONENT   ',9X,1H*,
     $ /,' *',F20.9,5X,'E1(3)  =              3-RD COMPONENT   ',9X,1H*,
     $ /,' *',F20.9,5X,'E2(1)  = SECOND BEAM  1-ST COMPONENT   ',9X,1H*,
     $ /,' *',F20.9,5X,'E2(2)  =              2-ND COMPONENT   ',9X,1H*,
     $ /,' *',F20.9,5X,'E2(3)  =              3-RD COMPONENT   ',9X,1H*)
 7001 FORMAT(
     $   ' *',     25X,'==== INPUT PARAMS IN NPAR =============',9X,1H*,
     $ /,' *',I20  ,5X,'ITFIN  =  TYPE OF THE FINAL STATE      ',9X,1H*,
     $ /,' *',I20  ,5X,'ISPIN  =  SPIN EFFECTS SWITCH          ',9X,1H*,
     $ /,' *',I20  ,5X,'INRAN  =  RANDOM NUMB. INITIALISATION  ',9X,1H*,
     $ /,' *',I20  ,5X,'KEYGSW =  GSW IMPLEMENTATION LEVEL     ',9X,1H*,
     $ /,' *',I20  ,5X,'KEYRAD =  BREMSSTRAHLUNG   SWITCH      ',9X,1H*,
     $ /,' *',I20  ,5X,'JAK1   =  DECAY TYPE TAU+              ',9X,1H*,
     $ /,' *',I20  ,5X,'JAK2   =  DECAY TYPE TAU-              ',9X,1H*,
     $ /,' *',I20  ,5X,'ITDKRC =  BREM. IN DECAY SWITCH        ',9X,1H*,
     $ /,' *',I20  ,5X,'ITRANS =  TRANSVERSE SPIN CORR. SWITCH ',9X,1H*)
 7002 FORMAT(
     $   ' *',     25X,'==== INPUT PARAMS IN XPAR =============',9X,1H*,
     $ /,' *',F20.9,5X,'AMZ   = MASS OF Z0 BOSON               ',9X,1H*,
     $ /,' *',F20.9,5X,'GAMZ  = WIDTH OF Z0 BOSON              ',9X,1H*,
     $ /,' *',F20.9,5X,'SINW2 =  SIN**2(THETAWEINBERG)         ',9X,1H*,
     $ /,' *',F20.9,5X,'GV    = VECTOR COUPLING CONST. IN DECAY',9X,1H*,
     $ /,' *',F20.9,5X,'GA    = AXIAL  COUPLING CONST. IN DECAY',9X,1H*,
     $ /,' *',F20.9,5X,'AMNUTA= MASS OF TAU-NEUTRINO  IN DECAY ',9X,1H*,
     $ /,' *',F20.9,5X,'XK0   = SOFT/HARD PHOTON CUT OFF       ',9X,1H*,
     $  /,1X,15(5H*****)/)
 7003 FORMAT(
     $   ' *',     25X,'==KORALB-INITIALIZATION THROUGH TAUOLA=',9X,1H*,
     $ /,' *',F20.9,5X,'AMFIN = MASS OF FINAL STATE FERMION    ',9X,1H*,
     $  /,1X,15(5H*****)/)
 7010 FORMAT(///1X,15(5H*****)
     $ /,' *',     25X,'==KORALB VERSION 2.1 FINAL REPORT======',9X,1H*,
     $ /,' *',     25X,'=========AUGUST  1990==================',9X,1H*,
     $ /,' *',I20  ,5X,'NUMBER OF THE GENERATED EVENTS         ',9X,1H*,
     $ /,' *',E20.5,5X,'TOTAL CROSS SECTION   CM**2            ',9X,1H*,
     $ /,' *',F20.9,5X,'RELATIVE ERROR                         ',9X,1H*,
     $  /,1X,15(5H*****)/)
      END
      SUBROUTINE KINCOP
C     *****************
C COPYING UTIL INTO UTIL4 AND UTIL8
      IMPLICIT LOGICAL(A-H,O-Z)
      COMMON / UTIL  /  QP(4), QM(4), PH(4)
      REAL*8            QP   , QM   , PH
      COMMON / UTIL8 / DQP(4),DQM(4),DPH(4)
      REAL*8           DQP   ,DQM   ,DPH
      COMMON / UTIL4 / AQP(4),AQM(4),APH(4)
      REAL*4           AQP   ,AQM   ,APH
      COMMON / ENERG / ENE,AEL2,AMF2,AMF,ALGEL,ALGMF,BETI,BT1,ATH2
      REAL*8           ENE,AEL2,AMF2,AMF,ALGEL,ALGMF,BETI,BT1,ATH2
      DO 30 I=1,4
      DQP(I)=QP(I)*ENE
      DQM(I)=QM(I)*ENE
      DPH(I)=PH(I)*ENE
      AQP(I)=QP(I)*ENE
      AQM(I)=QM(I)*ENE
 30   APH(I)=PH(I)*ENE
      RETURN
      END
      SUBROUTINE TRALO4(KTO,AP,BP,AM)
C     *******************************
C REAL*4 VERSION OF TRALOR
      REAL*4 AP(4),BP(4)
      REAL*8 DP(4)
      DO 30 I=1,4
  30  DP(I)=AP(I)
      CALL TRALOR(KTO,DP)
      DO 31 I=1,4
  31  AP(I)=DP(I)
      AM=AMAST(DP)
      RETURN
      END
      SUBROUTINE DUMPB4
C     ****************
C PRINTS FINAL STATE TAU AND PHOTON   MOMENTA
C CALLED OPTIONALY BY THE USER
      COMMON / UTIL4  / QP(4),QM(4),PH(4)
      COMMON / INOUT / NINP,NOUT
      DIMENSION SUM(4)
      DATA ICONT/0/
      ICONT=ICONT+1
      DO 30 I=1,4
  30  SUM(I)=QP(I)+QM(I)+PH(I)
      WRITE(NOUT,1100) ICONT
      AQP=     QP(4)**2-QP(3)**2-QP(2)**2-QP(1)**2
      AQM=     QM(4)**2-QM(3)**2-QM(2)**2-QM(1)**2
      APH=     PH(4)**2-PH(3)**2-PH(2)**2-PH(1)**2
      IF(AQP.GT.0.) AQP=SQRT(AQP)
      IF(AQM.GT.0.) AQM=SQRT(AQM)
      WRITE(NOUT,1501) (QP(I),I=1,4),AQP
      WRITE(NOUT,1502) (QM(I),I=1,4),AQM
      WRITE(NOUT,1503) (PH(I),I=1,4),APH
      WRITE(NOUT,1600) ( SUM(I),I=1,4)
 1100 FORMAT(///20X,'MOMENTA FROM KORAL,     EVENT NO.  ',I5/
     &40X,'  P(1)',7X,'  P(2)',7X,'  P(3)',7X,'  P(4)',7X,'  MASS')
 1501 FORMAT(20X,'QP  ',9X,5(1X,F12.5))
 1502 FORMAT(20X,'QM  ',9X,5(1X,F12.5))
 1503 FORMAT(20X,'PH  ',9X,5(1X,F12.5))
 1600 FORMAT(20X,'SUM ',9X,5(1X,F12.5),/)
      RETURN
      END
      SUBROUTINE KORAL(E1,E2)
C     ***********************
C=======================================================================
C==================COMPUT. PHYS. COMMUN. PART ==========================
C=======================================================================
C THIS IS PART OF KORALB ALMOST IDENTICAL TO THAT PUBLISHED
C IN COMPUTER PHYSICS COMMUNICATIONS 36, (1985) 191,
C BY S. JADACH AND Z. WAS
C CORRECTIONS WITH RESPECT TO CPC VERSION MARKED WITH C;; (SJ)
C CORRECTIONS WITH RESPECT TO CPC VERSION MARKED WITH *$  (ZW)
C     ****************************************************************
C     *  THIS IS CENTRAL MENAGER    ROUTINE FOR TAUPAIR PRODUCTION   *
C     *  PROCESS IT CALLS OTHER ROUTINES CALCULATES THE SPIN WEIGHT  *
C     *  WHICH IS NEXT USED TO DECIDE WHETHER EVENT IS ACCEPTED OR   *
C     *  REJECTED. THIS WEIGHING AND REJECTING PROCEDURE INTRODUCES  *
C     *  CORRELATION IN THE DECAYS OF TWO TAUS AND OTHER SPIN        *
C     *  EFFECTS. E1 AND E2 ARE POLARISATION VECTORS OF E+ AND E-    *
C     ****************************************************************
C
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / UTIL1 / XK,C1,S1,C2,S2,CF,SF,CG,SG,V
      COMMON / CONTRL/ SWT(6),ISPIN
      DIMENSION E1(3),E2(3),SA(4),SB(4),H1(4),H2(4)
      COMPLEX*16 T1(4,4),T2(4,4),S(4,4),E(4,4)
      COMPLEX*16 ZERO,ROT,TOT
      REAL*4 RRR
      LOGICAL LSOF,LHAR
      data init /0/
      ZERO=DCMPLX(0.D0,0.D0)
C-----
C-----POLARIZED ELECTRONS
      if (init.eq.0) then
       init=1
      XLSP=1.0D-20
      do 88 kkk=1,500
      CALL EVENTB
      CALL AMPLIT(T1,T2)
      KTO=1
      CALL DEKAY(KTO,H1)
      KTO=2
      CALL DEKAY(KTO,H2)
      H1(4)=1.
      H2(4)=1.
      CALL SPIN(E,H1,H2)
C-----ROTATION OF THE INITIAL POLARIZATION TO THE DYNAMIC COORDINATES
      SA(1)= CG*E1(1)+SG*E1(2)
      SA(2)=-SG*E1(1)+CG*E1(2)
      SA(3)=E1(3)
      SA(4)=1.
      SB(1)= CG*E2(1)+SG*E2(2)
      SB(2)=-SG*E2(1)+CG*E2(2)
      SB(3)=E2(3)
      SB(4)=1.
      CALL SPIN(S,SA,SB)
C---- CALCULATION OF THE SPIN WEIGHT FOR POLARIZED ELECTRONS
      LSOF= XK.EQ.0.
      LHAR= .NOT.LSOF
      ROT=ZERO
      TOT=ZERO
      DO 110 I=1,4
      DO 110 J=1,4
      IF(LSOF) TOT=TOT+DCONJG(T1(I,J))*T1(I,J)+DCONJG(T1(I,J))*T1(I,J)
      IF(LHAR) TOT=TOT+DCONJG(T1(I,J))*T1(I,J)+DCONJG(T2(I,J))*T2(I,J)
      DO 110 K=1,4
      DO 110 L=1,4
      IF(LSOF) ROT=ROT+
     +S(K,L)*(DCONJG(T2(K,I))*T2(L,J)+DCONJG(T2(K,I))*T2(L,J))*E(J,I)
      IF(LHAR) ROT=ROT+
     +S(K,L)*(DCONJG(T1(K,I))*T1(L,J)+DCONJG(T2(K,I))*T2(L,J))*E(J,I)
  110 CONTINUE
      WT=DREAL(ROT/TOT)
      XLSP=MAX(XLSP,1.2*WT)
 88   continue
      endif
C-----
C-----POLARIZED ELECTRONS
   50 CALL EVENTB
      CALL AMPLIT(T1,T2)
      KTO=1
      CALL DEKAY(KTO,H1)
      KTO=2
      CALL DEKAY(KTO,H2)
      H1(4)=1.
      H2(4)=1.
      CALL SPIN(E,H1,H2)
C-----ROTATION OF THE INITIAL POLARIZATION TO THE DYNAMIC COORDINATES
      SA(1)= CG*E1(1)+SG*E1(2)
      SA(2)=-SG*E1(1)+CG*E1(2)
      SA(3)=E1(3)
      SA(4)=1.
      SB(1)= CG*E2(1)+SG*E2(2)
      SB(2)=-SG*E2(1)+CG*E2(2)
      SB(3)=E2(3)
      SB(4)=1.
      CALL SPIN(S,SA,SB)
C---- CALCULATION OF THE SPIN WEIGHT FOR POLARIZED ELECTRONS
      LSOF= XK.EQ.0.
      LHAR= .NOT.LSOF
      ROT=ZERO
      TOT=ZERO
      DO 100 I=1,4
      DO 100 J=1,4
      IF(LSOF) TOT=TOT+DCONJG(T1(I,J))*T1(I,J)+DCONJG(T1(I,J))*T1(I,J)
      IF(LHAR) TOT=TOT+DCONJG(T1(I,J))*T1(I,J)+DCONJG(T2(I,J))*T2(I,J)
      DO 100 K=1,4
      DO 100 L=1,4
      IF(LSOF) ROT=ROT+
     +S(K,L)*(DCONJG(T2(K,I))*T2(L,J)+DCONJG(T2(K,I))*T2(L,J))*E(J,I)
      IF(LHAR) ROT=ROT+
     +S(K,L)*(DCONJG(T1(K,I))*T1(L,J)+DCONJG(T2(K,I))*T2(L,J))*E(J,I)
  100 CONTINUE
      WT=DREAL(ROT/TOT)
      SWT(4)=SWT(4)+1.
      SWT(5)=SWT(5)+WT
      SWT(6)=SWT(6)+WT*WT
      XLSP=MAX(XLSP,1.2*WT)
      CALL RANMAR(RRR,1)
      R=XLSP*RRR
      IF(R.GT.WT.AND.ISPIN.EQ.1) GOTO 50
      GOTO 400
C-----
C-----UNPOLARIZED ELECTRONS
  200 CONTINUE
      CALL EVENTB
      CALL AMPLIT(T1,T2)
  250 CONTINUE
      KTO=1
      CALL DEKAY(KTO,H1)
      KTO=2
      CALL DEKAY(KTO,H2)
      H1(4)=1.
      H2(4)=1.
      CALL SPIN(E,H1,H2)
C-----CALCULATE SPIN WEIGHT FOR UNPOLARIZED ELECTRONS
      LSOF= XK.EQ.0.
      LHAR= .NOT.LSOF
      ROT=ZERO
      TOT=ZERO
      DO 300 I=1,4
      DO 300 J=1,4
      IF(LSOF) TOT=TOT+DCONJG(T1(I,J))*T2(I,J)+DCONJG(T2(I,J))*T1(I,J)
      IF(LHAR) TOT=TOT+DCONJG(T1(I,J))*T1(I,J)+DCONJG(T2(I,J))*T2(I,J)
      DO 300 K=1,4
      IF(LSOF) ROT=ROT+
     +(DCONJG(T1(K,I))*T2(K,J)+DCONJG(T2(K,I))*T1(K,J))*E(J,I)
      IF(LHAR) ROT=ROT+
     +(DCONJG(T1(K,I))*T1(K,J)+DCONJG(T2(K,I))*T2(K,J))*E(J,I)
  300 CONTINUE
      WT=DREAL(ROT/TOT)
      SWT(4)=SWT(4)+1.
      SWT(5)=SWT(5)+WT
      SWT(6)=SWT(6)+WT*WT
      XLSP=2.
      CALL RANMAR(RRR,1)
      R=XLSP*RRR
      IF(R.GT.WT.AND.ISPIN.EQ.1) GOTO 250
  400 CONTINUE
C-----
      RETURN
      END
      SUBROUTINE AMPLIT(T1,T2)
C********************************************************************
C  IN THIS ROUTINE THE COMPLEX SPIN AMPLITUDES FOR THE TAU          *
C  PRODUCTION PROCESS ARE CALCULATED                                *
C********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)
C
      COMMON / CONST / PI,ALFA,ALF1,QE2,QF2,QEF
      COMMON / WEAK  / QCE,QCF,CVE,CVF,CAE,CAF,ZPROP
      COMMON / UTIL1 / XK,C1,S1,C2,S2,CF,SF,CG,SG,V
      COMMON / BOXY  / Z1,Z2,Z3
      COMMON / ENERG / ENE,AEL2,AMF2,AMF,ALGEL,ALGMF,BETI,BT1,ATH2
      COMMON / SIGMA / SIG0,SIG0CM,SOFT,SOF1,F2RE,BRM,BOX
      COMPLEX*16 T1(4,4),T2(4,4),ZERO,FAC,TCI(4,4),TCF(4,4)
      COMPLEX*16 ONEC,IMAG,F2,ZH,Z1,Z2,Z3,ZFAC
      ZERO=DCMPLX(0.D0,0.D0)
      IMAG=DCMPLX(0.D0,1.D0)
      ONEC=DCMPLX(1.D0,0.D0)
C
      IF(XK.GT.0.) GOTO 300
C  ********************************************
C  *                                          *
C  *       SOFT PHOTON CASE                   *
C  *                                          *
C  ********************************************
      C=C1
      S=S1
      DO 10 I=1,4
      DO 10 IJ=1,4
 10   T1(I,IJ)=ZERO
C-----BORN AMPLITUDE
      T1(1,1)=       ONEC
      T1(2,2)=     C*ONEC
      T1(2,4)= AMF*S*IMAG
C     TERMS PROPORTIONAL TO BORN
      A=ALF1*QEF
      FAC=(1.+SOF1+A*BRM)*ONEC
      DO 20 I=1,4
      DO 20 J=1,4
 20   T2(I,J)=T1(I,J)
      CALL ZNEW(ZFAC,ENE)
      ZFAC=ZFAC/QCE/QCF
      T2(1,1)=T2(1,1)+ZFAC*(CVE*CVF+C*CAE*CAF)*ONEC
      T2(1,2)=T2(1,2)+ZFAC*(CVE*CAF+C*CAE*CVF)*ONEC
      T2(2,1)=T2(2,1)+ZFAC*(CAE*CVF+C*CVE*CAF)*ONEC
      T2(2,2)=T2(2,2)+ZFAC*(CAE*CAF+C*CVE*CVF)*ONEC
      RETURN
  300 CONTINUE
C  *******************************************
C  *                                         *
C  *       HARD PHOTON CASE                  *
C  *                                         *
C  *******************************************
      Y=SQRT(1.-XK)
      BB=.5*XK/Y
      GB=(1.-.5*XK)/Y
      AM=AMF/Y
      HINI= QCE/Y/BB/(AEL2+S1**2)
      HFIN= QCF/BB/(AM**2+V**2*S2**2)
C-----
      TCI(1,1)= (-BB*SF                              )*HINI*ONEC
      TCI(1,2)= ( BB*C2*CF                           )*HINI*IMAG
      TCI(1,3)=                                             ZERO
      TCI(1,4)= ( AM*BB*S2*CF                        )*HINI*ONEC
      TCI(2,1)= ( BB*C1*CF                           )*HINI*IMAG
      TCI(2,2)= (-BB*C1*C2*SF                        )*HINI*ONEC
      TCI(2,3)=                                             ZERO
      TCI(2,4)= ( AM*BB*C1*S2*SF                     )*HINI*IMAG
      TCI(3,1)= (-GB*CF                              )*HINI*ONEC
      TCI(3,2)= (-GB*C2*SF                           )*HINI*IMAG
      TCI(3,3)=                                             ZERO
      TCI(3,4)= (-AM*GB*S2*SF                        )*HINI*ONEC
      TCI(4,1)= (-GB*C1*SF                           )*HINI*IMAG
      TCI(4,2)= (-GB*C1*C2*CF+S1*S2                  )*HINI*ONEC
      TCI(4,3)=                                             ZERO
      TCI(4,4)= ( AM*(GB*C1*S2*CF+S1*C2)             )*HINI*IMAG
C-----
      TCF(1,1)= (-BB*V*S2*SF                         )*HFIN*ONEC
      TCF(1,2)= ( BB*V*S2*C2*CF                      )*HFIN*IMAG
      TCF(1,3)= ( AM*BB*SF                           )*HFIN*IMAG
      TCF(1,4)= (-AM*BB*V*C2*C2*CF                   )*HFIN*ONEC
      TCF(2,1)= ( BB*V*S2*C1*CF                      )*HFIN*IMAG
      TCF(2,2)= (-BB*V*S2*C2*C1*SF                   )*HFIN*ONEC
      TCF(2,3)= ( AM*BB*C1*CF                        )*HFIN*ONEC
      TCF(2,4)= (-AM*BB*V*C2*C2*C1*SF                )*HFIN*IMAG
      TCF(3,1)= (-GB*V*S2*CF                         )*HFIN*ONEC
      TCF(3,2)= (-GB*V*S2*C2*SF                      )*HFIN*IMAG
      TCF(3,3)= ( AM*BB*CF                           )*HFIN*IMAG
      TCF(3,4)= (-AM*V*(GB*S2*S2-BB)*SF              )*HFIN*ONEC
      TCF(4,1)= (-GB*V*S2*C1*SF                      )*HFIN*IMAG
      TCF(4,2)= (-V*S2*(GB*C1*C2*CF-S1*S2)           )*HFIN*ONEC
      TCF(4,3)= (-AM*BB*C1*SF                        )*HFIN*ONEC
      TCF(4,4)= ( AM*V*((GB*S2*S2-BB)*C1*CF+S2*C2*S1))*HFIN*IMAG
      DO 140 K=1,4
      DO 140 I=1,2
      ZH=TCF(I,K)
      TCF(I  ,K)=    ZH*CF-TCF(I+2,K)*SF
  140 TCF(I+2,K)=    ZH*SF+TCF(I+2,K)*CF
 
C-----
*$ CORRECTION 16 11 87 Z.W.
      SGX=C1*SQRT(AEL2)
*$ CORRECTION 16 11 87 Z.W.
      SGX1= SGX*BB/GB
      DO 150 K=1,4
      T1(1,K)= S1*TCI(1,K)+TCF(1,K)
      T1(2,K)=(S1*TCI(2,K)+TCF(2,K))*IMAG
*$ CORRECTION 16 11 87 Z.W.
      T1(3,K)=-SGX*TCI(1,K)*IMAG*C1
      T1(4,K)=-SGX*TCI(2,K)
C-----
      T2(1,K)= S1*TCI(3,K)+TCF(3,K)
      T2(2,K)=(S1*TCI(4,K)+TCF(4,K))*IMAG
*$ CORRECTION 16 11 87 Z.W.
      T2(3,K)=-SGX1*TCI(3,K)*IMAG*C1
 150  T2(4,K)=-SGX1*TCI(4,K)
      RETURN
      END
      SUBROUTINE ZNEW(ZPROP,ENE)
      IMPLICIT REAL*8 (A-H,O-Z)
      common /znowy/ amz,gamz
       COMPLEX*16 ZPROP
       S   =4D0*ENE**2
       ZPROP=S/DCMPLX(S-AMZ**2,GAMZ*S/AMZ)
      END
      SUBROUTINE SPIN1(V,E,F)
C     **********************
      IMPLICIT REAL*8(A-H,O-Z)
      COMPLEX*16 V(4,4)
      DIMENSION E(4),F(4)
      DATA ZER/0./
C
      V(1,1)=DCMPLX( E(4)*F(4) -E(1)*F(1) +E(2)*F(2) +E(3)*F(3),  ZER)
      V(2,2)=DCMPLX( E(4)*F(4) +E(1)*F(1) -E(2)*F(2) +E(3)*F(3),  ZER)
      V(3,3)=DCMPLX( E(4)*F(4) -E(1)*F(1) -E(2)*F(2) -E(3)*F(3),  ZER)
      V(4,4)=DCMPLX( E(4)*F(4) +E(1)*F(1) +E(2)*F(2) -E(3)*F(3),  ZER)
      V(1,2)=DCMPLX( E(3)*F(4) +E(4)*F(3), E(1)*F(2) +E(2)*F(1))
      V(3,4)=DCMPLX( E(3)*F(4) -E(4)*F(3),-E(1)*F(2) +E(2)*F(1))
      V(1,3)=DCMPLX( E(4)*F(1) -E(1)*F(4),-E(2)*F(3) +E(3)*F(2))
      V(2,4)=DCMPLX( E(4)*F(1) +E(1)*F(4), E(2)*F(3) +E(3)*F(2))
      V(1,4)=DCMPLX( E(3)*F(1) +E(1)*F(3), E(4)*F(2) +E(2)*F(4))
      V(2,3)=DCMPLX( E(3)*F(1) -E(1)*F(3), E(4)*F(2) -E(2)*F(4))
      V(2,1)=DCONJG(V(1,2))
      V(4,3)=DCONJG(V(3,4))
      V(3,1)=DCONJG(V(1,3))
      V(4,1)=DCONJG(V(1,4))
      V(4,2)=DCONJG(V(2,4))
      V(3,2)=DCONJG(V(2,3))
      RETURN
      END
      SUBROUTINE STARB(ENE1,AMF1,IDE1,IDF1,AMZ1,SINW1,INRAN,XK1)
C     **********************************************************
C     ****************************************************************
C     *                                                              *
C     *  START IS ROUTINE SETING INITIAL VALUES TO CERTAIN VARIABLES *
C     *  AND PRINTING SOME USEFULL OUTPUT                            *
C     *  MUST BE CALLED BY USER BEFORE FIRST EVENT IS GENERATED      *
C     *  ENE=ABS(ENE1) IS BEAM ENERY IN GEV UNITS, NEGATIVE VALUE    *
C     *  CAUSES SWITCHING OFF RADIATIVE CORRECTIONS. SINW2 IN THE    *
C     *  SQUARE OF THE WEINBERG ANGLE, SWITCHING OFF Z0 CONTRIBUTION *
C     *  MAY BE ACHIEVED BY SETING SINW2=-0.5   .                    *
C     *  INRAN -INITIALISATION CONSTANT FOR RANDOM NUMBER GENERATOR  *
C     *  ANY POSITIVE INTEGER.                                       *
C     ****************************************************************
C
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / CONST / PI,ALFA,ALF1,QE2,QF2,QEF
      COMMON / UTIL1 / XK,C1,S1,C2,S2,CF,SF,CG,SG,V
      COMMON / UTIL2 / XK0,XKMIN,XKMAX
      COMMON / ENERG / ENE,AEL2,AMF2,AMF,ALGEL,ALGMF,BETI,BT1,ATH2
      COMMON / SIGMA / SIG0,SIG0CM,SOFT,SOF1,F2RE,BRM,BOX
      COMMON / WEAK  / QCE,QCF,CVE,CVF,CAE,CAF,ZPROP
      COMMON / INOUT / NINP,NOUT
      COMMON / CONTRL/ SWT(6),ISPIN
      COMMON / RNF   / INIRAN,NGEN
C     ****************************************************************
C     *  FIRST VERSION WRITTEN   20.04.1982, NO SPIN YET.            *
C     *  SECOND VERSION  20.03.1983,                                 *
C     *  SPIN CORRELATIONS IN THE DOUBLE DECAY INCLUDED.             *
C     *  THIRD VERSION 22.12.1983,                                   *
C     *  INITIAL SPIN INCLUDED.                                      *
C     ****************************************************************
C
      ENE=ABS(ENE1)
C-----PHOTON MINIMUM AND MAXIMUM ENERGY IN UNITS OF BEAM ENERGY ENE
      XKMIN=0.0
      XKMAX=1.0
C;;   XK0=0.01
      XK0=XK1
      INIRAN=INRAN
      PI=4.*ATAN(1.)
      ALFA=1./137.036
      ALF1=ALFA/PI
C-----SWITCHING OFF RADIATIVE CORRECTIONS
      IF(ENE1.LT.0.) ALF1=0.
      IF(ENE1.LT.0.) XKMAX=.9*XK0
      CMTR=389.385E-30
      DO 6 I=1,6
   6  SWT(I)=1.
C.....WARNING, THE ABOVE ASIGNMENTS IS NOT AN ERROR, IT IS FOR PURPOSE.
      ISPIN=1
      NGEN=9
      DUMMY=RNF100(-2)
      AEL=.51110034E-3/ENE
C     FINAL FERMION MASS
C;;   AMFIN=1.782
      AMFIN=AMF1
      AMF=AMFIN/ENE
      AEL2=AEL**2
      AMF2=AMF**2
      BT1=SQRT(1.-AMF)*SQRT(1.+AMF)
      SVAR=4.*ENE**2
      ALGEL= LOG(4./AEL2)
      ALGMF= LOG(4./AMF2)
C     ****************************************************************
C     *   FERMION CHARGES                                            *
C     *   QCE  INCOMING (BEAM)   FERMION CHARGE                      *
C     *   QCF  OUTGOING FERMION  ( HEAVY LEPTON )  CHARGE            *
C     *   SINW2  SIN OF WEIBERG ANGLE SQUARED                        *
C     *   ELECTRON COUPLING CONSTANTS  CVE , CAE                     *
C     *   AND TAU COUPLING CONSTANTS   CVF , CAF                     *
C     ****************************************************************
C     FOR QUARK PRODUCTION IDFIN SCHOULD BE CHANGED APROPRIATELY
C;;   IDFIN=-2
C;;   CALL SETWS(   -2,SINW2,QCE,CVE,CAE,AMZ,GAMM)
C;;   CALL SETWS(IDFIN,SINW2,QCF,CVF,CAF,AMZ,GAMM)
 
C;;;  WEAK COUPLING CONSTANTS
C     IDE IS THE INCOMING FERMION IDENTIFIER
C     IDF IS THE OUTGOING FERMION IDENTIFIER
C     IDF=2   MUON,TAU...
C     IDF=3   UP QUARK   ( U,C,T...)
C     IDF=4   DOWN QUARK  ( D,S,B...)
C     IDF NEGATIVE DENOTES ANTIPARTICLE
C     IDE=-2   INCOMING POSITRON
      IDE= IDE1
      IDF= IDF1
      SINW2=ABS(SINW1)
      CALL SETCUP(-IDE,SINW2,QCE,CVE,CAE)
      CALL SETCUP(-IDF,SINW2,QCF,CVF,CAF)
      AMZ=AMZ1
 
C-----SWITCHING OFF Z0 CONTRIBUTION FOR TESTS
      IF(SINW1.GT. 0D0) GOTO 15
      AMZ=10000.
      CAE=0.
      CVE=0.
   15 ZPROP=1./(1.-AMZ**2/ENE**2/4.)
      QE2=QCE**2
      QF2=QCF**2
      QEF=QCE*QCF
      COLR=1.
      IF(IABS(IDF).NE.2) COLR=3.
      SIG0=COLR*4.*QE2*QF2*PI*ALFA**2/3./SVAR
C------------------   SOFT PHOTON PARAMETERS
      ALK0= LOG(XK0)
      BETI=QE2*2.*ALF1*(ALGEL-1.)
      DSXI=BETI*ALK0+QE2*ALF1*(1.5*ALGEL-2.+PI*PI/3.)
      Y= LOG((1.+BT1)**2/AMF2)
      ATH2=QF2*Y
      BETF=QF2*2*ALF1*((1.+BT1**2)/(2.*BT1)*Y-1.)
      DSXF=BETF*(ALK0+.5*ALGMF)+QF2*ALF1*((3.+2.*BT1**2)/(2.*BT1)*Y
     $-2.-(1.+BT1**2)/(2.*BT1)*(Y**2+4.*DILOG(2.*BT1/(1.+BT1))-PI**2))
      F2RE=-QF2*ALF1*.25*AMF2/BT1*Y
      PIREL=ALF1*(5./9.-1./3.*ALGEL)
      PIRMU=ALF1*(5./9.-1./3.* LOG(SVAR/.10566**2))
      PIRTA=ALF1*(8./9.-BT1**2/3.-BT1/6.*(3.-BT1**2)*Y)
      PIRE=PIREL+PIRTA+PIRMU+PIRET(SVAR)
      IF(ALF1.EQ.0.) PIRE=0.
      ZCON=2.*ZPROP/QCE/QCF*CVE*CVF*0.0
      SOFT=DSXI+DSXF-2.*PIRE+ZCON
      SOF1=DSXI+DSXF-2.*PIRE
C
      IF(XKMIN.LT.XK0) XKMIN=0.
      XKMAX= MAX (XKMAX,XK0)
      XKMAX= MIN (XKMAX,(1.-AMF2))
C
      WRITE(NOUT,1000) ENE,SINW2,INIRAN
      WRITE(NOUT,1004) AMZ,CVE,CAE,CVF,CAF
C
      CSOFT=0.
      IF(XKMIN.EQ.0.) CSOFT=XKDEF(XK0)
      CSTOT=XKDEF(XKMAX)-XKDEF(XKMIN)
      WRITE(NOUT,1006) CSOFT,CSTOT
C
      SIG0CM=SIG0*CMTR
      WRITE(NOUT,1010)    SIG0CM
      IF(ALF1.EQ.0.) WRITE(NOUT,1017)
C
      RETURN
 1000 FORMAT(//,1H0,10(10H**********)//
     -  /20X,    40HSTART PARAMETERS ARE ...                ,
     -  /50X,    10HENERGY    ,F10.4,10H  GEV     ,
     -  /50X,    10HSINW2     ,F10.4,
     -  /50X,    10HINRAN     ,I10)
 1004 FORMAT(1H0,
     -  /20X,    40HMASS OF ELECTROWEAK BOSON Z0 IS  (GEV)  ,F10.4,
     -  /20X,    30HCOUPLIG CONSTANTS: CVE, CAE    ,2F10.4,
     -  /20X,    30H                   CVF, CAF    ,2F10.4)
 1006 FORMAT(1H0,
     -  /20X,    40HCONTRIBUTIONS TO SOFT CROSS SECT. ARE...,
     -  /20X,    40HBORN + SOFT PHOTON + VERTEX CORR. INIT. ,F10.4,
     -  /20X,    40HTOTAL CROSS SECTION - APPROXIMATE       ,F10.4)
 1010 FORMAT(1H ,
     -  /20X,    40HALL ABOVE CROSS SECTIONS ARE GIVEN IN   ,
     -  /20X,    40HUNITS OF THE LOWEST ORDER ( POINTLIKE ) ,
     -  /20X,    40HONE PHOTON CROSS-SECTION  SIG0 WHICH IS ,
     -  /20X,    10HEQUAL TO  ,E20.5,6H CM**2)
 1017 FORMAT(1H0,
     -  /20X,    40HNO RAD. CORRECTIONS, ONLY LOWEST ORDER    )
      END
      SUBROUTINE SETCUP(IDF,SIN2,QC,CV,CA)
C     ************************************
C SETCUP CALCULATES CV,CA WHICH CORRESPOND TO A GIVEN VALUE
C OF TEH WEINBERG ANGLE.
C NOTE THAT SIGN OF AXIAL COUPL CONST. MAY BE DIFFERENT FROM CHOICES IN
C THE LITERATURE. IT IS OPPOSITE TO THAT IN CERN BOOKLET.
C IDF IS QUARK OR LEPTON IDENTIFIER, QC ITS CHARGE  (S.J.)
      IMPLICIT REAL*8(A-H,O-Z)
      B=1./SQRT(16.*SIN2*(1.-SIN2))
      IDFERM=IABS(IDF)
      GO TO (1,2,3,4), IDFERM
C     NOT USED  ( NEUTRINO )
    1 QC=0.
      CV=B
      CA=B
      GO TO 500
C     ELECTRON, MUON ...
    2 QC=-1.
      CV=(-1.+4.*SIN2)*B
      CA=-B
      GO TO 500
C     UP QUARKS
    3 QC=2./3.
      CV=(1.-8./3.*SIN2)*B
      CA=B
      GO TO 500
C     DOWN QUARKS
    4 QC=-1./3.
      CV=(-1.+4./3.*SIN2)*B
      CA=-B
  500 IF(IDF.GT.0) RETURN
C     ANTIPARTICLES
      QC=-QC
      CV=-CV
      CA=+CA
      RETURN
      END
      FUNCTION PIRET(S)
C     *****************
      IMPLICIT REAL*8(A-H,O-Z)
C-----IT RETURNS THE VALUE OF THE HADRONIC CONTRIBUTION TO VACUUM
C-----POLARISATION, USING PARAMETRISATION OF H. BURKHARD TASSO NOTE
C-----NO. 192, DECEMBER 1981.
      IF(S.LT.1.)
     $                PIRET=-1.3450E-9 -2.3020E-3* LOG(1.+4.091*S)
      IF(S.GT.1.0.AND.S.LT.64.)
     $                PIRET=-1.5120E-3 -2.8220E-3* LOG(1.+1.218*S)
      IF(S.GT.64.)
     $                PIRET=-1.1344E-3 -3.0680E-3* LOG(1.+0.99992*S)
      RETURN
      END
      SUBROUTINE EVENTB
C     ****************
      IMPLICIT REAL*8(A-H,O-Z)
C     ****************************************************************
C     *  EVENT IS CENTRAL  ROUTINE FOR SIMULATION OF THE SPINLESS    *
C     *  EVENTS, IT GENERATES ALL ANGULAR VARIABLES AND THE MONENTA  *
C     *  OF TAU+ TAU- AND GAMMA (IF PRESENT).                        *
C     ****************************************************************
      COMMON / CONST / PI,ALFA,ALF1,QE2,QF2,QEF
      COMMON / UTIL  / QP(4),QM(4),PH(4)
      COMMON / UTIL2 / XK0,XKMIN,XKMAX
      COMMON / UTIL1 / XK,C1,S1,C2,S2,CF,SF,CG,SG,V
      COMMON / WEAK  / QCE,QCF,CVE,CVF,CAE,CAF,ZPROP
      COMMON / ENERG / ENE,AEL2,AMF2,AMF,ALGEL,ALGMF,BETI,BT1,ATH2
      COMMON / CONTRL/ SWT(6),ISPIN
      COMMON / SIGMA / SIG0,SIG0CM,SOFT,SOF1,F2RE,BRM,BOX
      REAL*4 RRR
      FUND(C1,S1,C2,S2)=(1.-.5*XK*(1.-C1))**2+
     +( XKT*S1*S2*CF-(1.-.5*XK)*C1*C2-.5*XK*C2 )**2
C-----
    1 CONTINUE
      R=RNF100(1)
C-----RAW  XK-DISTRIBUTION ,  CHOICE BETWEEN SOFT AND HARD EMISSION
      XK=RRGEN(R)
      IF(XK.GT.XK0) GO TO 20
C-----****************************************************************
C-----SOFT PHOTON EMISSION, FIRST THE ANGULAR DISTRIBUTION
C-----WITH ONLY C-SYMETRIC COMPONENT
   10 FIG=2.*PI*RNF100(4)
   11 XK=0.
      C1=1.-2.*RNF100(6)
      S1=SQRT((1.-C1)*(1.+C1))
      C2=1.
      S2=0.
      CF=1.
      SF=0.
      V=BT1
      WT=((2.-(S1*V)**2)*(1.+SOFT)+4.*F2RE)/(2.*(1.+SOFT))
      IF(RNF100(8).GT.WT) GO TO 11
C-----****************************************************************
C-----IMPOSING  C-ODD (INTERFERENCE,BOX ETC.) TERMS
      CALL BRMBOX
      IF(QEF.NE.0.) ZCONA=4.*ZPROP/QCE/QCF*CAE*CAF*0.
      IF(QEF.EQ.0.) ZCONA=0.
      WT=.5*( 1. + ( ALF1*QEF*((2.-(S1*V)**2)*BRM+BOX) +C1*V*ZCONA)
     $            /((2.-(S1*V)**2)*(1.+SOFT)+4.*F2RE)   )
      IF(RNF100(9).GT.WT) GO TO 10
      GO TO 400
C-----****************************************************************
C-----   HARD PHOTON BREMSSTRAHLUNG,
C.....1  MODELING XK-DISTRIBUTION
C.....2  CHOICE BETWEEN INITIAL AND FINAL BREMSSTRAHLUNG
C.....3  GENERATING ANGULAR VARIABLES
C------------------------
C.....1111111111111111111111111111111111111111111111111111111111111111
   20 CONTINUE
      XK1=1.-XK
      XKT=SQRT(XK1)
      V=SQRT(ABS(1.-AMF2/XK1))
      EPS1=AMF2/XK1/(1.+V)
      U=1.-.5*AEL2
      EPS=.5*AEL2
      ROIT=.5*BETI*(1.+XK1*XK1)/XK/XK1
      ROFT=1.5*QF2*ALF1*(1.+XK1*XK1-2.*AMF2)/XK*ATH2
      ROI=BETI*(1.+XK1**2)/XK/XK1*V*(3.-V**2)/4.
      ROF=QF2*ALF1/XK*( (1.+XK1*XK1-AMF2*XK-.50*AMF2**2)*
     $   LOG((1.+V)**2*XK1/AMF2) -((2.+AMF2)*XK1+XK**2)*V )
      WT=(ROI+ROF)/(ROIT+ROFT)
      SWT(1)=SWT(1)+1.
      SWT(2)=SWT(2)+WT
      SWT(3)=SWT(3)+WT*WT
      IF(RNF100(2).GT.WT) GO TO 1
C.....2222222222222222222222222222222222222222222222222222222222222222
   25 CONTINUE
      IF(RNF100(3)-ROI/(ROI+ROF)) 31,1,41
C-----
C.....3333333333333333333333333333333333333333333333333333333333333333
C-----INITIAL STATE RADIATION, ANGULAR VARIABLES
C-----
   31 FIG=2.*PI*RNF100(4)
      R=RNF100(5)
      DEL1=EPS*((2.-EPS)/EPS)**R
      DEL2=2.-DEL1
      CALL RANMAR(RRR,1)
      IF(RRR.GT.0.5) GO TO 35
      A=DEL1
      DEL1=DEL2
      DEL2=A
   35 C1=1.-DEL1
      S1=SQRT((DEL1-EPS)*(DEL2-EPS))
      ZET1=1.-V+2.*V*RNF100(6)
      ZET2=2.-ZET1
      C2=1.-ZET1
      S2=SQRT((ZET1-EPS1)*(ZET2-EPS1))
      FI=2.*PI*RNF100(7)
      CF=COS(FI)
      SF=SIN(FI)
      D1=2.*FUND( C1, S1, C2, S2)+2.*AMF2/XK1*(1.-.5*XK*DEL1)**2
      D2=2.*FUND(-C1,-S1, C2, S2)+2.*AMF2/XK1*(1.-.5*XK*DEL2)**2
      WT=(D1*(1.-AEL2/XK1/DEL2)+D2*(1.-AEL2/XK1/DEL1) )
     $    /(4.*(1.+XK1*XK1))
      IF(RNF100(8).GT.WT) GO TO 31
      GO TO 200
C.....2222222222222222222222222222222222222222222222222222222222222222
C-----FINAL STATE RADIATION, ANGULAR VARIABLES
C-----
   41 FIG=2.*PI*RNF100(4)
      DEL1=EPS+2.*U*RNF100(5)
      DEL2=2.-DEL1
      C1=1.-DEL1
      S1=SQRT((DEL1-EPS)*(DEL2-EPS))
      R=RNF100(6)
      ZET1=EPS1*((2.-EPS1)/EPS1)**R
      ZET2=2.-ZET1
      CALL RANMAR(RRR,1)
      IF(RRR.GT.0.5) GO TO 45
      A=ZET1
      ZET1=ZET2
      ZET2=A
   45 C2=1.-ZET1
      S2=SQRT((ZET1-EPS1)*(ZET2-EPS1))
      FI=2.*PI*RNF100(7)
      CF=COS(FI)
      SF=SIN(FI)
      D1=2.*FUND(-C2,-S2,-C1,-S1)+2.*AMF2
      D2=2.*FUND( C2, S2,-C1,-S1)+2.*AMF2
      D3=AMF2*XK*XK*(1.+C1*C1)-8.*AMF2*XK
      WT=(D1*(1.-AMF2/ZET2)+D2*(1.-AMF2/ZET1)+D3)
     $   /(4.*(1.+XK1*XK1)-2.*AMF2)
      IF(RNF100(8).GT.WT) GO TO 41
C-----****************************************************************
C-----INTRODUCING INTERFRRENCE TERMS IN GENERATED ANGULAR DISTRIBU-
C-----TIONS FOR HARD BREMSSTRAHLUNG EVENTS.
  200 CONTINUE
      WT=.5*WINTH(DUM)
      IF(RNF100(9).GT.WT) GO TO 25
C-----UNTIL THIS POINT VARIABLES C2 AND S2 WERE MULTIPLIED BY V.
      C2=C2/V
      S2=S2/V
C-----
C-----CONSTRUCTION MOMENTA OUT OF ANGULAR VARIABLES USING TRALOR
  400 CONTINUE
      CG=COS(FIG)
      SG=SIN(FIG)
      DO 420 I=1,3
      QP(I)=0.
  420 QM(I)=0.
      QP(4)=AMF
      CALL TRALOR(1,QP)
      QM(4)=AMF
      CALL TRALOR(2,QM)
      DO 430 I=1,4
  430 PH(I)=-QP(I)-QM(I)
      PH(4)=2.+PH(4)
      IF(XK.EQ.0.) PH(4)=0.
C
      RETURN
      END
      SUBROUTINE BRMBOX
C     *****************
      IMPLICIT REAL*8(A-H,O-Z)
C     ****************************************************************
C     *                                                              *
C     *  CONTRIBUTIONS FROM THE SOFT BREMSSTRAHLUNG AD BOX DIAGR.    *
C     *  SPIN AMPLITUDES FOR FINITE PART OF BOX CONTIBUTIONS         *
C     ****************************************************************
      COMMON / CONST / PI,ALFA,ALF1,QE2,QF2,QEF
      COMMON / ENERG / ENE,AEL2,AMF2,AMF,ALGEL,ALGMF,BETI,BT1,ATH2
      COMMON / SIGMA / SIG0,SIG0CM,SOFT,SOF1,F2RE,BRM,BOX
      COMMON / UTIL1 / XK,C1,S1,C2,S2,CF,SF,CG,SG,V
      COMMON / UTIL2 / XK0,XKMIN,XKMAX
      COMMON / BOXY  / Z1,Z2,Z3
      COMPLEX*16 Z1,Z2,Z3,ZERO,AA,BB,FD,FQ,ALP,ALM
      EQUIVALENCE (ALGEL,AE),(ALGMF,AF)
      DATA ENIT/0./
C
      IF(ENIT.EQ.ENE) GO TO 10
      ENIT=ENE
      ALK0= LOG(XK0)
      ZERO=DCMPLX(0.D0,0.D0)
      ALP=DCMPLX(AE+AF,2.D0*PI)
      ALM=DCMPLX(AE-AF,0.D0)
      BTR=(1.+BT1)**2/AMF2
      FDRE=.5*(AE*AE+PI*PI/3.)
      FQRE=.5*( LOG(BTR)**2+PI*PI/3.+4.*DILOG(-1./BTR))/BT1
      FD=DCMPLX(FDRE,-PI*AE)
      FQ=DCMPLX(FQRE,-PI* LOG(BTR))
   10 CONTINUE
      BRM=0.
      Z1=ZERO
      Z2=ZERO
      Z3=ZERO
      V=BT1
      DO 20 I=1,2
      SGN=1.
      IF(I.NE.1) SGN=-1.
      C=SGN*C1
      S=SGN*S1
      TR=2.-2.*C*V-AMF2
      DL=.5*TR/S/S/V/V
      RO=(V-C)/TR
      X=(1.-C*V)/SQRT(TR)
      X2=X*X
      Y=.5*(SQRT(TR)+AMF2/(1.+V))
      BRM=BRM+SGN*(
     $   2.*(DILOG(-X/(1.-Y))-DILOG((1.-X)/(1.-Y))-DILOG((1.+X)/Y)
     $  +DILOG(X/Y))+ LOG(Y/(1.-Y))**2-DILOG(X2)+.5* LOG(X2)**2
     $   - LOG(X2)* LOG(1.-X2)   )
      A1=2.* LOG(.5*(1.-C*V))+AE+AF
      B1= LOG(.5*(1.-C*V))**2-2.*DILOG(.5*TR/(1.-C*V))
     $                                       -.5*AE*AE-.5*AF*AF
      AA=DCMPLX(A1,0.D0)
      BB=DCMPLX(B1,PI*A1)
      Z1=Z1+(        V*RO*AA    +(DL-1.)*BB            +DL*FD
     $       +(1.-V*C*DL)*FQ        -.5*ALP   +.5*AMF2/TR*ALM )*SGN
      Z2=Z2           +RO*AA       -C*DL*BB     +C*(1.-DL)*FD
     $        +(V*DL+C-V)*FQ      -.5/V*ALP +.5*AMF2/TR/V*ALM
      Z3=Z3         -S/TR*AA-.5*TR/S/V/V*BB     +S*(1.-DL)*FD
     $       +.5*C*TR/V/S*FQ                       +S/TR*ALM
   20 CONTINUE
      BRM=BRM+4.* LOG((1.-V*C1)/(1.+V*C1))*ALK0
      BOX=DREAL(Z1+C1*Z2+AMF2*S1*Z3)
      RETURN
      END
      SUBROUTINE TRALOR(KTO,VEC)
C     **************************
      IMPLICIT REAL*8(A-H,O-Z)
C
C     ******************************************************************
C     *   TRALOR TRANSFORMES FOUR-VECTOR VEC FROM TAU REST SYSTEM      *
C     *   TO LAB SYSTEM. RECOMMENDED TO USE  FOR DECAY PRODUCTS.       *
C     *   KTO=1,2 DENOTES TAU+ AND TAU- CORRESPONDINGLY                *
C     ******************************************************************
C
      COMMON / UTIL1 / XK,C1,S1,C2,S2,CF,SF,CG,SG,V
 
      COMMON / ENERG / ENE,AEL2,AMF2,AMF,ALGEL,ALGMF,BETI,BT1,ATH2
      DIMENSION VEC(4),VEC1(4),TL(4,4)
C
      Y=SQRT(1.-XK)
      A=Y/AMF
      B=SQRT((A-1.)*(A+1.))
      IF(KTO.EQ.2) B=-B
      BB=.5*XK/Y
      GB=(1.-.5*XK)/Y
C
      H1= C1*S2*CF+GB*S1*C2
      H2=-S1*S2*CF+GB*C1*C2
      TL(1,1)= CF
      TL(1,2)=-C2*SF
      TL(1,3)=-A*S2*SF
      TL(1,4)=-B*S2*SF
      TL(2,1)= C1*SF
      TL(2,2)= C1*C2*CF-GB*S1*S2
      TL(2,3)=-B*BB*S1+A*H1
      TL(2,4)=-A*BB*S1+B*H1
      TL(3,1)=-S1*SF
      TL(3,2)=-S1*C2*CF-GB*C1*S2
      TL(3,3)=-B*BB*C1+A*H2
      TL(3,4)=-A*BB*C1+B*H2
      TL(4,1)= 0.
      TL(4,2)= BB*S2
      TL(4,3)= B*GB-A*BB*C2
      TL(4,4)= A*GB-B*BB*C2
C
      DO 100 I=1,4
  100 VEC1(I)=VEC(I)
      DO 110 I=1,4
      SUM=0.
      DO 105 J=1,4
  105 SUM=SUM+TL(I,J)*VEC1(J)
  110 VEC(I)=SUM
C     ROTATION ARROUND THE BEAM
      A=VEC(1)
      VEC(1)=CG*A-SG*VEC(2)
      VEC(2)=SG*A+CG*VEC(2)
C
      RETURN
      END
      FUNCTION WINTH(DUM)
C     *******************
      IMPLICIT REAL*8(A-H,O-Z)
C     ****************************************************************
C     * HERE THE CONTRIBUTION FROM THE INTERFERENCE OF THE INITIAL   *
C     * AND FINAL STATE BREMSSTRAHLUNG IS CALCULATED FOR USE IN      *
C     * SUBROUTINE EVENT  ( HARD BREMSSTARHLUNG ONLY)                *
C     ****************************************************************
C
      COMMON / CONST / PI,ALFA,ALF1,QE2,QF2,QEF
      COMMON / UTIL1 / XK,C1,S1,C2,S2,CF,SF,CG,SG,V
      COMMON / UTIL2 / XK0,XKMIN,XKMAX
      COMMON / ENERG / ENE,AEL2,AMF2,AMF,ALGEL,ALGMF,BETI,BT1,ATH2
      EQUIVALENCE (AMF2,AM),(AEL2,AL)
C
      Z1=SQRT(1.-XK)*S1*S2*CF-(1.-.5*XK)*C1*C2
      Z2=1.-.5*XK
      T = Z1+Z2+.5*XK*( C1-C2)
      U1=-Z1+Z2+.5*XK*(-C1-C2)
      U =-Z1+Z2+.5*XK*( C1+C2)
      T1= Z1+Z2+.5*XK*(-C1+C2)
      TT=T*T
      UU=U*U
      TT1=T1*T1
      UU1=U1*U1
      SS=2.
      SP=2.*(1.-XK)
      X1=XK*(1.-C1)
      X2=XK*(1.+C1)
      IF(C1.GT.0.9) X1=XK*(AEL2+S1**2)/(1.+C1)
      IF(C1.LT.-.9) X2=XK*(AEL2+S1**2)/(1.-C1)
      Y1=XK*(1.-C2)
      Y2=XK*(1.+C2)
      AINI=(TT+UU+AM/SP*(T+U)**2)*(1.-AL/SP*X1/X2)
     $   +(TT1+UU1+AM/SP*(T1+U1)**2)*(1.-AL/SP*X2/X1)
      AINI=AINI*QE2/(SP*X1*X2)
      AFIN=(TT+UU1+AM*SS)*(1.-AM*(Y1+Y2)/Y2/SS)
     $    +(UU+TT1+AM*SS)*(1.-AM*(Y1+Y2)/Y1/SS)
     $    +AM*(X1*X1+X2*X2)/SS-4.*AM*(SS-SP)
      AFIN=AFIN*QF2/(SS*Y1*Y2)
      ANTR=(TT+TT1+UU+UU1+AM*SS+AM*SP)
     $     *(T*X2*Y2+T1*X1*Y1-U*X2*Y1-U1*X1*Y2)
     $      +AM*X1*X2*((SS-SP)*(T+T1-U-U1)-(X1-X2)*(Y1-Y2))
      ANTR=ANTR*QEF/(SS*SP*X1*X2*Y1*Y2)
      WINTH=1.+ANTR/(AINI+AFIN)
      RETURN
      END
      SUBROUTINE FINISB(CSTCMT,ERREL)
C     ************************
      IMPLICIT REAL*8(A-H,O-Z)
C**********************************************************************
C     FINISH CALCULATES TOTAL CROSS SECTION FOR GENERATED EVENTS      I
C     USING AVERAGE WEIGHTS. IT GIVES STATISTICAL ERRORS DEPENDING ON I
C     THE NUMBER OF THE GENERATED EVENTS.                             I
C     CSTOT IS TOTAL EXACT CROSS SECTION GIVEN IN  CM**2              I
C     TO BE USED TO RENORMALISE HISTOGRAMS IN  CM**2 UNITS            I
C**********************************************************************
      COMMON / SIGMA / SIG0,SIG0CM,SOFT,SOF1,F2RE,BRM,BOX
      COMMON / CONTRL/ SWT(6),ISPIN
      COMMON / UTIL2 / XK0,XKMIN,XKMAX
      COMMON / INOUT / NINP,NOUT
C;;
C
      AWTA=0.
      DWTA=0.
      IF(SWT(1).EQ.0.) GO TO 10
      AWTA=SWT(2)/SWT(1)
      DWTA=SQRT(SWT(3)/SWT(2)**2-1./SWT(1))*100.
   10 AWTB=SWT(5)/SWT(4)
      DWTB=SQRT(SWT(6)/SWT(5)**2-1./SWT(4))*100.
C-----
      CSOFT=0.
      IF(XKMIN.EQ.0.) CSOFT=XKDEF(XK0)
      CSTOT=XKDEF(XKMAX)-XKDEF(XKMIN)
      CSHAR=CSTOT-CSOFT
C-----
      CSTOT=(CSOFT+AWTA*CSHAR)*AWTB
      DWTT=AWTA*CSHAR/(CSOFT+AWTA*CSHAR)
      DWTT=DWTT*DWTA+DWTB
      CSTCM=CSTOT*SIG0CM
C;;
      ERREL=SQRT((DWTT*DWTA)**2+DWTB**2)/100.
      CSTCMT=CSTCM
 
      NEVA=INT(SWT(1)-1.)
      NEVB=INT(SWT(4)-1.)
C
      WRITE(NOUT,1000)       NEVA,AWTA,DWTA,
     $                       NEVB,AWTB,DWTB,
     $                           CSTOT,DWTT
C
 1000 FORMAT(1H0,10(10H**********),/,7H FINISB,//
     -  /21X, 20H NO. OF CRUDE EVENTS,20H      AVERAGE WEIGHT,
     -                                20H      PERCENT ERRORS,
     -  /11X, 20HHARD BREMSS. WEIGHT ,I10, F20.6,F10.2,
     -  /11X, 20HSPIN WEIGHT         ,I10, F20.6,F10.2,
     - //11X, 40HTOTAL EXACT CROSS SECT. IN SIG0 UNITS
     -                                   , F10.6,F10.2)
      RETURN
      END
      FUNCTION XKDEF(XK)
C     ******************
      IMPLICIT REAL*8(A-H,O-Z)
C
C     ****************************************************************
C     *                                                              *
C     *  DEFINES RAW CROSS SECTION FOR PHOT. MOMENTUM  .LT. XK       *
C     *  USED IN RRGEN TO GENERATE RAW XK DISTRIBUTION               *
C     *                                                              *
C     ****************************************************************
C
      COMMON / SIGMA / SIG0,SIG0CM,SOFT,SOF1,F2RE,BRM,BOX
      COMMON / CONST / PI,ALFA,ALF1,QE2,QF2,QEF
      COMMON / UTIL2 / XK0,XKMIN,XKMAX
      COMMON / ENERG / ENE,AEL2,AMF2,AMF,ALGEL,ALGMF,BETI,BT1,ATH2
      DATA ENIT /.0/
      FUNH(X)=BETI*( LOG(X)-.5*X-.5* LOG(1.-X))
     $   +3.*ALF1*ATH2*((1.-AMF2)* LOG(X)-X+.25*X*X)
C
      IF(ENIT.EQ.ENE) GO TO 10
      ENIT=ENE
      SOFSIG=BT1*((1.+.5*AMF2)*(1.+SOFT)+3.*F2RE)
      SUDIS0=SOFSIG-FUNH(XK0)
      SUDIST=SUDIS0+FUNH(1.-AMF2)
   10 CONTINUE
      IF(XK.GT.0.) GO TO 110
      XKDEF=0.
      RETURN
  110 IF(XK.GT.XK0) GO TO 120
      XKDEF=SOFSIG*XK/XK0
      RETURN
  120 IF(XK.GT.(1.-AMF2)) GO TO 140
      XKDEF=SUDIS0+FUNH(XK)
      RETURN
  140 XKDEF=SUDIST
      RETURN
      END
      FUNCTION RRGEN(R)
C     *****************
      IMPLICIT REAL*8(A-H,O-Z)
C
C     ****************************************************************
C     *                                                              *
C     *  RRGEN IS GENERAL ROUTINE GENERATING ARBITRARY ONE DIM.      *
C     *  DISTRIBUTION DEFINED THROUGH XKDEF FUNCTION.                *
C     *  IT MEMORIZES THE SCHAPE OF DISTRIBUTION IN XX-MATRIX STEP   *
C     *  BY STEP WITH BETTER AND BETTER PRECISSION.  MATRIX XX       *
C     *  IS UPDATED AT THE MOMENT WHEN CERTAIN NUMBER OF EVENTS      *
C     *  (THAT LISTET IN MATRIX LIST ) HAS BEEN GENERATED.           *
C     *                                                              *
C     ****************************************************************
C
      COMMON / UTIL2 / XK0,XKMIN,XKMAX
      DIMENSION  XX(1025),LIST(11)
      DATA LIST
     $/1,2,3,4,64,256,1024,4096,16384,65536,-1/
      DATA IEV,LEN /0,1024/
C
      IEV=IEV+1
      IF(R.LT.0.) IEV=1
      IF(IEV.NE.1) GO TO 10
      IP=1
      EPS =.05/FLOAT(LEN)
      EPS1=.00025
      ID=LEN
      IW=1
      XW=1.
      XX(1)=XKMIN
      XX(LEN+1)=XKMAX
      F0=XKDEF(XKMIN)
      DF=XKDEF(XKMAX)-F0
   10 CONTINUE
      IF(IEV.NE.LIST(IP))GO TO 300
      IP=IP+1
      DO 100 I=1,IW
      IA1=ID*(I-1)+1
      IA2=IA1+ID
      IA=(IA1+IA2)/2
C     ITERATION COME ON
      X1=XX(IA1)
      X2=XX(IA2)
      IF((X2-X1).LT.EPS1) GO TO 31
      Y1=XKDEF(X1)
      Y2=XKDEF(X2)
      Y=FLOAT(IA-1)/FLOAT(LEN)
      DO 30  ITER=1,30
      X=.5*(X1+X2)
      F=(XKDEF(X)-F0)/DF
      IF(Y-F) 20,31,21
   20 X2=X
      Y2=Y
      GO TO 23
   21 X1=X
      Y1=Y
   23 IF((Y2-Y1) .LT.EPS.AND.(X2-X1).LT.EPS1) GO TO 31
   30 CONTINUE
   31 XX(IA)=.5*(X1+X2)
C     END OF ITERATION
  100 CONTINUE
      ID=ID/2
      IW=IW*2
      XW=XW*2.
C
  300 CONTINUE
      I=INT(.5*XW*R)
      IA1=2*I*ID+1
      IA2=IA1+ID
      IA3=IA2+ID
      X1=XX(IA1)
      X2=XX(IA2)
      X3=XX(IA3)
      Y=R*XW-FLOAT(2*I)
      X=.5*(Y-1.)*(Y-2.)*X1-Y*(Y-2.)*X2+.5*Y*(Y-1)*X3
      X= MAX (X,X1)
      X= MIN (X,X3)
      RRGEN=X
      RETURN
      END
      FUNCTION DILOG(X)
C     *****************
      IMPLICIT REAL*8(A-H,O-Z)
CERN      C304      VERSION    29/07/71 DILOG        59                C
      Z=-1.64493406684822
      IF(X .LT.-1.0) GO TO 1
      IF(X .LE. 0.5) GO TO 2
      IF(X .EQ. 1.0) GO TO 3
      IF(X .LE. 2.0) GO TO 4
      Z=3.2898681336964
    1 T=1.0/X
      S=-0.5
      Z=Z-0.5* LOG(ABS(X))**2
      GO TO 5
    2 T=X
      S=0.5
      Z=0.
      GO TO 5
    3 DILOG=1.64493406684822
      RETURN
    4 T=1.0-X
      S=-0.5
      Z=1.64493406684822 - LOG(X)* LOG(ABS(T))
    5 Y=2.66666666666666 *T+0.66666666666666
      B=      0.00000 00000 00001
      A=Y*B  +0.00000 00000 00004
      B=Y*A-B+0.00000 00000 00011
      A=Y*B-A+0.00000 00000 00037
      B=Y*A-B+0.00000 00000 00121
      A=Y*B-A+0.00000 00000 00398
      B=Y*A-B+0.00000 00000 01312
      A=Y*B-A+0.00000 00000 04342
      B=Y*A-B+0.00000 00000 14437
      A=Y*B-A+0.00000 00000 48274
      B=Y*A-B+0.00000 00001 62421
      A=Y*B-A+0.00000 00005 50291
      B=Y*A-B+0.00000 00018 79117
      A=Y*B-A+0.00000 00064 74338
      B=Y*A-B+0.00000 00225 36705
      A=Y*B-A+0.00000 00793 87055
      B=Y*A-B+0.00000 02835 75385
      A=Y*B-A+0.00000 10299 04264
      B=Y*A-B+0.00000 38163 29463
      A=Y*B-A+0.00001 44963 00557
      B=Y*A-B+0.00005 68178 22718
      A=Y*B-A+0.00023 20021 96094
      B=Y*A-B+0.00100 16274 96164
      A=Y*B-A+0.00468 63619 59447
      B=Y*A-B+0.02487 93229 24228
      A=Y*B-A+0.16607 30329 27855
      A=Y*A-B+1.93506 43008 6996
      DILOG=S*T*(A-B)+Z
      RETURN
C=======================================================================
C===================END OF CPC PART ====================================
C=======================================================================
      END
      FUNCTION RNF100(IGEN)
C     *********************
      REAL*8 RNF100
      RNF100 = RNDM(DUM)
      RETURN
      END
      FUNCTION RDUMMY(IGEN)
C======================================================================
C======================================================================
C===================*** JMCLIB ***=====================================
C============= LIBRARY OF THE MONTE CARLO =============================
C================AND LORENTZ KINEMATICS================================
C==============THIS ROUTINES WILL BECOME===============================
C====================OBSOLETE !========================================
C==EG==ALL RANDOM GENERATOS SHOULD BE REPLACED BY RANMAR===============
C======================================================================
C             R N F 1 0 0
C THIS IS THE QUASIRANDOM SHUFFLING TYPE RANDOM NUMBER GENERATOR
C EQUIVALENT TO THAT IN FOWL CERN PROGRAM LIBRARY W-505.
C IT CONTAINS NGEN INDEPENDENT PARALLEL SUBGENERATORS INDEXED
C WITH THE PARAMETER IGEN ( GENERALLY NGEN.LE.100).
C RANDOM NUMBERS PASS THROUGH A BUFFER ( MATRIX BUFOR ) OF THE
C LENGHT NGEN*LENBUF IN ORDER TO KILL CORRELATIONS BETWEEN
C SUBGENERATORS. EVERY SUBGENNERATOR SHOULD SERVICE ONE INTEGRATION
C VARIABLE. INIRAN IS AN INITIALIZATION POSITIVE INTEGER PARAMETER
C COMMON TO ALL SUBGENERATORS.
C LENBUF SHOULD NOT BE LESS THEN NGEN.
C               ***  WARNING   ***
C THIS PARTICULAR VERSION IS LIMITED TO 20 SUBGENERATORS, FOR MORE
C ONE SHOULD EXTEND BUFOR AND LENBUF.
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /RNF/ INIRAN,NGEN
      DIMENSION BUFOR(800)
      DIMENSION KQCONS(100),QQCONS(100),SSER(100)
      DATA LENBUF/40/
      DATA KQCONS /
     *   2,   3,   5,   7,  11,  13,  17,  19,  23,  29,  31,
     *  37,  41,  43,  47,  53,  59,  61,  67,  71,  73,  79,
     *  83,  89,  97, 101, 103, 107, 109, 113, 127, 131, 137,
     * 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193,
     * 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257,
     * 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317,
     * 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389,
     * 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457,
     * 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541/
      DATA IWARM /0/
 
      IF(IGEN.GT.0) GO TO 300
C INITIALISATION OR REINITIALISATION
      IF(IGEN.NE.-2) GO TO 910
      IF(NGEN.LE.0.OR.NGEN.GT.20) GOTO 920
      IF(INIRAN.LE.0) GOTO 930
      DUMX=RNDMD(-1)
      IWARM=1
      XLEN=FLOAT(LENBUF)
      DO 30 IGN=1,NGEN
      DXX=KQCONS(IGN)
      QQCONS(IGN)=SQRT(DXX)
      NDEX=LENBUF*(IGN-1)
      SER=INIRAN
      DO 29 K=1,LENBUF
      INDEX=NDEX+K
      BUFOR(INDEX)=MOD((SER*QQCONS(IGN)),1.D0)
   29 SER=SER+1.
   30 SSER(IGN)=SER
      RETURN
 
C GENERATION OF THE RANDOM NUMBER
  300 CONTINUE
      IF(IGEN.GT.NGEN) GOTO 940
      IF(IWARM.EQ.0)   GOTO 950
      INDEX=LENBUF*(IGEN-1)+INT(XLEN*RNDMD(1))+1
C     RNF100=BUFOR(INDEX)
      RDUMMY=BUFOR(INDEX)
      SER=SSER(IGEN)
      BUFOR(INDEX)=MOD((SER*QQCONS(IGEN)),1.D0)
      SSER(IGEN)=SER+1.
      RETURN
 
 910  PRINT 9100
 9100 FORMAT('  RNF100: WRONG IGEN ')
      STOP
 920  PRINT 9200
 9200 FORMAT('  RNF100: NGEN<1 OR NGEN>20 ')
      STOP
 930  PRINT 9300
 9300 FORMAT('  RNF100: INIRAN.LE.0 ')
      STOP
 940  PRINT 9400
 9400 FORMAT('  RNF100: IGEN>NGEN ')
      STOP
 950  PRINT 9500
 9500 FORMAT('  RNF100: LACK OF INITIALISATION')
      STOP
      END
      FUNCTION RNDMD(IDUMM)
C     ********************
      REAL*8 RNDMD
      RNDMD = DBLE(RNDM(DUM))
      RETURN
      END
      FUNCTION RNDMDUM(IDUMM)
      REAL*8 RNDMDUM
      DATA IC,IY /65536,13/
C---REINITIALISATION
      IF(IDUMM.EQ.-1) THEN
         IC=65536
         IY=13
         RNDMD=0
         RETURN
      ENDIF
      IY=IY*25
      IY=MOD(IY,IC)
      IY=IY*125
      IY=MOD(IY,IC)
C     RNDMD=FLOAT(IY)/FLOAT(IC)
      RNDMDUM=FLOAT(IY)/FLOAT(IC)
      RETURN
      END
      SUBROUTINE DUMPZ4(JK,PP)
C     **********************
C PRINTS SINGLE FOUR MOMENTUM
      REAL*4 PP(4)
      COMMON / INOUT / NINP,NOUT
      AMS=PP(4)**2
      DO 10 I=1,3
   10 AMS=AMS-PP(I)**2
      IF(AMS.GT.0.0) AMS=SQRT(AMS)
      WRITE(NOUT,1000)
      WRITE(NOUT,1502) JK,(PP(I),I=1,4),AMS
 1000 FORMAT(1X,'* DUMPZ4 *-------',8(10H----------))
 1502 FORMAT(10X,I6,3X,'FOURMOMENTUM  ',5(1X,F12.5))
      RETURN
C======================================================================
C================END OF JMCLIB=========================================
C======================================================================
      END
*****************
c stj 22.feb.92
c modifications:
c LULIST kicked out completely
*****************
 
      SUBROUTINE JAKER(JAK)
C     *********************
C
C **********************************************************************
C                                                                      *
C           *********TAUOLA LIBRARY: VERSION 1.5 ********              *
C           ***************AUGUST 1990*******************              *
C           *****AUTHORS: S.JADACH, J.H.KUEHN, Z.WAS*****              *
C           ********AVAILABLE FROM: WASM AT CERNVM ******              *
C           *****TO BE PUBLISHED IN COMP. PHYS. COMM.****              *
C           **PREPRINT CERN-TH-5856 SEPTEMBER 1990 ******              *
C **********************************************************************
C ----------------------------------------------------------------------
c SUBROUTINE JAKER,
C CHOOSES DECAY MODE ACCORDING TO LIST OF BRANCHING RATIOS
C JAK=1 ELECTRON MODE
C JAK=2 MUON MODE
C JAK=3 PION MODE
C JAK=4 RHO  MODE
C JAK=5 A1   MODE
C JAK=6 K    MODE
C JAK=7 K*   MODE
C JAK=8 nPI  MODE
C
C     called by : DEXAY
C ----------------------------------------------------------------------
      COMMON / TAUBRA / GAMPRT(30),JLIST(30),NCHAN
      REAL   CUMUL(20)
C
      IF(NCHAN.LE.0.OR.NCHAN.GT.30) GOTO 902
      CALL RANMAR(RRR,1)
      SUM=0
      DO 20 I=1,NCHAN
      SUM=SUM+GAMPRT(I)
  20  CUMUL(I)=SUM
      DO 25 I=NCHAN,1,-1
      IF(RRR.LT.CUMUL(I)/CUMUL(NCHAN)) JI=I
  25  CONTINUE
      JAK=JLIST(JI)
      RETURN
 902  PRINT 9020
 9020 FORMAT(' ----- JAKER: WRONG NCHAN')
      STOP
      END
      SUBROUTINE DEKAY(KTO,HX)
C     ***********************
C THIS DEKAY IS IN SPIRIT OF THE 'DECAY' WHICH
C WAS INCLUDED IN KORAL-B PROGRAM, COMP. PHYS. COMMUN.
C VOL. 36 (1985) 191, SEE COMMENTS  ON GENERAL PHILOSOPHY THERE.
C KTO=0 INITIALISATION (OBLIGATORY)
C KTO=1,11 DENOTES TAU+ AND KTO=2,12 TAU-
C DEKAY(1,H) AND DEKAY(2,H) IS CALLED INTERNALLY BY MC GENERATOR.
C H DENOTES THE POLARIMETRIC VECTOR, USED BY THE HOST PROGRAM FOR
C CALCULATION OF THE SPIN WEIGHT.
C USER MAY OPTIONALLY CALL DEKAY(11,H) DEKAY(12,H) IN ORDER
C TO TRANSFORM DECAY PRODUCTS TO CMS AND WRITE LUND RECORD IN /LUJETS/.
C KTO=100, PRINT FINAL REPORT  (OPTIONAL).
C DECAY MODES:
C JAK=1 ELECTRON DECAY
C JAK=2 MU  DECAY
C JAK=3 PI  DECAY
C JAK=4 RHO DECAY
C JAK=5 A1  DECAY
C JAK=6 K   DECAY
C JAK=7 K*  DECAY
C JAK=8 NPI DECAY
C JAK=0 INCLUSIVE:  JAK=1,2,3,4,5,6,7,8
      REAL  H(4)
      REAL*8 HX(4)
      COMMON / JAKI   /  JAK1,JAK2,JAKP,JAKM,KTOM
      COMMON / IDFC  / IDF
      COMMON / TAUBMC / GAMPMC(30),GAMPER(30),NEVDEC(30)
      REAL*4            GAMPMC    ,GAMPER
      COMMON / INOUT / INUT,IOUT
      REAL  PDUM1(4),PDUM2(4),PDUM3(4),PDUM4(4),PDUM5(4),HDUM(4)
      REAL  PDUMX(4,6)
      DATA IWARM/0/
      KTOM=KTO
      IF(KTO.EQ.-1) THEN
C     ==================
C       INITIALISATION OR REINITIALISATION
        KTOM=1
        IF (IWARM.EQ.1) X=5/(IWARM-1)
        IWARM=1
        WRITE(IOUT,7001) JAK1,JAK2
        NEVTOT=0
        NEV1=0
        NEV2=0
        IF(JAK1.NE.-1.OR.JAK2.NE.-1) THEN
          CALL DADMEL(-1,IDUM,HDUM,PDUM1,PDUM2,PDUM3,PDUM4)
          CALL DADMMU(-1,IDUM,HDUM,PDUM1,PDUM2,PDUM3,PDUM4)
          CALL DADMPI(-1,IDUM,PDUM,PDUM1,PDUM2)
          CALL DADMRO(-1,IDUM,HDUM,PDUM1,PDUM2,PDUM3,PDUM4)
          CALL DADMAA(-1,IDUM,HDUM,PDUM1,PDUM2,PDUM3,PDUM4,PDUM5,JDUM)
          CALL DADMKK(-1,IDUM,PDUM,PDUM1,PDUM2)
          CALL DADMKS(-1,IDUM,HDUM,PDUM1,PDUM2,PDUM3,PDUM4,JDUM)
          CALL DADNPI(-1,IDUM,HDUM,PDUM1,PDUM2,PDUMX)
        ENDIF
      ELSEIF(KTO.EQ.1) THEN
C     =====================
C DECAY OF TAU+ IN THE TAU REST FRAME
        NEVTOT=NEVTOT+1
        IF(IWARM.EQ.0) GOTO 902
        ISGN= IDF/IABS(IDF)
        CALL DEKAY1(0,H,ISGN)
      ELSEIF(KTO.EQ.2) THEN
C     =================================
C DECAY OF TAU- IN THE TAU REST FRAME
        NEVTOT=NEVTOT+1
        IF(IWARM.EQ.0) GOTO 902
        ISGN=-IDF/IABS(IDF)
        CALL DEKAY2(0,H,ISGN)
      ELSEIF(KTO.EQ.11) THEN
C     ======================
C REST OF DECAY PROCEDURE FOR ACCEPTED TAU+ DECAY
        NEV1=NEV1+1
        ISGN= IDF/IABS(IDF)
        CALL DEKAY1(1,H,ISGN)
      ELSEIF(KTO.EQ.12) THEN
C     ======================
C REST OF DECAY PROCEDURE FOR ACCEPTED TAU- DECAY
        NEV2=NEV2+1
        ISGN=-IDF/IABS(IDF)
        CALL DEKAY2(1,H,ISGN)
      ELSEIF(KTO.EQ.100) THEN
C     =======================
        IF(JAK1.NE.-1.OR.JAK2.NE.-1) THEN
          CALL DADMEL( 1,IDUM,HDUM,PDUM1,PDUM2,PDUM3,PDUM4)
          CALL DADMMU( 1,IDUM,HDUM,PDUM1,PDUM2,PDUM3,PDUM4)
          CALL DADMPI( 1,IDUM,PDUM,PDUM1,PDUM2)
          CALL DADMRO( 1,IDUM,HDUM,PDUM1,PDUM2,PDUM3,PDUM4)
          CALL DADMAA( 1,IDUM,HDUM,PDUM1,PDUM2,PDUM3,PDUM4,PDUM5,JDUM)
          CALL DADMKK( 1,IDUM,PDUM,PDUM1,PDUM2)
          CALL DADMKS( 1,IDUM,HDUM,PDUM1,PDUM2,PDUM3,PDUM4,JDUM)
          CALL DADNPI( 1,IDUM,HDUM,PDUM1,PDUM2,PDUMX)
          WRITE(IOUT,7010) NEV1,NEV2,NEVTOT
          WRITE(IOUT,7011) (NEVDEC(I),GAMPMC(I),GAMPER(I),I=1,11)
        ENDIF
      ELSE
C     ====
        GOTO 910
      ENDIF
C     =====
        DO 78 K=1,4
 78     HX(K)=H(K)
      RETURN
 
 7001 FORMAT(///1X,15(5H*****)
     $ /,' *',     25X,'*****TAUOLA LIBRARY: VERSION 1.5 ******',9X,1H*,
     $ /,' *',     25X,'************AUGUST 1990****************',9X,1H*,
     $ /,' *',     25X,'**AUTHORS: S.JADACH, J.H.KUEHN, Z.WAS**',9X,1H*,
     $ /,' *',     25X,'**AVAILABLE FROM: WASM AT CERNVM ******',9X,1H*,
     $ /,' *',     25X,'**TO BE PUBLISHED IN COMP. PHYS. COMM.*',9X,1H*,
     $ /,' *',     25X,'**PREPRINT CERN-TH 5856 SEPT. 1990*****',9X,1H*,
     $ /,' *',     25X,'****DEKAY ROUTINE: INITIALIZATION******',9X,1H*,
     $ /,' *',I20  ,5X,'JAK1   = DECAY MODE TAU+               ',9X,1H*,
     $ /,' *',I20  ,5X,'JAK2   = DECAY MODE TAU-               ',9X,1H*,
     $  /,1X,15(5H*****)/)
 7010 FORMAT(///1X,15(5H*****)
     $ /,' *',     25X,'*****TAUOLA LIBRARY: VERSION 1.5 ******',9X,1H*,
     $ /,' *',     25X,'************AUGUST 1990****************',9X,1H*,
     $ /,' *',     25X,'**AUTHORS: S.JADACH, J.H.KUEHN, Z.WAS**',9X,1H*,
     $ /,' *',     25X,'**AVAILABLE FROM: WASM AT CERNVM ******',9X,1H*,
     $ /,' *',     25X,'**TO BE PUBLISHED IN COMP. PHYS. COMM.*',9X,1H*,
     $ /,' *',     25X,'**PREPRINT CERN-TH 5856 SEPT. 1990*****',9X,1H*,
     $ /,' *',     25X,'*****DEKAY ROUTINE: FINAL REPORT*******',9X,1H*,
     $ /,' *',I20  ,5X,'NEV1   = NO. OF TAU+ DECS. ACCEPTED    ',9X,1H*,
     $ /,' *',I20  ,5X,'NEV2   = NO. OF TAU- DECS. ACCEPTED    ',9X,1H*,
     $ /,' *',I20  ,5X,'NEVTOT = SUM                           ',9X,1H*,
     $ /,' *','    NOEVTS ',
     $   ' PART.WIDTH     ERROR       ROUTINE    DECAY MODE    ',9X,1H*)
 7011 FORMAT(1X,'*'
     $       ,I10,2F12.7       ,'     DADMEL     ELECTRON      ',9X,1H*
     $ /,' *',I10,2F12.7       ,'     DADMMU     MUON          ',9X,1H*
     $ /,' *',I10,2F12.7       ,'     DADMPI     PION          ',9X,1H*
     $ /,' *',I10,2F12.7,       '     DADMRO     RHO (->2PI)   ',9X,1H*
     $ /,' *',I10,2F12.7,       '     DADMAA     A1  (->3PI)   ',9X,1H*
     $ /,' *',I10,2F12.7,       '     DADMKK     KAON          ',9X,1H*
     $ /,' *',I10,2F12.7,       '     DADMKS     K*            ',9X,1H*
     $ /,' *',I10,2F12.7,       '     DADNPI     PI- 2PI+ PI0  ',9X,1H*
     $ /,' *',I10,2F12.7,       '     DADNPI     PI- 3PI0      ',9X,1H*
     $ /,' *',I10,2F12.7,       '     DADNPI     3PI- 2PI+     ',9X,1H*
     $ /,' *',I10,2F12.7,       '     DADNPI     3PI- 2PI+ PI0 ',9X,1H*
     $ /,' *',20X,'THE ERROR IS RELATIVE AND  PART.WIDTH      ',10X,1H*
     $ /,' *',20X,'IN UNITS GFERMI**2*MASS**5/192/PI**3       ',10X,1H*
     $  /,1X,15(5H*****)/)
 902  PRINT 9020
 9020 FORMAT(' ----- DEKAY: LACK OF INITIALISATION')
      STOP
 910  PRINT 9100
 9100 FORMAT(' ----- DEKAY: WRONG VALUE OF KTO ')
      STOP
      END
      SUBROUTINE DEKAY1(IMOD,HH,ISGN)
C     *******************************
C THIS ROUTINE  SIMULATES TAU+  DECAY
      COMMON / DECP4 / PP1(4),PP2(4),KF1,KF2
      COMMON / JAKI   /  JAK1,JAK2,JAKP,JAKM,KTOM
      COMMON / TAUBMC / GAMPMC(30),GAMPER(30),NEVDEC(30)
      REAL*4            GAMPMC    ,GAMPER
      REAL  HH(4)
      REAL  HV(4),PNU(4),PPI(4)
      REAL  PWB(4),PMU(4),PNM(4)
      REAL  PRHO(4),PIC(4),PIZ(4)
      REAL  PAA(4),PIM1(4),PIM2(4),PIPL(4)
      REAL  PKK(4),PKS(4)
      REAL  PNPI(4,6)
      REAL  PHOT(4)
      REAL  PDUM(4)
      DATA NEV,NPRIN/0,10/
      KTO=1
      IF(JAK1.EQ.-1) RETURN
      IMD=IMOD
      IF(IMD.EQ.0) THEN
C     =================
      JAK=JAK1
      IF(JAK1.EQ.0) CALL JAKER(JAK)
CBB
      JAKP = JAK
      IF(JAK.EQ.1) THEN
        CALL DADMEL(0, ISGN,HV,PNU,PWB,PMU,PNM)
      ELSEIF(JAK.EQ.2) THEN
        CALL DADMMU(0, ISGN,HV,PNU,PWB,PMU,PNM)
      ELSEIF(JAK.EQ.3) THEN
        CALL DADMPI(0, ISGN,HV,PPI,PNU)
      ELSEIF(JAK.EQ.4) THEN
        CALL DADMRO(0, ISGN,HV,PNU,PRHO,PIC,PIZ)
      ELSEIF(JAK.EQ.5) THEN
        CALL DADMAA(0, ISGN,HV,PNU,PAA,PIM1,PIM2,PIPL,JAA)
      ELSEIF(JAK.EQ.6) THEN
        CALL DADMKK(0, ISGN,HV,PKK,PNU)
      ELSEIF(JAK.EQ.7) THEN
        CALL DADMKS(0, ISGN,HV,PNU,PKS ,PKK,PPI,JKST)
      ELSEIF(JAK.EQ.8) THEN
        CALL DADNPI(0, ISGN,PNU,PWB,PNPI,JNPI)
      DO 32 I=1,3
 32   HV(I)=0.
      ELSE
        GOTO 902
      ENDIF
      DO 33 I=1,3
 33   HH(I)=HV(I)
      HH(4)=1.0
 
      ELSEIF(IMD.EQ.1) THEN
C     =====================
      NEV=NEV+1
        IF (JAK.LT.8) THEN
           NEVDEC(JAK)=NEVDEC(JAK)+1
         ELSE
           NEVDEC(JAK+JNPI-1)=NEVDEC(JAK+JNPI-1)+1
         ENDIF
      DO 34 I=1,4
 34   PDUM(I)=.0
      IF(JAK.EQ.1) THEN
        CALL DWLUEL(1,ISGN,PNU,PWB,PMU,PNM)
        DO 10 I=1,4
 10     PP1(I)=PMU(I)
 
      ELSEIF(JAK.EQ.2) THEN
        CALL DWLUMU(1,ISGN,PNU,PWB,PMU,PNM)
        DO 20 I=1,4
 20     PP1(I)=PMU(I)
 
      ELSEIF(JAK.EQ.3) THEN
        CALL DWLUPI(1,ISGN,PPI,PNU)
        DO 30 I=1,4
 30     PP1(I)=PPI(I)
 
      ELSEIF(JAK.EQ.4) THEN
        CALL DWLURO(1,ISGN,PNU,PRHO,PIC,PIZ)
        DO 40 I=1,4
 40     PP1(I)=PRHO(I)
 
      ELSEIF(JAK.EQ.5) THEN
        CALL DWLUAA(1,ISGN,PNU,PAA,PIM1,PIM2,PIPL,JAA)
        DO 50 I=1,4
 50     PP1(I)=PAA(I)
      ELSEIF(JAK.EQ.6) THEN
        CALL DWLUKK(1,ISGN,PKK,PNU)
        DO 60 I=1,4
 60     PP1(I)=PKK(I)
      ELSEIF(JAK.EQ.7) THEN
        CALL DWLUKS(1,ISGN,PNU,PKS,PKK,PPI,JKST)
        DO 70 I=1,4
 70     PP1(I)=PKS(I)
      ELSEIF(JAK.EQ.8) THEN
CAM     MULTIPION DECAY
        CALL DWLNPI(1,ISGN,PNU,PWB,PNPI,JNPI)
        DO 80 I=1,4
 80     PP1(I)=PWB(I)
      ENDIF
 
      ENDIF
C     =====
      RETURN
 902  PRINT 9020
 9020 FORMAT(' ----- DEKAY1: WRONG JAK')
      STOP
      END
      SUBROUTINE DEKAY2(IMOD,HH,ISGN)
C     *******************************
C THIS ROUTINE  SIMULATES TAU-  DECAY
      COMMON / DECP4 / PP1(4),PP2(4),KF1,KF2
      COMMON / JAKI   /  JAK1,JAK2,JAKP,JAKM,KTOM
      COMMON / TAUBMC / GAMPMC(30),GAMPER(30),NEVDEC(30)
      REAL*4            GAMPMC    ,GAMPER
      REAL  HH(4)
      REAL  HV(4),PNU(4),PPI(4)
      REAL  PWB(4),PMU(4),PNM(4)
      REAL  PRHO(4),PIC(4),PIZ(4)
      REAL  PAA(4),PIM1(4),PIM2(4),PIPL(4)
      REAL  PKK(4),PKS(4)
      REAL  PNPI(4,6)
      REAL  PHOT(4)
      REAL  PDUM(4)
      DATA NEV,NPRIN/0,10/
      KTO=2
      IF(JAK2.EQ.-1) RETURN
      IMD=IMOD
      IF(IMD.EQ.0) THEN
C     =================
      JAK=JAK2
      IF(JAK2.EQ.0) CALL JAKER(JAK)
CBB
      JAKM = JAK
      IF(JAK.EQ.1) THEN
        CALL DADMEL(0, ISGN,HV,PNU,PWB,PMU,PNM)
      ELSEIF(JAK.EQ.2) THEN
        CALL DADMMU(0, ISGN,HV,PNU,PWB,PMU,PNM)
      ELSEIF(JAK.EQ.3) THEN
        CALL DADMPI(0, ISGN,HV,PPI,PNU)
      ELSEIF(JAK.EQ.4) THEN
        CALL DADMRO(0, ISGN,HV,PNU,PRHO,PIC,PIZ)
      ELSEIF(JAK.EQ.5) THEN
        CALL DADMAA(0, ISGN,HV,PNU,PAA,PIM1,PIM2,PIPL,JAA)
      ELSEIF(JAK.EQ.6) THEN
        CALL DADMKK(0, ISGN,HV,PKK,PNU)
      ELSEIF(JAK.EQ.7) THEN
        CALL DADMKS(0, ISGN,HV,PNU,PKS ,PKK,PPI,JKST)
      ELSEIF(JAK.EQ.8) THEN
        CALL DEXNPI(0, ISGN,PNU,PWB,PNPI,JNPI)
      DO 32 I=1,3
 32   HV(I)=0.
      ELSE
        GOTO 902
      ENDIF
      DO 33 I=1,3
 33   HH(I)=HV(I)
      HH(4)=1.0
      ELSEIF(IMD.EQ.1) THEN
C     =====================
      NEV=NEV+1
        IF (JAK.LT.8) THEN
           NEVDEC(JAK)=NEVDEC(JAK)+1
         ELSE
           NEVDEC(JAK+JNPI-1)=NEVDEC(JAK+JNPI-1)+1
         ENDIF
      DO 34 I=1,4
 34   PDUM(I)=.0
      IF(JAK.EQ.1) THEN
        CALL DWLUEL(2,ISGN,PNU,PWB,PMU,PNM)
        DO 10 I=1,4
 10     PP2(I)=PMU(I)
 
      ELSEIF(JAK.EQ.2) THEN
        CALL DWLUMU(2,ISGN,PNU,PWB,PMU,PNM)
        DO 20 I=1,4
 20     PP2(I)=PMU(I)
 
      ELSEIF(JAK.EQ.3) THEN
        CALL DWLUPI(2,ISGN,PPI,PNU)
        DO 30 I=1,4
 30     PP2(I)=PPI(I)
 
      ELSEIF(JAK.EQ.4) THEN
        CALL DWLURO(2,ISGN,PNU,PRHO,PIC,PIZ)
        DO 40 I=1,4
 40     PP2(I)=PRHO(I)
 
      ELSEIF(JAK.EQ.5) THEN
        CALL DWLUAA(2,ISGN,PNU,PAA,PIM1,PIM2,PIPL,JAA)
        DO 50 I=1,4
 50     PP2(I)=PAA(I)
      ELSEIF(JAK.EQ.6) THEN
        CALL DWLUKK(2,ISGN,PKK,PNU)
        DO 60 I=1,4
 60     PP1(I)=PKK(I)
      ELSEIF(JAK.EQ.7) THEN
        CALL DWLUKS(2,ISGN,PNU,PKS,PKK,PPI,JKST)
        DO 70 I=1,4
 70     PP1(I)=PKS(I)
      ELSEIF(JAK.EQ.8) THEN
CAM     MULTIPION DECAY
        CALL DWLNPI(2,ISGN,PNU,PWB,PNPI,JNPI)
        DO 80 I=1,4
 80     PP1(I)=PWB(I)
      ENDIF
 
      ENDIF
C     =====
      RETURN
 902  PRINT 9020
 9020 FORMAT(' ----- DEKAY2: WRONG JAK')
      STOP
      END
      SUBROUTINE DEXAY(KTO,POL)
C ----------------------------------------------------------------------
C THIS 'DEXAY' IS A ROUTINE WHICH GENERATES DECAY OF THE SINGLE
C POLARIZED TAU,  POL IS A POLARIZATION VECTOR (NOT A POLARIMETER
C VECTOR AS IN DEKAY) OF THE TAU AND IT IS AN INPUT PARAMETER.
C KTO=0 INITIALISATION (OBLIGATORY)
C KTO=1 DENOTES TAU+ AND KTO=2 TAU-
C DEXAY(1,POL) AND DEXAY(2,POL) ARE CALLED INTERNALLY BY MC GENERATOR.
C DECAY PRODUCTS ARE TRANSFORMED READILY
C TO CMS AND WRITEN IN THE  LUND RECORD IN /LUJETS/
C KTO=100, PRINT FINAL REPORT (OPTIONAL).
C
C     called by : KORALZ
C ----------------------------------------------------------------------
      COMMON / TAUBMC / GAMPMC(30),GAMPER(30),NEVDEC(30)
      REAL*4            GAMPMC    ,GAMPER
      COMMON / JAKI   /  JAK1,JAK2,JAKP,JAKM,KTOM
      COMMON / IDFC  / IDFF
      COMMON / INOUT / INUT,IOUT
      REAL  POL(4)
      REAL  PDUM1(4),PDUM2(4),PDUM3(4),PDUM4(4),PDUM5(4)
      REAL  PDUM(4)
      REAL  PDUMI(4,6)
      KTOM=KTO
      DATA IWARM/0/
C
      IF(KTO.EQ.-1) THEN
C     ==================
C       INITIALISATION OR REINITIALISATION
        IWARM=1
        WRITE(IOUT, 7001) JAK1,JAK2
        NEVTOT=0
        NEV1=0
        NEV2=0
        IF(JAK1.NE.-1.OR.JAK2.NE.-1) THEN
          CALL DEXEL(-1,IDUM,PDUM,PDUM1,PDUM2,PDUM3,PDUM4)
          CALL DEXMU(-1,IDUM,PDUM,PDUM1,PDUM2,PDUM3,PDUM4)
          CALL DEXPI(-1,IDUM,PDUM,PDUM1,PDUM2)
          CALL DEXRO(-1,IDUM,PDUM,PDUM1,PDUM2,PDUM3,PDUM4)
          CALL DEXAA(-1,IDUM,PDUM,PDUM1,PDUM2,PDUM3,PDUM4,PDUM5,IDUM)
          CALL DEXKK(-1,IDUM,PDUM,PDUM1,PDUM2)
          CALL DEXKS(-1,IDUM,PDUM,PDUM1,PDUM2,PDUM3,PDUM4,IDUM)
          CALL DEXNPI(-1,IDUM,PDUM,PDUM1,PDUMI,IDUM)
        ENDIF
        DO 21 I=1,30
        NEVDEC(I)=0
        GAMPMC(I)=0
 21     GAMPER(I)=0
      ELSEIF(KTO.EQ.1) THEN
C     =====================
C DECAY OF TAU+ IN THE TAU REST FRAME
        NEVTOT=NEVTOT+1
        NEV1=NEV1+1
        IF(IWARM.EQ.0) GOTO 902
        ISGN=IDFF/IABS(IDFF)
CAM     CALL DEXAY1(POL,ISGN)
        CALL DEXAY1(KTO,JAK1,JAKP,POL,ISGN)
      ELSEIF(KTO.EQ.2) THEN
C     =================================
C DECAY OF TAU- IN THE TAU REST FRAME
        NEVTOT=NEVTOT+1
        NEV2=NEV2+1
        IF(IWARM.EQ.0) GOTO 902
        ISGN=-IDFF/IABS(IDFF)
CAM     CALL DEXAY2(POL,ISGN)
        CALL DEXAY1(KTO,JAK2,JAKM,POL,ISGN)
      ELSEIF(KTO.EQ.100) THEN
C     =======================
        IF(JAK1.NE.-1.OR.JAK2.NE.-1) THEN
          CALL DEXEL( 1,IDUM,PDUM,PDUM1,PDUM2,PDUM3,PDUM4)
          CALL DEXMU( 1,IDUM,PDUM,PDUM1,PDUM2,PDUM3,PDUM4)
          CALL DEXPI( 1,IDUM,PDUM,PDUM1,PDUM2)
          CALL DEXRO( 1,IDUM,PDUM,PDUM1,PDUM2,PDUM3,PDUM4)
          CALL DEXAA( 1,IDUM,PDUM,PDUM1,PDUM2,PDUM3,PDUM4,PDUM5,IDUM)
          CALL DEXKK( 1,IDUM,PDUM,PDUM1,PDUM2)
          CALL DEXKS( 1,IDUM,PDUM,PDUM1,PDUM2,PDUM3,PDUM4,IDUM)
          CALL DEXNPI( 1,IDUM,PDUM,PDUM,PDUMI,IDUM)
          WRITE(IOUT,7010) NEV1,NEV2,NEVTOT
          WRITE(IOUT,7011) (NEVDEC(I),GAMPMC(I),GAMPER(I),I=1,11)
        ENDIF
      ELSE
        GOTO 910
      ENDIF
      RETURN
 7001 FORMAT(///1X,15(5H*****)
     $ /,' *',     25X,'*****TAUOLA LIBRARY: VERSION 1.5 ******',9X,1H*,
     $ /,' *',     25X,'**********AUGUST 1990 *****************',9X,1H*,
     $ /,' *',     25X,'**AUTHORS: S.JADACH, J.H.KUEHN, Z.WAS**',9X,1H*,
     $ /,' *',     25X,'**AVAILABLE FROM: WASM AT CERNVM ******',9X,1H*,
     $ /,' *',     25X,'**TO BE PUBLISHED IN COMP. PHYS. COMM.*',9X,1H*,
     $ /,' *',     25X,'**PREPRINT CERN-TH 5856 SEPT. 1990*****',9X,1H*,
     $ /,' *',     25X,'******DEXAY ROUTINE: INITIALIZATION****',9X,1H*
     $ /,' *',I20  ,5X,'JAK1   = DECAY MODE FERMION1 (TAU+)    ',9X,1H*
     $ /,' *',I20  ,5X,'JAK2   = DECAY MODE FERMION2 (TAU-)    ',9X,1H*
     $  /,1X,15(5H*****)/)
CHBU  format 7010 had more than 19 continuation lines
CHBU  split into two
 7010 FORMAT(///1X,15(5H*****)
     $ /,' *',     25X,'*****TAUOLA LIBRARY: VERSION 1.5 ******',9X,1H*,
     $ /,' *',     25X,'**********AUGUST 1990 *****************',9X,1H*,
     $ /,' *',     25X,'**AUTHORS: S.JADACH, J.H.KUEHN, Z.WAS**',9X,1H*,
     $ /,' *',     25X,'**AVAILABLE FROM: WASM AT CERNVM ******',9X,1H*,
     $ /,' *',     25X,'**TO BE PUBLISHED IN COMP. PHYS. COMM.*',9X,1H*,
     $ /,' *',     25X,'**PREPRINT CERN-TH 5856 SEPT. 1990*****',9X,1H*,
     $ /,' *',     25X,'******DEXAY ROUTINE: FINAL REPORT******',9X,1H*
     $ /,' *',I20  ,5X,'NEV1   = NO. OF TAU+ DECS. ACCEPTED    ',9X,1H*
     $ /,' *',I20  ,5X,'NEV2   = NO. OF TAU- DECS. ACCEPTED    ',9X,1H*
     $ /,' *',I20  ,5X,'NEVTOT = SUM                           ',9X,1H*
     $ /,' *','    NOEVTS ',
     $   ' PART.WIDTH     ERROR       ROUTINE    DECAY MODE    ',9X,1H*)
 7011 FORMAT(1X,'*'
     $       ,I10,2F12.7       ,'     DADMEL     ELECTRON      ',9X,1H*
     $ /,' *',I10,2F12.7       ,'     DADMMU     MUON          ',9X,1H*
     $ /,' *',I10,2F12.7       ,'     DADMPI     PION          ',9X,1H*
     $ /,' *',I10,2F12.7,       '     DADMRO     RHO (->2PI)   ',9X,1H*
     $ /,' *',I10,2F12.7,       '     DADMAA     A1  (->3PI)   ',9X,1H*
     $ /,' *',I10,2F12.7,       '     DADMKK     KAON          ',9X,1H*
     $ /,' *',I10,2F12.7,       '     DADMKS     K*            ',9X,1H*
     $ /,' *',I10,2F12.7,       '     DADNPI     PI- 2PI+ PI0  ',9X,1H*
     $ /,' *',I10,2F12.7,       '     DADNPI     PI- 3PI0      ',9X,1H*
     $ /,' *',I10,2F12.7,       '     DADNPI     3PI- 2PI+     ',9X,1H*
     $ /,' *',I10,2F12.7,       '     DADNPI     3PI- 2PI+ PI0 ',9X,1H*
     $ /,' *',20X,'THE ERROR IS RELATIVE AND  PART.WIDTH      ',10X,1H*
     $ /,' *',20X,'IN UNITS GFERMI**2*MASS**5/192/PI**3       ',10X,1H*
     $  /,1X,15(5H*****)/)
 902  WRITE(IOUT, 9020)
 9020 FORMAT(' ----- DEXAY: LACK OF INITIALISATION')
      STOP
 910  WRITE(IOUT, 9100)
 9100 FORMAT(' ----- DEXAY: WRONG VALUE OF KTO ')
      STOP
      END
      SUBROUTINE DEXAY1(KTO,JAKIN,JAK,POL,ISGN)
C ---------------------------------------------------------------------
C THIS ROUTINE  SIMULATES TAU+-  DECAY
C
C     called by : DEXAY
C ---------------------------------------------------------------------
      COMMON / TAUBMC / GAMPMC(30),GAMPER(30),NEVDEC(30)
      REAL*4            GAMPMC    ,GAMPER
      COMMON / TAURAD / XK0DEC,ITDKRC
      REAL*8            XK0DEC
      COMMON / INOUT / INUT,IOUT
      REAL  POL(4),POLAR(4)
      REAL  PNU(4),PPI(4)
      REAL  PRHO(4),PIC(4),PIZ(4)
      REAL  PWB(4),PMU(4),PNM(4)
      REAL  PAA(4),PIM1(4),PIM2(4),PIPL(4)
      REAL  PKK(4),PKS(4)
      REAL  PNPI(4,6)
      REAL PHOT(4)
      REAL PDUM(4)
C
      IF(JAKIN.EQ.-1) RETURN
      DO 33 I=1,3
 33   POLAR(I)=POL(I)
      POLAR(4)=0.
      DO 34 I=1,4
 34   PDUM(I)=.0
      JAK=JAKIN
      IF(JAK.EQ.0) CALL JAKER(JAK)
CAM
      IF(JAK.EQ.1) THEN
        CALL DEXEL(0, ISGN,POLAR,PNU,PWB,PMU,PNM)
        CALL DWLUEL(KTO,ISGN,PNU,PWB,PMU,PNM)
      ELSEIF(JAK.EQ.2) THEN
        CALL DEXMU(0, ISGN,POLAR,PNU,PWB,PMU,PNM)
        CALL DWLUMU(KTO,ISGN,PNU,PWB,PMU,PNM)
      ELSEIF(JAK.EQ.3) THEN
        CALL DEXPI(0, ISGN,POLAR,PPI,PNU)
        CALL DWLUPI(KTO,ISGN,PPI,PNU)
      ELSEIF(JAK.EQ.4) THEN
        CALL DEXRO(0, ISGN,POLAR,PNU,PRHO,PIC,PIZ)
        CALL DWLURO(KTO,ISGN,PNU,PRHO,PIC,PIZ)
      ELSEIF(JAK.EQ.5) THEN
        CALL DEXAA(0, ISGN,POLAR,PNU,PAA,PIM1,PIM2,PIPL,JAA)
        CALL DWLUAA(KTO,ISGN,PNU,PAA,PIM1,PIM2,PIPL,JAA)
      ELSEIF(JAK.EQ.6) THEN
        CALL DEXKK(0, ISGN,POLAR,PKK,PNU)
        CALL DWLUKK(KTO,ISGN,PKK,PNU)
      ELSEIF(JAK.EQ.7) THEN
        CALL DEXKS(0, ISGN,POLAR,PNU,PKS,PKK,PPI,JKST)
        CALL DWLUKS(KTO,ISGN,PNU,PKS,PKK,PPI,JKST)
      ELSEIF(JAK.EQ.8) THEN
CAM     MULTIPION DECAY
        CALL DEXNPI(0, ISGN,PNU,PWB,PNPI,JNPI)
        CALL DWLNPI(KTO,ISGN,PNU,PWB,PNPI,JNPI)
      ELSE
        GOTO 902
      ENDIF
      JAKK = JAK
      IF(JAK.EQ.8) JAKK=JAK+JNPI-1
      NEVDEC(JAKK)=NEVDEC(JAKK)+1
      RETURN
 902  WRITE(IOUT, 9020)
 9020 FORMAT(' ----- DEXAY1: WRONG JAK')
      STOP
      END
      SUBROUTINE DEXEL(MODE,ISGN,POL,PNU,PWB,Q1,Q2)
C ----------------------------------------------------------------------
C THIS SIMULATES TAU DECAY IN TAU REST FRAME
C INTO ELECTRON AND TWO NEUTRINOS
C
C     called by : DEXAY,DEXAY1
C ----------------------------------------------------------------------
      REAL  POL(4),HV(4),PWB(4),PNU(4),Q1(4),Q2(4)
      DATA IWARM/0/
C
      IF(MODE.EQ.-1) THEN
C     ===================
        IWARM=1
        CALL DADMEL( -1,ISGN,HV,PNU,PWB,Q1,Q2)
CC      CALL HBOOK1(813,'WEIGHT DISTRIBUTION  DEXEL    $',100,0,2)
C
      ELSEIF(MODE.EQ. 0) THEN
C     =======================
300     CONTINUE
        IF(IWARM.EQ.0) GOTO 902
        CALL DADMEL(  0,ISGN,HV,PNU,PWB,Q1,Q2)
        WT=(1+POL(1)*HV(1)+POL(2)*HV(2)+POL(3)*HV(3))/2.
CC      CALL HFILL(813,WT)
        CALL RANMAR(RN,1)
        IF(RN.GT.WT) GOTO 300
C
      ELSEIF(MODE.EQ. 1) THEN
C     =======================
        CALL DADMEL(  1,ISGN,HV,PNU,PWB,Q1,Q2)
CC      CALL HPRINT(813)
      ENDIF
C     =====
      RETURN
 902  PRINT 9020
 9020 FORMAT(' ----- DEXEL: LACK OF INITIALISATION')
      STOP
      END
      SUBROUTINE DADMEL(MODE,ISGN,HHV,PNU,PWB,Q1,Q2)
C ----------------------------------------------------------------------
C
C     called by : DEXEL,(DEKAY,DEKAY1)
C ----------------------------------------------------------------------
      COMMON / DECPAR / GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      REAL*4            GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
      COMMON / TAUBMC / GAMPMC(30),GAMPER(30),NEVDEC(30)
      REAL*4            GAMPMC    ,GAMPER
      COMMON / INOUT / INUT,IOUT
      REAL  HHV(4),HV(4),PWB(4),PNU(4),Q1(4),Q2(4)
      REAL  PDUM1(4),PDUM2(4),PDUM3(4),PDUM4(4)
      REAL*4 RRR(3)
      REAL*8 SWT, SSWT
      DATA PI /3.141592653589793238462643/
      DATA IWARM/0/
C
      IF(MODE.EQ.-1) THEN
C     ===================
        IWARM=1
        NEVRAW=0
        NEVACC=0
        NEVOVR=0
        SWT=0
        SSWT=0
        WTMAX=1E-20
        DO 15 I=1,500
        CALL DPHSEL(WT,HV,PDUM1,PDUM2,PDUM3,PDUM4)
        IF(WT.GT.WTMAX/1.2) WTMAX=WT*1.2
15      CONTINUE
CC      CALL HBOOK1(803,'WEIGHT DISTRIBUTION  DADMEL    $',100,0,2)
C
      ELSEIF(MODE.EQ. 0) THEN
C     =======================
300     CONTINUE
        IF(IWARM.EQ.0) GOTO 902
        NEVRAW=NEVRAW+1
        CALL DPHSEL(WT,HV,PNU,PWB,Q1,Q2)
CC      CALL HFILL(803,WT/WTMAX)
        SWT=SWT+WT
        SSWT=SSWT+WT**2
        CALL RANMAR(RRR,3)
        RN=RRR(1)
        IF(WT.GT.WTMAX) NEVOVR=NEVOVR+1
        IF(RN*WTMAX.GT.WT) GOTO 300
C ROTATIONS TO BASIC TAU REST FRAME
        RR2=RRR(2)
        COSTHE=-1.+2.*RR2
        THET=ACOS(COSTHE)
        RR3=RRR(3)
        PHI =2*PI*RR3
        CALL ROTOR2(THET,PNU,PNU)
        CALL ROTOR3( PHI,PNU,PNU)
        CALL ROTOR2(THET,PWB,PWB)
        CALL ROTOR3( PHI,PWB,PWB)
        CALL ROTOR2(THET,Q1,Q1)
        CALL ROTOR3( PHI,Q1,Q1)
        CALL ROTOR2(THET,Q2,Q2)
        CALL ROTOR3( PHI,Q2,Q2)
        CALL ROTOR2(THET,HV,HV)
        CALL ROTOR3( PHI,HV,HV)
        DO 44,I=1,3
 44     HHV(I)=-ISGN*HV(I)
        NEVACC=NEVACC+1
C
      ELSEIF(MODE.EQ. 1) THEN
C     =======================
        IF(NEVRAW.EQ.0) RETURN
        PARGAM=SWT/FLOAT(NEVRAW+1)
        ERROR=0
        IF(NEVRAW.NE.0) ERROR=SQRT(SSWT/SWT**2-1./FLOAT(NEVRAW))
        RAT=PARGAM/GAMEL
        WRITE(IOUT, 7010) NEVRAW,NEVACC,NEVOVR,PARGAM,RAT,ERROR
CC      CALL HPRINT(803)
        GAMPMC(1)=RAT
        GAMPER(1)=ERROR
CAM     NEVDEC(1)=NEVACC
      ENDIF
C     =====
      RETURN
 7010 FORMAT(///1X,15(5H*****)
     $ /,' *',     25X,'******** DADMEL FINAL REPORT  ******** ',9X,1H*
     $ /,' *',I20  ,5X,'NEVRAW = NO. OF EL  DECAYS TOTAL       ',9X,1H*
     $ /,' *',I20  ,5X,'NEVACC = NO. OF EL   DECS. ACCEPTED    ',9X,1H*
     $ /,' *',I20  ,5X,'NEVOVR = NO. OF OVERWEIGHTED EVENTS    ',9X,1H*
     $ /,' *',E20.5,5X,'PARTIAL WTDTH ( ELECTRON) IN GEV UNITS ',9X,1H*
     $ /,' *',F20.9,5X,'IN UNITS GFERMI**2*MASS**5/192/PI**3   ',9X,1H*
     $ /,' *',F20.9,5X,'RELATIVE ERROR OF PARTIAL WIDTH        ',9X,1H*
     $  /,1X,15(5H*****)/)
 902  WRITE(IOUT, 9020)
 9020 FORMAT(' ----- DADMEL: LACK OF INITIALISATION')
      STOP
      END
      SUBROUTINE DPHSEL(DGAMT,HV,PN,PWB,Q1,Q2)
C ----------------------------------------------------------------------
C IT SIMULATES TAU DECAY INTO ELECTRON PLUS NEUTRINA
C IN TAU REST FRAME WITH Z-AXIS ALONG W-BOSON MOMENTUM.
C Q1, AMF1 ARE ELECTRON FOUR MOMENTUM AND MASS.
C Q2, AMF2 ARE MUON-NEUTRINO FOUR MOMENTUM AND MASS.
C GV AND GA ARE TAU COUPLING CONSTANTS.
C
C     called by : DADMEL
C ----------------------------------------------------------------------
      COMMON / DECPAR / GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      REAL*4            GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
      REAL  HV(4),PT(4),PN(4),PWB(4),Q1(4),Q2(4)
      REAL*8 DENQ1,DAMF1
      DATA PI /3.141592653589793238462643/
C
      AMF1=AMEL
      AMF2=AMNUE
C
C THREE BODY PHASE SPACE NORMALISED AS IN BJORKEN-DRELL
      PHSPAC=1./2**11/PI**5
C TAU MOMENTUM
      PT(1)=0.
      PT(2)=0.
      PT(3)=0.
      PT(4)=AMTAU
C MASS OF VIRTUAL W-BOSON
      CALL RANMAR(RR1,1)
      AMS1=(AMF1+AMF2)**2
      AMS2=(AMTAU-AMNUTA)**2
C FLAT PHASE SPACE
      AMX2=AMS1+   RR1*(AMS2-AMS1)
      AMX=SQRT(AMX2)
      PHSPAC=PHSPAC*(AMS2-AMS1)
C
C TAU-NEUTRINO MOMENTUM
      PN(1)=0
      PN(2)=0
      PN(4)=1./(2*AMTAU)*(AMTAU**2+AMNUTA**2-AMX**2)
      PN(3)=-SQRT((PN(4)-AMNUTA)*(PN(4)+AMNUTA))
C W-BOSON MOMENTUM
      PWB(1)=0
      PWB(2)=0
      PWB(4)=1./(2*AMTAU)*(AMTAU**2-AMNUTA**2+AMX**2)
      PWB(3)=-PN(3)
      PHSPAC=PHSPAC*(4*PI)*(2*PWB(3)/AMTAU)
C
CAM
      ENQ1=(AMX**2+AMF1**2-AMF2**2)/(2.*AMX)
CAM   ENQ2=(AMX**2-AMF1**2+AMF2**2)/(2.*AMX)
      DENQ1=ENQ1
      DAMF1=AMF1
      PPP=DSQRT(DENQ1**2-DAMF1**2)
      PHSPAC=PHSPAC*(4*PI)*(2*PPP/AMX)
C ELECTRON MOMENTUM IN W REST FRAME
      CALL SPHERA(PPP,Q1)
CAM   Q1(4)=ENQ1
      Q1(4)=SQRT(PPP**2+AMF1**2)
C ELECTRON-NEUTRINO MOMENTUM IN W REST FRAME
      DO 20 I=1,3
20    Q2(I)=-Q1(I)
CAM   Q2(4)=ENQ2
      Q2(4)=SQRT(PPP**2+AMF2**2)
C EL AND NEUTRINO BOOSTED FROM W REST FRAME TO TAU REST FRAME
      EXE=(PWB(4)+PWB(3))/AMX
      CALL BOSTR3(EXE,Q1,Q1)
      CALL BOSTR3(EXE,Q2,Q2)
C
C AMPLITUDE
C
C PURE V-A TAU COUPLING
C     PROD1=PN(4)*Q1(4)-PN(1)*Q1(1)-PN(2)*Q1(2)-PN(3)*Q1(3)
C     PROD2=PT(4)*Q2(4)
C     BRAK=PROD1*PROD2
C     AMPLIT=128*(GFERMI**2/2.)*BRAK
C     DO 40 I=1,3
C40   HV(I)=-AMTAU*PROD1*Q2(I)
C
C V-A  AND  V+A COUPLINGS.
      PTXQ1=PT(4)*Q1(4)
      PNXQ1=PN(4)*Q1(4)-PN(1)*Q1(1)-PN(2)*Q1(2)-PN(3)*Q1(3)
      PTXQ2=PT(4)*Q2(4)
      PNXQ2=PN(4)*Q2(4)-PN(1)*Q2(1)-PN(2)*Q2(2)-PN(3)*Q2(3)
      Q1XQ2=Q1(4)*Q2(4)-Q1(1)*Q2(1)-Q1(2)*Q2(2)-Q1(3)*Q2(3)
      BRAK=(GV+GA)**2*PTXQ1*PNXQ2+(GV-GA)**2*PTXQ2*PNXQ1
     &    -(GV**2-GA**2)*AMTAU*AMNUTA*Q1XQ2
      AMPLIT= 32*(GFERMI**2/2.)*BRAK
      DO 40 I=1,3
      HV(I)=((GV+GA)**2*AMTAU*PNXQ2*Q1(I)
     &      -(GV-GA)**2*AMTAU*PNXQ1*Q2(I)
     &      +(GV**2-GA**2)*AMNUTA*PTXQ2*Q1(I)
     &      -(GV**2-GA**2)*AMNUTA*PTXQ1*Q2(I))/BRAK
 40   CONTINUE
C
      DGAMT=1/(2.*AMTAU)*AMPLIT*PHSPAC
      RETURN
      END
      SUBROUTINE SPHERA(R,X)
C ----------------------------------------------------------------------
C GENERATES UNIFORMLY THREE-VECTOR X ON SPHERE  OF RADIUS R
C
C     called by : DPHSxx,DADMPI,DADMKK
C ----------------------------------------------------------------------
      REAL  X(4)
      REAL*4 RRR(2)
      DATA PI /3.141592653589793238462643/
C
      CALL RANMAR(RRR,2)
      COSTH=-1.+2.*RRR(1)
      SINTH=SQRT(1.-COSTH**2)
      X(1)=R*SINTH*COS(2*PI*RRR(2))
      X(2)=R*SINTH*SIN(2*PI*RRR(2))
      X(3)=R*COSTH
      RETURN
      END
      SUBROUTINE DEXMU(MODE,ISGN,POL,PNU,PWB,Q1,Q2)
C ----------------------------------------------------------------------
C THIS SIMULATES TAU DECAY IN ITS REST FRAME
C INTO MUON AND TWO NEUTRINOS
C OUTPUT FOUR MOMENTA: PNU   TAUNEUTRINO,
C                      PWB   W-BOSON
C                      Q1    MUON
C                      Q2    MUON-NEUTRINO
C ----------------------------------------------------------------------
      COMMON / INOUT / INUT,IOUT
      REAL  POL(4),HV(4),PWB(4),PNU(4),Q1(4),Q2(4)
      DATA IWARM/0/
C
      IF(MODE.EQ.-1) THEN
C     ===================
        IWARM=1
        CALL DADMMU( -1,ISGN,HV,PNU,PWB,Q1,Q2)
CC      CALL HBOOK1(814,'WEIGHT DISTRIBUTION  DEXMU    $',100,0,2)
C
      ELSEIF(MODE.EQ. 0) THEN
C     =======================
300     CONTINUE
        IF(IWARM.EQ.0) GOTO 902
        CALL DADMMU(  0,ISGN,HV,PNU,PWB,Q1,Q2)
        WT=(1+POL(1)*HV(1)+POL(2)*HV(2)+POL(3)*HV(3))/2.
CC      CALL HFILL(814,WT)
        CALL RANMAR(RN,1)
        IF(RN.GT.WT) GOTO 300
C
      ELSEIF(MODE.EQ. 1) THEN
C     =======================
        CALL DADMMU(  1,ISGN,HV,PNU,PWB,Q1,Q2)
CC      CALL HPRINT(814)
      ENDIF
C     =====
      RETURN
 902  WRITE(IOUT, 9020)
 9020 FORMAT(' ----- DEXMU: LACK OF INITIALISATION')
      STOP
      END
      SUBROUTINE DADMMU(MODE,ISGN,HHV,PNU,PWB,Q1,Q2)
C ----------------------------------------------------------------------
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
      COMMON / DECPAR / GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      REAL*4            GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      COMMON / TAUBMC / GAMPMC(30),GAMPER(30),NEVDEC(30)
      REAL*4            GAMPMC    ,GAMPER
      COMMON / INOUT / INUT,IOUT
      REAL  HHV(4),HV(4),PNU(4),PWB(4),Q1(4),Q2(4)
      REAL  PDUM1(4),PDUM2(4),PDUM3(4),PDUM4(4)
      REAL*4 RRR(3)
      REAL*8 SWT, SSWT
      DATA PI /3.141592653589793238462643/
      DATA IWARM /0/
C
      IF(MODE.EQ.-1) THEN
C     ===================
        IWARM=1
        NEVRAW=0
        NEVACC=0
        NEVOVR=0
        SWT=0
        SSWT=0
        WTMAX=1E-20
        DO 15 I=1,500
        CALL DPHSMU(WT,HV,PDUM1,PDUM2,PDUM3,PDUM4)
        IF(WT.GT.WTMAX/1.2) WTMAX=WT*1.2
15      CONTINUE
CC      CALL HBOOK1(802,'WEIGHT DISTRIBUTION  DADMMU    $',100,0,2)
C
      ELSEIF(MODE.EQ. 0) THEN
C     =======================
300     CONTINUE
        IF(IWARM.EQ.0) GOTO 902
        NEVRAW=NEVRAW+1
        CALL DPHSMU(WT,HV,PNU,PWB,Q1,Q2)
CC      CALL HFILL(802,WT/WTMAX)
        SWT=SWT+WT
        SSWT=SSWT+WT**2
        CALL RANMAR(RRR,3)
        RN=RRR(1)
        IF(WT.GT.WTMAX) NEVOVR=NEVOVR+1
        IF(RN*WTMAX.GT.WT) GOTO 300
C ROTATIONS TO BASIC TAU REST FRAME
        COSTHE=-1.+2.*RRR(2)
        THET=ACOS(COSTHE)
        PHI =2*PI*RRR(3)
        CALL ROTOR2(THET,PNU,PNU)
        CALL ROTOR3( PHI,PNU,PNU)
        CALL ROTOR2(THET,PWB,PWB)
        CALL ROTOR3( PHI,PWB,PWB)
        CALL ROTOR2(THET,Q1,Q1)
        CALL ROTOR3( PHI,Q1,Q1)
        CALL ROTOR2(THET,Q2,Q2)
        CALL ROTOR3( PHI,Q2,Q2)
        CALL ROTOR2(THET,HV,HV)
        CALL ROTOR3( PHI,HV,HV)
        DO 44,I=1,3
 44     HHV(I)=-ISGN*HV(I)
        NEVACC=NEVACC+1
C
      ELSEIF(MODE.EQ. 1) THEN
C     =======================
        IF(NEVRAW.EQ.0) RETURN
        PARGAM=SWT/FLOAT(NEVRAW+1)
        ERROR=0
        IF(NEVRAW.NE.0) ERROR=SQRT(SSWT/SWT**2-1./FLOAT(NEVRAW))
        RAT=PARGAM/GAMEL
        WRITE(IOUT, 7010) NEVRAW,NEVACC,NEVOVR,PARGAM,RAT,ERROR
CC      CALL HPRINT(802)
        GAMPMC(2)=RAT
        GAMPER(2)=ERROR
CAM     NEVDEC(2)=NEVACC
      ENDIF
C     =====
      RETURN
 7010 FORMAT(///1X,15(5H*****)
     $ /,' *',     25X,'******** DADMMU FINAL REPORT  ******** ',9X,1H*
     $ /,' *',I20  ,5X,'NEVRAW = NO. OF MU  DECAYS TOTAL       ',9X,1H*
     $ /,' *',I20  ,5X,'NEVACC = NO. OF MU   DECS. ACCEPTED    ',9X,1H*
     $ /,' *',I20  ,5X,'NEVOVR = NO. OF OVERWEIGHTED EVENTS    ',9X,1H*
     $ /,' *',E20.5,5X,'PARTIAL WTDTH (MU  DECAY) IN GEV UNITS ',9X,1H*
     $ /,' *',F20.9,5X,'IN UNITS GFERMI**2*MASS**5/192/PI**3   ',9X,1H*
     $ /,' *',F20.9,5X,'RELATIVE ERROR OF PARTIAL WIDTH        ',9X,1H*
     $  /,1X,15(5H*****)/)
 902  WRITE(IOUT, 9020)
 9020 FORMAT(' ----- DADMMU: LACK OF INITIALISATION')
      STOP
      END
      SUBROUTINE DPHSMU(DGAMT,HV,PN,PWB,Q1,Q2)
C ----------------------------------------------------------------------
C IT SIMULATES TAU DECAY INTO MUON PLUS NEUTRINA
C IN TAU REST FRAME WITH Z-AXIS ALONG W-BOSON MOMENTUM.
C Q1, AMF1 ARE MUON FOUR MOMENTUM AND MASS.
C Q2, AMF2 ARE MUON-NEUTRINO FOUR MOMENTUM AND MASS.
C GV AND GA ARE TAU COUPLING CONSTANTS.
C
C     called by : DADMMU
C ----------------------------------------------------------------------
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
      COMMON / DECPAR / GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      REAL*4            GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      REAL  HV(4),PT(4),PN(4),PWB(4),Q1(4),Q2(4)
      DATA PI /3.141592653589793238462643/
C
      AMF1=AMMU
      AMF2=AMNUMU
C
C THREE BODY PHASE SPACE NORMALISED AS IN BJORKEN-DRELL
      PHSPAC=1./2**11/PI**5
C TAU MOMENTUM
      PT(1)=0.
      PT(2)=0.
      PT(3)=0.
      PT(4)=AMTAU
C MASS OF VIRTUAL W-BOSON
      AMS1=(AMF1+AMF2)**2
      AMS2=(AMTAU-AMNUTA)**2
C FLAT PHASE SPACE
10    CALL RANMAR(RR1,1)
      IF(RR1.LT.1.E-05) GO TO 10
      AMX2=AMS1+   RR1*(AMS2-AMS1)
C     IF(AMX2.LT..0) PRINT *, 'DPHSMU AMX2',AMX2
      AMX=SQRT(AMX2)
      PHSPAC=PHSPAC*(AMS2-AMS1)
C
C TAU-NEUTRINO MOMENTUM
      PN(1)=0
      PN(2)=0
      PN(4)=1./(2*AMTAU)*(AMTAU**2+AMNUTA**2-AMX**2)
      PN(3)=(PN(4)-AMNUTA)*(PN(4)+AMNUTA)
C     IF(PN(3).LT..0) PRINT *, 'DPHSMU PN(3)',PN,AMTAU,AMNUTA,AMX
      PN(3)=-SQRT((PN(4)-AMNUTA)*(PN(4)+AMNUTA))
C W-BOSON MOMENTUM
      PWB(1)=0
      PWB(2)=0
      PWB(4)=1./(2*AMTAU)*(AMTAU**2-AMNUTA**2+AMX**2)
      PWB(3)=-PN(3)
      PHSPAC=PHSPAC*(4*PI)*(2*PWB(3)/AMTAU)
C
CAM
      ENQ1=(AMX**2+AMF1**2-AMF2**2)/(2.*AMX)
      ENQ2=(AMX**2-AMF1**2+AMF2**2)/(2.*AMX)
      PPP=SQRT(ENQ1**2-AMF1**2)
      PHSPAC=PHSPAC*(4*PI)*(2*PPP/AMX)
C MUON MOMENTUM IN W REST FRAME
      CALL SPHERA(PPP,Q1)
      Q1(4)=ENQ1
C MUON-NEUTRINO MOMENTUM IN W REST FRAME
      DO 20 I=1,3
20    Q2(I)=-Q1(I)
      Q2(4)=ENQ2
      EXE=(PWB(4)+PWB(3))/AMX
C MU AND NEUTRINO BOOSTED FROM W REST FRAME TO TAU REST FRAME
      CALL BOSTR3(EXE,Q1,Q1)
      CALL BOSTR3(EXE,Q2,Q2)
C AMPLITUDE
C
C PURE V-A TAU COUPLING
C     PROD1=PN(4)*Q1(4)-PN(1)*Q1(1)-PN(2)*Q1(2)-PN(3)*Q1(3)
C     PROD2=PT(4)*Q2(4)
C     BRAK=PROD1*PROD2
C     AMPLIT=128*(GFERMI**2/2.)*BRAK
C     DO 40 I=1,3
C40   HV(I)=-AMTAU*PROD1*Q2(I)
C
C V-A  AND  V+A COUPLINGS.
      PTXQ1=PT(4)*Q1(4)
      PNXQ1=PN(4)*Q1(4)-PN(1)*Q1(1)-PN(2)*Q1(2)-PN(3)*Q1(3)
      PTXQ2=PT(4)*Q2(4)
      PNXQ2=PN(4)*Q2(4)-PN(1)*Q2(1)-PN(2)*Q2(2)-PN(3)*Q2(3)
      Q1XQ2=Q1(4)*Q2(4)-Q1(1)*Q2(1)-Q1(2)*Q2(2)-Q1(3)*Q2(3)
      BRAK=(GV+GA)**2*PTXQ1*PNXQ2+(GV-GA)**2*PTXQ2*PNXQ1
     &    -(GV**2-GA**2)*AMTAU*AMNUTA*Q1XQ2
      AMPLIT= 32*(GFERMI**2/2.)*BRAK
      DO 40 I=1,3
      HV(I)=((GV+GA)**2*AMTAU*PNXQ2*Q1(I)
     &      -(GV-GA)**2*AMTAU*PNXQ1*Q2(I)
     &      +(GV**2-GA**2)*AMNUTA*PTXQ2*Q1(I)
     &      -(GV**2-GA**2)*AMNUTA*PTXQ1*Q2(I))/BRAK
 40   CONTINUE
C
      DGAMT=1/(2.*AMTAU)*AMPLIT*PHSPAC
      RETURN
      END
      SUBROUTINE DEXPI(MODE,ISGN,POL,PPI,PNU)
C ----------------------------------------------------------------------
C TAU DECAY INTO PION AND TAU-NEUTRINO
C IN TAU REST FRAME
C OUTPUT FOUR MOMENTA: PNU   TAUNEUTRINO,
C                      PPI   PION CHARGED
C ----------------------------------------------------------------------
      REAL  POL(4),HV(4),PNU(4),PPI(4)
CC
      IF(MODE.EQ.-1) THEN
C     ===================
        CALL DADMPI(-1,ISGN,HV,PPI,PNU)
CC      CALL HBOOK1(815,'WEIGHT DISTRIBUTION  DEXPI    $',100,0,2)
 
      ELSEIF(MODE.EQ. 0) THEN
C     =======================
300     CONTINUE
        CALL DADMPI( 0,ISGN,HV,PPI,PNU)
        WT=(1+POL(1)*HV(1)+POL(2)*HV(2)+POL(3)*HV(3))/2.
CC      CALL HFILL(815,WT)
        CALL RANMAR(RN,1)
        IF(RN.GT.WT) GOTO 300
C
      ELSEIF(MODE.EQ. 1) THEN
C     =======================
        CALL DADMPI( 1,ISGN,HV,PPI,PNU)
CC      CALL HPRINT(815)
      ENDIF
C     =====
      RETURN
      END
      SUBROUTINE DADMPI(MODE,ISGN,HV,PPI,PNU)
C ----------------------------------------------------------------------
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
      COMMON / DECPAR / GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      REAL*4            GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      COMMON / TAUBMC / GAMPMC(30),GAMPER(30),NEVDEC(30)
      REAL*4            GAMPMC    ,GAMPER
      COMMON / INOUT / INUT,IOUT
      REAL  PPI(4),PNU(4),HV(4)
      DATA PI /3.141592653589793238462643/
C
      IF(MODE.EQ.-1) THEN
C     ===================
        NEVTOT=0
      ELSEIF(MODE.EQ. 0) THEN
C     =======================
        NEVTOT=NEVTOT+1
        EPI= (AMTAU**2+AMPI**2-AMNUTA**2)/(2*AMTAU)
        ENU= (AMTAU**2-AMPI**2+AMNUTA**2)/(2*AMTAU)
        XPI= SQRT(EPI**2-AMPI**2)
C PI MOMENTUM
        CALL SPHERA(XPI,PPI)
        PPI(4)=EPI
C TAU-NEUTRINO MOMENTUM
        DO 30 I=1,3
30      PNU(I)=-PPI(I)
        PNU(4)=ENU
        PXQ=AMTAU*EPI
        PXN=AMTAU*ENU
        QXN=PPI(4)*PNU(4)-PPI(1)*PNU(1)-PPI(2)*PNU(2)-PPI(3)*PNU(3)
        BRAK=(GV**2+GA**2)*(2*PXQ*QXN-AMPI**2*PXN)
     &      +(GV**2-GA**2)*AMTAU*AMNUTA*AMPI**2
        DO 40 I=1,3
40      HV(I)=-ISGN*2*GA*GV*AMTAU*(2*PPI(I)*QXN-PNU(I)*AMPI**2)/BRAK
        HV(4)=1
C
      ELSEIF(MODE.EQ. 1) THEN
C     =======================
        IF(NEVTOT.EQ.0) RETURN
        FPI=0.1284
        GAMM=(GFERMI*FPI)**2/(16.*PI)*AMTAU**3*
     *       (BRAK/AMTAU**4)**2
        ERROR=0
        RAT=GAMM/GAMEL
        WRITE(IOUT, 7010) NEVTOT,GAMM,RAT,ERROR
        GAMPMC(3)=RAT
        GAMPER(3)=ERROR
CAM     NEVDEC(3)=NEVTOT
      ENDIF
C     =====
      RETURN
 7010 FORMAT(///1X,15(5H*****)
     $ /,' *',     25X,'******** DADMPI FINAL REPORT  ******** ',9X,1H*
     $ /,' *',I20  ,5X,'NEVTOT = NO. OF PI  DECAYS TOTAL       ',9X,1H*
     $ /,' *',E20.5,5X,'PARTIAL WTDTH ( PI DECAY) IN GEV UNITS ',9X,1H*
     $ /,' *',F20.9,5X,'IN UNITS GFERMI**2*MASS**5/192/PI**3   ',9X,1H*
     $ /,' *',F20.8,5X,'RELATIVE ERROR OF PARTIAL WIDTH (STAT.)',9X,1H*
     $  /,1X,15(5H*****)/)
      END
      SUBROUTINE DEXRO(MODE,ISGN,POL,PNU,PRO,PIC,PIZ)
C ----------------------------------------------------------------------
C THIS SIMULATES TAU DECAY IN TAU REST FRAME
C INTO NU RHO, NEXT RHO DECAYS INTO PION PAIR.
C OUTPUT FOUR MOMENTA: PNU   TAUNEUTRINO,
C                      PRO   RHO
C                      PIC   PION CHARGED
C                      PIZ   PION ZERO
C ----------------------------------------------------------------------
      COMMON / INOUT / INUT,IOUT
      REAL  POL(4),HV(4),PRO(4),PNU(4),PIC(4),PIZ(4)
      DATA IWARM/0/
C
      IF(MODE.EQ.-1) THEN
C     ===================
        IWARM=1
        CALL DADMRO( -1,ISGN,HV,PNU,PRO,PIC,PIZ)
CC      CALL HBOOK1(816,'WEIGHT DISTRIBUTION  DEXRO    $',100,0,2)
CC      CALL HBOOK1(916,'ABS2 OF HV IN ROUTINE DEXRO   $',100,0,2)
C
      ELSEIF(MODE.EQ. 0) THEN
C     =======================
300     CONTINUE
        IF(IWARM.EQ.0) GOTO 902
        CALL DADMRO(  0,ISGN,HV,PNU,PRO,PIC,PIZ)
        WT=(1+POL(1)*HV(1)+POL(2)*HV(2)+POL(3)*HV(3))/2.
CC      CALL HFILL(816,WT)
CC      XHELP=HV(1)**2+HV(2)**2+HV(3)**2
CC      CALL HFILL(916,XHELP)
        CALL RANMAR(RN,1)
        IF(RN.GT.WT) GOTO 300
C
      ELSEIF(MODE.EQ. 1) THEN
C     =======================
        CALL DADMRO(  1,ISGN,HV,PNU,PRO,PIC,PIZ)
CC      CALL HPRINT(816)
CC      CALL HPRINT(916)
      ENDIF
C     =====
      RETURN
 902  WRITE(IOUT, 9020)
 9020 FORMAT(' ----- DEXRO: LACK OF INITIALISATION')
      STOP
      END
      SUBROUTINE DADMRO(MODE,ISGN,HHV,PNU,PRO,PIC,PIZ)
C ----------------------------------------------------------------------
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
      COMMON / DECPAR / GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      REAL*4            GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      COMMON / TAUBMC / GAMPMC(30),GAMPER(30),NEVDEC(30)
      REAL*4            GAMPMC    ,GAMPER
      COMMON / INOUT / INUT,IOUT
      REAL  HHV(4)
      REAL  HV(4),PRO(4),PNU(4),PIC(4),PIZ(4)
      REAL  PDUM1(4),PDUM2(4),PDUM3(4),PDUM4(4)
      REAL*4 RRR(3)
      REAL*8 SWT, SSWT
      DATA PI /3.141592653589793238462643/
      DATA IWARM/0/
C
      IF(MODE.EQ.-1) THEN
C     ===================
        IWARM=1
        NEVRAW=0
        NEVACC=0
        NEVOVR=0
        SWT=0
        SSWT=0
        WTMAX=1E-20
        DO 15 I=1,500
        CALL DPHSRO(WT,HV,PDUM1,PDUM2,PDUM3,PDUM4)
        IF(WT.GT.WTMAX/1.2) WTMAX=WT*1.2
15      CONTINUE
CC      CALL HBOOK1(801,'WEIGHT DISTRIBUTION  DADMRO    $',100,0,2)
CC      PRINT 7003,WTMAX
C
      ELSEIF(MODE.EQ. 0) THEN
C     =======================
300     CONTINUE
        IF(IWARM.EQ.0) GOTO 902
        CALL DPHSRO(WT,HV,PNU,PRO,PIC,PIZ)
CC      CALL HFILL(801,WT/WTMAX)
        NEVRAW=NEVRAW+1
        SWT=SWT+WT
        SSWT=SSWT+WT**2
        CALL RANMAR(RRR,3)
        RN=RRR(1)
        IF(WT.GT.WTMAX) NEVOVR=NEVOVR+1
        IF(RN*WTMAX.GT.WT) GOTO 300
C ROTATIONS TO BASIC TAU REST FRAME
        COSTHE=-1.+2.*RRR(2)
        THET=ACOS(COSTHE)
        PHI =2*PI*RRR(3)
        CALL ROTOR2(THET,PNU,PNU)
        CALL ROTOR3( PHI,PNU,PNU)
        CALL ROTOR2(THET,PRO,PRO)
        CALL ROTOR3( PHI,PRO,PRO)
        CALL ROTOR2(THET,PIC,PIC)
        CALL ROTOR3( PHI,PIC,PIC)
        CALL ROTOR2(THET,PIZ,PIZ)
        CALL ROTOR3( PHI,PIZ,PIZ)
        CALL ROTOR2(THET,HV,HV)
        CALL ROTOR3( PHI,HV,HV)
        DO 44 I=1,3
 44     HHV(I)=-ISGN*HV(I)
        NEVACC=NEVACC+1
C
      ELSEIF(MODE.EQ. 1) THEN
C     =======================
        IF(NEVRAW.EQ.0) RETURN
        PARGAM=SWT/FLOAT(NEVRAW+1)
        ERROR=0
        IF(NEVRAW.NE.0) ERROR=SQRT(SSWT/SWT**2-1./FLOAT(NEVRAW))
        RAT=PARGAM/GAMEL
        WRITE(IOUT, 7010) NEVRAW,NEVACC,NEVOVR,PARGAM,RAT,ERROR
CC      CALL HPRINT(801)
        GAMPMC(4)=RAT
        GAMPER(4)=ERROR
CAM     NEVDEC(4)=NEVACC
      ENDIF
C     =====
      RETURN
 7003 FORMAT(///1X,15(5H*****)
     $ /,' *',     25X,'******** DADMRO INITIALISATION ********',9X,1H*
     $ /,' *',E20.5,5X,'WTMAX  = MAXIMUM WEIGHT                ',9X,1H*
     $  /,1X,15(5H*****)/)
 7010 FORMAT(///1X,15(5H*****)
     $ /,' *',     25X,'******** DADMRO FINAL REPORT  ******** ',9X,1H*
     $ /,' *',I20  ,5X,'NEVRAW = NO. OF RHO DECAYS TOTAL       ',9X,1H*
     $ /,' *',I20  ,5X,'NEVACC = NO. OF RHO  DECS. ACCEPTED    ',9X,1H*
     $ /,' *',I20  ,5X,'NEVOVR = NO. OF OVERWEIGHTED EVENTS    ',9X,1H*
     $ /,' *',E20.5,5X,'PARTIAL WTDTH (RHO DECAY) IN GEV UNITS ',9X,1H*
     $ /,' *',F20.9,5X,'IN UNITS GFERMI**2*MASS**5/192/PI**3   ',9X,1H*
     $ /,' *',F20.8,5X,'RELATIVE ERROR OF PARTIAL WIDTH        ',9X,1H*
     $  /,1X,15(5H*****)/)
 902  WRITE(IOUT, 9020)
 9020 FORMAT(' ----- DADMRO: LACK OF INITIALISATION')
      STOP
      END
      SUBROUTINE DPHSRO(DGAMT,HV,PN,PR,PIC,PIZ)
C ----------------------------------------------------------------------
C IT SIMULATES RHO DECAY IN TAU REST FRAME WITH
C Z-AXIS ALONG RHO MOMENTUM
C ----------------------------------------------------------------------
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
      COMMON / DECPAR / GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      REAL*4            GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      REAL  HV(4),PT(4),PN(4),PR(4),PIC(4),PIZ(4),QQ(4)
      DATA PI /3.141592653589793238462643/
      DATA ICONT /0/
C
C THREE BODY PHASE SPACE NORMALISED AS IN BJORKEN-DRELL
      PHSPAC=1./2**11/PI**5
C TAU MOMENTUM
      PT(1)=0.
      PT(2)=0.
      PT(3)=0.
      PT(4)=AMTAU
C MASS OF (REAL/VIRTUAL) RHO
      AMS1=(AMPI+AMPIZ)**2
      AMS2=(AMTAU-AMNUTA)**2
C FLAT PHASE SPACE
C     AMX2=AMS1+   RR1*(AMS2-AMS1)
C     AMX=SQRT(AMX2)
C     PHSPAC=PHSPAC*(AMS2-AMS1)
C PHASE SPACE WITH SAMPLING FOR RHO RESONANCE
      ALP1=ATAN((AMS1-AMRO**2)/AMRO/GAMRO)
      ALP2=ATAN((AMS2-AMRO**2)/AMRO/GAMRO)
CAM
 100  CONTINUE
      CALL RANMAR(RR1,1)
      ALP=ALP1+RR1*(ALP2-ALP1)
      AMX2=AMRO**2+AMRO*GAMRO*TAN(ALP)
      AMX=SQRT(AMX2)
      IF(AMX.LT.2.*AMPI) GO TO 100
CAM
      PHSPAC=PHSPAC*((AMX2-AMRO**2)**2+(AMRO*GAMRO)**2)/(AMRO*GAMRO)
      PHSPAC=PHSPAC*(ALP2-ALP1)
C
C TAU-NEUTRINO MOMENTUM
      PN(1)=0
      PN(2)=0
      PN(4)=1./(2*AMTAU)*(AMTAU**2+AMNUTA**2-AMX**2)
      PN(3)=-SQRT((PN(4)-AMNUTA)*(PN(4)+AMNUTA))
C RHO MOMENTUM
      PR(1)=0
      PR(2)=0
      PR(4)=1./(2*AMTAU)*(AMTAU**2-AMNUTA**2+AMX**2)
      PR(3)=-PN(3)
      PHSPAC=PHSPAC*(4*PI)*(2*PR(3)/AMTAU)
C
CAM
      ENQ1=(AMX2+AMPI**2-AMPIZ**2)/(2.*AMX)
      ENQ2=(AMX2-AMPI**2+AMPIZ**2)/(2.*AMX)
      PPPI=SQRT((ENQ1-AMPI)*(ENQ1+AMPI))
      PHSPAC=PHSPAC*(4*PI)*(2*PPPI/AMX)
C CHARGED PI MOMENTUM IN RHO REST FRAME
      CALL SPHERA(PPPI,PIC)
      PIC(4)=ENQ1
C NEUTRAL PI MOMENTUM IN RHO REST FRAME
      DO 20 I=1,3
20    PIZ(I)=-PIC(I)
      PIZ(4)=ENQ2
      EXE=(PR(4)+PR(3))/AMX
C PIONS BOOSTED FROM RHO REST FRAME TO TAU REST FRAME
      CALL BOSTR3(EXE,PIC,PIC)
      CALL BOSTR3(EXE,PIZ,PIZ)
      DO 30 I=1,4
30    QQ(I)=PIC(I)-PIZ(I)
C AMPLITUDE
      PRODPQ=PT(4)*QQ(4)
      PRODNQ=PN(4)*QQ(4)-PN(1)*QQ(1)-PN(2)*QQ(2)-PN(3)*QQ(3)
      PRODPN=PT(4)*PN(4)
      QQ2= QQ(4)**2-QQ(1)**2-QQ(2)**2-QQ(3)**2
      BRAK=(GV**2+GA**2)*(2*PRODPQ*PRODNQ-PRODPN*QQ2)
     &    +(GV**2-GA**2)*AMTAU*AMNUTA*QQ2
      AMPLIT=(GFERMI*CCABIB)**2*BRAK*2*FPIRHO(AMX)
      DGAMT=1/(2.*AMTAU)*AMPLIT*PHSPAC
      DO 40 I=1,3
 40   HV(I)=2*GV*GA*AMTAU*(2*PRODNQ*QQ(I)-QQ2*PN(I))/BRAK
      RETURN
      END
      SUBROUTINE DEXAA(MODE,ISGN,POL,PNU,PAA,PIM1,PIM2,PIPL,JAA)
C ----------------------------------------------------------------------
* THIS SIMULATES TAU DECAY IN TAU REST FRAME
* INTO NU A1, NEXT A1 DECAYS INTO RHO PI AND FINALLY RHO INTO PI PI.
* OUTPUT FOUR MOMENTA: PNU   TAUNEUTRINO,
*                      PAA   A1
*                      PIM1  PION MINUS (OR PI0) 1      (FOR TAU MINUS)
*                      PIM2  PION MINUS (OR PI0) 2
*                      PIPL  PION PLUS  (OR PI-)
*                      (PIPL,PIM1) FORM A RHO
C ----------------------------------------------------------------------
      COMMON / INOUT / INUT,IOUT
      REAL  POL(4),HV(4),PAA(4),PNU(4),PIM1(4),PIM2(4),PIPL(4)
      DATA IWARM/0/
C
      IF(MODE.EQ.-1) THEN
C     ===================
        IWARM=1
        CALL DADMAA( -1,ISGN,HV,PNU,PAA,PIM1,PIM2,PIPL,JAA)
CC      CALL HBOOK1(816,'WEIGHT DISTRIBUTION  DEXAA    $',100,-2.,2.)
C
      ELSEIF(MODE.EQ. 0) THEN
*     =======================
 300    CONTINUE
        IF(IWARM.EQ.0) GOTO 902
        CALL DADMAA(  0,ISGN,HV,PNU,PAA,PIM1,PIM2,PIPL,JAA)
        WT=(1+POL(1)*HV(1)+POL(2)*HV(2)+POL(3)*HV(3))/2.
CC      CALL HFILL(816,WT)
        CALL RANMAR(RN,1)
        IF(RN.GT.WT) GOTO 300
C
      ELSEIF(MODE.EQ. 1) THEN
*     =======================
        CALL DADMAA(  1,ISGN,HV,PNU,PAA,PIM1,PIM2,PIPL,JAA)
CC      CALL HPRINT(816)
      ENDIF
C     =====
      RETURN
 902  WRITE(IOUT, 9020)
 9020 FORMAT(' ----- DEXAA: LACK OF INITIALISATION')
      STOP
      END
      FUNCTION GFUN(QKWA)
C ****************************************************************
C     G-FUNCTION USED TO INRODUCE ENERGY DEPENDENCE IN A1 WIDTH
C ****************************************************************
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
       IF (QKWA.LT.(AMRO+AMPI)**2) THEN
          GFUN=4.1*(QKWA-9*AMPIZ**2)**3
     $        *(1.-3.3*(QKWA-9*AMPIZ**2)+5.8*(QKWA-9*AMPIZ**2)**2)
       ELSE
          GFUN=QKWA*(1.623+10.38/QKWA-9.32/QKWA**2+0.65/QKWA**3)
       ENDIF
      END
      COMPLEX FUNCTION BWIGS(S,M,G)
C **********************************************************
C     P-WAVE BREIT-WIGNER  FOR K*
C **********************************************************
      REAL S,M,G
      REAL PI,PIM,QS,QM,W,GS,MK
      DATA INIT /0/
      P(A,B,C)=SQRT(((A+B-C)**2-4.*A*B)/4./A)
C ------------ PARAMETERS --------------------
      IF (INIT.EQ.0) THEN
      INIT=1
      PI=3.141592654
      PIM=.1340
      MK=.494
C -------  BREIT-WIGNER -----------------------
         ENDIF
         QS=P(S,PIM**2,MK**2)
         QM=P(M**2,PIM**2,MK**2)
         W=SQRT(S)
         GS=G*(M/W)*(QS/QM)**3
         BWIGS=M**2/CMPLX(M**2-S,-M*GS)
      RETURN
      END
      COMPLEX FUNCTION BWIG(S,M,G)
C **********************************************************
C     P-WAVE BREIT-WIGNER  FOR RHO
C **********************************************************
      REAL S,M,G
      REAL PI,PIM,QS,QM,W,GS
      DATA INIT /0/
C ------------ PARAMETERS --------------------
      IF (INIT.EQ.0) THEN
      INIT=1
      PI=3.141592654
      PIM=.1400
C -------  BREIT-WIGNER -----------------------
         ENDIF
         QS=SQRT(ABS(S/4.-PIM**2))
         QM=SQRT(M**2/4.-PIM**2)
         W=SQRT(S)
         GS=G*(M/W)*(QS/QM)**3
         BWIG=M**2/CMPLX(M**2-S,-M*GS)
      RETURN
      END
      COMPLEX FUNCTION FPIK(W)
C **********************************************************
C     PION FORM FACTOR
C **********************************************************
      COMPLEX BWIG
      REAL ROM,ROG,ROM1,ROG1,BETA1,PI,PIM,S,W
      EXTERNAL BWIG
      DATA  INIT /0/
C
C ------------ PARAMETERS --------------------
      IF (INIT.EQ.0 ) THEN
      INIT=1
      PI=3.141592654
      PIM=.140
      ROM=0.773
      ROG=0.145
      ROM1=1.370
      ROG1=0.510
      BETA1=-0.145
      ENDIF
C -----------------------------------------------
      S=W**2
      FPIK= (BWIG(S,ROM,ROG)+BETA1*BWIG(S,ROM1,ROG1))
     & /(1+BETA1)
      RETURN
      END
      FUNCTION FPIRHO(W)
C **********************************************************
C     SQUARE OF PION FORM FACTOR
C **********************************************************
      COMPLEX FPIK
      FPIRHO=CABS(FPIK(W))**2
      END
      SUBROUTINE DADMAA(MODE,ISGN,HHV,PNU,PAA,PIM1,PIM2,PIPL,JAA)
C ----------------------------------------------------------------------
* A1 DECAY UNWEIGHTED EVENTS
C ----------------------------------------------------------------------
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
      COMMON / DECPAR / GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      REAL*4            GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      COMMON / TAUBMC / GAMPMC(30),GAMPER(30),NEVDEC(30)
      REAL*4            GAMPMC    ,GAMPER
      COMMON / INOUT / INUT,IOUT
      REAL  HHV(4)
      REAL  HV(4),PAA(4),PNU(4),PIM1(4),PIM2(4),PIPL(4)
      REAL  PDUM1(4),PDUM2(4),PDUM3(4),PDUM4(4),PDUM5(4)
      REAL*4 RRR(3)
      REAL*8 SWT, SSWT
      DATA PI /3.141592653589793238462643/
      DATA IWARM/0/
C
      IF(MODE.EQ.-1) THEN
C     ===================
        IWARM=1
        NEVRAW=0
        NEVACC=0
        NEVOVR=0
        SWT=0
        SSWT=0
        WTMAX=1E-20
        DO 15 I=1,500
        CALL DPHSAA(WT,HV,PDUM1,PDUM2,PDUM3,PDUM4,PDUM5,JAA)
        IF(WT.GT.WTMAX/1.2) WTMAX=WT*1.2
15      CONTINUE
CC      CALL HBOOK1(801,'WEIGHT DISTRIBUTION  DADMAA    $',100,0,2)
C
      ELSEIF(MODE.EQ. 0) THEN
C     =======================
300     CONTINUE
        IF(IWARM.EQ.0) GOTO 902
        CALL DPHSAA(WT,HV,PNU,PAA,PIM1,PIM2,PIPL,JAA)
CC      CALL HFILL(801,WT/WTMAX)
        NEVRAW=NEVRAW+1
        SWT=SWT+WT
        SSWT=SSWT+WT**2
        CALL RANMAR(RRR,3)
        RN=RRR(1)
        IF(WT.GT.WTMAX) NEVOVR=NEVOVR+1
        IF(RN*WTMAX.GT.WT) GOTO 300
C ROTATIONS TO BASIC TAU REST FRAME
        COSTHE=-1.+2.*RRR(2)
        THET=ACOS(COSTHE)
        PHI =2*PI*RRR(3)
        CALL ROTPOL(THET,PHI,PNU)
        CALL ROTPOL(THET,PHI,PAA)
        CALL ROTPOL(THET,PHI,PIM1)
        CALL ROTPOL(THET,PHI,PIM2)
        CALL ROTPOL(THET,PHI,PIPL)
        CALL ROTPOL(THET,PHI,HV)
        DO 44 I=1,3
 44     HHV(I)=-ISGN*HV(I)
        NEVACC=NEVACC+1
C
      ELSEIF(MODE.EQ. 1) THEN
C     =======================
        IF(NEVRAW.EQ.0) RETURN
        PARGAM=SWT/FLOAT(NEVRAW+1)
        ERROR=0
        IF(NEVRAW.NE.0) ERROR=SQRT(SSWT/SWT**2-1./FLOAT(NEVRAW))
        RAT=PARGAM/GAMEL
        WRITE(IOUT, 7010) NEVRAW,NEVACC,NEVOVR,PARGAM,RAT,ERROR
CC      CALL HPRINT(801)
        GAMPMC(5)=RAT
        GAMPER(5)=ERROR
CAM     NEVDEC(5)=NEVACC
      ENDIF
C     =====
      RETURN
 7003 FORMAT(///1X,15(5H*****)
     $ /,' *',     25X,'******** DADMAA INITIALISATION ********',9X,1H*
     $ /,' *',E20.5,5X,'WTMAX  = MAXIMUM WEIGHT                ',9X,1H*
     $  /,1X,15(5H*****)/)
 7010 FORMAT(///1X,15(5H*****)
     $ /,' *',     25X,'******** DADMAA FINAL REPORT  ******** ',9X,1H*
     $ /,' *',I20  ,5X,'NEVRAW = NO. OF A1  DECAYS TOTAL       ',9X,1H*
     $ /,' *',I20  ,5X,'NEVACC = NO. OF A1   DECS. ACCEPTED    ',9X,1H*
     $ /,' *',I20  ,5X,'NEVOVR = NO. OF OVERWEIGHTED EVENTS    ',9X,1H*
     $ /,' *',E20.5,5X,'PARTIAL WTDTH (A1  DECAY) IN GEV UNITS ',9X,1H*
     $ /,' *',F20.9,5X,'IN UNITS GFERMI**2*MASS**5/192/PI**3   ',9X,1H*
     $ /,' *',F20.8,5X,'RELATIVE ERROR OF PARTIAL WIDTH        ',9X,1H*
     $  /,1X,15(5H*****)/)
 902  WRITE(IOUT, 9020)
 9020 FORMAT(' ----- DADMAA: LACK OF INITIALISATION')
      STOP
      END
      SUBROUTINE ROTPOL(THET,PHI,PP)
C ----------------------------------------------------------------------
C
C     called by : DADMAA,DPHSAA
C ----------------------------------------------------------------------
      REAL  PP(4)
C
      CALL ROTOR2(THET,PP,PP)
      CALL ROTOR3( PHI,PP,PP)
      RETURN
      END
      SUBROUTINE DPHSAA(DGAMT,HV,PN,PAA,PIM1,PIM2,PIPL,JAA)
C ----------------------------------------------------------------------
* IT SIMULATES A1  DECAY IN TAU REST FRAME WITH
* Z-AXIS ALONG A1  MOMENTUM
C ----------------------------------------------------------------------
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
      COMMON / DECPAR / GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      REAL*4            GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      REAL  HV(4),PT(4),PN(4),PAA(4),PIM1(4),PIM2(4),PIPL(4)
      REAL  PR(4)
      REAL  PSUM(4)
      REAL*4 RRR(6)
      DATA PI /3.141592653589793238462643/
      DATA ICONT /0/
      XLAM(X,Y,Z)=SQRT((X-Y-Z)**2-4.0*Y*Z)
C AMRO, GAMRO IS ONLY A PARAMETER FOR GETING HIGHT EFFICIENCY
C
C THREE BODY PHASE SPACE NORMALISED AS IN BJORKEN-DRELL
C D**3 P /2E/(2PI)**3 (2PI)**4 DELTA4(SUM P)
      PHSPAC=1./2**17/PI**8
C TAU MOMENTUM
      PT(1)=0.
      PT(2)=0.
      PT(3)=0.
      PT(4)=AMTAU
C
      CALL RANMAR(RRR,6)
C
* MASSES OF A1 AND RHO
* MASS OF (REAL/VIRTUAL) A1, PHASE SPACE with resonance sampling
CAM
CAM   FIRST CHOOSE WHICH DECAY MODE (RHO- PI0) OR (RHO0 PI-)
      RMOD=RRR(6)
      IF(RMOD.GT..5) THEN
CAM  A1- DECAY TO RHO0 PI-
        JAA=1
        RR1=RRR(1)
        AMS1=(3*AMPI)**2
        AMS2=(AMTAU-AMNUTA)**2
C       AM3SQ=AMS1+   RR1*(AMS2-AMS1)
C       AM3 =SQRT(AM3SQ)
C       PHSPAC=PHSPAC*(AMS2-AMS1)
* PHASE SPACE WITH SAMPLING FOR A1  RESONANCE
      ALP1=ATAN((AMS1-AMA1**2)/AMA1/GAMA1)
      ALP2=ATAN((AMS2-AMA1**2)/AMA1/GAMA1)
      ALP=ALP1+RR1*(ALP2-ALP1)
      AM3SQ =AMA1**2+AMA1*GAMA1*TAN(ALP)
      AM3 =SQRT(AM3SQ)
      PHSPAC=PHSPAC*((AM3SQ-AMA1**2)**2+(AMA1*GAMA1)**2)/(AMA1*GAMA1)
      PHSPAC=PHSPAC*(ALP2-ALP1)
C MASS OF (REAL/VIRTUAL) RHO 0
        RR2=RRR(2)
        AMS1=(2*AMPI)**2
        AMS2=(AM3-AMPI)**2
      ELSE
CAM  A1- DECAY TO RHO- PI0
        JAA=2
        RR1=RRR(1)
        AMS1=(2*AMPIZ+AMPI)**2
        AMS2=(AMTAU-AMNUTA)**2
C       AM3SQ=AMS1+   RR1*(AMS2-AMS1)
C       AM3 =SQRT(AM3SQ)
C       PHSPAC=PHSPAC*(AMS2-AMS1)
* PHASE SPACE WITH SAMPLING FOR A1  RESONANCE
      ALP1=ATAN((AMS1-AMA1**2)/AMA1/GAMA1)
      ALP2=ATAN((AMS2-AMA1**2)/AMA1/GAMA1)
      ALP=ALP1+RR1*(ALP2-ALP1)
      AM3SQ =AMA1**2+AMA1*GAMA1*TAN(ALP)
      AM3 =SQRT(AM3SQ)
      PHSPAC=PHSPAC*((AM3SQ-AMA1**2)**2+(AMA1*GAMA1)**2)/(AMA1*GAMA1)
      PHSPAC=PHSPAC*(ALP2-ALP1)
C MASS OF (REAL/VIRTUAL) RHO -
        RR2=RRR(2)
        AMS1=(AMPIZ+AMPI)**2
        AMS2=(AM3-AMPIZ)**2
      ENDIF
CAM
CAM
* FLAT PHASE SPACE;
      AM2SQ=AMS1+   RR2*(AMS2-AMS1)
      AM2 =SQRT(AM2SQ)
      PHSPAC=PHSPAC*(AMS2-AMS1)
* RHO REST FRAME, DEFINE PIPL AND PIM1
      IF(JAA.EQ.1) THEN
        ENPI=AM2/2
        PPI=         ENPI**2-AMPI**2
        PPPI=SQRT(ABS(ENPI**2-AMPI**2))
        PHSPAC=PHSPAC*(4*PI)*(2*PPPI/AM2)
* PI PLUS MOMENTUM IN RHO REST FRAME
        CALL SPHERA(PPPI,PIPL)
        PIPL(4)=ENPI
* PI MINUS 1 MOMENTUM IN RHO REST FRAME
        DO 20 I=1,3
 20     PIM1(I)=-PIPL(I)
        PIM1(4)=ENPI
      ELSE
        ENQ1=(AM2SQ-AMPIZ**2+AMPI**2)/(2*AM2)
        ENQ2=(AM2SQ+AMPIZ**2-AMPI**2)/(2*AM2)
        PPI=         ENQ1**2-AMPI**2
        PPPI=SQRT(ABS(ENQ1**2-AMPI**2))
        PHSPAC=PHSPAC*(4*PI)*(2*PPPI/AM2)
* PI MINUS MOMENTUM IN RHO REST FRAME
        CALL SPHERA(PPPI,PIPL)
        PIPL(4)=ENQ1
* PI0 1 MOMENTUM IN RHO REST FRAME
        DO 30 I=1,3
 30     PIM1(I)=-PIPL(I)
        PIM1(4)=ENQ2
      ENDIF
* A1 REST FRAME, DEFINE PIM2
      IF (JAA.EQ.1) THEN
*       RHO  MOMENTUM
        PR(1)=0
        PR(2)=0
        PR(4)=1./(2*AM3)*(AM3**2+AM2**2-AMPI**2)
        PR(3)= SQRT(ABS(PR(4)**2-AM2**2))
        PPI  =          PR(4)**2-AM2**2
*       PI MINUS 2 MOMENTUM
        PIM2(1)=0
        PIM2(2)=0
        PIM2(4)=1./(2*AM3)*(AM3**2-AM2**2+AMPI**2)
        PIM2(3)=-PR(3)
      ELSE
*       RHO  MOMENTUM
        PR(1)=0
        PR(2)=0
        PR(4)=1./(2*AM3)*(AM3**2+AM2**2-AMPIZ**2)
        PR(3)= SQRT(ABS(PR(4)**2-AM2**2))
        PPI  =          PR(4)**2-AM2**2
*       PI0 2 MOMENTUM
        PIM2(1)=0
        PIM2(2)=0
        PIM2(4)=1./(2*AM3)*(AM3**2-AM2**2+AMPIZ**2)
        PIM2(3)=-PR(3)
      ENDIF
      PHSPAC=PHSPAC*(4*PI)*(2*PR(3)/AM3)
* OLD PIONS BOOSTED FROM RHO REST FRAME TO A1 REST FRAME
      EXE=(PR(4)+PR(3))/AM2
      CALL BOSTR3(EXE,PIPL,PIPL)
      CALL BOSTR3(EXE,PIM1,PIM1)
      RR3=RRR(3)
      RR4=RRR(4)
CAM   THET =PI*RR3
      THET =ACOS(-1.+2*RR3)
      PHI = 2*PI*RR4
      CALL ROTPOL(THET,PHI,PIPL)
      CALL ROTPOL(THET,PHI,PIM1)
      CALL ROTPOL(THET,PHI,PIM2)
      CALL ROTPOL(THET,PHI,PR)
C
* NOW TO THE TAU REST FRAME, DEFINE A1 AND NEUTRINO MOMENTA
* A1  MOMENTUM
      PAA(1)=0
      PAA(2)=0
      PAA(4)=1./(2*AMTAU)*(AMTAU**2-AMNUTA**2+AM3**2)
      PAA(3)= SQRT(ABS(PAA(4)**2-AM3**2))
      PPI   =          PAA(4)**2-AM3**2
      PHSPAC=PHSPAC*(4*PI)*(2*PAA(3)/AMTAU)
* TAU-NEUTRINO MOMENTUM
      PN(1)=0
      PN(2)=0
      PN(4)=1./(2*AMTAU)*(AMTAU**2+AMNUTA**2-AM3**2)
      PN(3)=-PAA(3)
* ALL PIONS BOOSTED FROM A1  REST FRAME TO TAU REST FRAME
* Z-AXIS ANTIPARALLEL TO NEUTRINO MOMENTUM
      EXE=(PAA(4)+PAA(3))/AM3
      CALL BOSTR3(EXE,PIPL,PIPL)
      CALL BOSTR3(EXE,PIM1,PIM1)
      CALL BOSTR3(EXE,PIM2,PIM2)
      CALL BOSTR3(EXE,PR,PR)
* MOMENTA OF THE TWO PI-MINUS ARE RANDOMLY SYMMETRISED
* (cf DAMPAA for the shape of matrix element: it assumes
* the two like-sign pions are symmetrical)
      RR5= RRR(5)
      IF(RR5.LE.0.5) THEN
        DO 70 I=1,4
        X=PIM1(I)
        PIM1(I)=PIM2(I)
 70     PIM2(I)=X
      ENDIF
C PARTIAL WIDTH CONSISTS OF PHASE SPACE AND AMPLITUDE
      CALL DAMPAA(PT,PN,PIM1,PIM2,PIPL,AMPLIT,HV)
      DGAMT=1/(2.*AMTAU)*AMPLIT*PHSPAC
C THE STATISTICAL FACTOR FOR IDENTICAL PI'S IS CANCELLED WITH
C TWO, FOR TWO MODES OF A1 DECAY NAMELLY PI+PI-PI- AND PI-PI0PI0
      END
      SUBROUTINE DAMPAA(PT,PN,PIM1,PIM2,PIPL,AMPLIT,HV)
C ----------------------------------------------------------------------
* CALCULATES DIFFERENTIAL CROSS SECTION AND POLARIMETER VECTOR
* FOR TAU DECAY INTO A1, A1 DECAYS NEXT INTO RHO+PI AND RHO INTO PI+PI.
* ALL SPIN EFFECTS IN THE FULL DECAY CHAIN ARE TAKEN INTO ACCOUNT.
* CALCULATIONS DONE IN TAU REST FRAME WITH Z-AXIS ALONG NEUTRINO MOMENT
* THE ROUTINE IS WRITEN FOR ZERO NEUTRINO MASS.
C
C     called by : DPHSAA
C ----------------------------------------------------------------------
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
      COMMON / DECPAR / GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      REAL*4            GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      REAL  HV(4),PT(4),PN(4),PIM1(4),PIM2(4),PIPL(4)
      REAL  PAA(4),PR(4),VEC1(4),VEC2(4)
      REAL  PIVEC(4),PIAKS(4),HVM(4)
      REAL  PSUM(4)
      COMPLEX BWIGN,HADCUR(4),FPIK
      DATA ICONT /1/
C
* F CONSTANTS FOR A1, A1-RHO-PI, AND RHO-PI-PI
*
      DATA  FPI /93.3E-3/
* THIS INLINE FUNCT. CALCULATES THE SCALAR PART OF THE PROPAGATOR
      BWIGN(XM,AM,GAMMA)=1./CMPLX(XM**2-AM**2,GAMMA*AM)
C
* FOUR MOMENTUM OF A1
      DO 10 I=1,4
   10 PAA(I)=PIM1(I)+PIM2(I)+PIPL(I)
* MASSES OF A1, AND OF TWO PI-PAIRS WHICH MAY FORM RHO
      XMAA   =SQRT(ABS(PAA(4)**2-PAA(3)**2-PAA(2)**2-PAA(1)**2))
      XMRO1  =SQRT(ABS((PIPL(4)+PIM1(4))**2-(PIPL(1)+PIM1(1))**2
     $                -(PIPL(2)+PIM1(2))**2-(PIPL(3)+PIM1(3))**2))
      XMRO2  =SQRT(ABS((PIPL(4)+PIM2(4))**2-(PIPL(1)+PIM2(1))**2
     $                -(PIPL(2)+PIM2(2))**2-(PIPL(3)+PIM2(3))**2))
* ELEMENTS OF HADRON CURRENT
      PROD1  =PAA(4)*(PIM1(4)-PIPL(4))-PAA(1)*(PIM1(1)-PIPL(1))
     $       -PAA(2)*(PIM1(2)-PIPL(2))-PAA(3)*(PIM1(3)-PIPL(3))
      PROD2  =PAA(4)*(PIM2(4)-PIPL(4))-PAA(1)*(PIM2(1)-PIPL(1))
     $       -PAA(2)*(PIM2(2)-PIPL(2))-PAA(3)*(PIM2(3)-PIPL(3))
      DO 40 I=1,4
      VEC1(I)= PIM1(I)-PIPL(I) -PAA(I)*PROD1/XMAA**2
 40   VEC2(I)= PIM2(I)-PIPL(I) -PAA(I)*PROD2/XMAA**2
* HADRON CURRENT SATURATED WITH A1 AND RHO RESONANCES
      FNORM=2.0*SQRT(2.)/3.0/FPI
      GAMAX=GAMA1*GFUN(XMAA**2)/GFUN(AMA1**2)
      DO 45 I=1,4
      HADCUR(I)= CMPLX(FNORM) *AMA1**2*BWIGN(XMAA,AMA1,GAMAX)
     $            *(CMPLX(VEC1(I))*FPIK(XMRO1)
     $             +CMPLX(VEC2(I))*FPIK(XMRO2))
 45   CONTINUE
C
* CALCULATE PI-VECTORS: VECTOR AND AXIAL
      CALL CLVEC(HADCUR,PN,PIVEC)
      CALL CLAXI(HADCUR,PN,PIAKS)
      CALL CLNUT(HADCUR,BRAKM,HVM)
* SPIN INDEPENDENT PART OF DECAY DIFF-CROSS-SECT. IN TAU REST  FRAME
      BRAK= (GV**2+GA**2)*PT(4)*PIVEC(4) +2.*GV*GA*PT(4)*PIAKS(4)
     &     +2.*(GV**2-GA**2)*AMNUTA*AMTAU*BRAKM
      AMPLIT=(GFERMI*CCABIB)**2*BRAK/2.
C THE STATISTICAL FACTOR FOR IDENTICAL PI'S WAS CANCELLED WITH
C TWO, FOR TWO MODES OF A1 DECAY NAMELLY PI+PI-PI- AND PI-PI0PI0
C POLARIMETER VECTOR IN TAU REST FRAME
      DO 90 I=1,3
      HV(I)=-(AMTAU*((GV**2+GA**2)*PIAKS(I)+2.*GV*GA*PIVEC(I)))
     &      +(GV**2-GA**2)*AMNUTA*AMTAU*HVM(I)
C HV IS DEFINED FOR TAU-    WITH GAMMA=B+HV*POL
      HV(I)=-HV(I)/BRAK
 90   CONTINUE
      END
      SUBROUTINE CLVEC(HJ,PN,PIV)
C ----------------------------------------------------------------------
* CALCULATES THE "VECTOR TYPE"  PI-VECTOR  PIV
* NOTE THAT THE NEUTRINO MOM. PN IS ASSUMED TO BE ALONG Z-AXIS
C
C     called by : DAMPAA
C ----------------------------------------------------------------------
      REAL PIV(4),PN(4)
      COMPLEX HJ(4),HN
C
      HN= HJ(4)*CMPLX(PN(4))-HJ(3)*CMPLX(PN(3))
      HH= REAL(HJ(4)*CONJG(HJ(4))-HJ(3)*CONJG(HJ(3))
     $        -HJ(2)*CONJG(HJ(2))-HJ(1)*CONJG(HJ(1)))
      DO 10 I=1,4
   10 PIV(I)=4.*REAL(HN*CONJG(HJ(I)))-2.*HH*PN(I)
      RETURN
      END
      SUBROUTINE CLAXI(HJ,PN,PIA)
C ----------------------------------------------------------------------
* CALCULATES THE "AXIAL TYPE"  PI-VECTOR  PIA
* NOTE THAT THE NEUTRINO MOM. PN IS ASSUMED TO BE ALONG Z-AXIS
C SIGN is chosen +/- for decay of TAU +/- respectively
C     called by : DAMPAA, CLNUT
C ----------------------------------------------------------------------
      COMMON / JAKI   /  JAK1,JAK2,JAKP,JAKM,KTOM
      COMMON / IDFC  / IDFF
      REAL PIA(4),PN(4)
      COMPLEX HJ(4),HJC(4)
      DET2(I,J)=AIMAG(HJ(I)*HJC(J)-HJ(J)*HJC(I))
* -----------------------------------
      IF     (KTOM.EQ.1.OR.KTOM.EQ.-1) THEN
        SIGN= IDFF/ABS(IDFF)
      ELSEIF (KTOM.EQ.2) THEN
        SIGN=-IDFF/ABS(IDFF)
      ELSE
        PRINT *, 'STOP IN CLAXI: KTOM=',KTOM
        STOP
      ENDIF
C
      DO 10 I=1,4
 10   HJC(I)=CONJG(HJ(I))
      PIA(1)= -2.*PN(3)*DET2(2,4)+2.*PN(4)*DET2(2,3)
      PIA(2)= -2.*PN(4)*DET2(1,3)+2.*PN(3)*DET2(1,4)
      PIA(3)=  2.*PN(4)*DET2(1,2)
      PIA(4)=  2.*PN(3)*DET2(1,2)
C ALL FOUR INDICES ARE UP SO  PIA(3) AND PIA(4) HAVE SAME SIGN
      DO 20 I=1,4
  20  PIA(I)=PIA(I)*SIGN
      END
      SUBROUTINE CLNUT(HJ,B,HV)
C ----------------------------------------------------------------------
* CALCULATES THE CONTRIBUTION BY NEUTRINO MASS
* NOTE THE TAU IS ASSUMED TO BE AT REST
C
C     called by : DAMPAA
C ----------------------------------------------------------------------
      COMPLEX HJ(4)
      REAL HV(4),P(4)
      DATA P /3*0.,1.0/
C
      CALL CLAXI(HJ,P,HV)
      B=REAL( HJ(4)*AIMAG(HJ(4)) - HJ(3)*AIMAG(HJ(3))
     &      - HJ(2)*AIMAG(HJ(2)) - HJ(1)*AIMAG(HJ(1))  )
      RETURN
      END
      SUBROUTINE DEXKK(MODE,ISGN,POL,PKK,PNU)
C ----------------------------------------------------------------------
C TAU DECAY INTO KAON  AND TAU-NEUTRINO
C IN TAU REST FRAME
C OUTPUT FOUR MOMENTA: PNU   TAUNEUTRINO,
C                      PKK   KAON CHARGED
C ----------------------------------------------------------------------
      REAL  POL(4),HV(4),PNU(4),PKK(4)
C
      IF(MODE.EQ.-1) THEN
C     ===================
        CALL DADMKK(-1,ISGN,HV,PKK,PNU)
CC      CALL HBOOK1(815,'WEIGHT DISTRIBUTION  DEXPI    $',100,0,2)
C
      ELSEIF(MODE.EQ. 0) THEN
C     =======================
300     CONTINUE
        CALL DADMKK( 0,ISGN,HV,PKK,PNU)
        WT=(1+POL(1)*HV(1)+POL(2)*HV(2)+POL(3)*HV(3))/2.
CC      CALL HFILL(815,WT)
        CALL RANMAR(RN,1)
        IF(RN.GT.WT) GOTO 300
C
      ELSEIF(MODE.EQ. 1) THEN
C     =======================
        CALL DADMKK( 1,ISGN,HV,PKK,PNU)
CC      CALL HPRINT(815)
      ENDIF
C     =====
      RETURN
      END
      SUBROUTINE DADMKK(MODE,ISGN,HV,PKK,PNU)
C ----------------------------------------------------------------------
C FZ
      COMMON / DECPAR / GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      REAL*4            GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
      COMMON / TAUBMC / GAMPMC(30),GAMPER(30),NEVDEC(30)
      REAL*4            GAMPMC    ,GAMPER
      COMMON / INOUT / INUT,IOUT
      REAL  PKK(4),PNU(4),HV(4)
      DATA PI /3.141592653589793238462643/
C
      IF(MODE.EQ.-1) THEN
C     ===================
        NEVTOT=0
      ELSEIF(MODE.EQ. 0) THEN
C     =======================
        NEVTOT=NEVTOT+1
        EKK= (AMTAU**2+AMK**2-AMNUTA**2)/(2*AMTAU)
        ENU= (AMTAU**2-AMK**2+AMNUTA**2)/(2*AMTAU)
        XKK= SQRT(EKK**2-AMK**2)
C K MOMENTUM
        CALL SPHERA(XKK,PKK)
        PKK(4)=EKK
C TAU-NEUTRINO MOMENTUM
        DO 30 I=1,3
30      PNU(I)=-PKK(I)
        PNU(4)=ENU
        PXQ=AMTAU*EKK
        PXN=AMTAU*ENU
        QXN=PKK(4)*PNU(4)-PKK(1)*PNU(1)-PKK(2)*PNU(2)-PKK(3)*PNU(3)
        BRAK=(GV**2+GA**2)*(2*PXQ*QXN-AMK**2*PXN)
     &      +(GV**2-GA**2)*AMTAU*AMNUTA*AMK**2
        DO 40 I=1,3
40      HV(I)=-ISGN*2*GA*GV*AMTAU*(2*PKK(I)*QXN-PNU(I)*AMK**2)/BRAK
        HV(4)=1
C
      ELSEIF(MODE.EQ. 1) THEN
C     =======================
        IF(NEVTOT.EQ.0) RETURN
        FKK=0.0354
CFZ THERE WAS BRAK/AMTAU**4 BEFORE
        GAMM=(GFERMI*FKK)**2/(16.*PI)*AMTAU**3*
     *       (BRAK/AMTAU**4)**2
        ERROR=0
        RAT=GAMM/GAMEL
        WRITE(IOUT, 7010) NEVTOT,GAMM,RAT,ERROR
        GAMPMC(6)=RAT
        GAMPER(6)=ERROR
CAM     NEVDEC(6)=NEVTOT
      ENDIF
C     =====
      RETURN
 7010 FORMAT(///1X,15(5H*****)
     $ /,' *',     25X,'******** DADMKK FINAL REPORT   ********',9X,1H*
     $ /,' *',I20  ,5X,'NEVTOT = NO. OF K  DECAYS TOTAL        ',9X,1H*,
     $ /,' *',E20.5,5X,'PARTIAL WTDTH ( K DECAY) IN GEV UNITS  ',9X,1H*,
     $ /,' *',F20.9,5X,'IN UNITS GFERMI**2*MASS**5/192/PI**3   ',9X,1H*
     $ /,' *',F20.8,5X,'RELATIVE ERROR OF PARTIAL WIDTH (STAT.)',9X,1H*
     $  /,1X,15(5H*****)/)
      END
      SUBROUTINE DEXKS(MODE,ISGN,POL,PNU,PKS,PKK,PPI,JKST)
C ----------------------------------------------------------------------
C THIS SIMULATES TAU DECAY IN TAU REST FRAME
C INTO NU K*, THEN K* DECAYS INTO PI0,K+-(JKST=20)
C OR PI+-,K0(JKST=10).
C OUTPUT FOUR MOMENTA: PNU   TAUNEUTRINO,
C                      PKS   K* CHARGED
C                      PK0   K ZERO
C                      PKC   K CHARGED
C                      PIC   PION CHARGED
C                      PIZ   PION ZERO
C ----------------------------------------------------------------------
      COMMON / INOUT / INUT,IOUT
      REAL  POL(4),HV(4),PKS(4),PNU(4),PKK(4),PPI(4)
      DATA IWARM/0/
C
      IF(MODE.EQ.-1) THEN
C     ===================
        IWARM=1
CFZ INITIALISATION DONE WITH THE GHARGED PION NEUTRAL KAON MODE(JKST=10
        CALL DADMKS( -1,ISGN,HV,PNU,PKS,PKK,PPI,JKST)
CC      CALL HBOOK1(816,'WEIGHT DISTRIBUTION  DEXKS    $',100,0,2)
CC      CALL HBOOK1(916,'ABS2 OF HV IN ROUTINE DEXKS   $',100,0,2)
C
      ELSEIF(MODE.EQ. 0) THEN
C     =======================
300     CONTINUE
        IF(IWARM.EQ.0) GOTO 902
        CALL DADMKS(  0,ISGN,HV,PNU,PKS,PKK,PPI,JKST)
        WT=(1+POL(1)*HV(1)+POL(2)*HV(2)+POL(3)*HV(3))/2.
CC      CALL HFILL(816,WT)
CC      XHELP=HV(1)**2+HV(2)**2+HV(3)**2
CC      CALL HFILL(916,XHELP)
        CALL RANMAR(RN,1)
        IF(RN.GT.WT) GOTO 300
C
      ELSEIF(MODE.EQ. 1) THEN
C     ======================================
        CALL DADMKS( 1,ISGN,HV,PNU,PKS,PKK,PPI,JKST)
CC      CALL HPRINT(816)
CC      CALL HPRINT(916)
      ENDIF
C     =====
      RETURN
 902  WRITE(IOUT, 9020)
 9020 FORMAT(' ----- DEXKS: LACK OF INITIALISATION')
      STOP
      END
      SUBROUTINE DADMKS(MODE,ISGN,HHV,PNU,PKS,PKK,PPI,JKST)
C ----------------------------------------------------------------------
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
      COMMON / DECPAR / GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      REAL*4            GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      COMMON / TAUBMC / GAMPMC(30),GAMPER(30),NEVDEC(30)
      REAL*4            GAMPMC    ,GAMPER
      COMMON / INOUT / INUT,IOUT
      REAL  HHV(4)
      REAL  HV(4),PKS(4),PNU(4),PKK(4),PPI(4)
      REAL  PDUM1(4),PDUM2(4),PDUM3(4),PDUM4(4)
      REAL*4 RRR(3)
      REAL*8 SWT, SSWT
      DATA PI /3.141592653589793238462643/
      DATA IWARM/0/
C
      IF(MODE.EQ.-1) THEN
C     ===================
        IWARM=1
        NEVRAW=0
        NEVACC=0
        NEVOVR=0
        SWT=0
        SSWT=0
        WTMAX=1E-20
        DO 15 I=1,500
C THE INITIALISATION IS DONE WITH THE 66.7% MODE
        JKST=10
        CALL DPHSKS(WT,HV,PDUM1,PDUM2,PDUM3,PDUM4,JKST)
        IF(WT.GT.WTMAX/1.2) WTMAX=WT*1.2
15      CONTINUE
CC      CALL HBOOK1(801,'WEIGHT DISTRIBUTION  DADMKS    $',100,0,2)
CC      PRINT 7003,WTMAX
CC      CALL HBOOK1(112,'-------- K* MASS -------- $',100,0.,2.)
      ELSEIF(MODE.EQ. 0) THEN
C     =====================================
        IF(IWARM.EQ.0) GOTO 902
C  HERE WE CHOOSE RANDOMLY BETWEEN K0 PI+_ (66.7%)
C  AND K+_ PI0 (33.3%)
        DEC1=0.667
400     CONTINUE
        CALL RANMAR(RMOD,1)
        IF(RMOD.LT.DEC1) THEN
          JKST=10
        ELSE
          JKST=20
        ENDIF
        CALL DPHSKS(WT,HV,PNU,PKS,PKK,PPI,JKST)
        CALL RANMAR(RRR,3)
        RN=RRR(1)
        IF(WT.GT.WTMAX) NEVOVR=NEVOVR+1
        NEVRAW=NEVRAW+1
        SWT=SWT+WT
        SSWT=SSWT+WT**2
        IF(RN*WTMAX.GT.WT) GOTO 400
C ROTATIONS TO BASIC TAU REST FRAME
        COSTHE=-1.+2.*RRR(2)
        THET=ACOS(COSTHE)
        PHI =2*PI*RRR(3)
        CALL ROTOR2(THET,PNU,PNU)
        CALL ROTOR3( PHI,PNU,PNU)
        CALL ROTOR2(THET,PKS,PKS)
        CALL ROTOR3( PHI,PKS,PKS)
        CALL ROTOR2(THET,PKK,PKK)
        CALL ROTOR3(PHI,PKK,PKK)
        CALL ROTOR2(THET,PPI,PPI)
        CALL ROTOR3( PHI,PPI,PPI)
        CALL ROTOR2(THET,HV,HV)
        CALL ROTOR3( PHI,HV,HV)
        DO 44 I=1,3
 44     HHV(I)=-ISGN*HV(I)
        NEVACC=NEVACC+1
C
      ELSEIF(MODE.EQ. 1) THEN
C     =======================
        IF(NEVRAW.EQ.0) RETURN
        PARGAM=SWT/FLOAT(NEVRAW+1)
        ERROR=0
        IF(NEVRAW.NE.0) ERROR=SQRT(SSWT/SWT**2-1./FLOAT(NEVRAW))
        RAT=PARGAM/GAMEL
        WRITE(IOUT, 7010) NEVRAW,NEVACC,NEVOVR,PARGAM,RAT,ERROR
CC      CALL HPRINT(801)
        GAMPMC(7)=RAT
        GAMPER(7)=ERROR
CAM     NEVDEC(7)=NEVACC
      ENDIF
C     =====
      RETURN
 7003 FORMAT(///1X,15(5H*****)
     $ /,' *',     25X,'******** DADMKS INITIALISATION ********',9X,1H*
     $ /,' *',E20.5,5X,'WTMAX  = MAXIMUM WEIGHT                ',9X,1H*
     $  /,1X,15(5H*****)/)
 7010 FORMAT(///1X,15(5H*****)
     $ /,' *',     25X,'******** DADMKS FINAL REPORT   ********',9X,1H*
     $ /,' *',I20  ,5X,'NEVRAW = NO. OF K* DECAYS TOTAL        ',9X,1H*,
     $ /,' *',I20  ,5X,'NEVACC = NO. OF K*  DECS. ACCEPTED     ',9X,1H*,
     $ /,' *',I20  ,5X,'NEVOVR = NO. OF OVERWEIGHTED EVENTS    ',9X,1H*
     $ /,' *',E20.5,5X,'PARTIAL WTDTH (K* DECAY) IN GEV UNITS  ',9X,1H*,
     $ /,' *',F20.9,5X,'IN UNITS GFERMI**2*MASS**5/192/PI**3   ',9X,1H*
     $ /,' *',F20.8,5X,'RELATIVE ERROR OF PARTIAL WIDTH        ',9X,1H*
     $  /,1X,15(5H*****)/)
 902  WRITE(IOUT, 9020)
 9020 FORMAT(' ----- DADMKS: LACK OF INITIALISATION')
      STOP
      END
      SUBROUTINE DPHSKS(DGAMT,HV,PN,PKS,PKK,PPI,JKST)
C ----------------------------------------------------------------------
C IT SIMULATES KAON* DECAY IN TAU REST FRAME WITH
C Z-AXIS ALONG KAON* MOMENTUM
C     JKST=10 FOR K* --->K0 + PI+-
C     JKST=20 FOR K* --->K+- + PI0
C ----------------------------------------------------------------------
      COMMON / DECPAR / GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      REAL*4            GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
      REAL  HV(4),PT(4),PN(4),PKS(4),PKK(4),PPI(4),QQ(4)
      COMPLEX BWIGS
      DATA PI /3.141592653589793238462643/
C
      DATA ICONT /0/
C THREE BODY PHASE SPACE NORMALISED AS IN BJORKEN-DRELL
      PHSPAC=1./2**11/PI**5
C TAU MOMENTUM
      PT(1)=0.
      PT(2)=0.
      PT(3)=0.
      PT(4)=AMTAU
      CALL RANMAR(RR1,1)
C HERE BEGIN THE K0,PI+_ DECAY
      IF(JKST.EQ.10)THEN
C     ==================
C MASS OF (REAL/VIRTUAL) K*
        AMS1=(AMPI+AMKZ)**2
        AMS2=(AMTAU-AMNUTA)**2
C FLAT PHASE SPACE
C       AMX2=AMS1+   RR1*(AMS2-AMS1)
C       AMX=SQRT(AMX2)
C       PHSPAC=PHSPAC*(AMS2-AMS1)
C PHASE SPACE WITH SAMPLING FOR K* RESONANCE
        ALP1=ATAN((AMS1-AMKST**2)/AMKST/GAMKST)
        ALP2=ATAN((AMS2-AMKST**2)/AMKST/GAMKST)
        ALP=ALP1+RR1*(ALP2-ALP1)
        AMX2=AMKST**2+AMKST*GAMKST*TAN(ALP)
        AMX=SQRT(AMX2)
        PHSPAC=PHSPAC*((AMX2-AMKST**2)**2+(AMKST*GAMKST)**2)
     &                /(AMKST*GAMKST)
        PHSPAC=PHSPAC*(ALP2-ALP1)
C
C TAU-NEUTRINO MOMENTUM
        PN(1)=0
        PN(2)=0
        PN(4)=1./(2*AMTAU)*(AMTAU**2+AMNUTA**2-AMX**2)
        PN(3)=-SQRT((PN(4)-AMNUTA)*(PN(4)+AMNUTA))
C
C K* MOMENTUM
        PKS(1)=0
        PKS(2)=0
        PKS(4)=1./(2*AMTAU)*(AMTAU**2-AMNUTA**2+AMX**2)
        PKS(3)=-PN(3)
        PHSPAC=PHSPAC*(4*PI)*(2*PKS(3)/AMTAU)
C
CAM
        ENPI=( AMX**2+AMPI**2-AMKZ**2 ) / ( 2*AMX )
        PPPI=SQRT((ENPI-AMPI)*(ENPI+AMPI))
        PHSPAC=PHSPAC*(4*PI)*(2*PPPI/AMX)
C CHARGED PI MOMENTUM IN KAON* REST FRAME
        CALL SPHERA(PPPI,PPI)
        PPI(4)=ENPI
C NEUTRAL KAON MOMENTUM IN K* REST FRAME
        DO 20 I=1,3
20      PKK(I)=-PPI(I)
        PKK(4)=( AMX**2+AMKZ**2-AMPI**2 ) / ( 2*AMX )
        EXE=(PKS(4)+PKS(3))/AMX
C PION AND K  BOOSTED FROM K* REST FRAME TO TAU REST FRAME
        CALL BOSTR3(EXE,PPI,PPI)
        CALL BOSTR3(EXE,PKK,PKK)
        DO 30 I=1,4
30      QQ(I)=PPI(I)-PKK(I)
C QQ transverse to PKS
        PKSD =PKS(4)*PKS(4)-PKS(3)*PKS(3)-PKS(2)*PKS(2)-PKS(1)*PKS(1)
        QQPKS=PKS(4)* QQ(4)-PKS(3)* QQ(3)-PKS(2)* QQ(2)-PKS(1)* QQ(1)
        DO 31 I=1,4
31      QQ(I)=QQ(I)-PKS(I)*QQPKS/PKSD
C AMPLITUDE
        PRODPQ=PT(4)*QQ(4)
        PRODNQ=PN(4)*QQ(4)-PN(1)*QQ(1)-PN(2)*QQ(2)-PN(3)*QQ(3)
        PRODPN=PT(4)*PN(4)
        QQ2= QQ(4)**2-QQ(1)**2-QQ(2)**2-QQ(3)**2
        BRAK=(GV**2+GA**2)*(2*PRODPQ*PRODNQ-PRODPN*QQ2)
     &      +(GV**2-GA**2)*AMTAU*AMNUTA*QQ2
C A SIMPLE BREIT-WIGNER IS CHOSEN FOR K* RESONANCE
        FKS=CABS(BWIGS(AMX2,AMKST,GAMKST))**2
        AMPLIT=(GFERMI*SCABIB)**2*BRAK*2*FKS
        DGAMT=1/(2.*AMTAU)*AMPLIT*PHSPAC
        DO 40 I=1,3
 40     HV(I)=2*GV*GA*AMTAU*(2*PRODNQ*QQ(I)-QQ2*PN(I))/BRAK
C
C HERE BEGIN THE K+-,PI0 DECAY
      ELSEIF(JKST.EQ.20)THEN
C     ======================
C MASS OF (REAL/VIRTUAL) K*
        AMS1=(AMPIZ+AMK)**2
        AMS2=(AMTAU-AMNUTA)**2
C FLAT PHASE SPACE
C       AMX2=AMS1+   RR1*(AMS2-AMS1)
C       AMX=SQRT(AMX2)
C       PHSPAC=PHSPAC*(AMS2-AMS1)
C PHASE SPACE WITH SAMPLING FOR K* RESONANCE
        ALP1=ATAN((AMS1-AMKST**2)/AMKST/GAMKST)
        ALP2=ATAN((AMS2-AMKST**2)/AMKST/GAMKST)
        ALP=ALP1+RR1*(ALP2-ALP1)
        AMX2=AMKST**2+AMKST*GAMKST*TAN(ALP)
        AMX=SQRT(AMX2)
        PHSPAC=PHSPAC*((AMX2-AMKST**2)**2+(AMKST*GAMKST)**2)
     &                /(AMKST*GAMKST)
        PHSPAC=PHSPAC*(ALP2-ALP1)
C
C TAU-NEUTRINO MOMENTUM
        PN(1)=0
        PN(2)=0
        PN(4)=1./(2*AMTAU)*(AMTAU**2+AMNUTA**2-AMX**2)
        PN(3)=-SQRT((PN(4)-AMNUTA)*(PN(4)+AMNUTA))
C KAON* MOMENTUM
        PKS(1)=0
        PKS(2)=0
        PKS(4)=1./(2*AMTAU)*(AMTAU**2-AMNUTA**2+AMX**2)
        PKS(3)=-PN(3)
        PHSPAC=PHSPAC*(4*PI)*(2*PKS(3)/AMTAU)
C
CAM
        ENPI=( AMX**2+AMPIZ**2-AMK**2 ) / ( 2*AMX )
        PPPI=SQRT((ENPI-AMPIZ)*(ENPI+AMPIZ))
        PHSPAC=PHSPAC*(4*PI)*(2*PPPI/AMX)
C NEUTRAL PI MOMENTUM IN K* REST FRAME
        CALL SPHERA(PPPI,PPI)
        PPI(4)=ENPI
C CHARGED KAON MOMENTUM IN K* REST FRAME
        DO 50 I=1,3
50      PKK(I)=-PPI(I)
        PKK(4)=( AMX**2+AMK**2-AMPIZ**2 ) / ( 2*AMX )
        EXE=(PKS(4)+PKS(3))/AMX
C PION AND K  BOOSTED FROM K* REST FRAME TO TAU REST FRAME
        CALL BOSTR3(EXE,PPI,PPI)
        CALL BOSTR3(EXE,PKK,PKK)
        DO 60 I=1,4
60      QQ(I)=PKK(I)-PPI(I)
C QQ transverse to PKS
        PKSD =PKS(4)*PKS(4)-PKS(3)*PKS(3)-PKS(2)*PKS(2)-PKS(1)*PKS(1)
        QQPKS=PKS(4)* QQ(4)-PKS(3)* QQ(3)-PKS(2)* QQ(2)-PKS(1)* QQ(1)
        DO 61 I=1,4
61      QQ(I)=QQ(I)-PKS(I)*QQPKS/PKSD
C AMPLITUDE
        PRODPQ=PT(4)*QQ(4)
        PRODNQ=PN(4)*QQ(4)-PN(1)*QQ(1)-PN(2)*QQ(2)-PN(3)*QQ(3)
        PRODPN=PT(4)*PN(4)
        QQ2= QQ(4)**2-QQ(1)**2-QQ(2)**2-QQ(3)**2
        BRAK=(GV**2+GA**2)*(2*PRODPQ*PRODNQ-PRODPN*QQ2)
     &      +(GV**2-GA**2)*AMTAU*AMNUTA*QQ2
C A SIMPLE BREIT-WIGNER IS CHOSEN FOR THE K* RESONANCE
        FKS=CABS(BWIGS(AMX2,AMKST,GAMKST))**2
        AMPLIT=(GFERMI*SCABIB)**2*BRAK*2*FKS
        DGAMT=1/(2.*AMTAU)*AMPLIT*PHSPAC
        DO 70 I=1,3
 70     HV(I)=2*GV*GA*AMTAU*(2*PRODNQ*QQ(I)-QQ2*PN(I))/BRAK
      ENDIF
      RETURN
      END
      SUBROUTINE DEXNPI(MODE,ISGN,PNU,PWB,PNPI,JNPI)
C ----------------------------------------------------------------------
C
C     called by : DEXAY,DEXAY1
C ----------------------------------------------------------------------
      PARAMETER (NMODE=4)
      COMMON / TAUNPI / CBRNPI       ,AMAS
     &                 ,KPI(6,NMODE) ,MULT(NMODE)
      REAL*4            CBRNPI(NMODE),AMAS(6,NMODE)
      REAL*4  PNU(4),PWB(4),PNPI(4,6)
C
      IF(MODE.EQ.-1) THEN
C     ===================
        DO 1 JNPI=1,NMODE
C         CALL HBOOK1(1070+10*JNPI+1,
C    &    'NEUTRINO ENERGY FROM MULTIPI$', 100,.0,50.,.0)
C         CALL HBOOK1(1070+10*JNPI+2,
C    &    'VIRTUAL W MASS  FROM MULTIPI$', 100,.0,2.0,0.)
1       CONTINUE
        CALL DADNPI(-1,ISGN,PNU,PWB,PNPI,JNPI)
C
      ELSEIF(MODE.EQ. 0) THEN
C     =======================
        CALL DADNPI(0,ISGN,PNU,PWB,PNPI,JNPI)
C
      ELSEIF(MODE.EQ. 1) THEN
C     =======================
        CALL DADNPI(1,ISGN,PNU,PWB,PNPI,JNPI)
C
      ENDIF
C     =====
      RETURN
      END
      SUBROUTINE DADNPI(MODE,ISGN,PNU,PWB,PNPI,JNPI)
C ----------------------------------------------------------------------
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
      COMMON / DECPAR / GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      REAL*4            GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      COMMON / TAUBMC / GAMPMC(30),GAMPER(30),NEVDEC(30)
      REAL*4            GAMPMC    ,GAMPER
      COMMON / INOUT / INUT,IOUT
      PARAMETER (NMODE=4)
      COMMON / TAUNPI / CBRNPI       ,AMAS
     &                 ,KPI(6,NMODE) ,MULT(NMODE)
      REAL*4            CBRNPI(NMODE),AMAS(6,NMODE)
      REAL*4 PNU(4),PWB(4),PNPI(4,6)
      REAL*4 PDUM1(4),PDUM2(4),PDUMI(4,6)
      REAL*4 RRR(3)
      REAL*4 WTMAX(NMODE)
      REAL*8              SWT(NMODE),SSWT(NMODE)
      DIMENSION NEVRAW(NMODE),NEVOVR(NMODE),NEVACC(NMODE)
C
      DATA PI /3.141592653589793238462643/
      DATA IWARM/0/
C
      IF(MODE.EQ.-1) THEN
C     ===================
        IWARM=1
C       PRINT 7003
        DO 1 JNPI=1,NMODE
        NEVRAW(JNPI)=0
        NEVACC(JNPI)=0
        NEVOVR(JNPI)=0
        SWT(JNPI)=0
        SSWT(JNPI)=0
        WTMAX(JNPI)=-1.
        DO 3 I=1,500
        CALL DPHNPI(WT,PDUM1,PDUM2,PDUMI,JNPI)
        IF(WT.GT.WTMAX(JNPI)/1.2) WTMAX(JNPI)=WT*1.2
3       CONTINUE
C       CALL HBOOK1(801,'WEIGHT DISTRIBUTION  DADNPI    $',100,0.,2.,.0)
C       PRINT 7004,WTMAX(JNPI)
1       CONTINUE
        WRITE(IOUT,7005)
C
      ELSEIF(MODE.EQ. 0) THEN
C     =======================
        IF(IWARM.EQ.0) GOTO 902
C       FIRST CHOOSE DECAY MODE
        CALL RANMAR(RM,1)
        DO 200 JNPI=1,NMODE
        IF(RM.LT.CBRNPI(JNPI)) GO TO 210
200     CONTINUE
210     CONTINUE
C
300     CONTINUE
        CALL DPHNPI(WT,PNU,PWB,PNPI,JNPI)
C       CALL HFILL(801,WT/WTMAX(JNPI))
        NEVRAW(JNPI)=NEVRAW(JNPI)+1
        SWT(JNPI)=SWT(JNPI)+WT
        SSWT(JNPI)=SSWT(JNPI)+WT**2
        CALL RANMAR(RRR,3)
        RN=RRR(1)
        IF(WT.GT.WTMAX(JNPI)) NEVOVR(JNPI)=NEVOVR(JNPI)+1
        IF(RN*WTMAX(JNPI).GT.WT) GOTO 300
C ROTATIONS TO BASIC TAU REST FRAME
        COSTHE=-1.+2.*RRR(2)
        THET=ACOS(COSTHE)
        PHI =2*PI*RRR(3)
        CALL ROTOR2(THET,PNU,PNU)
        CALL ROTOR3( PHI,PNU,PNU)
        CALL ROTOR2(THET,PWB,PWB)
        CALL ROTOR3( PHI,PWB,PWB)
        ND=MULT(JNPI)
        DO 301 I=1,ND
        CALL ROTOR2(THET,PNPI(1,I),PNPI(1,I))
        CALL ROTOR3( PHI,PNPI(1,I),PNPI(1,I))
301     CONTINUE
        NEVACC(JNPI)=NEVACC(JNPI)+1
C
      ELSEIF(MODE.EQ. 1) THEN
C     =======================
        DO 500 JNPI=1,NMODE
          IF(NEVRAW(JNPI).EQ.0) RETURN
          PARGAM=SWT(JNPI)/FLOAT(NEVRAW(JNPI)+1)
          ERROR=0
          IF(NEVRAW(JNPI).NE.0)
     &    ERROR=SQRT(SSWT(JNPI)/SWT(JNPI)**2-1./FLOAT(NEVRAW(JNPI)))
          RAT=PARGAM/GAMEL
          WRITE(IOUT, 7010)
     &     NEVRAW(JNPI),NEVACC(JNPI),NEVOVR(JNPI),PARGAM,RAT,ERROR
CC        CALL HPRINT(801)
          GAMPMC(8+JNPI-1)=RAT
          GAMPER(8+JNPI-1)=ERROR
CAM       NEVDEC(8+JNPI-1)=NEVACC(JNPI)
  500     CONTINUE
      ENDIF
C     =====
      RETURN
 7003 FORMAT(///1X,15(5H*****)
     $ /,' *',     25X,'******** DADNPI INITIALISATION ********',9X,1H*
     $ )
 7004 FORMAT(' *',E20.5,5X,'WTMAX  = MAXIMUM WEIGHT  ',9X,1H*/)
 7005 FORMAT(
     $  /,1X,15(5H*****)/)
 7010 FORMAT(///1X,15(5H*****)
     $ /,' *',     25X,'******** DADNPI FINAL REPORT  ******** ',9X,1H*
     $ /,' *',I20  ,5X,'NEVRAW = NO. OF MULTIPI DECAYS TOTAL   ',9X,1H*
     $ /,' *',I20  ,5X,'NEVACC = NO. OF MULTIPI DECS. ACCEPTED ',9X,1H*
     $ /,' *',I20  ,5X,'NEVOVR = NO. OF OVERWEIGHTED EVENTS    ',9X,1H*
     $ /,' *',E20.5,5X,'PART. WIDTH (MULTIPI DEC) IN GEV UNITS ',9X,1H*
     $ /,' *',F20.9,5X,'IN UNITS GFERMI**2*MASS**5/192/PI**3   ',9X,1H*
     $ /,' *',F20.8,5X,'RELATIVE ERROR OF PARTIAL WIDTH        ',9X,1H*
     $  /,1X,15(5H*****)/)
 902  WRITE(IOUT, 9020)
 9020 FORMAT(' ----- DADNPI: LACK OF INITIALISATION')
      STOP
      END
      SUBROUTINE DPHNPI(DGAMT,PN,PR,PPI,JNPI)
C ----------------------------------------------------------------------
C IT SIMULATES MULTIPI DECAY IN TAU REST FRAME WITH
C Z-AXIS OPPOSITE TO NEUTRINO MOMENTUM
C ----------------------------------------------------------------------
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
      COMMON / DECPAR / GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      REAL*4            GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      PARAMETER (NMODE=4)
      COMMON / TAUNPI / CBRNPI       ,AMAS
     &                 ,KPI(6,NMODE) ,MULT(NMODE)
      REAL*4            CBRNPI(NMODE),AMAS(6,NMODE)
C
      REAL  PN(4),PR(4),PPI(4,6)
      REAL  PV(5,6),PT(4),UE(3),BE(3)
      REAL*4 RRR(6)
C
      DATA PI /3.141592653589793238462643/
C
      PAWT(A,B,C)=SQRT((A**2-(B+C)**2)*(A**2-(B-C)**2))/(2.*A)
      PAW2(A,B,C)=(A**2-(B+C)**2)*(A**2-(B-C)**2)/(4.*A*A)
C
C
C
C TAU MOMENTUM
      PT(1)=0.
      PT(2)=0.
      PT(3)=0.
      PT(4)=AMTAU
C
C MASS OF VIRTUAL W
      ND=MULT(JNPI)
      PS=0.
      PHSPAC = 1./2.**5 /PI**2
      DO 4 I=1,ND
4     PS  =PS+AMAS(I,JNPI)
      CALL RANMAR(RR1,1)
      AMS1=PS**2
      AMS2=(AMTAU-AMNUTA)**2
C
CAM   FLAT PHASE SPACE  !!!
C
      AMX2=AMS1+   RR1*(AMS2-AMS1)
      AMX =SQRT(AMX2)
      AMW =AMX
      PHSPAC=PHSPAC * (AMS2-AMS1)
C
C TAU-NEUTRINO MOMENTUM
      PN(1)=0
      PN(2)=0
      PN(4)=1./(2*AMTAU)*(AMTAU**2+AMNUTA**2-AMX2)
      PN(3)=-SQRT((PN(4)-AMNUTA)*(PN(4)+AMNUTA))
C W MOMENTUM
      PR(1)=0
      PR(2)=0
      PR(4)=1./(2*AMTAU)*(AMTAU**2-AMNUTA**2+AMX2)
      PR(3)=-PN(3)
      PHSPAC=PHSPAC * (4.*PI) * (2.*PR(3)/AMTAU)
C
C AMPLITUDE  (cf YS.Tsai Phys.Rev.D4,2821(1971)
C    or F.Gilman SH.Rhie Phys.Rev.D31,1066(1985)
C
        PXQ=AMTAU*PR(4)
        PXN=AMTAU*PN(4)
        QXN=PR(4)*PN(4)-PR(1)*PN(1)-PR(2)*PN(2)-PR(3)*PN(3)
        BRAK=2*(GV**2+GA**2)*(2*PXQ*PXN+AMX2*QXN)
     &      -6*(GV**2-GA**2)*AMTAU*AMNUTA*AMX2
CAM     Assume neutrino mass=0. and sum over final polarisation
C     BRAK= 2*(AMTAU**2-AMX2) * (AMTAU**2+2.*AMX2)
      AMPLIT=CCABIB**2*GFERMI**2/2. * BRAK * AMX2*SIGEE(AMX2,JNPI)
      DGAMT=1./(2.*AMTAU)*AMPLIT*PHSPAC
C
C   ISOTROPIC W DECAY IN W REST FRAME
      PHSMAX = 1.
      DO 200 I=1,4
  200 PV(I,1)=PR(I)
      PV(5,1)=AMW
      PV(5,ND)=AMAS(ND,JNPI)
C    COMPUTE MAX. PHASE SPACE FACTOR
      PMAX=AMW-PS+AMAS(ND,JNPI)
      PMIN=.0
      DO 220 IL=ND-1,1,-1
      PMAX=PMAX+AMAS(IL,JNPI)
      PMIN=PMIN+AMAS(IL+1,JNPI)
CB220 PHSMAX=PHSMAX*PAWT(PMAX,PMIN,AMAS(IL,JNPI))/PMAX
      XTEMP = PAW2(PMAX,PMIN,AMAS(IL,JNPI))
      IF ( XTEMP.GT.0.) THEN
         XTEMP = SQRT(XTEMP)
      ELSE
         XTEMP = 0.
      ENDIF
  220 PHSMAX=PHSMAX*XTEMP/PMAX
  100 CONTINUE
CAM  GENERATE ND-2 EFFECTIVE MASSES
      PHS=1.
      PHSPAC = 1./2.**(6*ND-7) /PI**(3*ND-4)
      AMX=AMW
      CALL RANMAR(RRR,ND-2)
      DO 230 IL=1,ND-2
      AMS1=.0
      DO 231 JL=IL+1,ND
  231 AMS1=AMS1+AMAS(JL,JNPI)
      AMS1=AMS1**2
      AMS2=(AMX-AMAS(IL,JNPI))**2
      RR1=RRR(IL)
      AMX2=AMS1+  RR1*(AMS2-AMS1)
      AMX=SQRT(AMX2)
      PV(5,IL+1)=AMX
      PHSPAC=PHSPAC * (AMS2-AMS1)
CBB   PA=PAWT(PV(5,IL),PV(5,IL+1),AMAS(IL,JNPI))
      XTEMP = PAW2(PV(5,IL),PV(5,IL+1),AMAS(IL,JNPI))
      IF ( XTEMP.GT.0.) THEN
         XTEMP = SQRT(XTEMP)
      ELSE
         XTEMP = 0.
      ENDIF
      PA= XTEMP
      PHS   =PHS    *PA/PV(5,IL)
  230 CONTINUE
      CALL RANMAR(RN,1)
      IF(RN*PHSMAX.GT.PHS) GO TO 100
C...PERFORM SUCCESSIVE TWO-PARTICLE DECAYS IN RESPECTIVE CM FRAME
  280 DO 300 IL=1,ND-1
CBB   PA=PAWT(PV(5,IL),PV(5,IL+1),AMAS(IL,JNPI))
      XTEMP = PAW2(PV(5,IL),PV(5,IL+1),AMAS(IL,JNPI))
      IF ( XTEMP.GT.0.) THEN
         XTEMP = SQRT(XTEMP)
      ELSE
         XTEMP = 0.
      ENDIF
      PA= XTEMP
      CALL RANMAR(RRR,2)
      UE(3)=2.*RRR(1)-1.
      PHI=2.*PI*RRR(2)
      UE(1)=SQRT(1.-UE(3)**2)*COS(PHI)
      UE(2)=SQRT(1.-UE(3)**2)*SIN(PHI)
      DO 290 J=1,3
      PPI(J,IL)=PA*UE(J)
  290 PV(J,IL+1)=-PA*UE(J)
      PPI(4,IL)=SQRT(PA**2+AMAS(IL,JNPI)**2)
      PV(4,IL+1)=SQRT(PA**2+PV(5,IL+1)**2)
      PHSPAC=PHSPAC *(4.*PI)*(2.*PA/PV(5,IL))
  300 CONTINUE
C...LORENTZ TRANSFORM DECAY PRODUCTS TO TAU FRAME
      DO 310 J=1,4
  310 PPI(J,ND)=PV(J,ND)
      DO 340 IL=ND-1,1,-1
      DO 320 J=1,3
  320 BE(J)=PV(J,IL)/PV(4,IL)
      GAM=PV(4,IL)/PV(5,IL)
      DO 340 I=IL,ND
      BEP=BE(1)*PPI(1,I)+BE(2)*PPI(2,I)+BE(3)*PPI(3,I)
      DO 330 J=1,3
  330 PPI(J,I)=PPI(J,I)+GAM*(GAM*BEP/(1.+GAM)+PPI(4,I))*BE(J)
      PPI(4,I)=GAM*(PPI(4,I)+BEP)
  340 CONTINUE
C
      RETURN
      END
      FUNCTION SIGEE(Q2,JNPI)
C ----------------------------------------------------------------------
C  e+e- cross section in the (1.GEV2,AMTAU**2) region
C  normalised to sig0 = 4/3 pi alfa2
C  used in matrix element for multipion tau decays
C  cf YS.Tsai        Phys.Rev D4 ,2821(1971)
C     F.Gilman et al Phys.Rev D17,1846(1978)
C     C.Kiesling, to be pub. in High Energy e+e- Physics (1988)
C  DATSIG(*,1) = e+e- -> pi+pi-2pi0
C  DATSIG(*,2) = e+e- -> 2pi+2pi-
C  DATSIG(*,3) = 5-pion contribution (a la TN.Pham et al)
C                (Phys Lett 78B,623(1978)
C  DATSIG(*,4) = e+e- -> 6pi
C
C  4- and 6-pion cross sections from data
C  5-pion contribution related to 4-pion cross section
C
C     Called by DPHNPI
C ----------------------------------------------------------------------
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
      REAL*4 DATSIG(17,4)
C
      DATA DATSIG/
     1  7.40,12.00,16.15,21.25,24.90,29.55,34.15,37.40,37.85,37.40,
     2 36.00,33.25,30.50,27.70,24.50,21.25,18.90,
     3  1.24, 2.50, 3.70, 5.40, 7.45,10.75,14.50,18.20,22.30,28.90,
     4 29.35,25.60,22.30,18.60,14.05,11.60, 9.10,
     5 17*.0,
     6 9*.0,.65,1.25,2.20,3.15,5.00,5.75,7.80,8.25/
      DATA SIG0 / 86.8 /
      DATA PI /3.141592653589793238462643/
      DATA INIT / 0 /
C
      IF(INIT.EQ.0) THEN
        INIT=1
        AMPI2=AMPI**2
        FPI = .943*AMPI
        DO 100 I=1,17
        DATSIG(I,2) = DATSIG(I,2)/2.
        DATSIG(I,1) = DATSIG(I,1) + DATSIG(I,2)
        S = 1.025+(I-1)*.05
        FACT=0.
        S2=S**2
        DO 200 J=1,17
        T= 1.025+(J-1)*.05
        IF(T . GT. S-AMPI ) GO TO 201
        T2=T**2
        FACT=(T2/S2)**2*SQRT((S2-T2-AMPI2)**2-4.*T2*AMPI2)/S2 *2.*T*.05
        FACT = FACT * (DATSIG(J,1)+DATSIG(J+1,1))
 200    DATSIG(I,3) = DATSIG(I,3) + FACT
 201    DATSIG(I,3) = DATSIG(I,3) /(2*PI*FPI)**2
 100    CONTINUE
C       WRITE(6,1000) DATSIG
 1000   FORMAT(///1X,' EE SIGMA USED IN MULTIPI DECAYS'/
     %        (17F7.2/))
      ENDIF
      Q=SQRT(Q2)
      QMIN=1.
      IF(Q.LT.QMIN) THEN
        SIGEE=DATSIG(1,JNPI)+
     &       (DATSIG(2,JNPI)-DATSIG(1,JNPI))*(Q-1.)/.05
      ELSEIF(Q.LT.1.8) THEN
        DO 1 I=1,16
        QMAX = QMIN + .05
        IF(Q.LT.QMAX) GO TO 2
        QMIN = QMIN + .05
 1      CONTINUE
 2      SIGEE=DATSIG(I,JNPI)+
     &       (DATSIG(I+1,JNPI)-DATSIG(I,JNPI)) * (Q-QMIN)/.05
      ELSEIF(Q.GT.1.8) THEN
        SIGEE=DATSIG(17,JNPI)+
     &       (DATSIG(17,JNPI)-DATSIG(16,JNPI)) * (Q-1.8)/.05
      ENDIF
      IF(SIGEE.LT..0) SIGEE=0.
C
      SIGEE = SIGEE/(6.*PI**2*SIG0)
C
      RETURN
      END
      SUBROUTINE TLUREB
C     *****************
C ---------------------------------------------------------------------
C FILLING LUND RECORD FOR KORAL-B
C
C THIS MUST BE CALLED JUST AFTER EVENT
C IT WRITES IN /LUJETS/ MOMENTA OF INITIAL BEAMS AND FINAL
C STATE MOMENTA OF TAU'S (MU'S ...) AND A PHOTON.
C---------------------------------------------------------
C
      PARAMETER (LJNPAR=4000)
      COMMON /LUJETS/ N,K(LJNPAR,5),P(LJNPAR,5),V7LU(LJNPAR,5)
      COMMON / UTIL4 / QP(4),QM(4),PH(4)
      COMMON / BEAMS / XPB1(4),XPB2(4),KFB1,KFB2
      COMMON / IDFC  / IDFF
      DATA ICONT/0/
      ICONT=ICONT+1
C INITIAL STATE.
C LUND IDENTIFIER FOR ELECTRON = 7
      LBEA1= KFB1
      LBEA2= KFB2
      AMEL=.511E-3
      CALL LEFILL(1,40000,LBEA1,XPB1,AMEL)
      CALL LEFILL(2,40000,LBEA2,XPB2,AMEL)
 
C
C OUTGOING LEPTONS
C LUND IDENT FOR TAU = 11
C LUND IDENT FOR  MU =  9
C
      IF(IABS(IDFF).EQ.11) THEN
C       outgoing decaying tau lepton
        KST = 20000
      ELSE
C       outgoing stable fermion
        KST = 0
      ENDIF
C
      AM=AMAS4(QP)
      CALL LEFILL(3,KST, IDFF,QP,AM)
      CALL LEFILL(4,KST,-IDFF,QM,AM)
C RADIATIVE PHOTON INCLUDE (IF ANY)
      IF (PH(4) .GT. 1E-6) THEN
        LPHOTO=1
        CALL LEFILL(0,10000,LPHOTO,PH,0.0)
      ENDIF
      RETURN
      END
      SUBROUTINE DWLUEL(KTO,ISGN,PNU,PWB,PEL,PNE)
C ----------------------------------------------------------------------
C LORENTZ TRANSF. TO CMS AND FILLING LUND RECORD
C Corrected by G. Bonneaud October 1988.
C              AM. Lutz    October 1988
C
C     called by : DEXAY,(DEKAY1,DEKAY2)
C ----------------------------------------------------------------------
      PARAMETER (LJNPAR=4000)
      COMMON /LUJETS/NPARLU,KODELU(LJNPAR,5),PARTLU(LJNPAR,5),
     &               v7lu(LJNPAR,5)
C

      REAL  PNU(4),PWB(4),PEL(4),PNE(4)
C
      IF(KTO.EQ. 1) THEN
        NPS=3
      ELSE
        NPS=4
      ENDIF
C TAU NEUTRINO
      CALL TRALO4(KTO,PNU,PNU,AM)
C GB  KHS=10000+NPS
      KHS=NPS
      CALL LEFILL(0,   KHS, 12*ISGN,PNU,AM)
C W BOSON MINUS(VIRTUAL)
      KHS=20000+NPS
      CALL TRALO4(KTO,PWB,PWB,AM)
      CALL LEFILL(0,   KHS, -3*ISGN,PWB,AM)
C ELECTRON
C GB  KHS=10000+NPARLU
      KHS=NPARLU
      CALL TRALO4(KTO,PEL,PEL,AM)
      CALL LEFILL(0,   KHS,  7*ISGN,PEL,AM)
C ELECTRON ANTI-NEUTRINO
      CALL TRALO4(KTO,PNE,PNE,AM)
      CALL LEFILL(0,   KHS, -8*ISGN,PNE,AM)
      RETURN
      END
      SUBROUTINE DWLUMU(KTO,ISGN,PNU,PWB,PMU,PNM)
C ----------------------------------------------------------------------
C LORENTZ TRANSF. TO CMS AND FILLING LUND RECORD
C Corrected by G. Bonneaud October 1988.
C              AM. Lutz    October 1988
C
C     called by : DEXAY,(DEKAY1,DEKAY2)
C ----------------------------------------------------------------------
      PARAMETER (LJNPAR=4000)
      COMMON /LUJETS/   NPARLU,KODELU(LJNPAR,5),PARTLU(LJNPAR,5),
     &                  v7lu(LJNPAR,5)
C
      REAL  PNU(4),PWB(4),PMU(4),PNM(4)
C
      IF(KTO.EQ. 1) THEN
        NPS=3
      ELSE
        NPS=4
      ENDIF
C TAU NEUTRINO
      CALL TRALO4(KTO,PNU,PNU,AM)
C GB  KHS=10000+NPS
      KHS=NPS
      CALL LEFILL(0,   KHS, 12*ISGN,PNU,AM)
C W BOSON MINUS(VIRTUAL)
      KHS=20000+NPS
      CALL TRALO4(KTO,PWB,PWB,AM)
      CALL LEFILL(0,   KHS, -3*ISGN,PWB,AM)
C MUON MINUS
C GB  KHS=10000+NPARLU
      KHS=NPARLU
      CALL TRALO4(KTO,PMU,PMU,AM)
      CALL LEFILL(0,   KHS,  9*ISGN,PMU,AM)
C MUON-ANTI-NEUTRINO
      CALL TRALO4(KTO,PNM,PNM,AM)
      CALL LEFILL(0,   KHS,-10*ISGN,PNM,AM)
      RETURN
      END
      SUBROUTINE DWLUPI(KTO,ISGN,PPI,PNU)
C ----------------------------------------------------------------------
C LORENTZ TRANSF. TO CMS AND FILLING LUND RECORD
C ISGN=1 FOR TAU- AND -1 FOR TAU+
C Corrected by G. Bonneaud October 1988.
C              AM. Lutz    October 1988
C
C     called by : DEXAY,(DEKAY1,DEKAY2)
C ----------------------------------------------------------------------
      REAL  PNU(4),PPI(4)
C
      IF(KTO.EQ. 1) THEN
        NPS=3
      ELSE
        NPS=4
      ENDIF
C TAU NEUTRINO
      CALL TRALO4(KTO,PNU,PNU,AM)
C GB  KHS=10000+NPS
      KHS=NPS
      CALL LEFILL(0,   KHS, 12*ISGN,PNU,AM)
C PI MINUS
      CALL TRALO4(KTO,PPI,PPI,AM)
C GB  KHS=10000+NPS
      KHS=NPS
      CALL LEFILL(0,   KHS,-17*ISGN,PPI,AM)
      RETURN
      END
      SUBROUTINE DWLURO(KTO,ISGN,PNU,PRHO,PIC,PIZ)
C ----------------------------------------------------------------------
C LORENTZ TRANSF. TO CMS AND FILLING LUND RECORD
C Corrected by G. Bonneaud October 1988.
C              AM. Lutz    October 1988
C
C     called by : DEXAY,(DEKAY1,DEKAY2)
C ----------------------------------------------------------------------
      PARAMETER (LJNPAR=4000)
      COMMON /LUJETS/   NPARLU,KODELU(LJNPAR,5),PARTLU(LJNPAR,5),
     &                  v7lu(LJNPAR,5)
C
      REAL  PNU(4),PRHO(4),PIC(4),PIZ(4)
C
      IF(KTO.EQ. 1) THEN
        NPS=3
      ELSE
        NPS=4
      ENDIF
C TAU NEUTRINO
      CALL TRALO4(KTO,PNU,PNU,AM)
C GB  KHS=10000+NPS
      KHS=NPS
      CALL LEFILL(0,   KHS, 12*ISGN,PNU,AM)
C RHO MINUS
      KHS=20000+NPS
      CALL TRALO4(KTO,PRHO,PRHO,AM)
      CALL LEFILL(0,   KHS,-27*ISGN,PRHO,AM)
C PI MINUS
C GB  KHS=10000+NPARLU
      KHS=NPARLU
      CALL TRALO4(KTO,PIC,PIC,AM)
      CALL LEFILL(0,   KHS,-17*ISGN,PIC,AM)
C PI ZERO
      CALL TRALO4(KTO,PIZ,PIZ,AM)
C GB  CALL LEFILL(0,   KHS, 23*ISGN,PIZ,AM)
      CALL LEFILL(0,   KHS, 23     ,PIZ,AM)
      RETURN
      END
      SUBROUTINE DWLUAA(KTO,ISGN,PNU,PAA,PIM1,PIM2,PIPL,JAA)
C ----------------------------------------------------------------------
C LORENTZ TRANSF. TO CMS AND FILLING LUND RECORD
C Corrected by G. Bonneaud October 1988.
C              AM. Lutz    October 1988
C
C     called by : DEXAY,(DEKAY1,DEKAY2)
C ----------------------------------------------------------------------
      COMMON / IDPART / IA1
      PARAMETER (LJNPAR=4000)
      COMMON /LUJETS/   NPARLU,KODELU(LJNPAR,5),PARTLU(LJNPAR,5),
     &                  v7lu(LJNPAR,5)
C
C
      REAL  PNU(4),PAA(4),PIM1(4),PIM2(4),PIPL(4)
      IF(KTO.EQ. 1) THEN
        NPS=3
      ELSE
        NPS=4
      ENDIF
C TAU NEUTRINO
      CALL TRALO4(KTO,PNU,PNU,AM)
C FB  KHS=10000+NPS
      KHS=NPS
      CALL LEFILL(0,   KHS, 12*ISGN,PNU,AM)
C A1  MINUS
      KHS=20000+NPS
      CALL TRALO4(KTO,PAA,PAA,AM)
      CALL LEFILL(0,   KHS,-IA1*ISGN,PAA,AM)
CAM
      IF(JAA.EQ.1) THEN
C PI MINUS 1
C FB    KHS=10000+NPARLU
        KHS=NPARLU
        CALL TRALO4(KTO,PIM1,PIM1,AM)
        CALL LEFILL(0,   KHS,-17*ISGN,PIM1,AM)
C PI MINUS 2
        CALL TRALO4(KTO,PIM2,PIM2,AM)
        CALL LEFILL(0,   KHS,-17*ISGN,PIM2,AM)
C PI PLUS
        CALL TRALO4(KTO,PIPL,PIPL,AM)
        CALL LEFILL(0,   KHS,+17*ISGN,PIPL,AM)
      ELSE
C PI0      1
C FB    KHS=10000+NPARLU
        KHS=NPARLU
        CALL TRALO4(KTO,PIM1,PIM1,AM)
C FB    CALL LEFILL(0,   KHS,-23*ISGN,PIM1,AM)
        CALL LEFILL(0,   KHS, 23,PIM1,AM)
C PI0      2
        CALL TRALO4(KTO,PIM2,PIM2,AM)
C FB    CALL LEFILL(0,   KHS,-23*ISGN,PIM2,AM)
        CALL LEFILL(0,   KHS, 23     ,PIM2,AM)
C PI MINUS
        CALL TRALO4(KTO,PIPL,PIPL,AM)
        CALL LEFILL(0,   KHS,-17*ISGN,PIPL,AM)
      ENDIF
      RETURN
      END
      SUBROUTINE DWLUKK (KTO,ISGN,PKK,PNU)
C ----------------------------------------------------------------------
CFZ
C LORENTZ TRANS. TO CMS AND FILLING LUND RECORD
C ISGN=1 FOR TAU- AND -1 FOR TAU+
C ----------------------------------------------------------------------
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
      REAL PKK(4),PNU(4)
C
      IF (KTO.EQ.1) THEN
        NPS=3
      ELSE
        NPS=4
      ENDIF
C TAU NEUTRINO
      CALL TRALO4 (KTO,PNU,PNU,AM)
C     KHS=10000+NPS
      KHS=NPS
      CALL LEFILL(0,KHS,12*ISGN,PNU,AM)
C KAON MINUS
      CALL TRALO4 (KTO,PKK,PKK,AM)
      KHS=NPS
      CALL LEFILL (0,KHS,-18*ISGN,PKK,AM)
      RETURN
      END
      SUBROUTINE DWLUKS(KTO,ISGN,PNU,PKS,PKK,PPI,JKST)
C ----------------------------------------------------------------------
C LORENTZ TRANSF. TO CMS AND FILLING LUND RECORD
C     JKST=10 CORRESPOND TO K0,PI- DECAY
C     JKST=20 CORRESPOND TO K-,PI0 DECAY
C ----------------------------------------------------------------------
      PARAMETER (LJNPAR=2000)
      COMMON /LUJETS/   NPARLU,KODELU(LJNPAR,5),PARTLU(LJNPAR,5),
     &                  v7lu(LJNPAR,5)
C
      REAL  PNU(4),PKS(4),PKK(4),PPI(4)
C
      IF(KTO.EQ. 1) THEN
        NPS=3
      ELSE
        NPS=4
      ENDIF
C TAU NEUTRINO
      CALL TRALO4(KTO,PNU,PNU,AM)
C FB  KHS=10000+NPS
      KHS=NPS
      CALL LEFILL(0,   KHS, 12*ISGN,PNU,AM)
C KSTAR MINUS
      KHS=20000+NPS
      CALL TRALO4(KTO,PKS,PKS,AM)
      CALL LEFILL(0,   KHS,-28*ISGN,PKS,AM)
      KHS=NPARLU
      IF(JKST.EQ.10) THEN
C     ===================
C PI MINUS
C FB  KHS=10000+NPARLU
        CALL TRALO4(KTO,PPI,PPI,AM)
        CALL LEFILL(0,   KHS,-17*ISGN,PPI,AM)
C KAON ZERO
CFZ TO USE GALEPH ; K0,K0B --> K0S K0L
        CALL RANMAR(XIO,1)
        IF(XIO.LT.0.5) THEN
          K0TYPE = 37
        ELSE
          K0TYPE = 38
        ENDIF
        CALL TRALO4(KTO,PKK,PKK,AM)
        CALL LEFILL(0,   KHS,K0TYPE,PKK,AM)
C
      ELSEIF(JKST.EQ.20) THEN
C     ======================
C PI ZERO
        CALL TRALO4(KTO,PPI,PPI,AM)
        CALL LEFILL(0,   KHS, 23,PPI,AM)
C KAON CHARGED
        CALL TRALO4(KTO,PKK,PKK,AM)
        CALL LEFILL(0,   KHS,-18*ISGN,PKK,AM)
      ENDIF
      RETURN
      END
      SUBROUTINE DWLNPI(KTO,ISGN,PNU,PWB,PNPI,JNPI)
C ----------------------------------------------------------------------
C LORENTZ TRANSF. TO CMS AND FILLING LUND RECORD
C              AM. Lutz    October 1988
C
C     called by : DEXAY,(DEKAY1,DEKAY2)
C ----------------------------------------------------------------------
      PARAMETER (LJNPAR=2000)
      COMMON /LUJETS/   NPARLU,KODELU(LJNPAR,5),PARTLU(LJNPAR,5),
     &                  v7lu(LJNPAR,5)
C
      PARAMETER (NMODE=4)
      COMMON / TAUNPI / CBRNPI       ,AMAS
     &                 ,KPI(6,NMODE) ,MULT(NMODE)
      REAL*4            CBRNPI(NMODE),AMAS(6,NMODE)
      REAL  PNU(4),PWB(4),PNPI(4,6)
      REAL  PPI(4)
      IF(KTO.EQ. 1) THEN
        NPS=3
      ELSE
        NPS=4
      ENDIF
C TAU NEUTRINO
      CALL TRALO4(KTO,PNU,PNU,AM)
      KHS=NPS
      CALL LEFILL(0,   KHS, 12*ISGN,PNU,AM)
C     CALL HFILL(1070+10*JNPI+1,PNU(4),1.,1.)
C W BOSON (VIRTUAL)
      KHS=20000+NPS
      CALL TRALO4(KTO,PWB,PWB,AM)
      CALL LEFILL(0,   KHS,-3*ISGN,PWB,AM)
C     CALL HFILL (1070+10*JNPI+2,AM,1.,1.)
CAM
C MULTIPIONS
      KHS=NPARLU
C     PRINT *,JNPI,MULT(JNPI),PNPI
      ND=MULT(JNPI)
      DO 100 I=1,ND
        KFS=KPI(I,JNPI)
        IF(KFS.NE.23) KFS=KFS*ISGN
        DO 101 J=1,4
 101    PPI(J)=PNPI(J,I)
        CALL TRALO4(KTO,PPI,PPI,AM)
        CALL LEFILL(0,    KHS, KFS,PPI,AM)
 100  CONTINUE
      RETURN
      END
      SUBROUTINE LEFILL(IP,KHS,KF,PP,AMASS)
C ----------------------------------------------------------------------
C THIS FILLS ONE ENTRY IN /LUJETS/
C     version 7.3 of commons ..... BBL dec 1996
C     called by : TLUREC,DWLUxx
C ----------------------------------------------------------------------
      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (LJNPAR=4000)
      COMMON /LUDAT1/MSTlU1(L1MST),PARlU1(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON /LUJETS/   NPARLU,KODELU(LJNPAR,5),PARTLU(LJNPAR,5),
     &                  v7lu(LJNPAR,5)
C
      REAL  PP(4)
      integer lu7co(40)
      data lu7co/22, 23, 24, 25, 22,  0, 11, 12, 13, 14,
     &           15, 16, 17, 18, 83,  0,211,321,311,421,
     &          411,431,111,221,331,441,213,323,313,423,
     &          413,433,113,223,333,443,310,130,  0,  0/
C
C FILL ONE PARTICLE (OR JET IF KF>=500)
      IF(IP.GE.MSTLU1( 4)-5 )   MSTLU1(24)=1
      IF(IP.EQ.0) THEN
C APPEND ONE ENTRY IN LUJETS
        IR=NPARLU+1
        NPARLU=NPARLU+1
      ELSE
C OVERWRITE OR WRITE ONE ENTRY IN LUJETS
        IR=IP
        NPARLU=IP
      ENDIF
      ks = khs/10000
      km = mod(khs,10000)
      if ( ks.le.1) then
         ks = ks+1
      else if ( ks.le.3) then
         ks = ks+9
      else if ( ks.ge.4) then
         ks = 21
      endif
      kfc = kf
      if(abs(kf).le.40) kfc = lu7co(abs(kf))*kf/abs(kf)
      KODELU(IR,1)=KS
      KODELU(IR,2)=KFc
      KODELU(IR,3)=Km
      DO 20 I=1,4
20    PARTLU(IR,I)=PP(I)
      PARTLU(IR,5)=AMASS
      RETURN
      END 
      FUNCTION AMAST(PP)
C ----------------------------------------------------------------------
C CALCULATES MASS OF PP (DOUBLE PRECISION)
C
C     USED BY : RADKOR
C ----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8  PP(4)
      AAA=PP(4)**2-PP(3)**2-PP(2)**2-PP(1)**2
C
      IF(AAA.NE.0.0) AAA=AAA/SQRT(ABS(AAA))
      AMAST=AAA
      RETURN
      END
      FUNCTION AMAS4(PP)
C     ******************
C ----------------------------------------------------------------------
C CALCULATES MASS OF PP
C
C     USED BY :
C ----------------------------------------------------------------------
      REAL  PP(4)
      AAA=PP(4)**2-PP(3)**2-PP(2)**2-PP(1)**2
      IF(AAA.NE.0.0) AAA=AAA/SQRT(ABS(AAA))
      AMAS4=AAA
      RETURN
      END
      FUNCTION ANGXY(X,Y)
C ----------------------------------------------------------------------
C
C     USED BY : KORALZ RADKOR
C ----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA PI /3.141592653589793238462643D0/
C
      IF(ABS(Y).LT.ABS(X)) THEN
        THE=ATAN(ABS(Y/X))
        IF(X.LE.0D0) THE=PI-THE
      ELSE
        THE=ACOS(X/SQRT(X**2+Y**2))
      ENDIF
      ANGXY=THE
      RETURN
      END
      FUNCTION ANGFI(X,Y)
C ----------------------------------------------------------------------
* CALCULATES ANGLE IN (0,2*PI) RANGE OUT OF X-Y
C
C     USED BY : KORALZ RADKOR
C ----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA PI /3.141592653589793238462643D0/
C
      IF(ABS(Y).LT.ABS(X)) THEN
        THE=ATAN(ABS(Y/X))
        IF(X.LE.0D0) THE=PI-THE
      ELSE
        THE=ACOS(X/SQRT(X**2+Y**2))
      ENDIF
      IF(Y.LT.0D0) THE=2D0*PI-THE
      ANGFI=THE
      END
      SUBROUTINE ROTOD1(PH1,PVEC,QVEC)
C ----------------------------------------------------------------------
C
C     USED BY : KORALZ
C ----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PVEC(4),QVEC(4),RVEC(4)
C
      PHI=PH1
      CS=COS(PHI)
      SN=SIN(PHI)
      DO 10 I=1,4
  10  RVEC(I)=PVEC(I)
      QVEC(1)=RVEC(1)
      QVEC(2)= CS*RVEC(2)-SN*RVEC(3)
      QVEC(3)= SN*RVEC(2)+CS*RVEC(3)
      QVEC(4)=RVEC(4)
      RETURN
      END
      SUBROUTINE ROTOD2(PH1,PVEC,QVEC)
C ----------------------------------------------------------------------
C
C     USED BY : KORALZ RADKOR
C ----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PVEC(4),QVEC(4),RVEC(4)
C
      PHI=PH1
      CS=COS(PHI)
      SN=SIN(PHI)
      DO 10 I=1,4
  10  RVEC(I)=PVEC(I)
      QVEC(1)= CS*RVEC(1)+SN*RVEC(3)
      QVEC(2)=RVEC(2)
      QVEC(3)=-SN*RVEC(1)+CS*RVEC(3)
      QVEC(4)=RVEC(4)
      RETURN
      END
      SUBROUTINE ROTOD3(PH1,PVEC,QVEC)
C ----------------------------------------------------------------------
C
C     USED BY : KORALZ RADKOR
C ----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION PVEC(4),QVEC(4),RVEC(4)
      PHI=PH1
      CS=COS(PHI)
      SN=SIN(PHI)
      DO 10 I=1,4
  10  RVEC(I)=PVEC(I)
      QVEC(1)= CS*RVEC(1)-SN*RVEC(2)
      QVEC(2)= SN*RVEC(1)+CS*RVEC(2)
      QVEC(3)=RVEC(3)
      QVEC(4)=RVEC(4)
      END
      SUBROUTINE BOSTR3(EXE,PVEC,QVEC)
C ----------------------------------------------------------------------
C BOOST ALONG Z AXIS, EXE=EXP(ETA), ETA= HIPERBOLIC VELOCITY.
C
C     USED BY : TAUOLA KORALZ (?)
C ----------------------------------------------------------------------
      REAL*4 PVEC(4),QVEC(4),RVEC(4)
C
      DO 10 I=1,4
  10  RVEC(I)=PVEC(I)
      RPL=RVEC(4)+RVEC(3)
      RMI=RVEC(4)-RVEC(3)
      QPL=RPL*EXE
      QMI=RMI/EXE
      QVEC(1)=RVEC(1)
      QVEC(2)=RVEC(2)
      QVEC(3)=(QPL-QMI)/2
      QVEC(4)=(QPL+QMI)/2
      END
      SUBROUTINE BOSTD3(EXE,PVEC,QVEC)
C ----------------------------------------------------------------------
C BOOST ALONG Z AXIS, EXE=EXP(ETA), ETA= HIPERBOLIC VELOCITY.
C
C     USED BY : KORALZ RADKOR
C ----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PVEC(4),QVEC(4),RVEC(4)
C
      DO 10 I=1,4
  10  RVEC(I)=PVEC(I)
      RPL=RVEC(4)+RVEC(3)
      RMI=RVEC(4)-RVEC(3)
      QPL=RPL*EXE
      QMI=RMI/EXE
      QVEC(1)=RVEC(1)
      QVEC(2)=RVEC(2)
      QVEC(3)=(QPL-QMI)/2
      QVEC(4)=(QPL+QMI)/2
      RETURN
      END
      SUBROUTINE ROTOR1(PH1,PVEC,QVEC)
C ----------------------------------------------------------------------
C
C     called by :
C ----------------------------------------------------------------------
      REAL*4 PVEC(4),QVEC(4),RVEC(4)
C
      PHI=PH1
      CS=COS(PHI)
      SN=SIN(PHI)
      DO 10 I=1,4
  10  RVEC(I)=PVEC(I)
      QVEC(1)=RVEC(1)
      QVEC(2)= CS*RVEC(2)-SN*RVEC(3)
      QVEC(3)= SN*RVEC(2)+CS*RVEC(3)
      QVEC(4)=RVEC(4)
      END
      SUBROUTINE ROTOR2(PH1,PVEC,QVEC)
C ----------------------------------------------------------------------
C
C     USED BY : TAUOLA
C ----------------------------------------------------------------------
      IMPLICIT REAL*4(A-H,O-Z)
      REAL*4 PVEC(4),QVEC(4),RVEC(4)
C
      PHI=PH1
      CS=COS(PHI)
      SN=SIN(PHI)
      DO 10 I=1,4
  10  RVEC(I)=PVEC(I)
      QVEC(1)= CS*RVEC(1)+SN*RVEC(3)
      QVEC(2)=RVEC(2)
      QVEC(3)=-SN*RVEC(1)+CS*RVEC(3)
      QVEC(4)=RVEC(4)
      END
      SUBROUTINE ROTOR3(PHI,PVEC,QVEC)
C ----------------------------------------------------------------------
C
C     USED BY : TAUOLA
C ----------------------------------------------------------------------
      REAL*4 PVEC(4),QVEC(4),RVEC(4)
C
      CS=COS(PHI)
      SN=SIN(PHI)
      DO 10 I=1,4
  10  RVEC(I)=PVEC(I)
      QVEC(1)= CS*RVEC(1)-SN*RVEC(2)
      QVEC(2)= SN*RVEC(1)+CS*RVEC(2)
      QVEC(3)=RVEC(3)
      QVEC(4)=RVEC(4)
      END
      SUBROUTINE RONMAR(RVEC,LENV)
C     SUBROUTINE RANMAR(RVEC,LENV)
C ----------------------------------------------------------------------
C<<<<<FUNCTION RANMAR(IDUMM)
C CERNLIB V113, VERSION WITH AUTOMATIC DEFAULT INITIALIZATION
C     Transformed to SUBROUTINE to be as in CERNLIB
C     AM.Lutz   November 1988, Feb. 1989
C
C!Universal random number generator proposed by Marsaglia and Zaman
C in report FSU-SCRI-87-50
C        modified by F. James, 1988 and 1989, to generate a vector
C        of pseudorandom numbers RVEC of length LENV, and to put in
C        the COMMON block everything needed to specify currrent state,
C        and to add input and output entry points RMARIN, RMARUT.
C
C     Unique random number used in the program
C ----------------------------------------------------------------------
      COMMON / INOUT / INUT,IOUT
      DIMENSION RVEC(*)
      COMMON/RASET1/U(97),C,I97,J97
      PARAMETER (MODCNS=1000000000)
      DATA NTOT,NTOT2,IJKL/-1,0,0/
C
      IF (NTOT .GE. 0)  GO TO 50
C
C        Default initialization. User has called RANMAR without RMARIN.
      IJKL = 54217137
      NTOT = 0
      NTOT2 = 0
      KALLED = 0
      GO TO 1
C
      ENTRY      RMORIN(IJKLIN, NTOTIN,NTOT2N)
C         Initializing routine for RANMAR, may be called before
C         generating pseudorandom numbers with RANMAR. The input
C         values should be in the ranges:  0<=IJKLIN<=900 OOO OOO
C                                          0<=NTOTIN<=999 999 999
C                                          0<=NTOT2N<<999 999 999!
C To get the standard values in Marsaglia's paper, IJKLIN=54217137
C                                            NTOTIN,NTOT2N=0
      IJKL = IJKLIN
      NTOT = MAX(NTOTIN,0)
      NTOT2= MAX(NTOT2N,0)
      KALLED = 1
C          always come here to initialize
    1 CONTINUE
      IJ = IJKL/30082
      KL = IJKL - 30082*IJ
      I = MOD(IJ/177, 177) + 2
      J = MOD(IJ, 177)     + 2
      K = MOD(KL/169, 178) + 1
      L = MOD(KL, 169)
      WRITE(IOUT,201) IJKL,NTOT,NTOT2
 201  FORMAT(1X,' RANMAR INITIALIZED: ',I10,2X,2I10)
      DO 2 II= 1, 97
      S = 0.
      T = .5
      DO 3 JJ= 1, 24
         M = MOD(MOD(I*J,179)*K, 179)
         I = J
         J = K
         K = M
         L = MOD(53*L+1, 169)
         IF (MOD(L*M,64) .GE. 32)  S = S+T
    3    T = 0.5*T
    2 U(II) = S
      TWOM24 = 1.0
      DO 4 I24= 1, 24
    4 TWOM24 = 0.5*TWOM24
      C  =   362436.*TWOM24
      CD =  7654321.*TWOM24
      CM = 16777213.*TWOM24
      I97 = 97
      J97 = 33
C       Complete initialization by skipping
C            (NTOT2*MODCNS + NTOT) random numbers
      DO 45 LOOP2= 1, NTOT2+1
      NOW = MODCNS
      IF (LOOP2 .EQ. NTOT2+1)  NOW=NTOT
      IF (NOW .GT. 0)  THEN
       WRITE (IOUT,'(A,I15)') ' RMARIN SKIPPING OVER ',NOW
       DO 40 IDUM = 1, NTOT
       UNI = U(I97)-U(J97)
       IF (UNI .LT. 0.)  UNI=UNI+1.
       U(I97) = UNI
       I97 = I97-1
       IF (I97 .EQ. 0)  I97=97
       J97 = J97-1
       IF (J97 .EQ. 0)  J97=97
       C = C - CD
       IF (C .LT. 0.)  C=C+CM
   40  CONTINUE
      ENDIF
   45 CONTINUE
      IF (KALLED .EQ. 1)  RETURN
C
C          Normal entry to generate LENV random numbers
   50 CONTINUE
      DO 100 IVEC= 1, LENV
      UNI = U(I97)-U(J97)
      IF (UNI .LT. 0.)  UNI=UNI+1.
      U(I97) = UNI
      I97 = I97-1
      IF (I97 .EQ. 0)  I97=97
      J97 = J97-1
      IF (J97 .EQ. 0)  J97=97
      C = C - CD
      IF (C .LT. 0.)  C=C+CM
      UNI = UNI-C
      IF (UNI .LT. 0.) UNI=UNI+1.
C        Replace exact zeroes by uniform distr. *2**-24
         IF (UNI .EQ. 0.)  THEN
         UNI = TWOM24*U(2)
C             An exact zero here is very unlikely, but let's be safe.
         IF (UNI .EQ. 0.) UNI= TWOM24*TWOM24
         ENDIF
      RVEC(IVEC) = UNI
  100 CONTINUE
      NTOT = NTOT + LENV
         IF (NTOT .GE. MODCNS)  THEN
         NTOT2 = NTOT2 + 1
         NTOT = NTOT - MODCNS
         ENDIF
      RETURN
C           Entry to output current status
      ENTRY RMORUT(IJKLUT,NTOTUT,NTOT2T)
      IJKLUT = IJKL
      NTOTUT = NTOT
      NTOT2T = NTOT2
      RETURN
      END
      SUBROUTINE INIMAS
C ----------------------------------------------------------------------
C     INITIALISATION OF MASSES
C
C ----------------------------------------------------------------------
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
C IN-COMING / OUT-GOING  FERMION MASSES
      AMTAU  = 1.784197
      AMEL   = 0.0005111
      AMNUE  = 0.0
      AMMU   = 0.105659
      AMNUMU = 0.0
C MASSES USED IN TAU DECAYS
      AMPIZ  = 0.134964
      AMPI   = 0.139568
      AMRO   = 0.7714
      GAMRO  = 0.153
      AMA1   = 1.275
      GAMA1  = 0.599
      AMK    = 0.493667
      AMKZ   = 0.49772
      AMKST  = 0.8921
      GAMKST = 0.0511
      END
      SUBROUTINE INITDK
C ----------------------------------------------------------------------
C     INITIALISATION OF TAU DECAY PARAMETERS  and routines
C
C ----------------------------------------------------------------------
      COMMON / DECPAR / GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      REAL*4            GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
C
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
      COMMON / TAUBRA / GAMPRT(30),JLIST(30),NCHAN
      PARAMETER (NMODE=4)
      COMMON / TAUNPI / CBRNPI       ,AMAS
     &                 ,KPI(6,NMODE) ,MULT(NMODE)
      REAL*4            CBRNPI(NMODE),AMAS(6,NMODE)
      REAL*4 PI
CAM
CAM  multipion decays
      REAL*4    BRMNPI(NMODE),AAMPI(2)
      DIMENSION NOPI(6,NMODE),NPI(NMODE)
CAM   INDIVIDUAL BRANCHING RATIOS RELATIVE TO TOTAL MULTIPION
CAM                2PI-PI+PI0,     PI-3PI0,      3PI-2PI+,   3PI-2PI+PI0
      DATA BRMNPI/     0.5561,      0.4351,        0.0044,        0.0044
     &/
CAM   OUTGOING  MULTIPLICITY
      DATA NPI /            4,           4,             5,             6
     &/
      DATA NOPI/-1,-1,1,2,0,0,-1,2,2,2,0,0,-1,-1,-1,1,1,0,-1,-1,-1,1,1,2
     &/
      NCHAN = 8
C LIST OF BRANCHING RATIOS
CAM normalised to e nu nutau channel
CAM                  enu   munu   pinu  rhonu   A1nu   Knu    K*nu   pi'
CAM        JLIST  /    1,     2,     3,     4,     5,     6,     7,
      DO 1 I = 1,30
      IF (I.LE.NCHAN) THEN
        JLIST(I) = I
        IF(I.EQ.1) GAMPRT(I) = 1.0000
        IF(I.EQ.2) GAMPRT(I) = 0.9730
        IF(I.EQ.3) GAMPRT(I) = 0.6054
        IF(I.EQ.4) GAMPRT(I) = 1.2435
        IF(I.EQ.5) GAMPRT(I) = 0.8432
        IF(I.EQ.6) GAMPRT(I) = 0.0432
        IF(I.EQ.7) GAMPRT(I) = 0.0811
        IF(I.EQ.8) GAMPRT(I) = 0.6162
      ELSE
        JLIST(I) = 0
        GAMPRT(I) = 0.
      ENDIF
   1  CONTINUE
      AAMPI(1)=AMPI
      AAMPI(2)=AMPIZ
CAM     CUMULATIVE BRANCHING RATIOS RELATIVE TO TOTAL MULTIPION
      CBRNPI(1)=BRMNPI(1)
      DO 2 I=2,NMODE
      CBRNPI(I)=CBRNPI(I-1) + BRMNPI(I)
 2    CONTINUE
      DO 3 I=1,NMODE
      MULT(I)=NPI(I)
CAM   OUTGOING PION MASSES
      ND=MULT(I)
      DO 4 J=1,ND
      IPI=ABS(NOPI(J,I))
      IF(IPI.EQ.1) KPI(J,I)=17*NOPI(J,I)/IPI
      IF(IPI.EQ.2) KPI(J,I)=23
 4    AMAS(J,I) = AAMPI(IPI)
 3    CONTINUE
      PI =4.*ATAN(1.)
      GFERMI = 1.16637E-5
      CCABIB = 0.975
      SCABIB = SQRT(1.-CCABIB**2)
      GAMEL  = GFERMI**2*AMTAU**5/(192*PI**3)
      END
      SUBROUTINE SPIN(V,E1,F1)
C     modified routine SPIN(....
C     **********************
      IMPLICIT REAL*8(A-H,O-Z)
      COMPLEX*16 V(4,4)
      DIMENSION E(4),F(4),E1(4),F1(4)
      COMMON /KEYTR/ ITRANS
      DATA ZER/0./
      DATA init/0/
C      DATA itrans/0/
C
      if (init.eq.0) then
       init=1
       WRITE(6,*) 'SPIN: itrans=',itrans,'   *****'
      endif
      if (itrans.eq.0) then
       do 7 k=1,4
        E(K)=E1(K)
        F(K)=F1(K)
 7     continue
        E(1)=0.0
        F(1)=0.0
        E(2)=0.0
        F(2)=0.0
       V(1,1)=DCMPLX( E(4)*F(4) -E(1)*F(1) +E(2)*F(2) +E(3)*F(3),  ZER)
       V(2,2)=DCMPLX( E(4)*F(4) +E(1)*F(1) -E(2)*F(2) +E(3)*F(3),  ZER)
       V(3,3)=DCMPLX( E(4)*F(4) -E(1)*F(1) -E(2)*F(2) -E(3)*F(3),  ZER)
       V(4,4)=DCMPLX( E(4)*F(4) +E(1)*F(1) +E(2)*F(2) -E(3)*F(3),  ZER)
       V(1,2)=DCMPLX( E(3)*F(4) +E(4)*F(3), E(1)*F(2) +E(2)*F(1))
       V(3,4)=DCMPLX( E(3)*F(4) -E(4)*F(3),-E(1)*F(2) +E(2)*F(1))
       V(1,3)=DCMPLX( E(4)*F(1) -E(1)*F(4),-E(2)*F(3) +E(3)*F(2))
       V(2,4)=DCMPLX( E(4)*F(1) +E(1)*F(4), E(2)*F(3) +E(3)*F(2))
       V(1,4)=DCMPLX( E(3)*F(1) +E(1)*F(3), E(4)*F(2) +E(2)*F(4))
       V(2,3)=DCMPLX( E(3)*F(1) -E(1)*F(3), E(4)*F(2) -E(2)*F(4))
       V(1,4)=DCMPLX( E(3)*F(1) +E(1)*F(3), E(4)*F(2) +E(2)*F(4))
       V(2,3)=DCMPLX( E(3)*F(1) -E(1)*F(3), E(4)*F(2) -E(2)*F(4))
      else
       do 8 k=1,4
        E(K)=E1(K)
        F(K)=F1(K)
 8    continue
       V(1,1)=DCMPLX( E(4)*F(4) -E(1)*F(1) +E(2)*F(2) +E(3)*F(3),  ZER)
       V(2,2)=DCMPLX( E(4)*F(4) +E(1)*F(1) -E(2)*F(2) +E(3)*F(3),  ZER)
       V(3,3)=DCMPLX( E(4)*F(4) -E(1)*F(1) -E(2)*F(2) -E(3)*F(3),  ZER)
       V(4,4)=DCMPLX( E(4)*F(4) +E(1)*F(1) +E(2)*F(2) -E(3)*F(3),  ZER)
       V(1,2)=DCMPLX( E(3)*F(4) +E(4)*F(3), E(1)*F(2) +E(2)*F(1))
       V(3,4)=DCMPLX( E(3)*F(4) -E(4)*F(3),-E(1)*F(2) +E(2)*F(1))
       V(1,3)=DCMPLX( E(4)*F(1) -E(1)*F(4),-E(2)*F(3) +E(3)*F(2))
       V(2,4)=DCMPLX( E(4)*F(1) +E(1)*F(4), E(2)*F(3) +E(3)*F(2))
       V(1,4)=DCMPLX( E(3)*F(1) +E(1)*F(3), E(4)*F(2) +E(2)*F(4))
       V(2,3)=DCMPLX( E(3)*F(1) -E(1)*F(3), E(4)*F(2) -E(2)*F(4))
       V(1,4)=DCMPLX( E(3)*F(1) +E(1)*F(3), E(4)*F(2) +E(2)*F(4))
       V(2,3)=DCMPLX( E(3)*F(1) -E(1)*F(3), E(4)*F(2) -E(2)*F(4))
      endif
      V(2,1)=DCONJG(V(1,2))
      V(4,3)=DCONJG(V(3,4))
      V(3,1)=DCONJG(V(1,3))
      V(4,1)=DCONJG(V(1,4))
      V(4,2)=DCONJG(V(2,4))
      V(3,2)=DCONJG(V(2,3))
      RETURN
      END
