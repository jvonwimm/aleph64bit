C --------------------------------------------------------------------
C GGG ORIGINE : received from Ramon Miquel (Barcelona) April, 27, 1988
C               2 and 3 gamma generator from Berends and Kleiss
C
C  GGG ORIGINE is the complete fortran code including a function called
C CUT which makes the cuts for a Bhabha-like configuration ( at least
C two photons are seen).
C  An other function CUTSGL contains cuts for single photon studies.
C
C G. Bonneaud April, 27, 1988
C --------------------------------------------------------------------
C modifications to the original code :
C
C 1. FUNCTIONS CUT and CUTSGL have been both included and are called
C    depending on the value of the variable ICONF (see data cards).
C
C 2. COMMON / GGDATA / has been introduced to distribute the data where
C    necessary.
C
C G. Bonneaud October 20, 1988.
C 3. Subroutine FINISH upgraded to use ranmar-like RDMOUT with 3 args
C 4. calls DDILOG instead of DILOG .....as the programs wants the REAL*8
C     version of the CERNLIB routine . Otherwise this messes up everything
C     on AXP !
C B. Bloch   November 1995
       PROGRAM  GGGMAIN
C **********************************************************************
C   GAMMA GAMMA (GAMMA) GENERATOR DRIVER
C     (FROM BERENDS & KLEISS WITH SOME MODIFICATIONS FOR
C                 CUTTING EVENTS )
C
C                                   M.MARTINEZ    DESY-1985
C **********************************************************************
      IMPLICIT REAL*8(A-H,K,O-Z)
      COMMON / / HMEMOR(10000)
      COMMON / OUTVEC / Q1(4),Q2(4),Q3(4),EBE
      COMMON / CON2 / K0,RHO,THRSLD
      COMMON / TRIGAM / I3
      DATA CONV/57.2958/
CIBM      DATA MTSTOP/10000/
C =================================================================
C INITIAL PARAMETERS:
      READ(5,555) EB,RHO,I3,NEV,IODD
555   FORMAT(///2D10.2,3I8)
      EBE=EB
      WRITE(6,5)
  5   FORMAT(///10X,' E+ E- ---->  GAMMA  GAMMA (GAMMA) '///)
C =================================================================
C     CALL HBOOK1(100,' WEIGHT(EXACT/APPROX.) DISTRIBUTION $',50,0.,RHO)
CIBM      CALL HBOOK1(120,' TIME/EVENT (SECONDS) $',50,0.,.1)
C     CALL HBOOK1(1,' COS (THETA) G1 $',60,-1.,1.)
C     CALL HLOGAR(1)
C     CALL HBOOK1(11,' ENERGY G1 $',50,0.,1.)
C     CALL HLOGAR(11)
C     CALL HBOOK1(2,' COS (THETA) G2 $',60,-1.,1.)
C     CALL HLOGAR(2)
C     CALL HBOOK1(12,' ENERGY G2 $',50,0.,1.)
C     CALL HLOGAR(12)
C     CALL HBOOK1(3,' COS (THETA) G3 $',60,-1.,1.)
C     CALL HLOGAR(3)
C     CALL HBOOK1(13,' ENERGY G3 $',50,0.,1.)
C     CALL HLOGAR(13)
C     CALL HBOOK2(21,' X->CT G1/ Y->CT G2 $',60,-1.,1.,
C    .  60,-1.,1.)
C     CALL HBOOK2(22,' X->EN G1/ Y->EN G2 $',50,0.,1.,
C    .  50,0.,1.)
C     CALL HBOOK1(30,' ACOLINEARITY G1 G2 $',60,0.,180.)
C     CALL HLOGAR(30)
C     CALL HBOOK1(40,' ACOPLANARITY G1 G2 $',60,0.,180.)
C     CALL HLOGAR(40)
      CALL START(EB,IODD)
C.......................................... EVENT LOOP ............
CIBM      MT0=MTIME(D)
      DO 100 IV=1,NEV
C
      CALL EVENT
C
      CG1=0.
      IF(Q1(4).EQ.0.)GO TO 120
      CG1=Q1(3)/Q1(4)
      CALL HF1(1,CG1,1.)
      CALL HF1(11,Q1(4),1.)
      CG2=0.
120   IF(Q2(4).EQ.0.)GO TO 130
      CG2=Q2(3)/Q2(4)
      CALL HF1(2,CG2,1.)
      CALL HF1(12,Q2(4),1.)
      CG3=0.
130   IF(Q3(4).EQ.0.)GO TO 140
      CG3=Q3(3)/Q3(4)
      CALL HF1(3,CG3,1.)
      CALL HF1(13,Q3(4),1.)
C
140   IF(Q1(4).EQ.0..OR.Q2(4).EQ.0.)GO TO 150
      CALL HF2(21,CG1,CG2,1.)
      CALL HF2(22,Q1(4),Q2(4),1.)
C
150   IF(Q1(4).EQ.0.OR.Q2(4).EQ.0.)GO TO 145
      Z=2.*(1.-Q3(4))/(Q1(4)*Q2(4))-1.
      IF(Z.LT.-1.)Z=-1.
      IF(Z.GT.1.)Z=1.
      Z=CONV*DACOS(Z)
      CALL HF1(30,Z,1.)
      Z=-(Q1(1)*Q2(1)+Q1(2)*Q2(2))/
     . DSQRT((Q1(1)**2+Q1(2)**2)*(Q2(1)**2+Q2(2)**2))
      IF(Z.LT.-1.)Z=-1.
      IF(Z.GT.1.)Z=1.
      Z=CONV*DACOS(Z)
      CALL HF1(40,Z,1.)
 145  CONTINUE
C........ WRITING THE EVENT IN THE STANDARD FORMAT
CMMR      CALL STDOUT
C........ CPU TIME CONTROL
CIBM      MT1=MTIME(D)
CIBM      IF(MT1.LT.MTSTOP) GO TO 240
CIBM      DELT=(MT0-MT1)/10000.
CIBM      CALL HF1(120,DELT,1.)
CIBM      MT0=MT1
 100  CONTINUE
C....................................................................
C
 240  CONTINUE
CIBM      IF(MT1.LT.MTSTOP) WRITE(6,9999) MTSTOP,MT1
CIBM 9999 FORMAT(//' *** CPU TIME EXHAUSTED ***',
CIBM     .           ' MTSTOP = ',I10,' MT1 = ',I10,' ******'//)
      CALL FINISH
      CALL HSTORE(0,20)
      CALL HISTDO
      STOP
      END
C      DOUBLE PRECISION FUNCTION DILOG(X)
C      IMPLICIT REAL*8(A-H,O-Z)
C      F1 = 0.D0
C      IF(X.NE.1.D0) F1=2.D0*(1.D0-X)*DLOG(1.D0-X)
C      DILOG =( 3.D0*X + F1 + X*X*(.25D0 - .18787D0*X + .02559D0*X*X)/
C     . (1.D0 - X*(.86258D0 - .17044D0*X + .00498D0*X*X)) )/(1.D0+X)
C      RETURN
C      END
      FUNCTION CUT(DUMMY)
C **********************************************************************
C             SYMMETRIC-CUTTING ROUTINE
C    (THIS ONE IMPLEMENTS BHABHA-LIKE SELECTION CUTS)
C
C                                   M.MARTINEZ    DESY-1985
C **********************************************************************
      IMPLICIT REAL*8(A-H,K-Z)
      COMMON / GGDATA / EB,RHO,X1,A1MIN,A1MAX,AVETO,I33,IODD,ICONF
      COMMON / VEC4 / K(3,4)
      DIMENSION CT(3)
C ....... INITIALIZATION  .......................................
      DATA CONV/57.2958/
      DATA IPASS /0/
C GB  DATA X1/.0/,A1MIN,A1MAX/0.,180./
      IF(IPASS.EQ.1)GO TO 200
      IPASS=1
C GB  READ(5,133)X1,A1MIN,A1MAX
C133  FORMAT(///3D10.2)
      CT1MIN=COS(A1MAX/CONV)
      CT1MAX=COS(A1MIN/CONV)
      PRINT 20,X1,A1MIN,A1MAX
 20   FORMAT(///,5X,40('@')/'  CUTS: '/
     . '    ENERGY G / EB >',G10.4/
     . '    THETA  G      >',G10.4,' DEGREES '/
     . '    THETA  G      <',G10.4,' DEGREES '/
     . '  G = OBSERVABLE PHOTONS (AT LEAST 2)'/
     . 5X,40('@')///)
200   CONTINUE
      CT(1)=0.
      CT(2)=0.
      CT(3)=0.
      IF(K(1,4).NE.0.)CT(1)=K(1,3)/K(1,4)
      IF(K(2,4).NE.0.)CT(2)=K(2,3)/K(2,4)
      IF(K(3,4).NE.0.)CT(3)=K(3,3)/K(3,4)
C ........ SYMMETRIZING THE CUTS .................................
      CUT=0.D0
      DO 100 I1=1,3
      DO 100 I=1,2
      I2=I1+I
      IF(I2.GT.3)I2=I2-3
      I3=6-I1-I2
C ===  CUTS  (NOT SYMMETRYCALY DEFINED) ==========================
      IF(K(I1,4).LT.X1)GOTO 100
      IF(K(I2,4).LT.X1)GOTO 100
      IF(CT(I1).LT.CT1MIN)GOTO 100
      IF(CT(I1).GT.CT1MAX)GOTO 100
      IF(CT(I2).LT.CT1MIN)GOTO 100
      IF(CT(I2).GT.CT1MAX)GOTO 100
C ================================================================
      CUT=1.D0
      GO TO 300
100   CONTINUE
300   RETURN
      END
      FUNCTION CUTSGL(DUMMY)
C **********************************************************************
C             SYMMETRIC-CUTTING ROUTINE
C             FOR SINGLE PHOTON CONFIGURATION
C
C                                   M.MARTINEZ    DESY-1985
C **********************************************************************
      IMPLICIT REAL*8(A-H,K-Z)
      COMMON / GGDATA / EB,RHO,X1,A1MIN,A1MAX,AVETO,I33,IODD,ICONF
      COMMON / VEC4 / K(3,4)
      DIMENSION CT(3)
C ....... INITIALIZATION  .......................................
      DATA CONV/57.2958/
C GB  DATA X1/.0/,A1MIN,A1MAX/0.,180./
C    . ,AVETO/90./
      DATA IPASS/0/
      IF(IPASS.EQ.1)GO TO 200
      IPASS=1
C GB  READ(5,133)X1,A1MIN,A1MAX,AVETO
C133  FORMAT(///5D10.2)
      CT1MIN=COS(A1MAX/CONV)
      CT1MAX=COS(A1MIN/CONV)
      CTVETO=COS(AVETO/CONV)
      UAVETO=180.-AVETO
      PRINT 20,X1,A1MIN,A1MAX,AVETO,UAVETO
 20   FORMAT(///,5X,40('@')/'  CUTS: '/
     . '    ENERGY GA / EB >',G10.4/
     . '    THETA  GA      >',G10.4,' DEGREES '/
     . '    THETA  GA      <',G10.4,' DEGREES '/
     . '    THETA  GB & GC <',G10.4,' DEGREES '/
     . ' OR THETA  GB & GC >',G10.4,' DEGREES '/
     . '    (A,B,C) = PERMUTATIONS (1,2,3)  '/
     . 5X,40('@')///)
200   CONTINUE
      CT(1)=0.
      CT(2)=0.
      CT(3)=0.
      IF(K(1,4).NE.0.)CT(1)=K(1,3)/K(1,4)
      IF(K(2,4).NE.0.)CT(2)=K(2,3)/K(2,4)
      IF(K(3,4).NE.0.)CT(3)=K(3,3)/K(3,4)
C ........ SYMMETRIZING THE CUTS .................................
      CUTSGL=0.D0
      DO 100 I1=1,3
      DO 100 I=1,2
      I2=I1+I
      IF(I2.GT.3)I2=I2-3
      I3=6-I1-I2
C ===  CUTS  (NOT SYMMETRYCALY DEFINED) ==========================
      IF(K(I1,4).LT.X1)GOTO 100
      IF(CT(I1).LT.CT1MIN)GOTO 100
      IF(CT(I1).GT.CT1MAX)GOTO 100
      IF(DABS(CT(I2)).LT.CTVETO)GOTO 100
      IF(DABS(CT(I3)).LT.CTVETO)GOTO 100
C ================================================================
      CUTSGL=1.D0
      GOTO 300
100   CONTINUE
300   RETURN
      END
      DOUBLE PRECISION FUNCTION DRN(XX)
C...THIS IS THE DOUBLE PRECISION VERSION
C...OF THE RANDOM NUMBER GENERATOR RNDM(DUMMY)
C...DRN IS OBTAINED BY CONCATENATING TWO CONSECUTIVE
C...VALUES OF RNDM
C...THE LEAST SIGNIFICANT BITS MUST BE CREATED AS A REAL NUMBER
C...IN ORDER TO AVOID READING SIGN- AND EXPONENT BITS AS NUMBERS
C     This doen't work on AXP......
      IMPLICIT REAL*8(A-H,K,O-Z)
      REAL*4 X1,X2,RN,V,rndm
C      COMMON / CATCOM / V,ICNCAT
C      EQUIVALENCE (V,ZZ)
      V=RNDM(X1)
C      ICNCAT=INT(RNDM(X2)*2147483647)
C      DRN=ZZ
       DRN = V
      RETURN
      END
      SUBROUTINE EVENT
      IMPLICIT REAL*8(A-Z)
      INTEGER IEV,IPS,ITRY
      COMMON / CON1 / PI,M,ALFA,PI2,LE,E,DL,MU,S
      COMMON / CON2 / K0,RHO,THRSLD
      COMMON / CON3 / BETA,SIG0,SIGA
      COMMON / ANSWRC / X0,X1,ANSWER
      COMMON / OUT1 / K,C,Z,FI,FG
      COMMON / OUT2 / SC,SZ,CGAM,E2
      COMMON / TRY / TTOT,IEV,ITRY,TTOT2,IPS
      COMMON / TESRAT / T
      EXTERNAL YK,YC,GENCOS
C...ADD TO EVENT COUNTER AND START TRIAL CYCLE
      IEV=IEV+1
    1 ITRY=ITRY+1
C...GENERATE RANDOM FI AND FG
      FI=2.*PI*DRN(1.D0)
      FG=2.*PI*DRN(2.D0)
C...GENERATE K VALUE USING HELMUT'S TRICK
      R=DRN(3.D0)
      IF(R.GT.THRSLD) GO TO 2
C......................................................................
C...SOFT CASE: NO BREMSSTRAHLUNG ASSUMED
C...SET K EQUAL TO 0 , Z EQUAL TO 1 AND ASSUME DISTRIBUTION FOR C WHICH
C...GOES AS 2./(SIN**2(TH)+M**2) (GIVEN BY "GENCOS" PLUS REFLECTION)
C......................................................................
      K=0.D0
      X0=-1.
      X1=1.
      C=GENCOS(K)
      R=2.*DRN(4.D0)
      IF(R.GT.1.D0) C=-C
      Z=1.D0
      GO TO 3
C......................................................................
C...HARD CASE; GENERATE BREMSSTRAHLUNG ABOVE K0
C...C IS GENERATED BY EVGENI/H
C...Z IS GIVEN BY GENCOS AND WEIGHTED FOR REFLECTION
C...BY CALCULATING WEIZ
C......................................................................
    2 CONTINUE
      X0=K0
      X1=1.D00
      CALL EVGENH(YK)
      K=ANSWER
C.......
      X0=-1.
      X1=1.
      CALL EVGENH(YC)
      C=ANSWER
C.......
      Z=GENCOS(K)
      R=DRN(5.D0)
      W=WEIZ(K,C)
      IF(R.GT.W) Z=-Z
C...TEST FOR EXIT, CALCULATE 4VECTORS AND PERMUTATE THEM
    3 R=RHO*DRN(6.D0)
      T=TEST(0.D0)
      IF(T.NE.0.)CALL HFILL(10001,REAL(T),0.,1.)
      IF(T.NE.0.)IPS=IPS+1
      TTOT=TTOT+T
      TTOT2=TTOT2+T*T
      IF(R.GT.T) GO TO 1
      CALL MIXUP
      RETURN
      END
      SUBROUTINE EVGENH(F)
C...THIS IS A MORE SOPHISTICATED VERSION OF EVGENI,
C...CREATED BY HARVEY NEWMAN ET AL.
C...IT COMBINES BINARY SEARCH AND A NEWTONIAN METHOD TO OBTAIN:
C... A) BETTER CONVERGENCE TOWARDS THE ZERO OF (F(X)-VAL)
C... B) IN GENERAL A SMALLER NUMBER OF FUNCTION CALLS
      IMPLICIT REAL*8(A-H,K,O-Z)
      COMMON / ANSWRC / X0,X1,ANSWER
      DATA EPS1/1.D-6/,EPS2/1.D-4/,NCALL/0/
      NCALL=NCALL+1
      J=0
      M=0
      DIFF=1.D00
      EPS=EPS1
      DEL0=DABS(1.D-6*(X1-X0))
      DEL1=0.1*DEL0
      F0=F(X0)
      F1=F(X1)
      Y0=X0
      Y1=X1
      VAL=(F1-F0)*DRN(8.D0)+F0
 100  M=M+1
      IF (M.GE.7) GO TO 999
      DO 11 I=1,6
      ANSO=ANSWER
      ANSWER=0.5D0*(Y1+Y0)
      DIFF=ANSWER-ANSO
      FM=F(ANSWER)
      FM1=FM-VAL
      IF(FM1)1,2,3
    1 Y0=ANSWER
      F0=FM
      GO TO 4
    2 CONTINUE
      RETURN
    3 Y1=ANSWER
      F1=FM
    4 CONTINUE
   11 CONTINUE
      ANSWER=0.5D0*(Y1+Y0)
      FM=F(ANSWER)
      OLD=DABS((VAL-FM)/VAL)
  200 IF (OLD.LE.EPS) GO TO 999
      J=J+1
      DELTA=DEL0
      IF (DABS(DIFF).LT.DELTA) DELTA=DABS(DIFF)
      ANO=ANSWER
      ANN=ANO+DELTA
      IF (ANN.LE.X1) GO TO 220
      EPS=EPS2
      DELTA=0.5*DELTA
      ANN=X1
      FM=F(ANN-DELTA)
  220 CONTINUE
      FM1=(F(ANN)-FM)/DELTA
      IF (FM1.EQ.0.) GO TO 300
      DIFF=(FM-VAL)/FM1
      IF (DABS(DIFF).LT.DEL1) GO TO 999
      ANSWER=ANO-DIFF
      IF (ANSWER.GE.Y0 .AND. ANSWER.LE.Y1) GO TO 250
  300 ANSWER=ANO
      GO TO 100
  250 FM=F(ANSWER)
      ANEW=DABS((VAL-FM)/VAL)
      IF (ANEW.GT.OLD .AND. J.GT.3) GO TO 100
      OLD=ANEW
      IF (J.LE.7) GO TO 200
  933 FORMAT(7X,' EVGENH: ',I6,4D15.7,3X,2I4)
  999 RETURN
      END
      DOUBLE PRECISION FUNCTION EXACT(K,Z,X,C,X1,C1)
C...CALCULATES EXACT CROSSECTION FOR THREE GAMMA PRODUCTION
      IMPLICIT REAL*8(A-Z)
      COMMON / CON1 / PI,M,ALFA,PI2,LE,E,DL,MU,S
C...CONSTRUCT SIX DOT PRODUCTS
      K11=K*(E-Z)
      K12=K*(E+Z)
      K21=X*(E-C)
      K22=X*(E+C)
      K31=X1*(E-C1)
      K32=X1*(E+C1)
C...CROSSECTION
      EXACT=-2.*M**2*((K22/K32+K32/K22)/K11**2
     Z               +(K21/K31+K31/K21)/K12**2
     Z               +(K12/K32+K32/K12)/K21**2
     Z               +(K11/K31+K31/K11)/K22**2
     Z               +(K12/K22+K22/K12)/K31**2
     Z               +(K11/K21+K21/K11)/K32**2)
     Z   +4.*E**2/K11/K12/K21/K22/K31/K32
     Z            *(  K11*K12*(K11**2+K12**2)
     Z               +K21*K22*(K21**2+K22**2)
     Z               +K31*K32*(K31**2+K32**2) )
C...NORMALIZATION AND RETURN
      EXACT=ALFA**3/8./PI2/S*EXACT
      RETURN
      END
      SUBROUTINE FINISH
      IMPLICIT REAL*8(A-H,K-Z)
      COMMON / CON2 / K0,RHO,THRSLD
      COMMON / TRY / TTOT,IEV,ITRY,TTOT2,IPS
      COMMON / CON3 / BETA,SIG0,SIGA
      COMMON / SIGTOT / SIGCOL
      DIMENSION ISEED(3)
C
      STESM=TTOT/ITRY
      ERRTES=DSQRT((TTOT2/ITRY-STESM**2)/ITRY)
      SIGOBS=STESM *SIGCOL
      ERRSIG=ERRTES *SIGCOL
      CALL RDMOUT(ISEED)
C
      PRINT 1,ITRY,IPS,IEV,SIGCOL,STESM,ERRTES,SIGOBS,ERRSIG,ISEED
    1 FORMAT(////,
     . 5X,'# EVENTS GENERATED        =',I10/
     . 5X,'# EVENTS IN CUT REGION    =',I10/
     . 5X,'# EVENTS ACCEPTED         =',I10//
     . 5X,'# INTEGRATION REGION SIZE =',G12.5,' NB'//
     . 5X,'  MEAN WEIGHT             =',G12.5,' +/-',G12.5,//,
     . 5X,'#########################################################'/
     . 5X,'###'/
     . 5X,'### FINAL CROSS SECTION  =',G12.5,' +/-',G12.5,' NB'/,
     . 5X,'###'/
     . 5X,'#########################################################'//
     . 5X,'  LAST SEED FOR RNDM      =',3I12//)
      RETURN
      END
      DOUBLE PRECISION FUNCTION G(C)
C...FINITE ANGLE-DEPENDENT PART OF VIRTUAL CORRECTIONS
C...THIS MUST BE EVEN IN C
C...THE PSEUDODIVERGENCE IN 1/(K-MU/2) IS TREATED SEPARATELY
      IMPLICIT REAL*8(A-H,K-Z)
      COMMON / CON1 / PI,M,ALFA,PI2,LE,E,DL,MU,S
      M2=0.5D0*MU
      G=0.D0
      I=0
      C1=C
    1 K=0.5*(1.+C1)+M2
      T=0.5*(1.-C1)+M2
      H0=.5D0/K+.5D0/T
      H1=0.5D0/K+1.D0/T-0.5D0
      H2=2.D0/K-0.5D0+MU/K/K
      H3=-K-0.75D0/T+0.75D0
      H4=-1.5D0/T+1.5D0 -MU/T/T
      H5=2.D0/T+1.D0/K-1.D0 +2.*M2*M2/K/K/K
      U=K/M2-1.D0
      FACLI2=PI2/6.-DDILOG(-U)
      IF(U.LE.1.D-03) GO TO 4
      DIVTRM=DLOG(U+1.)*(-1./MU/U-0.5/U/U)+0.5/U
      GO TO 5
    4 DIVTRM=-1.D0/MU
    5 G=(0.25D0*LE*LE-PI2/12.D0)*(H0 + 1.D0)
     Z +(DLOG(K)+LE)*(-2.D0*LE*H1 + H2)
     Z +(PI2-LE*LE)*H3 + (2.D0*M2/K-1.D0)*LE
     Z +H4 + FACLI2*H5 + DIVTRM + G
C     PRINT 111,G
C     PRINT 111,K,T,H0,H1,H2,H3,H4,H5,FACLI2,DIVTRM
  111 FORMAT(5X,10D12.4)
      IF (I.EQ.1) RETURN
      I=1
      C1=-C
      GO TO 1
      END
      DOUBLE PRECISION FUNCTION GENCOS(X)
C...THIS ROUTINE GENERATES THE COSINE COS(X) OF AN ANGLE
C...WITH A DISTRIBUTION PROPORTIONAL TO
C...             1/(1-COS(X)+M**2/2)
C...THIS CAN BE USED FOR GENERATING C THE LOWEST ORDER CROSSECTION
C...BEFORE TESTING , AS WELL AS FOR GENERATING Z
      IMPLICIT REAL*8(A-H,K-Z)
      COMMON / CON1 / PI,M,ALFA,PI2,LE,E,DL,MU,S
      ETA=DRN(X)
      GENCOS=E-DL*DEXP(-ETA*LE)
      RETURN
      END
      SUBROUTINE MIXUP
      IMPLICIT REAL*8(A-H,K-Z)
      COMMON / VEC4 / L(3,4)
      COMMON / OUTVEC / K1(4),K2(4),K3(4)
C...PICK ANY PERMUTATION OF THE THREE MOMENTA
C...TO ASSURE SYMMETRICAL EVENT SHAPES
      R=6.D0*DRN(0.D0)
      IT=IDINT(R)
      GO TO (1,2,3,4,5),IT
      J1=1
      J2=2
      J3=3
      GO TO 6
    1 J1=2
      J2=3
      J3=1
      GO TO 6
    2 J1=3
      J2=1
      J3=2
      GO TO 6
    3 J1=3
      J2=2
      J3=1
      GO TO 6
    4 J1=2
      J2=1
      J3=3
      GO TO 6
    5 J1=1
      J2=3
      J3=2
    6 DO 7 J=1,4
      K1(J)=L(J1,J)
      K2(J)=L(J2,J)
    7 K3(J)=L(J3,J)
      RETURN
      END
      DOUBLE PRECISION FUNCTION PROXIM(K,Z,X,C,X1,C1)
C...CALCULATES ONE-SIXTH OF THE APPROXIMANT BY CHOOSING A DEFINITE
C...LABELING OF THE THREE PHOTONS ; ONE OF THEM REMAINS DUMMY (3RD)
C...THE TRIAL EVENTS ARE GENERATED WITH VARIABLES K AND X , WHICH
C...GIVE RISE TO A PHASE SPACE FACTOR  X**2*K/2/(1-K); TO
C...MAKE THE PHASE SPACE SYMMETRIC UNDER ANY PERMUTATION AS IN THE
C...EXACT EXPRESSION , WE HAVE TO DIVIDE BY THIS FACTOR.
      IMPLICIT REAL*8(A-Z)
      COMMON / CON1 / PI,M,ALFA,PI2,LE,E,DL,MU,S
      KM=E-K
      CP=E+C
      CM=E-C
      ZP=E+Z
      ZM=E-Z
      YP=2.*E-K*(1.-C)
      YM=2.*E-K*(1.+C)
C...APPROXIMANT
      PROXIM=4./CM/ZM*(KM/K/CP+1./YP)+4./CP/ZP*(KM/K/CM+1./YM)
      PROXIM=ALFA**3/8./PI2/S*PROXIM
C...PHASE SPACE RENORMALIZATION
      PROXIM=PROXIM*2.*(E-K)/K/X**2
      RETURN
      END
      SUBROUTINE START(ENERGY,INITL)
C...SETUP ROUTINE FOR CONSTANTS, PARAMETERS AND CROSSECTIONS
      IMPLICIT REAL*8(A-Z)
      INTEGER INITL,IEV,ITRY,I3
      COMMON / CON1 / PI,M,ALFA,PI2,LE,E,DL,MU,S
      COMMON / CON2 / K0,RHO,THRSLD
      COMMON / CON3 / BETA,SIG0,SIGA
      COMMON / LK0C / LK0 / SIGTOT / SIGCOL
      COMMON / TRY / TTOT,IEV,ITRY,TTOT2,IPS
      COMMON / TRIGAM / I3
C... NATURAL CONSTANTS
      BARN=(1.9732858D0)**2*(1.D+05)
      PI=3.1415927D0
      ME=0.5110034D-03
      ALFA=1.D0/137.035982D0
      PI2=PI*PI
      K0=0.01
C Initialization cross sections (GB)
      SIG0   = 0.D0
      SIGA   = 0.D0
      SIGTOT = 0.D0
      SIGCOL = 0.D0
C ONE CAN FIDDLE AROUND WITH PARAMETER "K0".
C RESULTS SHOULD NOT DEPEND ON THIS AS LONG AS THE
C SO-CALLED "ANALYTIC CROSS SECTION" REMAINS POSITIVE.
C     THIS IS ONE OF THE PARAMETERS FOR SUBROUTINE EVENT
C...INPUT PARAMETERS
      S=4.D0*ENERGY*ENERGY
      M=ME/DSQRT(ENERGY*ENERGY-ME*ME)
      MU=0.5D0*M*M*(1.D0-0.25D0*M*M)
      E =1.D0+MU
      DL=2.D0+MU
      LE=DLOG(DL/MU)
      S=S/BARN
C...
      IF(I3.EQ.1)RHO=1.
      IF(I3.EQ.1)GO TO 180
C...COEFFICIENT OF INFRARED DIVERGENCY
      LK0=DLOG(K0)
      BETA=2.D0*ALFA/PI*(LE-1.D0)
C...LOWEST ORDER CROSSECTION
      SIG0=ALFA*PI2*BETA/S
C...ANALYTIC CORRECTION OF CROSSECTION
      SIGA=SIG0*(1.+BETA*LK0)
     Z   +2.*ALFA**3/S*(LE**3/6.+0.75*LE**2+(PI2/3.-3.)*LE-PI2/12.)
C...TOTAL CROSSECTIONS AND CORRECTION
C...EXACT TOTAL CROSSECTION INTEGRATED OVER ALL K (NO KMAX)
      SIGTOT=-SIG0*BETA*LK0  +  SIGA
     Z   +2.*ALFA**3*(-(LE-1.)**2+3.) /S
C...COLINEAR CROSSECTION INTEGRATED OVER ALL K
180   CONTINUE
      SIGCOL=YK(1.D00)
C...PARAMETERS FOR SUBROUTINE EVENT (COMPLETED INITIALIZATION GB)
      IEV=0
      IPS=0
      ITRY=0
      THRSLD=SIGA/SIGCOL
C QUANTITY "THRSLD" SHOULD BE ZERO IF ONLY THREE-
C PHOTON EVENTS ARE WANTED !!!!
      THRS=THRSLD*100.D0
C...INITIALIZE RANDOM NUMBER FUNCTION RNDM(DUMMY)
      SEED=1.*INITL
      CALL RDMIN(SEED)
      R=RNDM(0.)
C...PRINTOUT AND RETURN
      PRINT 1,ENERGY,INITL
    1 FORMAT(  / 5X,'BEAM ENERGY              =',F12.3,'  GEV'
     Z        ,/,5X,'RNDM #  INIT             =',I12)
      PRINT 2,K0,THRS,RHO
    2 FORMAT(  /,5X,'SOFT-HARD BORDER         =',F12.5
     Z        ,/,5X,'# SOFT EVENTS (COL.APP.) =',F12.5,' %'
     Z        ,/,5X,'TEST CUTOFF RHO          =',F12.5)
      PRINT 3,SIG0,SIGA,SIGTOT,SIGCOL
    3 FORMAT(  /,5X,'LOWEST ORDER CROSSECTION =',E12.4,' NB'
     Z        ,/,5X,'TOTAL ANALYTIC XSN       =',E12.4,' NB'
     Z        ,/,5X,'TOTAL EXACT XSN          =',E12.4,' NB'
     Z        ,/,5X,'TOTAL COLINEAR XSN       =',E12.4,' NB')
      RETURN
      END
      DOUBLE PRECISION FUNCTION TEST(DUMMY)
      IMPLICIT REAL*8(A-Z)
      INTEGER I1,I2,I3,I33,IODD,ICONF
      COMMON / GGDATA / EB,RHO,X11,A1MIN,A1MAX,AVETO,I33,IODD,ICONF
      COMMON / CON1 / PI,M,ALFA,PI2,LE,E,DL,MU,S
      COMMON / CON3 / BETA,SIG0,SIGA
      COMMON / LK0C / LK0
      COMMON / OUT1 / K,C,Z,FI,FG
      COMMON / OUT2 / SC,SZ,CGAM,X
C...COMPUTE RATIO BETWEEN EXACT CROSSECTION AND COLINEAR APPROXIMATION
C...IN BOTH EXPRESSIONS THERE IS AN OVERALL FACTOR
C...ALFA**3/8./PI**2/S LEFT OUT
      SC=DSQRT(1.-C*C)
      SZ=DSQRT(1.-Z*Z)
      K=E*K
      CGAM=C*Z+SC*SZ*COS(FI-FG)
      X=2.D0*E*(E-K)/(2.D0*E-K+K*CGAM)
      CALL VECT4
C...SYMMETRIC CUT
      TEST=0.
C
C GB Modification to take into account ICONF
C
      IF(ICONF.EQ.0) SCUT = 1.D0
      IF(ICONF.EQ.1) SCUT = CUTSGL(0.D0)
      IF(ICONF.EQ.2) SCUT = CUT(0.D0)
      IF(SCUT.EQ.1.)GO TO 200
      RETURN
200   CONTINUE
C..............................................................
      IF(K.EQ.(0.D0)) GO TO 1
C...HARD PART
C...CALCULATE ENERGY AND SCATTERING ANGLE OF THIRD PHOTON AS WELL
C...THIS IS NECESSARY TO CALCULATE SYMMETRIC APPROXIMANT
      X1=2.*E-K-X
      IF(X1.LE.0.)PRINT 11,X1
   11 FORMAT(5X,'  X1 <= 0',F20.16)
      C1=-(X*C+K*Z)/X1
      IF(DABS(C1).GT.1.)PRINT 12,C1
   12 FORMAT(5X,'  /C1/ > 1',F20.16)
C...CALCULATE VALUE OF APPROXIMANT AVERAGED OVER ALL PERMUTATIONS
      COLL=    PROXIM(  K,Z    ,X,C    ,X1,C1 )
     .       + PROXIM(  X,C    ,X1,C1  ,K,Z   )
     .       + PROXIM(  X1,C1  ,K,Z    ,X,C   )
     .       + PROXIM(  X1,C1  ,X,C    ,K,Z   )
     .       + PROXIM(  X,C    ,K,Z    ,X1,C1 )
     .       + PROXIM(  K,Z    ,X1,C1  ,X,C   )
      IF(COLL.LE.0.0) PRINT 13,COLL,K,C,Z
   13 FORMAT('COLL <= 0',4D15.7)
C...CALCULATE EXACT CROSSECTION
      ACOLL=EXACT(K,Z,X,C,X1,C1)
      IF(ACOLL.LE.0.) PRINT 14,ACOLL,K,Z,X,C,X1,C1
   14 FORMAT(5X,'  EXCT <= 0',7F15.6)
C...TEST IS THE RATIO BETWEEN THE TWO EXPRESSIONS
      TEST=ACOLL/COLL
      RETURN
C...SOFT CASE
C...REMEMBER THAT THE C DISTRIBUTION IN THE SOFT PART IS NOT
C...GENERATED AS (1+C**2)/(1-C**2) BUT AS 2/(1-C**2)
    1 TEST=1. + BETA*LK0
     Z +ALFA/2./PI*(LE*LE+PI2/3.+(E*E-C*C)/(E*E+C*C)*G(C))
C...NORMALIZE FOR CORRECT TOTAL NUMBER OF EVENTS
      TEST=TEST*SIG0/SIGA
C...NORMALIZE AGAIN FOR SLIGHTLY DIFFERENT ANGULAR DISTRIBUTION
      TEST=TEST*(1.+C*C)/2.*LE/(LE-1.)
      IF(TEST.LE.0.D0) PRINT 21,TEST,C
   21 FORMAT(5X,'TEST(K=0) <=0',2F20.16)
      RETURN
      END
      SUBROUTINE VECT4
C...CONSTRUCT 4VECTORS OUT OF GENERATED VARIABLES ;
C...THIS IS EXACT BECAUSE THE PARTICLES ARE MASSLES.
C...THE FOURTH COMPONENT IS THE ENERGY;THE THIRD COMPONENT
C...IS THE DIRECTION OF THE POSITRON BEAM
      IMPLICIT REAL*8(A-Z)
      COMMON / CON1 / PI,M,ALFA,PI2,LE,E,DL,MU,S
      COMMON / CON3 / BETA,SIG0,SIGA
      COMMON / OUT1 / E1,C,Z,FI,FG
      COMMON / OUT2 / SC,SZ,CGAM,E2
      COMMON / VEC4 / K(3,4)
      K(1,1)=E1*SZ*COS(FG)
      K(1,2)=E1*SZ*SIN(FG)
      K(1,3)=E1*Z
      K(1,4)=E1
      K(2,1)=E2*SC*COS(FI)
      K(2,2)=E2*SC*SIN(FI)
      K(2,3)=E2*C
      K(2,4)=E2
      K(3,1)=-K(1,1)-K(2,1)
      K(3,2)=-K(1,2)-K(2,2)
      K(3,3)=-K(1,3)-K(2,3)
      K(3,4)=-K(1,4)-K(2,4)+2.D0*E
      RETURN
      END
      DOUBLE PRECISION FUNCTION WEIZ(K,C)
      IMPLICIT REAL*8(A-Z)
      COMMON / CON1 / PI,M,ALFA,PI2,LE,E,DL,MU,S
      COMMON / CON2 / K0,RHO,THRSLD
      COMMON / CON3 / BETA,SIG0,SIGA
C...CALCULATE YES OR NO FOR REFLECTION OF Z TO -Z AFTER
C...GENERATING Z BY "GENCOS". THIS IS DONE BY CALCULATING THE
C...RATIO OF THE TWO COEFFICIENTS IN THE EXPRESSION FOR
C...THE Z DISTRIBUTION
      KM=1.-K
      CM=1.-C/E
      CP=1.+C/E
      COEFZM=(CP+KM**2*CM)*(2.-K*CP)
      COEFZP=(CM+KM**2*CP)*(2.-K*CM)
      WEIZ=COEFZM/(COEFZM+COEFZP)
      RETURN
      END
      DOUBLE PRECISION FUNCTION YC(C)
      IMPLICIT REAL*8(A-Z)
      COMMON / CON1 / PI,M,ALFA,PI2,LE,E,DL,MU,S
      COMMON / OUT1 / K,X,Z,FI,FG
C...CUMULATIVE INTEGRAL OVER "ORIGINAL" SCATTERING ANGLE
C...ONLY FOR HARD PART; FOR SOFT PART "GENCOS" IS USED
C...THERE IS AN OVERALL FACTOR 2.D0*ALFA**3/S*LOG(S/M**2)
C...WHICH IS LEFT OUT
      B1=DLOG((E+C)/(E-C))
      B2=DLOG((2.-K*(1.-C))/(2.-K*(1.+C)))
      YC=(1.-K)/K*B1+1./(2.+MU*K)*(B1+B2)
      RETURN
      END
      DOUBLE PRECISION FUNCTION YK(K)
      IMPLICIT REAL*8(A-Z)
      COMMON / CON1 / PI,M,ALFA,PI2,LE,E,DL,MU,S
      COMMON / CON2 / K0,RHO,THRSLD
      COMMON / CON3 / BETA,SIG0,SIGA
      DATA FACT/7.778D-07/
C     FACT=2.*ALFA**3
C...CUMULATIVE PHOTON MOMENTUM INTEGRAL FOR K LARGER THAN K0
C...FINITE PART
      DLOGK=0.
      IF(K.NE.1.)DLOGK=DLOG(1.-K)
      YK=FACT/S*LE*(2.*LE*DLOG(K/K0)-(LE-1.)*(K-K0)
     . +(1.-K)*DLOGK-(1.-K0)*DLOG(1.-K0))
C...ADD VIRTUAL AND SOFT FINITE TERMS AND RETURN
      YK=SIGA+YK
      RETURN
      END
