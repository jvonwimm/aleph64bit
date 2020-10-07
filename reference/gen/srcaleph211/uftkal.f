      SUBROUTINE UFTKAL(FIELD,
     +                  NCO, RRF, UUF, ZZF, SIGU, SIGZ, CORUZ, VV0IN,
     +                  CHI2IN, VV0, CC0, CHI2, NDEG )
C--------------------------------------------------------------------
C! FITTING ROUTINE FOR HELICES IN ALEPH
C!              ===>  INCLUDING MULTIPLE SCATTERING
C!              ===>  BASED ON EXTENDED KALMAN FILTERING
C!              ===>  CALLED BY UFTTRK
C!
CKEY COMPUTE FIT /INTERNAL
C!
C!    AUTHOR:   T. LOHSE
C!       Modified: Oct. 5/92 C. Gay.  Changed protection on
C!                 ABS(DET).LT.EPS to 1.E-25 from 1.E-15
C!    INPUT:  FIELD = magnetic field strength in kG
C!            VV0IN  = 5 INPUT TRACK PARAMETERS FROM
C!                     A PRELIMINARY FIT
C!              1 : 1/R         [1/CM]   NEG. IF CLOCKWISE
C!              2 : TAN(LAMBDA)  =DZ/DS} TAN(ANGLE TO X,Y PLANE)
C!              3 : PHI0        [0,2PI]} ANGLE TO X-AXIS
C!              4 : D0*SIGN      [CM]    MINIMAL DIST. TO Z-AXIS,
C!                                       sign OF ANGULAR MOM. LZ
C!              5 : Z0           [CM]    Z POS AT R=D0
C!            CHI2IN = CHI**2 OF PRELIMINARY FIT
C!
C!            NCO    = NUMBER OF COORDINATES
C!
C!            RRF    = ARRAY OF R- COORDINATES
C!            UUF    = ARRAY OF R-PHI-COORDINATES
C!            ZZF    = ARRAY OF Z-COORDINATES
C!            SIGU   = ARRAY OF SIGMA(U)**2
C!            SIGZ   = ARRAY OF SIGMA(Z)**2
C!            CORUZ  = ARRAY OF U-Z CORRELATIONS
C!
C!  OUTPUT:   VV0 = 6 FINAL TRACK PARAMETERS
C!              1 : 1/R          [1/CM]  NEG. IF CLOCKWISE
C!              2 : TAN(LAMBDA)  =DZ/DS} TAN(ANGLE TO X,Y PLANE)
C!              3 : PHI0        [0,2PI]} ANGLE TO X-AXIS
C!              4 : D0*SIGN      [CM]    MINIMAL DIST. TO Z-AXIS,
C!                                       sign OF ANGULAR MOM. LZ
C!              5 : Z0           [CM]    Z POS AT R=D0
C!              6 : ALPHA      [-PI,PI]: SCATTERING ANGLE
C!                                       AT ITC WALL (IN X-Y)
C!            CC0 = COVARIANCE MATRIX IN LOWER TRIANG. FORM
C!                     1
C!                     2  3
C!                     4  5  6
C!                     7  8  9 10
C!                    11 12 13 14 15
C!                    16 17 18 19 20 21
C!            CHI2= CHI SQUARED FROM LAST KALMAN FILTER STEP
C!            NDEG= NUMBER OF DEGREES OF FREEDOM
C!
C!  NOTE:     FIT DONE FOR THE 5 HELIX PARAMETERS ONLY.
C!            THE ANGLE ALPHA IS COMPUTED BUT ELEMENTS
C!            16,...,21 OF CC0 ARE DUMMY.
C!
C!  SWIMMING TO TRACK ORIGIN:
C!  =========================
C!            CALL UFSWIM( 1 ) ===>  FITS ARE GIVEN FOR TRACK
C!                                   ORIGIN FROM NOW ON
C!            CALL UFSWIM( 0 ) ===>  FITS ARE GIVEN AT INNERMOST
C!                                   COORDINATES FROM NOW ON
C!                                   (===> DEFAULT <===)
C!
C!  FILTERING OF BAD COORDINATES:
C!
C!            CALL UFTFIL( PRB )
C!            PRB   =  PROBABILITY TO THROW AWAY A GOOD (!)
C!                     COORDINATE WHEN FILTERING FOR BADIES
C!                     IN THE FIRST SMOOTHER STEP.
C!                     IF PRB=0, NO COORDINATE FILTERING WILL
C!                     BE DONE.
C|                     RECOMMENDED FOR FILTERING: PRB=5.E-4
C!
C!  CONSTRAINTS POSSIBLE:
C!
C!            OMEGA :       FIXED TO OMEFIX BY CALLING UFTOME(1)
C!                          RELEASED BY CALLING UFTOME(0)
C!
C!            TAN(LAMBDA) : FIXED TO TALFIX BY CALLING UFTTAL(1)
C!                          RELEASED BY CALLING UFTTAL(0)
C!                          IF TAN(LAMBDA) IS FIXED THERE IS NO
C!                          MULTIPLE SCATTERING IN S-Z PLANE
C!
C!            MOMENTUM :    FIXED TO PFIX (>0) BY CALLING
C!                          UFTMOM(1)
C!                          RELEASED BY CALLING UFTMOM(0)
C!
C!        THE VALUES FOR THE CONSTRAINTS ARE STEERED VIA
C!        COMMON /CONSTR/ OMEFIX, TALFIX, PFIX
C!
C!  SPECIAL MODE FOR EVENT DISPLAY APPLICATIONS:
C!
C!        CALL   DALINF(1)  TO FILL  COMMON / INFDAL /
C!        CALL   DALINF(0)  IF SMOOTHED LOCAL TRACK PARAMETERS
C!                          ARE NOT ANY LONGER NEEDED
C!
C!        DESCRIPTION OF COMMON / INFDAL /:
C!              NLOW  : INDEX OF FIRST COORDINATE USED
C!              NHIGH : INDEX OF LAST COORDINATE USED
C!              XSTS(5,40) : SMOOTHED STATE VECTORS  (DBLE PREC)
C!              XME(2,40)  : MEASUREMENT VECTORS (DOUBLE PREC)
C!              RF(40)     : RADII OF COORDINATES (DOUBLE PREC)
C!              IUSED(40)  : COORDINATE FLAGS
C!                           0 = NOT USED IN FIT
C!                           1 = USED IN FIT
C!              VMEAS(2,2,40) : COVARIANCE MATRIX OF COORDINATES
C!                                (DOUBLE PREC)
C!                  1,1   ===>  R-PHI ERROR SQUARED
C!                  2,2   ===>  Z ERROR SQUARED
C!                  1,2   ===>  CORRELATION (NORMALLY 0.)
C!                  2,1   ===>  SAME AS 1,2
C!
C!  SPECIAL STEERING INFORMATION FOR VDET PATTERN RECOGNITION
C!  =========================================================
C!      ENTRY POINT UFVDIN(NVDIN,ISAFVD,ISBEVD)
C!              INPUT:
C!              NVDIN  : NO OF VDET COORDINATES TO BE USED IN FILTER
C!              ISAFVD :  .NE. 0 <=> STOP AFTER VDET FILTERING (FLAG)
C!              ISBEVD :  .NE. 0 <=> START BEFORE VDET FILTERING (FLAG)
C!      ENTRY POINT UFVDOU(NVDOUT,CHISVD)
C!              OUTPUT:
C!              NVDOUT : NO OF VDET MEASUREMENTS SURVIVING THE FILTER
C!              CHI2VD : CHI**2 OF FIT AFTER VD FILTERING
C!
C----------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (EPS = 1.0E-15, ITMAX = 15, MPT=40, ZVARMA = 900.)
      PARAMETER (EPS1 = 1.0E-25)
      PARAMETER (UVARMA = 900.)
C
      COMMON / CONSTR / OMEFIX, TALFIX, PFIX
C
      COMMON / INFDAL / NLOW, NHIGH, XSTS, XME, RF, IUSED, VMEAS
C
      INTEGER IUSED(MPT)
      INTEGER IUSEDL(MPT)
      REAL   RRF(*), UUF(*), ZZF(*), SIGU(*), SIGZ(*), CORUZ(*)
      REAL    VV0IN(*), VV0(*), CC0(*)
      DOUBLE PRECISION  CTEMP(5,5),CWORK2(5,5)
      DOUBLE PRECISION  CWORK(5,5)
      DOUBLE PRECISION  WSCAL(5), SXY(MPT)
      DOUBLE PRECISION  RF(MPT), PF(MPT), ZF(MPT), WZF(MPT),
     1                  XF(MPT), YF(MPT), WF(MPT), CRFIZF(MPT)
      DOUBLE PRECISION XSTS(5,MPT), XSTP(5,MPT), XSTF(5,MPT),
     1                 XME(2,MPT),
     2                 CPRE(5,5,MPT), CFIL(5,5,MPT), CSMO(5,5,MPT),
     3                 QPROC(5,5,MPT), VMEAS(2,2,MPT), DFDX(5,5,MPT),
     4                 RESP(2,MPT), RESF(2,MPT), RESS(2,MPT),
     5                 RESMF(2,2,MPT), RESMS(2,2,MPT),
     6                 CHI2F(MPT), QQQ(2,2), CHI2L, TPRSMO, DTDRFI,
     7                 DTDPHI, DTDOME, DOMDRH, DLADTH, DP0DRH, DP0DPH,
     8                 DP0DRF, DD0DRF, DD0DPH, DD0DRH, DZ0DRF,
     9                 DZ0DZ,  DZ0DPH, DZ0DTH, DZ0DRH, DP0DTH, DD0DTH
      DOUBLE PRECISION VHCH(5,5), GAIN(5,2), SGAIN(5,5)
      DOUBLE PRECISION DELR, R2, S2, T2, ALP, TPROC,
     1                 XS1, XS2, XS3, XS4, XS5, CS, SS, DEN, FACT,
     2                 RK, RSFI, RCFI, STPHI, CTPHI, SUM, DET,
     3                 XFJ, YFJ, RFJ, PFJ, WFJ, ZFJ, WZFJ, CORJ,
     4                 PHIIN, PHIOUT, DS, ALPHA, SUM1, SCAL
      DOUBLE PRECISION SNA, SNB, XLL, XLOR, SXYTPC, SXYITC, CLAM
      DOUBLE PRECISION CUR,RR0,DD0,PH0,DZDS,ZZ0
      DOUBLE PRECISION DELTA, DELTAP, DELTAU
      DOUBLE PRECISION RMSO, RMSI
      DOUBLE PRECISION PI, PIO2, PIT2
      INTEGER IOMEF, ITALF, IMOMF, IDALF, IBPIP
      LOGICAL FIRST
      SAVE
      DATA FIRST/.TRUE./
      DATA IOMEF / 0 /, ITALF / 0 /, IMOMF / 0 /
      DATA ISWIM / 0 /
      DATA PRB / 0. / ,IDALF / 0 /
      DATA ISAFVD / 0 /, ISBEVD / 0 /
C! multiple-scattering constants in VDET,ITC,TPC
      COMMON /VRLDCOM/  UKRITC,UKSITC,UKRTPC,UKSTPC,UKSPITC,UKSPTPC,
     &                  UKRVAC,UKSVAC,UKZICA,UKRIICA,UKROICA,UKSICA,
     &                  UKZOCA,UKRIOCA,UKROOCA,UKSOCA
      REAL UKRITC,UKSITC,UKRTPC,UKSTPC,UKSPITC,UKSPTPC,UKRVAC,UKSVAC,
     &     UKZICA,UKRIICA,UKROICA,UKSICA,UKZOCA,UKRIOCA,UKROOCA,UKSOCA
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C ----------------------------------------------------------------
C-
C get the multiple scattering  material description from the database
C RETURN if VRLD bank is missing - job should STOP
C-
      CALL VRLDGT( IER)
      IF(IER.LT.0) GOTO 999
C
      IF(FIRST) THEN
        IF ( PRB .LT. 0. ) PRB = 0.
        IF ( PRB .GT. 1. ) PRB = 1.
        CH2CUT = 1.E30
        CH2CU1 = 1.E30
        IF ( PRB .GT. 0. )  THEN
          CH2CUT = -2.*ALOG(PRB)
          CH2CU1 = CHISIN(1.-PRB,1)
        ENDIF
        PI    = 2.0 * DASIN(1.0D0)
        PIO2  = 0.5*PI
        PIT2  = 2.0*PI
        FIRST =.FALSE.
      ENDIF
C
      IF (FIELD.EQ.0.) THEN
        CALL ALTELL('UFTKAL:  do not call fit with zero FIELD!',
     &               0,'RETURN')
        GOTO 999
      ENDIF
      ROVP = 1./(0.29979*FIELD/10.)*100.
      SCAFAC = .0141 * ROVP
C
      N = NCO
      IF(N.GT.MPT)                    GOTO 999
      IF(N.LT.3)                      GOTO 999
C
C---     FILL LOCAL BUFFERS
C---     CHECK FOR CONSTRAINTS
C
C protect against unphysical tracks
      IF(VV0IN(1)*VV0IN(4).GT.1.) GOTO 999
C
      RR0  = VV0IN(1)
      IF ( IOMEF .EQ. 1 ) RR0 = OMEFIX
      IF ( RR0 .EQ. 0. )   RR0 = .0000001
      SST  = 1.
      IF ( RR0 .LT. 0. )  SST = -1.
      PH0  = VV0IN(3)
      DD0  = VV0IN(4)
      ZZ0  = VV0IN(5)
      DZDS = VV0IN(2)
      IF ( ITALF .EQ. 1 ) DZDS = TALFIX
      IF ( IMOMF .EQ. 1 )
     +                    RR0 = SST/(ROVP*PFIX)*DSQRT(1.+DZDS*DZDS)
      CUR  = DABS(RR0)
      DO 10  I = 1, N
        RF(I)     = RRF(I)
        IF ( RF(I) .LE. 0. )          GOTO 999
        PF(I)     = UUF(I) / RRF(I)
        IF ( PF(I) .LT. 0. )   PF(I) = PF(I) + PIT2
        IF ( PF(I) .GT. PIT2 ) PF(I) = PF(I) - PIT2
        IF ( PF(I) .LT. 0. )          GOTO 999
        IF ( PF(I) .GT. PIT2 )        GOTO 999
        ZF(I)     = ZZF(I)
        XF(I)     = RF(I) * COS( PF(I) )
        YF(I)     = RF(I) * SIN( PF(I) )
        WF(I)     = 1./(SIGU(I) + .00000001)
        WZF(I)    = 1./(SIGZ(I) + .00000001)
        CRFIZF(I) = CORUZ(I)
   10 CONTINUE
C
C---     SORT RADII IN DECREASING ORDER TO PREPARE KALMAN FILTER
C
      DO 20  I = 1, N-1
        IF  ( RF(I) .LT. RF(I+1) )  THEN
          J    = I+1
          XFJ  = XF(J)
          YFJ  = YF(J)
          RFJ  = RF(J)
          PFJ  = PF(J)
          WFJ  = WF(J)
          ZFJ  = ZF(J)
          WZFJ = WZF(J)
          CORJ = CRFIZF(J)
 21       CONTINUE
            J1         = J
            J          = J-1
            XF(J1)     = XF(J)
            YF(J1)     = YF(J)
            RF(J1)     = RF(J)
            PF(J1)     = PF(J)
            WF(J1)     = WF(J)
            ZF(J1)     = ZF(J)
            WZF(J1)    = WZF(J)
            CRFIZF(J1) = CRFIZF(J)
            IF  ( J .EQ. 1 )             GOTO 22
            IF  ( RFJ .LT. RF(J-1) )     GOTO 22
            IF  ( RFJ .EQ. RF(J-1) )     GOTO 999
            GOTO 21
 22       CONTINUE
          XF(J)     = XFJ
          YF(J)     = YFJ
          RF(J)     = RFJ
          PF(J)     = PFJ
          WF(J)     = WFJ
          ZF(J)     = ZFJ
          WZF(J)    = WZFJ
          CRFIZF(J) = CORJ
        ENDIF
 20   CONTINUE
C***************************************************************C
C                                                               C
C           FIND USABLE COORDINATES                             C
C           =======================                             C
C   THE TRACK HAS TO COMPLETELY CROSS THE TPC PAD-ROW TO        C
C   JUSTIFY A HIT AT THE PAD-ROW RADIUS                         C
C   SIMILAR REQUIREMENTS HOLD FOR ITC / VD                      C
C***************************************************************C
      RK = (2./RR0 - DD0)*SST
      IF ( RK .GT. UKRTPC ) THEN
        RK = RK -10.
      ELSEIF ( RK .GT. UKRITC ) THEN
        RK = RK - 1.
      ELSE
        RK = RK - 0.01
      ENDIF
      N0 = 0
  140 N0 = N0 + 1
      IF ( N0.GT.N )                 GOTO 999
      IF ( RF(N0) .GE. RK )   GOTO 140
      RK = ABS( DD0 )
      IF ( RK .GT. UKRTPC ) THEN
        RK = RK + 10.
      ELSEIF ( RK .GT. UKRITC ) THEN
        RK = RK + 1.
      ELSE
        RK = RK - 0.01
      ENDIF
      N1 = N + 1
  150 N1 = N1 - 1
      IF ( N1.LT.N0 )                GOTO 999
      IF ( RF(N1) .LE. RK )   GOTO 150
      NOUT = N1 - N0 + 1
      IF  ( NOUT .LT. 3 )            GOTO 999
C
 1111 CONTINUE
      DO  130   II = 1, MPT
        IUSED(II)    = 1
        IUSEDL(II)   = 1
  130 CONTINUE
C
C===>  CHECK SPECIAL REQUIREMENTS FOR VDET PATTERN RECOGNITION
C
      N00 = N0
      IF ( ISBEVD .NE. 0 ) THEN
        N0 = N - NVDIN
        IF ( N0 .LT. N00 )   N0 = N00
      ENDIF
      IF ( N1 .LT. N0 )     GOTO 999
C
C***************************************************************C
C                                                               C
C           INITIALIZE KALMAN FILTER ALGORITHM                  C
C           ==================================                  C
C                                                               C
C***************************************************************C
C                                                               C
C   STATE VECTOR (1,K)  ===>  R*FI       AT R(K)                C
C                (2,K)  ===>  Z          AT R(K)                C
C                (3,K)  ===>  PHI        AT R(K)                C
C                (4,K)  ===>  LAMBDA     AT R(K)                C
C                (5,K)  ===>  OMEGA      AT R(K)                C
C                                                               C
C   "TIME"              ===>  PAD ROW RADIUS R(K)               C
C                                                               C
C   PREDICTED STATE VECTOR  ===>  XSTP(5,K)                     C
C   FILTERED  STATE VECTOR  ===>  XSTF(5,K)                     C
C   SMOOTHED  STATE VECTOR  ===>  XSTS(5,K)                     C
C                                                               C
C   MEASUREMENT VECTOR      ===>  XME(2,N)                      C
C             XME(1,K)  ===>  R*FI       AT R(K)                C
C             XME(2,K)  ===>  Z          AT R(K)                C
C                                                               C
C===============================================================C
C                                                               C
C   COVARIANCE MATRICES (SYMMETRIC):                            C
C     CPRE(5,5,K)   ===>  PREDICTION INTO ROW K                 C
C     CFIL(5,5,K)   ===>  FILTER IN ROW K                       C
C     CSMO(5,5,K)   ===>  SMOOTHING IN ROW K                    C
C                                                               C
C===============================================================C
C                                                               C
C   PROCESS NOISE MATRIX (WITH ALL CORRELATIONS):               C
C     QPROC(5,5,K) ===>  COVARIANCE MATRIX FOR STATE VECTOR     C
C                        FROM MULTIPLE SCATTERING ON THE WAY    C
C                        TO ROW K                               C
C                                                               C
C   MEASUREMENT NOISE MATRIX (DIAGONAL):                        C
C     VMEAS(2,2,K) ===>  COVARIANCE MATRIX FOR MEASUREMENT      C
C                        VECTOR FROM ERRORS IN THE TPC          C
C                        COORDINATES IN ROW K                   C
C                                                               C
C================================================================
C
C
C----------------------------------------------------------------
C---    USE PRELIMINARY FIT FOR INITIAL SMOOTHED STATE VECTOR
C---    THE JACOBIANS WILL BE COMPUTED AT THESE POSITIONS
C----------------------------------------------------------------
C
      CS  = DCOS(PH0)*(1.-DD0*RR0)
      SS  = DSIN(PH0)*(1.-DD0*RR0)
      DO 200  I = N0, N1
        RK        = RF(I)
        SXY(I)    = 2./RR0*DASIN( 0.5*RR0*
     +              DSQRT(DABS((RK**2-DD0**2)/(1.-RR0*DD0))) )
        XSTS(2,I) = ZZ0  + SXY(I)*DZDS
        XSTS(3,I) = PH0  + RR0*SXY(I)
        IF (XSTS(3,I).LT.0.)   XSTS(3,I) = XSTS(3,I)+PIT2
        IF (XSTS(3,I).GT.PIT2) XSTS(3,I) = XSTS(3,I)-PIT2
        XSTS(4,I) = ATAN( DZDS )
        XSTS(5,I) = RR0
        RSFI = - DCOS(PH0+RR0*SXY(I)) + CS
        RCFI = + DSIN(PH0+RR0*SXY(I)) - SS + RK*RR0
        IF (ABS(RCFI).LT.EPS) THEN
          IF (RCFI.GT.0.) THEN
            RCFI = EPS
                          ELSE
            RCFI = -EPS
          ENDIF
        ENDIF
        XSTS(1,I) = 2. * DATAN( RSFI/RCFI )
        IF (XSTS(1,I) .LT. 0. )      XSTS(1,I) = XSTS(1,I) + PIT2
        IF (XSTS(1,I)-PF(I) .GT. PI) XSTS(1,I) = XSTS(1,I) - PIT2
        IF (XSTS(1,I)-PF(I) .LT.-PI) XSTS(1,I) = XSTS(1,I) + PIT2
        XSTS(1,I) = RK * XSTS(1,I)
  200 CONTINUE
C
C----------------------------------------------------------------
C---    MEASUREMENT NOISE AND VECTOR
C----------------------------------------------------------------
C
      DO  210  I = 1, N
        VMEAS(1,1,I) = (WF(I))**(-1)
        VMEAS(2,2,I) = (WZF(I))**(-1)
        VMEAS(1,2,I) = CRFIZF(I)
        VMEAS(2,1,I) = CRFIZF(I)
        XME(1,I)   =  RF(I)*PF(I)
        XME(2,I)   =  ZF(I)
  210 CONTINUE
C
C----------------------------------------------------------------
C---    PROCESS NOISE (MULTIPLE SCATTERING)
C----------------------------------------------------------------
C
C--->   GENERAL PARAMETER OF TRACK
      CLAM   = 1. + DZDS**2
      SCACO  = (SCAFAC*RR0)**2 / CLAM
C--->  INITIALIZE M.S. COVARIANCE MATRICES
      DO  221  J = 1, 5
        DO  220  I = N0, N1
          QPROC(J,1,I) = 0.
          QPROC(J,2,I) = 0.
          QPROC(J,3,I) = 0.
          QPROC(J,4,I) = 0.
          QPROC(J,5,I) = 0.
  220   CONTINUE
  221 CONTINUE
C--->   LOOP OVER COORDINATE PAIRS
      DO  222  I = N0, N1
        IF ( ISWIM.EQ.0 .AND. I.EQ.N1 )      GOTO 222
        IF ( I .LT. N1 ) THEN
          RIN  = RF(I+1)
                         ELSE
          RIN  =  ABS( DD0 ) + 0.00001
        ENDIF
        ROUT = RF(I)
        QQQ(1,1) = 0.
        QQQ(1,2) = 0.
        QQQ(2,1) = 0.
        QQQ(2,2) = 0.
        DDR = ROUT - RIN
        XLL = SXY(I)
        IF (I.LT.N1) XLL = SXY(I)-SXY(I+1)
C--->    CALCULATE MATRICES
C
C--->    GAS OF TPC AND TPC/ITC WALL
        XL4 = 0.
        IF ( ROUT .GT. UKRTPC )  THEN
          XL4 = XLL
          IF ( RIN .LE. UKRTPC ) THEN
            SXYTPC = 2./RR0*DASIN( 0.5*RR0*
     +               DSQRT(DABS((UKRTPC**2-DD0**2)/(1.-RR0*DD0))) )
            XL4 = SXY(I)-SXYTPC
            XLOR = UKRTPC*RR0*DSQRT(CLAM)
     +             /DSIN(RR0*SXYTPC)/(1.-DD0*RR0)
          ENDIF
C---> EFFECTIVE LENGTH DUE TO MULTIPLE SCATTERING
          XL4 = XL4*(1.+0.35355*XL4*UKSPTPC*SCACO)
          XL4 = XL4 * DSQRT( CLAM )
          QQQ(1,1) = SCACO*UKSPTPC*XL4**3/3.
          QQQ(1,2) = SCACO*UKSPTPC*XL4*XL4*.5
          QQQ(2,1) = QQQ(1,2)
          QQQ(2,2) = SCACO*UKSPTPC*XL4
          IF ( RIN .LE. UKRTPC )
     +      QQQ(2,2) = QQQ(2,2) + SCACO*UKSTPC*XLOR
        ENDIF
C--->    GAS OF ITC AND ITC INNER WALL
        XL3 = 0.
        IF ( RIN .LT. UKRTPC ) THEN
          IF ( RIN .LE. UKRITC .AND. ROUT .GT. UKRITC ) THEN
            SXYITC = 2./RR0*DASIN( 0.5*RR0*
     +               DSQRT(DABS((UKRITC**2-DD0**2)/(1.-RR0*DD0))) )
            XLOR = UKRITC*RR0*DSQRT(CLAM)
     +             /DSIN(RR0*SXYITC)/(1.-DD0*RR0)
            XL3 = SXY(I) - SXYITC
            IF ( ROUT .GT. UKRTPC ) XL3 = SXYTPC - SXYITC
          ELSEIF ( RIN .GT. UKRITC .AND. ROUT .GT. UKRTPC) THEN
            XL3 = XLL - SXY(I) + SXYTPC
          ELSEIF ( RIN .GT. UKRITC .AND. ROUT .LE. UKRTPC) THEN
            XL3 = XLL
          ENDIF
C---> EFFECTIVE LENGTH DUE TO MULTIPLE SCATTERING
          XL3 = XL3*(1.+0.35355*XL3*UKSPITC*SCACO)
          XL3 = XL3 * DSQRT( CLAM )
          QQQ(1,1) = QQQ(1,1) + 2.*XL3*QQQ(1,2) + XL3*XL3*QQQ(2,2)
          QQQ(1,2) = QQQ(1,2) + XL3*QQQ(2,2)
          QQQ(1,1) = QQQ(1,1) + SCACO*UKSPITC*XL3**3/3.
          QQQ(1,2) = QQQ(1,2) + SCACO*UKSPITC*XL3*XL3*.5
          QQQ(2,1) = QQQ(1,2)
          QQQ(2,2) = QQQ(2,2) + SCACO*UKSPITC*XL3
          IF ( RIN .LE. UKRITC .AND. ROUT .GT. UKRITC )
     +      QQQ(2,2) = QQQ(2,2) + SCACO*UKSITC*XLOR
        ENDIF
C--->    MATERIAL OF VDET (APPROXIMATED AS HOMOGENEOUS MATERIAL)
        RMSO = ROUT
        IF ( ROUT .GT. UKRITC )    RMSO = UKRITC
        RMSI = RIN
C--->    IF NO SWIMMING IS REQUIRED FLAG LAST COORDINATE BY
C        NEGATIVE RADIUS
        IF ( ISWIM .EQ. 0 .AND. I .EQ. N1-1 )   RMSI = - RMSI
        IF ( RMSO .GT. DABS(RMSI) )
     +       CALL UFVDMS( FIELD, RR0, DZDS, PH0, DD0, ZZ0,
     +                    RMSI, RMSO, QQQ )
C
C--->     CONVERT INTO COVARIANCE MATRICES
C--->     FOR LOCAL HELIX PARAMETERS
        IF ( I .EQ. N1 ) THEN
C           THESE ARE THE MULTIPLE SCATTERING PARAMETERS FOR THE
C           GLOBAL FIT PARAMETERS ON THE WAY TO THE ORIGIN
          FACT = DSQRT(1.+DZDS**2)
          SUM  = 1. - RR0*DD0/FACT**2
          DEN  = 1. - DD0*RR0
          ALP  = 1./DSQRT(1.+1./DZDS**2)
          IF ( DZDS .LT. 0. )   ALP = - ALP
          IF ( IOMEF .EQ. 0 ) THEN
            QPROC(1,1,I) = (RR0*DZDS)**2*QQQ(2,2)
            IF ( ITALF .EQ. 0 ) THEN
              QPROC(1,2,I) = RR0*DZDS*FACT**2 * QQQ(2,2)
              QPROC(2,1,I) = QPROC(1,2,I)
            ENDIF
            QPROC(1,3,I) = -RR0**2*DZDS*ALP/DEN * QQQ(1,2)
            QPROC(3,1,I) =  QPROC(1,3,I)
            QPROC(1,5,I) = -RR0*DZDS*SUM/DEN*FACT * QQQ(1,2)
            QPROC(5,1,I) = QPROC(1,5,I)
          ENDIF
          IF ( ITALF .EQ. 0 )  THEN
            QPROC(2,2,I) = FACT**4 * QQQ(2,2)
            QPROC(2,3,I) = -RR0*ALP*FACT**2/DEN * QQQ(1,2)
            QPROC(3,2,I) = QPROC(2,3,I)
            QPROC(2,5,I) = -FACT**3*SUM/DEN * QQQ(1,2)
            QPROC(5,2,I) = QPROC(2,5,I)
          ENDIF
          QPROC(3,3,I) = (FACT/DEN)**2 * QQQ(2,2) +
     +                   (RR0*ALP/DEN)**2 * QQQ(1,1)
          QPROC(3,4,I) = +FACT/DEN * QQQ(1,2)
          QPROC(4,3,I) = QPROC(3,4,I)
          QPROC(3,5,I) = DD0*DZDS*FACT**2/DEN**2 * QQQ(2,2)
     +                 + RR0*DZDS*SUM/DEN**2*QQQ(1,1)
          QPROC(5,3,I) = QPROC(3,5,I)
          QPROC(4,4,I) = QQQ(1,1)
          QPROC(4,5,I) = +DD0*DZDS*FACT/DEN * QQQ(1,2)
          QPROC(5,4,I) = QPROC(4,5,I)
          QPROC(5,5,I) = (DD0*DZDS*FACT/DEN)**2 * QQQ(2,2)
     +                   + (FACT*SUM/DEN)**2 * QQQ(1,1)
                         ELSE
C
C--->  THIS IS FOR LOCAL HELIX PARAMETERS
C
          SNA  = 1./DCOS(XSTS(1,I+1)/RF(I+1)-XSTS(3,I+1))
          SNB  = 1./DCOS(XSTS(4,I+1))
          ALP  = DSIN(XSTS(4,I+1))
          SUM  = DTAN( XSTS(1,I+1)/RF(I+1) - XSTS(3,I+1) )
          FACT = DTAN(XSTS(4,I+1)) * SUM
C           NOTE: MS IS BEING DONE BACKWARDS ===> ANGLES HAVE
C                 TO BE COUNTED NEGATIVE
          QPROC(1,1,I) =  QQQ(1,1) * SNA*SNA
          QPROC(1,3,I) = -QQQ(1,2)*SNA*SNB
     +                  - QQQ(1,1)*XSTS(5,I+1)*SNA*SUM
          QPROC(3,1,I) =  QPROC(1,3,I)
          QPROC(3,3,I) = QQQ(2,2)*SNB**2
     +                        + XSTS(5,I+1)**2*(ALP**2+SUM**2)*QQQ(1,1)
     +                        + 2.*XSTS(5,I+1)*SUM*SNB*QQQ(1,2)
C--->     MULTIPLE SCATTERING IN LAMBDA ONLY IF LAMBDA IS NOT FIXED
          QPROC(2,2,I) =  QQQ(1,1) * ( SNB*SNB + FACT*FACT )
          IF ( ITALF .EQ. 0 )  THEN
            QPROC(2,4,I) = -QQQ(1,2) * SNB
            QPROC(4,2,I) =  QPROC(2,4,I)
            QPROC(4,4,I) =  QQQ(2,2)
            QPROC(3,4,I) = -XSTS(5,I+1)*ALP * QQQ(1,2)
            QPROC(4,3,I) =  QPROC(3,4,I)
          ENDIF
          QPROC(2,3,I) = +QQQ(1,2)*FACT*SNB
     +                 + QQQ(1,1)*XSTS(5,I+1)*DTAN(XSTS(4,I+1))*SNA**2
          QPROC(3,2,I) =  QPROC(2,3,I)
          QPROC(1,2,I) = -QQQ(1,1)*FACT*SNA
          QPROC(2,1,I) =  QPROC(1,2,I)
C---> CORRELATED CHANGE IN CURVATURE IF OMEGA IS NOT FIXED
          IF ( IOMEF .EQ. 0 )  THEN
            FACT = XSTS(5,I+1) * DTAN(XSTS(4,I+1))
            QPROC(5,5,I) = FACT*FACT*QPROC(4,4,I)
            QPROC(5,4,I) = FACT*QPROC(4,4,I)
            QPROC(4,5,I) = QPROC(5,4,I)
            QPROC(5,2,I) = FACT*QPROC(4,2,I)
            QPROC(2,5,I) = QPROC(5,2,I)
            QPROC(5,3,I) = FACT*QPROC(4,3,I)
            QPROC(3,5,I) = QPROC(5,3,I)
          ENDIF
        ENDIF
  222 CONTINUE
C
C----------------------------------------------------------------
C     INITIALIZE JACOBIAN
C----------------------------------------------------------------
C
      DO  235  I = 1, 5
      DO  235  J = 1, I
        IF ( I .NE. J ) THEN
          DO  236  L = N0, N1
            DFDX(I,J,L) = 0.
            DFDX(J,I,L) = 0.
  236     CONTINUE
                        ELSE
          DO  237  L = N0, N1
            DFDX(I,I,L) = 1.D0
  237     CONTINUE
        ENDIF
  235 CONTINUE
C
C--->  IF THE VDET IS FILTERED ALONE, THE STARTING VECTOR AT
C--->  THE INNERMOST ITC/TPC COORDINATE IS ALREADY DEFINED
C
      IF ( ISBEVD .NE. 0 )       GOTO 207
C
C----------------------------------------------------------------
C---    STARTING VECTOR WITH INFINITE COVARIANCE MATRIX
C---    DERIVED FROM PRELIMINARY HELIX FIT
C----------------------------------------------------------------
C
      NCOUNT = 0
      CHI2L  = CHI2IN
      IBADCL = 0
 1000 CONTINUE
C
      ITCNOZ = 0
      IVDNOU = 0
      IBADC  = 0
      RK = RF(N0)
      XSTP(1,N0) = XSTS(1,N0)
      XSTP(2,N0) = XSTS(2,N0)
      XSTP(3,N0) = XSTS(3,N0)
      XSTP(4,N0) = XSTS(4,N0)
      XSTP(5,N0) = XSTS(5,N0)
      RESP(1,N0) = PF(N0)*RK - XSTP(1,N0)
      RESP(2,N0) = ZF(N0)    - XSTP(2,N0)
      ALP        = RESP(1,N0) / RF(N0)
      IF  ( ALP .GT. PI )  ALP = ALP - PIT2
      IF  ( ALP .LT.-PI )  ALP = ALP + PIT2
      RESP(1,N0) = ALP * RF(N0)
C
C----     LARGE COVARIANCE MATRIX
C
      DO  206  I = 1, 5
        DO  205  J = 1, 5
          CPRE(I,J,N0) = 0.
  205   CONTINUE
  206 CONTINUE
      FACT        = ABS(PF(N0) - XSTP(1,N0)/RK)
      FAC         = FACT/PIT2
      FACT        = FACT - IFIX(FAC)*PIT2
      IF (FACT .GT. PI)   FACT = PIT2 - FACT
      CPRE(1,1,N0) = 100./WF(N0)  + (RK*FACT)**2
      CPRE(2,2,N0) = 100./WZF(N0) + (ZF(N0)-XSTP(2,N0))**2
      CPRE(3,3,N0) = 1.
      CPRE(4,4,N0) = 1.
      CPRE(5,5,N0) = 4.*CPRE(1,1,N0)/RK**4 + XSTP(5,N0)**2
C
C--->  CHECK WHETHER USER REQUESTED CONSTRAINTS
C
      IF ( IOMEF .EQ. 1 ) THEN
        XSTP(5,N0)   = OMEFIX
        CPRE(5,5,N0) = 0.
      ENDIF
      IF ( ITALF .EQ. 1 ) THEN
        XSTP(4,N0)   = ATAN( TALFIX )
        CPRE(4,4,N0) = 0.
      ENDIF
      IF ( IMOMF .EQ. 1 ) THEN
        XSTP(5,N0)   = SST/(ROVP*PFIX*DCOS(XSTP(4,N0)))
        CPRE(5,5,N0) = 0.
      ENDIF
C
C----------------------------------------------------------------
C---    RECURSION FOR KALMAN FILTER STARTS HERE
C----------------------------------------------------------------
C
      NCOUNT = NCOUNT + 1
  207 CONTINUE
      DO  500  K = N0, N1
C
C----------------------------------------------------------------
C---    STEP 1 :  FILTER
C----------------------------------------------------------------
C
C
C--->   CHECK WHETHER Z IS REALLY MEASURED
C
        IF ( VMEAS(2,2,K) .GT. ZVARMA )  THEN
          IF ( IUSED(K) .GT. 0 .AND. ISAFVD .EQ. 0 )
     +               ITCNOZ = ITCNOZ + 1
          RESP(2,K) = 0.
          XME(2,K)  = XSTP(2,K)
        ENDIF
C
C--->   CHECK WHETHER R-PHI IS REALLY MEASURED
C
        IF ( VMEAS(1,1,K) .GT. UVARMA ) THEN
          IF ( IUSED(K) .GT. 0 .AND. ISAFVD .EQ. 0 )
     +               IVDNOU = IVDNOU + 1
          RESP(1,K) = 0.
          XME(1,K)  = XSTP(1,K)
        ENDIF
C
C--->   GET KALMAN GAIN MATRIX
C
C--->   CHECK WHETHER THE COORDINATE IS TO BE USED
C--->   IF NOT, THE GAIN MATRIX IS 0
C
        IF ( IUSED(K) .GT. 0 )  THEN
          VHCH(2,2) = VMEAS(1,1,K) + CPRE(1,1,K)
          VHCH(1,2) =-VMEAS(1,2,K) - CPRE(1,2,K)
          VHCH(1,1) = VMEAS(2,2,K) + CPRE(2,2,K)
          DET = ( VHCH(2,2)*VHCH(1,1) - VHCH(1,2)**2 )**(-1)
          VHCH(1,1) = VHCH(1,1) * DET
          VHCH(2,2) = VHCH(2,2) * DET
          VHCH(1,2) = VHCH(1,2) * DET
          VHCH(2,1) = VHCH(1,2)
          DO  240  I = 1, 5
            DO  241  J = 1, 2
              GAIN(I,J) = CPRE(I,1,K)*VHCH(1,J) +
     +                    CPRE(I,2,K)*VHCH(2,J)
  241       CONTINUE
  240     CONTINUE
                                ELSE
          IBADC = IBADC + 1
          IF ( NOUT-IBADC .LT. 3 )                   GOTO 999
          DO  242  I = 1, 5
            GAIN(I,1) = 0.
            GAIN(I,2) = 0.
  242     CONTINUE
        ENDIF
C
C--->   FILTERED STATE VECTOR
C
        DO  250  I = 1, 5
          XSTF(I,K) = XSTP(I,K) + GAIN(I,1)*RESP(1,K)
     +                          + GAIN(I,2)*RESP(2,K)
  250   CONTINUE
        ALP = XSTF(1,K)/RF(K)
        IF (ALP.LT.0.  )      ALP = ALP+PIT2
        IF (ALP.GT.PIT2)      ALP = ALP-PIT2
        XSTF(1,K) = ALP*RF(K)
        ALP = XSTF(3,K)
        IF (ALP.LT.0.)        ALP = ALP+PIT2
        IF (ALP.GT. PIT2)     ALP = ALP-PIT2
        XSTF(3,K) = ALP
        ALP = XSTF(4,K)
        IF (ALP.LT.-PIO2)      ALP = ALP+PI
        IF (ALP.GT.PIO2)      ALP = ALP-PI
        XSTF(4,K) = ALP
C
C--->   FILTERED COVARIANCE MATRIX
C
        DO  260  I = 1, 5
          DO 261  J = 1, I
            CFIL(I,J,K) = CPRE(I,J,K) - GAIN(I,1)*CPRE(1,J,K)
     +                                - GAIN(I,2)*CPRE(2,J,K)
            CFIL(J,I,K) = CFIL(I,J,K)
  261     CONTINUE
  260   CONTINUE
C
C--->   CHECK WHETHER USER REQUESTED FIXED MOMENTUM
C
        IF ( IMOMF .EQ. 1 )   THEN
          XSTF(5,K) = SST/(ROVP*PFIX*DCOS(XSTF(4,K)))
          FACT = XSTF(5,K) * DTAN(XSTF(4,K))
          CFIL(5,5,K) = FACT * FACT * CFIL(4,4,K)
          CFIL(1,5,K) = FACT * CFIL(1,4,K)
          CFIL(5,1,K) = CFIL(1,5,K)
          CFIL(2,5,K) = FACT * CFIL(1,4,K)
          CFIL(5,2,K) = CFIL(1,5,K)
          CFIL(3,5,K) = FACT * CFIL(1,4,K)
          CFIL(5,3,K) = CFIL(1,5,K)
          CFIL(4,5,K) = FACT * CFIL(1,4,K)
          CFIL(5,4,K) = CFIL(1,5,K)
        ENDIF
C
C--->   FILTERED RESIDUALS
C
        ALP       = RESP(1,K) - GAIN(1,1)*RESP(1,K)
     +                        - GAIN(1,2)*RESP(2,K)
        ALP       = ALP / RF(K)
        IF  ( ALP .GT. PI )  ALP = ALP - PIT2
        IF  ( ALP .LT.-PI )  ALP = ALP + PIT2
        RESF(1,K) = ALP*RF(K)
        RESF(2,K) = RESP(2,K) - GAIN(2,1)*RESP(1,K)
     +                        - GAIN(2,2)*RESP(2,K)
C
C--->   COVARIANCE MATRIX OF FILTERED RESIDUALS
C
C--->   ....OF COORDINATES USED IN THE FIT
C
        IF ( IUSED(K) .GT. 0 ) THEN
          RESMF(1,1,K) = VMEAS(1,1,K) - CFIL(1,1,K)
          RESMF(1,2,K) = VMEAS(1,2,K) - CFIL(1,2,K)
          RESMF(2,1,K) = RESMF(1,2,K)
          RESMF(2,2,K) = VMEAS(2,2,K) - CFIL(2,2,K)
                               ELSE
C
C--->   ....  AND FOR THOSE PRESENTLY NOT USED
C
          RESMF(1,1,K) = VMEAS(1,1,K) + CFIL(1,1,K)
          RESMF(1,2,K) = VMEAS(1,2,K) + CFIL(1,2,K)
          RESMF(2,1,K) = RESMF(1,2,K)
          RESMF(2,2,K) = VMEAS(2,2,K) + CFIL(2,2,K)
        ENDIF
C
C--->   IF THE COORDINATE IS FILTERED OUT, THE REST IS NOT NECESSARY
C
        IF  ( IUSED(K) .EQ. 0 )  THEN
          IF ( K .GT. N0 )  THEN
            CHI2F(K) = CHI2F(K-1)
                           ELSE
            CHI2F(K) = 0.
          ENDIF
          GOTO 270
        ENDIF
C
C--->   FILTERED CHI-SQUARED
C
        DET = RESMF(1,1,K)*RESMF(2,2,K) - RESMF(1,2,K)**2
        IF ( ABS(DET) .LT. EPS1 )   DET = EPS1
        CHI2F(K) = ( RESMF(2,2,K)*RESF(1,K)**2
     +             + RESMF(1,1,K)*RESF(2,K)**2
     +             - 2.*RESMF(1,2,K)*RESF(1,K)*RESF(2,K) ) / DET
        IF ( K .GT. N0 ) CHI2F(K) = CHI2F(K) + CHI2F(K-1)
        IF ( K.EQ.N0 .AND. ISBEVD.NE.0 .AND. N0.GT.N00 )
     +                   CHI2F(K) = CHI2F(K) + CHI2F(K-1)
  270   CONTINUE
C
C----------------------------------------------------------------
C---    STEP 2 :  PREDICTION
C----------------------------------------------------------------
C
        IF ( K .EQ. N1 )           GOTO 500
C
C--->   NONLINEAR TRANSPORT OF HELIX PARAMETERS TO NEXT PAD-ROW
C
C
C--->   ADVANCE IN PROCESS-TIME
C
        DELR = RF(K+1) - RF(K)
        R2   = DELR * ( 2.*RF(K) + DELR )
        S2   = 2.*RF(K)/XSTF(5,K)
        T2   = 2./XSTF(5,K)**2
        ALP  = XSTF(3,K) - XSTF(1,K)/RF(K)
        FACT = S2 * DCOS(ALP)
        DEN  = R2 - 2.*T2 + 2.*S2*DSIN(ALP)
        IF ( ABS(FACT) .LT. EPS )          GOTO  999
        TPROC = R2*DEN/FACT**2
        IF ( ABS(TPROC)  .LT. 1.E-7 )   THEN
          TPROC  =  0.5*R2/FACT
                                        ELSE
          TPROC  = 1. - TPROC
C
C===>   STABILITY OF EXTRAPOLATION CHECK (LOW MOMENTUM TRACKS)
C
          IF ( TPROC .LT. 0. )     THEN
C--->  IF THIS HAPPENS IN VDET PATTERN RECOGNITION THIS IS CERTAINLY
C--->  NOT A SOLUTION
            IF ( ISAFVD .NE. 0 )  GOTO 999
            IF ( K-N0 .LE. N1-K )  THEN
              N0   = K + 1
                                   ELSE
              N1   = K
            ENDIF
            NOUT = N1 - N0 + 1
            IF (NOUT.LT.3)        GOTO 999
            NCOUNT = NCOUNT - 1
            GOTO   1111
          ENDIF
          TPROC  = FACT/DEN * ( 1. - DSQRT( TPROC ) )
        ENDIF
        TPROC  = 2. * DATAN( TPROC )
C
C--->   DO TRANSPORT OF STATE VECTOR
C
        XSTP(5,K+1)    =  XSTF(5,K)
        XSTP(4,K+1)    =  XSTF(4,K)
        ALP            =  XSTF(3,K) + TPROC
        IF ( ALP .LT. 0. )    ALP = ALP + PIT2
        IF ( ALP .GT. PIT2 )  ALP = ALP - PIT2
        XSTP(3,K+1)    =  ALP
        XSTP(2,K+1)    =  XSTF(2,K) + TPROC*DTAN(XSTF(4,K))/XSTF(5,K)
        STPHI =  DSIN( ALP )
        CTPHI =  DCOS( ALP )
        FACT   =  ( RF(K)*DSIN(XSTF(1,K)/RF(K))
     +           + (DCOS(XSTF(3,K))-CTPHI)/XSTF(5,K) ) / RF(K+1)
        DEN    =  ( RF(K)*DCOS(XSTF(1,K)/RF(K))
     +           - (DSIN(XSTF(3,K))-STPHI)/XSTF(5,K) ) / RF(K+1)
        IF ( DABS(FACT) .GT. DABS(DEN) )   THEN
          FACT = 2. * DATAN( (1.-DEN)/FACT )
                                           ELSE
          FACT = 2. * DATAN( (1.+FACT)/DEN ) - PIO2
        ENDIF
        IF  (  FACT .LT. 0.  )        FACT = FACT + PIT2
        IF ( FACT-PF(K+1) .GT. PI )   FACT = FACT - PIT2
        IF ( FACT-PF(K+1) .LT. -PI)   FACT = FACT + PIT2
        XSTP(1,K+1)    =  FACT * RF(K+1)
C
C--->   GET JACOBIAN OF TRANSPORT MAPPING AT SMOOTHED POINT
C--->   (THIS IS THE APPROXIMATION)
C
        S2   = 2.*RF(K)/XSTS(5,K)
        T2   = 2./XSTS(5,K)**2
        ALP  = XSTS(3,K) - XSTS(1,K)/RF(K)
        FACT = S2 * DCOS(ALP)
        DEN  = R2 - 2.*T2 + 2.*S2*DSIN(ALP)
        IF ( ABS(FACT) .LT. EPS )          GOTO  999
        TPRSMO = R2*DEN/FACT**2
        IF ( ABS(TPRSMO)  .LT. 1.E-7 )   THEN
          TPRSMO =  0.5*R2/FACT
                                         ELSE
          TPRSMO = 1. - TPRSMO
          IF ( TPRSMO .LT. 0. )               GOTO  999
          TPRSMO  = FACT/DEN * ( 1. - DSQRT( TPRSMO ) )
        ENDIF
        TPRSMO  = 2. * DATAN( TPRSMO )
        XS1    =  XSTS(1,K)
        XS2    =  XSTS(2,K)
        XS3    =  XSTS(3,K)
        XS4    =  XSTS(4,K)
        XS5    =  XSTS(5,K)
        RK     =  RF(K)
C
        CTPHI  =  DCOS( XS3 )
        STPHI  =  DSIN( XS3 )
        CS     =  DCOS( XS1/RK )
        SS     =  DSIN( XS1/RK )
        ALP    =  XS3 - XS1/RK
        FACT   =  DCOS( ALP + TPRSMO )
        DEN    =  ( DSIN(TPRSMO)/XS5 + RK*FACT )**(-1)
        DTDRFI =  ( FACT - DCOS(ALP) ) * DEN
        DTDPHI =  -RK*DTDRFI
        DTDOME =  DEN/XS5* ( 4.*(DSIN(0.5*TPRSMO))**2/XS5
     +             + RK*(DSIN(ALP+TPRSMO)-DSIN(ALP)) )
        ALP    =  XS3 + TPRSMO
        DEN    =  DSIN( ALP )
        FACT   =  RK*CS - (STPHI-DEN)/XS5
        FACT   =  RF(K+1) / FACT
        DFDX(1,1,K) = FACT*( CS+DTDRFI*DEN/XS5 )
        DFDX(1,3,K) = FACT/XS5*( (1.+DTDPHI)*DEN - STPHI )
        DFDX(1,5,K) = FACT/XS5*( (DCOS(XS3+TPRSMO) - CTPHI)/XS5
     +                + DEN*DTDOME )
        FACT        = DTAN(XS4) / XS5
        DFDX(2,1,K) = DTDRFI * FACT
        DFDX(2,3,K) = DTDPHI * FACT
        DFDX(2,4,K) = TPRSMO / ( XS5 * (DCOS(XS4))**2 )
        DFDX(2,5,K) = FACT * ( DTDOME - TPRSMO/XS5 )
        DFDX(3,1,K) = DTDRFI
        DFDX(3,3,K) = 1. + DTDPHI
        DFDX(3,5,K) = DTDOME
C
C--->  CHECK WHETHER USER REQUESTED CONSTRAINTS
C
        IF ( IOMEF .EQ. 1 ) THEN
          DO  290  I = 1, 5
            DFDX(I,5,K) = 0.
            DFDX(5,I,K) = 0.
  290     CONTINUE
        ENDIF
        IF ( ITALF .EQ. 1 ) THEN
          DO  291  I = 1, 5
            DFDX(I,4,K) = 0.
            DFDX(4,I,K) = 0.
  291     CONTINUE
        ENDIF
        IF ( IMOMF .EQ. 1 )  THEN
          DO  292  I = 1, 4
            DFDX(I,4,K) = DFDX(I,4,K) +
     +                    DFDX(I,5,K)*SST*DTAN(XS4)/DCOS(XS4)/
     +                    (ROVP*PFIX)
  292     CONTINUE
          DO  293  I = 1, 5
            DFDX(I,5,K) = 0.
            DFDX(5,I,K) = 0.
  293     CONTINUE
        ENDIF
C
C--->   APPLY LINEARIZED ERROR PROPAGATION
C
C
C--->   PREDICTED COVARIANCE MATRIX
C
        DO 301 I = 1, 5
          DO 302 J = 1, 5
            SUM=0.D0
            DO 303 M = 1, 5
              SUM=SUM+CFIL(I,M,K)*DFDX(J,M,K)
 303        CONTINUE
            CTEMP(I,J)=SUM
 302      CONTINUE
 301    CONTINUE

        DO 310 I= 1, 5
          DO 311 J = 1, I
            SUM=QPROC(I,J,K)
            DO 312 L = 1, 5
              SUM=SUM+DFDX(I,L,K)*CTEMP(L,J)
 312        CONTINUE
            CPRE(I,J,K+1)=SUM
            CPRE(J,I,K+1)=CPRE(I,J,K+1)
 311      CONTINUE
 310    CONTINUE
C
C--->   GET RESIDUALS
C
        RESP(1,K+1) = XME(1,K+1) - XSTP(1,K+1)
        RESP(2,K+1) = XME(2,K+1) - XSTP(2,K+1)
        ALP         = RESP(1,K+1) / RF(K+1)
        IF  ( ALP .GT. PI )  ALP = ALP - PIT2
        IF  ( ALP .LT.-PI )  ALP = ALP + PIT2
        RESP(1,K+1) = ALP * RF(K+1)
  500 CONTINUE
C
      DELTA = XSTF(5,N1) - XSTS(5,N1)
      DELTA = DELTA*DELTA
      DELTAP = XSTF(3,N1) - XSTS(3,N1)
      IF ( DELTAP .GT. PI ) DELTAP = DELTAP - PIT2
      IF ( DELTAP .LT.-PI ) DELTAP = DELTAP + PIT2
      DELTAP = DELTAP**2
      DELTAU = ( XSTF(1,N1) - XSTS(1,N1) ) / RF(N1)
      IF ( DELTAU .GT. PI ) DELTAU = DELTAU - PIT2
      IF ( DELTAU .LT.-PI ) DELTAU = DELTAU + PIT2
      DELTAU = ( DELTAU * RF(N1) )**2
      IFLG  = 0
      IF ( CHI2F(N1) .LE. 0. )                             GOTO 999
C
C---> CHECK SPECIAL REQUIREMENTS FOR VDET PATTERN RECOGNITION
C
      CHI2VD = CHI2F(N1)
      NVDOUT = 0
      IF ( ISBEVD .NE. 0 )   THEN
        DO 501  I = N0+1, N1
          IF ( VMEAS(1,1,I) .LT. UVARMA )  NVDOUT = NVDOUT+1
          IF ( VMEAS(2,2,I) .LT. ZVARMA )  NVDOUT = NVDOUT+1
  501   CONTINUE
        IF ( ISAFVD .NE. 0 )   THEN
          ISAFVD = 0
          ISBEVD = 0
          RETURN
        ENDIF
      ELSEIF ( ISAFVD .NE. 0 )   THEN
        J = N - NVDIN + 1
        IF ( J .LT. N0 )   J = N0
        DO 502  I = J, N1
          IF ( VMEAS(1,1,I) .LT. UVARMA )  NVDOUT = NVDOUT+1
          IF ( VMEAS(2,2,I) .LT. ZVARMA )  NVDOUT = NVDOUT+1
  502   CONTINUE
        ISAFVD = 0
        ISBEVD = 0
        RETURN
      ENDIF
C
C---> VDET PATTERN RECOGNITION DONE, SWITCH BACK TO DEFAULTS
C
      ISAFVD = 0
      ISBEVD = 0
      N0 = N00
C
C---> CHECK IF FIT CONFIGURATION CHANGED
C
      DO  509  I = N0, N1
        IF ( IUSED(I) .NE. IUSEDL(I) )      IBADCL = -1
  509 CONTINUE
C
C--->  CHECK MAXIMAL NUMBER OF ITERATIONS
C
      IF ( NCOUNT .GE. ITMAX )                             GOTO 510
C
C--->  IF COORDINATE FILTERING IS REQUIRED, SMOOTHING IN THE
C      FIRST STEP IS UNAVOIDABLE
C
      IF ( NCOUNT .EQ. 1 .AND. PRB .GT. 0. )               GOTO 2000
C
C--->  IF COORDINATES GOT FILTERED, SMOOTHING HAS TO BE
C      REPEATED TO RECOVER SOME OF THEM, IF POSSIBLE
      IF ( IBADC .NE. IBADCL )                             GOTO 2000
C
C--->  CHECK CONVERGENCE
C
      IF ( NCOUNT .LT. ITMAX .AND.
     1   ( ABS(CHI2F(N1)-CHI2L).GT.0.001*CHI2L    .OR.
     2     DELTA.GT.0.01*CFIL(5,5,N1)             .OR.
     3     (XSTF(4,N1)-XSTS(4,N1))**2.GT.0.01*CFIL(4,4,N1)  .OR.
     4     DELTAP.GT.0.01*CFIL(3,3,N1)            .OR.
     5     (XSTF(2,N1)-XSTS(2,N1))**2.GT.0.01*CFIL(2,2,N1)  .OR.
     6     DELTAU.GT.0.01*CFIL(1,1,N1)  )  )
     7                                                     GOTO 2000
  510 CONTINUE
C
C----------------------------------------------------------------
C---    FILTER FINISHED
C---    ===>   HELIX PARAMETERS AT INNERMOST COORDINATE
C----------------------------------------------------------------
C
      IF ( NCOUNT .GE. ITMAX .AND.
     1   ( ABS(CHI2F(N1)-CHI2L).GT.0.001*CHI2L             .OR.
     2     DELTA.GT.0.01*CFIL(5,5,N1)                      .OR.
     3     (XSTF(4,N1)-XSTS(4,N1))**2.GT.0.01*CFIL(4,4,N1) .OR.
     4     DELTAP.GT.0.01*CFIL(3,3,N1)                     .OR.
     5     (XSTF(2,N1)-XSTS(2,N1))**2.GT.0.01*CFIL(2,2,N1) .OR.
     6     DELTAU.GT.0.01*CFIL(1,1,N1)                     .OR.
     7     IBADC.NE.IBADCL )   )                           GOTO 999
      NLOW  = N0
      NHIGH = N1
      DO  550  II = 1, N0-1
        IUSED(II) = 0
  550 CONTINUE
      DO  551  II = N1+1, MPT
        IUSED(II) = 0
  551 CONTINUE
      DO  552  II = N0, N1
        IF ( IUSED(II) .GT. 0 )  IUSED(II) = 1
  552 CONTINUE
      ALP  = XSTF(3,N1) - XSTF(1,N1)/RF(N1)
      DEN  = RF(N1) * DCOS(ALP)
      FACT = (XSTF(5,N1))**(-1) - RF(N1)*DSIN(ALP)
      IF ( ABS(FACT) .LT. EPS )    THEN
        IF ( ABS(DEN) .LT. EPS )        GOTO 999
        TPROC = DATAN( FACT / DEN ) - PIO2
                                   ELSE
        TPROC = - DATAN( DEN / FACT )
      ENDIF
      IF ( TPROC .GT. 0. .AND. XSTF(5,N1) .GT. 0. ) TPROC = TPROC - PI
      IF ( TPROC .LT. 0. .AND. XSTF(5,N1) .LT. 0. ) TPROC = TPROC + PI
      ALP    = XSTF(3,N1) + TPROC
      IF ( ALP .LT. 0.   ) ALP = ALP + PIT2
      IF ( ALP .GT. PIT2 ) ALP = ALP - PIT2
      DD0  = RF(N1)*DSIN(ALP-XSTF(1,N1)/RF(N1)) +
     +       2./XSTF(5,N1)*(DSIN(0.5*TPROC))**2
      IF ( ABS(DD0) .GT. RF(N1) ) THEN
        IF ( TPROC .GT. 0. ) THEN
          TPROC = TPROC - PI
                             ELSE
          TPROC = TPROC + PI
        ENDIF
        ALP    = XSTF(3,N1) + TPROC
        IF ( ALP .LT. 0.   ) ALP = ALP + PIT2
        IF ( ALP .GT. PIT2 ) ALP = ALP - PIT2
        DD0  = RF(N1)*DSIN(ALP-XSTF(1,N1)/RF(N1)) +
     +         2./XSTF(5,N1)*(DSIN(0.5*TPROC))**2
      ENDIF
      VV0(1) = XSTF(5,N1)
      VV0(2) = DTAN( XSTF(4,N1) )
      VV0(3) = ALP
      VV0(4) = DD0
      VV0(5) = XSTF(2,N1) + TPROC/XSTF(5,N1)*DTAN(XSTF(4,N1))
      CHI2   = CHI2F(N1)
C
C--->     COVARIANCE MATRIX FOR OUTPUT PARAMETERS
C
      ALP = XSTF(3,N1) - XSTF(1,N1)/RF(N1) + TPROC
      CS  = DCOS( ALP )
      SS  = DSIN( ALP )
      DEN = ( XSTF(5,N1)*DD0 - 1. )**(-1)
      DTDRFI =   XSTF(5,N1) * SS * DEN
      DTDPHI = - RF(N1) * DTDRFI
      DTDOME =   RF(N1) * CS * DEN
      DOMDRH = 1.
      DLADTH = ( DCOS(XSTF(4,N1)) )**(-2)
      DP0DRH =   DTDOME
      DP0DPH = 1. + DTDPHI
      DP0DRF =   DTDRFI
      DD0DRF =   DEN * DCOS(XSTF(3,N1)-XSTF(1,N1)/RF(N1))
      DD0DPH = -RF(N1) * DD0DRF
      DD0DRH =  RF(N1)/XSTF(5,N1)*DEN * ( XSTF(5,N1)*RF(N1) -
     +          DSIN(XSTF(3,N1)-XSTF(1,N1)/RF(N1)) ) -
     +          DD0 / XSTF(5,N1)
      FACT   = DTAN(XSTF(4,N1)) / XSTF(5,N1)
      DZ0DRF = FACT * DTDRFI
      DZ0DZ  = 1.
      DZ0DPH = FACT * DTDPHI
      DZ0DTH = TPROC * DLADTH / XSTF(5,N1)
      DZ0DRH = FACT * ( DTDOME - TPROC/XSTF(5,N1) )
C
C--->  CHECK WHETHER USER REQUESTED CONSTRAINT
C
      IF ( IOMEF .EQ. 1 )  THEN
        DTDOME = 0.
        DOMDRH = 0.
        DP0DRH = 0.
        DD0DRH = 0.
        DZ0DRH = 0.
      ENDIF
      IF ( ITALF .EQ. 1 )  THEN
        DLADTH = 0.
        DZ0DTH = 0.
      ENDIF
      IF ( IMOMF .EQ. 1 ) THEN
        FACT = SST*DTAN(XSTF(4,N1))/DCOS(XSTF(4,N1)) /
     +             (ROVP*PFIX)
        DP0DTH = DP0DRH*FACT
        DD0DTH = DD0DRH*FACT
        DZ0DTH = DZ0DTH + DZ0DRH*FACT
        DZ0DRH = 0.
        DD0DRH = 0.
        DP0DRH = 0.
        DOMDRH = 0.
        DTDOME = 0.
      ENDIF
      CC0(1) = CFIL(5,5,N1) * DOMDRH * DOMDRH
      CC0(2) = CFIL(4,5,N1) * DLADTH * DOMDRH
      CC0(3) = CFIL(4,4,N1) * DLADTH * DLADTH
      CC0(4) = DOMDRH * (
     +            CFIL(1,5,N1) * DP0DRF + CFIL(3,5,N1) * DP0DPH
     +          + CFIL(5,5,N1) * DP0DRH )
      CC0(5) = DLADTH * (
     +            CFIL(1,4,N1) * DP0DRF + CFIL(3,4,N1) * DP0DPH
     +          + CFIL(4,5,N1) * DP0DRH )
      CC0(6) = DP0DRH*DP0DRH*CFIL(5,5,N1) + DP0DPH*DP0DPH*CFIL(3,3,N1) +
     +         DP0DRF*DP0DRF*CFIL(1,1,N1)
     +         + 2. * ( DP0DPH*DP0DRH*CFIL(3,5,N1) +
     +         DP0DRF*DP0DRH*CFIL(1,5,N1) + DP0DPH*DP0DRF*CFIL(1,3,N1) )
      CC0(7) = DOMDRH * (
     +            CFIL(1,5,N1) * DD0DRF + CFIL(3,5,N1) * DD0DPH
     +          + CFIL(5,5,N1) * DD0DRH )
      CC0(8) = DLADTH * (
     +            CFIL(1,4,N1) * DD0DRF + CFIL(3,4,N1) * DD0DPH
     +          + CFIL(4,5,N1) * DD0DRH )
      CC0(9) = DP0DRH*( DD0DRH*CFIL(5,5,N1) + DD0DPH*CFIL(3,5,N1) +
     +                  DD0DRF*CFIL(1,5,N1) ) +
     +         DP0DPH*( DD0DRH*CFIL(3,5,N1) + DD0DPH*CFIL(3,3,N1) +
     +                  DD0DRF*CFIL(1,3,N1) ) +
     +         DP0DRF*( DD0DRH*CFIL(1,5,N1) + DD0DPH*CFIL(1,3,N1) +
     +                  DD0DRF*CFIL(1,1,N1) )
      CC0(10)= DD0DRH*DD0DRH*CFIL(5,5,N1) + DD0DPH*DD0DPH*CFIL(3,3,N1) +
     +         DD0DRF*DD0DRF*CFIL(1,1,N1)
     +         + 2. * ( DD0DPH*DD0DRH*CFIL(3,5,N1) +
     +         DD0DRF*DD0DRH*CFIL(1,5,N1) + DD0DPH*DD0DRF*CFIL(1,3,N1) )
      CC0(11)= DOMDRH * (
     +            CFIL(1,5,N1) * DZ0DRF + CFIL(2,5,N1) * DZ0DZ
     +          + CFIL(3,5,N1) * DZ0DPH + CFIL(4,5,N1) * DZ0DTH
     +          + CFIL(5,5,N1) * DZ0DRH )
      CC0(12)= DLADTH * (
     +            CFIL(1,4,N1) * DZ0DRF + CFIL(2,4,N1) * DZ0DZ
     +          + CFIL(3,4,N1) * DZ0DPH + CFIL(4,4,N1) * DZ0DTH
     +          + CFIL(4,5,N1) * DZ0DRH )
      CC0(13)= DP0DRH*( DZ0DRH*CFIL(5,5,N1) + DZ0DTH*CFIL(4,5,N1) +
     +                  DZ0DPH*CFIL(3,5,N1) + DZ0DZ*CFIL(2,5,N1)  +
     +                  DZ0DRF*CFIL(1,5,N1) ) +
     +         DP0DPH*( DZ0DRH*CFIL(3,5,N1) + DZ0DTH*CFIL(3,4,N1) +
     +                  DZ0DPH*CFIL(3,3,N1) + DZ0DZ*CFIL(2,3,N1)  +
     +                  DZ0DRF*CFIL(1,3,N1) ) +
     +         DP0DRF*( DZ0DRH*CFIL(1,5,N1) + DZ0DTH*CFIL(1,4,N1) +
     +                  DZ0DPH*CFIL(1,3,N1) + DZ0DZ*CFIL(1,2,N1)  +
     +                  DZ0DRF*CFIL(1,1,N1) )
      CC0(14)= DD0DRH*( DZ0DRH*CFIL(5,5,N1) + DZ0DTH*CFIL(4,5,N1) +
     +                  DZ0DPH*CFIL(3,5,N1) + DZ0DZ*CFIL(2,5,N1)  +
     +                  DZ0DRF*CFIL(1,5,N1) ) +
     +         DD0DPH*( DZ0DRH*CFIL(3,5,N1) + DZ0DTH*CFIL(3,4,N1) +
     +                  DZ0DPH*CFIL(3,3,N1) + DZ0DZ*CFIL(2,3,N1)  +
     +                  DZ0DRF*CFIL(1,3,N1) ) +
     +         DD0DRF*( DZ0DRH*CFIL(1,5,N1) + DZ0DTH*CFIL(1,4,N1) +
     +                  DZ0DPH*CFIL(1,3,N1) + DZ0DZ*CFIL(1,2,N1)  +
     +                  DZ0DRF*CFIL(1,1,N1) )
      CC0(15)= DZ0DRH*DZ0DRH*CFIL(5,5,N1) + DZ0DTH*DZ0DTH*CFIL(4,4,N1)
     +       + DZ0DPH*DZ0DPH*CFIL(3,3,N1) + DZ0DZ*DZ0DZ*CFIL(2,2,N1)
     +       + DZ0DRF*DZ0DRF*CFIL(1,1,N1) + 2.*(
     +         DZ0DRH*( DZ0DTH*CFIL(4,5,N1) + DZ0DPH*CFIL(3,5,N1) +
     +                  DZ0DZ*CFIL(2,5,N1)  + DZ0DRF*CFIL(1,5,N1) )
     +       + DZ0DTH*( DZ0DPH*CFIL(3,4,N1) + DZ0DZ*CFIL(2,4,N1)  +
     +                  DZ0DRF*CFIL(1,4,N1) )
     +       + DZ0DPH*( DZ0DZ*CFIL(2,3,N1)  + DZ0DRF*CFIL(1,3,N1) )
     +       + DZ0DZ*DZ0DRF*CFIL(1,2,N1) )
C
C--->   MODIFY FOR FIXED MOMENTUM CONSTRAINT
C
      IF ( IMOMF .EQ. 1 )  THEN
        CC0(5) = CC0(5) + DLADTH*DP0DTH*CFIL(4,4,N1)
        CC0(6) = CC0(6) + DP0DTH*DP0DTH*CFIL(4,4,N1) + 2.*DP0DTH*(
     +         DP0DRH*CFIL(4,5,N1) + DP0DPH*CFIL(3,4,N1) +
     +         DP0DRF*CFIL(1,4,N1) )
        CC0(8) = CC0(8) + DLADTH*DD0DTH*CFIL(4,4,N1)
        CC0(9) = CC0(9) + DP0DTH*DD0DTH*CFIL(4,4,N1)
     +         + (DP0DTH*DD0DRH+DP0DRH*DD0DTH)*CFIL(4,5,N1)
     +         + (DP0DTH*DD0DPH+DP0DPH*DD0DTH)*CFIL(3,4,N1)
     +         + (DP0DTH*DD0DRF+DP0DRF*DD0DTH)*CFIL(1,4,N1)
        CC0(10)= CC0(10) + DD0DTH*DD0DTH*CFIL(4,4,N1) + 2.*DD0DTH*(
     +         DD0DRH*CFIL(4,5,N1) + DD0DPH*CFIL(3,4,N1) +
     +         DD0DRF*CFIL(1,4,N1) )
        CC0(13)= CC0(13) +
     +         DP0DTH*( DZ0DRH*CFIL(4,5,N1) + DZ0DTH*CFIL(4,4,N1) +
     +                  DZ0DPH*CFIL(3,4,N1) + DZ0DZ*CFIL(2,4,N1)  +
     +                  DZ0DRF*CFIL(1,4,N1) )
        CC0(14)= CC0(14) +
     +         DD0DTH*( DZ0DRH*CFIL(4,5,N1) + DZ0DTH*CFIL(4,4,N1) +
     +                  DZ0DPH*CFIL(3,4,N1) + DZ0DZ*CFIL(2,4,N1)  +
     +                  DZ0DRF*CFIL(1,4,N1) )
        CC0(1) = CC0(3) * FACT**2
        CC0(2) = CC0(3) * FACT
        CC0(4) = CC0(5) * FACT
        CC0(7) = CC0(8) * FACT
        CC0(11)= CC0(12) * FACT
      ENDIF
      DO  400  I = 16, 20
        CC0(I) = 0.
  400 CONTINUE
      CC0(21) = 1.
C
C--->  ADD MULTIPLE SCATTERING TO ORIGIN IF SWIMMING IS REQUESTED
C
      IF ( ISWIM .NE. 0 ) THEN
        CC0(1)  = CC0(1)  + QPROC(1,1,N1)
        CC0(2)  = CC0(2)  + QPROC(1,2,N1)
        CC0(3)  = CC0(3)  + QPROC(2,2,N1)
        CC0(4)  = CC0(4)  + QPROC(1,3,N1)
        CC0(5)  = CC0(5)  + QPROC(2,3,N1)
        CC0(6)  = CC0(6)  + QPROC(3,3,N1)
        CC0(9)  = CC0(9)  + QPROC(3,4,N1)
        CC0(10) = CC0(10) + QPROC(4,4,N1)
        CC0(11) = CC0(11) + QPROC(1,5,N1)
        CC0(12) = CC0(12) + QPROC(2,5,N1)
        CC0(13) = CC0(13) + QPROC(3,5,N1)
        CC0(14) = CC0(14) + QPROC(4,5,N1)
        CC0(15) = CC0(15) + QPROC(5,5,N1)
      ENDIF
C
C--->  GET NDF
C
      NOUT = NOUT - IBADC
      NDEG = 2 * NOUT - 5
C---> SUBTRACT NON-MEASURED COORDINATES
      NDEG = NDEG - ITCNOZ
      NDEG = NDEG - IVDNOU
C---> IF NO Z-COORDINATE IS MEASURED, CONSIDER THIS AS CIRCLE FIT
      IF ( ITCNOZ .EQ. NOUT )    NDEG = NDEG + 2
C
      IF ( NDEG .LT. 1 )         GOTO 999
C
C--->  CHECK WHETHER USER REQUESTED CONSTRAINTS
C
      IF ( IOMEF .EQ. 1 ) NDEG = NDEG + 1
      IF ( ITALF .EQ. 1 ) NDEG = NDEG + 1
      IF ( IMOMF .EQ. 1 ) NDEG = NDEG + 1
C
      IFLG = 1
C
C----------------------------------------------------------------
C---    STEP 3 :  SMOOTHING
C
C---   IF   IFLG = 0   :  GENERAL SMOOTHING FOR NEXT ITERATION
C---   IF   IFLG = 1   :  SMOOTHING TO FIRST TPC COORDINATE
C---                      TO COMPUTE MULTIPLE SCATTERING ANGLE
C---
C----------------------------------------------------------------
C
 2000 CONTINUE
      NOUT  = N1 - N0 + 1
      CHI2L = CHI2F(N1)
      IBADCL = IBADC
      DO  600  I = N0, N1
        IUSEDL(I) = IUSED(I)
  600 CONTINUE
      DO  601  I = 1, 5
        XSTS(I,N1) = XSTF(I,N1)
  601 CONTINUE
      IF ( PRB .GT. 0. .AND. (NCOUNT .EQ. 1 .OR. IBADC .GT. 0) )  THEN
        DO  603  I = 1, 5
          DO  602  J = 1, 5
            CSMO(I,J,N1) = CFIL(I,J,N1)
  602     CONTINUE
  603   CONTINUE
        RESS(1,N1) = RESF(1,N1)
        RESS(2,N1) = RESF(2,N1)
        RESMS(1,1,N1) = RESMF(1,1,N1)
        RESMS(1,2,N1) = RESMF(1,2,N1)
        RESMS(2,1,N1) = RESMF(2,1,N1)
        RESMS(2,2,N1) = RESMF(2,2,N1)
      ENDIF
      ALP = (XSTS(1,N1)-XSTP(1,N1))/RF(N1)
      IF ( ALP .GT. PI )  XSTS(1,N1) = XSTS(1,N1) - PIT2*RF(N1)
      IF ( ALP .LT.-PI )  XSTS(1,N1) = XSTS(1,N1) + PIT2*RF(N1)
      ALP = XSTS(3,N1) - XSTP(3,N1)
      IF ( ALP .LT.-PI )  XSTS(3,N1) = XSTS(3,N1) + PIT2
      IF ( ALP .GT. PI )  XSTS(3,N1) = XSTS(3,N1) - PIT2
      ALP = XSTS(4,N1)
      IF ( ALP .LT.-PIO2 )  ALP = ALP + PI
      IF ( ALP .GT. PIO2 )  ALP = ALP - PI
      XSTS(4,N1) = ALP
C
      IF ( IFLG .EQ. 0  ) THEN
        NL = N0
        NNL = N0
                         ELSE
        DO  604  I = N0 , N1
          IF ( RF(I) .LT. UKRTPC  )   GOTO 605
  604   CONTINUE
        I = 0
  605   CONTINUE
        NL = I - 1
        IF ( IDALF .GT. 0 ) THEN
          NNL = N0
                            ELSE
          NNL = NL
        ENDIF
        IF ( NNL .LT. N0 ) GOTO 4000
      ENDIF
      DO  690  K = N1, NNL, -1
        IF ( K .EQ. N1 )   GOTO 688
C
C--->   SMOOTHER GAIN MATRIX
C
        DO  606  I = 1, 5
          IF ( CPRE(I,I,K+1) .LT. 0. )       GOTO 999
          WSCAL(I) = DSQRT( CPRE(I,I,K+1) )
  606   CONTINUE
C
C--->  CHECK WHETHER USER REQUESTED CONSTRAINT
C
        IF ( IOMEF .EQ. 1 )  WSCAL(5) = 1.
        IF ( ITALF .EQ. 1 )  WSCAL(4) = 1.
        IF ( IMOMF .EQ. 1 )  WSCAL(5) = 1.
        DO  610  I = 1, 5
          DO  611  J = 1, I
            CWORK(I,J) = (CPRE(I,J,K+1)/(WSCAL(I)*WSCAL(J)))
            CWORK(J,I) = CWORK(I,J)
  611     CONTINUE
  610   CONTINUE
C
C--->  CHECK WHETHER USER REQUESTED CONSTRAINT
C
        IF ( IOMEF .EQ. 1 ) CWORK(5,5) = 1.
        IF ( ITALF .EQ. 1 ) CWORK(4,4) = 1.
        IF ( IMOMF .EQ. 1 ) THEN
          CWORK(5,5) = 1.
          CWORK(4,5) = 0.
          CWORK(5,4) = 0.
          CWORK(3,5) = 0.
          CWORK(5,3) = 0.
          CWORK(2,5) = 0.
          CWORK(5,2) = 0.
          CWORK(1,5) = 0.
          CWORK(5,1) = 0.
        ENDIF
        CALL DSINV (5,CWORK,5,IFAIL)
C
C--->  Give up on this track if the matrix inversion fails because the
C      matrix is not positive definite
C
        IF (IFAIL.NE.0) GOTO 999
        MATDIM = 5
        IF ( IMOMF .EQ. 1 )  MATDIM = 4
        DO 626 I = 1, MATDIM
          DO 627 J = 1, MATDIM
            CWORK2(I,J)=CWORK(I,J) / (WSCAL(I)*WSCAL(J))
 627      CONTINUE
 626    CONTINUE

        DO 620 I = 1, MATDIM
          DO 621 J = 1, MATDIM
            SUM=0.D0
            DO 622 M = 1, MATDIM
              SUM=SUM+CFIL(I,M,K)*DFDX(J,M,K)
 622        CONTINUE
            CTEMP(I,J)=SUM
 621      CONTINUE
 620    CONTINUE

        DO 623 I = 1, MATDIM
          DO 624 J = 1, MATDIM
            SUM=0.D0
            DO 625 L = 1, MATDIM
              SUM=SUM+CTEMP(I,L)*CWORK2(L,J)
 625        CONTINUE
            SGAIN(I,J)=SUM
 624      CONTINUE
 623    CONTINUE
C
C--->   SMOOTHED STATE VECTOR
C
        DO  640  I = 1, MATDIM
          SUM = XSTF(I,K)
          DO  641  J = 1, MATDIM
            SUM = SUM + SGAIN(I,J)*(XSTS(J,K+1)-XSTP(J,K+1))
  641     CONTINUE
          XSTS(I,K) = SUM
  640   CONTINUE
        FACT = XSTS(1,K) / RF(K)
        IF ( FACT .GT. PIT2 )  FACT = FACT - PIT2
        IF ( FACT .LT. 0.   )  FACT = FACT + PIT2
        ALP = FACT-XSTP(1,K)/RF(K)
        IF ( ALP .GT. PI )  FACT = FACT - PIT2
        IF ( ALP .LT.-PI )  FACT = FACT + PIT2
        XSTS(1,K) = FACT * RF(K)
        ALP = XSTS(3,K)
        IF ( ALP .GT. PIT2 )  ALP = ALP - PIT2
        IF ( ALP .LT.-PIT2 )  ALP = ALP + PIT2
        XSTS(3,K) = ALP
        ALP = ALP - XSTP(3,K)
        IF ( ALP .GT. PI )  XSTS(3,K) = XSTS(3,K) - PIT2
        IF ( ALP .LT.-PI )  XSTS(3,K) = XSTS(3,K) + PIT2
        ALP = XSTS(4,K)
        IF ( ALP .GT. PIO2 )  ALP = ALP - PI
        IF ( ALP .LT.-PIO2 )  ALP = ALP + PI
        XSTS(4,K) = ALP
C
C--->    CHECK WHETHER USER REQUESTED FIXED MOMENTUM
C
        IF ( IMOMF .EQ. 1 )  THEN
          XSTS(5,K) = SST / (ROVP*PFIX*DCOS(XSTS(4,K)))
        ENDIF
C
C--->    IF COORDINATE FILTERING ALREADY DONE OR NOT REQUIRED ...
C--->              GET OUT OF HERE
C
  688   CONTINUE
        IF ( PRB .EQ. 0. .OR. (NCOUNT .GT. 1 .AND. IBADC .LE. 0) )
     +                                           GOTO 690
        IF ( K .EQ. N1 )                         GOTO 689
C
C--->    SMOOTHED COVARIANCE MATRIX
C
        DO  684  I = 1, MATDIM
          DO  685  J = 1, MATDIM
            SUM = 0.
            DO  686  L = 1, MATDIM
              DO  687  M = 1, MATDIM
                SCAL = SGAIN(I,L)*SGAIN(J,M)
                SUM1 = CSMO(L,M,K+1) - CPRE(L,M,K+1)
                SUM1 = SCAL * SUM1
                SUM  = SUM + SUM1
  687         CONTINUE
  686       CONTINUE
            CSMO(I,J,K) = SUM + CFIL(I,J,K)
            CSMO(J,I,K) = CSMO(I,J,K)
  685     CONTINUE
  684   CONTINUE
C
C--->    CHECK IF USER REQUESTED FIXED MOMENTUM
C
        IF ( IMOMF .EQ. 1 )  THEN
          FACT = XSTS(5,K) * DTAN(XSTS(4,K))
          CSMO(5,5,K) = FACT * FACT * CSMO(4,4,K)
          CSMO(1,5,K) = FACT * CSMO(1,4,K)
          CSMO(5,1,K) = CSMO(1,5,K)
          CSMO(2,5,K) = FACT * CSMO(1,4,K)
          CSMO(5,2,K) = CSMO(1,5,K)
          CSMO(3,5,K) = FACT * CSMO(1,4,K)
          CSMO(5,3,K) = CSMO(1,5,K)
          CSMO(4,5,K) = FACT * CSMO(1,4,K)
          CSMO(5,4,K) = CSMO(1,5,K)
        ENDIF
C
C--->    SMOOTHED RESIDUALS
C
        ALP       = ( RESF(1,K) - (XSTS(1,K)-XSTF(1,K)) ) / RF(K)
        IF ( ALP .GT. PI )   ALP = ALP - PIT2
        IF ( ALP .LT.-PI )   ALP = ALP + PIT2
        RESS(1,K) = ALP * RF(K)
        RESS(2,K) = RESF(2,K) - (XSTS(2,K)-XSTF(2,K))
C
C--->    COVARIANCE MATRIX OF SMOOTHED RESIDUALS
C
C--->     FOR A COORDINATE WHICH WAS USED IN THE FIT
C
        IF ( IUSED(K) .GT. 0 )  THEN
          RESMS(1,1,K) = VMEAS(1,1,K) - CSMO(1,1,K)
          RESMS(1,2,K) = VMEAS(1,2,K) - CSMO(1,2,K)
          RESMS(2,1,K) = RESMS(1,2,K)
          RESMS(2,2,K) = VMEAS(2,2,K) - CSMO(2,2,K)
                                ELSE
C
C--->   ...AND A COORDINATE WHICH WAS NOT USED IN THE FIT
C
          RESMS(1,1,K) = VMEAS(1,1,K) + CSMO(1,1,K)
          RESMS(1,2,K) = VMEAS(1,2,K) + CSMO(1,2,K)
          RESMS(2,1,K) = RESMS(1,2,K)
          RESMS(2,2,K) = VMEAS(2,2,K) + CSMO(2,2,K)
        ENDIF
  689   CONTINUE
        DET = RESMS(1,1,K)*RESMS(2,2,K) - RESMS(1,2,K)**2
C
C--->   IN CASE OF NUMERICAL INSTABILITY USE THE SAFE ASSUMPTION
C--->   THAT THE FIT HAS SMALL ERRORS COMPARED TO THE COORDINATE
C
        IF ( DET .LE. 0. )  THEN
          RESMS(1,1,K) = VMEAS(1,1,K)
          RESMS(2,2,K) = VMEAS(2,2,K)
          RESMS(1,2,K) = VMEAS(1,2,K)
          RESMS(2,1,K) = VMEAS(2,1,K)
          DET = RESMS(1,1,K)*RESMS(2,2,K) - RESMS(1,2,K)**2
          IF ( DET .LE. 0. )           GOTO 999
        ENDIF
        SUM  =  RESMS(2,2,K)*RESS(1,K)**2 + RESMS(1,1,K)*RESS(2,K)**2
     +          - 2.*RESMS(1,2,K)*RESS(1,K)*RESS(2,K)
        CH2S = SUM / DET
C
C--->   CHECK IF COORDINATE HAS ONE OR TWO DEGREES OF FREEDOM
C
        CH2CC = CH2CUT
        IF ( VMEAS(2,2,K) .GT. ZVARMA )  CH2CC = CH2CU1
        IF ( VMEAS(1,1,K) .GT. UVARMA )  CH2CC = CH2CU1
C
C--->   RECOVER COORDINATES
C
        IF (IUSED(K).EQ.0 .AND. CH2S.LE.CH2CC .AND. IFLG .EQ. 0 ) THEN
          IUSED(K) = 2
        ENDIF
C
C--->   TEST ON BAD COORDINATES HERE, KEEP RECOVERED COORDINATES
C
        IF ( CH2S .LE. CH2CC .OR. IUSED(K) .EQ. 2 )   GOTO 690
        IF ( IFLG .NE. 0 )                            GOTO 690
        IUSED(K) = 0
  690 CONTINUE
C
C----------------------------------------------------------------
C---    SMOOTHING DONE .... READY TO REITERATE FILTER
C----------------------------------------------------------------
C
      IF  ( IFLG .EQ. 0 )     GOTO   1000
C
C----------------------------------------------------------------
C---    SMOOTHING TO TPC DONE  ... GET MULT.SCAT. ANGLE
C----------------------------------------------------------------
C
 4000 CONTINUE
      IF  ( NL .LT. N0 )    THEN
        VV0(6)  = 0.
        CC0(21) = 1.
                            ELSE
        PHIIN  =  XSTS(3,NL+1)
        PHIOUT =  XSTS(3,NL)
        VV0(6)  = 0.
        CC0(21) = 1.
        DELR   =  UKRTPC - RF(NL)
        R2     =  DELR * ( 2.*RF(NL) + DELR )
        S2     =  2.*RF(NL)/XSTF(5,NL)
        T2     =  2./XSTF(5,NL)**2
        ALP    =  XSTF(3,NL) - XSTF(1,NL)/RF(NL)
        FACT   =  S2*DCOS(ALP)
        DEN    =  R2-2.*T2+2.*S2*DSIN(ALP)
        IF  ( ABS(FACT) .LT. EPS )        RETURN
        TPROC  =  R2*DEN/FACT**2
        IF  ( ABS(TPROC) .LT. 1.E-7 )   THEN
          TPROC = 0.5*R2/FACT
                                        ELSE
          TPROC = 1. - TPROC
          IF ( TPROC .LT. 0. )            RETURN
          TPROC = FACT/DEN * ( 1. - DSQRT(TPROC) )
        ENDIF
        TPROC = 2.*DATAN(TPROC)
        PHIOUT = PHIOUT + TPROC
        DELR   =  UKRTPC - RF(NL+1)
        R2     =  DELR * ( 2.*RF(NL+1) + DELR )
        S2     =  2.*RF(NL+1)/XSTF(5,NL+1)
        T2     =  2./XSTF(5,NL+1)**2
        ALP    =  XSTF(3,NL+1) - XSTF(1,NL+1)/RF(NL+1)
        FACT   =  S2*DCOS(ALP)
        DEN    =  R2-2.*T2+2.*S2*DSIN(ALP)
        IF  ( ABS(FACT) .LT. EPS )        RETURN
        TPROC  =  R2*DEN/FACT**2
        IF  ( ABS(TPROC) .LT. 1.E-7 )   THEN
          TPROC = 0.5*R2/FACT
                                        ELSE
          TPROC = 1. - TPROC
          IF ( TPROC .LT. 0. )            RETURN
          TPROC = FACT/DEN * ( 1. - DSQRT(TPROC) )
        ENDIF
        TPROC = 2.*DATAN(TPROC)
        PHIIN = PHIIN + TPROC
        ALPHA = PHIIN - PHIOUT
        IF ( ALPHA .GT. PI ) ALPHA = ALPHA - PIT2
        IF ( ALPHA .LT.-PI ) ALPHA = ALPHA + PIT2
        VV0(6)  = ALPHA
        CC0(21) = VV0(6)**2
      ENDIF
      RETURN
  999 CONTINUE
C------> JUMP HERE IF SOMETHING GOES CRAZY
      NVDOUT = 0
      CHI2VD = 1.0E30
      CHI2 = 1.0E30
      ISAFVD = 0
      ISBEVD = 0
      RETURN
C
C-------------------------------------------------------------
C
C--->    STEERING FOR CONSTRAINTS
C
C-------------------------------------------------------------
C
      ENTRY UFTOME(IFLAG)
        IOMEF = 1
        IF ( IFLAG .EQ. 0 )  IOMEF = 0
      RETURN
      ENTRY UFTTAL(IFLAG)
        ITALF = 1
        IF ( IFLAG .EQ. 0 )  ITALF = 0
      RETURN
      ENTRY UFTMOM(IFLAG)
        IMOMF = 1
        IF ( IFLAG .EQ. 0 )  IMOMF = 0
        IF ( IMOMF .EQ. 1 ) THEN
          IOMEF = 0
          ITALF = 0
        ENDIF
      RETURN
C
C-------------------------------------------------------------
C
C--->    STEERING FOR COORDINATE FILTERING
C
C-------------------------------------------------------------
C
      ENTRY UFTFIL(PR)
        PRB = PR
        IF ( PRB .LT. 0. )  PRB = 0.
        IF ( PRB .GT. 1. )  PRB = 1.
        CH2CUT = 1.E30
        CH2CU1 = 1.E30
        IF (PRB.NE.0.)   THEN
          CH2CUT = -2.*ALOG(PRB)
          CH2CU1 = CHISIN(1.-PRB,1)
        ENDIF
      RETURN
C
C-------------------------------------------------------------
C
C--->    SWITCHING FOR OUTPUT OF LOCAL HELIX PARAMETERS
C
C-------------------------------------------------------------
C
      ENTRY DALINF(IFLAG)
        IDALF = 1
        IF ( IFLAG .EQ. 0 )  IDALF = 0
      RETURN
C
C
C-------------------------------------------------------------
C
C--->    SWITCHING FOR SWIMMING TO TRACK ORIGIN
C
C-------------------------------------------------------------
C
      ENTRY UFSWIM(IFLAG)
        ISWIM = 1
        IF ( IFLAG .EQ. 0 )  ISWIM = 0
      RETURN
C
C-------------------------------------------------------------
C       INPUT FOR VDET PATTERN RECOGNITION STEERING
C-------------------------------------------------------------
C
      ENTRY UFVDIN(MVDIN,JSAFVD,JSBEVD)
        NVDIN  = MVDIN
        ISAFVD = JSAFVD
        ISBEVD = JSBEVD
      RETURN
C
C-------------------------------------------------------------
C       OUTPUT FROM SPECIAL VDET FILTER STEP
C-------------------------------------------------------------
C
      ENTRY UFVDOU(MVDOUT,CHSVD)
        MVDOUT = NVDOUT
        CHSVD  = CHI2VD
      RETURN
      END
