      SUBROUTINE UFITZS(N1,N2,N3,LIST1,LIST2,LIST3,IOPT,NMULT,RMS,XLEN,
     -VV0,EE0,COV,CHI2,IERR,FIELRC,CHIAC)
C -------------------------------------------------------------
C!    FITTING ROUTINE FOR SCATTERED HELICES IN ALEPH
C!    AUTHOR: LL. GARRIDO      JANUARY 1989
C!         warning: in the calling routine you have to have the
C!                  following dimensions:
C!                  VV0(7),EE0(28),COV(28)
C!                  FIELRC is a input parameter!
C!            N1   = NUMBER OF POINTS IN TPC TO BE FITTED
C!            N2   = NUMBER OF POINTS IN IPC TO BE FITTED
C!            N3   = NUMBER OF POINTS IN VDET TO BE FITTED
C!            LISTI= LIST OF POINTS TO BE FITTED
C!            IOPT =
C!                   1 -> CIRCLE+LINE
C!                   2 -> 3-DIMENSIONAL ITERATION
C!                   3 -> M.S. A*(R-R0)/R
C!                   4 -> M.S. I-DD0(A) I-PH0(A)
C!                   5 -> M.S. IN X-Y AND R-Z PLANE
C!            NMULT= FLAG TO INCREASE THE ERROS INSIDE TO TPC
C!                    DO TO MULTIPLE SATTERING (NMULT=1 YES)
C!                    (THIS FLAG IS NOT USED FOR IOPT=1)
C!            RMS=RADIUS OF SCATERING ANGLE
C!            XLEN=LENGH OF MATERIAL IN RADIATION LENGH UNITS
C!            FIELRC = MAGNETIC FIELD VALUE
C!  OUTPUT:   VV0 = 1/R*CHARGE   [1/CM]  NEG. IF CLOCKWISE
C!                  TAN(LAMBDA)  -=DZ/DS}TAN(ANGLE TO X,Y PLANE)
C!                  PHI0         -0,2PI} ANGLE TO X-AXIS
C!                  D0*SIGN      [CM] POS. IF AT THIS PONIT THE
C!                     ANGULAR MOMENTUM AROUND THE ORIGIN IS POS
C!                  Z0           [CM]    Z POS AT R=D0
C!                  ALFA         [RAD]   SCATTERING ANGLE IN X-Y
C!                  ALFAZ        [RAD]   SCATTERING ANGLE IN R-Z
C!            EE0 = INVERSE COVAR MATRIX IN TRIANG. FORM
C!            COV = COVAR MATRIX IN TRIANG. FORM
C!            CHI2= CHI SQUARED = SUM (DEVIATIONS/ERRORS)**2
C!            IERR= :
C!                  =1 IF CIRCLE+LINE FIT FAILS
C!                  =2 IF COVARIANCE MATRIX CAN NOT BE INVERTED
C!                  =3 IF COVAR. MAT. NOT DIAGONAL POSITIVE
C!                  =4 IF THERE IS NO FIELD (FIELRC=0.)
C!             CHIAC= VECTOR THAT CONTAINS THE CHI**2
C!                    CONTRIBUTION IF EACH POINT IN THE FIT
C---------------------------------------------------------------
C     BASED ON   1) SUBROUTINE CIRCLE  (N.CHERNOV, G. OSOSKOV )
C     REFERENCE:  COMPUTER PHYSICS COMMUNICATIONS VOL 33,P329
C                2) 3-DIMENSIONAL ITERACTION  (MARTIN POPPE)
C     REFERENCE:  ALEPH NOTE 87-102
C---------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
      PARAMETER (EPS = 1.0E-16, ITMAX =15, MPT=80,NINT0=4)
      PARAMETER (SCACO=.0160 , ALPD = .0141)
      REAL   PF(MPT),RF(MPT),SP2(MPT),VV0(*),EE0(*),
     1       DEL(MPT),ZF(MPT),WZF(MPT),SS0(MPT),
     2       DELZ(MPT),GRAD(7),COV(*),DV(7),CHIAC(MPT)
      DOUBLE PRECISION XF(MPT),YF(MPT),WF(MPT),XMID,YMID
      DOUBLE PRECISION ALF,ALM,A0,A1,A2,A22,BEM,BET,CUR,
     1   DD,DEN,DET,DY,D2,F,FACT,FG,F1,G,GAM,GAM0,GMM,G1,
     2   H,H2,P2,Q2,RM,RN,ROOT,
     3   XA,XB,XD,XI,XM,XX,XY,X1,X2,DEN2,
     4   YA,YB,YD,YI,YM,YY,   Y1,Y2,WN,SA2B2,DD0,CU2,PHIC
      INTEGER LIST1(*),LIST2(*),LIST3(*)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C     OFFSET FOR COORDINATE "IJ"
C
      KKTPCO(IJ) = KTPCO+2+(IJ-1)*(IW(KTPCO+1))
      KKIPCO(IJ) = KIPCO+2+(IJ-1)*(IW(KIPCO+1))
      KKVPCO(IJ) = KVPCO+2+(IJ-1)*(IW(KVPCO+1))
      IF(FIRST) THEN
          PI    = 2.0*ASIN(1.0)
          PIO2  = 0.5*PI
          PIT2  = 2.0*PI
          NTPCO = NAMIND('TPCO')
          NIPCO = NAMIND('ITCO')
          NVPCO = NAMIND('VDCO')
          IF(FIELRC.EQ.0.) THEN
            IERR = 4
            RETURN
          ELSE
            ROFP=1./(0.29979*FIELRC/10.)*100.
          ENDIF
          FIRST =.FALSE.
      END IF
      IERR=0
      N=N1+N2+N3
      IF(N.GT.MPT) RETURN
      IF(N.LT.3)   RETURN
C
C-----> INPUT DATA
C
      KTPCO = IW(NTPCO)
      KIPCO = IW(NIPCO)
      KVPCO = IW(NVPCO)
      DO 12 J=1,N3
        I=J
        KSTRT = KKVPCO(LIST3(J))
        XF(I)  = RW(KSTRT+2)*COS(RW(KSTRT+3))
        YF(I)  = RW(KSTRT+2)*SIN(RW(KSTRT+3))
        RF(I)  = RW(KSTRT+2)
        PF(I)  = RW(KSTRT+3)
        WF(I)  = (RW(KSTRT+5)+0.000000001)**(-1)
        SP2(I) = WF(I)*(RF(I)*RF(I))
        ZF(I)  = RW(KSTRT+4)
        WZF(I) = 1.0/(RW(KSTRT+6)+0.000001)
   12 CONTINUE
      DO 11 J=1,N2
        I=J+N3
        IPLUS=3
        IF(LIST2(J).LT.0) IPLUS=4
        KSTRT = KKIPCO(ABS(LIST2(J)))
        XF(I)  = RW(KSTRT+2)*COS(RW(KSTRT+IPLUS))
        YF(I)  = RW(KSTRT+2)*SIN(RW(KSTRT+IPLUS))
        RF(I)  = RW(KSTRT+2)
        PF(I)  = RW(KSTRT+IPLUS)
        WF(I)  = (RW(KSTRT+6)+0.000000001)**(-1)
        SP2(I) = WF(I)*(RF(I)*RF(I))
        ZF(I)  = RW(KSTRT+5)
        WZF(I) = 1.0/(RW(KSTRT+7)+0.000001)
   11 CONTINUE
      DO 10 J=1,N1
        I=J+N3+N2
        KSTRT = KKTPCO(LIST1(J))
        XF(I)  = RW(KSTRT+2)*COS(RW(KSTRT+3))
        YF(I)  = RW(KSTRT+2)*SIN(RW(KSTRT+3))
        RF(I)  = RW(KSTRT+2)
        PF(I)  = RW(KSTRT+3)
        WF(I)  = (RW(KSTRT+5)+0.000000001)**(-1)
        SP2(I) = WF(I)*(RF(I)*RF(I))
        ZF(I)  = RW(KSTRT+4)
        WZF(I) = 1.0/(RW(KSTRT+6)+0.000001)
   10 CONTINUE
C***************************************************************C
C                                                               C
C           CIRCLE FIT IN THE [X,Y] PLANE                       C
C           =============================                       C
C                                                               C
C***************************************************************C
      WSUM= 0.0
      RSS = 0.0
      PRO = 0.0
      XM = 0.
      YM = 0.
      WN=0.0
      DO 100 I= 1, N
        XM = XM + XF(I)*WF(I)
        YM = YM + YF(I)*WF(I)
        WN = WN + WF(I)
  100 CONTINUE
      RN = 1.D0/WN
C **
      XM = XM * RN
      YM = YM * RN
      X2 = 0.
      Y2 = 0.
      XY = 0.
      XD = 0.
      YD = 0.
      D2 = 0.
      DO 102 I= 1, N
         XI = XF(I) - XM
         YI = YF(I) - YM
         XX = XI**2
         YY = YI**2
         X2 = X2 + XX*WF(I)
         Y2 = Y2 + YY*WF(I)
         XY = XY + XI*YI*WF(I)
         DD = XX + YY
         XD = XD + XI*DD*WF(I)
         YD = YD + YI*DD*WF(I)
         D2 = D2 + DD**2*WF(I)
  102 CONTINUE
C **
      X2 = X2*RN
      Y2 = Y2*RN
      XY = XY*RN
      D2 = D2*RN
      XD = XD*RN
      YD = YD*RN
      F = 3.D0*X2 + Y2
      G = 3.D0*Y2 + X2
      FG = F*G
      H = XY + XY
      H2 = H**2
      P2 = XD**2
      Q2 = YD**2
      GAM0 = X2 + Y2
      FACT = GAM0**2
      A2 = (FG-H2-D2)/FACT
      FACT = FACT*GAM0
      A1 = (D2*(F+G) - 2.D0*(P2+Q2))/FACT
      FACT = FACT*GAM0
      A0 = (D2*(H2-FG) + 2.D0*(P2*G + Q2*F) - 4.D0*XD*YD*H)/FACT
      A22 = A2 + A2
      YB = 1.0E30
      ITER = 0
      XA = 1.D0
C **                MAIN ITERATION
  103 YA = A0 + XA*(A1 + XA*(A2 + XA*(XA-4.D0)))
      IF (ITER .GE. ITMAX)                      GO TO 105
      DY = A1 + XA*(A22 + XA*(4.D0*XA - 12.D0))
      XB = XA - YA/DY
      IF (ABS(YA).GT.ABS(YB)) XB=0.5D0*(XB+XA)
      IF (ABS(XA-XB) .LT. EPS)                  GO TO 105
      XA = XB
      YB = YA
      ITER = ITER + 1
      GO TO 103
C **
  105 CONTINUE
      ROOT = XB
      GAM = GAM0*XB
      F1 = F - GAM
      G1 = G - GAM
      X1 = XD*G1 - YD*H
      Y1 = YD*F1 - XD*H
      DET = F1*G1 - H2
      DEN2= 1.D0/(X1**2 + Y1**2 + GAM*DET**2)
      IF (DEN2.LE.0.D0)                GO TO 999
      DEN = DSQRT(DEN2)
      CUR = DET*DEN                  + 0.0000000001D0
      ALF = -(XM*DET + X1)*DEN
      BET = -(YM*DET + Y1)*DEN
      RM = XM**2 + YM**2
      GAM = ((RM-GAM)*DET + 2.D0*(XM*X1 + YM*Y1))*DEN*0.5D0
C
C--------> CALCULATION OF STANDARD CIRCLE PARAMETERS
C          NB: CUR IS ALWAYS POSITIVE
C
      RR0 = CUR
      ASYM = BET*XM-ALF*YM
      SST = -1.0
      IF(ASYM.LT.0.0) SST=1.0
      RR0 = SST*CUR
      IF((ALF*ALF+BET*BET).LE.0.D0)              GO TO 999
      SA2B2 = 1.D0/DSQRT(ALF*ALF+BET*BET)
      DD0 = SST*(1.D0-1.D0/SA2B2)/CUR
      PHIC = DASIN(ALF*SA2B2)+PIO2
      IF(BET.GT.0)    PHIC=PIT2-PHIC
      PH0 = PHIC+PIO2
      IF (RR0.GE.0)    PH0=PH0-PI
      IF (PH0.GT.PIT2) PH0=PH0-PIT2
      IF (PH0.LT.0.0)  PH0=PH0+PIT2
      ALFA=0.
      VV0(1) = RR0
      VV0(3) = PH0
      VV0(4) = DD0
      VV0(6) = ALFA
      CHECK=RR0*DD0
      IF(CHECK.EQ.1.) THEN
        DD0=DD0-.007
        VV0(4)=DD0
      ENDIF
C
C-----> CALCULATE PHI DISTANCES TO MEASURED POINTS
C
      GG0 = RR0*DD0-1.
      HH0 =1.0/GG0
      DO 210 I=1,N
        ASYM   = BET*XF(I)-ALF*YF(I)
        SS0(I) =-1.0
        IF (ASYM.LT.0.0) SS0(I)=1.0
      FF0=SST*(RR0*(RF(I)*RF(I)-DD0*DD0)/(2.*RF(I)*GG0)+DD0/RF(I))
        IF (FF0.LT.-1.0) FF0 = -1.0
        IF (FF0.GT.1.0)  FF0 = 1.0
        DEL(I)= PH0+(SST-SS0(I))*PIO2-SS0(I)*ASIN(FF0) - PF(I)
        IF (DEL(I).GT .PI) DEL(I)=DEL(I)-PIT2
        IF (DEL(I).LT.-PI) DEL(I)=DEL(I)+PIT2
  210 CONTINUE
C***************************************************************C
C                                                               C
C           STRAIGHT LINE FIT IN THE [S,Z] PLANE                C
C           ====================================                C
C                                                               C
C***************************************************************C
      SUMS  = 0.0
      SUMSS = 0.0
      SUMZ  = 0.0
      SUMSZ = 0.0
      SUMW  = 0.0
      DO 130 I=1,N
        EEE =
     = -.5*RR0*SQRT(ABS( (RF(I)*RF(I)-DD0*DD0)/(1.0-RR0*DD0)))
        IF(EEE.GT. 0.99990) EEE=  0.99990
        IF(EEE.LT.-0.99990) EEE= -0.99990
        SXY=-2.0*ASIN(EEE)/RR0
        IF(SS0(I).NE.SST) THEN
        SMAX=ABS(PIO2/RR0)
          IF(SXY.LT.SMAX) THEN
            SXY=-SXY
          ELSE
            SXY=4*SMAX-SXY
          ENDIF
        ENDIF
        SUMW  = SUMW  +                 WZF(I)
        SUMS  = SUMS  + SXY           * WZF(I)
        SUMSS = SUMSS + SXY*SXY       * WZF(I)
        SUMZ  = SUMZ  + ZF(I)         * WZF(I)
        SUMSZ = SUMSZ + ZF(I)*SXY     * WZF(I)
  130 CONTINUE
      DENOM = SUMW*SUMSS - SUMS*SUMS
      DZDS  = (SUMW*SUMSZ-SUMS*SUMZ) /DENOM
      ZZ0   = (SUMSS*SUMZ-SUMS*SUMSZ)/DENOM
      ALFAZ=0.
      VV0(2)= DZDS
      VV0(5)=ZZ0
      VV0(7)=ALFAZ
C
C-----> CALCULATE Z   DISTANCES TO MEASURED POINTS
C
        DO 371 I=1,N
          EEE =
     =    -.5*RR0*SQRT(ABS( (RF(I)*RF(I)-DD0*DD0)/(1.0-RR0*DD0)))
          IF(EEE.GT. 0.99990) EEE=  0.99990
          IF(EEE.LT.-0.99990) EEE= -0.99990
          SXY=-2.0*ASIN(EEE)/RR0
        IF(SS0(I).NE.SST) THEN
        SMAX=ABS(PIO2/RR0)
          IF(SXY.LT.SMAX) THEN
            SXY=-SXY
          ELSE
            SXY=4*SMAX-SXY
          ENDIF
        ENDIF
          DELZ(I)= ZZ0+DZDS*SXY-ZF(I)
 371    CONTINUE
C
C-----> CALCULATION CHI**2
C
      CHI2=0.
      DO 370 I=1,N
        CHIAC(I)= SP2(I)*DEL(I)*DEL(I)
     1                 + WZF(I)*DELZ(I)*DELZ(I)
       CHI2 = CHI2 + CHIAC(I)
  370 CONTINUE
      PM=1./ABS(ROFP*RR0)
      IF (IOPT.EQ.1) RETURN
C***************************************************************C
C                                                               C
C              MULTIPLE SCATTERING ERRORS INSIDE TPC            C
C              =====================================            C
C                                                               C
C***************************************************************C
      IF (NMULT.EQ.1) THEN
      COSZS=COS(ATAN(DZDS))
      PSI0  = SCACO * COSZS * CUR
C     PSI0  = SCACO * COS( ATAN(DZDS) ) * CUR
      XL = 0.0
      DO 140 J=2,N1
         I=N3+N2+J
         DDR = RF(I) - RF(I-1)
         IF(DDR.EQ.0.) GOTO 140
         DDX = XF(I) - XF(I-1)
         DDY = YF(I) - YF(I-1)
         DDZ = ZF(I) - ZF(I-1)
         XL  = XL + SQRT( DDX*DDX + DDY*DDY + DDZ*DDZ )
         SNA = DDR/SQRT( DDX*DDX + DDY*DDY )
         SNB = DDR/SQRT( DDR*DDR + DDZ*DDZ )
         WF(I)  = WF(I)/(1.D0+WF(I)*XL*(XL*PSI0/COSZS/SNA)**2)
C        WF(I)  = WF(I) / (1.D0 + WF(I)*XL*(XL*PSI0/SNA)**2 )
         SP2(I) = WF(I)*RF(I)*RF(I)
         WZF(I) = WZF(I) / (1.D0 + WZF(I)*XL*(XL*PSI0/SNB)**2 )
  140 CONTINUE
      ENDIF
C***************************************************************C
C                                                               C
C                     ERROR MATRIX                              C
C                     ============                              C
C                                                               C
C***************************************************************C
      CHIOL=CHI2
C     AL0=ALPD/PM*SQRT(XLEN)/SQRT(SQRT(1.+DZDS**2))
      AL0 =ALPD/PM*SQRT(XLEN)*SQRT(SQRT(1.+DZDS**2))
      AL0Z=ALPD/PM*SQRT(XLEN)/SQRT(SQRT(1.+DZDS**2))
      NINT=0
  555 NINT=NINT+1
      DO 51 I=1,28
 51     EE0(I)=0.0
      DO 52 I=1,7
        GRAD(I)=0.0
 52   CONTINUE
      EMS = -0.5*RR0
     1    *SQRT(ABS( (RMS*RMS-DD0*DD0)/(1.0-RR0*DD0)))
      IF (EMS.GT.1.) THEN
        EMS = 1.
      ELSEIF (EMS.LT.-1) THEN
        EMS = -1.
      ENDIF
      SMS= -2.0*ASIN(EMS)/RR0
C      ZMS= ZZ0+DZDS*SMS
C      TMS=-(ZMS-ZZ0)/DZDS*RR0
      TMS=-SMS*RR0
      SENMS=SIN(TMS)
      COSMS=COS(TMS)
      SECA2=1.+DZDS**2
      X2O=SENMS*ALFA/RR0
      X1O=-COSMS*ALFA/(1.-DD0*RR0)
      XTO=ALFAZ*SECA2
      XZO=-ALFAZ*SECA2*SMS
      DO 380 I=1,N
        X1=0.
        X2=0.
        XZ=0.
        XT=0.
        IF (RF(I).GT.RMS.AND.IOPT.EQ.3) THEN
          X2=0.
          X1=-ALFA*(RF(I)-RMS)/RF(I)
        ENDIF
         IF(RF(I).GT.RMS.AND.IOPT.GE.4) THEN
          X2=X2O
          X1=X1O
         ENDIF
         IF(RF(I).GT.RMS.AND.IOPT.EQ.5) THEN
          XZ=XZO
          XT=XTO
        ENDIF
        GG0 = RR0*DD0-1.
        HH0 = 1./GG0
        EEE = -0.5*RR0
     1      *SQRT(ABS((RF(I)*RF(I)-DD0**2)/(1.0-RR0*DD0)))
        IF (EEE.GT.0.99990) THEN
           EEE=  0.99990
        ELSEIF (EEE.LT.-0.99990)  THEN
           EEE= -0.99990
        ENDIF
        SXY=-2.0*ASIN(EEE)/RR0
        SIGZ=1.
        IF(SS0(I).NE.SST) THEN
            SMAX=ABS(PIO2/RR0)
            SIGZ=-1.
          IF(SXY.LT.SMAX) THEN
            SXY=-SXY
          ELSE
            SXY=4*SMAX-SXY
          ENDIF
        ENDIF
C-----> DERIVATIVES OF Z COMPONENT
        DZDSP=DZDS+XT
        GGG = EEE/SQRT(ABS( (1.0+EEE)*(1.0-EEE)))
        DZA = SXY
        CHECK=RF(I)*RF(I)-DD0*DD0
        IF(CHECK.EQ.0.) CHECK=2.*.007
        DZD = -2.0*( DZDSP/RR0) * GGG
     1   *(0.5*RR0/(1.0-DD0*RR0)-DD0/ CHECK )
        DZO = -DZDSP*SXY/RR0
     1        -SIGZ*DZDSP*GGG/( RR0*RR0)
     2         *( 2.0+ RR0*DD0/(1.0-RR0*DD0) )
C------ OTHER DERIVATIVES
        DPAL= 0.
        DZALZ=0.
        DD0P=DD0+X2
        GG0P= RR0*DD0P-1.
        HH0P=1./GG0P
       FF0=SST*(RR0*(RF(I)*RF(I)-DD0P*DD0P)/(2.0*RF(I)*GG0P)+DD0P/RF(I))
        IF(FF0.GT. 0.99990)  FF0=  0.99990
        IF(FF0.LT.-0.99990)  FF0= -0.99990
        ETA = -SST*SS0(I)/SQRT(ABS((1.0+FF0)*(1.0-FF0)))
        DFD = (1.0+HH0P*HH0P*(1.0-RR0*RR0*RF(I)*RF(I)))/(2.0*RF(I))
        DFO = -(RF(I)*RF(I)-DD0P*DD0P)*HH0P*HH0P/(2.0*RF(I))
        DPD = ETA*DFD
        DPO = ETA*DFO
        IF(RF(I).GT.RMS.AND.IOPT.EQ.3) THEN
          DPAL=-(RF(I)-RMS)/RF(I)
        ENDIF
        IF(RF(I).GT.RMS.AND.IOPT.GE.4) THEN
          DDPAL=SENMS/RR0
          DPPAL=-COSMS/(1.-RR0*DD0)
          DPAL=DPD*DDPAL +1.*DPPAL
C         IF(IOPT.EQ.5) DZALZ=DZA*SECA2
          IF(IOPT.EQ.5) DZALZ=(DZA-SMS)*SECA2
        ENDIF
C-----> ERROR MARTIX
        EE0(1) = EE0(1) + SP2(I)*  DPO*DPO   + WZF(I) * DZO*DZO
        EE0(2) = EE0(2)                      + WZF(I) * DZA*DZO
        EE0(3) = EE0(3)                      + WZF(I) * DZA*DZA
        EE0(4) = EE0(4) + SP2(I)*  DPO
        EE0(6) = EE0(6) + SP2(I)
        EE0(7) = EE0(7) + SP2(I)*  DPO*DPD   + WZF(I) * DZO*DZD
        EE0(8) = EE0(8)                      + WZF(I) * DZA*DZD
        EE0(9) = EE0(9) + SP2(I)*      DPD
        EE0(10)= EE0(10)+ SP2(I)*  DPD*DPD   + WZF(I) * DZD*DZD
        EE0(11)= EE0(11)                     + WZF(I) * DZO
        EE0(12)= EE0(12)                     + WZF(I) * DZA
        EE0(14)= EE0(14)                     + WZF(I) * DZD
        EE0(15)= EE0(15)                     + WZF(I)
        EE0(16)= EE0(16)+ SP2(I)*DPO*DPAL
        EE0(18)= EE0(18)+ SP2(I)*1.*DPAL
        EE0(19)= EE0(19)+ SP2(I)*DPD*DPAL
        EE0(21)= EE0(21)+ SP2(I)*DPAL*DPAL
        EE0(22)= EE0(22)                      + WZF(I) * DZO*DZALZ
        EE0(23)= EE0(23)                      + WZF(I) * DZA*DZALZ
        EE0(25)= EE0(25)                      + WZF(I) * DZD*DZALZ
        EE0(26)= EE0(26)                      + WZF(I) * 1. *DZALZ
        EE0(28)= EE0(28)                      + WZF(I) * DZALZ*DZALZ
C-----> GRADIENT VECTOR
        GRAD(1)=GRAD(1) - DEL(I) *SP2(I)*DPO - DELZ(I)*WZF(I)*DZO
        GRAD(2)=GRAD(2) -                      DELZ(I)*WZF(I)*DZA
        GRAD(3)=GRAD(3) - DEL(I) *SP2(I)
        GRAD(4)=GRAD(4) - DEL(I) *SP2(I)*DPD - DELZ(I)*WZF(I)*DZD
        GRAD(5)=GRAD(5) -                      DELZ(I)*WZF(I)
        GRAD(6)=GRAD(6) - DEL(I)*SP2(I)*DPAL
        GRAD(7)=GRAD(7)                      - DELZ(I)*WZF(I)*DZALZ
 380  CONTINUE
      IF(IOPT.GT.2) THEN
        EE0(21)=EE0(21)+1./AL0**2
        GRAD(6)=GRAD(6)-1./AL0**2*ALFA
        IF(IOPT.EQ.5) THEN
          EE0(28)=EE0(28)+1./AL0Z**2
          GRAD(7)=GRAD(7)-1./AL0Z**2*ALFAZ
        ENDIF
      ENDIF
C***************************************************************C
C                                                               C
C         NEWTONIAN ITERATION IN NDIM PARAMETERS                C
C         ===================================                   C
C                                                               C
C***************************************************************C
      DO 401 I=1,28
  401   COV(I)=EE0(I)
      NDIM = 5
      IF (IOPT.GT.2) NDIM=6
      IF (IOPT.EQ.5) NDIM=7
      CALL SMINV(COV,VV1,NDIM,0,NRANK)
      IF(NRANK.NE.NDIM) GOTO 998
      IF(COV(1).LE.0.) GOTO 997
      IF(COV(3).LE.0.) GOTO 997
      IF(COV(6).LE.0.) GOTO 997
      IF(COV(10).LE.0.) GOTO 997
      IF(COV(15).LE.0.) GOTO 997
      IF(NDIM.GT.5.AND.COV(21).LE.0.) GOTO 997
      IF(NDIM.EQ.7.AND.COV(28).LE.0.) GOTO 997
      CALL SMAV(DV,COV,GRAD,NDIM)
      DO 402 I=1,NDIM
  402   VV0(I)=VV0(I)+DV(I)
      RR0 =VV0(1)
      DZDS=VV0(2)
      PH0 =VV0(3)
      DD0 =VV0(4)
      ZZ0 =VV0(5)
      ALFA =VV0(6)
      ALFAZ=VV0(7)
      CHECK=RR0*DD0
      IF(CHECK.EQ.1.) THEN
        DD0=DD0-.007
        VV0(4)=DD0
      ENDIF
C
C------>  NEW DIFFERENCES IN PHI AND Z
C
      DO 410 I=1,N
        X1=0.
        X2=0.
        XT=0.
        XZ=0.
        IF(RF(I).GT.RMS.AND.IOPT.EQ.3) X1=-ALFA*(RF(I)-RMS)/RF(I)
        IF(RF(I).GT.RMS.AND.IOPT.GT.3) THEN
          X2=SENMS*ALFA/RR0
          X1=-COSMS*ALFA/(1.-RR0*DD0)
          IF(IOPT.EQ.5) THEN
          XT=ALFAZ*(1.+DZDS**2)
          XZ=-ALFAZ*(1.+DZDS**2)*SMS
          ENDIF
        ENDIF
        DD0P=DD0+X2
        DZDSP=DZDS+XT
        GG0=RR0*DD0-1.
        HH0=1./GG0
        GG0P=RR0*DD0P-1.
        HH0P=1./GG0P
      FF0=SST*(RR0*(RF(I)*RF(I)-DD0P**2)/(2.0*RF(I)*GG0P)+DD0P/RF(I))
        IF(FF0.GT.1.0)  FF0 = 1.0
        IF(FF0.LT.-1.0) FF0 = -1.0
        DEL(I)=PH0+X1+(SST-SS0(I))*PIO2-SS0(I)*ASIN(FF0)-PF(I)
        IF(DEL(I).GT.PI) DEL(I)=DEL(I)-PIT2
        IF(DEL(I).LT.-PI)DEL(I)=DEL(I)+PIT2
        EEE=-0.5*RR0
     1     *SQRT(ABS((RF(I)*RF(I)-DD0**2)/(1.0-RR0*DD0)))
        IF(EEE.GT. 0.99990)  EEE=  0.99990
        IF(EEE.LT.-0.99990)  EEE= -0.99990
        SXY=-2.0*ASIN(EEE)/RR0
        IF(SS0(I).NE.SST) THEN
        SMAX=ABS(PIO2/RR0)
          IF(SXY.LT.SMAX) THEN
            SXY=-SXY
          ELSE
            SXY=4*SMAX-SXY
          ENDIF
        ENDIF
        DELZ(I)= ZZ0+XZ+DZDSP*SXY-ZF(I)
  410 CONTINUE
C-----> CALCULATION CHI**2
      CHI1 = 0.0
      DO 420 I=1,N
        CHIAC(I)= SP2(I)*DEL(I)*DEL(I)
     1                 + WZF(I)*DELZ(I)*DELZ(I)
       CHI1 = CHI1 + CHIAC(I)
  420 CONTINUE
      IF(IOPT.GT.2) CHI1=CHI1+(ALFA/AL0)**2
      IF(IOPT.EQ.5) CHI1=CHI1+(ALFAZ/AL0Z)**2
      CHI2 = CHI1
      PM = 1./ABS(ROFP*RR0)
      XDIF = ABS(CHIOL-CHI2)
      IF (XDIF.LT..03) RETURN
      CHIOL = CHI2
      IF (NINT.LT.NINT0) GOTO 555
      RETURN
 997  IERR=  3
      GOTO 1000
 998  IERR = 2
      GOTO 1000
 999  CHI2 = 1.E30
      IERR =  1
1000  CALL VZERO(EE0,28)
      CALL VZERO(COV,28)
C     TAKING STANDAR ERRORS FOR A PARTICLE OF 3 GEV
      IF(VV0(1).EQ.0.) VV0(1)=.01
      COV(1)=(.005/ROFP)**2
      COV(3)=.002**2
      COV(6)=.003**2
      COV(10)=.007**2
      COV(15)=.007**2
      IF(NDIM.GT.5) COV(21)=.001**2
      IF(NDIM.EQ.7) COV(28)=.001**2
      EE0(1)=1./COV(1)
      EE0(3)=1./COV(3)
      EE0(6)=1./COV(6)
      EE0(10)=1./COV(10)
      EE0(15)=1./COV(15)
      IF(NDIM.GT.5) EE0(21)=1./COV(21)
      IF(NDIM.EQ.7) EE0(28)=1./COV(28)
      RETURN
      END
#endif
