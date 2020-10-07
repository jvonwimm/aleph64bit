*DK DCCTR
      LOGICAL FUNCTION DCCTR(DA1,DA2)
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      LOGICAL FCUT(2)
      DCCTR=.FALSE.
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PCF,J_PTE,J_PCT'
      CALL DPARAM(11
     &  ,J_PFI,J_PCF,J_PTE,J_PCT)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF((PARADA(4,J_PCF).EQ.1.AND.PARADA(2,J_PCF).GT.0.).OR.
     &   (PARADA(4,J_PCT).EQ.1.AND.PARADA(2,J_PCT).GT.0.)) THEN
         DA=DA1
         DO   700  J=1,2
            FCUT(J)=.FALSE.
            IF(PARADA(4,J_PCF).EQ.1.AND.PARADA(2,J_PCF).GE.0.)THEN
               FI=DFINXT(PARADA(2,J_PFI),DHELIX(DA,IVFIDV))
               D=0.5*PARADA(2,J_PCF)
               IF(FI.LT.PARADA(2,J_PFI)-D.OR.
     &           FI.GT.PARADA(2,J_PFI)+D) THEN
                  FCUT(J)=.TRUE.
                  GO TO 102
               END IF
            END IF
            IF(PARADA(4,J_PCT).EQ.1.AND.PARADA(2,J_PCT).GE.0.)THEN
               TE=DHELIX(DA,IVTEDV)
               D=0.5*PARADA(2,J_PCT)
               IF(TE.LT.PARADA(2,J_PTE)-D.OR.
     &           TE.GT.PARADA(2,J_PTE)+D) THEN
                  FCUT(J)=.TRUE.
                  GO TO 102
               END IF
            END IF
  102       DA=DA2
  700    CONTINUE
         IF(FCUT(1).AND.FCUT(2)) THEN
            DCCTR=.TRUE.
         END IF
      END IF
      END
*DK DCCVX
      LOGICAL FUNCTION DCCVX(XYZ)
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION XYZ(3)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PCF,J_PTE,J_PCT'
      CALL DPARAM(11
     &  ,J_PFI,J_PCF,J_PTE,J_PCT)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PARADA(4,J_PCT).EQ.1.AND.PARADA(2,J_PCT).GE.0.)THEN
         RO=SQRT(XYZ(1)**2+XYZ(2)**2)
         IF(XYZ(3).EQ.0..AND.RO.EQ.0.) GO TO 5
         TE=DATN2D(RO,XYZ(3))
         D=0.5*PARADA(2,J_PCT)
         IF(TE.LT.PARADA(2,J_PTE)-D.OR.TE.GT.PARADA(2,J_PTE)+D)
     &     GO TO 9
      END IF
      IF(PARADA(4,J_PCF).EQ.1.AND.PARADA(2,J_PCF).GE.0.)THEN
         IF(XYZ(1).EQ.0..AND.XYZ(2).EQ.0.) GO TO 5
         FI=DFINXT(PARADA(2,J_PFI),DATN2D(XYZ(2),XYZ(1)))
         D=0.5*PARADA(2,J_PCF)
         IF(FI.LT.PARADA(2,J_PFI)-D.OR.FI.GT.PARADA(2,J_PFI)+D)
     &     GO TO 9
      END IF
    5 DCCVX=.FALSE.
      RETURN
    9 DCCVX=.TRUE.
      END
*DK DCIRCL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      SUBROUTINE DCIRCL
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:calculate vector TP(as defined in bank TGFT) from 3 points
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      PARAMETER (BFIELD=15.0,CLGHT=29.9792458)
      DIMENSION RIN(3),FIN(3),ZIN(3),S2RF(3),S2ZZ(3)
      DATA PIDEG /57.2957/
      DATA NPT/3/
      CHARACTER *3 DT3,TC
      CHARACTER *4 DT4,T4,T5
      CHARACTER *5 TP
      Q=1./PIDEG
      DO   700  N=1,3
         RIN(N)=EVARDP(IVRODV,N)
         FIN(N)=EVARDP(IVFIDV,N)*Q
         ZIN(N)=EVARDP(IVZZDV,N)
  700 CONTINUE
      CALL DCIRHL(RIN,FIN,ZIN,S2RF,S2ZZ,NPT,TPHEDT,CHIZ)
      RHO = 1./ABS(TPHEDT(1))
      PT = BFIELD*RHO*CLGHT/100000.
      THETA = ATAN2(1.,TPHEDT(2))
      PTOT = PT/SIN(THETA)
      TC=DT3(CHIZ)
      T4=DT4(TPHEDT(4))
      T5=DT4(TPHEDT(5))
      CALL DTP(SIGN(1.,TPHEDT(1))*PTOT,4,TP)
      CALL DWRT(' 1c fit dz='//TC//' dist to 0: dr='//
     &  T4//' dz='//T5//' p='//TP)
      PZ=PTOT*COS(THETA)
      PX=PT  *COS(TPHEDT(3))
      PY=PT  *SIN(TPHEDT(3))
      CALL DWRT(
     &  ' px='//DT4(PX)//
     &  ' py='//DT4(PY)//
     &  ' pz='//DT4(PZ) )
      END
*DK DCIRHL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      SUBROUTINE DCIRHL(RIN,FIN,ZIN,S2RF,S2ZZ,NPT,VV0,CHIZ)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by N. CHERNOV, G. OSOSKOV & M. POPPE    28-JUL-1988
C    Modified for use of IOPT=0 only BY H.DREVERMANN
C!:Fast fitting routine for helixes
C    Inputs    :RIN  = vector of RADII            /CM
C               FIN  = VECTOR OF PHI VALUES
C               ZIN  = VECTOR OF Z VALUES         /CM
C              S2RF  = VECTOR OF SIGMA**2(R*PHI)  /CM**2
C              S2ZZ  = VECTOR OF SIGMA**2(Z)      /CM**2
C               NPT  = NUMBER OF POINTS TO BE FITTED
C               IOPT = 0 -> DISTANCE**2=X**2+Y**2 MINIMISED
C                      1 -> WEIGHTED WITH 1/SIMA(R*PHI)**2
C                      2 -> ERROR MATRIX CALCULATED
C                      3 -> 3-DIMENSIONAL ITERATION
C    Outputs   : VV0 = 1/R*CHARGE   [1/CM]  POS. IF CLOCKWISE
C                TAN(LAMBDA)  {=DZ/DS}TAN(ANGLE TO X,Y PLANE)
C                PHI0         {0,2PI} ANGLE TO X-AXIS
C                D0*SIGN      [CM]    MINIMAL DIST. TO Z-AXIS,
C                                     POS. IF AXIS ENCIRCLED
C                Z0           [CM]    Z POS AT R=D0
C                CHI2= CHI SQUARED = SUM (DEVIATIONS/ERRORS)**2
C    Note: DEGREES OF FREEDOM = 2*NPT-5
C          BASED ON  SUBROUTINE CIRCLE
C          REFERENCE:  COMPUTER PHYSICS COMMUNICATIONS VOL 33,P329
C
C    Called by :
C ---------------------------------------------------------------------
      PARAMETER (EPS = 1.0E-16, ITMAX =15, MPT=40)
      REAL   PF(MPT),RF(MPT),SP2(MPT),VV0(*),
     1  DEL(MPT),SXY(MPT),ZF(MPT),WZF(MPT),SS0(MPT),EEE(MPT),
     2  GRAD(5)
      DOUBLE PRECISION XF(MPT),YF(MPT),WF(MPT)
         DOUBLE PRECISION ALF,A0,A1,A2,A22,BET,CUR,
     1        DD,DEN,DET,DY,D2,F,FACT,FG,F1,G,GAM,GAM0,G1,
     2        H,H2,P2,Q2,RM,RN,ROOT,
     3        XA,XB,XD,XI,XM,XX,XY,X1,X2,DEN2,
     4        YA,YB,YD,YI,YM,YY,   Y1,Y2,SA2B2,DD0,PHIC
            REAL RIN(*),FIN(*),ZIN(*),S2RF(*),S2ZZ(*)
            LOGICAL FIRST
            DATA FIRST/.TRUE./
            IF(FIRST) THEN
               PI    = 2.0*ASIN(1.0)
               PIO2  = 0.5*PI
               PIT2  = 2.0*PI
               FIRST =.FALSE.
            END IF
            DO 2   I=1,5
               GRAD(I)=0.0
               VV0(I) =0.0
    2       CONTINUE
            CHI2=0.0
            IF(NPT.GT.MPT) RETURN
C-----> INPUT DATA
            DO 10 I=1,NPT
               XF(I)  = RIN(I)*COS(FIN(I))
               YF(I)  = RIN(I)*SIN(FIN(I))
               RF(I)  = RIN(I)
               PF(I)  = FIN(I)
               WF(I)  = (S2RF(I)+0.000000001)**(-1)
               SP2(I) = WF(I)*(RF(I)*RF(I))
               ZF(I)  = ZIN(I)
               WZF(I) = 1.0/(S2ZZ(I)+0.000001)
   10       CONTINUE
*
            WSUM= 0.0
            RSS = 0.0
            PRO = 0.0
            IF (NPT .LE. 2)  RETURN
            N = NPT
            XM = 0.
            YM = 0.
            DO 100 I= 1, N
               WZF(I)= 1.0
               WF(I) = 1.0
               XM    = XM + XF(I)
               YM    = YM + YF(I)
  100       CONTINUE
            RN = 1./FLOAT(N)
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
  102       CONTINUE
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
  103       YA = A0 + XA*(A1 + XA*(A2 + XA*(XA-4.D0)))
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
  105    CONTINUE
         ROOT = XB
         GAM = GAM0*XB
         F1 = F - GAM
         G1 = G - GAM
         X1 = XD*G1 - YD*H
         Y1 = YD*F1 - XD*H
         DET = F1*G1 - H2
         DEN2= 1.D0/(X1**2 + Y1**2 + GAM*DET**2)
         IF(DEN2.LE.0.D0)                GO TO 999
         DEN = DSQRT(DEN2)
         CUR = DET*DEN                  + 0.0000000001D0
         ALF = -(XM*DET + X1)*DEN
         BET = -(YM*DET + Y1)*DEN
         RM = XM**2 + YM**2
         GAM = ((RM-GAM)*DET + 2.D0*(XM*X1 + YM*Y1))*DEN*0.5D0
C
C--------> CALCULATION OF STANDARD CIRCLE PARAMETERS
C          NB: CUR IS ALWAYS POSITIVE
         RR0=CUR
         ASYM = BET*XM-ALF*YM
         SST = 1.0
         IF(ASYM.LT.0.0) SST=-1.0
         RR0 = SST*CUR
         IF((ALF*ALF+BET*BET).LE.0.D0)              GO TO 999
         SA2B2 = 1.D0/DSQRT(ALF*ALF+BET*BET)
         DD0 = (1.D0-1.D0/SA2B2)/CUR
         PHIC = DASIN(ALF*SA2B2)+PIO2
         IF(BET.GT.0)    PHIC=PIT2-PHIC
         PH0 = PHIC+PIO2
         IF(RR0.LE.0)    PH0=PH0-PI
         IF(PH0.GT.PIT2) PH0=PH0-PIT2
         IF(PH0.LT.0.0)  PH0=PH0+PIT2
         VV0(1) = RR0
         VV0(3) = PH0
         VV0(4) = DD0
C
C-----> CALCULATE PHI DISTANCES TO MEASURED POINTS
C
         AA0 =SST
         OME =RR0
         GG0 = OME*DD0-AA0
         HH0 = 0.0000000001
         IF(ABS(GG0).LT.(1.0/HH0)) HH0=1.0/GG0
         HH0=1.0/GG0
         DO 300 I=1,N
            ASYM   = BET*XF(I)-ALF*YF(I)
            SS0(I) =1.0
            IF(ASYM.LT.0.0) SS0(I)=-1.0
            FF0= OME*(RF(I)*RF(I)-DD0*DD0)/(2.0*RF(I)*GG0)+DD0/RF(I)
C
            IF(FF0.LT.-1.0) FF0 = -1.0
            IF(FF0.GT.1.0)  FF0 = 1.0
C
            DEL(I)= PH0 + (SS0(I)-AA0)*PIO2 + SS0(I)*ASIN(FF0) - PF(I)
            IF(DEL(I).GT.PI) DEL(I)=DEL(I)-PIT2
            IF(DEL(I).LT.-PI)DEL(I)=DEL(I)+PIT2
  300    CONTINUE
C
C-----> FIT STRAIGHT LINE IN S-Z
C
         DO 350 I=1,N
            EEE(I) = 0.5*VV0(1)
     1        *SQRT(ABS( (RF(I)*RF(I)-VV0(4)*VV0(4))
     2        /(1.0-AA0*VV0(1)*VV0(4))     ) )
C
            IF(EEE(I).GT.1.0)  EEE(I)= 1.0
            IF(EEE(I).LT.-1.0) EEE(I)= -1.0
C
            SXY(I)=2.0*ASIN(EEE(I))/OME
  350    CONTINUE
         SUMS  = 0.0
         SUMSS = 0.0
         SUMZ  = 0.0
         SUMZZ = 0.0
         SUMSZ = 0.0
         SUMW  = 0.0
         DO 360 I=1,N
            SUMW  = SUMW  +                 WZF(I)
            SUMS  = SUMS  + SXY(I)        * WZF(I)
            SUMSS = SUMSS + SXY(I)*SXY(I) * WZF(I)
            SUMZ  = SUMZ  + ZF(I)         * WZF(I)
            SUMZZ = SUMZZ + ZF(I)*ZF(I)   * WZF(I)
            SUMSZ = SUMSZ + ZF(I)*SXY(I)  * WZF(I)
  360    CONTINUE
         DENOM = SUMW*SUMSS - SUMS*SUMS
         DZDS  = (SUMW*SUMSZ-SUMS*SUMZ) /DENOM
         ZZ0   = (SUMSS*SUMZ-SUMS*SUMSZ)/DENOM
         VV0(2)= DZDS
         VV0(5)= ZZ0
         CHIZ=0.
  999    DO 370 I=1,N
            DZ    = ZZ0+DZDS*SXY(I)-ZF(I)
            CHIZ=CHIZ+DZ**2
  370    CONTINUE
         CHIZ=SQRT(CHIZ)
      END
*DK DCTYCD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DCTYCD
CH
      SUBROUTINE DCTYCD
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *3 DT3
      CHARACTER *41 TCTF
      CHARACTER *124 TCUT
      CHARACTER *(*) THMID
      DATA PH1/-2./,PV1/-12./,PH2/14./,PV2/5./,PV3/3./
      DIMENSION NMAX(0:12)
      DATA NMAX/122,6*40,6*80/
      DIMENSION NPGR(2),NPTF(2)
      DATA N24/24/
      LOGICAL FDOWN
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_HCA'
      CALL DPARAM(20
     &  ,J_HCA)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(MODTP.EQ.1) THEN
        MODTP=0
        IF(IHTRDO(5).EQ.2) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+5
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          TCUT(NCUT-4:NCUT)=' U.C.'
        ELSE IF(IHTRDO(5).EQ.3) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+5
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          TCUT(NCUT1:NCUT)=' T.C.'
        ELSE IF(IHTRDO(5).EQ.4) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+5
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          TCUT(NCUT1:NCUT)=' F.C.'
        ELSE IF(IHTRDO(5).EQ.5) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+5
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          TCUT(NCUT1:NCUT)=' C.C.'
        END IF
        IF(FCUTDT.AND.PARADA(2,J_HCA).NE.0.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+5
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          TCUT(NCUT1:NCUT)=' imp.'
          GO TO 1
        END IF
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DCTYTR
CH
      ENTRY DCTYTR
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
    1 TPARDA=
     &  'J_HNI,J_HNT,J_HCM,J_HZ0,J_HD0,J_HF1,J_HF2,J_HT1,J_HT2,J_HZZ'
      CALL DPARAM(20
     &  ,J_HNI,J_HNT,J_HCM,J_HZ0,J_HD0,J_HF1,J_HF2,J_HT1,J_HT2,J_HZZ)
      TPARDA=
     &  'J_HST,J_HCQ'
      CALL DPARAM(20
     &  ,J_HST,J_HCQ)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(MODTR.EQ.1) THEN
        MODTR=0
        IF(PARADA(4,J_HCM).EQ.1..AND.PARADA(2,J_HCM).NE.0.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+6
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          IF(PARADA(2,J_HCM).GE.0) THEN
            CMO1=PARADA(2,J_HCM)
            TCUT(NCUT1:NCUT)=' P>'//DT3(CMO1)
          ELSE
            CMO2=-PARADA(2,J_HCM)
            TCUT(NCUT1:NCUT)=' P<'//DT3(CMO2)
          END IF
        END IF
        IF(PARADA(4,J_HZ0).EQ.1.AND.PARADA(2,J_HZ0).NE.0.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+7
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          IF(PARADA(2,J_HZ0).LT.0.) THEN
            CDZ1=-PARADA(2,J_HZ0)
            TCUT(NCUT1:NCUT)=' Z0>'//DT3(CDZ1)
          ELSE
            CDZ2=PARADA(2,J_HZ0)
            TCUT(NCUT1:NCUT)=' Z0<'//DT3(CDZ2)
          END IF
        END IF
        IF(PARADA(4,J_HD0).EQ.1.AND.PARADA(2,J_HD0).NE.0.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+7
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          IF(PARADA(2,J_HD0).LT.0.) THEN
            CDR1=-PARADA(2,J_HD0)
            TCUT(NCUT1:NCUT)=' D0>'//DT3(CDR1)
          ELSE
            CDR2=PARADA(2,J_HD0)
            TCUT(NCUT1:NCUT)=' D0<'//DT3(CDR2)
          END IF
        END IF
        IF(PARADA(4,J_HCQ).EQ.1.AND.PARADA(2,J_HCQ).NE.0.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+5
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          CCH=PARADA(2,J_HCQ)
          IF(CCH.GT.0.) THEN
            TCUT(NCUT1:NCUT)=' CH=+'
          ELSE
            TCUT(NCUT1:NCUT)=' CH=-'
          END IF
        END IF
        IF(PARADA(4,J_HNI).EQ.1.AND.PARADA(2,J_HNI).NE.0.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+7
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          CITC=PARADA(2,J_HNI)
          TCUT(NCUT1:NCUT)=' #i>'//DT3(CITC)
        END IF
        IF(PARADA(4,J_HNT).EQ.1.AND.PARADA(2,J_HNT).NE.0.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+7
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          TCUT(NCUT1:NCUT)=' NT='//DT3(PARADA(2,J_HNT))
        END IF
        CT1=-99999.
        CT2= 99999.
        IF(PARADA(4,J_HT1).EQ.1.) CT1=PARADA(2,J_HT1)
        IF(PARADA(4,J_HT2).EQ.1.) CT2=PARADA(2,J_HT2)
        IF(PARADA(4,J_HZZ).EQ.1.AND.PARADA(2,J_HZZ).NE.0.) THEN
          IF(PARADA(2,J_HZZ).LT.0.) THEN
            CT1=MAX(90.,CT1)
          ELSE
            CT2=MIN(90.,CT2)
          END IF
        END IF
        IF     (CT1.NE.-99999.AND.CT2.NE.99999.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+13
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          TCUT(NCUT1:NCUT)=' '//DT3(CT1)//'<J-tr<'//DT3(CT2)
          NGRK=NGRK+1
          NPGR(NGRK)=NCUT1+4
        ELSE IF(CT1.NE.-99999.AND.CT2.EQ.99999.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+9
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          TCUT(NCUT1:NCUT)=' J-tr>'//DT3(CT1)
          NGRK=NGRK+1
          NPGR(NGRK)=NCUT1
        ELSE IF(CT1.EQ.-99999.AND.CT2.NE.99999.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+11
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          TCUT(NCUT1:NCUT)=' J-tr<'//DT3(CT2)
          NGRK=NGRK+1
          NPGR(NGRK)=NCUT1
        END IF
        IF(PARADA(4,J_HF1).EQ.1..AND.PARADA(4,J_HF2).EQ.1.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+13
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          CF1=PARADA(2,J_HF1)
          CF2=PARADA(2,J_HF2)
          TCUT(NCUT1:NCUT)=' '//DT3(CF1)//'<F_tr<'//DT3(CF2)
          NGRK=NGRK+1
          NPGR(NGRK)=NCUT1+4
C                         +5 IS WRONG: DGTXTG(TCUT(2: ...
        ELSE IF(PARADA(4,J_HF1).EQ.1..AND.PARADA(4,J_HF2).NE.1.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+9
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          CF1=PARADA(2,J_HF1)
          TCUT(NCUT1:NCUT)=' F_tr>'//DT3(CF1)
          NGRK=NGRK+1
          NPGR(NGRK)=NCUT1
        ELSE IF(PARADA(4,J_HF1).NE.1..AND.PARADA(4,J_HF2).EQ.1.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+9
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          CF2=PARADA(2,J_HF2)
          TCUT(NCUT1:NCUT)=' F_tr<'//DT3(CF2)
          NGRK=NGRK+1
          NPGR(NGRK)=NCUT1
        END IF
        IF(IHTRDO(2).NE.3.AND.
     &    PARADA(4,J_HST).EQ.1.AND.PARADA(2,J_HST).NE.0.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+7
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          CST=PARADA(2,J_HST)
          IF(CST.GT.0.) THEN
            TCUT(NCUT1:NCUT)=' TR='//DT3(CST)
          ELSE
            TCUT(NCUT1:NCUT)=' TR#'//DT3(-CST)
          END IF
        END IF
        NUMT=BNUMDB(2,FRFTDB)
        IF(BNUMDB(4,FRFTDB).GT.0.) THEN
          DO 700 K=1,NUMT
            IF(COLRDT(K).LT.0.) THEN
              NCUT1=NCUT+1
              NCUT=NCUT+7
              TCUT(NCUT1:NCUT)='man.cut'
              GO TO 8
            END IF
  700     CONTINUE
        END IF
      END IF
    8 RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DCTYBA
CH
      ENTRY DCTYBA
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      IF(MODBA.EQ.1) THEN
        MODBA=0
        NCUT1=NCUT+1
        NCUT=NCUT+5
        IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
        TCUT(NCUT1:NCUT)=' B.C.'
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DCUTBA
CH
      ENTRY DCTYPA
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      IF(MODPA.EQ.1) THEN
        MODPA=0
        NCUT1=NCUT+1
        NCUT=NCUT+5
        IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
        TCUT(NCUT1:NCUT)=' Pads'
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DCTYCA
CH
      ENTRY DCTYCA
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_HCE,J_HCL'
      CALL DPARAM(20
     &  ,J_HCE,J_HCL)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(MODCA.EQ.1) THEN
        MODCA=0
        IF(PARADA(4,J_HCE).EQ.1.) THEN
          NCUT1=NCUT+1
          NCUT=NCUT+7
          IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
          TCUT(NCUT1:NCUT)=' En>'//DT3(PARADA(2,J_HCE))
        END IF
        IF(PARADA(4,J_HCL).EQ.1.) THEN
          IF(PARADA(2,J_HCL).EQ.0.)THEN
            NCUT1=NCUT+1
            NCUT=NCUT+5
            IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
            TCUT(NCUT1:NCUT)=' CL#0'
          ELSE
            NCUT1=NCUT+1
            NCUT=NCUT+7
            IF(NCUT.GT.NMAX(IAREDO)) GO TO 998
            TCUT(NCUT1:NCUT)=' CL='//DT3(PARADA(2,J_HCL))
          END IF
        END IF
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DCUTWR
CH
      ENTRY DCTYEX(THMID,NMID)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      IF(FPIKDP.OR.NOCLDT.EQ.1) RETURN
      CALL DQLEVL(ICTXDD)
      H=HMINDG(IAREDO)
      V=VMINDG(IAREDO)
      FDOWN=.FALSE.
      IF(NCUT.NE.0) THEN
        IF(IAREDO.LT.9.OR.IAREDO.GT.11.OR.NCUT.LE.NMX) THEN
          FDOWN=.TRUE.
          CALL DGTXTG(H+PH2,V+PV2,TCUT(2:NCUT),NCUT-1,NPGR,NGRK)
        ELSE
          CALL DGTDIR(90)
          CALL DGTXTG(H+PH1,V+PV3,TCUT(2:NCUT),NCUT-1,NPGR,NGRK)
          CALL DQLEVL(ICTXDD)
        END IF
      END IF
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PCF,J_PTE,J_PCT'
      CALL DPARAM(11
     &  ,J_PFI,J_PCF,J_PTE,J_PCT)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(MOD(IFIX(DFWIDU(IZOMDO))/100,10).NE.0) THEN
        NMD=MIN(NMID,N24)
        TCTF=THMID(1:NMD)
        NCTF=NMD
        NGTF=0
        IF(PARADA(4,J_PCF).EQ.1.) THEN
          NCTF1=NCTF+1
          NCTF=NCTF+11
          CF=PARADA(2,J_PCF)*0.5
          FI=PARADA(2,J_PFI)
          TCTF(NCTF1:NCTF)=' '//DT3(FI-CF)//'<F <'//DT3(FI+CF)
          NGTF=NGTF+1
          NPTF(NGTF)=NCTF1+5
        END IF
        IF(PARADA(4,J_PCT).EQ.1.) THEN
          NCTF1=NCTF+1
          NCTF=NCTF+11
          CT=PARADA(2,J_PCT)*0.5
          TE=PARADA(2,J_PTE)
          TCTF(NCTF1:NCTF)=' '//DT3(TE-CT)//'<J <'//DT3(TE+CT)
          NGTF=NGTF+1
          NPTF(NGTF)=NCTF1+5
        END IF
        IF(FDOWN) THEN
          CALL DGTDIR(90)
          CALL DGTXTG(H+PH1,V+PV1,TCTF,NCTF,NPTF,NGTF)
          CALL DQLEVL(ICTXDD)
        ELSE
          CALL DGTXTG(H    ,V+PV2,TCTF,NCTF,NPTF,NGTF)
        END IF
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DCTYP0
CH
      ENTRY DCTYP0(M)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      NCUT=0
      IF(FPIKDP.OR.
     &  MOD(IFIX(DFWIDU(IZOMDO))/1000,10).EQ.0) GO TO 999
C     IF(WISUDW.GT.0..AND.IAREDO.GE.1.AND.IAREDO.LE.6) GO TO 999
      TCUT=' '
      NGRK=0
      MODTP=M
      MODTR=M
      MODBA=M
      MODPA=M
      MODCA=M
      NMX=NMAX(IAREDO)
      RETURN
  998 NCUT=NMX+1
      TCUT(NCUT:NCUT)='-'
  999 MODTP=0
      MODTR=0
      MODBA=0
      MODPA=0
      MODCA=0
      END
*DK DCUTEC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      SUBROUTINE DCUTEC(K,FOUT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      LOGICAL FOUT
      FOUT=.TRUE.
      E=DVEC(IVENDV,K)
      IF(E.LT.ECMIDU) RETURN
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_HCE,J_HCL'
      CALL DPARAM(20
     &  ,J_HCE,J_HCL)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PARADA(4,J_HCE).EQ.1..AND.E.LT.PARADA(2,J_HCE)) RETURN
      IF(PARADA(4,J_HCL).EQ.1.) THEN
         IF(PARADA(2,J_HCL).EQ.0.)THEN
            IF(DVEC(IVU1DV,K).EQ.0.) RETURN
         ELSE
            IF(PARADA(2,J_HCL).NE.DVEC(IVU1DV,K)) RETURN
         END IF
      END IF
      FOUT=.FALSE.
      END
*DK DCUTHC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      SUBROUTINE DCUTHC(K,FOUT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      LOGICAL FOUT
      FOUT=.TRUE.
      E=DVHC(IVENDV,K)
      IF(E.LT.HCMIDU) RETURN
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_HCE'
      CALL DPARAM(20
     &  ,J_HCE)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PARADA(4,J_HCE).EQ.1..AND.E.LT.PARADA(2,J_HCE)) RETURN
      FOUT=.FALSE.
      END
*DK DCUTLC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      SUBROUTINE DCUTLC(K,FOUT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      LOGICAL FOUT
      DATA CE/0.03/
      FOUT=.TRUE.
      E=DVLC(IVENDV,K)
      IF(E.LT.CCMIDU) RETURN
      IF(E.LT.CE) RETURN
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_HCE'
      CALL DPARAM(20
     &  ,J_HCE)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PARADA(4,J_HCE).EQ.1..AND.
     &   PARADA(2,J_HCE).GE.E) RETURN
      FOUT=.FALSE.
      END
*DK DCUTFT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      SUBROUTINE DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:Get cut limits
C    Inputs    :
C    Outputs   :symbol cut fs1<phi<fs2  ts1<theta<ts2
C               total  cut fc1<phi<fc2  tc1<theta<tc2
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PCF,J_PTE,J_PCT'
      CALL DPARAM(11
     &  ,J_PFI,J_PCF,J_PTE,J_PCT)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      FI=PARADA(2,J_PFI)
      FS1=-999.
      FS2= 999.
      IF(PARADA(4,J_PCF).EQ.-1.) THEN
         D=999.
      ELSE
         D=0.5*PARADA(2,J_PCF)
      END IF
      IF(D.EQ.0.) D=999.
      FC1=FI-D
      FC2=FI+D
      TE=PARADA(2,J_PTE)
      TS1=-999.
      TS2= 999.
      IF(PARADA(4,J_PCT).EQ.-1.) THEN
         D=999.
      ELSE
         D=0.5*PARADA(2,J_PCT)
      END IF
      IF(D.EQ.0.) D=999.
      TC1=TE-D
      TC2=TE+D
      RETURN
      END
*DK DCUTTF
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      SUBROUTINE DCUTTF(FCUTF,FC1,FC2,FCUTT,TC1,TC2)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:Get cut limits
C    Inputs    :
C    Outputs   :symbol cut fs1<phi<fs2  ts1<theta<ts2
C               total  cut fc1<phi<fc2  tc1<theta<tc2
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      LOGICAL FCUTF,FCUTT
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PCF,J_PTE,J_PCT'
      CALL DPARAM(11
     &  ,J_PFI,J_PCF,J_PTE,J_PCT)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PARADA(4,J_PCF).EQ.-1..OR.PARADA(2,J_PCF).EQ.0.) THEN
         FC1=-1024.
         FC2= 1024.
         FCUTF=.FALSE.
      ELSE
         FI=PARADA(2,J_PFI)
         D=0.5*PARADA(2,J_PCF)
         FC1=FI-D
         FC2=FI+D
         FCUTF=.TRUE.
      END IF
      IF(PARADA(4,J_PCT).EQ.-1..OR.PARADA(2,J_PCT).EQ.0.) THEN
         TC1=-1024.
         TC2= 1024.
         FCUTT=.FALSE.
      ELSE
         TE=PARADA(2,J_PTE)
         D=0.5*PARADA(2,J_PCT)
         TC1=TE-D
         TC2=TE+D
         FCUTT=.TRUE.
      END IF
      END
*DK DCUTTR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      SUBROUTINE DCUTTR
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      PARAMETER (BFIELD=15.0,CLGHT=29.9792458)
      DATA PIDEG/57.29577951/
      DATA ZNT2/2./
      LOGICAL FCUT
      FCUTDT=.FALSE.
      CALL DVTR0(NUM)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_HCA,J_HNI,J_HNT,J_HCM,J_HZ0,J_HD0,J_HF1,J_HF2,J_HT1,J_HT2'
      CALL DPARAM(20
     &  ,J_HCA,J_HNI,J_HNT,J_HCM,J_HZ0,J_HD0,J_HF1,J_HF2,J_HT1,J_HT2)
      TPARDA=
     &  'J_HZZ,J_HST,J_HCQ'
      CALL DPARAM(20
     &  ,J_HZZ,J_HST,J_HCQ)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(NUM.EQ.0) GO TO 10
C
C     ========================= SET UP TRACK CUTS
C
      IF(PARADA(4,J_HCM).EQ.1..AND.PARADA(2,J_HCM).NE.0.) THEN
         IF(PARADA(2,J_HCM).GE.0) THEN
            CMO1=PARADA(2,J_HCM)
            CMO2=99999.
         ELSE
            CMO1=0.
            CMO2=-PARADA(2,J_HCM)
         END IF
         FCUTDT=.TRUE.
      ELSE
         CMO1=0.
         CMO2=99999.
      END IF
      IF(PARADA(4,J_HZ0).EQ.1.AND.PARADA(2,J_HZ0).NE.0.) THEN
         IF(PARADA(2,J_HZ0).LT.0.) THEN
            CDZ1=-PARADA(2,J_HZ0)
            CDZ2=99999.
         ELSE
            CDZ1=0.
            CDZ2=PARADA(2,J_HZ0)
         END IF
         FCUTDT=.TRUE.
      ELSE
         CDZ1=0.
         CDZ2=99999.
      END IF
      IF(PARADA(4,J_HD0).EQ.1.AND.PARADA(2,J_HD0).NE.0.) THEN
         IF(PARADA(2,J_HD0).LT.0.) THEN
            CDR1=-PARADA(2,J_HD0)
            CDR2=99999.
         ELSE
            CDR1=0.
            CDR2=PARADA(2,J_HD0)
         END IF
         FCUTDT=.TRUE.
      ELSE
         CDR1=0.
         CDR2=99999.
      END IF
      IF(PARADA(4,J_HT1).EQ.1.) THEN
         CT1=PARADA(2,J_HT1)
         FCUTDT=.TRUE.
      ELSE
         CT1=-99999.
      END IF
      IF(PARADA(4,J_HT2).EQ.1.) THEN
         CT2=PARADA(2,J_HT2)
         FCUTDT=.TRUE.
      ELSE
         CT2=99999.
      END IF
      IF(PARADA(4,J_HZZ).EQ.1.AND.PARADA(2,J_HZZ).NE.0.) THEN
         IF(PARADA(2,J_HZZ).LT.0.) THEN
            CT1=MAX(90.,CT1)
         ELSE
            CT2=MIN(90.,CT2)
         END IF
         FCUTDT=.TRUE.
      END IF
      IF(PARADA(4,J_HF1).EQ.1.) THEN
         CF1=PARADA(2,J_HF1)
         FCUTDT=.TRUE.
      ELSE
         CF1=-99999.
      END IF
      IF(PARADA(4,J_HF2).EQ.1.) THEN
         CF2=PARADA(2,J_HF2)
         FCUTDT=.TRUE.
      ELSE
         CF2=99999.
      END IF
      IF(IHTRDO(2).NE.3.AND.
     &  PARADA(4,J_HST).EQ.1.AND.PARADA(2,J_HST).NE.0.) THEN
         CST=PARADA(2,J_HST)
         FCUTDT=.TRUE.
      ELSE
         CST=0.
      END IF
      IF(PARADA(4,J_HCQ).EQ.1.AND.PARADA(2,J_HCQ).NE.0.) THEN
         CCH=-SIGN(1.,PARADA(2,J_HCQ))
         FCUTDT=.TRUE.
      ELSE
         CCH=0.
      END IF
      NCITC=0
      IF(PARADA(4,J_HNI).EQ.1.AND.PARADA(2,J_HNI).NE.0.) THEN
         CALL=DVCHI0(NTRI)
         IF(NTRI.GT.0) THEN
           CITC=PARADA(2,J_HNI)
           FCUTDT=.TRUE.
           NCITC=CITC
         END IF
      END IF
      NCTPC=0
C     ......... IN TF,RZ,RO,FZ TRACKS NEED AT LEAST 2 TPC HITS TO BE DRAWN
C     ................................................... -> FZTRDT=.TRUE.
      IF(FZTRDT.OR.
     &  (PARADA(4,J_HNT).EQ.1.AND.PARADA(2,J_HNT).NE.0.) ) THEN
        CALL=DVCHT0(NTRT)
        IF(NTRT.GT.0) THEN
          IF(PARADA(4,J_HNT).LE.0.) THEN
            CTPC=ZNT2
          ELSE
            CTPC=MAX(ZNT2,PARADA(2,J_HNT))
          END IF
          FCUTDT=.TRUE.
          NCTPC=CTPC
        END IF
      END IF
      IF(IHTRDO(5).EQ.3) THEN
         FNOTDT(0)=.TRUE.
         FCUT=.TRUE.
      ELSE
         FNOTDT(0)=.FALSE.
      END IF
C
C     ============================= STORE IF TRACK IS CUT OUT
C
      IF(FCUTDT) THEN
         DO   700  N=1,NUM
            IF(COLRDT(N).LT.0.) GO TO 1
            IF(NCITC.GT.0) THEN
              CALL=DVCHI(N,NITC)
              IF(NITC.LT.NCITC) GO TO 1
            END IF
            IF(NCTPC.GT.0) THEN
              CALL=DVCHT(N,NTPC)
              IF(NTPC.LT.NCTPC) GO TO 1
            END IF
            CALL DVTRV(N)
            TE=DATN2D(1.,TPHEDT(2))
            IF(TE.LT.CT1.OR.TE.GT.CT2) GO TO 1
            CH=SIGN(1.,TPHEDT(1))
            IF(CH.EQ.CCH) GO TO 1
            RHO=1./ABS(TPHEDT(1))
            PT=BFIELD*RHO*CLGHT/100000.
            PTOT=PT/SIND(TE)
            IF(PTOT.LT.CMO1.OR.PTOT.GT.CMO2) GO TO 1
            FI=MOD(3600.+TPHEDT(3)*PIDEG,360.)
            IF(FI.LT.CF1.OR.FI.GT.CF2) GO TO 1
            D0=ABS(TPHEDT(4))
            IF(D0.LT.CDR1.OR.D0.GT.CDR2) GO TO 1
            Z0=ABS(TPHEDT(5))
            IF(Z0.LT.CDZ1.OR.Z0.GT.CDZ2) GO TO 1
            NST=CST
            IF(NST.GT.0.) THEN
               IF(NST.NE.N) GO TO 1
            ELSE
               IF(-NST.EQ.N) GO TO 1
            END IF
            FNOTDT(N)=.FALSE.
            GO TO 700
    1       FNOTDT(N)=.TRUE.
  700    CONTINUE
      ELSE
         DO   710  N=1,NUM
            IF(COLRDT(N).LT.0.) THEN
              FNOTDT(N)=.TRUE.
              FCUTDT=.TRUE.
            ELSE
              FNOTDT(N)=.FALSE.
            END IF
  710    CONTINUE
      END IF
C
C     ======================== CUT HITS
C                                               UC
   10 IF(IHTRDO(5).EQ.2) THEN
        FCUHDT=.TRUE.
        DO 720 K=1,NUM
          FNOHDT( K)=.TRUE.
          FNOHDT(-K)=.TRUE.
  720   CONTINUE
        FNOHDT(0)=.FALSE.
        RETURN
      END IF
      IF(FCUTDT.AND.PARADA(2,J_HCA).NE.0.) THEN
C       ================================= TRACK CUTS IMPOSED ON HITS
        FCUHDT=FCUTDT
        IF(IHTRDO(5).LE.1) THEN
C         ===============================         AC
          DO 731 K=1,NUM
            FNOHDT( K)=FNOTDT(K)
            FNOHDT(-K)=FNOTDT(K)
  731     CONTINUE
          FNOHDT(0)=.FALSE.
        ELSE IF(IHTRDO(5).EQ.3) THEN
C         ===============================         TC
          DO 733 K=1,NUM
            FNOHDT( K)=FNOTDT(K)
            FNOHDT(-K)=FNOTDT(K)
  733     CONTINUE
          FNOHDT(0)=.TRUE.
          FCUHDT=.TRUE.
        ELSE IF(IHTRDO(5).GE.4) THEN
C         ===============================         FC,CC
          DO 734 K=1,NUM
            FNOHDT( K)=FNOTDT(K)
            FNOHDT(-K)=.TRUE.
  734     CONTINUE
          IF(IHTRDO(5).EQ.4) THEN
C           =============================         FC
            FNOHDT(0)=.TRUE.
          ELSE
C           =============================         CC
            FNOHDT(0)=.FALSE.
          END IF
          FCUHDT=.TRUE.
        END IF
      ELSE
C       ================================= TRACK CUTS NOT IMPOSED ON HITS
        FCUHDT=.FALSE.
        IF(IHTRDO(5).LE.1) THEN
C         ===============================         AC
          DO 741 K=1,NUM
            FNOHDT( K)=.FALSE.
            FNOHDT(-K)=.FALSE.
  741     CONTINUE
          FNOHDT(0)=.FALSE.
        ELSE IF(IHTRDO(5).EQ.3) THEN
C         ===============================         TC
          DO 743 K=1,NUM
            FNOHDT( K)=.FALSE.
            FNOHDT(-K)=.FALSE.
  743     CONTINUE
          FNOHDT(0)=.TRUE.
          FCUHDT=.TRUE.
        ELSE IF(IHTRDO(5).GE.4) THEN
C         ===============================         FC,CC
          DO 744 K=1,NUM
            FNOHDT( K)=.FALSE.
            FNOHDT(-K)=.TRUE.
  744     CONTINUE
          IF(IHTRDO(5).EQ.4) THEN
C           =============================         FC
            FNOHDT(0)=.TRUE.
          ELSE
C           =============================         CC
            FNOHDT(0)=.FALSE.
          END IF
          FCUHDT=.TRUE.
        END IF
      END IF
      END
