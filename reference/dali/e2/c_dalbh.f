*DK DHCROS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DHCROS
CH
      SUBROUTINE DHCROS(CH2,X1,Y1,Z,A,X2,Y2)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!: calculate the two crossing angles and positions of two circles
C    Outputs   :FOUT=TRUE IF NO CROSSING
C               output X,Y,Z,A FOR BOTH CROSSINGS
C               X2 Y2 = TWO POINTS 0.2 DEGRESS DOWNWARDS
C
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      DIMENSION X1(2,2),Y1(2,2),Z(2,2),A(2,2),X2(2,2),Y2(2,2)
      DOUBLE PRECISION C12,S12,CS,Q,W,VRTX,VRTY
      DOUBLE PRECISION CC(2),SS(2),RC(2)
      DIMENSION A0(2),BZ(2),Z0(2),CH(2),FI(2)
C     DATA PIDGDK /57.2957/
      DATA DF/.2/
      DATA LDEB/0/
      CH2=0.
      L2=2
      L1=1
      C12=CC(L1)-CC(L2)
      S12=SS(L1)-SS(L2)
      CS=C12*C12+S12*S12
      Q=2.*RC(L1)*SQRT(CS)
      IF(Q.EQ.0.) RETURN
      W=(RC(L2)*RC(L2)-RC(L1)*RC(L1)-CS)/Q
      IF(ABS(W).GT.1.) RETURN
      ACW=ACOSD(W)
      S12_7=S12
      C12_7=C12
      B=DATN2D(S12_7,C12_7)
      A(1,1)=B+ACW
      A(1,2)=B-ACW
      A(1,1)=MOD(A(1,1)+36000.,360.)
      A(1,2)=MOD(A(1,2)+36000.,360.)
      SSRS=S12+RC(L1)*SIND(A(1,1))
      CCRC=C12+RC(L1)*COSD(A(1,1))
      A(2,1)=DATN2D(SSRS,CCRC)
      SSRS=S12+RC(L1)*SIND(A(1,2))
      CCRC=C12+RC(L1)*COSD(A(1,2))
      A(2,2)=DATN2D(SSRS,CCRC)
      DO 700 NT=1,2
        DO 701 NA=1,2
          CA=A(NT,NA)
          DA=A(NT,NA)-CH(NT)*DF
          X1(NT,NA)=CC(NT)+RC(NT)*COSD(CA)
          Y1(NT,NA)=SS(NT)+RC(NT)*SIND(CA)
          X2(NT,NA)=CC(NT)+RC(NT)*COSD(DA)
          Y2(NT,NA)=SS(NT)+RC(NT)*SIND(DA)
          A(NT,NA)=DFINXT(FI(NT),CH(NT)*(A0(NT)-CA))
          Z(NT,NA)=Z0(NT)+BZ(NT)*A(NT,NA)
  701   CONTINUE
  700 CONTINUE
      CH2=CH(1)*CH(2)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DHCRS0
CH
      ENTRY DHCRS0(N)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :N=TRACK 1 OR 2
C     CALL TWICE WITH 2 DIFFERENT TRACKS BEFORE DHCRS
C ---------------------------------------------------------------------
      RC(N)=1./ABS(TPHEDT(1))
      CH(N)=SIGN(1.,TPHEDT(1))
      A0(N)=TPHEDT(3)*PIDGDK+CH(N)*90.
      X0   =TPHEDT(4)*COSD(A0(N))
      Y0   =TPHEDT(4)*SIND(A0(N))
      Z0(N)=TPHEDT(5)
      BZ(N)=RC(N)*TPHEDT(2)/PIDGDK
      CC(N)=X0   -RC(N)*COSD(A0(N))
      SS(N)=Y0   -RC(N)*SIND(A0(N))
      IF(LDEB.EQ.0) THEN
        VRTY=VRTYDV
        VRTX=VRTXDV
        FIV=DATN2D(VRTY-SS(N),VRTX-CC(N))
        F00=DATN2D(    -SS(N),    -CC(N))
        FI(N)=180.+CH(N)*(F00-FIV)
      ELSE
        FI(N)=180.
      END IF
      END
*DK DHPICK
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DHPICK
CH
      SUBROUTINE DHPICK(TPR,X,Y,Z)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!: calculate the two crossing angles and positions of two circles
C    Outputs   :FOUT=TRUE IF NO CROSSING
C               output X,Y,Z,A FOR BOTH CROSSINGS
C               X2 Y2 = TWO POINTS 0.2 DEGRESS DOWNWARDS
C
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
C     DATA PIDGDK/57.29577951/
      DATA LDEB/0/,CRHOP/0./
      IF(MODEDT.EQ.1) THEN
        RV=ABS(TPHEDT(4))
        RC=1./ABS(TPHEDT(1))
        CH=SIGN(1.,TPHEDT(1))
C                                                    RO=RC-TPHEDT(4)
        A0=TPHEDT(3)*PIDGDK+CH*90.
        CA0=COSD(A0)
        SA0=SIND(A0)
C                                                    XC=RO*COS(FC)
C                                                    YC=RO*SIN(FC)
        X0=TPHEDT(4)*COSD(A0)
        Y0=TPHEDT(4)*SIND(A0)
        XC=X0-RC*CA0
        YC=Y0-RC*SA0
        Z0=TPHEDT(5)
        BZ=RC*TPHEDT(2)/PIDGDK
      ELSE
        CH=CLTMDT(1)
        PT=SQRT(PTRADT(1)**2+PTRADT(2)**2)
        IF(PT.EQ.0.) RETURN
        RC=CRHOP*PT/ABS(CH)
        CH=SIGN(1.,CH)
        F0=DATN2D(PTRADT(2),PTRADT(1))
        X0=VTX1DT(1)
        Y0=VTX1DT(2)
        A0=F0+CH*90.
        SA0=SIND(A0)
        CA0=COSD(A0)
        XC=X0-RC*CA0
        YC=Y0-RC*SA0
        Z0=VTX1DT(3)
        BZ=CRHOP*PTRADT(3)/(PIDGDK*ABS(CH))
      END IF
      CALL DQINV(IAREDO,HPIKDP,VPIKDP,HPU,VPU)
      FI=DATN2D(VPU-YC,HPU-XC)
      FIV=DATN2D(VRTYDV-YC,VRTXDV-XC)
      F00=DATN2D(      -YC,      -XC)
      FI1=CH*(F00-FIV)
      F90=FI1+90.
      IF     (LDEB.EQ.0) THEN
        AL=-CH*(FI-A0)
      ELSE IF(LDEB.EQ.1) THEN
        AL= CH*(FI-A0)
      ELSE IF(LDEB.EQ.2) THEN
        AL=FI-A0
      ELSE IF(LDEB.EQ.1) THEN
        AL=A0-FI
      END IF
      AL=DFINXT(F90,AL)
      IF(AL.LT.FI1) THEN
        AL=FI1
        FI=A0+AL*CH
      ELSE IF(AL.GT.FI1+180.) THEN
        AL=FI1+180.
        FI=A0+AL*CH
      END IF
      X=XC+RC*COSD(FI)
      Y=YC+RC*SIND(FI)
      Z=Z0+BZ*AL
      END
*DK DHELCI
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DHELCI
CH
      SUBROUTINE DHELCI(XC,YC,RH,CH,A0,RDET,AEND,AMIN)
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
      INCLUDE 'DALI_CF.INC'
C     DATA PIDGDK /57.2957/
      IF(RH.GT.0.) THEN
        RC2=XC**2+YC**2
        RC=SQRT(RC2)
        BB=(RDET**2-RH**2-RC2)/(2.*RH*RC)
        IF(ABS(BB).LE.1.) THEN
          CC=ACOSD(BB)
          GA=DATN2D(YC,XC)
          A1=MOD(3600.+CH*(A0-GA+CC),360.)
          A2=MOD(3600.+CH*(A0-GA-CC),360.)
          AMIN=MIN(AEND,A1,A2)
        ELSE
          AMIN=AEND
        END IF
      ELSE
        AMIN=0.
      END IF
      END
*DK DHELIX
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  DHELIX
CH
      FUNCTION DHELIX(DAL,NV)
CH
CH ********************************************************************
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
C     DATA PIDGDK /57.2957/
C     DATA RTMIN/39.1/,RIIT/12.8/
      DATA RTMIN/39.1/,RIIT/6./
      DIMENSION RBECA(2)
C     DATA EBIR,EBOR/184.7,229.44/
      DATA     RBECA/184.7,229.44/
      DATA TTPOR/180./,TTPOZ/220./
      DIMENSION PART(5)
      DIMENSION TBX(13),TBY(13),DX(12),DY(12),A(12),Q(12),B(12)
      DIMENSION DAECA(2),DATPC(2)
      LOGICAL FBAR(2),FOUT,FNVD,FSECV
      DIMENSION XYZ(3)
C     CRHOP=100000/(BFIELD*CLGHT)=100000/(15.0*29.9792458)
      DATA CRHOP/222.3760635/,A180/180./
      DATA MDEB/0/
      IF(DAL.NE.DAOLD) THEN
        DA=-CH*DAL
        X=X0+RC*(COSD(A0+DA)-CA0)
        Y=Y0+RC*(SIND(A0+DA)-SA0)
        Z=Z0+BZ*DAL
        DAOLD=DAL
      END IF
      GO TO (1,2,3,4,5,6,7,8),NV
   99 DHELIX=0.
      RETURN
    1 DHELIX=X
      RETURN
    2 DHELIX=Y
      RETURN
    3 DHELIX=Z
      RETURN
    4 DHELIX=SQRT(X**2+Y**2)
      RETURN
    5 IF(FNVD) THEN
        DHELIX=ATAN2(Y,X)*PIDGDK
        IF(DHELIX.LT.0.) DHELIX=DHELIX+360.
      ELSE
        DHELIX=ATAN2(Y-VRTYDV,X-VRTXDV)*PIDGDK
        IF(DHELIX.LT.0.) DHELIX=DHELIX+360.
      END IF
      RETURN
    6 IF(FNVD) THEN
        RO=SQRT(X**2+Y**2)
        IF(RO.GT.TTPOR.OR.Z.GT.TTPOZ) THEN
          DHELIX=ATAN2(RO,Z)*PIDGDK
        ELSE
          TEDZ=ATAN2(RO,Z-VRDZDV)*PIDGDK
          IF(TEDZ.LE.90.) THEN
            DHELIX=TEDZ-QTETA*TEDZ
          ELSE
            DHELIX=TEDZ-QTETA*(180.-TEDZ)
          END IF
        END IF
      ELSE
        XVD=X-VRTXDV
        YVD=Y-VRTYDV
        RO=SQRT(XVD*XVD+YVD*YVD)
        DHELIX=ATAN2(RO,Z-VRDZDV)*PIDGDK
      END IF
      RETURN
    7 DHELIX=SQRT(X**2+Y**2+Z**2)
      RETURN
    8 IF(FNVD) THEN
        ZA=ABS(Z)
        RO=X**2+Y**2
        D=SQRT(RO+Z**2)
        RO=SQRT(RO)
        IF(RO*ZMAXDK.GT.ZA*RMAXDK) THEN
          DHELIX=D*(RMAXDK/RO-1.)
        ELSE
          DHELIX=D*(ZMAXDK/ZA-1.)
        END IF
      ELSE
        XVD=X-VRTXDV
        YVD=Y-VRTYDV
        ZVD=Z-VRDZDV
        RO2=XVD*XVD+YVD*YVD
        DHELIX=(SQRT(RO2)-ROVDM)*SQRT(1.+ZVD*ZVD/RO2)
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DHELX0
CH
      ENTRY DHELX0(DASVX,DAITC,DATPC,DAECA,FBAR)
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
C     .............. TPHEDT
C     ..............  1     = CH * 1/R
C     ..............  2     = TE
C     ..............  3     = FI
C     ..............  4     = D0
C     ..............  5     = Z0

      DHELX0=0
      QTETA=VRDZDV*FTDZDU
      RV=ABS(TPHEDT(4))
      RC=1./ABS(TPHEDT(1))
      CH=SIGN(1.,TPHEDT(1))
C                                                    RO=RC-TPHEDT(4)
      A0=TPHEDT(3)*PIDGDK+CH*90.
      CA0=COSD(A0)
      SA0=SIND(A0)
C                                                    XC=RO*COS(FC)
C                                                    YC=RO*SIN(FC)
      X0=TPHEDT(4)*COSD(A0)
      Y0=TPHEDT(4)*SIND(A0)
      XC=X0-RC*CA0
      YC=Y0-RC*SA0
C                                                    F0=PIDGDK*FC
      Z0=TPHEDT(5)
      BZ=RC*TPHEDT(2)/PIDGDK
      AEND=A180
      CALL DHPERP(N3DIDT,FRFTDT,XYZVDT,ARCL,PART)
      DASVX=PIDGDK*ARCL/RC
      DASV=DASVX
      GO TO 101
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DHELX1
CH
      ENTRY DHELX1(DAITC,DATPC,DAECA,FBAR,FOUT)
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
      DASV=0
      DHELX1=0
      QTETA=VRDZDV*FTDZDU
      CH=CLTMDT(1)
      PT=SQRT(PTRADT(1)**2+PTRADT(2)**2)
      IF(PT.EQ.0.) THEN
        FOUT=.TRUE.
      ELSE
        FOUT=.FALSE.
      END IF
      RC=CRHOP*PT/ABS(CH)
      CH=SIGN(1.,CH)
      F0=DATN2D(PTRADT(2),PTRADT(1))
      X0=VTX1DT(1)
      Y0=VTX1DT(2)
      A0=F0+CH*90.
      SA0=SIND(A0)
      CA0=COSD(A0)
      XC=X0-RC*CA0
      YC=Y0-RC*SA0
      IF(FVTXDT) THEN
        BE=DATN2D(VTX2DT(2)-YC,VTX2DT(1)-XC)
        AEND=MOD(CH*(A0-BE)+3600.,360.)
      ELSE
        AEND=A180
      END IF
      Z0=VTX1DT(3)
      BZ=CRHOP*PTRADT(3)/(PIDGDK*ABS(CH))
      RV=SQRT(VTX1DT(1)**2+VTX1DT(2)**2)
C
C
C
  101 FNVD=.TRUE.
      IF(RV.GE.RIIT) THEN
        DAITC=0.
      ELSE
        CALL DHELCI(XC,YC,RC,CH,A0,RIIT  ,AEND,DAITC   )
      END IF
      DAITC=MAX(DASV,DAITC)
      IF(RV.GE.RTMIN) THEN
        DATPC(1)=0.
      ELSE
        CALL DHELCI(XC,YC,RC,CH,A0,RTMIN ,AEND,DATPC(1))
      END IF
      DATPC(1)=MAX(DASV,DATPC(1))
      IF(NSVXDT.GT.0) THEN
        CALL DVKNV(NSVXDT,XYZ)
        CALL DHPERP(N3DIDT,FRFTDT,XYZ,ARCL,PART)
        DATPC(2)=PIDGDK*ARCL/RC
        DAECA(1)=DATPC(2)
        DAECA(2)=DATPC(2)
        FBAR(1)=.TRUE.
        FBAR(2)=.TRUE.
        RETURN
      END IF
      IF(RV.GE.RMAXDK.OR.ABS(Z0).GE.ZMAXDK) THEN
        DATPC(2)=0.
      ELSE
        CALL DHELCI(XC,YC,RC,CH,A0,RMAXDK,AEND,DATPC(2))
        IF(BZ.NE.0.) THEN
          DAZ=(ZMAXDK-Z0)/BZ
          IF(DAZ.LT.0.) THEN
            DAZ=(-ZMAXDK-Z0)/BZ
          END IF
          DATPC(2)=MIN(DATPC(2),DAZ)
        END IF
      END IF
      IF(ISTART.EQ.0) THEN
        ISTART=1
C          CALL DGTTPC(TTPIR,TTPOR,TTPOZ)
        CALL DGTECA(TBX,TBY,QTR,TECIZ,TECOZ)
        TBX(13)=TBX(1)
        TBY(13)=TBY(1)
        DO   700  K=1,12
          DX(K)=TBX(K+1)-TBX(K)
          DY(K)=TBY(K+1)-TBY(K)
          A(K) =TBX(K)*TBY(K+1)-TBY(K)*TBX(K+1)
          Q(K) =1./SQRT(DX(K)**2+DY(K)**2)
          B(K) =DATN2D(DX(K),DY(K))
  700   CONTINUE
      END IF
      QT=1.
      ZECA=TECIZ
      DO   710  L=1,2
        IF(RV.GE.RBECA(L).OR.ABS(Z0).GE.ZECA) THEN
          DAECA(L)=0.
        ELSE
          AMIN=AEND
          DO   720  K=1,12
            CC=(YC*DX(K)-XC*DY(K)+QT*A(K))*Q(K)/RC
            IF(ABS(CC).LE.1.) THEN
              DD=ACOSD(CC)
              A1=MOD(CH*(A0+B(K)+DD)+3600.,360.)
              A2=MOD(CH*(A0+B(K)-DD)+3600.,360.)
              AMIN=MIN(AMIN,A1,A2)
            END IF
  720     CONTINUE
          FBAR(L)=.TRUE.
          IF(BZ.NE.0.) THEN
            DAZ=(ZECA-Z0)/BZ
            IF(DAZ.LT.0.) THEN
              DAZ=(-ZECA-Z0)/BZ
            END IF
            IF(DAZ.LE.AMIN) THEN
              AMIN=DAZ
              FBAR(L)=.FALSE.
            END IF
          END IF
          DAECA(L)=AMIN
        END IF
        QT=QTR
        ZECA=TECOZ
  710 CONTINUE
      DAOLD=-999.
      IF(DATPC(1) .LT. 0) CALL DWRT('TPC1=0')
      IF(DATPC(2) .LT. 0) CALL DWRT('TPC2=0')
      IF(DAECA(1) .LT. 0) CALL DWRT('ECA1=0')
      IF(DAECA(2) .LT. 0) CALL DWRT('ECA2=0')
      IF(LWRT.GT.5) THEN
        CHRC=CH*RC
        WRITE(LWRT,1060) X0,Y0,Z0,A0,BZ,CHRC,DASVX,DATPC(2)
 1060   FORMAT(1X,5F9.3,F13.3,2F7.2)
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------- DHELWRT
CH
      ENTRY DHELWRT(JWRT)
CH
CH --------------------------------------------------------------------
CH
      DHELWRT=0
      LWRT=JWRT
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DHELX0
CH
      ENTRY DHELXV(ROVD,DAVDT)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
      DHELXV=0
      IF(MDEB.EQ.1) THEN
        ROVDM=0.
        DAVDT=0.
        FNVD=.FALSE.
        RETURN
      END IF
      IF(RV.GE.ROVD) THEN
        DAVDT=180.
      ELSE
        CALL DHELCI(XC,YC,RC,CH,A0,ROVD ,AEND,DAVDT)
      END IF
      ROVDM=ROVD
      FNVD=.FALSE.
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DHELVX
CH
      ENTRY DHELVX(XVTX,YVTX,XCC,YCC,RCC,FIVTX)
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
      DHELVX=0
      XCC=XC
      YCC=YC
      RCC=RC
      FIV=DATN2D(YVTX-YC,XVTX-XC)
      F00=DATN2D(    -YC,    -XC)
      FIVTX=CH*(F00-FIV)
      END
*DK DHFD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DHFD
CH
      SUBROUTINE DHFD
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
      INCLUDE 'DALI_EX.INC'
      DIMENSION HRU(10),HRB(4),VRB(4),H(2),V(2)
C    MDET+1=MDET: LUM,ITC,TPC TPC,EC1,EC2,EC3,HCAL,MUON  end
C                  1   2   3   4   5   6   7   8    9    10
      DATA M10/10/
      DATA DMUON/12./,DHRU/4./
      DIMENSION NSCA(0:12)
C     NSCA=1 IF WINDOW IS HORIZ. LARGE;  =2 IF VERTICAL LARGE;  =3 IF BOTH
C               W 1 2 3 4 5 6 U D L M R S
      DATA NSCA/3,0,0,0,0,0,0,1,1,2,2,2,3/
      CALL DQWIL(MOD(DFWIDU(IZOMDO),10.))
      HRU( 1)=HLOWDG(IAREDO)
      IF(BNUMDB(4,PLSDDB).LE.0..OR.BNUMDB(2,PLSDDB).LE.0.) THEN
        HRU(2)=HRU(1)
        NRG=3
      ELSE
        NRG=2
      END IF
      HRU(M10  )=HHGHDG(IAREDO)
      HRU(M10-1)=HRU(M10)-DMUON
      D=(HRU(M10-1)-HRU(1))/FLOAT(M10-NRG)
      DO 700 N=NRG,M10-2
        HRU(N)=HRU(N-1)+D
  700 CONTINUE
      CALL DQCL(IAREDO)
      CALL DQWIL(MOD(DFWIDU(IZOMDO),10.))
      CALL DHTINP
      CALL DCTYP0(1)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PDF,J_PMF'
      CALL DPARAM(10
     &  ,J_PFI,J_PDF,J_PMF)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(IZOMDO.EQ.0) THEN
        CALL DQLEVL(ICFRDD)
        CALL DQRER(0,0.,0.,1.,400.,HRB,VRB)
        CALL DQRU(HRB,VRB)
        CALL DQL2E(0.,360.,1.,360.)
        CALL DQL2E(0.,180.,1.,180.)
        FI=180.
      ELSE
        FI=PARADA(2,J_PFI)
      END IF
      CALL DDRFLO
      CALL DQAR0(1.,1.)
      IF(PARADA(2,J_PMF).EQ.0.) THEN
        IF(PDCODD(2,ISTYDD).GE.2.) THEN
C          CALL DQRER(0,HLOWDG(IAREDO),0.,HHGHDG(IAREDO),1.,HRB,VRB)
C          CALL DQRU(HRB,VRB)
          IF(NRG.EQ.2) CALL DHFAR(ICLUDD,HRU(1),HRU(2))
          CALL DHFAR(ICITDD,HRU(2),HRU(3))
          CALL DHFAR(ICTPDD,HRU(3),HRU(5))
          CALL DHFAR(ICECDD,HRU(5),HRU(8))
          CALL DHFAR(ICHCDD,HRU(8),HRU(9))
          CALL DHFAR(ICM1DD,HRU(9),HRU(10))
        END IF
        V(1)=VLOWDG(IAREDO)
        V(2)=VHGHDG(IAREDO)
        CALL DQLEVL(KCLGDD)
        DO 710 IH=2,9
          IF(IH.EQ.4) GO TO 710
          H(1)=HRU(IH)
          H(2)=HRU(IH)
          CALL DGDRAW(2,H,V)
  710   CONTINUE
        IF(NRG.EQ.2) CALL DHFLC('S',FI,HRU(1),HRU(2)-DHRU,NSCA(IAREDO))
        CALL DHFITC(FI,HRU(2),HRU(3),NSCA(IAREDO))
        CALL DHFTPC(FI,HRU(3),HRU(5),NSCA(IAREDO))
        CALL DHFEC0(FI,EMAX)
        EMAX=MAX(EMAX,1.)
        IF(EMAX.GT.0.) THEN
          CALL DHFEC(EMAX,1,1,HRU(5),HRU(6)-DHRU,NSCA(IAREDO))
          CALL DHFEC(EMAX,2,2,HRU(6),HRU(7)-DHRU,NSCA(IAREDO))
          CALL DHFEC(EMAX,3,3,HRU(7),HRU(8)-DHRU,NSCA(IAREDO))
        END IF
        CALL DHFHC(FI,HRU(8),HRU(9)-DHRU,EMAX,NSCA(IAREDO))
        CALL DHFPM(FI,HRU(9),HRU(10))
      ELSE
        IF(PDCODD(2,ISTYDD).GE.2.) THEN
          CALL DQRER(0,HLOWDG(IAREDO),0.,HHGHDG(IAREDO),1.,HRB,VRB)
          CALL DQRU(HRB,VRB)
          CALL DHFAR(ICLUDD,HLOWDG(IAREDO),HHGHDG(IAREDO))
        END IF
        CALL DHFLC('D',FI,HLOWDG(IAREDO),HHGHDG(IAREDO),NSCA(IAREDO))
      END IF
      CALL DHFRDH(0.,0.)
   99 CALL DCTYEX('HF',2)
      IF(IZOMDO.EQ.0) THEN
        V1=0.
        V2=400.
      ELSE
        DF=PARADA(2,J_PDF)*0.5
        V1=(FI-DF)
        V2=(FI+DF)
      END IF
      CALL DQSCA('V',V1,V2,' deg',4,'&f',2)
      CALL DQFR(IAREDO)
C                                       store parameters (pstods)
      CALL DPCSAR
      RETURN
      END
*DK DHFAR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DHFAR
CH
      SUBROUTINE DHFAR(IC,H1,H2)
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
      DIMENSION H(4),V(4)
      DATA V/0.,0.,1.,1./
      IF(PDCODD(4,IC).NE.1.) RETURN
C      CALL DQPO0('AREA',IFIX(PDCODD(2,IC)),0,' ')
      CALL DQLEVL(IC)
      H(1)=H1
      H(2)=H2
      H(3)=H2
      H(4)=H1
      V(1)=VLOWDG(IAREDO)
      V(2)=V(1)
      V(3)=VHGHDG(IAREDO)
      V(4)=V(3)
      CALL DGAREA(4,H,V)
      END
*DK DHFEC0
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DHFEC0
CH
      SUBROUTINE DHFEC0(FIMID,EMAX)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C_CG 11-May-1989   C.Grab  Adapted to CERNVM
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      INCLUDE 'DALI_EX.INC'
      DIMENSION HRB(4),VRB(4)
      DIMENSION HIST(386,3)
      DATA DV/8./
      CHARACTER *3 DT3
      LOGICAL FOUT,FCUT
      IF(FPIKDP) RETURN
      CALL DV0(ESDADB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
C      CALL DORCAL
C     ORDER TO EXCLUDE BAD HITS
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PDF,J_PTE'
      CALL DPARAM(11
     &  ,J_PDF,J_PTE)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      TEMID=PARADA(2,J_PTE)
      CALL VZERO(HIST,1152)
      DO 1 K=NUM1,NUM2
        CALL DCUTEC(K,FCUT)
        IF(FCUT) GO TO 1
        TE=DVEC(IVTEDV,K)
        IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 1
        FI=DFINXT(FIMID,DVEC(IVFIDV,K))
        IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 1
C         CALL DLCOLH(DVEC,MECADL,K,NCOL)
C         IF(NCOL.EQ.0) GO TO 1
        JJ=DVEC(IVJJDV,K)
        II=DVEC(IVIIDV,K)
        KK=DVEC(IVKKDV,K)
        E =DVEC(IVENDV,K)
        IF     (JJ.GE.51.AND.JJ.LE.178) THEN
C          BARREL : I=1,384
          HIST(II,KK)=HIST(II,KK)+E
        ELSE IF(JJ.GE.41.AND.JJ.LE.188) THEN
C          ENDCAP : I=1,384
          HIST(II,KK)=HIST(II,KK)+E
        ELSE IF(JJ.GE.25.AND.JJ.LE.204) THEN
C          ENDCAP : I=1,288
          M=MOD(II,3)
          IF(M.EQ.0) THEN
            EQ=E*0.25
            N=(4*II  )/3
            HIST(N-1,KK)=HIST(N-1,KK)+EQ
            HIST(N  ,KK)=HIST(N  ,KK)+E-EQ
          ELSE IF(M.EQ.1) THEN
            EQ=E*0.25
            N=(4*II-1)/3
            HIST(N+1,KK)=HIST(N+1,KK)+EQ
            HIST(N  ,KK)=HIST(N  ,KK)+E-EQ
          ELSE
            E=E*0.5
            N=(4*II-2)/3
            HIST(N+1,KK)=HIST(N+1,KK)+E
            HIST(N  ,KK)=HIST(N  ,KK)+E
          END IF
        ELSE IF(JJ.GE.9.AND.JJ.LE.220) THEN
C          ENDCAP : I=1,192
          E=E*0.5
          N=2*II
          HIST(N  ,KK)=HIST(N  ,KK)+E
          HIST(N-1,KK)=HIST(N-1,KK)+E
        ELSE
C          ENDCAP : I=1,96
          N2=4*II
          E=E*0.25
          DO 701 N=N2-3,N2
            HIST(N,KK)=HIST(N,KK)+E
  701     CONTINUE
        END IF
    1 CONTINUE
      EMAX=.01
      DO 3 KK=1,3
        DO 2 II=1,384
          IF(HIST(II,KK).GT.EMAX) EMAX=HIST(II,KK)
    2   CONTINUE
        HIST(385,KK)=HIST(1,KK)
        HIST(386,KK)=HIST(2,KK)
    3 CONTINUE
      IF(IZOMDO.EQ.0) THEN
        V1=0.
        V2=384.*(400./360.)
        V3=V2-DV
        V4=V3-DV
      ELSE
        Q=384./360.
        DF=PARADA(2,J_PDF)*0.5
        V1=Q*(FIMID-DF)
        V2=Q*(FIMID+DF)
        V4=0.
      END IF
      RETURN
CH
CH
CH
      ENTRY DHFEC(EMAX,K1,K2,HLOW,HHGH,NSCA)
CH
CH
CH
CH
      IF(FOUT) RETURN
      CALL DHFRDH(HLOW,HHGH)
      CALL DQRER(0,0.,V1,EMAX,V2,HRB,VRB)
      CALL DQRU(HRB,VRB)
      IF(NSCA.EQ.1.OR.NSCA.EQ.3)
     &  CALL DQSCA('H',0.,EMAX,' ',0,' ',0)
      IF(K1.EQ.K2) THEN
        CALL DQLEVL(LCEKDD(K1))
      ELSE
        CALL DQLEVL(LCECDD)
      END IF
      EMX=0.
      DO 702 I=3,386
        IF(HIST(I,K1).GT.0.) THEN
          IF(HIST(I,K1).GT.EMX) EMX=HIST(I,K1)
          VAR1=I-3
          CALL DQAR(0.,VAR1,HIST(I,K1),VAR1+1.)
        END IF
  702 CONTINUE
      DO 703 I=3,22
        IF(HIST(I,K1).GT.0.) THEN
          IF(HIST(I,K1).GT.EMX) EMX=HIST(I,K1)
          VAR1=I+381
          CALL DQAR(0.,VAR1,HIST(I,K1),VAR1+1.)
        END IF
  703 CONTINUE
      IF(IZOMDO.EQ.0) THEN
        IF(NSCA.GE.2) CALL DQTXT(0.,V3,'MAX=',4)
        IF(NSCA.EQ.1.OR.NSCA.EQ.3) THEN
          CALL DQTXT(0.,V4,DT3(EMX)//' GEV',7)
        ELSE
          CALL DQTXT(0.,V4,DT3(EMX),3)
        END IF
      ELSE
        CALL DQTXT(0.,V4,DT3(EMX),3)
      END IF
      END
*DK DHFHC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DHFHC
CH
      SUBROUTINE DHFHC(FIMID,HLOW,HHGH,EMX,NSCA)
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
      INCLUDE 'DALI_EX.INC'
      DIMENSION HRB(4),VRB(4)
      DIMENSION HIST(96)
      DATA DV/2./
      CHARACTER *3 DT3
      DATA NN/0/,MODE/0/
      LOGICAL FOUT
      IF(FPIKDP) RETURN
      CALL DV0(HSDADB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PDF,J_PTE'
      CALL DPARAM(11
     &  ,J_PDF,J_PTE)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      TEMID=PARADA(2,J_PTE)
      CALL VZERO(HIST,96)
      DO 1 K=NUM1,NUM2
        CALL DCUTHC(K,FOUT)
        IF(FOUT) GO TO 1
        TE=DVHC(IVTEDV,K)
        IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 1
        FI=DFINXT(FIMID,DVHC(IVFIDV,K))
        IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 1
C         CALL DLCOLH(DVHC,MHCADL,K,NCOL)
C         IF(NCOL.EQ.0) GO TO 1
        JJ=DVHC(IVJJDV,K)
        II=DVHC(IVIIDV,K)
        E =DVHC(IVENDV,K)
        IF     (NN.EQ.2) THEN
C          BARREL : I=1,96
          IF(MODE.GE.0.) HIST(II)=HIST(II)+E
        ELSE IF(JJ.GE.11.AND.JJ.LE.52) THEN
C          ENDCAP : I=1,96
          IF(MODE.LE.0.) HIST(II)=HIST(II)+E
        ELSE IF(JJ.GE. 5.AND.JJ.LE.58) THEN
C          ENDCAP : I=1,48
          IF(MODE.LE.0) THEN
            E=E*0.5
            N=2*II
            HIST(N  )=HIST(N  )+E
            HIST(N-1)=HIST(N-1)+E
          END IF
        ELSE
C          ENDCAP : I=1,24
          IF(MODE.LE.0) THEN
            N2=4*II
            E=E*0.25
            DO 701 N=N2-3,N2
              HIST(N)=HIST(N)+E
  701       CONTINUE
          END IF
        END IF
    1 CONTINUE
      EMAX=.01
      DO 2 II=1,96
        IF(HIST(II).GT.EMAX) EMAX=HIST(II)
    2 CONTINUE
CH
CH
CH
CH
      CALL DHFRDH(HLOW,HHGH)
      IF(IZOMDO.EQ.0) THEN
        V1=0.
        V2=96.*(400./360.)
        V3=V2-DV
        V4=V3-DV
      ELSE
        Q=96./360.
        DF=PARADA(2,J_PDF)*0.5
        V1=Q*(FIMID-DF)
        V2=Q*(FIMID+DF)
        V4=0.
      END IF
      EM=MAX(EMX,EMAX)
      CALL DQRER(0,0.,V1,EM,V2,HRB,VRB)
      CALL DQRU(HRB,VRB)
      IF(NSCA.EQ.1.OR.NSCA.EQ.3)
     &   CALL DQSCA('H',0.,EM,' ',0,' ',0)
      CALL DQLEVL(LCHCDD)
      DO 702 I=1,96
        IF(HIST(I).GT.0.) THEN
          VAR1=I-1
          VAR2=VAR1+1.
          CALL DQAR(0.,VAR1,HIST(I),VAR2)
        END IF
  702 CONTINUE
      DO 703 I=1,5
        IF(HIST(I).GT.0.) THEN
          VAR1=I+95
          VAR2=VAR1+1.
          CALL DQAR(0.,VAR1,HIST(I),VAR2)
        END IF
  703 CONTINUE
      CALL DQLEVL(ICTXDD)
      IF(IZOMDO.EQ.0) THEN
        IF(NSCA.GE.2) CALL DQTXT(0.,V3,'MAX=',4)
        IF(NSCA.EQ.1.OR.NSCA.EQ.3) THEN
          CALL DQTXT(0.,V4,DT3(EMAX)//' GEV',7)
        ELSE
          CALL DQTXT(0.,V4,DT3(EMAX),3)
        END IF
      ELSE
        CALL DQTXT(0.,V4,DT3(EMAX),3)
      END IF
      END
*DK DHFLC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DHFLC
CH
      SUBROUTINE DHFLC(TMOD,FIMID,HLOW,HHGH,NSCA)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
CC    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      INCLUDE 'DALI_EX.INC'
C      DATA EMX/1./
      DATA NBIN/72/
      DIMENSION HRB(4),VRB(4)
      DIMENSION HIST(72,2)
      DATA DV/2./
      DATA EM/51./
      CHARACTER *1 TMOD
      CHARACTER *3 DT3
      LOGICAL FOUT
      IF(FPIKDP) RETURN
      CALL DV0(PLSDDB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PDF,J_PTE'
      CALL DPARAM(11
     &  ,J_PDF,J_PTE)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      TEMID=PARADA(2,J_PTE)
      CALL VZERO(HIST,2*NBIN)
      QBIN=FLOAT(NBIN)/360.
      IS=1
      DO 1 K=NUM1,NUM2
        CALL DCUTLC(K,FOUT)
        IF(FOUT) GO TO 1
        TE=DVLC(IVTEDV,K)
        IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 1
        FI=DFINXT(FIMID,DVLC(IVFIDV,K))
        IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 1
        II=FI*QBIN
        IF(TMOD.EQ.'D') THEN
          IF(TE.LT.90.) THEN
            IS=1
          ELSE
            IS=2
          END IF
        END IF
        HIST(II,IS)=HIST(II,IS)+DVLC(IVENDV,K)
    1 CONTINUE
      EMAX=.01
      DO 2 II=1,NBIN
        IF(HIST(II,1).GT.EMAX) EMAX=HIST(II,1)
    2 CONTINUE
      IF(TMOD.EQ.'D') THEN
        DO 111 II=1,NBIN
          IF(HIST(II,2).GT.EMAX) EMAX=HIST(II,2)
  111   CONTINUE
      END IF
CH
CH
CH
CH
      CALL DHFRDH(HLOW,HHGH)
      IF(IZOMDO.EQ.0) THEN
        V1=0.
        V2=400.*QBIN
        V3=V2-DV
        V4=V3-DV
      ELSE
        DF=PARADA(2,J_PDF)*0.5
        V1=QBIN*(FIMID-DF)
        V2=QBIN*(FIMID+DF)
        V1=FIMID-DF
        V2=FIMID+DF
        V4=0.
      END IF
C      EM=MAX(EMX,EMAX)
      IF(TMOD.EQ.'S') THEN
        CALL DQRER(0,0.,V1,EM   ,V2,HRB,VRB)
      ELSE
        CALL DQRER(0,0.,V1,EM*2.,V2,HRB,VRB)
      END IF
      CALL DQRU(HRB,VRB)
      IF(TMOD.EQ.'S') THEN
        IF(NSCA.EQ.1.OR.NSCA.EQ.3)
     &    CALL DQSCA('H',0.,EM,' ',0,' ',0)
      ELSE
        CALL DQLEVL(KCLGDD)
        CALL DQL2E(EM,-360.,EM,720.)
        CALL DQSCA('H',0.,EM*2.,' ',0,' ',0)
      END IF
      CALL DQLEVL(LCLCDD)
      DO 702 I=1,NBIN
        IF(HIST(I,1).GT.0.) THEN
          VAR1=I-1
          VAR2=VAR1+1.
          CALL DQAR(0.,VAR1,HIST(I,1),VAR2)
        END IF
  702 CONTINUE
      I2=20.*QBIN
      DO 703 I=1,I2
        IF(HIST(I,1).GT.0.) THEN
          VAR1=I+NBIN
          VAR2=VAR1+1.
          CALL DQAR(0.,VAR1,HIST(I,1),VAR2)
        END IF
  703 CONTINUE
      IF(TMOD.EQ.'D') THEN
        HUP=2.*EM
        DO 802 I=1,NBIN
          IF(HIST(I,2).GT.0.) THEN
            VAR1=I-1
            VAR2=VAR1+1.
            CALL DQAR(HUP-HIST(I,2),VAR1,HUP,VAR2)
          END IF
  802   CONTINUE
        I2=20.*QBIN
        DO 803 I=1,I2
          IF(HIST(I,2).GT.0.) THEN
            VAR1=I+NBIN
            VAR2=VAR1+1.
            CALL DQAR(HUP-HIST(I,2),VAR1,HUP,VAR2)
          END IF
  803   CONTINUE
      END IF
      CALL DQLEVL(ICTXDD)
      IF(TMOD.EQ.'S') THEN
        CALL DQTXT(0.,V4,DT3(EMAX)//' GEV',7)
      ELSE IF(IZOMDO.EQ.0) THEN
        IF(NSCA.GE.2) CALL DQTXT(0.,V3,'MAX=',4)
        IF(NSCA.EQ.1.OR.NSCA.EQ.3) THEN
          CALL DQTXT(0.,V4,DT3(EMAX)//' GEV',7)
        ELSE
          CALL DQTXT(0.,V4,DT3(EMAX),3)
        END IF
      ELSE
        CALL DQTXT(0.,V4,DT3(EMAX),3)
      END IF
      END
*DK DHFITC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DHFITC
CH
      SUBROUTINE DHFITC(FIMID,HLOW,HHGH,NSCA)
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
      DIMENSION HRB(4),VRB(4)
      DATA HMAX/30.1/,HMIN/10./
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PDF'
      CALL DPARAM(11
     &  ,J_PDF)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(IZOMDO.EQ.0) THEN
        V1=0.
        V2=400.
      ELSE
        DF=PARADA(2,J_PDF)*0.5
        V1=(FIMID-DF)
        V2=(FIMID+DF)
      END IF
      CALL DHFRDH(HLOW,HHGH)
      CALL DQRER(0,HMIN,V1,HMAX,V2,HRB,VRB)
      CALL DQRU(HRB,VRB)
      IF(NSCA.EQ.1.OR.NSCA.EQ.3)
     &  CALL DQSCA('H',0.,HMAX-HMIN, 'cm' ,2,'&r',2)
      CALL DYF_IT_HI
C     CALL DHFPI(FIMID)
      RETURN
      END
*DK DHFPI
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DHFPI
CH
      SUBROUTINE DHFPI(FIMID)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C_CG 11-May-1989   C.Grab  Adapted to CERNVM + Bug-fix in loops
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      EQUIVALENCE (KPIKDP,K)
      LOGICAL FOUT
C     ............................ ONLY RESIDUALS ARE DRAWN BUT NOT OTHER HITS
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_STC'
      CALL DPARAM(31
     &  ,J_STC)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(FGRYDR.AND.PARADA(4,J_STC).LT.0.) RETURN
      CALL DV0(ITCODB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
      CALL=DVITST(IVNTDV,IVNT,IDUM)
      CALL DSCIT(IVNT)
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
      DO 2 K=NUM1,NUM2
        IF(FCUHDT) THEN
          NTRIT=DVIT(IVNTDV,K)
          IF(FNOHDT(NTRIT)) GO TO 2
        END IF
        T1=DVIT(IVTEDV,K,1)
        T2=DVIT(IVTEDV,K,2)
        IF( (T1.LT.TC1.AND.T2.LT.TC1).OR.
     &    (T1.GT.TC2.AND.T2.GT.TC2) ) GO TO 2
        F1=DVIT(IVFIDV,K,1)
        F2=DFINXT(F1,DVIT(IVFIDV,K,2))
        FID=F2-F1
        F1=DFINXT(FIMID,F1)
        F2=F1+FID
        IF( (F1.LT.FC1.AND.F2.LT.FC1).OR.
     &      (F1.GT.FC2.AND.F2.GT.FC2) ) GO TO 2
        IF(FVITDC) CALL DGLEVL(NCITDC(K))
        H1=DVIT(IVRODV,K,1)
        H2=DVIT(IVRODV,K,2)
        CALL DQL2E(H1,F1,H2,F2)
        IF(IZOMDO.EQ.0) CALL DQL2E(H1,F1+360.,H2,F2+360.)
    2 CONTINUE
      END
*DK DHFPM
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DHFPM
CH
      SUBROUTINE DHFPM(FIMID,HLOW,HHGH)
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
      DIMENSION HRB(4),VRB(4)
      EXTERNAL DVMD
      EQUIVALENCE (KPIKDP,K)
      LOGICAL FOUT
      DATA HMIN,HMAX/.5,3.5/
      CALL DV0(MHITDB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
      CALL DHFRDH(HLOW,HHGH)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PDF'
      CALL DPARAM(11
     &  ,J_PDF)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(IZOMDO.EQ.0) THEN
        V1=0.
        V2=400.
      ELSE
        DF=PARADA(2,J_PDF)*0.5
        V1=(FIMID-DF)
        V2=(FIMID+DF)
      END IF
      CALL DQRER(0,HMIN,V1,HMAX,V2,HRB,VRB)
      CALL DQRU(HRB,VRB)
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
C//   CALL DQPD0(8,12.,0.)
      CALL DPAR_SET_SY(65,40.)
      IF(PDCODD(4,ICCNDD).EQ.1.) THEN
        CALL DQLEVL(ICCNDD)
      ELSE
        CALL DQLEVL(LCMUDD(1))
      END IF
      DO 1 K=NUM1,NUM2
        TE=DVMD(IVTEDV,K)
        IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 1
        FI=DFINXT(FIMID,DVMD(IVFIDV,K))
        IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 1
        H=DVMD(IVKKDV,K)
        IF(FPIKDP) THEN
          CALL DQPIF(H,FI)
        ELSE
          CALL DQPD(H,FI)
          IF(IZOMDO.EQ.0) CALL DQPD(H,FI+360.)
        END IF
    1 CONTINUE
      END
*DK DHFTPC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DHFTPC
CH
      SUBROUTINE DHFTPC(FIMID,HLOW,HHGH,NSCA)
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
      INCLUDE 'DALI_EX.INC'
      DIMENSION HRB(4),VRB(4)
      DATA HMAX/225./,HMIN/-10./
      CALL DHFRDH(HLOW,HHGH)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PDF'
      CALL DPARAM(11
     &  ,J_PDF)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(IZOMDO.EQ.0) THEN
        V1=0.
        V2=400.
      ELSE
        DF=PARADA(2,J_PDF)*0.5
        V1=(FIMID-DF)
        V2=(FIMID+DF)
      END IF
      CALL DQRER(0,HMAX,V1,HMIN,V2,HRB,VRB)
      CALL DQRU(HRB,VRB)
      IF(NSCA.EQ.1.OR.NSCA.EQ.3)
     &  CALL DQSCA('H',-HMAX,-HMIN,'cm' ,2,'Rout',4)
      MOHT=MOHTDC
      MOHTDC=1
      IF(IHTRDO(6).LE.1) THEN
        CALL DAPPT('HF',DVTP,TPCODB,0)
      ELSE IF(IHTRDO(6).EQ.2) THEN
        CALL DAPPT('HF',DVPADA,TPADDB,0)
      ELSE IF(IHTRDO(6).EQ.3) THEN
        CALL DAPPT('HF',DVTP,TBCODB,0)
      END IF
      MOHTDC=MOHT
      END
*DK DHFRDH
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DHFRDH
CH
      SUBROUTINE DHFRDH(H1,H2)
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
C     ------------------------------------
C
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      LOGICAL FSET
      DATA FSET /.FALSE./
      IF(FSET) THEN
        HLOWDG(IAREDO)=HLOW
        HHGHDG(IAREDO)=HHGH
      END IF
      IF(H2.EQ.0.) THEN
        FSET=.FALSE.
      ELSE
        FSET=.TRUE.
        HLOW=HLOWDG(IAREDO)
        HHGH=HHGHDG(IAREDO)
        HLOWDG(IAREDO)=H1
        HHGHDG(IAREDO)=H2
      END IF
      END
*DK DHTINP
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DHTINP
CH
      SUBROUTINE DHTINP
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
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_HIN'
      CALL DPARAM(20
     &  ,J_HIN)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IN=PARADA(2,J_HIN)
      DO   700  K=1,7
        IHTRDO(K)=MOD(IN,10)
        IN=IN/10
  700 CONTINUE
      CALL DCUTTR
      FZTRDT=.FALSE.
      END
*DK DHTMOD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DHTMOD
CH
      SUBROUTINE DHTMOD(TA,FYES)
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
      PARAMETER (MCOM=6)
      CHARACTER *2 TA,THT(MCOM,7)
      DATA THT/
     &  'HI','HT','TR','MO','++','RM',
     &  'CH','JU','MC','KI','KO','KH',
     &  '  ','  ','  ','  ','  ',' ',
     &  '  ','  ','  ','  ','  ',' ',
     &  'AC','UC','TC','FC','UF',' ',
     &  'CD','PA','BC','  ','  ',' ',
     &  'LL','SL','  ','  ','  ',' '/
      LOGICAL FYES
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_HIN'
      CALL DPARAM(20
     &  ,J_HIN)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IN=PARADA(2,J_HIN)
      CALL DHTINP
      DO   700  N=1,7
        DO   710  K=1,MCOM
          IF(THT(K,N).EQ.'  ') GO TO 700
          IF(TA.EQ.THT(K,N)) THEN
            IHTRDO(N)=K
            GO TO 90
          END IF
  710   CONTINUE
  700 CONTINUE
      FYES=.FALSE.
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DHTMO1
CH
      ENTRY DHTMO1(NH,TA,FYES)
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
      DO   720  K=1,MCOM
        IF(TA.EQ.THT(K,NH)) THEN
          CALL DHTINP
          IHTRDO(NH)=K
          GO TO 90
        END IF
  720 CONTINUE
      FYES=.FALSE.
      RETURN
   90 FYES=.TRUE.
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DHTMO2
CH
      ENTRY DHTMO2
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
      PA=IHTRDO(1)
      F=1.
      DO   730  K=2,7
        F=F*10.
        PA=PA+F*IHTRDO(K)
  730 CONTINUE
      CALL DPARSV(20,'HIN',2,PA)
      END
*DK DHZD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DHZD
CH
      SUBROUTINE DHZD
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
      INCLUDE 'DALI_EX.INC'
      DATA ZTPCM/220./,EBIR/184.7/,QEC/0.2/
      DATA AMIN/1./
      CHARACTER *5 TH
      DIMENSION HRB(4),VRB(4)
      DATA DLHST/1./
C     DEFINITION PARADO(IPL1DO) from DDRCA0
C 0   &  'VX',
C     &  'V0','V1',
C     &  'I0','I1',
C     &  'T0','T1','T2','T3',
C     &  'E0','E1','E2','E3',
C     &  'H0','H1','H2',
C     &  'M0','M1','M2',
C 19  &  'oo'/
C      L=       5   6   7    8    9   10   11    12   13
C               |  TPC       |    | ECAL         | HCAL |
C      L1=MAX(IFIX(PARADO(2,IPL1DO),5)
C      L1=MIN(12,L1)
C      L2=MAX(IFIX(PARADO(2,IPL2DO),10,L1+1)
C      L2=MIN(13,L2)
      CALL DQCL(IAREDO)
      CALL DQWIL(MOD(DFWIDU(IZOMDO),10.))
      CALL DDRFLO
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PTE,J_PTO,J_PPR'
      CALL DPARAM(10
     &  ,J_PTE,J_PTO,J_PPR)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      PR=MAX(1.,PARADA(2,J_PPR))
      ZTO=ZTPCM*100./PR
      CALL DQRER(0,-ZTO,-ZTO,ZTO,ZTO,HRB,VRB)
      TH='Z   '
      CALL DQRU(HRB,VRB)
      CALL DQSC0('2 SC')
      CALL DQINV(IAREDO,0.,0.,H0,V0)
      CALL DQINV(IAREDO,AMIN,AMIN,H1,V1)
      HMIN=ABS(H1-H0)
      VMIN=ABS(V1-V0)
      IF(FPIKDP) THEN
        DO 700 L=1,3
          CALL DHZPIE(L)
  700   CONTINUE
        RETURN
      END IF
      CALL DQSC0('2 SC')
      CALL DQLEVL(ICTPDD)
      CALL DQL2E(-1000.,0.,-110.,0.)
      CALL DQL2E(  110.,0.,1000.,0.)
      RPOSDT=ZTO
      CALL DHTINP
      CALL DCTYP0(1)
      IF(IHTRDO(1).EQ.0) THEN
        CALL DRZET1(SP)
        RETURN
      END IF
      PARADA(4,J_PTE)=1.
      CALL DODITC('SRZ')
      CALL DODTPC('SRZ')
      CALL DRZPI(SP)
      CALL DRZTPC(SP)
      IF(BNUMDB(2,PYERDB).NE.0.) CALL DRZVRT
      IF(FPIKDP) RETURN
      CALL DQMID
      RCOR=EBIR
      DD=QEC*(ZTO-ZTPCM)
      IF(PDCODD(2,ISTYDD).GE.2.) THEN
        CALL DHZAEC(ZTO,RCOR)
        CALL DHZAHC(ZTO,DD)
      END IF
C     DD=QEC*(ZTO-ZTPCM)
      DLINDD=DLHST
      CALL DHZPH(.TRUE.,ZTO ,DD,HMIN,VMIN,0,3)
      CALL DHZPH(.TRUE.,ZTO ,DD,HMIN,VMIN,2,3)
      CALL DHZPE(.TRUE.,RCOR,DD,HMIN,VMIN,3)
      COLA=PDCODD(2,LCHCDD)
      PDCODD(2,LCHCDD)=PDCODD(2,LCHLDD)
      CALL DHZPH(.TRUE.,ZTO ,DD,HMIN,VMIN,1,3)
      PDCODD(2,LCHCDD)=COLA
      CALL DAPTRF('RZ')
      DLINDD=PDCODD(2,LITRDD)
      CALL DCTYEX('HZ',2)
      CALL DQSCA('H',HRB(1),HRB(2),'cm',2,TH,5)
      CALL DQSCA('V',VRB(2),VRB(3),'cm',2,'&r   ',5)
      IF(PARADA(2,J_PTO).LT.1005.) THEN
        CALL DLSITX(MTPCDL)
      ELSE IF(PARADA(2,J_PTO).LT.1008.) THEN
        CALL DLSITX(MECADL)
      ELSE
        CALL DLSITX(MHCADL)
      END IF
   99 CALL DQFR(IAREDO)
      CALL DPCSAR
      END
*DK DHZAEC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DHZAEC
CH
      SUBROUTINE DHZAEC(ZTO,RCOR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      CALL DOFISH('EC','HZ')
      CALL=DVEC1(1,51,1)
      TE=DVEC(IVTEDV,-1)
      DT=DVEC(IVDTDV,-1)
      ZCOR=RCOR/TAND(TE-DT)
      CALL=DVEC1(1,1,1)
      TE=DVEC(IVTEDV,-1)
      DT=DVEC(IVDTDV,-1)
      RO1=ZCOR*TAND(TE-DT)
      CALL DHZAR(ICECDD,RO1,RCOR,ZCOR,ZTO)
      END
*DK DHZAHC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DHZAHC
CH
      SUBROUTINE DHZAHC(ZTO,DD)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modifications:
C_CG     9-May-1989 C.Grab  Adapted to CERNVM
C
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      DATA QCOR/1.25/
      CALL DVHC0(NUM)
      IF(NUM.LE.0) RETURN
      CALL DOFISH('HC','HZ')
      ZCOR=ZTO-QCOR*DD
      CALL HUSRAN(15,1,1,TE,FI,DT,DF)
      RCOR=ZCOR*TAN(TE-0.5*DT)
      CALL HUSRAN( 1,1,1,TE,FI,DT,DF)
      RO1 =ZCOR*TAN(TE-0.5*DT)
      CALL DHZAR(ICHCDD,RO1,RCOR,ZCOR,ZTO)
      END
*DK DHZAR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DHZAR
CH
      SUBROUTINE DHZAR(IC,R1,R2,ZZ,SPIC)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modifications:
C
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION H(8),V(8)
      CALL DQPO0('AREA',IFIX(PDCODD(2,IC)),0,' ')
      Q=SPIC/ZZ
      DO 700 S=-1.,1.,2.
        H(1)=-ZZ
        V(1)=S*R1
        H(2)=-ZZ
        V(2)=R2*S
        H(3)= ZZ
        V(3)=V(2)
        H(4)= ZZ
        V(4)=V(1)
        H(5)=SPIC
        V(5)=V(4)*Q
        H(6)=SPIC
        V(6)=SPIC*S
        H(7)=-SPIC
        V(7)=V(6)
        H(8)=-H(5)
        V(8)= V(5)
        CALL DQPOL(8,H,V)
  700 CONTINUE
      END
*DK DHZPE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DHZPE
CH
      SUBROUTINE DHZPE(FSR,RCOR,DD,HMIN,VMIN,K3)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modifications:
C_CG     9-May-1989 C.Grab  Adapted to CERNVM
C
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C As the endcap is not circular, rho of the outer range of endcap stories
C varies by +-4 cm. A story is 3.6 cm wide. A histogram in rho-bins, is
C different from one in J-bins by at most 1 bin, but only for end cap stories
C in the outer half of the endcap near to the center or the edge of a sector.
C The interpretation of a histogram in J-bins is more straight forward and it
C is faster to draw.
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      INCLUDE 'DALI_EX.INC'
C      DATA JEA1,JEA2,JB1,JBM,JB2,JEB1,JEB2
C     &    /  1,  45,  51,114,178, 184, 228/
      LOGICAL FOUT,FSR
      DIMENSION HIST(228,2,3),HIS(1368)
      EQUIVALENCE (HIS,HIST)
      DATA N3/0/
      CHARACTER *3 T3,DT3
      CHARACTER *2 DT2
      CHARACTER *4 DT4
      CHARACTER *49 T
      CHARACTER  *1 TABOV(2),T1(3)
      DATA TABOV/'+','-'/,T1/'1','2','3'/
      IF(FPIMDP) RETURN
      CALL DV0(ESDADB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_PSE'
      CALL DPARAM(10
     &  ,J_PFI,J_PTE,J_PSE)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      FIMID=PARADA(2,J_PFI)
      TEMID=PARADA(2,J_PTE)
      IF(FSR) THEN
        FMIN=FIMID-90.
        FMAX=FIMID+90.
      ELSE
        FMIN=-999.
        FMAX= 999.
      END IF
      CALL VZERO(HIST,K3*456)
      DO 1 K=NUM1,NUM2
        CALL DCUTEC(K,FOUT)
        IF(FOUT) GO TO 1
        TE=DVEC(IVTEDV,K)
        IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 1
        FI=DFINXT(FIMID,DVEC(IVFIDV,K))
        IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 1
        JJ=DVEC(IVJJDV,K)
        IF(K3.EQ.1) THEN
          KK=1
        ELSE
          KK=DVEC(IVKKDV,K)
        END IF
        EN=DVEC(IVENDV,K)
        IF(FI.GE.FMIN.AND.FI.LE.FMAX) THEN
          HIST(JJ,1,KK)=HIST(JJ,1,KK)+EN
        ELSE
          HIST(JJ,2,KK)=HIST(JJ,2,KK)+EN
        END IF
    1 CONTINUE
      DO 7 NS=1,2
        DO 5 KK=1,K3
          DO 3 JJ=46,50
            HIST(JJ+5,NS,KK)=HIST(JJ+5,NS,KK)+HIST(JJ,NS,KK)
    3     CONTINUE
          DO 4 JJ=179,183
            HIST(JJ-5,NS,KK)=HIST(JJ-5,NS,KK)+HIST(JJ,NS,KK)
    4     CONTINUE
    5   CONTINUE
    7 CONTINUE
      IF(PARADA(4,J_PSE).EQ.-1.) THEN
        HIMAX=-999.
        DO 2 K=1,K3*456
          HIMAX=MAX(HIMAX,HIS(K))
    2   CONTINUE
        HIMAX=HIMAX/SHECDU
        PARADA(2,J_PSE)=HIMAX
      END IF
      QE=DD/PARADA(2,J_PSE)
      IF(K3.EQ.3) THEN
        ICGL=KCLGDD
C       LCEK=LCEKDD(KL)
        LCEL=LCEK
      ELSE
        ICGL=LCELDD
        LCEK=LCECDD
        LCEL=LCELDD
      END IF
      IF((PDCODD(2,ISTYDD).GE.2.).AND.(.NOT.FPIKDP)) THEN
        DO 700 KL=K3,1,-1
          IF(K3.EQ.3) LCEK=LCEKDD(KL)
          R=RCOR+(KL-1)*DD
          CALL DHZBEC( R  ,QE,HIST,1,KL,VMIN,ZCOR,ICGL,LCEK,2)
          CALL DHZEEC(ZCOR,QE,HIST,1,KL,HMIN,     ICGL,LCEK,2)
          IF(FSR) THEN
            CALL DHZBEC(-R  ,QE,HIST,2,KL,VMIN,ZCOR,ICGL,LCEK,2)
            CALL DHZEEC(ZCOR,QE,HIST,2,KL,HMIN,     ICGL,LCEK,2)
          END IF
  700   CONTINUE
      END IF
      PES=PDCODD(2,LCESDD)
      IF(PDCODD(2,ISTYDD).GE.2.) THEN
        IF(SQRT(AHSCDQ*AHSCDQ+BHSCDQ*BHSCDQ).LE.PES.AND.
     &     SQRT(AVSCDQ*AVSCDQ+BVSCDQ*BVSCDQ).LE.PES)  GO TO 9
      ELSE
        LCEL=LCECDD
      END IF
      DLINDD=PDCODD(2,LIGLDD)
      DO 701 KL=1,K3
        R=RCOR+(KL-1)*DD
        CALL DHZBEC( R  ,QE,HIST,1,KL,VMIN,ZCOR,ICGL,LCEL,1)
        CALL DHZEEC(ZCOR,QE,HIST,1,KL,HMIN,     ICGL,LCEL,1)
        IF(FSR) THEN
          CALL DHZBEC(-R  ,QE,HIST,2,KL,VMIN,ZCOR,ICGL,LCEL,1)
          CALL DHZEEC(ZCOR,QE,HIST,2,KL,HMIN,     ICGL,LCEL,1)
        END IF
  701 CONTINUE
    9 DLINDD=PDCODD(2,LITRDD)
      IF(.NOT.FPIKDP) THEN
        CALL DQLEVL(ICTXDD)
        T3=DT3(DD/QE)
        CALL DQSCE(T3//'Gev EC',9,DD)
        SCALDS(IAREDO,MSECDS)=QE
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DHZPEP
CH
      ENTRY DHZPEP(NTOWR)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:  CALLED BY PICKING: TYPE HIST(NTOWR,N...), N=1 UP,N=2 DOWN
C    Inputs    :$ OF TOWER
C    Outputs   :NO
C
C    Called by :
C ---------------------------------------------------------------------
C     MDLRDP=2000+NRP*10+KLP
      NRP=(MDLRDP-2000)/10
      KLP=MOD(MDLPDP,10)
C        123456789 123456789 123456789 123456789 123456789
      T='RZ:EC:Energy of tower +123=12.4  Sum(12)=12.4 GeV'
      IF(N3.EQ.3) T( 5: 5)=T1(KLP)
      T(23:23)=TABOV(NRP)
      T(24:26)=DT3(FLOAT(NTOWR))
      T(28:31)=DT4(HIST(NTOWR,NRP,KLP))
      STW=0.
      SUM=0.
      DO LP=NTOWR,228
        IF(HIST(LP,NRP,KLP).EQ.0.) GO TO 101
        SUM=SUM+HIST(LP,NRP,KLP)
        STW=STW+1.
      END DO
  101 DO LP=NTOWR-1,1,-1
        IF(HIST(LP,NRP,KLP).EQ.0.) GO TO 102
        SUM=SUM+HIST(LP,NRP,KLP)
        STW=STW+1.
      END DO
  102 T(38:39)=DT2(STW)
      T(42:45)=DT4(SUM)
      CALL DWRT(T)
      END
*DK DHZBEC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DHZBEC
CH
      SUBROUTINE DHZBEC(R1,QE,HIST,NR,KL,DMIN,ZCOR,ICGL,LCEK,LORA)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modifications:
C_CG     9-May-1989 C.Grab  Adapted to CERNVM
C
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      DIMENSION HIST(228,2,3),H(4),V(4)
      DIMENSION ZZ(51:179),RR(51:178)
      LOGICAL FSTRT
      DATA FSTRT/.TRUE./
      EQUIVALENCE (J,KPIKDP)
      IF(FSTRT) THEN
        FSTRT=.FALSE.
        DO 1 J=51,178
C         FOR RHO=1
          CALL=DVEC1(1,J,1)
          TE=DVEC(IVTEDV,-1)
          DT=DVEC(IVDTDV,-1)
          ZZ(J)=1./TAND(TE-DT)
          RR(J)=1./SIND(TE)
    1   CONTINUE
        ZZ(179)=1./TAND(TE+DT)
      END IF
      RB=ABS(R1)
      ZCOR=ZZ(51)*RB
      IF(FPIKDP) THEN
        MDLRDP=2000+NR*10+KL
      ELSE
        CALL DQLEVL(ICGL)
        CALL DQL2E(ZCOR,R1,-ZCOR,R1)
        CALL DQRA0(LORA,LCEK)
      END IF
      DO 2 J=51,178
        IF(HIST(J,NR,KL).EQ.0.) GO TO 2
        H(1)=ZZ(J  )*RB
        H(2)=ZZ(J+1)*RB
        DH=0.5*(H(2)-H(1))
        HM=0.5*(H(2)+H(1))
        R0  =RR(J)  *RB
        Q=(MAX(DMIN,QE*HIST(J,NR,KL))+R0)/R0
C        H(3)=H(2)*Q
C        H(4)=H(1)*Q
        HM=HM*Q
        H(3)=HM+DH
        H(4)=HM-DH
        V(1)=R1
        V(2)=R1
        V(3)=V(2)*Q
        V(4)=V(3)
        IF(FPIKDP) THEN
          CALL DQPIK(0.5*(H(1)+H(2)),0.5*(V(1)+V(2)))
        ELSE
          CALL DQRAR(H,V)
        END IF
    2 CONTINUE
      END
*DK DHZEEC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DHZEEC
CH
      SUBROUTINE DHZEEC(Z1,QE,HIST,NR,KL,DMIN,ICGL,LCEK,LORA)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modifications:
C_CG     9-May-1989 C.Grab  Adapted to CERNVM
C
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      DIMENSION HIST(228,2,3),H(4),V(4)
      DIMENSION RO(46),RR(46)
      EQUIVALENCE (J,KPIKDP)
      LOGICAL FSTRT
      DATA FSTRT/.TRUE./
      IF(FSTRT) THEN
        FSTRT=.FALSE.
        DO 1 J=1,46
          CALL=DVEC1(1,J,1)
C         FOR Z=1
          TE=DVEC(IVTEDV,-1)
          DT=DVEC(IVDTDV,-1)
          RO(J)=TAND(TE-DT)
          RR(J)=1./COSD(TE)
    1   CONTINUE
      END IF
      IF(NR.EQ.1) THEN
        ZS= Z1
      ELSE
        ZS=-Z1
      END IF
      IF(FPIKDP) THEN
        MDLRDP=2000+NR*10+KL
      ELSE
        CALL DQLEVL(ICGL)
        CALL DQL2E( Z1,RO(1)*ZS, Z1,RO(46)*ZS)
        CALL DQL2E(-Z1,RO(1)*ZS,-Z1,RO(46)*ZS)
        CALL DQRA0(LORA,LCEK)
      END IF
      DO 2 J=1,45
        IF(HIST(J,NR,KL).EQ.0.) GO TO 2
        V(1)=RO(J)  *ZS
        V(2)=RO(J+1)*ZS
        DV=0.5*(V(2)-V(1))
        VM=0.5*(V(2)+V(1))
        R0  =RR(J)  *Z1
        Q=(MAX(DMIN,QE*HIST(J,NR,KL))+R0)/R0
C        V(3)=V(2)*Q
C        V(4)=V(1)*Q
        VM=VM*Q
        V(3)=VM+DV
        V(4)=VM-DV
        H(1)=Z1
        H(2)=Z1
        H(3)=H(2)*Q
        H(4)=H(3)
        IF(FPIKDP) THEN
          CALL DQPIK(0.5*(H(1)+H(2)),0.5*(V(1)+V(2)))
        ELSE
          CALL DQRAR(H,V)
        END IF
    2 CONTINUE
      J=229
      DO 3 N=1,45
        J=J-1
        IF(HIST(J,NR,KL).EQ.0.) GO TO 3
        V(1)=RO(N)  *ZS
        V(2)=RO(N+1)*ZS
        R0  =RR(N)  *Z1
        Q=(MAX(DMIN,QE*HIST(J,NR,KL))+R0)/R0
        V(3)=V(2)*Q
        V(4)=V(1)*Q
        H(1)=-Z1
        H(2)=-Z1
        H(3)=H(2)*Q
        H(4)=H(3)
        IF(FPIKDP) THEN
          CALL DQPIK(0.5*(H(1)+H(2)),0.5*(V(1)+V(2)))
        ELSE
          CALL DQRAR(H,V)
        END IF
    3 CONTINUE
      END
*DK DHZPH
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DHZPH
CH
      SUBROUTINE DHZPH(FSR,ZTO,DD,HMIN,VMIN,MODE,K3)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modifications:
C_CG     9-May-1989 C.Grab  Adapted to CERNVM
C
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      INCLUDE 'DALI_EX.INC'
C      DATA JEA1,JEA2,JB1,JBM,JB2,JEB1,JEB2
C     &    /  1,  45,  51,114,178, 184, 228/
      LOGICAL FOUT,FSR
      DIMENSION HIST(63,2)
      DATA QCOR/1.25/,N3/0/,KLP/0/
      CHARACTER *3 T3,DT3
      CHARACTER *2 DT2
      CHARACTER *4 DT4
      CHARACTER *49 T
      CHARACTER  *1 TABOV(2),T1(3)
      DATA TABOV/'+','-'/,T1/'1','2','3'/
      IF(FPIMDP) RETURN
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_PSH'
      CALL DPARAM(10
     &  ,J_PFI,J_PTE,J_PSH)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(MODE.EQ.0) THEN
        CALL DV0(HSDADB,NUM1,NUM2,FOUT)
        IF(FOUT) RETURN
        CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
        FIMID=PARADA(2,J_PFI)
        TEMID=PARADA(2,J_PTE)
        IF(FSR) THEN
          FMIN=FIMID-90.
          FMAX=FIMID+90.
        ELSE
          FMIN=-999.
          FMAX= 999.
        END IF
        CALL VZERO(HIST,126)
        EMAX=-999.
        DO 1 K=NUM1,NUM2
          CALL DCUTHC(K,FOUT)
          IF(FOUT) GO TO 1
          TE=DVHC(IVTEDV,K)
          IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 1
          FI=DFINXT(FIMID,DVHC(IVFIDV,K))
          IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 1
          JJ=DVHC(IVJJDV,K)
          EN=DVHC(IVENDV,K)
          EMAX=MAX(EMAX,EN)
          IF(FI.GE.FMIN.AND.FI.LE.FMAX) THEN
            HIST(JJ,1)=HIST(JJ,1)+EN
          ELSE
            HIST(JJ,2)=HIST(JJ,2)+EN
          END IF
    1   CONTINUE
        IF(PARADA(4,J_PSH).EQ.-1.) THEN
          EMAX=EMAX/SHHCDU
          PARADA(2,J_PSH)=EMAX
        END IF
        IF(EMAX.LE.0.) FOUT=.TRUE.
        RETURN
      END IF
      IF(FOUT) RETURN
      IF(.NOT.FPIKDP) THEN
        IF(MODE.EQ.1) THEN
          IF(PDCODD(2,LCHLDD).EQ.0.) RETURN
          CALL DQRA0(MODE,LCHLDD)
        ELSE
          IF(PDCODD(2,LCHCDD).LE.0..OR.PDCODD(2,ISTYDD).LE.1.) RETURN
          CALL DQRA0(MODE,LCHCDD)
        END IF
      END IF
      QE=DD/PARADA(2,J_PSH)
      ZCOR=ZTO-QCOR*DD
      DLINDD=PDCODD(2,LIGLDD)
      CALL DHZEHC( ZCOR,QE,HIST,1,HMIN,RCOR,K3)
      CALL DHZBHC( RCOR,QE,HIST,1,VMIN,K3)
      IF(FSR) THEN
        CALL DHZEHC( ZCOR,QE,HIST,2,HMIN,RCOR,K3)
        CALL DHZBHC(-RCOR,QE,HIST,2,VMIN,K3)
      END IF
      DLINDD=PDCODD(2,LITRDD)
      IF(FPIKDP) RETURN
      IF(MODE.EQ.2) THEN
        T3=DT3(DD/QE)
        CALL DQSCE(T3//'Gev HC',9,DD)
      END IF
      SCALDS(IAREDO,MSHCDS)=QE
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DHZPHP
CH
      ENTRY DHZPHP(NTOWR)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:  CALLED BY PICKING: TYPE HIST(NTOWR,N), N=1 UP,N=2 DOWN
C    Inputs    :$ OF TOWER
C    Outputs   :NO
C
C    Called by :
C ---------------------------------------------------------------------
C        123456789 123456789 123456789 123456789 123456789
      T='RZ:HC:Energy of tower +123=12.4  Sum(12)=12.4 GeV'
      IF(N3.EQ.3) T( 5: 5)=T1(KLP)
      NUD=MDLPDP-2000
      T(23:23)=TABOV(NUD)
      T(24:26)=DT3(FLOAT(NTOWR))
      T(28:31)=DT4(HIST(NTOWR,NUD))
      STW=0.
      SUM=0.
      DO LP=NTOWR,63
        IF(HIST(LP,NUD).EQ.0.) GO TO 101
        SUM=SUM+HIST(LP,NUD)
        STW=STW+1.
      END DO
  101 DO LP=NTOWR-1,1,-1
        IF(HIST(LP,NUD).EQ.0.) GO TO 102
        SUM=SUM+HIST(LP,NUD)
        STW=STW+1.
      END DO
  102 T(38:39)=DT2(STW)
      T(42:45)=DT4(SUM)
      CALL DWRT(T)
      END
*DK DHZBHC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DHZBHC
CH
      SUBROUTINE DHZBHC(R1,QE,HIST,NR,DMIN,K3)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modifications:
C_CG     9-May-1989 C.Grab  Adapted to CERNVM
C
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      DIMENSION HIST(63,2),H(4),V(4)
      DIMENSION ZZ(15:49),RR(15:49)
      LOGICAL FSTRT
      DATA FSTRT/.TRUE./
      EQUIVALENCE(KPIKDP,J)
      IF(FSTRT) THEN
        FSTRT=.FALSE.
        DO 1 J=15,49
          CALL HUSRAN(J,1,1,TE,FI,DT,DF)
C         FOR RHO=1
          ZZ(J)=1./TAN(TE-0.5*DT)
          RR(J)=1./SIN(TE)
    1   CONTINUE
      END IF
      RB=ABS(R1)
      IF(FPIKDP) THEN
        MDLRDP=2000+NR
      ELSE
        IF(K3.EQ.3) CALL DQLEVL(KCLGDD)
        CALL DQL2E(ZZ(15)*RB,R1,ZZ(49)*RB,R1)
        IF(K3.EQ.3) CALL DQLEVL(LCHCDD)
      END IF
      DO 2 J=15,48
        IF(HIST(J,NR).EQ.0.) GO TO 2
        H(1)=ZZ(J  )*RB
        H(2)=ZZ(J+1)*RB
        R0  =RR(J)  *RB
        Q=(MAX(DMIN,QE*HIST(J,NR))+R0)/R0
        H(3)=H(2)*Q
        H(4)=H(1)*Q
        V(1)=R1
        V(2)=R1
        V(3)=V(2)*Q
        V(4)=V(3)
        IF(FPIKDP) THEN
          CALL DQPIK(0.5*(H(1)+H(2)),0.5*(V(1)+V(2)))
        ELSE
          CALL DQRAR(H,V)
        END IF
    2 CONTINUE
      END
*DK DHZEHC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DHZEHC
CH
      SUBROUTINE DHZEHC(Z1,QE,HIST,NR,DMIN,RCOR,K3)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modifications:
C_CG     9-May-1989 C.Grab  Adapted to CERNVM
C
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      DIMENSION HIST(63,2),H(4),V(4)
      DIMENSION RO(15),RR(15)
      LOGICAL FSTRT
      DATA FSTRT/.TRUE./,RIN/45./
      EQUIVALENCE(KPIKDP,J)
      IF(FSTRT) THEN
        FSTRT=.FALSE.
        DO 1 J=1,15
          CALL HUSRAN(J,1,1,TE,FI,DT,DF)
C         FOR Z=1
          RO(J)=TAN(TE-0.5*DT)
          RR(J)=1./COS(TE)
    1   CONTINUE
      END IF
      IF(NR.EQ.1) THEN
        ZS= Z1
      ELSE
        ZS=-Z1
      END IF
      RCOR=RO(15)*Z1
      IF(FPIKDP) THEN
        MDLRDP=2000+NR
      ELSE
        IF(K3.EQ.3) THEN
          CALL DQLEVL(KCLGDD)
          CALL DQL2E( Z1,RO(1)*ZS, Z1,RO(15)*ZS)
          CALL DQL2E(-Z1,RO(1)*ZS,-Z1,RO(15)*ZS)
          CALL DQLEVL(LCHCDD)
        ELSE
          RINS=SIGN(RIN,ZS)
          CALL DQL2E( Z1,RINS, Z1,RO(15)*ZS)
          CALL DQL2E(-Z1,RINS,-Z1,RO(15)*ZS)
        END IF
      END IF
      DO 2 J=1,14
        IF(HIST(J,NR).EQ.0.) GO TO 2
        V(1)=RO(J)  *ZS
        V(2)=RO(J+1)*ZS
        R0  =RR(J)  *Z1
        Q=(MAX(DMIN,QE*HIST(J,NR))+R0)/R0
        V(3)=V(2)*Q
        V(4)=V(1)*Q
        H(1)=Z1
        H(2)=Z1
        H(3)=H(2)*Q
        H(4)=H(3)
        IF(FPIKDP) THEN
          CALL DQPIK(0.5*(H(1)+H(2)),0.5*(V(1)+V(2)))
        ELSE
          CALL DQRAR(H,V)
        END IF
    2 CONTINUE
      J=63
      DO 3 N=1,14
        J=J-1
        IF(HIST(J,NR).EQ.0.) GO TO 3
        V(1)=RO(N)  *ZS
        V(2)=RO(N+1)*ZS
        R0  =RR(N)  *Z1
        Q=(MAX(DMIN,QE*HIST(J,NR))+R0)/R0
C        Q=(DMIN+QE*HIST(J,NR)+R0)/R0
        V(3)=V(2)*Q
        V(4)=V(1)*Q
        H(1)=-Z1
        H(2)=-Z1
        H(3)=H(2)*Q
        H(4)=H(3)
        IF(FPIKDP) THEN
          CALL DQPIK(0.5*(H(1)+H(2)),0.5*(V(1)+V(2)))
        ELSE
          CALL DQRAR(H,V)
        END IF
    3 CONTINUE
      END
*DK DHZPIE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DHZPIE
CH
      SUBROUTINE DHZPIE(KL)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modifications:
C_CG     9-May-1989 C.Grab  Adapted to CERNVM
C
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
      CALL DV0(ESDADB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE'
      CALL DPARAM(11
     &  ,J_PFI,J_PTE)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      FIMID=PARADA(2,J_PFI)
      TEMID=PARADA(2,J_PTE)
      FMIN=FIMID-90.
      FMAX=FIMID+90.
      DO 1 N=NUM1,NUM2
        CALL DCUTEC(K,FOUT)
        IF(FOUT) GO TO 1
        TE=DVEC(IVTEDV,K)
        IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 1
        FI=DFINXT(FIMID,DVEC(IVFIDV,K))
        IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 1
        H=DVEC(IVZZDV,K)
        IF(PARADA(4,J_PTE).NE.0.) THEN
          IF(FI.GE.FMIN.AND.FI.LE.FMAX) THEN
            V=DVEC(IVRODV,K)
          ELSE
            V=-DVEC(IVRODV,K)
          END IF
        ELSE
          V=DVEC(IVRODV,K)
        END IF
        CALL DQPIK(H,V)
    1 CONTINUE
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DHPERP
CH
      SUBROUTINE DHPERP(IFLAG,TPAR,IP,SDIST,TPAR2)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by DAVE BROWN
C
C
C  Find the arc length along a track for its point of closest approach
C  to a point.
C  Dave Brown, 4-3-92
C  Updated 16-4 to return the track parameters relative to the point
C  Updated 29-4 to ignore Z information with ITC only tracks
C
C  INPUTS;
C
C   IFLAG = Track type flag 0 = ITC only
C                           1 = Normal track
C   TPAR  = 5 helix parameters
C   IP    = Point at which to find closest approach
C  Outputs;
C
C   SDIST = Transverse arc length to the closest approach point
C   TPAR2 = Track parameters re-evaluated at this point
C
      IMPLICIT NONE
      INTEGER IFLAG
      INTEGER ILOOP,MAXLOOP,ICOR
      REAL TPAR(5),IP(3),SDIST,TPAR2(5)
      REAL COSP,SINP,COSL,SINL,PHI,IR,SINP0,COSP0,TANL
      REAL D0,Z0
      REAL FACTOR,DS,DS_CUT
      REAL T1(3),T0(3)

      DATA FACTOR/.6/,MAXLOOP/20/,DS_CUT/0.0001/
C
C  Unpack the track parameters
C
      IR  = TPAR(1)
      TANL= TPAR(2)
      PHI = TPAR(3)
      D0  = TPAR(4)
      Z0  = TPAR(5)
C
C  For ITC only tracks, ignore the Z information
C
      IF(IFLAG.EQ.0)THEN
        TANL = 0.0
      END IF
C
      COSL = 1./SQRT(1.+TANL**2)
      SINL = SIGN(SQRT(1.-COSL**2),TANL)
      COSP0 = COS(PHI)
      SINP0 = SIN(PHI)
C
C  Loop until the track position doesn't appreciably change.
C
      DS = 1.0
      SDIST = 0.0
      ILOOP=0
      DO WHILE(ABS(DS) .GT. DS_CUT .AND. ILOOP.LT.MAXLOOP)
C
C  Approximate the track as a line, evaluated at the current point.
C  T1 is a unit vector pointing in the track tangential direction,
C  T0 is a point on the track.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ILOOP=ILOOP+1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        COSP = COS(IR*SDIST+PHI)
        SINP = SIN(IR*SDIST+PHI)
        T1(1) =  COSL*COSP
        T1(2) =  COSL*SINP
        T1(3) =  SINL
        T0(1) =   SINP/IR+(D0-1./IR)*SINP0
        T0(2) =  -COSP/IR-(D0-1./IR)*COSP0
        T0(3) =  Z0+TANL*SDIST
C
C  Find the change in the arc length at the point of
C  closest approach between the linearized track and the vertex
C
        DS = 0.0
        DO ICOR=1,3
          DS  = DS  + (IP(ICOR)-T0(ICOR))*T1(ICOR)
        END DO
C
C  This is the total arc length; correct to make this into
C  perpendicular arc length.
C
        DS = DS*COSL
C
C  Add this to the existing arc length, deweighted by a factor
C  to prevent oscillations
C
        SDIST = SDIST + DS*FACTOR
      END DO
C
C  Compute the new track paramters
C
      COSP = COS(IR*SDIST+PHI)
      SINP = SIN(IR*SDIST+PHI)
      TPAR2(1) = TPAR(1)
      TPAR2(2) = TPAR(2)
      TPAR2(3) = IR*SDIST+PHI
      TPAR2(4) = (SINP/IR+(D0-1./IR)*SINP0-IP(1))/SINP
      TPAR2(5) = Z0+TANL*SDIST-IP(3)
C
      RETURN
      END
