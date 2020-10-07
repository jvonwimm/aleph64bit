*DK DTDD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTDD
CH
      SUBROUTINE DTDD
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
      EXTERNAL DTDDIS
      PARAMETER (MTR=99)
      DIMENSION KP(MTR)
      DIMENSION HRB(4),VRB(4)
      DIMENSION ICOL(-1:1)
      DATA L1/1/,NUMP/20./,S1/1./,QTR/0.1/
      DIMENSION NTR(2),NP(2)
      DATA NP/0,24/
      CHARACTER *2 DT2
      CHARACTER *5 DT5
      LOGICAL F2L2,F0,FOUT,FPNM
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFR,J_PTO,J_RVS,J_RCA,J_RST'
      CALL DPARAM(10
     &  ,J_PFR,J_PTO,J_RVS,J_RCA,J_RST)
      TPARDA=
     &  'J_RCP'
      CALL DPARAM(43
     &  ,J_RCP)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
C     ........................................... store parameters
      RFR=PARADA(2,J_PFR)
      RTO=PARADA(2,J_PTO)
      RV =PARADA(2,J_RVS)
      CALL DQCL(IAREDO)
      DFWI=DFWIDU(IZOMDO)
      DFWIDU(IZOMDO)=1111.
      CALL DQWIL(MOD(DFWIDU(IZOMDO),10.))
      IF(PARADA(4,J_RCP).EQ.1.) THEN
        LPNT=PARADA(2,J_RCP)
        CALL DQPD0(8,DLINDD+S1,0.)
      ELSE
        LPNT=-1
      END IF
C     .................................................... set up user range
      IF(IZOMDO.EQ.0) THEN
        CALL DQRER(0,-RTO,0.,RTO,RV,HRB,VRB)
      ELSE
        CALL DQRER(0, RFR,0.,RTO,RV,HRB,VRB)
      END IF
      CALL DQRU(HRB,VRB)
      IF(NOCLDT.EQ.0.AND.(.NOT.FPIKDP)) THEN
        CALL DQFWIA(L1)
        CALL DQDWI
      END IF
C     .......................... select tracks and store straight segments
      DH0=(HRB(2)-HRB(1))/FLOAT(NUMP-1)
      DXTR=DH0*QTR
      CALL DV0(FRFTDB,NUM1,NUM2,FOUT)
      IF(FOUT)GO TO 99
      CALL DTDTR0(HRB(1),HRB(2),NUM,KP,DH0)
      IF(NUM.EQ.0) GO TO 99
C     .................................. Draw track correlations
      IF(PARADA(4,J_RCA).NE.1.) THEN
        NU=NUM                          ! WITHOUT AXIS
      ELSE
        NU=NUM+2                        ! WITH AXIS
      END IF
      F0=.FALSE.
      IF(FPIKDP.AND.FPIMDP) THEN
        NST4=1
        NST2=NUM1
      ELSE
        NST4=PARADA(4,J_RST)
        NST2=PARADA(2,J_RST)
      END IF
      IF(NST4.NE.1) THEN
        N1L1=2                          ! NO TRACKS SELECTED
        N1L2=NU
        N2L1=1
        F2L2=.TRUE.
        NST2=0
      ELSE
        F2L2=.FALSE.
        IF(NST2.EQ.0) THEN
          N1L1=1                        ! AXIS TRACKS SELECTED
          N1L2=NUM
          N2L1=NUM+1
          N2L2=NUM+2
          IF(PARADA(4,J_RCA).NE.1.) F0=.TRUE.
        ELSE
          N1L1=1                        ! TRACK NST2 SELECTED
          N1L2=NU
          N2L1=NST2
          N2L2=NST2
        END IF
      END IF
      FPNM=.FALSE.
      IF(FPIKDP) THEN                                    
        IF(FPIMDP) THEN
          KPIKDP=NUM1
        ELSE
          MDLRDP=2500
          FPNM=.TRUE.
        END IF
      END IF
      DO N1=N1L1,N1L2
        IF(KP(N1).GT.0.AND.N1.NE.NST2) THEN                           
          ICOL(1)=NCTRDC(N1)
          IF(F2L2) N2L2=N1-1
          DO N2=N2L1,N2L2
            IF(KP(N2).GT.0) THEN
              JC=1
              CALL DTDTR1(N1,N2,KP,XR,DR,DH,NS)
              IF(NS.GT.0) THEN
                IF(F0) THEN
                  ICOL(-1)=ICOL(1)
                ELSE
                  ICOL(-1)=NCTRDC(N2)
                END IF
                DO K=2,NS
                  DL=DR
                  XL=XR
                  XR=XR+DH
                  CALL DTDTR2(N1,N2,KP,XR,DR)
                  IF(DL.GE.0.AND.DR.GE.0.) THEN
                    JC=-JC
                    IF(FPNM) THEN
                      IF(JC.EQ.1) THEN
                        KPIKDP=1000*N1+N2
                      ELSE
                        KPIKDP=1000*N2+N1
                      END IF
                    ELSE
                      CALL DGLEVL(ICOL(JC))
                    END IF
                    IF(ABS(XR-XL).GT.DXTR) THEN
                      CALL DDRAWA(DTDDIS,XL,XL,DL,XR,XR,DR)
                      IF(.NOT.FPIKDP) THEN
                        IF(LPNT.GE.0) THEN
                          CALL DGLEVL(LPNT)
                          CALL DQPD(XL,DL)
                        END IF
                      END IF
                    END IF
                  END IF
                END DO
              END IF
            END IF
          END DO
        END IF
      END DO
   99 CALL DCTYEX('TD',2)
      CALL DQSCA('H',HRB(1),HRB(3),'cm',2,'X"',2)
      CALL DQSCA('V',VRB(1),VRB(3),'cm',2,'dist.',5)
      CALL DQFR(IAREDO)
      CALL DPCSAR
      DFWIDU(IZOMDO)=DFWI
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DTDPIK
CH
      ENTRY DTDPIK(MODUL,NT12)
CH
CH --------------------------------------------------------------------
C             123456789 123456789 123456789 123456789 123456789
      TXTADW='TR=12 P=12345 RD        TR=12 P=12345 YE'
      NTR(1)=NT12/1000
      NTR(2)=MOD(NT12,1000)
      DO I=1,2
        CALL DVTRTP(NTR(I),PTOT)
        TXTADW(NP(I)+ 4:NP(I)+ 5)=DT2(FLOAT(NTR(I)))
        TXTADW(NP(I)+ 9:NP(I)+13)=DT5(PTOT)
        TXTADW(NP(I)+15:NP(I)+16)=TCOLDF(NCTRDC(NTR(I)))
      END DO
      CALL DWRC
      MODUL=FRFTDB
      NT12=NTR(1)
      END
*DK DTDTR0
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTDTR0
CH
      SUBROUTINE DTDTR0(HD1,HD2,NUM,KP,DSTEP)
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
C INVOKED BY TANSW.EQ.'RO'  (DKRO)
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION KP(*)
      DIMENSION FFRTO(7)
      PARAMETER (MTR=99,NSEG=10)
      DIMENSION XP(0:NSEG,-1:MTR),XT(2),YT(2),ZT(2)
      DIMENSION AY(NSEG,-1:MTR),BY(NSEG,-1:MTR),
     &          AZ(NSEG,-1:MTR),BZ(NSEG,-1:MTR)
C     .. axis left to right       axis right to left
      DATA XP(0,-1),XP(1,-1)  ,  XP(0,0),XP(1,0)/-9.,9.,9.,-9./
      DATA AY(1,-1),BY(1,-1)  ,  AY(1,0),BY(1,0)/4*0./
      DATA AZ(1,-1),BZ(1,-1)  ,  AZ(1,0),BZ(1,0)/4*0./
      LOGICAL FIN,FOUT,DCCTR,FBAR(2)
      DATA DF/0.2/,DSMAL/0.000001/
      NUM=BNUMDB(2,FRFTDB)
      IF(NUM.LE.0) RETURN
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_RAL,J_RXX,J_RYY,J_RZZ,J_RCA,J_RST'
      CALL DPARAM(10
     &  ,J_PFI,J_PTE,J_RAL,J_RXX,J_RYY,J_RZZ,J_RCA,J_RST)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DHTINP
      CALL DSCTR
C     ........................................... store parameters
      H1=HD1
      H2=HD2
      DH0=DSTEP
C      IF(PARADA(4,J_RRC).EQ.1.) THEN
C        R2=PARADA(2,J_RRC)**2
C      ELSE
C        R2=99.**2
C      END IF
      FI=    PARADA(2,J_PFI)
      TE=90.-PARADA(2,J_PTE)
      AL=    PARADA(2,J_RAL)
      SF=SIND(FI)
      CF=COSD(FI)
      ST=SIND(TE)
      CT=COSD(TE)
      SG=SIND(AL)
      CG=COSD(AL)
      X0=PARADA(2,J_RXX)
      Y0=PARADA(2,J_RYY)
      Z0=PARADA(2,J_RZZ)
C     .................. the picture is independent of AL: we set SG=0 CG=1
      X2= CF*X0+SF*Y0
      YA=-SF*X0+CF*Y0
      ZA=-ST*X2+CT*Z0
C     .......................... select tracks and store straight segments
      DO 11 N=1,NUM
        KP(N)=0
        IF(FNOTDT(N)) GO TO 11
        CALL DVTRV(N)
        CALL DVXT(N,XYZVDT)
        CALL=DHELX0(FFRTO(2),FFRTO(3),FFRTO(4),FFRTO(6),FBAR)
        IF(DCCTR(FFRTO(4),FFRTO(5))) GO TO 11
        IHTFR=MIN(IHTRDO(3),2)
        F=FFRTO(IHTFR)
        DO M=1,99
          IF(KP(N).GE.NSEG) GO TO 11
          X1=DHELIX(F,IVXXDV)
          Y1=DHELIX(F,IVYYDV)
          Z1=DHELIX(F,IVZZDV)
          F=F+DF
C         ...................................... Rotate straight segment
C         .................................. independent of AL: SG=0 CG=1
          X2 = CF*X1+SF*Y1
          YT2=-SF*X1+CF*Y1-YA
          XT2= CT*X2+ST*Z1
          ZT2=-ST*X2+CT*Z1-ZA
C         ......................................
          IF(M.EQ.1) THEN
            XP(0,N)=XT2
          ELSE
C           .............. no more segment stored if track runs outwards
            IF(XT2.GT.XT1) THEN
              IF(XT1.GT.H2) GO TO 11
            ELSE
              IF(XT1.LT.H1) GO TO 11
            END IF
C           ..................... Store straight line parameters
            DX=XT2-XT1
            IF(DX.NE.0.) THEN
C             ......................... Y=AY*X+BY  Z=AZ*X+BZ
              K=KP(N)+1
              AY(K,N)=(YT2-YT1)/DX
              BY(K,N)= YT1-XT1*AY(K,N)
              AZ(K,N)=(ZT2-ZT1)/DX
              BZ(K,N)= ZT1-XT1*AZ(K,N)
C              FIN=.FALSE.
C              IF(YT1**2+ZT1**2.LE.R2.OR.
C     &           YT2**2+ZT2**2.LE.R2) THEN
C                FIN=.TRUE.
C              ELSE
CC               ........ calculate nearest point on segment in 2D from 0,0
C                Q=AY(K,N)**2+AZ(K,N)**2
C                XN=-(AY(K,N)*BY(K,N)+AZ(K,N)*BZ(K,N))/Q
C                IF(XT1.LE.XN.AND.XN.LE.XT2) THEN
C                  FIN=.TRUE.
C                ELSE IF(XT2.LE.XN.AND.XN.LE.XT1) THEN
C                  FIN=.TRUE.
C                END IF
C              END IF
C              IF(FIN) THEN
              XP(K,N)=XT2
              KP(N)=K
C              END IF
            END IF
          END IF
          XT1=XT2
          YT1=YT2
          ZT1=ZT2
        END DO
   11 CONTINUE
C     .............................................. CONNECTING TRACKS
      CALL DVXTN
      IF(NTRKDN.GT.0) THEN
        DO 111 N=1,NTRKDN
          IF(ICOLDN(N).GE.0) THEN
            NUM=NUM+1
            KP(NUM)=0
            IF(ICOLDN(N).EQ.0) THEN
              NCTRDC(NUM)=8
            ELSE
              NCTRDC(NUM)=ICOLDN(N)
            END IF
            MM=2
            DO M=1,2
              X1=XYZTDN(1,M,N)
              Y1=XYZTDN(2,M,N)
              Z1=XYZTDN(3,M,N)
              X2 = CF*X1+SF*Y1
              YT(MM)=-SF*X1+CF*Y1-YA
              XT(MM)= CT*X2+ST*Z1
              ZT(MM)=-ST*X2+CT*Z1-ZA
C             .................... CONNECTING TRACKS RUN BACKWARDS
              MM=1
            END DO
            IF(XT(2).GT.XT(1)) THEN
              IF(XT(1).GT.H2) GO TO 111
            ELSE
              IF(XT(1).LT.H1) GO TO 111
            END IF
C           ..................... Store straight line parameters
            DX=XT(2)-XT(1)
            IF(DX.NE.0.) THEN
C             ......................... Y=AY*X+BY  Z=AZ*X+BZ
              AY(1,NUM)=(YT(2)-YT(1))/DX
              BY(1,NUM)= YT(1)-XT(1)*AY(1,NUM)
              AZ(1,NUM)=(ZT(2)-ZT(1))/DX
              BZ(1,NUM)= ZT(1)-XT(1)*AZ(1,NUM)
              FIN=.FALSE.
C?            IF(YT(1)**2+ZT(1)**2.LE.R2.OR.
C?   &           YT(2)**2+ZT(2)**2.LE.R2) THEN
C?              FIN=.TRUE.
C?            ELSE
C               ........ calculate nearest point on segment in 2D from 0,0
                Q=AY(1,N)**2+AZ(1,N)**2
                XN=-(AY(1,N)*BY(1,N)+AZ(1,N)*BZ(1,N))/Q
                IF(XT(1).LE.XN.AND.XN.LE.XT(2)) THEN
                  FIN=.TRUE.
                ELSE IF(XT(2).LE.XN.AND.XN.LE.XT(1)) THEN
                  FIN=.TRUE.
                END IF
C?            END IF
              IF(FIN) THEN
                KP(NUM)=1
                XP(0,NUM)=XT(1)
                XP(1,NUM)=XT(2)
              END IF
            END IF
          END IF
  111   CONTINUE
      END IF
C     ..................................................... STORE AXIS TRACKS
      IF(PARADA(4,J_RCA).EQ.1..OR.
     &  (PARADA(4,J_RST).EQ.1..AND.PARADA(2,J_RST).EQ.0.) ) THEN
        DO N=NUM+1,NUM+2
          KP(N)=1
          AY(1,N)=0.
          BY(1,N)=0.
          AZ(1,N)=0.
          BZ(1,N)=0.
          NCTRDC(N)=PARADA(2,J_RCA)
        END DO
        XP(0,NUM+1)=-9.
        XP(1,NUM+1)= 9.
        XP(0,NUM+2)= 9.
        XP(1,NUM+2)=-9.
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
      ENTRY DTDTR1(M1,M2,KP,X,DR,DH,NS)
CH
CH --------------------------------------------------------------------
CH
CH..............---
      NS=0
      IF(XP(0,M1).LT.XP(1,M1)) THEN     ! --> tracks
        IF(XP(0,M2).GT.XP(1,M2)) RETURN ! <-- tracks rejected
        IF(XP(0,M1).GT.XP(0,M2)) THEN
C         .................................. begin at start of track 1
          I1=1
          X=XP(0,M1)
          DO I2=1,KP(M2)
            IF(X.LE.XP(I2,M2)) GO TO 201
          END DO
          RETURN
        ELSE  
C         .................................. begin at start of track 2
          I2=1
          X=XP(0,M2)
          DO I1=1,KP(M1)
            IF(X.LE.XP(I1,M1)) GO TO 201
          END DO
          RETURN
        END IF
  201   XEND=MIN(XP(KP(M1),M1),XP(KP(M2),M2))
        IF(XEND.LT.H2) THEN
          D=ABS(XEND-X)
          NN=2.+D/DH0
          DH=D/NN-DSMAL
        ELSE
          DH=DH0
        END IF
C       ................. EXAMPLE : H2=1.8,X=-0.1,DH=1 -> NS=3
        NS=2+(H2-X)/DH
      ELSE                              ! <-- tracks
        IF(XP(0,M2).LT.XP(1,M2)) RETURN ! --> tracks rejected
        IF(XP(0,M1).LT.XP(0,M2)) THEN
C         .................................. begin at start of track 1
          I1=1
          X=XP(0,M1)
          DO I2=1,KP(M2)
            IF(X.GE.XP(I2,M2)) GO TO 202
          END DO
          RETURN
        ELSE  
C         .................................. begin at start of track 2
          I2=1
          X=XP(0,M2)
          DO I1=1,KP(M1)
            IF(X.GE.XP(I1,M1)) GO TO 202
          END DO
          RETURN
        END IF
  202   XEND=MAX(XP(KP(M1),M1),XP(KP(M2),M2))
        IF(XEND.GT.H1) THEN
          D=ABS(XEND-X)
          NN=2.+D/DH0
          DH=-D/NN+DSMAL
        ELSE
          DH=-DH0
        END IF
        NS=2+(H1-X)/DH
      END IF
      DR=SQRT( ((AY(I2,M2)-AY(I1,M1))*X + BY(I2,M2)-BY(I1,M1))**2
     &       + ((AZ(I2,M2)-AZ(I1,M1))*X + BZ(I2,M2)-BZ(I1,M1))**2 )
      RETURN
CH
CH
CH
CH
      ENTRY DTDTR2(M1,M2,KP,X,DR)
CH
CH --------------------------------------------------------------------
CH
      DR=-1.
      N1=M1
      N2=M2
      K1=KP(N1)
      K2=KP(N2)
      FOUT=.TRUE.
      IF(XP(0,N1).LE.XP(K1,N1)) THEN
        IF(X.GE.XP( 0,N1).AND.X.LE.XP(K1,N1)) FOUT=.FALSE.
      ELSE
        IF(X.GE.XP(K1,N1).AND.X.LE.XP( 0,N1)) FOUT=.FALSE.
      END IF
      IF(FOUT) RETURN
      FOUT=.TRUE.
      IF(XP(0,N2).LE.XP(K2,N2)) THEN
        IF(X.GE.XP( 0,N2).AND.X.LE.XP(K2,N2)) FOUT=.FALSE.
      ELSE
        IF(X.GE.XP(K2,N2).AND.X.LE.XP( 0,N2)) FOUT=.FALSE.
      END IF
      IF(FOUT) RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DTDTR3
CH
      ENTRY DTDTR3(X,DR)
CH
CH --------------------------------------------------------------------
CH
      IF(XP(0,N1).LE.XP(K1,N1)) THEN
        DO I1=1,K1
          IF(X.LE.XP(I1,N1)) GO TO 101
        END DO
      ELSE
        DO I1=1,K1
          IF(X.GE.XP(I1,N1)) GO TO 101
        END DO
      END IF
  101 IF(XP(0,N2).LE.XP(K2,N2)) THEN
        DO I2=1,K2
          IF(X.LE.XP(I2,N2)) GO TO 102
        END DO
      ELSE
        DO I2=1,K2
          IF(X.GE.XP(I2,N2)) GO TO 102
        END DO
      END IF
  102 DR=SQRT( ((AY(I2,N2)-AY(I1,N1))*X + BY(I2,N2)-BY(I1,N1))**2
     &       + ((AZ(I2,N2)-AZ(I1,N1))*X + BZ(I2,N2)-BZ(I1,N1))**2 )
      RETURN
CH..............---
CH
CH
CH
CH --------------------------------------------------------------------  DTDTR3
CH
      ENTRY DTDTR4(X,DR)
CH
CH --------------------------------------------------------------------
CH
      END
*DK DTDDIS
CH..............***
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTDDIS
CH
      SUBROUTINE DTDDIS(F1,H1,V1,F2,H2,V2,FM,HM,VM,D,FC)
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      INCLUDE 'DALI_CF.INC'
      LOGICAL FC
      IF(FC) THEN
         FKTRDP=FM
         CALL DQL2EP(H1,V1,HM,VM)
         CALL DQL2EP(HM,VM,H2,V2)
         RETURN
      END IF
      FM=0.5*(F1+F2)
      HM=FM
      CALL DTDTR3(HM,VM)  
      DHU=H2-H1
      DVU=V2-V1
      IF(DHU.EQ.0..AND.DVU.EQ.0.) THEN
        FKTRDP=FM
        FC=.FALSE.
        RETURN
      END IF
      DH21=AHSCDQ*DHU+BHSCDQ*DVU
      DV21=AVSCDQ*DHU+BVSCDQ*DVU
      DHU=HM-H1
      DVU=VM-V1
      DHM1=AHSCDQ*DHU+BHSCDQ*DVU
      DVM1=AVSCDQ*DHU+BVSCDQ*DVU
      DM=(DV21*DHM1-DH21*DVM1)**2/(DV21**2+DH21**2)
      IF(D.LT.DMAXDQ.AND.DM.LT.DMAXDQ) THEN
         FKTRDP=FM
         CALL DQL2EP(H1,V1,H2,V2)
         FC=.FALSE.
      ELSE
         FC=.TRUE.
         D=DM
      END IF
      END
*DK DTD_PV
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTD_PV
CH
      SUBROUTINE DTD_PV
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     Put axis through primary vertex
      INCLUDE 'DALI_CF.INC'
      DIMENSION XYZ(3)
      CALL DVVTX0(NVX)
      IF(NVX.GT.0) THEN
        DO N=NVX,1,-1
          CALL DVVTX(N,NTYPE,XYZ)
          IF(NTYPE.EQ.6.OR.NTYPE.EQ.1) THEN
C           :::::::::::::::::::::::::::::::::::::::::::::::::::::::
            TPARDA=
     &        'J_RXX,J_RYY,J_RZZ'
            CALL DPARAM(10
     &        ,J_RXX,J_RYY,J_RZZ)
C           :::::::::::::::::::::::::::::::::::::::::::::::::::::::
            PARADA(2,J_RXX)=XYZ(1)
            PARADA(2,J_RYY)=XYZ(2)
            PARADA(2,J_RZZ)=XYZ(3)
            RETURN
          END IF
        END DO
      END IF
      CALL DWRT('A primary vertex does not exist !#')
      END
*DK DTDM
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DM2D
CH
      SUBROUTINE DTDM
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Draw phi cursors on XY,FR,FT,FZ for RZ
C    Called by : DKRZ
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      PARAMETER (NLIN=13,NLIN2=26)
      DIMENSION H(2*NLIN),V(2*NLIN),IH(2*NLIN),IV(2*NLIN),NDSEG(2,NLIN)
      EXTERNAL DTDMM
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE'
      CALL DPARAM(11
     &  ,J_PFI,J_PTE)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      K=1
      DO N=1,NLIN
        NDSEG(1,N)=K
        NDSEG(2,N)=2
        K=K+2
      END DO
      CALL DTDM0(IHC,IVC,H,V,NDSEG,NUMPL)
      IF(NUMPL.EQ.0) RETURN
      CALL DGLINM(IHC,IVC,H,V,IH,IV,NAR,DTDMM,.FALSE.,NDSEG,NUMPL)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DTDMF
CH
      ENTRY DTDMF

CH
CH --------------------------------------------------------------------
      CALL DMFIX(H(3),V(3),NDSEG,NUMPL-1)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DTDMVL
CH
      ENTRY DTDMVL(HVL)

CH
CH --------------------------------------------------------------------
      IF(NUMPL.GT.1) THEN
        DO K=2,NUMPL
          IF(H(NDSEG(1,K)).GT.HLOWDG(IAREDO).AND.
     &       H(NDSEG(1,K)).LT.HHGHDG(IAREDO)) THEN
            HVL=H(NDSEG(1,K))
            RETURN
          END IF
        END DO
      END IF
      HVL=0.
      END
*DK DTDM0
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTDM0
CH
      SUBROUTINE DTDM0(IHC,IVC,H,V,NDSEG,NUMPL)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      PARAMETER (NLIN=8)
      DIMENSION H(2*NLIN),V(2*NLIN),NDSEG(2,NLIN)
      DIMENSION VU(13)
      DATA DH/8./,DS/2./,HC/200./,VC/600./
      LOGICAL FBUT(4),FIN,FW(0:12)
      CHARACTER *1 TKBD
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_PBP'
      CALL DPARAM(11
     &  ,J_PFI,J_PTE,J_PBP)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IHC=HC
      IVC=VC
      FIN=.FALSE.
      DO I=0,12
        FW(I)=.FALSE.
        IF(ISTODS(4,I,IWUSDO).GT.0) THEN
          IPIC=ISTODS(5,I,IWUSDO)
          IF((TPICDP(IPIC).EQ.'RO'.OR.TPICDP(IPIC).EQ.'TD').AND.
     &      PSTODS(1,J_PBP,I,IWUSDO).EQ.0.) THEN
            VM=0.5*(VLOWDG(I)+VHGHDG(I))
            CALL DQINV(I,0.,VM,HDUM,VU(I))
            FW(I)=.TRUE.
            FIN=.TRUE.
          END IF
        END IF
      END DO
      IF(.NOT.FIN) THEN
        NUMPL=0
        RETURN
      END IF
      CALL DWRT(' ')
      HC=IHC
      VC=IVC
      H(1)=HC-DH
      H(2)=HC+DH
      V(1)=VC
      V(2)=VC
      GO TO 1
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DTDPIK
CH
      ENTRY DTDMM(IHC,IVC,H,V,NDSEG,NUMPL,FBUT,TKBD)

CH
CH --------------------------------------------------------------------
      IF(.NOT.FBUT(2)) THEN
        CALL DWRT(' ')
        RETURN
      END IF
      HC=IHC
      VC=IVC
      H(1)=HC-DH
      H(2)=HC+DH
      V(1)=VC
      V(2)=VC
      IF(.NOT.FBUT(1)) RETURN
    1 K=1
      NUMPL=1
      DO I=0,12
        IF(FW(I).AND.HMINDG(I).LE.HC.AND.HC.LT.HHGHDG(I).AND.
     &               VMINDG(I).LE.VC.AND.VC.LT.VHGHDG(I)) GO TO 10
      END DO
      RETURN
   10 H(1)=HC-DS
      H(2)=HC+DS
      K=K+2
      H(K  )=HC
      H(K+1)=HC
      V(K  )=VLOWDG(I)
      V(K+1)=VHGHDG(I)
      NUMPL=NUMPL+1
      CALL DQINV(I,HC,0.,HU,VDUM)
      WRITE(TXTADW,1001) HU
 1001 FORMAT(F10.3,'=H>')
      CALL DWR_OVER_PRINT(13)
      FI=PSTODS(1,J_PFI,I,IWUSDO)
      TE=PSTODS(1,J_PTE,I,IWUSDO)
      DO L=0,12
        IF(L.NE.I.AND.FW(L).AND.
     &    FI.EQ.PSTODS(1,J_PFI,L,IWUSDO).AND.
     &    TE.EQ.PSTODS(1,J_PTE,L,IWUSDO) ) THEN
          CALL DQSET(L,0.,0.)
          CALL DQPOC(HU,VU(L),HD,VDUM,FIN)
          IF(FIN) THEN
            K=K+2
            H(K  )=HD
            H(K+1)=HD
            V(K  )=VLOWDG(L)
            V(K+1)=VHGHDG(L)
            NUMPL=NUMPL+1
          END IF
        END IF
      END DO
      END
*DK DTN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTN
CH
      SUBROUTINE DTN(F,N,TOUT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:CHANGE TO DALI FORMAT
C    Inputs    :F=FLOATING POINT #, N=#OF LETTERS IN OUTPUT
C    Outputs   :TOUT=CHARACTER STRING
C
C ---------------------------------------------------------------------
      CHARACTER *(*) TOUT
      CHARACTER *17 T17
      CHARACTER *7 TF(2:9)
      DATA TF/
     &  '(F3.1)',
     &  '(F5.2)',
     &  '(F7.3)',
     &  '(F9.4)',
     &  '(F11.5)',
     &  '(F13.6)',
     &  '(F15.7)',
     &  '(F17.8)'/
      CHARACTER *7 TG(2:9)
      DATA TG/
     &  '(G13.1)',
     &  '(G13.2)',
     &  '(G13.3)',
     &  '(G13.4)',
     &  '(G13.5)',
     &  '(G13.6)',
     &  '(G13.7)',
     &  '(G13.8)'/
      DIMENSION FMIN(9),FMAX(9)
      DATA FMIN/-0.5,-9.5,-99.5,-999.5,-9999.5,-99999.5,-999999.5,
     &  -9999999.5,-99999999.5/
      DATA FMAX/ 9.5,99.5,999.5,9999.5,99999.5,999999.5,9999999.5,
     &  99999999.5,999999999.5/
      IF(N.LE.0) GO TO 99 
C     .................................. EXCLUDE TOO BIG AND TOO SMALL NUMBERS
      IF(F.GE.FMAX(N).OR.F.LE.FMIN(N)) GO TO 99
      J=N-1
C     .............................. IF NUMBERS ARE TOO BIG CONVERT TO INTEGER
      IF(N.EQ.1.OR.F.GE.FMAX(J).OR.F.LE.FMIN(J)) THEN
        I=NINT(F)
        GO TO 10
      END IF
      I=F
      G=I
C     ......................... IF NUMBER IS ZERO AFTER "," CONVERT TO INTEGER
      IF(G.EQ.F) GO TO 10
C     ..... ONLY POSITIVE NUMBERS ARE HANDLED, THE "-" IS INSTALLED AT THE END
      FA=ABS(F)
      IF(F.GE.0.) THEN
        K=N
      ELSE
        K=N-1
      END IF
      IF(FA.GE.0.1) THEN
C       .......................... USE G-FORMAT WHER IT DOES NOT GIVE EXPONENT
        WRITE(T17,TG(K),ERR=99) FA
      ELSE
C       ..                                  . USE F-FORMAT FOR SMALLER NUMBERS
        WRITE(T17,TF(K),ERR=99) FA
      END IF
C     ............................................................ ADJUST LEFT
      DO 701 L=1,17
        IF(T17(L:L).NE.' ') GO TO 21
  701 CONTINUE
   21 IF(K.EQ.N) THEN
C       ..................................................... POSITIVE NUMBERS
        IF(T17(L:L).NE.'0') THEN
          TOUT=T17(L:L+K-1)
        ELSE
C         ........................................... CHANGE "0.XXX" TO ".XXX"
          TOUT=T17(L+1:L+K)
        END IF
      ELSE
C       ..................................................... NEGATIVE NUMBERS
        IF(T17(L:L).NE.'0') THEN
          TOUT='-'//T17(L:L+K-1)
        ELSE
C         ........................................... CHANGE "0.XXX" TO ".XXX"
          TOUT='-'//T17(L+1:L+K)
        END IF
      END IF
      RETURN
   10 WRITE(T17,1010,ERR=99) I
 1010 FORMAT(I9)
C    .......................................................... ADJUST LEFT
      DO 700 L=1,17
        IF(T17(L:L).NE.' ') GO TO 11
  700 CONTINUE
   11 TOUT=T17(L:L+J)
      RETURN
C     ............................................................... OVERFLOW
   99 TOUT='*'
      RETURN
      END
*DK DTP
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DTP
CH
      SUBROUTINE DTP(F,N,TOUT)
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
      CHARACTER *(*) TOUT
      IF(F.GT.0.) THEN
         TOUT(1:1)='+'
         CALL DTN(F,N-1,TOUT(2:N))
      ELSE
         CALL DTN(F,N,TOUT)
      END IF
      END
*DK DT1
      CHARACTER *(*) FUNCTION DT1(F)
      CHARACTER *1 DTT
      CALL DTN(F,1,DTT)
      DT1=DTT
      END
*DK DT5
      CHARACTER *(*) FUNCTION DT5(F)
      CHARACTER *5 DTT
      CALL DTN(F,5,DTT)
      DT5=DTT
      END
*DK DT2
      CHARACTER *(*) FUNCTION DT2(F)
      CHARACTER *2 DTT
      CALL DTN(F,2,DTT)
      DT2=DTT
      END
*DK DT3
      CHARACTER *(*) FUNCTION DT3(F)
      CHARACTER *3 DTT
      CALL DTN(F,3,DTT)
      DT3=DTT
      END
*DK DT3Z
      CHARACTER *(*) FUNCTION DT3Z(F)
      CHARACTER *3 DTT
      IF(F.EQ.0.) THEN
        DT3Z='   '
      ELSE
        CALL DTN(F,3,DTT)
        DT3Z=DTT
      END IF
      END
*DK DT4
      CHARACTER *(*) FUNCTION DT4(F)
      CHARACTER *4 DTT
      CALL DTN(F,4,DTT)
      DT4=DTT
      END
*DK DT6
      CHARACTER *(*) FUNCTION DT6(F)
      CHARACTER *6 DTT
      CALL DTN(F,6,DTT)
      DT6=DTT
      END
*DK DT7
      CHARACTER *(*) FUNCTION DT7(F)
      CHARACTER *7 DTT
      CALL DTN(F,7,DTT)
      DT7=DTT
      END
*DK DT8
      CHARACTER *(*) FUNCTION DT8(F)
      CHARACTER *8 DTT
      CALL DTN(F,8,DTT)
      DT8=DTT
      END
*DK DTINT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DTINT
CH
      SUBROUTINE DTINT(I,N1,N2,T)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     Integer I is filled left adjusted into character string T(N1:N2)
C
      CHARACTER *(*) T
      CHARACTER *18 TW
      ND=N2-N1+1
      IF(ND.LT.1 ) RETURN
      IF(ND.GT.18) GO TO 9
      WRITE(TW,1000,ERR=9) I
 1000 FORMAT(I18)
      DO L=1,18
        IF(TW(L:L).NE.' ') THEN
          IF(N2-N1.LT.18-L) GO TO 9
          T(N1:N2)=TW(L:18)
          RETURN
        END IF
      END DO
    9 T(N1:N2)='*'
      END
*DK DTINTR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DTINTR
CH
      SUBROUTINE DTINTR(I,N1,N2,T)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     Integer I is filled right adjusted into character string T(N1:N2)
C
      CHARACTER *(*) T
      CHARACTER *19 TW
      ND=N2-N1+1
      IF(ND.LT.1 ) RETURN
      IF(ND.GT.18) GO TO 9
      WRITE(TW,1000,ERR=9) I
 1000 FORMAT(I18)
      IF(TW(18-ND:18-ND).NE.' ') GO TO 9
      T(N1:N2)=TW(18-ND+1:18)
      RETURN
    9 T(N1:N2)=' '
      IF(N1.EQ.N2.OR.I.GT.0) THEN
        T(N2  :N2)='*'
      ELSE
        T(N2-1:N2)='-*'
      END IF
      END
*DK DTFLR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DTFLR
CH
      SUBROUTINE DTFLR(R,TF,N1,N2,T)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     Real R is filled right adjusted into character string T(N1:N2)
C     with Format TF
C                      Example:      CALL DTFLR(R,'(F5.2)',4,8,T)
C
      CHARACTER *(*) T,TF
      CHARACTER *19 TW
      ND=N2-N1+1
      IF(ND.LT.1 ) RETURN
      IF(ND.GT.18) GO TO 9
      WRITE(TW,TF,ERR=9) R
      T(N1:N2)=TW(1:ND)
      RETURN
    9 T(N1:N2)=' '
      T(N2:N2)='*'
      IF(N2.NE.N1) THEN
        IF(R.GE.0) THEN
          T(N2-1:N2)='+*'
        ELSE
          T(N2-1:N2)='-*'
        END IF
      END IF
      END
*DK DTYP_VAR
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DTYP_VAR
CH
      SUBROUTINE DTYP_VAR(TH,TF,K,INTF,FLOATF,T)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     TH= ' MN NH  D0      Z0    FI  TE   PT    LA  NT'
C     TF= 'iii ii FF.FFF FFF.FF fff fff FFFF.FF ii iii
C     T =  123 12 -5.345 -55.45 123 123 -123.45 12 123

      INCLUDE 'DALI_CF.INC'
      DIMENSION L1(17),L2(17)
      CHARACTER *(*) TH,TF,T
      CHARACTER *1 TI
      CHARACTER *2 TV
      CHARACTER *6 TFL
      EXTERNAL INTF,FLOATF
      LEN=LENOCC(TF)
      N=0
      M=1
      DO L=1,LEN
        IF(M.EQ.1) THEN
          IF(TF(L:L).NE.' ') THEN
            N=N+1
            L1(N)=L
            L2(N)=LEN
            M=2
          END IF
        ELSE
          IF(TF(L:L).EQ.' ') THEN
            L2(N)=L-1
            M=1
          END IF
        END IF
      END DO

    2 DO I=1,N
        DO L=L1(I),L2(I)
          IF(TH(L:L).NE.' ') THEN
            TV=TH(L:L+1)
            GO TO 3
          END IF
        END DO
        GO TO 9

    3   TI=TF(L1(I):L1(I))
        IF(TI.NE.'F') THEN
          IF(TI.EQ.'i') THEN
            IN=INTF(TV,K)
          ELSE
            IN=FLOATF(TV,K)
          END IF
          CALL DTINTR(IN,L1(I),L2(I),T)
        ELSE
          DO L=L1(I)+1,L2(I)-1
            IF(TF(L:L).EQ.'.') GO TO 4
          END DO
          L=L2(I)
    4     WRITE(TFL,1000,ERR=9) L2(I)-L1(I)+1,L2(I)-L
 1000     FORMAT('(F',I1,'.',I1,')')
          R=FLOATF(TV,K)
          CALL DTFLR(R,TFL,L1(I),L2(I),T)
        END IF
      END DO
    9 END
*DK DTRAP0
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      SUBROUTINE DTRAPZ(FI,DF,RO,DR,H,V)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Outputs   :H(4),V(4) = TRAPEZ POLYGON
C
C ---------------------------------------------------------------------
C
      INCLUDE 'DALI_CF.INC'
      DIMENSION H(4),V(4)
      FI1=FI-DF
      FI2=FI+DF
      RO1=RO-DR
      RO2=RO+DR
      SF1=SIND(FI1)
      SF2=SIND(FI2)
      CF1=COSD(FI1)
      CF2=COSD(FI2)
      H(1)=SF1*RO1
      V(1)=CF1*RO1
      H(2)=SF1*RO2
      V(2)=CF1*RO2
      H(3)=SF2*RO2
      V(3)=CF2*RO2
      H(4)=SF2*RO1
      V(4)=CF2*RO1
      END
