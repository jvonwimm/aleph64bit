*DK DFRRBC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFRRBC
CH
      SUBROUTINE DFRRBC(HRB,VRB)
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
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PDF,J_PFR,J_PTO'
      CALL DPARAM(11
     &  ,J_PFI,J_PDF,J_PFR,J_PTO)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      PARADA(2,J_PFR)=MIN(HRB(1),HRB(3))
      PARADA(2,J_PTO)=MAX(HRB(1),HRB(3))
      PARADA(2,J_PFI)=0.5*(VRB(1)+VRB(3))
      PARADA(2,J_PDF)=DMIX(PARADA(1,J_PDF),
     &  ABS(VRB(1)-VRB(3)), PARADA(3,J_PDF))
      END
*DK DFTMO
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTMO
CH
      SUBROUTINE DFTMO(NAR,FI,TE,H,V,FIN)
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
      LOGICAL FIN
      CALL DQSET(NAR,0.,0.)
      CALL DQINV(NAR,
     &  HLOWDG(IAREDO)+1.,0.5*(VLOWDG(IAREDO)+VHGHDG(IAREDO)),
     &  TDUM,FIMID)
      FFI=DFINXT(FIMID,FI)
      CALL DQPOC(-TE,FFI,HH,VV,FIN)
      IF(FIN) THEN
         H=HH
         V=VV
      END IF
      RETURN
      END
*DK DFTM
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTM
CH
      SUBROUTINE DFTM(IAR1,IAR2,FOK)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION H(244),V(244),IH(244),IV(244),NDSEG(2,40)
      LOGICAL FIN,FOK
      DATA TE0/90./,FI0/90./
      DATA D/100./
C      DATA NCOL/8/
      EXTERNAL DFTMM
      IF(TPICDP(ISTODS(5,IAR1,IWUSDO)).NE.'FT'.OR.
     &          ISTODS(4,IAR1,IWUSDO).LE. 0) THEN
        CALL DWRT('Select window with phi/theta (FT) projection.')
        FOK=.FALSE.
        RETURN
      END IF
      CALL DQSET(IAR1,0.,0.)
      IF(IAR2.GT.0) THEN
        IF(TPICDP(ISTODS(5,IAR2,IWUSDO)).NE.'FT'.OR.
     &            ISTODS(4,IAR2,IWUSDO).LE. 0.OR.
     &            ISTODS(6,IAR2,IWUSDO).NE. 0) THEN
          CALL DWRT(
     &      'Select windows with FT projection in no zoom mode.')
          FOK=.FALSE.
          RETURN
        ELSE
          DHW=HHGHDG(IAR2)-HHGHDG(IAR1)
        END IF
      ELSE
        DHW=0.
      END IF
      FOK=.TRUE.
      IF(P.NE.0.) THEN
        FI=FI0
        TE=TE0
      END IF
      CALL DQPOC(-TE,FI,HC,VC,FIN)
      IF(FIN) THEN
        IHC=HC
        IVC=VC
      ELSE
        IHC=HLOWDG(IAR1)+D
        IVC=VLOWDG(IAR1)+D
      END IF
      CALL DFTMS(P,IAR1,DHW)
      P=0.
      CALL DGLINM(IHC,IVC,H,V,IH,IV,NAR,DFTMM,.FALSE.,NDSEG,NUMPL)
      CALL DQINV(IAR1,FLOAT(IHC),FLOAT(IVC),TE,FI)
      TE=-TE
      CALL DWRT(' ')
      RETURN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTM0
CH
      ENTRY DFTM0(FN,TN,PN)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      FI0=FN
      TE0=TN
      P = PN
      RETURN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTMF
CH
      ENTRY DFTMF
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      CALL DM_FIX_MARKERS(NUMPL,NDSEG,H,V)
      END
*DK DFTMS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTMS
CH
      SUBROUTINE DFTMS(PNEW,NAR,DHW)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      DIMENSION H(244),V(244),NDSEG(2,40)
      DIMENSION X(61,2),Y(61,2),Z1(2),TES(2)
      DATA NS9/9/,NS18/18/,NS19/19/,NS20/20/,NS37/37/,NS38/38/
      DATA NUM/60/,QMX/0.3/,DT/10./,DMIN/7./
      DATA D/20./,RP/50./,W/45./,AL/0./,IVC1/2/,IVC2/348/,IVC3/696/
      DATA Q10/10./,QDT/0.1/,QVA/0.26/
      CHARACTER *15 TDT
      CHARACTER *2 DT2
      LOGICAL FOUT,FPIK,FGC,FCVA
      DATA FPIK/.FALSE./,FCVA/.TRUE./
      LOGICAL FBUT(4)
      CHARACTER *(*) TKBD
      CHARACTER *6 T6
C      EQUIVALENCE (DT,PARADO(2,15)) REMOVED 3.1.93
C      DATA GEX1,GEX2/3.,14/
      DHWI=DHW
      IAR=NAR
      IZOM=ISTODS(6,IAR,IWUSDO)
C     EQUIVALENCE USED INSTEAD:
C     DT=PARADO(2,IFDGDO)
      IF(DT.LE.0.) DT=10.
      TES(1)=90.-DT
      TES(2)=90.+DT
      Z1(2)=COSD(90.+DT)
C          123456789
      TDT=' delta=+-'//DT2(DT)//' deg'
      DO J=1,2
        Z1(J)=COSD(TES(J))
        RO=SIND(TES(J))
        A=0.
        DA=360./NUM
        DO N=1,NUM
          X(N,J)=COSD(A)*RO
          Y(N,J)=SIND(A)*RO
          A=A+DA
        END DO
      END DO
      X(61,1)=X(1,1)
      Y(61,1)=Y(1,1)
      X(61,2)=X(1,2)
      Y(61,2)=Y(1,2)
      N1=NUM+1
      N2=2*N1
      IHLOW=HLOWDG(IAR)
      IHHGH=HHGHDG(IAR)
      IVLOW=VLOWDG(IAR)
      IVHGH=VHGHDG(IAR)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_PPU'
      CALL DPARAM(10
     &  ,J_PFI,J_PTE,J_PPU)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      GEV45=ABS(Q10/PSTODS(1,J_PPU,IAR,IWUSDO))*BVSCDQ/AHSCDQ
      IF(PNEW.NE.0.) THEN
        DH=PNEW/GEV45
        W=DATN2D(1.,DH)
        IF(W.GT.90.) THEN
          W=W-180.
        ELSE IF(W.LT.-90.) THEN
          W=W+180.
        END IF
      END IF
      DH=COSD(W)*RP
      DV=SIND(W)*RP
      P=GEV45*DH/DV
      MXDST=QMX*(VHGHDG(IAR)-VLOWDG(IAR))
      FI1=PSTODS(1,J_PFI,IAR,IWUSDO)
      TE1=PSTODS(1,J_PTE,IAR,IWUSDO)
      TMIN=  1.+DT
      TMAX=179.-DT
      HMIN=CHSCDQ-(180.-DMIN)*AHSCDQ
      HMAX=CHSCDQ-      DMIN *AHSCDQ
      DT8=2.*DT/8.
      CALL DFTV0
      T6=DT2(DT)//' DeG'
C               123456789 123456789 123456789 123456789 123456789
      CALL DWRT('Move mouse! Use mouse buttons:')
      CALL DWRT('No button   : move V and +. + is at antipode.')
      CALL DWRT('Left        : change momentum = V-angle.')
      CALL DWRT('Left+middle : pick track, set V-angle to mentum.')
      CALL DWRT('Middle      : rotate great circle with=+-'//T6)
      CALL DWRT('Middle+right: set width of great circle +-'//T6)
      CALL DWRT('Right       : draw solid angles with = +-'//T6)
      CALL DWRT('Right+middle: set width of solid angles = +-'//T6)
      CALL DWRT('    <CR>    : STOP.  Reenable terminal and help.')
C      WRITE(TXTADW,1000) FI1,TE1,P
C 1000 FORMAT('phi=',F4.0,' theta=',F4.0,F11.2,' GeV >')
C      CALL DWRT(TXTADW)
      CALL DWRT('Move mouse')

      RETURN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTMM
CH
      ENTRY DFTMM(IHC,IVC,H,V,NDSEG,NUMPL,FBUT,TKBD)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C     FBUT=.FALSE. IF BUTTON DOWN
      IF(.NOT.FBUT(4)) RETURN ! DO NOT REACT ON KEYBOARD INPUT
      IHC=MIN(IHHGH,MAX(IHLOW,IHC))
      IF(.NOT.FBUT(1)) THEN
        IVC=MIN(IVC3,MAX(IVC1,IVC))
        IF(.NOT.FBUT(2)) THEN
          IF(FPIK) RETURN
          FPIK=.TRUE.
C         ............................................. GET V ANGLE FROM PICK
          CALL DV0(FRFTDB,NUM1,NUM2,FOUT)
          IF(FOUT) THEN
            P=0.
          ELSE
            CALL DCOPTL
            FPIKDP=.TRUE.
            CALL DPIFT(NDUM,FLOAT(IHC),FLOAT(IVC),NHIT,MODUL,FDUM,TDUM)
            CALL DCOPFL
            FPIKDP=.FALSE.
            CALL DV0TYP(NHIT,MODUL,TRK)
            CALL DWRT(' ')

            NTRK=TRK
            IF(NTRK.GE.NUM1.AND.NTRK.LE.NUM2) THEN
              CALL DVTRTP(IFIX(TRK),P)
              DH=P/GEV45
              W=DATN2D(1.,DH)
              IF(W.GT.90.) THEN
                W=W-180.
              ELSE IF(W.LT.-90.) THEN
                W=W+180.
              END IF
            END IF
          END IF
        ELSE
          FPIK=.FALSE.
C         .................................................... CHANGE V-ANGLE
          IF(FCVA) THEN
            IVC=IVC2+W/QVA
            FCVA=.FALSE.
          END IF
          W=FLOAT(IVC-IVC2)*QVA
        END IF
        W=MAX(-90.,MIN(90.,W))
        DH=COSD(W)*RP
        DV=SIND(W)*RP
        IF(DV.NE.0.) THEN
          P=GEV45*DH/DV
          IF(P.GT.999..OR.P.LT.-999.) P=999.
        ELSE
          P=999.
        END IF
        IF(IZOMDO.EQ.0) THEN
C         ............................................ LINEAR (APPROXIMATED) V
          H(2)=T1-0.5
          H(1)=H(2)-DH
          H(3)=T1+0.5
          H(4)=H(3)+DH
          V(2)=F1
          V(3)=F1
          V(1)=F1+DV
          V(4)=V(1)
          NUMPL=2
          NDSEG(1,1)=1
          NDSEG(1,2)=3
          NDSEG(2,1)=2
          NDSEG(2,2)=2
        ELSE
C         .......................................... CURVED (UNAPPROXIMATED) V
          CALL DFTVC(FI1,TE1,P,H,V,NDSEG,NUMPL)
        END IF

        WRITE(TXTADW,1001) FI1,TE1,P
 1001   FORMAT('phi=',F4.0,' theta=',F4.0,F11.2,' GeV>')
        CALL DWR_OVER_PRINT(35)
        IHC=IHCPR
        GO TO 300
      ELSE IF(.NOT.FBUT(2).AND.IZOM.EQ.0) THEN
        IF(.NOT.FCVA) THEN
          IVC=IVCPR
          FCVA=.TRUE.
        END IF
        IF(.NOT.FBUT(3)) THEN
C         ....................................................... change width
          IVC=MIN(IVHGH,MAX(IVLOW,IVC))
          DT=DT+FLOAT(IVC-IVCPR)*QDT
          DT=MAX(DT,0.1)
          TDT(10:11)=DT2(DT)
          TMAX=179.-DT
          TMIN=  1.+DT
          DT8=2.*DT/8.
          TES(1)=90.-DT
          TES(2)=90.+DT
          DO J=1,2
            Z1(J)=COSD(TES(J))
            RO=SIND(TES(J))
            A=0.
            DA=360./NUM
            DO N=1,NUM
              X(N,J)=COSD(A)*RO
              Y(N,J)=SIND(A)*RO
              A=A+DA
            END DO
          END DO
          X(61,1)=X(1,1)
          Y(61,1)=Y(1,1)
          X(61,2)=X(1,2)
          Y(61,2)=Y(1,2)
          IF(FGC) GO TO 30
          IVC=IVCPR
          IHC=IHCPR
          GO TO 40
        END IF
C       ......................................................... Grand circle
        FGC=.TRUE.
        AL=AL+FLOAT(IVC-IVCPR)
        CALL DFTMC(FI1,TE1,ST1,CT1,SG1,CG1)
   30   IVC=IVCPR
        IHC=IHCPR
        X1=COSD(AL)
        Y1=SIND(AL)
        X4= CT1*X1
        Z3=-ST1*X1
        Y4= CG1*Y1+SG1*Z3
        Z4=-SG1*Y1+CG1*Z3
        FI=DATN2D(Y4,X4)
        TE=ACOSD(Z4)
        WRITE(TXTADW,1002) FI,TE,TDT
 1002   FORMAT('phi=',F4.0,' theta=',F4.0,A,'>')
        CALL DWR_OVER_PRINT(35)
        CALL DFTMC(FI,TE,ST,CT,SG,CG)
        K=0
        DO J=1,2
          NCUT=1
          DO N=1,N1
C           .................................................. X2= CF*X1+SF*Y1
C           .................................................. Y2=-X1*SF+CF*Y1
C           .................................................. Z2= Z1
C           .................................................. X3= CT*X2+ST*Z2
C           .................................................. Y3= Y2
C           .................................................. Z3=-ST*X2+CT*Z2
C           .................................................  X4= X3
C           ................................................   Y4= CG*Y3+SG*Z3
C           .................................................. Z4=-SG*Y3+CG*Z3
C           ..................................................................
C           ....................................... X1=X(N) Y1=Y(N) Z1=Z(N)=0
C           ................................................... SET: CF=1 SF=0
            X4= CT*X(N,J)+ST*Z1(J)
            Z3=-ST*X(N,J)+CT*Z1(J)
            Y4= CG*Y(N,J)+SG*Z3
            Z4=-SG*Y(N,J)+CG*Z3
C           ............................................................... FI
            FN=MOD(ATAN2D(Y4,X4)+3600.,360.)
C           ............................................................... TE
            TN=-ACOSD(Z4)
            K=K+1
            H(K)=TN*AHSCDQ+CHSCDQ
            V(K)=FN*BVSCDQ+CVSCDQ
          END DO
        END DO
  777   NDSEG(1,1)=1
        NUMPL=1
        KK=0
    1   KK=KK+1
        K1=KK+1
        IF(KK.EQ.N1.OR.KK.GE.N2.OR.ABS(V(KK)-V(K1)).GT.MXDST.
     &    OR.H(KK).LT.HMIN.OR.H(KK).GT.HMAX) THEN
          NR=K1-NDSEG(1,NUMPL)
          IF(NR.GT.1) THEN
            NDSEG(2,NUMPL)=NR
            NUMPL=NUMPL+1
          END IF
          IF(KK.GE.N2) GO TO 9
          NDSEG(1,NUMPL)=K1
        END IF
        GO TO 1
    9   NUMPL=NUMPL-1
        GO TO 300
      ELSE
        IF(.NOT.FCVA) THEN
          IVC=IVCPR
          FCVA=.TRUE.
        END IF
        IVC=MIN(IVHGH,MAX(IVLOW,IVC))
        T1=IHC
        F1=IVC
        CALL DQINV(IAR,FLOAT(IHC),FLOAT(IVC),TE1,FI1)
        TE1=-TE1
        FI2=FI1+180.
        IF(FI2.GT.380.) FI2=FI2-360.
        IF(IZOM.NE.0.OR.FBUT(3)) THEN
C         ....................................................... MOVE V AND +
          WRITE(TXTADW,1001) FI1,TE1,P
          CALL DWR_OVER_PRINT(35)
          TE2=180.-TE1
          T2=-TE2*AHSCDQ+CHSCDQ
          F2= FI2*BVSCDQ+CVSCDQ
          H(2)=T1-0.5
          H(1)=H(2)-DH
          H(3)=T1+0.5
          H(4)=H(3)+DH
          V(2)=F1
          V(3)=F1
          V(1)=F1+DV
          V(4)=V(1)
          NDSEG(1,1)=1
          NDSEG(1,2)=3
          NDSEG(2,1)=2
          NDSEG(2,2)=2
          IF(IZOM.EQ.0) THEN
            H(5)=T2
            H(6)=T2
            H(7)=T2-D
            H(8)=T2+D
            V(5)=F2-D
            V(6)=F2+D
            V(7)=F2
            V(8)=F2
            NDSEG(1,3)=5
            NDSEG(1,4)=7
            NDSEG(2,3)=2
            NDSEG(2,4)=2
            NUMPL=4
          ELSE
            NUMPL=2
          END IF
          IHCPR=IHC
          IVCPR=IVC
          GO TO 300
        ELSE
C         ..................................................... move 2 squares
   40     FGC=.FALSE.
          IF(TE1.GT.TMAX.OR.TE1.LT.TMIN) RETURN
          WRITE(TXTADW,1002) FI1,TE1,TDT
          CALL DWR_OVER_PRINT(35)
          T=TE1-DT
          J1=NS18
          I2=NS20
          J2=NS37
          DO I1=1,NS9
            H(I1)=CHSCDQ-AHSCDQ*T
            H(J1)=H(I1)
            H(I2)=CHSCDQ-AHSCDQ*(180.-T)
            H(J2)=H(I2)
            DF=DT/SIND(T)
            V(I1)=CVSCDQ+BVSCDQ*(FI1-DF)
            V(J1)=CVSCDQ+BVSCDQ*(FI1+DF)
            V(I2)=CVSCDQ+BVSCDQ*(FI2-DF)
            V(J2)=CVSCDQ+BVSCDQ*(FI2+DF)
            J1=J1-1
            I2=I2+1
            J2=J2-1
            T=T+DT8
          END DO
          H(NS19)=H(1)
          V(NS19)=V(1)
          H(NS38)=H(NS20)
          V(NS38)=V(NS20)
          NUMPL=2
          NDSEG(1,1)=1
          NDSEG(1,2)=NS20
          NDSEG(2,1)=NS19
          NDSEG(2,2)=NS19
        END IF
      END IF
  300 IF(DHWI.NE.0..AND.NUMPL.GT.0) THEN
        KD=NDSEG(1,NUMPL)+NDSEG(2,NUMPL)-1
        DO K=1,KD
          H(K+KD)=H(K)+DHWI
          V(K+KD)=V(K)
        END DO
        N=NUMPL
        DO K=1,NUMPL
          N=N+1
          NDSEG(1,N)=NDSEG(1,K)+KD
          NDSEG(2,N)=NDSEG(2,K)
        END DO
        NUMPL=N
      END IF
      RETURN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTMM
CH
      ENTRY DFTMG(FM,TM)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
      FM=FI1
      TM=TE1
      END
*DK DFTMC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTMC
CH
      SUBROUTINE DFTMC(FP,TP,STR,CTR,SGR,CGR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      DATA SGS/0./,CGS/1./
      STP=SIND(TP)
      CTP=COSD(TP)
      SFP=SIND(FP)
      CFP=COSD(FP)
      XP=STP*CFP
      YP=STP*SFP
      ZP=CTP
      STR=XP
      CTR=SQRT(1-STR*STR)
      IF(CTR.EQ.0.) THEN
        SGR=SGS
        CGR=CGS
      ELSE
        SGR=YP/CTR
        CGR=ZP/CTR
      END IF
      END
*DK DFTVO
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTVO
CH
      SUBROUTINE DFTV0
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      DIMENSION H(244),V(244),NDSEG(2,40)
      DATA QCP/-0.012883/
      DATA ZMAX/217./,ROMAX/170.622/,RE1/188.7/,ZE1/257.9/
      DATA NP/11/,QNP/0.1/
      DIMENSION FTR(11),TTR(11),BTR(11),RTR(11),XTR(11),YTR(11),ZTR(11)
      DATA PIDEG/57.29577951/,PI/3.1416/,PSET/0.4/
      DIMENSION NPR(12),FRZ(12)
      DIMENSION AHS(12),BHS(12),CHS(12)
      DIMENSION AVS(12),BVS(12),CVS(12)
      CHARACTER *2 TPIC
      DATA DMI/2./,I99/99/
      LOGICAL FLEX
C     .................................................... calculate constants
      DMIN=DMI/(AHSCDQ*PIDEG)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_PDS,J_PPU'
      CALL DPARAM(10
     &  ,J_PFI,J_PTE,J_PDS,J_PPU)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      PU=ABS(QCP*PARADA(2,J_PPU))
      C=-1./(0.003*1.5)
      TC=2.*C
      TCUT1=ATAN2(ROMAX,ZMAX)
      TCUT2=180./PIDEG-TCUT1
      QEC= -ZMAX/(C*PI)
      QBA=-ROMAX/(2.*C)
      PIDGM=-PIDEG
      IPR=0
      DO 700 L=1,12
        IF(ISTODS(4,L,IWUSDO).GT.0) THEN
          IPIC=ISTODS(5,L,IWUSDO)
          TPIC=TPICDP(IPIC)
           IF     (TPIC.EQ.'YX'.AND.
     &       PSTODS(2,J_PDS,L,IWUSDO).LT.0.) THEN
             IPR=IPR+1
             NPR(IPR)=1
             GO TO 1
           ELSE IF(TPIC.EQ.'FR') THEN
             IPR=IPR+1
             NPR(IPR)=2
             GO TO 1
           ELSE IF(TPIC.EQ.'FZ') THEN
             IPR=IPR+1
             NPR(IPR)=3
             GO TO 1
           ELSE IF(TPIC.EQ.'RZ'.AND.
     &       PSTODS(2,J_PTE,L,IWUSDO).EQ.1.) THEN
             IPR=IPR+1
             NPR(IPR)=4
             FRZ(IPR)=PSTODS(1,J_PFI,L,IWUSDO)
             GO TO 1
           END IF
        END IF
        GO TO 700
    1   CALL DQSET(L,0.,0.)
        AHS(IPR)=AHSCDQ
        BHS(IPR)=BHSCDQ
        CHS(IPR)=CHSCDQ
        AVS(IPR)=AVSCDQ
        BVS(IPR)=BVSCDQ
        CVS(IPR)=CVSCDQ
  700 CONTINUE
      IF(IPR.NE.0) CALL DQSET(IAREDO,0.,0.)
      RETURN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTM0
CH
      ENTRY DFTVC(FIN,TIN,P,H,V,NDSEG,NUMPL)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      F1=FIN/PIDEG
      T1=TIN/PIDEG
C
C
C     ......................................................... calculate PMIN
C     ....................................... ROMAX=2*|C|*PMIN*SIN(T1)*SIN(90)
C     ........................................ ZMAX=2*|C|*PMIN*COS(T1)*PI/2
      IF(T1.GT.TCUT1.AND.T1.LT.TCUT2) THEN
        PMIN=MAX(PSET,QBA/SIN(T1))
      ELSE
        PMIN=MAX(PSET,QEC/COS(T1))
      END IF
      IF(ABS(P).LE.PMIN) THEN
        IF(P.GE.0.) THEN
          P= PMIN
        ELSE
          P=-PMIN
        END IF
      END IF
      TCP=2.*C*P
C     .................................................. CALCULATE F0,DF,ST,CT
C     ................................ FI=F0+DF
C     ................................ RO=2*C*P*SIN(T0)*SIN(DF)=TCP*ST*SIN(DF)
C     ................................  Z=2*C*P*COS(T0)*DF     =TCP*CT*DF
C     .......... TE=RO/Z=ARCTG(TCP*ST*SIN(DF)/TCP*CT*DF)=ATAN2(ST*SD/(CT*DF))
C     ..... These equations containing df and sin(df) are solved by iteration.
      QR=ROMAX/TCP
      QRA=ABS(QR)
      QRE1=RE1/TCP
      QRE1A=ABS(QRE1)
      IF(TIN.LT.90.) THEN
        QZ=  ZMAX/TCP
        QZE1= ZE1/TCP
      ELSE
        QZ= -ZMAX/TCP
        QZE1=-ZE1/TCP
      END IF
      T0=T1
      DT=0.
      ITER=0
    2 T0=T0+DT
      ST=SIN(T0)
      CT=COS(T0)
      ITER=ITER+1
      IF(T1.GT.TCUT1.AND.T1.LT.TCUT2) THEN
        IF(QRA.GE.ABS(ST)) THEN
          DF=SIGN(PI,QR*ST)
        ELSE
          DF=ASIN(QR/ST)
        END IF
      ELSE
        DF=QZ/CT
      END IF
      T2=ATAN2(ST*SIN(DF),CT*DF)
      IF(T2.LT.0.) T2=T2+PI
C
C
c      TOUT=T2*PIDEG
c      FOUT=DF*PIDEG
c      TYPE *,'      ',TIN,TOUT,FOUT,P
c      ACCEPT '(A)',TANSW
C
C
      DT=T1-T2
      IF(ITER.LT.I99.AND.ABS(DT).GT.DMIN) GO TO 2
C     ....................................................... End of iteration
      F0=F1-DF
      DDF=DF*QNP
C     .............................................. Calculate np track points
      TCPZ=TCP*CT
      TCPT=TCP*ST
      D=0.
      DO K=2,NP
        D=D+DDF
        FIK=F0+D
        FTR(K)=PIDEG*(FIK)
        SD=SIN(D)
        TTR(K)=PIDGM*(ATAN2(ST*SD,CT*D))
        IF(TTR(K).GE.0.) TTR(K)=TTR(K)-180.
        Z0=TCPZ*D
        ZTR(K)=Z0
        R0=TCPT*SD
        RTR(K)=R0
        XTR(K)=R0*COS(FIK)
        YTR(K)=R0*SIN(FIK)
        ZA=ABS(Z0)
        D0=SQRT(Z0**2+R0**2)
        IF(R0*ZMAX.GT.ZA*ROMAX) THEN
          BTR(K)=PU*D0*(ROMAX/R0-1.)
        ELSE
          BTR(K)=PU*D0*(ZMAX/ZA-1.)
        END IF
      END DO
      FTR(1)=F0*PIDEG
      TTR(1)=T0*PIDGM
      IF(TTR(1).GE.0.) TTR(1)=TTR(1)-180.
      BTR(1)=2.*BTR(2)-BTR(3)
      CHSCL=CHSCDQ-0.5
      CHSCR=CHSCDQ+0.5
      J=11
      DO K=1,11
        J=J+1
        H(K)=(TTR(K)-BTR(K))*AHSCDQ+CHSCL
        H(J)=(TTR(K)+BTR(K))*AHSCDQ+CHSCR
        V(K)= FTR(K)        *BVSCDQ+CVSCDQ
        V(J)=V(K)
      END DO
      DHL=H( 1)-H( 2)
      DHR=H(12)-H(13)
      DVL=V( 1)-V( 2)
      DVR=V(12)-V(13)
      NUMPL=2
      NDSEG(1,1)=1
      NDSEG(1,2)=12
      NDSEG(2,1)=11
      NDSEG(2,2)=11
      IF(COLIDF(4,2).EQ.1.) THEN
        IF(TIN.GT.40..AND.TIN.LT.140.) THEN
          IF(QRE1A.GE.ABS(ST)) THEN
            FLEX=.FALSE.
          ELSE
            FLEX=.TRUE.
            DFEX=ASIN(QRE1/ST)
          END IF
        ELSE
          FLEX=.TRUE.
          DFEX=QZE1/CT
        END IF
        IF(FLEX) THEN
          FEX=(F0+DFEX)*PIDEG
          TEX=ATAN2(ST*SIN(DFEX),CT*DFEX)
          IF(TEX.LT.0.) TEX=TEX+PI
          TEX=TEX*PIDEG
          H(23)=H(11)+0.5
          V(23)=V(11)
          H(24)=-AHSCDQ*TEX+CHSCDQ
          V(24)= BVSCDQ*FEX+CVSCDQ
          NUMPL=3
          NDSEG(1,NUMPL)=23
          NDSEG(2,NUMPL)=2
        END IF
      END IF
  100 IF(IPR.EQ.0) RETURN
      DO 800 I=1,IPR
        NUMPL=NUMPL+1
        NDSEG(1,NUMPL)=NDSEG(1,NUMPL-1)+NDSEG(2,NUMPL-1)
        NDSEG(2,NUMPL)=11
        J=NDSEG(1,NUMPL)
        GO TO (110,120,130,140),NPR(I)
  110   DO N=1,11
          H(J)=AHS(I)*XTR(N)+BHS(I)*YTR(N)+CHS(I)
          V(J)=AVS(I)*XTR(N)+BVS(I)*YTR(N)+CVS(I)
          J=J+1
        END DO
        GO TO 800
  120   DO N=1,11
          H(J)=AHS(I)*RTR(N)+BHS(I)*FTR(N)+CHS(I)
          V(J)=AVS(I)*RTR(N)+BVS(I)*FTR(N)+CVS(I)
          J=J+1
        END DO
        GO TO 800
  130   DO N=1,11
          H(J)=AHS(I)*ZTR(N)+CHS(I)
          V(J)=BVS(I)*FTR(N)+CVS(I)
          J=J+1
        END DO
        GO TO 800
  140   FI=DFINXT(FRZ(I),FIN)
        IF(FRZ(I)-90..LE.FI.AND.FI.LT.FRZ(I)+90.) THEN
          DO N=1,11
            H(J)=AHS(I)*ZTR(N)+BHS(I)*RTR(N)+CHS(I)
            V(J)=AVS(I)*ZTR(N)+BVS(I)*RTR(N)+CVS(I)
            J=J+1
          END DO
        ELSE
          DO N=1,11
            H(J)=AHS(I)*ZTR(N)-BHS(I)*RTR(N)+CHS(I)
            V(J)=AVS(I)*ZTR(N)-BVS(I)*RTR(N)+CVS(I)
            J=J+1
          END DO
        END IF
  800 CONTINUE
      END
*DK DFTPO
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTPO
CH
      SUBROUTINE DFTPO(NAR,H,V,FI,TE)
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
      CALL DQINV(NAR,H,V,TE,FI)
      TE=-TE
C      FI= (V-AVSCDT)/BVSCDT
C      TE=-(H-AHSCDT)/BHSCDT
      RETURN
      END
*DK DFTRBC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTRBC
CH
      SUBROUTINE DFTRBC(HRB,VRB)
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
      FI= 0.5*(VRB(1)+VRB(3))
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PDF,J_PCF,J_PTE,J_PDT,J_PCT'
      CALL DPARAM(11
     &  ,J_PFI,J_PDF,J_PCF,J_PTE,J_PDT,J_PCT)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(FI.LT.PARADA(1,J_PFI).OR.FI.GT.PARADA(3,J_PFI)) RETURN
      TE=-0.5*(HRB(1)+HRB(3))
      IF(TE.LT.PARADA(1,J_PTE).OR.TE.GT.PARADA(3,J_PTE)) RETURN
      DF=ABS(VRB(3)-VRB(1))
      IF(DF.LT.PARADA(1,J_PDF).OR.DF.GT.PARADA(3,J_PDF)) RETURN
      DT=ABS(HRB(2)-HRB(1))
      IF(DT.LT.PARADA(1,J_PDT).OR.DT.GT.PARADA(3,J_PDT)) RETURN
      PARADA(2,J_PFI)=FI
      PARADA(2,J_PTE)=TE
      PARADA(2,J_PDF)=DF
      PARADA(2,J_PDT)=DT
      PARADA(2,J_PCF)=DF
      PARADA(2,J_PCT)=DT
      END
*DK DFTVDS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTVDS
CH
      SUBROUTINE DFTVDS
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
      DATA PIDEG/57.29577951/
      DIMENSION H(2),V(2)
      DATA VZ0/180./,HF0/-90./
      DATA DA10/1./,DA1/.2/,DS/.1/,N8/8/
      DIMENSION ROVD(2,2)
      CALL DGCURG(HC,VC)
      IF(HC.EQ.0.) RETURN
      CALL DPOAR(HC,VC,IAR)
C     ....................................... 1 AND ONLY 1 VD LAYER IS DRAWN ?
      IF(IAR.LT.0)            GO TO 99
      IF(ISTODS(4,IAR,IWUSDO).NE.1 ) GO TO 99
      
      IF(TPICDP(ISTODS(5,IAR,IWUSDO)).NE.'FT') GO TO 99
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PP1,J_PP2'
      CALL DPARAM(11
     &  ,J_PP1,J_PP2)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      LAY1=PSTODS(1,J_PP1,IAR,IWUSDO)-997.
      IF(LAY1.GT.2)           GO TO 99
      LAY2=PSTODS(1,J_PP2,IAR,IWUSDO)-997.
      IF(LAY1.EQ.1.AND.LAY2.EQ.2) GO TO 99
      CALL DQSET(IAR,0.,0.)
      CALL DQINV(IAR,HC,VC,HF0,VZ0)
      CALL DGLEVL(N8)
      CALL DGIVDR(ROVD)
      RO=0.5*(ROVD(1,LAY1)+ROVD(2,LAY1))
      DZ=DS
      DO L=1,2
        Z=-VRDZDV
        DO N=0,100
          H(L)=-DATN2D(RO, Z)
          IF(MOD(N,10).EQ.0) THEN
            D=DA10
          ELSE
            D=DA1
          END IF
          CALL DQL2E(H(L),VZ0-D,H(L),VZ0+D)
          Z=Z+DZ
        END DO
        DZ=-DZ
      END DO
      CALL DQL2E(H(1),VZ0,H(2),VZ0)
      DF=PIDEG*DS/RO
      DO L=1,2
        V(L)=VZ0
        DO N=1,100
          V(L)=V(L)+DF
          IF(MOD(N,10).EQ.0) THEN
            D=DA10
          ELSE
            D=DA1
          END IF
          CALL DQL2E(HF0-D,V(L),HF0+D,V(L))
        END DO
        DF=-DF
      END DO
      CALL DQL2E(HF0,V(1),HF0,V(2))
      RETURN
   99 CALL DWRT('Vertex detector scale cannot be drawn.')
      CALL DWRT('Did you move the cursor to the right position?#')
      END
*DK DFZMO
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFZMO
CH
      SUBROUTINE DFZMO(NAR,FI,H,V,FIN)
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
      LOGICAL FIN
      CALL DQSET(NAR,0.,0.)
      CALL DQINV(NAR,
     &  HLOWDG(IAREDO)+1.,0.5*(VLOWDG(IAREDO)+VHGHDG(IAREDO)),
     &  Z,FIMID)
      FFI=DFINXT(FIMID,FI)
      CALL DQPOC(Z,FFI,HH,VV,FIN)
      IF(FIN) THEN
         H=HLOWDG(IAREDO)
         V=VV
      END IF
      END
*DK DFZPO
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFZPO
CH
      SUBROUTINE DFZPO(NAR,V,FI,FMES)
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
      LOGICAL FMES
      CALL DQINV(NAR,HLOWDG(IAREDO),V,Z,FI)
      FMES=.TRUE.
      END
*DK DFZRBC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFZRBC
CH
      SUBROUTINE DFZRBC(HRB,VRB)
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
      DATA P89/89.9999/,P91/90.0001/
      H1=ABS(HRB(1))
      H3=ABS(HRB(3))
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PDF,J_PTE,J_PFR,J_PTO'
      CALL DPARAM(11
     &  ,J_PFI,J_PDF,J_PTE,J_PFR,J_PTO)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(H1.LT.H3) THEN
        PARADA(2,J_PTO)=H3
        IF(HRB(3).GT.0.) THEN
          PARADA(2,J_PFR)=HRB(1)
          IF(PARADA(2,J_PTE).GT.90.) THEN
            PARADA(2,J_PTE)=180.-PARADA(2,J_PTE)
          ELSE IF(PARADA(2,J_PTE).EQ.90.) THEN
            PARADA(2,J_PTE)=P89
          END IF
        ELSE
          PARADA(2,J_PFR)=-HRB(1)
          IF(PARADA(2,J_PTE).LT.90.) THEN
            PARADA(2,J_PTE)=180.-PARADA(2,J_PTE)
          ELSE IF(PARADA(2,J_PTE).EQ.90.) THEN
            PARADA(2,J_PTE)=P91
          END IF
        END IF
      ELSE
        PARADA(2,J_PTO)=H1
        IF(HRB(1).GT.0.) THEN
          PARADA(2,J_PFR)=HRB(3)
          IF(PARADA(2,J_PTE).GT.90.) THEN
            PARADA(2,J_PTE)=180.-PARADA(2,J_PTE)
          ELSE IF(PARADA(2,J_PTE).EQ.90.) THEN
            PARADA(2,J_PTE)=P89
          END IF
        ELSE
          PARADA(2,J_PFR)=-HRB(3)
          IF(PARADA(2,J_PTE).LT.90.) THEN
            PARADA(2,J_PTE)=180.-PARADA(2,J_PTE)
          ELSE IF(PARADA(2,J_PTE).EQ.90.) THEN
            PARADA(2,J_PTE)=P91
          END IF
        END IF
      END IF
      PARADA(2,J_PFI)=0.5*(VRB(1)+VRB(3))
      PARADA(2,J_PDF)=DMIX(PARADA(1,J_PDF),
     &  ABS(VRB(1)-VRB(3)), PARADA(3,J_PDF))
      END
