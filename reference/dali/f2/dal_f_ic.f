*DK DFR_HV_TO_CONE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DFR_HV_TO_CONE
CH
      SUBROUTINE DFR_HV_TO_CONE(HRB,VRB)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C ---------------------------------------------------------------------
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION HRB(4),VRB(4)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PDF,J_PFR,J_PTO'
      CALL DPARAM(11
     &  ,J_PFI,J_PDF,J_PFR,J_PTO)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      PARADA(2,J_PFI)=0.5*(VRB(3)+VRB(1))
      PARADA(2,J_PDF)= ABS(VRB(3)-VRB(1))
      PARADA(2,J_PFR)= MIN(HRB(1),HRB(3))
      PARADA(2,J_PTO)= MAX(HRB(1),HRB(3))
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------  DFR_CONE_TO_HV
CH
      ENTRY DFR_CONE_TO_HV(HRB,VRB)
CH --------------------------------------------------------------------
CH
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PDF,J_PFR,J_PTO'
      CALL DPARAM(11
     &  ,J_PFI,J_PDF,J_PFR,J_PTO)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      DF=0.5*PARADA(2,J_PDF)
      VRB(1)=PARADA(2,J_PFI)-DF
      VRB(3)=PARADA(2,J_PFI)+DF
      HRB(1)=PARADA(2,J_PFR)
      HRB(3)=PARADA(2,J_PTO)
      CALL DQ24(HRB,VRB)
      END

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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
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

*DK DFT_HV_TO_CONE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DFT_HV_TO_CONE
CH
      SUBROUTINE DFT_HV_TO_CONE(HRB,VRB)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C ---------------------------------------------------------------------
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION HRB(4),VRB(4)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PDF,J_PCF,J_PTE,J_PDT,J_PCT'
      CALL DPARAM(11
     &  ,J_PFI,J_PDF,J_PCF,J_PTE,J_PDT,J_PCT)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      PARADA(2,J_PTE)=-0.5*(HRB(3)+HRB(1))
      PARADA(2,J_PFI)= 0.5*(VRB(3)+VRB(1))
      PARADA(2,J_PDT)= ABS(HRB(3)-HRB(1))
      PARADA(2,J_PDF)= ABS(VRB(3)-VRB(1))
      PARADA(2,J_PCT)=PARADA(2,J_PDT)
      PARADA(2,J_PCF)=PARADA(2,J_PDF)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------  DFT_CONE_TO_HV
CH
      ENTRY DFT_CONE_TO_HV(HRB,VRB)
CH
CH --------------------------------------------------------------------
CH
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PDF,J_PTE,J_PDT'
      CALL DPARAM(11
     &  ,J_PFI,J_PDF,J_PTE,J_PDT)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      DF=0.5*PARADA(2,J_PDF)
      DT=0.5*PARADA(2,J_PDT)
      HRB(1)=-PARADA(2,J_PTE)-DT
      HRB(3)=-PARADA(2,J_PTE)+DT
      VRB(1)= PARADA(2,J_PFI)-DF
      VRB(3)= PARADA(2,J_PFI)+DF
      CALL DQ24(HRB,VRB)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      PARAMETER (MAXPDP=120,M11PDP=131)
      COMMON /DPICT/ TPICDP(-10:MAXPDP)
      CHARACTER *2 TPICDP
C------------------------------------------------------------------- DS
      PARAMETER (MPARDS=12,MVIRDS=-4,MPNPDS=200,MWUSDS=4)
      COMMON /DSTRC1/ IAUTDS,ISTODS(8,MVIRDS:MPARDS,0:MWUSDS)
      COMMON /DSTRC2/PSTODS(2,MPNPDS,MVIRDS:MPARDS,0:MWUSDS)
      COMMON /DSTRC3/TVRUDS(6,MVIRDS:MPARDS,0:MWUSDS)
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DS
      PARAMETER (MPARDS=12,MVIRDS=-4,MPNPDS=200,MWUSDS=4)
      COMMON /DSTRC1/ IAUTDS,ISTODS(8,MVIRDS:MPARDS,0:MWUSDS)
      COMMON /DSTRC2/PSTODS(2,MPNPDS,MVIRDS:MPARDS,0:MWUSDS)
      COMMON /DSTRC3/TVRUDS(6,MVIRDS:MPARDS,0:MWUSDS)
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DQ
      COMMON /DQQQ1C/ AHSCDQ,BHSCDQ,CHSCDQ,AVSCDQ,BVSCDQ,CVSCDQ
      COMMON /DQQQ2C/ DMAXDQ
      COMMON /DQQQ3C/ FPRSDQ,FPSQDQ
      LOGICAL FPRSDQ,FPSQDQ
      COMMON /DQQQ4C/ PRSHDQ,PRSVDQ,PRS1DQ,PRS2DQ,PRV1DQ
      COMMON /DQQQ5C/ HUSRDQ(4,0:12),VUSRDQ(4,0:12)
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DW
      COMMON /DWORKT/ TXTADW
      CHARACTER *80 TXTADW
      CHARACTER *1 TXT1DW(80)
      EQUIVALENCE (TXTADW,TXT1DW)
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
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
        END IF
      END IF

C     ..................................................... move 2 squares
   40 FGC=.FALSE.
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
C     INCLUDE 'DALI_CF.INC'
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DQ
      COMMON /DQQQ1C/ AHSCDQ,BHSCDQ,CHSCDQ,AVSCDQ,BVSCDQ,CVSCDQ
      COMMON /DQQQ2C/ DMAXDQ
      COMMON /DQQQ3C/ FPRSDQ,FPSQDQ
      LOGICAL FPRSDQ,FPSQDQ
      COMMON /DQQQ4C/ PRSHDQ,PRSVDQ,PRS1DQ,PRS2DQ,PRV1DQ
      COMMON /DQQQ5C/ HUSRDQ(4,0:12),VUSRDQ(4,0:12)
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DS
      PARAMETER (MPARDS=12,MVIRDS=-4,MPNPDS=200,MWUSDS=4)
      COMMON /DSTRC1/ IAUTDS,ISTODS(8,MVIRDS:MPARDS,0:MWUSDS)
      COMMON /DSTRC2/PSTODS(2,MPNPDS,MVIRDS:MPARDS,0:MWUSDS)
      COMMON /DSTRC3/TVRUDS(6,MVIRDS:MPARDS,0:MWUSDS)
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DP
      PARAMETER (MAXPDP=120,M11PDP=131)
      COMMON /DPICT/ TPICDP(-10:MAXPDP)
      CHARACTER *2 TPICDP
C------------------------------------------------------------------- DF
      PARAMETER (MCOLDF=10)
      COMMON /DFTCLC/ COLIDF(4,MCOLDF),COLMDF(4,3)
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
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
C     INCLUDE 'DALI_CF.INC'
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DS
      PARAMETER (MPARDS=12,MVIRDS=-4,MPNPDS=200,MWUSDS=4)
      COMMON /DSTRC1/ IAUTDS,ISTODS(8,MVIRDS:MPARDS,0:MWUSDS)
      COMMON /DSTRC2/PSTODS(2,MPNPDS,MVIRDS:MPARDS,0:MWUSDS)
      COMMON /DSTRC3/TVRUDS(6,MVIRDS:MPARDS,0:MWUSDS)
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DP
      PARAMETER (MAXPDP=120,M11PDP=131)
      COMMON /DPICT/ TPICDP(-10:MAXPDP)
      CHARACTER *2 TPICDP
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DV
      COMMON /DVRTDV/ FNEVDV,MOCADV,FVXYDV,FVZZDV,VRTXDV,VRTYDV,VRTZDV,
     &  VRDZDV,QTETDV
      LOGICAL FVXYDV,FVZZDV,FNEVDV
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
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

C*DK DFZ_HV_TO_CONE
CCH..............+++
CCH
CCH
CCH
CCH
CCH
CCH
CCH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DFZ_HV_TO_CONE
CCH
C      SUBROUTINE DFZ_HV_TO_CONE(HRB,VRB)
CCH
CCH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CCH
CC ---------------------------------------------------------------------
CC
CC    Created by H.Drevermann                   28-JUL-1988
CC
CC ---------------------------------------------------------------------
C*CA DALLCO
C      DIMENSION HRB(4),VRB(4)
C
C      CALL DPAR_GET_VALUE(11,'PTE',PTE,IDUM)
C
C      IF(ABS(HRB(1)).LT.ABS(HRB(3)) ) THEN           ! H3 -> TO
C        IF(HRB(3).GT.0.) THEN
C          PTO= HRB(3)
C          PFR= HRB(1)
C          PTE=90.-ABS(90.-PTE)
C        ELSE
C          PTO=-HRB(3)
C          PFR=-HRB(1)
C          PTE=90.+ABS(90.-PTE)
C        END IF
C      ELSE                                          ! H1 -> TO
C        IF(HRB(1).GT.0.) THEN
C          PTO= HRB(1)
C          PFR= HRB(3)
C          PTE=90.-ABS(90.-PTE)
C        ELSE
C          PTO=-HRB(1)
C          PFR=-HRB(3)
C          PTE=90.+ABS(90.-PTE)
C        END IF
C      END IF
C      PFI=0.5*(VRB(1)+VRB(3))
C      PDF= ABS(VRB(1)-VRB(3))
C
C      CALL DPAR_SET_ANGLE(11,'PFI',2,PFI)     ! no limit but angle
C      CALL DPAR_SET_VALUE(11,'PTE',2,PTE)     ! limit between 1 and 3
C      CALL DPAR_SET_VALUE(11,'PDF',2,PDF)
C      CALL DPAR_SET_VALUE(11,'PFR',2,PFR)
C      CALL DPAR_SET_VALUE(11,'PTO',2,PTO)
C
C      RETURN
CCH..............---
CCH
CCH
CCH
CCH
CCH
CCH
CCH -------------------------------------------------------  DFZ_CONE_TO_HV
CCH
C      ENTRY DFZ_CONE_TO_HV(HRB,VRB)
CCH
CCH --------------------------------------------------------------------
CCH
C      CALL DPAR_GET_VALUE(11,'PFI',PFI,IDUM)
C      CALL DPAR_GET_VALUE(11,'PDF',PDF,IDUM)
C      CALL DPAR_GET_VALUE(11,'PTE',PTE,IDUM)
C      CALL DPAR_GET_VALUE(11,'PFR',PFR,IDUM)
C      CALL DPAR_GET_VALUE(11,'PTO',PTO,IDUM)
C
C      DF=0.5*PDF
C      VRB(1)=PFI-DF
C      VRB(3)=PFI+DF
C      IF(PTE.LE.90.) THEN
C        HRB(1)= PFR
C        HRB(3)= PTO
C      ELSE
C        HRB(1)=-PTO
C        HRB(3)=-PFR
C      END IF
C      CALL DQ24(HRB,VRB)
C      END

*DK DFZ_HV_TO_CONE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DFZ_HV_TO_CONE
CH
      SUBROUTINE DFZ_HV_TO_CONE(HRB,VRB)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C ---------------------------------------------------------------------
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION HRB(4),VRB(4)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PDF,J_PTE,J_PFR,J_PTO'
      CALL DPARAM(11
     &  ,J_PFI,J_PDF,J_PTE,J_PFR,J_PTO)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(ABS(HRB(1)).LT.ABS(HRB(3))) THEN           ! H3 -> TO
        IF(HRB(3).GT.0.) THEN
          PARADA(2,J_PTO)= HRB(3)
          PARADA(2,J_PFR)= HRB(1)
          PARADA(2,J_PTE)=90.-ABS(90.-PARADA(2,J_PTE))
        ELSE
          PARADA(2,J_PTO)=-HRB(3)
          PARADA(2,J_PFR)=-HRB(1)
          PARADA(2,J_PTE)=90.+ABS(90.-PARADA(2,J_PTE))
        END IF
      ELSE                                          ! H1 -> TO
        IF(HRB(1).GT.0.) THEN
          PARADA(2,J_PTO)= HRB(1)
          PARADA(2,J_PFR)= HRB(3)
          PARADA(2,J_PTE)=90.-ABS(90.-PARADA(2,J_PTE))
        ELSE
          PARADA(2,J_PTO)=-HRB(1)
          PARADA(2,J_PFR)=-HRB(3)
          PARADA(2,J_PTE)=90.+ABS(90.-PARADA(2,J_PTE))
        END IF
      END IF
      PARADA(2,J_PFI)=0.5*(VRB(1)+VRB(3))
      PARADA(2,J_PDF)= ABS(VRB(1)-VRB(3))
      PARADA(2,J_PDF)=MAX(PARADA(2,J_PDF),PARADA(1,J_PDF))
      PARADA(2,J_PDF)=MIN(PARADA(2,J_PDF),PARADA(3,J_PDF))
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------  DFZ_CONE_TO_HV
CH
      ENTRY DFZ_CONE_TO_HV(HRB,VRB)
CH --------------------------------------------------------------------
CH
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PDF,J_PTE,J_PFR,J_PTO'
      CALL DPARAM(11
     &  ,J_PFI,J_PDF,J_PTE,J_PFR,J_PTO)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      DF=0.5*PARADA(2,J_PDF)
      VRB(1)=PARADA(2,J_PFI)-DF
      VRB(3)=PARADA(2,J_PFI)+DF
      IF(PARADA(2,J_PTE).LE.90.) THEN
        HRB(1)= PARADA(2,J_PFR)
        HRB(3)= PARADA(2,J_PTO)
      ELSE
        HRB(1)=-PARADA(2,J_PTO)
        HRB(3)=-PARADA(2,J_PFR)
      END IF
      CALL DQ24(HRB,VRB)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
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
*DK DF180
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  DF180
CH
      SUBROUTINE DF180
CH
CH ********************************************************************
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:  DRAW LINES AT 180 AND 360 DEGREES
C ---------------------------------------------------------------------
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      IF(FPIKDP.OR.IZOMDO.NE.0.OR.PDCODD(4,ICLFDD).NE.1.) RETURN
      DLINDD=PDCODD(2,LIGLDD)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI'
      CALL DPARAM(11
     &  ,J_PFI)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      FIMID=DFINXT(180.,PARADA(2,J_PFI)+90.)
      CALL DQLEVL(ICLFDD)
      CALL DQL2E(-200.,FIMID-180.,400.,FIMID-180.)
      CALL DQL2E(-200.,FIMID     ,400.,FIMID     )
      CALL DQL2E(-200.,FIMID+180.,400.,FIMID+180.)
      DLINDD=PDCODD(2,LITRDD)
      END
*DK DFINXT
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  DFINXT
CH
      FUNCTION DFINXT(F1,F2)
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
C     DFINEXT=F2-360,F2 or F2+360 whichever is nearest to F1.
      IF(F2.LE.F1) THEN
         F3=F2+360.
         IF((F1-F2).LE.(F3-F1)) F3=F2
      ELSE
         F3=F2-360.
         IF((F2-F1).LE.(F1-F3)) F3=F2
      END IF
      DFINXT=F3
      RETURN
      END
*DK DFR1TR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFR1TR
CH
      SUBROUTINE DFR1TR
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DT
      PARAMETER (MTRKDT=401,MMCTDT=500)
      COMMON /DTRK1C/ FCUTDT,FNOTDT(   0   :MTRKDT),
     &                FCUHDT,FNOHDT(-MTRKDT:MTRKDT)
      LOGICAL FCUTDT,FNOTDT,FCUHDT,FNOHDT
      COMMON /DTRK2C/ VTX1DT(3),VTX2DT(3),PTRADT(3),CLTMDT(3),FVTXDT
      LOGICAL FVTXDT
      COMMON /DTRK6C/ FRFTDT(6),XYZVDT(3)
      COMMON /DTRK7C/ N3DIDT,FZTRDT,NSVXDT,KINVDT(2,MTRKDT)
      LOGICAL FZTRDT
      COMMON /DTRK3C/ MODEDT,TPHEDT(5)
      COMMON /DTRK4C/ FMCCDT,FMNTDT(MMCTDT),FMVXDT(100)
      LOGICAL FMCCDT,FMNTDT,FMVXDT
      COMMON /DTRK5C/ SZVXDT,NCVXDT
      COMMON /DTRK1T/ TGFTDT,TGTLDT,TGCLDT
      CHARACTER *4 TGFTDT,TGTLDT,TGCLDT
C PARAMETER (MTRKDT=100,MMCTDT=500) is found in:
C C_DALBC.FOR,C_DALBD.FOR,C_DALBH.FOR,C_DALBM.FOR,C_DALBR.FOR,C_DALBT.FOR
C C_DALBV.FOR,C_DALBX.FOR,C_DALBY.FOR,C_DALIA.FOR,C_DALID.FOR,C_DALIR.FOR
C C_DALIS.FOR,C_DALIV.FOR,N_DALBA.FOR,N_DALBF.FOR,US3.FOR
C------------------------------------------------------------------- DO
      COMMON /DOPR4C/ NOARDO,
     1  NTF1DO,NSP1DO,IRZ1DO,IHTRDO(8)
      COMMON /DOPR5C/ FGETDO
      LOGICAL FGETDO
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      EXTERNAL DFRDIS
      DIMENSION FFRTO(7)
C                VX VD IT T0 T3 E1 E3
      DATA FFRTO/0.,0.,3.,4.,5.,6.,7./
      LOGICAL FBAR(2),FOUT
      LOGICAL DCCTR
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PTE,J_PTO'
      CALL DPARAM(11
     &  ,J_PTE,J_PTO)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      TEMID=PARADA(2,J_PTE)
      IF(MODEDT.NE.2) THEN
         CALL=DHELX0(FFRTO(2),FFRTO(3),FFRTO(4),FFRTO(6),FBAR)
      ELSE
         FFRTO(2)=0.
         CALL=DHELX1(FFRTO(3),FFRTO(4),FFRTO(6),FBAR,FOUT)
         IF(FOUT) RETURN
      END IF
      IF(DCCTR(FFRTO(4),FFRTO(5))) RETURN
      IHTFR=MAX(MIN(IHTRDO(3),7),3)
      IHTTO=MIN(IHTRDO(4),7)
      IF(IHTFR.GE.IHTTO) RETURN
      F1=FFRTO(IHTFR)
      H1=DHELIX(F1,IVRODV)
      V1=DHELIX(F1,IVFIDV)
      F2=FFRTO(IHTTO)
      IF(PARADA(2,J_PTO).GE.1001.AND.
     &  PARADA(2,J_PTO).LE.1004.)THEN
         F2=MIN(F2,FFRTO(5))
      ELSE
         IF(.NOT.FBAR(1)) THEN
            F2=MIN(F2,FFRTO(5))
         END IF
      END IF
      IF(F1.GE.F2) RETURN
      H2=DHELIX(F2,IVRODV)
      V2=DHELIX(F2,IVFIDV)
      VMIN=MIN(V1,V2)
      V1=DFINXT(VMIN,V1)
      V2=DFINXT(VMIN,V2)
      CALL DFRDS0(VMIN,TEMID)
      CALL DDRAWA(DFRDIS,F1,H1,V1,F2,H2,V2)
      END
*DK DFRCC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFRCC
CH
      SUBROUTINE DFRCC(FIMID,SK,SP,TEMID)
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
C---Routine to connect ITC and TPC tracks(phi-rho projection).-ecb
c---o  r each track, connect last ITC coor. to first TPC coor.
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DR
      COMMON /DRNG1C/
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      LOGICAL
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      COMMON /DRNG2C/DET1DR(6),DET2DR(6)
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DT
      PARAMETER (MTRKDT=401,MMCTDT=500)
      COMMON /DTRK1C/ FCUTDT,FNOTDT(   0   :MTRKDT),
     &                FCUHDT,FNOHDT(-MTRKDT:MTRKDT)
      LOGICAL FCUTDT,FNOTDT,FCUHDT,FNOHDT
      COMMON /DTRK2C/ VTX1DT(3),VTX2DT(3),PTRADT(3),CLTMDT(3),FVTXDT
      LOGICAL FVTXDT
      COMMON /DTRK6C/ FRFTDT(6),XYZVDT(3)
      COMMON /DTRK7C/ N3DIDT,FZTRDT,NSVXDT,KINVDT(2,MTRKDT)
      LOGICAL FZTRDT
      COMMON /DTRK3C/ MODEDT,TPHEDT(5)
      COMMON /DTRK4C/ FMCCDT,FMNTDT(MMCTDT),FMVXDT(100)
      LOGICAL FMCCDT,FMNTDT,FMVXDT
      COMMON /DTRK5C/ SZVXDT,NCVXDT
      COMMON /DTRK1T/ TGFTDT,TGTLDT,TGCLDT
      CHARACTER *4 TGFTDT,TGTLDT,TGCLDT
C PARAMETER (MTRKDT=100,MMCTDT=500) is found in:
C C_DALBC.FOR,C_DALBD.FOR,C_DALBH.FOR,C_DALBM.FOR,C_DALBR.FOR,C_DALBT.FOR
C C_DALBV.FOR,C_DALBX.FOR,C_DALBY.FOR,C_DALIA.FOR,C_DALID.FOR,C_DALIR.FOR
C C_DALIS.FOR,C_DALIV.FOR,N_DALBA.FOR,N_DALBF.FOR,US3.FOR
C------------------------------------------------------------------- DC
      PARAMETER (MCTRDC=1000)
      COMMON /DCOTR/ FDTRDC,FVTRDC,NCTRDC(0:MCTRDC)
      LOGICAL FDTRDC,FVTRDC
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION H(2),V(2)
      EQUIVALENCE (K,KPIKDP)
      LOGICAL FOUT
      IF(FPIKDP) RETURN
      IF((.NOT.FITCDR).OR.(.NOT.FTPCDR)) RETURN
      CALL DSCTR
      CALL DV0(FRTLDB,NUM1,NTRK,FOUT)
      IF(FOUT) RETURN
      CALL DV0(ITCODB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
      CALL DV0(TPCODB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
C      CALL DLHITR('TR',MTPCDL,ICOL)
      DO 3 M=1,NTRK
         IF(FCUTDT) THEN
            IF(FNOTDT(M)) GO TO 3
         END IF
c---i     nd numbers of ITC and TPC coordinates for track M
         CALL DVCHCC(M,K1,K2,NFI)
         IF(K1.EQ.0) GO TO 3
C         CALL DLCOLT(M,NCOL)
C         IF(NCOL.EQ.0) GO TO 3
C         IF(ICOL.NE.0) CALL DGLEVL(ICOL)
         IF(FVTRDC) CALL DGLEVL(NCTRDC(M))
c---o     ad itc coordinate into H(1),V(1)
         I=0
         TE=DVIT(IVTEDV,K2,NFI)
         IF(TE.LT.TC1.OR.TE.GT.TC2) THEN
            I=0
            GO TO 1
         END IF
         FII=DFINXT(FIMID,DVIT(IVFIDV,K2,NFI))
         IF(FII.LT.FC1.OR.FII.GT.FC2) THEN
            I=0
            GO TO 1
         END IF
         I=I+1
         H(I)=DVIT(IVRODV,K2,NFI)
         V(I)=FII
C         IF(SK.EQ.0.) THEN
C            V(I)=FII
C         ELSE
C            V(I)=FII+SK*(TE-TEMID)
C         END IF
C         IF(SP.NE.0.) THEN
C            H(I)=H(I)*(1.+SP*(DVIT(IVTEDV,K2)-TEMID))
C         END IF

c---o     ad tpc coordinate into H(2),V(2)

         FIT=DFINXT(FII,DVTP(IVFIDV,K1))
C         FI=DFINXT(FIT,DVTP(IVFIDV,K1))
C         IF(FI.LT.FC1.OR.FI.GT.FC2) THEN
C            I=0
C            GO TO 1
C         END IF
         I=I+1
         H(I)=DVTP(IVRODV,K1)
         V(I)=FIT
C         IF(SK.EQ.0.) THEN
C            V(I)=FIT
C         ELSE
C            V(I)=FIT+SK*(TE-TEMID)
C         END IF
C         IF(SP.NE.0.) THEN
C            H(I)=H(I)*(1.+SP*(TE-TEMID))
C         END IF
C         IF(FPIKDP) THEN
C            CALL DQPIK(H,V)
C            CALL DQPIK(H(2),V(2))
C            GO TO 1
C         END IF
         IF(I.EQ.2) THEN
            CALL DQLIE(H,V)
            IF(IZOMDO.EQ.0) THEN
               CALL DQL2E(H(1),V(1)+360.,H(2),V(2)+360.)
               CALL DQL2E(H(1),V(1)-360.,H(2),V(2)-360.)
            END IF
         END IF
    1 CONTINUE
    3 CONTINUE
      END
*DK DFRD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFRD
CH
      SUBROUTINE DFRD
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DU
      PARAMETER (MPNPDU=60)
      REAL DACUDU
      COMMON /DUSDAC/ NUNIDU,
     1 WAITDU,ECCHDU,BOXSDU,ECSQDU,HCSQDU,
     1 HIMXDU,RESCDU,SLOWDU,WISUDU,SETTDU,
     1 USLVDU,SYLRDU,SDATDU,RZDHDU,ROTADU,
     1 SCMMDU,CLMMDU,CUMMDU,WIMIDU,CCMIDU,
     1 ECMIDU,HCMIDU,SHTFDU(2)    ,FTDZDU,
     1 FRNLDU,DACUDU,SHECDU,SHHCDU,DRLIDU,
     1 SQLCDU,CHLCDU,HEADDU,VRDZDU,CHHCDU,
     1 AREADU,DFWIDU(0:1)  ,DFLGDU,PBUGDU,
     1 CHTFDU,RNCLDU,PR43DU,PR44DU,PR45DU,
     1 DSOFDU,TPOFDU,ROVDDU,ROECDU,ROHCDU,
     1 HSTRDU,REALDU,PR53DU,PR54DU,PR55DU,
     1 PR56DU(4),                  PR60DU
      DIMENSION PARADU(MPNPDU)
      EQUIVALENCE (PARADU,WAITDU)
      COMMON /DUSDA2/PLIMDU(2,MPNPDU)
      COMMON /DUSDAT/ TPR1DU(MPNPDU)
      CHARACTER *2 TPR1DU
C------------------------------------------------------------------- DR
      COMMON /DRNG1C/
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      LOGICAL
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      COMMON /DRNG2C/DET1DR(6),DET2DR(6)
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DO
      COMMON /DOPR4C/ NOARDO,
     1  NTF1DO,NSP1DO,IRZ1DO,IHTRDO(8)
      COMMON /DOPR5C/ FGETDO
      LOGICAL FGETDO
C------------------------------------------------------------------- DP
      COMMON /DPERSC/ FRPRDP,FBUGDP,FFDRDP
      LOGICAL FRPRDP,FBUGDP,FFDRDP
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DL
      PARAMETER (MPNPDL=62,MPDVDL=6,MPWDDL=8,MPRADL=8)
      COMMON /DLUTSC/MNUMDL,MVDEDL,MITCDL,MTPCDL,MECADL,MHCADL,MMDEDL,
     1  MGRCDL(MPDVDL),MVARDL(MPDVDL,2),MODEDL(MPDVDL,2),
     1  MCOLDL(MPDVDL),MSYMDL(MPDVDL,0:1),
     1  MLABDL(MPDVDL),MDUMDL,
     1  WDSNDL(4,MPWDDL,MPDVDL),RAMPDL(4,MPRADL,MPDVDL,2)
C     COMMON /DLUTSC/MNUMDL,MVDEDL,MITCDL,MTPCDL,MECADL,MHCADL,MMDEDL,
C    1  MGRCDL(MPDVDL),
C    1  MCOLDL(MPDVDL),
C    1  MLABDL(MPDVDL),MDUMDL
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C------------------------------------------------------------------- DW
      PARAMETER (MPOSDW=30,MPNWDW=12,MNUWDW=14)
      COMMON /DWINDC/NWINDW(0:MPNWDW,0:MPNWDW),
     &  NEXTDW(0:MPNWDW),LASTDW(0:MPNWDW),
     &  POSIDW(MPOSDW)
      COMMON /DWINDO/TWINDW(0:MNUWDW)
      CHARACTER *1 TWINDW
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      INCLUDE 'DALI_EX.INC'
      CHARACTER *3 DT3
      CHARACTER *5 DT5,TPT
      DATA QPTT/10./,DPTH/2./,DPTV1/3./,DPTV2/17./DVAR/28./,L1/1/
      DATA DEB/0./
      LOGICAL FEMTY
      DIMENSION HRB(4),VRB(4)
      DIMENSION SCA(4)
      DATA LDEB/0/
      DATA QPT/1./,L8/8/
      CALL DQCL(IAREDO)
      CALL DQWIL(MOD(DFWIDU(IZOMDO),10.))
      CALL DDRFLG
      IF(FNOPDR) GO TO 99
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_PFR,J_PTO,J_PPT'
      CALL DPARAM(11
     &  ,J_PFI,J_PTE,J_PFR,J_PTO,J_PPT)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      RFR=MAX(0.,PARADA(2,J_PFR))
      RTO=PARADA(2,J_PTO)
      CALL DGRBST(HRB,VRB,FEMTY)
      IF(.NOT.FEMTY) THEN
        CALL DGRBSC(HRB,VRB,SCA,HM3,VM3,ADUM)
        IZOMDO=0
      ELSE
        IF(IZOMDO.EQ.0) THEN
          CALL DQRER(0,RFR,-1.,RTO,400.,HRB,VRB)
        ELSE
          CALL DFRRCB(HRB,VRB)
          TEMID=PARADA(2,J_PTE)
          FI=PARADA(2,J_PFI)
        END IF
        IF(PARADA(4,J_PPT).EQ.1.) THEN
          SKEW=QPT*PARADA(2,J_PPT)
          VRB(1)=VRB(1)+SKEW
          VRB(2)=VRB(2)-SKEW
          VRB(3)=VRB(3)-SKEW
          VRB(4)=VRB(4)+SKEW
        END IF
      END IF
      CALL DQRU(HRB,VRB)
      CALL DHTINP
      CALL DCTYP0(1)
      IF(IHTRDO(1).EQ.0) THEN
         CALL DFRET1(FI,SK,SP,TEMID)
         CALL DQFR(IAREDO)
         RETURN
      END IF
      IF(LDEB.EQ.9.AND.FRPRDP) GO TO 391
      CALL DODVDT(' FR')
      CALL DODITC(' FR')
      CALL DODTPC(' FR')
      CALL DODECA(' FR','barrel')
      CALL DODMAG(' FR')
      CALL DODHCA(' FR','barrel')
      CALL DODMUD(' FR','BAR+MA',1)
391   CALL DQSC0('1 SC')
      CALL DF180
      IF(IHTRDO(1).GE.2.AND.IHTRDO(1).LT.6) THEN
        IF(IHTRDO(2).EQ.1) THEN
          CALL DFRTT(FI,SK,SP,TEMID)
          CALL DFRTI(FI,SK,SP,TEMID)
          CALL DFRCC(FI,SK,SP,TEMID)
C       ELSE IF(IHTRDO(2).GE.4) THEN
C         CALL D_AP_KALMAN_TR
        ELSE
C         CALL DFRETA(FI,SK,SP,TEMID)
          CALL DAP_TR
        END IF
      END IF
      IF(IHTRDO(1).LE.2) THEN
        CALL DAPPV('FR',0)
        CALL DAPVC('FR',0)
C       CALL DFRPI(FI,SK,SP,TEMID)
        IF(FITCDR) CALL DYF_IT_HI
        IF(FTPCDR) THEN
          IF(IHTRDO(6).LE.1) THEN
            CALL DAPPT('FR',DVTP,TPCODB,0)
          ELSE IF(IHTRDO(6).EQ.2) THEN
            CALL DAPPT('FR',DVPADA,TPADDB,0)
          ELSE IF(IHTRDO(6).EQ.3) THEN
            CALL DAPPT('FR',DVTP,TBCODB,0)
          END IF
        END IF
      END IF
      CALL DAPVX('FR',LDUM)
      CALL DAP_KNV('FR',LDUM)
      CALL=DVMDCU(1.)
      CALL DAPEF('FR')
      CALL DAPPT('FR',DVMD,MHITDB,0)
      IF(PARADA(2,J_PTO).GE.1005..OR.PARADA(2,J_PTO).LT.1000.) THEN
         CALL DAPEO('FR')
         CALL DAPPE('FR')
      END IF
      IF(PARADA(2,J_PTO).GE.1008..OR.PARADA(2,J_PTO).LT.1000.) THEN
         CALL DFRPH(FI,SK,SP,TEMID)
      END IF
      CALL DAPTRF('FR')
      IF(FPIKDP) RETURN
      IF(PARADA(2,J_PTO).LT.1005.) THEN
         CALL DLSITX(MTPCDL)
      ELSE IF(PARADA(2,J_PTO).LT.1008.) THEN
         CALL DLSITX(MECADL)
      ELSE
         CALL DLSITX(MHCADL)
      END IF
      CALL DCTYEX('FR',2)
      IF(FEMTY) THEN
        CALL DQSCA('H',HRB(1),HRB(2), 'cm' ,2,'&r',1)
        CALL DQSCA('V',VRB(2),VRB(3),' deg',4,'&f',2)
      ELSE
        CALL DQSCA('H',SCA(1),SCA(2),' ',0,' ',0)
        CALL DQSCA('V',SCA(3),SCA(4),' ',0,' ',0)
      END IF
   99 IF(PARADA(4,J_PPT).GE.1..AND.
     &   PARADA(2,J_PPT).NE.0.) THEN
        CALL DGLEVL(L1)
        CALL DQFAR(HLOWDG(IAREDO),VLOWDG(IAREDO),
     &             HHGHDG(IAREDO),VLOWDG(IAREDO)+DVAR)
        CALL DGLEVL(L8)
        IF(DEB.EQ.0.) THEN
          TPT(1:4)=DT3(     PARADA(2,J_PPT))
          TPT(5:5)=TWINDW(IAREDO)
          CALL DGTEXT(HLOWDG(IAREDO)+DPTH,VLOWDG(IAREDO)+DPTV1,TPT,5)
        END IF
        TPT=DT5(QPTT/PARADA(2,J_PPT))
        CALL DGTEXT(HLOWDG(IAREDO)+DPTH,VLOWDG(IAREDO)+DPTV2-DEB,TPT,5)
      END IF
      CALL DQFR(IAREDO)
      CALL DPCSAR
      RETURN
      END
*DK DFRDIS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFRDIS
CH
      SUBROUTINE DFRDIS(F1,H1,V1,F2,H2,V2,FM,HM,VM,D,FC)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C------------------------------------------------------------------- DQ
      COMMON /DQQQ1C/ AHSCDQ,BHSCDQ,CHSCDQ,AVSCDQ,BVSCDQ,CVSCDQ
      COMMON /DQQQ2C/ DMAXDQ
      COMMON /DQQQ3C/ FPRSDQ,FPSQDQ
      LOGICAL FPRSDQ,FPSQDQ
      COMMON /DQQQ4C/ PRSHDQ,PRSVDQ,PRS1DQ,PRS2DQ,PRV1DQ
      COMMON /DQQQ5C/ HUSRDQ(4,0:12),VUSRDQ(4,0:12)
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      LOGICAL FC
      IF(FC) THEN
         FKTRDP=FM
         CALL DQL2EP(H1,V1,HM,VM)
         CALL DQL2EP(H1,V1+360.,HM,VM+360.)
         CALL DQL2EP(HM,VM,H2,V2)
         CALL DQL2EP(HM,VM+360.,H2,V2+360.)
         RETURN
      END IF
      FM=0.5*(F1+F2)
C      IF(FRPRDP) THEN
C        X=DHELIX(FM,IVXXDV)
C        Y=DHELIX(FM,IVYYDV)
C        CALL DRPR(X,Y,HM,VM)
C      ELSE
C        HM=DHELIX(FM,IVRODV)
C        VM=DHELIX(FM,IVFIDV)
C      END IF
      HM=DHELIX(FM,IVRODV)
      VM=DHELIX(FM,IVFIDV)
      VM=DFINXT(VMIN,VM)
      DHU=H2-H1
      DVU=V2-V1
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
         CALL DQL2EP(H1,V1+360.,H2,V2+360.)
         FC=.FALSE.
      ELSE
         FC=.TRUE.
         D=DM
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DFRDS0
CH
      ENTRY DFRDS0(VMI,TEM)
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
      VMIN=VMI
      TEMID=TEM
      END
*DK DFRET1
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFRET1
CH
      SUBROUTINE DFRET1(FI,SK,SP,TEMID)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DT
      PARAMETER (MTRKDT=401,MMCTDT=500)
      COMMON /DTRK1C/ FCUTDT,FNOTDT(   0   :MTRKDT),
     &                FCUHDT,FNOHDT(-MTRKDT:MTRKDT)
      LOGICAL FCUTDT,FNOTDT,FCUHDT,FNOHDT
      COMMON /DTRK2C/ VTX1DT(3),VTX2DT(3),PTRADT(3),CLTMDT(3),FVTXDT
      LOGICAL FVTXDT
      COMMON /DTRK6C/ FRFTDT(6),XYZVDT(3)
      COMMON /DTRK7C/ N3DIDT,FZTRDT,NSVXDT,KINVDT(2,MTRKDT)
      LOGICAL FZTRDT
      COMMON /DTRK3C/ MODEDT,TPHEDT(5)
      COMMON /DTRK4C/ FMCCDT,FMNTDT(MMCTDT),FMVXDT(100)
      LOGICAL FMCCDT,FMNTDT,FMVXDT
      COMMON /DTRK5C/ SZVXDT,NCVXDT
      COMMON /DTRK1T/ TGFTDT,TGTLDT,TGCLDT
      CHARACTER *4 TGFTDT,TGTLDT,TGCLDT
C PARAMETER (MTRKDT=100,MMCTDT=500) is found in:
C C_DALBC.FOR,C_DALBD.FOR,C_DALBH.FOR,C_DALBM.FOR,C_DALBR.FOR,C_DALBT.FOR
C C_DALBV.FOR,C_DALBX.FOR,C_DALBY.FOR,C_DALIA.FOR,C_DALID.FOR,C_DALIR.FOR
C C_DALIS.FOR,C_DALIV.FOR,N_DALBA.FOR,N_DALBF.FOR,US3.FOR
C------------------------------------------------------------------- DL
      PARAMETER (MPNPDL=62,MPDVDL=6,MPWDDL=8,MPRADL=8)
      COMMON /DLUTSC/MNUMDL,MVDEDL,MITCDL,MTPCDL,MECADL,MHCADL,MMDEDL,
     1  MGRCDL(MPDVDL),MVARDL(MPDVDL,2),MODEDL(MPDVDL,2),
     1  MCOLDL(MPDVDL),MSYMDL(MPDVDL,0:1),
     1  MLABDL(MPDVDL),MDUMDL,
     1  WDSNDL(4,MPWDDL,MPDVDL),RAMPDL(4,MPRADL,MPDVDL,2)
C     COMMON /DLUTSC/MNUMDL,MVDEDL,MITCDL,MTPCDL,MECADL,MHCADL,MMDEDL,
C    1  MGRCDL(MPDVDL),
C    1  MCOLDL(MPDVDL),
C    1  MLABDL(MPDVDL),MDUMDL
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      MODEDT=1
      CALL DCIRCL
      CALL DGLEVL(MCOLDL(MTPCDL))
      CALL DFR1TR
      END
*DK DFRETA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFRETA
CH
      SUBROUTINE DFRETA(FIMID,SK,SP,TEMID)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DO
      COMMON /DOPR4C/ NOARDO,
     1  NTF1DO,NSP1DO,IRZ1DO,IHTRDO(8)
      COMMON /DOPR5C/ FGETDO
      LOGICAL FGETDO
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DT
      PARAMETER (MTRKDT=401,MMCTDT=500)
      COMMON /DTRK1C/ FCUTDT,FNOTDT(   0   :MTRKDT),
     &                FCUHDT,FNOHDT(-MTRKDT:MTRKDT)
      LOGICAL FCUTDT,FNOTDT,FCUHDT,FNOHDT
      COMMON /DTRK2C/ VTX1DT(3),VTX2DT(3),PTRADT(3),CLTMDT(3),FVTXDT
      LOGICAL FVTXDT
      COMMON /DTRK6C/ FRFTDT(6),XYZVDT(3)
      COMMON /DTRK7C/ N3DIDT,FZTRDT,NSVXDT,KINVDT(2,MTRKDT)
      LOGICAL FZTRDT
      COMMON /DTRK3C/ MODEDT,TPHEDT(5)
      COMMON /DTRK4C/ FMCCDT,FMNTDT(MMCTDT),FMVXDT(100)
      LOGICAL FMCCDT,FMNTDT,FMVXDT
      COMMON /DTRK5C/ SZVXDT,NCVXDT
      COMMON /DTRK1T/ TGFTDT,TGTLDT,TGCLDT
      CHARACTER *4 TGFTDT,TGTLDT,TGCLDT
C PARAMETER (MTRKDT=100,MMCTDT=500) is found in:
C C_DALBC.FOR,C_DALBD.FOR,C_DALBH.FOR,C_DALBM.FOR,C_DALBR.FOR,C_DALBT.FOR
C C_DALBV.FOR,C_DALBX.FOR,C_DALBY.FOR,C_DALIA.FOR,C_DALID.FOR,C_DALIR.FOR
C C_DALIS.FOR,C_DALIV.FOR,N_DALBA.FOR,N_DALBF.FOR,US3.FOR
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DC
      PARAMETER (MCTRDC=1000)
      COMMON /DCOTR/ FDTRDC,FVTRDC,NCTRDC(0:MCTRDC)
      LOGICAL FDTRDC,FVTRDC
C------------------------------------------------------------------- DP
      COMMON /DPCOLR/ ICOLDP
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C------------------------------------------------------------------- DK
      COMMON /DKNSTC/ PIFCDK,RMAXDK,ZMAXDK,TCO1DK,TCO2DK,
     1   ROTODK(-1:10),ZZTODK(-1:10),ZMTODK(-1:10),DMAXDK,RMINDK,
     1   RLOWDK
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      EQUIVALENCE (KPIKDP,N)
      DATA W2/2./
      LOGICAL FOUT,DVM0
      LOGICAL DCCVX
      EXTERNAL DVTR0
C     IF(.NOT.FTPCDR) RETURN
C     CALL DLHITR('TR',MTPCDL,ICOL)
      CALL DAPTRN('FR',0)
      IF(IHTRDO(2).LE.2) THEN
         CALL DSCTR
         CALL DV0(FRFTDB,NUM1,NUM2,FOUT)
         IF(FOUT) RETURN
         MODEDT=1
         IF(.NOT.FPIKDP.AND.PDCODD(4,NFTRDD).GT.0.) THEN
           CALL DQLEVL(NFTRDD)
           DLINDD=DLINDD+W2
           DO 301 N=NUM1,NUM2
             IF(FNOTDT(N)) GO TO 301
             CALL DVTRV(N)
             CALL DVXT(N,XYZVDT)
             CALL DFR1TR
  301      CONTINUE
           DLINDD=DLINDD-W2
         END IF
         DO 1 N=NUM1,NUM2
           IF(FNOTDT(N)) GO TO 1
           IF(FVTRDC) CALL DGLEVL(NCTRDC(N))
           CALL DVTRV(N)
           CALL DFR1TR
           CALL DAPTRN('FR',N)
    1    CONTINUE
      ELSE
         MODEDT=2
         CALL DMCNVX(NVTX1)
         IF(NVTX1.LE.0) RETURN
         DO 12 NV=1,NVTX1
            CALL DMCVX(NV,KT1,KT2)
            IF(KT2.LE.0) GO TO 12
            DO 11 N=KT1,KT2
               IF(DVM0(N)) GO TO 11
               CALL DMCTR(N,FOUT)
               IF(FOUT) GO TO 11
               CALL DFR1TR
   11       CONTINUE
   12    CONTINUE
         IF(.NOT.FPIKDP) THEN
            IF(NVTX1.LE.0) RETURN
            CALL DPAR_SET_CO(49,'CMC')
            IF(ICOLDP.LT.0) RETURN
            CALL DPARGV(67,'SSY',2,SYMB)
            CALL DPARGV(67,'SSZ',2,SYSZ)
            MSYMB=SYMB
            CALL DQPD0(MSYMB,SZVXDT*0.1*SYSZ,0.)
C           ........................... DEFINE COLOR LATER
            DO 13 N=1,NVTX1
               IF(FMVXDT(N)) THEN
                  CALL DVMCVX(N,KDUM1,KDUM2,KDUM3,VTX1DT)
                  IF(DCCVX(VTX1DT)) GO TO 13
                  RO=DVCORD(VTX1DT,IVRODV)
                  IF(RO.LT.RLOWDK.OR.RO.GT.RMAXDK) GO TO 13
                  TE=DVCORD(VTX1DT,IVTEDV)
                  H=DVCORD(VTX1DT,IVRODV)
                  FI=DFINXT(FIMID,DVCORD(VTX1DT,IVFIDV))
                  CALL DQPD(H,V)
               END IF
   13       CONTINUE
         END IF
      END IF
      END
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
      SUBROUTINE DFRRCB(HRB,VRB)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION HRB(4),VRB(4)
      DATA R0/0./
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PDF,J_PFR,J_PTO'
      CALL DPARAM(11
     &  ,J_PFI,J_PDF,J_PFR,J_PTO)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      HRB(1)=MAX(R0,PARADA(2,J_PFR))
      HRB(2)=PARADA(2,J_PTO)
      HRB(3)=HRB(2)
      HRB(4)=HRB(1)
      DF=0.5*PARADA(2,J_PDF)
      VRB(1)=PARADA(2,J_PFI)-DF
      VRB(2)=VRB(1)
      VRB(3)=PARADA(2,J_PFI)+DF
      VRB(4)=VRB(3)
      END
*DK DFRPH
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFRPH
CH
      SUBROUTINE DFRPH(FIMID,SK,SP,TEMID)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DR
      COMMON /DRNG1C/
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      LOGICAL
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      COMMON /DRNG2C/DET1DR(6),DET2DR(6)
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C------------------------------------------------------------------- DC
      PARAMETER (MCHCDC=2000)
      COMMON /DCOHC/ FDHCDC,FVHCDC,NCHCDC(MCHCDC)
      COMMON /DCOHC1/ LCHCDC
      LOGICAL FDHCDC,FVHCDC
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION HL(2),VL(2)
      EQUIVALENCE (KPIKDP,K)
      LOGICAL FOUT
      EXTERNAL DVHC
      DATA SHRNK /0.8/
      IF(.NOT.FHCADR) RETURN
      CALL DSCHC
      CALL DV0(HSDADB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
      DO 1 K=NUM1,NUM2
         CALL DCUTHC(K,FOUT)
         IF(FOUT) GO TO 1
         TE=DVHC(IVTEDV,K)
         IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 1
         FI=DFINXT(FIMID,DVHC(IVFIDV,K))
         IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 1
         IF(FVHCDC) CALL DGLEVL(NCHCDC(K))
         NN=DVHC(IVNNDV,K)
         IF(NN.NE.2) GO TO 1
         H=DVHC(IVRODV,K)
C         IF(SP.NE.0.) THEN
C            Q=(1.+SP*(DVHC(IVTEDV,K)-TEMID))
C            H=H*Q
C         END IF
         IF(FPIKDP) THEN
            CALL DQPIF(H,FI)
         ELSE
            D=DVHC(IVDFDV,K)*SHRNK
            HL(1)=H
            HL(2)=H
            VL(1)=FI-D
            VL(2)=FI+D
            CALL DQLIE(HL,VL)
            IF(IZOMDO.EQ.0)
     &        CALL DQL2E(HL(1),VL(1)+360.,HL(2),VL(2)+360.)
         END IF
    1 CONTINUE
      END
C*DK DFRPI
CCH..............+++
CCH
CCH
CCH
CCH
CCH
CCH
CCH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFRPI
CCH
C      SUBROUTINE DFRPI(FIMID,SK,SP,TEMID)
CCH
CCH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CCH
CC ---------------------------------------------------------------------
CC
CC    Created by H.Drevermann                   28-JUL-1988
CC_CG 11-May-1989   C.Grab  Adapted to CERNVM + Bug-fix in loops
CC
CC!:
CC    Inputs    :
CC    Outputs   :
CC
CC    Called by :
CC ---------------------------------------------------------------------
C*CA DALLCO
C      INCLUDE 'DALI_CF.INC'
C      EQUIVALENCE (KPIKDP,K)
C      DATA N8/8/,FOVER/40./
C      LOGICAL FOUT
CC     ............................ ONLY RESIDUALS ARE DRAWN BUT NOT OTHER HITS
CC     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
C      TPARDA=
C     &  'J_STC'
C      CALL DPARAM(31
C     &  ,J_STC)
C      TPARDA=
C     &  'J_SSY,J_SSZ'
C      CALL DPARAM(61
C     &  ,J_SSY,J_SSZ)
CC     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
C      IF(FGRYDR.AND.PARADA(4,J_STC).LT.0.) RETURN
C      IF(.NOT.FITCDR) RETURN
C      CALL DSCIT(IVNT)
C      CALL DV0(ITCODB,NUM1,NUM2,FOUT)
C      IF(FOUT) RETURN
C      CALL DVITST(IVNTDV,IVNT1,IVNT2)
C      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
C      MSYM=IDPAR(J_SSY)
C      SYSZ=PARADA(2,J_SSZ)
C      IF(PARADA(4,J_SSY).LT.0.) MSYM=0
C      IF(FPIKDP.OR.PARADA(4,J_SWI).LT.0.) THEN
CC//   IF(FPIKDP.OR.(PARADA(2,J_SSY).GT.2.AND.IZOMDO.EQ.1)) THEN
CC//   IF(FPIKDP.OR.
CC//  &  ((MODEDL(MITCDL,2).NE.1.OR.MSYMDL(MITCDL,1).GT.2)
CC//  &  .AND.IZOMDO.EQ.1))THEN
CC//     CALL DQPD0(MSYMDL(MITCDL,0),WDSNDL(2,4,MITCDL),0.)
C        CALL DQPD1(61,0.)
C        DO   700  K=NUM1,NUM2
C          IF(FCUHDT) THEN
C            NTRIT=DVIT(IVNTDV,K)
C            IF(FNOHDT(NTRIT)) GO TO 700
C          END IF
C          DO 1 NFI=1,2
C            TE=DVIT(IVTEDV,K,NFI)
C            FI=DFINXT(FIMID,DVIT(IVFIDV,K,NFI))
C            IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 1
C            IF(FVITDC) CALL DGLEVL(NCITDC(K))
C            H=DVIT(IVRODV,K,NFI)
C            V=FI
C            IF(FPIKDP) THEN
C              CALL DQPIF(H,V)
C              GO TO 1
C            END IF
C            CALL DQPD(H,V)
C    1     CONTINUE
C  700   CONTINUE
C      ELSE
CC//     CALL DQPD0(N8,WDSNDL(2,4,MITCDL),0.)
C        CALL DQPD0(N8,PARADA(2,J_SSZ),0.)
C        DO 2 K=NUM1,NUM2
C          IF(FCUHDT) THEN
C            NTRIT=DVIT(IVNTDV,K)
C            IF(FNOHDT(NTRIT)) GO TO 2
C          END IF
C          T1=DVIT(IVTEDV,K,1)
C          T2=DVIT(IVTEDV,K,2)
C          F1=DVIT(IVFIDV,K,1)
C          F2=DFINXT(F1,DVIT(IVFIDV,K,2))
C          FID=F2-F1
C          F1=DFINXT(FIMID,F1)
C          F2=F1+FID
C          IF( (F1.LT.FC1.AND.F2.LT.FC1).OR.
C     &      (F1.GT.FC2.AND.F2.GT.FC2) ) GO TO 2
C          H=DVIT(IVRODV,K,1)
C          IF(FVITDC) CALL DGLEVL(NCITDC(K))
C          CALL DQL2E(H,F1,H,F2)
C          IF(IZOMDO.EQ.0) CALL DQL2E(H,F1+360.,H,F2+360.)
CC//       IF(MSYMDL(MITCDL,0).EQ.1) THEN
C          IF(PARADA(2,J_SSY).EQ.1.) THEN
C            IF(NITCDV(IVNT1,K).NE.0) THEN
C              CALL DQPD(H,F1)
C              IF(IZOMDO.EQ.0.AND.F1.LE.FOVER) CALL DQPD(H,F1+360.)
C            ELSE IF(NITCDV(IVNT2,K).NE.0) THEN
C              CALL DQPD(H,F2)
C              IF(IZOMDO.EQ.0.AND.F2.LE.FOVER) CALL DQPD(H,F2+360.)
C            END IF
C          END IF
C    2   CONTINUE
C      END IF
C      END
*DK DFRSP
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFRSP
CH
      SUBROUTINE DFRSP(IGO)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DO
      COMMON /DOPR4C/ NOARDO,
     1  NTF1DO,NSP1DO,IRZ1DO,IHTRDO(8)
      COMMON /DOPR5C/ FGETDO
      LOGICAL FGETDO
C------------------------------------------------------------------- DR
      COMMON /DRNG1C/
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      LOGICAL
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      COMMON /DRNG2C/DET1DR(6),DET2DR(6)
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      INCLUDE 'DALI_EX.INC'
      DIMENSION HRB(4),VRB(4)
      DATA RFR/30./,RTO/180./
C      DIMENSION QWIND(0:12,0:1)
C                 0  1  2  3  4  5  6  U  D  L  M  R  S
C      DATA QWIND/.4,1.,1.,1.,1.,1.,1.,.5,.5,1.,1.,1.,.5,
C     &           .4,0.,0.,0.,0.,0.,0.,0.,0.,.7,0.,.7,0./
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_PRF'
      CALL DPARAM(10
     &  ,J_PFI,J_PTE,J_PRF)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PARADA(4,J_PRF).EQ.1..AND.PARADA(2,J_PRF).GE.1.) THEN
        IF(IGO.EQ.0) THEN
          HLOW=HLOWDG(IAREDO)
          HHGH=HHGHDG(IAREDO)
          DH=0.01*PARADA(2,J_PRF)*(HHGH-HLOW)
C          DH=0.01*PARADA(2,J_PRF)*(HHGH-HLOW)
C     &    *QWIND(IAREDO,IFIX(WISUDW))
          IF(DH.EQ.0.) RETURN
          HHGHDG(IAREDO)=HHGH-DH
          RETURN
        END IF
        HLOWDG(IAREDO)=HHGHDG(IAREDO)
        HHGHDG(IAREDO)=HHGH
        CALL DQRER(0,RFR,-1.,RTO,400.,HRB,VRB)
        IF(IZOMDO.EQ.0) THEN
          FI=180.
        ELSE
          CALL DFRRCB(HVDUM,VRB)
          TEMID=PARADA(2,J_PTE)
          FI=PARADA(2,J_PFI)
        END IF
        CALL DQRU(HRB,VRB)
        CALL DODTPC(' FR')
        CALL DF180
        CALL DQDWI
        IF(IHTRDO(1).GE.2.AND.IHTRDO(1).LT.6) THEN
          IF(IHTRDO(2).EQ.1) THEN
            CALL DFRTT(FI,SK,SP,TEMID)
          ELSE
C            CALL DFRETA(FI,SK,SP,TEMID)
            CALL DAP_TR
          END IF
        END IF
        IF(IHTRDO(1).LE.2) THEN
          IF(FTPCDR) THEN
            IF(IHTRDO(6).LE.1) THEN
              CALL DAPPT('FR',DVTP,TPCODB,0)
            ELSE IF(IHTRDO(6).EQ.2) THEN
              CALL DAPPT('FR',DVPADA,TPADDB,0)
            ELSE IF(IHTRDO(6).EQ.3) THEN
              CALL DAPPT('FR',DVTP,TBCODB,0)
            END IF
          END IF
        END IF
        HHGHDG(IAREDO)=HLOWDG(IAREDO)
        HLOWDG(IAREDO)=HLOW
      END IF
      END
*DK DFRTI
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFRTI
CH
      SUBROUTINE DFRTI(FIMID,SK,SP,TEMID)
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
C---Routine to draw connected hits for ITC (phi-rho projection).-ecb
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DR
      COMMON /DRNG1C/
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      LOGICAL
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      COMMON /DRNG2C/DET1DR(6),DET2DR(6)
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DT
      PARAMETER (MTRKDT=401,MMCTDT=500)
      COMMON /DTRK1C/ FCUTDT,FNOTDT(   0   :MTRKDT),
     &                FCUHDT,FNOHDT(-MTRKDT:MTRKDT)
      LOGICAL FCUTDT,FNOTDT,FCUHDT,FNOHDT
      COMMON /DTRK2C/ VTX1DT(3),VTX2DT(3),PTRADT(3),CLTMDT(3),FVTXDT
      LOGICAL FVTXDT
      COMMON /DTRK6C/ FRFTDT(6),XYZVDT(3)
      COMMON /DTRK7C/ N3DIDT,FZTRDT,NSVXDT,KINVDT(2,MTRKDT)
      LOGICAL FZTRDT
      COMMON /DTRK3C/ MODEDT,TPHEDT(5)
      COMMON /DTRK4C/ FMCCDT,FMNTDT(MMCTDT),FMVXDT(100)
      LOGICAL FMCCDT,FMNTDT,FMVXDT
      COMMON /DTRK5C/ SZVXDT,NCVXDT
      COMMON /DTRK1T/ TGFTDT,TGTLDT,TGCLDT
      CHARACTER *4 TGFTDT,TGTLDT,TGCLDT
C PARAMETER (MTRKDT=100,MMCTDT=500) is found in:
C C_DALBC.FOR,C_DALBD.FOR,C_DALBH.FOR,C_DALBM.FOR,C_DALBR.FOR,C_DALBT.FOR
C C_DALBV.FOR,C_DALBX.FOR,C_DALBY.FOR,C_DALIA.FOR,C_DALID.FOR,C_DALIR.FOR
C C_DALIS.FOR,C_DALIV.FOR,N_DALBA.FOR,N_DALBF.FOR,US3.FOR
C------------------------------------------------------------------- DC
      PARAMETER (MCTRDC=1000)
      COMMON /DCOTR/ FDTRDC,FVTRDC,NCTRDC(0:MCTRDC)
      LOGICAL FDTRDC,FVTRDC
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      EQUIVALENCE (KPIKDP,K)
C     DATA NBCOL/2/
      DIMENSION H(2),V(2)
      LOGICAL FOUT
      IF(.NOT.FITCDR) RETURN
      CALL DSCTR
      CALL DV0(FRTLDB,NUM1,NTRK,FOUT)
      IF(FOUT) RETURN
      CALL DV0(ITCODB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
      DO 3 M=1,NTRK
         IF(FCUTDT) THEN
            IF(FNOTDT(M)) GO TO 3
         END IF
c---i     nd number of ITC coordinates for track M
         CALL=DVCHI(M,NUM)
         IF(NUM.LE.0) GO TO 3
c---e     lect color for track M
         IF(FVTRDC) CALL DGLEVL(NCTRDC(M))
         I=0
c---o     loop over coordinates
         DO 1 KIND=1,NUM
c---e        solve l/r ambiguity
            K=IDVCHI(KIND)
            IF(K.GT.0) THEN
               NFI=1
            ELSE
               NFI=2
               K=-K
            END IF
            TE=DVIT(IVTEDV,K,NFI)
C           IF(TE.LT.TC1.OR.TE.GT.TC2) THEN
C              I=0
C              GO TO 1
C           END IF
            FI=DFINXT(FIMID,DVIT(IVFIDV,K,NFI))
            IF(FI.LT.FC1.OR.FI.GT.FC2) THEN
               I=0
               GO TO 1
            END IF
            I=I+1
            H(I)=DVIT(IVRODV,K,NFI)
            V(I)=FI
C            IF(SK.EQ.0.) THEN
C               V(I)=FI
C            ELSE
C               V(I)=FI+SK*(TE-TEMID)
C            END IF
C            IF(SP.NE.0.) THEN
C               H(I)=H(I)*(1.+SP*(DVIT(IVTEDV,K)-TEMID))
C            END IF
            IF(FPIKDP) THEN
               CALL DQPIK(H(I),V(I))
               I=0
               GO TO 1
            END IF
C            IF(FI.LT.FS1.OR.FI.GT.FS2.OR.TE.LT.TS1.OR.TE.GT.TS2)
C     1        CALL DGLEVL(NBCOL)
c---f        this is not the first coordinate on the track, connect previous
c---o        int to current point.  -ecb
            IF(I.EQ.2) THEN
               CALL DQLIE(H,V)
               IF(IZOMDO.EQ.0) THEN
                  CALL DQL2E(H(1),V(1)+360.,H(2),V(2)+360.)
                  CALL DQL2E(H(1),V(1)-360.,H(2),V(2)-360.)
               END IF
               I=1
               H(1)=H(2)
               V(1)=V(2)
            END IF
C            END IF
    1    CONTINUE
    3 CONTINUE
      END
*DK DFRPIT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFRPIT
CH
      SUBROUTINE DFRPIT(F1,F2)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      PARAMETER (MXYZDP=19)
      COMMON /DPICK2/ NXYZDP,XYZPDP(3,19)
      COMMON /DPICK3/ NTRKDP,LTRKDP(8)
      COMMON /DPICK4/ NUMPDP,LORPDP
      PARAMETER (MTRSDP=26)
      COMMON /DPICK5/ TRSTDP(MTRSDP)
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
C      DATA FP/20./,KP/20/
C      DF=(F2-F1)/FP
C      F=F1
C      DO 700 K=1,KP
C        F=F+DF
      DF=F2-F1
      DO 700 K=1,MTRSDP
        F=F1+TRSTDP(K)*DF
C        F=TRSTDP(K)*DF
        H1=DHELIX(F,IVRODV)
        V1=DHELIX(F,IVFIDV)
        CALL DQPIK(H1,V1)
        CALL DQPIK(H1,V1+360.)
  700 CONTINUE
      END
*DK DFRTT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFRTT
CH
      SUBROUTINE DFRTT(FIMID,SK,SP,TEMID)
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
C---Routine to draw connected hits for TPC (phi-rho projection).-ecb
c---s  es FRFT, FRTL, etc.
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DR
      COMMON /DRNG1C/
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      LOGICAL
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      COMMON /DRNG2C/DET1DR(6),DET2DR(6)
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DT
      PARAMETER (MTRKDT=401,MMCTDT=500)
      COMMON /DTRK1C/ FCUTDT,FNOTDT(   0   :MTRKDT),
     &                FCUHDT,FNOHDT(-MTRKDT:MTRKDT)
      LOGICAL FCUTDT,FNOTDT,FCUHDT,FNOHDT
      COMMON /DTRK2C/ VTX1DT(3),VTX2DT(3),PTRADT(3),CLTMDT(3),FVTXDT
      LOGICAL FVTXDT
      COMMON /DTRK6C/ FRFTDT(6),XYZVDT(3)
      COMMON /DTRK7C/ N3DIDT,FZTRDT,NSVXDT,KINVDT(2,MTRKDT)
      LOGICAL FZTRDT
      COMMON /DTRK3C/ MODEDT,TPHEDT(5)
      COMMON /DTRK4C/ FMCCDT,FMNTDT(MMCTDT),FMVXDT(100)
      LOGICAL FMCCDT,FMNTDT,FMVXDT
      COMMON /DTRK5C/ SZVXDT,NCVXDT
      COMMON /DTRK1T/ TGFTDT,TGTLDT,TGCLDT
      CHARACTER *4 TGFTDT,TGTLDT,TGCLDT
C PARAMETER (MTRKDT=100,MMCTDT=500) is found in:
C C_DALBC.FOR,C_DALBD.FOR,C_DALBH.FOR,C_DALBM.FOR,C_DALBR.FOR,C_DALBT.FOR
C C_DALBV.FOR,C_DALBX.FOR,C_DALBY.FOR,C_DALIA.FOR,C_DALID.FOR,C_DALIR.FOR
C C_DALIS.FOR,C_DALIV.FOR,N_DALBA.FOR,N_DALBF.FOR,US3.FOR
C------------------------------------------------------------------- DC
      PARAMETER (MCTRDC=1000)
      COMMON /DCOTR/ FDTRDC,FVTRDC,NCTRDC(0:MCTRDC)
      LOGICAL FDTRDC,FVTRDC
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION H(2),V(2)
      EQUIVALENCE (K,KPIKDP)
      LOGICAL FOUT
      IF(.NOT.FTPCDR) RETURN
      CALL DSCTR
      CALL DV0(FRTLDB,NUM1,NTRK,FOUT)
      IF(FOUT) RETURN
      CALL DV0(TPCODB,NUM0,NUM2,FOUT)
      IF(FOUT) RETURN
C     CALL DVTQ0(NUM2)
C     IF(NUM2.EQ.0) RETURN
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
C      CALL DLHITR('TR',MTPCDL,ICOL)
      DO 3 M=1,NTRK
         IF(FCUTDT) THEN
            IF(FNOTDT(M)) GO TO 3
         END IF
         CALL=DVCHT(M,NUM)
         IF(NUM.LE.0) GO TO 3
         K1=IDVCHT(1)
C         CALL DLCOLT(M,NCOL)
C         IF(NCOL.EQ.0) GO TO 3
C         IF(ICOL.NE.0) CALL DGLEVL(ICOL)
         IF(FVTRDC) CALL DGLEVL(NCTRDC(M))
         I=0
         FIMTR=DFINXT(FIMID,DVTP(IVFIDV,K1))
    2    DO 1 KIND=1,NUM
            K=IDVCHT(KIND)
            IF(FPIMDP.AND.K.NE.NPIKDP) GO TO 1
            TE=DVTP(IVTEDV,K)
            IF(TE.LT.TC1.OR.TE.GT.TC2) THEN
               I=0
               GO TO 1
            END IF
            FI=DFINXT(FIMTR,DVTP(IVFIDV,K))
            IF(FI.LT.FC1.OR.FI.GT.FC2) THEN
               I=0
               GO TO 1
            END IF
            I=I+1
            H(I)=DVTP(IVRODV,K)
            V(I)=FI
            IF(FPIKDP) THEN
               CALL DQPIK(H,V)
               I=0
            ELSE IF(I.EQ.2) THEN
               CALL DQLIE(H,V)
               IF(IZOMDO.EQ.0) THEN
                  CALL DQL2E(H(1),V(1)+360.,H(2),V(2)+360.)
                  CALL DQL2E(H(1),V(1)-360.,H(2),V(2)-360.)
               END IF
               I=1
               H(1)=H(2)
               V(1)=V(2)
            END IF
    1    CONTINUE
    3 CONTINUE
      END
*DK DFT1TR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFT1TR
CH
      SUBROUTINE DFT1TR(PU,ROVD)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DT
      PARAMETER (MTRKDT=401,MMCTDT=500)
      COMMON /DTRK1C/ FCUTDT,FNOTDT(   0   :MTRKDT),
     &                FCUHDT,FNOHDT(-MTRKDT:MTRKDT)
      LOGICAL FCUTDT,FNOTDT,FCUHDT,FNOHDT
      COMMON /DTRK2C/ VTX1DT(3),VTX2DT(3),PTRADT(3),CLTMDT(3),FVTXDT
      LOGICAL FVTXDT
      COMMON /DTRK6C/ FRFTDT(6),XYZVDT(3)
      COMMON /DTRK7C/ N3DIDT,FZTRDT,NSVXDT,KINVDT(2,MTRKDT)
      LOGICAL FZTRDT
      COMMON /DTRK3C/ MODEDT,TPHEDT(5)
      COMMON /DTRK4C/ FMCCDT,FMNTDT(MMCTDT),FMVXDT(100)
      LOGICAL FMCCDT,FMNTDT,FMVXDT
      COMMON /DTRK5C/ SZVXDT,NCVXDT
      COMMON /DTRK1T/ TGFTDT,TGTLDT,TGCLDT
      CHARACTER *4 TGFTDT,TGTLDT,TGCLDT
C PARAMETER (MTRKDT=100,MMCTDT=500) is found in:
C C_DALBC.FOR,C_DALBD.FOR,C_DALBH.FOR,C_DALBM.FOR,C_DALBR.FOR,C_DALBT.FOR
C C_DALBV.FOR,C_DALBX.FOR,C_DALBY.FOR,C_DALIA.FOR,C_DALID.FOR,C_DALIR.FOR
C C_DALIS.FOR,C_DALIV.FOR,N_DALBA.FOR,N_DALBF.FOR,US3.FOR
C------------------------------------------------------------------- DO
      COMMON /DOPR4C/ NOARDO,
     1  NTF1DO,NSP1DO,IRZ1DO,IHTRDO(8)
      COMMON /DOPR5C/ FGETDO
      LOGICAL FGETDO
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      EXTERNAL DFTDIS
      DATA DL/0.5/
      DIMENSION FFRTO(7)
C                VX VD IT T0 T3 E1 E3
      DATA FFRTO/0.,0.,3.,4.,5.,6.,7./
      DATA IPDEB/0/,MTR4/4/
      LOGICAL FBAR(2),FOUT
      LOGICAL DCCTR
      IF(MODEDT.NE.2) THEN
         CALL=DHELX0(FFRTO(2),FFRTO(3),FFRTO(4),FFRTO(6),FBAR)
      ELSE
         FFRTO(2)=0.
         CALL=DHELX1(FFRTO(3),FFRTO(4),FFRTO(6),FBAR,FOUT)
         IF(FOUT) RETURN
      END IF
      IF(ROVD.GE.0.) CALL=DHELXV(ROVD,FFRTO(4))
      IF(FFRTO(4).EQ.180.) RETURN
      IF(DCCTR(FFRTO(4),FFRTO(5))) RETURN
      IHTFR=MAX(MIN(IHTRDO(3),6),MTR4)
      IHTTO=MIN(IHTRDO(4),7)
      IF(IHTFR.GE.IHTTO) RETURN
      IF(IHTFR.LE.5) THEN
         IF(IHTFR.EQ.4) THEN
            F1=FFRTO(4)
            V1=DHELIX(F1,IVFIDV)
            T1=DHELIX(F1,IVTEDV)
            H1=-T1
            IF(PU.NE.0.) H1=H1-PU*DHELIX(F1,IVRBDV)
            F2=FFRTO(5)
            IF(F1.GE.F2) RETURN
            V2=DHELIX(F2,IVFIDV)
            VMIN=MIN(V1,V2)
            IF(FPIKDP.AND.IPDEB.EQ.1) THEN
              CALL DFTPIT(F1,F2,PU,VMIN)
            ELSE
              T2=DHELIX(F2,IVTEDV)
              H2=-T2
              IF(PU.NE.0.) H2=H2-PU*DHELIX(F2,IVRBDV)
              IF(H1.EQ.H2.AND.V1.EQ.V2) RETURN
              V1=DFINXT(VMIN,V1)
              V2=DFINXT(VMIN,V2)
C               VMIN IS USED SO THAT ONLY V=V+360 IS NECESSARY AND NOT V-3
              CALL DFTDS0(PU,VMIN)
              CALL DDRAWA(DFTDIS,F1,H1,V1,F2,H2,V2)
              IF(PU.LT.0.) THEN
                 H1=-T1
                 H1=H1+PU*DHELIX(F1,IVRBDV)
                 H2=-T2
                 H2=H2+PU*DHELIX(F2,IVRBDV)
                 CALL DFTDS0(-PU,VMIN)
                 CALL DDRAWA(DFTDIS,F1,H1,V1,F2,H2,V2)
              END IF
            END IF
         END IF
      END IF
      IF(IHTTO.GE.6) THEN
C                                                       TRACK FROM END OF TPC
         IF(FFRTO(6).NE.180.) THEN
            F1=FFRTO(MAX(IHTFR,5))
            H1=-DHELIX(F1,IVTEDV)
            V1=DHELIX(F1,IVFIDV)
            IF(FFRTO(7).EQ.180.) THEN
               F2=FFRTO(MIN(IHTTO,6))
            ELSE
               F2=FFRTO(IHTTO)
            END IF
            IF(F1.GE.F2) RETURN
            IF(FPIKDP.AND.IPDEB.EQ.1) THEN
              CALL DFTPIT(F1,F2,0.,VMIN)
            ELSE
              H2=-DHELIX(F2,IVTEDV)
              V2=DHELIX(F2,IVFIDV)
              V1=DFINXT(VMIN,V1)
              V2=DFINXT(VMIN,V2)
              CALL DFTDS0(0.,VMIN)
              CALL DDRAWA(DFTDIS,F1,H1,V1,F2,H2,V2)
            END IF
         END IF
      END IF
      IF(IHTFR.LE.6.AND.IHTTO.EQ.7) THEN
C                                               ENTRY + EXIT OF ECAL
         F1=FFRTO(6)
         IF(F1.NE.180.) THEN
            H1=-DHELIX(F1,IVTEDV)
            V1=DHELIX(F1,IVFIDV)
            CALL DQL2E(H1-DL,V1,H1+DL,V1)
            F2=FFRTO(7)
            IF(F2.NE.180.) THEN
               H2=-DHELIX(F2,IVTEDV)
               V2=DHELIX(F2,IVFIDV)
               CALL DQL2E(H2-DL,V2,H2+DL,V2)
            END IF
         END IF
      END IF
      END
*DK DFTCE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTCE
CH
      SUBROUTINE DFTCE(FIMID,TEMID,TSKW,CE,SC)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *1 TSKW
      DIMENSION KCOL(3)
      DATA FOVER/40./
      LOGICAL FOUT,FCOL
      EXTERNAL DVEC
      CALL DV0(ESDADB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
C      CALL DORCAL
      FCOL=.FALSE.
      IF(PDCODD(4,ICCNDD).EQ.1.) THEN
        CALL UFILL(KCOL,1,3,IFIX(PDCODD(2,ICCNDD)))
      ELSE
        DO 700 K=1,3
          KCOL(K)=PDCODD(2,LCEKDD(K))
          IF(KCOL(K).NE.KCOL(1)) FCOL=.TRUE.
  700   CONTINUE
      END IF
      CALL DCUTFT(FP1,FP2,FC1,FC2,TP1,TP2,TC1,TC2)
      IF(IZOMDO.NE.0) CALL DFTCUE(FP1,FP2,FC1,FC2,TP1,TP2,TC1,TC2)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PP1,J_PP2'
      CALL DPARAM(11
     &  ,J_PP1,J_PP2)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      K1=PARADA(2,J_PP1)-1004.
      K2=PARADA(2,J_PP2)-1004.
C      KD=KECADI-1
      VCMAX=VHGHDG(IAREDO)
      DO 1 K=NUM1,NUM2
         TE=DVEC(IVTEDV,K)
         IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 1
         FFI=DVEC(IVFIDV,K)
         FI=DFINXT(FIMID,FFI)
         IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 1
         H=-TE
         J=DVEC(IVJJDV,K)
         JJ=MIN(221-J,J)
         KK=DVEC(IVKKDV,K)
C            NN=DVEC(IVNNDV,K)
C            IF(K1.GE.KECADI.AND.JJ.GE.45.AND.JJ.LE.52) THEN
C                  KK=KNECDK(JJ,KK-KD,NN)
C                  IF(KK.LT.KECADI) NOTSH=NOTSH+1
C            END IF
         IF(KK.LT.K1.OR.KK.GT.K2) GO TO 1
         IF(DVEC(IVENDV,K).LT.CE) GO TO 1
         IF(FCOL) CALL DGLEVL(KCOL(KK))
         CHGT=SC*DVEC(IVENDV,K)
         CALL DQCH(H,FI,CHGT)
         IF(IZOMDO.EQ.0.AND.FI.LT.FOVER)
     &     CALL DQCH(H,FI+360.,CHGT,VCMAX)
    1 CONTINUE
      END
*DK DFTCH
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTCH
CH
      SUBROUTINE DFTCH(FIMID,TEMID,TSKW,CE,SC)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *1 TSKW
      LOGICAL FOUT
      EXTERNAL DVHC
      CALL DV0(HSDADB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
      CALL DQLEVL(LCHCDD)
      CALL DCUTFT(FP1,FP2,FC1,FC2,TP1,TP2,TC1,TC2)
      IF(IZOMDO.NE.0) CALL DFTCUE(FP1,FP2,FC1,FC2,TP1,TP2,TC1,TC2)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PP1,J_PP2'
      CALL DPARAM(11
     &  ,J_PP1,J_PP2)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      K1=PARADA(2,J_PP1)-1007.
      K2=PARADA(2,J_PP2)-1007.
      VCMAX=VHGHDG(IAREDO)
      DO 1 K=NUM1,NUM2
         TE=DVHC(IVTEDV,K)
         IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 1
         FFI=DVHC(IVFIDV,K)
         FI=DFINXT(FIMID,FFI)
         IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 1
         H=-TE
         KK=DVHC(IVKKDV,K)
         IF(KK.LT.K1.OR.KK.GT.K2) GO TO 1
         IF(DVHC(IVENDV,K).LT.CE) GO TO 1
C         CALL DLCOLH(DVHC,MHCADL,K,NCOL)
C         IF(NCOL.EQ.0) GO TO 1
C         IF(FI.LT.FP1.OR.FI.GT.FP2.OR.TE.LT.TP1.OR.TE.GT.TP2) THEN
C            CALL DLOASY(MHCADL,'N','N','S',H,FI)
C            CALL DLOASY(MHCADL,'N','N','S',H,FI+360.)
C         ELSE
            CHGT=SC*DVHC(IVENDV,K)
            CALL DQCH(H,FI,CHGT,VCMAX)
            IF(IZOMDO.EQ.0)
     1        CALL DQCH(H,FI+360.,CHGT,VCMAX)
C         END IF
    1 CONTINUE
      RETURN
      END
*DK DFTCUE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTCUE
CH
      SUBROUTINE DFTCUE(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PDF,J_PTE,J_PDT'
      CALL DPARAM(11
     &  ,J_PFI,J_PDF,J_PTE,J_PDT)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      TEMID=PARADA(2,J_PTE)
      FIMID=PARADA(2,J_PFI)
      DC=0.5*PARADA(2,J_PDT)
      TC1=MAX(TEMID-DC,TC1)
      TC2=MIN(TEMID+DC,TC2)
      DC=0.5*PARADA(2,J_PDF)
      FC1=MAX(FIMID-DC,FC1)
      FC2=MIN(FIMID+DC,FC2)
      END
*DK DFTCUT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTCUT
CH
      SUBROUTINE DFTCUT(FP1,FP2,FC1,FC2,TP1,TP2,TC1,TC2)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PDF,J_PCF,J_PTE,J_PDT,J_PCT'
      CALL DPARAM(11
     &  ,J_PFI,J_PDF,J_PCF,J_PTE,J_PDT,J_PCT)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PARADA(4,J_PCT).EQ.-1.) THEN
         TEMID=PARADA(2,J_PTE)
         DC=0.5*PARADA(2,J_PDT)
         TC1=TEMID-DC
         TC2=TEMID+DC
      END IF
      IF(PARADA(4,J_PCF).EQ.-1.) THEN
         FIMID=PARADA(2,J_PFI)
         DC=0.5*PARADA(2,J_PDF)
         FC1=FIMID-DC
         FC2=FIMID+DC
      END IF
      RETURN
      END
*DK DFTD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTD
CH
      SUBROUTINE DFTD
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DU
      PARAMETER (MPNPDU=60)
      REAL DACUDU
      COMMON /DUSDAC/ NUNIDU,
     1 WAITDU,ECCHDU,BOXSDU,ECSQDU,HCSQDU,
     1 HIMXDU,RESCDU,SLOWDU,WISUDU,SETTDU,
     1 USLVDU,SYLRDU,SDATDU,RZDHDU,ROTADU,
     1 SCMMDU,CLMMDU,CUMMDU,WIMIDU,CCMIDU,
     1 ECMIDU,HCMIDU,SHTFDU(2)    ,FTDZDU,
     1 FRNLDU,DACUDU,SHECDU,SHHCDU,DRLIDU,
     1 SQLCDU,CHLCDU,HEADDU,VRDZDU,CHHCDU,
     1 AREADU,DFWIDU(0:1)  ,DFLGDU,PBUGDU,
     1 CHTFDU,RNCLDU,PR43DU,PR44DU,PR45DU,
     1 DSOFDU,TPOFDU,ROVDDU,ROECDU,ROHCDU,
     1 HSTRDU,REALDU,PR53DU,PR54DU,PR55DU,
     1 PR56DU(4),                  PR60DU
      DIMENSION PARADU(MPNPDU)
      EQUIVALENCE (PARADU,WAITDU)
      COMMON /DUSDA2/PLIMDU(2,MPNPDU)
      COMMON /DUSDAT/ TPR1DU(MPNPDU)
      CHARACTER *2 TPR1DU
C------------------------------------------------------------------- DT
      PARAMETER (MTRKDT=401,MMCTDT=500)
      COMMON /DTRK1C/ FCUTDT,FNOTDT(   0   :MTRKDT),
     &                FCUHDT,FNOHDT(-MTRKDT:MTRKDT)
      LOGICAL FCUTDT,FNOTDT,FCUHDT,FNOHDT
      COMMON /DTRK2C/ VTX1DT(3),VTX2DT(3),PTRADT(3),CLTMDT(3),FVTXDT
      LOGICAL FVTXDT
      COMMON /DTRK6C/ FRFTDT(6),XYZVDT(3)
      COMMON /DTRK7C/ N3DIDT,FZTRDT,NSVXDT,KINVDT(2,MTRKDT)
      LOGICAL FZTRDT
      COMMON /DTRK3C/ MODEDT,TPHEDT(5)
      COMMON /DTRK4C/ FMCCDT,FMNTDT(MMCTDT),FMVXDT(100)
      LOGICAL FMCCDT,FMNTDT,FMVXDT
      COMMON /DTRK5C/ SZVXDT,NCVXDT
      COMMON /DTRK1T/ TGFTDT,TGTLDT,TGCLDT
      CHARACTER *4 TGFTDT,TGTLDT,TGCLDT
C PARAMETER (MTRKDT=100,MMCTDT=500) is found in:
C C_DALBC.FOR,C_DALBD.FOR,C_DALBH.FOR,C_DALBM.FOR,C_DALBR.FOR,C_DALBT.FOR
C C_DALBV.FOR,C_DALBX.FOR,C_DALBY.FOR,C_DALIA.FOR,C_DALID.FOR,C_DALIR.FOR
C C_DALIS.FOR,C_DALIV.FOR,N_DALBA.FOR,N_DALBF.FOR,US3.FOR
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C------------------------------------------------------------------- DE
      COMMON /DECAEN/ FECEDE,ENECDE(7)
      LOGICAL FECEDE
C------------------------------------------------------------------- DI
      COMMON /DINPUC/ IFILDI,IEVEDI,ZOFFDI,
     1 KVDEDI,KITCDI,KTPCDI,KECADI,KHCADI,KMDEDI
C------------------------------------------------------------------- DO
      COMMON /DOPR4C/ NOARDO,
     1  NTF1DO,NSP1DO,IRZ1DO,IHTRDO(8)
      COMMON /DOPR5C/ FGETDO
      LOGICAL FGETDO
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DF
      PARAMETER (MCOLDF=10)
      COMMON /DFTCLC/ COLIDF(4,MCOLDF),COLMDF(4,3)
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DR
      COMMON /DRNG1C/
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      LOGICAL
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      COMMON /DRNG2C/DET1DR(6),DET2DR(6)
C------------------------------------------------------------------- DW
      COMMON /DWORKT/ TXTADW
      CHARACTER *80 TXTADW
      CHARACTER *1 TXT1DW(80)
      EQUIVALENCE (TXTADW,TXT1DW)
C------------------------------------------------------------------- DL
      PARAMETER (MPNPDL=62,MPDVDL=6,MPWDDL=8,MPRADL=8)
      COMMON /DLUTSC/MNUMDL,MVDEDL,MITCDL,MTPCDL,MECADL,MHCADL,MMDEDL,
     1  MGRCDL(MPDVDL),MVARDL(MPDVDL,2),MODEDL(MPDVDL,2),
     1  MCOLDL(MPDVDL),MSYMDL(MPDVDL,0:1),
     1  MLABDL(MPDVDL),MDUMDL,
     1  WDSNDL(4,MPWDDL,MPDVDL),RAMPDL(4,MPRADL,MPDVDL,2)
C     COMMON /DLUTSC/MNUMDL,MVDEDL,MITCDL,MTPCDL,MECADL,MHCADL,MMDEDL,
C    1  MGRCDL(MPDVDL),
C    1  MCOLDL(MPDVDL),
C    1  MLABDL(MPDVDL),MDUMDL
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      INCLUDE 'DALI_EX.INC'
      LOGICAL FDUM,FSKW,FEMTY
      CHARACTER *1 TSKW
      DIMENSION JORD(3)
      DIMENSION KPL(-2:11)
      DATA KPL/0,1,2,3,3,3,3,4,4,4,7,7,9,11/
      CHARACTER *3 DT3
C      CHARACTER *3,TGL(5:11)
C      DATA TGL/'FT1','FT2','FT3','FT1','FT2','FT1','FT1'/
      CHARACTER *2 TPL2(-2:11)
      CHARACTER *5 TPL5(-2:11)
      DATA TPL5/
     &  'V1   ','V2   ','ITC  ',4*'TPC  ',
     &  'ECAL1','ECAL2','ECAL3','HCAL1','HCAL2','M.D.1','M.D.2'/
      DATA TPL2/
     &  'V1','V2','IT',4*'TP','E1','E2','E3','H1','H2','M1','M2'/
      DATA QCP/-0.012883/,QCP10/0.12883/
      DATA TETC1,TETC2/-179.,-1./
      DATA TETT1,TETT2/-179.,-1./
C     DATA TETC1,TETC2/-185., 5./
C     DATA TETT1,TETT2/-172.,-8./
C     DATA TETT1,TETT2/-175.,-5./
C     DATA TETT1,TETT2/-185., 5./
      DIMENSION HRB(4),VRB(4),SCA(4)
      CALL DQCL(IAREDO)
      BORD=MOD(DFWIDU(IZOMDO),10.)
      CALL DQWIL(BORD)
      CALL DDRFLO
C     ......... IN FT ONLY 3D TRACKS ARE DRAWN -> AT LEAST 1 POINT IN THE TPC.
      FZTRDT=.TRUE.
      CALL DHTINP
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PDF,J_PTE,J_PDT,J_PP1,J_PP2,J_PPU'
      CALL DPARAM(10
     &  ,J_PFI,J_PDF,J_PTE,J_PDT,J_PP1,J_PP2,J_PPU)
      TPARDA=
     &  'J_HEO'
      CALL DPARAM(20
     &  ,J_HEO)
      TPARDA=
     &  'J_SSZ'
      CALL DPARAM(62
     &  ,J_SSZ)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      L1=PARADA(2,J_PP1)-1000.
      L2=PARADA(2,J_PP2)-1000.
      K1=KPL(L1)
      K2=KPL(L2)
      HHGH=HHGHDG(IAREDO)
      VHGH=VHGHDG(IAREDO)
      CALL DGRBST(HRB,VRB,FEMTY)
      IF(K1.EQ.3.AND.K2.EQ.3) THEN
        IF(FEMTY) THEN
          CALL DFRSP(0)
          CALL DRTSP
          CALL DFRSP(1)
        END IF
      END IF
      CALL DFTSK0(FSKW)
      IF(PARADA(4,J_PPU).EQ.-1.) THEN
         PU=0.
      ELSE
         PU=QCP*PARADA(2,J_PPU)
      END IF
      IF(FECEDE) CALL DQLAST('STORE')
      IF(.NOT.FEMTY) THEN
        CALL DGRBSC(HRB,VRB,SCA,HM3,VM3,ADUM)
        IZOMDO=0
        CALL DQRU(HRB,VRB)
      ELSE IF(IZOMDO.EQ.0) THEN
        IF(K2.LE.KTPCDI.AND.(.NOT.FSKW)) THEN
          TETA1=TETT1
          TETA2=TETT2
        ELSE
          TETA1=TETC1
          TETA2=TETC2
        END IF
        PHI1=-1.
        PHI2=400.
        CALL DQRER(0,TETA1,PHI1,TETA2,PHI2,HRB,VRB)
        CALL DFTSKW(HRB,VRB)
        CALL DQRU(HRB,VRB)
        FI=180.
        TE=90.
      ELSE
        CALL DFRRCB(HRB,VRB)
        DT=PARADA(2,J_PDT)*0.5
        TE=PARADA(2,J_PTE)
        DF=PARADA(2,J_PDF)*0.5
        FI=PARADA(2,J_PFI)
        CALL DFTRCB(HRB,VRB)
        CALL DFTSKW(HRB,VRB)
        CALL DQRU(HRB,VRB)
        IF(FECEDE) THEN
          CALL DFTEN(FI,TE)
          HHGHDG(IAREDO)=HHGH
          VHGHDG(IAREDO)=VHGH
          CALL DQLAST('RESTORE')
          GO TO 900
        END IF
      END IF
      CALL DCTYP0(1)
      IF(IHTRDO(1).EQ.0) GO TO 2
      DMODE=PDCODD(2,MODEDD)
C     STYLE=PDCODD(2,ISTYDD)
      COLTP=PDCODD(4,KCTPDD)
      COLEC=PDCODD(4,KCECDD)
      COLHC=PDCODD(4,KCHCDD)
      COLM1=PDCODD(4,KCM1DD)
      COLM2=PDCODD(4,KCM2DD)
C//   SZTPC=WDSNDL(2,4,MTPCDL)
      SZTPC=PARADA(2,J_SSZ)
      IF(COLIDF(4,5).EQ.1.) THEN
C       IF(STYLE.EQ.0.) PDCODD(2,ISTYDD)=1.
C       IF(STYLE.EQ.3.) PDCODD(2,ISTYDD)=2.
        PDCODD(4,KCTPDD)=1.
        PDCODD(4,KCECDD)=1.
        PDCODD(4,KCHCDD)=1.
        PDCODD(4,KCM1DD)=1.
        PDCODD(4,KCM2DD)=1.
        PDCODD(2,MODEDD)=1.
      ELSE
        PDCODD(4,KCTPDD)=-1.
        PDCODD(4,KCECDD)=-1.
        PDCODD(4,KCHCDD)=-1.
        PDCODD(4,KCM1DD)=-1.
        PDCODD(4,KCM2DD)=-1.
        PDCODD(2,MODEDD)=0.
      END IF
      IF(K1.LE.KTPCDI) THEN
         IF(COLIDF(4,6).EQ.1..AND.IZOMDO.EQ.0)
     &     PARADA(2,J_SSZ)=COLIDF(2,6)
C//  &     WDSNDL(2,4,MTPCDL)=COLIDF(2,6)
         CALL DODTPC(' FT')
      ELSE IF(K1.EQ.KECADI.OR.K2.EQ.11) THEN
         CALL DODLCA(' FT')
         CALL DODECA(' FT','bar+ec')
      ELSE IF(K1.EQ.KHCADI) THEN
         CALL DODHCA(' FT','bar+ec')
      ELSE IF(K1.EQ.KMDEDI) THEN
        CALL DODMUD(' FT','B+MA+E',1)
C       LAYER (=1) CANNOT YET BE VARIED WITH K1, BUT VIA GT:CD:DA:B2=ON
      END IF
      CALL DQSC0('0 SC')
      CALL DF180
C     PDCODD(2,ISTYDD)=STYLE
      PDCODD(4,KCTPDD)=COLTP
      PDCODD(4,KCECDD)=COLEC
      PDCODD(4,KCHCDD)=COLHC
      PDCODD(4,KCM1DD)=COLM1
      PDCODD(4,KCM2DD)=COLM2
      PDCODD(2,MODEDD)=DMODE
      IF(PU.NE.0..AND.(.NOT.FPIKDP).AND.K1.GT.1) THEN
C                                                               PULL
         IF(COLIDF(4,4).EQ.1.) THEN
C          ----------------- DCTYCD MUST BE CALLED HERE, AS WHEN FIRST (SECOND
C          ----------------- CAL IGNORED) CALLED BY DV0, IHTRDO(6) IS WRONG.
           CALL DCTYCD
           IHTR6=IHTRDO(6)
           IHTRDO(6)=1
           P1=PARADA(2,J_PP1)
           P2=PARADA(2,J_PP2)
           IF(K1.NE.KTPCDI) THEN
             PARADA(2,J_PP1)=1001.
             PARADA(2,J_PP2)=1004.
           END IF
           DLINDD=PDCODD(2,LIGLDD)
           CALL DAPPT('VP',DVTP,TPCODB,0)
           DLINDD=PDCODD(2,LITRDD)
           PARADA(2,J_PP1)=P1
           PARADA(2,J_PP2)=P2
           IHTRDO(6)=IHTR6
         END IF
      END IF
C      IF(IZOMDO.EQ.0) THEN
C         IF(COLIDF(4,1).EQ.1.AND.COLIDF(2,1).NE.0.)
C     1     CALL DFTLT(FI,TE)
C         GO TO 2
C      END IF
      CALL VZERO(JORD,3)
      DO   700  J1=1,2
         DO   710  J2=J1+1,3
            IF(COLIDF(2,J1).GT.COLIDF(2,J2)) THEN
               JORD(J1)=JORD(J1)+1
            ELSE
               JORD(J2)=JORD(J2)+1
            END IF
  710    CONTINUE
  700 CONTINUE
      DO 19 J=1,3
         GO TO (11,12,13),JORD(J)+1
C  11    IF(K1.EQ.KTPCDI.AND.COLIDF(4,1).EQ.1.) CALL DFTLT(FI,TE)
   11    CONTINUE
         GO TO 19
   12    IF(IZOMDO.EQ.0) GO TO 19
         IF(K1.LE.KECADI.AND.K2.GE.KECADI) THEN
            CALL DFTLE(FI,TE,'EC')
         ELSE IF(K2.EQ.11) THEN
            CALL DFTLE(FI,TE,'HC')
         ELSE IF(K1.EQ.KTPCDI.AND.COLIDF(4,2).EQ.1) THEN
C           IF(MDEB.EQ.0) THEN
C             CALL DFTLE(FI,TE,'EC')
C           ELSE
C             CALL DFTLE(FI,TE,'TP')
C           END IF
           CALL DFTLE(FI,TE,'EC')
         ELSE IF(K1.EQ.KHCADI.AND.COLIDF(4,2).EQ.1) THEN
            CALL DFTLE(FI,TE,'HC')
         END IF
         GO TO 19
   13    IF(IZOMDO.EQ.0) GO TO 19
         IF(K1.LE.KHCADI.AND.K2.GE.KHCADI) THEN
            CALL DFTLH(FI,TE)
         ELSE IF(K1.EQ.KECADI.AND.COLIDF(4,3).EQ.1.) THEN
            CALL DFTLH(FI,TE)
         END IF
   19 CONTINUE
    2 IF(K1.LE.KTPCDI.AND.K2.GE.KTPCDI) THEN
         IF(IHTRDO(1).EQ.0) THEN
            CALL DFTET1(TSKW,PU)
C//         WDSNDL(2,4,MTPCDL)=SZTPC
            PARADA(2,J_SSZ)=SZTPC
            CALL DQFR(IAREDO)
            HHGHDG(IAREDO)=HHGH
            VHGHDG(IAREDO)=VHGH
            GO TO 900
         END IF
         IF(IHTRDO(1).GE.2.AND.IHTRDO(1).LT.6) THEN
            IF(IHTRDO(2).EQ.1) THEN
               CALL DFTTT(FI,TE,TSKW,PU,FTPEC,0)
               IF(PU.LT.0.) CALL DFTTT(FI,TE,TSKW,-PU,FTPEC,0)
            ELSE IF(IHTRDO(2).EQ.6) THEN
               CALL DAP_KALMAN_HE
            ELSE IF(IHTRDO(2).GE.4) THEN
               CALL DAP_KALMAN_CH
            ELSE
               CALL DFTETA(FI,TE,TSKW,PU,FTPEC,K1+1)
            END IF
         END IF
         IF(FTPCDR.AND.IHTRDO(1).LE.2) THEN
           IF(IHTRDO(6).LE.1) THEN
             CALL DAPPT('FT',DVTP,TPCODB,K1+1)
           ELSE IF(IHTRDO(6).EQ.2) THEN
             CALL DAPPT('FT',DVPADA,TPADDB,K1+1)
           ELSE IF(IHTRDO(6).EQ.3) THEN
             CALL DAPPT('FT',DVTP,TBCODB,K1+1)
           END IF
         END IF
         IF(IZOMDO.EQ.1) THEN
           CALL DAPVX('FT',K1+1)
           CALL DAP_KNV('FT',K1+1)
         END IF
      END IF
      IF(K1.LE.1) THEN
        IF(K1.EQ.0.AND.K2.EQ.1) THEN
          CALL DAPPV('FI',1)
          CALL DAPPV('TE',1)
          CALL DAPPV('FI',2)
          CALL DAPPV('TE',2)
        ELSE
          LAYER=K1+1
          CALL DAPPV('FI',LAYER)
          CALL DAPPV('TE',LAYER)
          CALL DAPVC('FT',LAYER)
C         The vertex detector can only be combined with the TPC, if X,Y,Z are
C         corrected to X-VRTXDV, Y-VRTYDV, Z-VRDZDV, when calculating B,FI,TE!
        END IF
      END IF
    3 CALL DAPEF('FT')
      IF(K2.EQ.11) THEN
        CALL DAPEO('FT')
      ELSE
        IF(K1.LE.KECADI.AND.KECADI.LE.K2) THEN
          CALL DECGL('FT')
          CALL DECPL('FT')
          CALL DFTPE(FI,TE)
          IF(PARADA(4,J_HEO).EQ.1.) CALL DAPEO('CH')
        END IF
        IF(K1.LE.KHCADI.AND.KHCADI.LE.K2) CALL DFTPH(FI,TE)
      END IF
      CALL DAPTRF('FT')
      IF(FPIKDP) THEN
         HHGHDG(IAREDO)=HHGH
         VHGHDG(IAREDO)=VHGH
      ELSE
         CALL DQDWI
C        FT TP-E1 /=1.3 GEV 123<FI<123 123<TE<123
C        FT  ECAL1 - ECAL3  123<FI<123 123<TE<123
C        123456789 123456789 123456789 123456789 1
         TXTADW='FT'
         IF(L1.EQ.L2.OR.(L1.GE.1.AND.L2.LE.4))THEN
           TXTADW(4:8)=TPL5(L1)
         ELSE IF(L1.GT.4.OR.PU.EQ.0.) THEN
           TXTADW(5:17)=TPL5(L1)//' - '//TPL5(L2)
         ELSE
           TXTADW(4:8)=TPL2(L1)//'-'//TPL2(L2)
         END IF
         IF(PU.NE.0..AND.L1.LE.4.AND.L2.GT.0) THEN
            GEV45=QCP10/ABS(PU)
            GEV45=GEV45*(HRB(3)-HRB(1))/(VRB(3)-VRB(1))
            GEV45=GEV45*(VHGHDG(IAREDO)-VLOWDG(IAREDO))
     1        /         (HHGHDG(IAREDO)-HLOWDG(IAREDO))
            TXTADW(10:18)='/='//DT3(GEV45)//' Gev'
         END IF
         CALL DCTYEX(TXTADW,18)
         IF(K1.EQ.K2) THEN
            IF(K1.EQ.KTPCDI) THEN
               CALL DLSITX(MTPCDL)
            ELSE IF(K1.EQ.KECADI) THEN
               CALL DLSITX(MECADL)
            ELSE
               CALL DLSITX(MHCADL)
            END IF
         END IF
         IF(FEMTY) THEN
           CALL DQSCA('V',VRB(2),VRB(3),' deg',4, '&f',2)
           IF(IZOMDO.EQ.0.AND.FSKW) THEN
C            ............................... HLOWDG AND HHGHDG ARE CHANGED AGAIN
             CALL DQPOC(-180.,0.,HLOWDG(IAREDO),VDUM,FDUM)
             CALL DQPOC(   5.,0.,HHGHDG(IAREDO),VDUM,FDUM)
             CALL DQSCA('H',-180.,5.,' deg',4,'-&j',3)
           ELSE
             CALL DQSCA('H',HRB(1),HRB(2),' deg',4,'-&j',3)
           END IF
         ELSE
           CALL DQSCA('H',SCA(1),SCA(2),' ',0,' ',0)
           CALL DQSCA('V',SCA(3),SCA(4),' ',0,' ',0)
         END IF
    9    HHGHDG(IAREDO)=HHGH
         VHGHDG(IAREDO)=VHGH
         CALL DQFR(IAREDO)
         CALL DPCSAR
      END IF
C//   WDSNDL(2,4,MTPCDL)=SZTPC
      PARADA(2,J_SSZ)=SZTPC
  900 END
*DK DFTDIS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTDIS
CH
      SUBROUTINE DFTDIS(F1,H1,V1,F2,H2,V2,FM,HM,VM,D,FC)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C------------------------------------------------------------------- DQ
      COMMON /DQQQ1C/ AHSCDQ,BHSCDQ,CHSCDQ,AVSCDQ,BVSCDQ,CVSCDQ
      COMMON /DQQQ2C/ DMAXDQ
      COMMON /DQQQ3C/ FPRSDQ,FPSQDQ
      LOGICAL FPRSDQ,FPSQDQ
      COMMON /DQQQ4C/ PRSHDQ,PRSVDQ,PRS1DQ,PRS2DQ,PRV1DQ
      COMMON /DQQQ5C/ HUSRDQ(4,0:12),VUSRDQ(4,0:12)
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      LOGICAL FC
      IF(FC) THEN
         IF(ABS(V1-VM).LT.180.) THEN
            FKTRDP=FM
            CALL DQL2EP(H1,V1,HM,VM)
            CALL DQL2EP(H1,V1+360.,HM,VM+360.)
         END IF
         IF(ABS(VM-V2).LT.180.) THEN
            FKTRDP=FM
            CALL DQL2EP(HM,VM,H2,V2)
            CALL DQL2EP(HM,VM+360.,H2,V2+360.)
         END IF
         RETURN
      END IF
      FM=0.5*(F1+F2)
      HM=-DHELIX(FM,IVTEDV)
      VM=DHELIX(FM,IVFIDV)
      VM=DFINXT(VMIN,VM)
      IF(PU.NE.0.) HM=HM-PU*DHELIX(FM,IVRBDV)
      DHU=H2-H1
      DVU=V2-V1
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
         CALL DQL2EP(H1,V1+360.,H2,V2+360.)
         FC=.FALSE.
      ELSE
         FC=.TRUE.
         D=DM
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DFTDS0
CH
      ENTRY DFTDS0(PUIN,VMI)
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
      PU=PUIN
      VMIN=VMI
      END
*DK DFTEN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTEN
CH
      SUBROUTINE DFTEN(FIMID,TEMID)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DE
      COMMON /DECAEN/ FECEDE,ENECDE(7)
      LOGICAL FECEDE
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      LOGICAL FOUT
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
      CALL DFTCUE(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
      CALL VZERO(ENECDE,7)
      CALL DV0(ESDADB,NUM1,NUM2,FOUT)
      IF(FOUT) GO TO 100
      DO 11 K=NUM1,NUM2
         CALL DCUTEC(K,FOUT)
         IF(FOUT) GO TO 11
         TE=DVEC(IVTEDV,K)
         IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 11
         FFI=DVEC(IVFIDV,K)
         FI=DFINXT(FIMID,FFI)
         IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 11
         H=-TE
         KK=DVEC(IVKKDV,K)+3
         EN=DVEC(IVENDV,K)
         IF(EN.GT.0.) ENECDE(KK)=ENECDE(KK)+EN
   11 CONTINUE
  100 CALL DV0(HSDADB,NUM1,NUM2,FOUT)
      IF(FOUT) GO TO 200
      DO 111 K=NUM1,NUM2
         CALL DCUTHC(K,FOUT)
         IF(FOUT) GO TO 111
         TE=DVHC(IVTEDV,K)
         IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 111
         FFI=DVHC(IVFIDV,K)
         FI=DFINXT(FIMID,FFI)
         IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 111
         H=-TE
         EN=DVHC(IVENDV,K)
         IF(EN.GT.0.) ENECDE(7)=ENECDE(7)+EN
  111 CONTINUE
  200 CALL DV0(PLSDDB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
      DO 211 K=NUM1,NUM2
         CALL DCUTLC(K,FOUT)
         IF(FOUT) GO TO 211
         TE=DVLC(IVTEDV,K)
         IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 211
         FFI=DVLC(IVFIDV,K)
         FI=DFINXT(FIMID,FFI)
         IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 211
         H=-TE
         KK=DVLC(IVKKDV,K)
         EN=DVLC(IVENDV,K)
         IF(EN.GT.0.) ENECDE(KK)=ENECDE(KK)+EN
  211 CONTINUE
      END
*DK DFTET1
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTET1
CH
      SUBROUTINE DFTET1(TSKW,PU)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DT
      PARAMETER (MTRKDT=401,MMCTDT=500)
      COMMON /DTRK1C/ FCUTDT,FNOTDT(   0   :MTRKDT),
     &                FCUHDT,FNOHDT(-MTRKDT:MTRKDT)
      LOGICAL FCUTDT,FNOTDT,FCUHDT,FNOHDT
      COMMON /DTRK2C/ VTX1DT(3),VTX2DT(3),PTRADT(3),CLTMDT(3),FVTXDT
      LOGICAL FVTXDT
      COMMON /DTRK6C/ FRFTDT(6),XYZVDT(3)
      COMMON /DTRK7C/ N3DIDT,FZTRDT,NSVXDT,KINVDT(2,MTRKDT)
      LOGICAL FZTRDT
      COMMON /DTRK3C/ MODEDT,TPHEDT(5)
      COMMON /DTRK4C/ FMCCDT,FMNTDT(MMCTDT),FMVXDT(100)
      LOGICAL FMCCDT,FMNTDT,FMVXDT
      COMMON /DTRK5C/ SZVXDT,NCVXDT
      COMMON /DTRK1T/ TGFTDT,TGTLDT,TGCLDT
      CHARACTER *4 TGFTDT,TGTLDT,TGCLDT
C PARAMETER (MTRKDT=100,MMCTDT=500) is found in:
C C_DALBC.FOR,C_DALBD.FOR,C_DALBH.FOR,C_DALBM.FOR,C_DALBR.FOR,C_DALBT.FOR
C C_DALBV.FOR,C_DALBX.FOR,C_DALBY.FOR,C_DALIA.FOR,C_DALID.FOR,C_DALIR.FOR
C C_DALIS.FOR,C_DALIV.FOR,N_DALBA.FOR,N_DALBF.FOR,US3.FOR
C------------------------------------------------------------------- DL
      PARAMETER (MPNPDL=62,MPDVDL=6,MPWDDL=8,MPRADL=8)
      COMMON /DLUTSC/MNUMDL,MVDEDL,MITCDL,MTPCDL,MECADL,MHCADL,MMDEDL,
     1  MGRCDL(MPDVDL),MVARDL(MPDVDL,2),MODEDL(MPDVDL,2),
     1  MCOLDL(MPDVDL),MSYMDL(MPDVDL,0:1),
     1  MLABDL(MPDVDL),MDUMDL,
     1  WDSNDL(4,MPWDDL,MPDVDL),RAMPDL(4,MPRADL,MPDVDL,2)
C     COMMON /DLUTSC/MNUMDL,MVDEDL,MITCDL,MTPCDL,MECADL,MHCADL,MMDEDL,
C    1  MGRCDL(MPDVDL),
C    1  MCOLDL(MPDVDL),
C    1  MLABDL(MPDVDL),MDUMDL
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      MODEDT=1
      CALL DCIRCL
      CALL DGLEVL(MCOLDL(MTPCDL))
      CALL DFT1TR(PU,-1.)
      END
*DK DFTETA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTETA
CH
      SUBROUTINE DFTETA(FIMID,TEMID,TSKW,PU,FTPEC,LAYVD)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DO
      COMMON /DOPR4C/ NOARDO,
     1  NTF1DO,NSP1DO,IRZ1DO,IHTRDO(8)
      COMMON /DOPR5C/ FGETDO
      LOGICAL FGETDO
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DT
      PARAMETER (MTRKDT=401,MMCTDT=500)
      COMMON /DTRK1C/ FCUTDT,FNOTDT(   0   :MTRKDT),
     &                FCUHDT,FNOHDT(-MTRKDT:MTRKDT)
      LOGICAL FCUTDT,FNOTDT,FCUHDT,FNOHDT
      COMMON /DTRK2C/ VTX1DT(3),VTX2DT(3),PTRADT(3),CLTMDT(3),FVTXDT
      LOGICAL FVTXDT
      COMMON /DTRK6C/ FRFTDT(6),XYZVDT(3)
      COMMON /DTRK7C/ N3DIDT,FZTRDT,NSVXDT,KINVDT(2,MTRKDT)
      LOGICAL FZTRDT
      COMMON /DTRK3C/ MODEDT,TPHEDT(5)
      COMMON /DTRK4C/ FMCCDT,FMNTDT(MMCTDT),FMVXDT(100)
      LOGICAL FMCCDT,FMNTDT,FMVXDT
      COMMON /DTRK5C/ SZVXDT,NCVXDT
      COMMON /DTRK1T/ TGFTDT,TGTLDT,TGCLDT
      CHARACTER *4 TGFTDT,TGTLDT,TGCLDT
C PARAMETER (MTRKDT=100,MMCTDT=500) is found in:
C C_DALBC.FOR,C_DALBD.FOR,C_DALBH.FOR,C_DALBM.FOR,C_DALBR.FOR,C_DALBT.FOR
C C_DALBV.FOR,C_DALBX.FOR,C_DALBY.FOR,C_DALIA.FOR,C_DALID.FOR,C_DALIR.FOR
C C_DALIS.FOR,C_DALIV.FOR,N_DALBA.FOR,N_DALBF.FOR,US3.FOR
C------------------------------------------------------------------- DP
      COMMON /DPCOLR/ ICOLDP
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DC
      PARAMETER (MCTRDC=1000)
      COMMON /DCOTR/ FDTRDC,FVTRDC,NCTRDC(0:MCTRDC)
      LOGICAL FDTRDC,FVTRDC
C------------------------------------------------------------------- DK
      COMMON /DKNSTC/ PIFCDK,RMAXDK,ZMAXDK,TCO1DK,TCO2DK,
     1   ROTODK(-1:10),ZZTODK(-1:10),ZMTODK(-1:10),DMAXDK,RMINDK,
     1   RLOWDK
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      EQUIVALENCE (KPIKDP,N)
      DATA W2/2./,IDEB/0/
      LOGICAL FOUT,DVM0
      LOGICAL DCCVX
      EXTERNAL DVTR0
      DIMENSION ROVD(2,2)
      CALL DPARGV(87,'STR',2,DLINDD)
      IF(LAYVD.GT.2) THEN
        RVDM=-1.
      ELSE
        CALL DGIVDR(ROVD)
        RVDM=0.5*(ROVD(1,LAYVD)+ROVD(2,LAYVD))
        IF(IDEB.EQ.1) THEN
          ROVD(1,LAYVD)=RVDM
          ROVD(2,LAYVD)=RVDM
        END IF
      END IF
      CALL DAPTRN('FT',0)
      IF(IHTRDO(2).LE.2) THEN
         CALL DV0(FRFTDB,NUM1,NUM2,FOUT)
         IF(FOUT) RETURN
         MODEDT=1
         CALL DPAR_SET_CO(58,'CFR')
         IF(.NOT.FPIKDP.AND.ICOLDP.GT.0.) THEN
           CALL DPARGV(87,'SFR',2,W2)
           W2=2.*W2
           DLINDD=DLINDD+W2
           DO 301 N=NUM1,NUM2
             IF(FNOTDT(N)) GO TO 301
             CALL DVTRV(N)
             CALL DVXT(N,XYZVDT)
             IF(IZOMDO.EQ.0) THEN
               CALL DFT1TR(PU,RVDM)
             ELSE
               IF(LAYVD.GT.2) THEN
                 CALL DFT1TR(PU,-1.)
               ELSE
                 CALL DFT1TR(PU,ROVD(1,LAYVD))
                 CALL DFT1TR(PU,ROVD(2,LAYVD))
               END IF
             END IF
  301      CONTINUE
           DLINDD=DLINDD-W2
         END IF
         CALL DSCTR
         DO 1 N=NUM1,NUM2
           IF(FNOTDT(N)) GO TO 1
           IF(FVTRDC) CALL DGLEVL(NCTRDC(N))
           CALL DVTRV(N)
           CALL DVXT(N,XYZVDT)
           IF(IZOMDO.EQ.0) THEN
             CALL DFT1TR(PU,RVDM)
           ELSE
             IF(LAYVD.GT.2) THEN
               CALL DFT1TR(PU,-1.)
             ELSE
               CALL DFT1TR(PU,ROVD(1,LAYVD))
               CALL DFT1TR(PU,ROVD(2,LAYVD))
             END IF
           END IF
           CALL DAPTRN('FT',N)
    1    CONTINUE
      ELSE
         MODEDT=2
         CALL DMCNVX(NVTX1)
         IF(NVTX1.LE.0) RETURN
         DO 12 NV=1,NVTX1
            CALL DMCVX(NV,KT1,KT2)
            IF(KT2.LE.0) GO TO 12
            DO 11 N=KT1,KT2
               IF(DVM0(N)) GO TO 11
               CALL DMCTR(N,FOUT)
               IF(FOUT) GO TO 11
               CALL DFT1TR(PU,RVDM)
               IF(IZOMDO.EQ.0) THEN
                 CALL DFT1TR(PU,RVDM)
               ELSE
                 CALL DFT1TR(PU,ROVD(1,LAYVD))
                 CALL DFT1TR(PU,ROVD(2,LAYVD))
               END IF
   11       CONTINUE
   12    CONTINUE
         IF(.NOT.FPIKDP) THEN
            IF(NVTX1.LE.0) RETURN
            CALL DPAR_SET_CO(49,'CMC')
            IF(ICOLDP.LT.0) RETURN
            CALL DPARGV(67,'SSY',2,SYMB)
            CALL DPARGV(67,'SSZ',2,SYSZ)
            MSYMB=SYMB
            CALL DQPD0(MSYMB,SZVXDT*0.1*SYSZ,0.)
            CALL DQPD0(3,DS,0.)
C           CALL DGLEVL(NCVXDT)
            DO 13 N=1,NVTX1
               IF(FMVXDT(N)) THEN
                  CALL DVMCVX(N,KDUM1,KDUM2,KDUM3,VTX1DT)
                  IF(DCCVX(VTX1DT)) GO TO 13
                  IF(ABS(VTX1DT(3)).GT.ZMAXDK) GO TO 13
                  RO=DVCORD(VTX1DT,IVRODV)
                  IF(RO.LT.RMINDK.OR.RO.GT.RMAXDK) GO TO 13
                  H=-DVCORD(VTX1DT,IVTEDV)
                  V= DFINXT(FIMID,DVCORD(VTX1DT,IVFIDV))
                  IF(PU.NE.0.) THEN
                     DPU=PU*DVCORD(VTX1DT,IVRBDV)
                  ELSE
                     DPU=0.
                  END IF
                  CALL DQPD(H-DPU,V)
                  IF(PU.LT.0.) THEN
                     CALL DQPD(H+DPU,V)
                  END IF
                  IF(IZOMDO.EQ.0) THEN
                     CALL DQPD(H-DPU,V+360.)
                     IF(PU.LT.0.) THEN
                        CALL DQPD(H+DPU,V+360.)
                     END IF
                  END IF
               END IF
   13       CONTINUE
         END IF
      END IF
      END
*DK DFTLE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTLE
CH
      SUBROUTINE DFTLE(FIMID,TEMID,TD)
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
C     KDEB = 1  DRAW TOTAL GRID
C ---------------------------------------------------------------------
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DF
      PARAMETER (MCOLDF=10)
      COMMON /DFTCLC/ COLIDF(4,MCOLDF),COLMDF(4,3)
C------------------------------------------------------------------- DT
      COMMON /DTVCCC/ DUMYDT(4),
     1         HUMIDT,HUMADT,VUMIDT,VUMADT,
     1         NSQUDT,NOBJDT,NOCLDT,HPOSDT,VPOSDT,
     1         FPOSDT,TPOSDT,AROTDT,RPOSDT,NTVIDT,
     1         FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,
     1         FX11DT
      LOGICAL  FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,FX11DT
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *(*) TD
      DIMENSION S1(2,2),LEV(2),NSY(2)
      DATA S1/1.,-1.,1.,1./,NSY/8,6/,N1/1/,L1/1/,L8/8/
      DATA DDF/0./,DDT/0./
      DATA KDEB/0/
      LOGICAL FOUT
      EQUIVALENCE (JJ,JECA),(II,IECA)
      IF(KDEB.EQ.0) THEN
        CALL DV0(ESDADB,NUM1,NUM2,FOUT)
        IF(FOUT) RETURN
      ELSE
        CALL DQLEVL(LCEGDD)
      END IF
      IF(FPIKDP.OR.COLIDF(2,2).EQ.0.) RETURN
      CALL DCUTFT(FP1,FP2,FC1,FC2,TP1,TP2,TC1,TC2)
      CALL DFTCUE(FP1,FP2,FC1,FC2,TP1,TP2,TC1,TC2)
      DLINDD=PDCODD(2,LIGLDD)
      IF(FMONDT) THEN
        J1=2
        LEV(2)=8
      ELSE
        IF     (TD.EQ.'EC') THEN
          J1=N1
          LEV(1)=L1
          LEV(2)=PDCODD(2,LCEGDD)
C        ELSE IF(TD.EQ.'TP') THEN
C          J1=N1
C          LEV(1)=PDCODD(2,ICECDD)
C          LEV(2)=PDCODD(2,LCETDD)
        ELSE IF(TD.EQ.'HC') THEN
          J1=2
          LEV(2)=L8
        END IF
      END IF
      IF(KDEB.EQ.0) THEN
        DO J=J1,2
          CALL DQPU0(NSY(J),S1(1,J))
          CALL DGLEVL(LEV(J))
          DO 1 K=NUM1,NUM2
            CALL DCUTEC(K,FOUT)
            IF(FOUT) GO TO 1
            TE=DVEC(IVTEDV,K)
            IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 1
            FI=DVEC(IVFIDV,K)
            FI=DFINXT(FIMID,FI)
            IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 1
            H=-TE
            DF=DVEC(IVDFDV,K)+DDF
            DT=DVEC(IVDTDV,K)+DDT
            CALL DQPU(H,FI,DT,DF)
    1     CONTINUE
        END DO
      ELSE
        CALL DQPU0(6,S1(1,2))
        KECA=KDEB
        DO 702 JECA=1,228
          IF     (JECA.GE.51.AND.JECA.LE.178) THEN
C           BARREL : I=1,384
            I2=384
          ELSE IF(JECA.GE.41.AND.JECA.LE.188) THEN
C           ENDCAP : I=1,384
            I2=384
          ELSE IF(JECA.GE.25.AND.JECA.LE.204) THEN
C           ENDCAP : I=1,288
            I2=288
          ELSE IF(JECA.GE.9.AND.JECA.LE.220) THEN
C           ENDCAP : I=1,192
            I2=192
          ELSE
C           ENDCAP : I=1,96
            I2=96
          END IF
          DO 703 IECA=1,I2
            CALL=DVEC1(IECA,JECA,KECA)
            TE=DVEC(IVTEDV,0)
            IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 703
            FI=DVEC(IVFIDV,0)
            FI=DFINXT(FIMID,FI)
            IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 703
            H=-TE
            DF=DVEC(IVDFDV,0)+DDF
            DT=DVEC(IVDTDV,0)+DDT
            CALL DQPU(H,FI,DT,DF)
  703     CONTINUE
  702   CONTINUE
      END IF
      DLINDD=PDCODD(2,LITRDD)
      RETURN
      END
*DK DFTLH
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTLH
CH
      SUBROUTINE DFTLH(FIMID,TEMID)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DF
      PARAMETER (MCOLDF=10)
      COMMON /DFTCLC/ COLIDF(4,MCOLDF),COLMDF(4,3)
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DT
      COMMON /DTVCCC/ DUMYDT(4),
     1         HUMIDT,HUMADT,VUMIDT,VUMADT,
     1         NSQUDT,NOBJDT,NOCLDT,HPOSDT,VPOSDT,
     1         FPOSDT,TPOSDT,AROTDT,RPOSDT,NTVIDT,
     1         FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,
     1         FX11DT
      LOGICAL  FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,FX11DT
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION S1(2,2),LEV(2),NSY(2)
      DATA LEV/1,7/,S1/1.,-1.,1.,1./,N1/1/,NSY/8,6/
      DATA DDF/0./,DDT/0./
      LOGICAL FOUT
      CALL DV0(HSDADB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
      IF(FPIKDP.OR.COLIDF(2,3).EQ.0.) RETURN
      CALL DCUTFT(FP1,FP2,FC1,FC2,TP1,TP2,TC1,TC2)
      CALL DFTCUE(FP1,FP2,FC1,FC2,TP1,TP2,TC1,TC2)
      DLINDD=PDCODD(2,LIGLDD)
      LEV(2)=PDCODD(2,LCHGDD)
      IF(FMONDT) THEN
        L1=2
      ELSE
        L1=N1
      END IF
      DO L=L1,2
        CALL DQPU0(NSY(L),S1(1,L))
        CALL DGLEVL(LEV(L))
        DO 1 K=NUM1,NUM2
          TE=DVHC(IVTEDV,K)
          IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 1
          FI=DVHC(IVFIDV,K)
          FI=DFINXT(FIMID,FI)
          IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 1
          H=-TE
          DF=DVHC(IVDFDV,K)+DDF
          DT=DVHC(IVDTDV,K)+DDT
          CALL DQPU(H,FI,DT,DF)
    1   CONTINUE
      END DO
      END
C*DK DFTLT
CCH..............+++
CCH
CCH
CCH
CCH
CCH
CCH
CCH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTLT
CCH
C      SUBROUTINE DFTLT(FI,TE)
CCH
CCH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CCH
CC ---------------------------------------------------------------------
CC
CC    Created by H.Drevermann                   28-JUL-1988
CC
CC!:
CC    Inputs    :
CC    Outputs   :
CC
CC    Called by :
CC ---------------------------------------------------------------------
C*CA DALLCO
C      INCLUDE 'DALI_CF.INC'
C      CHARACTER *1,TSKW
C      DIMENSION HH(2),VV(2)
C      IF(FPIKDP.OR.COLIDF(2,1).EQ.0..OR.IZOMDO.EQ.1) RETURN
C      LEV=COLIDF(2,1)
C      CALL DGLEVL(LEV)
CC      IF(IZOMDO.EQ.0) THEN
C         I1=0
C         I2=390
C         ID=30
C         N1=-180
C         N2=0
C         ND=30
C      HH(1)=-180.
C      HH(2)=0.
C      DO   700  I=I1,I2,ID
C         VV(1)=I
C         VV(2)=VV(1)
C         CALL DQLIE(HH,VV)
C  700 CONTINUE
C      VV(1)=-1.
C      VV(2)=400.
C      DO   710  N=N1,N2,ND
C         HH(1)=N
C         HH(2)=HH(1)
C         CALL DQLIE(HH,VV)
C  710 CONTINUE
C      RETURN
C      END
*DK DFTRCB
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTRCB
CH
      SUBROUTINE DFTRCB(HRB,VRB)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION HRB(4),VRB(4)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PDF,J_PTE,J_PDT'
      CALL DPARAM(10
     &  ,J_PFI,J_PDF,J_PTE,J_PDT)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      DT=PARADA(2,J_PDT)*0.5
      TE=PARADA(2,J_PTE)
      DF=PARADA(2,J_PDF)*0.5
      FI=PARADA(2,J_PFI)
      HRB(1)=-PARADA(2,J_PTE)-0.5*PARADA(2,J_PDT)
      HRB(2)=-PARADA(2,J_PTE)+0.5*PARADA(2,J_PDT)
      HRB(3)=HRB(2)
      HRB(4)=HRB(1)
      VRB(1)= PARADA(2,J_PFI)-0.5*PARADA(2,J_PDF)
      VRB(2)=VRB(1)
      VRB(3)= PARADA(2,J_PFI)+0.5*PARADA(2,J_PDF)
      VRB(4)=VRB(3)
      END
*DK DFTOE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTOE
CH
      SUBROUTINE DFTOE(FIMID,TEMID)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DS
      COMMON /DSCALE/ SCALDS(0:12,3)
      PARAMETER (MSECDS=1,MSHCDS=2,MSWIDS=3)
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      EQUIVALENCE (KPIKDP,K)
      CHARACTER *3 DT3,T3
C      EXTERNAL DVEO
      DATA SC/50./
      LOGICAL FOUT
      CALL DV0(PECODB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
      IF(IZOMDO.NE.0) CALL DFTCUE(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
      CALL DPARGV(13,'PHI',2,PHI)
      CALL DPARGV(69,'SWI',2,WID)
      QE=PHI*10.
C//   CALL DQCH0(QE,WDSNDL(2,7,MECADL))
      CALL DQCH0(QE,WID)
      T3=DT3(SC/QE)
      CALL DGLEVL(8)
      CALL DQSCE(T3//'Gev EO',9,SC)
      SCALDS(IAREDO,MSECDS)=QE
      CALL DQLEVL(LCEODD)
      DO 1 K=NUM1,NUM2
         TE=DVEO(IVTEDV,K)
         IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 1
         FFI=DVEO(IVFIDV,K)
         FI=DFINXT(FIMID,FFI)
         IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 1
         H=-TE
         IF(FPIKDP) THEN
            CALL DQPIK(H,FI)
            GO TO 1
         END IF
         E=DVEO(IVENDV,K)
         CALL DQCH(H,FI,E)
         IF(IZOMDO.EQ.0) CALL DQCH(H,FI+360.,E)
    1 CONTINUE
      END
*DK DFTPE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTPE
CH
      SUBROUTINE DFTPE(FIMID,TEMID)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DU
      PARAMETER (MPNPDU=60)
      REAL DACUDU
      COMMON /DUSDAC/ NUNIDU,
     1 WAITDU,ECCHDU,BOXSDU,ECSQDU,HCSQDU,
     1 HIMXDU,RESCDU,SLOWDU,WISUDU,SETTDU,
     1 USLVDU,SYLRDU,SDATDU,RZDHDU,ROTADU,
     1 SCMMDU,CLMMDU,CUMMDU,WIMIDU,CCMIDU,
     1 ECMIDU,HCMIDU,SHTFDU(2)    ,FTDZDU,
     1 FRNLDU,DACUDU,SHECDU,SHHCDU,DRLIDU,
     1 SQLCDU,CHLCDU,HEADDU,VRDZDU,CHHCDU,
     1 AREADU,DFWIDU(0:1)  ,DFLGDU,PBUGDU,
     1 CHTFDU,RNCLDU,PR43DU,PR44DU,PR45DU,
     1 DSOFDU,TPOFDU,ROVDDU,ROECDU,ROHCDU,
     1 HSTRDU,REALDU,PR53DU,PR54DU,PR55DU,
     1 PR56DU(4),                  PR60DU
      DIMENSION PARADU(MPNPDU)
      EQUIVALENCE (PARADU,WAITDU)
      COMMON /DUSDA2/PLIMDU(2,MPNPDU)
      COMMON /DUSDAT/ TPR1DU(MPNPDU)
      CHARACTER *2 TPR1DU
C------------------------------------------------------------------- DS
      COMMON /DSCALE/ SCALDS(0:12,3)
      PARAMETER (MSECDS=1,MSHCDS=2,MSWIDS=3)
C------------------------------------------------------------------- DC
      PARAMETER (MCECDC=9000)
      COMMON /DCOEC/ FDECDC,FVECDC,NCECDC(MCECDC)
      LOGICAL FDECDC,FVECDC
C------------------------------------------------------------------- DW
      COMMON /DWORKT/ TXTADW
      CHARACTER *80 TXTADW
      CHARACTER *1 TXT1DW(80)
      EQUIVALENCE (TXTADW,TXT1DW)
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      EQUIVALENCE (KPIKDP,K)
      CHARACTER *3 DT3,T3
      DATA FOVER/40./
      DATA DFSET,DTSET/2*15./,SC/50./
      DIMENSION ICAR(8)
      EXTERNAL DVEC
      LOGICAL FOUT
      DATA KDEB/0/
      CALL DV0(ESDADB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PP1,J_PP2'
      CALL DPARAM(11
     &  ,J_PP1,J_PP2)
      TPARDA=
     &  'J_SVS,J_SSY,J_SSZ,J_SSS,J_SWI,J_SHI'
      CALL DPARAM(63
     &  ,J_SVS,J_SSY,J_SSZ,J_SSS,J_SWI,J_SHI)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
      IF(IZOMDO.NE.0) CALL DFTCUE(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
      NOTSH=0
      K1=PARADA(2,J_PP1)-1004.
      K2=PARADA(2,J_PP2)-1004.
      DF=DFSET
      DT=DTSET
      MOSY=PARADA(2,J_SVS)
      CALL DQPD0(IDPAR(J_SSY),PARADA(2,J_SSZ),0.)
      CALL DQPU0(IDPAR(J_SSY),SHTFDU)
      CALL DQSQ0(PARADA(2,J_SSS)*10.,SHTFDU)
      QE=PARADA(2,J_SHI)*10.
      IF(MOSY.EQ.2.AND.QE.NE.0.) THEN
        CALL DQCH0(QE,PARADA(2,J_SWI))
        T3=DT3(SC/QE)
        CALL DGLEVL(8)
        CALL DQSCE(T3//'Gev EC',9,SC)
        SCALDS(IAREDO,MSECDS)=QE
      END IF
      IF(FPIKDP) GO TO 11
      CALL DSET_ECAL_COL(MODE,NUCOL,ICCN,ICAR)
      CALL DPARGI(70,'FEC',MOCO)
      CALL DSCELM(K1,K2,MOSY,MOCO,FVECDC,IM)
      GO TO (10,20,30,40,50) IM
   10 CALL DPAR_SET_CO(53,'CCN')
C     ................................ constant color 1 loop
   11 MODE=0
      GO TO 21
   20 MODE=-1
C     ........................................... varying color 1 loop
   21 L1=1
      L2=1
      LD=1
      GO TO 100
   30 MODE=1
C     .......................................... varying color forward loop
      L1=1
      L2=NUCOL
      LD=1
      GO TO 100
   40 MODE=1
C     .......................................... varying color backward loop
      L1=NUCOL
      L2=1
      LD=-1
      GO TO 100
   50 CALL DPAR_SET_CO(53,'CCN')
C     ....................... k1#k2: stacks are added; constant color is used
      GO TO 200
C  100 IF(MOCO.EQ.1) THEN
C        CALL DPAR_GET_ARRAY(47,'CC1',8,ICAR)
C      ELSE IF(MOCO.EQ.2) THEN
C        CALL DPAR_GET_ARRAY(53,'CE1',2,ICAR)
C      END IF
  100 IF(KDEB.EQ.1) THEN
        WRITE(TXTADW,5273) 'IM',IM,'L1',L1,'L2',L2,'LD',LD,'MODE',MODE
 5273   FORMAT(5(A,'=',I2,4X))
        CALL DWRC
      END IF
      DO 118 L=L1,L2,LD
C       ......... MODE =-1 : non overlapping colors; draw in 1 loop
C       ......... MODE = 0 : Color=constant
C       ......... MODE = 1 : overlapping colors draw 8->1 if squares 1-> if CN
        IF(MODE.EQ.1) THEN
          NCOL=ICAR(L)
          CALL DGLEVL(NCOL)
        END IF
        DO 115 K=NUM1,NUM2
          IF(MODE) 111,112,113
  111     CALL DGLEVL(ICAR(NCECDC(K)))
          GO TO 112
  113     IF(NCECDC(K).NE.L) GO TO 115
  112     CALL DCUTEC(K,FOUT)
          IF(FOUT) GO TO 115
          KK=DVEC(IVKKDV,K)
          IF(KK.LT.K1.OR.KK.GT.K2) GO TO 115
          TE=DVEC(IVTEDV,K)
          IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 115
          FFI=DVEC(IVFIDV,K)
          FI=DFINXT(FIMID,FFI)
          IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 115
          H=-TE
          J=DVEC(IVJJDV,K)
          IF(IZOMDO.NE.0) THEN
            DF=DVEC(IVDFDV,K)
            DT=DVEC(IVDTDV,K)
          END IF
          IF(FPIKDP) THEN
            CALL DQPIF(H,FI)
            GO TO 115
          END IF
          IF(IZOMDO.EQ.0.AND.(MOSY.EQ.4.OR.MOSY.EQ.1)) THEN
            CALL DQPD(H,FI)
            IF(FI.LT.FOVER) CALL DQPD(H,FI+360.)
            GO TO 115
          END IF
          IF(MOSY.EQ.4) THEN
            T3=DT3(DVEC(IVENDV,K))
            CALL DQTXT(H-DT,FI,T3,3)
            IF(IZOMDO.EQ.0.AND.FI.LT.FOVER)
     &        CALL DQTXT(H-DT,FI+360.,T3,3)
          ELSE IF(MOSY.EQ.5) THEN
            SQ=DVEC(IVENDV,K)
            CALL DQSQ(H,FI,SQ,DT,DF)
            IF(IZOMDO.EQ.0.AND.FI.LT.FOVER)
     &         CALL DQSQ(H,FI+360.,SQ,DT,DF)
          ELSE IF(MOSY.EQ.2) THEN
            EN=DVEC(IVENDV,K)
            CALL DQCH(H,FI,EN)
            IF(IZOMDO.EQ.0.AND.FI.LT.FOVER)
     &         CALL DQCH(H,FI+360.,EN)
          ELSE
            CALL DQPU(H,FI,DT,DF)
            IF(IZOMDO.EQ.0.AND.FI.LT.FOVER)
     &         CALL DQPU(H,FI+360.,DT,DF)
          END IF
  115   CONTINUE
  118 CONTINUE
      GO TO 999
C     ....................................... stacks are added
C     ....................................... color is set constant
  200 K=NUM1-1
C     DO K=1,NUM2  ..................    loop parameter is changed inside loop
  221   K=K+1
        IF(K.GT.NUM2) GO TO 999
        CALL DCUTEC(K,FOUT)
        IF(FOUT) GO TO 221
        EN=DVEC(IVENDV,K)
        TE=DVEC(IVTEDV,K)
        IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 221
        FFI=DVEC(IVFIDV,K)
        FI=DFINXT(FIMID,FFI)
        IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 221
        H=-TE
        KK=DVEC(IVKKDV,K)
        IF(KK.LT.K1.OR.KK.GT.K2) GO TO 221
        JJ=DVEC(IVJJDV,K)
        II=DVEC(IVIIDV,K)
        ES=EN
C       .................... esda is ordered with stacks behind each other
  222   K=K+1
        IF(K.LE.NUM2.AND.
     &      DVEC(IVJJDV,K).EQ.JJ.AND.
     &      DVEC(IVIIDV,K).EQ.II.AND.
     &      DVEC(IVKKDV,K).GE.K1.AND.
     &      DVEC(IVKKDV,K).LE.K2)     THEN
          EN=DVEC(IVENDV,K)
          CALL DCUTEC(K,FOUT)
          IF(FOUT)GO TO 222
          ES=ES+EN
          GO TO 222
        END IF
        K=K-1
        IF(MOSY.EQ.4) THEN
          DT=DVEC(IVDTDV,K)
          T3=DT3(ES)
          CALL DQTXT(H-DT,FI,T3,3)
        ELSE IF(MOSY.EQ.5) THEN
          IF(IZOMDO.NE.0) THEN
            DF=DVEC(IVDFDV,K)
            DT=DVEC(IVDTDV,K)
          END IF
          CALL DQSQ(H,FI,ES,DT,DF)
        ELSE IF(MOSY.EQ.2) THEN
          CALL DQCH(H,FI,ES)
          IF(IZOMDO.EQ.0) CALL DQCH(H,FI+360.,ES)
        END IF
        GO TO 221
C     END DO    ........................... end of assumed loop
  999 END
*DK DFTPH
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTPH
CH
      SUBROUTINE DFTPH(FIMID,TEMID)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DU
      PARAMETER (MPNPDU=60)
      REAL DACUDU
      COMMON /DUSDAC/ NUNIDU,
     1 WAITDU,ECCHDU,BOXSDU,ECSQDU,HCSQDU,
     1 HIMXDU,RESCDU,SLOWDU,WISUDU,SETTDU,
     1 USLVDU,SYLRDU,SDATDU,RZDHDU,ROTADU,
     1 SCMMDU,CLMMDU,CUMMDU,WIMIDU,CCMIDU,
     1 ECMIDU,HCMIDU,SHTFDU(2)    ,FTDZDU,
     1 FRNLDU,DACUDU,SHECDU,SHHCDU,DRLIDU,
     1 SQLCDU,CHLCDU,HEADDU,VRDZDU,CHHCDU,
     1 AREADU,DFWIDU(0:1)  ,DFLGDU,PBUGDU,
     1 CHTFDU,RNCLDU,PR43DU,PR44DU,PR45DU,
     1 DSOFDU,TPOFDU,ROVDDU,ROECDU,ROHCDU,
     1 HSTRDU,REALDU,PR53DU,PR54DU,PR55DU,
     1 PR56DU(4),                  PR60DU
      DIMENSION PARADU(MPNPDU)
      EQUIVALENCE (PARADU,WAITDU)
      COMMON /DUSDA2/PLIMDU(2,MPNPDU)
      COMMON /DUSDAT/ TPR1DU(MPNPDU)
      CHARACTER *2 TPR1DU
C------------------------------------------------------------------- DS
      COMMON /DSCALE/ SCALDS(0:12,3)
      PARAMETER (MSECDS=1,MSHCDS=2,MSWIDS=3)
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C------------------------------------------------------------------- DC
      PARAMETER (MCHCDC=2000)
      COMMON /DCOHC/ FDHCDC,FVHCDC,NCHCDC(MCHCDC)
      COMMON /DCOHC1/ LCHCDC
      LOGICAL FDHCDC,FVHCDC
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      EQUIVALENCE (KPIKDP,K)
      CHARACTER *3 DT3,T3
      EXTERNAL DVHC
      DATA DFSET,DTSET/2*15/,SC/50./
      LOGICAL FOUT
      CALL DV0(HSDADB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
C      CALL DORCAL
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
      IF(IZOMDO.NE.0) CALL DFTCUE(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PP1,J_PP2'
      CALL DPARAM(11
     &  ,J_PP1,J_PP2)
      TPARDA=
     &  'J_SVS,J_SSY,J_SSZ,J_SSS,J_SWI,J_SHI'
      CALL DPARAM(64
     &  ,J_SVS,J_SSY,J_SSZ,J_SSS,J_SWI,J_SHI)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      K1=PARADA(2,J_PP1)-1007.
      K2=PARADA(2,J_PP2)-1007.
      MO=PARADA(2,J_SVS)
      CALL DQPD0(IDPAR(J_SSY),PARADA(2,J_SSZ),0.)
      CALL DQPU0(IDPAR(J_SSY),SHTFDU)
      CALL DQSQ0(PARADA(2,J_SSS)*3.,SHTFDU)
      QE=PARADA(2,J_SHI)*CHHCDU
      IF(MO.EQ.2.AND.QE.NE.0.) THEN
        CALL DQCH0(QE,PARADA(2,J_SWI))
        T3=DT3(SC/QE)
        CALL DGLEVL(8)
        CALL DQSCE(T3//'Gev HC',9,SC)
        SCALDS(IAREDO,MSHCDS)=QE
      END IF
      CALL DSCHC
      DO 1 K=NUM1,NUM2
         CALL DCUTHC(K,FOUT)
         IF(FOUT) GO TO 1
         KK=DVHC(IVKKDV,K)
         IF(KK.LT.K1.OR.KK.GT.K2) GO TO 1
         TE=DVHC(IVTEDV,K)
         IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 1
         FFI=DVHC(IVFIDV,K)
         FI=DFINXT(FIMID,FFI)
         IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 1
C         IF(FNCON) CALL DLCOLH(DVHC,MHCADL,K,NCOL)
         IF(FVHCDC) CALL DGLEVL(NCHCDC(K))
         H=-TE
         IF(FPIKDP) THEN
            CALL DQPIK(H,FI)
            GO TO 1
         END IF
         IF(MO.EQ.4) THEN
           DF=DVHC(IVDFDV,K)
           DT=DVHC(IVDTDV,K)
C//        T3=DT3(DVHC(MV,K))
           T3=DT3(DVHC(IVENDV,K))
           IF(IZOMDO.EQ.0) CALL DQTXT(H-DT,FI+360.,T3,3)
           CALL DQTXT(H-DT,FI,T3,3)
         ELSE IF(MO.EQ.2) THEN
           EN=DVHC(IVENDV,K)
           CALL DQCH(H,FI,EN)
           IF(IZOMDO.EQ.0) CALL DQCH(H,FI+360.,EN)
         ELSE IF(MO.EQ.5) THEN
           SQ=DVHC(IVENDV,K)
           IF(IZOMDO.EQ.0) THEN
             CALL DQSQ(H,FI,SQ,DTSET,DFSET)
             CALL DQSQ(H,FI+360.,SQ,DTSET,DFSET)
           ELSE
             DF=DVHC(IVDFDV,K)
             DT=DVHC(IVDTDV,K)
             CALL DQSQ(H,FI,SQ,DT,DF)
           END IF
         ELSE
           DF=DVHC(IVDFDV,K)
           DT=DVHC(IVDTDV,K)
           CALL DQPU(H,FI,DT,DF)
           IF(IZOMDO.EQ.0)CALL DQPU(H,FI+360.,DT,DF)
         END IF
    1 CONTINUE
      END
*DK DFTPIT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTPIT
CH
      SUBROUTINE DFTPIT(F1,F2,PU,VMIN)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      PARAMETER (MXYZDP=19)
      COMMON /DPICK2/ NXYZDP,XYZPDP(3,19)
      COMMON /DPICK3/ NTRKDP,LTRKDP(8)
      COMMON /DPICK4/ NUMPDP,LORPDP
      PARAMETER (MTRSDP=26)
      COMMON /DPICK5/ TRSTDP(MTRSDP)
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
C      DATA FP/20./,KP/20/
C      DF=(F2-F1)/FP
C      F=F1
C      DO 700 K=1,KP
C        F=F+DF
      DF=F2-F1
      DO 700 K=1,MTRSDP
        F=F1+TRSTDP(K)*DF
        V1=DFINXT(VMIN,DHELIX(F,IVFIDV))
        T1=DHELIX(F,IVTEDV)
        H1=-T1-PU*DHELIX(F,IVRBDV)
        CALL DQPIK(H1,V1)
        CALL DQPIK(H1,V1+360.)
        IF(PU.LT.0.) THEN
          H1=-T1+PU*DHELIX(F,IVRBDV)
          CALL DQPIK(H1,V1)
          CALL DQPIK(H1,V1+360.)
        END IF
  700 CONTINUE
      END
*DK DFTSKW
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTSKW
CH
      SUBROUTINE DFTSKW(HRB,VRB)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION HRB(4),VRB(4)
      LOGICAL FSKW,F1
      F1=.FALSE.
      GO TO 1
CH
CH
CH
      ENTRY DFTSK0(FSKW)
      F1=.TRUE.
      FSKW=.FALSE.
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
    1 TPARDA=
     &  'J_PSK'
      CALL DPARAM(13
     &  ,J_PSK)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DPARGV(63,'SVS',2,SYME)
      CALL DPARGV(64,'SVS',2,SYMH)
      CALL DPARGV(20,'HEO',4,EF)
      IF(SYME.EQ.2..OR.SYMH.EQ.2..OR.EF.EQ.2.) THEN
        IF(PARADA(4,J_PSK).EQ.1.) THEN
          SK=PARADA(2,J_PSK)
          IF(SK.NE.0.) THEN
            IF(F1) THEN
              FSKW=.TRUE.
              RETURN
            END IF
            DV=0.01*SK*(VRB(1)-VRB(3))
            IF(IZOMDO.NE.0) THEN
              HRB(1)=HRB(1)-DV
              HRB(2)=HRB(2)-DV
              HRB(3)=HRB(3)+DV
              HRB(4)=HRB(4)+DV
            ELSE
              DV=2.*DV
              IF(DV.GT.0.) THEN
                HRB(1)=HRB(1)-DV
                HRB(3)=HRB(3)+DV
              ELSE
                HRB(2)=HRB(2)-DV
                HRB(4)=HRB(4)+DV
              END IF
            END IF
          END IF
        END IF
      END IF
      END
*DK DFTTT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFTTT
CH
      SUBROUTINE DFTTT(FIMID,TEMID,TSKW,PU,FTPEC)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DT
      PARAMETER (MTRKDT=401,MMCTDT=500)
      COMMON /DTRK1C/ FCUTDT,FNOTDT(   0   :MTRKDT),
     &                FCUHDT,FNOHDT(-MTRKDT:MTRKDT)
      LOGICAL FCUTDT,FNOTDT,FCUHDT,FNOHDT
      COMMON /DTRK2C/ VTX1DT(3),VTX2DT(3),PTRADT(3),CLTMDT(3),FVTXDT
      LOGICAL FVTXDT
      COMMON /DTRK6C/ FRFTDT(6),XYZVDT(3)
      COMMON /DTRK7C/ N3DIDT,FZTRDT,NSVXDT,KINVDT(2,MTRKDT)
      LOGICAL FZTRDT
      COMMON /DTRK3C/ MODEDT,TPHEDT(5)
      COMMON /DTRK4C/ FMCCDT,FMNTDT(MMCTDT),FMVXDT(100)
      LOGICAL FMCCDT,FMNTDT,FMVXDT
      COMMON /DTRK5C/ SZVXDT,NCVXDT
      COMMON /DTRK1T/ TGFTDT,TGTLDT,TGCLDT
      CHARACTER *4 TGFTDT,TGTLDT,TGCLDT
C PARAMETER (MTRKDT=100,MMCTDT=500) is found in:
C C_DALBC.FOR,C_DALBD.FOR,C_DALBH.FOR,C_DALBM.FOR,C_DALBR.FOR,C_DALBT.FOR
C C_DALBV.FOR,C_DALBX.FOR,C_DALBY.FOR,C_DALIA.FOR,C_DALID.FOR,C_DALIR.FOR
C C_DALIS.FOR,C_DALIV.FOR,N_DALBA.FOR,N_DALBF.FOR,US3.FOR
C------------------------------------------------------------------- DC
      PARAMETER (MCTRDC=1000)
      COMMON /DCOTR/ FDTRDC,FVTRDC,NCTRDC(0:MCTRDC)
      LOGICAL FDTRDC,FVTRDC
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION H(2),V(2),VV(2)
      LOGICAL FTPEC
      EQUIVALENCE (K,KPIKDP)
      CHARACTER *1 TSKW
      LOGICAL FOUT
      CALL DSCTR
      CALL DV0(FRTLDB,NUM1,NTRK,FOUT)
      IF(FOUT) RETURN
      CALL DV0(TPCODB,NUM0,NUM2,FOUT)
      IF(FOUT) RETURN
C     CALL DVTQ0(NUM2)
C     IF(NUM2.EQ.0) RETURN
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
      IF(FTPEC) CALL DFTCUT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PP1,J_PP2'
      CALL DPARAM(11
     &  ,J_PP1,J_PP2)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      K1=PARADA(2,J_PP1)-1000.
      K2=PARADA(2,J_PP2)-1000.
      CALL DMAXLG('X',K1,0.,R1)
      CALL DMAXLG('X',K2,0.,R2)
C      CALL DLHITR('TR',MTPCDL,ICOL)
      DO 3 M=1,NTRK
         IF(FCUTDT) THEN
            IF(FNOTDT(M)) GO TO 3
         END IF
         CALL=DVCHT(M,NUM)
         IF(NUM.LE.0) GO TO 3
         KK1=IDVCHT(1)
C         CALL DLCOLT(M,NCOL)
C         IF(NCOL.EQ.0) GO TO 3
C         IF(ICOL.NE.0) CALL DGLEVL(ICOL)
         IF(FVTRDC) CALL DGLEVL(NCTRDC(M))
         I=0
         HS=-9999.
    2    DO 1 KIND=1,NUM
            K=IDVCHT(KIND)
            IF(FPIMDP.AND.K.NE.NPIKDP) GO TO 1
            RO=DVTP(IVRODV,K)
            IF(RO.LT.R1.OR.R2.LT.RO) THEN
               I=0
               GO TO 1
            END IF
            TE=DVTP(IVTEDV,K)
            IF(TE.LT.TC1.OR.TE.GT.TC2) THEN
               I=0
               GO TO 1
            END IF
            FI=DFINXT(FIMID,DVTP(IVFIDV,K))
            IF(FI.LT.FC1.OR.FI.GT.FC2) THEN
               I=0
               GO TO 1
            END IF
            I=I+1
            H(I)=-TE
            V(I)= FI
            IF(PU.NE.0.) H(I)=H(I)-PU*DVTP(IVRBDV,K)
            IF(K.EQ.KK1) THEN
               HS=H(1)
               VS=V(1)
               VVS=VV(1)
            END IF
            IF(FPIKDP) THEN
               CALL DQPIF(H,V)
               I=0
            ELSE IF(I.EQ.2) THEN
               IF(ABS(V(2)-V(1)).LE.90.) THEN
                  CALL DQLIE(H,V)
                  IF(IZOMDO.EQ.0) THEN
                     VV(1)=V(1)+360.
                     VV(2)=V(2)+360.
                     CALL DQLIE(H,VV)
                  END IF
               END IF
               I=1
               H(1)=H(2)
               V(1)=V(2)
            END IF
    1    CONTINUE
    3 CONTINUE
      END
*DK DFZ1TR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFZ1TR
CH
      SUBROUTINE DFZ1TR
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DT
      PARAMETER (MTRKDT=401,MMCTDT=500)
      COMMON /DTRK1C/ FCUTDT,FNOTDT(   0   :MTRKDT),
     &                FCUHDT,FNOHDT(-MTRKDT:MTRKDT)
      LOGICAL FCUTDT,FNOTDT,FCUHDT,FNOHDT
      COMMON /DTRK2C/ VTX1DT(3),VTX2DT(3),PTRADT(3),CLTMDT(3),FVTXDT
      LOGICAL FVTXDT
      COMMON /DTRK6C/ FRFTDT(6),XYZVDT(3)
      COMMON /DTRK7C/ N3DIDT,FZTRDT,NSVXDT,KINVDT(2,MTRKDT)
      LOGICAL FZTRDT
      COMMON /DTRK3C/ MODEDT,TPHEDT(5)
      COMMON /DTRK4C/ FMCCDT,FMNTDT(MMCTDT),FMVXDT(100)
      LOGICAL FMCCDT,FMNTDT,FMVXDT
      COMMON /DTRK5C/ SZVXDT,NCVXDT
      COMMON /DTRK1T/ TGFTDT,TGTLDT,TGCLDT
      CHARACTER *4 TGFTDT,TGTLDT,TGCLDT
C PARAMETER (MTRKDT=100,MMCTDT=500) is found in:
C C_DALBC.FOR,C_DALBD.FOR,C_DALBH.FOR,C_DALBM.FOR,C_DALBR.FOR,C_DALBT.FOR
C C_DALBV.FOR,C_DALBX.FOR,C_DALBY.FOR,C_DALIA.FOR,C_DALID.FOR,C_DALIR.FOR
C C_DALIS.FOR,C_DALIV.FOR,N_DALBA.FOR,N_DALBF.FOR,US3.FOR
C------------------------------------------------------------------- DO
      COMMON /DOPR4C/ NOARDO,
     1  NTF1DO,NSP1DO,IRZ1DO,IHTRDO(8)
      COMMON /DOPR5C/ FGETDO
      LOGICAL FGETDO
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      EXTERNAL DFZDIS
      DIMENSION FFRTO(7)
C                VX VD IT T0 T3 E1 E3
      DATA FFRTO/0.,0.,3.,4.,5.,6.,7./
      DATA IPDEB/0/
      LOGICAL FBAR(2),FOUT
      LOGICAL DCCTR
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PTE,J_PTO'
      CALL DPARAM(11
     &  ,J_PTE,J_PTO)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      TEMID=PARADA(2,J_PTE)
      IF(MODEDT.NE.2) THEN
         CALL=DHELX0(FFRTO(2),FFRTO(3),FFRTO(4),FFRTO(6),FBAR)
      ELSE
         FFRTO(2)=0.
         CALL=DHELX1(FFRTO(3),FFRTO(4),FFRTO(6),FBAR,FOUT)
         IF(FOUT) RETURN
      END IF
      IF(DCCTR(FFRTO(4),FFRTO(5))) RETURN
      IHTFR=MAX(MIN(IHTRDO(3),7),4)
      IHTTO=MIN(IHTRDO(4),7)
      IF(IHTFR.GE.IHTTO) RETURN
      F1=FFRTO(IHTFR)
      H1=DHELIX(F1,IVZZDV)
      V1=DHELIX(F1,IVFIDV)
      F2=FFRTO(IHTTO)
      IF(PARADA(2,J_PTO).GE.1001.AND.
     &  PARADA(2,J_PTO).LE.1004.) THEN
         F2=MIN(F2,FFRTO(5))
      ELSE
         IF(FBAR(1)) THEN
            F2=MIN(F2,FFRTO(5))
         END IF
      END IF
      IF(F1.GE.F2) RETURN
      IF(FPIKDP.AND.IPDEB.EQ.1) THEN
        CALL DFZPIT(F1,F2)
      ELSE
        H2=DHELIX(F2,IVZZDV)
        V2=DHELIX(F2,IVFIDV)
        VMIN=MIN(V1,V2)
        V1=DFINXT(VMIN,V1)
        V2=DFINXT(VMIN,V2)
        CALL DFZDS0(VMIN,TEMID)
        CALL DDRAWA(DFZDIS,F1,H1,V1,F2,H2,V2)
      END IF
      END
*DK DFZVDD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFZVDD
CH
      SUBROUTINE DFZVDD
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DU
      PARAMETER (MPNPDU=60)
      REAL DACUDU
      COMMON /DUSDAC/ NUNIDU,
     1 WAITDU,ECCHDU,BOXSDU,ECSQDU,HCSQDU,
     1 HIMXDU,RESCDU,SLOWDU,WISUDU,SETTDU,
     1 USLVDU,SYLRDU,SDATDU,RZDHDU,ROTADU,
     1 SCMMDU,CLMMDU,CUMMDU,WIMIDU,CCMIDU,
     1 ECMIDU,HCMIDU,SHTFDU(2)    ,FTDZDU,
     1 FRNLDU,DACUDU,SHECDU,SHHCDU,DRLIDU,
     1 SQLCDU,CHLCDU,HEADDU,VRDZDU,CHHCDU,
     1 AREADU,DFWIDU(0:1)  ,DFLGDU,PBUGDU,
     1 CHTFDU,RNCLDU,PR43DU,PR44DU,PR45DU,
     1 DSOFDU,TPOFDU,ROVDDU,ROECDU,ROHCDU,
     1 HSTRDU,REALDU,PR53DU,PR54DU,PR55DU,
     1 PR56DU(4),                  PR60DU
      DIMENSION PARADU(MPNPDU)
      EQUIVALENCE (PARADU,WAITDU)
      COMMON /DUSDA2/PLIMDU(2,MPNPDU)
      COMMON /DUSDAT/ TPR1DU(MPNPDU)
      CHARACTER *2 TPR1DU
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DT
      PARAMETER (MTRKDT=401,MMCTDT=500)
      COMMON /DTRK1C/ FCUTDT,FNOTDT(   0   :MTRKDT),
     &                FCUHDT,FNOHDT(-MTRKDT:MTRKDT)
      LOGICAL FCUTDT,FNOTDT,FCUHDT,FNOHDT
      COMMON /DTRK2C/ VTX1DT(3),VTX2DT(3),PTRADT(3),CLTMDT(3),FVTXDT
      LOGICAL FVTXDT
      COMMON /DTRK6C/ FRFTDT(6),XYZVDT(3)
      COMMON /DTRK7C/ N3DIDT,FZTRDT,NSVXDT,KINVDT(2,MTRKDT)
      LOGICAL FZTRDT
      COMMON /DTRK3C/ MODEDT,TPHEDT(5)
      COMMON /DTRK4C/ FMCCDT,FMNTDT(MMCTDT),FMVXDT(100)
      LOGICAL FMCCDT,FMNTDT,FMVXDT
      COMMON /DTRK5C/ SZVXDT,NCVXDT
      COMMON /DTRK1T/ TGFTDT,TGTLDT,TGCLDT
      CHARACTER *4 TGFTDT,TGTLDT,TGCLDT
C PARAMETER (MTRKDT=100,MMCTDT=500) is found in:
C C_DALBC.FOR,C_DALBD.FOR,C_DALBH.FOR,C_DALBM.FOR,C_DALBR.FOR,C_DALBT.FOR
C C_DALBV.FOR,C_DALBX.FOR,C_DALBY.FOR,C_DALIA.FOR,C_DALID.FOR,C_DALIR.FOR
C C_DALIS.FOR,C_DALIV.FOR,N_DALBA.FOR,N_DALBF.FOR,US3.FOR
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C------------------------------------------------------------------- DR
      COMMON /DRNG1C/
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      LOGICAL
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      COMMON /DRNG2C/DET1DR(6),DET2DR(6)
C------------------------------------------------------------------- DO
      COMMON /DOPR4C/ NOARDO,
     1  NTF1DO,NSP1DO,IRZ1DO,IHTRDO(8)
      COMMON /DOPR5C/ FGETDO
      LOGICAL FGETDO
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DL
      PARAMETER (MPNPDL=62,MPDVDL=6,MPWDDL=8,MPRADL=8)
      COMMON /DLUTSC/MNUMDL,MVDEDL,MITCDL,MTPCDL,MECADL,MHCADL,MMDEDL,
     1  MGRCDL(MPDVDL),MVARDL(MPDVDL,2),MODEDL(MPDVDL,2),
     1  MCOLDL(MPDVDL),MSYMDL(MPDVDL,0:1),
     1  MLABDL(MPDVDL),MDUMDL,
     1  WDSNDL(4,MPWDDL,MPDVDL),RAMPDL(4,MPRADL,MPDVDL,2)
C     COMMON /DLUTSC/MNUMDL,MVDEDL,MITCDL,MTPCDL,MECADL,MHCADL,MMDEDL,
C    1  MGRCDL(MPDVDL),
C    1  MCOLDL(MPDVDL),
C    1  MLABDL(MPDVDL),MDUMDL
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      INCLUDE 'DALI_EX.INC'
      LOGICAL FOUT,FEMTY,FHIST
      DIMENSION HRB(4),VRB(4),HST(5)
      DIMENSION SCA(4)
      DATA HST/0.,0.,0.,0.,0./
      CHARACTER *2 TL(2)
      DATA TL/'V1','V2'/
      CALL DQCL(IAREDO)
      BORD=MOD(DFWIDU(IZOMDO),10.)
      CALL DQWIL(BORD)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PFR,J_PTO,J_FLA,J_FFA,J_FWA,J_FSC,J_FHW'
      CALL DPARAM(10
     &  ,J_PFI,J_PFR,J_PTO,J_FLA,J_FFA,J_FWA,J_FSC,J_FHW)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      FZTRDT=.TRUE.
      CALL DHTINP
      RFR=PARADA(2,J_PFR)
      RTO=PARADA(2,J_PTO)
      CALL DGRBST(HRB,VRB,FEMTY)
      IF(.NOT.FEMTY) THEN
        CALL DGRBSC(HRB,VRB,SCA,HM3,VM3,ADUM)
        IZOMDO=0
      ELSE IF(IZOMDO.EQ.0) THEN
        CALL DQRER(0,-RTO,-1.,RTO,400.,HRB,VRB)
        FI=180.
      ELSE
        CALL DFZRCB(HRB,VRB)
        FI=PARADA(2,J_PFI)
      END IF
      IF(IZOMDO.EQ.0) PARADA(4,J_FWA)=-1.
      LAYER=PARADA(2,J_FLA)
      CALL DGVDSC(LAYER)
      IF(  IZOMDO      .EQ.0. OR.     ! NZ
     &  PARADA(4,J_FWA).LE.0..OR.     ! no wafer SELECTED
     &  PARADA(2,J_FWA).GT.6..OR.     ! no WAFER selected
     &  PARADA(4,J_FHW).LE.0.) THEN   ! pulsehight not selected
        CALL DQRU(HRB,VRB)
        FHIST=.FALSE.
      ELSE
        FHIST=.TRUE.
        H1=HLOWDG(IAREDO)
        V1=VLOWDG(IAREDO)
        H2=HHGHDG(IAREDO)
        V2=VHGHDG(IAREDO)
        DHV=0.01*PARADA(2,J_FHW)*MIN((H2-H1),(V2-V1))
        HHS=H2-DHV
        VHS=V2-DHV
        VLOWDG(IAREDO)=VHS
        HHGHDG(IAREDO)=HHS
        HST(3)=PARADA(2,J_FSC)
        HST(4)=HST(3)
        CALL DQRU(HRB,HST)
        CALL DFZHSH
        CALL DQSCA('V',HST(1),HST(3),' ',0,' ',0)
        VLOWDG(IAREDO)=V1
        VHGHDG(IAREDO)=VHS
        HLOWDG(IAREDO)=HHS
        HHGHDG(IAREDO)=H2
        CALL DQRU(HST(2),VRB)
        CALL DFZHSV
        CALL DQSCA('H',HST(1),HST(3),' ',0,' ',0)
        HLOWDG(IAREDO)=H1
        HHGHDG(IAREDO)=HHS
        VHGHDG(IAREDO)=VHS
        CALL DQRU(HRB,VRB)
      END IF
      CALL DCTYP0(1)
      FVDEDR=.TRUE.
      CALL DADVD(LAYER)
      CALL DAPPV('FZ',LAYER)
      CALL DAPPV('ZZ',LAYER)
      IF(IHTRDO(1).GE.2.AND.IHTRDO(1).LT.6) CALL DFZVEL
   90 IF(.NOT.FPIKDP) THEN
        IF(PARADA(2,J_PTO).LT.1005.) THEN
          CALL DLSITX(MTPCDL)
        ELSE IF(PARADA(2,J_PTO).LT.1008.) THEN
          CALL DLSITX(MECADL)
        ELSE
          CALL DLSITX(MHCADL)
        END IF
        IF(PARADA(2,J_FLA).LE.0.) THEN
          CALL DCTYEX('VD',2)
        ELSE
          CALL DCTYEX(TL(LAYER),2)
        END IF
        IF(FEMTY) THEN
          CALL DQSCA('H',HRB(1),HRB(2),  'cm',2, 'Z',1)
          CALL DQSCA('V',VRB(2),VRB(3),' deg',4,'&f',2)
        ELSE
          CALL DQSCA('H',SCA(1),SCA(2),' ',0,' ',0)
          CALL DQSCA('V',SCA(3),SCA(4),' ',0,' ',0)
        END IF
        CALL DQFR(IAREDO)
        CALL DPCSAR
      END IF
      IF(FHIST) THEN
        HHGHDG(IAREDO)=H2
        VHGHDG(IAREDO)=V2
        CALL DQFR(IAREDO)
      END IF
      END
*DK DFZD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFZD
CH
      SUBROUTINE DFZD
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DU
      PARAMETER (MPNPDU=60)
      REAL DACUDU
      COMMON /DUSDAC/ NUNIDU,
     1 WAITDU,ECCHDU,BOXSDU,ECSQDU,HCSQDU,
     1 HIMXDU,RESCDU,SLOWDU,WISUDU,SETTDU,
     1 USLVDU,SYLRDU,SDATDU,RZDHDU,ROTADU,
     1 SCMMDU,CLMMDU,CUMMDU,WIMIDU,CCMIDU,
     1 ECMIDU,HCMIDU,SHTFDU(2)    ,FTDZDU,
     1 FRNLDU,DACUDU,SHECDU,SHHCDU,DRLIDU,
     1 SQLCDU,CHLCDU,HEADDU,VRDZDU,CHHCDU,
     1 AREADU,DFWIDU(0:1)  ,DFLGDU,PBUGDU,
     1 CHTFDU,RNCLDU,PR43DU,PR44DU,PR45DU,
     1 DSOFDU,TPOFDU,ROVDDU,ROECDU,ROHCDU,
     1 HSTRDU,REALDU,PR53DU,PR54DU,PR55DU,
     1 PR56DU(4),                  PR60DU
      DIMENSION PARADU(MPNPDU)
      EQUIVALENCE (PARADU,WAITDU)
      COMMON /DUSDA2/PLIMDU(2,MPNPDU)
      COMMON /DUSDAT/ TPR1DU(MPNPDU)
      CHARACTER *2 TPR1DU
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DR
      COMMON /DRNG1C/
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      LOGICAL
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      COMMON /DRNG2C/DET1DR(6),DET2DR(6)
C------------------------------------------------------------------- DT
      PARAMETER (MTRKDT=401,MMCTDT=500)
      COMMON /DTRK1C/ FCUTDT,FNOTDT(   0   :MTRKDT),
     &                FCUHDT,FNOHDT(-MTRKDT:MTRKDT)
      LOGICAL FCUTDT,FNOTDT,FCUHDT,FNOHDT
      COMMON /DTRK2C/ VTX1DT(3),VTX2DT(3),PTRADT(3),CLTMDT(3),FVTXDT
      LOGICAL FVTXDT
      COMMON /DTRK6C/ FRFTDT(6),XYZVDT(3)
      COMMON /DTRK7C/ N3DIDT,FZTRDT,NSVXDT,KINVDT(2,MTRKDT)
      LOGICAL FZTRDT
      COMMON /DTRK3C/ MODEDT,TPHEDT(5)
      COMMON /DTRK4C/ FMCCDT,FMNTDT(MMCTDT),FMVXDT(100)
      LOGICAL FMCCDT,FMNTDT,FMVXDT
      COMMON /DTRK5C/ SZVXDT,NCVXDT
      COMMON /DTRK1T/ TGFTDT,TGTLDT,TGCLDT
      CHARACTER *4 TGFTDT,TGTLDT,TGCLDT
C PARAMETER (MTRKDT=100,MMCTDT=500) is found in:
C C_DALBC.FOR,C_DALBD.FOR,C_DALBH.FOR,C_DALBM.FOR,C_DALBR.FOR,C_DALBT.FOR
C C_DALBV.FOR,C_DALBX.FOR,C_DALBY.FOR,C_DALIA.FOR,C_DALID.FOR,C_DALIR.FOR
C C_DALIS.FOR,C_DALIV.FOR,N_DALBA.FOR,N_DALBF.FOR,US3.FOR
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C------------------------------------------------------------------- DO
      COMMON /DOPR4C/ NOARDO,
     1  NTF1DO,NSP1DO,IRZ1DO,IHTRDO(8)
      COMMON /DOPR5C/ FGETDO
      LOGICAL FGETDO
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DL
      PARAMETER (MPNPDL=62,MPDVDL=6,MPWDDL=8,MPRADL=8)
      COMMON /DLUTSC/MNUMDL,MVDEDL,MITCDL,MTPCDL,MECADL,MHCADL,MMDEDL,
     1  MGRCDL(MPDVDL),MVARDL(MPDVDL,2),MODEDL(MPDVDL,2),
     1  MCOLDL(MPDVDL),MSYMDL(MPDVDL,0:1),
     1  MLABDL(MPDVDL),MDUMDL,
     1  WDSNDL(4,MPWDDL,MPDVDL),RAMPDL(4,MPRADL,MPDVDL,2)
C     COMMON /DLUTSC/MNUMDL,MVDEDL,MITCDL,MTPCDL,MECADL,MHCADL,MMDEDL,
C    1  MGRCDL(MPDVDL),
C    1  MCOLDL(MPDVDL),
C    1  MLABDL(MPDVDL),MDUMDL
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      INCLUDE 'DALI_EX.INC'
      LOGICAL FOUT,FEMTY,FHIST
      DIMENSION HRB(4),VRB(4),HST(5)
      DIMENSION SCA(4)
      DATA HST/0.,0.,0.,0.,0./
      DATA DLSH/0./
      CHARACTER *2 TL(2)
      DATA TL/'V1','V2'/
      CALL DQCL(IAREDO)
      BORD=MOD(DFWIDU(IZOMDO),10.)
      CALL DQWIL(BORD)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PFR,J_PTO,J_FLA,J_FFA,J_FWA,J_FSC,J_FHW'
      CALL DPARAM(10
     &  ,J_PFI,J_PFR,J_PTO,J_FLA,J_FFA,J_FWA,J_FSC,J_FHW)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PARADA(2,J_FLA).LE.0.) THEN
        CALL DDRFLG
        FTPCDR=.TRUE.
        IF(FNOPDR) RETURN
      END IF
      FZTRDT=.TRUE.
      CALL DHTINP
      RFR=PARADA(2,J_PFR)
      RTO=PARADA(2,J_PTO)
      CALL DGRBST(HRB,VRB,FEMTY)
      IF(.NOT.FEMTY) THEN
        CALL DGRBSC(HRB,VRB,SCA,HM3,VM3,ADUM)
        IZOMDO=0
      ELSE IF(IZOMDO.EQ.0) THEN
        CALL DQRER(0,-RTO,-1.,RTO,400.,HRB,VRB)
        FI=180.
      ELSE
        CALL DFZRCB(HRB,VRB)
        FI=PARADA(2,J_PFI)
      END IF
      IF(PARADA(2,J_FLA).GT.0.) THEN
        LAYER=PARADA(2,J_FLA)
        CALL DGVDSC(LAYER)
      END IF
      IF(  IZOMDO      .EQ.0. OR.     ! NZ
     &  PARADA(2,J_FLA).LE.0..OR.     ! NO VDET
     &  PARADA(4,J_FWA).LE.0..OR.     ! no wafer SELECTED
     &  PARADA(2,J_FWA).GT.6..OR.     ! no WAFER selected
     &  PARADA(4,J_FHW).LE.0.) THEN   ! pulsehight not selected
        CALL DQRU(HRB,VRB)
        FHIST=.FALSE.
      ELSE
        FHIST=.TRUE.
        H1=HLOWDG(IAREDO)
        V1=VLOWDG(IAREDO)
        H2=HHGHDG(IAREDO)
        V2=VHGHDG(IAREDO)
        DHV=0.01*PARADA(2,J_FHW)*MIN((H2-H1),(V2-V1))
        HHS=H2-DHV
        VHS=V2-DHV
        VLOWDG(IAREDO)=VHS
        HHGHDG(IAREDO)=HHS
        HST(3)=PARADA(2,J_FSC)
        HST(4)=HST(3)
        CALL DQRU(HRB,HST)
        CALL DFZHSH
        CALL DQSCA('V',HST(1),HST(3),' ',0,' ',0)
        VLOWDG(IAREDO)=V1
        VHGHDG(IAREDO)=VHS
        HLOWDG(IAREDO)=HHS
        HHGHDG(IAREDO)=H2
        CALL DQRU(HST(2),VRB)
        CALL DFZHSV
        CALL DQSCA('H',HST(1),HST(3),' ',0,' ',0)
        HLOWDG(IAREDO)=H1
        HHGHDG(IAREDO)=HHS
        VHGHDG(IAREDO)=VHS
        CALL DQRU(HRB,VRB)
      END IF
      CALL DCTYP0(1)
      IF(IHTRDO(1).EQ.0) THEN
         CALL DFZET1(FI,SK,SP,TEMID)
         CALL DQFR(IAREDO)
         GO TO 90
      END IF
      IF(PARADA(2,J_FLA).GT.0.) THEN
        FVDEDR=.TRUE.
        CALL DADVD(LAYER)
        IF(DLSH.GT.0..AND.(.NOT.FPIKDP)) THEN
          PCN2=PDCODD(2,ICCNDD)
          PCN4=PDCODD(4,ICCNDD)
          PDCODD(2,ICCNDD)=1.
          PDCODD(4,ICCNDD)=1.
          DLINDD=DLINDD+DLSH
          CALL DAPPV('FZ',LAYER)
          CALL DAPPV('ZZ',LAYER)
          DLINDD=DLINDD-DLSH
          PDCODD(2,ICCNDD)=PCN2
          PDCODD(4,ICCNDD)=PCN4
        END IF
        CALL DAPPV('FZ',LAYER)
        CALL DAPPV('ZZ',LAYER)
        IF(IHTRDO(1).GE.2.AND.IHTRDO(1).LT.6) CALL DFZVEL
        GO TO 90
      END IF
      CALL DODTPC(' FZ')
      CALL DODECA(' FZ','endcap')
      CALL DODHCA(' FZ','endcap')
      CALL DODMUD(' FZ','ENDCAP',1)
      CALL DQSC0('1 SC')
      CALL DF180
      IF(IHTRDO(1).GE.2.AND.IHTRDO(1).LT.6) THEN
        IF(IHTRDO(2).EQ.1) THEN
          CALL DFZTT(FI,SK,SP,TEMID)
C        ELSE IF(IHTRDO(2).GE.4) THEN
C         CALL D_AP_KALMAN_TR
C       ELSE
C          CALL DFZETA(FI,SK,SP,TEMID)
        ELSE
          CALL DAP_TR
        END IF
      END IF
      IF(IHTRDO(1).LE.2) THEN
        IF(FTPCDR) THEN
          IF(IHTRDO(6).LE.1) THEN
            CALL DAPPT('FZ',DVTP,TPCODB,0)
          ELSE IF(IHTRDO(6).EQ.2) THEN
            CALL DAPPT('FZ',DVPADA,TPADDB,0)
          ELSE IF(IHTRDO(6).EQ.3) THEN
            CALL DAPPT('FZ',DVTP,TBCODB,0)
          END IF
        END IF
      END IF
      CALL DAPVX('FZ',LDUM)
      CALL DAP_KNV('FZ',LDUM)
      CALL DV0(MHITDB,NUM1,NUM2,FOUT)
      IF(.NOT.FOUT) THEN
        CALL=DVMDCU(2.)
        CALL DAPPT('FZ',DVMD,MHITDB,0)
      END IF
      CALL DAPEF('FZ')
      IF(PARADA(2,J_PTO).GE.1005..OR.PARADA(2,J_PTO).LT.1000.) THEN
        CALL DAPEO('FZ')
        CALL DAPPE('FZ')
      END IF
      IF(PARADA(2,J_PTO).GE.1008..OR.PARADA(2,J_PTO).LT.1000.) THEN
        CALL DFZPH(FI,SK,SP,TEMID)
      END IF
      CALL DAPTRF('FZ')
   90 IF(.NOT.FPIKDP) THEN
        IF(PARADA(2,J_PTO).LT.1005.) THEN
          CALL DLSITX(MTPCDL)
        ELSE IF(PARADA(2,J_PTO).LT.1008.) THEN
          CALL DLSITX(MECADL)
        ELSE
          CALL DLSITX(MHCADL)
        END IF
        IF(PARADA(2,J_FLA).LE.0.) THEN
          CALL DCTYEX('FZ',2)
        ELSE
          CALL DCTYEX(TL(LAYER),2)
        END IF
        IF(FEMTY) THEN
          CALL DQSCA('H',HRB(1),HRB(2),  'cm',2, 'Z',1)
          CALL DQSCA('V',VRB(2),VRB(3),' deg',4,'&f',2)
        ELSE
          CALL DQSCA('H',SCA(1),SCA(2),' ',0,' ',0)
          CALL DQSCA('V',SCA(3),SCA(4),' ',0,' ',0)
        END IF
        CALL DQFR(IAREDO)
        CALL DPCSAR
      END IF
      IF(FHIST) THEN
        HHGHDG(IAREDO)=H2
        VHGHDG(IAREDO)=V2
        CALL DQFR(IAREDO)
      END IF
      END
*DK DFZDIS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFZDIS
CH
      SUBROUTINE DFZDIS(F1,H1,V1,F2,H2,V2,FM,HM,VM,D,FC)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C------------------------------------------------------------------- DQ
      COMMON /DQQQ1C/ AHSCDQ,BHSCDQ,CHSCDQ,AVSCDQ,BVSCDQ,CVSCDQ
      COMMON /DQQQ2C/ DMAXDQ
      COMMON /DQQQ3C/ FPRSDQ,FPSQDQ
      LOGICAL FPRSDQ,FPSQDQ
      COMMON /DQQQ4C/ PRSHDQ,PRSVDQ,PRS1DQ,PRS2DQ,PRV1DQ
      COMMON /DQQQ5C/ HUSRDQ(4,0:12),VUSRDQ(4,0:12)
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      LOGICAL FC
      IF(FC) THEN
         FKTRDP=FM
         CALL DQL2EP(H1,V1,HM,VM)
         CALL DQL2EP(H1,V1+360.,HM,VM+360.)
         CALL DQL2EP(HM,VM,H2,V2)
         CALL DQL2EP(HM,VM+360.,H2,V2+360.)
         RETURN
      END IF
      FM=0.5*(F1+F2)
      HM=DHELIX(FM,IVZZDV)
      VM=DHELIX(FM,IVFIDV)
      VM=DFINXT(VMIN,VM)
      DHU=H2-H1
      DVU=V2-V1
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
         CALL DQL2EP(H1,V1+360.,H2,V2+360.)
         FC=.FALSE.
      ELSE
         FC=.TRUE.
         D=DM
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DFZDS0
CH
      ENTRY DFZDS0(VMI,TEM)
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
      VMIN=VMI
      TEMID=TEM
      END
*DK DFZET1
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFZET1
CH
      SUBROUTINE DFZET1(FI,SK,SP,TEMID)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DT
      PARAMETER (MTRKDT=401,MMCTDT=500)
      COMMON /DTRK1C/ FCUTDT,FNOTDT(   0   :MTRKDT),
     &                FCUHDT,FNOHDT(-MTRKDT:MTRKDT)
      LOGICAL FCUTDT,FNOTDT,FCUHDT,FNOHDT
      COMMON /DTRK2C/ VTX1DT(3),VTX2DT(3),PTRADT(3),CLTMDT(3),FVTXDT
      LOGICAL FVTXDT
      COMMON /DTRK6C/ FRFTDT(6),XYZVDT(3)
      COMMON /DTRK7C/ N3DIDT,FZTRDT,NSVXDT,KINVDT(2,MTRKDT)
      LOGICAL FZTRDT
      COMMON /DTRK3C/ MODEDT,TPHEDT(5)
      COMMON /DTRK4C/ FMCCDT,FMNTDT(MMCTDT),FMVXDT(100)
      LOGICAL FMCCDT,FMNTDT,FMVXDT
      COMMON /DTRK5C/ SZVXDT,NCVXDT
      COMMON /DTRK1T/ TGFTDT,TGTLDT,TGCLDT
      CHARACTER *4 TGFTDT,TGTLDT,TGCLDT
C PARAMETER (MTRKDT=100,MMCTDT=500) is found in:
C C_DALBC.FOR,C_DALBD.FOR,C_DALBH.FOR,C_DALBM.FOR,C_DALBR.FOR,C_DALBT.FOR
C C_DALBV.FOR,C_DALBX.FOR,C_DALBY.FOR,C_DALIA.FOR,C_DALID.FOR,C_DALIR.FOR
C C_DALIS.FOR,C_DALIV.FOR,N_DALBA.FOR,N_DALBF.FOR,US3.FOR
C------------------------------------------------------------------- DL
      PARAMETER (MPNPDL=62,MPDVDL=6,MPWDDL=8,MPRADL=8)
      COMMON /DLUTSC/MNUMDL,MVDEDL,MITCDL,MTPCDL,MECADL,MHCADL,MMDEDL,
     1  MGRCDL(MPDVDL),MVARDL(MPDVDL,2),MODEDL(MPDVDL,2),
     1  MCOLDL(MPDVDL),MSYMDL(MPDVDL,0:1),
     1  MLABDL(MPDVDL),MDUMDL,
     1  WDSNDL(4,MPWDDL,MPDVDL),RAMPDL(4,MPRADL,MPDVDL,2)
C     COMMON /DLUTSC/MNUMDL,MVDEDL,MITCDL,MTPCDL,MECADL,MHCADL,MMDEDL,
C    1  MGRCDL(MPDVDL),
C    1  MCOLDL(MPDVDL),
C    1  MLABDL(MPDVDL),MDUMDL
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      MODEDT=1
      CALL DCIRCL
      CALL DGLEVL(MCOLDL(MTPCDL))
      CALL DFZ1TR
      END
*DK DFZETA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFZETA
CH
      SUBROUTINE DFZETA(FIMID,SK,SP,TEMID)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DR
      COMMON /DRNG1C/
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      LOGICAL
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      COMMON /DRNG2C/DET1DR(6),DET2DR(6)
C------------------------------------------------------------------- DO
      COMMON /DOPR4C/ NOARDO,
     1  NTF1DO,NSP1DO,IRZ1DO,IHTRDO(8)
      COMMON /DOPR5C/ FGETDO
      LOGICAL FGETDO
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DT
      PARAMETER (MTRKDT=401,MMCTDT=500)
      COMMON /DTRK1C/ FCUTDT,FNOTDT(   0   :MTRKDT),
     &                FCUHDT,FNOHDT(-MTRKDT:MTRKDT)
      LOGICAL FCUTDT,FNOTDT,FCUHDT,FNOHDT
      COMMON /DTRK2C/ VTX1DT(3),VTX2DT(3),PTRADT(3),CLTMDT(3),FVTXDT
      LOGICAL FVTXDT
      COMMON /DTRK6C/ FRFTDT(6),XYZVDT(3)
      COMMON /DTRK7C/ N3DIDT,FZTRDT,NSVXDT,KINVDT(2,MTRKDT)
      LOGICAL FZTRDT
      COMMON /DTRK3C/ MODEDT,TPHEDT(5)
      COMMON /DTRK4C/ FMCCDT,FMNTDT(MMCTDT),FMVXDT(100)
      LOGICAL FMCCDT,FMNTDT,FMVXDT
      COMMON /DTRK5C/ SZVXDT,NCVXDT
      COMMON /DTRK1T/ TGFTDT,TGTLDT,TGCLDT
      CHARACTER *4 TGFTDT,TGTLDT,TGCLDT
C PARAMETER (MTRKDT=100,MMCTDT=500) is found in:
C C_DALBC.FOR,C_DALBD.FOR,C_DALBH.FOR,C_DALBM.FOR,C_DALBR.FOR,C_DALBT.FOR
C C_DALBV.FOR,C_DALBX.FOR,C_DALBY.FOR,C_DALIA.FOR,C_DALID.FOR,C_DALIR.FOR
C C_DALIS.FOR,C_DALIV.FOR,N_DALBA.FOR,N_DALBF.FOR,US3.FOR
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DC
      PARAMETER (MCTRDC=1000)
      COMMON /DCOTR/ FDTRDC,FVTRDC,NCTRDC(0:MCTRDC)
      LOGICAL FDTRDC,FVTRDC
C------------------------------------------------------------------- DP
      COMMON /DPCOLR/ ICOLDP
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C------------------------------------------------------------------- DK
      COMMON /DKNSTC/ PIFCDK,RMAXDK,ZMAXDK,TCO1DK,TCO2DK,
     1   ROTODK(-1:10),ZZTODK(-1:10),ZMTODK(-1:10),DMAXDK,RMINDK,
     1   RLOWDK
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      EQUIVALENCE (KPIKDP,N)
      DATA W2/2./
      LOGICAL FOUT,DVM0
      LOGICAL DCCVX
      EXTERNAL DVTR0
      IF(.NOT.FTPCDR) RETURN
C     CALL DLHITR('TR',MTPCDL,ICOL)
      CALL DAPTRN('FZ',0)
      IF(IHTRDO(2).LE.2) THEN
         CALL DSCTR
         CALL DV0(FRFTDB,NUM1,NUM2,FOUT)
         IF(FOUT) RETURN
         MODEDT=1
         IF(.NOT.FPIKDP.AND.PDCODD(4,NFTRDD).GT.0.) THEN
           CALL DQLEVL(NFTRDD)
           DLINDD=DLINDD+W2
           DO 301 N=NUM1,NUM2
             IF(FNOTDT(N)) GO TO 301
             CALL DVTRV(N)
             CALL DVXT(N,XYZVDT)
             CALL DFZ1TR
  301      CONTINUE
           DLINDD=DLINDD-W2
         END IF
         DO 1 N=NUM1,NUM2
           IF(FNOTDT(N)) GO TO 1
           IF(FVTRDC) CALL DGLEVL(NCTRDC(N))
           CALL DVTRV(N)
           CALL DFZ1TR
           CALL DAPTRN('FZ',N)
    1    CONTINUE
      ELSE
         MODEDT=2
         CALL DMCNVX(NVTX1)
         IF(NVTX1.LE.0) RETURN
         DO 12 NV=1,NVTX1
            CALL DMCVX(NV,KT1,KT2)
            IF(KT2.LE.0) GO TO 12
            DO 11 N=KT1,KT2
               IF(DVM0(N)) GO TO 11
               CALL DMCTR(N,FOUT)
               IF(FOUT) GO TO 11
               CALL DFZ1TR
   11       CONTINUE
   12    CONTINUE
         IF(.NOT.FPIKDP) THEN
            IF(NVTX1.LE.0) RETURN
            CALL DPAR_SET_CO(49,'CMC')
            IF(ICOLDP.LT.0) RETURN
            CALL DPARGV(67,'SSY',2,SYMB)
            CALL DPARGV(67,'SSZ',2,SYSZ)
            MSYMB=SYMB
            CALL DQPD0(MSYMB,SZVXDT*0.1*SYSZ,0.)
            CALL DQPD0(3,DS,0.)
C           CALL DGLEVL(NCVXDT)
            DO 13 N=1,NVTX1
               IF(FMVXDT(N)) THEN
                  CALL DVMCVX(N,KDUM1,KDUM2,KDUM3,VTX1DT)
                  IF(DCCVX(VTX1DT)) GO TO 13
                  RO=DVCORD(VTX1DT,IVRODV)
                  IF(RO.LT.RMINDK.OR.RO.GT.RMAXDK) GO TO 13
                  TE=DVCORD(VTX1DT,IVTEDV)
                  FI=DFINXT(FIMID,DVCORD(VTX1DT,IVFIDV))
                  H=DVCORD(VTX1DT,IVZZDV)
C                  IF(SP.NE.0.) THEN
C                     Q=(1.+SP*(TE-TEMID))
C                     H=H*Q
C                  END IF
                  CALL DQPD(H,V)
               END IF
   13       CONTINUE
         END IF
      END IF
      END
*DK DFZRCB
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFZRCB
CH
      SUBROUTINE DFZRCB(HRB,VRB)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION HRB(4),VRB(4)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PDF,J_PTE,J_PFR,J_PTO'
      CALL DPARAM(11
     &  ,J_PFI,J_PDF,J_PTE,J_PFR,J_PTO)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PARADA(2,J_PTE).LE.90.) THEN
        HRB(1)=PARADA(2,J_PFR)
        HRB(2)=PARADA(2,J_PTO)
      ELSE
        HRB(1)=-PARADA(2,J_PTO)
        HRB(2)=-PARADA(2,J_PFR)

C        IF(PARADA(2,J_PBM).EQ.2.) THEN
C          HRB(1)=-PARADA(2,J_PFR)
C          HRB(2)=-PARADA(2,J_PTO)
C        ELSE
C          HRB(1)=-PARADA(2,J_PTO)
C          HRB(2)=-PARADA(2,J_PFR)
C        END IF

      END IF


      HRB(3)=HRB(2)
      HRB(4)=HRB(1)
      DF=0.5*PARADA(2,J_PDF)
      VRB(1)=PARADA(2,J_PFI)-DF
      VRB(2)=VRB(1)
      VRB(3)=PARADA(2,J_PFI)+DF
      VRB(4)=VRB(3)
      END
*DK DFZPH
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFZPH
CH
      SUBROUTINE DFZPH(FIMID,SK,SP,TEMID)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DR
      COMMON /DRNG1C/
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      LOGICAL
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      COMMON /DRNG2C/DET1DR(6),DET2DR(6)
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C------------------------------------------------------------------- DC
      PARAMETER (MCHCDC=2000)
      COMMON /DCOHC/ FDHCDC,FVHCDC,NCHCDC(MCHCDC)
      COMMON /DCOHC1/ LCHCDC
      LOGICAL FDHCDC,FVHCDC
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION HL(2),VL(2)
      EQUIVALENCE (KPIKDP,K)
      EXTERNAL DVHC
      DATA SHRNK /0.8/
      LOGICAL FOUT
      IF(.NOT.FHCADR) RETURN
      CALL DSCHC
      CALL DV0(HSDADB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
      DO 1 K=NUM1,NUM2
         CALL DCUTHC(K,FOUT)
         IF(FOUT) GO TO 1
         TE=DVHC(IVTEDV,K)
         IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 1
         FI=DFINXT(FIMID,DVHC(IVFIDV,K))
         IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 1
         IF(FVHCDC) CALL DGLEVL(NCHCDC(K))
         NN=DVHC(IVNNDV,K)
         IF(NN.EQ.2) GO TO 1
         H=DVHC(IVZZDV,K)
         V=FI
C         IF(SK.EQ.0.) THEN
C            V=FI
C         ELSE
C            V=FI+SK*(TE-TEMID)
C         END IF
C         IF(SP.NE.0.) THEN
C            Q=(1.+SP*(TE-TEMID))
C            H=H*Q
C            V=V*Q
C         END IF
         IF(FPIKDP) THEN
            CALL DQPIF(H,V)
         ELSE
            D=DVHC(IVDFDV,K)*SHRNK
            HL(1)=H
            HL(2)=H
            VL(1)=V-D
            VL(2)=V+D
            CALL DQLIE(HL,VL)
            IF(IZOMDO.EQ.0)
     &        CALL DQL2E(HL(1),VL(1)+360.,HL(2),VL(2)+360.)
         END IF
    1 CONTINUE
      END
*DK DFZPIT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFZPIT
CH
      SUBROUTINE DFZPIT(F1,F2)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      PARAMETER (MXYZDP=19)
      COMMON /DPICK2/ NXYZDP,XYZPDP(3,19)
      COMMON /DPICK3/ NTRKDP,LTRKDP(8)
      COMMON /DPICK4/ NUMPDP,LORPDP
      PARAMETER (MTRSDP=26)
      COMMON /DPICK5/ TRSTDP(MTRSDP)
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
C      DATA FP/20./,KP/20/
C      DF=(F2-F1)/FP
C      F=F1
C      DO 700 K=1,KP
C        F=F+DF
      DF=F2-F1
      DO 700 K=1,MTRSDP
        F=F1+TRSTDP(K)*DF
        H1=DHELIX(F,IVZZDV)
        V1=DHELIX(F,IVFIDV)
        CALL DQPIK(H1,V1)
        CALL DQPIK(H1,V1+360.)
  700 CONTINUE
      END
*DK DFZTT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFZTT
CH
      SUBROUTINE DFZTT(FIMID,SK,SP,TEMID)
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DR
      COMMON /DRNG1C/
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      LOGICAL
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      COMMON /DRNG2C/DET1DR(6),DET2DR(6)
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DT
      PARAMETER (MTRKDT=401,MMCTDT=500)
      COMMON /DTRK1C/ FCUTDT,FNOTDT(   0   :MTRKDT),
     &                FCUHDT,FNOHDT(-MTRKDT:MTRKDT)
      LOGICAL FCUTDT,FNOTDT,FCUHDT,FNOHDT
      COMMON /DTRK2C/ VTX1DT(3),VTX2DT(3),PTRADT(3),CLTMDT(3),FVTXDT
      LOGICAL FVTXDT
      COMMON /DTRK6C/ FRFTDT(6),XYZVDT(3)
      COMMON /DTRK7C/ N3DIDT,FZTRDT,NSVXDT,KINVDT(2,MTRKDT)
      LOGICAL FZTRDT
      COMMON /DTRK3C/ MODEDT,TPHEDT(5)
      COMMON /DTRK4C/ FMCCDT,FMNTDT(MMCTDT),FMVXDT(100)
      LOGICAL FMCCDT,FMNTDT,FMVXDT
      COMMON /DTRK5C/ SZVXDT,NCVXDT
      COMMON /DTRK1T/ TGFTDT,TGTLDT,TGCLDT
      CHARACTER *4 TGFTDT,TGTLDT,TGCLDT
C PARAMETER (MTRKDT=100,MMCTDT=500) is found in:
C C_DALBC.FOR,C_DALBD.FOR,C_DALBH.FOR,C_DALBM.FOR,C_DALBR.FOR,C_DALBT.FOR
C C_DALBV.FOR,C_DALBX.FOR,C_DALBY.FOR,C_DALIA.FOR,C_DALID.FOR,C_DALIR.FOR
C C_DALIS.FOR,C_DALIV.FOR,N_DALBA.FOR,N_DALBF.FOR,US3.FOR
C------------------------------------------------------------------- DC
      PARAMETER (MCTRDC=1000)
      COMMON /DCOTR/ FDTRDC,FVTRDC,NCTRDC(0:MCTRDC)
      LOGICAL FDTRDC,FVTRDC
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION H(2),V(2)
      LOGICAL FOUT
      IF(.NOT.FTPCDR) RETURN
      CALL DSCTR
      CALL DV0(FRTLDB,NUM1,NTRK,FOUT)
      IF(FOUT) RETURN
      CALL DV0(TPCODB,NUM0,NUM2,FOUT)
      IF(FOUT) RETURN
C     CALL DVTQ0(NUM2)
C     IF(NUM2.EQ.0) RETURN
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
C      CALL DLHITR('TR',MTPCDL,ICOL)
      DO 3 M=1,NTRK
         IF(FCUTDT) THEN
            IF(FNOTDT(M)) GO TO 3
         END IF
         CALL=DVCHT(M,NUM)
         IF(NUM.LE.0) GO TO 3
         K1=IDVCHT(1)
C         CALL DLCOLT(M,NCOL)
C         IF(NCOL.EQ.0) GO TO 3
C         IF(ICOL.NE.0) CALL DGLEVL(ICOL)
         IF(FVTRDC) CALL DGLEVL(NCTRDC(M))
         I=0
         FIMTR=DFINXT(FIMID,DVTP(IVFIDV,K1))
    2    DO 1 KIND=1,NUM
            K=IDVCHT(KIND)
            IF(FPIMDP.AND.K.NE.NPIKDP) GO TO 1
            TE=DVTP(IVTEDV,K)
            IF(TE.LT.TC1.OR.TE.GT.TC2) THEN
               I=0
               GO TO 1
            END IF
            FI=DFINXT(FIMTR,DVTP(IVFIDV,K))
            IF(FI.LT.FC1.OR.FI.GT.FC2) THEN
               I=0
               GO TO 1
            END IF
            I=I+1
            H(I)=DVTP(IVZZDV,K)
            V(I)=FI
C            IF(SK.EQ.0.) THEN
C               V(I)=FI
C            ELSE
C               V(I)=FI+SK*(DVTP(IVTEDV,K)-TEMID)
C            END IF
C            IF(SP.NE.0.) THEN
C               Q=(1.+SP*(DVTP(IVTEDV,K)-TEMID))
C               H(I)=H(I)*Q
C            END IF
            IF(FPIKDP) THEN
               CALL DQPIK(H,V)
               I=0
            ELSE IF(I.EQ.2) THEN
               CALL DQLIE(H,V)
               IF(IZOMDO.EQ.0) THEN
                  CALL DQL2E(H(1),V(1)+360.,H(2),V(2)+360.)
                  CALL DQL2E(H(1),V(1)-360.,H(2),V(2)-360.)
               END IF
               I=1
               H(1)=H(2)
               V(1)=V(2)
            END IF
    1    CONTINUE
    3 CONTINUE
      END
*DK DFZHSH
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFZHSH
CH
      SUBROUTINE DFZHSH
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
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DV
      COMMON /DVDGEO/ NSLIDV,NSLODV,NWAFDV,PALWDV,ZALWDV,
     &                TIILDV(15),TIOLDV(15),
     &                RMNIDV,RMXIDV,RMNODV,RMXODV,
     &                DELXDV,DELYDV,DELZDV,DELPDV,
     &                ZOFWDV(6), NWTSDV(15,2),
     &                RAILDV,RAOLDV,DFILDV(12),DFOLDV(15),
     &                CRILDV(2,2,15),CROLDV(2,2,15),
     &                NFRSDV(2),NZRSDV(2),
     &                PFRSDV(2),PZRSDV(2)
C     HARD WIRED IN P_DALB_ALEPH.FOR
      COMMON /DVDGE1/ FEVDDV
      LOGICAL FEVDDV
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DV
      COMMON /DVDDFZ/ DLSTDV,NTOTDV(2),
     &  FIMDDV(-2:18,2),DLFIDV(-2:18,2),
     &  ZLFTDV(6,2),ZRGTDV(6,2)
      COMMON /DVDLTF/ FISCDV
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DC
      PARAMETER (MONUDC=10)
      COMMON /DCOLM/ MOTRDC,MOSADC,MOVDDC,MOITDC,MOTPDC,
     &  MOLCDC,MOECDC,MOEODC,MOHCDC,MOHTDC
C------------------------------------------------------------------- DC
      PARAMETER (MCTRDC=1000)
      COMMON /DCOTR/ FDTRDC,FVTRDC,NCTRDC(0:MCTRDC)
      LOGICAL FDTRDC,FVTRDC
C------------------------------------------------------------------- DQ
      COMMON /DQQQ1C/ AHSCDQ,BHSCDQ,CHSCDQ,AVSCDQ,BVSCDQ,CVSCDQ
      COMMON /DQQQ2C/ DMAXDQ
      COMMON /DQQQ3C/ FPRSDQ,FPSQDQ
      LOGICAL FPRSDQ,FPSQDQ
      COMMON /DQQQ4C/ PRSHDQ,PRSVDQ,PRS1DQ,PRS2DQ,PRV1DQ
      COMMON /DQQQ5C/ HUSRDQ(4,0:12),VUSRDQ(4,0:12)
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C------------------------------------------------------------------- DT
      PARAMETER (MTRKDT=401,MMCTDT=500)
      COMMON /DTRK1C/ FCUTDT,FNOTDT(   0   :MTRKDT),
     &                FCUHDT,FNOHDT(-MTRKDT:MTRKDT)
      LOGICAL FCUTDT,FNOTDT,FCUHDT,FNOHDT
      COMMON /DTRK2C/ VTX1DT(3),VTX2DT(3),PTRADT(3),CLTMDT(3),FVTXDT
      LOGICAL FVTXDT
      COMMON /DTRK6C/ FRFTDT(6),XYZVDT(3)
      COMMON /DTRK7C/ N3DIDT,FZTRDT,NSVXDT,KINVDT(2,MTRKDT)
      LOGICAL FZTRDT
      COMMON /DTRK3C/ MODEDT,TPHEDT(5)
      COMMON /DTRK4C/ FMCCDT,FMNTDT(MMCTDT),FMVXDT(100)
      LOGICAL FMCCDT,FMNTDT,FMVXDT
      COMMON /DTRK5C/ SZVXDT,NCVXDT
      COMMON /DTRK1T/ TGFTDT,TGTLDT,TGCLDT
      CHARACTER *4 TGFTDT,TGTLDT,TGCLDT
C PARAMETER (MTRKDT=100,MMCTDT=500) is found in:
C C_DALBC.FOR,C_DALBD.FOR,C_DALBH.FOR,C_DALBM.FOR,C_DALBR.FOR,C_DALBT.FOR
C C_DALBV.FOR,C_DALBX.FOR,C_DALBY.FOR,C_DALIA.FOR,C_DALID.FOR,C_DALIR.FOR
C C_DALIS.FOR,C_DALIV.FOR,N_DALBA.FOR,N_DALBF.FOR,US3.FOR
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      COMMON/MVDPULSE/nclu(2),puls(1021,2)
C
      INTEGER VROSTM
C      PARAMETER (NWAF=4, NMOD=15,  NVIEW=2,  NLAYER=2)
C      REAL PHIPOS(NMOD,NLAYER)
C      REAL ZPOS  (NWAF,NMOD,NLAYER)
C      INTEGER NPhi_Strips(NMOD,NLAYER)
C      INTEGER NZ_Strips  (NWAF,NMOD,NLAYER)
C      COMMON/MVDGEOM/PHIPOS,ZPOS,NPhi_Strips,NZ_Strips
      DIMENSION FACE(2,2,15,2)
      EQUIVALENCE (FACE,CRILDV)
      DATA JFP/2/,JFZ/1/,DESET/5./,PESET/5./
      DIMENSION H(5),V(5)
      DATA NCA/1/,NCL/8/,QE/0.1/
      DIMENSION ICOL(0:7)
      DATA ICOL/9,11,8,13,15,10,12,14/,M8/8/
      DIMENSION MFRW(4)
      DATA MFRW/-1,-1,0,0/
      CHARACTER *4 TST
      DATA TST/'AR+L'/
      LOGICAL FOUT,FNBK,FCOL,FCOLT
      FOUT=.TRUE.
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_FLA,J_FFA,J_FWA,J_FSC,J_FHS,J_FHW'
      CALL DPARAM(16
     &  ,J_FLA,J_FFA,J_FWA,J_FSC,J_FHS,J_FHW)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(FPIKDP) RETURN
      FOUT=.FALSE.
      CALL DPAR_SET_CO(50,'CPH')
      CALL DPARGI(50,'CPH',NCA)
      CALL DPARGI(50,'CPF',NCL)
      LAYER=PARADA(2,J_FLA)
      NFACE=PARADA(2,J_FFA)
      NWAFR=PARADA(2,J_FWA)
      IF(PARADA(4,J_FSC).GT.0.) THEN
        CALL DVVDPH(NWAFDV,LAYER,NFACE,NWAFR,IER)
        CALL DQPO0( TST,NCA,NCL,'NSKP')
C       ....................................... HORIZONTAL HISTOGRAMS (P.H./Z)
C        IZ=1
C        IF(NZSID.LT.0) THEN
C          KW1=1
C          KW2=NWAFDV/2
C        ELSE
C          KW1=NWAFDV/2+1
C          KW2=NWAFDV
C        END IF
        ISTAT = VROSTM(1,NSTRPS,RPITCH,IRFREQ)
        V(1)=0.
        V(2)=V(1)
        V(5)=V(1)
        Z=ZLFTDV(NWAFR,LAYER)
        DO K=1,NSTRPS
          P=PULS(K,JFZ)
          IF(P.GT.0.) THEN
            P=P*QE
            H(1)=Z
            H(3)=Z+PZRSDV(LAYER)
            H(2)=H(3)
            H(4)=H(1)
            H(5)=H(1)
            V(3)=P
            V(4)=V(3)
            CALL DQPOL(5,H,V)
          END IF
          Z=Z+PZRSDV(LAYER)
        END DO
      END IF
      IF(PARADA(4,J_FHS).GT.0.) THEN
        QH=QE/PARADA(2,J_FHS)
C       ............................................................ DRAW HITS
        CALL DV0(VDXYDB,NUM1,NUM2,FNBK)
        IF(FNBK) RETURN
        IF(PDCODD(4,ICCNDD).EQ.1.) THEN
C         ....................................................... all constant
          CALL DQLEVL(ICCNDD)
        ELSE IF(MOVDDC.EQ.0) THEN
          CALL DQLEVH(LCVDDD)
        ELSE IF(MOVDDC.EQ.1) THEN
          FCOL=.TRUE.
        ELSE
          CALL DSCTR0(FCOLT,NCOL)
          IF(FCOLT) THEN
            FCOL=.TRUE.
            NCTRDC(0)=PDCODD(2,LCVDDD)
          END IF
        END IF
        DE=DESET/AHSCDQ
        PE=PESET/BVSCDQ
        DO 300 K=NUM1,NUM2
          IL=DVVDRZ(IVKKDV,K,1)
          IF(IL.NE.LAYER) GO TO 300
          NF=DVVDRZ(IVNNDV,K,1)
          IF(NF.NE.NFACE) GO TO 300
          NW=DVVDRZ(IVJJDV,K,1)
          IF(NW.NE.NWAFR) GO TO 300
          NTRVD=ABS(DVVDRZ(IVNTDV,K,1))
          IF(FCUHDT.AND.FNOHDT(NTRVD)) GO TO 300
          IF(FCOL) THEN
            IF(MOVDDC.EQ.1) THEN
              MODUL=2*NF+MFRW(NW)
              NCOL=MOD(MODUL,M8)
              CALL DGLEVL(ICOL(NCOL))
            ELSE
              CALL DGLEVL(NCTRDC(NTRVD))
            END IF
          END IF
          UU=DVVDRZ(10,K,1)
          ZZ=ZOFWDV(NW)+UU
          PH=DVVDRZ(IVENDV,K,1)*QH
          CALL DQL2EP(ZZ,0.,ZZ,PH)
          CALL DQL2E(ZZ-DE,PH-PE,ZZ+DE,PH+PE)
  300   CONTINUE
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DFZHSV
CH
      ENTRY DFZHSV
CH
CH --------------------------------------------------------------------
CH
      IF(FOUT) RETURN
C     ....................................... VERTICAL HISTOGRAM (P.H./FI)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_FSC,J_FHS'
      CALL DPARAM(10
     &  ,J_PFI,J_FSC,J_FHS)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DPAR_SET_CO(50,'CPH')
      FIM=DFINXT(PARADA(2,J_PFI),FIMDDV(NFACE,LAYER))
      IF(PARADA(4,J_FSC).GT.0.) THEN
        CALL DQPO0( TST,NCA,NCL,'NSKP')
        FI=FIM-FISCDV*DLFIDV(NFACE,LAYER)
        D=PFRSDV(LAYER)*FISCDV
        H(1)=0.
        H(4)=H(1)
        H(5)=H(1)
        ISTAT = VROSTM(2,NSTRPS,RPITCH,IRFREQ)
        DO K=1,NSTRPS
          P=PULS(K,JFP)*QE
          IF(P.GT.0.) THEN
            H(3)=P
            H(2)=H(3)
            V(1)=FI
            V(3)=FI+D
            V(2)=V(1)
            V(4)=V(3)
            V(5)=V(1)
            CALL DQPOL(5,H,V)
          END IF
          FI=FI+D
        END DO
      END IF
      IF(PARADA(4,J_FHS).GT.0.) THEN
C        ..................................................... DRAW PHI HITS
        CALL DV0(VDXYDB,NUM1,NUM2,FNBK)
        IF(FNBK) RETURN
        DE=DESET/BVSCDQ
        PE=PESET/AHSCDQ
        DO 301 K=NUM1,NUM2
          IL=DVVDXY(IVKKDV,K)
          IF(IL.NE.LAYER) GO TO 301
          NF=DVVDXY(IVNNDV,K)
          IF(NF.NE.NFACE) GO TO 301
          NW=DVVDXY(IVJJDV,K)
          IF(NW.NE.NWAFR) GO TO 301
          NTRVD=ABS(DVVDXY(IVNTDV,K))
          IF(FCUHDT.AND.FNOHDT(NTRVD)) GO TO 301
          IF(FCOL) THEN
            IF(MOVDDC.EQ.1) THEN
              MODUL=2*NF+MFRW(NW)
              NCOL=MOD(MODUL,M8)
              CALL DGLEVL(ICOL(NCOL))
            ELSE
              CALL DGLEVL(NCTRDC(NTRVD))
            END IF
          END IF
          UU=DVVDXY(10,K)
          VV=FIM+UU*FISCDV
          PH=DVVDXY(IVENDV,K)*QH
          CALL DQL2EP(0.,VV,PH,VV)
          CALL DQL2E(PH-PE,VV-DE,PH+PE,VV+DE)
  301   CONTINUE
      END IF
      END
*DK DFZVEL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DFZVEL
CH
      SUBROUTINE DFZVEL
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DV
      COMMON /DVDGEO/ NSLIDV,NSLODV,NWAFDV,PALWDV,ZALWDV,
     &                TIILDV(15),TIOLDV(15),
     &                RMNIDV,RMXIDV,RMNODV,RMXODV,
     &                DELXDV,DELYDV,DELZDV,DELPDV,
     &                ZOFWDV(6), NWTSDV(15,2),
     &                RAILDV,RAOLDV,DFILDV(12),DFOLDV(15),
     &                CRILDV(2,2,15),CROLDV(2,2,15),
     &                NFRSDV(2),NZRSDV(2),
     &                PFRSDV(2),PZRSDV(2)
C     HARD WIRED IN P_DALB_ALEPH.FOR
      COMMON /DVDGE1/ FEVDDV
      LOGICAL FEVDDV
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DT
      PARAMETER (MTRKDT=401,MMCTDT=500)
      COMMON /DTRK1C/ FCUTDT,FNOTDT(   0   :MTRKDT),
     &                FCUHDT,FNOHDT(-MTRKDT:MTRKDT)
      LOGICAL FCUTDT,FNOTDT,FCUHDT,FNOHDT
      COMMON /DTRK2C/ VTX1DT(3),VTX2DT(3),PTRADT(3),CLTMDT(3),FVTXDT
      LOGICAL FVTXDT
      COMMON /DTRK6C/ FRFTDT(6),XYZVDT(3)
      COMMON /DTRK7C/ N3DIDT,FZTRDT,NSVXDT,KINVDT(2,MTRKDT)
      LOGICAL FZTRDT
      COMMON /DTRK3C/ MODEDT,TPHEDT(5)
      COMMON /DTRK4C/ FMCCDT,FMNTDT(MMCTDT),FMVXDT(100)
      LOGICAL FMCCDT,FMNTDT,FMVXDT
      COMMON /DTRK5C/ SZVXDT,NCVXDT
      COMMON /DTRK1T/ TGFTDT,TGTLDT,TGCLDT
      CHARACTER *4 TGFTDT,TGTLDT,TGCLDT
C PARAMETER (MTRKDT=100,MMCTDT=500) is found in:
C C_DALBC.FOR,C_DALBD.FOR,C_DALBH.FOR,C_DALBM.FOR,C_DALBR.FOR,C_DALBT.FOR
C C_DALBV.FOR,C_DALBX.FOR,C_DALBY.FOR,C_DALIA.FOR,C_DALID.FOR,C_DALIR.FOR
C C_DALIS.FOR,C_DALIV.FOR,N_DALBA.FOR,N_DALBF.FOR,US3.FOR
C------------------------------------------------------------------- DC
      PARAMETER (MCTRDC=1000)
      COMMON /DCOTR/ FDTRDC,FVTRDC,NCTRDC(0:MCTRDC)
      LOGICAL FDTRDC,FVTRDC
C------------------------------------------------------------------- DV
      COMMON /DVDDFZ/ DLSTDV,NTOTDV(2),
     &  FIMDDV(-2:18,2),DLFIDV(-2:18,2),
     &  ZLFTDV(6,2),ZRGTDV(6,2)
      COMMON /DVDLTF/ FISCDV
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77

C--------------------------------------------------------------------

      INTEGER MAX_TRACKS,ZP,XYZ,XYC
      PARAMETER (MAX_TRACKS=200, ZP=2, XYZ=3, XYC=3)

      COMMON/VD_TRACKS/  NTRACKS, TRK_FLAG (MAX_TRACKS),
     $                   TRK_NUMBER(MAX_TRACKS),
     $                   TRK_LOCAL (ZP,MAX_TRACKS),
     $                   TRK_GLOBAL(XYZ,MAX_TRACKS),
     $                   TRK_ELLIPS(XYC,MAX_TRACKS),
     $                   TRK_ADDRESS(MAX_TRACKS),
     $                   TRK_MOMENTUM(MAX_TRACKS)

      INTEGER     NTRACKS, TRK_FLAG, TRK_NUMBER
      REAL        TRK_LOCAL, TRK_GLOBAL, TRK_ELLIPS
      INTEGER     TRK_ADDRESS
      REAL        TRK_MOMENTUM

C
      INTEGER ISTAT
C--------------------------------------------------------------------
C
      DIMENSION WA(2),MA(2),LCOL(2),DLIN(2)
      DATA MA/8,8/
      DATA NPTS/61/
      DIMENSION XE(61),YE(61),XC(2,2),YC(2,2),HE(2),VE(2)
      DATA FOVER/40./,ELTPC/100./
      LOGICAL FCOL(2),FEL
      DATA FCOL/2*.FALSE./
      DATA DHLIM/2.56/
      CALL DPARGI(81,'EFZ',NEL)
      IF(NEL.EQ.0) RETURN
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_FLA,J_FFA,J_FWA,J_FEL'
      CALL DPARAM(10
     &  ,J_PFI,J_FLA,J_FFA,J_FWA,J_FEL)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DPARGI(50,'CEF',LCOL(1))
      CALL DPARGI(60,'STE',MA(2))
      CALL DPARGV(60,'STS',2,WA(2))
      CALL DPARGV(60,'STF',2,DFRAM)
      CALL DPARGV(60,'SEF',2,DLFRM)

      LAYER=PARADA(2,J_FLA)
      NFACE=PARADA(2,J_FFA)
      IF(PARADA(4,J_FWA).GT.0.) THEN
        NWFR1=PARADA(2,J_FWA)
        IF(     NWFR1.EQ.7) THEN
          NWFR1=1
          NWFR2=NWAFDV/2
        ELSE IF(NWFR1.EQ.8) THEN
          NWFR1=NWAFDV/2+1
          NWFR2=NWAFDV
        ELSE
          NWFR2=NWFR1
        END IF
      END IF
      CALL DPARGV(60,'SEL',4,DHLI4)
      IF(DHLI4.LT.0.) THEN
        DHLIM=99999.
      ELSE
        CALL DPARGV(60,'SEL',2,DHLIM)
      END IF
      CALL VDET_DECODE_TRACKS(ISTAT)
      CALL DSCTR0(FCOL(2),LCOL(2))
      WA(1)=WA(2)+DFRAM
      CALL DPARGV(60,'SWE',2,DLIN(2))
      DLIN(1)=DLIN(2)+DLFRM
      IF(IZOMDO.EQ.0.OR.
     &  PARADA(4,J_FEL).LE.0.) THEN
        FEL=.FALSE.
      ELSE
        FEL=.TRUE.
      END IF
      IF(FPIKDP) THEN
        MDLRDP=FRFTDB
        I2=1
      ELSE
        I2=2
      END IF
C     CALL DQPD0(MA(1),WA(1),FOVER)
      DO I=1,I2
        DLINDD=DLIN(I)
        CALL DGLEVL(LCOL(I))
        CALL DQPD0(MA(I),WA(I),FOVER)
        DO 200 K=1,NTRACKS
          KPIKDP=TRK_NUMBER(K)
          IF(FNOTDT(KPIKDP)) GO TO 200
          IF(FPIMDP.AND.KPIKDP.NE.NPIKDP) GO TO 200
          KCOL=NCTRDC(KPIKDP)
          IF(FCOL(I)) CALL DGLEVL(KCOL)
          CALL VADEWA(TRK_ADDRESS(K),IL,MW,NF,NV)
          IF(IL.EQ.LAYER) THEN
            IF(PARADA(4,J_FWA).GT.0.) THEN
              IF(NF.NE.NFACE) GO TO 200
              IF(MW.LT.NWFR1.OR.
     &           MW.GT.NWFR2) GO TO 200
            END IF
            PLOC=TRK_LOCAL(1,K)
            IF(PARADA(4,J_FFA).LT.0..AND.ABS(PLOC).GT.DHLIM) GO TO 200
            IF(IZOMDO.EQ.0) THEN
              FIM=FIMDDV(NF,LAYER)
            ELSE
              FIM=DFINXT(PARADA(2,J_PFI),FIMDDV(NF,LAYER))
            END IF
            IF(FEL) THEN
              IF(TRK_ELLIPS(1,K).GT.ELTPC.OR.
     &           TRK_ELLIPS(2,K).GT.ELTPC) GO TO 7
              CALL VDET_ELLIPS(TRK_LOCAL(1,K),TRK_ELLIPS(1,K),
     &          PARADA(2,J_FEL),NPTS,XE,YE,XC,YC)
              H=ZOFWDV(MW)
              V=FIM
              HE(1)=H+XE(1)
              VE(1)=V+YE(1)*FISCDV
              DO N=2,61
                HE(2)=H+XE(N)
                VE(2)=V+YE(N)*FISCDV
                IF(FPIKDP) THEN
                  CALL DQPIK(HE,VE)
                ELSE
                  CALL DQLIE(HE,VE)
                END IF
                HE(1)=HE(2)
                VE(1)=VE(2)
              END DO
              GO TO 200
            END IF
    7       H=ZOFWDV(MW)+TRK_LOCAL(2,K)
            V=FIM+FISCDV*TRK_LOCAL(1,K)
            IF(FPIKDP) THEN
              CALL DQPIF(H,V)
            ELSE
              CALL DQPD(H,V)
              IF(IZOMDO.EQ.0.AND.V.LT.FOVER) CALL DQPD(H,V+360.)
            END IF
          END IF
  200   CONTINUE
      END DO
      END

*DK D_FORCED_DRAW
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ D_FORCED_DRAW
CH
      SUBROUTINE D_FORCED_DRAW(TA,NTVI,FRET,NEX)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      CHARACTER *(*) TA
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPERSC/ FRPRDP,FBUGDP,FFDRDP
      LOGICAL FRPRDP,FBUGDP,FFDRDP
C------------------------------------------------------------------- DU
      PARAMETER (MPNPDU=60)
      REAL DACUDU
      COMMON /DUSDAC/ NUNIDU,
     1 WAITDU,ECCHDU,BOXSDU,ECSQDU,HCSQDU,
     1 HIMXDU,RESCDU,SLOWDU,WISUDU,SETTDU,
     1 USLVDU,SYLRDU,SDATDU,RZDHDU,ROTADU,
     1 SCMMDU,CLMMDU,CUMMDU,WIMIDU,CCMIDU,
     1 ECMIDU,HCMIDU,SHTFDU(2)    ,FTDZDU,
     1 FRNLDU,DACUDU,SHECDU,SHHCDU,DRLIDU,
     1 SQLCDU,CHLCDU,HEADDU,VRDZDU,CHHCDU,
     1 AREADU,DFWIDU(0:1)  ,DFLGDU,PBUGDU,
     1 CHTFDU,RNCLDU,PR43DU,PR44DU,PR45DU,
     1 DSOFDU,TPOFDU,ROVDDU,ROECDU,ROHCDU,
     1 HSTRDU,REALDU,PR53DU,PR54DU,PR55DU,
     1 PR56DU(4),                  PR60DU
      DIMENSION PARADU(MPNPDU)
      EQUIVALENCE (PARADU,WAITDU)
      COMMON /DUSDA2/PLIMDU(2,MPNPDU)
      COMMON /DUSDAT/ TPR1DU(MPNPDU)
      CHARACTER *2 TPR1DU
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      LOGICAL FRET

      IF(TA.EQ.'FD'.AND.NTVI.EQ.1) THEN
C       ........................ DOFISH: set filling of complicated areas
        FFDRDP=.TRUE.
        DFLGDU=-ABS(DFLGDU)
        NEX=4
        FRET=.TRUE.
      ELSE
        FRET=.FALSE.
      END IF
      END
