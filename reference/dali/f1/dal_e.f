CH
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DEVNAM
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DEV
CH
      SUBROUTINE DEVNAM(T8)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C   Author    Christoph Grab
C              31-Aug-89
C
C   Purpose : Extract the actual filename (max. of 8 char), stripping
C             device, uic and extension.
C
C
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *8 T8
C
C LP : position of "."     L1 : Position of "]" or ":" or "/"
C
      DO K=LNINDE(1),1,-1
        IF(TFINDE(1)(K:K).EQ.'.') LP=K
        IF(TFINDE(1)(K:K).EQ.':'.OR.
     &     TFINDE(1)(K:K).EQ.']'.OR.
     &     TFINDE(1)(K:K).EQ.'/') THEN
          L1=K
          GO TO 200
        END IF
      END DO
C
      IF(LP.EQ.0) THEN
        T8='None    '
        RETURN
      END IF
C
      L1=0
  200 L1=L1+1
      LP=LP-1
      T8=TFINDE(1)(L1:MIN(L1+7,LP))
      RETURN
      END
*DK DECD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DECD
CH
      SUBROUTINE DECD
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
      LOGICAL FLEGO
      DATA QLEGO/.15/
      DIMENSION ZEC(3)
      DATA ZEC/259.,274.,290./,ZHC/378./
      CHARACTER *19 T
      CHARACTER *1 T1(3)
      DATA T1/'1','2','3'/
      CHARACTER *3 TENDC(-1:1)
      DATA TENDC/'B  ','A+B','A  '/
      CHARACTER *2 TH,TV
      DIMENSION SCA(4)
      DATA IDEB/0/
      DIMENSION HRB(4),VRB(4),HOD(4),VOD(4)
      CHARACTER *2 TX,TY
      PARAMETER (TX='X'//(''''))
      PARAMETER (TY='Y'//(''''))
C                                       clear window
      CALL DQCL(IAREDO)
      CALL DQWIL(MOD(DFWIDU(IZOMDO),10.))
C                                       setup of detector flags
      CALL DDRFLG
C                                       no detector selected from>to
      IF(FNOPDR) GO TO 99
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFR,J_PTO,J_PLY,J_PAB'
      CALL DPARAM(10
     &  ,J_PFR,J_PTO,J_PLY,J_PAB)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      RFR=PARADA(2,J_PFR)
      RTO=PARADA(2,J_PTO)
      IF(IZOMDO.EQ.0) THEN
        CALL DQRER(1,RTO,0.,0.,0.,HRB,VRB)
      ELSE
C       CALL DYXRCB(HRB,VRB,TH,TV,SCA)
        CALL DYX_CONE_TO_HV(HRB,VRB)
      END IF
C     .................................... calculate scales
      SCA(1)=HRB(1)
      SCA(2)=HRB(3)
      SCA(3)=VRB(1)
      SCA(4)=VRB(3)
      TH='X '
      TV='Y '
C                                       calculate 2-d linear transformation
      CALL DECSKW(HRB,VRB,FLEGO)
      VHGH=VHGHDG(IAREDO)
C     ........................................... change display area for lego
      IF(FLEGO.AND.IZOMDO.EQ.0) VHGHDG(IAREDO)=VHGHDG(IAREDO)-
     &  QLEGO*(VHGHDG(IAREDO)-VLOWDG(IAREDO))
      CALL DQRU(HRB,VRB)
      CALL DQSCA('H',SCA(1),SCA(2),'cm',2,TH,2)
      CALL DQSCA('V',SCA(3),SCA(4),'cm',2,TV,2)
      CALL DCTYP0(1)
      RPOSDT=RTO
      KK=PARADU(43)
      IF(KK.NE.0) CALL DWRT('colors (DSCEL ...) not up to date !!!###')
      GO TO (600,610,610,610,640,650,660,670,680), KK+1
  600 CALL DODLCA(' YX')
      GO TO 690
  610 FECADR=.TRUE.
      DO 611 K=1,4
        HOD(K)=HRB(K)*ZEC(KK)
        VOD(K)=VRB(K)*ZEC(KK)
  611 CONTINUE
      CALL DQRU(HOD,VOD)
      CALL DODECA(' YX','ENDCAP')
      CALL DQRU(HRB,VRB)
      IF(IZOMDO.EQ.1) CALL DECPE(1,1,3)
      CALL DECPE(2,KK,KK)
      CALL DQMID
      IF(IDEB.EQ.0) GO TO 99
      CALL DQRU(HOD,VOD)
      GO TO 600
  640 FTPCDR=.TRUE.
      FECADR=.TRUE.
      IF(PDCODD(2,ISTYDD).GT.1) THEN
        CALL DQLEVL(ICTPDD)
        CALL DGRAR(HLOWDG(IAREDO),VLOWDG(IAREDO),
     &             HHGHDG(IAREDO),VHGHDG(IAREDO))
      END IF
      DO 641 K=1,4
        HOD(K)=HRB(K)*ZEC(1)
        VOD(K)=VRB(K)*ZEC(1)
  641 CONTINUE
      CALL DQRU(HOD,VOD)
      STYLE=PDCODD(2,ISTYDD)
      PDCODD(2,ISTYDD)=0.
      CALL DODECA(' YX','ENDCAP')
      PDCODD(2,ISTYDD)=STYLE
      CALL DQRU(HRB,VRB)
      IF(IZOMDO.EQ.1) CALL DECPE(1,1,3)
      CALL DAPPT('EV',DVTP,TPCODB,0)
      CALL DAPPT('EC',DVTP,TPCODB,0)
      CALL DQMID
      GO TO 99
C 650 CALL DODSAT(' YX')
  650 GO TO 99
  660 DO 661 K=1,4
        HOD(K)=HRB(K)*ZHC
        VOD(K)=VRB(K)*ZHC
  661 CONTINUE
      CALL DQRU(HOD,VOD)
      CALL DODHCA(' YX','ENDCAP')
      CALL DQRU(HRB,VRB)
C// ?? IF(MSYMDL(MHCADL,2).EQ.6) THEN
C                                                   DIMENSION MSYMDL(*,0:1))
C        IF(IZOMDO.EQ.1) CALL DECPE(1,1,3)
C        CALL DECPH(1)
C      ELSE
      CALL DECPH(1)
C        IF(IZOMDO.EQ.1) CALL DECPE(1,1,3)
C      END IF
      CALL DQMID
      GO TO 99
  670 CALL DODMUD(' YX','ENDCAP',1)
      GO TO 99
  680 CALL DODMUD(' YX','ENDCAP',2)
      GO TO 99
  690 CALL DQMID
C                                       draw primary vertex
      IF(BNUMDB(2,PYERDB).NE.0.) CALL DQVRT(VRTXDV,VRTYDV)
C     .......................... Lego: change display area back (not scaling!)
      VHGHDG(IAREDO)=VHGH
      IF(FLEGO) CALL DQSET(IAREDO,0.,0.)
      CALL DQSC0('2 SC')
      CALL DECGL('EC')
      CALL DECPL('EC')
C                                       write text
C        123456789 123456789
C        EC L1-L3 endcap A+B
      T='EC L1-L3 endcap'
      L1=PARADA(2,J_PLY)
      L2=PARADA(4,J_PLY)
      IF(L1.EQ.L2) THEN
        T(4:6)=' '
      ELSE
        T(5:5)=T1(L1)
      END IF
      T(8:8)=T1(L2)
      NENDC=PARADA(2,J_PAB)
      T(17:19)=TENDC(NENDC)
      CALL DCTYEX(T,19)
C                                       finish = draw frame
   99 CALL DQFR(IAREDO)
C                                       store parameters (pstods)
      CALL DPCSAR
      RETURN
      END
*DK DECEN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DECEN
CH
      SUBROUTINE DECEN(HRB,VRB)
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
      DIMENSION HRB(*),VRB(*)
      LOGICAL FOUT
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
      CALL VZERO(ENECDE,7)
      CALL DV0(PLSDDB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE'
      CALL DPARAM(11
     &  ,J_PFI,J_PTE)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      FIMID=PARADA(2,J_PFI)
      TEMID=PARADA(2,J_PTE)
      H1=MIN(HRB(1),HRB(3))
      H2=MAX(HRB(1),HRB(3))
      V1=MIN(VRB(1),VRB(3))
      V2=MAX(VRB(1),VRB(3))
      DO 211 K=NUM1,NUM2,3
         TE=DVLC(IVTEDV,K)
         IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 211
         FI=DFINXT(FIMID,DVLC(IVFIDV,K))
         IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 211
         H=DVLC(IVXXDV,K)
         V=DVLC(IVYYDV,K)
         IF(H.LT.H1.OR.H.GT.H2.OR.V.LT.V1.OR.V.GT.V2) GO TO 211
         IF(DVLC(IVZZDV,K).GT.0.) THEN
           KK=1
         ELSE
           KK=4
         END IF
         DO 212 I=K,K+2
           CALL DCUTLC(I,FOUT)
           IF(.NOT.FOUT) THEN
             EN=DVLC(IVENDV,I)
             IF(EN.GT.0.) ENECDE(KK)=ENECDE(KK)+EN
           END IF
           KK=KK+1
  212    CONTINUE
  211 CONTINUE
      END
*DK DECGL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DECGL
CH
      SUBROUTINE DECGL(TPR)
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
      LOGICAL FOUT
      DATA DLXY/1.4875/
      CHARACTER *2 TPR
      IF(FPIKDP) RETURN
      IF(TPR.EQ.'FT'.AND.IZOMDO.EQ.0) RETURN
      CALL DV0(PLSDDB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_PAB'
      CALL DPARAM(10
     &  ,J_PFI,J_PTE,J_PAB)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(IZOMDO.EQ.0) THEN
        FIMID=180.
      ELSE
        FIMID=PARADA(2,J_PFI)
      END IF
      TEMID=PARADA(2,J_PTE)
      K1=0
      K2=2
      CALL DQRA0(1,LCLGDD)
      CALL DQPU0(6,SHTFDU)
C      CALL DQLEVL(LCLGDD)
      DLINDD=PDCODD(2,LIGLDD)
      IF(TPR.EQ.'EC') THEN
        DIR=PARADA(2,J_PAB)
      ELSE
        DIR=0.
      END IF
      DO 115 K=NUM1,NUM2,3
        IF(DIR*DVLC(IVZZDV,K).LT.0.) GO TO 115
        TE=DVLC(IVTEDV,K)
        IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 115
        FI=DFINXT(FIMID,DVLC(IVFIDV,K))
        IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 115
        CALL DCUTLC(K,FOUT)
        IF(FOUT) THEN
          CALL DCUTLC(K+1,FOUT)
          IF(FOUT) THEN
            CALL DCUTLC(K+2,FOUT)
            IF(FOUT) GO TO 115
          END IF
        END IF
        X=DVLC(IVXXDV,K)
        Y=DVLC(IVYYDV,K)
        IF(TPR.EQ.'FT') THEN
          Z=DVLC(IVZZDV,K)
          CALL DECTFB(X,Y,Z)
        ELSE
          CALL DQPU(X,Y,DLXY,DLXY)
        END IF
  115 CONTINUE
      DLINDD=PDCODD(2,LITRDD)
      END
*DK DECPE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DECPE
CH
      SUBROUTINE DECPE(MO,K1,K2)
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
      EQUIVALENCE (KPIKDP,K)
      DIMENSION HH(4),VV(4)
      LOGICAL FOUT,FCOL
      CALL DV0(ESDADB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI'
      CALL DPARAM(11
     &  ,J_PFI)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      FIMID=PARADA(2,J_PFI)
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
      IF(FPIKDP) THEN
        IF(MO.EQ.1) RETURN
        FCOL=.FALSE.
      ELSE
        IF(MO.EQ.1) THEN
          DLINDD=PDCODD(2,LIGLDD)
          CALL DQRA0(1,LCEGDD)
          FCOL=.FALSE.
        ELSE
          IF(K1.EQ.K2) THEN
C           Not yet corrected for new DSCEL!
C            CALL DSCEL(ESDADB,LCEKDD, FVECDC,FDECDC, MODE,LCOL,NCDUM)
            CALL DQRA1(2,LCOL)
            FCOL=FVECDC
          ELSE
            CALL DQRA0(1,LCECDD)
            FCOL=.FALSE.
          END IF
        END IF
C       ....................... COLOR IS SET CONSTANT, CORRECT LATER
        FCOL=.FALSE.
        CALL DPAR_SET_CO(53,'CCN')
      END IF
      DO 115 K=NUM1,NUM2
        JJ=DVEC(IVJJDV,K)
        IF(JJ.GE.51.AND.JJ.LE.178) GO TO 115
        KK=DVEC(IVKKDV,K)
        IF(KK.LT.K1.OR.KK.GT.K2) GO TO 115
        CALL DCUTEC(K,FOUT)
        IF(FOUT) GO TO 115
        TE=DVEC(IVTEDV,K)
        IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 115
        FFI=DVEC(IVFIDV,K)
        FI=DFINXT(FIMID,FFI)
        IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 115
        IF(FCOL) CALL DQRA1(2,NCECDC(K))
        IF(FPIKDP) THEN
          Z=ABS(DVEC(IVZZDV,K))
          H=DVEC(IVXXDV,K)/Z
          V=DVEC(IVYYDV,K)/Z
          CALL DQPIK(H,V)
        ELSE
          DF=DVEC(IVDFDV,K)
          DT=DVEC(IVDTDV,K)
          TT1=ABS(TAND(TE-DT))
          TT2=ABS(TAND(TE+DT))
          CF1=COSD(FI-DF)
          CF2=COSD(FI+DF)
          SF1=SIND(FI-DF)
          SF2=SIND(FI+DF)
          HH(1)=CF2*TT1
          HH(2)=CF1*TT1
          HH(3)=CF1*TT2
          HH(4)=CF2*TT2
          VV(1)=SF2*TT1
          VV(2)=SF1*TT1
          VV(3)=SF1*TT2
          VV(4)=SF2*TT2
          CALL DQRAR(HH,VV)
        END IF
  115 CONTINUE
      DLINDD=PDCODD(2,LITRDD)
      END
*DK DECPH
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DECPH
CH
      SUBROUTINE DECPH
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
      EQUIVALENCE (KPIKDP,K)
      DIMENSION HH(4),VV(4)
      LOGICAL FOUT,FCOL
      CALL DV0(HSDADB,NUM1,NUM2,FOUT)
      IF(FOUT) RETURN
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI'
      CALL DPARAM(11
     &  ,J_PFI)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      FIMID=PARADA(2,J_PFI)
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
      IF(FPIKDP) THEN
        FCOL=.FALSE.
      ELSE
C// ??IF(MSYMDL(MHCADL,0).EQ.6) THEN
C          MO=1
C        ELSE
C          MO=2
C        END IF
        MO=2
        CALL DSCHC
        CALL DQRA1(MO,LCHCDC)
        FCOL=FVHCDC
      END IF
      DO 115 K=NUM1,NUM2
        JJ=DVEC(IVJJDV,K)
        IF(JJ.GE.51.AND.JJ.LE.178) GO TO 115
        NN=DVHC(IVNNDV,K)
        IF(NN.EQ.2) GO TO 115
        CALL DCUTHC(K,FOUT)
        IF(FOUT) GO TO 115
        TE=DVHC(IVTEDV,K)
        IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 115
        FFI=DVHC(IVFIDV,K)
        FI=DFINXT(FIMID,FFI)
        IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 115
        IF(FCOL) CALL DQRA1(MO,NCHCDC(K))
        IF(FPIKDP) THEN
          Z=ABS(DVHC(IVZZDV,K))
          H=DVHC(IVXXDV,K)/Z
          V=DVHC(IVYYDV,K)/Z
          CALL DQPIK(H,V)
        ELSE
          DF=DVHC(IVDFDV,K)
          DT=DVHC(IVDTDV,K)
          TT1=ABS(TAND(TE-DT))
          TT2=ABS(TAND(TE+DT))
          CF1=COSD(FI-DF)
          CF2=COSD(FI+DF)
          SF1=SIND(FI-DF)
          SF2=SIND(FI+DF)
          HH(1)=CF2*TT1
          HH(2)=CF1*TT1
          HH(3)=CF1*TT2
          HH(4)=CF2*TT2
          VV(1)=SF2*TT1
          VV(2)=SF1*TT1
          VV(3)=SF1*TT2
          VV(4)=SF2*TT2
          CALL DQRAR(HH,VV)
        END IF
  115 CONTINUE
      END
*DK DECPL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DECPL
CH
      SUBROUTINE DECPL(TPR)
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
      EQUIVALENCE (KPIKDP,K)
      CHARACTER *6 DT6
      LOGICAL FOUT,FV,FMAX
      EXTERNAL DVLC
      DATA DXY/1.4875/,DTX/.3/,DFI1/2.7/,DTH1/.23/
     &             ,FOVER/40./,DFI0/15./,DTH0/15./
      DATA SC/50./

      DIMENSION HCR(5),VCRU(5),VCRD(5),PMLCR(0:4)
      DATA HCR / -2.5,  2.5,  2.5, -2.5, -2.5/
      DATA VCRU/ 17.3, 17.3, 43.9, 43.9, 17.3/
      DATA VCRD/-17.3,-17.3,-43.9,-43.9,-17.3/
      DATA HTCL,HTCR/-54.5,15./,VTCU,VTCD/52.,-52./,RLC/55./

      CHARACTER *2 TPR
      CHARACTER *3 T3,DT3
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_PP1,J_PP2,J_PTO,J_PLY,J_PAB'
      CALL DPARAM(10
     &  ,J_PFI,J_PTE,J_PP1,J_PP2,J_PTO,J_PLY,J_PAB)
      TPARDA=
     &  'J_SVS,J_SSY,J_SSS,J_SWI,J_SHI'
      CALL DPARAM(63
     &  ,J_SVS,J_SSY,J_SSS,J_SWI,J_SHI)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
C//      CALL DSCEL(PLSDDB,LCLKDD,FVLCDC,FDLCDC,MODE,LCOL,NCDUM)
      CALL DV0(PLSDDB,NUM1,NUM2,FOUT)
      IF(FOUT) GO TO 99
      CALL DCUTFT(FS1,FS2,FC1,FC2,TS1,TS2,TC1,TC2)
      IF(IZOMDO.EQ.0) THEN
        DTH=DTH0
        DFI=DFI0
        FIMID=180.
      ELSE
        FIMID=PARADA(2,J_PFI)
        DTH=DTH1
        DFI=DFI1
      END IF
      TEMID=PARADA(2,J_PTE)
      IF(TPR.EQ.'EC') THEN
        K1=PARADA(2,J_PLY)-1.
        K2=PARADA(4,J_PLY)-1.
        DIR=PARADA(2,J_PAB)
      ELSE
        K1=PARADA(2,J_PP1)-1005.
        K2=PARADA(2,J_PP2)-1005.
        IF(K1.LT.0) K1=0
        DIR=0.
      END IF
      IF(FPIKDP) THEN
        FMAX=.FALSE.
        FV=.FALSE.
        GO TO 100
      END IF

C//   CALL DQPD0(MSYMDL(MECADL,0),WDSNDL(2,4,MECADL),0.)
      CALL DPAR_SET_SY(63,0.)
      CALL DQRA0(2,LCLCDD)
C//   CALL DQPU0(MSYMDL(MECADL,0)         ,SHTFDU)
      CALL DQPU0(IDPAR(J_SSY),SHTFDU)
C//   CALL DQSQ0(WDSNDL(2,5,MECADL)*SQLCDU,SHTFDU)
      CALL DQSQ0(PARADA(2,J_SSS)*SQLCDU,SHTFDU)
C//   MO=MODEDL(MECADL,2)
      MOSY=PARADA(2,J_SVS)
C//   QE=WDSNDL(2,6,MECADL)*CHLCDU
      QE=PARADA(2,J_SHI)*CHLCDU
      IF(MOSY.EQ.2.AND.QE.NE.0.) THEN
C//     CALL DQCH0(QE,WDSNDL(2,7,MECADL))
        CALL DQCH0(QE,PARADA(2,J_SWI))
        T3=DT3(SC/QE)
        CALL DGLEVL(8)
        CALL DQSCE(T3//'Gev LC',9,SC)
        SCALDS(IAREDO,MSECDS)=QE
      END IF
C      CALL DSET_LCAL_COL(MODE,NUCOL,ICCN,ICAR)
      CALL DPARGI(70,'FLC',MOCO)
      CALL DSCELM(K1,K2,MOSY,MOCO,FVLCDC,IM)
      GO TO(10,20,30,40,10),IM
   10 CALL DPAR_SET_CO(53,'CCN')
      FV=.FALSE.
      FMAX=.FALSE.
      GO TO 100
   20 FV=FVLCDC
      FMAX=.FALSE.
      GO TO 100
   30 FV=FVLCDC
      FMAX=.TRUE.
      GO TO 100
   40 IF(K1.EQ.K2) THEN
        FV=FVLCDC
      ELSE
C//        IF(PDCODD(4,ICCNDD).NE.1.) CALL DQLEVL(LCLCDD)
        FV=.FALSE.
      END IF
      FMAX=.FALSE.
  100 CONTINUE
C     ....... For the time being color is set constant,
C     ....... REMOVE NEXT LINE, WHEN MOCO#0 IS USED AGAIN
      IF(MOCO.EQ.0) FV=.FALSE.
      DO 115 K=NUM1,NUM2,3
        IF(DIR*DVLC(IVZZDV,K).LT.0.) GO TO 115
        TE=DVLC(IVTEDV,K)
        IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 115
        FI=DFINXT(FIMID,DVLC(IVFIDV,K))
        IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 115
        EN=0.
        EM=-999.
        DO 102 I=K+K1,K+K2
          CALL DCUTLC(I,FOUT)
          IF(.NOT.FOUT) THEN
            E=DVLC(IVENDV,I)
            EN=EN+E
            IF(FMAX) THEN
              IF(E.GT.EM) THEN
                EM=E
                MX=I
              END IF
            ELSE
              MX=I
            END IF
          END IF
  102   CONTINUE
        IF(EN.LE.0.) GO TO 115
        IF(FV) CALL DQRA0(2,NCLCDC(MX))
        IF(IZOMDO.NE.0.AND.TPR.EQ.'FT'.AND.MOSY.EQ.1) THEN
          X=DVLC(IVXXDV,K)
          Y=DVLC(IVYYDV,K)
          Z=DVLC(IVZZDV,K)
          CALL DECTFB(X,Y,Z)
        ELSE IF(TPR.EQ.'FT') THEN
          H=-TE
          IF(FPIKDP) THEN
            CALL DQPIF(H,FI)
            GO TO 115
          END IF
          IF (MOSY.EQ.2) THEN
            CALL DQCH(H,FI,EN)
            IF(IZOMDO.EQ.0.AND.FI.LT.FOVER) CALL DQCH(H,FI,EN)
          ELSE IF (MOSY.EQ.4) THEN
            T3=DT3(EN)
            CALL DQTXT(H-DTX,FI,T3,3)
          ELSE IF(MOSY.EQ.5) THEN
            CALL DQSQ(H,FI,EN,DTH,DFI)
            IF(IZOMDO.EQ.0.AND.FI.LT.FOVER) CALL DQSQ(H,FI,EN,DTH,DFI)
          ELSE
            CALL DQPD(H,FI)
            IF(IZOMDO.EQ.0.AND.FI.LT.FOVER) CALL DQPD(H,FI)
          END IF
        ELSE
          H=DVLC(IVXXDV,K)
          V=DVLC(IVYYDV,K)
          IF(FPIKDP) THEN
            CALL DQPIK(H,V)
            GO TO 115
          END IF
          IF (MOSY.EQ.2) THEN
            CALL DQCH(H,V,EN)
          ELSE IF (MOSY.EQ.4) THEN
            T3=DT3(EN)
            CALL DQTXT(H-DXY,V,T3,3)
          ELSE IF(MOSY.EQ.5) THEN
            CALL DQSQ(H,V,EN,DXY,DXY)
          ELSE
            CALL DQPU(H,V,DXY,DXY)
          END IF
        END IF
  115 CONTINUE

C     ................................................... LCAL crack
   99 CALL DPARGI_24(56,'CLC',ICLC,ICLC4)
      CALL DPARGV_24(14,'PTH',PCUT,IPTH4)
      IF(IPTH4.LT.0) PCUT=-1.
      
      IF(ICLC4.GT.0) THEN
        CALL DV_LC_CRACK(PMLCR)

        IF(PMLCR(0).GT.0..OR.IPTH4.LT.0.) THEN
          CALL DQPO0('LINE',0,ICLC,' ')
          IF(PARADA(2,J_PAB).GE.0.) THEN

            IF(PMLCR(3).GE.PCUT) CALL DQPOL(5,HCR,VCRD)
            IF(PMLCR(4).GE.PCUT) CALL DQPOL(5,HCR,VCRU)

            IF(PARADA(2,J_PTO).EQ.RLC) THEN
              IF(PMLCR(3).GE.PCUT.OR.PMLCR(4).GE.PCUT) THEN
                TXTADW='crackA='//DT6(PMLCR(3))
                CALL DQTXT(HTCR,VTCD,TXTADW,13)
                TXTADW='crackA='//DT6(PMLCR(4))
                CALL DQTXT(HTCR,VTCU,TXTADW,13)
              END IF
            END IF

          END IF

          IF(PARADA(2,J_PAB).LE.0.) THEN
            IF(PMLCR(1).GE.PCUT) CALL DQPOL(5,HCR,VCRU)
            IF(PMLCR(2).GE.PCUT) CALL DQPOL(5,HCR,VCRD)

            IF(PARADA(2,J_PTO).EQ.RLC) THEN
              IF(PMLCR(1).GE.PCUT.OR.PMLCR(2).GE.PCUT) THEN
                TXTADW='crackB='//DT6(PMLCR(1))
                CALL DQTXT(HTCL,VTCU,TXTADW,13)
                TXTADW='crackB='//DT6(PMLCR(2))
                CALL DQTXT(HTCL,VTCD,TXTADW,13)
              END IF
            END IF

          END IF
        END IF
      END IF
      END


*DK DECSKW
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DECSKW
CH
      SUBROUTINE DECSKW(HRB,VRB,FLEGO)
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
      DIMENSION HRB(4),VRB(4)
      LOGICAL FLEGO,F1
      F1=.FALSE.
      GO TO 1
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DECSK0
CH
      ENTRY DECSK0(FLEGO)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
      F1=.TRUE.
    1 FLEGO=.FALSE.
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PSK'
      CALL DPARAM(13
     &  ,J_PSK)
      TPARDA=
     &  'J_SSY'
      CALL DPARAM(63
     &  ,J_SSY)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
C//   IF( MODEDL(MECADL,2).EQ.2) THEN
      IF(PARADA(2,J_SSY).EQ.10) THEN
        IF(PARADA(4,J_PSK).EQ.1.) THEN
          SK=PARADA(2,J_PSK)
          IF(SK.NE.0.) THEN
            FLEGO=.TRUE.
            IF(F1) RETURN
            ST=SIND(SK)
            CT=COSD(SK)
            H1=HRB(1)
            V1=VRB(1)
            H3=HRB(3)
            V3=VRB(3)
            HRB(1)=CT*H1-ST*V1
            VRB(1)=ST*H1+CT*V1
            HRB(2)=CT*H3-ST*V1
            VRB(2)=ST*H3+CT*V1
            HRB(3)=CT*H3-ST*V3
            VRB(3)=ST*H3+CT*V3
            HRB(4)=CT*H1-ST*V3
            VRB(4)=ST*H1+CT*V3
          END IF
        END IF
      END IF
      END
*DK DECTFB
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DECTFB
CH
      SUBROUTINE DECTFB(XM,YM,Z)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!: Draw xy square as thete phi box
C    Inputs    :lower and upper corner
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      DIMENSION X(2),Y(2)
      DIMENSION NX(4),NY(4),H(4),V(4)
      DATA DL/1.3/
      DATA NX/1,2,2,1/
      DATA NY/1,1,2,2/
      DATA FOVER/44./
C     ........................ FOVER>40, as V(1) is used instead of box center
      X(1)=XM-DL
      Y(1)=YM-DL
      X(2)=XM+DL
      Y(2)=YM+DL
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI'
      CALL DPARAM(11
     &  ,J_PFI)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      DO 700 K=1,4
        XX=X(NX(K))
        YY=Y(NY(K))
        RO=SQRT(XX*XX+YY*YY)
        H(K)=-DATN2D(RO,Z)
        V(K)=DFINXT(PARADA(2,J_PFI),DATN2D(YY,XX))
  700 CONTINUE
      CALL DQRAR(H,V)
      IF(IZOMDO.EQ.0.AND.V(1).LT.FOVER) THEN
        DO 701 K=1,4
          V(K)=V(K)+360.
  701   CONTINUE
        CALL DQRAR(H,V)
      END IF
      END
*DK DEFD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DEFD
CH
      SUBROUTINE DEFD
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
      DATA TET3L/9./,TETA1/-181./,PHI1/-1./,PHI2/361./
      DATA HTXT/110./,VTXT/20./,HPHI/1./,N8/8/
      DIMENSION HRB(4),VRB(4),NGR(1)
      DATA NGR/1/,QSK/0.005/
      CHARACTER *3 DT3
      CHARACTER *10 TP
C              123456789 12345
      DATA TP/'f (90)=123'/
      CHARACTER *12 TYP1,TYP2
C                123456789 12
      DATA TYP1/'Type 1234567'/
      CHARACTER *1 TYPPH(0:9)
      DATA TYPPH/'T','E','M','V','G','R','H','L','?','?'/
      CHARACTER *7 DT7
      LOGICAL FOUT
      IF(FPIKDP) RETURN
      CALL DQCL(IAREDO)
      BORD=MOD(DFWIDU(IZOMDO),10.)
      CALL DQWIL(0.)
C     ............................................................... IZOMDO=0!
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_EFI,J_ESK,J_ETY,J_ENB,J_END,J_EDF'
      CALL DPARAM(32
     &  ,J_EFI,J_ESK,J_ETY,J_ENB,J_END,J_EDF)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PARADA(4,J_EDF).EQ.0.) THEN
        TETA2=TET3L*(HHGHDG(   0  )-HMINDG(   0  ))
     &             /(HHGHDG(IAREDO)-HMINDG(IAREDO))
        CALL DQRER(0,TETA1,PHI1,TETA2,PHI2,HRB,VRB)
      ELSE
        DT=0.5*(PHI2-PHI1)*(VHGHDG(IAREDO)-VMINDG(IAREDO))
     &                    /(HHGHDG(IAREDO)-HMINDG(IAREDO))
        CALL DQRER(0,PHI1,-90.+DT,PHI2,-90.-DT,HRB,VRB)
      END IF
      IF(ABS(PARADA(4,J_END)).EQ.2.) THEN
        DH=QSK*PARADA(2,J_ESK)*(VRB(1)-VRB(3))
        HRB(1)=HRB(1)-DH
        HRB(2)=HRB(2)-DH
        HRB(3)=HRB(3)+DH
        HRB(4)=HRB(4)+DH
      END IF
      CALL DQRU(HRB,VRB)
      NB=ABS(PARADA(4,J_ENB))
      IF     (NB.EQ.1) THEN
        NUM1=1
        CALL=DVEF0(NUM2)
        CALL DEFBIN(DVEF,NUM1,NUM2)
      ELSE IF(NB.EQ.2) THEN
        NUM1=1
        CALL=DVEF0(NUM2)
        CALL DEFBIN(DVEF,NUM1,NUM2)
      ELSE IF(NB.EQ.3) THEN
        CALL DV0(ESDADB,NUM1,NUM2,FOUT)
        CALL DEFBIN(DVEC,NUM1,NUM2)
      ELSE IF(NB.EQ.4) THEN
        CALL DV0(HSDADB,NUM1,NUM2,FOUT)
        CALL DEFBIN(DVHC,NUM1,NUM2)
      END IF
      CALL DGLEVL(N8)
      H=HMINDG(IAREDO)+HPHI
      V=VHGHDG(IAREDO)-VTXT
      TP(8:10)=DT3(PARADA(2,J_EFI))
      CALL DGTXTG(H,V,TP,10,NGR,1)
      IF(ABS(PARADA(4,J_ENB)).LE.2..AND.PARADA(4,J_ETY).EQ.1.) THEN
        TYP1(6:12)=DT7(PARADA(2,J_ETY))
        V=VMINDG(IAREDO)+VTXT
        CALL DGTEXT(H,V,TYP1,12)
        V=V+VTXT
        TYP2=' '
        DO L=6,12
          IF(TYP1(L:L).EQ.' ') GO TO 7
          READ(TYP1(L:L),1007) IP
 1007     FORMAT(I1)
          TYP2(L:L)=TYPPH(IP)
        END DO
    7   CALL DGTEXT(H,V,TYP2,12)
      END IF
      IF(PARADA(4,J_EDF).EQ.0.) THEN
        H=HHGHDG(IAREDO)-HTXT
        V=VHGHDG(IAREDO)-VTXT
        CALL DGTEXT(H,V,'MAXIMUM=',8)
      END IF
      CALL DPCSAR
      CALL DQFR(IAREDO)
      END
*DK DEFBIN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DEFBIN
CH
      SUBROUTINE DEFBIN(DFU,NUM1,NUM2)
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
      EXTERNAL DFU
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_EDF'
      CALL DPARAM(32
     &  ,J_EDF)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PARADA(4,J_EDF).EQ.0.) THEN
        CALL DEFB2(DFU,NUM1,NUM2,1,2)
      ELSE
        CALL DEFB2(DFU,NUM1,NUM2,2,1)
      END IF
      END
*DK DEFB2
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DEFB2
CH
      SUBROUTINE DEFB2(DFU,NUM1,NUM2,J1,J2)
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
      EXTERNAL DFU
      DIMENSION E(0:39,0:79),EM(0:79)
      DIMENSION W(0:39,0:79),TVL(5)
      DATA N7/7/,N8/8/,NS/12/,N12/12/,NAR/4/
      DIMENSION HVL(2,2),HV(2),DHV(2)
      CHARACTER *5 T5
      DATA QSL/3./
      DATA NW/1/
      DIMENSION ICOL(0:5)
      DATA ICOL/14,13,12,10,9,8/
      DATA HD/45./
      DIMENSION HVA(82,2),HVAR(2)
      DATA LDEB/0/
      LOGICAL FIN,FTYP0,FTYP(0:8),FCOL
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_EFI,J_ETY,J_ENB,J_END,J_ESZ,J_EDF,J_EDT'
      CALL DPARAM(32
     &  ,J_EFI,J_ETY,J_ENB,J_END,J_ESZ,J_EDF,J_EDT)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      NA=PARADA(2,J_ENB)
      ND=PARADA(2,J_END)
      DA1=180./PARADA(2,J_ENB)
      FIMID=PARADA(2,J_EFI)+90.
      IF(FIMID.GE.360.) FIMID=FIMID-360.
      IF(NAR.GE.0) THEN
        CALL DGLEVL(NAR)
        N=0
        DO TE=0.,180.,DA1
          CT=SIND(TE)
          HVAR(2)=180.-180.*CT
          HVAR(1)=-TE
          N=N+1
          CALL DQPOC(HVAR(J1),HVAR(J2),HVA(N,J1),HVA(N,J2),FIN)
        END DO
        DO TE=180.,0.,-DA1
          CT=SIND(TE)
          HVAR(2)=180.+180.*CT
          HVAR(1)=-TE
          N=N+1
          CALL DQPOC(HVAR(J1),HVAR(J2),HVA(N,J1),HVA(N,J2),FIN)
        END DO
        CALL DGAREA(N,HVA(1,J1),HVA(1,J2))
      END IF
      CALL DPARGV(68,'SDW',2,DLINDD)
      CALL DPARGV(98,'UDF',2,DFL)
      CALL DGLEVL(N7)
      DO FI=-180.,180.,DFL
        HVL(1,2)=180.
        HVL(1,1)=0.
        DO TE=DA1,180.,DA1
          CT=SIND(TE)
          HVL(2,2)=180.+FI*CT
          HVL(2,1)=-TE
          CALL DQLIE(HVL(1,J1),HVL(1,J2))
          HVL(1,2)=HVL(2,2)
          HVL(1,1)=HVL(2,1)
        END DO
      END DO
      CALL DPARGI(98,'UNT',NVL)
      IF(NVL.GT.0) THEN
        CALL DPAR_GET_REAL(98,'UT1',5,TVL)
        DO K=1,NVL
          CT=SIND(TVL(K))
          HVL(1,2)=180.-180.*CT
          HVL(2,2)=180.+180.*CT
          HVL(1,1)=-TVL(K)
          HVL(2,1)=-TVL(K)
          CALL DQLIE(HVL(1,J1),HVL(1,J2))
        END DO
      END IF
      IF(NUM2.NE.0) THEN
        EMT0=0.
        IF(ABS(PARADA(4,J_ENB)).LE.2.
     &    .AND.PARADA(4,J_ETY).EQ.1.) THEN
          NTYP=PARADA(2,J_ETY)
          CALL VZERO(FTYP,8)
          DO K=1,7
            KK=MOD(NTYP,10)
            FTYP(KK)=.TRUE.
            NTYP=NTYP/10
            IF(NTYP.EQ.0) GO TO 5
          END DO
    5     FTYP0=.TRUE.
          CALL VZERO(E,3200)
          CALL VZERO(EM,80)
          DO K=NUM1,NUM2
            TE=DFU(IVTEDV,K)
            FI=DFU(IVFIDV,K)
            F0=FI-FIMID
            IF(F0.LT.-180.) F0=F0+360.
            IF(F0.GE. 180.) F0=F0-360.
            ST=SIND(TE)
            FT=180.+F0*ST
            NF=FT/DA1
            NT=TE/DA1
            EE=DFU(IVENDV,K)
            IF(EE.GT.0.) THEN
              E(NT,NF)=E(NT,NF)+EE
              EM(NF)=1.
              EMT0=MAX(EMT0,E(NT,NF))
            END IF
          END DO
          IF(EMT0.GT.0) THEN
            DA2=0.5*DA1
            CALL DQPU0(6,SHTFDU)
            CALL DGLEVL(NS)
            CALL DPARGV(68,'SBW',2,DLINDD)
            DO NF=0,2*NA-1
              IF(EM(NF).GT.0.) THEN
                HV(2)=DA2+NF*DA1
                DO NT=0,NA-1
                  IF(E(NT,NF).GT.0.) THEN
                    HV(1)=-(DA2+NT*DA1)
                    CALL DQPU(HV(J1),HV(J2),DA2,DA2)
                  END IF
                END DO
              END IF
            END DO
          END IF
        ELSE
          FTYP0=.FALSE.
        END IF
        CALL VZERO(W,3200)
        CALL VZERO(E,3200)
        CALL VZERO(EM,80)
        EMAX=0.
        DO 700 K=NUM1,NUM2
          IF(FTYP0) THEN
            IT=DFU(IVLVDV,K)
            IF(.NOT.FTYP(IT)) GO TO 700
          END IF
          TE=DFU(IVTEDV,K)
          FI=DFU(IVFIDV,K)
          F0=FI-FIMID
          IF(F0.LT.-180.) F0=F0+360.
          IF(F0.GE. 180.) F0=F0-360.
          CT=SIND(TE)
          FT=180.+F0*CT
          NF=FT/DA1
          NT=TE/DA1
          EE=DFU(IVENDV,K)
          IF(EE.GT.0.) THEN
            NW=3-NW
            IF(NW.EQ.1) W(NT,NF)=W(NT,NF)+EE
            E(NT,NF)=E(NT,NF)+EE
            EM(NF)=MAX(EM(NF),E(NT,NF))
            EMAX=MAX(EMAX,E(NT,NF))
          END IF
  700   CONTINUE
        EMAX=MAX(EMAX,EMT0)
        IF(EMAX.GT.0) THEN
          MO=ABS(PARADA(4,J_END))
          DA2=0.5*DA1
          IF(MO.NE.6) THEN
            DHV(1)=DA2
            DHV(2)=DA2
          ELSE
            DHV(1)=DA2*PARADA(2,J_EDT)
            DHV(2)=DA2*PARADA(2,J_EDF)
          END IF
          CALL DGLEVL(N8)
          IF     (MO.GE.5) THEN
            SZ=PARADA(2,J_ESZ)
            CALL DQSQ0(SZ*ECSQDU,SHTFDU)
            IF(PARADA(4,J_ESZ).EQ.-1.) THEN
              CALL DQSQ1(DHV(J1),DHV(J2),SMAX)
              SZ=SMAX/EMAX
              CALL DQSQ0(SZ,SHTFDU)
              PARADA(2,J_ESZ)=SZ/ECSQDU
            END IF
          ELSE IF(MO.EQ.2) THEN
            SL=PARADA(2,J_ESZ)
            QE=SL*ECCHDU*QSL
            CALL DPARGV(87,'SLW',2,WIDCH)
            CALL DQCH0(QE,WIDCH)
            HN=DA2+(NA-1)*DA1
          ELSE IF(MO.EQ.3) THEN
            CALL DQPU0(  8      ,SHTFDU)
            IF(RDCODD(N12).EQ.GRCODD(N12).AND.
     &         RDCODD(N12).EQ.BLCODD(N12) ) THEN
              FCOL=.FALSE.
            ELSE
              FCOL=.TRUE.
            END IF
          END IF
          DO NF=0,2*NA-1
            IF(EM(NF).GT.0.) THEN
              HV(2)=DA2+NF*DA1
              LMIN=15
              DO NT=0,NA-1
                IF(E(NT,NF).GT.0.) THEN
                  IF(LDEB.EQ.1) THEN
                    QL=W(NT,NF)/E(NT,NF)
                    IF     (QL.EQ.0.) THEN
                      LEV=0
                    ELSE IF(QL.EQ.1.) THEN
                      LEV=5
                    ELSE
                      LEV=1+4.*QL
                    END IF
                    CALL DGLEVL(ICOL(LEV))
                  END IF
                  HV(1)=-(DA2+NT*DA1)
                  IF(     MO.EQ.4) THEN
                    CALL DTN(E(NT,NF),ND,T5)
                    CALL DQTXT(HV(J1)-DA2,HV(J2),T5,ND)
                  ELSE IF(MO.GE.5) THEN
                    SQ=E(NT,NF)
                    CALL DQSQ(HV(J1),HV(J2),SQ,DHV(J1),DHV(J2))
                  ELSE IF(MO.EQ.2) THEN
                    CALL DQCH(HV(J1),HV(J2),E(NT,NF))
                  ELSE
                    EE=E(NT,NF)
                    ER=0.5*EMAX
                    IF(LDEB.EQ.0) THEN
                      DO L=8,14
                        IF(EE.GT.ER) GO TO 7
                        ER=0.5*ER
                      END DO
                      L=15
    7                 LMIN=MIN(L,LMIN)
                      CALL DGLEVL(L)
                    END IF
                    CALL DQPU(HV(J1),HV(J2),DA2,DA2)
                  END IF
                END IF
              END DO
              IF(J2.EQ.2) THEN
                IF(LDEB.EQ.0) THEN
                  IF(FCOL.AND.MO.EQ.3) CALL DGLEVL(LMIN)
                ELSE
                  CALL DGLEVL(N8)
                END IF
                WRITE(T5,1000) EM(NF)
 1000           FORMAT(F5.2)
                IF(T5(1:2).EQ.' 0') T5(1:2)='  '
                CALL DQPOC(0.,HV(2),HDUM,VD,FIN)
                HTD=HHGHDG(IAREDO)-HD
                CALL DGTEXT(HTD,VD,T5,5)
              END IF
            END IF
          END DO
          IF(MO.NE.6.AND.NS.GE.0) THEN
            CALL DQPU0(6,SHTFDU)
            CALL DGLEVL(NS)
            CALL DPARGV(68,'SBW',2,DLINDD)
            DO NF=0,2*NA-1
              IF(EM(NF).GT.0.) THEN
                HV(2)=DA2+NF*DA1
                DO NT=0,NA-1
                  IF(E(NT,NF).GT.0.) THEN
                    HV(1)=-(DA2+NT*DA1)
                    CALL DQPU(HV(J1),HV(J2),DA2,DA2)
                  END IF
                END DO
              END IF
            END DO
          END IF
        END IF
      END IF
      END
*DK DEVNEW
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DEVNEW
CH
      SUBROUTINE DEVNEW
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
      INCLUDE 'J_RLUNIT.INC'
C      IF(FMINDE) CALL DJMINI
      FORDDT=.FALSE.
      FNLADN=.TRUE.
      FMHTDN=.TRUE.
      FNOCDC=.FALSE.
      CALL=DVITCL(0)
      CALL=DVTPCL(0)
      CALL DSC0
      CALL VZERO(COLRDT,999)
      CALL VZERO(ULABDT,999)
      PARADR(2,1)=0.
      CALL DAC0
      IFULDB=1
      CALL DQTIT(IFULDB)
      CALL DJSTR
C     CALL DPITR0(0,0)
      CALL DTE0
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_CFM'
      CALL DPARAM(40
     &  ,J_CFM)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      PARADA(4,J_CFM)=-1.
      DO N=0,12
        PSTODS(2,J_CFM,N,IWUSDO)=-1.
      END DO
      CALL DVVXRN(IRUNDE(1))
      CALL DVFTR(0,NDUM)
      CALL DAP_KALMANN_TRACKS_IN
      IF(RNCLDU.EQ.0) CALL DTXCA
      END
*DK DEVSET
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DEVSET
CH
      SUBROUTINE DEVSET(NOPFI)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C_CG   7-May-1989 C.Grab  Adapted to CERNVM
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C     If MBCNDB is changed IN DALI_CF.INC
C     DALBE.FOR    DEVSET: DATA BNAME,MAXB
C                          CALL DV___0( NUM(____DB) ),
C                  DEVTYP: FORMAT 1004
C     DALBX.FOR    DALBX : DATA NUMBO
C     DALIJ.FOR      DJEV: DATA TPR2
C     DALIP.FOR    RECOMPILE
C     DALIA.FOR    RECOMPILE
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      INCLUDE 'J_RLUNIT.INC'
      DIMENSION NUM(MBNCDB),NUSL(MBNCDB)
      CHARACTER *4 TNUM(MBNCDB),DT4
      DATA TNUM/MBNCDB*'    '/
      CHARACTER *49 T
      CHARACTER *1 TSL(-1:1)
      DATA TSL/'/','=',':'/
      DIMENSION MAXB(MBNCDB)
      CHARACTER *4 BNAME(MBNCDB)
      DATA BNAME/
     &  'fver' ,  'FKin' ,  'ITco' ,  'tPAd' ,  'TBco' ,
     &  'TPco' ,  'ESda' ,  'HSda' ,  'HTub' ,  'MHit' ,
     &  'FRft' ,  'FrTl' ,  'ftcl' ,  'ficl' ,  'PYer' ,
     &  'FriD' ,  'pLSd' ,  'pEcO' ,  '*EW*' ,  'reco' ,
     &  'VdZt' ,  'VdXy' ,  'CRft' ,  'NRft' ,  'VdCo' ,
     &  'VcPl' ,  'YKn*'/
      DATA MAXB/
     &   99    ,   999   ,   1999  ,  5999   ,  1999   ,
     & 1999    ,  9999   ,   1999  ,  9999   ,  1999   ,
     &   99    ,    99   ,   1999  ,  1999   ,    99   ,
     &   99    ,  2999   ,    999  ,  5999   ,   999   ,
     &  999    ,   999   ,     99  ,    99   ,   999   ,
     &  999    ,    99  /
      CHARACTER *5 DT5,TPTOT
      T=' '
      IF(NOPFI.EQ.0) THEN
        DO 790 N=1,MBNCDB
          BNUMDB(2,N)=0.
          BNUMDB(3,N)=0.
  790   CONTINUE
        RETURN
      END IF
      IF(IRUNDE(1).LT.4000) THEN
        CALL DVMCNV(  NUM(FVERDB),NUM(FKINDB))
      ELSE
        NUM(FVERDB)=0
        NUM(FKINDB)=0
      END IF
      CALL=DVIT0(   NUM(ITCODB))
      CALL=DVPAD0(0,NUM(TPADDB))
      CALL=DVTB0(   NUM(TBCODB))
      CALL=DVTQ0(   NUM(TPCODB))
      CALL=DVEC0(   NUM(ESDADB))
      CALL=DVHC0(   NUM(HSDADB))
      CALL=DVHT0(   NUM(HTUBDB))
      CALL=DVMD0(   NUM(MHITDB))
      CALL DVTR0(   NUM(FRFTDB))
      CALL DVCHIT(  NUM(FRTLDB),NUM(FTCLDB),NUM(FICLDB),NCH)
      CALL DVPRVX(  NUM(PYERDB))
      CALL DVVTX0(  NUM(PYERDB))
      CALL DVFRID(  NUM(FRIDDB))
      CALL=DVLC0(   NUM(PLSDDB))
      CALL=DVEO0(   NUM(PECODB))
      CALL DVEW0(   NUM(PEWDDB))
      CALL=DVVDR1(  NUM(VDZTDB))
      CALL=DVVDX1(  NUM(VDXYDB))
      CALL DVVDTR(NWAFDV,  
     &              NUM(FRFTDB)
     &             ,NUM(VDXYDB)
     &             ,NUM(VDZTDB)
     &             ,NUM(VCPLDB))
      CALL=DVVDC0(  NUM(VDCODB))
      CALL DVTC0(   NUM(CRFTDB))
      CALL DVTN0(   NUM(NRFTDB))
      CALL DVKNV0(  NUM(YKNVDB))
      DO 700 K=1,MBNCDB
         TNUM(K)=DT4(FLOAT(NUM(K)))
C         ...................................... Why min? 16.7.98 
C         BNUMDB(2,K)=MIN(NUM(K),MAXB(K))
         BNUMDB(2,K)=NUM(K)
         BNUMDB(3,K)=BNUMDB(2,K)
  700 CONTINUE
C     BNUMDB(2,FRFTDB)=NCH      ??
      PMAX=0.
      NTRMX=0
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_XZF,J_XZT'
      CALL DPARAM(10
     &  ,J_PFI,J_PTE,J_XZF,J_XZT)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(NUM(FRFTDB).EQ.NUM(FRTLDB).
     &  AND.NUM(TPCODB).GT.0.
     &  AND.NUM(FTCLDB).GT.0.) THEN
         CALL=DVCHT0(NTRAK)
         DO 710 N=1,NTRAK
            CALL DVTRTP(N,PTT)
            PTOT=ABS(PTT)
            IF(PTOT.GT.PMAX) THEN
               CALL=DVCHT(N,NCO)
               IF(NCO.NE.0) THEN
                  PMAX=PTOT
                  PMX=PTT
                  NTRMX=N
               END IF
            END IF
  710    CONTINUE
C           123456789 123456789 123456789 123456789 123456789
C        T='FI,TE set to: FI=123. TE=123.   DZ=12.3 TE=TE(DZ)'
         T=' FI , TE unchanged              DZ=?    TE=TE(0)'
         IF(PMAX.NE.0.) THEN
            CALL=DVCHT(NTRMX,NCOMX)
            K=IDVCHT(NCOMX)
            FI=DVTP(IVFIDV,K)
            TE=DVTP(IVTEDV,K)
            CALL DFTM0(FI,TE,PMX)
            IF(FI.GT.180.) THEN
              FI=FI-180.
              TE=180.-TE
            END IF
            FIMXDE=FI
            TEMXDE=TE
            IF(CHTFDU.NE.0.) THEN
              IF(CHTFDU.GT.1.) CALL DRZFI(FI)
              PARADA(2,J_PFI)=FI
              PARADA(2,J_PTE)=TE
              PARADA(2,J_XZF)=FI
              PARADA(2,J_XZT)=TE
              DO 720 J=0,MPARDS
                PSTODS(1,J_PFI,J,IWUSDO)=FI
                PSTODS(1,J_PTE,J,IWUSDO)=TE
  720         CONTINUE
              CALL DYZNUD('CALC')
              T( 1:29)='FI,TE set to: FI=123. TE=123.'
              T(18:21)=DT4(FI)
              T(26:29)=DT4(TE)
            END IF
         END IF
      END IF
      IF(NUM(PYERDB).GT.0) THEN
        T(33:49)='DZ=12.4 TE=TE(DZ)'
        T(36:39)=DT4(VRTZDV)
        IF(VRDZDV.EQ.0.) T(47:49)='0)'
      END IF
      IF(T.NE.' ') CALL DWRT(T)
      CALL DORDTR
      CALL DVX0
      CALL DKRO0
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DEVTYP
CH
      ENTRY DEVTYP(NOPFI)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modifications:
C_CG 9-May-1989 C.Grab  Adapted to CERNVM
C
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C      IF(NOPFI.EQ.0) RETURN
      CALL DWRT('.................................................')
      DO 730 K=1,MBNCDB
         NUSL(K)=BNUMDB(4,K)
  730 CONTINUE
      TNUM(PYERDB)=DT4(BNUMDB(2,PYERDB))
      DO I1=1,MBNCDB,5
        I2=MIN(MBNCDB,I1+4)
        WRITE(TXTADW,1004)(BNAME(I),TSL(NUSL(I)),TNUM(I),I=I1,I2)
 1004   FORMAT(5(1X,3A))
        CALL DWRC
      END DO
      TPTOT=DT5(PMX)
      IF(PMAX.GT.0.) THEN
        WRITE(TXTADW,1005) NTRMX,TPTOT
 1005 FORMAT('TPC track',I4,' has maximum momentum ',A,'[GEV]')
        CALL DWRC
      END IF
      IF(.NOT.FMHTDN) CALL DVMD2D
      END
