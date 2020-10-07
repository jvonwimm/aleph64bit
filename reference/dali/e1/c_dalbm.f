*DK DMAXLG
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DMAXLG
CH
      SUBROUTINE DMAXLG(TXZR,N,TETA,RA)
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
      CHARACTER *1 TXZR
CDELET      DATA TC/42./,DT/0./
      IF(TXZR.EQ.'X') THEN
         RA=ROTODK(N)
         RETURN
      END IF
      IF(TXZR.EQ.'Z'.OR.TXZR.EQ.'S') THEN
         RA=ZZTODK(N)
         RETURN
      END IF
      IF(TETA.LE.90.) THEN
         TE=TETA
      ELSE
         TE=180.-TETA
      END IF
CDELET      IF(TE+DT.LT.TC) THEN
CDELET            RO1=ZZTODK(N)*TAND(TE)
CDELET            RO2=ZZTODK(N)*TAND(TE+DT)
CDELET            DRO=RO2-RO1
CDELET            R1=ZZTODK(N)/COSD(TE)
CDELET            R2=DRO*SIND(TE)
CDELET            RA=R1+R2
CDELET      ELSE IF(TE-DT.GT.TC) THEN
CDELET            Z1=ROTODK(N)/TAND(TE)
CDELET            Z2=ROTODK(N)/TAND(TE-DT)
CDELET            DZ=Z2-Z1
CDELET            R1=ROTODK(N)/SIND(TE)
CDELET            R2=DZ*COSD(TE)
CDELET            RA=R1+R2
CDELET      ELSE
CDELET            RA=SQRT(ROTODK(N)**2+ZZTODK(N)**2)
CDELET      END IF
      CT=COSD(TE)
      ST=SIND(TE)
      IF(TE.GT.60.) THEN
         RA=ROTODK(N)/ST
      ELSE IF(TE.LT.10.) THEN
         RA=ZMTODK(N)/CT
      ELSE
         RA=MIN(ROTODK(N)/ST,ZMTODK(N)/CT)
      END IF
      RETURN
      END
*DK DMCNVX
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DMCNVX
CH
      SUBROUTINE DMCNVX(NVTX1)
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
      LOGICAL FOUT,FCOL
      DATA CP2/0.000001/,CLOGT/-13./
      DIMENSION JCOL(-1:6)
      DATA JCOL/9,12,14,10,15,13,8,11/
C      LOGICAL FMVXDT(100)
      DIMENSION RMAX(9),ZMAX(9)
C               VX  VD   IT    T0    T3   E1   E3     H1    H2
      DATA RMAX/0.,12.8, 28.,31.64,177.9,84.7,229.4 ,297.3,468.4/
      DATA ZMAX/0.,100.,100.,220. ,220.,250.5,307.16,315. ,483.4/
      DATA NCCPV,NCUPV,NCCSV,NCUSV/9,10,12,13/
      NVTX1=-1
      LVTX1=0
      IF(BNUMDB(2,FKINDB).EQ.0..OR.
     &  BNUMDB(2,FVERDB).EQ.0..OR.
     &  BNUMDB(4,FKINDB).LE.0.) RETURN
C//?? IF(MODEDL(MTPCDL,1).EQ.1..AND.MCOLDL(MTPCDL).EQ.0) RETURN
      CALL DVMCNV(NVTX1,NTRK)
      LVTX1=NVTX1
      IF(NVTX1.EQ.0.OR.NTRK.EQ.0) RETURN
      MODEDT=2
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_HLI,J_HP0,J_HCM,J_HF1,J_HF2,J_HT1,J_HT2,J_HZZ,J_HST,J_HCQ'
      CALL DPARAM(20
     &  ,J_HLI,J_HP0,J_HCM,J_HF1,J_HF2,J_HT1,J_HT2,J_HZZ,J_HST,J_HCQ)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(ICOL.NE.0.OR.PARADA(2,J_HP0).EQ.1.) THEN
         FCOL=.FALSE.
         CALL DGLEVL(ICOL)
      ELSE
C//      IF(MODEDL(MTPCDL,1).EQ.1) THEN
C//         FCOL=.FALSE.
C//         CALL DGLEVL(MCOLDL(MTPCDL))
C//      ELSE
C//         FCOL=.TRUE.
C//         NOFS=ABS(WDSNDL(2,2,MTPCDL))
C//         MAMP=WDSNDL(2,3,MTPCDL)
C//         MAMP=MAX(1,MIN(8,MAMP)-1)
C//      END IF
        FCOL=.TRUE.
        CALL DPARGV(58,'CMA',2,AMP)
        MAMP=AMP
        CALL DPARGV(58,'CMO',2,OFS)
        NOFS=OFS
C       ................................     NCTRDM=JCOL(MOD(NVX+NOFS,MAMP))
      END IF
      IF(PARADA(4,J_HLI).NE.1.) THEN
         IHTO=0
         ZM1=9999.
         RM2=9999999.
      ELSE
         IHTO=PARADA(2,J_HLI)
         IHTO=IDMIX(2,IHTO,9)
         ZM1=ZMAX(IHTO)
         RM2=RMAX(IHTO)**2
      END IF
      L=-1
      FMCCDT=.FALSE.
      IF(PARADA(4,J_HCM).EQ.1..AND.PARADA(2,J_HCM).NE.0.) THEN
         IF(PARADA(2,J_HCM).GE.0) THEN
            CMO1=PARADA(2,J_HCM)**2
            CMO2=9999999.
         ELSE
            CMO1=0.
            CMO2=PARADA(2,J_HCM)**2
         END IF
         FMCCDT=.TRUE.
      ELSE
         CMO1=0.
         CMO2=9999999.
      END IF
      IF(PARADA(4,J_HT1).EQ.1.) THEN
         CT1=PARADA(2,J_HT1)
         FMCCDT=.TRUE.
      ELSE
         CT1=-99999.
      END IF
      IF(PARADA(4,J_HT2).EQ.1.) THEN
         CT2=PARADA(2,J_HT2)
         FMCCDT=.TRUE.
      ELSE
         CT2=99999.
      END IF
      IF(PARADA(4,J_HZZ).EQ.1.AND.PARADA(2,J_HZZ).NE.0.) THEN
         IF(PARADA(2,J_HZZ).LT.0.) THEN
            CT1=MAX(90.,CT1)
         ELSE
            CT2=MIN(90.,CT2)
         END IF
         FMCCDT=.TRUE.
      END IF
      IF(PARADA(4,J_HF1).EQ.1.) THEN
         CF1=PARADA(2,J_HF1)
         FMCCDT=.TRUE.
      ELSE
         CF1=-99999.
      END IF
      IF(PARADA(4,J_HF2).EQ.1.) THEN
         CF2=PARADA(2,J_HF2)
         FMCCDT=.TRUE.
      ELSE
         CF2=99999.
      END IF
      IF(PARADA(4,J_HST).EQ.1.AND.PARADA(2,J_HST).NE.0.) THEN
         CST=PARADA(2,J_HST)
         FMCCDT=.TRUE.
      ELSE
         CST=0.
      END IF
      IF(PARADA(4,J_HCQ).EQ.1.AND.PARADA(2,J_HCQ).NE.0.) THEN
         CCH=PARADA(2,J_HCQ)
         FMCCDT=.TRUE.
      ELSE
         CCH=99.
      END IF
      CALL VZERO(FMNTDT,MMCTDT)
      MAXTR=0
      RETURN
C                                      DO 12 N=1,NVTX1
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DMCVX
CH
      ENTRY DMCVX(NVX,KT1,KT2)
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
      KVX=NVX
      IF(FCOL) THEN
         IF(NVX.EQ.1) THEN
            NCTRDM=JCOL(-1)
         ELSE
            NCTRDM=JCOL(MOD(NVX+NOFS,MAMP))
         END IF
      END IF
      CALL DGLEVL(NCTRDM)
      FMVXDT(NVX)=.FALSE.
      CALL DVMCVX(NVX,KT1,KT2,KIN,VTX1DT)
      IF(KT2.EQ.0) GO TO 12
      IF(KT2.GT.MMCTDT) THEN
         IF(MAXTR.EQ.0) THEN
            MAXTR=1
            WRITE(TXTADW,1012)MMCTDT
 1012       FORMAT('More than',I4,' tracks ??')
            CALL DWRC
         END IF
         GO TO 12
      END IF
      ZA=ABS(VTX1DT(3))
      IF(ZA.GE.ZM1) GO TO 12
      RO=VTX1DT(1)**2+VTX1DT(2)**2
      IF(RO.GE.RM2) GO TO 12
      RETURN
   12 KT2=0
      RETURN
C                                       DO 11 K=KT1,KT2
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DMCTR
CH
      ENTRY DMCTR(KTR,FOUT)
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
      FOUT=.TRUE.
      IOK=IHTO
      CALL DVMCTR(KTR,PTRADT,CLTMDT,ITYPE,NVTX2)
      IF(ITYPE.LE.0) RETURN
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_HP0'
      CALL DPARAM(20
     &  ,J_HP0)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(CLTMDT(1).EQ.0.) THEN
         IF(PARADA(4,J_HP0).EQ.-1..OR.PARADA(2,J_HP0).EQ.0.) RETURN
         PT=PTRADT(1)**2+PTRADT(2)**2
         IF(PT.EQ.0.) RETURN
         CLTMDT(1)=PT/PARADA(2,J_HP0)
         IF(KVX.EQ.1) THEN
            NCTRDM=NCUPV
         ELSE
            NCTRDM=NCUSV
         END IF
      ELSE
         IF(PARADA(4,J_HP0).EQ.1..AND.PARADA(2,J_HP0).NE.0.) THEN
            IF(KVX.EQ.1) THEN
               NCTRDM=NCCPV
            ELSE
               NCTRDM=NCCSV
            END IF
         END IF
      END IF
      CALL DGLEVL(NCTRDM)
      IF(CLTMDT(2).LE.CLOGT) RETURN
      PT=PTRADT(1)**2+PTRADT(2)**2
      P2=PT+PTRADT(3)**2
      IF(P2.LT.CP2) RETURN
      IF(FMCCDT) THEN
         IF(P2.LT.CMO1.OR.P2.GT.CMO2) RETURN
         FI=DATN2D(PTRADT(2),PTRADT(1))
         IF(FI.LT.CF1.OR.FI.GT.CF2) RETURN
         PT=SQRT(PT)
         TE=DATN2D(PT,PTRADT(3))
         IF(TE.LT.CT1.OR.TE.GT.CT2) RETURN
         IF(CCH.NE.99..AND.CLTMDT(1).NE.CCH) RETURN
         NST=CST
         IF(NST.GT.0.) THEN
            IF(NST.NE.KTR) RETURN
         ELSE
            IF(-NST.EQ.KTR) RETURN
         END IF
      END IF
      FMNTDT(KTR)=.TRUE.
      IF(NVTX2.NE.0) THEN
         CALL DVMCVX(NVTX2,KDUM1,KDUM2,KDUM3,VTX2DT)
         FVTXDT=.TRUE.
      ELSE
         FVTXDT=.FALSE.
      END IF
      FMVXDT(KVX)=.TRUE.
      FOUT=.FALSE.
      RETURN
      END
*DK IDMIX
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  IDMIX
CH
      FUNCTION IDMIX(M1,M2,M3)
CH
CH ********************************************************************
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:M2=MIN(MAX(M1,M2),M3)
C    Inputs    :M1,M2,M3
C    Outputs   :M2
C
C    Called by :
C ---------------------------------------------------------------------
      IDMIX=MIN(MAX(M1,M2),M3)
      RETURN
      END
