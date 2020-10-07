*DK DAE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DAE
CH
      SUBROUTINE DAE
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
      CHARACTER *4 DT4
      DATA IVNT/9/,INO/0/
      PARAMETER (MP2=10)
      CHARACTER *2 TANSW,TP1(1),TP2(MP2),TLAST
      DATA TLAST/'WF'/
      DIMENSION PR2(4,MP2)
      DATA TP1/'NH'/
      DATA TP2/'NF','DT','DZ','D1','D2','DA','DF','CO','RO','FS'/
      DATA PR2/9.,92.,99.,0., .01,2.,99.,1., -999.,0.,999.,0.,
     &  -9.,2.,99.,0.,  0.,16.,99.,0., 0.,1.,99.,0., 0.,10.,99.,0.,
     &  0.,12.,15.,0., 1.,180.,9999.,1.,  -99.,1.,99.,-1./
      DIMENSION LCTE(0:7)
      DATA LCTE/8,12,10,13,9,14,11,15/,NCTE/6/,ROMID/105./
      DATA RM/170./
      LOGICAL FCHG
      DATA LC5/5/,LC7/7/,LC8/8/,LC9/9/
      CHARACTER *49 T1,T2,T3,T4
C              123456789 123456789 123456789 123456789 123456789
      DATA T1/'P?: ev=12 tr=1234 NH$12345 sum=12345  file: NF_12'/
      DATA T2/'   "CH": col=col(te) DT$123  "CZ": DZ_1234  CO_12'/ 
      DATA T3/'CLean/UnClean: D1_123 D2_123 DA_123 DF_123'/
      DATA T4/'XY compression and skew ("CR") : RO$1234 FS$123'/ 

C
  930 CALL DTSTOI('A',T1, 'ev=',  NEV,2)
      CALL DTSTOI('A',T1, 'tr=',NUMTR,4)
      CALL DTSTOI('A',T1,'sum=', LAST,5)
      CALL DTYPT('SAVE',  ' ' ,1,3,             PR2,TP2,T1)
      CALL DTYPT('TYPE',TPICDO,1,1,BNUMDB(1,TPCODB),TP1,T1)
      CALL DTYPT('TYPE',  ' ' ,1,MP2,           PR2,TP2,T2)
      CALL DTYPT('TYPE',  ' ' ,1,MP2,           PR2,TP2,T3)
      CALL DTYPT('TYPE',  ' ' ,1,MP2,           PR2,TP2,T4)
  936 FCHG=.FALSE.
      CALL DOPER(1,0,
     &  1,1,TP1,BNUMDB(1,TPCODB),
     &  1,MP2,TP2,PR2,
     &  NEXEC,FCHG,TANSW)
      GO TO (910,920,930,940),NEXEC
  910 RETURN
  920 IF(TANSW.EQ.'W0') THEN
        TLAST=TANSW
        CALL=DVTPCL(0)
        VRDZDV=0.
        GO TO 921
      END IF
      IF(TANSW.EQ.'WF') THEN
        TLAST=TANSW
  921   FTDZ=FTDZDU
        FTDZDU=0.
        VRDZ=VRDZDU
        VRDZDU=-1.
        DO N=1,8
          CALL=DVTPST(N,J)
        END DO
        FTDZDU=FTDZ
        VRDZDU=VRDZ
        CALL=DVTPST(IVNTDV,IVNT)
        NUM=BNUMDB(2,TPCODB)
        IF(NW.EQ.0) THEN
          LAST=0
          NUMTR=0
          NEV=0
        END IF
        NW=PR2(2,1)
        NLT=0
        NEV=NEV+1
        DO K=1,NUM
          NT=NTPCDV(IVNT,K)
          WRITE(NW,1000) (VTPCDV(N,K),N=1,8),NT,NEV
 1000     FORMAT(1X,8(F8.3, ',' ),I3,',',I2)
          IF(NT.NE.NLT.AND.NT.GT.0) THEN
            NUMTR=NUMTR+1
            NLT=NT
          END IF
        END DO
        LAST=LAST+NUM
        CALL DWRT(DT4(BNUMDB(2,TPCODB))//' hits added.')
        GO TO 930
      END IF
      IF(TANSW.EQ.'CF') THEN
        CLOSE(UNIT=NW)
        CALL DWRT('File closed.')
        NW=0
        GO TO 930
      END IF
      IF(TANSW.EQ.'RF') THEN
        NUMTR=0
        NLT=0
        NR=PR2(2,1)
        DO K=1,MTPCDV
          READ(NR,1000,END=922) (VTPCDV(N,K),N=1,8),NTPCDV(IVNT,K),NEV
          VTPCDV(IVDIDV,K)=VTPCDV(IVTEDV,K)
          NT=NTPCDV(IVNT,K)
          IF(NT.EQ.0) THEN
            NCTPDC(K)=LC8
          ELSE
C           ............. EXAMPLE: track 12 gets color 12
            NCTPDC(K)=LC9+MOD(NT+LC5,LC7)
          END IF
          IF(NT.NE.NLT.AND.NT.GT.0) THEN
            NUMTR=NUMTR+1
            NLT=NT
          END IF
        END DO
        K=K+1
  922   LAST=K-1
        DO N=1,MBNCDB
          BNUMDB(4,N)=-1.
          BNUMDB(2,N)=0.
        END DO
        BNUMDB(2,TPCODB)=LAST
        BNUMDB(3,TPCODB)=LAST
        BNUMDB(4,TPCODB)=1.
        CALL=DVTPS1(DUMMY)
        CALL VZERO(COLRDT,999)
        FNOCDC=.TRUE.
        FVTPDC=.TRUE.
        CALL DWRT('TPC hits stored')
        CLOSE(UNIT=NR)
        GO TO 930
      END IF
      IF(TANSW.EQ.'CR') THEN
        DO K=1,LAST
          VTPCDV(IVXXDV,K)=VTPCDV(IVRODV,K)*COSD(VTPCDV(IVFIDV,K))
          VTPCDV(IVYYDV,K)=VTPCDV(IVRODV,K)*SIND(VTPCDV(IVFIDV,K))
        END DO
        IF(PR2(4,9).EQ.1.) THEN
          A=PR2(2,9)
          Q=RM/(A+RM)
          Q2CPT=0.
          ASRM=0.
          IF(PR2(4,10).EQ.1.) THEN
            IF(PR2(2,10).NE.0.) THEN
              CCC=-1./(0.003*1.5)
              Q2CPT=1./(2.*CCC*PR2(2,10))
              ASRM=ASIN(ROMID*Q2CPT)
            END IF
          END IF
          DO K=1,LAST
            RS=(A+VTPCDV(IVRODV,K))*Q
            H=VTPCDV(IVRODV,K)
            FS=VTPCDV(IVFIDV,K)-PIFCDK*(ASIN(H*Q2CPT)-ASRM)
            VTPCDV(IVXXDV,K)=RS*COSD(FS)
            VTPCDV(IVYYDV,K)=RS*SIND(FS)
          END DO
        END IF
        GO TO 930
      END IF
      IF(TANSW.EQ.'CH') THEN
  923   IF(PR2(4,2).GT.0.) THEN
          Q=1./PR2(2,2)
          DO K=1,LAST
            ITE=VTPCDV(IVDIDV,K)*Q
            NCTPDC(K)=LCTE(MOD(ITE,NCTE))
          END DO
          FVTPDC=.TRUE.
        ELSE
          FVTPDC=.FALSE.
        END IF
        CALL DSC0
        GO TO 930
      END IF
      IF(TANSW.EQ.'CN') THEN
        NCOL=PR2(2,8)
        DO K=1,LAST
          NCTPDC(K)=NCOL
        END DO
        FVTPDC=.TRUE.
        CALL DSC0
        GO TO 930
      END IF
      IF(TANSW.EQ.'CZ') THEN
        DZ=PR2(2,3)
        DO K=1,LAST
          VTPCDV(IVTEDV,K)=ATAN2(VTPCDV(IVRODV,K),
     &                           VTPCDV(IVZZDV,K)-DZ)*PIDGDK
          VTPCDV(IVDIDV,K)=VTPCDV(IVTEDV,K)
        END DO
        CALL DWRT('DZ changed')
        GO TO 930
      END IF
      IF(TANSW.EQ.'RE') THEN
        NCUT=0
        DR1=PR2(2,4)
        DR2=PR2(2,5)
        DTS=PR2(2,6)
        DFS=PR2(2,7)
        DO K=1,LAST
          VTPCDV(IVTEDV,K)=-128.
        END DO
        DO K=1,LAST-1
          DO N=K+1,LAST
            DR=ABS(VTPCDV(IVRODV,K)-VTPCDV(IVRODV,N))
            IF(DR.GT.DR1.AND.DR.LT.DR2) THEN
              DT=ABS(VTPCDV(IVDIDV,K)-VTPCDV(IVDIDV,N))
              IF(DT.LT.DTS) THEN
                DF=ABS(VTPCDV(IVFIDV,K)-VTPCDV(IVFIDV,N))
                IF(DF.LT.DFS) THEN
                  VTPCDV(IVTEDV,K)=VTPCDV(IVDIDV,K)
                  VTPCDV(IVTEDV,N)=VTPCDV(IVDIDV,N)
                  GO TO 800
                END IF
              END IF
            END IF
          END DO
          NCUT=NCUT+1
  800   END DO
        WRITE(TXTADW,1800) NCUT
 1800   FORMAT(I6,' noise hits')
        CALL DWRC
        GO TO 930
      END IF
      IF(TANSW.EQ.'TN') THEN
        DO K=1,LAST
          IF(VTPCDV(IVTEDV,K).EQ.-128.) THEN
            VTPCDV(IVTEDV,K)=VTPCDV(IVDIDV,K)
          ELSE
            VTPCDV(IVTEDV,K)=-128.
          END IF
        END DO
        GO TO 930
      END IF
      IF(TANSW.EQ.'IN') THEN
        DO K=1,LAST
          VTPCDV(IVTEDV,K)=VTPCDV(IVDIDV,K)
        END DO
        GO TO 930
      END IF
      CALL DWR_IC(TANSW)
      GO TO 936
  940 TANSW=TLAST
      GO TO 920
      END
*DK DAPD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DAPD
CH
      SUBROUTINE DAPD
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
      LOGICAL FOUT,FUSFI,FUSTE
      DIMENSION HRB(4),VRB(4)
      CALL DQCL(IAREDO)
      CALL DQWIL(W0)
      IF(NOCLDT.EQ.0) CALL DQFFWI(IFIX(PDCODD(2,ICTPDD)))
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE'
      CALL DPARAM(11
     &  ,J_PFI,J_PTE)
      TPARDA=
     &  'J_AHM,J_ADH,J_AVM,J_ADV,J_AAL,J_AVV,J_AHV,J_ATH,J_ATV'
      CALL DPARAM(35
     &  ,J_AHM,J_ADH,J_AVM,J_ADV,J_AAL,J_AVV,J_AHV,J_ATH,J_ATV)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      DH=0.5*PARADA(2,J_ADH)
      IF(PARADA(4,J_ADV).LT.0.) THEN
        DV=DH
      ELSE
        DV=0.5*PARADA(2,J_ADV)
      END IF
      CALL DQRER(0,
     &  PARADA(2,J_AHM)-DH,PARADA(2,J_AVM)-DV,
     &  PARADA(2,J_AHM)+DH,PARADA(2,J_AVM)+DV,
     &  HRB,VRB)
      CALL DQRU(HRB,VRB)
      CALL DSCTP
      CALL DV0(TPCODB,NUM1,NUM2,FOUT)
      IF(FOUT) GO TO 9
      CALL DCUTTF(FUSFI,FC1,FC2,FUSTE,TC1,TC2)
C//   CALL DQPD0(MSYMDL(MTPCDL,0),WDSNDL(2,4,MTPCDL),0.)
      CALL DPAR_SET_SY(62,0.)
      FIMID=PARADA(2,J_PFI)
      TEMID=PARADA(2,J_PTE)
      NH=ABS(PARADA(2,J_AHV))
      NV=ABS(PARADA(2,J_AVV))
      SH=SIGN(1.,PARADA(2,J_AHV))
      SV=SIGN(1.,PARADA(2,J_AVV))
      AL=PARADA(2,J_AAL)
      CA=COSD(AL)
      SA=SIND(AL)
      IF(PARADA(4,J_ATH).EQ.1.) THEN
        TH=PARADA(2,J_ATH)
      ELSE
        TH=0.
      END IF
      IF(PARADA(4,J_ATH).EQ.1.) THEN
        TV=PARADA(2,J_ATV)
      ELSE
        TV=0.
      END IF
      DO K=NUM1,NUM2
        IF(FCUHDT) THEN
          CALL=DVTPNT(K,NTRK)
          IF(FNOHDT(NTRK)) GO TO 300
        END IF
        IF(FUSTE) THEN
          TE=DVTP(IVTEDV,K)
          IF(TE.LT.TC1.OR.TE.GT.TC2) GO TO 300
        END IF
        IF(FUSFI) THEN
          FI=DFINXT(FIMID,DVTP(IVFIDV,K))
          IF(FI.LT.FC1.OR.FI.GT.FC2) GO TO 300
        END IF
        IF(FVTPDC) CALL DGLEVL(NCTPDC(K))
        T=DVTP(IVTEDV,K)-TEMID
        X=SH*DVTP(NH,K)
        Y=SV*DVTP(NV,K)
        H= X*CA+Y*SA+TH*T
        V=-X*SA+Y*CA+TV*T
        IF(FPIKDP) THEN
          CALL DQPIK(H,V)
        ELSE
          CALL DQPD(H,V)
        END IF
  300 END DO
    9 CALL DQFR(IAREDO)
      CALL DPCSAR
      END
*DK DAREA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DAREA
CH
      SUBROUTINE DAREA(A1,A,N1,N2,NAR,NYES)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:  RETURN NUMBER OF WINDOW
C    Inputs    :A=operator input must start with letter A1
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) A
      CHARACTER *1 A1
      DATA NOF/1/,NLET/6/,DVIRT/35./
      DIMENSION IWCH(0:12)
C               W 1 2 3 4 5 6 U D  L  M  R  S
C               W 3 4 5 6 2 1 D U  M  R  S  L
C               0 1 2 3 4 5 6 7 8  9 10 11 12
      DATA IWCH/0,3,4,5,6,2,1,8,7,10,11,12, 9/
      CHARACTER *1 T789(7:9)
      DATA T789/'7','8','9'/
      LOGICAL NYES
      NYES=.FALSE.
      IF(A1.EQ.'D'.AND.A.EQ.'C*') A='D*'
      IF(A.EQ.A1//'*') THEN
        CALL DGCURG(HW,VW)
        IF(HW.LE.0.OR.HW.GT.HHGHDG(13).OR.
     &     VW.LE.0.OR.VW.GT.VHGHDG(13)) THEN
          CALL DWRT('Set pointer to center of window to be selected.#')
          RETURN
        END IF
        IF(     VW.GT.VMINDG(13)) THEN
C         ............................................. virtual windows 7,8,9
          IF(A1.EQ.'C'.OR.A1.EQ.'G') THEN
            DH=(HHGHDG(13)-HMINDG(13))/3.
            HH=HMINDG(13)
            DO N=7,9
              HH=HH+DH
              IF(HW.LT.HH) THEN
                A=A1//T789(N)
                GO TO 5
              END IF
            END DO
C           ....... nyes = false is right, to go in doper into the right loop.
          END IF
          RETURN
        ELSE IF(VW.GT.VHGHDG(0)-DVIRT) THEN
          NAR=0
        ELSE 
          DMIN=9999999.
          DO K=1,12
            HC=0.5*(HMINDG(K)+HHGHDG(K))
            VC=0.5*(VMINDG(K)+VHGHDG(K))
            D=(HC-HW)**2+(VC-VW)**2
            IF(D.LT.DMIN) THEN
              NAR=K
              DMIN=D
            END IF
          END DO
        END IF
        NYES=.TRUE.
        A(2:2)=TWINDW(NAR)
    5   CALL DWR_BACKSPACE_SLASH(NLET)
        CALL DWR_ADD(A)
        RETURN
      END IF
      IF(A1.EQ.'W'.AND.NOF.LE.0) RETURN
      IF(A1.EQ.'W') THEN
        IF(     A.EQ.'W+'.OR.A.EQ.'w+') THEN
          IAREDO=IAREDO+1
          IF(IAREDO.GT.12) IAREDO=0
          NYES=.TRUE.
          RETURN
        ELSE IF(A.EQ.'W-'.OR.A.EQ.'w-') THEN
          IAREDO=IAREDO-1
          IF(IAREDO.LT.0) IAREDO=12
          NYES=.TRUE.
          RETURN
        ELSE IF(A.EQ.'W%'.OR.A.EQ.'w%') THEN
          IAREDO=IWCH(IAREDO)
          NYES=.TRUE.
          RETURN
        END IF
      END IF
      IF(A(1:1).NE.A1) RETURN
      DO N=N1,N2
        IF(A(2:2).EQ.TWINDW(N)) THEN
          NAR=N
          NYES=.TRUE.
          RETURN
        END IF
      END DO
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DAREAS
CH
      ENTRY DAREAW(IWOF)
CH
CH --------------------------------------------------------------------
CH
C     Switch off on window selection in DOPER.
      NOF=IWOF
      END
*DK DASHYP
CH..............***
CH
CH
CH
CH
CH
CH
CH ********************************************************************  DASHYP
CH
      FUNCTION DASHYP(A)
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
      DASHYP=ALOG(A+SQRT(A**2+1))
      RETURN
      END
