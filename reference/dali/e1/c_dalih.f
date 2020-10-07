*DK DHT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DHT
CH
      SUBROUTINE DHT
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
C INVOKED BY TANSW.EQ.'DD'  (DHT)
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *2 TANSW
      CHARACTER *2 DT2
      CHARACTER *4 DT4
      CHARACTER *1 TSLA(-1:1)
      DATA TSLA/'/','=',':'/
      CHARACTER *2 TDET(6),TTR(6)
      DATA TDET/'VD','IT','TP','EC','HC','MD'/
      DATA TTR/'CH','JU','MC','KI','KO','KH'/
      CHARACTER *2 TFRTO(9),TEFEO(6)
      DATA TFRTO/'CC','VX','IT','T0','T3',
     &           'E1','E3','H2','M2'/
      DATA TEFEO/'N1','N2','N3','S1','S2','S3'/
      CHARACTER *32 TEFOB(2)
      DATA TEFOB/'     ECAL-objects drawn between ',
     &           'Neutral EF tracks drawn between '/
      CHARACTER *16 TEFRG(6)
      DATA TEFRG/'0 and E0',
     &           '0 and E3',
     &           '0 and E0 or H0',
     &           'TPC and E0',
     &           'TPC and E3',
     &           'TPC and E0 or H0'/
      CHARACTER *1 TAUTF(4)
      DATA TAUTF/'A','U','T','F'/
      DIMENSION PSTO(0:MPNWDW),NCUT(30),NUBA(4)
      CHARACTER *2 TVDH(0:3),TVDC(0:3)
      DATA TVDH/'VN','VH','VU','VC'/,TVDC/'V0','VG','VA','V2'/
      CHARACTER *11 TVHT(0:3)
      CHARACTER *4 TVCT(0:3)
C
C     VDXY,VDZT IHDHDO 0=NOT   1=ALL  2=NO TRACK ASSIGNMENT  3=ASSIGNED
C                       VN      VH        VU                   VC
C     VDCO      IHVCDO 0=NOT   1=3D     2=ALL  3=2D
C                       V0      VG       VA     V2
C                 ( 3D=A 2D,GOOD=R 2D EXTR=X)
C
      DATA TVHT/'         no','        all','unconnected','  connected'/
      DATA TVCT/'  no','good',' all','  2D'/
      CHARACTER *49 TV
C              123456789 123456789 123456789 123456789 123456789
      DATA TV/'Vertex det.: unconnected hits, good coordinates.'/
      LOGICAL CHG,FYES,FACT,FTV
      DATA FTV/.TRUE./
      DATA P4/1./
      CHARACTER *49 T0,T1,T2,T3,T4,T5,TX1,TX2,TX3,TX4,TX5
      DATA TX1/'VD IT TP EC HC MD    PI CA CT    DC RC SC'/
      DATA TX2/'HI HT TR MO CH JU MC AC UC TC FC UF    CD PA BC'/
      DATA TX3/'FR TO CC VX IT T0 T3 E1 E3 H2 M2    AT VT LI'/ 
      DATA TX4/'VD VN VH VU VC V0 VG VA V2'/
C     DATA TX4/'V? VD VN VH VU VC V0 VG VA V2'/
      DATA TX5/'NE EO N0 N3 S0 S3    D*    DT'/
C                       1         2         3         4         5
C              12345678901234567890123456789012345678901234567890
      DATA T1/'P?:W?: Hits(CD,AC)+Tracks(JU) FR=VX To=E3 TB_1,  '/
      DATA T4/'P?:W?: No hits and charged tracks. (Restore:"HT")'/  
      DATA T2/' active cuts: no '/
      DATA T3/'    Cuts on tracks Not imposed on hits'/
      DATA T5/'12 V0-tracks are drawn (YLV0). YO=on, YF=of'/ 
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_HIN,J_HV0,J_HLI,J_HTB,J_HCA,J_HEO,J_HVD,J_HSE'
      CALL DPARAM(20
     &  ,J_HIN,J_HV0,J_HLI,J_HTB,J_HCA,J_HEO,J_HVD,J_HSE)
      TPARDA=
     &  'J_HNI,J_HSH,J_HDE,J_HCL'
      CALL DPARAM(21
     &  ,J_HNI,J_HSH,J_HDE,J_HCL)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DPARGG(21,N20_1,N20_2)
      CALL DPARP0
      CALL DPAROP(20,'HTB_HP0_HSE_HNI_HNT_HCM_HZ0_HD0_HF1_HF2_HT1_HT2')
      CALL DPAROP(20,'HZZ_HST_HSH_HDE_HCN_HCQ_HCE_HCL')
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DCOPTL
  930 CALL DHTINP
      IF(IHTRDO(1).LT.6) THEN
        T0=T1
        IF(     IHTRDO(1).EQ.1) THEN
          T0( 9: 9)='I'
          T0(19:49)=' '
        ELSE IF(IHTRDO(1).EQ.3) THEN
          T0(21:21)='R'
          T0( 7:19)=' '
        END IF
        IF(IHTRDO(1).LE.2) THEN
          T0(16:16)=TAUTF(IHTRDO(5))
          IF(IHTRDO(6).EQ.2) THEN
            T0(13:14)='PA'
            T0(16:17)=DT2(PARADA(2,J_HSE))
          END IF
          IF(IHTRDO(6).EQ.3) THEN
            T0(13:14)='BC'
          END IF
        END IF
        IF(IHTRDO(1).GE.2.AND.IHTRDO(1).LT.6) THEN
          IF(IHTRDO(2).EQ.1.OR.
     &       IHTRDO(2).GE.4) THEN
            T0(27:28)=TTR(IHTRDO(2))
            T0(31:49)=' '
          ELSE
            IF(IHTRDO(2).EQ.3) THEN
              T0(27:28)='MC'
              IF(PARADA(4,J_HLI).EQ.1.) THEN
                ILIM=PARADA(2,J_HLI)
                T0(29:49)=',AT)  LI='//TFRTO(ILIM)
              ELSE
                T0(29:49)=',VT)'
              END IF
            ELSE
              T0(34:35)=TFRTO(IHTRDO(3))
            END IF
            T0(40:41)=TFRTO(IHTRDO(4))
          END IF
          IF(IHTRDO(1).LE.3.AND.IHTRDO(2).EQ.2) THEN
            CALL DVLBKN('FRFT',4,NUBA,NUMB)
            IF(     NUMB.EQ.0) THEN
              T0(43:49)=' '
            ELSE IF(NUMB.EQ.1) THEN
              T0(47:49)=' '
              PARADA(2,J_HTB)=NUBA(1)
            ELSE
              NTB=PARADA(2,J_HTB)
              LTB=NUBA(2)
              DO K=1,NUMB
                IF(NTB.EQ.NUBA(K)) THEN
                  IF(K.EQ.2) LTB=NUBA(1)
                  GO TO 933
                END IF
              END DO
              NTB=NUBA(1)
              PARADA(2,J_HTB)=NTB
  933         IF(NUMB.EQ.2) THEN
                WRITE(T0(45:49),1933,ERR=1934) NTB,LTB
 1933           FORMAT('=',I1,',',I2)
              ELSE
                T0(49:49)='#'
              END IF
            END IF
          END IF
        END IF
 1934   CALL DTYPT('TYPE',TPICDO,N_1_DA,N_2_DA,PARADA,TPOPDA,T0)
      ELSE
        CALL DTYPT('TYPE',TPICDO,0,0,0,' ',T4)
      END IF
      IF(PARADA(2,J_HV0).NE.0.) THEN
        CALL DVKN_V0_TR_0(NV0)
        CALL DTINT(NV0,1,2,T5)
        CALL DTYPT('TYPE',' '   ,0,0,0,' ',T5)
      END IF
      IF(PARADA(4,J_HEO).GT.0.) THEN
        NEO=PARADA(2,J_HEO)
        IEO=PARADA(4,J_HEO)
        TXTADW=TEFOB(IEO)//TEFRG(NEO)
        CALL DWRC
      END IF
      FACT=.FALSE.
      NT=7
      T2(14:49)=' '
      DO N=N20_1,N20_2
        IF(TPOPDA(N).NE.'  '.AND.PARADA(4,N).GE.P4) THEN
          FACT=.TRUE.
          NT=NT+8
          IF(NT.GT.40) THEN
            CALL DWRT(T2)
            NT=7
            T2(14:49)=' '
          END IF
          NSLA=PARADA(4,N)
          T2(  NT  :NT+1)=TPOPDA(N)
          T2(  NT+2:NT+2)=TSLA(NSLA)
          IF(N.EQ.J_HDE) THEN
            NDEV=PARADA(2,J_HDE)
            T2(NT+3:NT+4)=TDET(NDEV)
          ELSE
            T2(NT+3:NT+6)=DT4(PARADA(2,N))
          END IF
        END IF
      END DO
      P4=1.
      IF(NT.NE.7) CALL DWRT(T2)
      IF(FACT.AND.IHTRDO(1).LE.2) THEN
        IF(PARADA(2,J_HCA).NE.0.) THEN
          T3(20:22)=' '
        ELSE
          T3(20:22)='not'
        END IF
        CALL DWRT(T3)
      END IF
      IF(FTV) THEN
  935   TV(14:24)=TVHT(IFIX(PARADA(2,J_HVD)))
        TV(32:35)=TVCT(IFIX(PARADA(4,J_HVD)))
        CALL DWRT(TV)
      END IF
  936 CALL DGZOOM(6,IAREDO,0,0)
      CALL DOPER(1,0,
     &  N_1_DA,N_2_DA,TPOPDA,PARADA,
     &  1,0,' ',0,
     &  NEXEC,CHG,TANSW)
      CALL DGZOOM(6,-1,0,0)
C      PARADO(2,IHCODO)=SIGN(PARADO(2,IHCODO),PCOL)
      IF(TANSW.EQ.'PI'.AND.NEXEC.EQ.2)
     1  CALL DPOS(TANSW,PFI,PTE,NEXEC,CHG)
      GO TO(910,920,930,940),NEXEC
  910 IF(TANSW.NE.'GW') THEN
        CALL DPACGT(20,PLSTDA)
        CALL DCOPFL
      END IF
      NFRTO=0
      CALL DSC0
      RETURN
  920 CALL DHTMOD(TANSW,FYES)
      IF(FYES) GO TO 930
      IF(TANSW.EQ.'CA') THEN
        PARADA(2,J_HCA)=1.
        GO TO 930
      END IF
      IF(TANSW.EQ.'CT') THEN
        PARADA(2,J_HCA)=0.
        GO TO 930
      END IF
      IF(PARADA(2,J_HSH).NE.0..AND.PARADA(4,J_HSH).EQ.1.) THEN
         DO 740 K=1,6
            IF(TANSW.EQ.TDET(K)) THEN
               PARADA(2,J_HDE)=K
               NFRTO=0
               GO TO 930
            END IF
  740    CONTINUE
      END IF
      IF(IHTRDO(1).GE.2.AND.IHTRDO(1).LT.6.AND.IHTRDO(2).GE.2) THEN
         IF(IHTRDO(2).EQ.2) THEN
            IF(TANSW.EQ.'FR') THEN
               NFRTO=3
               GO TO 936
            END IF
         ELSE
            IF(TANSW.EQ.'VT') THEN
               PARADA(4,J_HLI)=-1.
               GO TO 930
            END IF
            IF(TANSW.EQ.'AT') THEN
               PARADA(4,J_HLI)=1.
               GO TO 930
            END IF
            IF(TANSW.EQ.'LI') THEN
               NFRTO=8
               GO TO 936
            END IF
         END IF
         IF(TANSW.EQ.'TO') THEN
            NFRTO=4
            GO TO 936
         END IF
      END IF
      IF(NFRTO.NE.0) THEN
         DO 750 K=1,9
            IF(TANSW.EQ.TFRTO(K)) THEN
               IF(NFRTO.EQ.8) THEN
                  PARADA(2,J_HLI)=K
               ELSE
                  IHTRDO(NFRTO)=K
                  CALL DHTMO2
               END IF
               GO TO 930
            END IF
  750    CONTINUE
      END IF
      IF(TANSW.EQ.'DC') THEN
        KCUT=0
        DO N=J_HNI,J_HCL
          IF(PARADA(4,N).EQ.1.) THEN
            PARADA(4,N)=-1.
            KCUT=KCUT+1
            NCUT(KCUT)=N
          END IF
        END DO
        GO TO 930
      END IF
      IF(TANSW.EQ.'RC'.AND.KCUT.GT.0) THEN
        DO K=1,KCUT
          PARADA(4,NCUT(K))=1.
        END DO
        GO TO 930
      END IF
      IF(TANSW.EQ.'NE') THEN
        PARADA(4,J_HEO)=-ABS(PARADA(4,J_HEO))
        GO TO 930
      END IF
      IF(TANSW.EQ.'EO') THEN
        PARADA(4,J_HEO)=1.
        GO TO 930
      END IF
      IF(TANSW.EQ.'EF') THEN
        PARADA(4,J_HEO)=2.
        GO TO 930
      END IF
      CALL DO_STR('N0"N1"N2"S0"S1"S2": range of EF, EO')
      DO K=1,6
        IF(TANSW.EQ.TEFEO(K)) THEN
          PARADA(2,J_HEO)=K
          PARADA(4,J_HEO)=ABS(PARADA(4,J_HEO))
          GO TO 930
        END IF
      END DO
      CALL DAREA('D',TANSW,0,12,IAREDO,FYES)
      IF(FYES) GO TO 940
      IF(TANSW.EQ.'DT') THEN
         PIN=PARADA(2,J_HIN)
         IN=PIN
         IN=IN-MOD(IN,10)
         DO 760 M=0,MPNWDW
            PSTO(M)=PSTODS(1,J_HIN,M,IWUSDO)
            PSTODS(1,J_HIN,M,IWUSDO)=IN
  760    CONTINUE
         NOCLDT=1
         CALL DPCEAR(IAREDO)
         PARADA(2,J_HIN)=PIN
         DO 770 M=0,MPNWDW
            PSTODS(1,J_HIN,M,IWUSDO)=PSTO(M)
  770    CONTINUE
         GO TO 936
      END IF
      IF(TANSW.EQ.'SC') THEN
         P4=-1.
         GO TO 930
      END IF
      IF(TANSW.EQ.'VD') THEN
         FTV=.FALSE.
         PARADA(2,J_HVD)=2.
         PARADA(4,J_HVD)=1.
         GO TO 935
      END IF
C     IF(TANSW.EQ.'V?') THEN
C        FTV=.TRUE.
C        GO TO 935
C     END IF
      DO K=0,3
        IF(TANSW.EQ.TVDH(K)) THEN
          PARADA(2,J_HVD)=K
          FTV=.TRUE.
          GO TO 935
        END IF
      END DO
      DO K=0,3
        IF(TANSW.EQ.TVDC(K)) THEN
          PARADA(4,J_HVD)=K
          FTV=.TRUE.
          GO TO 935
        END IF
      END DO
      CALL DO_STR('YO: switch bank YLV0 on')
      IF(TANSW.EQ.'YO') THEN
        PARADA(2,J_HV0)=1.
        GO TO 930
      END IF
      IF(TANSW.EQ.'YF') THEN
        PARADA(2,J_HV0)=0.
        GO TO 930
      END IF
      IF(TANSW.EQ.'X?') THEN
        CALL DWRT(TX1)
        CALL DWRT(TX2)
        CALL DWRT(TX3)
        CALL DWRT(TX4)
        CALL DWRT(TX5)
        GO TO 936
      END IF
      CALL DWR_IC(TANSW)
      GO TO 936
  940 CALL DSC0
      NAR=IAREDO
      DO 780 M=0,MPNWDW
        IF(NWINDW(M,NAR).EQ.-2.OR.NWINDW(M,NAR).EQ.1) THEN
          CALL DPACGT(20,PSTODS(1,1,M,IWUSDO))
        END IF
  780 CONTINUE
      CALL DPCEAR(NAR)
      IAREDO=NAR
      CALL DHTTYP(FYES)
      IF(FYES) GO TO 936
      IF(CHG) GO TO 930
      GO TO 936
      END
*DK DHTTYP
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DHTTYP
CH
      SUBROUTINE DHTTYP(FTYPE)
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
      LOGICAL FTYPE
      DIMENSION PTR(3),CLTM(3),VTX1(3)
      CHARACTER *59 T
      CHARACTER *3 DT3
      CHARACTER *4 DT4
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_HST'
      CALL DPARAM(20
     &  ,J_HST)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(IHTRDO(2).EQ.3.AND.PARADA(4,J_HST).EQ.1..AND.
     &  PARADA(2,J_HST).GT.0.) THEN
         NST=PARADA(2,J_HST)
         CALL DVMCNV(NVTX,NDUM)
         DO 700 N=1,NVTX
            CALL DVMCVX(N,KT1,KT2,KIN,VTX1)
            IF(KT1.LE.NST.AND.NST.LE.KT2) GO TO 10
  700    CONTINUE
         RETURN
   10    CALL DVMCTR(NST,PTR,CLTM,ITYPE,NVTX2)
         PT=PTR(1)**2+PTR(2)**2
         PTOT=SQRT(PT+PTR(3)**2)
         PT=SQRT(PT)
         IF(PT.GT.0.) THEN
            FI=DATN2D(PTR(2),PTR(1))
         ELSE
            FI=0.
         END IF
         IF(PTOT.GT.0.) THEN
            TE=DATN2D(PT,PTR(3))
         ELSE
            TE=0.
         END IF
         IF(CLTM(2).GT.0.) THEN
            CLT=ALOG10(CLTM(2))
         ELSE
            CLT=0.
         END IF
C                    1         2         3         4         5
C           12345678901234567890123456789012345678901234567890123456789
         T='mc-t.123 ty=123 p=+.123 fi=123 te=123 lt=-12 v1=123 v2=123:'
         IF(ITYPE.LE.0.OR.ITYPE.GT.47) THEN
            T(13:15)=DT3(FLOAT(ITYPE))
         ELSE
            CALL DVPART(ITYPE,6,T(10:15))
         END IF
         T( 6: 9)=DT3(PARADA(2,J_HST))
         T(20:23)=DT4(PTOT)
         T(28:30)=DT3(FI)
         T(35:37)=DT3(TE)
         T(42:44)=DT3(CLT)
         T(49:51)=DT3(FLOAT(N))
         T(56:58)=DT3(FLOAT(NVTX2))
         IF(CLTM(1).EQ.0.) THEN
            T(19:19)=' '
         ELSE IF(CLTM(1).EQ.-1.) THEN
            T(19:19)='-'
         ELSE IF(CLTM(1).EQ.1.) THEN
            T(19:19)='+'
         ELSE
            T(19:19)='*'
         END IF
         CALL DWRT(T)
         FTYPE=.TRUE.
      ELSE
         FTYPE=.FALSE.
      END IF
      END
