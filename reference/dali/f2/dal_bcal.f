*DK DKBC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DKBC
CH
      SUBROUTINE DKBC
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *2 TANSW
      PARAMETER (MV=4)
      CHARACTER *2 TSID(0:8),TPRO(MV)
C                  0    1    2    3    4    5    6    7    8
      DATA TSID /'SU','AB','BA','AA','BB','AL','BL','AR','BR'/
      DATA TPRO/'XY','FR','LL','LD'/
      LOGICAL FCHG
      CHARACTER *49 TP38,TP102,T1,T2
      DATA TP38/'BDR_BCE'/
      DATA TP102/'BBC_BLI_BLW_BHI_BOV_BTX_BCC_BWC_BSC_BTR'/
C              123456789 123456789 123456789 123456789 123456789
      DATA T1/'P?:W?:BCAL rz AB  DR$123 CE$1234  TR$12  TX$12'/
      DATA T2/'BC_12 LI_12 LW_1  HI_12  CC$12 WC_1 SC_12'/
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DPARP0
      CALL DPAROP(38,TP38)
      CALL DPAROP(102,TP102)
      TPARDA=
     &  'J_BPR,J_BSI,J_BDR'
      CALL DPARAM(38
     &  ,J_BPR,J_BSI,J_BDR)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
  930 CALL DO_BAR_STATUS_0

      NPR=PARADA(2,J_BPR)
      CALL DO_BAR_STATUS(TPRO(NPR),12,T1)

      NSID=PARADA(2,J_BSI)
      CALL DO_BAR_STATUS(TSID(NSID),15,T1)

      CALL DTYPT('TYPE',TPICDO,N_1_DA,N_2_DA,PARADA,TPOPDA,T1)
      CALL DTYPT('TYPE',  ' ' ,N_1_DA,N_2_DA,PARADA,TPOPDA,T2)
  936 FCHG=.FALSE.
      CALL DGZOOM(6,IAREDO,0,0)
      CALL DOPER(1,1,
     &  N_1_DA,N_2_DA,TPOPDA,PARADA,
     &  1,0,' ',0,
     &  NEXEC,FCHG,TANSW)
      CALL DGZOOM(6,-1,0,0)
      GO TO (910,920,930,940),NEXEC
  910 RETURN
  920 CALL DO_STR_LIST(MV,TPRO,'projection')
      DO N=1,MV
        IF(TANSW.EQ.TPRO(N)) THEN
          PARADA(2,J_BPR)=N
          GO TO 930
        END IF
      END DO
      CALL DO_STR_LIST(9,TSID,' sides')
      DO N=0,8
        IF(TANSW.EQ.TSID(N)) THEN
          PARADA(2,J_BSI)=N
          GO TO 930
        END IF
      END DO
      CALL DO_STR('TY: type hits')
      IF(TANSW.EQ.'TY') THEN
        CALL DBC_TYPE(0)
        GO TO 936
      END IF
      IF(TANSW.EQ.'X?') THEN
        CALL DO_TY_COMMAND_LIST('DKBC')
        GO TO 936
      END IF
      CALL DWR_IC(TANSW)
      GO TO 936
  940 CALL DBCD
      GO TO 930
      END
*DK DBCD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DBCD
CH
      SUBROUTINE DBCD
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'

      CHARACTER *4 DT4
      DIMENSION NDL(0:8),NDR(0:8),H(30),V(30),HM(3),VM(3)
      DIMENSION ETOT(-2:2),NTRG(-2:2),FC(-2:2),RC(-2:2)

C             SU AB BA AA BB AL BL AR BR    -:-Z +:+Z 1=LEFT 2=RIGHT
C                -+ +- -- ++ -  +  -  +
C                12 12 12 12 1  1  2  2
C                            4  2  3  1



                           

      DATA NDL/0,-1, 1,-1, 1,-1, 1, 0, 0/
      DATA NDR/0, 2,-2,-2, 2, 0, 0,-2, 2/

      DATA F1/-33.75/,F2/33.75/,DF/11.25/,DFC/1.125/
      DATA QS/0.001/,FD/2./,RR1/6.43/,D1/1./
      DATA DHT0,DHTL,DHTR/10.,50.,10./,DHTL5/70./,DHTR5/20./
      DATA DHFR/40./,DVFR/14./
      DATA NSY/3/,DSY/9./
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_BPR,J_BSI,J_BDR,J_BSE,J_BCE'
      CALL DPARAM(38
     &  ,J_BPR,J_BSI,J_BDR,J_BSE,J_BCE)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      NPR=PARADA(2,J_BPR)
      IF(NPR.GE.3) THEN
        CALL DBC_TYPE(NPR)
        RETURN
      END IF
      NSI=PARADA(2,J_BSI)
      CALL DV_BCAL_0(NUM,ETOT,EMAX)
      IF(NUM.EQ.0) RETURN
      CALL DBC_DETECTOR(NPR)
      CALL DPARGI(102,'BHI',LHI)
      CALL DPARGI(102,'BOV',LOV)
      CALL DPARGV(102,'BLW',2,DLINDD)
      CALL DPARGV(102,'BWC',2,BWC)
      CALL DPARGV(102,'BSC',2,BSC)
C      SCE=PARADA(2,J_BSE)*QS
      SCE=1./EMAX
      IF(PARADA(4,J_BCE).GT.0.) THEN
        CE=PARADA(2,J_BCE)
      ELSE
        CE=0.
      END IF
      CALL DV_BCAL_CENTROID(FC,RC)
      CALL DQPD0(NSY,BSC,0.)
      IF(NPR.EQ.1) THEN
C       ............................................................. YX
        IF(PARADA(4,J_BDR).GT.0..AND.NSI.LE.4) THEN
          DR=PARADA(2,J_BDR)
        ELSE
          DR=0.
        END IF
        NL=NDL(NSI)
        NR=NDR(NSI)
        DF2=0.5*DF
        JL=0
        JR=0
        DO K=1,NUM
          CALL DV_BCAL(K,MD,FM,R1,R2,E)          ! MD = modul AL,BL,AR,BR
          IF(E.GE.CE) THEN
            IF(NSI.EQ.0.OR.MD.EQ.NL.OR.MD.EQ.NR) THEN
              IF(MD.EQ.NL) JL=MD
              IF(MD.EQ.NR) JR=MD
              IF(ABS(MD).EQ.1) THEN
C               ............................................. LEFT
                D= DR
              ELSE
                D=-DR
              END IF
C             ...................... c*E = (R2-R1)*RM*QF = (R2**2-R1**2)*QF*0.5
C             QF=SCE*E/(R2*R2-R1*R1)
              QF=SCE*E*DF2
C             ....................... the area may be too small, therefore:
              CALL DGLEVL(LOV)
              CF=COSD(FM)
              SF=SIND(FM)
              CALL DQL2E(R1*CF+D,R1*SF,R2*CF+D,R2*SF)
              F1=FM-QF
              F2=FM+QF
              NFC=2+(F2-F1)/DFC
              DD=(F2-F1)/(NFC-1)
              NF2=2*NFC
              NP=NF2+1
              FF=F1
              DO NF1=1,NFC
                SF=SIND(FF)
                CF=COSD(FF)
                H(NF1)=CF*R1+D
                V(NF1)=SF*R1
                H(NF2)=CF*R2+D
                V(NF2)=SF*R2
                NF2=NF2-1
                FF=FF+DD
              END DO
              H(NP)=H(1)
              V(NP)=V(1)
              CALL DQPO0('AREA',LHI,0,' ')
              CALL DQPOL(NP,H,V)
            END IF
          END IF
        END DO
        CALL DPAR_SET_CO(102,'BCC')
        IF(ICOLDP.GE.0) THEN
          IF(NSI.EQ.0) THEN
            DO I=-2,2
              IF(RC(I).GT.0..AND.ETOT(I).GT.0.) THEN
                IF(ABS(I).EQ.1) THEN
C                 ............................................. LEFT
                  D= DR
                ELSE
                  D=-DR
                END IF
                HC=COSD(FC(I))*RC(I)+D
                VC=SIND(FC(I))*RC(I)
                CALL DBC_CROSS(HC,VC,BWC,LHI,ICOLDP)
              END IF
            END DO
          ELSE
            IF(NL.NE.0.AND.RC(NL).GT.0..AND.ETOT(NL).GT.0.) THEN
              HC=COSD(FC(NL))*RC(NL)+DR
              VC=SIND(FC(NL))*RC(NL)
              CALL DBC_CROSS(HC,VC,BWC,LHI,ICOLDP)
            END IF
            IF(NR.NE.0.AND.RC(NR).GT.0..AND.ETOT(NR).GT.0.) THEN
              HC=COSD(FC(NR))*RC(NR)-DR
              VC=SIND(FC(NR))*RC(NR)
              CALL DBC_CROSS(HC,VC,BWC,LHI,ICOLDP)
            END IF
          END IF
        END IF
        HT=0.5*(HLOWDG(IAREDO)+HHGHDG(IAREDO))
        IF(NSI.EQ.0) THEN
          CALL DBC_TEXT( 1,HT-DHTL,ETOT, 0.)
          CALL DBC_TEXT(-1,HT-DHTL,ETOT,12.)
          CALL DBC_TEXT( 2,HT-DHTR,ETOT, 0.)
          CALL DBC_TEXT(-2,HT-DHTR,ETOT,12.)
        ELSE IF(NSI.LE.4) THEN
          CALL DBC_TEXT(JL,HT-DHTL,ETOT,0.)
          CALL DBC_TEXT(JR,HT+DHTR,ETOT,0.)
        ELSE
          CALL DBC_TEXT(JL,HHGHDG(IAREDO)-DHTL5,ETOT,0.)
          CALL DBC_TEXT(JR,HLOWDG(IAREDO)+DHTR5,ETOT,0.)
        END IF
      ELSE IF(NPR.EQ.2) THEN
C       ............................................................. FR
        IF(     NSI.LE.4) THEN
          RETURN
        ELSE IF(NSI.LE.6) THEN
          S=-1.
        ELSE
          S= 1.
        END IF
        NL=NDL(NSI)
        NR=NDR(NSI)
        JD=0
        DF2=0.5*DF
        DO K=1,NUM
          CALL DV_BCAL(K,MD,FM,R1,R2,E)          ! MD = modul AL,BL,AR,BR
          IF(MD.EQ.NL.OR.MD.EQ.NR) THEN
            IF(E.GE.CE) THEN
C             ...................... c*E = (R2-R1)*RM*QF = (R2**2-R1**2)*QF*0.5
              JD=MD
C             QF=SCE*E/(R2*R2-R1*R1)
              QF=SCE*E
              FF=QF*DF2
              H(1)=S*R1
              V(1)=FM-FF
              H(3)=S*R2
              V(3)=FM+FF
              CALL DQ245(H,V)
              CALL DGLEVL(LOV)
              CALL DQL2E(H(1),FM,H(2),FM)
              CALL DQPO0('AREA',LHI,0,' ')
              CALL DQPOL(5,H,V)
            END IF
          END IF
        END DO
        CALL DPAR_SET_CO(102,'BCC')
        IF(ICOLDP.GE.0) THEN
          DLINDD=BWC
          IF(RC(JD).GT.0..AND.ETOT(JD).GT.0.) THEN
            HC=RC(JD)
            VC=FC(JD)
            CALL DBC_CROSS(HC,VC,BWC,LH,ICOLDP)
          END IF
        END IF
        CALL DBC_TEXT(JD,HLOWDG(IAREDO)+DHFR,ETOT,0.)
      END IF
      END
*DK DBC_CROSS
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++ DBC_CROSS
CH
      SUBROUTINE DBC_CROSS(HC,VC,BWC,LH,ICOL)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      DATA D2/2./
      DLINDD=BWC+D2
      CALL DGLEVL(LH)
      CALL DQPD(HC,VC)
      DLINDD=BWC
      CALL DGLEVL(ICOL)
      CALL DQPD(HC,VC)
      END
*DK DBC_DETECTOR
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++ DBC_DETECTOR
CH
      SUBROUTINE DBC_DETECTOR(NPR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      DIMENSION HRB(4),VRB(4)
      DATA DS/.17/,FSIDE/35./,DRS/6.2/,RSIDE/4.2/
      DIMENSION RR(17)
      DATA RR/6.43,6.555,
     &  6.68,6.93,7.18,7.43,7.68,7.93,8.18,8.43,
     &  8.68,8.93,9.18,9.43,9.68,9.93,10.18/

      DATA Z1/753.8/,Z2/773.8/,RCR/50./
      DATA F1/-33.75/,F2/33.75/,DF/11.25/,NF/7/,NFC/61/,DFC/1.125/
      DIMENSION NZ(0:8)

      CHARACTER *1 TSL(0:8),TSR(0:8)
      DATA TSL/' ','A','B','A','B','A','B',' ',' '/
      DATA TSR/' ','B','A','A','B',' ',' ','A','B'/

      DATA DVT/14./,DHL/2./,DHR/15./,LT/8/,DVH/10./
      DATA NSY/3/,DSY/9./,WSY/2./
      DIMENSION H(123),V(123),FC(-2:2),RC(-2:2),ETOT(-2:2)
      IF(FPIKDP) RETURN
      CALL DPARGI_24(102,'BBC',LA,LA4)
      IF(LA4.LT.0) RETURN
      CALL DPARGI(102,'BLI',LF)
      CALL DPARGV(102,'BLW',2,DLINDD)
      IF(NPR.EQ.0) THEN
C       ..................................... detector in RZ
        CALL DV_BCAL_0(NUM,ETOT,EMAX)
        IF(NUM.EQ.0) RETURN
        DO SZ=-1.,1.,2.
          H(1)=SZ*Z1
          H(3)=SZ*Z2
          NRZ=SZ
          DO SR=-1.,1.,2.
            CALL DQPO0('AR+L',LA,LF,' ')
            V(1)=SR*RR(1)
            V(3)=SR*RR(17)
            CALL DQ245(H,V)
            CALL DQPOL(5,H,V)
            NRZ=2.*SZ
          END DO
        END DO
        CALL DPAR_SET_CO(102,'BTR')
        IF(ICOLDP.GE.0) THEN
          CALL DPARGV(87,'STR',2,DLINDD)
          CALL DPARGV(11,'PFI',2,FIMID)
          FMIN=FIMID-90.
          FMAX=FIMID+90.
          CALL DV_BCAL_CENTROID(FC,RC)
          DO I=-2,2
            IF(RC(I).GT.0..AND.ETOT(I).GT.0.) THEN
              IF(I.LT.0) THEN
                HC=-Z1
              ELSE
                HC= Z1
              END IF
              FI=DFINXT(FIMID,FC(I))
              IF(FI.GE.FMIN.AND.FI.LE.FMAX) THEN
                VC= RC(I)
              ELSE
                VC=-RC(I)
              END IF
              CALL DQL2E(0.,0.,HC,VC)
            END IF
          END DO
        END IF
        RETURN
      END IF
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_BSI,J_BDR'
      CALL DPARAM(38
     &  ,J_BSI,J_BDR)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DQCL(IAREDO)
      CALL DQWIL(MOD(DFWIDU(IZOMDO),10.))

      IF(NPR.EQ.1) THEN
C       ............................................................... XY
        NSI=PARADA(2,J_BSI)
        VV=RR(17)*SIND(F2)+DS
        HL=RR( 1)*COSD(F2)-DS
        HH=RR(17)+DS
        IF(NSI.LE.4) THEN
C         ....................................... left and right modul 
          IF(PARADA(4,J_BDR).LE.0.) THEN
            DR=0.
          ELSE
            DR=PARADA(2,J_BDR)
            HH=HH-DR
          END IF
          H1=-HH
          H2= HH
          V1=-VV
          V2= VV
          L2=2
          FA=F1
          FL=F1
          FR=F1
          D=DR
        ELSE
          IF(NSI.LE.6) THEN
C           ....................................... left side
            H1=-HH
            H2=-HL
            FA=F1+180.
          ELSE
            H1= HL
            H2= HH
            FA=F1
          END IF
          V1=-VV
          V2= VV
          L2=1
          FL=FA
          FR=FA
          DR=0.
        END IF
        DH=HHGHDG(IAREDO)-HLOWDG(IAREDO)
        DV=VHGHDG(IAREDO)-VLOWDG(IAREDO)
        H21=0.5*(H2-H1)
        V21=0.5*(V2-V1)
        QH=DH/H21
        QV=DV/V21
        IF(QH.GE.QV) THEN
          HM=0.5*(H2+H1)
          H21=H21*QH/QV
          H1=HM-H21
          H2=HM+H21
        ELSE
          VM=0.5*(V2+V1)
          V21=V21*QV/QH
          V1=VM-V21
          V2=VM+V21
        END IF
        HRB(1)=H1
        HRB(3)=H2
        VRB(1)=V1
        VRB(3)=V2
        CALL DQ24(HRB,VRB)
        CALL DQRU(HRB,VRB)
C       ..................................................... draw area
        CALL DQPO0('AREA',LA,LF,'SKIP')
        D=DR
        NP=2*NFC+1
        DO LS=1,L2
          N1=0
          N2=NP
          DO N=1,NFC
            N1=N1+1
            N2=N2-1
            SF=SIND(FA)
            CF=COSD(FA)
            H(N1)=CF*RR(1)-D
            V(N1)=SF*RR(1)
            H(N2)=CF*RR(17)-D
            V(N2)=SF*RR(17)
            FA=FA+DFC
          END DO
          H(NP)=H(1)
          V(NP)=V(1)
          CALL DQPOL(NP,H,V)
          FA=F1+180.
          D=-DR
        END DO
C       ..................................................... draw lines
        CALL DGLEVL(LF)
        D=DR
C       ..................................................... radial lines
        DO LS=1,L2
          DO N=1,NF
            SF=SIND(FL)
            CF=COSD(FL)
            H(1)=CF*RR(1)-D
            V(1)=SF*RR(1)
            H(2)=CF*RR(17)-D
            V(2)=SF*RR(17)
            CALL DQLIE(H,V)
            FL=FL+DF
          END DO
C         ................................................ circular lines
          DO K=1,17
            F=FR
            DO N=1,NFC
              SF=SIND(F)
              CF=COSD(F)
              H(2)=CF*RR(K)-D
              V(2)=SF*RR(K)
              IF(N.GT.1) CALL DQLIE(H,V)
              H(1)=H(2)
              V(1)=V(2)
              F=F+DFC
            END DO
          END DO
          FR=F1+180.
          FL=F1+180.
          D=-DR
        END DO
        CALL DQSCA('H',HRB(1),HRB(3),'cm',2,'Y',1)
        CALL DQSCA('V',VRB(1),VRB(3),'cm',2,'X',1)
      ELSE IF(NPR.EQ.2.) THEN
C       ............................................................. FR        
        NSI=PARADA(2,J_BSI)
        HL=RR( 1)-DS
        HH=RR(17)+DS
        IF(NSI.LE.4) THEN
C         ....................................... left and right modul 
          CALL DWRT('Select 1 side only: AL,AR,BL,BR #')
        ELSE
          IF(NSI.LE.6) THEN
            S=-1.
            A180=180.
            HRB(1)=-HH
            HRB(3)=-HL
            VRB(1)=180.-FSIDE
            VRB(3)=180.+FSIDE
          ELSE
            S=1.
            A180=0.
            HRB(1)= HL
            HRB(3)= HH
            VRB(1)=-FSIDE
            VRB(3)= FSIDE
          END IF
          CALL DQ24(HRB,VRB)
          VHGHDG(IAREDO)=VHGHDG(IAREDO)-DVH
          CALL DQRU(HRB,VRB)
          CALL DQPO0('AREA',LA,LF,'SKIP')
          H(1)=S*RR( 1)
          H(3)=S*RR(17)
          V(1)=F1+A180
          V(3)=F2+A180
          CALL DQ245(H,V)
          CALL DQPOL(5,H,V)
          CALL DGLEVL(LF)
          H(1)=S*RR( 1)
          H(2)=S*RR(17)
          FF=F1+A180
          DO N=1,NF
            V(1)=FF
            V(2)=FF
            CALL DQLIE(H,V)
            FF=FF+DF
          END DO
          V(1)=F1+A180
          V(2)=F2+A180
          DO K=1,17
            H(1)=S*RR(K)
            H(2)=H(1)
            CALL DQLIE(H,V)
          END DO
        END IF
        CALL DQSCA('H',HRB(1),HRB(3),'cm',2,'&r',2)
        CALL DQSCA('V',VRB(1),VRB(3),'deg',3,'&f',2)
        VHGHDG(IAREDO)=VHGHDG(IAREDO)+DVH
      END IF
      CALL DGLEVL(LT)
      CALL DGTEXT(HLOWDG(IAREDO)+DHL,VHGHDG(IAREDO)-DVT,TSL(NSI),1)
      CALL DGTEXT(HHGHDG(IAREDO)-DHR,VHGHDG(IAREDO)-DVT,TSR(NSI),1)
      CALL DQFR(IAREDO)
      CALL DPCSAR
      END
*DK DBC_TEXT
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++ DBC_TEXT
CH
      SUBROUTINE DBC_TEXT(MD,HT,ETOT,DVT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      DIMENSION ETOT(-2:2)
      DATA DE/8./
      CHARACTER *3 DT3
      CHARACTER *4 DT4
      CALL DPAR_SET_CO(102,'BTX')
      IF(ICOLDP.GE.0.AND.MD.NE.0) THEN
        VT=VHGHDG(IAREDO)-DE-DVT
        TXTADW='E='//DT4(ETOT(MD))
        CALL DGTEXT(HT,VT-DE,TXTADW,6)
      END IF
      END
*DK DBC_TYPE
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DBC_TYPE
CH
      SUBROUTINE DBC_TYPE(NPR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      CHARACTER *23 TT
      DIMENSION NDL(0:8),NDR(0:8),ETOT(-2:2),FC(-2:2),RC(-2:2)

C             SU AB BA AA BB AL BL AR BR    -:-Z +:+Z 1=LEFT 2=RIGHT
C                -+ +- -- ++ -  +  -  +
C                12 12 12 12 1  1  2  2

      DATA NDL/0,-1, 1,-1, 1,-1, 1, 0, 0/
      DATA NDR/0, 2,-2,-2, 2, 0, 0,-2, 2/
      CHARACTER *2 TM(-2:2)
      DATA TM/'AR','AL','  ','BL','BR'/
      DATA LA/8/,LT/1/,N33/33/,N28/28/,DV/14./

C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_BSI,J_BCE'
      CALL DPARAM(38
     &  ,J_BSI,J_BCE)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      NSI=PARADA(2,J_BSI)
      CALL DV_BCAL_0(NUM,ETOT,EMAX)
      IF(NUM.EQ.0) THEN
        CALL DWRT('BCAL is empty.')
        RETURN
      END IF
      IF(NPR.GT.0) THEN
        CALL DQCL(IAREDO)
        V=VHGHDG(IAREDO)-DV
        H=HMINDG(IAREDO)
        IF(NPR.EQ.3) THEN
          CALL DQFWAF(LA)
          CALL DGLEVL(LT)
        ELSE
          CALL DQFWAF(LT)
          CALL DGLEVL(LA)
        END IF
      END IF
      CALL DV_BCAL_CENTROID(FC,RC)
      WRITE(TXTADW,1001) '  E: ',ETOT(-2),ETOT(-1),ETOT(2),ETOT(1)
 1001 FORMAT(A,F6.0,'=AR',F6.0,'=AL',F6.0,'=BR',F6.0,'=BL')
      CALL DBC_TYPE_LINE(NPR,V)
      WRITE(TXTADW,1002) 'phi: ',  FC(-2),  FC(-1),  FC(2),  FC(1)
 1002 FORMAT(A,4F9.1)
      CALL DBC_TYPE_LINE(NPR,V)
      WRITE(TXTADW,1002) 'rho: ',  RC(-2),  RC(-1),  RC(2),  RC(1)
      CALL DBC_TYPE_LINE(NPR,V)
      IF(PARADA(4,J_BCE).GT.0.) THEN
        CE=PARADA(2,J_BCE)
      ELSE
        CE=0.
      END IF
      NL=NDL(NSI)
      NR=NDR(NSI)
      DO K=1,NUM
        CALL DV_BCAL_NUM(K,MD,JF,JR,E)
        IF(E.GE.CE) THEN
          IF(NSI.EQ.0.OR.MD.EQ.NL.OR.MD.EQ.NR) THEN
            J=J+1
            WRITE(TXTADW,1000) K,TM(MD),JF,JR,E
 1000       FORMAT(I3,1X,A2,I3,'=phi',I4,'=r',F7.0,'=E')
            CALL DBC_TYPE_LINE(NPR,V)
          END IF
        END IF
      END DO
      END
*DK DBC_TYPE_LINE
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++ DBC_TYPE_LINE
CH
      SUBROUTINE DBC_TYPE_LINE(NPR,V)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      DATA DMIN/4./,DV/14./,DH/6./
      IF(NPR.GT.0) THEN
        V=V-DV
        IF(V.LE.VMINDG(IAREDO)+DMIN) RETURN
        L=LENOCC(TXTADW)
        CALL DGTEXT(HMINDG(IAREDO)+DH,V,TXTADW,L)
      ELSE
        CALL DWRC
      END IF
      END
*DK DV_BCAL_0
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++ DV_BCAL_0
CH
      SUBROUTINE DV_BCAL_0(NUM,ETOT,EMAX)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      DIMENSION ETOT(-2:2),NTRG(-2:2),RHOC(4),PHIC(4)
      DIMENSION RR(17)
      DATA RR/6.43,6.555,
     &  6.68,6.93,7.18,7.43,7.68,7.93,8.18,8.43,
     &  8.68,8.93,9.18,9.43,9.68,9.93,10.18/
      DATA F1/-33.75/,F2/33.75/,DF/11.25/,DF2/5.625/
C
C      DIMENSION NR(9),NF(9),ND(9),EE(9)
C

      DIMENSION SC(8),ACON(4),ACON1(4),SIL(4,6,16)
      DIMENSION FC(-2:2),RC(-2:2)
      DATA ACON1/0.051,0.048,0.038,0.035/
      
 
C     ...... BCAL naming conventions              DALI
C                                              1=LEFT 2=RIGHT A=-Z B=+Z
C     ......                                      -x     +x 
C     ...... 1    BR +2                         outside inside ring
C     ...... 2    BL +1                         
C     ...... 3    AR -2
C     ...... 4    AL -1
C
      DIMENSION NAME(4),A180(4)
      DATA NAME/  2,   1,  -2,  -1/
      DATA A180/  0.,180.,  0.,180./

C      DATA ND/ -1 , -1 , -1 , -2 ,  2 ,  2 ,  2 ,  1 ,  1 /
C      DATA NF/  1 ,  3 ,  6 ,  1 ,  1 ,  6 ,  5 ,  2 ,  3 /
C      DATA NR/ 12 , 10 ,  3 ,  1 ,  2 , 15 , 13 ,  8 , 10 /
C      DATA EE/ .5 , 4. ,9.78, 1. , .2 ,8.52, 2. , 7. , 1. /
C      DATA ET/101.35/
      INCLUDE 'A_BCS.INC'
      INCLUDE 'A_BMACRO.INC'

C     AL -1 14.28 9.78
C     AR -2  1. 1.
C     BR  2 10.72 8.52
C     BL  1  8. 7.

      NUM=0
      IF(IW(30).NE.12345) RETURN
      IBCSC = IW(NAMIND('BCSC'))
      IF(IBCSC.EQ.0) RETURN
      IBCSL = IW(NAMIND('BCSL'))
      IF(IBCSL.EQ.0) RETURN
      IBCGN = IW(NAMIND('BCGN'))
      IF(IBCGN.EQ.0) THEN
        CALL UCOPY(ACON1,ACON,4)
      ELSE
        DO I = 1, 4
          ACON(I) = RW(IBCGN+I+14)
        END DO
      END IF
      NUM=IW(IBCSL+1)
C
C Scintillator DATA
C
      DO I = 1 , IW(IBCSC+1)
        NCH = IBITS( IW(IBCSC+2+I), 16, 16)+1
        SC(NCH) = FLOAT(IBITS( IW(IBCSC+2+I), 0, 16))
      END DO 
c
      DO I = 1, 4
        ETOT(NAME(I)) = SC(4+I)*ACON(I)
      END DO

      EMAX=0.
      DO N=1,NUM
        ED = FLOAT(IBITS( IW(IBCSL+2+N), 0, 16))
        EMAX=MAX(EMAX,ED)
      END DO

      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------- DV_BCAL
CH
      ENTRY DV_BCAL(K,MD,FM,R1,R2,E)
CH
CH --------------------------------------------------------------------
CH
C      MD=ND(K)
C      FM=F1+NF(K)*DF-DF2
C      IF(ABS(MD).EQ.1) FM=FM+180.
C      R1=RR(K)
C      R2=RR(K+1)
C      E=EE(K)
      
      CALL GET_PAD(IBCSL,K,NPAD,NWAF,NMOD)

      MD=NAME(NMOD)
C     FM=F1+NWAF*DF-DF2
      FM=F2-NWAF*DF+DF2
      IF(ABS(MD).EQ.1) FM=FM+180.
      R1=RR(NPAD)
      R2=RR(NPAD+1)
      E = FLOAT(IBITS( IW(IBCSL+2+K), 0, 16))
 
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------- DV_BCAL_NUM
CH
      ENTRY DV_BCAL_NUM(K,MD,JF,JR,E)
CH
CH --------------------------------------------------------------------
CH
C      MD=ND(K)
C      JF=NF(K)
C      JR=NR(K)
C      E=EE(K)
C      ETOT=ET

      CALL GET_PAD(IBCSL,K,JR,JF,NMOD)
      MD=NAME(NMOD)
      E = FLOAT(IBITS( IW(IBCSL+2+K), 0, 16))
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------- DV_BCAL
CH
      ENTRY DV_BCAL_CENTROID(FC,RC)
CH
CH --------------------------------------------------------------------
CH
      CALL VZERO(SIL,384)
C
      DO I = 1 , IW(IBCSL+1)
C
        CALL GET_PAD(IBCSL,I,NPAD,NWAF,NMOD)
C     
        SIL(NMOD,NWAF,NPAD) = FLOAT(IBITS( IW(IBCSL+2+I), 0, 16))
C
      END DO 
C
      CALL GET_CENTROID(SIL,RHOC,PHIC)

      DO I=1,4
        RC(NAME(I))=RHOC(I)
        FC(NAME(I))=PHIC(I)+A180(I)
      END DO
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------ DV_BCAL_TRIGGER
CH
      ENTRY DV_BCAL_TRIGGER(NTRG)
CH
CH --------------------------------------------------------------------
CH
C     0=not triggered   1=triggered      
      IBCTR = IW(NAMIND('BCTR'))
      DO I=1,4
        I8=8-I
        NTRG(NAME(I))=IBITS(IBCTR,I8,1)
      END DO
      END
      SUBROUTINE GET_CENTROID(SILLOC,RCENLOC,PHCENLOC)
C
C-----------------------------------------------------------------
C     INPUTS: SILLOC(N_MOD,N_WAFER_PER_MODULE,N_PAD_PER WAFER) [REAL]:
c             Silicon deposition in ADC counts.
c
c
C     OUTPUT : RCENLOC(N_MOD) [REAL]: Radial centroid value in cm
c              PHCENLOC(N_MOD) [REAL]: centroid in Phi in degrees
c
C-----------------------------------------------------------------
c
c  Mapping the silicon from ADC-Channel-Event to Module-Wafer-Pad.
c
c
c  Define DATA statements and PARAMETERS needed.
c


c      15_9_97 12:30



      INCLUDE 'A_BCS.INC'

      PARAMETER (N_SIL_ADC_CH =  8)
      PARAMETER (N_SIL_ADC    =  3)
      PARAMETER (N_MULTPX     = 16)

      INTEGER  PAD_CODE(N_MULTPX) 

      DATA PAD_CODE
     &  /0,2,4,6,8,10,12,14,15,13,11,9,7,5,3,1/

      INTEGER WAFER_CODE(N_SIL_ADC_CH,N_MULTPX,N_SIL_ADC) 

      DATA WAFER_CODE
c
c 1st ADC
c
     &   /4,5,2,3,0,1,1,0,
     &    4,5,2,3,0,1,1,0,
     &    4,5,2,3,0,1,1,0,
     &    4,5,2,3,0,1,1,0,
     &    4,5,2,3,0,1,1,0,
     &    4,5,2,3,0,1,1,0,
     &    4,5,2,3,0,1,1,0,
     &    4,5,2,3,0,1,1,0,
C
     &    5,4,3,2,1,0,0,1,
     &    5,4,3,2,1,0,0,1,
     &    5,4,3,2,1,0,0,1,
     &    5,4,3,2,1,0,0,1,
     &    5,4,3,2,1,0,0,1,
     &    5,4,3,2,1,0,0,1,
     &    5,4,3,2,1,0,0,1,
     &    5,4,3,2,1,0,0,1, 
c
c 2nd ADC
c
     &    3,2,5,4,1,0,3,2,
     &    3,2,5,4,1,0,3,2,
     &    3,2,5,4,1,0,3,2,
     &    3,2,5,4,1,0,3,2,
     &    3,2,5,4,1,0,3,2,
     &    3,2,5,4,1,0,3,2, 
     &    3,2,5,4,1,0,3,2,
     &    3,2,5,4,1,0,3,2,
C
     &    2,3,4,5,0,1,2,3,
     &    2,3,4,5,0,1,2,3,
     &    2,3,4,5,0,1,2,3,
     &    2,3,4,5,0,1,2,3,
     &    2,3,4,5,0,1,2,3,
     &    2,3,4,5,0,1,2,3,
     &    2,3,4,5,0,1,2,3,
     &    2,3,4,5,0,1,2,3,
C
C 3rd ADC 
C
     &    5,4,4,5,3,2,0,1, 
     &    5,4,4,5,3,2,0,1, 
     &    5,4,4,5,3,2,0,1, 
     &    5,4,4,5,3,2,0,1, 
     &    5,4,4,5,3,2,0,1, 
     &    5,4,4,5,3,2,0,1, 
     &    5,4,4,5,3,2,0,1, 
     &    5,4,4,5,3,2,0,1,
C
     &    4,5,5,4,2,3,1,0,
     &    4,5,5,4,2,3,1,0,
     &    4,5,5,4,2,3,1,0,
     &    4,5,5,4,2,3,1,0,
     &    4,5,5,4,2,3,1,0,
     &    4,5,5,4,2,3,1,0,
     &    4,5,5,4,2,3,1,0,
     &    4,5,5,4,2,3,1,0 /
C
C
      INTEGER MOD_CODE(N_SIL_ADC_CH,N_SIL_ADC) 
C
      DATA MOD_CODE
     &  /0,0,0,0,0,0,1,1,
     &   1,1,1,1,2,2,2,2,
     &   2,2,3,3,3,3,3,3 /
c
c-----------------------------------------------------------
c
c Define BCAL parameters
c
      PARAMETER (N_MOD              = 4)
      PARAMETER (N_PAD_PER_WAFER    = 16)
      PARAMETER (N_WAFER_PER_MODULE = 6)
      PARAMETER (CONVERT_TO_CM      = 0.01)
c
c Common for the Silicon Information
c
      COMMON/SILICONDAT/SMAX_MOD(N_MOD),IPMAX_MOD(N_MOD),
     &                  IWMAX_MOD(N_MOD)
c
      REAL SMAX_MOD
      INTEGER  IPMAX_MOD,IWMAX_MOD
      REAL RCENLOC(N_MOD),PHCENLOC(N_MOD)
      REAL SILLOC(N_MOD,N_WAFER_PER_MODULE,N_PAD_PER_WAFER)
      REAL    PHNUM(N_MOD),RDEN(N_MOD),RNUM(N_MOD)
      REAL ENERGY_CORR_MOD_PMT(N_MOD), ENERGY_CORR_MOD_APD(N_MOD),
     &     SUM_SIL_MOD(N_MOD)
      REAL DWAF,DPAD,DSUM,F,
     .     PHPOS(N_WAFER_PER_MODULE),
     .     RCORR(N_WAFER_PER_MODULE),RPOS(N_PAD_PER_WAFER),
     .     SMAX
      INTEGER J,JMAX,IMAX,K,I,
     .      PADS(N_PAD_PER_WAFER),WAFER(N_WAFER_PER_MODULE)
C
C*--------------------------------------------------------------------------*/
C*                                                                          */
C*           Radial data base                                               */
C*                                                                          */
C*              _______________                                             */
C*             |   16  |       |-                                           */
C*            -|   15  |       |                                            */
C*             |   14  |       |-                                           */
C*            -|   13  |       |                                            */
C*              |  12  |      |-                                            */
C*             -|  11  |      |                                             */
C*              |  10  |      |-                                            */
C*             -|   9  |      |                                             */
C*               |  8  |      |-                                            */
C*              -|  7  |     |                                              */
C*               |  5  |     |-                                             */
C*              -|  5  |     |                                              */
C*                | 4  |    |-                                              */
C*               -| 3  |    |                                               */
C*                | 2  |    |-                                              */
C*               -| 1  |    |                                               */
C*                |____|____|                                               */
C*--------------------------------------------------------------------------*/
C
C Radial position of the center of the pads in microns.
C
      DATA RPOS/ 649.25,661.75,680.5,705.5,730.5,755.5,780.5,805.5,
     &           830.5, 855.5, 880.5,905.5,930.5,955.5,980.5,1005.5/
c
C*--------------------------------------------------------------------------*/
C*                                                                          */
C*              phi data base                                               */
C*                                                                          */
C*                                                                          */
C*                 __________________________                               */
C*        / \     |                          | 1         |                  */
C*         |      |--------------------------| 2         |                  */
C*  +ph    |      |__________________________|           |                  */
C*                |                          | 3         |   g              */
C*                |--------------------------|           |                  */
C*         |      |__________________________| 4         |                  */
C*  -ph    |      |                          | 5         |                  */
C*        \ /     |--------------------------|          \ /                 */
C*                |__________________________| 6                            */
C*                                                                          */
C*                                                                          */
C*                                                                          */
C*--------------------------------------------------------------------------*/
C
C Phi position of the center of the pads in degrees.
C
      DATA PHPOS/28.125,16.875,5.625,-5.625,-16.875,-28.125/
C
      DATA PADS/
     &      1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16/
C
      DATA WAFER/ 1,2,3,4,5,6/
c
c Variable Initialization
c
c      IMAX = 0
c      JMAX = 0
c
      CALL VZERO(RCENLOC,4)
      CALL VZERO(PHCENLOC,4)
c
C Loop over modules (total N_MOD=4). For each module, evaluate the centroid
c in R and PHI
c
      DO K = 1, N_MOD
c
        SMAX = 0.0 
        IMAX = 0
        JMAX = 0
c        RCENLOC(K)  = 0.
c        PHCENLOC(K) = 0.
c
c Loop over wafers (total N_WAFER_PER_MODULE = 6)
c
        DO I = 1, N_WAFER_PER_MODULE
c
c Loop over pads (total N_PAD_PER_WAFER = 16)
c     
           DO J = 1, N_PAD_PER_WAFER
c
C Pads number 1 and 2 are half of the others: get the weight to apply.
c
              IF(PADS(J).EQ.1.OR.PADS(J).EQ.2.) THEN
                 F=2.
              ELSE
                 F=1.
              ENDIF
c
c Localize the most energetic pad
c
              IF(SILLOC(K,I,J).EQ.0.0) GOTO 100
c
              IF (F*SILLOC(K,I,J).GT.SMAX) THEN
                 SMAX = F*SILLOC(K,I,J)
                 IMAX = I
                 JMAX = J
              ENDIF
c
 100          CONTINUE
c
           ENDDO
        ENDDO
c
C Centroid computation for each module
c   - centroid definition used -
C
C        |        |        |        |
C        -------------------------------
C        |        | =====  |        |           === Pads used for
C        |        | =====  |        |                 centroid
C        -------------------------------            computation
C        | =====  | =====  | =====  |
C        | =====  | =====  | =====  |
C        -------------------------------
C        | =====  | Maxim  | =====  |
C        | =====  | Energy | =====  |        ^
C        -------------------------------     |
C        | =====  | =====  | =====  |                R direction
C        | =====  | =====  | =====  |                  (pad)
C        ------------------------------
C        |        | =====  |        |
C        |        | =====  |        |
C        ------------------------------
C        |        |        |        |    -->Phi direction (wafers)
C
C
        RNUM(K)  = 0.
        RDEN(K)  = 0.
        PHNUM(K) = 0.
c
c Avoid any calculation when no deposition in a Module.
c
        IF(IMAX.EQ.0 .OR. JMAX.EQ.0) THEN
           RCENLOC(K)  = 0.0
           PHCENLOC(K) = 0.0
           GOTO 888
        ENDIF
c
        DO I = 1, N_WAFER_PER_MODULE
c
           DO J = 1, N_PAD_PER_WAFER
c
c              JPAD = PADS(J)
C
C "Distance" with repect to the max. energy deposi. wafer
C
              DWAF = WAFER(IMAX) - WAFER(I)
C
C "Distance" with respect to the max energy deposi. pad
C
              DPAD = PADS(JMAX) - PADS(J)
              DSUM = ABS(DWAF) + ABS(DPAD)
c
C Applied decided centroid definition (leave the "corners")
C
              IF(ABS(DWAF).LE.1.)THEN
                 IF(DSUM.LE.2)THEN
                    RDEN(K)  = RDEN(K)  + SILLOC(K,I,J)
                    RNUM(K)  = RNUM(K)  + SILLOC(K,I,J)*RPOS(J)
                    PHNUM(K) = PHNUM(K) + SILLOC(K,I,J)*PHPOS(I)
                 ENDIF
              ENDIF
           ENDDO
        ENDDO
C
C Centroid computation
C
        IF( RDEN(K).LT.1. )THEN
           RCENLOC(K)  = 0.0
           PHCENLOC(K) = 0.0 
        ELSE
           RCENLOC(K)  = (RNUM(K)/RDEN(K))*CONVERT_TO_CM
           PHCENLOC(K) = PHNUM(K)/RDEN(K)
        ENDIF
C
        SMAX_MOD(K)  = SMAX 
        IPMAX_MOD(K) = JMAX 
        IWMAX_MOD(K) = IMAX
C
C End loop over modules
C
 888    CONTINUE
c
      ENDDO
C
 999  CONTINUE
C
      RETURN
c
c
      ENTRY GET_PAD(IBCSL,KPAD,NPAD,NWAF,NMOD)
c------------------------------------------------------------------------
c Maps the Silicon information to get the 
c      module, pad & wafer that were fired.
c 
c  INPUTS:  IBCSL: pointer to the start of silicon data in BCSL bank
c           KPAD: number of channel fired.
c
c  OUTPUTS: NPAD: pad fired (in r)
c           NWAF: wafer fired (in phi)
c           NMOD: module fired
c
c------------------------------------------------------------------------
      NCHNN = IBITS( INT(IW(IBCSL+2+KPAD)), 16, 16) + 1
C
      NADC = INT(NCHNN/N_MULTPX/N_SIL_ADC_CH)+1
      NCH  = MOD(MOD(NCHNN,N_MULTPX*N_SIL_ADC_CH),N_SIL_ADC_CH)+1
      NMTX = INT(MOD(NCHNN,N_MULTPX*N_SIL_ADC_CH)/N_SIL_ADC_CH)+1
C     
      NPAD = PAD_CODE(NMTX)+1
      NWAF = WAFER_CODE(NCH,NMTX,NADC)+1
      NMOD = MOD_CODE(NCH,NADC)+1

      END

