*DK DKAP
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DKAP
CH
      SUBROUTINE DKAP
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
      CHARACTER *2 TANSW
      LOGICAL FCHG
      CHARACTER *49 T1,T2,T3,T4
C
C              123456789 123456789 123456789 123456789 123456789
      DATA T1/'P?:W?:All projections of X,Y,Z,rho,phi,theta,R,D '/
      DATA T2/'      VV_12 /  HV_12     1 2 3  4   5    6   7 8 '/
      DATA T3/'      HM_12345 DH_1234 VM_12345 DV$1234 AL_123   '/
      DATA T4/' FI_1234 CF$123  TE_1234 CT$123   TH$-123 TV$-123'/
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE'
      CALL DPARAM(11
     &  ,J_PFI,J_PTE)
      CALL DPARP0
      CALL DPAROP(11,'PFI_PCF_PTE_PCT_PCC')
      CALL DPAROP(35,'AHM_ADH_AVM_ADV_AAL_AVV_AHV_ATH_ATV')
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
  930 CALL DO_BAR_STATUS_0
      CALL DTYPT('TYPE',TPICDO, 0    ,  0   ,  0   ,  ' ' ,T1)
      CALL DTYPT('TYPE',' '   ,N_1_DA,N_2_DA,PARADA,TPOPDA,T2)
      CALL DTYPT('TYPE',' '   ,N_1_DA,N_2_DA,PARADA,TPOPDA,T3)
      CALL DTYPT('TYPE',' '   ,N_1_DA,N_2_DA,PARADA,TPOPDA,T4)
  936 FCHG=.FALSE.
      CALL DGZOOM(6,IAREDO,0,0)
      CALL DOPER(1,1,
     &  N_1_DA,N_2_DA,TPOPDA,PARADA,
     &  1,0,' ',0,
     &  NEXEC,FCHG,TANSW)
      CALL DGZOOM(6,-1,0,0)
      GO TO (910,920,930,940),NEXEC
  910 RETURN
  920 IF(TANSW.EQ.'X?') GO TO 930
      CALL DWR_IC(TANSW)
      GO TO 936
  940 CALL DAPD
      GO TO 930
      END
*DK DKDL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DKDL
CH
      SUBROUTINE DKDL
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Updated by C.Grab                         24-Jun-1990
C
C ---------------------------------------------------------------------
C INVOKED BY TANSW.EQ.'DL'  (DKDL)
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *2 TANSW
      PARAMETER (MLIST=12)
      CHARACTER *17 TLIST(MLIST)
      DATA TLIST/
     &  'List of Tracks   ',
     &  'Trig.bits level i',
     &  'Detector bits    ',
     &  '                 ',
     &  'HCAL-Muon assoc. ',
     &  'Tracks with Cuts ',
     &  'Short track list ',
     &  'Track-Muon assoc.',
     &  'Ionisation list  ',
     &  'Full list of 1 t.',
     &  'Full list of 2 t.',
     &  'Ioni. list/sector'/
      CHARACTER *2 TCOM(MLIST)
C                 1    2    3    4    5    6    7    8    9   10
      DATA TCOM/'LT','TB','DB','**','HM','CT','ST','TM','IO','F1',
     &          'F2','IL'/
      LOGICAL FCHG
      CHARACTER *49 T1
C                       1         2         3         4         5
C              12345678901234567890123456789012345678901234567890
      DATA T1/'P?:W?: display Trig.bits level i   NS$-1'/
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_LLI,J_LNS,J_LRG'
      CALL DPARAM(33
     &  ,J_LLI,J_LNS,J_LRG)
      CALL DPARP0
      CALL DPAROP(33,'LNS')
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
  930 CALL DO_BAR_STATUS_0
      LIST=PARADA(2,J_LLI)
      T1(16:32)=TLIST(LIST)
      CALL DO_BAR_STATUS(TCOM(LIST),0,' ')
      IF(LIST.EQ.12) THEN
        PARADA(4,J_LNS)=-1.
      ELSE
        PARADA(4,J_LNS)=1.
      END IF
      CALL DTYPT('TYPE',TPICDO,N_1_DA,N_2_DA,PARADA,TPOPDA,T1)
  936 CALL DGZOOM(6,IAREDO,0,0)
      CALL DOPER(NTVIDT,1,
     &  1,0,' ',0,
     &  N_1_DA,N_2_DA,TPOPDA,PARADA,
     1  NEXEC,FCHG,TANSW)
      CALL DGZOOM(6,-1,0,0)
      GO TO (910,920,930,940),NEXEC
  910 RETURN
  920 CALL DO_STR_LIST(MLIST,TCOM,'name of list')
      DO N=1,MLIST
        IF(TANSW.EQ.TCOM(N)) THEN
          PARADA(2,J_LLI)=N
          GO TO 930
        END IF
      END DO
      CALL DO_STR('RA')
      IF(TANSW.EQ.'RA') THEN
        PARADA(2,J_LRG)=1.
        GO TO 940
      END IF
      CALL DO_STR('DG')
      IF(TANSW.EQ.'DG') THEN
        PARADA(2,J_LRG)=0.
        GO TO 940
      END IF
      CALL DO_STR('LH')
      IF(TANSW.EQ.'LH') THEN
        CALL DDLHIT
        GO TO 930
      END IF
      IF(TANSW.EQ.'X?') THEN
        CALL DO_TY_COMMAND_LIST('DKDL')
        GO TO 936
      END IF
      CALL DWR_IC(TANSW)
      GO TO 936
  940 CALL DDLD
      GO TO 930
      END
*DK DKEC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DKEC
CH
      SUBROUTINE DKEC
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
C INVOKED BY TANSW.EQ.'EC'  (DKEC)
C     PROJECTION PARAMETERS:
C     1: no zoom:  1 parameter = max radius = side length of square = to
C     2: zoom : a) no rotation centered: user area = square ;
C                  3 parameters= square from .. to .. with center at phi.
C               b) no rotation uncentered:
C               c) rotation centered: 4 parameters: rectangle defined
C                  by from .. to .. aspect ratio with center at phi.
C                  The aspect ratio is calculated at start from DF.
C                  DF is calculated at exit from the aspect ratio.
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *1 TL(3)
      DATA TL/'1','2','3'/
      CHARACTER *2 TANSW
      CHARACTER *36 TE(2)
      DIMENSION HRB(4),VRB(4)
      CHARACTER *2 TRB(4),TRE(4)
      DATA TRB/'**','SQ','**','**'/
      DATA TRE/'RE','**','**','**'/
      CHARACTER *4 DT4
      DATA RLC/55./
      CHARACTER *2 TENDC(-1:1)
      DATA TENDC/'B ','AB','A '/
      CHARACTER *2 TFR(3,3)
      DATA TFR/
     &  'F1','F2','F3',
     &  'T1','T2','T3',
     &  'L1','L2','L3'/
      DATA DSE/10./,DVE/2./,DHE/0./
      LOGICAL FCHG
      CHARACTER *49 T1,T2,T3
C                       1         2         3         4         5
C              12345678901234567890123456789012345678901234567890
      DATA T1/'P?:W?:Z?:endcap A+B From layer 1 To layer 3 of LC'/
      DATA T2/'   SK$12.4  FI_12345 CF$12345 FR_1234'/
      DATA T3/'            TE_12345 CT$12345 TO_1234    TH$12345'/
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_PFR,J_PTO,J_PAS,J_PBX,J_PX0,J_PY0,J_PLY,J_PAB'
      CALL DPARAM(10
     &  ,J_PFI,J_PTE,J_PFR,J_PTO,J_PAS,J_PBX,J_PX0,J_PY0,J_PLY,J_PAB)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DPARP0
      CALL DPAROP(10,'PFI_PCF_PTE_PCT_PCC_PFR_PTO_PSK_PTH')

      PARADA(2,J_PBX)=2.
      MODEB=2
  930 CALL DO_BAR_STATUS_0

      NENDC=PARADA(2,J_PAB)
      CALL DO_BAR_STATUS(TENDC(NENDC),17,T1)
C      T1(17:19)=TENDC(NENDC)

      L1=PARADA(2,J_PLY)
      L2=PARADA(4,J_PLY)
      IF(L1.EQ.L2) THEN
        T1(21:43)='Layer '//TL(L1)
      ELSE
        T1(21:43)='From layer '//TL(L1)//' To layer '//TL(L2)
      END IF
      CALL DTYPT('TYPE',TPICDO, 0 ,  0,  0   ,  ' ' ,T1)
      CALL DTYPT('TYPE',' '   ,N_1_DA,N_2_DA,PARADA,TPOPDA,T2)
      CALL DTYPT('TYPE',' '   ,N_1_DA,N_2_DA,PARADA,TPOPDA,T3)

      FCHG=.FALSE.
  936 CALL DOPER(NTVIDT,1,
     &  N_1_DA,N_2_DA,TPOPDA,PARADA,
     &  1,0,' ',0,
     1  NEXEC,FCHG,TANSW)

      GO TO (910,920,930,940),NEXEC
  910 IF(TANSW.NE.'GW') CALL DYX_AS_TO_DF
      RETURN

  920 CALL DO_STR('RB: rubberband')
      CALL DQ_ZOOM(TRB,TANSW,J_PX0,J_PY0,0.,HRB,VRB,MODEB,NYES)
      IF(NYES.EQ.1) GO TO 936
      IF(NYES.EQ.2) GO TO 930
      IF(NYES.GT.2) THEN
        CALL DYX_HV_TO_CONE(HRB,VRB)
        IZOMDO=1
        GO TO 930
      END IF

      CALL DO_STR('FB"DB"CB: fix/display/clear rubber band box')
      IF(TANSW.EQ.'FB'.OR.TANSW.EQ.'DB'.OR.TANSW.EQ.'CB') THEN
        IF(FNOPDR) GO TO 936
        CALL DYX_CONE_TO_HV(HRB,VRB)        
        CALL DQZO_DISPLAY(TANSW,HRB,VRB)
        IF(FCHG) GO TO 930
        GO TO 936
      END IF

      CALL DO_STR('NZ')
      IF(TANSW.EQ.'NZ') THEN
         IZOMDO=0
         PARADA(2,J_PTO)=RLC
         GO TO 930
      END IF

      CALL DO_STR('ZO')
      IF(TANSW.EQ.'ZO') THEN
        IZOMDO=1
        GO TO 930
      END IF

      CALL DO_STR('EA')
      IF(TANSW.EQ.'EA') THEN
        PARADA(2,J_PAB)= 1.
        GO TO 930
      END IF
      CALL DO_STR('EB')
      IF(TANSW.EQ.'EB') THEN
        PARADA(2,J_PAB)=-1.
        GO TO 930
      END IF
      CALL DO_STR('AB')
      IF(TANSW.EQ.'AB') THEN
        PARADA(2,J_PAB)= 0.
        GO TO 930
      END IF
      CALL DO_STR_LIST(6,TFR,'layer')
      DO N=1,2
        DO K=1,3
          IF(TANSW.EQ.TFR(K,N)) THEN
            M=2*N
            PARADA(M,J_PLY)=K
            GO TO 936
          END IF
        END DO
      END DO
      DO K=1,3
        IF(TANSW.EQ.TFR(K,3)) THEN
          PARADA(2,J_PLY)=K
          PARADA(4,J_PLY)=K
          GO TO 936
        END IF
      END DO
      CALL DO_STR('LA')
      IF(TANSW.EQ.'LA') THEN
        PARADA(2,J_PLY)=1
        PARADA(4,J_PLY)=3
        GO TO 936
      END IF
      CALL DO_STR('LC')
      IF(TANSW.EQ.'LC') THEN
        PARADA(2,J_PTO)=RLC
        PARADA(2,J_PFR)=0.
        GO TO 936
      END IF
      CALL DO_STR('EE"EN')
      IF(TANSW.EQ.'EE'.OR.TANSW.EQ.'EN') THEN
        CALL DWRT('Select area via rubber band cursor')
        MODEE=1
        CALL DQ_ZOOM(TRB,'RB',J_PX0,J_PY0,0.,HRB,VRB,MODEE,NYES)
        IF(NYES.LE.2) GO TO 936
        CALL DECEN(HRB,VRB)
        FECEDE=.FALSE.
C              123456789 123456789 123456789 123456
        TE(1)='L1=--.- L2=--.- L3=--.- SUM=--.- (B)'
        TE(2)='L1=--.- L2=--.- L3=--.- SUM=--.- (A)'
        JT=0
        DO J=1,4,3
          JT=JT+1
          J1=4
          SUM=0.
          DO L=J,J+2
            TE(JT)(J1:J1+3)=DT4(ENECDE(L))
            SUM=SUM+ENECDE(L)
            J1=J1+8
          END DO
          IF(SUM.GT.0.) THEN
            TE(JT)(29:32)=DT4(SUM)
            CALL DWRT(TE(JT))
          END IF
        END DO
        GO TO 936
      END IF
      CALL DO_STR('TY')
      IF(TANSW.EQ.'TY') THEN
        V=VLOWDG(IAREDO)+DVE
        H=HLOWDG(IAREDO)+DHE
        CALL DQLEVL(ICTXDD)
        IF(TE(1)(29:29).NE.'-') THEN
          CALL DGTEXT(H,V,TE(1),36)
          V=V+DSE
        END IF
        IF(TE(2)(29:29).NE.'-') CALL DGTEXT(H,V,TE(2),36)
        GO TO 936
      END IF
      IF(TANSW.EQ.'X?') THEN
        CALL DO_TY_COMMAND_LIST('DKEC')
        GO TO 936
      END IF
      CALL DWR_IC(TANSW)
      GO TO 936
  940 CALL DECD
      GO TO 930
      END
*DK DKEF
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DKEF
CH
      SUBROUTINE DKEF
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
C INVOKED BY TANSW.EQ.'EF'  (DEF)
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *2 TANSW
      CHARACTER *2 TSY(2:6),TBB(4),TFT(0:1)
C               2    3    4    5    6
      DATA TSY/'LE','CN','NU','SQ','SP'/
      DATA TBB/'EF','**','EC','HC'/
      DATA TFT/'FT','TF'/
      LOGICAL FCHG
      CHARACTER *49 T1(2:6),T2,T3
C                        1          2        3         4
C              123456789 123456789 123456789 123456789 123456789
      DATA T1/'P?:W?:FT EF FI_123 NB_12 TY$1234567    LE: SZ$123',
     &        'P?:W?:FT EF FI_123 NB_12 TY$1234567    CN:       ',
     &        'P?:W?:FT EF FI_123 NB_12 TY$1234567    NU: ND_1  ',
     &        'P?:W?:FT EF FI_123 NB_12 TY$1234567    SQ: SZ$123',
     &        'P?:W?:FT EF FI_123 NB_12 TY$1234567    SP: SZ$123'/
      DATA T2/'                                           SK_12 '/
      DATA T3/' Boxsize factor: DF_1 DT_1                       '/
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     & 'J_ENB,J_END,J_EDF'
      CALL DPARAM(32
     & ,J_ENB,J_END,J_EDF)
      CALL DPARP0
      CALL DPAROP(32,'EFI_ESK_ETY_ENB_END_ESZ_EDF_EDT')
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
  930 CALL DO_BAR_STATUS_0
      MO=ABS(PARADA(4,J_END))
      MB=ABS(PARADA(4,J_ENB))
      MF=ABS(PARADA(4,J_EDF))
      T1(MO)( 7: 8)=TFT(MF)
      T1(MO)(10:11)=TBB(MB)
      CALL DTYPT('TYPE',TPICDO,N_1_DA,N_2_DA,PARADA,TPOPDA,T1(MO))
      IF(MO.EQ.2) CALL DTYPT('TYPE',' ',N_1_DA,N_2_DA,PARADA,TPOPDA,T2)
      IF(MO.EQ.6) CALL DTYPT('TYPE',' ',N_1_DA,N_2_DA,PARADA,TPOPDA,T3)
  936 FCHG=.FALSE.
      CALL DGZOOM(6,IAREDO,0,0)
      CALL DOPER(0,1,
     &  1,0,' ',0,
     &  N_1_DA,N_2_DA,TPOPDA,PARADA,
     1  NEXEC,FCHG,TANSW)
      CALL DGZOOM(6,-1,0,0)
      GO TO (910,920,930,940),NEXEC
  910 RETURN
  920 CALL DO_STR_LIST(5,TSY(2),'symbol')
      DO N=2,6
        IF(TANSW.EQ.TSY(N)) THEN
          PARADA(4,J_END)=N
          GO TO 930
        END IF
      END DO
      CALL DO_STR_LIST(4,TBB,' ')
      DO N=1,4
        IF(TANSW.EQ.TBB(N)) THEN
          PARADA(4,J_ENB)=N
          GO TO 930
        END IF
      END DO
      CALL DO_STR('SG')
      IF(TANSW.EQ.'SG') THEN
        CALL DCDGRY
        GO TO 936
      END IF
      CALL DO_STR('FT')
      IF(TANSW.EQ.'FT') THEN
        PARADA(4,J_EDF)=0.
        GO TO 936
      END IF
      CALL DO_STR('TF')
      IF(TANSW.EQ.'TF') THEN
        PARADA(4,J_EDF)=1.
        GO TO 936
      END IF
      IF(TANSW.EQ.'X?') THEN
        CALL DO_TY_COMMAND_LIST('DKEF')
        GO TO 936
      END IF
  929 CALL DWR_IC(TANSW)
      GO TO 936
  940 CALL DEFD
      GO TO 930
      END
*DK DKFR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DKFR
CH
      SUBROUTINE DKFR
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
C INVOKED BY TANSW.EQ.'FR'  (DKFR)
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *2 TANSW
      CHARACTER *5 DT5
      DIMENSION HRB(4),VRB(4)
      CHARACTER *2 TRB(4)
      DATA TRB/4*'**'/
      DATA QPTT/10./
      LOGICAL FCHG,FRET
      CHARACTER *49 T1,T2,T3
C              12345678901234567890123456789012345678901234567890
      DATA T1/'P?:W?:Z?:  DF_12345 FI_12345 CF$12345 FR_1234 >[]'/
      DATA T2/'                    TE_12345 CT$12345 TO_1234 <[]'/
      DATA T3/'L-Skew: PT$1234 ~ 12345 NS_12 DP_123 DW_123 DS_12'/
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_PPT'
      CALL DPARAM(11
     &  ,J_PFI,J_PTE,J_PPT)
      CALL DPARP0
      CALL DPAROP(10,'PFI_PDF_PCF_PTE_PCT_PCC_PFR_PTO_PPT_PLM')
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(FGETDO) CALL DDRCAL('IN','RO')

  930 CALL DO_BAR_STATUS_0

      CALL DTYPT('TYPE',TPICDO,N_1_DA,N_2_DA,PARADA,TPOPDA,T1)
      CALL DTYPT('TYPE',' '   ,N_1_DA,N_2_DA,PARADA,TPOPDA,T2)
      IF(PARADA(4,J_PPT).GE.1.) THEN
        TPARDA=
     &    'J_FNS,J_FDP,J_FDW,J_FDS'
        CALL DPARAM(99
     &    ,J_FNS,J_FDP,J_FDW,J_FDS)
        CALL DPAROP(99,'FNS_FDP_FDW_FDS')
        IF(PARADA(4,J_PPT).EQ.1.) THEN
          T3(1:1)='L'
        ELSE
          T3(1:1)='A'
        END IF
        IF(PARADA(2,J_PPT).EQ.0.) THEN
          T3(19:23)=' '
        ELSE
          T3(19:23)=DT5(QPTT/PARADA(2,J_PPT))
        END IF
        CALL DTYPT('TYPE',' '   ,N_1_DA,N_2_DA,PARADA,TPOPDA,T3)
      END IF
      CALL DGRBTY
      FCHG=.FALSE.
  936 CALL DGZOOM(6,IAREDO,0,0)
      CALL DOPER(NTVIDT,1,
     &  N_1_DA,N_2_DA,TPOPDA,PARADA,
     &  1,0,' ',0,
     1  NEXEC,FCHG,TANSW)
      CALL DGZOOM(6,-1,0,0)
      IF(NEXEC.EQ.1.AND.TANSW.EQ.'GW') GO TO 910
      CALL DDRCAL('OP','RO')
      GO TO (910,920,930,940),NEXEC
  910 RETURN

  920 CALL DO_STR('RB"NS"SQ: rubberband, zoom modes')
      CALL DQ_ZOOM(TRB,TANSW,0,0,0.,HRB,VRB,1,NYES)
      IF(NYES.EQ.1) GO TO 936
      IF(NYES.EQ.2) GO TO 930
      IF(NYES.GT.2) THEN
        CALL DFR_HV_TO_CONE(HRB,VRB)
        IZOMDO=1
        GO TO 930
      END IF
      CALL DO_STR('FB"DB"CB: fix/display/clear rubber band box')
      IF(TANSW.EQ.'FB'.OR.TANSW.EQ.'DB'.OR.TANSW.EQ.'CB') THEN
        CALL DFR_CONE_TO_HV(HRB,VRB)
        CALL DQZO_DISPLAY(TANSW,HRB,VRB)
        IF(FCHG) GO TO 930
        GO TO 936
      END IF

      IF(TANSW(1:1).EQ.'Z') THEN
C       ...........................................  'B3'S3'N3'R3'F3'M3'Q3'A3'
        CALL DGRB(TANSW,FRET)
        IF(FRET) GO TO 930
      END IF

      CALL DO_STR('NZ')
      IF(TANSW.EQ.'NZ') THEN
         IZOMDO=0
         CALL DDRCAL('NZ','RO')
         GO TO 930
      END IF
      CALL DO_STR('ZO')
      IF(TANSW.EQ.'ZO') THEN
        IZOMDO=1
        GO TO 930
      END IF

      IF(PARADA(4,J_PPT).GE.1.) THEN
        CALL DO_STR('AA: toggle between linear and non linear skew')
        IF(TANSW.EQ.'AA') THEN
          IF(PARADA(4,J_PPT).EQ.1.) THEN
            PARADA(4,J_PPT)=2.
          ELSE
            PARADA(4,J_PPT)=1.
          END IF
          GO TO 930
        END IF
        CALL DO_STR('A1: set DP for ++')
        IF(TANSW.EQ.'A1') THEN
          CALL DOPERS(1,J_PPT,PARADA(2,J_FDP))
          GO TO 930
        END IF
        CALL DO_STR('A2: set DP/NS for ++')
        IF(TANSW.EQ.'A2') THEN
          CALL DOPERS(1,J_PPT,PARADA(2,J_FDP)/PARADA(2,J_FNS))
          GO TO 930
        END IF
        CALL DO_STR('AW: define new windows')
        IF(TANSW.EQ.'AW') THEN
          H0=HHGHDG(0)
          HHGHDG(0)=0.
          V1=VMINDG(IAREDO)
          V2=VHGHDG(IAREDO)
          DO I=1,12
            VMINDG(I)=V1
            VHGHDG(I)=V2
            HMINDG(I)=HHGHDG(I-1)+PARADA(2,J_FDS)
            HHGHDG(I)=HMINDG( I )+PARADA(2,J_FDW)
          END DO
          HHGHDG( 0)=H0
          HHGHDG(12)=H0
          CALL DWSETC
          CALL DWRT('New window setup 1,2,3,4,5,6,U,D,L,M,R, S')
          IAREDO=1
          GO TO 936
        END IF
      END IF
      IF(TANSW.EQ.'X?') THEN
        CALL DO_TY_COMMAND_LIST('DKFR')
        GO TO 936
      END IF
      CALL DWR_IC(TANSW)
      GO TO 936
  940 CALL DFRD
      IF(FCHG) GO TO 930
      GO TO 936
      END

*DK DKFZ
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DKFZ
CH
      SUBROUTINE DKFZ
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
C INVOKED BY TANSW.EQ.'FZ'  (DKFZ)
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *2 TANSW
      DIMENSION HRB(4),VRB(4)
      CHARACTER *2 TRB(4)
      DATA TRB/4*'**'/
      DIMENSION NFLVD(2)
      DATA NFLVD/9,15/
      CHARACTER *2 TLAY(0:2)
      DATA TLAY/'LT','L1','L2'/
      CHARACTER *2 TWAF(8,2)
      DATA TWAF/'S1','S2','S3','S4','**','**','SL','SR',
     &          'S1','S2','S3','S4','S5','S6','SL','SR'/
      LOGICAL FCHG,FRET

      CHARACTER *49 T1,T2,T3,T4,T5
C              12345678901234567890123456789012345678901234567890
      DATA T1/'P?:W?:Z?:  DF_12345 FI_12345 CF$12345 FR_1234 >[]'/
      DATA T2/'Layer=VV            TE_12345 CT$12345 TO_1234 <[]'/
      DATA T3/'DL$12  FA_12 WA$?????  HW$12 SC$123 HS$123  EL$12'/
C     DATA T4/' 9 |  1 |  2 |  3 |  4 |  5 |  6 |  7 |  8 |  9 |'/
C     DATA T5/'15| 1| 2| 3| 4| 5| 6| 7| 8| 9|10|11|12|13|14|15| '/
      DATA T4/'|  2 |  3 |  4 |  5 |  6 |  7 |  8 |  9 |  1 |'/
      DATA T5/'| 3| 4| 5| 6| 7| 8| 9|10|11|12|13|14|15| 1| 2|'/
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PDF,J_PTE,J_PL1,J_PFR,J_PTO'
      CALL DPARAM(10
     &  ,J_PFI,J_PDF,J_PTE,J_PL1,J_PFR,J_PTO)
      TPARDA=
     &  'J_FLA,J_FFA,J_FWA'
      CALL DPARAM(16
     &  ,J_FLA,J_FFA,J_FWA)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DPARP0
      CALL DPAROP(10,'PFI_PDF_PCF_PTE_PCT_PCC_PFR_PTO')
      CALL DPAROP(16,'FFA_FWA_FEL_FDL_FSC_FHS_FHW')

      IF(NWAFDV.GE.6) THEN
C       ............................................. new VDET has 6 wafers
        NVD=2
        CALL DPARGV(82,'TVN',2,TOVD)
      ELSE
C       ............................................. old VDET has 4 wafers
        NVD=1
        CALL DPARGV(82,'TVO',2,TOVD)
        IF(PARADA(2,J_FWA).EQ.5.OR.
     &     PARADA(2,J_FWA).EQ.6) PARADA(2,J_FWA)=4.
      END IF
      PARADA(3,J_FWA)=NWAFDV
      IF(FGETDO) THEN
         IF(PARADA(2,J_FLA).GT.0.) THEN
           PARADA(2,J_PL1)=0.
         ELSE
           CALL DDRCAL('IN','ZZ')
         END IF
      END IF
  930 CALL DO_BAR_STATUS_0

      NLAY=PARADA(2,J_FLA)
      IF(NLAY.NE.0.) THEN
        T1(47:49)=' '
        T2(47:49)=' '
      ELSE
        T1(47:49)='>[]'
        T2(47:49)='<[]'
      END IF
      CALL DTYPT('TYPE',TPICDO,N_1_DA,N_2_DA,PARADA,TPOPDA,T1)

      CALL DO_BAR_STATUS(TLAY(NLAY),7,T2)
C     T2(7:8)=TLAY(NLAY)

      CALL DTYPT('TYPE',' '   ,N_1_DA,N_2_DA,PARADA,TPOPDA,T2)
      IF(NLAY.NE.0.) THEN
        IF(     PARADA(2,J_FWA).EQ.7) THEN
          T3(14:21)='WA:left'
          IF(PARADA(4,J_FWA).LT.0.) T3(16:16)='/'
        ELSE IF(PARADA(2,J_FWA).EQ.8) THEN
          T3(14:21)='WA:right'
          IF(PARADA(4,J_FWA).LT.0.) T3(16:16)='/'
        ELSE
          T3(14:21)='WA$123'
        END IF
        CALL DTYPT('TYPE',' ' ,N_1_DA,N_2_DA,PARADA,TPOPDA,T3)
        PARADA(1,J_FFA)=-NFLVD(NLAY)
        PARADA(3,J_FFA)= NFLVD(NLAY)
        PARADA(2,J_FFA)=MIN(FLOAT(NFLVD(NLAY)),PARADA(2,J_FFA))
      END IF
      CALL DGRBTY
      FCHG=.FALSE.

936      CALL DOPER(NTVIDT,1,
     &  N_1_DA,N_2_DA,TPOPDA,PARADA,
     &  1,0,' ',0,
     1  NEXEC,FCHG,TANSW)
      CALL DGZOOM(6,-1,0,0)
      IF(NEXEC.EQ.1.AND.TANSW.EQ.'GW') GO TO 910
      IF(PARADA(4,J_FWA).LT.0.) CALL DDRCAL('OP','ZZ')
      GO TO (910,920,930,940),NEXEC

  910 RETURN

  920 CALL DO_STR('RB"NS"SQ: rubberband, zoom modes')
      CALL DQ_ZOOM(TRB,TANSW,0,0,0.,HRB,VRB,1,NYES)
      IF(NYES.EQ.1) GO TO 936
      IF(NYES.EQ.2) GO TO 930
      IF(NYES.GT.2) THEN
        CALL DFZ_HV_TO_CONE(HRB,VRB)
        IZOMDO=1
        GO TO 930
      END IF

      CALL DO_STR('FB"DB"CB: fix/display/clear rubber band box')
      IF(TANSW.EQ.'FB'.OR.TANSW.EQ.'DB'.OR.TANSW.EQ.'CB') THEN
        CALL DFZ_CONE_TO_HV(HRB,VRB)
        CALL DQZO_DISPLAY(TANSW,HRB,VRB)
        IF(FCHG) GO TO 930
        GO TO 936
      END IF

      CALL DO_STR_LIST(8,TWAF(1,NVD),'wafer')
      CALL DO_STR('NZ')
      IF(TANSW.EQ.'NZ') THEN
         IZOMDO=0
         IF(PARADA(2,J_FLA).LT.0.) THEN
           CALL DDRCAL('NZ','ZZ')
         ELSE
           PARADA(2,J_PTO)=TOVD
         END IF
         PARADA(4,J_FWA)=-1.
         GO TO 930
      END IF

      CALL DO_STR('ZO')
      IF(TANSW.EQ.'ZO') THEN
        IZOMDO=1
        GO TO 930
      END IF

      IF(TANSW(1:1).EQ.'Z') THEN
C       .......................................................... B3,R3,F3 M3
        CALL DGRB(TANSW,FRET)
        IF(FRET) GO TO 930
      END IF

      CALL DO_STR('TZ')
      IF(TANSW.EQ.'TZ') THEN
        PARADA(2,J_PTE)=180.-PARADA(2,J_PTE)
        GO TO 930
      END IF

      CALL DO_STR_LIST(3,TLAY,'layer')
      DO L=0,2
        IF(TANSW.EQ.TLAY(L)) THEN
          PARADA(2,J_FLA)=L
          IF(L.EQ.0) THEN
            CALL DPARGV(82,'TFZ',2,PARADA(2,J_PTO))
          ELSE
            PARADA(2,J_PTO)=TOVD
          END IF
          GO TO 930
        END IF
      END DO

      IF(PARADA(2,J_FLA).GT.0.) THEN
        DO K=1,8
          IF(TANSW.EQ.TWAF(K,NVD)) THEN
            PARADA(2,J_FWA)=K
            CALL DADVDM
            IZOMDO=1
            GO TO 930
          END IF
        END DO

        CALL DO_STR('SW')
        IF(TANSW.EQ.'SW') THEN
          CALL DADVDM
          IZOMDO=1
          GO TO 930
        END IF

        CALL DO_STR('SH')
        IF(TANSW.EQ.'SH') THEN
          Z1=PARADA(2,J_PFR)
          Z2=PARADA(2,J_PTO)
          ZM=0.5*(Z2+Z1)
          DL=0.5*PARADA(2,J_PDF)/FISCDV
          PARADA(2,J_PFR)=ZM-DL
          PARADA(2,J_PTO)=ZM+DL
          GO TO 930
        END IF

        CALL DO_STR('SV')
        IF(TANSW.EQ.'SV') THEN
          Z1=PARADA(2,J_PFR)
          Z2=PARADA(2,J_PTO)
          DZ=Z2-Z1
          PARADA(2,J_PDF)=FISCDV*DZ
          GO TO 930
        END IF

        CALL DO_STR('TL')
        IF(TANSW.EQ.'TL') THEN
          CALL DWRT(T4)
          CALL DWRT(T5)
          GO TO 936
        END IF
      END IF

      IF(TANSW.EQ.'X?') THEN
        CALL DO_TY_COMMAND_LIST('DKFZ')
        GO TO 936
      END IF

      CALL DWR_IC(TANSW)
      GO TO 936
  940 CALL DFZD
      IF(FCHG) GO TO 930
      GO TO 936
      END

*DK DKFT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DKFT
CH
      SUBROUTINE DKFT
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
C INVOKED BY TANSW.EQ.'TF'  (DKFT)
C     CALCULATION OF PULL DOWN FACTOR FOR 1 GEV TRACKS AT 45 DEG.
C     P_TRANSVERSAL = 0.2998 * 15[KGAUSS] * R_TRACK[CM]       R_T = C *
C     C = 1 / ( 0.2998 * 15 )  [ CM / MEV ]
C     R = 2 * C * P * DF      DF[RAD]
C     R = 2 * C * P * DF / 57.2957            DF [ DEG ]
C     CP = 2 / ( 0.2998 * 15 * 57.2957 )
C     R [CM] = P * CP * DF [DEG]
C     DR * PDOWN / DDF = P * PDOWN * CP
C     AT 45 DEG  1 = P * PDOWN * CP    ON THE PICTURE [CM]/[DEG]
C     AT 45 DEG  1 = P * PDOWN * CP * 360 / 384               [CM]/[I_UN
C     P = ( 384 / 360 ) / ( CP * PDOWN ) = 137.42 / PDOWN
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *2 TANSW
      LOGICAL FCHG,FYES,FOK
      DATA FOK/.FALSE./
      PARAMETER (MCDF=6)
      CHARACTER *2 TCOL(MCDF)
      DATA TCOL/'LV','LE','LH','LP','LD','SZ'/
      DIMENSION NARE(6)
      DATA NARE/4,2,1,3,5,6/

      LOGICAL FRET
      DIMENSION HRB(4),VRB(4)
      CHARACTER *2 TRB(4)
      DATA TRB/4*'**'/
      DATA MO/0/
      CHARACTER *2 DT2
      CHARACTER *3 DT3
      CHARACTER *4 DT4
      DATA CLAST/99./
      CHARACTER *17 TMC1,TMC2
      DATA TMC1/'(ZO:C2:OP:C1:OP:)'/
      DATA TMC2/'(ZO:C1:OP:C2:OP:)'/
      CHARACTER *60 TMC3
      DATA TMC3/
     &  '(GG:XY:DS=OF:CC:TO=T3:C1:GG:FR:C2:GG:RZ:C3:GG:TF:C4:CC=OF)'/
C                       1         2         3         4
C              1234567890123456789012345678901234567890123456789
C              FT:WS:NZ  ecal objects HI=123   SK:12.4  PU/12345
C              FT:WS:NZ     LAyer E1           SK:12.4  PU/12345
C              FT:WS:NZ  max FRom E1 TO E3     SK:12.4  PU/12345
      CHARACTER *49 T,T1,T2,T3,T4,T5
      DATA T1/'P?:W?:Z?:                       SK$-123  PU$12345'/
      DATA T2/'LP$12 LE$12 LH$12 |  FI_12345  DF_12345  CF$12345'/
      DATA T3/'SZ$12 LD$         |  TE_12345  DT_12345  CT$12345'/
      DATA T4/'Neutral energy flow "tracks": HI_123'/
      DATA T5/'Vertical skew : VS$123'/
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PDF,J_PCF,J_PTE,J_PDT,J_PCT,J_PP1,J_PP2'
      CALL DPARAM(10
     &  ,J_PFI,J_PDF,J_PCF,J_PTE,J_PDT,J_PCT,J_PP1,J_PP2)
      TPARDA=
     &  'J_PRF,J_PRT,J_VSK,J_PHI'
      CALL DPARAM(13
     &  ,J_PRF,J_PRT,J_VSK,J_PHI)
      TPARDA=
     &  'J_HEO'
      CALL DPARAM(20
     &  ,J_HEO)
      CALL DPARP0
      CALL DPAROP(11,'PFI_PDF_PCF_PTE_PDT_PCT_PDD_PCC_PLA_PP1_PP2')
      CALL DPAROP(13,'PRF_PRT_PPU_VSK_PSK_PHI')
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(CLAST.NE.PDCODD(2,KCLGDD)) THEN
        COLIDF(2,5)=PDCODD(2,KCLGDD)
        CLAST      =PDCODD(2,KCLGDD)
      END IF
  930 CALL DO_BAR_STATUS_0

      T=T1
      N1=PARADA(2,J_PP1)-1000.
      N2=PARADA(2,J_PP2)-1000.
      IF(N2.EQ.11) THEN
         IF(N1.LE.4) THEN
           T(11:26)='TPc + ec.ob. HI_'
         ELSE
           T(11:26)='ecal objects HI_'
         END IF
         T(27:29)=DT3(PARADA(2,J_PHI))
      ELSE IF(N1.GT.0.AND.N2.LE.4) THEN
         T(14:21)='LAyer TP'
      ELSE IF(N1.EQ.N2) THEN
         T(14:21)='LAyer '//TPLNDO(N1)
      ELSE
         IF(IZOMDO.EQ.0.AND.MO.NE.2) THEN
           T(11:13)='max'
         ELSE
           T(11:13)='add'
         END IF
         T(15:27)='FRom '//TPLNDO(N1)//' TO '//TPLNDO(N2)
      END IF
      CALL DTYPT('TYPE',TPICDO,N_1_DA,N_2_DA,PARADA,TPOPDA,T)
      CALL DTYPT('SAVE',' ',1     ,MCDF  ,COLIDF,TCOL  ,T2 )
      CALL DTYPT('TYPE',' ',N_1_DA,N_2_DA,PARADA,TPOPDA,' ')
      CALL DTYPT('SAVE',' ',1     ,MCDF  ,COLIDF,TCOL  ,T3 )
      CALL DTYPT('TYPE',' ',N_1_DA,N_2_DA,PARADA,TPOPDA,' ')
      IF(PARADA(4,J_HEO).EQ.2.)
     &  CALL DTYPT('TYPE',' ',N_1_DA,N_2_DA,PARADA,TPOPDA,T4)
      T=' '
      IF(PARADA(4,J_PRT).EQ.1.)
     &  T='rho/theta above uses '//DT2(PARADA(2,J_PRT))//'%'
      IF(PARADA(4,J_PRF).EQ.1.)
     &  T(27:)='phi/rho left uses '//DT2(PARADA(2,J_PRF))//'%'
      IF(T.NE.' ') CALL DWRT(T)
      IF(PARADA(4,J_VSK).EQ.1.)
     &  CALL DTYPT('TYPE',' ',N_1_DA,N_2_DA,PARADA,TPOPDA,T5)
      CALL DGRBTY
  936 FCHG=.FALSE.
      CALL DGZOOM(6,IAREDO,0,0)
      CALL DOPER(NTVIDT,1,
     &  N_1_DA,N_2_DA,TPOPDA,PARADA,
     &  1     ,MCDF  ,TCOL  ,COLIDF,
     1  NEXEC,FCHG,TANSW)
      CALL DGZOOM(6,-1,0,0)
      GO TO (910,920,930,940),NEXEC
  910 RETURN

 920  CALL DO_STR('RB: rubberband, zoom modes')
      CALL DQ_ZOOM(TRB,TANSW,0,0,0.,HRB,VRB,1,NYES)
      IF(NYES.EQ.1) GO TO 936
      IF(NYES.EQ.2) GO TO 930
      IF(NYES.GT.2) THEN
        CALL DFT_HV_TO_CONE(HRB,VRB)
        IZOMDO=1
        GO TO 930
      END IF

      CALL DO_STR('FB"DB"CB: fix/display/clear rubber band box')
      IF(TANSW.EQ.'FB'.OR.TANSW.EQ.'DB'.OR.TANSW.EQ.'CB') THEN
        CALL DFT_CONE_TO_HV(HRB,VRB)
        CALL DQZO_DISPLAY(TANSW,HRB,VRB)
        IF(FCHG) GO TO 930
        GO TO 936
      END IF

      CALL DO_STR('NZ')
      IF(TANSW.EQ.'NZ') THEN
         IZOMDO=0
         GO TO 930
      END IF
      CALL DO_STR('ZO')
      IF(TANSW.EQ.'ZO') THEN
         IZOMDO=1
         GO TO 930
      END IF
      IF(TANSW(1:1).EQ.'Z') THEN
C       .......................................................... B3,R3,F3
        CALL DGRB(TANSW,FRET)
        IF(FRET) GO TO 930
      END IF
      CALL DHTMO1(7,TANSW,FYES)
      IF(FYES) GO TO 930
      CALL DO_STR('SF')
      IF(TANSW.EQ.'SF') THEN
         ST=MAX(0.1,SIND(PARADA(2,J_PTE)))
         PARADA(2,J_PDF)=PARADA(2,J_PDT)/ST
         GO TO 930
      END IF
      CALL DO_STR('ST')
      IF(TANSW.EQ.'ST') THEN
         ST=MAX(0.1,SIND(PARADA(2,J_PTE)))
         PARADA(2,J_PDT)=PARADA(2,J_PDF)*ST
         GO TO 930
      END IF
      CALL DO_STR('ZZ: zoom projection on W1,W2,W3,W4= macro F10')
      IF(TANSW.EQ.'ZZ') THEN
        CALL DGINMA(TMC3)
        GO TO 936
      END IF
      CALL DO_STR('VV: draw normal no zoom V_plot')
      IF(TANSW.EQ.'VV') THEN
        CALL DHTMOD('HI',FDUM)
        IZOMDO=0
        PARADA(4,J_PCF)=-1.
        PARADA(4,J_PCT)=-1.
        PARADA(2,J_PP1)=1001.
        PARADA(2,J_PP2)=1004.
        GO TO 940
      END IF
      CALL DO_STR('ZE')
      IF(TANSW.EQ.'ZE') THEN
         IEND=4
         TANSW='zh'
      END IF
      CALL DO_STR('ZH')
      IF(TANSW.EQ.'ZH') THEN
        IEND=5
        TANSW='zh'
      END IF

      IF(TANSW.EQ.'zh') THEN
        IF(IZOMDO.EQ.1) THEN
          COLIDF(4,1)=-1.
          COLIDF(4,2)=1.
          COLIDF(4,3)=-1.
        END IF
        IARE=IAREDO
        PP1=PARADA(2,J_PP1)
        PP2=PARADA(2,J_PP2)
        P1=1001.
        P2=1004.
        DO   700  I=1,IEND
          IAREDO=NARE(I)
          PARADA(2,J_PP1)=P1
          PARADA(2,J_PP2)=P2
          CALL DFTD
          P2=P2+1.
          IF(TANSW.EQ.'ZE'.AND.FX11DT) CALL DGCHKX
          P1=P2
  700   CONTINUE
        PARADA(2,J_PP1)=PP1
        PARADA(2,J_PP2)=PP2
        IAREDO=IARE
        IF(FCHG) GO TO 930
        GO TO 936
      END IF
      CALL DO_STR('EE"EN')
      IF(TANSW.EQ.'EE'.OR.TANSW.EQ.'EN') THEN
        FECEDE=.TRUE.
        FPIKDP=.TRUE.
        CALL DFTD
        FPIKDP=.FALSE.
        FECEDE=.FALSE.
C          123456789 123456789 123456789 123456789 123456789
        T=' L1=12.4  L2=12.4  L3=12.4  SUM=12.4'
        N1=5
        SUM=0.
        DO L=1,3
          T(N1:N1+3)=DT4(ENECDE(L))
          SUM=SUM+ENECDE(L)
          N1=N1+9
        END DO
        IF(SUM.GT.0.) THEN
          T(33:36)=DT4(SUM)
          CALL DWRT(T)
        END IF
C          123456789 123456789 123456789 123456789 123456789
        T=' E1=12.4  E2=12.4  E3=12.4  SUM=12.4  HC=12.4 GEV'
        N1=5
        SUM=0.
        DO L=4,6
          T(N1:N1+3)=DT4(ENECDE(L))
          SUM=SUM+ENECDE(L)
          N1=N1+9
        END DO
        IF(SUM.GT.0..OR.ENECDE(7).GT.0.) THEN
          T(33:36)=DT4(SUM)
          T(42:45)=DT4(ENECDE(7))
          CALL DWRT(T)
        END IF
        GO TO 936
      END IF

      CALL DO_STR('MM')
      IF(TANSW.EQ.'MM') THEN
        CALL DFTM(IAREDO,-1,FOK)
        GO TO 936
      END IF
      CALL DO_STR('LM')
      IF(TANSW.EQ.'LM') THEN
        CALL DFTM( 9,10,FOK)
        GO TO 936
      END IF
      CALL DO_STR('LR')
      IF(TANSW.EQ.'LR') THEN
        CALL DFTM( 9,11,FOK)
        GO TO 936
      END IF
      CALL DO_STR('MR')
      IF(TANSW.EQ.'MR') THEN
        CALL DFTM(10,11,FOK)
        GO TO 936
      END IF
      CALL DO_STR('FM')
      IF(TANSW.EQ.'FM'.AND.FOK) THEN
        CALL DFTMF
        GO TO 936
      END IF
      CALL DO_STR('SA')
      IF(TANSW.EQ.'SA'.AND.FOK) THEN
        CALL DFTMG(PARADA(2,J_PFI),PARADA(2,J_PTE))
        GO TO 930
      END IF
      CALL DO_STR('Z2')
      IF(TANSW.EQ.'Z2') THEN
        FI=PARADA(2,J_PFI)
        TE=PARADA(2,J_PTE)
        IF(FI.LT.180.) THEN
          CALL DGINMA(TMC1)
        ELSE
          CALL DGINMA(TMC2)
        END IF
        GO TO 936
      END IF
      CALL DO_STR('SV')
      IF(TANSW.EQ.'SV') THEN
        CALL DFTVDS
        GO TO 936
      END IF
      IF(TANSW.EQ.'X?') THEN
        CALL DO_TY_COMMAND_LIST('DKFT')
        GO TO 936
      END IF
  929 CALL DWR_IC(TANSW)
      GO TO 936
  940 CALL DFTD
      IF(FCHG) GO TO 930
      GO TO 936
      END
*DK DKHF
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DKHF
CH
      SUBROUTINE DKHF
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
C INVOKED BY TANSW.EQ.'HF'  (DKHF)
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *2 TANSW
C      DIMENSION HRB(4),VRB(4)
      CHARACTER *4 TDET(0:1)
      DATA TDET/
     &  'ALl ',
     &  'LCal'/
      LOGICAL FCHG
      CHARACTER *49 T1,T2
C                       1         2         3         4         5
C              12345678901234567890123456789012345678901234567890
      DATA T1/'P?:W?:Z?:  show 1234   DF_12345 FI_12345 CF$12345'/
      DATA T2/'         se_1234 sh_1234        TE_12345 CT$12345'/
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PMF'
      CALL DPARAM(10
     &  ,J_PMF)
      CALL DPARP0
      CALL DPAROP(10,'PFI_PDF_PCF_PTE_PCT_PCC')
      CALL DPARCH(12,'PSE','se')
      CALL DPARCH(12,'PSH','sh')
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
  930 CALL DO_BAR_STATUS_0
      NDET=PARADA(2,J_PMF)
      T1(17:20)=TDET(NDET)
      CALL DTYPT('TYPE',TPICDO,N_1_DA,N_2_DA,PARADA,TPOPDA,T1)
      CALL DTYPT('TYPE',' '   ,N_1_DA,N_2_DA,PARADA,TPOPDA,T2)
      FCHG=.FALSE.
  936 CALL DGZOOM(6,IAREDO,0,0)
      CALL DOPER(NTVIDT,1,
     &  N_1_DA,N_2_DA,TPOPDA,PARADA,
     &  1,0,' ',0,
     1  NEXEC,FCHG,TANSW)
      CALL DGZOOM(6,-1,0,0)
      GO TO (910,920,930,940),NEXEC
  910 RETURN
  920 CALL DO_STR('NZ')
      IF(TANSW.EQ.'NZ') THEN
         IZOMDO=0
         GO TO 930
      END IF
      CALL DO_STR('ZO')
      IF(TANSW.EQ.'ZO') THEN
        IZOMDO=1
        GO TO 930
      END IF
      CALL DO_STR('AL')
      IF(TANSW.EQ.'AL') THEN
        PARADA(2,J_PMF)=0.
        GO TO 930
      END IF
      CALL DO_STR('LC')
      IF(TANSW.EQ.'LC') THEN
        PARADA(2,J_PMF)=1.
        GO TO 930
      END IF
      IF(TANSW.EQ.'X?') THEN
        CALL DO_TY_COMMAND_LIST('DKHF')
        GO TO 936
      END IF
      CALL DWR_IC(TANSW)
      GO TO 936
  940 CALL DHFD
      IF(FCHG) GO TO 930
      GO TO 936
      END
*DK DKHZ
CH..............+++
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DKHZ
CH
      SUBROUTINE DKHZ
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
C INVOKED BY TANSW.EQ.'HZ'  (DKHZ)
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *2 TANSW
      LOGICAL FCHG
      LOGICAL FYES
      CHARACTER *49 T1,T2
C                       1         2         3         4         5
C              12345678901234567890123456789012345678901234567890
      DATA T1/'P?:W?:          PR_12         FI_12345 CF$12345'/
      DATA T2/'  SE_123 SH_123               TE_12345 CT$12345'/
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE'
      CALL DPARAM(10
     &  ,J_PFI,J_PTE)
      CALL DPARP0
      CALL DPAROP(10,'PFI_PCF_PTE_PCT_PCC_PSE_PSH_PPR')
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
  930 CALL DO_BAR_STATUS_0
      CALL DTYPT('TYPE',TPICDO,N_1_DA,N_2_DA,PARADA,TPOPDA,T1)
      CALL DTYPT('TYPE',' '   ,N_1_DA,N_2_DA,PARADA,TPOPDA,T2)
  936 FCHG=.FALSE.
      CALL DGZOOM(6,IAREDO,0,0)
      CALL DOPER(NTVIDT,1,
     &  N_1_DA,N_2_DA,TPOPDA,PARADA,
     &  1,0,' ',0,
     &  NEXEC,FCHG,TANSW)
      CALL DGZOOM(6,-1,0,0)
      CALL DDRCAL('IN','RZ')
      GO TO (910,920,930,940),NEXEC
  910 RETURN
  920 CALL DM2D(TANSW,FYES)
      IF(FYES) GO TO 930
      CALL DO_STR('FL')
      IF(TANSW.EQ.'FL') THEN
        CALL DM2DF
        GO TO 936
      END IF
      IF(TANSW.EQ.'X?') THEN
        CALL DO_TY_COMMAND_LIST('DKHZ')
        GO TO 936
      END IF
      CALL DWR_IC(TANSW)
      GO TO 936
  940 CALL DHZD
      GO TO 930
      END
*DK DKLT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DKLT
CH
      SUBROUTINE DKLT
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
C INVOKED BY TANSW.EQ.'LT'  (DLT)
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *49 T
      COMMON /DLSTRC/
     &  AXYFDL(30),BXYFDL(30),CXYFDL(30),
     &  AZRFDL(30),BZRFDL(30),CZRFDL(30),
     &  FILADL(30),TELADL(30),
     &  NTRKDL(3,0:9),
     &  NSTRDL, NWIRDL(3),WIRRDL(6000),WIRZDL(6000),WIREDL(6000)
      COMMON /DLSTRT/TTRKDL(0:30)
      CHARACTER *2 TTRKDL
      CHARACTER *2 TANSW,TDIR(8)
      DIMENSION FDIR(8)
      DATA TDIR/'DY','DZ','DD','RY','RZ','WI','WZ','WE'/
      DATA FDIR/ 1. , 2. , 3. , 4. , 8. ,128.,256.,512./
      CHARACTER *2 DT2
      CHARACTER *3 DT3,DT3Z
      DIMENSION X(21),Y(21),R(21),Z(21)
      DIMENSION NFI(30),IFI(30),ITE(30),NPO(30)
      DIMENSION NTE(0:4)
      LOGICAL FCHG
      DATA NTE   /24,36,53,78,100/
      CHARACTER *3 TFOP(3)
      DATA TFOP/' 90','210','330'/
      CHARACTER *1 TNUM(0:9),TFI(3)
      DATA TNUM/'0','1','2','3','4','5','6','7','8','9'/
      DATA TFI/'U','L','R'/
C               123456789 123456789 123456789 123456789 123456789
      CALL=DVTP0(1,0,NUM)
      IF(NUM.LE.0) GO TO 99
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE'
      CALL DPARAM(11
     &  ,J_PFI,J_PTE)
      TPARDA=
     &  'J_LDS,J_LDL,J_LNT,J_LDI,J_LFI'
      CALL DPARAM(34
     &  ,J_LDS,J_LDL,J_LNT,J_LDI,J_LFI)
      CALL DPARP0
      CALL DPAROP(34,'LDS,LDL')
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(FNLADN) THEN
        FNLADN=.FALSE.
        CALL=DVCHT0(NTRK)
        CALL VZERO(NTRKDL,30)
        CALL VZERO(NPO,30)
        DO K=0,30
          TTRKDL(K)='  '
        END DO
        IF(NTRK.GT.30)
     &    CALL DWRT('Too many laser tracks = '//DT3(FLOAT(NTRK)))
        DO   700  N=1,MIN(27,NTRK)
           CALL=DVCHT(N,NUM)
           ZSUM=0.
           DO   710  KIND=1,NUM
              K=IDVCHT(KIND)
              X(KIND)=DVTP(IVXXDV,K)
              Y(KIND)=DVTP(IVYYDV,K)
              R(KIND)=DVTP(IVRODV,K)
              Z(KIND)=DVTP(IVZZDV,K)
              ZSUM=ZSUM+SIGN(1.,Z(KIND))
  710      CONTINUE
           IF(ZSUM.GE.0.) THEN
             PARADA(4,J_LFI)=1.
           ELSE
             PARADA(4,J_LFI)=-1.
           END IF
           CALL LFIT(X,Y,NUM,1,AXYFDL(N),BXYFDL(N),CXYFDL(N))
C
C++         Take greater care with phi evaluation. (SJH 06/02/88)
C
           FI =MOD(3600.+DATN2D(AXYFDL(N),1.),360.)
           FI1=MOD(3600.+DATN2D(Y(1),X(1))   ,360.)
           IF(ABS(FI-FI1).GT.90.) FI = AMOD(FI+180.,360.)
           FILADL(N)=FI
           IFI(N)=FI
           CXYFDL(N)=100.*SQRT(CXYFDL(N))*ABS(COSD(FI))
           CALL LFIT(R,Z,NUM,1,AZRFDL(N),BZRFDL(N),CZRFDL(N))
           CZRFDL(N)=100.*SQRT(CZRFDL(N))
           TE=DATN2D(1.,AZRFDL(N))
           TELADL(N)=TE
           ITE(N)=TE
           NPO(N)=NUM
           DO   720  L=0,4
             IF(Z(1).GT.0.) THEN
               IF(ITE(N).LT.NTE(L)) THEN
                 I=9-L
                 GO TO 1
               END IF
             ELSE
               NTETA=180-ITE(N)
               IF(NTETA.LT.NTE(L)) THEN
                 I=L
                 GO TO 1
               END IF
             END IF
  720      CONTINUE
    1      TTRKDL(N)(2:2)=TNUM(I)
           IF(IFI(N).GT.0.AND.IFI(N).LT.180.) THEN
             NTRKDL(1,I)=N
             TTRKDL(N)(1:1)=TFI(1)
             NFI(N)=1
           ELSE IF(IFI(N).LT.270) THEN
             NTRKDL(2,I)=N
             TTRKDL(N)(1:1)=TFI(2)
             NFI(N)=2
           ELSE
             NTRKDL(3,I)=N
             TTRKDL(N)(1:1)=TFI(3)
             NFI(N)=3
           END IF
  700   CONTINUE
C
C++     Now set up wire coordinates - supply PHI from first (pad) track.
C
C        CALL DVWI(FI, NUMW,WIRRDL,WIRZDL,WIREDL)
C        NWIRDL(1) = NUMW
      END IF
C        123456789 123456789 123456789 123456789 123456789
  930 CALL DO_BAR_STATUS_0
      T='LT:W1:   Display TPC laser tracks and wires.'
      CALL DWR_HL_AR(T)
      JF=MAX(PARADA(2,J_LFI),1.)
      WRITE(TXTADW,1002)(     TTRKDL(NTRKDL(JF,I)) ,I=0,9),'track-name'
      CALL DWRC
      WRITE(TXTADW,1000)(            NTRKDL(JF,I)  ,I=0,9) ,'track #'
      CALL DWRC
      WRITE(TXTADW,1000)(        NPO(NTRKDL(JF,I)) ,I=0,9),'# of hits '
      CALL DWRC
      WRITE(TXTADW,1000)(        IFI(NTRKDL(JF,I)) ,I=0,9),' phi'
      CALL DWRC
      WRITE(TXTADW,1000)(        ITE(NTRKDL(JF,I)) ,I=0,9),' theta'
      CALL DWRC
      WRITE(TXTADW,1003)(DT3Z(CXYFDL(NTRKDL(JF,I))),I=0,9),'rms(xy)'
      CALL DWRC
      WRITE(TXTADW,1003)(DT3Z(CZRFDL(NTRKDL(JF,I))),I=0,9),'rms(rz)'
      CALL DWRC
 1000 FORMAT(10I4,1X,A)
 1002 FORMAT(10(2X,A),1X,A)
 1003 FORMAT(2X,10(A,1X),A)
  931 NT=PARADA(2,J_LNT)
      NDIR=PARADA(2,J_LDI)
      IF(NDIR.LE.8) THEN
         IF(PARADA(4,J_LNT).NE.1.) THEN
C            123456789 123456789 123456789 123456789 123456789
          T='view XY:AT DL=.23 Show tracks with phi= 90(A),Z<0'
          T(40:42)=TFOP(JF)
          T(44:44)=TFI(JF)
          IF(NDIR.GE.4) THEN
            T(46:49)=' '
          ELSE
            IF(PARADA(4,J_LFI).GE.0) T(48:48)='>'
          END IF
        ELSE
C            123456789 123456789 123456789 123456789 123456789
          T='view XY:ST DL=.23  Display track 12=A2'
          T(34:35)=DT2(PARADA(2,J_LNT))
          T(37:38)=TTRKDL(NT)
        END IF
      ELSE
        T='view XY    DL=.23  Display wires'
      END IF
      DO   730  JDIR=1,9
        IF(NDIR.EQ.NINT(FDIR(JDIR))) GO TO 932
  730 CONTINUE
  932 T(6:7)=TDIR(JDIR)
      IF(NDIR.EQ.4.OR.NDIR.EQ.8) THEN
        T(13:13)='S'
        T(15:17)=DT3(PARADA(2,J_LDS))
      ELSE
        T(15:17)=DT3(PARADA(2,J_LDL))
      END IF
      CALL DWRT(T)
  936 CALL DGZOOM(6,IAREDO,0,0)
      CALL DOPER(NTVIDT,1,
     &  N_1_DA,N_2_DA,TPOPDA,PARADA,
     &  1,0,' ',0,
     1  NEXEC,FCHG,TANSW)
      CALL DGZOOM(6,-1,0,0)
C     NEXEC=1: QWIT OR OTHER PICTURE ...
C     NEXEC=2: TWO LETTER COMMAND NOT FOUND IN DOPER
C     NEXEC=3: RETURN IN DOPER
C     NEXEC=4: TWO BLANCS = EXECUTE
      GO TO (910,920,930,940),NEXEC
  910 RETURN
  920 CALL DO_STR('UT')
      IF(TANSW.EQ.'UT') THEN
        PARADA(2,J_LFI)=1.
        GO TO 930
      END IF
      CALL DO_STR('LT')
      IF(TANSW.EQ.'LT') THEN
        PARADA(2,J_LFI)=2.
        GO TO 930
      END IF
      CALL DO_STR('RT')
      IF(TANSW.EQ.'RT') THEN
        PARADA(2,J_LFI)=3.
        GO TO 930
      END IF
      CALL DO_STR('ZP')
      IF(TANSW.EQ.'ZP') THEN
        PARADA(4,J_LFI)= 1.
        GO TO 930
      END IF
      CALL DO_STR('ZN')
      IF(TANSW.EQ.'ZN') THEN
        PARADA(4,J_LFI)=-1.
        GO TO 930
      END IF
      CALL DO_STR('AT')
      IF(TANSW.EQ.'AT') THEN
        PARADA(4,J_LNT)=-1.
        GO TO 931
      END IF
      CALL DO_STR('ST')
      IF(TANSW.EQ.'ST') THEN
        PARADA(4,J_LNT)=1.
        GO TO 931
      END IF
      CALL DO_STR_LIST(8,TDIR,' ')
      DO   740  J=1,8
         IF(TANSW.EQ.TDIR(J)) THEN
            PARADA(2,J_LDI)=FDIR(J)
            GO TO 931
         END IF
  740 CONTINUE
      CALL DO_STR_LIST(30,TTRKDL,' ')
      DO 750 K=1,30
        IF(TANSW.EQ.TTRKDL(K)) THEN
          PARADA(2,J_LNT)=K
          PARADA(4,J_LNT)=1.
          PARADA(2,J_LFI)=NFI(K)
          IF(ITE(K).GE.5) THEN
            PARADA(4,J_LFI)= 1.
          ELSE
            PARADA(4,J_LFI)=-1.
          END IF
          GO TO 940
        END IF
  750 CONTINUE
      IF(TANSW.EQ.'X?') THEN
        CALL DO_TY_COMMAND_LIST('DKLT')
        GO TO 936
      END IF
      CALL DWR_IC(TANSW)
      GO TO 936
  940 CALL DLTD
      CALL DPCSAR
      GO TO 931
   99 CALL DWRT('No TPC hits or tracks ?')
      TPICDO='GB'
      END

*DK DKRO
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DKRO
CH
      SUBROUTINE DKRO
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
      CHARACTER *2 TANSW
      PARAMETER (MP2=4)
      CHARACTER *2 TPVW(6)

      DIMENSION FIVW(6),TEVW(6),ALVW(6)
      DATA TPVW/'YX','YZ','ZX','XZ','ZY','XY'/
      DATA FIVW/  0.,  0.,  0.,270., 90., 90./
      DATA TEVW/ 90.,  0., 90.,  0., 90., 90./
      DATA ALVW/  0.,  0., 90.,  0., 90.,180./

      LOGICAL FCHG
      LOGICAL FRET,FCU
      DIMENSION HRB(4),VRB(4)
      CHARACTER *2 TRB(4)
C                                       ! SQ=NO RELFECTION OF SQUARE AS=1
C                                       ! NS=NO REFLECTION OF RECTANGLE AS#1
      DATA TRB/'NS','SQ','**','**'/
      CHARACTER *2 TM(9)
      DATA TM/'O0','O1','O2','O3','O4','LD','PX','PY','PZ'/
      CHARACTER *4 DT4
      DATA GT3/8./
      DATA MOCA,MOCJ,MOC2/1,3,1/
      CHARACTER *49 T1,T2,T3,T4,T5,T6,T7,T8,T9
C                       1         2         3         4         5
C              123456789 123456789 123456789 123456789 123456789
      DATA T1/'P?:W?:Z?:SQ   AS_1334 FI_12345 CF$12345 FR_1234'/
      DATA T2/'AL_123 DA_123 DY$1234 TE_12345 CT$12345 TO_1234'/
      DATA T3/'x=1234 y=1234 z=1234  TN_12 VN_12 JN_12  YC$123'/
      DATA T4/'Stereo: ES$123 EY$123'/
      DATA T5/'Track length for smooth rotations: DF_123'/
      DATA T6/'Speed vector: SV$123 set:ES,EY,DY=OF (TF:LP=x)'/
      DATA T7/'Vdet, ecal, hcal wire frames: WV$1  WE$1  WH$1'/
      DATA T8/'artific. cube : FA$12345 TA$12345 GA$12345 BA$123'/
      DATA T9/'Energy flow tracks (also chrged) are drawn. SP_12'/
C              123456789 123456789 123456789 123456789 123456789
C
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_PL1,J_PL2,J_PAS,J_PBR'
      CALL DPARAM(11
     &  ,J_PFI,J_PTE,J_PL1,J_PL2,J_PAS,J_PBR)
      TPARDA=
     &  'J_RAL,J_RDA,J_RSV,J_RRM,J_RDY,J_RES,J_REY,J_RXX,J_RYY,J_RZZ'
      CALL DPARAM(15
     &  ,J_RAL,J_RDA,J_RSV,J_RRM,J_RDY,J_RES,J_REY,J_RXX,J_RYY,J_RZZ)
      TPARDA=
     &  'J_RDF,J_RST'
      CALL DPARAM(15
     &  ,J_RDF,J_RST)
      TPARDA=
     &  'J_HEO'
      CALL DPARAM(20
     &  ,J_HEO)
      TPARDA=
     &  'J_RTN,J_RJN,J_RYC,J_RWV,J_RWE,J_RWH'
      CALL DPARAM(43
     &  ,J_RTN,J_RJN,J_RYC,J_RWV,J_RWE,J_RWH)
      TPARDA=
     &  'J_RFA,J_RTA,J_RGA,J_RBA'
      CALL DPARAM(86
     &  ,J_RFA,J_RTA,J_RGA,J_RBA)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DPARP0
      CALL DPAROP(10,'PFI_PCF_PTE_PCT_PCC_PFR_PTO_PAS')
      CALL DPAROP(15,'RAL_RDA_RSV_RDY_RES_REY_RDF_RSP')
      CALL DPAROP(43,'RVN_RTN_RJN_RYC_RWV_RWE_RWH')
      CALL DPAROP(86,'RFA_RTA_RGA_RBA')
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      RFA=PARADA(2,J_RFA)
      RTA=parada(2,J_RTA)
      RGA=PARADA(2,J_RGA)
      RBA=PARADA(2,J_RBA)
      PARADA(3,J_RTN)=BNUMDB(2,FRFTDB)
      PARADA(2,J_RTN)=MIN(PARADA(2,J_RTN),PARADA(3,J_RTN))
      PARADA(3,J_RST)=BNUMDB(2,FRFTDB)
      PARADA(2,J_RST)=MIN(PARADA(2,J_RST),PARADA(3,J_RST))
      MODEB=PARADA(2,J_PBR)

  930 CALL DO_BAR_STATUS_0

      IF(MODEB.NE.2.AND.IZOMDO.EQ.1) CALL DO_BAR_STATUS('$A',0,T1)
      CALL DO_BAR_STATUS(TRB(MODEB),10,T1)
      CALL DTYPT('TYPE',TPICDO,N_1_DA,N_2_DA,PARADA,TPOPDA,T1)
      CALL DTYPT('TYPE',' '   ,N_1_DA,N_2_DA,PARADA,TPOPDA,T2)
      IF(PARADA(4,J_RDY).LT.0.) THEN
        T3( 3: 6)=DT4(PARADA(2,J_RXX))
        T3(10:14)=DT4(PARADA(2,J_RYY))
        T3(17:20)=DT4(PARADA(2,J_RZZ))
        CALL DTYPT('TYPE',' '   ,N_1_DA,N_2_DA,PARADA,TPOPDA,T3)
        IF(NAX.EQ.4) CALL DROJTT
      END IF
      IF(PARADA(4,J_RES).EQ.1..OR.
     &   PARADA(4,J_REY).EQ.1.)
     &  CALL DTYPT('TYPE',' '   ,N_1_DA,N_2_DA,PARADA,TPOPDA,T4)
      IF(PARADA(4,J_RDF).EQ.1.)
     &  CALL DTYPT('TYPE',' '   ,N_1_DA,N_2_DA,PARADA,TPOPDA,T5)
      IF(PARADA(4,J_RSV).EQ.1.)
     &  CALL DTYPT('TYPE',' '   ,N_1_DA,N_2_DA,PARADA,TPOPDA,T6)
      IF(PARADA(4,J_RWV).EQ.1..OR.
     &   PARADA(4,J_RWE).EQ.1..OR.
     &   PARADA(4,J_RWH).EQ.1.)
     &  CALL DTYPT('TYPE',' '   ,N_1_DA,N_2_DA,PARADA,TPOPDA,T7)
      IF(PARADA(4,J_RFA).EQ.1..OR.
     &   PARADA(4,J_RTA).EQ.1..OR.
     &   PARADA(4,J_RGA).EQ.1..OR.
     &   PARADA(4,J_RBA).EQ.1.)
     &  CALL DTYPT('TYPE',' '   ,N_1_DA,N_2_DA,PARADA,TPOPDA,T8)
      IF(PARADA(4,J_HEO).EQ.2.)
     &  CALL DTYPT('TYPE',' '   ,N_1_DA,N_2_DA,PARADA,TPOPDA,T9)
      CALL DGRBTY
      FCHG=.FALSE.
  936 P4J=PARADA(4,J_RYC)
      P2J=PARADA(2,J_RYC)

      CALL DOPER(NTVIDT,1,
     &  N_1_DA,N_2_DA,TPOPDA,PARADA,
     &  0,0,' ',0,
     1  NEXEC,FCHG,TANSW)

      IF(PARADA(4,J_RYC).NE.P4J.OR.PARADA(2,J_RYC).NE.P2J) THEN
        CALL DROJTC(PARADA(1,J_RJN))
        PARADA(4,J_RDY)=-1.
        NAX=4
      END IF

      CALL DDRCAL('OP','RR')
      GO TO (910,920,930,940),NEXEC

  910 CALL DGSPPT(MOCA,MOC2)
      RETURN

  920 CALL DO_STR('RB"NS"SQ: rubberband, zoom modes')
      CALL DQ_ZOOM(TRB,TANSW,0,0,0.,HRB,VRB,MODEB,NYES)
      PARADA(2,J_PBR)=MODEB

      IF(NYES.EQ.1) GO TO 936
      IF(NYES.EQ.2) GO TO 930
      IF(NYES.GT.2) THEN
        CALL DRO_HV_TO_CONE(HRB,VRB)
        CALL DDRCAL('RB','RR')
        IZOMDO=1
        GO TO 930
      END IF

      CALL DO_STR('FB"DB"CB: fix/display/clear rubber band box')
      IF(TANSW.EQ.'FB'.OR.TANSW.EQ.'DB'.OR.TANSW.EQ.'CB') THEN
        CALL DDRFLG
        IF(FNOPDR) GO TO 936
        CALL DRO_CONE_TO_HV(HRB,VRB)        
        CALL DQZO_DISPLAY(TANSW,HRB,VRB)
        IF(FCHG) GO TO 930
        GO TO 936
      END IF

      CALL DO_STR('NZ')
      IF(TANSW.EQ.'NZ') THEN
         IZOMDO=0
         PARADA(2,J_PL1)=0.
         PARADA(2,J_PL2)=GT3
         CALL DDRCAL('IN','RR')
         GO TO 930
      END IF
      CALL DO_STR('ZO')
      IF(TANSW.EQ.'ZO') THEN
        IZOMDO=1
        GO TO 930
      END IF

      IF(TANSW(1:1).EQ.'Z') THEN
C       .......................................................... B3,R3,F3 M3
        CALL DGRB(TANSW,FRET)
        IF(FRET) THEN
C         ........... Stereo or perspective give problems in DAPPT1 with 'Z' !
          PARADA(4,J_RES)=-1.
          PARADA(4,J_REY)=-1.
          GO TO 930
        END IF
      END IF
      CALL DO_STR('FF')
      IF(TANSW.EQ.'FF') THEN
         PARADA(2,J_RAL)=
     &     MOD(3600.+PARADA(2,J_RAL)+PARADA(2,J_RDA),360.)
         CALL DROD1
         WRITE(TXTADW,1001)PARADA(2,J_RAL)
         CALL DWRC
 1001    FORMAT(32X,'AL=',F4.0)
         GO TO 936
      END IF
      CALL DO_STR('BB')
      CALL DO_STR('BA')
      IF(TANSW.EQ.'BB'.OR.TANSW.EQ.'BA') THEN
         PARADA(2,J_RAL)=
     &     MOD(3600.+PARADA(2,J_RAL)-PARADA(2,J_RDA),360.)
         CALL DROD1
         WRITE(TXTADW,1001)PARADA(2,J_RAL)
         CALL DWRC
         GO TO 936
      END IF
      CALL DO_STR('AO')
      IF(TANSW.EQ.'AO') THEN
C       ................................................... TURN BY 180 DEGREES
        PARADA(2,J_RAL)=MOD(PARADA(2,J_RAL)+180.,360.)
        GO TO 940
      END IF
      CALL DO_STR('RP:Rotate axis perpendicular to screen and back')
      IF(TANSW.EQ.'RP') THEN
        IF(PARADA(2,J_PTE).LT.90.) THEN
          PARADA(2,J_PTE)=PARADA(2,J_PTE)+90.
        ELSE
          PARADA(2,J_PTE)=PARADA(2,J_PTE)-90.
        END IF
        PARADA(2,J_RAL)=0.
        GO TO 940
      END IF
      CALL DO_STR('AF')
      IF(TANSW.EQ.'AF') THEN
C       ................................................... TURN BY  90 DEGREES
        PARADA(2,J_RAL)=MOD(PARADA(2,J_RAL)+ 90.,360.)
        GO TO 940
      END IF
      CALL DO_STR('AB')
      IF(TANSW.EQ.'AB') THEN
C       ................................................... TURN BY -90 DEGREES
        PARADA(2,J_RAL)=MOD(PARADA(2,J_RAL)+270.,360.)
        GO TO 940
      END IF
      CALL DO_STR('AX')
      IF(TANSW.EQ.'AX') THEN
C       .............. ...................... CHOOSE AXIS
        PARADA(4,J_RDY)=-1.
        GO TO 930
      END IF
      CALL DO_STR('DV')
      IF(TANSW.EQ.'DV') THEN
C       .............. ...................... SEC. VERTEX DIRECTION OF MOMENTUM
        CALL DVXROV
        NAX=2
        PARADA(4,J_RDY)=-1.
        GO TO 930
      END IF
      CALL DO_STR('DT')
      IF(TANSW.EQ.'DT') THEN
C       .................................................... DIRECTION OF TRACK
        IF(PARADA(2,J_RTN).LE.0.) THEN
          CALL DWRT('Select track !')
          GO TO 936
        ELSE
          CALL DROTRK(IFIX(PARADA(2,J_RTN)))
          NAX=3
          PARADA(4,J_RDY)=-1.
          GO TO 930
        END IF
      END IF
      CALL DO_STR('RJ')
      IF(TANSW.EQ.'RJ') THEN
        CALL DROJTC(PARADA(1,J_RJN))
        PARADA(4,J_RDY)=-1.
        NAX=4
        GO TO 930
      END IF
      CALL DO_STR('PV: axis through primary vertex')
      IF(TANSW.EQ.'PV') THEN
        CALL DTD_PV
        GO TO 930
      END IF
      CALL DO_STR('DJ')
      IF(TANSW.EQ.'DJ') THEN
C       ...................................................... DIRECTION OF JET
        CALL DROJTC(PARADA(1,J_RJN))
        IF(PARADA(4,J_RDY).EQ.-1.) THEN
          CALL DVXPV
          CALL DROJTA(IFIX(PARADA(2,J_RJN)),
     &      PARADA(2,J_PFI),PARADA(2,J_PTE) )
        END IF
        NAX=4
        PARADA(4,J_RDY)=-1.
        GO TO 930
      END IF
C     ...................................... Show jet direction in THETA/PHI
      CALL DO_STR('SJ')
      IF(TANSW.EQ.'SJ') THEN
        CALL DPCPAR(29,IAREDO,IPAR)
        IF(IPAR.GE.0) THEN
          CALL DQSET(IPAR,0.,0.)
          CALL DROJTA(IFIX(PARADA(2,J_RJN)),AF,AT)
          CALL DQPOC(-AT,AF,HCU,VCU,FCU)
          IF(FCU) THEN
            CALL DGSPPT(MOCJ,MOC2)
            CALL DGSCUR(HCU,VCU)
          ELSE
            CALL DWRT('FI,TE outside.')
          END IF
        ELSE
          CALL DWRT('No full FT picture on the screen.')
        END IF
        GO TO 936
      END IF
      CALL DO_STR('D0')
      IF(TANSW.EQ.'D0') THEN
C       ............................................................. THROUGH 0
        PARADA(2,J_RDY)=0.
        PARADA(4,J_RDY)=1.
        GO TO 930
      END IF
      CALL DO_STR('DD')
      IF(TANSW.EQ.'DD') THEN
        PARADA(2,J_PFI)=FIMXDE
        PARADA(2,J_PTE)=TEMXDE
        GO TO 930
      END IF
C     ........................................... Standard projections XY,YZ,XZ
      CALL DO_STR_LIST(6,TPVW,'projection')
      DO 700 N=1,6
         IF(TANSW.EQ.TPVW(N))THEN
            PARADA(2,J_PFI)=FIVW(N)
            PARADA(2,J_PTE)=TEVW(N)
            PARADA(2,J_RAL)=ALVW(N)
            CALL DWR_ADD(':')
            CALL DROD
            GO TO 930
         END IF
  700 CONTINUE
      CALL DO_STR_LIST(9,TM,'orientation')
      DO 710 I=1,9
        IF(TANSW.EQ.TM(I)) THEN
          PARADA(2,J_RRM)=I
          GO TO 936
        END IF
  710 CONTINUE
      CALL DO_STR('SS')
      IF(TANSW.EQ.'SS') THEN
        CALL DROSMR(TANSW)
        GO TO 930
      END IF
      CALL DO_STR('PS')
      IF(TANSW.EQ.'PS') THEN
        CALL DROSMR(TANSW)
        GO TO 930
      END IF
      CALL DO_STR('AC: smooth rotation of data and artificial cube.')
      IF(TANSW.EQ.'AC') THEN
        CALL DRO_SMOOTH_ROT_AC
        GO TO 930
      END IF
      CALL DO_STR('AD: smooth rotation of data with art, cube fixed')
      IF(TANSW.EQ.'AD') THEN
        CALL DRO_SMOOTH_ROT_AC_FIXED
        GO TO 930
      END IF
      CALL DO_STR('AR: smooth rotation of art.cube with data fixed')
      IF(TANSW.EQ.'AR') THEN
        CALL DRO_SMOOTH_ROT_AC_ONLY
        GO TO 930
      END IF
      CALL DO_STR('AI: smooth rot. of a.c. + data with inclined axis')
      IF(TANSW.EQ.'AI') THEN
        CALL DRO_SMOOTH_ROT_AC_INCLINED
        GO TO 930
      END IF
      CALL DO_STR('A0: set default angles of artificial cube')
      IF(TANSW.EQ.'A0') THEN
        PARADA(4,J_RFA)=1.
        PARADA(2,J_RFA)=RFA
        PARADA(2,J_RTA)=RTA
        PARADA(2,J_RGA)=RGA
        PARADA(2,J_RBA)=RBA
        GO TO 930
      END IF
      CALL DO_STR('MM')
      IF(TANSW.EQ.'MM') THEN
        CALL DTDM
        GO TO 930
      END IF
      CALL DO_STR('FM')
      IF(TANSW.EQ.'FM') THEN
        CALL DTDMF
        GO TO 930
      END IF
      IF(TANSW.EQ.'X?') THEN
        CALL DO_TY_COMMAND_LIST('DKRO')
        GO TO 936
      END IF
      CALL DWR_IC(TANSW)
      GO TO 936
  940 PARADA(2,J_PL1)=0.
      PARADA(2,J_PL2)=GT3
      CALL DROD
      IF(FCHG) GO TO 930
      GO TO 936
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DKRO0
CH
      ENTRY DKRO0
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      CALL DROJT0
C      PAR2DR(3,3)=0.
      CALL DPARSV(43,'RJN',3,0.)
      END
*DK DKRS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DKRS
CH
      SUBROUTINE DKRS
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
C INVOKED BY TANSW.EQ.'RS'  (DKRS)
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
C      CHARACTER *2 TPR1(4)
C      DATA TPR1/'DF','DZ','VD','IT'/
      CHARACTER *2 TPR2(3)
      DATA TPR2/'TR','BC','FC'/
      CHARACTER *2 TANSW
      LOGICAL FYES,FCHG
      CHARACTER *51 T1,T2
C                       1         2         3         4         5
C              123456789012345678901234567890123456789012345678901
      DATA T1/'P?:W?:   DF$1234 DZ$1234   VD$123 IT$123   TR_123'/
      DATA T2/'Background colour:BC$12  Foreground colour: FC$12'/
      PARADR(3,1)=BNUMDB(2,FRTLDB)
      PARADR(2,1)=MIN(PARADR(2,1),PARADR(3,1))
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_STC'
      CALL DPARAM(31
     &  ,J_STC)
      CALL DPARP0
      CALL DPAROP(31,'SDF_SDZ_SVD_SIT')
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
  930 CALL DO_BAR_STATUS_0
      CALL DTYPT('SAVE',' '   ,N_1_DA,N_2_DA,PARADA,TPOPDA,T1)
      CALL DTYPT('TYPE',TPICDO,   1  ,   4  ,PARADR,TPR2  ,T1)
      IF(FGRYDR)
     &  CALL DTYPT('TYPE', ' ',   1  ,   3  ,PARADR,TPR2  ,T2)
      IF(PARADR(3,1).LT.1.)
     &   CALL DWRT('The selected event has no tracks!')
  936 FCHG=.FALSE.
      CALL DGZOOM(6,IAREDO,0,0)
C     PTR=PARADR(2,1)
      CALL DOPER(NTVIDT,1,
     &  N_1_DA,N_2_DA,TPOPDA,PARADA,
     &  1,3,TPR2,PARADR,
     &  NEXEC,FCHG,TANSW)
C      IF(PARADR(2,1).NE.PTR) CALL DVFTR(IFIX(PARADR(2,1)),NPNTDR)
      CALL DGZOOM(6,-1,0,0)
      CALL DO_STR('PI')
      IF(TANSW.EQ.'PI') THEN
        NTRKDP=0
        CALL DPOS(TANSW,PAF,PAT,NEXEC,FCHG)
        IF(NTRKDP.GT.0) THEN
          PARADR(2,1)=NTRKDP
          CALL DVFTR(IFIX(PARADR(2,1)),NPNTDR)
        END IF
      END IF
      GO TO (910,920,930,940),NEXEC
  910 PARADA(4,J_STC)=PARADR(4,2)
      RETURN
  920 CALL DO_STR('TC: type coordinates')
      IF(TANSW.EQ.'TC') THEN
        CALL DRSTYP
        GO TO 930
      END IF
      CALL DO_STR('TY: type residuals')
      IF(TANSW.EQ.'TY') THEN
        CALL DRS_TYP_RES
        GO TO 936
      END IF
      CALL DO_STR('MT')
      IF(TANSW.EQ.'MT') THEN
        CALL DRSTIM
        GO TO 936
      END IF
      CALL DO_STR('SB')
      CALL DO_STR('SN')
      IF(TANSW.EQ.'SB'.OR.TANSW.EQ.'SN') THEN
        CALL DRSCOL(TANSW)
        GO TO 930
      END IF
      CALL DAREA('D',TANSW,0,12,IAREDO,FYES)
      IF(FYES) GO TO 943
      IF(TANSW.EQ.'X?') THEN
        CALL DO_TY_COMMAND_LIST('DKRS')
        GO TO 936
      END IF
      CALL DWR_IC(TANSW)
      GO TO 936
C     ....................... 940 IF(TANSW(1:1).EQ.'C') THEN
  940 CALL DRSD
      CALL DRSCTR
      GO TO 930
C     .......................      END IF
  943 CALL DSC0
      NAR=IAREDO
      PARADA(4,J_STC)=PARADR(4,2)
      DO M=0,MPNWDW
        IF(NWINDW(M,NAR).EQ.-2.OR.NWINDW(M,NAR).EQ.1)
     &    CALL DPACGT(31,PSTODS(1,1,M,IWUSDO))
      END DO
      CALL DRSCTR
      CALL DCOPTL
      CALL DPCEAR(NAR)
      CALL DCOPFL
      IAREDO=NAR
      GO TO 936
      END
*DK DKRZ
CH..............+++
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DKRZ
CH
      SUBROUTINE DKRZ
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
C INVOKED BY TANSW.EQ.'RZ'  (DKRZ)
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *2 TANSW
      DIMENSION HRB(4),VRB(4)

      CHARACTER *2 TRB(4)
      DATA TRB/'**','SQ','**','NS'/

      LOGICAL FCHG
      LOGICAL FRET,FYES
      CHARACTER *49 T1,T2,T3
C                       1         2         3         4         5
C              12345678901234567890123456789012345678901234567890
      DATA T1/'P?:W?:Z?:SQ AS$1234 FI_12345 CF$12345 FR_1234 >[]'/
      DATA T2/' SR+        DS$1234 TE_12345 CT$12345 TO_1234 <[]'/
      DATA T3/' IT$1       SE$1234 SH$1234 HD$12345'/
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_PAS,J_PBZ,J_PZ0,J_PR0,J_PHM,J_PDH'
      CALL DPARAM(10
     &  ,J_PFI,J_PTE,J_PAS,J_PBZ,J_PZ0,J_PR0,J_PHM,J_PDH)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DPARP0
      CALL DPAROP(10,'PFI_PCF_PTE_PCT_PCC_PIT')
      CALL DPAROP(10,'PFR_PTO_PAS_PDS_PSE_PSH_PDH')

      IF(FGETDO) THEN
        IF(PARADA(2,J_PFI).NE.FIOLD) PARADA(4,J_PFI)=1.
        CALL DDRCAL('IN','RZ')
      END IF
      CALL DRZ_DT_TO_AS
      MODEB=PARADA(2,J_PBZ)

      FIOLD=PARADA(2,J_PFI)

  930 CALL DO_BAR_STATUS_0

      IF(MODEB.NE.2.AND.IZOMDO.EQ.1) CALL DO_BAR_STATUS('$A',0,T1)
      CALL DO_BAR_STATUS(TRB(MODEB),10,T1)
      CALL DTYPT('TYPE',TPICDO,N_1_DA,N_2_DA,PARADA,TPOPDA,T1)
      IF(     PARADA(4,J_PTE).EQ.0.) THEN
        T2(2:4)='PR '
      ELSE IF(PARADA(4,J_PTE).GT.0.) THEN
        T2(2:4)='SR+'
      ELSE
        T2(2:4)='SR-'
      END IF
      CALL DTYPT('TYPE',' '   ,N_1_DA,N_2_DA,PARADA,TPOPDA,T2)
      IF(PARADA(4,J_PDH).EQ.1.)
     &  CALL DTYPT('TYPE',' '   ,N_1_DA,N_2_DA,PARADA,TPOPDA,T3)
      CALL DGRBTY

  936 FCHG=.FALSE.
      CALL DGZOOM(6,IAREDO,0,0)
      CALL DOPER(NTVIDT,1,
     &  N_1_DA,N_2_DA,TPOPDA,PARADA,
     &  1,0,' ',0,
     &  NEXEC,FCHG,TANSW)

      CALL DDRCAL('IN','RZ')
      IF(PARADA(2,J_PFI).NE.FIOLD) THEN
         PARADA(4,J_PFI)=1.
         FIOLD=PARADA(2,J_PFI)
      END IF

      GO TO (910,920,930,940),NEXEC
  910 IF(TANSW.NE.'GW') CALL DRZ_AS_TO_DT
      RETURN
  920 CALL DO_STR('PR')
      IF(TANSW.EQ.'PR') THEN
C     SINGLE MODE,DOUBLE MODE
         PARADA(4,J_PTE)=0.
         GO TO 936
      END IF
      CALL DO_STR('SR')
      IF(TANSW.EQ.'SR') THEN
         PARADA(4,J_PTE)=1.
         GO TO 936
      END IF

      IF(TANSW.EQ.'RB') THEN
        CALL DPCGV(IAREDO,10,'PDS',4,PDS4)
        IF(PDS4.GT.0.) THEN
          CALL DWRT('No rubber band if fish eye transformation.#')
          GO TO 936
        END IF
      END IF

      CALL DO_STR('RB"NS"SQ: rubberband, zoom modes')
      CALL DQ_ZOOM(TRB,TANSW,J_PZ0,J_PR0,0.,
     &  HRB,VRB,MODEB,NYES)
      PARADA(2,J_PBZ)=MODEB
      IF(NYES.EQ.1) GO TO 936
      IF(NYES.EQ.2) GO TO 930
      IF(NYES.GT.2) THEN
C       .............................. cone is given by FR,TO,TE,(FI),AS
        CALL DRZ_HV_TO_CONE(HRB,VRB)
        IZOMDO=1
        CALL DDRCAL('RB','RZ')
        GO TO 930
      END IF
      CALL DO_STR('FB"DB"CB: fix/display/clear rubber band box')
      IF(TANSW.EQ.'FB'.OR.TANSW.EQ.'DB'.OR.TANSW.EQ.'CB') THEN
C       .............................. cone is given by FR,TO,TE,(FI),AS
        CALL DRZ_CONE_TO_HV(HRB,VRB)
C       ........................... vrb -> -vrb if fi different by more then 90
        FW=PSTODS(1,J_PFI,IAREDO,0)
        FI=PARADA(2,J_PFI)
        FI=DFINXT(FW,FI)
        IF(FI.LT.FW-90..OR.FI.GT.FW+90.) THEN
          DO K=1,4
            VRB(K)=-VRB(K)
          END DO
        END IF
        CALL DQZO_DISPLAY(TANSW,HRB,VRB)
        IF(FCHG) GO TO 930
        GO TO 936
      END IF

      CALL DO_STR('NZ')
      IF(TANSW.EQ.'NZ') THEN
         IZOMDO=0
         CALL DDRCAL('NZ','RZ')
         GO TO 930
      END IF
      CALL DO_STR('ZO')
      IF(TANSW.EQ.'ZO') THEN
        IZOMDO=1
        GO TO 930
      END IF

      IF(TANSW(1:1).EQ.'Z') THEN
C       .......................................................... B3,R3,F3 M3
        CALL DGRB(TANSW,FRET)
        IF(FRET) GO TO 930
      END IF
      CALL DO_STR('AF')
      IF(TANSW.EQ.'AF') THEN
        CALL DRZFI(PARADA(2,J_PFI))
        GO TO 930
      END IF
      CALL DO_STR('HA')
      IF(TANSW.EQ.'HA') THEN
        PARADA(4,J_PDH)= 1.
        GO TO 930
      END IF
      CALL DO_STR('HO')
      IF(TANSW.EQ.'HO') THEN
        PARADA(4,J_PDH)=-1.
        GO TO 930
      END IF
      CALL DO_STR('NH')
      IF(TANSW.EQ.'NH') THEN
        PARADA(4,J_PHM)=-1.
        GO TO 936
      END IF
      CALL DO_STR('HH')
      IF(TANSW.EQ.'HH') THEN
        PARADA(4,J_PHM)= 1.
        GO TO 936
      END IF
      CALL DM2D(TANSW,FYES)
      IF(FYES) GO TO 930
      CALL DO_STR('FL')
      IF(TANSW.EQ.'FL') THEN
        CALL DM2DF
        GO TO 936
      END IF

      IF(TANSW.EQ.'X?') THEN
        CALL DO_TY_COMMAND_LIST('DKRZ')
        GO TO 936
      END IF
      CALL DWR_IC(TANSW)
      GO TO 936
  940 CALL DRZD
      GO TO 930
      END
*DK DKSC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DKSC
CH
      SUBROUTINE DKSC
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
CH
C INVOKED BY TANSW.EQ.'SC'  (DKSC)
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *2 TANSW
      PARAMETER (MV=9)
      CHARACTER *2 TSID(-1:1),TPRO(MV),TMO(4)
      DATA TSID /'B ','AB',' A'/
C     ........... PU: PUZZLE PLOT .. PH: PUZZLE horizontal .. SP: SPINDLE PLOT
      DATA TPRO/'XY','FR','RZ','FZ','PU','PH','SP','PE','HX'/
      DATA TMO/'SQ','LI','NU','NF'/
C      PARAMETER (MP=18,MR=2)
C      CHARACTER *2 TP(MP),TR(MR)
CC               1    2    3    4    5    6    7    8    9   10
C      DATA TP/'FO','FL','TL','SC','SR','SH','SF','FH','FM','R1',
C     &        'E1','E2','CN','NR','NP','MC','CG','CH'/
C      DATA TR/'FI','DF'/
      DATA N2/2/,IAR/11/
      DIMENSION JCOL(0:21)
      LOGICAL FCHG
      CHARACTER *49 T(5),TALL,TCOL
C                123456789 123456789 123456789 123456789 123456789
      DATA TALL/'P?:W?:Z?:Sical RZ Side AB CN$12 E1$1234 E2$-23 SQ'/
      DATA T(1)/'FI_123 R1_12 FO$- CG$12 SC$1234 FL$12 TL$12 MC$-1'/
      DATA T(2)/'FM$123 DF_123    CG$12  SC$1234 FL$12 TL$12 MC$-1'/
      DATA T(3)/'SH$1234 SR$1234  CG$12  FH$12'/
      DATA T(4)/'FM$123 DF_123    CG$12  NR$12 SF$1234'/
      DATA T(5)/'FM$123 DF_123   NP_123  NR$12'/
      DATA TCOL/'constant colour of hits: CH$12'/
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DPARP0
      TPARDA=
     &  'J_PFI,J_PTE,J_PAB'
      CALL DPARAM(10
     &  ,J_PFI,J_PTE,J_PAB)
      TPARDA=
     &  'J_CFL,J_CTL,J_CFM,J_CCH,J_CPR,J_CSY,J_CZZ'
      CALL DPARAM(40
     &  ,J_CFL,J_CTL,J_CFM,J_CCH,J_CPR,J_CSY,J_CZZ)
      CALL DPAROP(11,'PFI_PDF')
      CALL DPAROP(40,'CFO_CFL_CTL_CSX_CSR_CSH_CSF_CFH_CFM_CR1')
      CALL DPAROP( 0,'CE1_CE2_CCN_CNR_CNP_CMC_CCG_CCH')
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      PARADA(4,J_CZZ)=-1.
      PARADA(4,J_CFL)=SIGN(2.,PARADA(4,J_CFL))
  930 CALL DO_BAR_STATUS_0

      NPR=PARADA(2,J_CPR)
      CALL DO_BAR_STATUS(TPRO(NPR) ,16,TALL)
C      TALL(16:17)=TPRO(NPR)

      NSID=PARADA(2,J_PAB)
      CALL DO_BAR_STATUS(TSID(NSID),24,TALL)
C      TALL(24:25)=TSID(NSID)

      NMO=PARADA(2,J_CSY)
      CALL DO_BAR_STATUS(TMO(NMO),  48,TALL)
C      TALL(48:49)=TMO(NMO)

      IF(NPR.EQ.9) THEN
        NPRT=1
      ELSE
        NPRT=MIN(NPR,5)
      END IF
      CALL DTYPT('TYPE',TPICDO,N_1_DA,N_2_DA,PARADA,TPOPDA,TALL)
      CALL DTYPT('TYPE',' '   ,N_1_DA,N_2_DA,PARADA,TPOPDA,T(NPRT))
      IF(PARADA(4,J_CCH).GT.0.)
     &  CALL DTYPT('TYPE',' ' ,N_1_DA,N_2_DA,PARADA,TPOPDA,TCOL)
  936 FCHG=.FALSE.
      CALL DGZOOM(6,IAREDO,0,0)
      CALL DOPER(1,N2,
     &  N_1_DA,N_2_DA,TPOPDA,PARADA,
     &  1,0,' ',0,
     &  NEXEC,FCHG,TANSW)
      CALL DGZOOM(6,-1,0,0)
      GO TO (910,920,930,940),NEXEC
  910 RETURN
  920 CALL DO_STR_LIST(MV,TPRO,'projection')
      DO N=1,MV
        IF(TANSW.EQ.TPRO(N)) THEN
          PARADA(2,J_CPR)=N
          GO TO 930
        END IF
      END DO
      CALL DO_STR('NZ')
      IF(TANSW.EQ.'NZ') THEN
         IZOMDO=0
         GO TO 930
      END IF
      CALL DO_STR('ZO')
      IF(TANSW.EQ.'ZO') THEN
        IZOMDO=1
        GO TO 930
      END IF
      CALL DO_STR('SA')
      IF(TANSW.EQ.'SA') THEN
        PARADA(2,J_PAB)= 1.
        GO TO 930
      END IF
      CALL DO_STR('SB')
      IF(TANSW.EQ.'SB') THEN
        PARADA(2,J_PAB)=-1.
        GO TO 930
      END IF
      CALL DO_STR('AB')
      IF(TANSW.EQ.'AB') THEN
        PARADA(2,J_PAB)= 0.
        GO TO 930
      END IF
      CALL DO_STR('SQ')
      IF(TANSW.EQ.'SQ') THEN
        PARADA(2,J_CSY)= 1.
        GO TO 930
      END IF
      CALL DO_STR('LI')
      IF(TANSW.EQ.'LI') THEN
        PARADA(2,J_CSY)= 2.
        GO TO 930
      END IF
      CALL DO_STR('NU')
      IF(TANSW.EQ.'NU') THEN
        PARADA(2,J_CSY)= 3.
        GO TO 930
      END IF
      CALL DO_STR('NF')
      IF(TANSW.EQ.'NF') THEN
        PARADA(2,J_CSY)= 4.
        GO TO 930
      END IF
      CALL DO_STR('AL')
      IF(TANSW.EQ.'AL') THEN
        PARADA(4,J_CFL)=-2.
        PARADA(4,J_CTL)=-1.
        GO TO 930
      END IF
      CALL DO_STR('TY')
      IF(TANSW.EQ.'TY') THEN
        CALL DSCHST(JCOL)
        GO TO 936
      END IF
      CALL DO_STR_1_LET_LIST('Z',12,TWINDW(0))
      IF(TANSW(1:1).EQ.'Z') THEN
        IF(PARADA(2,J_CPR).LT.4.) THEN
          CALL DWRT('select Puzzle-plot.')
          GO TO 936
        END IF
        DO I=0,12
          IF(TANSW(2:2).EQ.TWINDW(I)) THEN
            IAR=I
            GO TO 200
          END IF
        END DO
      END IF
      CALL DO_STR('ZZ')
      IF(TANSW.EQ.'ZZ') GO TO 200
      CALL DO_STR('FF')
      IF(TANSW.EQ.'FF') THEN
        CALL DGGCUR(HCD,VCD)
        CALL DPOAR(HCD,VCD,NAR)
        IF(ISTODS(5,NAR,IWUSDO).EQ.IPICDO.AND.
     &    (PSTODS(1,J_CPR,NAR,IWUSDO).EQ.2.OR.
     &     PSTODS(1,J_CPR,NAR,IWUSDO).GE.4.) ) THEN
          CALL DQINV(NAR,HCD,VCD,HCU,VCU)
          PARADA(2,J_CFM)=VCU
          PARADA(4,J_CFM)=1.
          GO TO 940
        ELSE
          CALL DWRT('Wrong image.')
          GO TO 936
        END IF
      END IF
      CALL DO_STR('RO')
      IF(TANSW.EQ.'RO') THEN
        CALL DROD
        GO TO 930
      END IF
      CALL DWR_IC(TANSW)
      GO TO 936
  200 CALL DGGCUR(HCD,VCD)
      CALL DPOAR(HCD,VCD,NAR)
      IF(ISTODS(5,NAR,IWUSDO).EQ.IPICDO.AND.
     &   PSTODS(1,J_CPR,NAR,IWUSDO).GE.4.) THEN
        CALL DQINV(NAR,HCD,VCD,HCU,VCU)
        PARADA(2,J_CFM)=VCU
        PARADA(2,J_CZZ)=HCU
        PARADA(4,J_CZZ)=1.
        IAOLD=IAREDO
        IAREDO=IAR
        CALL DSCD
        IAREDO=IAOLD
        PARADA(4,J_CZZ)=-1.
      ELSE
        CALL DWRT('Move cursor to right image.')
      END IF
      IF(TANSW.EQ.'X?') THEN
        CALL DO_TY_COMMAND_LIST('DKSC')
        GO TO 936
      END IF
      GO TO 936
  940 CALL DSCD
      GO TO 930
      END
*DK DKYX
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DKYX
CH
      SUBROUTINE DKYX
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
C INVOKED BY TANSW.EQ.'XY'  (DKYX)
C     PROJECTION PARAMETERS:
C     1: no zoom:  1 parameter = max radius = side length of square = to
C     2: zoom : a) no rotation centered: user area = square ;
C                  3 parameters= square from .. to .. with center at phi.
C               b) no rotation uncentered:
C               c) rotation centered: 4 parameters: rectangle defined
C                  by from .. to .. aspect ratio with center at phi.
C                  The aspect ratio is calculated at start from DF.
C                  DF is calculated at exit from the aspect ratio.
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
C      DIMENSION TRAK(9)
      CHARACTER *2 TANSW
      DIMENSION HRB(4),VRB(4)
      CHARACTER *2 TRB(4)
      DATA TRB/'**','SQ','NS','**'/
      CHARACTER *2 TSID(-1:1),TRO
      DATA TSID/'B ','AB','A '/,TRO/'RO'/
      DATA RLCAL/55./,RSCAL/15./,RMCAL/595./
      CHARACTER *15 THIST(3)
      DATA THIST/'endcap only    ',
     &           'barrel + endcap',
     &           'barrel only    '/
      LOGICAL FCHG
      DATA RRZ/180./DSIGN/9./,DSYM/6/,RRZM/350./,GRZ/.95/
      LOGICAL FRET
      CHARACTER *49 T1,T2,T3,T4,T5
C                       1         2         3         4         5
C              12345678901234567890123456789012345678901234567890
      DATA T1/'P?:W?:Z?:SQ AS_1234 FI_12345 CF$12345 FR_1234 >[]'/
      DATA T2/'            DS$12   TE_12345 CT$12345 TO_1234 <[]'/
      DATA T3/' SE$12. SH$12.  Pad histogram for barrel + endcap'/
      DATA T4/' SW$123  NB$1  BW$1  MW$123  for ECAL wires.     '/
C
      DATA T5/'SICAL/LCAL AB set par. in GT:SC   CE=go to normal'/
C
      NFT=0
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_PTO,J_PAS,J_PBX,J_PX0,J_PY0'
      CALL DPARAM(10
     &  ,J_PFI,J_PTE,J_PTO,J_PAS,J_PBX,J_PX0,J_PY0)

      TPARDA=
     &  'J_PSE,J_PSH,J_PSW,J_PHM'
      CALL DPARAM(12
     &  ,J_PSE,J_PSH,J_PSW,J_PHM)

       TPARDA=
     &  'J_PAB'
       CALL DPARAM(14
     &  ,J_PAB)

       TPARDA=
     &  'J_CFL'
       CALL DPARAM(40
     &  ,J_CFL)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::

      CALL DPARP0
      CALL DPAROP(10,'PFI_PCF_PTE_PCT_PCC_PFR_PTO_PAS_PDS')
      CALL DPAROP( 0,'PSE_PSH_PNB_PSW_PBW_PMW_PLM')
      IF(FGETDO) THEN
         CALL DDRCAL('IN',TRO)
         CALL DYX_DF_TO_AS
      END IF
      MODEB=PARADA(2,J_PBX)

  930 CALL DO_BAR_STATUS_0

      IF(MODEB.NE.2.AND.IZOMDO.EQ.1) CALL DO_BAR_STATUS('$A',0,T1)
      CALL DO_BAR_STATUS(TRB(MODEB),10,T1)
      NLAY=ABS(PARADA(4,J_CFL))
      IF(NLAY.EQ.1) THEN
        T1(47:49)='>[]'
        T2(47:49)='<[]'
        T2(13:17)='DS$12'
      ELSE
        T1(47:49)=' '
        T2(47:49)=' '
        T2(13:17)=' '
      END IF
      CALL DTYPT(    'TYPE',TPICDO,N_1_DA,N_2_DA,PARADA,TPOPDA,T1)
      CALL DTYPT(    'TYPE',' '   ,N_1_DA,N_2_DA,PARADA,TPOPDA,T2)
      NHST=PARADA(2,J_PHM)
      IF(NHST.GT.0) THEN
        T3(35:49)=THIST(NHST)
        CALL DTYPT(  'TYPE',' '   ,N_1_DA,N_2_DA,PARADA,TPOPDA,T3)
      END IF
      IF(ABS(PARADA(4,J_PSW)).EQ.2.)
     &  CALL DTYPT(  'TYPE',' '   ,N_1_DA,N_2_DA,PARADA,TPOPDA,T4)
      IF(ABS(PARADA(4,J_CFL)).EQ.2.) THEN
        LSID=PARADA(2,J_PAB)
        T5(12:13)=TSID(LSID)
        CALL DWRT(                                          T5)
      END IF
      CALL DGRBTY

  936 FCHG=.FALSE.
      CALL DOPER(NTVIDT,1,
     &  N_1_DA,N_2_DA,TPOPDA,PARADA,
     &  1,0,' ',0,
     &  NEXEC,FCHG,TANSW)
      CALL DDRCAL('OP',TRO)

      GO TO (910,920,930,940),NEXEC
  910 IF(TANSW.NE.'GW') CALL DYX_AS_TO_DF
      RETURN

  920 IF(TANSW.EQ.'RB') THEN
        CALL DPCGV(IAREDO,10,'PDS',4,PDS4)
        IF(PDS4.GT.0.) THEN
          CALL DWRT('No rubber band if fish eye transformation.#')
          GO TO 936
        END IF
      END IF

      CALL DO_STR('RB"NS"SQ: rubberband, zoom modes')
      CALL DQ_ZOOM(TRB,TANSW,J_PX0,J_PY0,0.,HRB,VRB,MODEB,NYES)
      PARADA(2,J_PBX)=MODEB

      IF(NYES.EQ.1) GO TO 936
      IF(NYES.EQ.2) GO TO 930
      IF(NYES.GT.2) THEN
        CALL DYX_HV_TO_CONE(HRB,VRB)
        CALL DDRCAL('RB',TRO)
        IZOMDO=1
        GO TO 930
      END IF

      CALL DO_STR('FB"DB"CB: fix/display/clear rubber band box')
      IF(TANSW.EQ.'FB'.OR.TANSW.EQ.'DB'.OR.TANSW.EQ.'CB') THEN
        CALL DDRFLG
        IF(FNOPDR) GO TO 936
        CALL DYX_CONE_TO_HV(HRB,VRB)        
        CALL DQZO_DISPLAY(TANSW,HRB,VRB)
        IF(FCHG) GO TO 930
        GO TO 936
      END IF

      CALL DO_STR('NZ')
      IF(TANSW.EQ.'NZ') THEN
         IZOMDO=0
         CALL DDRCAL('NZ',TRO)
         GO TO 930
      END IF

      CALL DO_STR('ZO')
      IF(TANSW.EQ.'ZO') THEN
        IZOMDO=1
        GO TO 930
      END IF
      IF(TANSW(1:1).EQ.'Z') THEN
C       .......................................................... B3,R3,F3
        CALL DGRB(TANSW,FRET)
        IF(FRET) GO TO 930
      END IF
      CALL DO_STR('CE')
      IF(TANSW.EQ.'CE') THEN
        PARADA(4,J_CFL)=SIGN(1.,PARADA(4,J_CFL))
        PARADA(2,J_PTO)=RMCAL
        TRO='RO'
        CALL DDRNUM(J_PTO)
        CALL DDRCAL('OP',TRO)
        GO TO 930
      END IF
      CALL DO_STR('SL')
      IF(TANSW.EQ.'SL') THEN
        PARADA(4,J_CFL)=SIGN(2.,PARADA(4,J_CFL))
        PARADA(2,J_PTO)=RLCAL
        TRO='  '
        GO TO 930
      END IF
      CALL DO_STR('SI')
      IF(TANSW.EQ.'SI') THEN
        PARADA(4,J_CFL)=SIGN(2.,PARADA(4,J_CFL))
        PARADA(2,J_PTO)=RSCAL
        TRO='  '
        GO TO 930
      END IF
      CALL DO_STR('SA')
      IF(TANSW.EQ.'SA') THEN
        PARADA(2,J_PAB)= 1.
        GO TO 930
      END IF
      CALL DO_STR('SB')
      IF(TANSW.EQ.'SB') THEN
        PARADA(2,J_PAB)=-1.
        GO TO 930
      END IF
      CALL DO_STR('AB')
      IF(TANSW.EQ.'AB') THEN
        PARADA(2,J_PAB)= 0.
        GO TO 930
      END IF
      CALL DO_STR('NH')
      IF(TANSW.EQ.'NH') THEN
        PARADA(4,J_PHM)=-1.
        GO TO 936
      END IF
      CALL DO_STR('HH')
      IF(TANSW.EQ.'HH') THEN
        PARADA(4,J_PHM)= 1.
        GO TO 936
      END IF
      CALL DO_STR('HO')
      IF(TANSW.EQ.'HO') THEN
        PARADA(2,J_PHM)=0.
        GO TO 930
      END IF
      CALL DO_STR('HB')
      IF(TANSW.EQ.'HB') THEN
        PARADA(2,J_PHM)=3.
        GO TO 930
      END IF
      CALL DO_STR('HE')
      IF(TANSW.EQ.'HE') THEN
        PARADA(2,J_PHM)=1.
        GO TO 930
      END IF
      CALL DO_STR('HA')
      IF(TANSW.EQ.'HA') THEN
        PARADA(2,J_PHM)=2.
        GO TO 930
      END IF
      CALL DO_STR('WO')
      IF(TANSW.EQ.'WO') THEN
        IF(PARADA(4,J_PSW).GT.0.) THEN
          PARADA(4,J_PSW)=1.
        ELSE
          PARADA(4,J_PSW)=-1.
        END IF
        GO TO 930
      END IF
      CALL DO_STR('WI')
      IF(TANSW.EQ.'WI') THEN
        IF(PARADA(4,J_PSW).GT.0.) THEN
          PARADA(4,J_PSW)=2.
        ELSE
          PARADA(4,J_PSW)=-2.
        END IF
        GO TO 930
      END IF
      CALL DO_STR('RZ')
      IF(TANSW.EQ.'RZ') THEN
        FI=PARADA(2,J_PFI)-90.
        SF=SIND(FI)
        CF=COSD(FI)
        R=PARADA(2,J_PTO)
        HRZ=8.*R*CF
        VRZ=8.*R*SF
        DLINDD=PDCODD(2,LIDTDD)
        CALL DQLEVL(ICRZDD)
        CALL DQSET(IAREDO,0.,0.)
        CALL DQL2E(HRZ,VRZ,-HRZ,-VRZ)
        DLINDD=PDCODD(2,LITRDD)
        IF(R.GT.RRZM) THEN
          HRZ=GRZ*R*CF
          VRZ=GRZ*R*SF
        ELSE
          HRZ=RRZ*CF
          VRZ=RRZ*SF
        END IF
        D=DSIGN/SQRT(AHSCDQ**2+AVSCDQ**2)
        DH=-D*SF
        DV= D*CF
        CALL DQSYMB(HRZ-DH,VRZ-DV,DSYM,'-')
        CALL DQSYMB(HRZ+DH,VRZ+DV,DSYM,'+')
        GO TO 936
      END IF
      CALL DO_STR('SC')
      IF(TANSW.EQ.'SC') THEN
        CALL DYXSC
        GO TO 936
      END IF
      CALL DO_STR('FS')
      IF(TANSW.EQ.'FS') THEN
        CALL DYXSCF
        GO TO 936
      END IF
      CALL DO_STR('SS')
      IF(TANSW.EQ.'SS') THEN
        CALL DYXSB
        GO TO 936
      END IF
      CALL DO_STR('FX')
      IF(TANSW.EQ.'FX') THEN
        CALL DYXSBF
        GO TO 936
      END IF
      CALL DO_STR('MM')
      IF(TANSW.EQ.'MM') THEN
        CALL DWRT('MM was modified. Type "GT:MM".')
        GO TO 936
      END IF
      IF(TANSW.EQ.'X?') THEN
        CALL DO_TY_COMMAND_LIST('DKYX')
        GO TO 936
      END IF
      CALL DWR_IC(TANSW)
      GO TO 936
  940 CALL DYX_AS_TO_DF
      CALL DYXD
      IF(PARADA(4,J_PSE).EQ.-1..AND.PARADA(2,J_PSE).NE.0.) GO TO 930
      IF(PARADA(4,J_PSH).EQ.-1..AND.PARADA(2,J_PSH).NE.0.) GO TO 930
      GO TO 930
      END

*DK DKYZ
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DKYZ
CH
      SUBROUTINE DKYZ
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
C INVOKED BY TANSW.EQ.'YZ'  (DKYZ)
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *2 TANSW
      DIMENSION HRB(4),VRB(4)
      CHARACTER *2 TRB(4)
C                                       ! NR=NO RELFECTION OF SQUARE AS=1
C                                       ! RE=NO REFLECTION OF RECTANGLE AS#1
C                                       ! RO= REFLECTION OF RECTANGLE AS#1
      DATA TRB/'NS','SQ','**','**'/
      LOGICAL FCHG,FRET
      CHARACTER *49 T1,T2
C                       1         2         3         4         5
C              12345678901234567890123456789012345678901234567890
      DATA T1/'P?:W?:Z?:SQ FY_123  FI_12345 CF$12345 FR_1234 >[]'/
      DATA T2/'T0$-12345   DY_1234 TE_12345 CT$12345 TO_1234 <[]'/
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFY,J_PFI,J_PTE,J_PAS,J_PBZ,J_RAL,J_RDY'
      CALL DPARAM(10
     &  ,J_PFY,J_PFI,J_PTE,J_PAS,J_PBZ,J_RAL,J_RDY)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DPARP0
      CALL DPAROP(10,'PFY_PFI_PCF_PTE_PCT_PCC_PFR_PTO_PAS_PT0_RDY')

      PARADA(4,J_RAL)=-1.
      IF(FGETDO) THEN
        CALL DDRCAL('IN','YZ')
        PARADA(2,J_RDY)=0.
C       CALL DYZRAS
        CALL DYZ_DF_TO_AS
      END IF
      MODEB=PARADA(2,J_PBZ)
  930 CALL DO_BAR_STATUS_0

      IF(MODEB.NE.2.AND.IZOMDO.EQ.1) CALL DO_BAR_STATUS('$A',0,T1)
      CALL DO_BAR_STATUS(TRB(MODEB),10,T1)

      CALL DTYPT('TYPE',TPICDO,N_1_DA,N_2_DA,PARADA,TPOPDA,T1)
      CALL DTYPT('TYPE',' '   ,N_1_DA,N_2_DA,PARADA,TPOPDA,T2)
      CALL DYZNUD('TYPE')
      CALL DGRBTY
      FCHG=.FALSE.

  936 CALL DOPER(NTVIDT,1,
     &  N_1_DA,N_2_DA,TPOPDA,PARADA,
     &  1,0,' ',0,
     &  NEXEC,FCHG,TANSW)

      CALL DDRCAL('OP','YZ')
      GO TO (910,920,930,940),NEXEC

C 910 IF(TANSW.NE.'GW') CALL DYZRDF
  910 IF(TANSW.NE.'GW') CALL DYZ_AS_TO_DF
      RETURN

  920 CALL DO_STR('RB"NS"SQ: rubberband, zoom modes')
      CALL DQ_ZOOM(TRB,TANSW,0,0,0.,HRB,VRB,MODEB,NYES)
      PARADA(2,J_PBZ)=MODEB

      IF(NYES.EQ.1) GO TO 936
      IF(NYES.EQ.2) GO TO 930
      IF(NYES.GT.2) THEN
C       CALL DYZRBC(HRB,VRB)
        CALL DYZ_HV_TO_CONE(HRB,VRB)
        CALL DDRCAL('RB','YZ')
        IZOMDO=1
        GO TO 930
      END IF

      CALL DO_STR('FB"DB"CB: fix/display/clear rubber band box')
      IF(TANSW.EQ.'FB'.OR.TANSW.EQ.'DB'.OR.TANSW.EQ.'CB') THEN
        CALL DDRFLG
        IF(FNOPDR) GO TO 936
C       CALL DYZRCB(HRB,VRB)
        CALL DYZ_CONE_TO_HV(HRB,VRB)        
        CALL DQZO_DISPLAY(TANSW,HRB,VRB)
        IF(FCHG) GO TO 930
        GO TO 936
      END IF

      CALL DO_STR('NZ')
      IF(TANSW.EQ.'NZ') THEN
         IZOMDO=0
         CALL DDRCAL('NZ','YZ')
         GO TO 930
      END IF
      CALL DO_STR('ZO')
      IF(TANSW.EQ.'ZO') THEN
        IZOMDO=1
        GO TO 930
      END IF
      IF(TANSW(1:1).EQ.'Z') THEN
C       .......................................................... B3,R3,F3
        CALL DGRB(TANSW,FRET)
        IF(FRET) GO TO 930
      END IF
      CALL DO_STR('F1')
      IF(TANSW.EQ.'F1') THEN
        PARADA(2,J_PFY)=0.
        GO TO 930
      END IF
      CALL DO_STR('F2')
      IF(TANSW.EQ.'F2') THEN
        PARADA(2,J_PFY)=60.
        GO TO 930
      END IF
      CALL DO_STR('F3')
      IF(TANSW.EQ.'F3') THEN
        PARADA(2,J_PFY)=120.
        GO TO 930
      END IF
      CALL DO_STR('FM')
      IF(TANSW.EQ.'FM') THEN
        CALL DYZNUD('CALC')
        GO TO 930
      END IF
      CALL DO_STR('XY')
      IF(TANSW.EQ.'XY') THEN
        PARADA(2,J_PFY)=PARADA(2,J_PFI)
        GO TO 930
      END IF
      CALL DO_STR('RZ')
      IF(TANSW.EQ.'RZ') THEN
        PARADA(2,J_PFY)=PARADA(2,J_PFI)+90.
        GO TO 930
      END IF
      IF(TANSW.EQ.'X?') THEN
        CALL DO_TY_COMMAND_LIST('DKYZ')
        GO TO 936
      END IF
      CALL DWR_IC(TANSW)
      GO TO 936
  940 CALL DYZD
      IF(FCHG) GO TO 930
      GO TO 936
      END
*DK DKTD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DKTD
CH
      SUBROUTINE DKTD
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
C INVOKED BY TANSW.EQ.'TC'  (DKTD)
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *2 TANSW
      CHARACTER *4 DT4
      LOGICAL FCHG
C                                       ! SQ=NO RELFECTION OF SQUARE AS=1
C                                       ! NS=NO REFLECTION OF RECTANGLE AS#1
C                                       ! RO= REFLECTION OF RECTANGLE AS#1
      CHARACTER *49 T1,T2,T3
C                       1         2         3         4         5
C              123456789 123456789 123456789 123456789 123456789
      DATA T1/'P?:W?:Z?:     VS_1334 FI_12345 CF$12345 FR_1234'/
      DATA T2/'CA$12 ST$12           TE_12345 CT$12345 TO_1234'/
      DATA T3/'CP$12 x=1234 y=1234 z=1234 TN_12 VN_1 JN_1 YC$123'/
C
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_RDY,J_RXX,J_RYY,J_RZZ,J_RST'
      CALL DPARAM(10
     &  ,J_PFI,J_PTE,J_RDY,J_RXX,J_RYY,J_RZZ,J_RST)
      TPARDA=
     &  'J_RTN,J_RJN,J_RYC'
      CALL DPARAM(43
     &  ,J_RTN,J_RJN,J_RYC)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DPARP0
      CALL DPAROP(10,'PFI_PCF_PTE_PCT_PCC_PFR_PTO')
      CALL DPAROP(15,'RAL_RVS_RCA_RST')
      CALL DPAROP(43,'RVN_RTN_RJN_RYC_RCP')
      PARADA(3,J_RTN)=BNUMDB(2,FRFTDB)
      PARADA(2,J_RTN)=MIN(PARADA(2,J_RTN),PARADA(3,J_RTN))
      PARADA(3,J_RST)=BNUMDB(2,FRFTDB)
      PARADA(2,J_RST)=MIN(PARADA(2,J_RST),PARADA(3,J_RST))
      P2J=0.2
      P4J=0.
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
  930 CALL DO_BAR_STATUS_0
      CALL DTYPT('TYPE',TPICDO,N_1_DA,N_2_DA,PARADA,TPOPDA,T1)
      CALL DTYPT('TYPE',' '   ,N_1_DA,N_2_DA,PARADA,TPOPDA,T2)
      T3( 9:12)=DT4(PARADA(2,J_RXX))
      T3(16:20)=DT4(PARADA(2,J_RYY))
      T3(23:26)=DT4(PARADA(2,J_RZZ))
      CALL DTYPT('TYPE',' '   ,N_1_DA,N_2_DA,PARADA,TPOPDA,T3)
      IF(PARADA(4,J_RYC).NE.P4J.OR.PARADA(2,J_RYC).NE.P2J)
     &  CALL DROJTC(PARADA(1,J_RJN))
      CALL DROJTT
  936 P4J=PARADA(4,J_RYC)
      P2J=PARADA(2,J_RYC)
      FCHG=.FALSE.
      CALL DGZOOM(6,IAREDO,0,0)
      CALL DOPER(NTVIDT,1,
     &  N_1_DA,N_2_DA,TPOPDA,PARADA,
     &  0,0,' ',0,
     1  NEXEC,FCHG,TANSW)
      CALL DGZOOM(6,-1,0,0)
      GO TO (910,920,930,940),NEXEC
  910 RETURN
  920 CALL DO_STR('NZ')
      IF(TANSW.EQ.'NZ') THEN
         IZOMDO=0
         GO TO 930
      END IF
      CALL DO_STR('ZO')
      IF(TANSW.EQ.'ZO') THEN
        IZOMDO=1
        GO TO 930
      END IF
      CALL DO_STR('DV')
      IF(TANSW.EQ.'DV') THEN
C       .............. ...................... SEC. VERTEX DIRECTION OF MOMENTUM
        CALL DVXROV
        PARADA(4,J_RDY)=-1.
        GO TO 930
      END IF
      CALL DO_STR('DT')
      IF(TANSW.EQ.'DT') THEN
C       .................................................... DIRECTION OF TRACK
        IF(PARADA(2,J_RTN).LE.0.) THEN
          CALL DWRT('Select track !')
          GO TO 936
        ELSE
          CALL DROTRK(IFIX(PARADA(2,J_RTN)))
          PARADA(4,J_RDY)=-1.
          GO TO 930
        END IF
      END IF
C     ....................................................... Calculate jet
      CALL DO_STR('RJ')
      IF(TANSW.EQ.'RJ') THEN
        CALL DROJTC(PARADA(1,J_RJN))
        GO TO 930
      END IF
      CALL DO_STR('DJ')
      IF(TANSW.EQ.'DJ') THEN
C       ...................................................... DIRECTION OF JET
        CALL DROJTC(PARADA(1,J_RJN))
        CALL DVXPV
        CALL DROJTA(IFIX(PARADA(2,J_RJN)),
     &    PARADA(2,J_PFI),PARADA(2,J_PTE) )
        PARADA(4,J_RDY)=-1.
        GO TO 930
      END IF
      CALL DO_STR('PV: axis through primary vertex')
      IF(TANSW.EQ.'PV') THEN
        CALL DTD_PV
        GO TO 930
      END IF
      CALL DO_STR('MM')
      IF(TANSW.EQ.'MM') THEN
        CALL DTDM
        GO TO 930
      END IF
      CALL DO_STR('FM')
      IF(TANSW.EQ.'FM') THEN
        CALL DTDMF
        GO TO 930
      END IF
      IF(TANSW.EQ.'X?') THEN
        CALL DO_TY_COMMAND_LIST('DKTD')
        GO TO 936
      END IF
      CALL DWR_IC(TANSW)
      GO TO 936
  940 CALL DTDD
      IF(FCHG) GO TO 930
      GO TO 936
      END
