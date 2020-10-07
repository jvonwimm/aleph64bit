*DK DWAIT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DWAIT1
CH
      SUBROUTINE DWAIT1(W)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      IF(W.GT.0.) CALL DGWAIT(W)
      END
*DK DWARNR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DWARNR
CH
      SUBROUTINE DWARNR
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
C------------------------------------------------------------------  DC
C     HARD WIRED IN P_DALB_ALEPH.FOR           !!!
C     ++++++++++++++++++++++++++ *12 TFILDC
      COMMON /DCFTVT/ TFILDC,TITLDC,TDEFDC,TNAMDC
      CHARACTER  *8 TFILDC
      CHARACTER *10 TITLDC
      CHARACTER  *3 TDEFDC
      CHARACTER  *4 TNAMDC
C------------------------------------------------------------------- DW
      COMMON /DWORKT/ TXTADW
      CHARACTER *80 TXTADW
      CHARACTER *1 TXT1DW(80)
      EQUIVALENCE (TXTADW,TXT1DW)
C------------------------------------------------------------------- DW
      COMMON /DWARFL/ FNEWDW
      LOGICAL FNEWDW
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CALL DGOPEN(NUNIDU,TFILDC//'WAR',2,*99,IER)
    1 READ(NUNIDU,'(A)',END=9) TXTADW
      LT=LENOCC(TXTADW)
      CALL DWRT(TXTADW(1:LT))
      GO TO 1
    9 CLOSE(UNIT=NUNIDU)
      IF(FNEWDW) THEN
        CALL DGPOP('PUSH')
  100   CALL DWRT(
     &    'You will be stopped here only once ! Please read ! |#')
        CALL DTYANS(
     &    '   Did you read all ?                    Y = yes.  |','Y',Na)
        IF(NA.NE.1) GO TO 100
        CALL DJFIW(' ')
      END IF
   99 END
*DK DWC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DWC
CH
      SUBROUTINE DWC
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
C INVOKED BY TANSW.EQ.9=WC'  (DWC)
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DD
      COMMON /DDEFCO/ RDCODD(0:127),GRCODD(0:127),BLCODD(0:127),
     &  LTABDD(0:127)
      COMMON /DDEFCD/ RDCDDD(0:127),GRCDDD(0:127),BLCDDD(0:127)
      COMMON /DDEFCP/ RDCPDD(0:127),GRCPDD(0:127),BLCPDD(0:127)
      COMMON /DDEFCF/ FPCODD
      LOGICAL FPCODD
      COMMON /DDEFCN/ NUMCDD
C------------------------------------------------------------------- DO
      COMMON /DOPR1T/ TPLNDO(-2:11)
      COMMON /DOPR2T/ TAREDO( 0:14)
      COMMON /DOPR3T/ TZOODO( 0: 1)
      COMMON /DOPR4T/ TPICDO,THLPDO,TNAMDO
      CHARACTER *2 TPLNDO,TAREDO,TZOODO,TPICDO,THLPDO,TNAMDO
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
C------------------------------------------------------------------- DW
      COMMON /DWORKT/ TXTADW
      CHARACTER *80 TXTADW
      CHARACTER *1 TXT1DW(80)
      EQUIVALENCE (TXTADW,TXT1DW)
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
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C------------------------------------------------------------------  DC
C     HARD WIRED IN P_DALB_ALEPH.FOR           !!!
C     ++++++++++++++++++++++++++ *12 TFILDC
      COMMON /DCFTVT/ TFILDC,TITLDC,TDEFDC,TNAMDC
      CHARACTER  *8 TFILDC
      CHARACTER *10 TITLDC
      CHARACTER  *3 TDEFDC
      CHARACTER  *4 TNAMDC
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
      DIMENSION RDCO(0:127),GRCO(0:127),BLCO(0:127)
      DATA ICOL/7/
      LOGICAL CHG,FM
      CHARACTER *25 TNAME
      CHARACTER *(*) TID
      CHARACTER *1 T1
      CHARACTER *2 DT2,TANSW,TS,TCOS1(7),TCOS2(7)
      DIMENSION CRED(7),CGRN(7),CBLU(7)
      DATA TCOS1/'SR','SG','SB','SY','SM','SC','SW'/
      DATA TCOS2/'RD','GR','BL','YE','MA','CY','WH'/
      DATA CRED/   1.,  0.,  0.,  1.,  1.,  0.,  1./
      DATA CGRN/   0.,  1.,  0.,  1.,  0.,  1.,  1./
      DATA CBLU/   0.,  0.,  1.,  0.,  1.,  1.,  1./
      DIMENSION GRSC(3),RDSC(3),BLSC(3)
      CHARACTER *3 TC3(3)
      CHARACTER *2 TCC(7,3)
      DATA TCC/' G','SG',' G','SG',' G','SG','SG',
     &         'SR',' R',' R','SR','SR',' R','SR',
     &         ' B',' B','SB',' B','SB','SB','SB'/
      CHARACTER *2 TCDEF(4)
      DATA TCDEF/'FR','FG','FB','FS'/
      DIMENSION PCDEF(4,4)
      DATA PCDEF/
     &  0.,0.,100.,0.,
     &  0.,0.,100.,0.,
     &  0.,0.,100.,0.,
     &  0.,0.,100.,0./
      CHARACTER *2 TCOL1(0:127),TCOL2(0:127),TDUM
      DATA TCOL1/
     &  'A0','A1','A2','A3','A4','A5','A6','A7','A8','A9',
     &  'B0','B1','B2','B3','B4','B5','B6','B7','B8','B9',
     &  'E0','E1','E2','E3','E4','E5','E6','E7','E8','E9',
     &  'F0','F1','F2','F3','F4','F5','F6','F7','F8','F9',
     &  'H0','H1','H2','H3','H4','H5','H6','H7','H8','H9',
     &  'I0','I1','I2','I3','I4','I5','I6','I7','I8','I9',
     &  'J0','J1','J2','J3','J4','J5','J6','J7','J8','J9',
     &  'K0','K1','K2','K3','K4','K5','K6','K7','K8','K9',
     &  'M0','M1','M2','M3','M4','M5','M6','M7','M8','M9',
     &  'N0','N1','N2','N3','N4','N5','N6','N7','N8','N9',
     &  'P0','P1','P2','P3','P4','P5','P6','P7','P8','P9',
     &  'R0','R1','R2','R3','R4','R5','R6','R7','R8','R9',
     &  'S0','S1','S2','S3','S4','S5','S6','S7'/
      DATA NUCOL/32/,DHT/2./,DVT/22./,M13/13/,IDSP/1/
      DATA TCOL2/'BG','BK','TP','IT','EC','HC','MC','GY',
     &           'WH','GN','YE','BR','RD','MA','CY','BL',
     &           'Q1','Q2','Q3','Q4','Q5','Q6','Q7','Q8',
     &           'D1','D2','D3','D4','D5','D6','D7','D8',
     &           '  ','  ','  ','  ','  ','  ','  ','  ',
     &           '  ','  ','  ','  ','  ','  ','  ','  ',
     &           '  ','  ','  ','  ','  ','  ','  ','  ',
     &           '  ','  ','  ','  ','  ','  ','  ','  ',
     &           '  ','  ','  ','  ','  ','  ','  ','  ',
     &           '  ','  ','  ','  ','  ','  ','  ','  ',
     &           '  ','  ','  ','  ','  ','  ','  ','  ',
     &           '  ','  ','  ','  ','  ','  ','  ','  ',
     &           '  ','  ','  ','  ','  ','  ','  ','  ',
     &           '  ','  ','  ','  ','q4','q3','q2','q1',
     &           'bl','cy','ma','rd','br','ye','gn','wh',
     &           'gy','c5','c4','c3','c2','c1','bk','bg'/
      DATA DVHD/15./,NF3/3/,JSC/28/,DHCC/-3./
      LOGICAL FIO
      CHARACTER *49 T
C             123456789 123456789 123456789 123456789 123456789
      DATA T/'P?: set up Workstation Colours. FS => AB  CI_12'/
      CALL DPARP0
      CALL DPAROP(84,'OCI')
      CALL DPARGI(84,'ONC',NUMC)
      CALL DCDMS
      CALL UCOPY(RDCODD,RDCO,127)
      CALL UCOPY(GRCODD,GRCO,127)
      CALL UCOPY(BLCODD,BLCO,127)
      IF(IDSP.EQ.1) THEN
        CALL DQCL(M13)
        GO TO 941
      END IF
  930 T(39:40)=TCOS2(ICOL)
      CALL DTYPT('TYPE',TPICDO,N_1_DA,N_2_DA,PARADA,TPOPDA,T)
  931 INCO=127-NCOL
      WRITE(TXTADW,1001) GRCO(INCO),NCOL,TCOL2(NCOL),TCOL1(NCOL),
     &  'FR=',RDCO(NCOL),
     &  'FG=',GRCO(NCOL),
     &  'FB=',BLCO(NCOL)
C     .............................................  123456789 123456
C     .............................................   123 123 D2 = MA :
 1001 FORMAT(F4.2,I4,1X,A,3X,A,' :',3(2X,A,1X,F4.2))
      NI=128-NUMC
      IF(TXTADW(10:11).NE.' ') THEN
        IF(NCOL.GE.NI) THEN
          TXTADW(13:13)='#'
        ELSE
          TXTADW(13:13)='='
        END IF
      END IF
      IF(NCOL.LT.NI) TXTADW(1:4)=' '
      CALL DWRC
      CHG=.FALSE.
  936 DO K=1,4
        PCDEF(4,K)=-1.
      END DO
      FIO=.FALSE.
      CALL DOPER(1,0,
     &  N_1_DA,N_2_DA,TPOPDA,PARADA,
     &  1,4,TCDEF,PCDEF,
     &  NEXEC,CHG,TANSW)
      DO K=1,4
        IF(PCDEF(2,K).GT.1.) PCDEF(2,K)=PCDEF(2,K)*0.1
        IF(PCDEF(2,K).GT.1.) PCDEF(2,K)=PCDEF(2,K)*0.1
      END DO
      IF(TANSW.EQ.'BC'.OR.PCDEF(4,4).EQ.1.) THEN
        RDCO(NCOL)=PCDEF(2,4)*CRED(ICOL)
        GRCO(NCOL)=PCDEF(2,4)*CGRN(ICOL)
        BLCO(NCOL)=PCDEF(2,4)*CBLU(ICOL)
        IF(TANSW.EQ.'BC') GO TO 940
      ELSE
        IF(PCDEF(4,1).EQ.1.) RDCO(NCOL)=PCDEF(2,1)
        IF(PCDEF(4,2).EQ.1.) GRCO(NCOL)=PCDEF(2,2)
        IF(PCDEF(4,3).EQ.1.) BLCO(NCOL)=PCDEF(2,3)
      END IF
      GO TO (910,920,930,940),NEXEC
  910 CALL DQTIT(IFULDB)
      RETURN
  920 DO K=0,127
        IF(TANSW.EQ.TCOL1(K)) THEN
          NCOL=K
          GO TO 941
        END IF
      END DO
      DO K=0,NUMC-1
        IF(TANSW.EQ.TCOL2(K)) THEN
          NCOL=K
          GO TO 941
        END IF
      END DO
      DO K=1,7
        IF(TANSW.EQ.TCOS1(K)) THEN
          ICOL=K
          GO TO 930
        END IF
      END DO
      FM=.FALSE.

      IF(TANSW.EQ.'CC') THEN
        IF(NCOL.LE.JSC) THEN
          DO K=1,3
            GRSC(K)=GRCODD(JSC+K)
            RDSC(K)=RDCODD(JSC+K)
            BLSC(K)=BLCODD(JSC+K)
          END DO
        END IF
        TANSW='cc'
      END IF

  800 IF(TANSW.EQ.'cc') THEN
        IF(NCOL.LE.JSC.AND.IDSP.EQ.1) THEN
          DO K=1,3
            GRCO(JSC+K)=0.
            RDCO(JSC+K)=0.
            BLCO(JSC+K)=0.
          END DO
          GRCO(JSC+1)=GRCO(NCOL)
          RDCO(JSC+2)=RDCO(NCOL)
          BLCO(JSC+3)=BLCO(NCOL)
          CALL UCOPY(GRCO(JSC+1),GRCODD(JSC+1),3)
          CALL UCOPY(RDCO(JSC+1),RDCODD(JSC+1),3)
          CALL UCOPY(BLCO(JSC+1),BLCODD(JSC+1),3)
          WRITE(TC3(1),1946) IFIX(GRCO(NCOL)*100.)
          WRITE(TC3(2),1946) IFIX(RDCO(NCOL)*100.)
          WRITE(TC3(3),1946) IFIX(BLCO(NCOL)*100.)
 1946     FORMAT(I3)
          CALL DGSETC
          H3=H2
          DO K=1,3
            H4=H3+DH
            CALL DGLEVL(JSC+K)
            CALL DQFAR(H3,V1,H4,V3)
            IF(GRCO(JSC+K).GT.CI) THEN
              CALL DGLEVL(KMIN)
            ELSE
              CALL DGLEVL(KMAX)
            END IF
            CALL DGTEXT(H3+DHCC,V1+DVT,TC3(K),3)
            CALL DGTEXT(H3+DHT,V1+DHT,TCC(ICOL,K),2)
            H3=H4
          END DO
          CALL DGCHKX
C         CALL DGLEVL(8)
C         CALL DQDRAW(H2,V1,H2,V3)
        END IF
c      END IF
c
c      IF(TANSW.EQ.'cc') THEN
        CALL DO_BAR_ANSWER_PLATFORM_TEXT('cc')
        WRITE(TXTADW,1007) TCOS1(ICOL),NCOL,TCOL2(NCOL),TCOL1(NCOL),
     &    'FR=',RDCO(NCOL),
     &    'FG=',GRCO(NCOL),
     &    'FB=',BLCO(NCOL)
 1007   FORMAT(A2,I6,1X,A,3X,A,' :',3(2X,A,1X,F4.2))
        CALL DWR_OVER_PRINT(49)
        CALL DGETLN(T1,NA,1)
        IF(NA.LT.1) GO TO 802
        IF(     T1.EQ.'+'.OR.T1.EQ.'=') THEN
          SI= 0.01
        ELSE IF(T1.EQ.'-'.OR.T1.EQ.'_') THEN
          SI=-0.01
        ELSE IF(T1.EQ.'U') THEN
          SI= 0.05
        ELSE IF(T1.EQ.'D') THEN
          SI=-0.05
C       ELSE IF(T1.EQ.'Z') THEN
C         RDCO(NCOL)=0.
C         GRCO(NCOL)=0.
C         BLCO(NCOL)=0.
C         GO TO 940
        ELSE IF(T1.EQ.'Z') THEN
          SI=-1.
        ELSE IF(T1.EQ.'1') THEN
          SI= 1.
        ELSE
          DO JCOL=1,7
            IF(T1.EQ.TCOS1(JCOL)(2:2)) THEN
              ICOL=JCOL
              GO TO 800
            END IF
          END DO
          GO TO 802
        END IF
        RDCO(NCOL)=RDCO(NCOL)+SI*CRED(JCOL)
        GRCO(NCOL)=GRCO(NCOL)+SI*CGRN(JCOL)
        BLCO(NCOL)=BLCO(NCOL)+SI*CBLU(JCOL)
        RDCO(NCOL)=MIN(1.,MAX(0.,RDCO(NCOL)))
        GRCO(NCOL)=MIN(1.,MAX(0.,GRCO(NCOL)))
        BLCO(NCOL)=MIN(1.,MAX(0.,BLCO(NCOL)))
        GO TO 940
  802   CALL DGLEVL(0)
        CALL DQFAR(H2,V1,HHGHDG(M13),V3)
        DO K=1,3
          GRCODD(JSC+K)=GRSC(K)
          RDCODD(JSC+K)=RDSC(K)
          BLCODD(JSC+K)=BLSC(K)
        END DO
        CALL DGSETC
        GO TO 930
      END IF
      IF(TANSW.EQ.'LC') TANSW='L1'
      IF(TANSW(1:1).EQ.'L') THEN
        READ(TANSW(2:2),1002,ERR=9) LC
 1002   FORMAT(I1)
        IF(LC.GE.1.AND.LC.LE.8) THEN
          N0=16*(LC-1)
          N1=N0+15
          TANSW='wr'
        END IF
      END IF
      IF(TANSW.EQ.'RF') THEN
        TNAME(1:12)=TFILDC//'COL_'
        CALL DWRT('Read '//TNAME//'. Type identifier')
        CALL DGETLN(TNAME(13:25),NID,12)
        IF(NID.LE.0) GO TO 930
        GO TO 300
      END IF
      IF(TANSW.EQ.'WF') THEN
        TNAME(1:12)=TFILDC//'COL_'
        CALL DWRT('Write '//TNAME//'. Type identifier')
        CALL DGETLN(TNAME(13:25),NID,12)
        IF(NID.LE.0) GO TO 930
        CALL DGOPEN(NUNIDU,TNAME,1,*991,IER)
        FIO=.TRUE.
        N0=0
        N1=NUMC-1
        TANSW='wr'
      END IF
      IF(TANSW.EQ.'wr') THEN
        CALL DWRT('           color       Red      Green     Blue')
        DO N=N0,N1
          I=127-N
          WRITE(TXTADW,1001) GRCO(I),N,TCOL2(N),TCOL1(N),
     &     'FR=',RDCO(N),
     &     'FG=',GRCO(N),
     &     'FB=',BLCO(N)
          IF(TXTADW(10:11).NE.' ') THEN
            IF(N.GE.NI) THEN
              TXTADW(13:13)='#'
            ELSE
              TXTADW(13:13)='='
            END IF
          END IF
          IF(N.LT.NI) TXTADW(1:4)=' '
          IF(FIO) WRITE(NUNIDU,1004) TXTADW
 1004     FORMAT(A)
          CALL DWRC
        END DO
        IF(FIO) THEN
          CLOSE(UNIT=NUNIDU)
          CALL DWRT(TNAME//' closed')
        END IF
        GO TO 931
      END IF
    9 IF(TANSW.EQ.'RS') THEN
        CALL DCDMR
        GO TO 930
      END IF
      IF(TANSW.EQ.'CA') THEN
        CALL UCOPY(RDCO,RDCODD,128)
        CALL UCOPY(GRCO,GRCODD,128)
        CALL UCOPY(BLCO,BLCODD,128)
        CALL DGSETC
        GO TO 941
      END IF
      IF(TANSW.EQ.'DC') THEN
        IDSP=-IDSP
        GO TO 941
      END IF
      IF(TANSW.EQ.'DH') THEN
        CALL DQTIT(IFULDB)
        IDSP=-1
        GO TO 936
      END IF
      CALL DO_STR('ID: inversed color on dark background')
      IF(TANSW.EQ.'ID') THEN
        CALL DPARSV(84,'OGX',2,GRCO(NCOL))
        CALL DPARSV(84,'ORX',2,RDCO(NCOL))
        CALL DPARSV(84,'OBX',2,BLCO(NCOL))
        GO TO 940
      END IF
      CALL DO_STR('IL: inversed color on LIGHT background')
      IF(TANSW.EQ.'IL') THEN
        CALL DPARSV(84,'OGZ',2,GRCO(NCOL))
        CALL DPARSV(84,'ORZ',2,RDCO(NCOL))
        CALL DPARSV(84,'OBZ',2,BLCO(NCOL))
        GO TO 940
      END IF
      CALL DO_STR('PS: store colors for PS')
      IF(TANSW.EQ.'PS') THEN
        CALL DW_STORE_PS_COLORS
        GO TO 930
      END IF
      CALL DWR_IC(TANSW)
      GO TO 936
  991 CALL DWRT(' I/O error ##')
      GO TO 936
  940 CALL UCOPY(RDCO,RDCODD,NI)
      CALL UCOPY(GRCO,GRCODD,NI)
      CALL UCOPY(BLCO,BLCODD,NI)
      CALL DW_SET_CO
      CALL UCOPY(RDCODD(NI),RDCO(NI),NUMC)
      CALL UCOPY(GRCODD(NI),GRCO(NI),NUMC)
      CALL UCOPY(BLCODD(NI),BLCO(NI),NUMC)
  941 IF(IDSP.EQ.1) THEN
        V1=VMINDG(M13)
        V3=VHGHDG(M13)
        V2=V3-DVHD
        H1=HMINDG(M13)
        H2=HHGHDG(M13)
        CALL DGLEVL(NCOL)
        DH=HHGHDG(M13)-H1
        DH=DH/(NUCOL+NF3)
        HH=NUCOL*DH
        CALL DQFAR(H1,V1,HH,V3)
        GMIN= 8
        GMAX=-1
        DO K=1,NUCOL
          G=4.*GRCODD(K)+2.*RDCODD(K)+BLCODD(K)
          IF(G.GT.GMAX) THEN
            GMAX=G
            KMAX=K
          END IF
          IF(G.LT.GMIN) THEN
            GMIN=G
            KMIN=K
          END IF
        END DO
        CALL DPARGV(84,'OCI',2,CI)
        CI=0.01*CI
        IF(GRCODD(NCOL).GT.CI) THEN
          NTCOL=KMIN
        ELSE
          NTCOL=KMAX
        END IF
        DO K=0,NUCOL-1
          H2=H1+DH
          CALL DGLEVL(K)
          CALL DQFAR(H1,V1,H2,V2)
          IF(GRCODD(K).GT.CI) THEN
            CALL DGLEVL(KMIN)
          ELSE
            CALL DGLEVL(KMAX)
          END IF
          CALL DGTEXT(H1+DHT,V1+DHT,TCOL2(K),2)
          CALL DGLEVL(NTCOL)
          WRITE(TXTADW,1945) K
 1945     FORMAT(I2)
          CALL DGTEXT(H1+DHT,V1+DVT,TXTADW,2)
          H1=H2
        END DO
        CALL DQFR(M13)
        CALL DGCHKX
      END IF
      IF(TANSW.EQ.'cc') GO TO 800
      GO TO 930
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------------------- DWC_READ
CH
      ENTRY DWC_READ(TID)
CH
CH --------------------------------------------------------------------
      CALL DPARGI(84,'ONC',NUMC)
      TNAME(1:12)=TFILDC//'COL_'
      TNAME(13:25)=TID
      FIO=.TRUE.
  300 CALL DGOPEN(NUNIDU,TNAME,2,*993,IER)
      DO N=0,NUMC-1
  927   READ(NUNIDU,1004,END=928) TXTADW
        READ(TXTADW,1003,ERR=927) K,RDCO(K),GRCO(K),BLCO(K)
C       ............... 123456781234567890123451234512345123451234512345
C       ...............       10 YE = B0 :  FR= 1.00  FG= 1.00  FB= 0.00
 1003   FORMAT(I8,10X,3(5X,F5.2))
        IF(.NOT.FIO) CALL DWRC
      END DO
  928 CLOSE(UNIT=NUNIDU)
      IF(.NOT.FIO) GO TO 940
      CALL UCOPY(RDCO,RDCODD,NUMC)
      CALL UCOPY(GRCO,GRCODD,NUMC)
      CALL UCOPY(BLCO,BLCODD,NUMC)
      IF(     TID(1:7).EQ.'DISPLAY') THEN
        CALL UCOPY(RDCODD,RDCDDD,NUMC)
        CALL UCOPY(GRCODD,GRCDDD,NUMC)
        CALL UCOPY(BLCODD,BLCDDD,NUMC)
      ELSE IF(TID(1:5).EQ.'PRINT'  ) THEN
        CALL UCOPY(RDCODD,RDCPDD,NUMC)
        CALL UCOPY(GRCODD,GRCPDD,NUMC)
        CALL UCOPY(BLCODD,BLCPDD,NUMC)
      END IF
      CALL DW_SET_CO
      RETURN
  993 CALL DWRT(TNAME//' not found.#')
      END
*DK DW_SET_CO
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DW_SET_CO
CH
      SUBROUTINE DW_SET_CO
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
C INVOKED BY TANSW.EQ.'WC'  (DWC)
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DD
      COMMON /DDEFCO/ RDCODD(0:127),GRCODD(0:127),BLCODD(0:127),
     &  LTABDD(0:127)
      COMMON /DDEFCD/ RDCDDD(0:127),GRCDDD(0:127),BLCDDD(0:127)
      COMMON /DDEFCP/ RDCPDD(0:127),GRCPDD(0:127),BLCPDD(0:127)
      COMMON /DDEFCF/ FPCODD
      LOGICAL FPCODD
      COMMON /DDEFCN/ NUMCDD
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DATA I127/127/
      CALL DPARGI(84,'ONC',NUM)
      CALL DPARGV(84,'OCI',2,CI)
      CI=0.01*CI
      DO K=0,NUM
        I=I127-K
        IF(GRCODD(K).GT.CI) THEN
C         ..................................... light background
          CALL DPARGV(84,'OGZ',2,GRCODD(I))
          CALL DPARGV(84,'ORZ',2,RDCODD(I))
          CALL DPARGV(84,'OBZ',2,BLCODD(I))
        ELSE
C         ..................................... dark background
          IF(I.NE.126) THEN
            CALL DPARGV(84,'OGX',2,GRCODD(I))
            CALL DPARGV(84,'ORX',2,RDCODD(I))
            CALL DPARGV(84,'OBX',2,BLCODD(I))
          ELSE
            GRCODD(I)=1.
            RDCODD(I)=1.
            BLCODD(I)=1.
          END IF
        END IF
      END DO
      CALL DGSETC
      END
*DK DW_STORE_PS_COLORS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DW_STORE_PS_COLORS
CH
      SUBROUTINE DW_STORE_PS_COLORS
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DW
      COMMON /DWORKT/ TXTADW
      CHARACTER *80 TXTADW
      CHARACTER *1 TXT1DW(80)
      EQUIVALENCE (TXTADW,TXT1DW)
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
C------------------------------------------------------------------- DD
      COMMON /DDEFCO/ RDCODD(0:127),GRCODD(0:127),BLCODD(0:127),
     &  LTABDD(0:127)
      COMMON /DDEFCD/ RDCDDD(0:127),GRCDDD(0:127),BLCDDD(0:127)
      COMMON /DDEFCP/ RDCPDD(0:127),GRCPDD(0:127),BLCPDD(0:127)
      COMMON /DDEFCF/ FPCODD
      LOGICAL FPCODD
      COMMON /DDEFCN/ NUMCDD
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *30 TNAME,TIN
      DATA TNAME/'DALI.COLPS'/
      DATA LNAM/10/
      CHARACTER *49 T1
      CHARACTER *60 TD,TL,TG,TC
      CHARACTER *5 TE
      CHARACTER *3 TLIWI(7)
      DATA TLIWI/'0.0','0.5','1.0','1.5','2.0','2.5','3.0'/
C        123456789 123456789 123456789 123456789 123456789 123456789
      DATA TD/
     &  'colordisplay { % Display in color.'/
      DATA TL/
     &  '/ourlinewidth {0.5 mul setlinewidth} bind def'/
      DATA TG/
     &  '/setcolor00  {1.0000 setgray} bind def'/
      DATA TC/
     &  '/setcolor18  {1.0000 0.0000 0.0000 setrgbcolor} bind def'/
      DATA T1/
     &  'Linewidth =0.5*n : BW:0, Col:1 or higher = 1 < 7'/
      DATA TE/
     &  '} if'/
      TXTADW='Store PS colors: File = '//TNAME(1:LNAM)
      CALL DWRC
      CALL DWRT('<CR> = stop <Y>= O.K.')
      CALL DGETLN(TIN,LIN,30)
      IF(LIN.LE.0) GO TO 9
      CALL CLTOU(TIN)
      IF(LIN.GT.1.OR.TIN.NE.'Y') THEN
        TNAME=TIN
        LNAM=LIN
        DO L=1,LNAM
          IF(TNAME(L:L).EQ.'.') GO TO 1
        END DO
        TNAME(LNAM+1:LNAM+6)='.COLPS'
        LNAM=LNAM+6
      END IF
    1 CALL DTYANS(T1,'0123456',N)
      IF(N.LT.0) GO TO 9
      IF(N.EQ.0) N=2
      CALL DGOPEN(NUNIDU,TNAME,1,*9,IER)
      WRITE(NUNIDU,1000) TD
      TL(16:18)=TLIWI(N)
      WRITE(NUNIDU,1000) TL
 1000 FORMAT(1X,A)
      DO K=0,31
        IF(RDCODD(K).EQ.GRCODD(K).AND.
     &     RDCODD(K).EQ.BLCODD(K)) THEN
          WRITE(TG(10:11),1002) K
          WRITE(TG(15:20),1001) RDCODD(K)
          WRITE(NUNIDU,1000) TG
        ELSE
          WRITE(TC(10:11),1002) K
          WRITE(TC(15:20),1001) RDCODD(K)
          WRITE(TC(22:27),1001) GRCODD(K)
          WRITE(TC(29:34),1001) BLCODD(K)
          WRITE(NUNIDU,1000) TC
        END IF
 1002   FORMAT(I2.2)
 1001   FORMAT(F6.4)
      END DO
      WRITE(NUNIDU,1000) TE
      CLOSE(UNIT=NUNIDU)
      TXTADW=TNAME//' stored.'
      CALL DWRC
      RETURN
    9 CALL DWRT('Nothing stored.')
      END
*DK DWS
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DWS
CH
      SUBROUTINE DWS
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C INVOKED BY TANSW.EQ.'SW'  (DWS)
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
C------------------------------------------------------------------- DW
      PARAMETER (MPOSDW=30,MPNWDW=12,MNUWDW=14)
      COMMON /DWINDC/NWINDW(0:MPNWDW,0:MPNWDW),
     &  NEXTDW(0:MPNWDW),LASTDW(0:MPNWDW),
     &  POSIDW(MPOSDW)
      COMMON /DWINDO/TWINDW(0:MNUWDW)
      CHARACTER *1 TWINDW
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C------------------------------------------------------------------- DO
      COMMON /DOPR1T/ TPLNDO(-2:11)
      COMMON /DOPR2T/ TAREDO( 0:14)
      COMMON /DOPR3T/ TZOODO( 0: 1)
      COMMON /DOPR4T/ TPICDO,THLPDO,TNAMDO
      CHARACTER *2 TPLNDO,TAREDO,TZOODO,TPICDO,THLPDO,TNAMDO
C------------------------------------------------------------------- DW
      COMMON /DWORKT/ TXTADW
      CHARACTER *80 TXTADW
      CHARACTER *1 TXT1DW(80)
      EQUIVALENCE (TXTADW,TXT1DW)
C------------------------------------------------------------------- DS
      PARAMETER (MPARDS=12,MVIRDS=-4,MPNPDS=200,MWUSDS=4)
      COMMON /DSTRC1/ IAUTDS,ISTODS(8,MVIRDS:MPARDS,0:MWUSDS)
      COMMON /DSTRC2/PSTODS(2,MPNPDS,MVIRDS:MPARDS,0:MWUSDS)
      COMMON /DSTRC3/TVRUDS(6,MVIRDS:MPARDS,0:MWUSDS)
C------------------------------------------------------------------- DW
      COMMON /DWINSU/ WISUDW
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
C------------------------------------------------------------------  DC
C     HARD WIRED IN P_DALB_ALEPH.FOR           !!!
C     ++++++++++++++++++++++++++ *12 TFILDC
      COMMON /DCFTVT/ TFILDC,TITLDC,TDEFDC,TNAMDC
      CHARACTER  *8 TFILDC
      CHARACTER *10 TITLDC
      CHARACTER  *3 TDEFDC
      CHARACTER  *4 TNAMDC
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
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *49 TT1,TT2,TT3,TLIST
      DATA TLIST/'ident  W#       H1     V1     H2     V2   WI'/
      CHARACTER *80 T
      DATA T/' '/
      CHARACTER *1 TC(-2:1),T1,TIDNT
      DATA TC/'C','D','u','='/
      DATA NUNIT/75/,DTXH1,DTXH2,DTXV1,DTXV2/8.,20.,10.,20./,DFN/0./
      DATA K10/10/,HEAD/9999./,HRH/-1027./
      PARAMETER (MP=13)
      CHARACTER *2 DT2,TANSW,TP(MP)
      DIMENSION PR(4,MP)
C               1    2    3    4    5    6    7    8   9    10
      DATA TP/'**','**','H1','H2','dh','V1','V2','dv','**','**',
     &        'HD','VD','DF'/
      DATA PR/0., 0.,99.  , 0.,
     &        6., 6.,99.  , 0.,
     &     -999., 0., 999., 0.,
     &        1., 0.,9999., 0.,
     &        2., 0.,9999., 0.,
     &     -999., 0., 999., 0.,
     &        1., 0.,9999., 0.,
     &        2., 0.,9999., 0.,
     &        0., 0.,  31.,-1.,
     &       .01, 1.,  99.,-1.,
     &      256., 0.,1280., 0.,
     &      256., 0.,1280., 0.,
     &        0.,60., 512.,-1./
      DATA IDSP/1/
      LOGICAL CHG,FYES,FSTRT
      DATA FSTRT/.TRUE./
C               123456789 123456789 123456789 123456789 123456789
      DATA TT1/'P?:W?: setup windows      HD_1234 VD_1234 DF$1234'/
      DATA TT2/'                          V1_1234 V2_1234 dv_1234'/
C     DATA TT3/'    CO$12 AS$123          H1_1234 H2_1234 dh_1234'/
      DATA TT3/'                          H1_1234 H2_1234 dh_1234'/
      IAOLD=IAREDO
      FRNL=FRNLDU
      FRNLDU=ABS(FRNLDU)
      IF(FSTRT) THEN
        PR(2,11)=POSIDW(8)
        POS8=POSIDW(8)
        PR(2,12)=POSIDW(9)
        POS9=POSIDW(9)
        FSTRT=.FALSE.
      END IF
  930 PR(2,3)=HMINDG(IAREDO)
      PR(2,4)=HHGHDG(IAREDO)
      PR(2,5)=HHGHDG(IAREDO)-HMINDG(IAREDO)
      PR(2,6)=VMINDG(IAREDO)
      PR(2,7)=VHGHDG(IAREDO)
      PR(2,8)=VHGHDG(IAREDO)-VMINDG(IAREDO)
      CALL DTYPT('TYPE',TPICDO,1,MP,PR,TP,TT1)
      CALL DTYPT('TYPE',' '   ,1,MP,PR,TP,TT2)
      CALL DTYPT('TYPE',' '   ,1,MP,PR,TP,TT3)
  936 CALL DGZOOM(6,IAREDO,0,0)
      PR(2,3)=HMINDG(IAREDO)
      PR(2,4)=HHGHDG(IAREDO)
      PR(2,5)=HHGHDG(IAREDO)-HMINDG(IAREDO)
      PR(2,6)=VMINDG(IAREDO)
      PR(2,7)=VHGHDG(IAREDO)
      PR(2,8)=VHGHDG(IAREDO)-VMINDG(IAREDO)
      DO K=3,7
        PR(4,K)=-1.
      END DO
      CALL DAREAW(0)
      CALL DOPER(1,0,
     &  1,0,' ',0,
     &  1,MP,TP,PR,
     &  NEXEC,CHG,TANSW)
      CALL DAREAW(1)
C     ................................................... STORE HORIZONTAL
      IF(PR(4,3).EQ.1.) HMINDG(IAREDO)=PR(2,3)
      IF(PR(4,4).EQ.1.) HHGHDG(IAREDO)=PR(2,4)
      HHGHDG(IAREDO)=MAX(1.,HHGHDG(IAREDO),HMINDG(IAREDO)+1.)
C     ................................................... STORE VERTICAL
      IF(PR(4,6).EQ.1.) VMINDG(IAREDO)=PR(2,6)
      IF(PR(4,7).EQ.1.) VHGHDG(IAREDO)=PR(2,7)
      VHGHDG(IAREDO)=MAX(1.,VHGHDG(IAREDO),VMINDG(IAREDO)+1.)
C     .....................................................
      CALL DGZOOM(6,-1,0,0)
      CALL DGCHKX
      GO TO (910,920,930,930),NEXEC
  910 FRNLDU=FRNL
      CALL DWSETC
      IF(IAREDO.GT.12) IAREDO=IAOLD
      RETURN
  920 CALL DO_STR('TD: toggle display size')
      IF(TANSW.EQ.'TD') THEN
        IF(IDSP.EQ.1) THEN
          CALL DPARGV(84,'OHD',2,PR(2,11))
          CALL DPARGV(84,'OVD',2,PR(2,12))
        ELSE
          PR(2,11)=POS8
          PR(2,12)=POS9
        END IF
        POSIDW(8)=PR(2,11)
        POSIDW(9)=PR(2,12)
        CALL DGNSIZ(1.,1.,POSIDW(8),POSIDW(9)+POSIDW(10))
        CALL DQWIN(1.,0.,PR(2,11),PR(2,12)+PR13,POSIDW(10))
        IDSP=-IDSP
        GO TO 930
      END IF
      CALL DO_STR('WH"WC: set up header window and comment window')
      CALL DAREA('W',TANSW,0,14,IAREDO,FYES)
      IF(FYES) GO TO 930
      CALL DO_STR('CW: calc. and display window correlation')
      IF(TANSW.EQ.'CW') THEN
C       .................................................. WINDOW CORRELATION
        CALL DWSETC
        CALL DWRT('C=contains, D=destroys, u=uncorrelated.')
        DO I=0,12
          WRITE(TXTADW,1000) (TC(NWINDW(N,I)),N=0,12),TAREDO(I)
 1000     FORMAT(14(2X,A))
          CALL DWRC
        END DO
        WRITE(TXTADW,1001) (TAREDO(N),N=0,12)
        CALL DWRC
 1001   FORMAT(1X,13(1X,A2))
        DO M=0,MPNWDW
          ISTODS(4,M,IWUSDO)=-1
        END DO
        GO TO 936
      END IF
      CALL DO_STR('S1"S2: select window set 1 or 2')
      IF(TANSW.EQ.'S1'.OR.TANSW.EQ.'S2') THEN
        IF(TANSW.EQ.'S1') THEN
          WISUDW=0.
        ELSE
          WISUDW=1.
        END IF
        CALL DQWIS
        DO M=0,MPNWDW
          ISTODS(4,M,IWUSDO)=-1
        END DO
        GO TO 930
      END IF
      CALL DO_STR('HS"HW: fit header window to window S or H')
      IF(TANSW.EQ.'HS'.OR.TANSW.EQ.'HW') THEN
        IAREDO=13
        IF(TANSW.EQ.'HS') THEN
          HHGHDG(IAREDO)=HHGHDG(12)
          HEAD=HEADDU
          HEADDU=HRH
        ELSE
          HHGHDG(IAREDO)=HHGHDG(0)
          IF(HEAD.NE.9999.) HEADDU=HEAD
        END IF
        IFULDB=1
        CALL DQTIT(IFULDB)
        GO TO 936
      END IF
      CALL DO_STR('LF: list file DALI_**.WINDOW_SETUP')
      IF(TANSW.EQ.'LF') THEN
        T=TFILDC//'WINDOW_SETUP'
        CALL DGOPEN(NUNIDU,T,2,*929,IER)
        DO K=1,999
  921     READ(NUNIDU,1013,END=923) TXTADW
 1013     FORMAT(A)
          CALL DWRC
        END DO
  923   CLOSE(UNIT=NUNIDU)
        CALL DWRT(TLIST)
        GO TO 936
      END IF
      CALL DO_STR('RD: read windows from file DALI_**.WINDOW_SETUP')
      IF(TANSW.EQ.'RD') THEN
        T=TFILDC//'WINDOW_SETUP'
        CALL DTYANS('File '//T(1:12)//' will be read. No=<CR> Ident ='
     &    ,'*',NANSW)
        IF(NANSW.NE.-1) GO TO 930
        T1=TXTADW(1:1)
        CALL DGOPEN(NUNIDU,T,2,*929,IER)
  922   READ(NUNIDU,1010,END=924) T
 1010   FORMAT(A)
        IF(T(1:2).EQ.' ') GO TO 922
        READ(T,1011,ERR=922) TIDNT,I,HMIN,VMIN,DH,DV,TAR
 1011   FORMAT(1X,A,I7,F10.0,3F7.0,2X,A,1X,A)
        IF(T1.EQ.TIDNT.AND.I.GE.0.AND.I.LE.13) THEN
          HMINDG(I)=HMIN
          HHGHDG(I)=HMIN+DH
          VMINDG(I)=VMIN
          VHGHDG(I)=VMIN+DV
          WRITE(TXTADW,1020) I,HMINDG(I),HHGHDG(I),DH,
     &                         VMINDG(I),VHGHDG(I),DV,TAREDO(I)
          CALL DWRC
        END IF
        GO TO 922
  924   CLOSE(UNIT=NUNIDU)
        CALL DWSETC
        GO TO 930
      END IF
      CALL DO_STR('TY: type window coordinates')
      IF(TANSW.EQ.'TY') THEN
        DO I=0,14
          DH=HHGHDG(I)-HMINDG(I)
          DV=VHGHDG(I)-VMINDG(I)
          WRITE(TXTADW,1020) I,HMINDG(I),HHGHDG(I),DH,
     &                         VMINDG(I),VHGHDG(I),DV,TAREDO(I)
          CALL DWRC
 1020     FORMAT(I2,' H1=',F4.0,' H2=',F4.0, ' D=',F4.0,
     &              ' V1=',F4.0,' V2=',F4.0, ' D=',F4.0,1X,A)
        END DO
        GO TO 930
      END IF
      CALL DO_STR('WF: write window positions to file FOR075.DAT')
      IF(TANSW.EQ.'WF') THEN
        T=TFILDC//'WINDOW_SETUP'
        CALL DTYANS('File '//T//' will be written. No=<CR> Ident =','*',
     &    NANSW)
        IF(NANSW.NE.-1) GO TO 930
        TIDNT=TXTADW(1:1)
        CALL DWRT('      Comment of 20 letters = ')
        CALL DGETLM(T,LENTC,20)
        DO I=0,14
          DH=HHGHDG(I)-HMINDG(I)
          DV=VHGHDG(I)-VMINDG(I)
          WRITE(NUNIT,1011) TIDNT,I,HMINDG(I),
     &                              VMINDG(I),DH,DV,TAREDO(I),T(1:20)
          T=' '
        END DO
        IF(NUNIT.NE.6) CLOSE(UNIT=NUNIT)
        CALL DWRT('Written to FOR075.DAT')
        GO TO 930
      END IF
C      CALL DO_STR_1_LET_LIST('C',15,TAREDO)
C      CALL DAREA('C',TANSW,0,14,IAREDO,FYES)
C      IF(FYES) GO TO 927
C      CALL DAREA('F',TANSW,0,14,IAREDO,FYES)
C      IF(FYES) GO TO 927
C      CALL DAREA('N',TANSW,0,14,IAREDO,FYES)
C      IF(TANSW.EQ.'NN'.OR.FYES) THEN
      IF(TANSW.EQ.'NN') THEN
        CALL DQLEVL(ICTXDD)
        H=HMINDG(IAREDO)+DTXH1
        DO K=1,2
          V=VMINDG(IAREDO)+DTXV1
          DO N=1,2
            CALL DGTEXT(H,V,TWINDW(IAREDO),1)
            V=VHGHDG(IAREDO)-DTXV2
          END DO
          H=HHGHDG(IAREDO)-DTXh2
        END DO
  928   CALL DQLEVL(ICFRDD)
        CALL DQDAR(HMINDG(IAREDO),VMINDG(IAREDO),
     &             HHGHDG(IAREDO),VHGHDG(IAREDO))
        CALL DGEXEC
        GO TO 936
      END IF
C      IF(TANSW.EQ.'CC'.OR.TANSW.EQ.'FF') THEN
C  927   CALL DQCL(IAREDO)
C        IF(TANSW(1:1).EQ.'F') THEN
C          IF(PR(4,9).GT.0.) THEN
C            CALL DGLEVL(IFIX(PR(2,9)))
C          ELSE
C            CALL DGLEVL(IAREDO+1)
C          END IF
C          CALL DGRAR(HMINDG(IAREDO),VMINDG(IAREDO),
C     &               HHGHDG(IAREDO),VHGHDG(IAREDO))
C        END IF
C        GO TO 928
c      END IF
      IF(TANSW.EQ.'MM') THEN
        IF(H2.LE.0.) THEN
          TANSW='MW'
        ELSE
          CALL DWSM(PR(1,10),H1,V1,H2,V2)
          GO TO 936
        END IF
      END IF
      IF(TANSW.EQ.'MW') THEN
        H1=HMINDG(IAREDO)
        H2=HHGHDG(IAREDO)
        V1=VMINDG(IAREDO)
        V2=VHGHDG(IAREDO)
        CALL DWSM(PR(1,10),H1,V1,H2,V2)
        GO TO 936
      END IF
      IF(TANSW.EQ.'SW') THEN
        HMINDG(IAREDO)=H1
        HHGHDG(IAREDO)=H2
        VMINDG(IAREDO)=V1
        VHGHDG(IAREDO)=V2
        GO TO 930
      END IF
      CALL DO_STR('MD: modify display size')
      IF(TANSW.EQ.'MD') THEN
C       ............ type "RN" afterwards to recalculate normal windows
        POSIDW(8)=PR(2,11)
        POSIDW(9)=PR(2,12)
        IF(PR(4,13).GT.0.) POSIDW(9)=POSIDW(9)+PR(2,13)
        CALL DGNSIZ(1.,1.,POSIDW(8),POSIDW(9)+POSIDW(10))
        CALL DO_BAR(2)
        CALL DQ_HEADER
        GO TO 930
      END IF
      CALL DO_STR('RN: recalculate windows fo new display size')
C     ............................ set DF=OF if no comment window is set up.
      IF(TANSW.EQ.'RN') THEN
        IF(PR(4,13).GT.0.) THEN
          PR13=PR(2,13)
        ELSE
          PR13=0.
        END IF
        CALL DQWIN(1.,PR13,PR(2,11),PR(2,12)+PR13,POSIDW(10))
        GO TO 29
      END IF
      IF(TANSW.EQ.'AW') THEN
        N=0
        DO K=0,13
          CALL DGZOOM(6,K,0,0)
          CALL DGCHKX
          IF(N.LT.3.AND.K.NE.13) CALL DTYANS('Move window W'//
     &      TWINDW(K)//' N=<CR>,Y,A','NYA',N)
          IF(N.GT.1) THEN
            VMINDG(K)=VMINDG(K)+PR(2,13)
            VHGHDG(K)=VHGHDG(K)+PR(2,13)
          END IF
        END DO
        GO TO 29
      END IF
      CALL DWR_IC(TANSW)
      GO TO 930
   29 IAREDO=14
      HMINDG(IAREDO)=1.
      VMINDG(IAREDO)=1.
      HHGHDG(IAREDO)=POSIDW(8)
      VHGHDG(IAREDO)=PR(2,13)
      GO TO 930
  929 CALL DWRT(' File not found: '//T)
      GO TO 930
      END
*DK DWSETC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DWSETC
CH
       SUBROUTINE DWSETC
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DW
      PARAMETER (MPOSDW=30,MPNWDW=12,MNUWDW=14)
      COMMON /DWINDC/NWINDW(0:MPNWDW,0:MPNWDW),
     &  NEXTDW(0:MPNWDW),LASTDW(0:MPNWDW),
     &  POSIDW(MPOSDW)
      COMMON /DWINDO/TWINDW(0:MNUWDW)
      CHARACTER *1 TWINDW
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
C            WINDOW SETUP 1: Destroy=-1,Contain=-2,Uncorrelated=0
C       =  C  C  C  C  C  C  C  C  C  C  C  C WW
C       D  =  u  u  u  u  u  D  u  D  u  u  D W1
C       D  u  =  u  u  u  u  u  D  D  u  u  D W2
C       D  u  u  =  u  u  u  D  u  u  D  u  D W3
C       D  u  u  u  =  u  u  u  D  u  D  u  D W4
C       D  u  u  u  u  =  u  u  u  u  u  D  u W5
C       D  u  u  u  u  u  =  u  u  u  u  D  u W6
C       D  C  u  C  u  u  u  =  u  D  D  u  D WU
C       D  u  C  u  C  u  u  u  =  D  D  u  D WD
C       D  C  C  u  u  u  u  D  D  =  u  u  D WL
C       D  u  u  C  C  u  u  D  D  u  =  u  D WM
C       D  u  u  u  u  C  C  u  u  u  u  =  u WR
C       D  C  C  C  C  u  u  C  C  C  C  u  = WS
C       WW W1 W2 W3 W4 W5 W6 WU WD WL WM WR WS
      CHARACTER *1 T
      DATA N1,N2,N3,N4,N5,N6,N7,N8,N9/1,-2,-1,-1,-2,-1,-1,0,0/
      DATA D1/1./
      LOGICAL F1,F2,F3,F4,F5,F6,F7,F8
   10 DO I=0,12
        DO N=0,I
C         .... WINDOW N = WINDOW I
          IF(I.EQ.N) THEN
            NWINDW(N,N)=N1
            GO TO 1
          END IF
C         ............................................... WINDOW N = WINDOW I
          IF(HMINDG(I).EQ.HMINDG(N).AND.
     &       VMINDG(I).EQ.VMINDG(N).AND.
     &       HHGHDG(I).EQ.HHGHDG(N).AND.
     &       VHGHDG(I).EQ.VHGHDG(N)) THEN
            NWINDW(N,I)=N1
            NWINDW(I,N)=N1
            GO TO 1
          END IF
          CALL DQCL0(HMINDG(I),VMINDG(I),HHGHDG(I),VHGHDG(I),1.)
          HMIN=HMINDG(N)+D1
          VMIN=VMINDG(N)+D1
          HHGH=HHGHDG(N)-D1
          VHGH=VHGHDG(N)-D1
          CALL DWSCLP(HMIN,VMIN,HHGH,VMIN,F1)
          CALL DWSCLP(HHGH,VMIN,HHGH,VHGH,F2)
          CALL DWSCLP(HHGH,VHGH,HMIN,VHGH,F3)
          CALL DWSCLP(HMIN,VHGH,HMIN,VMIN,F4)
          IF(F1.AND.F2.AND.F3.AND.F4) THEN
C           ................................. WINDOW N IS IS PART OF WINDOW I
            NWINDW(N,I)=N2
            NWINDW(I,N)=N3
            GO TO 1
          END IF
          CALL DQCL0(HMINDG(N),VMINDG(N),HHGHDG(N),VHGHDG(N),1.)
          HMIN=HMINDG(I)+D1
          VMIN=VMINDG(I)+D1
          HHGH=HHGHDG(I)-D1
          VHGH=VHGHDG(I)-D1
          CALL DWSCLP(HMIN,VMIN,HHGH,VMIN,F5)
          CALL DWSCLP(HHGH,VMIN,HHGH,VHGH,F6)
          CALL DWSCLP(HHGH,VHGH,HMIN,VHGH,F7)
          CALL DWSCLP(HMIN,VHGH,HMIN,VMIN,F8)
          IF(F5.AND.F6.AND.F7.AND.F8) THEN
C           .................................. WINDOW I IS IS PART OF WINDOW N
            NWINDW(N,I)=N4
            NWINDW(I,N)=N5
            GO TO 1
          END IF
          IF(F1.OR.F2.OR.F3.OR.F4.OR.F5.OR.F6.OR.F7.OR.F8) THEN
C           ....................................... partly overlapping windows
            NWINDW(I,N)=N6
            NWINDW(N,I)=N7
          ELSE
            NWINDW(I,N)=N8
            NWINDW(N,I)=N9
          END IF
    1   END DO
      END DO
      END
*DK DWSCLP
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DWSCLP
CH
      SUBROUTINE DWSCLP(H1,V1,H2,V2,FIN)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
      LOGICAL FIN
      X1=H1
      Y1=V1
      X2=H2
      Y2=V2
      CALL DQCLP(X1,Y1,X2,Y2,FIN)
      END
*DK DWSM
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DWSM
CH
      SUBROUTINE DWSM(AS,H1,V1,H2,V2)
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
      DIMENSION H(5),V(5),IH(5),IV(5),NDSEG(2,1),AS(4)
      DATA NDSEG/1,5/,NUMPL/1/
      EXTERNAL DWSMM
      CALL DWSMM0(AS)
      H(1)=H1
      H(2)=H2
      H(3)=H2
      H(4)=H1
      H(5)=H1
      V(1)=V1
      V(2)=V1
      V(3)=V2
      V(4)=V2
      V(5)=V1
      IHC=H1
      IVC=V1

      CALL DO_BAR_ANSWER_PLATFORM_TEXT('sw')
      CALL DGLINM(IHC,IVC,H,V,IH,IV,NAR,DWSMM,.FALSE.,NDSEG,NUMPL)
      H1=MIN(H(1),H(3))
      H2=MAX(H(1),H(3),H1+1.)
      V1=MIN(V(1),V(3))
      V2=MAX(V(1),V(3),V1+1.)
      CALL DWSMM1(AS(4))

C#ifndef ultrix

      CALL DWRT('Type SW to store new window into selected window.')

C#else
C      WRITE(MUN6DU,1000)
C      WRITE(MUN9DU,1000)
C 1000 FORMAT(' Type SW to store new window into selected window.',T53,
C     & ':',$)
C#endif /* Ultrix */

      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DWSMM
CH
      SUBROUTINE DWSMM(IHC,IVC,H,V,NDSEG,NUMPL,FBUT,TKBD)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DW
      COMMON /DWORKT/ TXTADW
      CHARACTER *80 TXTADW
      CHARACTER *1 TXT1DW(80)
      EQUIVALENCE (TXTADW,TXT1DW)
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION H(5),V(5),NDSEG(2,1),AS(4)
      CHARACTER *(*) TKBD
      CHARACTER *4 TP(3),TS(2)
C              123456789 123456789 123456789 123456789 123456789'
      DATA TP/'Win ',
     &        'LlC ',
     &        'UrC '/
      DATA TS/'Rec ',
     &        'Squ '/
      DATA M2/1/
      LOGICAL FBUT(4),FUP
      DATA FUP/.TRUE./
      IF(.NOT.FBUT(4)) THEN
C       ..... IF A LETTER IS PUSHED H AND V MUST NOT BE CHANGED.!!!!
        IF(FUP) THEN
          FUP=.FALSE.
          IF      (TKBD.EQ.'R') THEN
            M2=1
          ELSE IF (TKBD.EQ.'S') THEN
            M2=2
          END IF
        END IF
        GO TO 6
      END IF
      IF(.NOT.FBUT(1)) THEN
        IF(FUP) THEN
          FUP=.FALSE.
          M1=MOD(M1,3)+1
          IF(M1.EQ.3) THEN
            IHC=H(3)
            IVC=V(3)
          ELSE
            IHC=H(1)
            IVC=V(1)
          END IF
          GO TO 6
        END IF
        RETURN
      END IF
      IF(.NOT.FBUT(2)) THEN
        IF(FUP) THEN
          FUP=.FALSE.
          M2=MOD(M2,2)+1
        END IF
        GO TO 6
      END IF
      FUP=.TRUE.
      HC=IHC
      VC=IVC
      GO TO (10,20,30) M1
C     .........................
   10 DH=H(3)-H(1)
      DV=V(3)-V(1)
      H(1)=IHC
      V(1)=IVC
      H(3)=H(1)+DH
      V(3)=V(1)+DV
      GO TO 90
   20 IF(HC.LT.H(3)) H(1)=HC
      IF(VC.LT.V(3)) V(1)=VC
      GO TO 90
   30 IF(HC.GT.H(1)) H(3)=HC
      IF(VC.GT.V(1)) V(3)=VC
   90 IF(M2.EQ.2) THEN
        DH=H(3)-H(1)
        IF(M1.EQ.2) THEN
          V(1)=V(3)-DH*QA
        ELSE
          V(3)=V(1)+DH*QA
        END IF
      END IF
      H(2)=H(3)
      H(4)=H(1)
      H(5)=H(1)
      V(2)=V(1)
      V(4)=V(3)
      V(5)=V(1)
      IH1=H(1)
      IH3=H(3)
      IDH=IH3-IH1
      IV1=V(1)
      IV3=V(3)
      IDV=IV3-IV1
    6 WRITE(TXTADW,1003)
     &  TP(M1),TS(M2),'V:',IV1,IDV,IV3,' H:',IH1,IDH,IH3
 1003 FORMAT(2A,2(A,I5,'+ ',I4,' = ',I4),'>')
      CALL DWR_OVER_PRINT(50)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DWSMM0
CH
      ENTRY DWSMM0(AS)
CH
CH --------------------------------------------------------------------
C     AS(2)=ASPECT RATIO = HORIZONTAL/VERTICAL
      QA=1./AS(2)
      IF(AS(4).EQ.1.) THEN
        M2=2
      ELSE
        M2=1
      END IF
      M1=1
      WRITE(TXTADW,1006) TP(M1),TS(M2)
 1006 FORMAT(2A,'Move mouse:')
      CALL DWRC

C      WRITE(9,1006) TP(M1),TS(M2)
C#ifndef ultrix
C 1006 FORMAT(' ',2A,'Move mouse:',T53,':')
C#else
C 1006 FORMAT(/,' ',2A,'Move mouse:',T53,':')
C#endif /* Ultrix */

      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DWSMM1
CH
      ENTRY DWSMM1(AS4)
CH
CH --------------------------------------------------------------------
      IF(M2.EQ.2) THEN
        AS4=1.
      ELSE
        AS4=-1.
      END IF
      END
*DK DWIZ
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++  DWIZ
CH
      SUBROUTINE DWIZ
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    : Example:
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
C------------------------------------------------------------------  DC
C     HARD WIRED IN P_DALB_ALEPH.FOR           !!!
C     ++++++++++++++++++++++++++ *12 TFILDC
      COMMON /DCFTVT/ TFILDC,TITLDC,TDEFDC,TNAMDC
      CHARACTER  *8 TFILDC
      CHARACTER *10 TITLDC
      CHARACTER  *3 TDEFDC
      CHARACTER  *4 TNAMDC
C------------------------------------------------------------------- DW
      COMMON /DWIZCC/ FWIZDW,FCONDW
      LOGICAL FWIZDW,FCONDW
C------------------------------------------------------------------- DO
      COMMON /DOPR1T/ TPLNDO(-2:11)
      COMMON /DOPR2T/ TAREDO( 0:14)
      COMMON /DOPR3T/ TZOODO( 0: 1)
      COMMON /DOPR4T/ TPICDO,THLPDO,TNAMDO
      CHARACTER *2 TPLNDO,TAREDO,TZOODO,TPICDO,THLPDO,TNAMDO
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
C------------------------------------------------------------------- DW
      COMMON /DWORKT/ TXTADW
      CHARACTER *80 TXTADW
      CHARACTER *1 TXT1DW(80)
      EQUIVALENCE (TXTADW,TXT1DW)
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
C------------------------------------------------------------------- DG
      COMMON /DGRDEC/
     1      HMINDG(0:14),VMINDG(0:14),
     1      HLOWDG(0:14),VLOWDG(0:14),HHGHDG(0:14),VHGHDG(0:14)
      COMMON /DGRDET/ TGRADG
      CHARACTER *3 TGRADG
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *(*) TNC
      CHARACTER *2 TANSW,TSTOP,TA
      DATA TSTOP/'cw'/
      CHARACTER *5 TCLK
      DATA TCLK/'Click'/
      DATA DH1/2./,DH2/600./,DV1/8./,DV2/20./,M13/13/,LUP/0/
      CHARACTER *25 TNAM
      CHARACTER *11 TWIZ
      CHARACTER *16 TFIL
      DATA TFIL/'DALI_D#.EXAMPLES'/
      DATA TWIZ/'DALI_D#.WIZ'/
      LOGICAL FCHG,FCONT,FYES
      PARAMETER (MMAC=99)
      CHARACTER *49 T1
      CHARACTER *74 T,TMAC(MMAC),TUP,TCUR
      DATA TUP/' '/
      DIMENSION LMAC(MMAC)
      DATA L1/1/,L8/8/
C
C              123456789 123456789 123456789 123456789 123456789
      DATA T1/'P?:learn from example    WZ_99'/
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_OWZ'
      CALL DPARAM(84
     &  ,J_OWZ)
      CALL DPARP0
      CALL DPAROP(84,'OWZ')
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TFIL(1:8)=TFILDC
      TWIZ(1:8)=TFILDC
      CALL DPARGI(81,'USL',LEVUS)
      FWIZDW=.FALSE.
  930 CALL DTYPT('TYPE',TPICDO,N_1_DA,N_2_DA,PARADA,TPOPDA,T1)
  936 FCHG=.FALSE.
      CALL DOPER(1,1,
     &  N_1_DA,N_2_DA,TPOPDA,PARADA,
     &  1,0,' ',0,
     &  NEXEC,CHG,TANSW)
      IF(NEXEC.EQ.1.AND.TANSW.EQ.'GW') GO TO 910
      GO TO (910,920,930,940),NEXEC
  910 RETURN
  920 CALL DO_STR('LW: List all examples')
      IF(TANSW.EQ.'LW') THEN
        CALL DGOPEN(NUNIDU,TFIL,2,*930,ISTAT)
        NEX=0
  921   READ(NUNIDU,1000,ERR=941,END=944) IEX,TNAM
        IF(IEX.GT.0..AND.IEX.LT.100.AND.IEX.NE.NEX) THEN
          WRITE(TXTADW,1000) IEX,TNAM
          CALL DWRC
          NEX=IEX
        END IF
        GO TO 921
      END IF
      CALL DO_STR('MA: run wizard as macro')
      IF(TANSW.EQ.'MA') THEN
        CALL DGOPEN(NUNIDU,TFIL,2,*930,ISTAT)
        NEX=PARADA(2,J_OWZ)
        NEX=100+NEX
  922   READ(NUNIDU,1000,ERR=941,END=923) IEX,T
        IF(IEX.NE.NEX) GO TO 922
        CLOSE(UNIT=NUNIDU)
        CALL DGINMA(T)
        GO TO 936
  923   CALL DWRT('No macro for that number.#')
        GO TO 936
      END IF
      CALL DO_STR('RW: read wizard from DALI_D?.WIZ')
      IF(TANSW.EQ.'RW') THEN
        CALL DGOPEN(NUNIDU,TWIZ,2,*936,ISTAT)
        DO K=1,1000
          READ(NUNIDU,3000) T
 3000     FORMAT(A)
          IF(T(1:2).EQ.'%%') THEN
            READ(T(3:4),3001,END=927) NEX
 3001       FORMAT(I2)
            GO TO 941
          END IF
        END DO
  927   TXTADW='Error in '//TWIZ//'#'
        CALL DWRC
        CLOSE(UNIT=NUNIDU)
        GO TO 936
      END IF
      CALL DO_STR('Z0: set wizard to 0; not to help')
      IF(TANSW.EQ.'Z0') THEN
        PARADA(2,J_OWZ)=0.
        GO TO 930
      END IF
      IF(TANSW.EQ.'X?') GO TO 930
      CALL DWR_IC(TANSW)
      GO TO 936
  940 CALL DGOPEN(NUNIDU,TFIL,2,*930,ISTAT)
      NEX=PARADA(2,J_OWZ)
  941 READ(NUNIDU,1000,ERR=941,END=949) IEX,TNAM
 1000 FORMAT(I4,1X,A)
      IF(IEX.NE.NEX) GO TO 941
      LNAM=LENOCC(TNAM)
      KMAC=0
      LUP=0
  942 READ(NUNIDU,1000,ERR=942,END=943) IEX,T
      IF(IEX.EQ.NEX) THEN
        L=LENOCC(T)
        IF(KMAC.GE.MMAC) THEN
          CALL DWRT('Example too long.#')
          GO TO 943
        END IF
        KMAC=KMAC+1
        TMAC(KMAC)=T
        LMAC(KMAC)=L
        GO TO 942
      END IF
      CLOSE(UNIT=NUNIDU)
  943 IMAC=0
      FWIZDW=.TRUE.
      FCONDW=.TRUE.
  944 CLOSE(UNIT=NUNIDU)
      GO TO 936
  949 CLOSE(UNIT=NUNIDU)
      CALL DWRT('No example of that number.#')
      GO TO 930
CH..............---
CH
CH
CH
CH
CH
CH
CH -------------------------------------------------------------  DWIZ_NEXT
CH
      ENTRY DWIZ_NEXT(LNC,TNC,FCONT)
CH
CH --------------------------------------------------------------------
CH
    1 IF(IMAC.EQ.KMAC) THEN
        CALL DWRT('Example finished.#')
        FWIZDW=.FALSE.
        CALL DQTIT(IFULDB)
        RETURN
      END IF
      IF(FCONDW) THEN
        IMAC=IMAC+1
        TCUR=TMAC(IMAC)
        LCUR=LMAC(IMAC)
        IF(TCUR(1:1).EQ.'!') THEN
          TUP=TCUR(3:LCUR)
          LUP=LCUR-2
    2     IF(TUP(LUP:LUP).EQ.'#') THEN
            CALL DWR_BELL(1,.FALSE.)
            LUP=LUP-1
            GO TO 2
          END IF
          GO TO 1
        END IF
        IF(TCUR(1:1).EQ.':') THEN
          TUP='The wizard typed for you "'
     &      //TCUR(3:LCUR)//'"'
          LUP=25+LCUR
          CALL DGINMA(TCUR(3:LCUR))
          TNC='**'
          LNC=2
          RETURN
        END IF
C       IF(TCUR(1:3).EQ.'IF(') THEN
C         CALL DOPER_TANSW(TA,NTYPE)
C         IF(NTYPE.EQ.2.AND.TA.EQ.TCUR(4:5)) THEN
C           TCUR=TCUR(8:LCUR)
C           LCUR=LCUR-7
C         ELSE
C           GO TO 1
C         END IF
C       END IF
      ELSE
        TCUR=TMAC(IMAC)
        LCUR=LMAC(IMAC)
      END IF
      IARE=IAREDO
      IAREDO=13
      CALL DQFFWI(L1)
      IAREDO=IARE
      V=VMINDG(M13)
      H=HMINDG(M13)
      CALL DGLEVL(L8)
      CALL DGTEXT(H+DH1,V+DV2,TUP,LUP)
      CALL DGTEXT(H+DH2,V+DV2,TNAM,LNAM)
      CALL DGTEXT(H+DH2,V+DV1,'Type "QW" to quit wizard.',25)
      DO I=1,LCUR
        IF(TCUR(I:I).EQ.'!') THEN
   22     IF(TCUR(LCUR:LCUR).EQ.'#') THEN
            CALL DWR_BELL(1,.FALSE.)
            LCUR=LCUR-1
            GO TO 22
          END IF
          CALL DGTEXT(H+DH1,V+DV1,TCUR(I+2:LCUR),LCUR-I-1)
          LNC=I-2
          GO TO 5
        END IF
      END DO
      CALL DGTEXT(H+DH1,V+DV1,TCLK,5)
      LNC=LCUR
    5 CALL DGCHKX
      IF(TCUR(1:2).EQ.TSTOP) THEN
        FCONDW=.FALSE.
        FCONT=.TRUE.
        TNC=TCUR(4:LNC)
        LNC=LNC-3
      ELSE
        TNC=TCUR(1:LNC)
        FCONT=.FALSE.
      END IF
      CALL DAREA('W',TNC(1:2),0,12,IAR,FYES)
      IF(FYES) THEN
c        IAREDO=IAR
        CALL DGINMA(TNC(1:2))
        TNC=TNC(4:LNC)
        LNC=LNC-3
        IF(LNC.LT.2) GO TO 1
      END IF
      END
