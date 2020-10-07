*DK DUL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DUL
CH
      SUBROUTINE DUL
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modified by B.S. Nilsson                  23-Feb-1990
C      "      "     "                          16-Sep-1991 for X
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- D0
      COMMON /D0PLAF/ TPLFD0,TWRTD0
      CHARACTER *6 TPLFD0
      CHARACTER *1 TWRTD0
      COMMON /D0PFLG/ FTRMD0,FLOGD0,FPROD0,FSOLD0
      LOGICAL FTRMD0,FLOGD0,FPROD0,FSOLD0
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
C------------------------------------------------------------------- DM
      COMMON /DMACRC/ FMACDM,NLETDM,FMOPDM,FMINDM,FANIDM,FNTADM,
     &  FINMDM
      LOGICAL FMACDM,FMOPDM,FMINDM,FANIDM,FNTADM,FINMDM
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
C------------------------------------------------------------------- DA
      COMMON /DAUTOM/ IFILDA
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *1 DT1
      LOGICAL LPOP,FUSLR,FT
      CHARACTER *15 T
      DATA FUSLR/.FALSE./
      FT=FTRMD0
      CALL DWRT_SETUP('TERMINAL=ON')
      IF(FUSLR) GO TO 999
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_USL'
      CALL DPARAM(81
     &  ,J_USL)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      CALL DQHLP('UL ')
      IF(FMACDM) CALL DQHLP(']]')
      CALL DQHGT('++',LPOP)
      IF(.NOT.LPOP) CALL DQHLP('<+')
      LOLD=PARADA(2,J_USL)
C     LOLD=USLVDU
    1 WRITE(TXTADW,1003)
 1003 FORMAT('UL: Read Menu to select user level: 1 - 7 .')
      CALL DWR_HIGH_LIGHT(TXTADW,1,2)
      WRITE(TXTADW,1002) LOLD
 1002 FORMAT('current user level = ',I1,' <CR>=accept')
      CALL DTYANS(TXTADW,'01234567',LANSW)
      IF(LANSW.LT.0) GO TO 1
      LANSW=LANSW-1
      IF(LANSW.GE.0.AND.LANSW.NE.LOLD) THEN
C       USLVDU=LANSW
        PARADA(2,J_USL)=LANSW
        CALL DQHL0
      END IF
C     LOLD=USLVDU
      LOLD=PARADA(2,J_USL)
      WRITE(TXTADW,1004) LOLD
 1004 FORMAT('User level set to ',I1)
      CALL DWRC
  999 IF(FMACDM) CALL DQHLP('[[')
      FUSLR=.FALSE.
C     ............... Terminal = OFF/ON is also changed in DQHL0, therefore:
      FTRMD0=FT
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DUSL0
CH
      ENTRY DUSL0
CH
CH --------------------------------------------------------------------
CH
      FUSLR=.FALSE.
      OPEN(UNIT=NUNIDU,FILE='DALI.USL',STATUS='OLD',ERR=7)
      READ(NUNIDU,1006,ERR=7) T
 1006 FORMAT(A)
      IF(T(1:9).EQ.'USERLEVEL') THEN
        READ(T(10:11),1007,ERR=6) NUS
 1007   FORMAT(I2)
      END IF
      IF(NUS.GE.0.AND.NUS.LE.7) THEN
        USLVD=NUS
        CALL DWRT('DALI.USL: Userlevel = '//DT1(USLVD))
        CALL DPARSV(81,'USL',2,USLVD)
        FUSLR=.TRUE.
      END IF
      READ(NUNIDU,1006,END=6) T
      IF(T(1:9).EQ.'AUTO-READ') THEN
        READ(T(10:11),1007,ERR=6) IFILDA
        CALL DWRT('DALI.USL: AUTO-READ= '//DT1(FLOAT(IFILDA)))
      END IF
    6 CLOSE(UNIT=NUNIDU)
    7 END
*DK DUPCHG
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DUPCHG
CH
      SUBROUTINE DUPCHG
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
C INVOKED BY TANSW.EQ.'PM'  (DUPCHG)
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
C------------------------------------------------------------------- DW
      COMMON /DWORKT/ TXTADW
      CHARACTER *80 TXTADW
      CHARACTER *1 TXT1DW(80)
      EQUIVALENCE (TXTADW,TXT1DW)
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DT
      COMMON /DTVCCC/ DUMYDT(4),
     1         HUMIDT,HUMADT,VUMIDT,VUMADT,
     1         NSQUDT,NOBJDT,NOCLDT,HPOSDT,VPOSDT,
     1         FPOSDT,TPOSDT,AROTDT,RPOSDT,NTVIDT,
     1         FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,
     1         FX11DT
      LOGICAL  FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,FX11DT
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
      LOGICAL CHG
      CHARACTER *49 T
      CHARACTER *2 TANSW
      CHARACTER*2 TNU/'  '/ ! Why ?????
      CHARACTER *6 DT6
      DIMENSION PR(4,MPNPDU)
      LOGICAL FYES
      CALL DCOPTL
  930 T='PM:W?: Set up default user Parameters'
      CALL DWR_HL_AR(T)
      DO K=1,MPNPDU
         PR(2,K)=PARADU(K)
         PR(1,K)=PLIMDU(1,K)
         PR(3,K)=PLIMDU(2,K)
      END DO
      DO M=1,MPNPDU,5
        M4=MIN(MPNPDU,M+4)
        WRITE(TXTADW,1936) (TPR1DU(K),DT6(PARADU(K)),K=M,M4)
        CALL DWRC
 1936   FORMAT(5(A,'=',A,1X))
      END DO
      CHG=.FALSE.
  936 CALL DGZOOM(6,IAREDO,0,0)
      CALL DOPER(1,0,
     &  1,0,' ',0,
     &  1,MPNPDU,TPR1DU,PR,
     &  NEXEC,CHG,TANSW)
      CALL DGZOOM(6,-1,0,0)
      DO K=1,MPNPDU
         PARADU(K)=PR(2,K)
      END DO
      CALL DQWIL(BOTVDU)
C  Next call to define cursors for DALI window.
      CALL DGDFCU
      GO TO (910,920,930,940),NEXEC
  910 IF(TANSW.NE.'GW') CALL DCOPFL
      RETURN
  920 CALL DAREA('D',TANSW,0,12,IAREDO,FYES)
      IF(FYES) GO TO 940
      IF(TANSW.EQ.'NG') THEN
         FRETDT=.TRUE.
         GO TO 940
      END IF
      IF(TANSW.EQ.'DH') THEN
C       CALL DEVNEW
        IFULDB=1
        CALL DQTIT(IFULDB)
        GO TO 936
      END IF
      IF(TANSW.EQ.'DB') THEN
        CALL DGSYNC(1)
        GO TO 936
      END IF
      IF(TANSW.EQ.'NO') THEN
        CALL DGSYNC(0)
        GO TO 936
      END IF
      CALL DWR_IC(TANSW)
      GO TO 936
  940 NAR=IAREDO
      NCNT=NCNT+1
      CALL DGTIM0
      CALL DPCEAR(NAR)
      IAREDO=NAR
      CALL DGTIM1
      WRITE(TXTADW,1940) TWINDW(NAR),NCNT,TNU
 1940 FORMAT('Dali-timing: Displayed was window ',A,I10,1X,A)
      CALL DWRC
      FRETDT=.FALSE.
      TNU='  '
      GO TO 936
      END
