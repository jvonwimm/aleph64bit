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
      INCLUDE 'DALI_CF.INC'
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
      INCLUDE 'DALI_CF.INC'
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
