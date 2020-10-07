CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      SUBROUTINE DWR_INPUT(TFIL)
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
C
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TFIL,TMOD
      LOGICAL FNTRM
      DATA FNTRM/.TRUE./
      IF(FNTRM) CALL DWRT_SETUP('TERMINAL=OFF')
      TXTADW='Input from file '//TFIL(1:LENOCC(TFIL))
      CALL DWRC
      IF(FNTRM) CALL DWRT_SETUP('TERMINAL=LAST')
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ------------------------------------------------ DWR_INPUT_SETUP
CH
      ENTRY DWR_INPUT_SETUP(TMOD)
CH
CH --------------------------------------------------------------------
CH
      IF(TMOD.EQ.'TERMINAL=ON') THEN
        FNTRM=.FALSE.
      ELSE
        FNTRM=.TRUE.
      END IF
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DWRD
CH
      SUBROUTINE DWRD(T,TPOS)

CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      CHARACTER *(*) T,TPOS,TV
      CHARACTER *80 TEXT
      CHARACTER *10 TOLD
      CHARACTER *8 TIM
      CALL DPARGV(81,'JDB',4,D4)
      IF(D4.EQ.1.) THEN
        CALL DPARGV(81,'JDB',2,D2)
        L=D2
        IF(TPOS.NE.TOLD) THEN
          CALL DWR_TO_FILE(L,' ')
          TOLD=TPOS
        END IF
        CALL TIME(TIM)
        WRITE(TEXT,1001) T,TIM,TPOS
        CALL DWRT(TEXT)
        WRITE(L,2000) TEXT
      END IF
      RETURN
 1000 FORMAT('C')
 1001 FORMAT(A,T60,'! ',A,1X,A)
 2000 FORMAT(A)
CH..............---
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DWRDI
CH
      ENTRY DWRDI(T,N,TPOS)

CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      CALL DPARGV(81,'JDB',4,D4)
      IF(D4.EQ.1.) THEN
        CALL DPARGV(81,'JDB',2,D2)
        L=D2
        IF(TPOS.NE.TOLD) THEN
          WRITE(L,1000)
          TOLD=TPOS
        END IF
        CALL TIME(TIM)
        WRITE(TEXT,1002) T,N,TIM,TPOS
        CALL DWRT(TEXT)
        WRITE(L,2000) TEXT
      END IF
      RETURN
 1002 FORMAT(A,I7,T60,'! ',A,1X,A)
CH..............---
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DWRDI
CH
      ENTRY DWRDT(T,TV,TPOS)

CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      CALL DPARGV(81,'JDB',4,D4)
      IF(D4.EQ.1.) THEN
        CALL DPARGV(81,'JDB',2,D2)
        L=D2
        IF(TPOS.NE.TOLD) THEN
          WRITE(L,1000)
          TOLD=TPOS
        END IF
        CALL TIME(TIM)
        LT=LENOCC(T)
        LV=MAX(1,LENOCC(TV))
        WRITE(TEXT,1003) T(1:LT),TV(1:LV),TIM,TPOS
        CALL DWRT(TEXT)
        WRITE(L,2000) TEXT
      END IF
 1003 FORMAT(A,'''',A,'''',T60,'! ',A,1X,A)
      END
*DK DWR_PLATFORM_TEXT
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DWR_PLATFORM_TEXT
CH
      SUBROUTINE DWR_PLATFORM_TEXT(TIND)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
      CHARACTER *(*) TP
      PARAMETER (KMAX=66)
      CHARACTER *2 TIND,TINDX(KMAX)
      CHARACTER *80 T(KMAX),TR
      CHARACTER *21 TFIL
      DATA TFIL/'DALI_D#.PLATFORM_TEXT'/
      LOGICAL FSTRT,FT
      DATA FSTRT/.TRUE./
      FT=.FALSE.
      GO TO 1
CH..............---
CH
CH
CH
CH
CH
CH
CH ----------------------------------------------  DWR_GET_PLATFORM_TEXT
CH
      ENTRY DW_GET_PLATFORM_TEXT(TIND,TP,LP)
CH
CH --------------------------------------------------------------------

C      TWRTD0=CHAR(10) CHAR(13)


      FT=.TRUE.
    1 IF(FSTRT) THEN
        TFIL(1:8)=TFILDC
        CALL DGOPEN(NUNIDU,TFIL,2,*9,ISTAT)
        FSTRT=.FALSE.
        K=0
    2   READ(NUNIDU,1000,END=4) TR
 1000   FORMAT(A)
        LR=LENOCC(TR)
        IF(TR(1:1).EQ.' '.AND.LR.GE.3) THEN
          IF(K.GE.KMAX) GO TO 4
          K=K+1
          TINDX(K)=TR(2:3)
          IF(LR.LT.5) THEN
            T(K)=' '
          ELSE
            T(K)=TR(5:LR)
          END IF
        END IF
        GO TO 2
    4   CLOSE(UNIT=NUNIDU)
      END IF
      IF(FT) TP(1:LP)=' '
      DO N=1,K
        IF(TIND.EQ.TINDX(N)) THEN
          IF(FT) THEN
            TP=T(N)(1:LP)
            IF(TIND.EQ.'WR'.AND.TP.EQ.'L') TP=CHAR(10)
            N1=K+1
            RETURN
          ELSE
            CALL DWRT(T(N))
          END IF
        END IF
      END DO
      IF(FT) THEN
        CALL DWRT(TIND//' not found in '//TFIL)
        TP=' '
      END IF
      RETURN
    9 CALL DWRT_SETUP('TERMINAL=ON')
      CALL DWRT('Fatal: File '//TFIL//' is missing!')
      STOP
CH..............---
CH
CH
CH
CH
CH
CH
CH -----------------------------------------------  DWR_NEXT_PLATFORM_TEXT
CH
      ENTRY DW_NEXT_PLATFORM_TEXT(TIND,TP,LP)
CH
CH --------------------------------------------------------------------
      DO N=N1,K
        IF(TIND.EQ.TINDX(N)) THEN
          TP=T(N)(1:LP)
          RETURN
        END IF
      END DO
      TP=' '
      END
*DK DWR_ADD_SEMICOLON
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DWR_ADD_SEMICOLON
CH
      SUBROUTINE DWR_ADD_SEMICOLON
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH  DT002
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modified by B.S. Nilsson                  26-Feb-1990
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C     DOP2LT
      INCLUDE 'DALI_CF.INC'

      LOGICAL FLGOOB,FLGDOP
      COMMON /OOBCMN/ FLGOOB(0:31),FLGDOP
      INTEGER IPNTDM
      CHARACTER TMACDM*2000
      COMMON/DMMACR/ IPNTDM,TMACDM
      CALL DWR_ADD('|')
      IF(FLGOOB(12).AND..NOT.FMACDM) THEN
        TMACDM(IPNTDM:IPNTDM)=':'
        IPNTDM=IPNTDM+1
      END IF
      RETURN
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++ DWR_ADD_TEXT_SETUP
CH
      SUBROUTINE DWR_ADD_TEXT_SETUP(T)
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
C     DOP2LT,DOPER
      CHARACTER *1 T,TT
      INCLUDE 'DALI_CF.INC'
      DATA IDEB/0/

      LOGICAL FLGOOB,FLGDOP
      COMMON /OOBCMN/ FLGOOB(0:31),FLGDOP

      INTEGER IPNTDM
      CHARACTER TMACDM*2000
      COMMON/DMMACR/ IPNTDM,TMACDM

      TT=T
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ---------------------------------------------------------  DWR_ADD_TEXT_DO
CH
      ENTRY DWR_ADD_TEXT_DO
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    Modified by B.S. Nilsson for macro recording, 26-Feb-1990
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C     DOP2LT,DOPER
      IF(IDEB.EQ.0) RETURN
      IF(TT.NE.'*') THEN
        CALL DWR_ADD(TT)
        IF(FLGOOB(12).AND..NOT.FMACDM) THEN
          TMACDM(IPNTDM:IPNTDM)=TT
          IPNTDM=IPNTDM+1
        END IF
      END IF
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DWR_LINE
CH
      SUBROUTINE DWR_LINE(L,TC)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TC
      IF(TC.EQ.'DEFAULT') THEN
        WRITE(TXTADW,1000) ('-----',I=1,10)
      ELSE
        WRITE(TXTADW,1000) (TC,I=1,L)
      END IF
 1000 FORMAT(80A)
      CALL DWRC
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DWR_HL_AR
CH
      SUBROUTINE DWR_HL_AR(TIN)
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
      CHARACTER *(*) TIN
      CHARACTER *49 T
      T=TIN
C     ............................. THIS IS NOT ALLOWED IF T IS IN THE
C     ..................... ARGUMENT AND THE ROUTINE IS CALLED: CALL D..('X')
      T(5:5)=TAREDO(IAREDO)(2:2)
      CALL DWR_HIGH_LIGHT(T,1,2)
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DWR_IC
CH
      SUBROUTINE DWR_IC(TANSW)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TANSW
      CALL DPARGV(84,'OBE',2,BELL)
      CALL DWR_BELL(IFIX(BELL),.FALSE.)
C
C   Terminate macro reading if any.
C
      IF(FMACDM) CALL DMCTRM
      WRITE (TXTADW,1000) TANSW
 1000 FORMAT('Invalid command "',A,'"')
      CALL DWRC
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++ DW_SPAWN_AND_PRINT
CH
      SUBROUTINE DW_SPAWN_AND_PRINT(T2,IER)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) T2
      CHARACTER *24 TPRT
      IER=0
      IF(TPLFD0.EQ.'VMS') THEN
        TPRT='DALI_PRINT_PS_BW.COM'
C       ................................................. INQUIREY
        OPEN(UNIT=NUNIDU,FILE=TPRT,STATUS='OLD',ERR=925)
        CALL DWRT('Executing: '//TPRT)
        CLOSE(UNIT=NUNIDU)
        CALL DSPAPR('@'//TPRT,T2(11:39))
        RETURN
  925   TPRT='DAL:DALI_PRINT_PS_BW.COM'
        OPEN(UNIT=NUNIDU,FILE=TPRT,STATUS='OLD',ERR=927)
        CALL DSPAPR('@'//TPRT,T2(11:39))
        RETURN
  927   CALL DWRT(TPRT//' not found: No print.')
        IER=1
      ELSE
        TPRT='lpr -Pps'
        CALL DSPAPR(TPRT,T2(11:39))
      END IF
      END
