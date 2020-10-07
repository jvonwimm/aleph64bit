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

      LOGICAL FLGOOB,FLGDOP
      COMMON /OOBCMN/ FLGOOB(0:31),FLGDOP

      INTEGER IPNTDM
      CHARACTER TMACDM*2000
      COMMON/DMMACR/ IPNTDM,TMACDM
      DATA IDEB/0/

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
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++  DWR_TO_FILE
CH
      SUBROUTINE DWR_TO_FILE(NOFI,TXT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    
C ---------------------------------------------------------------------
      CHARACTER *(*) TXT
      LL=LENOCC(TXT)
      WRITE(NOFI,1000) TXT(1:LL)
 1000 FORMAT(1X,A)
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DWRT
CH
      SUBROUTINE DWRT(TXTA)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :TEXT STRING = TXTA 
C    
C ---------------------------------------------------------------------
*CA DALLCO
      LOGICAL FLGOOB, FLGDOP
      COMMON /OOBCMN/ FLGOOB(0:31),FLGDOP

      INTEGER IPNTDM
      CHARACTER TMACDM*2000
      COMMON/DMMACR/ IPNTDM,TMACDM

      INCLUDE 'DALI_CF.INC'

      CHARACTER *(*) TXTA,TLC,TIN
      CHARACTER *1 TC,TCOLD,TCC,TCHA(-2:3)
C                                  -2  -1   0   1   2   3 
      DATA TC/' '/,TCOLD/'|'/,TCHA/'=','-','r','.',':','"'/
      LOGICAL FLOLD,FTOLD,FTRM,FTDEB,FSTRT

      DATA FTDEB/.FALSE./,FSTRT/.TRUE./
C     .......... Set FTDEB/.TRUE./ for a test version only, if you want
C     .......... to see on the terminal the logfile output.
C     .......... Set FTDEB/.FALSE./ for the official version.

      DATA LMAX/52/
      DATA FLOLD/.TRUE./,FTOLD/.TRUE./
      DATA JDEB/0/
      TXTADW=TXTA
CH..............---
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DWRC
CH
      ENTRY DWRC
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     Text input from common, TXTADW
C
C     ....................... TWRTD0=' ' for VMS
C     ....................... TWRTD0='0' for ULTRIX
C     ....................... TWRTD0 is read from file DALI_??.PLATFORM_TEXT
C
C     Bjorn: This might be a simple method to get rid of as much as possible
C     platform dependent code.
C
      IF(FSTRT) THEN
        FSTRT=.FALSE.
        IF(FTDEB) WRITE(MUN6DU,1000) TWRTD0,'FTDEB = .TRUE.',TCC
      END IF

      LL=MAX(1,LENOCC(TXTADW))
      IF(TXTADW(LL:LL).EQ.'+') THEN
        TCC=' '
        LL=LL-1
      ELSE
        TCC=TC
      END IF
      FTRM=FTRMD0
      IF(TXTADW(LL:LL).EQ.'#') THEN
        CALL DWR_BELL(1,.FALSE.)
        LL=LL-1
        FTRMD0=.TRUE.
      END IF
      IF(LL.LE.LMAX.AND.TCC.NE.' ') THEN
        IF(FTRMD0) THEN
          LL1=MIN(LL,52-LEN(TWRTD0))
          WRITE(MUN6DU,1000) TWRTD0,TXTADW(1:LL1),TCC
          FSOLD0=.TRUE.
        END IF
        IF(FLOGD0) WRITE(MUN9DU,1000) TWRTD0,TXTADW(1:LL),TCC
      ELSE
        IF(FTRMD0) THEN
          WRITE(MUN6DU,1001) TWRTD0,TXTADW(1:LL)
          FSOLD0=.TRUE.
        END IF
        IF(FLOGD0) WRITE(MUN9DU,1001) TWRTD0,TXTADW(1:LL)
      END IF
      FTRMD0=FTRM
      IF(TXTADW.EQ.'???') THEN
C       ................................ This is required to set a break point
  333   TXTADW( 1: 3)='???'
      END IF  
      TXTADW( 1: 3)='???'
      TXTADW(49:50)='#'
      RETURN
 1000 FORMAT(2A,T53,A,$)
 1001 FORMAT(2A,$)

CH..............---
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++ DWR_HIGH_LIGHT_FROM_N1_TO_N2
CH
      ENTRY DWR_HIGH_LIGHT(TIN,N1,N2)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C --------------------------------------------------------------------
C
      ENTRY DWR_HIGH_LIGHT_FROM_N1_TO_N2(TIN,N1,N2)
      N4=MIN(LEN(TIN),52-LEN(TWRTD0))
      IF(FTRMD0) THEN
        IF(N1.GT.0) THEN
          N3=MIN(LEN(TIN),60-LEN(TWRTD0)-LEN(TREVDT)-LEN(TNORDT))
          WRITE(MUN6DU,2000) TWRTD0,TIN(1:N1-1),
     &                       TREVDT,TIN(N1:N2),
     &                       TNORDT,TIN(N2+1:N3)
        ELSE
          WRITE(MUN6DU,2001) TWRTD0,TIN(1:N4)
        END IF
        FSOLD0=.TRUE.
      END IF
      IF(FLOGD0) WRITE(MUN9DU,2001) TWRTD0,TIN(1:N4)
      RETURN
 2000 FORMAT(6A,T61,'|',$)
 2001 FORMAT(2A,T53,'|',$)

CH..............---
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DWR0
CH
      ENTRY DWRT_SETUP(TLC)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     INPUT         LOGFILE=ON 
C                   TERMINAL=ON
C                   LOGFILE=OFF
C                   TERMINAL=OFF
C                   LOGFILE=LAST
C                   TERMINAL=LAST
C                   END CHARACTER=:
C                   DEFAULT
      IF(TLC.EQ.'DEFAULT') THEN
        FLOLD=FLOGD0
        FTOLD=FTRMD0
        TCOLD=TC
        FTRMD0=.TRUE.
        FLOGD0=.TRUE.
        TC='|'
      ELSE IF(TLC.EQ.'LOGFILE=ON') THEN
        FLOLD=FLOGD0
        FLOGD0=.TRUE.
      ELSE IF(TLC.EQ.'TERMINAL=ON') THEN
        FTOLD=FTRMD0
        FTRMD0=.TRUE.
      ELSE IF(TLC.EQ.'LOGFILE=OFF') THEN
        FLOLD=FLOGD0
        FLOGD0=.FALSE.
C       WRITE(MUN9DU,1001) TWRTD0,'Output on LOGFILE supressed.'
      ELSE IF(TLC.EQ.'TERMINAL=OFF') THEN
        FTOLD=FTRMD0
        FTRMD0=FTDEB
      ELSE IF(TLC.EQ.'LOGFILE=LAST') THEN
C       IF((.NOT.FLOLD).AND.FLOGD0)
C    &    WRITE(MUN9DU,1001) TWRTD0,'Output on LOGFILE supressed.'
        FLOGD0=FLOLD
      ELSE IF(TLC.EQ.'TERMINAL=LAST') THEN
        FTRMD0=FTOLD
      ELSE IF(TLC(1:14).EQ.'END CHARACTER=') THEN
        TC=TLC(15:15)
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ DWRT_END
CH
      ENTRY DWRT_END(NCHA)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C     ................................ add ', ", ~ or > to last line of text
C     ................................ before accepting terminal input.
C      F2LTD0=.FALSE.
      IF(JDEB.EQ.1.AND.FMACDM) RETURN
      IF(FPROD0) THEN 
        IF(     NCHA.EQ.0) THEN
          CALL DWR_ADD('<cr>')
        ELSE IF(NCHA.EQ.1) THEN
          CALL DWR_ADD(TCHA(NCHA))
        ELSE IF(NCHA.EQ.2.OR.NCHA.EQ.-2) THEN
          CALL DWR_ADD(TCHA(NCHA))
C         ............................................ macro learning
          IF(FLGOOB(12).AND..NOT.FMACDM) THEN
            TMACDM(IPNTDM:IPNTDM)=TCHA(NCHA)
            IPNTDM=IPNTDM+1
          END IF
        ELSE IF(NCHA.EQ.3) THEN
          CALL DWR_ADD(TCHA(1))
          CALL DWR_ADD(TCHA(2))
        ELSE IF(NCHA.EQ.4) THEN
          CALL DWR_ADD(TCHA(2))
          CALL DWR_ADD(TCHA(2))
        ELSE IF(NCHA.LT.0) THEN
          RETURN
        ELSE
          CALL DWR_ADD(TCHA(3))
        END IF
      END IF
      FPROD0=.TRUE.
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++  DWR_ADD_SEMICOLON_DELET
CH
      SUBROUTINE DWR_ADD_SEMICOLON_DELET
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH  DT002A
C ---------------------------------------------------------------------
C
C    Modified by B.S. Nilsson                  26-Feb-1990
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C   This subroutine is similar to DWR_ADD('|')
C   but is called after the Delete
C  button has been pressed. It writes to the log file and then backs up
C  the cursor and blanks the previous character.
C
C  Björn S. Nilsson, 24-Apr-1989
C
      INCLUDE 'DALI_CF.INC'
      IF(TPLFD0.EQ.'HP'.OR.TPLFD0.EQ.'SGI'.OR.TPLFD0.EQ.'LINUX') THEN
        WRITE(MUN9DU,'(A,$)') '<DEL>|'
        WRITE(MUN6DU,'(A,$)')      CHAR(8)//CHAR(32)//CHAR(8)
      ELSE
        WRITE(MUN9DU,'(A,$)') '+<DEL>|'
        WRITE(MUN6DU,'(A,$)') '+'//CHAR(8)//CHAR(32)//CHAR(8)
      END IF
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DWR_ADD
CH
      SUBROUTINE DWR_ADD(T)
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
      CHARACTER *(*) T
      IF(FTRMD0) THEN
        IF(TPLFD0.EQ.'HP'.OR.TPLFD0.EQ.'SGI'.OR.TPLFD0.EQ.'LINUX') THEN
          WRITE(MUN6DU,'(A,$)')      T
        ELSE
          WRITE(MUN6DU,'(A,A,$)') '+',T
        END IF
      END IF
      IF(TPLFD0.EQ.'HP'.OR.TPLFD0.EQ.'SGI'.OR.TPLFD0.EQ.'LINUX') THEN
        WRITE(MUN9DU,'(A,$)')      T
      ELSE
        WRITE(MUN9DU,'(A,A,$)') '+',T
      END IF
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++ DWR_OVER_PRINT
CH
      SUBROUTINE DWR_OVER_PRINT(L)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH  
C --------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    : L = length of string
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      IF(TPLFD0.EQ.'HP'.OR.TPLFD0.EQ.'SGI'.OR.TPLFD0.EQ.'LINUX') THEN
        IF(     TXTADW(L:L).EQ.'>') THEN
          WRITE(MUN6DU,1000) CHAR(13),TXTADW(1:L-1),'<cr>'
        ELSE IF(TXTADW(L:L).EQ.'m') THEN
          WRITE(MUN6DU,1000) CHAR(13),TXTADW(1:L-1),'mm'
        ELSE
          WRITE(MUN6DU,1000) CHAR(13),TXTADW(1:L-1),TXTADW(L:L)
        END IF
C       ................................. why ' |' and not '|' ???? HD
 1000   FORMAT(2A,T53,'|',A,$)
      ELSE
        IF(     TXTADW(L:L).EQ.'>') THEN
          WRITE(MUN6DU,2000) CHAR(13),TXTADW(1:L-1),'<cr>'
        ELSE IF(TXTADW(L:L).EQ.'m') THEN
          WRITE(MUN6DU,2000) CHAR(13),TXTADW(1:L-1),'mm'
        ELSE
          WRITE(MUN6DU,2000) CHAR(13),TXTADW(1:L-1),TXTADW(L:L)
        END IF
C       ................................. why ' |' and not '|' ???? HD
 2000   FORMAT('+',2A,T53,' |',A,$)
      END IF
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++ DWR_BACKSPACE_SLASH
CH
      SUBROUTINE DWR_BACKSPACE_SLASH(LENB)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH  
      INCLUDE 'DALI_CF.INC'
      IF(TPLFD0.EQ.'HP'.OR.TPLFD0.EQ.'SGI'.OR.TPLFD0.EQ.'LINUX') THEN
        DO I = 1, LENB
          WRITE(*,'(A,$)')      CHAR(8)//CHAR(32)//CHAR(8)
        END DO
      ELSE
        DO I = 1, LENB
          WRITE(*,'(A,$)') '+'//CHAR(8)//CHAR(32)//CHAR(8)
        END DO
      END IF
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++ DWR_BLBL_TO_DO
CH
      SUBROUTINE DWR_BLBL_TO_DO
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH 
      INCLUDE 'DALI_CF.INC'
      IF(TPLFD0.EQ.'HP'.OR.TPLFD0.EQ.'SGI'.OR.TPLFD0.EQ.'LINUX') THEN
        WRITE(*,'(A,$)')      CHAR(8)//CHAR(8)//'DO' ! Backspace and 'DO'
      ELSE
        WRITE(*,'(A,$)') '+'//CHAR(8)//CHAR(8)//'DO' ! Backspace and 'DO'
      END IF
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++ DWR_BACKSPACE_DO
CH
      SUBROUTINE DWR_BACKSPACE_DO
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH  
      INCLUDE 'DALI_CF.INC'
      IF(TPLFD0.EQ.'HP'.OR.TPLFD0.EQ.'SGI'.OR.TPLFD0.EQ.'LINUX') THEN
        WRITE(*,'(A,$)')      CHAR(8)//'DO' ! Backspace and 'DO'
      ELSE
        WRITE(*,'(A,$)') '+'//CHAR(8)//'DO' ! Backspace and 'DO'
      END IF
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++ DWR_BACKSPACE_DO
CH
      SUBROUTINE DWR_BACKSPACE_BLANK
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH  
      INCLUDE 'DALI_CF.INC'
      IF(TPLFD0.EQ.'HP'.OR.TPLFD0.EQ.'SGI'.OR.TPLFD0.EQ.'LINUX') THEN
        WRITE(*,'(A,$)')      CHAR(8)       ! Backspace on blank
      ELSE
        WRITE(*,'(A,$)') '+'//CHAR(8)       ! Backspace on blank
      END IF
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++ DWR_BELL
CH
      SUBROUTINE DWR_BELL(NBELL,LERAS)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH  
C      SUBROUTINE DTBELL(NBELL,LERAS)
C   Author : Chris Grab   3-Sep-89 and B. S. Nilsson, 7-Feb-1990
C   Purpose: Ring bell in interactive modes; wake user .....
C
      INCLUDE 'DALI_CF.INC'
      INTEGER NBELL,I
      LOGICAL LERAS
      CHARACTER TFORM*8/'( A,$)'/
      N=MIN(8,NBELL)
      IF(TPLFD0.EQ.'HP'.OR.TPLFD0.EQ.'SGI'.OR.TPLFD0.EQ.'LINUX') THEN
        IF(LERAS) THEN
CSGI      WRITE(*,'(<N+1>A,$)')      CHAR(8)//CHAR(32)//CHAR(8),
          WRITE(TFORM(2:2),'(I1)') N+1
          WRITE(*,TFORM)      CHAR(8)//CHAR(32)//CHAR(8),
     &                              (CHAR(7),I=1,N)
        ELSE
CSGI      WRITE(*,'(<N  >A,$)')     (CHAR(7),I=1,N)
          WRITE(TFORM(2:2),'(I1)') N
          WRITE(*,TFORM)     (CHAR(7),I=1,N)
        END IF
      ELSE
        IF(LERAS) THEN
CSGI      WRITE(*,'(<N+2>A,$)') '+', CHAR(8)//CHAR(32)//CHAR(8),
          WRITE(TFORM(5:5),'(I1)') N+2
          WRITE(*,TFORM) '+', CHAR(8)//CHAR(32)//CHAR(8),
     &                              (CHAR(7),I=1,N)
        ELSE
CSGI      WRITE(*,'(<N+1>A,$)') '+',(CHAR(7),I=1,N)
          WRITE(TFORM(2:2),'(I1)') N+1
          WRITE(*,TFORM) '+',(CHAR(7),I=1,N)
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
CH ++++++++++++++++++++++++++++++++++++++++ DW_TIME
CH
      SUBROUTINE DW_TIME(TIM)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH  
C
C     20-SEP-1995 17:39:00 is converted to:
C     output:    TIM='_950920_1739'     as character string
C
      CHARACTER *12 TIM
      CALL TIME(TIM(1:8))
      TIM(11:12)=TIM(4:5)
      TIM( 9:10)=TIM(1:2)
      CALL IDATE2(IM,ID,IY)
      WRITE(TIM(1:8),1000) IY,IM,ID
 1000 FORMAT('_',3I2,'_')
      DO L=1,12
        IF(TIM(L:L).EQ.' ') TIM(L:L)='0'
      END DO
      END
