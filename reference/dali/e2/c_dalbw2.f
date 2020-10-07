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
      LOGICAL FLOLD,FTOLD,FTRM
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
        FTRMD0=.FALSE.
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
      CHARACTER TFORM*8/'( A,$)'/  ! For SGI
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
          WRITE(*,'(<N+2>A,$)') '+', CHAR(8)//CHAR(32)//CHAR(8),
     &                              (CHAR(7),I=1,N)
        ELSE
          WRITE(*,'(<N+1>A,$)') '+',(CHAR(7),I=1,N)
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
      CALL IDATE(IM,ID,IY)
      WRITE(TIM(1:8),1000) IY,IM,ID
 1000 FORMAT('_',3I2,'_')
      DO L=1,12
        IF(TIM(L:L).EQ.' ') TIM(L:L)='0'
      END DO
      END
