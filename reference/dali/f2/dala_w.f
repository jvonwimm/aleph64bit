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
      CHARACTER *(*) TP,TAR(*)
      PARAMETER (KMAX=366)
      CHARACTER *2 TIND,TINDX(KMAX)
      CHARACTER *80 T(KMAX),TR
      CHARACTER *13 TFIL
      DATA TFIL/'PLATFORM_TEXT'/
      DO N=1,K
        IF(TIND.EQ.TINDX(N)) CALL DWRT(T(N))
      END DO
      RETURN
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
      TP(1:LP)=' '
      DO N=1,K
        IF(TIND.EQ.TINDX(N)) THEN
          TP=T(N)(1:LP)
          IF(TIND.EQ.'WR'.AND.TP.EQ.'L') TP=CHAR(10)
          N1=K+1
          RETURN
        END IF
      END DO
      CALL DWRT(TIND//' not found in '//TFILDC//TFIL)
      TP=' '
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ----------------------------------------------  DWR_READ_PLATFORM_TEXT
CH
      ENTRY DW_READ_PLATFORM_TEXT
CH
CH --------------------------------------------------------------------
      CALL DGOPEN(NUNIDU,TFILDC//TFIL,2,*9,ISTAT)
      K=0
    2 READ(NUNIDU,1000,END=4) TR
 1000 FORMAT(A)
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
    4 CLOSE(UNIT=NUNIDU)
      RETURN
    9 CALL DWRT_SETUP('TERMINAL=ON')
      CALL DWRT('Fatal: File '//TFILDC//TFIL//' is missing!')
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
      ENTRY DW_GET_PLATFORM_TEXT_ARRAY(TIND,LP,NAR,TAR,LAR)
CH
CH --------------------------------------------------------------------
CH
      LAR=0
      DO N=1,K
        IF(TIND.EQ.TINDX(N)) THEN
          LAR=LAR+1
          TAR(LAR)=T(N)(1:LP)
          IF(LAR.GE.NAR) RETURN
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
CH ---------------------------------------------- DWF_PLATFORM_TEXT
CH
      ENTRY DWF_PLATFORM_TEXT(TIND,NOFI)
CH
CH --------------------------------------------------------------------
CH
      DO N=1,K 
        IF(TIND.EQ.TINDX(N)) THEN
          LL=LENOCC(T(N))
          IF(LL.EQ.2) THEN
            READ(T(N)(1:2),2001,ERR=10) NBLIN
 2001       FORMAT(I2)
            DO L=1,NBLIN
              WRITE(NOFI,2000) ' '
            END DO
            GO TO 11

   10       WRITE(NOFI,2000) T(N)(1:LL)
 2000       FORMAT(A)

          ELSE
            WRITE(NOFI,2000) T(N)(1:LL)
          END IF
        END IF
   11 END DO
      RETURN
      END
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++  DWF
CH
      SUBROUTINE DWF
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C    
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) TFIL
      CHARACTER *80 TROT
      DATA NOFI/29/,N1/6/,N3/50/,N2/12/
      LL=LENOCC(TXTADW)
      IF(IROT.EQ.0) THEN
        WRITE(NOFI,1000) TXTADW(1:LL)
      ELSE
        IF(TXTADW(N1:N1).EQ.'|'.AND.TXTADW(N3:N3).EQ.'|') THEN
          TROT=TXTADW(   1:N1  )
     &       //TXTADW(N2+1:N3-1)
     &       //'|'
     &       //TXTADW(N1+1:N2-1)
     &       //TXTADW(N3:LL)
C         TROT=TXTADW(   1:N1  )
C    &       //TXTADW(N2+1:N3-1)
C    &       //TXTADW(N2  :N2  )
C    &       //TXTADW(N1+1:N2-1)
C    &       //TXTADW(N3:LL)
          WRITE(NOFI,1000) TROT(1:LL)
        ELSE
          WRITE(NOFI,1000) TXTADW(1:LL)
        END IF
      END IF
 1000 FORMAT(1X,A)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ----------------------------------------------------------- DWF_OPEN
CH
      ENTRY DWF_OPEN(IOFI,TFIL,IER,NROT)
CH
CH --------------------------------------------------------------------
CH
      NOFI=IOFI
      IROT=NROT
      OPEN(NOFI,STATUS='NEW',FILE=TFIL,ERR=999)
      IER=0
      RETURN
  999 IER=1
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH ----------------------------------------------------------- DWF_CLOSE
CH
      ENTRY DWF_CLOSE
CH
CH --------------------------------------------------------------------
CH
      CLOSE (UNIT=NOFI)
      END
