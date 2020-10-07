*DK DLK
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DLK
CH
      SUBROUTINE DLK
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
C INVOKED BY TANSW.EQ.'LK'  (DLK)
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      PARAMETER (MP1=1)
      PARAMETER(NRLEN=1000)
      DIMENSION PR1(4,MP1)
      CHARACTER *2 TANSW,TP1(MP1),TC
      DATA TP1/'NU'/
      DATA PR1/0.,0.,99999.,0./
      CHARACTER *4 TB
      CHARACTER *8 DT8
      LOGICAL FCHG,FTYP
      DATA LB/20/
      DIMENSION NUBA(NRLEN)
      CHARACTER *49 T1,T2
C              123456789 123456789 123456789 123456789 123456789
      DATA T1/'P?:W?:Look   BanK: ???? NU_12345'/
  930 NUM=0
      IF(T1(LB:LB).EQ.'?') THEN
        CALL DTYPT('TYPE',TPICDO,1,MP1,PR1,TP1,T1)
      ELSE
        CALL DZLBKN(T1(LB:LB+3),NRLEN,NUBA,NUM)
  931   IF     (NUM.EQ.0) THEN
          CALL DTYPT('TYPE',TPICDO,1,MP1,PR1,TP1,T1)
          IF(T1(LB:LB).NE.'?') CALL DWRT('Non existent bank.')
        ELSE IF(NUM.EQ.1) THEN
          PR1(2,1)=NUBA(1)
          CALL DTYPT('TYPE',TPICDO,1,MP1,PR1,TP1,T1)
        ELSE
          CALL DTYPT('TYPE',TPICDO,1,MP1,PR1,TP1,T1)
          N=0
  934     T2='nu='
  935     L=LENOCC(T2)+1
          IF(L.GT.41) THEN
            CALL DWRT(T2)
            GO TO 934
          END IF
          IF(L.GT.4) THEN
            T2(L:L)=','
            L=L+1
          END IF
          N=N+1
          T2(L:L+7)=DT8(FLOAT(NUBA(N)))
          IF(N.LT.NUM) GO TO 935
          IF(T2(4:4).NE.' ') CALL DWRT(T2)
        END IF
      END IF
  936 FCHG=.FALSE.
      CALL DOPER(0,1,
     &  1,0,' ',0,
     &  1,MP1,TP1,PR1,
     &  NEXEC,FCHG,TANSW)
      IF(FCHG) CALL DLSETN(NUM,NUBA,PR1(2,1))
  937 GO TO (910,920,930,940) NEXEC
  910 RETURN
  920 IF(TANSW.EQ.'NA') THEN
        CALL DWRT('       Type 4 letter bank name without <CR>.')
        CALL DGETLN(TB,LN,4)
        IF(TB(1:1).EQ.'?') GO TO 923
        IF(LN.EQ.4) THEN
          FTYP=.FALSE.
          CALL DZLBKN(TB,NRLEN,NUBA,NUM)
          IF(NUM.GT.0)THEN
            T1(LB:LB+3)=TB
            CALL DLSETN(NUM,NUBA,PR1(2,1))
            GO TO 931
          ELSE
            CALL DWRT('Non existent bank.')
          END IF
        ELSE
          CALL DWRT('Type as in the following example "NA:FRFT"')
        END IF
        GO TO 930
      END IF
      IF(TANSW.EQ.'TY') THEN
  923   CALL DJLBNK(MUN6DU,MUN9DU)
        GO TO 930
      END IF
      IF(TANSW.EQ.'LB') THEN
        FTYP=.TRUE.
        NW=80
        NR=PR1(2,1)
        TC='LB'
        GO TO 939
      END IF
      IF(TANSW.EQ.'LA') THEN
        FTYP=.TRUE.
        NW=80
        TC='LA'
        GO TO 939
      END IF
      CALL DWR_IC(TANSW)
      GO TO 936
  939 CALL DQHLP('LKB')
      CALL DLWRB0(0,0.)
      CALL DZLOOK(NW,TANSW,T1(LB:LB+3),NR)
      GO TO 930
  940 IF(FTYP) THEN
        CALL DLKD0(T1(LB:LB+3),NR,TC)
        CALL DLKD
        GO TO 930
      ELSE
        CALL DWRT('Type "LB" OR "LA" first.')
        GO TO 936
      END IF
      END
*DK DLKD
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DLKD
CH
      SUBROUTINE DLKD
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'
      CHARACTER *2 TCOM(0:12),TC
      CHARACTER *4 TBNK(0:12),TB
      DIMENSION NLST(0:12),NDIR(0:12),NBNK(0:12)
      DATA DTH/8./,DTV/12.05/,N8/8/,LD/8/,L4/4/
      CALL DQCL(IAREDO)
      CALL DQWIL(0.)
      DH=HHGHDG(IAREDO)-HLOWDG(IAREDO)
      DV=VHGHDG(IAREDO)-VLOWDG(IAREDO)
      LEN=DH/DTH
      NUM=DV/DTV
      CALL DLWRB0(LEN,DTV)
      N2=NLST(IAREDO)
      N1=N2-NDIR(IAREDO)*(NUM-L4)
      IF(N1.LT.1) THEN
        N2=N2-N1+1
C       N1=N1-N1+1
        N1=1
      END IF
      CALL DGLEVL(N8)
      CALL DZLOKA(LEN+LD,TCOM(IAREDO),
     &                   TBNK(IAREDO),
     &                   NBNK(IAREDO),N1,N2)
      CALL DQFR(IAREDO)
      CALL DPCSAR
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DLKD0
CH
      ENTRY DLKD0(TB,NR,TC)
CH
CH --------------------------------------------------------------------
CH
C ---------------------------------------------------------------------
      TBNK(IAREDO)=TB
      TCOM(IAREDO)=TC
      NBNK(IAREDO)=NR
      CALL DZLGLI(NLST(IAREDO),NDIR(IAREDO))
      END
*DK DLOPER
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DLOPER
CH
      SUBROUTINE DLOPER(TCOM,GNUM)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   18-JUN-1992
C
C!: Read user commands
C   outputs    :
C         TCOM : 1 letter command
C         GNUM : number
C    Called by : DZLBKA,DZLBNK
C-------------------------------------------------------------------------
      PARAMETER (MCOM=12,M1=4)
      CHARACTER *1 TCOM,TIN(MCOM),T1(M1)
      DATA TIN/'L','C','N','W','T','D','M','V','~','+','=','-'/
C               1   2   3   4   5   6   7   8   9  10  11  12
      DATA T1/ ' ','F','B','S'/
      CHARACTER *5 TNUM
    1 CALL DGETLN(TCOM,LCOM,1)
      IF(LCOM.LE.0) THEN
        TCOM='S'
        GNUM=0.
        RETURN
      ELSE IF(TCOM.EQ.'H') THEN
        CALL DQHLP('<+')
        GO TO 1
      ELSE IF(TCOM.EQ.'<') THEN
        CALL DQHLP('<<')
        GO TO 1
      ELSE
        DO K=1,M1
          IF(TCOM.EQ.T1(K)) RETURN
        END DO
        DO K=1,MCOM
          IF(TCOM.EQ.TIN(K)) THEN
            CALL DGETLN(TNUM,NN,5)
            IF(NN.LE.0) THEN
              GNUM=0.
              RETURN
            ELSE
              READ(TNUM(1:NN),1000,ERR=90) NUM
 1000         FORMAT(BN,I5)
              GNUM=NUM
              RETURN
   90         READ(TNUM(1:NN),1001,ERR=91) GNUM
 1001         FORMAT(F6.4)
              RETURN
            END IF
          END IF
        END DO
   91   CALL DWR_ADD('?')
 1009   FORMAT('+?',$)
        GO TO 1
      END IF
      END
*DK DLSETN
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DLSETN
CH
      SUBROUTINE DLSETN(NUM,NUBA,PR1)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      DIMENSION NUBA(*)
      IF(NUM.EQ.0) THEN
        PR1=0.
      ELSE
        NP=PR1
        DO N=1,NUM
          IF(NP.EQ.NUBA(N)) RETURN
        END DO
        PR1=NUBA(1)
      END IF
      END
*DK DLWRBA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DLWRBA
CH
      SUBROUTINE DLWRBA(T)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   18-JUN-1992
C
C!: Write information on terminel
C    Inputs    :
C            T : Character variable
C    Called by :
C ---------------------------------------------------------------------
      INCLUDE 'DALI_CF.INC'
      CHARACTER *(*) T
      DATA DV1/14./,DH1/1./
      IF(NL.EQ.0) THEN
        CALL DWRT(T)
      ELSE
        IF(T(2:6).EQ.'-----') RETURN
        IF(T(2:6).EQ.'ALEPH') THEN
          LEN=LENOCC(T)
          IF(NL.LT.LEN) THEN
            DO L=1,LEN
              IF(T(L:L).EQ.':') THEN
                CALL DGTEXT(H1,V1,T,L-2)
                GO TO 7
              END IF
            END DO
          ELSE
            CALL DGTEXT(H1,V1,T,NL)
          END IF
        ELSE
          CALL DGTEXT(H1,V1,T,NL)
        END IF
    7   V1=V1-DV
      END IF
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DLWRB0
CH
      ENTRY DLWRB0(NLS,DVS)
CH
CH --------------------------------------------------------------------
CH
      DV=DVS
      NL=NLS
      H1=HMINDG(IAREDO)+DH1
      V1=VHGHDG(IAREDO)-DV1
      END
