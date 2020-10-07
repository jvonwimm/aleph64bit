*DK edtadj
      SUBROUTINE EDTADJ(JPHI,KTET,NBVOI,LIVOI)
C----------------------------------------------------------------
C     O.CALLOT  26-NOV-85
C! Give list of adj. towers
C  in LIVOI ordered from I and J indices
C.    from Phi and Theta indices.
C. - called from  EDTONO,EDTZSU                          this .HLB
C. - calls        ECRWRG                                 this .HLB
C.---------------------------------------------------------------
#ifndef DOC
      SAVE
      DIMENSION LIVOI(2,*)
      DIMENSION JINF(0:2,3),JNB(0:2,3)
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
C
C  === initialize JINF and JNB
C
      IF(FIRST) THEN
        CALL EDTAIN(JINF,JNB)
        FIRST = .FALSE.
      ENDIF
C
      NBVOI = 0
      CALL ECRWRG(KTET,NRC,MAXC)
C
C  === first, look for lower theta index
C
      KTMI = KTET-1
      IF(KTMI.EQ.0)  GO TO 100
      CALL ECRWRG(KTMI,NRM,MAXM)
      NB1  = 1
      IF(NRM.EQ.NRC) THEN
        JMIN = JPHI-1
        NB2 = NB1 + 2
      ELSE IF(NRM.LT.NRC) THEN
        JMIN = NRM * ((JPHI-1)/NRC) + MOD(JPHI-1,NRC)
        NB2 = NB1 + 1
      ELSE
        K = (JPHI-1)/NRC
        L = MOD(JPHI-1,NRC)
        JMIN = NRM * K + JINF(L,NRC)
        NB2  = NBVOI + JNB(L,NRC)
      ENDIF
      DO 10 J=NB1,NB2
        LIVOI(1,J) = JMIN
        LIVOI(2,J) = KTMI
        JMIN = JMIN + 1
   10 CONTINUE
      IF(LIVOI(1,NB1).EQ.0) THEN
        CALL UCOPY2(LIVOI(1,NB1+1),LIVOI(1,NB1),2*(NB2-NB1))
        LIVOI(1,NB2) = MAXM
      ELSE IF(LIVOI(1,NB2).EQ.MAXM+1) THEN
        CALL UCOPY2(LIVOI(1,NB1),LIVOI(1,NB1+1),2*(NB2-NB1))
        LIVOI(1,NB1) = 1
      ENDIF
      NBVOI = NB2
C
C  === now, the actual theta row
C
  100 IF(JPHI.EQ.1) THEN
        LIVOI(1,NBVOI+1) = JPHI+1
        LIVOI(1,NBVOI+2) = MAXC
      ELSE IF(JPHI.EQ.MAXC) THEN
        LIVOI(1,NBVOI+1) = 1
        LIVOI(1,NBVOI+2) = JPHI-1
      ELSE
        LIVOI(1,NBVOI+1) = JPHI-1
        LIVOI(1,NBVOI+2) = JPHI+1
      ENDIF
      LIVOI(2,NBVOI+1) = KTET
      LIVOI(2,NBVOI+2) = KTET
      NBVOI = NBVOI + 2
C
C  === now, look for upper theta index
C
      KTPL = KTET+1
      IF(KTPL.EQ.229)  GO TO 200
      CALL ECRWRG(KTPL,NRP,MAXP)
      NB1  = NBVOI + 1
      IF(NRP.EQ.NRC) THEN
        JMIN = JPHI-1
        NB2 = NB1 + 2
      ELSE IF(NRP.LT.NRC) THEN
        JMIN = NRP * ((JPHI-1)/NRC) + MOD(JPHI-1,NRC)
        NB2 = NB1 + 1
      ELSE
        K = (JPHI-1)/NRC
        L = MOD(JPHI-1,NRC)
        JMIN = NRP * K + JINF(L,NRC)
        NB2 = NBVOI + JNB(L,NRC)
      ENDIF
      DO 110 J=NB1,NB2
        LIVOI(1,J) = JMIN
        LIVOI(2,J) = KTPL
        JMIN = JMIN + 1
  110 CONTINUE
      IF(LIVOI(1,NB1).EQ.0) THEN
        CALL UCOPY2(LIVOI(1,NB1+1),LIVOI(1,NB1),2*(NB2-NB1))
        LIVOI(1,NB2) = MAXP
      ELSE IF(LIVOI(1,NB2).EQ.MAXP+1) THEN
        CALL UCOPY2(LIVOI(1,NB1),LIVOI(1,NB1+1),2*(NB2-NB1))
        LIVOI(1,NB1) = 1
      ENDIF
      NBVOI = NB2
  200 CONTINUE
      RETURN
      END
#endif
