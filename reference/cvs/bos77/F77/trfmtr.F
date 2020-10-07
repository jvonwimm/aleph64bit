      SUBROUTINE TRFMTR(FMT,LFM,IFMT,NFMT)

*     translate format FMT FROM integer array IFMT(1)...IFMT(NFMT)
*     into text string for printing

      CHARACTER*(*) FMT, NUMBER(0:9)*1
      INTEGER IFMT(*)
      DATA NUMBER/'0','1','2','3','4','5','6','7','8','9'/
      L = LEN(FMT)
      IF(NFMT.EQ.0) THEN
         FMT='F (DEFAULT)'
         LFM=11
         GOTO 30
      END IF
      IF(IFMT(3).NE.0) THEN
         LFM=3
         IF(IFMT(3).EQ.1) FMT='B32'
         IF(IFMT(3).EQ.2) FMT='B16'
         IF(IFMT(3).EQ.3) FMT='B08'
      ELSE
         LFM=0
         J=4
   10    IF(NFMT.GE.J) THEN
            IF(LFM.GE.L) THEN
               FMT(LFM:LFM+2)='...'
               GOTO 30
            END IF
            JL=IFMT(J)/10
            JR=MOD(IFMT(J),10)
            J =J+1
            IF(JL.EQ.0) THEN
               LFM=LFM+1
               FMT(LFM:LFM)=')'
               GOTO 10
            ELSE
            IF(LFM.NE.0) THEN
               IF(FMT(LFM:LFM).NE.'(') THEN
                  LFM=LFM+1
                  FMT(LFM:LFM)=','
               END IF
            END IF

*              insert number

               IF(JL.NE.1.AND.JL.NE.100000) THEN
                  NDIV=10000
                  LSTART=LFM
                  DO 20 KL=1,5
                  K=MOD(JL/NDIV,10)
                  NDIV=NDIV/10
                  IF(K.NE.0.OR.LFM.NE.LSTART) THEN
                     LFM=LFM+1
                     FMT(LFM:LFM)=NUMBER(K)
                  END IF
   20             CONTINUE
               END IF
               IF(JR.EQ.0) THEN
                  LFM=LFM+1
                  FMT(LFM:LFM)='('
               ELSE
                  LFM=LFM+1
                  IF(JR.EQ.1) FMT(LFM:LFM)='F'
                  IF(JR.EQ.2) FMT(LFM:LFM)='I'
                  IF(JR.EQ.3) FMT(LFM:LFM)='A'
                  IF(JR.EQ.4) FMT(LFM:LFM)='J'
                  IF(JR.EQ.5) FMT(LFM:LFM)='K'
               END IF
               GOTO 10
            END IF
         END IF
      END IF
   30 CONTINUE
  100 RETURN
      END
