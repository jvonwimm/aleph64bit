      FUNCTION RNDM(DUMMY)
C-----------------------------------------------------------------------
C - Author : B.Bloch-Devaux -890201
C!Interface from RNDM to RANECU.
C act as a server of 100 numbers from one RANECU call
C
C - Structure : REAL FUNCTION subprogram
C               User Entry Name :RNDM,RECUST
C               Comdecks references : ARNDMC
C - Input : DUMMY   dummy
C - Output : Real value between 0. and 1.
C
C-----------------------------------------------------------------------
      PARAMETER (LRVEC=100)
      COMMON /ARNDMC/ RVEC(LRVEC),NRVEC,ISEED1,ISEED2
      IF(NRVEC.EQ.LRVEC) THEN
C
C =================== CALL RANECU(RVEC,LRVEC)========================
         DO 100 I= 1, LRVEC
            K = ISEED1/53668
            ISEED1 = 40014*(ISEED1 - K*53668) - K*12211
            IF (ISEED1 .LT. 0) ISEED1=ISEED1+2147483563
C
            K = ISEED2/52774
            ISEED2 = 40692*(ISEED2 - K*52774) - K* 3791
            IF (ISEED2 .LT. 0) ISEED2=ISEED2+2147483399
C
            IZ = ISEED1 - ISEED2
            IF (IZ .LT. 1)  IZ = IZ + 2147483562
C
            RVEC(I) = REAL(IZ) * 4.6566128E-10
  100    CONTINUE
C =================== end of RANECU ===============================
        NRVEC=0
      ENDIF
      NRVEC=NRVEC+1
      RNDM=RVEC(NRVEC)
      RETURN
C - start up of RANECU
      ENTRY RECUST
      ISEED1 = 12345
      ISEED2 = 67890
      NRVEC = LRVEC
      RETURN
      END
