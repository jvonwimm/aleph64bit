      SUBROUTINE GRNDM(RNDMV,LRNDV)
C-----------------------------------------------------------------------
C - Author : F.RANJARD - 890608
C! GEANT user routine to get random numbers
C act as a server of 100 numbers from one RANECU call
CKEY GUSER RNDM RANECU / INTERNAL
C - Structure : subprogram
C               User Entry Name :GRNDM
C               External references :RANECU
C               Comdecks references : ARNDMC
C - Input  : LRNDV  = number of output random numbers
C - Output : RNDMV  = vector of random numbers in the range [0.,1.]
C
C-----------------------------------------------------------------------
      SAVE
      REAL RNDMV(*)
      PARAMETER (LRVEC=100)
      COMMON /ARNDMC/ RVEC(LRVEC),NRVEC,ISEED1,ISEED2
C
      IF(NRVEC+LRNDV.GT.LRVEC) THEN
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
C
      IF (LRNDV.EQ.1) THEN
         RNDMV(1) = RVEC(NRVEC+1)
      ELSEIF (LRNDV.EQ.5) THEN
         RNDMV(1) = RVEC(NRVEC+1)
         RNDMV(2) = RVEC(NRVEC+2)
         RNDMV(3) = RVEC(NRVEC+3)
         RNDMV(4) = RVEC(NRVEC+4)
         RNDMV(5) = RVEC(NRVEC+5)
      ELSEIF (LRNDV.EQ.2) THEN
         RNDMV(1) = RVEC(NRVEC+1)
         RNDMV(2) = RVEC(NRVEC+2)
      ELSEIF (LRNDV.EQ.3) THEN
         RNDMV(1) = RVEC(NRVEC+1)
         RNDMV(2) = RVEC(NRVEC+2)
         RNDMV(3) = RVEC(NRVEC+3)
      ELSE
         DO 1 I=1,LRNDV
            RNDMV(I)=RVEC(NRVEC+I)
 1       CONTINUE
      ENDIF
      NRVEC = NRVEC +LRNDV
      RETURN
      END
