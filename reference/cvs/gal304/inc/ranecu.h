*CD ranecu
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
