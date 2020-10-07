      SUBROUTINE RDMIN(ISEED)
C-----------------------------------------------------------------------
C - Author : B.Bloch-Devaux -890201
C
C!Interface from RDMIN to RECUIN (RANECU initialization)
C initialize with ISEED(1) and ISEED(2) and reset the server
C - Structure : SUBROUTINE program
C               User Entry Name :RDMIN
C               External references :RECUIN, RRESET
C - Input : ISEED   2 input seeds
C - Output : none
C
C-----------------------------------------------------------------------
      PARAMETER (LRVEC=100)
      COMMON /ARNDMC/ RVEC(LRVEC),NRVEC,ISEED1,ISEED2
      INTEGER ISEED(*)
C
      ISEED1 = ISEED(1)
      ISEED2 = ISEED(2)
      NRVEC = LRVEC
C
      END
