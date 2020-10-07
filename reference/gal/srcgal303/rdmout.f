      SUBROUTINE RDMOUT(ISEED)
C-----------------------------------------------------------------------
C - Author : B.Bloch-Devaux -890201
C! Initialize RANECU and reset the server
C - Structure : SUBROUTINE subprogram
C               User Entry Name :RDMOUT

C               Comdecks references : ARNDMC
C - Input : none
C - Output : Array of generator seeds (integer)
C
C-----------------------------------------------------------------------
      PARAMETER (LRVEC=100)
      COMMON /ARNDMC/ RVEC(LRVEC),NRVEC,ISEED1,ISEED2
      INTEGER ISEED(*)
C
      ISEED(1) = ISEED1
      ISEED(2) = ISEED2
      NRVEC = LRVEC
      RETURN
      END
