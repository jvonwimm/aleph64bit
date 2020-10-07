      SUBROUTINE ALSEED(IRGEN,ISD1,ISD2)
C-----------------------------------------------------------------------
C - Author : B.Bloch-Devaux -890201
C! returns original seeds from RANECU random generator
CKEY GAL RANECU SEED / USER
C - Structure : SUBROUTINE subprogram
C               User Entry Name :ALSEED
C               External references :RECUUT
C               Comdecks references : none
C - Input : none
C - Output : IRGEN = generator ident (2 is RANECU)
C            ISD1= first original seed
C            ISD2= second original seed
C
C-----------------------------------------------------------------------
      PARAMETER (LRVEC=100)
      COMMON /ARNDMC/ RVEC(LRVEC),NRVEC,ISEED1,ISEED2
      IRGEN = 2
      ISD1 = ISEED1
      ISD2 = ISEED2
      RETURN
      END
