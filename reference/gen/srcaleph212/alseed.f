      SUBROUTINE ALSEED(IRGEN,ISEED1,ISEED2)
C-----------------------------------------------------------------------
C - Author : B.Bloch-Devaux -890201
C! returns original seeds from RNDM random generator
C
C - Structure : SUBROUTINE subprogram
C               User Entry Name :ALSEED
C               External references :RDMOUT
C               Comdecks references : none
C - Input : none
C - Output : IRGEN = generator ident (1 is RNDM)
C            ISEED1= first original seed
C            ISEED2= second original seed,irrelevant for RNDM
C
C  -------------------------------------------------
      IRGEN = 1
      CALL RDMOUT(ISEED1)
      ISEED2 = 0
      RETURN
      END
