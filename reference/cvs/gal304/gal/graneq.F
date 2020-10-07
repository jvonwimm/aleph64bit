*DK graneq
#if defined(DOC)
CKEY GAL RNDM RANECU ALSEED
C!  GALEPH interface between RNDM and RANECU.
    The code of RANECU is put in the comdeck RANECU and then called
    in RNDM and GRNDM.
    the package must be initialize by a call to RECUST before the
    1st call to GRNDM or RNDM.
    the package contains the following entries:
      SUBROUTINE ALSEED(IRGEN,ISD1,ISD2)
      Output : IRGEN = generator ident (2 is RANECU)
               ISD1= first original seed
               ISD2= second original seed
      SUBROUTINE RDMIN(ISEED)
      FUNCTION RNDM(DUMMY)
      SUBROUTINE RDMOUT(ISEED)

#endif
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
#include "arndmc.h"
      IRGEN = 2
      ISD1 = ISEED1
      ISD2 = ISEED2
      RETURN
      END
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
#include "arndmc.h"
      INTEGER ISEED(*)
C
      ISEED1 = ISEED(1)
      ISEED2 = ISEED(2)
      NRVEC = LRVEC
C
      END
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
#include "arndmc.h"
      IF(NRVEC.EQ.LRVEC) THEN
#include "ranecu.h"
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
#include "arndmc.h"
      INTEGER ISEED(*)
C
      ISEED(1) = ISEED1
      ISEED(2) = ISEED2
      NRVEC = LRVEC
      RETURN
      END
