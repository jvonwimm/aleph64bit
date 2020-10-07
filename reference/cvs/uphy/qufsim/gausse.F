      SUBROUTINE GAUSSE(IPA,ENE)
C------------------------------------------------------------------
C------------------------------------------------------------------
#include "lundcom.h"
#include "resolu.h"
      DATA EPS /.000001/
C
      ENE= 0.
C
      POL= PLU(IPA,14)
      IF(POL.GT.90.) POL= 180.-POL
      AZI= PLU(IPA,16)
      EELE=p7lu(IPA,4)
      IF(EELE.LT.EPS) GOTO 999
C -- Gaussian fluctuation
      CALL RANNOR(ALEA,DUMDUM)
C -- Resolution depends on POL and AZI
      RESOL=1.+FLUCHE*FLUFAC(POL,AZI)*ALEA/SQRT(EELE)
     .        +FLUCON*DUMDUM/EELE
      IF(RESOL.LT.EPS) RESOL=0.
      ENE= RESOL*EELE
C
999   CONTINUE
      RETURN
      END
