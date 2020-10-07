*DK euppar
      SUBROUTINE EUPPAR
C----------------------------------------------
C-   M.N Minard              10.3.95
C    Update digitisation parameters for ECAL
C    when available
C    Called from ECIRUN
C----------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "edpajj.h"
#include "edpara.h"
#include "bmacro.h"
      NAEDPA = NAMIND('EDPA')
      KEDPA = IW(NAEDPA)
      IF ( KEDPA.EQ.0) GO TO 900
C
C-   EDPA BANK exist update constant value
C
      EDTNOI(1) = RTABL(KEDPA,1,JEDPN1)
      EDTNOI(2) = RTABL(KEDPA,1,JEDPN2)
      EDTNOI(3) = RTABL(KEDPA,1,JEDPN3)
      KALEDT(1) = ITABL(KEDPA,1,JEDPG1)
      KALEDT(2) = ITABL(KEDPA,1,JEDPG2)
      KALEDT(3) = ITABL(KEDPA,1,JEDPG3)
      EDTGAI(1) = 1./FLOAT(KALEDT(1))
      EDTGAI(2) = 1./FLOAT(KALEDT(2))
      EDTGAI(3) = 1./FLOAT(KALEDT(3))
      EDTGER(1) = RTABL(KEDPA,1,JEDPE1)
      EDTGER(2) = RTABL(KEDPA,1,JEDPE2)
      EDTGER(3) = RTABL(KEDPA,1,JEDPE3)
      DO I=1,12
         ETTNOI(I,1) = RTABL(KEDPA,1,JEDPO1)
         ETTNOI(I,2) = RTABL(KEDPA,1,JEDPO2)
         ETTNOI(I,3) = RTABL(KEDPA,1,JEDPO3)
      ENDDO
 900  CONTINUE
      RETURN
      END
#endif
