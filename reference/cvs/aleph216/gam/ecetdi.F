      FUNCTION ECETDI (JT,IP)
C
C-    M.N Minard                   29-07-94
C!    For each storey give the correction factor
C!    from bank ECMC
C     JT = Tower number in theta unit
C     IP = Tower number in phi unit
C---------------------------------------
#ifndef DOC
#include "bcs.h"
#include "ecmcjj.h"
      DATA NAECMC / 0 /
#include "bmacro.h"
      ECETDI = 1.
      IF (NAECMC.EQ.0) NAECMC = NAMIND('ECMC')
      IF ( IW(NAECMC).EQ.0) GO TO 999
      KECMC = IW(NAECMC)
C
C     From tower unit calculate module number
C
       CALL EMDTOW(JT,IP,ISCO,IMDO,IRG)
       IMODU = (ISCO-1)*12+IMDO
       ECETDI = RTABL(KECMC,IMODU,1)
 999   RETURN
       END
#endif
