C! YTOP block data
      BLOCK DATA YBDTOP
#ifndef DOC
#include "ypafma.h"
#include "ydumto.h"
#if defined(YDEBUG)
C ----------------------------------------------------------
C
      DATA IDUMPP/  5, 0,  0, 0,  0, 0,  0, 0,  0, 0,
     &              0, 0,  0, 0,  0, 0,  0, 0,  0, 0,
     &              0, 0,  0, 0,  0, 0,  0, 0,  0, 0,
     &              0, 0,  0, 0,  0, 0,  0, 0,  0, 0/
C
C     DATA IDUMPP/40*0/
      DATA NCLROU/12*0/
C -----------------------------------------------------------
#endif
      DATA YPMASS(JPAFEP),YPMASS(JPAFEM)/0.511000E-03,0.511000E-03/
     &     YPMASS(JPAFMP),YPMASS(JPAFMM)/0.105659,  0.105659/
     &     YPMASS(JPAFPP),YPMASS(JPAFPM)/0.139568,0.139568/
     &     YPMASS(JPAFKP),YPMASS(JPAFKM)/0.493646,0.493646/
     &     YPMASS(JPAFPR),YPMASS(JPAFPB)/0.938270,0.938270/
     &     YPMASS(JPAFPH),YPMASS(JPAFPZ)/0.      ,0.134973/
     &     YPMASS(JPAFKZ)/0.497670/
     &     YPMASS(JPAFLA),YPMASS(JPAFLB)/ 1.11563,1.11563/
      END
#endif
