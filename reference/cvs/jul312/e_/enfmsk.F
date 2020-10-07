      SUBROUTINE ENFMSK
C=======================================================================
C!    Fills name common ECNMSK
C
C-    Authors : M.N. Minard , M. Pepe       26/02/89
C-
C?
C!======================================================================
#ifndef DOC
#include "ecnmsk.h"
      NAECLU = NAMIND ('ECLU')
      NAESDA = NAMIND ('ESDA')
      NAHSDA = NAMIND ('HSDA')
      NAETP1 = NAMIND ('ETP1')
      NAECT1 = NAMIND ('ECT1')
      NAERL3 = NAMIND ('ERL3')
      NAECOB = NAMIND ('ECOB')
      NAEHYP = NAMIND ('EHYP')
      NAEIDT = NAMIND ('EIDT')
      NAFRFT = NAMIND ('FRFT')
      RETURN
      END
#endif
