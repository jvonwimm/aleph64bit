      SUBROUTINE QGTAOF(I1,I2)
CKEY UTIL ALPHA CONST /USER
C----------------------------------------------------------------------
C!  - Get KFMCT and KFCHT from Alpha comdecks
C!
C!   Author   :- A. Bonissent 30 May 1996
C!======================================================================
#ifndef DOC
#include "qcde.h"
      I1=KFMCT
      I2=KFCHT
      RETURN
      END
#endif

