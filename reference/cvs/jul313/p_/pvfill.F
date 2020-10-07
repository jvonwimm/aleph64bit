      SUBROUTINE PVFILL ( IFIT , PVEC , WEIGHT , IRANG, ISTOP )
C========================================================================
C!   To create weighted P-vectors for Energy-flow calculation
C-
C-   Authors : M.N. Minard , M. Pepe       10/05/89
C
C-   INPUT : IFIT    type of fit
C-           PVEC    unweighted P-vector
C-           WEIGHT  weighting coefficient
C-   OUTPUT: IRANG   cell range in lujets common
C-           ISTOP   =1 JETSET array too small
C
C?
C!======================================================================
#ifndef DOC
#include "jetset.h"
      DIMENSION PVEC(4)
      DATA NELMAX / 1999 /
C
C- Get weighted P-vectors
C
      ISTOP = 0
      DO 10 IL =1,4
       P(N+1,IL) = PVEC(IL)*WEIGHT
 10   CONTINUE

      K(N+1,1) = IFIT
      K(N+1,2) = 0
      N = N+1
      IRANG = N
      IF(N.GT.NELMAX ) ISTOP = 1
      RETURN
      END
#endif
