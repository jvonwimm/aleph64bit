*DK ehtran
      SUBROUTINE EHTRAN (XV,YV,ZV,UV,VV,WV,X,Y,Z,U,V,W)
C.----------------------------------------------------------------
C R.CLIFFT
C! Towards local coordinates
C
C.----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "trkcom.h"
C
      X = TRKELE(18)*(XV-TRKELE(15))  +
     1    TRKELE(19)*(YV-TRKELE(16))  +
     2    TRKELE(20)*(ZV-TRKELE(17))
      Y = TRKELE(21)*(XV-TRKELE(15))  +
     1    TRKELE(22)*(YV-TRKELE(16))  +
     2    TRKELE(23)*(ZV-TRKELE(17))
      Z = TRKELE(24)*(XV-TRKELE(15))  +
     1    TRKELE(25)*(YV-TRKELE(16))  +
     2    TRKELE(26)*(ZV-TRKELE(17))
      U = TRKELE(18)*UV  +
     1    TRKELE(19)*VV  +
     2    TRKELE(20)*WV
      V = TRKELE(21)*UV  +
     1    TRKELE(22)*VV  +
     2    TRKELE(23)*WV
      W = TRKELE(24)*UV  +
     1    TRKELE(25)*VV  +
     2    TRKELE(26)*WV
C
      RETURN
      END
#endif
