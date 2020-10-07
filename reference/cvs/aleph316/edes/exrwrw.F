      INTEGER FUNCTION EXRWRW(XRW)
C.----------------------------------------------------------------------
CKEY ECALDES ROW NUMBER / USER
C     H.Videau      Creation 26/03/88
C! Extended row # --> Physical row #
C   Input : XRW extended row number (integer)   [1,228]
C   Output: EXRWRW       row number (integer)   [1,218]
C   Called by USER.
C   Calls: none
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C    Input
      INTEGER XRW
C
      EXRWRW=EXROER(XRW)
      END
#endif
