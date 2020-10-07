      SUBROUTINE LKPREVE
C -----------------------------------------------------------
C! print E-list on unit LFILE(2)
C ----------------------------------------------------------
#ifndef DOC
#include "lkfil.h"
#include "bcs.h"
C -----------------------------------------------------------
      IW(6)=LFILE(2)
      CALL DLUNP (IW(6))
      CALL BPRNT(IW,'E')
      IW(6)=6
      CALL DLUNP (IW(6))
      RETURN
      END
#endif
