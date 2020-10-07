      SUBROUTINE UTPRDB(IBKNA)
C ---------------------------------------------------------------------
C.
C! - Print the content of tables on d/a files
C.
C. - Author   : A. Putzer  - 89/03/07
C.
C.
C.   Arguments: -  IBKNA (input) Name of the bank wanted (or 'ALL ')
C.              -                (or 'X   ' for all banks starting
C.              -                with X)
C.
C ---------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
C
      IBSNR = 0
      IRWNR = 0
      LDBAS = JUNIDB(0)
      CALL UTDBPR(IBKNA,IBSNR,IRWNR,LDBAS,IW(6),IFLAG)
      RETURN
      END
#endif
