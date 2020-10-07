      SUBROUTINE CODSEL(NEWCOD)
CKEY   QIPBTAG / INTERNAL
C ----------------------------------------------------------------------
C! Selects John carrs modifications depending on year and CARDS
C  Manoj Thulasidas 13-2-97
C  Called from QIPBTAG and TRSEL
C
C  OUTPUT:
C          NECOD     : logical, true if new code is called for
C ----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
C  Global includes
C
#include "bcs.h"
      LOGICAL NEWCOD
C
C  Functions
C
      INTEGER NAMIND,VDYEAR
C
C  Locals
C
      LOGICAL OLDCOD
C  Inline functions
C
      INTEGER LCOLS, ID, LROWS, KNEXT, KROW, NRBOS, LFRWRD,
     $   LFRROW, ITABL, L
      REAL RTABL
#include "bmacro.h"
C ----------------------------------------------------------------------
C
C  see if John Carr's modifications are called for explicitly.
C
      NEWCOD = IW(NAMIND('NQIP')).GT.0
      OLDCOD = IW(NAMIND('OQIP')).GT.0
C
C  if not, see if the VDET geometry is old or new
C
      IF (.NOT.NEWCOD .AND. .NOT.OLDCOD) THEN
        NEWCOD = VDYEAR().GT.92
        OLDCOD = VDYEAR().LE.92
      ENDIF
C
C  make sure that NEWCOD and OLDCOD are mutually exclusive
C  default is old code
C
      IF (NEWCOD .AND. OLDCOD) THEN
        NEWCOD = .FALSE.
      ENDIF
      RETURN
      END
#endif
