      SUBROUTINE CODSEL(NEWCOD)
CKEY   QIPBTAG / INTERNAL
C ----------------------------------------------------------------------
C! Selects John carrs modifications depending on year and CARDS
C  Manoj Thulasidas 13-2-97
C  Mod. Ian Tomalin 10/3/99 - New code used by default.
C  Called from QIPBTAG and TRSEL
C
C  OUTPUT:
C         NEWCOD     : logical, true if new code is called for
C ----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
C
      LOGICAL NEWCOD
C
C  Global includes
C
#include "bcs.h"
      LOGICAL FIRST,NEW
      INTEGER JULVER
      DATA FIRST/.TRUE./
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
      IF (FIRST) THEN
        FIRST = .FALSE.

C By default use the new John Carr modifications ...
        NEW = .TRUE.

C ... except if running on LEP1 data processed with old tracking.
        IF (VDYEAR().LE.92) THEN
          CALL GETJUV(JULVER)
          IF (JULVER.LT.300) NEW = .FALSE.
        END IF
C
C Cards can overrule default.
        IF (IW(NAMIND('OQIP')).GT.0) NEW = .FALSE.
        IF (IW(NAMIND('NQIP')).GT.0) NEW = .TRUE.

        IF (NEW) THEN
          WRITE (IW(6),*)'Using new version of QIPBTAG'
        ELSE
          WRITE (IW(6),*)'Using old version of QIPBTAG'
        END IF
      END IF
C
      NEWCOD = NEW
      END
#endif




