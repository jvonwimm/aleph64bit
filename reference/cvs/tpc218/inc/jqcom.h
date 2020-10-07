*CD jqcom
#include "bcs.h"
      COMMON /NAMCOM/   NARUNH, NAPART, NAEVEH, NAVERT, NAKINE, NAKRUN
     &                 ,NAKEVH, NAIMPA, NAASEV, NARUNE, NAKLIN, NARUNR
     &                 ,NAKVOL, NAVOLU
C
#if defined(DOC)
  Standard name-index definition

      NAxxxx                  name-index of bank 'xxxx'
                              index of bank 'xxxx' is Kxxxx = IW(NAxxxx)
                              NAxxxx are set to 0 in ASINIT and defined
                              booking time.
#endif
