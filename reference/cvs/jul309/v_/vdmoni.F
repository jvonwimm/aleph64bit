      SUBROUTINE  VDMONI
C-----------------------------------------------------
C! monitoring of MVD reconstruction quality
C!
C!
C!    by Hans-Guenther Moser
C!
C!    created:           09/03/88
C!   Modified :- E. Lancon              8-MAR-1991
C!              Make it Dummy
C!
C!    last modification: 09/01/90
C!
C!    -----------------------------------------
C!
C!    this program makes use of the MC truth information,
C!    with real data the results are unpredictable and
C!    lots of error messages and warnigns are ensured
C!
C     =========================================
#ifndef DOC
#include "bcs.h"
        LOGICAL FIRST
        DATA FIRST /.TRUE./
C
C?   E. Lancon Dummy Routine To make Julia run
C
        IF ( FIRST ) THEN
          FIRST = .FALSE.
          WRITE (IW(6),'(///,A,/,A,/,A,/)')
     &  '  +---------------------------------------------------+',
     &  '  | VDMONI Is Dummy  (E. Lancon 8-MAR-1991)           |',
     &  '  +---------------------------------------------------+'
        ELSE
        ENDIF
C
C     =========================================
C
      RETURN
      END
#endif
