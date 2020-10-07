      SUBROUTINE X2WSUM
C ----------------------------------------------------------------------
CKEY TRIG LEVEL2 SUMMARY / INTERNAL
C.   Modified : T. Medcalf - 89/10/03
C! - Print Level2 Trigger Summary
C.
C. - Called by      XTWSUM                        from this .HLB
C.
#ifndef DOC
      SAVE
#include "bcs.h"
#include "xtrcom.h"
C ----------------------------------------------------------------------
C.
C. - Print trigger level2 summary
C.
      WRITE(IW(6),100) NACCL2
  100 FORMAT(/// 22X,' Level 2     : ',I10,' events with good tracks')
C.
      RETURN
      END
#endif
