*DK ehcpwi
      SUBROUTINE EHCPWI
C.----------------------------------------------------------------------
C Y.Karyotakis March 86
C! Fill Wire bank EWHT
C  - Called by ECASIG
C  - Calls ALBOS,BKFMT,BLIST
C.----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "jqcom.h"
#include "ewires.h"
#include "ecnamc.h"
C -------------------------------------------------------------------
C
C - If hit bank does not exist return
C
      IF (IW(NAETHT) .EQ. 0) RETURN
C
C        Find number of fired modules
C
      NMODF = 0
      DO 1 I = 1,LMODU * LCOMP
         IF( IEWHTS(1,I) .NE. 0 ) NMODF = NMODF + 1
    1 CONTINUE
C
C        Book EWHT bank with the right number of words
C
            CALL ALBOS('EWHT',0,NMODF*(LWPLA+1)+LMHLEN,KTHW,IGARB)
            CALL BLIST(IW,'E+','EWHT')
C
            IW( KTHW + LMHCOL ) = LWPLA + 1
            IW( KTHW + LMHROW ) = NMODF
C
C        Fill EWHT bank from IEWHTS table
C
      KTHW = KTHW + LMHLEN
      ICW  = 1
      DO 2 I = 1,LMODU * LCOMP
         IF( IEWHTS(1,I) .NE. 0 ) THEN
              DO 3 J=1,LWPLA + 1
               IW(KTHW+ICW+J-1) = IEWHTS(1+J-1,I)
    3         CONTINUE
              ICW = ICW + LWPLA+1
         ENDIF
    2 CONTINUE
C
 980  RETURN
      END
#endif
